//! REPL: the interactive Read-Eval-Print Loop.
//!
//! Orchestrates the main interaction loop:
//! 1. Display prompt (with git branch, custom prompt function support)
//! 2. Read input via the line editor (with syntax highlighting, history, completion)
//! 3. Execute command via the interpreter
//! 4. Repeat until exit
//!
//! The REPL manages terminal mode transitions - raw mode for editing, cooked mode
//! for command execution - and handles history persistence between sessions.

const std = @import("std");

const lexer = @import("../language/lexer.zig");
const parser = @import("../language/parser.zig");
const expand = @import("../interpreter/expansion/word.zig");
const expansion = @import("../interpreter/expansion/pipeline.zig");
const State = @import("../runtime/state.zig").State;
const io = @import("../terminal/io.zig");
const ansi = @import("../terminal/ansi.zig");
const tui = @import("../terminal/tui.zig");
const prompt = @import("prompt.zig");
const Editor = @import("editor/editor.zig").Editor;

/// History file name stored in user's home directory.
const HISTORY_FILE = ".oshen_history";

/// Errors that can occur during command evaluation.
/// This is the union of all errors from the shell pipeline stages.
pub const EvalError = lexer.LexError || parser.ParseError || expand.ExpandError || expansion.ExpandError || std.posix.ExecveError || error{Unexpected};

/// Function signature for evaluating input.
/// Returns the exit status (0-255) of the executed command.
pub const EvalFn = *const fn (std.mem.Allocator, *State, []const u8) EvalError!u8;

// =============================================================================
// Text utilities
// =============================================================================

/// Find the start of the word at or before the given position.
/// Used for completion to determine what word is being typed.
pub inline fn findWordStart(buf: []const u8, pos: usize) usize {
    if (pos == 0) return 0;
    var i = pos;
    while (i > 0) {
        i -= 1;
        if (isWordBreak(buf[i])) {
            return i + 1;
        }
    }
    return 0;
}

/// Check if a character is a word break (whitespace).
pub inline fn isWordBreak(c: u8) bool {
    return c == ' ' or c == '\t';
}

/// Expand a tilde prefix to the home directory.
/// Returns the expanded path in the provided buffer, or null on error.
/// If the path doesn't start with ~, returns null.
pub fn expandTilde(path: []const u8, home: []const u8, buf: []u8) ?[]const u8 {
    if (path.len == 0 or path[0] != '~') return null;
    if (path.len == 1 or path[1] == '/') {
        const rest = if (path.len > 1) path[1..] else "";
        return std.fmt.bufPrint(buf, "{s}{s}", .{ home, rest }) catch null;
    }
    return null;
}

/// Contract a path by replacing the home directory prefix with ~.
/// Returns the contracted path in the provided buffer, or the original path if no contraction.
pub fn contractTilde(path: []const u8, home: []const u8, buf: []u8) []const u8 {
    if (!std.mem.startsWith(u8, path, home)) return path;
    if (path.len == home.len) return "~";
    if (path[home.len] == '/') {
        return std.fmt.bufPrint(buf, "~{s}", .{path[home.len..]}) catch path;
    }
    return path;
}

// =============================================================================
// REPL
// =============================================================================

/// Run the interactive REPL
pub fn run(allocator: std.mem.Allocator, state: *State, evalFn: EvalFn) !void {
    var editor = Editor.init(allocator);
    defer editor.deinit();

    // Enter raw mode immediately to prevent the kernel from echoing
    // any input that arrives before we're ready (e.g., commands sent
    // to the PTY by terminal multiplexers or workspace restore features)
    try editor.enableRawMode();

    // Set state for alias-aware highlighting
    editor.state = state;

    // History file path (owned copy for async saving)
    var history_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const history_path: ?[]const u8 = if (state.home) |home|
        std.fmt.bufPrint(&history_path_buf, "{s}/{s}", .{ home, HISTORY_FILE }) catch null
    else
        null;

    // Load the user's shell history
    if (history_path) |path| {
        editor.loadHistory(path);
    }

    var prompt_buf: [4096]u8 = undefined;

    while (true) {
        const prompt_str = prompt.build(allocator, state, &prompt_buf);

        // Emit OSC 7 right before readLine so any command sent in response
        // arrives when we're ready to receive it
        if (state.getCwd()) |cwd| {
            tui.emitOsc7(cwd);
        } else |_| {}

        const line = editor.readLine(prompt_str) catch |err| {
            io.printError("oshen: input error: {}\n", .{err});
            continue;
        } orelse {
            io.writeStdout("Goodbye!\n");
            break;
        };
        defer allocator.free(line);

        if (line.len == 0) continue;

        // Restore terminal to normal mode before executing command
        // (commands expect echo, line buffering, etc.)
        editor.restoreTerminal();

        // Clear any stale interrupt flag before executing (could be set by Ctrl+C during prompt)
        state.interrupted = false;

        // Execute the command (Note: Parse errors will be automatically printed to stdout)
        _ = evalFn(allocator, state, line) catch {};

        // If command was interrupted, print ^C for visual feedback
        if (state.interrupted) {
            io.writeStderr("^C\n");
            state.interrupted = false;
        }

        // Add to history with context (CWD and exit status)
        const cwd = state.getCwd() catch "";
        _ = editor.hist.add(.{
            .command = line,
            .cwd = cwd,
            .exit_status = state.status,
        });

        // Re-enable raw mode for the next prompt
        try editor.enableRawMode();

        // Save history after each command
        if (history_path) |path| {
            editor.saveHistory(path);
        }

        // Check if exit was requested
        if (state.should_exit) {
            break;
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

// -----------------------------------------------------------------------------
// Word Utilities
// -----------------------------------------------------------------------------

test "findWordStart: finds beginning of current word" {
    const cases = [_]struct { []const u8, usize, usize }{
        .{ "echo", 4, 0 }, // end of single word
        .{ "echo hello", 10, 5 }, // end of second word
        .{ "echo hello", 3, 0 }, // middle of first word
        .{ "echo\thello", 10, 5 }, // tab separator
    };
    for (cases) |c| try testing.expectEqual(c[2], findWordStart(c[0], c[1]));
}

// -----------------------------------------------------------------------------
// Tilde Utilities
// -----------------------------------------------------------------------------

test "expandTilde: expands ~ to home directory" {
    var buf: [256]u8 = undefined;
    const home = "/home/user";

    const cases = [_]struct { []const u8, ?[]const u8 }{
        .{ "~", "/home/user" }, // ~ alone
        .{ "~/src/project", "/home/user/src/project" }, // ~/subpath
        .{ "/usr/bin", null }, // absolute path (no expansion)
        .{ "~other/path", null }, // ~username (unsupported)
    };
    for (cases) |c| {
        if (c[1]) |expected| {
            try testing.expectEqualStrings(expected, expandTilde(c[0], home, &buf).?);
        } else {
            try testing.expect(expandTilde(c[0], home, &buf) == null);
        }
    }
}

test "contractTilde: replaces home prefix with ~" {
    var buf: [256]u8 = undefined;
    const home = "/home/user";

    const cases = [_]struct { []const u8, []const u8 }{
        .{ "/home/user", "~" }, // exact home
        .{ "/home/user/src/project", "~/src/project" }, // with subpath
        .{ "/usr/bin", "/usr/bin" }, // outside home (no contraction)
        .{ "/home/username/foo", "/home/username/foo" }, // partial match (boundary check)
    };
    for (cases) |c| try testing.expectEqualStrings(c[1], contractTilde(c[0], home, &buf));
}
