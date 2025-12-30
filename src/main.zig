//! Oshen Shell - A modern, Zig-powered shell.
//!
//! # Architecture
//!
//! Oshen follows a classic 4-stage pipeline architecture:
//!
//! 1. **Lexer** (`src/language/lexer.zig`): Converts raw source text into a stream of tokens.
//! 2. **Parser** (`src/language/parser.zig`): Consumes tokens and produces an Abstract Syntax Tree (AST).
//!    - Supports "Parse Once, Execute Many" for loops by storing bodies as raw source.
//! 3. **Expansion** (`src/interpreter/expansion/`): Processes the AST before execution.
//!    - Variable expansion (`$var`), Command substitution (`$(cmd)`), Globs (`*.zig`).
//! 4. **Execution** (`src/interpreter/execution/`): Traverses the AST and runs commands.
//!    - Manages job control, signals, and process groups.
//!    - Implements builtins and external command execution.
//!
//! # Entry Points
//!
//! - `main.zig`: CLI entry point, handles argument parsing and initialization.
//! - `repl/`: Interactive Read-Eval-Print Loop with syntax highlighting and history.
//! - `interpreter/`: Core execution engine.

const std = @import("std");
const builtin = @import("builtin");
const cli = @import("cli.zig");
const State = @import("runtime/state.zig").State;
const repl = @import("repl/repl.zig");
const io = @import("terminal/io.zig");
const interpreter = @import("interpreter/interpreter.zig");
const signals = @import("interpreter/execution/signals.zig");

/// Config file path relative to home directory
const CONFIG_FILE = ".oshen_floor";

/// C library setenv (not exposed in Zig std for macOS)
extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const opts = cli.parseArgs(args) catch {
        // Error already printed by args parser
        std.process.exit(1);
    } orelse {
        // Help or version was printed, exit successfully
        return;
    };

    // Initialize shell state
    var state = State.init(allocator);
    state.initCurrentScope(); // Fix up self-referential pointer after struct is in final location
    defer state.deinit();

    // On macOS, run path_helper to get the full system PATH for login/interactive shells
    if (builtin.os.tag == .macos and (opts.is_login or opts.mode == .interactive)) {
        runPathHelper(allocator);
    }

    switch (opts.mode) {
        .interactive => {
            // Set up signal handling for interactive mode
            state.interactive = true;
            signals.initInteractive(&state);

            // Load config file
            loadConfig(allocator, &state);

            try repl.run(allocator, &state, interpreter.execute);
        },
        .command => |cmd| {
            const status = interpreter.execute(allocator, &state, cmd) catch 1;
            std.process.exit(status);
        },
        .script => |path| {
            const status = interpreter.executeFile(allocator, &state, path) catch 1;
            std.process.exit(status);
        },
    }
}

/// Load the user's config file if it exists
fn loadConfig(allocator: std.mem.Allocator, state: *State) void {
    const home = state.home orelse return;

    const config_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ home, CONFIG_FILE }) catch return;
    defer allocator.free(config_path);

    // Silently ignore if config doesn't exist
    _ = interpreter.executeFile(allocator, state, config_path) catch {
        // Config file doesn't exist or can't be read - that's fine
    };
}

/// Run macOS path_helper to get the full system PATH from /etc/paths and /etc/paths.d
fn runPathHelper(allocator: std.mem.Allocator) void {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "/usr/libexec/path_helper", "-s" },
    }) catch return;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Output format: PATH="..."; export PATH;
    // Extract the PATH value between quotes
    const prefix = "PATH=\"";
    const start = std.mem.indexOf(u8, result.stdout, prefix) orelse return;
    const path_start = start + prefix.len;
    const path_end = std.mem.indexOfPos(u8, result.stdout, path_start, "\"") orelse return;
    const path_value = result.stdout[path_start..path_end];

    // Set the PATH environment variable using POSIX setenv
    const path_z = allocator.dupeZ(u8, path_value) catch return;
    defer allocator.free(path_z);
    _ = setenv("PATH", path_z, 1);
}

// =============================================================================
// Tests
// =============================================================================

test {
    // Include modules with tests that aren't in the main import graph
    // Language
    _ = @import("language/lexer.zig");
    _ = @import("language/parser.zig");

    // Interpreter
    _ = @import("interpreter/expansion/glob.zig");
    _ = @import("interpreter/expansion/word.zig");
    _ = @import("interpreter/expansion/statement.zig");

    // Runtime
    _ = @import("runtime/state.zig");
    _ = @import("runtime/scope.zig");
    _ = @import("runtime/jobs.zig");
    _ = @import("runtime/builtins/calc.zig");
    _ = @import("runtime/builtins/export.zig");
    _ = @import("runtime/builtins/increment.zig");
    _ = @import("runtime/builtins/string.zig");
    _ = @import("runtime/builtins/terminal.zig");
    _ = @import("runtime/builtins/test.zig");

    // Terminal
    _ = @import("terminal/args.zig");
    _ = @import("terminal/io/writer.zig");

    // REPL
    _ = @import("repl/editor/editor.zig");
    _ = @import("repl/editor/history.zig");
    _ = @import("repl/editor/ui/complete.zig");
    _ = @import("repl/editor/ui/highlight.zig");
    _ = @import("repl/editor/ui/suggest.zig");
    _ = @import("repl/prompt.zig");
    _ = @import("repl/repl.zig");
}
