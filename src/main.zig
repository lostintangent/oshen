//! Oshen shell - A modern, Zig-powered shell.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const State = @import("runtime/state.zig").State;
const repl = @import("repl/repl.zig");
const io = @import("terminal/io.zig");
const Args = @import("terminal/args.zig");
const interpreter = @import("interpreter/interpreter.zig");
const signals = @import("interpreter/execution/signals.zig");

/// Config file path relative to home directory
const CONFIG_FILE = ".oshen_floor";

/// C library setenv (not exposed in Zig std for macOS)
extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

// =============================================================================
// CLI Argument Parsing
// =============================================================================

const RunMode = union(enum) {
    interactive,
    command: []const u8,
    script: []const u8,
};

const Options = struct {
    mode: RunMode = .interactive,
    is_login: bool = false,
};

/// CLI argument spec - single source of truth for parsing and help
const cli_spec = Args.Spec("oshen", .{
    .usage = "oshen [OPTIONS] [SCRIPT]",
    .desc = "Oshen Shell (v" ++ build_options.version ++ ")",
    .args = .{
        .help = Args.Flag(.{ .short = "h", .long = "help", .desc = "Show this help" }),
        .version = Args.Flag(.{ .short = "v", .long = "version", .desc = "Show version" }),
        .login = Args.Flag(.{ .short = "l", .long = "login", .desc = "Run as login shell" }),
        .interactive = Args.Flag(.{ .short = "i", .long = "interactive", .desc = "Force interactive mode" }),
        .command = Args.StringOption(.{ .short = "c", .long = "command", .desc = "Execute command and exit" }),
        .script = Args.StringPositional(.{ .desc = "Script file to execute", .default = "" }),
    },
    .examples = &.{
        "oshen                   Start interactive REPL",
        "oshen -c 'echo hello'   Run a single command",
        "oshen script.osh        Execute a script file",
    },
});

fn parseArgs(args: []const []const u8) Args.ParseError!?Options {
    const r = try cli_spec.parse(args[1..]);

    if (r.help) {
        io.writeStdout(cli_spec.help);
        return null;
    }

    if (r.version) {
        io.writeStdout(build_options.version ++ "\n");
        return null;
    }

    return .{
        .is_login = r.login or args[0][0] == '-',
        .mode = if (r.command) |cmd|
            .{ .command = cmd }
        else if (r.script.len > 0)
            .{ .script = r.script }
        else
            .interactive,
    };
}

// =============================================================================
// Main Entry Point
// =============================================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const opts = parseArgs(args) catch {
        std.process.exit(1);
    } orelse {
        return; // Help or version was printed
    };

    // Initialize shell state
    var state = State.init(allocator);
    state.initCurrentScope();
    defer state.deinit();

    // On macOS, run path_helper to get the full system PATH for login/interactive shells
    if (builtin.os.tag == .macos and (opts.is_login or opts.mode == .interactive)) {
        runPathHelper(allocator);
    }

    switch (opts.mode) {
        .interactive => {
            state.interactive = true;
            signals.initInteractive(&state);
            
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

    _ = interpreter.executeFile(allocator, state, config_path) catch {};
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
    const prefix = "PATH=\"";
    const start = std.mem.indexOf(u8, result.stdout, prefix) orelse return;
    const path_start = start + prefix.len;
    const path_end = std.mem.indexOfPos(u8, result.stdout, path_start, "\"") orelse return;
    const path_value = result.stdout[path_start..path_end];

    const path_z = allocator.dupeZ(u8, path_value) catch return;
    defer allocator.free(path_z);
    _ = setenv("PATH", path_z, 1);
}

// =============================================================================
// Tests
// =============================================================================

test {
    _ = @import("language/lexer.zig");
    _ = @import("language/parser.zig");

    _ = @import("interpreter/expansion/glob.zig");
    _ = @import("interpreter/expansion/word.zig");
    _ = @import("interpreter/expansion/pipeline.zig");
    
    _ = @import("runtime/state.zig");
    _ = @import("runtime/scope.zig");
    _ = @import("runtime/jobs.zig");

    _ = @import("runtime/builtins/calc.zig");
    _ = @import("runtime/builtins/export.zig");
    _ = @import("runtime/builtins/increment.zig");
    _ = @import("runtime/builtins/list.zig");
    _ = @import("runtime/builtins/string.zig");
    _ = @import("runtime/builtins/terminal.zig");
    _ = @import("runtime/builtins/test.zig");

    _ = @import("terminal/args.zig");
    _ = @import("terminal/io/writer.zig");

    _ = @import("repl/editor/editor.zig");
    _ = @import("repl/editor/history.zig");
    _ = @import("repl/editor/ui/complete.zig");
    _ = @import("repl/editor/ui/highlight.zig");
    _ = @import("repl/editor/ui/suggest.zig");
    _ = @import("repl/prompt.zig");
    _ = @import("repl/repl.zig");
}
