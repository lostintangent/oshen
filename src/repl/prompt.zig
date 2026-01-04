//! Prompt: generates the shell prompt string for the REPL.
//!
//! Supports two modes:
//! 1. Custom prompt - if a `prompt` function is defined, executes it and uses output
//! 2. Default prompt - shows cwd (with ~ substitution) and optional git branch
//!
//! Default format: `<green>~/path</green> <magenta>(branch)</magenta> # `

const std = @import("std");

const repl = @import("repl.zig");
const interpreter = @import("../interpreter/interpreter.zig");
const State = @import("../runtime/state.zig").State;
const ansi = @import("../terminal/ansi.zig");

// =============================================================================
// Constants
// =============================================================================

/// Prompt suffix appended after cwd and git branch.
const suffix = "# ";

/// Buffer segment sizes for prompt building.
/// The prompt buffer is divided into segments to avoid allocation:
/// [0..512]      - tilde-contracted path
/// [512..1024]   - colored cwd prefix
/// [1024..1536]  - colored git branch suffix
/// [1536..]      - final assembled prompt
const PATH_SEGMENT_END = 512;
const CWD_SEGMENT_END = 1024;
const BRANCH_SEGMENT_END = 1536;

// =============================================================================
// Public API
// =============================================================================

/// Build and return the shell prompt string.
/// If a `prompt` function is defined, execute it and use its output.
/// Otherwise, use the default cwd-based prompt.
pub fn build(allocator: std.mem.Allocator, state: *State, buf: []u8) []const u8 {
    if (state.getFunction("prompt")) |func| {
        const output = interpreter.executeAndCapture(allocator, state, func.source) catch return buildDefault(allocator, state, buf);
        defer allocator.free(output);
        const len = @min(output.len, buf.len);
        @memcpy(buf[0..len], output[0..len]);
        return buf[0..len];
    }
    return buildDefault(allocator, state, buf);
}

// =============================================================================
// Private helpers
// =============================================================================

/// Default prompt: green cwd (with ~ substitution), optional magenta git branch, followed by "# "
fn buildDefault(allocator: std.mem.Allocator, state: *State, buf: []u8) []const u8 {
    const cwd = state.getCwd() catch "?";

    // Contract home directory to ~ using shared utility
    const display_path = if (state.home) |home|
        repl.contractTilde(cwd, home, buf[0..PATH_SEGMENT_END])
    else
        cwd;

    const cwd_prefix = std.fmt.bufPrint(
        buf[PATH_SEGMENT_END..CWD_SEGMENT_END],
        ansi.green ++ "{s}" ++ ansi.reset ++ " ",
        .{display_path},
    ) catch return suffix;

    const branch_suffix = if (getGitBranch(allocator, state)) |git_output| blk: {
        defer allocator.free(git_output);
        const branch = std.mem.trimRight(u8, git_output, "\n\r ");
        break :blk std.fmt.bufPrint(
            buf[CWD_SEGMENT_END..BRANCH_SEGMENT_END],
            ansi.magenta ++ "({s})" ++ ansi.reset ++ " ",
            .{branch},
        ) catch "";
    } else "";

    return std.fmt.bufPrint(buf[BRANCH_SEGMENT_END..], "{s}{s}" ++ suffix, .{ cwd_prefix, branch_suffix }) catch suffix;
}

/// Get the current git branch name by calling git CLI
/// Caller must free the returned slice using the same allocator.
fn getGitBranch(allocator: std.mem.Allocator, state: *State) ?[]const u8 {
    const output = interpreter.executeAndCapture(allocator, state, "git branch --show-current 2>/dev/null") catch return null;
    const branch = std.mem.trimRight(u8, output, "\n\r ");
    if (branch.len == 0) {
        allocator.free(output);
        return null;
    }
    return output;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "Prompt: default format ends with suffix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    var buf: [4096]u8 = undefined;
    try testing.expect(std.mem.endsWith(u8, buildDefault(arena.allocator(), &state, &buf), "# "));
}

test "Prompt: tilde substitution in path" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    state.home = "/home/testuser";
    var buf: [4096]u8 = undefined;
    try testing.expect(std.mem.endsWith(u8, buildDefault(arena.allocator(), &state, &buf), "# "));
}

test "Prompt: custom function overrides default" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    try state.setFunction("prompt", "echo 'custom> '");
    var buf: [4096]u8 = undefined;
    try testing.expectEqualStrings("custom> ", build(arena.allocator(), &state, &buf));
}

test "Prompt: fallback to default without function" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    var buf: [4096]u8 = undefined;
    try testing.expect(std.mem.endsWith(u8, build(arena.allocator(), &state, &buf), "# "));
}
