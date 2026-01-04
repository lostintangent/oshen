//! terminal builtin - Terminal control for TUI scripting
//!
//! Provides cursor control, screen clearing, and terminal mode management.
//! All commands are no-ops when stdout is not a TTY, making scripts safe to pipe.
//!
//! NOTE: Does not use Args API - implements subcommand dispatch pattern which
//! Args API doesn't currently support well (would require awkward argv construction).

const std = @import("std");
const builtins = @import("../builtins.zig");
const ansi = @import("../../terminal/ansi.zig");
const io = @import("../../terminal/io.zig");

pub const builtin = builtins.Builtin{
    .name = "terminal",
    .run = run,
    .help =
    \\terminal <command> [options]
    \\
    \\Terminal control for TUI scripting. All commands are no-ops when stdout
    \\is not a TTY, making scripts safe to pipe.
    \\
    \\Commands:
    \\  save                        Save cursor position
    \\  restore                     Restore cursor position
    \\  move [options]              Move cursor
    \\  clear [options]             Clear screen/lines
    \\  enable [options]            Enable terminal features
    \\  disable [options]           Disable terminal features
    \\  title <text>                Set window title
    \\
    \\Move options:
    \\  --up [N]                    Move cursor up N lines (default: 1)
    \\  --down [N]                  Move cursor down N lines (default: 1)
    \\  --left [N]                  Move cursor left N columns (default: 1)
    \\  --right [N]                 Move cursor right N columns (default: 1)
    \\  --home                      Move cursor to home position (0,0)
    \\
    \\Clear options:
    \\  --lines N                   Clear N lines starting from current
    \\  --screen                    Clear entire screen
    \\  --below                     Clear from cursor to end of screen
    \\  --all                       Clear screen and scrollback
    \\  (no options)                Clear current line
    \\
    \\Enable/disable options:
    \\  --cursor                    Show/hide cursor
    \\  --mouse                     Enable/disable mouse reporting
    \\  --focus                     Enable/disable focus events
    \\  --alternate                 Enter/exit alternate screen buffer
    \\
    \\Examples:
    \\  terminal move --up 5 --left 3       # Move cursor up 5, left 3
    \\  terminal clear --lines 10           # Clear 10 lines
    \\  terminal enable --mouse --cursor    # Enable mouse and show cursor
    \\  terminal title "My Script"          # Set window title
    ,
};

// =============================================================================
// Command Dispatch
// =============================================================================

const Command = *const fn (args: []const []const u8) void;

const commands = std.StaticStringMap(Command).initComptime(.{
    .{ "save", cmdSave },
    .{ "restore", cmdRestore },
    .{ "move", cmdMove },
    .{ "clear", cmdClear },
    .{ "enable", cmdEnable },
    .{ "disable", cmdDisable },
    .{ "title", cmdTitle },
});

fn run(_: *builtins.State, args: []const []const u8) u8 {
    // Validate arguments first (even when not a TTY)
    if (args.len == 0) {
        builtins.io.writeStderr("terminal: missing command\n");
        return 1;
    }

    const handler = commands.get(args[0]) orelse {
        builtins.io.printError("terminal: unknown command '{s}'\n", .{args[0]});
        return 1;
    };

    // No-op when not a TTY, but still return success
    if (!io.isStdoutTty()) return 0;

    handler(args[1..]);
    return 0;
}

// =============================================================================
// Command Implementations
// =============================================================================

fn cmdSave(_: []const []const u8) void {
    io.writeStdout(ansi.cursor_save);
}

fn cmdRestore(_: []const []const u8) void {
    io.writeStdout(ansi.cursor_restore);
}

const move_flags = std.StaticStringMap(u8).initComptime(.{
    .{ "--up", 'A' },
    .{ "--down", 'B' },
    .{ "--right", 'C' },
    .{ "--left", 'D' },
});

fn cmdMove(args: []const []const u8) void {
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const flag = args[i];
        if (std.mem.eql(u8, flag, "--home")) {
            io.writeStdout(ansi.cursor_home);
        } else if (move_flags.get(flag)) |code| {
            writeMove(code, parseNextArg(args, &i));
        }
    }
}

const clear_flags = std.StaticStringMap([]const u8).initComptime(.{
    .{ "--screen", ansi.clear_screen },
    .{ "--below", ansi.clear_below },
    .{ "--all", ansi.clear_all },
});

fn cmdClear(args: []const []const u8) void {
    if (args.len == 0) {
        io.writeStdout(ansi.clear_line);
        return;
    }

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const flag = args[i];
        if (clear_flags.get(flag)) |seq| {
            io.writeStdout(seq);
        } else if (std.mem.eql(u8, flag, "--lines")) {
            const n = parseNextArg(args, &i);
            if (n > 1) writeMove('A', n - 1);
            io.writeStdout("\r");
            io.writeStdout(ansi.clear_below);
        }
    }
}

const enable_flags = std.StaticStringMap([]const u8).initComptime(.{
    .{ "--cursor", ansi.cursor_show },
    .{ "--mouse", ansi.mouse_on },
    .{ "--focus", ansi.focus_on },
    .{ "--alternate", ansi.alt_screen_enter },
});

const disable_flags = std.StaticStringMap([]const u8).initComptime(.{
    .{ "--cursor", ansi.cursor_hide },
    .{ "--mouse", ansi.mouse_off },
    .{ "--focus", ansi.focus_off },
    .{ "--alternate", ansi.alt_screen_exit },
});

fn cmdEnable(args: []const []const u8) void {
    applyFlagSequences(args, enable_flags);
}

fn cmdDisable(args: []const []const u8) void {
    applyFlagSequences(args, disable_flags);
}

/// Apply escape sequences for matching flags from a flag map.
fn applyFlagSequences(args: []const []const u8, comptime flag_map: anytype) void {
    for (args) |flag| {
        if (flag_map.get(flag)) |seq| io.writeStdout(seq);
    }
}

fn cmdTitle(args: []const []const u8) void {
    if (args.len == 0) return;
    io.writeStdout(ansi.osc_title_start);
    io.writeStdout(args[0]);
    io.writeStdout(ansi.osc_end);
}

// =============================================================================
// Helpers
// =============================================================================

/// Write a parameterized cursor movement sequence: CSI {n} {code}
fn writeMove(code: u8, n: u16) void {
    if (n == 0) return;
    var buf: [16]u8 = undefined;
    const len = std.fmt.bufPrint(&buf, "\x1b[{d}{c}", .{ n, code }) catch return;
    io.writeStdout(len);
}

/// Parse the next argument as a u16, defaulting to 1 if missing or invalid
fn parseNextArg(args: []const []const u8, i: *usize) u16 {
    if (i.* + 1 < args.len) {
        const next = args[i.* + 1];
        if (next.len > 0 and next[0] != '-') {
            i.* += 1;
            return std.fmt.parseInt(u16, next, 10) catch 1;
        }
    }
    return 1;
}

// =============================================================================
// Tests
// =============================================================================
//
// NOTE: Terminal output is pure string formatting (ANSI escape codes), similar
// to highlight.zig. Unit tests verify flag lookups and argument parsing.
// No E2E tests needed - escape sequences are TTY-dependent.

const testing = std.testing;

// -----------------------------------------------------------------------------
// Terminal: argument parsing
// -----------------------------------------------------------------------------

test "Terminal: parseNextArg" {
    const cases = [_]struct {
        args: []const []const u8,
        expected_value: u16,
        expected_index: usize,
    }{
        // valid numbers
        .{ .args = &.{ "--up", "5" }, .expected_value = 5, .expected_index = 1 },
        .{ .args = &.{ "--up", "999" }, .expected_value = 999, .expected_index = 1 },
        .{ .args = &.{ "--up", "0" }, .expected_value = 0, .expected_index = 1 },
        // missing/invalid - defaults to 1
        .{ .args = &.{"--up"}, .expected_value = 1, .expected_index = 0 },
        .{ .args = &.{ "--up", "--down" }, .expected_value = 1, .expected_index = 0 }, // next is flag
        .{ .args = &.{ "--up", "abc" }, .expected_value = 1, .expected_index = 1 }, // invalid number
        .{ .args = &.{ "--up", "-5" }, .expected_value = 1, .expected_index = 0 }, // negative
    };
    for (cases) |case| {
        var i: usize = 0;
        const value = parseNextArg(case.args, &i);
        try testing.expectEqual(case.expected_value, value);
        try testing.expectEqual(case.expected_index, i);
    }
}

// -----------------------------------------------------------------------------
// Terminal: command dispatch
// -----------------------------------------------------------------------------

test "Terminal: command lookup" {
    // valid commands
    const valid_cmds = [_][]const u8{ "save", "restore", "move", "clear", "enable", "disable", "title" };
    for (valid_cmds) |cmd| {
        try testing.expect(commands.get(cmd) != null);
    }
    // invalid commands
    try testing.expectEqual(@as(?Command, null), commands.get("invalid"));
    try testing.expectEqual(@as(?Command, null), commands.get(""));
    try testing.expectEqual(@as(?Command, null), commands.get("SAVE")); // case sensitive
}

// -----------------------------------------------------------------------------
// Terminal: flag mappings
// -----------------------------------------------------------------------------

test "Terminal: move flags" {
    const cases = .{
        .{ "--up", @as(?u8, 'A') },
        .{ "--down", @as(?u8, 'B') },
        .{ "--right", @as(?u8, 'C') },
        .{ "--left", @as(?u8, 'D') },
        .{ "--home", @as(?u8, null) }, // handled separately
        .{ "up", @as(?u8, null) }, // missing --
        .{ "--diagonal", @as(?u8, null) }, // invalid
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], move_flags.get(case[0]));
    }
}

test "Terminal: clear flags" {
    // valid flags produce non-null sequences
    try testing.expect(clear_flags.get("--screen") != null);
    try testing.expect(clear_flags.get("--below") != null);
    try testing.expect(clear_flags.get("--all") != null);
    // --lines is handled separately (needs arg parsing)
    try testing.expectEqual(@as(?[]const u8, null), clear_flags.get("--lines"));
}

test "Terminal: enable/disable flags" {
    const flags = [_][]const u8{ "--cursor", "--mouse", "--focus", "--alternate" };
    for (flags) |flag| {
        // both enable and disable have mappings
        const enable_seq = enable_flags.get(flag);
        const disable_seq = disable_flags.get(flag);
        try testing.expect(enable_seq != null);
        try testing.expect(disable_seq != null);
        // and they produce different sequences
        try testing.expect(!std.mem.eql(u8, enable_seq.?, disable_seq.?));
    }
}

// -----------------------------------------------------------------------------
// Terminal: ANSI sequence validation
// -----------------------------------------------------------------------------

test "Terminal: ANSI sequences are non-empty" {
    // cursor
    try testing.expect(ansi.cursor_save.len > 0);
    try testing.expect(ansi.cursor_restore.len > 0);
    try testing.expect(ansi.cursor_home.len > 0);
    try testing.expect(ansi.cursor_show.len > 0);
    try testing.expect(ansi.cursor_hide.len > 0);
    // clear
    try testing.expect(ansi.clear_line.len > 0);
    try testing.expect(ansi.clear_screen.len > 0);
    try testing.expect(ansi.clear_below.len > 0);
    try testing.expect(ansi.clear_all.len > 0);
    // modes
    try testing.expect(ansi.mouse_on.len > 0);
    try testing.expect(ansi.mouse_off.len > 0);
    try testing.expect(ansi.alt_screen_enter.len > 0);
    try testing.expect(ansi.alt_screen_exit.len > 0);
}

// -----------------------------------------------------------------------------
// Terminal: edge cases
// -----------------------------------------------------------------------------

test "Terminal: writeMove with zero" {
    // writeMove(code, 0) should be a no-op (returns early)
    // We verify it doesn't crash with edge case inputs
    writeMove('A', 0);
    writeMove('B', 0);
}
