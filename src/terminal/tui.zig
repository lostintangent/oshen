//! TUI primitives: raw mode, input handling, and terminal interaction.

const std = @import("std");
const posix = std.posix;

const ansi = @import("ansi.zig");
const io = @import("io.zig");

// =============================================================================
// Types
// =============================================================================

/// Input key/event from the terminal.
pub const Key = union(enum) {
    char: u8,
    ctrl: u8,
    up,
    down,
    left,
    right,
    home,
    end,
    page_up,
    page_down,
    delete,
    backspace,
    enter,
    tab,
    eof,
    word_left,
    word_right,
    kill_line,
    kill_word,
    clear_screen,
    interrupt,
    focus_in,
    focus_out,
    escape,
    mouse: MouseEvent,
    unknown,
};

pub const MouseEvent = struct {
    x: u16,
    y: u16,
};

// =============================================================================
// Terminal Mode
// =============================================================================

/// Enable raw mode for the terminal. Returns original settings for restoration.
pub fn enableRawMode(fd: posix.fd_t) !posix.termios {
    const orig = try posix.tcgetattr(fd);
    var raw = orig;

    // Input: no break, no CR to NL, no parity check, no strip, no flow control
    raw.iflag.BRKINT = false;
    raw.iflag.ICRNL = false;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;
    raw.iflag.IXON = false;

    // Output: disable post processing
    raw.oflag.OPOST = false;

    // Control: 8-bit chars
    raw.cflag.CSIZE = .CS8;

    // Local: no echo, no canonical, no extended, no signals
    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.IEXTEN = false;
    raw.lflag.ISIG = false;

    // Read at least 1 char, no timeout
    raw.cc[@intFromEnum(posix.V.MIN)] = 1;
    raw.cc[@intFromEnum(posix.V.TIME)] = 0;

    try posix.tcsetattr(fd, .NOW, raw);
    return orig;
}

/// Restore terminal to original settings.
pub fn restoreTerminal(fd: posix.fd_t, orig: posix.termios) void {
    var cooked = orig;
    // Disable ECHOCTL to prevent ^C or ^[[I from being displayed
    if (@hasField(@TypeOf(cooked.lflag), "ECHOCTL")) {
        cooked.lflag.ECHOCTL = false;
    }
    posix.tcsetattr(fd, .FLUSH, cooked) catch {};
}

// =============================================================================
// Reporting
// =============================================================================

pub fn enableFocusReporting(fd: posix.fd_t) void {
    io.writeToFd(fd, ansi.focus_on);
}

pub fn disableFocusReporting(fd: posix.fd_t) void {
    io.writeToFd(fd, ansi.focus_off);
}

pub fn enableMouseReporting(fd: posix.fd_t) void {
    io.writeToFd(fd, ansi.mouse_on);
}

pub fn disableMouseReporting(fd: posix.fd_t) void {
    io.writeToFd(fd, ansi.mouse_off);
}

// =============================================================================
// Input
// =============================================================================

/// Read a key from the terminal.
pub fn readKey(fd: posix.fd_t) !Key {
    var buf: [1]u8 = undefined;
    const n = try posix.read(fd, &buf);
    if (n == 0) return .eof;

    const c = buf[0];

    // Control characters (0-31)
    if (c < 32) return controlKey(c) orelse readEscapeSequence(fd);

    // DEL key
    if (c == 127) return .backspace;

    return .{ .char = c };
}

/// Map control character to Key.
fn controlKey(c: u8) ?Key {
    return switch (c) {
        0 => .unknown,
        1 => .{ .ctrl = 'a' },
        2 => .{ .ctrl = 'b' },
        3 => .interrupt,
        4 => .eof,
        5 => .{ .ctrl = 'e' },
        6 => .{ .ctrl = 'f' },
        7 => .unknown,
        8 => .backspace,
        9 => .tab,
        10, 13 => .enter,
        11 => .kill_line,
        12 => .clear_screen,
        14 => .{ .ctrl = 'n' },
        16 => .{ .ctrl = 'p' },
        21 => .{ .ctrl = 'u' },
        23 => .kill_word,
        27 => null, // ESC - needs further parsing
        else => .unknown,
    };
}

/// Parse escape sequences (CSI, SS3, Alt+key).
fn readEscapeSequence(fd: posix.fd_t) Key {
    const c1 = readByteWithTimeout(fd, 10) orelse return .escape;

    return switch (c1) {
        '[' => parseCsiSequence(fd),
        'O' => parseSs3Sequence(fd),
        'b' => .word_left,
        'f' => .word_right,
        else => .unknown,
    };
}

/// Parse CSI sequence (ESC [).
fn parseCsiSequence(fd: posix.fd_t) Key {
    const c = readByteWithTimeout(fd, 10) orelse return .unknown;

    // Extended sequence (starts with digit or '<')
    if ((c >= '0' and c <= '9') or c == '<') {
        return parseExtendedCsi(fd, c);
    }

    // Simple CSI sequence
    return switch (c) {
        'A' => .up,
        'B' => .down,
        'C' => .right,
        'D' => .left,
        'H' => .home,
        'F' => .end,
        'I' => .focus_in,
        'O' => .focus_out,
        else => .unknown,
    };
}

/// Parse extended CSI sequence (ESC [ <digit/params> <final>).
fn parseExtendedCsi(fd: posix.fd_t, first: u8) Key {
    var seq: [32]u8 = undefined;
    seq[0] = first;
    var len: usize = 1;

    // Read until final byte (64-126) or buffer full
    while (len < seq.len) {
        const c = readByteWithTimeout(fd, 10) orelse break;
        seq[len] = c;
        len += 1;
        if (c >= 64 and c <= 126) break;
    }

    if (len == 0) return .unknown;

    // Mouse: ESC [ < params M/m
    if (first == '<') return parseMouseSequence(seq[1..len]);

    // Tilde sequences: ESC [ n ~
    if (len >= 2 and seq[1] == '~') {
        return switch (first) {
            '1', '7' => .home,
            '3' => .delete,
            '4', '8' => .end,
            '5' => .page_up,
            '6' => .page_down,
            else => .unknown,
        };
    }

    // Modifier sequences: ESC [ 1 ; 5 C (ctrl+right)
    const final = seq[len - 1];
    if (final == 'C' or final == 'D') {
        const slice = seq[0..len];
        if (std.mem.indexOf(u8, slice, ";5") != null or std.mem.indexOf(u8, slice, ";3") != null) {
            return if (final == 'C') .word_right else .word_left;
        }
    }

    return .unknown;
}

/// Parse mouse sequence content (after '<', before 'M'/'m').
fn parseMouseSequence(seq: []const u8) Key {
    if (seq.len == 0) return .unknown;
    const final = seq[seq.len - 1];
    if (final != 'M') return .unknown; // Only handle press, not release

    const content = seq[0 .. seq.len - 1];
    var iter = std.mem.splitScalar(u8, content, ';');

    const button_str = iter.next() orelse return .unknown;
    const x_str = iter.next() orelse return .unknown;
    const y_str = iter.next() orelse return .unknown;

    const button = std.fmt.parseInt(u16, button_str, 10) catch return .unknown;
    const x = std.fmt.parseInt(u16, x_str, 10) catch return .unknown;
    const y = std.fmt.parseInt(u16, y_str, 10) catch return .unknown;

    // Button 0 is left click
    if (button == 0) return .{ .mouse = .{ .x = x, .y = y } };

    return .unknown;
}

/// Parse SS3 sequence (ESC O).
fn parseSs3Sequence(fd: posix.fd_t) Key {
    const c = readByteWithTimeout(fd, 10) orelse return .unknown;
    return switch (c) {
        'H' => .home,
        'F' => .end,
        else => .unknown,
    };
}

/// Read a single byte with timeout (milliseconds). Returns null on timeout or error.
fn readByteWithTimeout(fd: posix.fd_t, timeout_ms: i32) ?u8 {
    var pfd = [1]posix.pollfd{.{
        .fd = fd,
        .events = posix.POLL.IN,
        .revents = 0,
    }};

    const ready = posix.poll(&pfd, timeout_ms) catch return null;
    if (ready == 0) return null;

    var buf: [1]u8 = undefined;
    const n = posix.read(fd, &buf) catch return null;
    if (n == 0) return null;

    return buf[0];
}

// =============================================================================
// Terminal Info
// =============================================================================

/// Get the current terminal width, or null if unavailable.
pub fn getTerminalWidth() ?usize {
    var ws: posix.system.winsize = undefined;
    const rc = posix.system.ioctl(posix.STDOUT_FILENO, posix.system.T.IOCGWINSZ, @intFromPtr(&ws));
    if (rc == 0 and ws.col > 0) {
        return ws.col;
    }
    return null;
}

/// Emit OSC 7 to notify terminal of current working directory.
pub fn emitOsc7(cwd: []const u8) void {
    var hostname_buf: [posix.HOST_NAME_MAX]u8 = undefined;
    const hostname = posix.gethostname(&hostname_buf) catch "localhost";

    var buf: [2048]u8 = undefined;
    const seq = std.fmt.bufPrint(&buf, "{s}7;file://{s}{s}{s}", .{
        ansi.osc_title_start[0..2], // Just ESC ]
        hostname,
        cwd,
        ansi.osc_end,
    }) catch return;
    io.writeStdout(seq);
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "controlKey: basic mappings" {
    // Special keys
    try testing.expectEqual(Key.interrupt, controlKey(3).?);
    try testing.expectEqual(Key.eof, controlKey(4).?);
    try testing.expectEqual(Key.backspace, controlKey(8).?);
    try testing.expectEqual(Key.tab, controlKey(9).?);
    try testing.expectEqual(Key.enter, controlKey(10).?);
    try testing.expectEqual(Key.enter, controlKey(13).?);
    try testing.expectEqual(Key.kill_line, controlKey(11).?);
    try testing.expectEqual(Key.clear_screen, controlKey(12).?);
    try testing.expectEqual(Key.kill_word, controlKey(23).?);

    // Ctrl+letter
    try testing.expect(controlKey(1).?.ctrl == 'a');
    try testing.expect(controlKey(2).?.ctrl == 'b');
    try testing.expect(controlKey(5).?.ctrl == 'e');
    try testing.expect(controlKey(6).?.ctrl == 'f');

    // ESC returns null (needs further parsing)
    try testing.expect(controlKey(27) == null);

    // Unknown
    try testing.expectEqual(Key.unknown, controlKey(0).?);
    try testing.expectEqual(Key.unknown, controlKey(7).?);
}

test "parseMouseSequence: left click" {
    const result = parseMouseSequence("0;42;10M");
    try testing.expect(result.mouse.x == 42);
    try testing.expect(result.mouse.y == 10);
}

test "parseMouseSequence: release ignored" {
    const result = parseMouseSequence("0;42;10m");
    try testing.expectEqual(Key.unknown, result);
}

test "parseMouseSequence: non-left button ignored" {
    const result = parseMouseSequence("1;42;10M");
    try testing.expectEqual(Key.unknown, result);
}
