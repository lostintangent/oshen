//! ANSI escape sequences and terminal color utilities.
//!
//! Provides color codes, styles, and escape sequence handling for terminal output.

// =============================================================================
// Escape Sequence Prefixes
// =============================================================================

const esc = "\x1b";
const csi = esc ++ "["; // Control Sequence Introducer
const osc = esc ++ "]"; // Operating System Command
const dec = csi ++ "?"; // DEC private mode

// =============================================================================
// Text Styles
// =============================================================================

pub const reset = csi ++ "0m";
pub const bold = csi ++ "1m";
pub const dim = csi ++ "2m";
pub const inverse = csi ++ "7m";

// =============================================================================
// Foreground Colors
// =============================================================================

pub const red = csi ++ "31m";
pub const green = csi ++ "32m";
pub const yellow = csi ++ "33m";
pub const blue = csi ++ "34m";
pub const magenta = csi ++ "35m";
pub const cyan = csi ++ "36m";
pub const gray = csi ++ "90m";
pub const bright_magenta = csi ++ "95m";

// =============================================================================
// Background Colors
// =============================================================================

pub const bg_dark_gray = csi ++ "100m";

// =============================================================================
// Cursor Control
// =============================================================================

pub const cursor_home = csi ++ "H";
pub const cursor_save = csi ++ "s";
pub const cursor_restore = csi ++ "u";
pub const cursor_hide = dec ++ "25l";
pub const cursor_show = dec ++ "25h";
pub const cursor_left = csi ++ "D";
pub const cursor_right = csi ++ "C";

// =============================================================================
// Screen/Line Clearing
// =============================================================================

pub const clear_line = "\r" ++ csi ++ "2K";
pub const clear_line_right = csi ++ "K";
pub const clear_below = csi ++ "J";
pub const clear_screen = csi ++ "2J" ++ cursor_home;
pub const clear_scrollback = csi ++ "3J";
pub const clear_all = clear_screen ++ clear_scrollback;

// =============================================================================
// Screen Buffers
// =============================================================================

pub const alt_screen_enter = dec ++ "1049h";
pub const alt_screen_exit = dec ++ "1049l";

// =============================================================================
// Mouse Tracking (SGR extended mode)
// =============================================================================

pub const mouse_on = dec ++ "1000h" ++ dec ++ "1006h";
pub const mouse_off = dec ++ "1000l" ++ dec ++ "1006l";

// =============================================================================
// Focus Reporting
// =============================================================================

pub const focus_on = dec ++ "1004h";
pub const focus_off = dec ++ "1004l";

// =============================================================================
// OSC Sequences
// =============================================================================

pub const osc_title_start = osc ++ "0;";
pub const osc_end = "\x07";

// =============================================================================
// Helpers
// =============================================================================

/// Calculate display length of a string, ignoring ANSI escape sequences.
pub fn displayLength(s: []const u8) usize {
    var len: usize = 0;
    var i: usize = 0;
    while (i < s.len) {
        if (i + 1 < s.len and s[i] == '\x1b' and s[i + 1] == '[') {
            // Skip CSI sequence: ESC [ params final_byte
            i += 2;
            while (i < s.len and s[i] >= 0x20 and s[i] < 0x40) : (i += 1) {}
            if (i < s.len) i += 1;
        } else {
            len += 1;
            i += 1;
        }
    }
    return len;
}

// =============================================================================
// Tests
// =============================================================================

const testing = @import("std").testing;

test "displayLength: plain text" {
    try testing.expectEqual(0, displayLength(""));
    try testing.expectEqual(5, displayLength("hello"));
    try testing.expectEqual(11, displayLength("hello world"));
}

test "displayLength: with CSI color codes" {
    // Green text
    try testing.expectEqual(5, displayLength(green ++ "hello" ++ reset));
    // Nested styles
    try testing.expectEqual(5, displayLength(bold ++ green ++ "hello" ++ reset));
    // Multiple colored words
    try testing.expectEqual(11, displayLength(red ++ "hello" ++ reset ++ " " ++ blue ++ "world" ++ reset));
}

test "displayLength: typical prompt" {
    // Simulates: green path + magenta branch + plain suffix
    // "~/code" (6) + " " (1) + "(main)" (6) + " $ " (3) = 16
    const prompt = green ++ "~/code" ++ reset ++ " " ++ bright_magenta ++ "(main)" ++ reset ++ " $ ";
    try testing.expectEqual(16, displayLength(prompt));
}
