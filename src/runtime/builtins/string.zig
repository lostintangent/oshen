//! string builtin - high-performance string manipulation
//!
//! Design principles:
//!   - Zero heap allocations (stack buffers only)
//!   - Minimal syscalls (buffered output with single flush)
//!   - Composable transforms (--trim --upper --reverse all work together)
//!   - Trailing newline for interactive usability
//!   - Stdin support (reads lines when no string arguments given)
//!
//! Performance characteristics:
//!   - O(n) for all operations where n = total input length
//!   - Single write syscall for outputs under 64KB
//!   - Graceful overflow handling for larger outputs

const std = @import("std");
const builtins = @import("../builtins.zig");
const Args = @import("../../terminal/args.zig").Args;
const glob = @import("../../interpreter/expansion/glob.zig");

const io = builtins.io;
const StdoutWriter = io.StdoutWriter;
const Writer = StdoutWriter; // Alias for brevity
const StdinReader = io.StdinReader;
const BUF_SIZE = io.BUF_SIZE;

pub const builtin = builtins.Builtin{
    .name = "string",
    .run = run,
    .help =
    \\string [FLAGS...] STRING...
    \\
    \\Transform flags (combinable):
    \\  --upper        Convert to uppercase
    \\  --lower        Convert to lowercase
    \\  --trim         Trim whitespace from both ends
    \\  --trim-left    Trim whitespace from start only
    \\  --trim-right   Trim whitespace from end only
    \\  --reverse      Reverse the string
    \\  --length       Output length instead of content
    \\  --escape       Escape special characters for shell
    \\  --unescape     Process escape sequences (\n, \t, etc.)
    \\
    \\Padding:
    \\  --pad N [C]         Right-pad to width N with char C (default: space)
    \\  --pad-left N [C]    Left-pad to width N with char C
    \\  --pad-center N [C]  Center-pad to width N with char C
    \\
    \\Truncation:
    \\  --shorten N [ELLIPSIS]  Truncate to N chars with ellipsis (default: ...)
    \\
    \\Operations:
    \\  --split SEP       Split by separator (one result per line)
    \\  --join SEP        Join arguments with separator
    \\  --replace OLD NEW Replace all occurrences
    \\  --sub START [LEN] Substring (1-indexed, negative from end)
    \\  --repeat N        Repeat N times
    \\  --match PATTERN   Exit 0 if glob matches, 1 otherwise (no output)
    \\  --contains STR    Exit 0 if substring found, 1 otherwise (no output)
    \\
    \\Stdin: When no STRING arguments given, reads lines from stdin.
    \\
    \\Examples:
    \\  string --upper hello                      -> HELLO
    \\  string --trim --upper "  hello  "         -> HELLO
    \\  string --split : "a:b:c"                  -> a\nb\nc
    \\  string --sub -3 "hello"                   -> llo
    \\  string --pad-left 8 0 "42"                -> 00000042
    \\  string --match "*.txt" "file.txt"         -> (exit 0)
    \\  echo hello | string --upper               -> HELLO
    ,
};

// =============================================================================
// Types
// =============================================================================

const Mode = enum { transform, split, join, replace, sub, repeat, pad, shorten, match, contains };
const PadMode = enum { left, right, center };

const Transforms = packed struct {
    upper: bool = false,
    lower: bool = false,
    trim: bool = false,
    trim_left: bool = false,
    trim_right: bool = false,
    reverse: bool = false,
    length: bool = false,
    escape: bool = false,
    unescape: bool = false,

    fn needsBuffer(self: Transforms) bool {
        return self.reverse or self.upper or self.lower or self.escape or self.unescape;
    }
};

/// Parsed command configuration
const Config = struct {
    tx: Transforms = .{},
    mode: Mode = .transform,
    arg1: []const u8 = "",
    arg2: []const u8 = "",
    pad_mode: PadMode = .right,
};

const isNumeric = @import("../../terminal/args.zig").isNumeric;

// =============================================================================
// Entry Point
// =============================================================================

fn run(_: *builtins.State, cmd: builtins.ExpandedCmd) u8 {
    var p = Args("string").init(cmd.argv);
    var cfg = Config{};

    while (p.next()) |arg| {
        // Stop at non-flag or end-of-flags marker
        if (arg.len == 0 or arg[0] != '-') {
            // Rewind to include this arg in strings
            p.idx -= 1;
            break;
        }
        if (p.isEndOfFlags()) break;

        // Transform flags (simple toggles)
        if (p.flag(null, "--upper")) {
            cfg.tx.upper = true;
        } else if (p.flag(null, "--lower")) {
            cfg.tx.lower = true;
        } else if (p.flag(null, "--trim")) {
            cfg.tx.trim = true;
        } else if (p.flag(null, "--trim-left")) {
            cfg.tx.trim_left = true;
        } else if (p.flag(null, "--trim-right")) {
            cfg.tx.trim_right = true;
        } else if (p.flag(null, "--reverse")) {
            cfg.tx.reverse = true;
        } else if (p.flag(null, "--length")) {
            cfg.tx.length = true;
        } else if (p.flag(null, "--escape")) {
            cfg.tx.escape = true;
        } else if (p.flag(null, "--unescape")) {
            cfg.tx.unescape = true;
        }
        // Mode flags with required values
        else if (p.option(null, "--split")) |val| {
            cfg.arg1 = val;
            cfg.mode = .split;
        } else if (p.option(null, "--join")) |val| {
            cfg.arg1 = val;
            cfg.mode = .join;
        } else if (p.option(null, "--replace")) |old| {
            cfg.arg1 = old;
            cfg.arg2 = p.next() orelse return p.err("--replace requires NEW");
            cfg.mode = .replace;
        } else if (p.option(null, "--sub")) |start| {
            cfg.arg1 = start;
            // Optional length (peek for numeric arg)
            if (p.peek()) |next_arg| {
                if (isNumeric(next_arg)) cfg.arg2 = p.next().?;
            }
            cfg.mode = .sub;
        } else if (p.option(null, "--repeat")) |val| {
            cfg.arg1 = val;
            cfg.mode = .repeat;
        } else if (p.option(null, "--pad")) |val| {
            cfg.arg1 = val;
            // Optional pad char
            if (p.peek()) |next_arg| {
                if (next_arg.len == 1 and next_arg[0] != '-') cfg.arg2 = p.next().?;
            }
            cfg.mode = .pad;
        } else if (p.option(null, "--pad-left")) |val| {
            cfg.pad_mode = .left;
            cfg.arg1 = val;
            if (p.peek()) |next_arg| {
                if (next_arg.len == 1 and next_arg[0] != '-') cfg.arg2 = p.next().?;
            }
            cfg.mode = .pad;
        } else if (p.option(null, "--pad-center")) |val| {
            cfg.pad_mode = .center;
            cfg.arg1 = val;
            if (p.peek()) |next_arg| {
                if (next_arg.len == 1 and next_arg[0] != '-') cfg.arg2 = p.next().?;
            }
            cfg.mode = .pad;
        } else if (p.option(null, "--shorten")) |val| {
            cfg.arg1 = val;
            // Optional ellipsis
            if (p.peek()) |next_arg| {
                if (next_arg.len > 0 and next_arg[0] != '-') cfg.arg2 = p.next().?;
            }
            cfg.mode = .shorten;
        } else if (p.option(null, "--match")) |val| {
            cfg.arg1 = val;
            cfg.mode = .match;
        } else if (p.option(null, "--contains")) |val| {
            cfg.arg1 = val;
            cfg.mode = .contains;
        } else {
            return p.errArg("unknown flag: ", arg);
        }
    }

    const strings = p.rest();

    // If no string arguments, read from stdin (except --join which can join zero args)
    if (strings.len == 0 and cfg.mode != .join) {
        return processStdin(&p, &cfg);
    }

    return processStrings(&p, strings, &cfg);
}

// =============================================================================
// String Processing
// =============================================================================

/// Pre-parsed numeric arguments for modes that need them.
const ParsedArgs = struct {
    start_val: i64 = 0,
    len_val: ?usize = null,
    count: usize = 0,
    width: usize = 0,
    max_len: usize = 0,
    ellipsis: []const u8 = "...",
};

fn parseNumericArgs(p: *Args("string"), cfg: *const Config) ?ParsedArgs {
    var result = ParsedArgs{};
    switch (cfg.mode) {
        .sub => {
            result.start_val = p.asI64(cfg.arg1) orelse return null;
            if (cfg.arg2.len > 0) result.len_val = p.asUsize(cfg.arg2) orelse return null;
        },
        .repeat => result.count = p.asUsizeFor(cfg.arg1, "--repeat N") orelse return null,
        .pad => result.width = p.asUsizeFor(cfg.arg1, "--pad WIDTH") orelse return null,
        .shorten => {
            result.max_len = p.asUsizeFor(cfg.arg1, "--shorten N") orelse return null;
            if (cfg.arg2.len > 0) result.ellipsis = cfg.arg2;
        },
        else => {},
    }
    return result;
}

/// Process string arguments (space-separated output).
fn processStrings(p: *Args("string"), strings: []const []const u8, cfg: *const Config) u8 {
    const parsed = parseNumericArgs(p, cfg) orelse return p.fail();

    // Match/contains: check all strings, return exit code
    if (cfg.mode == .match or cfg.mode == .contains) {
        var buf: [BUF_SIZE]u8 = undefined;
        for (strings) |str| {
            const result = transform(str, cfg.tx, &buf);
            const found = if (cfg.mode == .match)
                glob.globMatch(cfg.arg1, result.slice)
            else
                std.mem.indexOf(u8, result.slice, cfg.arg1) != null;
            if (found) return 0;
        }
        return 1;
    }

    var ctx = OutputContext.init(cfg, &parsed, ' ');
    var buf: [BUF_SIZE]u8 = undefined;
    for (strings) |str| {
        const result = transform(str, cfg.tx, &buf);
        if (result.truncated) warnTruncated();
        ctx.process(result.slice);
    }
    ctx.flush();
    return 0;
}

/// Process stdin (newline-separated output).
fn processStdin(p: *Args("string"), cfg: *const Config) u8 {
    const parsed = parseNumericArgs(p, cfg) orelse return p.fail();

    var reader = StdinReader.init();
    var line_buf: [BUF_SIZE]u8 = undefined;
    var transform_buf: [BUF_SIZE]u8 = undefined;
    var any_matched = false;

    // Match/contains: scan all lines, track if any matched
    if (cfg.mode == .match or cfg.mode == .contains) {
        while (reader.readLine(&line_buf)) |line| {
            const result = transform(line, cfg.tx, &transform_buf);
            const found = if (cfg.mode == .match)
                glob.globMatch(cfg.arg1, result.slice)
            else
                std.mem.indexOf(u8, result.slice, cfg.arg1) != null;
            if (found) any_matched = true;
        }
        return @intFromBool(!any_matched);
    }

    var ctx = OutputContext.init(cfg, &parsed, '\n');
    while (reader.readLine(&line_buf)) |line| {
        const result = transform(line, cfg.tx, &transform_buf);
        if (result.truncated) warnTruncated();
        ctx.process(result.slice);
    }
    ctx.flush();
    return 0;
}

/// Unified output context - handles all modes with configurable separator.
const OutputContext = struct {
    w: Writer,
    cfg: *const Config,
    parsed: *const ParsedArgs,
    sep: u8,
    first: bool,

    fn init(cfg: *const Config, parsed: *const ParsedArgs, sep: u8) OutputContext {
        return .{ .w = Writer{}, .cfg = cfg, .parsed = parsed, .sep = sep, .first = true };
    }

    fn process(self: *OutputContext, s: []const u8) void {
        switch (self.cfg.mode) {
            .transform => {
                if (self.cfg.tx.length) {
                    self.writeSep('\n'); // --length always newline-separated
                    self.w.writeInt(s.len);
                } else {
                    self.writeSep(self.sep);
                    self.w.write(s);
                }
            },
            .split => {
                if (self.cfg.arg1.len == 0) {
                    for (s) |c| {
                        self.writeSep('\n');
                        self.w.writeByte(c);
                    }
                } else {
                    var iter = std.mem.splitSequence(u8, s, self.cfg.arg1);
                    while (iter.next()) |part| {
                        self.writeSep('\n');
                        self.w.write(part);
                    }
                }
            },
            .join => {
                if (!self.first) self.w.write(self.cfg.arg1);
                self.first = false;
                self.w.write(s);
            },
            .replace => {
                self.writeSep(self.sep);
                writeReplaced(&self.w, s, self.cfg.arg1, self.cfg.arg2);
            },
            .sub => {
                self.writeSep(self.sep);
                writeSub(&self.w, s, self.parsed.start_val, self.parsed.len_val);
            },
            .repeat => {
                self.writeSep(self.sep);
                for (0..self.parsed.count) |_| self.w.write(s);
            },
            .pad => {
                self.writeSep(self.sep);
                writePadded(&self.w, s, self.parsed.width, self.cfg.arg2, self.cfg.pad_mode);
            },
            .shorten => {
                self.writeSep(self.sep);
                writeShortened(&self.w, s, self.parsed.max_len, self.parsed.ellipsis);
            },
            .match, .contains => {}, // Handled separately
        }
    }

    inline fn writeSep(self: *OutputContext, sep: u8) void {
        if (!self.first) self.w.writeByte(sep);
        self.first = false;
    }

    fn flush(self: *OutputContext) void {
        if (!self.first) self.w.writeByte('\n');
        self.w.flush();
    }
};

// =============================================================================
// Transform Engine
// =============================================================================

/// Result of transform operation
const TransformResult = struct {
    slice: []const u8,
    truncated: bool = false,
};

/// Apply transforms, returning the result slice.
/// Order: trim -> reverse -> case -> escape/unescape
fn transform(input: []const u8, tx: Transforms, buf: *[BUF_SIZE]u8) TransformResult {
    var s = input;

    // Trim operations (narrow the slice, no copy needed)
    if (tx.trim) {
        s = std.mem.trim(u8, s, " \t\n\r");
    } else {
        if (tx.trim_left) s = std.mem.trimLeft(u8, s, " \t\n\r");
        if (tx.trim_right) s = std.mem.trimRight(u8, s, " \t\n\r");
    }

    if (s.len == 0) return .{ .slice = s };
    if (!tx.needsBuffer()) return .{ .slice = s };

    // Escape mode: encode special chars (can expand)
    if (tx.escape) return escapeString(s, buf);

    // Unescape mode: decode escape sequences (can shrink)
    if (tx.unescape) return unescapeString(s, buf);

    // Buffer too small - return as-is with truncation flag
    if (s.len > BUF_SIZE) return .{ .slice = s, .truncated = true };

    // Single-pass: reverse (optional) + case transform (optional)
    const len = s.len;
    if (tx.reverse) {
        for (0..len) |i| buf[i] = transformChar(s[len - 1 - i], tx);
    } else {
        for (s, 0..) |c, i| buf[i] = transformChar(c, tx);
    }
    return .{ .slice = buf[0..len] };
}

inline fn transformChar(c: u8, tx: Transforms) u8 {
    return if (tx.upper) std.ascii.toUpper(c) else if (tx.lower) std.ascii.toLower(c) else c;
}

/// Emit a warning (once) when string exceeds buffer size for transform operations.
/// Uses a static flag to avoid spamming warnings for multiple large strings.
var truncation_warned: bool = false;
fn warnTruncated() void {
    if (!truncation_warned) {
        truncation_warned = true;
        io.writeStderr("string: warning: string exceeds 64KB buffer, transform skipped\n");
    }
}

/// Characters that need escaping for safe shell usage.
/// These are escaped with a backslash by --escape:
///   - Whitespace: space, tab, newline, carriage return
///   - Quotes: " '
///   - Shell metacharacters: $ ` ! | & ; < > ( ) { } [ ]
///   - Glob characters: * ? [ ]
///   - Other special: \ # ~
const SHELL_SPECIAL_CHARS = " \t\n\r\\\"'$`!*?[](){}|&;<>#~";

fn escapeString(s: []const u8, buf: *[BUF_SIZE]u8) TransformResult {
    var pos: usize = 0;
    for (s) |c| {
        const needs_escape = std.mem.indexOfScalar(u8, SHELL_SPECIAL_CHARS, c) != null;
        if (needs_escape) {
            if (pos + 2 > BUF_SIZE) return .{ .slice = s, .truncated = true };
            buf[pos] = '\\';
            buf[pos + 1] = c;
            pos += 2;
        } else {
            if (pos >= BUF_SIZE) return .{ .slice = s, .truncated = true };
            buf[pos] = c;
            pos += 1;
        }
    }
    return .{ .slice = buf[0..pos] };
}

fn unescapeString(s: []const u8, buf: *[BUF_SIZE]u8) TransformResult {
    var pos: usize = 0;
    var i: usize = 0;
    while (i < s.len) {
        if (pos >= BUF_SIZE) return .{ .slice = s, .truncated = true };
        if (s[i] == '\\' and i + 1 < s.len) {
            const replacement: ?u8 = switch (s[i + 1]) {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '0' => 0,
                'e' => '\x1b',
                else => null,
            };
            if (replacement) |c| {
                buf[pos] = c;
                pos += 1;
                i += 2;
            } else {
                buf[pos] = s[i];
                pos += 1;
                i += 1;
            }
        } else {
            buf[pos] = s[i];
            pos += 1;
            i += 1;
        }
    }
    return .{ .slice = buf[0..pos] };
}

// =============================================================================
// Output Helpers
// =============================================================================
//
// These functions write transformed output directly to a Writer, avoiding
// intermediate allocations. Each handles a specific --mode operation.

/// Write string with all occurrences of `old` replaced by `new`.
/// If `old` is empty, writes the string unchanged.
fn writeReplaced(w: *Writer, s: []const u8, old: []const u8, new: []const u8) void {
    if (old.len == 0) {
        w.write(s);
        return;
    }
    var pos: usize = 0;
    while (std.mem.indexOfPos(u8, s, pos, old)) |match| {
        w.write(s[pos..match]);
        w.write(new);
        pos = match + old.len;
    }
    w.write(s[pos..]);
}

/// Write a substring of `s`.
/// - `start_val`: 1-based index (positive) or from-end (negative). 0 is treated as 1.
/// - `len_val`: optional length limit; if null, writes to end of string.
fn writeSub(w: *Writer, s: []const u8, start_val: i64, len_val: ?usize) void {
    const start: usize = if (start_val < 0)
        s.len -| @as(usize, @intCast(-start_val))
    else if (start_val == 0)
        0
    else
        @min(@as(usize, @intCast(start_val)) -| 1, s.len);

    const end = if (len_val) |l| @min(start + l, s.len) else s.len;
    if (start < s.len) w.write(s[start..end]);
}

/// Write string padded to `width` with pad character (default: space).
/// If string is already >= width, writes unchanged.
fn writePadded(w: *Writer, s: []const u8, width: usize, char_str: []const u8, mode: PadMode) void {
    const pad_char: u8 = if (char_str.len > 0) char_str[0] else ' ';
    if (s.len >= width) {
        w.write(s);
        return;
    }
    const pad_total = width - s.len;
    switch (mode) {
        .left => {
            w.writeByteN(pad_char, pad_total);
            w.write(s);
        },
        .right => {
            w.write(s);
            w.writeByteN(pad_char, pad_total);
        },
        .center => {
            w.writeByteN(pad_char, pad_total / 2);
            w.write(s);
            w.writeByteN(pad_char, pad_total - pad_total / 2);
        },
    }
}

/// Write string truncated to `max_len`, appending ellipsis if truncated.
/// If string fits, writes unchanged. If max_len <= ellipsis.len, truncates ellipsis.
fn writeShortened(w: *Writer, s: []const u8, max_len: usize, ellipsis: []const u8) void {
    if (s.len <= max_len) {
        w.write(s);
    } else if (max_len <= ellipsis.len) {
        w.write(ellipsis[0..max_len]);
    } else {
        w.write(s[0 .. max_len - ellipsis.len]);
        w.write(ellipsis);
    }
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

/// Test context encapsulating buffer allocation and common assertions.
const TestContext = struct {
    buf: [BUF_SIZE]u8 = undefined,

    fn transform(self: *TestContext, input: []const u8, tx: Transforms) []const u8 {
        return string.transform(input, tx, &self.buf).slice;
    }
};

// For accessing the transform function in tests
const string = @This();

// -----------------------------------------------------------------------------
// String: transform operations
// -----------------------------------------------------------------------------

test "String: individual transforms" {
    var ctx = TestContext{};
    const cases = .{
        // uppercase
        .{ "hello", Transforms{ .upper = true }, "HELLO" },
        .{ "hello world", Transforms{ .upper = true }, "HELLO WORLD" },
        .{ "123", Transforms{ .upper = true }, "123" }, // numbers unchanged
        // lowercase
        .{ "HELLO", Transforms{ .lower = true }, "hello" },
        .{ "HELLO WORLD", Transforms{ .lower = true }, "hello world" },
        // reverse
        .{ "hello", Transforms{ .reverse = true }, "olleh" },
        .{ "", Transforms{ .reverse = true }, "" },
        .{ "a", Transforms{ .reverse = true }, "a" },
        // trim
        .{ "  hello  ", Transforms{ .trim = true }, "hello" },
        .{ "\t\nhello\r\n", Transforms{ .trim = true }, "hello" },
        .{ "   ", Transforms{ .trim = true }, "" },
        // trim-left
        .{ "  hello  ", Transforms{ .trim_left = true }, "hello  " },
        .{ "\nhello\t", Transforms{ .trim_left = true }, "hello\t" },
        // trim-right
        .{ "  hello  ", Transforms{ .trim_right = true }, "  hello" },
        .{ "\thello\n", Transforms{ .trim_right = true }, "\thello" },
    };
    inline for (cases) |case| {
        try testing.expectEqualStrings(case[2], ctx.transform(case[0], case[1]));
    }
}

test "String: transform composition" {
    var ctx = TestContext{};
    const cases = .{
        .{ "  hello  ", Transforms{ .trim = true, .upper = true }, "HELLO" },
        .{ "hello", Transforms{ .reverse = true, .upper = true }, "OLLEH" },
        .{ "  hello  ", Transforms{ .trim = true, .reverse = true, .upper = true }, "OLLEH" },
        .{ "  HELLO  ", Transforms{ .trim = true, .reverse = true, .lower = true }, "olleh" },
    };
    inline for (cases) |case| {
        try testing.expectEqualStrings(case[2], ctx.transform(case[0], case[1]));
    }
}

// -----------------------------------------------------------------------------
// String: escape and unescape
// -----------------------------------------------------------------------------

test "String: escapeString" {
    var buf: [BUF_SIZE]u8 = undefined;
    const cases = .{
        // spaces
        .{ "hello world", "hello\\ world" },
        // shell metacharacters
        .{ "$HOME", "\\$HOME" },
        .{ "foo|bar", "foo\\|bar" },
        .{ "a&&b", "a\\&\\&b" },
        .{ "cmd;cmd2", "cmd\\;cmd2" },
        // quotes
        .{ "say 'hi'", "say\\ \\'hi\\'" },
        .{ "say \"hi\"", "say\\ \\\"hi\\\"" },
        // glob characters
        .{ "*.txt", "\\*.txt" },
        .{ "file?", "file\\?" },
        .{ "[abc]", "\\[abc\\]" },
        // no escaping needed
        .{ "file.txt", "file.txt" },
        .{ "hello", "hello" },
    };
    inline for (cases) |case| {
        try testing.expectEqualStrings(case[1], escapeString(case[0], &buf).slice);
    }
}

test "String: unescapeString" {
    var buf: [BUF_SIZE]u8 = undefined;
    const cases = .{
        // common escapes
        .{ "hello\\nworld", "hello\nworld" },
        .{ "hello\\tworld", "hello\tworld" },
        .{ "hello\\rworld", "hello\rworld" },
        // backslash
        .{ "hello\\\\world", "hello\\world" },
        // null and escape
        .{ "hello\\0world", "hello\x00world" },
        .{ "hello\\eworld", "hello\x1bworld" },
        // unknown escapes preserved
        .{ "hello\\xworld", "hello\\xworld" },
    };
    inline for (cases) |case| {
        try testing.expectEqualStrings(case[1], unescapeString(case[0], &buf).slice);
    }
}

// -----------------------------------------------------------------------------
// Output Helpers
// -----------------------------------------------------------------------------
// Note: writeReplaced, writeSub, writePadded, and writeShortened are tested
// indirectly through the full command integration. Their internal buffer
// access is not exposed for direct testing from this module.

// -----------------------------------------------------------------------------
// String: edge cases
// -----------------------------------------------------------------------------

test "String: edge cases" {
    var ctx = TestContext{};
    var buf: [BUF_SIZE]u8 = undefined;

    // transform: empty string
    try testing.expectEqualStrings("", ctx.transform("", .{ .upper = true }));
    try testing.expectEqualStrings("", ctx.transform("", .{ .reverse = true }));
    try testing.expectEqualStrings("", ctx.transform("", .{ .trim = true }));

    // transform: no transforms applied
    try testing.expectEqualStrings("hello", ctx.transform("hello", .{}));

    // escape/unescape: empty string
    try testing.expectEqualStrings("", escapeString("", &buf).slice);
    try testing.expectEqualStrings("", unescapeString("", &buf).slice);

    // unescape: trailing backslash
    try testing.expectEqualStrings("hello\\", unescapeString("hello\\", &buf).slice);
}
