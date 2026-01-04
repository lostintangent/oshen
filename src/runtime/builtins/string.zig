//! string builtin - string manipulation (parallel to list.zig)
//!
//! Unified pipeline for all inputs (arguments, @varname, stdin):
//!   1. Parse arguments → Config
//!   2. Collect inputs (stdin reads into ArrayList, same as arguments)
//!   3. OutputWriter.init() → detects @varname binding
//!   4. processStrings() → resolve, transform, apply mode → collect results
//!   5. Output via OutputWriter → handles variable mutation vs stdout
//!
//! Composable transforms: --trim --upper --reverse all work together

const std = @import("std");
const builtins = @import("../builtins.zig");
const binding = @import("../binding.zig");
const Args = @import("../../terminal/args.zig");
const glob = @import("../../interpreter/expansion/glob.zig");

const io = builtins.io;
const BUF_SIZE = io.BUF_SIZE;

const isNumeric = Args.isNumeric;

pub const builtin = builtins.Builtin{
    .name = "string",
    .run = run,
    .help =
    \\string [FLAGS...] STRING|@VARNAME...
    \\
    \\Variable binding:
    \\  @VARNAME       Two-way binding: read from variable, write result back
    \\                 Use $VARNAME instead if you don't want mutation
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
    \\  string --upper @text                      -> (mutates text in-place)
    \\  string --upper $text                      -> (outputs to stdout, text unchanged)
    \\  string --split , @csv                     -> (mutates csv to list)
    \\  string --length @text                     -> (outputs length, no mutation)
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

// =============================================================================
// Entry Point
// =============================================================================

fn run(state: *builtins.State, args: []const []const u8) u8 {
    // 1. Parse flags into Config
    var p = Args.Args("string").init(args);
    var cfg = Config{};
    if (!parseArgs(&p, &cfg)) return 1;

    const positionals = p.rest();

    // 2. Collect inputs (handles @varname expansion, stdin)
    var inputs = std.ArrayListUnmanaged([]const u8){};
    defer inputs.deinit(state.allocator);

    binding.collectInputs(state, .{
        .args = positionals,
        .stdin_if_empty = (cfg.mode != .join),
    }, state.allocator, &inputs) catch |err| switch (err) {
        error.OutOfMemory => return oom(),
        error.VariableNotFound => return 1,
    };

    // 3. Create output writer with automatic binding detection
    var output = binding.OutputWriter.init(state, .{
        .args = positionals,
        .readonly = cfg.mode == .match or cfg.mode == .contains or cfg.tx.length,
    });

    // 4. Process and output
    return processStrings(state, &p, inputs.items, &cfg, &output);
}

// =============================================================================
// Argument Parsing
// =============================================================================

fn parseArgs(p: *Args.Args("string"), cfg: *Config) bool {
    while (p.next()) |arg| {
        // Stop at non-flag or end-of-flags marker
        if (arg.len == 0 or arg[0] != '-') {
            p.idx -= 1; // Rewind to include this arg in positionals
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
            cfg.arg2 = p.next() orelse {
                _ = p.err("--replace requires NEW");
                return false;
            };
            cfg.mode = .replace;
        } else if (p.option(null, "--sub")) |start| {
            cfg.arg1 = start;
            if (p.peek()) |next_arg| {
                if (isNumeric(next_arg)) cfg.arg2 = p.next().?;
            }
            cfg.mode = .sub;
        } else if (p.option(null, "--repeat")) |val| {
            cfg.arg1 = val;
            cfg.mode = .repeat;
        } else if (p.option(null, "--pad")) |val| {
            cfg.arg1 = val;
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
            _ = p.errArg("unknown flag: ", arg);
            return false;
        }
    }
    return true;
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

fn parseNumericArgs(p: *Args.Args("string"), cfg: *const Config) ?ParsedArgs {
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

/// Process already-resolved strings. Collects results, then outputs via OutputWriter.
fn processStrings(state: *builtins.State, p: *Args.Args("string"), strings: []const []const u8, cfg: *const Config, output: *binding.OutputWriter) u8 {
    const parsed = parseNumericArgs(p, cfg) orelse return p.fail();
    const allocator = state.allocator;

    // Match/contains: check all strings, return exit code (no output)
    if (cfg.mode == .match or cfg.mode == .contains) {
        var buf: [BUF_SIZE]u8 = undefined;
        for (strings) |s| {
            const result = transform(s, cfg.tx, &buf);
            const found = if (cfg.mode == .match)
                glob.globMatch(cfg.arg1, result.slice)
            else
                std.mem.indexOf(u8, result.slice, cfg.arg1) != null;
            if (found) return 0;
        }
        return 1;
    }

    // Length mode: output counts (like list --length)
    if (cfg.tx.length) {
        if (strings.len == 1) {
            var buf: [BUF_SIZE]u8 = undefined;
            const result = transform(strings[0], cfg.tx, &buf);
            output.writeInt(result.slice.len) catch return oom();
        } else {
            var results = std.ArrayListUnmanaged([]const u8){};
            defer results.deinit(allocator);
            var buf: [BUF_SIZE]u8 = undefined;
            var len_buf: [21]u8 = undefined;
            for (strings) |s| {
                const result = transform(s, cfg.tx, &buf);
                const len_str = std.fmt.bufPrint(&len_buf, "{d}", .{result.slice.len}) catch unreachable;
                results.append(allocator, allocator.dupe(u8, len_str) catch return oom()) catch return oom();
            }
            output.writeList(results.items) catch return oom();
        }
        return 0;
    }

    // Collect results (like list.zig's collectInput + applyTransforms)
    var results = std.ArrayListUnmanaged([]const u8){};
    defer results.deinit(allocator);

    var buf: [BUF_SIZE]u8 = undefined;

    for (strings) |str| {
        const result = transform(str, cfg.tx, &buf);
        if (result.truncated) warnTruncated();
        const s = result.slice;

        switch (cfg.mode) {
            .transform, .join => {
                // Transform: just collect; Join: collect then concatenate at end
                results.append(allocator, allocator.dupe(u8, s) catch return oom()) catch return oom();
            },
            .split => {
                if (cfg.arg1.len == 0) {
                    for (s) |c| {
                        results.append(allocator, allocator.dupe(u8, &[_]u8{c}) catch return oom()) catch return oom();
                    }
                } else {
                    var iter = std.mem.splitSequence(u8, s, cfg.arg1);
                    while (iter.next()) |part| {
                        results.append(allocator, allocator.dupe(u8, part) catch return oom()) catch return oom();
                    }
                }
            },
            .replace => {
                results.append(allocator, applyReplace(allocator, s, cfg.arg1, cfg.arg2) catch return oom()) catch return oom();
            },
            .sub => {
                results.append(allocator, allocator.dupe(u8, computeSub(s, parsed.start_val, parsed.len_val)) catch return oom()) catch return oom();
            },
            .repeat => {
                results.append(allocator, applyRepeat(allocator, s, parsed.count) catch return oom()) catch return oom();
            },
            .pad => {
                results.append(allocator, applyPad(allocator, s, parsed.width, cfg.arg2, cfg.pad_mode) catch return oom()) catch return oom();
            },
            .shorten => {
                results.append(allocator, applyShorten(allocator, s, parsed.max_len, parsed.ellipsis) catch return oom()) catch return oom();
            },
            .match, .contains => unreachable,
        }
    }

    // Output via OutputWriter (like list.zig's executeMode)
    switch (cfg.mode) {
        .split => output.writeList(results.items) catch return oom(),
        .join => {
            const joined = applyJoin(allocator, results.items, cfg.arg1) catch return oom();
            output.writeScalar(joined) catch return oom();
        },
        else => output.writeScalarOrList(results.items, " ") catch return oom(),
    }
    return 0;
}

fn oom() u8 {
    return builtins.reportOOM("string");
}

// =============================================================================
// Mode Operations (pure functions, no I/O)
// =============================================================================

fn computeSub(s: []const u8, start_val: i64, len_val: ?usize) []const u8 {
    const start: usize = if (start_val < 0)
        s.len -| @as(usize, @intCast(-start_val))
    else if (start_val == 0)
        0
    else
        @min(@as(usize, @intCast(start_val)) -| 1, s.len);
    const end = if (len_val) |l| @min(start + l, s.len) else s.len;
    return if (start < s.len) s[start..end] else "";
}

fn applyReplace(allocator: std.mem.Allocator, s: []const u8, old: []const u8, new: []const u8) ![]const u8 {
    if (old.len == 0) return try allocator.dupe(u8, s);
    var result = std.ArrayListUnmanaged(u8){};
    var pos: usize = 0;
    while (std.mem.indexOfPos(u8, s, pos, old)) |match| {
        try result.appendSlice(allocator, s[pos..match]);
        try result.appendSlice(allocator, new);
        pos = match + old.len;
    }
    try result.appendSlice(allocator, s[pos..]);
    return result.toOwnedSlice(allocator);
}

fn applyRepeat(allocator: std.mem.Allocator, s: []const u8, count: usize) ![]const u8 {
    if (count == 0 or s.len == 0) return "";
    const result = try allocator.alloc(u8, s.len * count);
    for (0..count) |i| @memcpy(result[i * s.len ..][0..s.len], s);
    return result;
}

fn applyPad(allocator: std.mem.Allocator, s: []const u8, width: usize, char_str: []const u8, mode: PadMode) ![]const u8 {
    if (s.len >= width) return try allocator.dupe(u8, s);
    const pad_char: u8 = if (char_str.len > 0) char_str[0] else ' ';
    const pad_total = width - s.len;
    const result = try allocator.alloc(u8, width);
    switch (mode) {
        .left => {
            @memset(result[0..pad_total], pad_char);
            @memcpy(result[pad_total..][0..s.len], s);
        },
        .right => {
            @memcpy(result[0..s.len], s);
            @memset(result[s.len..][0..pad_total], pad_char);
        },
        .center => {
            const left = pad_total / 2;
            @memset(result[0..left], pad_char);
            @memcpy(result[left..][0..s.len], s);
            @memset(result[left + s.len ..][0 .. pad_total - left], pad_char);
        },
    }
    return result;
}

fn applyShorten(allocator: std.mem.Allocator, s: []const u8, max_len: usize, ellipsis: []const u8) ![]const u8 {
    if (s.len <= max_len) return try allocator.dupe(u8, s);
    if (max_len <= ellipsis.len) return try allocator.dupe(u8, ellipsis[0..max_len]);
    const result = try allocator.alloc(u8, max_len);
    const text_len = max_len - ellipsis.len;
    @memcpy(result[0..text_len], s[0..text_len]);
    @memcpy(result[text_len..], ellipsis);
    return result;
}

fn applyJoin(allocator: std.mem.Allocator, items: []const []const u8, sep: []const u8) ![]const u8 {
    if (items.len == 0) return "";
    if (items.len == 1) return try allocator.dupe(u8, items[0]);
    var total: usize = 0;
    for (items) |item| total += item.len;
    total += sep.len * (items.len - 1);
    const result = try allocator.alloc(u8, total);
    var pos: usize = 0;
    for (items, 0..) |item, i| {
        if (i > 0) {
            @memcpy(result[pos..][0..sep.len], sep);
            pos += sep.len;
        }
        @memcpy(result[pos..][0..item.len], item);
        pos += item.len;
    }
    return result;
}

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
