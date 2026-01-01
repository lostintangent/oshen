//! Word expansion: transforms shell words into executable arguments.
//!
//! This module implements the second parsing layer that interprets expansion
//! syntax within words. While the structural parser handles command grammar,
//! this expander interprets the content of each word segment:
//!
//! - Variables: `$var`, `${var}`, `$var[1]`, `$1`, `$#`, `$*`, `$?`
//! - Command substitution: `$(cmd)`
//! - Tilde expansion: `~` → home directory
//! - Glob patterns: `*.txt`, `**/*.zig`, `[a-z].md`
//! - Brace expansion: `{*.txt}_backup`, `{a,b,c}`, `{$var}_suffix`
//! - Escape sequences: `\n`, `\t`, `\$`
//!
//! Expansion respects quoting context:
//! - Single quotes (`'...'`): No expansion, literal text
//! - Double quotes (`"..."`): Variables and escapes expand, globs don't
//! - Bare words: Full expansion including globs
//!
//! List variables expand to multiple arguments (no word-splitting surprises).
//! For example, if `$xs` is `["a", "b"]`, then `echo $xs` becomes `echo a b`.
//!
//! Brace expansion enables Cartesian products with explicit syntax:
//! - `{*.txt}_backup` → globs `*.txt`, then appends `_backup` to each match
//! - `{a,b,c}_suffix` → expands to `a_suffix b_suffix c_suffix`
//! - `{$items}_test` → if `$items` is `[x, y]`, produces `x_test y_test`
//! - `{a,b}_{1,2}` → nested expansion produces `a_1 a_2 b_1 b_2`

const std = @import("std");
const token_types = @import("../../language/tokens.zig");
const State = @import("../../runtime/state.zig").State;
const interpreter = @import("../interpreter.zig");
const glob = @import("glob.zig");
const WordPart = token_types.WordPart;

// Re-export glob utilities for external use
pub const globMatch = glob.globMatch;
pub const hasGlobChars = glob.hasGlobChars;

// Re-use shared helper from glob module
const singletonSlice = glob.singletonSlice;

pub const ExpandError = error{
    UnterminatedVariable,
    InvalidVariableName,
    EmptyCommandSubstitution,
    CommandSubstitutionFailed,
};

// =============================================================================
// Ownership Convention
// =============================================================================
//
// All slices returned by functions in this module (expandWord, expandWords, etc.)
// are allocated using the ExpandContext's allocator. Callers should:
//
// 1. Use an ArenaAllocator for the ExpandContext (recommended)
// 2. Free the arena after the expanded values are no longer needed
// 3. NOT manually free individual returned slices - the arena handles cleanup
//
// The typical pattern is:
//   var arena = std.heap.ArenaAllocator.init(allocator);
//   defer arena.deinit();
//   var ctx = ExpandContext.init(arena.allocator(), state);
//   const expanded = try expandWord(&ctx, parts);  // Valid until arena.deinit()
// =============================================================================

pub const ExpandContext = struct {
    allocator: std.mem.Allocator,
    state: *State,
    mock_cmdsub: ?std.StringHashMap([]const u8),
    mock_glob: ?std.StringHashMap([]const []const u8),

    pub fn init(allocator: std.mem.Allocator, state: *State) ExpandContext {
        return .{
            .allocator = allocator,
            .state = state,
            .mock_cmdsub = null,
            .mock_glob = null,
        };
    }

    pub fn deinit(self: *ExpandContext) void {
        if (self.mock_cmdsub) |*m| m.deinit();
        if (self.mock_glob) |*m| m.deinit();
    }

    /// Get variable value from shell state or environment (as a list)
    pub fn getVar(self: *ExpandContext, name: []const u8) ?[]const []const u8 {
        // Special variables
        if (std.mem.eql(u8, name, "?") or std.mem.eql(u8, name, "status")) {
            // Status is handled specially - return as single-element list
            return null; // Let caller handle status formatting
        }

        // Check shell vars using scope chain
        if (self.state.getVarList(name)) |list| {
            return list;
        }

        // For scalars (returned by getVar but not getVarList), wrap in a slice
        if (self.state.getVar(name)) |value| {
            const slice = self.allocator.alloc([]const u8, 1) catch return null;
            slice[0] = value;
            return slice;
        }

        // Fall back to environment - wrap single value in slice
        // Note: For env vars, we return null and let expandText handle it
        return null;
    }

    /// Get home directory from state
    pub fn getHome(self: *ExpandContext) []const u8 {
        return self.state.home orelse "/home/user";
    }

    /// Get current status as string
    pub fn getStatus(self: *ExpandContext) u8 {
        return self.state.status;
    }

    pub fn setMockCmdsub(self: *ExpandContext, cmd: []const u8, output: []const u8) !void {
        if (self.mock_cmdsub == null) {
            self.mock_cmdsub = std.StringHashMap([]const u8).init(self.allocator);
        }
        try self.mock_cmdsub.?.put(cmd, output);
    }

    pub fn setMockGlob(self: *ExpandContext, pattern: []const u8, matches: []const []const u8) !void {
        if (self.mock_glob == null) {
            self.mock_glob = std.StringHashMap([]const []const u8).init(self.allocator);
        }
        try self.mock_glob.?.put(pattern, matches);
    }

    /// Set a variable (delegates to state, used by tests)
    pub fn setVar(self: *ExpandContext, name: []const u8, values: []const []const u8) !void {
        try self.state.setVarList(name, values);
    }
};

/// Expands a complete word token (slice of WordParts) into a list of strings.
/// Each WordPart is expanded according to its quote type, and results are
/// combined via cartesian product to handle multi-value expansions.
///
/// Example: `hello_$(echo "a b")_world` with parts ["hello_", cmd("echo a b"), "_world"]
/// where cmd outputs "a\nb" produces ["hello_a_world", "hello_b_world"]
pub fn expandWord(ctx: *ExpandContext, parts: []const WordPart) (ExpandError || std.mem.Allocator.Error)![]const []const u8 {
    var results: std.ArrayListUnmanaged([]const u8) = .empty;
    try results.append(ctx.allocator, "");

    for (parts) |part| {
        const expanded = try expandWordPart(ctx, part);
        results = try cartesian(ctx.allocator, results.items, expanded);
    }

    return try results.toOwnedSlice(ctx.allocator);
}

/// Expands a single WordPart based on its quote type.
/// - `.single`: literal text, no expansion
/// - `.double`: variable and escape expansion only
/// - `.none`: full expansion (variables, escapes, globs)
/// - `.command`: execute command and split output by newlines
/// - `.brace`: brace expansion pattern (comma list, range, or nested content)
fn expandWordPart(ctx: *ExpandContext, part: WordPart) (ExpandError || std.mem.Allocator.Error)![]const []const u8 {
    switch (part.quotes) {
        .single => return singletonSlice(ctx.allocator, part.text),
        .double => return expandText(ctx, part.text, .{ .expand_glob = false }),
        .none => return expandText(ctx, part.text, .{ .expand_glob = true }),
        .command => {
            const output = try runCmdsub(ctx, part.text);
            var lines: std.ArrayListUnmanaged([]const u8) = .empty;
            var iter = std.mem.splitScalar(u8, output, '\n');
            while (iter.next()) |line| {
                try lines.append(ctx.allocator, line);
            }
            if (lines.items.len == 0) {
                try lines.append(ctx.allocator, "");
            }
            return try lines.toOwnedSlice(ctx.allocator);
        },
        .brace => return expandBracePattern(ctx, part.text),
    }
}

// =============================================================================
// Brace Expansion
// =============================================================================

/// Expands a brace pattern into a list of strings.
/// Handles: ranges (`1..5`, `1..$n`), comma lists (`a,b,c`), globs (`*.txt`), variables (`$var`)
fn expandBracePattern(ctx: *ExpandContext, pattern: []const u8) (ExpandError || std.mem.Allocator.Error)![]const []const u8 {
    // Try numeric range: 1..5, 5..1, or with variables like 1..$n
    if (try expandNumericRange(ctx, pattern)) |result| {
        return result;
    }

    // Try comma-separated list: a,b,c
    if (std.mem.indexOfScalar(u8, pattern, ',') != null) {
        return expandCommaList(ctx.allocator, pattern);
    }

    // Fallback: expand as normal text (globs, variables, etc.)
    return expandText(ctx, pattern, .{ .expand_glob = true });
}

/// Attempts to expand a numeric range pattern like `1..5` or `1..$n`.
/// Returns null if the pattern is not a valid numeric range.
fn expandNumericRange(ctx: *ExpandContext, pattern: []const u8) !?[]const []const u8 {
    // Resolves variables in a range bound, taking first value for lists.
    const expandBound = struct {
        fn call(c: *ExpandContext, bound: []const u8) ![]const u8 {
            if (std.mem.indexOfScalar(u8, bound, '$') == null) return bound;
            const expanded = try expandText(c, bound, .{});
            return if (expanded.len > 0) expanded[0] else bound;
        }
    }.call;

    const dot_pos = std.mem.indexOf(u8, pattern, "..") orelse return null;
    const start_str = pattern[0..dot_pos];
    const end_str = pattern[dot_pos + 2 ..];

    if (start_str.len == 0 or end_str.len == 0) return null;

    const start_val = try expandBound(ctx, start_str);
    const end_val = try expandBound(ctx, end_str);

    const start = std.fmt.parseInt(i64, start_val, 10) catch return null;
    const end = std.fmt.parseInt(i64, end_val, 10) catch return null;

    return try generateRange(ctx.allocator, start, end);
}

/// Generates a sequence of numbers from start to end (inclusive).
fn generateRange(allocator: std.mem.Allocator, start: i64, end: i64) ![]const []const u8 {
    var items = std.ArrayListUnmanaged([]const u8){};
    errdefer items.deinit(allocator);

    const step: i64 = if (start <= end) 1 else -1;
    var i = start;
    while (true) : (i += step) {
        var buf: [21]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{i}) catch break;
        try items.append(allocator, try allocator.dupe(u8, s));
        if (i == end) break;
    }

    return items.toOwnedSlice(allocator);
}

/// Expands a comma-separated list: `a,b,c` → `["a", "b", "c"]`
fn expandCommaList(allocator: std.mem.Allocator, pattern: []const u8) ![]const []const u8 {
    var items = std.ArrayListUnmanaged([]const u8){};
    errdefer items.deinit(allocator);

    var iter = std.mem.splitScalar(u8, pattern, ',');
    while (iter.next()) |item| {
        const trimmed = std.mem.trim(u8, item, " \t");
        if (trimmed.len > 0) {
            try items.append(allocator, trimmed);
        }
    }

    return try items.toOwnedSlice(allocator);
}

const ExpandOptions = struct {
    expand_vars: bool = true,
    expand_escapes: bool = true,
    expand_glob: bool = false,
};

fn appendLiteral(allocator: std.mem.Allocator, current: []const []const u8, literal: []const u8) !std.ArrayListUnmanaged([]const u8) {
    var next: std.ArrayListUnmanaged([]const u8) = .empty;
    for (current) |s| {
        const combined = try std.fmt.allocPrint(allocator, "{s}{s}", .{ s, literal });
        try next.append(allocator, combined);
    }
    return next;
}

/// Low-level text expansion: processes raw text for variables ($var), escapes (\n),
/// tilde (~), and optionally globs (*). Called by expandWordPart for .none and .double parts.
/// Returns a list of strings (multiple values when variables expand to lists or globs match multiple files).
fn expandText(ctx: *ExpandContext, text: []const u8, opts: ExpandOptions) (ExpandError || std.mem.Allocator.Error)![]const []const u8 {
    // Handle tilde expansion for bare words at the start
    if (opts.expand_glob and text.len > 0 and text[0] == '~') {
        if (text.len == 1 or text[1] == '/') {
            const rest = if (text.len > 1) text[1..] else "";
            const expanded = try std.fmt.allocPrint(ctx.allocator, "{s}{s}", .{ ctx.getHome(), rest });
            return singletonSlice(ctx.allocator, expanded);
        }
    }

    var results: std.ArrayListUnmanaged([]const u8) = .empty;
    try results.append(ctx.allocator, "");

    var i: usize = 0;
    var literal_start: usize = 0;

    while (i < text.len) {
        const is_var = text[i] == '$' and opts.expand_vars;
        const is_escape = text[i] == '\\' and i + 1 < text.len and opts.expand_escapes;

        if (is_var or is_escape) {
            // Flush literal if any
            if (i > literal_start) {
                const chunk = text[literal_start..i];
                const next_results = try appendLiteral(ctx.allocator, results.items, chunk);
                results = next_results;
            }

            if (is_var) {
                const var_result = try parseAndExpandVar(ctx, text, i);
                results = try cartesian(ctx.allocator, results.items, var_result.values);
                i = var_result.new_pos;
            } else {
                // is_escape
                const ch = text[i + 1];
                const escaped = switch (ch) {
                    'n' => "\n",
                    't' => "\t",
                    'e' => "\x1b", // ESC character for ANSI codes
                    '\\' => "\\",
                    '"' => "\"",
                    '$' => "$",
                    else => blk: {
                        // Strip backslash: \* → *, \( → (, etc.
                        const s = try ctx.allocator.alloc(u8, 1);
                        s[0] = ch;
                        break :blk s;
                    },
                };
                const escaped_list = try ctx.allocator.alloc([]const u8, 1);
                escaped_list[0] = escaped;
                results = try cartesian(ctx.allocator, results.items, escaped_list);
                i += 2;
            }
            literal_start = i;
        } else {
            i += 1;
        }
    }

    // Flush remaining literal
    if (i > literal_start) {
        const chunk = text[literal_start..i];
        const next_results = try appendLiteral(ctx.allocator, results.items, chunk);
        results = next_results;
    }

    const expanded_items = try results.toOwnedSlice(ctx.allocator);

    // Handle glob expansion for bare words
    // Check for unescaped glob chars in ORIGINAL text, not the expanded result
    if (opts.expand_glob and expanded_items.len == 1) {
        if (glob.hasGlobChars(text)) {
            const pattern = expanded_items[0];
            return try glob.expandGlob(ctx.allocator, pattern, ctx.mock_glob);
        }
    }

    return expanded_items;
}

/// Return type for variable expansion functions
const VarExpansionResult = struct { values: []const []const u8, new_pos: usize };

fn parseAndExpandVar(ctx: *ExpandContext, text: []const u8, start: usize) (ExpandError || std.mem.Allocator.Error)!VarExpansionResult {
    var i = start + 1;

    if (i < text.len and text[i] == '{') {
        i += 1;
        const name_start = i;

        while (i < text.len and text[i] != '}' and text[i] != '[') : (i += 1) {}
        if (i >= text.len) return ExpandError.UnterminatedVariable;

        const name = text[name_start..i];
        const values = ctx.getVar(name) orelse try getEnvOrStatus(ctx, name);

        if (i < text.len and text[i] == '[') {
            const index_result = try applyIndexing(ctx, values, text, i);
            var close_pos = index_result.new_pos;
            while (close_pos < text.len and text[close_pos] != '}') : (close_pos += 1) {}
            if (close_pos >= text.len) return ExpandError.UnterminatedVariable;
            return .{ .values = index_result.values, .new_pos = close_pos + 1 };
        }

        if (i >= text.len or text[i] != '}') return ExpandError.UnterminatedVariable;
        i += 1;

        if (i < text.len and text[i] == '[') {
            return try applyIndexing(ctx, values, text, i);
        }
        return .{ .values = values, .new_pos = i };
    }

    // Handle special single-character variables: $#, $*, $?
    if (i < text.len) {
        const ch = text[i];
        if (ch == '#' or ch == '*' or ch == '?') {
            const name = text[i .. i + 1];
            const values = try getEnvOrStatus(ctx, name);
            return .{ .values = values, .new_pos = i + 1 };
        }
    }

    const name_start = i;
    while (i < text.len and token_types.isIdentChar(text[i])) : (i += 1) {}
    const name = text[name_start..i];

    if (name.len == 0) {
        return .{ .values = try singletonSlice(ctx.allocator, "$"), .new_pos = start + 1 };
    }

    const values = ctx.getVar(name) orelse try getEnvOrStatus(ctx, name);

    // Check for indexing: $var[index] or $var[start..end]
    if (i < text.len and text[i] == '[') {
        return try applyIndexing(ctx, values, text, i);
    }

    return .{ .values = values, .new_pos = i };
}

/// Apply array indexing to a list of values.
/// Supports: [n], [-n], [start..end], [start..], [..end]
/// Indices are 1-based. Negative indices count from end.
fn applyIndexing(ctx: *ExpandContext, values: []const []const u8, text: []const u8, bracket_pos: usize) (ExpandError || std.mem.Allocator.Error)!VarExpansionResult {
    var i = bracket_pos + 1; // skip '['

    // Find the closing bracket
    const close_bracket = std.mem.indexOfScalarPos(u8, text, i, ']') orelse {
        return ExpandError.UnterminatedVariable;
    };

    const index_expr = text[i..close_bracket];
    i = close_bracket + 1;

    // Check for range syntax (..)
    if (std.mem.indexOf(u8, index_expr, "..")) |dot_pos| {
        // Range: [start..end], [start..], [..end]
        const start_str = index_expr[0..dot_pos];
        const end_str = index_expr[dot_pos + 2 ..];

        const start_idx = if (start_str.len == 0)
            @as(isize, 1) // default to first element
        else
            std.fmt.parseInt(isize, start_str, 10) catch return ExpandError.InvalidVariableName;

        const end_idx = if (end_str.len == 0)
            @as(isize, @intCast(values.len)) // default to last element
        else
            std.fmt.parseInt(isize, end_str, 10) catch return ExpandError.InvalidVariableName;

        const resolved_start = resolveIndex(start_idx, values.len);
        const resolved_end = resolveIndex(end_idx, values.len);

        if (resolved_start == null or resolved_end == null or resolved_start.? > resolved_end.?) {
            return .{ .values = &[_][]const u8{}, .new_pos = i };
        }

        const slice = values[resolved_start.? .. resolved_end.? + 1];
        return .{ .values = slice, .new_pos = i };
    } else {
        // Single index: [n] or [-n]
        const idx = std.fmt.parseInt(isize, index_expr, 10) catch {
            return ExpandError.InvalidVariableName;
        };

        const resolved = resolveIndex(idx, values.len);
        if (resolved == null) {
            return .{ .values = try singletonSlice(ctx.allocator, ""), .new_pos = i };
        }

        return .{ .values = try singletonSlice(ctx.allocator, values[resolved.?]), .new_pos = i };
    }
}

/// Convert 1-based (possibly negative) index to 0-based index.
/// Returns null if out of bounds.
/// Positive: 1 = first element, 2 = second, etc.
/// Negative: -1 = last element, -2 = second-to-last, etc.
fn resolveIndex(idx: isize, len: usize) ?usize {
    if (len == 0) return null;

    const len_signed: isize = @intCast(len);

    if (idx > 0) {
        // 1-based positive index
        if (idx > len_signed) return null;
        return @intCast(idx - 1);
    } else if (idx < 0) {
        // Negative index: -1 = last element
        const resolved = len_signed + idx;
        if (resolved < 0) return null;
        return @intCast(resolved);
    } else {
        // idx == 0 is invalid in 1-based indexing
        return null;
    }
}

/// Get variable from environment or handle special variables like $?, $status, $1, $#, $*
fn getEnvOrStatus(ctx: *ExpandContext, name: []const u8) ![]const []const u8 {
    // Handle status variables
    if (std.mem.eql(u8, name, "?") or std.mem.eql(u8, name, "status")) {
        const status_str = try std.fmt.allocPrint(ctx.allocator, "{d}", .{ctx.getStatus()});
        return singletonSlice(ctx.allocator, status_str);
    }

    // Handle $# (argument count)
    if (std.mem.eql(u8, name, "#")) {
        const argv = ctx.state.getVarList("argv") orelse &[_][]const u8{};
        const count_str = try std.fmt.allocPrint(ctx.allocator, "{d}", .{argv.len});
        return singletonSlice(ctx.allocator, count_str);
    }

    // Handle $* (all arguments as separate words)
    if (std.mem.eql(u8, name, "*")) {
        const argv = ctx.state.getVarList("argv") orelse &[_][]const u8{};
        if (argv.len == 0) {
            return singletonSlice(ctx.allocator, "");
        }
        return argv;
    }

    // Handle positional parameters $1, $2, etc.
    if (name.len > 0 and name[0] >= '1' and name[0] <= '9') {
        const idx = std.fmt.parseInt(usize, name, 10) catch {
            // Not a valid number, fall through to env lookup
            return singletonSlice(ctx.allocator, std.posix.getenv(name) orelse "");
        };

        const argv = ctx.state.getVarList("argv") orelse &[_][]const u8{};
        // $1 is argv[0], $2 is argv[1], etc.
        if (idx > 0 and idx <= argv.len) {
            return singletonSlice(ctx.allocator, argv[idx - 1]);
        }
        return singletonSlice(ctx.allocator, "");
    }

    // Check environment, fall back to empty string
    return singletonSlice(ctx.allocator, std.posix.getenv(name) orelse "");
}

pub fn runCmdsub(ctx: *ExpandContext, cmd: []const u8) (ExpandError || std.mem.Allocator.Error)![]const u8 {
    if (ctx.mock_cmdsub) |mock| {
        if (mock.get(cmd)) |output| {
            return output;
        }
    }

    return interpreter.executeAndCapture(ctx.allocator, ctx.state, cmd) catch {
        return ExpandError.CommandSubstitutionFailed;
    };
}

fn cartesian(allocator: std.mem.Allocator, left: []const []const u8, right: []const []const u8) std.mem.Allocator.Error!std.ArrayListUnmanaged([]const u8) {
    var result: std.ArrayListUnmanaged([]const u8) = .empty;

    for (left) |l| {
        for (right) |r| {
            const combined = try std.fmt.allocPrint(allocator, "{s}{s}", .{ l, r });
            try result.append(allocator, combined);
        }
    }

    return result;
}

pub fn expandWords(ctx: *ExpandContext, words: []const []const WordPart) (ExpandError || std.mem.Allocator.Error)![]const []const u8 {
    var result: std.ArrayListUnmanaged([]const u8) = .empty;

    for (words) |word| {
        const expanded = try expandWord(ctx, word);
        for (expanded) |e| {
            try result.append(ctx.allocator, e);
        }
    }

    return try result.toOwnedSlice(ctx.allocator);
}

// =============================================================================
// Tests
// =============================================================================
//
// Note: These tests use explicit setup instead of a TestContext wrapper because
// ExpandContext stores a pointer to State, which creates aliasing issues when
// returning a struct containing both.

const testing = std.testing;

// -----------------------------------------------------------------------------
// Basic Expansion
// -----------------------------------------------------------------------------

test "expand: bare word" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const parts = [_]WordPart{.{ .quotes = .none, .text = "hello" }};
    try testing.expectEqualStrings("hello", (try expandWord(&ctx, &parts))[0]);
}

test "expand: variable" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const values = [_][]const u8{ "a", "b" };
    try ctx.setVar("xs", &values);

    const parts = [_]WordPart{.{ .quotes = .none, .text = "$xs" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 2), result.len);
    try testing.expectEqualStrings("a", result[0]);
    try testing.expectEqualStrings("b", result[1]);
}

test "expand: tilde" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    state.home = "/home/user";
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const parts = [_]WordPart{.{ .quotes = .none, .text = "~/src" }};
    try testing.expectEqualStrings("/home/user/src", (try expandWord(&ctx, &parts))[0]);
}

test "expand: command substitution" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    // Single-line output
    try ctx.setMockCmdsub("whoami", "jon");
    const parts1 = [_]WordPart{.{ .quotes = .command, .text = "whoami" }};
    try testing.expectEqualStrings("jon", (try expandWord(&ctx, &parts1))[0]);

    // Multi-line output splits into list
    try ctx.setMockCmdsub("ls", "a\nb");
    const parts2 = [_]WordPart{.{ .quotes = .command, .text = "ls" }};
    const result = try expandWord(&ctx, &parts2);
    try testing.expectEqual(@as(usize, 2), result.len);
    try testing.expectEqualStrings("a", result[0]);
    try testing.expectEqualStrings("b", result[1]);
}

test "expand: glob pattern" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const matches = [_][]const u8{ "src/main.zig", "src/util.zig" };
    try ctx.setMockGlob("src/*.zig", &matches);

    const parts = [_]WordPart{.{ .quotes = .none, .text = "src/*.zig" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 2), result.len);
    try testing.expectEqualStrings("src/main.zig", result[0]);
}

// -----------------------------------------------------------------------------
// Quoting
// -----------------------------------------------------------------------------

test "quoting: single quotes preserve literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const values = [_][]const u8{ "a", "b" };
    try ctx.setVar("xs", &values);

    const parts = [_]WordPart{.{ .quotes = .single, .text = "$xs" }};
    try testing.expectEqualStrings("$xs", (try expandWord(&ctx, &parts))[0]);
}

test "quoting: escaped dollar" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const parts1 = [_]WordPart{.{ .quotes = .double, .text = "\\$HOME" }};
    try testing.expectEqualStrings("$HOME", (try expandWord(&ctx, &parts1))[0]);

    const parts2 = [_]WordPart{.{ .quotes = .none, .text = "\\$HOME" }};
    try testing.expectEqualStrings("$HOME", (try expandWord(&ctx, &parts2))[0]);
}

// -----------------------------------------------------------------------------
// Cartesian Products
// -----------------------------------------------------------------------------

test "cartesian: prefix with list" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const values = [_][]const u8{ "a", "b" };
    try ctx.setVar("xs", &values);

    const parts = [_]WordPart{ .{ .quotes = .none, .text = "pre" }, .{ .quotes = .none, .text = "$xs" } };
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqualStrings("prea", result[0]);
    try testing.expectEqualStrings("preb", result[1]);
}

test "cartesian: two lists" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const xs = [_][]const u8{ "a", "b" };
    const ys = [_][]const u8{ "1", "2" };
    try ctx.setVar("xs", &xs);
    try ctx.setVar("ys", &ys);

    const parts = [_]WordPart{.{ .quotes = .none, .text = "$xs$ys" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 4), result.len);
    try testing.expectEqualStrings("a1", result[0]);
    try testing.expectEqualStrings("b2", result[3]);
}

test "cartesian: command output with prefix/suffix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    try ctx.setMockCmdsub("ls", "a\nb");

    // prefix_$(cmd)_suffix with multi-line output produces cartesian product
    const parts = [_]WordPart{
        .{ .quotes = .none, .text = "pre_" },
        .{ .quotes = .command, .text = "ls" },
        .{ .quotes = .none, .text = "_suf" },
    };
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 2), result.len);
    try testing.expectEqualStrings("pre_a_suf", result[0]);
    try testing.expectEqualStrings("pre_b_suf", result[1]);
}

// -----------------------------------------------------------------------------
// Positional Parameters
// -----------------------------------------------------------------------------

test "positional: $1, $2, $#, $*" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const argv = [_][]const u8{ "foo", "bar", "baz" };
    try ctx.setVar("argv", &argv);

    const p1 = [_]WordPart{.{ .quotes = .none, .text = "$1" }};
    try testing.expectEqualStrings("foo", (try expandWord(&ctx, &p1))[0]);
    const p2 = [_]WordPart{.{ .quotes = .none, .text = "$2" }};
    try testing.expectEqualStrings("bar", (try expandWord(&ctx, &p2))[0]);
    const p3 = [_]WordPart{.{ .quotes = .none, .text = "$#" }};
    try testing.expectEqualStrings("3", (try expandWord(&ctx, &p3))[0]);
    const p4 = [_]WordPart{.{ .quotes = .none, .text = "$*" }};
    try testing.expectEqual(@as(usize, 3), (try expandWord(&ctx, &p4)).len);
    const p5 = [_]WordPart{.{ .quotes = .none, .text = "$5" }};
    try testing.expectEqualStrings("", (try expandWord(&ctx, &p5))[0]);
}

// -----------------------------------------------------------------------------
// Array Indexing
// -----------------------------------------------------------------------------

test "index: single element" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const values = [_][]const u8{ "a", "b", "c" };
    try ctx.setVar("xs", &values);

    const p1 = [_]WordPart{.{ .quotes = .none, .text = "$xs[1]" }};
    try testing.expectEqualStrings("a", (try expandWord(&ctx, &p1))[0]);
    const p2 = [_]WordPart{.{ .quotes = .none, .text = "$xs[-1]" }};
    try testing.expectEqualStrings("c", (try expandWord(&ctx, &p2))[0]);
    const p3 = [_]WordPart{.{ .quotes = .none, .text = "$xs[10]" }};
    try testing.expectEqualStrings("", (try expandWord(&ctx, &p3))[0]);
}

test "index: range slicing" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const values = [_][]const u8{ "a", "b", "c", "d", "e" };
    try ctx.setVar("xs", &values);

    const p1 = [_]WordPart{.{ .quotes = .none, .text = "$xs[1..2]" }};
    try testing.expectEqual(@as(usize, 2), (try expandWord(&ctx, &p1)).len);
    const p2 = [_]WordPart{.{ .quotes = .none, .text = "$xs[2..]" }};
    try testing.expectEqual(@as(usize, 4), (try expandWord(&ctx, &p2)).len);
    const p3 = [_]WordPart{.{ .quotes = .none, .text = "$xs[-2..-1]" }};
    const r3 = try expandWord(&ctx, &p3);
    try testing.expectEqualStrings("d", r3[0]);
    try testing.expectEqualStrings("e", r3[1]);
}

// -----------------------------------------------------------------------------
// Brace Expansion
// -----------------------------------------------------------------------------

test "brace: comma list" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    // Lexer now produces .brace parts (without the surrounding braces)
    const parts = [_]WordPart{.{ .quotes = .brace, .text = "a,b,c" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqualStrings("a", result[0]);
}

test "brace: with prefix and suffix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    // prefix_{a,b} becomes three parts: prefix_ + brace(a,b)
    const parts = [_]WordPart{
        .{ .quotes = .none, .text = "prefix_" },
        .{ .quotes = .brace, .text = "a,b" },
    };
    try testing.expectEqualStrings("prefix_a", (try expandWord(&ctx, &parts))[0]);
}

test "brace: nested braces cartesian product" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    // {a,b}_{x,y} becomes: brace(a,b) + _ + brace(x,y)
    const parts = [_]WordPart{
        .{ .quotes = .brace, .text = "a,b" },
        .{ .quotes = .none, .text = "_" },
        .{ .quotes = .brace, .text = "x,y" },
    };
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 4), result.len);
    try testing.expectEqualStrings("a_x", result[0]);
}

test "brace: variable inside braces" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const values = [_][]const u8{ "x", "y", "z" };
    try ctx.setVar("items", &values);

    // {$items}_suffix becomes: brace($items) + _suffix
    const parts = [_]WordPart{
        .{ .quotes = .brace, .text = "$items" },
        .{ .quotes = .none, .text = "_suffix" },
    };
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqualStrings("x_suffix", result[0]);
}

test "brace: glob pattern with suffix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    const matches = [_][]const u8{ "file1.txt", "file2.txt" };
    try ctx.setMockGlob("*.txt", &matches);

    // {*.txt}_backup becomes: brace(*.txt) + _backup
    const parts = [_]WordPart{
        .{ .quotes = .brace, .text = "*.txt" },
        .{ .quotes = .none, .text = "_backup" },
    };
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 2), result.len);
    try testing.expectEqualStrings("file1.txt_backup", result[0]);
}

test "brace: numeric range with variable at end" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    try ctx.setVar("n", &.{"5"});

    // {1..$n} should expand to 1 2 3 4 5
    const parts = [_]WordPart{.{ .quotes = .brace, .text = "1..$n" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 5), result.len);
    try testing.expectEqualStrings("1", result[0]);
    try testing.expectEqualStrings("5", result[4]);
}

test "brace: numeric range with variables at both ends" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    try ctx.setVar("start", &.{"3"});
    try ctx.setVar("end", &.{"7"});

    // {$start..$end} should expand to 3 4 5 6 7
    const parts = [_]WordPart{.{ .quotes = .brace, .text = "$start..$end" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 5), result.len);
    try testing.expectEqualStrings("3", result[0]);
    try testing.expectEqualStrings("7", result[4]);
}

test "brace: numeric range with variable descending" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    try ctx.setVar("n", &.{"5"});

    // {$n..1} should expand to 5 4 3 2 1
    const parts = [_]WordPart{.{ .quotes = .brace, .text = "$n..1" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 5), result.len);
    try testing.expectEqualStrings("5", result[0]);
    try testing.expectEqualStrings("1", result[4]);
}

test "brace: numeric range with list variable uses first value" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    // When variable is a list, use first value
    try ctx.setVar("n", &.{ "3", "5", "7" });

    // {1..$n} should expand to 1 2 3 (using first value 3)
    const parts = [_]WordPart{.{ .quotes = .brace, .text = "1..$n" }};
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqualStrings("1", result[0]);
    try testing.expectEqualStrings("3", result[2]);
}

test "brace: numeric range with variable and prefix/suffix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    var ctx = ExpandContext.init(arena.allocator(), &state);

    try ctx.setVar("n", &.{"3"});

    // prefix_{1..$n}_suffix becomes prefix_1_suffix prefix_2_suffix prefix_3_suffix
    const parts = [_]WordPart{
        .{ .quotes = .none, .text = "prefix_" },
        .{ .quotes = .brace, .text = "1..$n" },
        .{ .quotes = .none, .text = "_suffix" },
    };
    const result = try expandWord(&ctx, &parts);
    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqualStrings("prefix_1_suffix", result[0]);
    try testing.expectEqualStrings("prefix_3_suffix", result[2]);
}
