//! Glob pattern matching and filesystem expansion.
//!
//! Supports standard glob metacharacters:
//! - `*` matches any sequence of characters (except `/`)
//! - `**` matches any path segments (recursive directory traversal)
//! - `?` matches any single character
//! - `[abc]` matches any character in the set
//! - `[a-z]` matches any character in the range
//! - `[!abc]` or `[^abc]` matches any character NOT in the set
//!
//! Hidden files (starting with `.`) are excluded unless the pattern
//! explicitly starts with `.`.
//!
//! If no files match, the original pattern is returned literally.

const std = @import("std");

/// Check if a pattern contains unescaped glob metacharacters
pub fn hasGlobChars(pattern: []const u8) bool {
    var i: usize = 0;
    while (i < pattern.len) : (i += 1) {
        if (pattern[i] == '\\' and i + 1 < pattern.len) {
            i += 1; // skip escaped char
            continue;
        }
        if (pattern[i] == '*' or pattern[i] == '?' or pattern[i] == '[') return true;
    }
    return false;
}

/// Allocate a single-element slice containing the given value
pub fn singletonSlice(allocator: std.mem.Allocator, value: []const u8) ![]const []const u8 {
    const result = try allocator.alloc([]const u8, 1);
    result[0] = value;
    return result;
}

/// Expand a glob pattern against the filesystem
/// Returns matching files sorted alphabetically, or the original pattern if no matches
pub fn expandGlob(
    allocator: std.mem.Allocator,
    pattern: []const u8,
    mock_glob: ?std.StringHashMap([]const []const u8),
) std.mem.Allocator.Error![]const []const u8 {
    if (mock_glob) |mock| {
        if (mock.get(pattern)) |matches| {
            return matches;
        }
    }

    var matches: std.ArrayListUnmanaged([]const u8) = .empty;

    // Split pattern into segments
    var segments: std.ArrayListUnmanaged([]const u8) = .empty;
    defer segments.deinit(allocator);

    var iter = std.mem.splitScalar(u8, pattern, '/');
    while (iter.next()) |seg| {
        try segments.append(allocator, seg);
    }

    // Handle absolute path
    var start_dir: std.fs.Dir = undefined;
    var start_path: []const u8 = "";
    var seg_idx: usize = 0;

    if (pattern.len > 0 and pattern[0] == '/') {
        start_dir = std.fs.openDirAbsolute("/", .{ .iterate = true }) catch {
            return singletonSlice(allocator, pattern);
        };
        start_path = "/";
        // First segment is empty string due to split
        if (segments.items.len > 0 and segments.items[0].len == 0) {
            seg_idx = 1;
        }
    } else {
        start_dir = std.fs.cwd().openDir(".", .{ .iterate = true }) catch {
            return singletonSlice(allocator, pattern);
        };
        start_path = "";
    }
    defer start_dir.close();

    try matchRecursive(allocator, start_dir, start_path, segments.items[seg_idx..], &matches);

    if (matches.items.len == 0) return singletonSlice(allocator, pattern);

    std.mem.sort([]const u8, matches.items, {}, lessThanStr);
    return matches.toOwnedSlice(allocator);
}

fn matchRecursive(
    allocator: std.mem.Allocator,
    dir: std.fs.Dir,
    current_path: []const u8,
    segments: []const []const u8,
    matches: *std.ArrayListUnmanaged([]const u8),
) !void {
    if (segments.len == 0) {
        return;
    }

    const segment = segments[0];
    const is_last = segments.len == 1;

    if (segment.len == 0) {
        // Empty segment (e.g. // or trailing /)
        if (is_last) {
            // Trailing slash - match if current_path is dir (it is)
            try matches.append(allocator, try allocator.dupe(u8, current_path));
        } else {
            try matchRecursive(allocator, dir, current_path, segments[1..], matches);
        }
        return;
    }

    if (std.mem.eql(u8, segment, "**")) {
        // Recursive match

        // 1. Match rest against current dir (consume **)
        try matchRecursive(allocator, dir, current_path, segments[1..], matches);

        // 2. Recurse into subdirs
        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind == .directory) {
                if (entry.name[0] == '.') continue; // Skip hidden

                var sub_dir = dir.openDir(entry.name, .{ .iterate = true }) catch continue;
                defer sub_dir.close();

                const sub_path = try joinPath(allocator, current_path, entry.name);
                defer allocator.free(sub_path);

                // Keep ** in segments
                try matchRecursive(allocator, sub_dir, sub_path, segments, matches);
            }
        }
        return;
    }

    // Regular segment (literal or glob)
    if (!hasGlobChars(segment)) {
        if (is_last) {
            _ = dir.statFile(segment) catch return;
            const match = try joinPath(allocator, current_path, segment);
            try matches.append(allocator, match);
        } else {
            var sub_dir = dir.openDir(segment, .{ .iterate = true }) catch return;
            defer sub_dir.close();
            const sub_path = try joinPath(allocator, current_path, segment);
            defer allocator.free(sub_path);
            try matchRecursive(allocator, sub_dir, sub_path, segments[1..], matches);
        }
        return;
    }

    // Glob segment
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.name[0] == '.' and segment[0] != '.') continue;

        if (globMatch(segment, entry.name)) {
            const sub_path = try joinPath(allocator, current_path, entry.name);
            defer allocator.free(sub_path);

            if (is_last) {
                try matches.append(allocator, try allocator.dupe(u8, sub_path));
            } else if (entry.kind == .directory) {
                var sub_dir = dir.openDir(entry.name, .{ .iterate = true }) catch continue;
                defer sub_dir.close();
                try matchRecursive(allocator, sub_dir, sub_path, segments[1..], matches);
            }
        }
    }
}

fn joinPath(allocator: std.mem.Allocator, base: []const u8, part: []const u8) ![]u8 {
    if (base.len == 0) return allocator.dupe(u8, part);
    if (base.len == 1 and base[0] == '/') return std.fmt.allocPrint(allocator, "/{s}", .{part});
    return std.fmt.allocPrint(allocator, "{s}/{s}", .{ base, part });
}

fn lessThanStr(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

/// Glob pattern matching with support for:
/// - `*` matches any sequence of characters
/// - `?` matches any single character
/// - `[abc]` matches any character in the set
/// - `[a-z]` matches any character in the range
/// - `[!abc]` or `[^abc]` matches any character NOT in the set
pub fn globMatch(pattern: []const u8, name: []const u8) bool {
    var pi: usize = 0;
    var ni: usize = 0;
    var star_pi: ?usize = null;
    var star_ni: usize = 0;

    while (ni < name.len or pi < pattern.len) {
        if (pi < pattern.len) {
            const pc = pattern[pi];
            if (pc == '*') {
                // Star: remember position for backtracking
                star_pi = pi;
                star_ni = ni;
                pi += 1;
                continue;
            } else if (ni < name.len) {
                if (pc == '?' or pc == name[ni]) {
                    pi += 1;
                    ni += 1;
                    continue;
                } else if (pc == '[') {
                    // Character class [abc] or [a-z]
                    if (matchCharClass(pattern, pi, name[ni])) |new_pi| {
                        pi = new_pi;
                        ni += 1;
                        continue;
                    }
                }
            }
        }

        // No match - try backtracking to last star
        if (star_pi) |sp| {
            pi = sp + 1;
            star_ni += 1;
            ni = star_ni;
            if (ni > name.len) return false;
            continue;
        }

        return false;
    }

    return true;
}

/// Match a character class like [abc] or [a-z]
/// Returns the position after the closing ] if matched, null otherwise
fn matchCharClass(pattern: []const u8, start: usize, ch: u8) ?usize {
    if (start >= pattern.len or pattern[start] != '[') return null;

    var i = start + 1;
    var matched = false;
    var negate = false;

    // Check for negation
    if (i < pattern.len and (pattern[i] == '!' or pattern[i] == '^')) {
        negate = true;
        i += 1;
    }

    while (i < pattern.len and pattern[i] != ']') {
        // Check for range a-z
        if (i + 2 < pattern.len and pattern[i + 1] == '-' and pattern[i + 2] != ']') {
            const lo = pattern[i];
            const hi = pattern[i + 2];
            if (ch >= lo and ch <= hi) {
                matched = true;
            }
            i += 3;
        } else {
            if (pattern[i] == ch) {
                matched = true;
            }
            i += 1;
        }
    }

    if (i >= pattern.len) return null; // Unterminated class

    // Return position after ]
    const result = if (negate) !matched else matched;
    return if (result) i + 1 else null;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

// -----------------------------------------------------------------------------
// Glob Matching
// -----------------------------------------------------------------------------

test "globMatch: star patterns" {
    const cases = [_]struct { []const u8, []const u8, bool }{
        // Star at end
        .{ "*.zig", "main.zig", true },
        .{ "*.zig", "test.zig", true },
        .{ "*.zig", "main.c", false },
        .{ "*.zig", "main.zigx", false },
        // Star at start
        .{ "*test", "mytest", true },
        .{ "*test", "test", true },
        .{ "*test", "testing", false },
        // Star in middle
        .{ "foo*bar", "foobar", true },
        .{ "foo*bar", "fooxbar", true },
        .{ "foo*bar", "fooxxxbar", true },
        .{ "foo*bar", "foobaz", false },
        // Double star (acts like * in pattern matching)
        .{ "**", "anything", true },
        .{ "**", "", true },
        .{ "**.zig", "main.zig", true },
    };
    for (cases) |c| try testing.expectEqual(c[2], globMatch(c[0], c[1]));
}

test "globMatch: question mark" {
    const cases = [_]struct { []const u8, []const u8, bool }{
        .{ "?.zig", "a.zig", true },
        .{ "?.zig", "ab.zig", false },
        .{ "test?.zig", "test1.zig", true },
    };
    for (cases) |c| try testing.expectEqual(c[2], globMatch(c[0], c[1]));
}

test "globMatch: character class and range" {
    const cases = [_]struct { []const u8, []const u8, bool }{
        // Character class [abc]
        .{ "[abc].txt", "a.txt", true },
        .{ "[abc].txt", "b.txt", true },
        .{ "[abc].txt", "d.txt", false },
        // Character range [a-z]
        .{ "[a-z].txt", "m.txt", true },
        .{ "[a-z].txt", "M.txt", false },
        .{ "[0-9].txt", "5.txt", true },
        // Negated class [!abc] and [^abc]
        .{ "[!abc].txt", "d.txt", true },
        .{ "[!abc].txt", "a.txt", false },
        .{ "[^xyz].txt", "a.txt", true },
        .{ "[^xyz].txt", "x.txt", false },
    };
    for (cases) |c| try testing.expectEqual(c[2], globMatch(c[0], c[1]));
}

test "globMatch: exact match" {
    const cases = [_]struct { []const u8, []const u8, bool }{
        .{ "exact", "exact", true },
        .{ "exact", "exactx", false },
        .{ "exactx", "exact", false },
    };
    for (cases) |c| try testing.expectEqual(c[2], globMatch(c[0], c[1]));
}

// -----------------------------------------------------------------------------
// Metacharacter Detection
// -----------------------------------------------------------------------------

test "hasGlobChars: detects metacharacters" {
    const cases = [_]struct { []const u8, bool }{
        .{ "*.txt", true },
        .{ "file?.log", true },
        .{ "[abc].md", true },
        .{ "**/*.zig", true },
        .{ "plain.txt", false },
        .{ "no-globs-here", false },
    };
    for (cases) |c| try testing.expectEqual(c[1], hasGlobChars(c[0]));
}

test "hasGlobChars: ignores escaped metacharacters" {
    const cases = [_]struct { []const u8, bool }{
        .{ "\\*.txt", false },
        .{ "file\\?.log", false },
        .{ "\\[abc\\].md", false },
        .{ "\\*.txt*", true }, // escaped star, then real star
        .{ "literal\\*asterisk", false },
    };
    for (cases) |c| try testing.expectEqual(c[1], hasGlobChars(c[0]));
}
