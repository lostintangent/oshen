//! Complete: tab completion for the line editor.
//!
//! Provides context-aware completions:
//! - Command position (first word): builtins + PATH executables
//! - Argument position: file/directory names
//! - Path-like words (contain '/', start with '.', '~'): file/directory names
//!
//! Features:
//! - Common prefix insertion (partial completion)
//! - Single-match auto-complete with trailing space
//! - Multiple-match display for user selection
//! - Tilde expansion for home directory paths

const std = @import("std");

const repl = @import("../../repl.zig");
const builtins = @import("../../../runtime/builtins.zig");

// =============================================================================
// Types
// =============================================================================

/// Result of a completion attempt (unused, kept for API compatibility).
pub const Completion = struct {
    /// The text to insert (replaces the current word)
    text: []const u8,
    /// Whether this is the only completion (can auto-insert)
    unique: bool,
};

/// Result of completion with metadata for the editor.
pub const CompletionResult = struct {
    /// List of possible completions.
    completions: []const []const u8,
    /// Start position of word being completed.
    word_start: usize,
    /// End position of word (cursor position).
    word_end: usize,

    pub fn deinit(self: *CompletionResult, allocator: std.mem.Allocator) void {
        for (self.completions) |c| allocator.free(c);
        allocator.free(self.completions);
    }

    /// Get common prefix of all completions (for partial completion).
    pub fn commonPrefix(self: *const CompletionResult) []const u8 {
        if (self.completions.len == 0) return "";
        if (self.completions.len == 1) return self.completions[0];

        const first = self.completions[0];
        var prefix_len: usize = first.len;

        for (self.completions[1..]) |c| {
            var i: usize = 0;
            while (i < prefix_len and i < c.len and first[i] == c[i]) : (i += 1) {}
            prefix_len = i;
        }

        return first[0..prefix_len];
    }
};

// =============================================================================
// Public API
// =============================================================================

/// Find completions for the given input at cursor position.
/// Returns a list of possible completions, or null if none found
pub fn complete(allocator: std.mem.Allocator, input: []const u8, cursor: usize) !?CompletionResult {
    // Find the word being completed (from last space to cursor)
    const word_start = repl.findWordStart(input, cursor);
    const word = input[word_start..cursor];
    const is_first_word = isFirstWord(input, word_start);

    if (word.len == 0) return null;

    var completions: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer {
        for (completions.items) |c| allocator.free(c);
        completions.deinit(allocator);
    }

    // Determine completion type based on context
    if (isPathLike(word)) {
        // File/directory completion
        try completeFiles(allocator, word, &completions);
    } else if (is_first_word) {
        // Command completion (builtins + executables)
        try completeCommands(allocator, word, &completions);
    } else {
        // Argument position - try file completion
        try completeFiles(allocator, word, &completions);
    }

    if (completions.items.len == 0) return null;

    // Sort completions for consistent display
    std.mem.sort([]const u8, completions.items, {}, lessThan);

    return CompletionResult{
        .completions = try completions.toOwnedSlice(allocator),
        .word_start = word_start,
        .word_end = cursor,
    };
}

fn lessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

// =============================================================================
// Private helpers
// =============================================================================

/// Check if this is the first word (command position).
fn isFirstWord(input: []const u8, word_start: usize) bool {
    // If word starts at 0, it's the first word
    if (word_start == 0) return true;

    // Check if everything before word_start is whitespace
    for (input[0..word_start]) |c| {
        if (c != ' ' and c != '\t') return false;
    }
    return true;
}

/// Check if word looks like a path.
inline fn isPathLike(word: []const u8) bool {
    if (word.len == 0) return false;
    return word[0] == '/' or word[0] == '.' or word[0] == '~' or
        std.mem.indexOf(u8, word, "/") != null;
}

/// Complete file/directory names.
fn completeFiles(allocator: std.mem.Allocator, word: []const u8, completions: *std.ArrayListUnmanaged([]const u8)) !void {
    // Handle tilde expansion using shared utility
    var expanded_buf: [std.fs.max_path_bytes]u8 = undefined;
    const home = std.posix.getenv("HOME") orelse "";
    const expanded = repl.expandTilde(word, home, &expanded_buf);
    const search_word = expanded orelse word;
    const has_tilde = expanded != null;

    // Split into directory and file prefix
    const last_slash = std.mem.lastIndexOf(u8, search_word, "/");
    const dir_path = if (last_slash) |idx| search_word[0 .. idx + 1] else "./";
    const file_prefix = if (last_slash) |idx| search_word[idx + 1 ..] else search_word;

    // Calculate the prefix to preserve in completions
    const preserve_prefix = if (last_slash) |idx|
        if (has_tilde) blk: {
            // ~/ case - show ~/ + relative path
            break :blk word[0 .. idx + 1];
        } else word[0 .. idx + 1]
    else if (has_tilde)
        "~/"
    else
        "";

    // Open directory
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return;
    defer dir.close();

    // Iterate and find matches
    var iter = dir.iterate();
    while (iter.next() catch return) |entry| {
        if (file_prefix.len == 0 or std.mem.startsWith(u8, entry.name, file_prefix)) {
            // Skip hidden files unless prefix starts with '.'
            if (entry.name[0] == '.' and (file_prefix.len == 0 or file_prefix[0] != '.')) {
                continue;
            }

            const is_dir = entry.kind == .directory;
            const suffix: []const u8 = if (is_dir) "/" else "";

            const completion = try std.fmt.allocPrint(allocator, "{s}{s}{s}", .{
                preserve_prefix,
                entry.name,
                suffix,
            });
            try completions.append(allocator, completion);
        }
    }
}

/// Complete command names (builtins + PATH executables).
fn completeCommands(allocator: std.mem.Allocator, word: []const u8, completions: *std.ArrayListUnmanaged([]const u8)) !void {
    // Add matching builtins (from centralized registry)
    for (builtins.getNames()) |name| {
        if (std.mem.startsWith(u8, name, word)) {
            try completions.append(allocator, try allocator.dupe(u8, name));
        }
    }

    // Add matching executables from PATH
    const path_env = std.posix.getenv("PATH") orelse return;
    var path_iter = std.mem.splitScalar(u8, path_env, ':');

    // Track seen names to avoid duplicates.
    // We use the completion strings themselves as keys (no separate allocation).
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();

    while (path_iter.next()) |path_dir| {
        if (path_dir.len == 0) continue;

        var dir = std.fs.cwd().openDir(path_dir, .{ .iterate = true }) catch continue;
        defer dir.close();

        var iter = dir.iterate();
        while (iter.next() catch continue) |entry| {
            if (entry.kind != .file and entry.kind != .sym_link) continue;
            if (!std.mem.startsWith(u8, entry.name, word)) continue;
            if (seen.contains(entry.name)) continue;

            const completion = try allocator.dupe(u8, entry.name);
            try completions.append(allocator, completion);
            // Use the completion string as the key - no extra allocation needed.
            // The completion outlives `seen`, so this is safe.
            try seen.put(completion, {});
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "isFirstWord: detects command position" {
    const cases = [_]struct { []const u8, usize, bool }{
        .{ "echo", 0, true }, // at start
        .{ "echo hello", 5, false }, // second word
        .{ "  echo", 2, true }, // leading whitespace
        .{ "\tls", 1, true }, // leading tab
        .{ "", 0, true }, // empty input
    };
    for (cases) |c| try testing.expectEqual(c[2], isFirstWord(c[0], c[1]));
}

test "isPathLike: detects path-like words" {
    const cases = [_]struct { []const u8, bool }{
        .{ "/usr/bin", true }, // absolute
        .{ "./test", true }, // relative
        .{ "~/", true }, // tilde
        .{ "echo", false }, // simple word
    };
    for (cases) |c| try testing.expectEqual(c[1], isPathLike(c[0]));
}

test "commonPrefix: finds longest common prefix" {
    const Case = struct { []const []const u8, []const u8 };
    const cases = [_]Case{
        .{ &[_][]const u8{"hello"}, "hello" }, // single item
        .{ &[_][]const u8{ "hello", "help", "helicopter" }, "hel" }, // shared prefix
        .{ &[_][]const u8{}, "" }, // empty
        .{ &[_][]const u8{ "apple", "banana", "cherry" }, "" }, // no common prefix
    };
    for (cases) |c| {
        var result = CompletionResult{ .completions = c[0], .word_start = 0, .word_end = 0 };
        try testing.expectEqualStrings(c[1], result.commonPrefix());
    }
}

