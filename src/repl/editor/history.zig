//! History: command history with rich metadata for context-aware suggestions.
//!
//! Each entry tracks command, working directory, timestamp, exit status, and frequency.
//! This enables intelligent scoring based on directory context, recency, and usage patterns.
//!
//! ## Scoring
//!
//! Entries are scored for both suggestions and eviction using `scoreEntry`:
//! - **Directory context (40%)**: Exact match > parent > child > sibling > unrelated
//! - **Recency (30%)**: Exponential decay with 1-week half-life
//! - **Frequency (25%)**: Logarithmic growth, capped at 100
//! - **Success (5%)**: Small bonus for exit code 0
//!
//! When `current_cwd` is null (eviction), directory score is 0 and other factors dominate.
//!
//! ## Eviction Strategy
//!
//! Uses score-based batch eviction for O(1) amortized adds:
//! - History grows until EVICT_THRESHOLD (12K entries)
//! - Then prunes to TARGET_SIZE (10K) by removing lowest-scored entries
//!
//! ## File Format (OSHEN_HIST_V1)
//!
//! Binary format for fast load/save:
//! ```
//! OSHEN_HIST_V1\n              # 14-byte magic header
//! [entry_count: u32]           # Little-endian entry count
//! Per entry:
//!   [cmd_len: u16][cmd: bytes] # Command text
//!   [cwd_len: u16][cwd: bytes] # Working directory
//!   [timestamp: i64]           # Unix timestamp (seconds)
//!   [exit_status: u8]          # Exit code (0-255)
//!   [frequency: u32]           # Usage count
//! ```

const std = @import("std");

/// Target number of history entries after pruning.
const TARGET_SIZE: usize = 10_000;

/// Threshold that triggers batch eviction. Allows ~2K new entries between prunes.
const EVICT_THRESHOLD: usize = 12_000;

/// Magic header identifying the file format version.
const FILE_MAGIC = "OSHEN_HIST_V1\n";

// =============================================================================
// Scoring constants
// =============================================================================

/// Scoring weights (sum to 100).
const weight_directory: f64 = 40.0;
const weight_recency: f64 = 30.0;
const weight_frequency: f64 = 25.0;
const weight_success: f64 = 5.0;

/// Natural log of 2, for exponential decay calculation.
const LN_2: f64 = 0.693147180559945;

/// Recency half-life in seconds (1 week).
const RECENCY_HALF_LIFE: f64 = 7 * 24 * 60 * 60;

/// A single history entry with full context.
pub const Entry = struct {
    command: []const u8,
    cwd: []const u8,
    timestamp: i64,
    exit_status: u8,
    frequency: u32,
};

/// Input for adding a new history entry.
pub const AddInput = struct {
    command: []const u8,
    cwd: []const u8,
    exit_status: u8,
};

/// Command history with deduplication and frequency tracking.
///
/// Entries are deduplicated by (command, cwd) pair. Running the same command
/// in the same directory increments frequency rather than creating a duplicate.
pub const History = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry),
    /// Maps hash(command, cwd) -> entry index for O(1) deduplication.
    dedup: std.AutoHashMapUnmanaged(u64, usize),

    pub fn init(allocator: std.mem.Allocator) History {
        return .{ .allocator = allocator, .entries = .empty, .dedup = .empty };
    }

    pub fn deinit(self: *History) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.command);
            self.allocator.free(entry.cwd);
        }
        self.entries.deinit(self.allocator);
        self.dedup.deinit(self.allocator);
    }

    /// Add a command to history. Increments frequency if command+cwd already exists.
    /// O(1) for new entries; O(n) for duplicates (moved to end for recency).
    /// O(n log n) when pruning is triggered (every ~2K unique entries).
    pub fn add(self: *History, input: AddInput) bool {
        if (input.command.len == 0) return false;

        const key = hash(input.command, input.cwd);
        const now = std.time.timestamp();

        // Update existing entry if found - move to end for proper recency navigation
        if (self.dedup.get(key)) |old_index| {
            const old_entry = self.entries.orderedRemove(old_index);

            // Append updated entry at end
            self.entries.appendAssumeCapacity(.{
                .command = old_entry.command,
                .cwd = old_entry.cwd,
                .timestamp = now,
                .exit_status = input.exit_status,
                .frequency = old_entry.frequency + 1,
            });

            // Fix dedup indices: entries after old_index shifted down by 1
            var it = self.dedup.valueIterator();
            while (it.next()) |idx_ptr| {
                if (idx_ptr.* > old_index) idx_ptr.* -= 1;
            }
            self.dedup.putAssumeCapacity(key, self.entries.items.len - 1);

            return true;
        }

        // Create new entry
        const command = self.allocator.dupe(u8, input.command) catch return false;
        const cwd = self.allocator.dupe(u8, input.cwd) catch {
            self.allocator.free(command);
            return false;
        };

        const index = self.entries.items.len;
        self.entries.append(self.allocator, .{
            .command = command,
            .cwd = cwd,
            .timestamp = now,
            .exit_status = input.exit_status,
            .frequency = 1,
        }) catch {
            self.allocator.free(command);
            self.allocator.free(cwd);
            return false;
        };

        self.dedup.put(self.allocator, key, index) catch {};

        // Prune if we've crossed the threshold
        if (self.entries.items.len > EVICT_THRESHOLD) {
            self.pruneToTarget();
        }

        return true;
    }

    /// Prune history to TARGET_SIZE by removing lowest-scored entries.
    /// Keeps entries with highest recency + frequency scores.
    fn pruneToTarget(self: *History) void {
        const now = std.time.timestamp();

        // Score all entries
        const ScoredIndex = struct { index: usize, score: f64 };
        var scored = self.allocator.alloc(ScoredIndex, self.entries.items.len) catch return;
        defer self.allocator.free(scored);

        for (self.entries.items, 0..) |entry, i| {
            // Pass null for cwd - eviction doesn't have directory context
            scored[i] = .{ .index = i, .score = scoreEntry(&entry, null, now) };
        }

        // Sort by score descending (highest scores first)
        std.mem.sort(ScoredIndex, scored, {}, struct {
            fn cmp(_: void, a: ScoredIndex, b: ScoredIndex) bool {
                return a.score > b.score;
            }
        }.cmp);

        // Mark entries to keep
        var keep = self.allocator.alloc(bool, self.entries.items.len) catch return;
        defer self.allocator.free(keep);
        @memset(keep, false);

        for (scored[0..TARGET_SIZE]) |s| {
            keep[s.index] = true;
        }

        // Free entries we're removing
        for (self.entries.items, 0..) |entry, i| {
            if (!keep[i]) {
                self.allocator.free(entry.command);
                self.allocator.free(entry.cwd);
            }
        }

        // Compact: keep only marked entries
        var write_idx: usize = 0;
        for (self.entries.items, 0..) |entry, read_idx| {
            if (keep[read_idx]) {
                self.entries.items[write_idx] = entry;
                write_idx += 1;
            }
        }
        self.entries.items.len = write_idx;

        // Rebuild dedup index
        self.dedup.clearRetainingCapacity();
        for (self.entries.items, 0..) |entry, index| {
            self.dedup.put(self.allocator, hash(entry.command, entry.cwd), index) catch {};
        }
    }

    // =========================================================================
    // Queries
    // =========================================================================

    pub fn count(self: *const History) usize {
        return self.entries.items.len;
    }

    pub fn get(self: *const History, index: usize) ?Entry {
        return if (index < self.entries.items.len) self.entries.items[index] else null;
    }

    // =========================================================================
    // Persistence
    // =========================================================================

    pub fn load(self: *History, path: []const u8) void {
        const file = std.fs.openFileAbsolute(path, .{}) catch return;
        defer file.close();

        const data = file.readToEndAlloc(self.allocator, 100 * 1024 * 1024) catch return;
        defer self.allocator.free(data);

        self.parse(data);
    }

    pub fn save(self: *History, path: []const u8) void {
        const file = std.fs.createFileAbsolute(path, .{}) catch return;
        defer file.close();

        file.writeAll(FILE_MAGIC) catch return;
        file.writeAll(std.mem.asBytes(&@as(u32, @intCast(self.entries.items.len)))) catch return;

        for (self.entries.items) |entry| writeEntry(file, entry) catch return;
    }

    fn parse(self: *History, data: []const u8) void {
        if (data.len < FILE_MAGIC.len) return;
        if (!std.mem.eql(u8, data[0..FILE_MAGIC.len], FILE_MAGIC)) return;

        var position: usize = FILE_MAGIC.len;
        if (position + 4 > data.len) return;

        const entry_count = std.mem.readInt(u32, data[position..][0..4], .little);
        position += 4;

        for (0..entry_count) |_| {
            const entry = self.readEntry(data, &position) orelse break;
            const index = self.entries.items.len;
            self.entries.append(self.allocator, entry) catch break;
            self.dedup.put(self.allocator, hash(entry.command, entry.cwd), index) catch {};
        }
    }

    fn readEntry(self: *History, data: []const u8, position: *usize) ?Entry {
        const command = self.readString(data, position) orelse return null;
        const cwd = self.readString(data, position) orelse {
            self.allocator.free(command);
            return null;
        };

        if (position.* + 13 > data.len) {
            self.allocator.free(command);
            self.allocator.free(cwd);
            return null;
        }

        const timestamp = std.mem.readInt(i64, data[position.*..][0..8], .little);
        const exit_status = data[position.* + 8];
        const frequency = std.mem.readInt(u32, data[position.* + 9 ..][0..4], .little);
        position.* += 13;

        return .{
            .command = command,
            .cwd = cwd,
            .timestamp = timestamp,
            .exit_status = exit_status,
            .frequency = frequency,
        };
    }

    fn readString(self: *History, data: []const u8, position: *usize) ?[]const u8 {
        if (position.* + 2 > data.len) return null;
        const length = std.mem.readInt(u16, data[position.*..][0..2], .little);
        position.* += 2;
        if (position.* + length > data.len) return null;
        const string = self.allocator.dupe(u8, data[position.*..][0..length]) catch return null;
        position.* += length;
        return string;
    }

    fn writeEntry(file: std.fs.File, entry: Entry) !void {
        try file.writeAll(std.mem.asBytes(&@as(u16, @intCast(entry.command.len))));
        try file.writeAll(entry.command);
        try file.writeAll(std.mem.asBytes(&@as(u16, @intCast(entry.cwd.len))));
        try file.writeAll(entry.cwd);
        try file.writeAll(std.mem.asBytes(&entry.timestamp));
        try file.writeAll(&[_]u8{entry.exit_status});
        try file.writeAll(std.mem.asBytes(&entry.frequency));
    }
};

fn hash(command: []const u8, cwd: []const u8) u64 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(command);
    hasher.update("\x00");
    hasher.update(cwd);
    return hasher.final();
}

// =============================================================================
// Scoring (shared by suggestions and eviction)
// =============================================================================

/// Score an entry for ranking. Higher = more relevant/valuable.
/// When `current_cwd` is null, directory scoring is skipped (used for eviction).
pub fn scoreEntry(entry: *const Entry, current_cwd: ?[]const u8, now: i64) f64 {
    const dir = if (current_cwd) |cwd| directoryScore(entry.cwd, cwd) else 0.0;
    return dir * (weight_directory / 100.0) +
        recencyScore(entry.timestamp, now) * (weight_recency / 100.0) +
        frequencyScore(entry.frequency) * (weight_frequency / 100.0) +
        successScore(entry.exit_status) * (weight_success / 100.0);
}

/// Directory relevance: exact match > parent > child > sibling > unrelated.
pub fn directoryScore(entry_cwd: []const u8, current_cwd: []const u8) f64 {
    if (std.mem.eql(u8, entry_cwd, current_cwd)) return 100.0;

    // Entry is parent of current (e.g., /home vs /home/user/project)
    if (current_cwd.len > entry_cwd.len and
        std.mem.startsWith(u8, current_cwd, entry_cwd) and
        (entry_cwd.len == 0 or current_cwd[entry_cwd.len] == '/'))
        return 50.0;

    // Current is parent of entry (e.g., /home/user vs /home/user/project)
    if (entry_cwd.len > current_cwd.len and
        std.mem.startsWith(u8, entry_cwd, current_cwd) and
        (current_cwd.len == 0 or entry_cwd[current_cwd.len] == '/'))
        return 30.0;

    // Sibling directories (same parent)
    if (areSiblingDirectories(entry_cwd, current_cwd))
        return 20.0;

    return 0.0;
}

/// Returns true if two paths are sibling directories (share the same parent).
pub fn areSiblingDirectories(path_a: []const u8, path_b: []const u8) bool {
    const parent_a = parentDirectory(path_a);
    const parent_b = parentDirectory(path_b);
    // Both must have non-root parents (top-level dirs like /var and /home aren't related)
    if (parent_a.len <= 1 or parent_b.len <= 1) return false;
    return std.mem.eql(u8, parent_a, parent_b);
}

/// Returns the parent directory of a path, or empty string if at root.
pub fn parentDirectory(path: []const u8) []const u8 {
    if (path.len == 0) return "";
    const search_end = if (path[path.len - 1] == '/') path.len - 1 else path.len;
    if (search_end == 0) return "";
    const last_slash = std.mem.lastIndexOf(u8, path[0..search_end], "/");
    if (last_slash) |pos| {
        return if (pos == 0) "/" else path[0..pos];
    }
    return "";
}

/// Exponential decay: score = 100 * 0.5^(age / half_life).
pub fn recencyScore(timestamp: i64, now: i64) f64 {
    const age: f64 = @floatFromInt(@max(0, now - timestamp));
    return 100.0 * std.math.exp(-LN_2 * age / RECENCY_HALF_LIFE);
}

/// Logarithmic growth: log2(freq + 1) * 20, capped at 100.
pub fn frequencyScore(frequency: u32) f64 {
    return @min(std.math.log2(@as(f64, @floatFromInt(frequency + 1))) * 20.0, 100.0);
}

/// Success bonus: 100 for exit 0, 20 otherwise.
pub fn successScore(exit_status: u8) f64 {
    return if (exit_status == 0) 100.0 else 20.0;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

const TestContext = struct {
    hist: History,

    fn init() TestContext {
        return .{ .hist = History.init(testing.allocator) };
    }

    fn deinit(self: *TestContext) void {
        self.hist.deinit();
    }

    fn add(self: *TestContext, cmd: []const u8, cwd: []const u8, status: u8) bool {
        return self.hist.add(.{ .command = cmd, .cwd = cwd, .exit_status = status });
    }
};

// -----------------------------------------------------------------------------
// Adding Entries
// -----------------------------------------------------------------------------

test "History: adding entries" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Stores command with metadata
    try testing.expect(ctx.add("echo hello", "/home", 0));
    try testing.expect(ctx.add("ls -la", "/home", 0));
    try testing.expectEqual(@as(usize, 2), ctx.hist.count());
    try testing.expectEqualStrings("echo hello", ctx.hist.get(0).?.command);
    try testing.expectEqualStrings("/home", ctx.hist.get(0).?.cwd);

    // Ignores empty commands
    try testing.expect(!ctx.add("", "/home", 0));
}

test "History: deduplication moves entry to end" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Duplicate command+cwd increments frequency and moves to end
    _ = ctx.add("git status", "/project", 0);
    _ = ctx.add("other cmd", "/project", 0);
    _ = ctx.add("git status", "/project", 0); // should move to end
    try testing.expectEqual(@as(usize, 2), ctx.hist.count());
    try testing.expectEqual(@as(u32, 2), ctx.hist.get(1).?.frequency); // now at index 1
    try testing.expectEqualStrings("git status", ctx.hist.get(1).?.command);
    try testing.expectEqualStrings("other cmd", ctx.hist.get(0).?.command);

    // Same command in different cwd creates separate entries
    _ = ctx.add("make", "/project-a", 0);
    _ = ctx.add("make", "/project-b", 0);
    try testing.expectEqual(@as(usize, 4), ctx.hist.count());
}

test "History: duplicate updates exit status" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Add command with failure status
    _ = ctx.add("make build", "/project", 1);
    try testing.expectEqual(@as(u8, 1), ctx.hist.get(0).?.exit_status);

    // Re-run same command with success - should update exit status (entry moves to end)
    _ = ctx.add("make build", "/project", 0);
    try testing.expectEqual(@as(usize, 1), ctx.hist.count()); // still one entry
    try testing.expectEqual(@as(u8, 0), ctx.hist.get(0).?.exit_status); // updated
}

test "History: repeated command always appears at end for up-arrow" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Simulate: run clear, then other commands, then clear again
    _ = ctx.add("clear", "/home", 0);
    _ = ctx.add("ls", "/home", 0);
    _ = ctx.add("git status", "/home", 0);
    _ = ctx.add("clear", "/home", 0); // should move to end

    // INVARIANT: Editor.historyPrev walks entries backward from the end.
    // So entries[len-1] is shown on first up-arrow, entries[len-2] on second, etc.
    // This test ensures duplicate commands are moved to the end, so up-arrow
    // shows them immediately after re-running (not buried at their original index).
    try testing.expectEqual(@as(usize, 3), ctx.hist.count());
    try testing.expectEqualStrings("clear", ctx.hist.get(2).?.command); // 1st up-arrow
    try testing.expectEqualStrings("git status", ctx.hist.get(1).?.command); // 2nd up-arrow
    try testing.expectEqualStrings("ls", ctx.hist.get(0).?.command); // 3rd up-arrow
}

// -----------------------------------------------------------------------------
// Scoring
// -----------------------------------------------------------------------------

test "Scoring: recency score decays over time" {
    const now: i64 = 1000000;

    // Just now = ~100
    try testing.expect(recencyScore(now, now) > 99.0);

    // One week ago = ~50 (half-life)
    const week_ago = now - 7 * 24 * 60 * 60;
    const week_score = recencyScore(week_ago, now);
    try testing.expect(week_score > 45.0 and week_score < 55.0);

    // Two weeks ago = ~25
    const two_weeks_ago = now - 14 * 24 * 60 * 60;
    const two_week_score = recencyScore(two_weeks_ago, now);
    try testing.expect(two_week_score > 20.0 and two_week_score < 30.0);
}

test "Scoring: recency and frequency" {
    const now = std.time.timestamp();
    const week_ago = now - 7 * 24 * 60 * 60;

    // Recent beats old at same frequency
    const recent: Entry = .{ .command = "a", .cwd = "/", .timestamp = now, .exit_status = 0, .frequency = 1 };
    const old: Entry = .{ .command = "b", .cwd = "/", .timestamp = week_ago, .exit_status = 0, .frequency = 1 };
    try testing.expect(scoreEntry(&recent, null, now) > scoreEntry(&old, null, now));

    // High frequency compensates for age
    const old_frequent: Entry = .{ .command = "a", .cwd = "/", .timestamp = week_ago, .exit_status = 0, .frequency = 100 };
    const old_rare: Entry = .{ .command = "b", .cwd = "/", .timestamp = week_ago, .exit_status = 0, .frequency = 1 };
    try testing.expect(scoreEntry(&old_frequent, null, now) > scoreEntry(&old_rare, null, now));
}

test "Scoring: directory relevance" {
    const cases = [_]struct { []const u8, []const u8, f64 }{
        .{ "/home/user", "/home/user", 100.0 }, // exact match
        .{ "/home", "/home/user/project", 50.0 }, // ancestor
        .{ "/home/user/project", "/home/user", 30.0 }, // child
        .{ "/projects/app-a", "/projects/app-b", 20.0 }, // sibling
        .{ "/var/log", "/home/user", 0.0 }, // unrelated
    };
    for (cases) |c| try testing.expectEqual(c[2], directoryScore(c[0], c[1]));
}

test "Scoring: sibling directory detection" {
    try testing.expect(areSiblingDirectories("/projects/a", "/projects/b"));
    try testing.expect(areSiblingDirectories("/home/user/proj1", "/home/user/proj2"));
    try testing.expect(!areSiblingDirectories("/projects/a", "/other/b"));
    try testing.expect(!areSiblingDirectories("/", "/home"));
    try testing.expect(!areSiblingDirectories("/a", "/b")); // both under root, but root is special
}

test "Scoring: helper functions" {
    // Parent directory extraction
    const parent_cases = [_]struct { []const u8, []const u8 }{
        .{ "/home/user/projects", "/home/user" },
        .{ "/home", "/" },
        .{ "/", "" },
        .{ "/a/b/c/", "/a/b" },
    };
    for (parent_cases) |c| try testing.expectEqualStrings(c[1], parentDirectory(c[0]));

    // Frequency scoring
    try testing.expectEqual(@as(f64, 0.0), frequencyScore(0));
    try testing.expectEqual(@as(f64, 20.0), frequencyScore(1));
    try testing.expectEqual(@as(f64, 40.0), frequencyScore(3));

    // Success scoring
    try testing.expectEqual(@as(f64, 100.0), successScore(0));
    try testing.expectEqual(@as(f64, 20.0), successScore(1));
    try testing.expectEqual(@as(f64, 20.0), successScore(127));
}

// -----------------------------------------------------------------------------
// Persistence
// -----------------------------------------------------------------------------

test "History: save and load preserves entries" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    _ = ctx.add("echo test", "/tmp", 0);
    _ = ctx.add("ls", "/home", 1);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const file = try tmp.dir.createFile("history", .{});
    file.close();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try tmp.dir.realpath("history", &buf);

    ctx.hist.save(path);

    var ctx2 = TestContext.init();
    defer ctx2.deinit();
    ctx2.hist.load(path);

    try testing.expectEqual(@as(usize, 2), ctx2.hist.count());
    try testing.expectEqualStrings("echo test", ctx2.hist.get(0).?.command);
    try testing.expectEqualStrings("/tmp", ctx2.hist.get(0).?.cwd);
    try testing.expectEqual(@as(u8, 1), ctx2.hist.get(1).?.exit_status);
}
