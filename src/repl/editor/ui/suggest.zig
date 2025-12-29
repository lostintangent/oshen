//! Suggest: context-aware autosuggestions for the line editor.
//!
//! Provides fish-style autosuggestions using the unified scoring from history.zig.
//! See that module for scoring weights and algorithm details.

const std = @import("std");
const history = @import("../history.zig");
const History = history.History;

/// Find the best suggestion for the given input.
/// Returns the suffix to append (not the full command) or null if none found.
pub fn fromHistory(input: []const u8, hist: *const History, cwd: []const u8) ?[]const u8 {
    if (input.len == 0) return null;

    const now = std.time.timestamp();
    var best: ?[]const u8 = null;
    var best_score: f64 = -1;

    for (hist.entries.items) |e| {
        if (e.command.len <= input.len) continue;
        if (!std.mem.startsWith(u8, e.command, input)) continue;

        const score = history.scoreEntry(&e, cwd, now);
        if (score >= best_score) {
            best_score = score;
            best = e.command[input.len..];
        }
    }

    return best;
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

    fn add(self: *TestContext, cmd: []const u8, cwd: []const u8, status: u8) void {
        _ = self.hist.add(.{ .command = cmd, .cwd = cwd, .exit_status = status });
    }

    fn suggest(self: *const TestContext, input: []const u8, cwd: []const u8) ?[]const u8 {
        return fromHistory(input, &self.hist, cwd);
    }
};

// -----------------------------------------------------------------------------
// Matching
// -----------------------------------------------------------------------------

test "Suggestions: basic matching" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    ctx.add("git commit -m 'test'", "/", 0);
    try testing.expectEqualStrings("mmit -m 'test'", ctx.suggest("git co", "/").?);

    // Most recent wins when multiple matches
    ctx.add("echo hello", "/", 0);
    ctx.add("echo world", "/", 0);
    try testing.expectEqualStrings(" world", ctx.suggest("echo", "/").?);
}

test "Suggestions: no match cases" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    ctx.add("echo hello", "/", 0);

    try testing.expect(ctx.suggest("xyz", "/") == null); // no prefix match
    try testing.expect(ctx.suggest("echo hello", "/") == null); // exact match
    try testing.expect(ctx.suggest("", "/") == null); // empty input

    var empty = TestContext.init();
    defer empty.deinit();
    try testing.expect(empty.suggest("echo", "/") == null); // empty history
}

// -----------------------------------------------------------------------------
// Ranking
// -----------------------------------------------------------------------------

test "Suggestions: ranking preferences" {
    // Prefers current directory
    var ctx1 = TestContext.init();
    defer ctx1.deinit();
    ctx1.add("make test", "/project", 0);
    ctx1.add("make build", "/other", 0);
    try testing.expectEqualStrings(" test", ctx1.suggest("make", "/project").?);

    // Prefers successful over failed
    var ctx2 = TestContext.init();
    defer ctx2.deinit();
    ctx2.add("make success", "/", 0);
    ctx2.add("make failed", "/", 1);
    try testing.expectEqualStrings(" success", ctx2.suggest("make", "/").?);

    // Prefers frequent over rare
    var ctx3 = TestContext.init();
    defer ctx3.deinit();
    ctx3.add("npm run rare", "/", 0);
    ctx3.add("npm run frequent", "/", 0);
    ctx3.add("npm run frequent", "/", 0);
    ctx3.add("npm run frequent", "/", 0);
    try testing.expectEqualStrings(" frequent", ctx3.suggest("npm run", "/").?);
}
