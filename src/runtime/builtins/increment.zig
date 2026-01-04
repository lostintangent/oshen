//! increment builtin - increment or decrement a variable's value

const std = @import("std");
const builtins = @import("../builtins.zig");
const Args = @import("../../terminal/args.zig");

const spec = Args.Spec("increment", .{
    .desc = "Increment or decrement a variable's numeric value.",
    .args = .{
        .by = Args.I64Option(.{ .short = "b", .long = "by", .desc = "Amount to increment", .default = 1 }),
        .varname = Args.StringPositional(.{ .desc = "Variable to increment" }),
    },
    .examples = &.{
        "increment count         # count = count + 1",
        "increment --by 5 count  # count = count + 5",
        "increment --by -3 count # count = count - 3",
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    const current_str = state.getVar(r.varname) orelse {
        builtins.io.printError("increment: variable '{s}' does not exist\n", .{r.varname});
        return 1;
    };

    const current_val = std.fmt.parseInt(i64, current_str, 10) catch {
        builtins.io.printError("increment: '{s}' is not a number\n", .{r.varname});
        return 1;
    };

    const new_val = std.math.add(i64, current_val, r.by) catch {
        builtins.io.printError("increment: integer overflow\n", .{});
        return 1;
    };

    var buf: [32]u8 = undefined;
    const new_str = std.fmt.bufPrint(&buf, "{d}", .{new_val}) catch unreachable;

    state.setVar(r.varname, new_str) catch {
        builtins.io.printError("increment: out of memory\n", .{});
        return 1;
    };

    return 0;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;
const State = @import("../state.zig").State;

fn runIncrement(state: *State, args: []const []const u8) u8 {
    const r = spec.parse(args) catch return 1;
    return run(state, r);
}

// -----------------------------------------------------------------------------
// Basic Operations (table-driven)
// -----------------------------------------------------------------------------

test "increment: basic operations" {
    const cases = [_]struct {
        initial: []const u8,
        args: []const []const u8,
        expected: []const u8,
    }{
        // Increment by 1 (default)
        .{ .initial = "5", .args = &.{"count"}, .expected = "6" },
        // Increment from zero
        .{ .initial = "0", .args = &.{"count"}, .expected = "1" },
        // Increment negative number
        .{ .initial = "-5", .args = &.{"count"}, .expected = "-4" },
        // Custom increment amount
        .{ .initial = "10", .args = &.{ "--by", "5", "count" }, .expected = "15" },
        // Decrement with negative value
        .{ .initial = "10", .args = &.{ "--by", "-3", "count" }, .expected = "7" },
        // Large increment
        .{ .initial = "0", .args = &.{ "--by", "100", "count" }, .expected = "100" },
        // Short flag
        .{ .initial = "1", .args = &.{ "-b", "9", "count" }, .expected = "10" },
    };

    for (cases) |case| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try state.setVar("count", case.initial);
        try testing.expectEqual(@as(u8, 0), runIncrement(&state, case.args));
        try testing.expectEqualStrings(case.expected, state.getVar("count").?);
    }
}

// -----------------------------------------------------------------------------
// Error Cases (table-driven)
// -----------------------------------------------------------------------------

test "increment: error cases" {
    const cases = [_]struct {
        setup_var: ?[]const u8, // null = don't set any variable
        setup_val: []const u8,
        args: []const []const u8,
    }{
        // Nonexistent variable
        .{ .setup_var = null, .setup_val = "", .args = &.{"nonexistent"} },
        // Non-numeric variable
        .{ .setup_var = "text", .setup_val = "hello", .args = &.{"text"} },
        // Invalid --by value
        .{ .setup_var = "count", .setup_val = "5", .args = &.{ "--by", "abc", "count" } },
        // Missing --by value
        .{ .setup_var = "count", .setup_val = "5", .args = &.{"--by"} },
        // Too many arguments
        .{ .setup_var = "count", .setup_val = "5", .args = &.{ "--by", "1", "count", "extra" } },
        // Invalid option
        .{ .setup_var = "count", .setup_val = "5", .args = &.{ "--invalid", "1", "count" } },
        // Missing variable name
        .{ .setup_var = null, .setup_val = "", .args = &.{} },
        // Empty variable value (not a number)
        .{ .setup_var = "count", .setup_val = "", .args = &.{"count"} },
    };

    for (cases) |case| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        if (case.setup_var) |name| {
            try state.setVar(name, case.setup_val);
        }
        try testing.expectEqual(@as(u8, 1), runIncrement(&state, case.args));
    }
}
