//! export builtin - export environment variables

const std = @import("std");
const builtins = @import("../builtins.zig");
const Args = @import("../../terminal/args.zig");

const spec = Args.Spec("export", .{
    .desc = "Export environment variables.",
    .args = .{
        .args = Args.Rest(.{ .desc = "NAME, NAME=VALUE, or NAME VALUE" }),
    },
    .examples = &.{
        "export              # List all exports",
        "export PATH         # Export existing variable",
        "export FOO=bar      # Export with value",
        "export FOO bar      # Export with value (alt)",
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    // No args: list all exports
    if (r.args.len == 0) {
        var iter = state.exports.iterator();
        while (iter.next()) |entry| {
            builtins.io.printStdout("{s}={s}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
        return 0;
    }

    // Check if first arg contains '=' (export foo=bar syntax)
    if (std.mem.indexOfScalar(u8, r.args[0], '=') != null) {
        for (r.args) |arg| {
            const result = exportVar(state, arg, null);
            if (result != 0) return result;
        }
    } else {
        // export NAME [VALUE] syntax
        if (r.args.len == 1) {
            return exportVar(state, r.args[0], null);
        } else if (r.args.len == 2) {
            return exportVar(state, r.args[0], r.args[1]);
        } else {
            builtins.io.writeStderr("export: too many arguments\n");
            return 1;
        }
    }

    return 0;
}

fn exportVar(state: *builtins.State, arg: []const u8, separate_value: ?[]const u8) u8 {
    // Parse NAME=VALUE or just NAME
    const eq_pos = std.mem.indexOfScalar(u8, arg, '=');
    const name = if (eq_pos) |pos| arg[0..pos] else arg;
    const value = if (separate_value) |v| v else if (eq_pos) |pos| arg[pos + 1 ..] else blk: {
        // Just NAME: get existing value from shell var or environment
        if (state.getVar(name)) |v| break :blk v;
        if (builtins.env.get(name)) |v| break :blk v;
        builtins.io.printError("export: {s}: not set\n", .{name});
        return 1;
    };

    // Store in exports map (freeing old entry if exists)
    if (state.exports.fetchRemove(name)) |old| {
        state.freeStringEntry(old);
    }

    const key = state.allocator.dupe(u8, name) catch return builtins.reportOOM("export");
    const val = state.allocator.dupe(u8, value) catch return builtins.reportOOM("export");
    state.exports.put(key, val) catch return builtins.reportOOM("export");

    // Set in actual environment
    builtins.env.set(state.allocator, name, value) catch return builtins.reportOOM("export");

    return 0;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;
const State = @import("../state.zig").State;

fn runExport(state: *State, args: []const []const u8) u8 {
    const r = spec.parse(args) catch return 1;
    return run(state, r);
}

fn expectExported(state: *State, name: []const u8, expected: []const u8) !void {
    const exported = state.exports.get(name);
    try testing.expect(exported != null);
    try testing.expectEqualStrings(expected, exported.?);

    const env_value = builtins.env.get(name);
    try testing.expect(env_value != null);
    try testing.expectEqualStrings(expected, env_value.?);
}

// -----------------------------------------------------------------------------
// Export: syntax variants
// -----------------------------------------------------------------------------

test "Export: syntax variants" {
    // NAME=VALUE syntax
    {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_TEST_VAR=testvalue" }));
        try expectExported(&state, "OSHEN_TEST_VAR", "testvalue");
    }
    // NAME=VALUE with empty value
    {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_EMPTY=" }));
        try expectExported(&state, "OSHEN_EMPTY", "");
    }
    // Multiple NAME=VALUE pairs
    {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_MULTI_A=aval", "OSHEN_MULTI_B=bval" }));
        try expectExported(&state, "OSHEN_MULTI_A", "aval");
        try expectExported(&state, "OSHEN_MULTI_B", "bval");
    }
    // Space-separated NAME VALUE syntax
    {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_SPACE_VAR", "spacevalue" }));
        try expectExported(&state, "OSHEN_SPACE_VAR", "spacevalue");
    }
}

// -----------------------------------------------------------------------------
// Export: existing variables and overwriting
// -----------------------------------------------------------------------------

test "Export: existing variables and overwriting" {
    // Export existing shell variable
    {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try state.setVar("OSHEN_SHELLVAR", "shellvalue");
        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_SHELLVAR" }));
        try expectExported(&state, "OSHEN_SHELLVAR", "shellvalue");
    }
    // Overwriting existing export
    {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_OVERWRITE=first" }));
        try expectExported(&state, "OSHEN_OVERWRITE", "first");
        try testing.expectEqual(@as(u8, 0), runExport(&state, &.{ "OSHEN_OVERWRITE=second" }));
        try expectExported(&state, "OSHEN_OVERWRITE", "second");
    }
}

// -----------------------------------------------------------------------------
// Export: error cases
// -----------------------------------------------------------------------------

test "Export: error cases" {
    const cases = .{
        .{ &[_][]const u8{"OSHEN_NONEXISTENT_67890"}, @as(u8, 1) }, // nonexistent variable
        .{ &[_][]const u8{ "OSHEN_TOO_MANY", "value", "extra" }, @as(u8, 1) }, // too many arguments
    };
    inline for (cases) |case| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var state = State.init(arena.allocator());
        state.initCurrentScope();
        defer state.deinit();

        try testing.expectEqual(case[1], runExport(&state, case[0]));
    }
}
