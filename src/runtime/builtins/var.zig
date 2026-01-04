//! var/set builtin - get or set shell variables
const std = @import("std");
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const ScopeValue = @import("../scope.zig").Value;

const spec = args.Spec("var", .{
    .desc = "Get or set shell variables.",
    .args = .{
        .name = args.StringPositional(.{ .desc = "Variable name", .default = "" }),
        .values = args.Rest(.{ .desc = "Values to assign" }),
    },
    .examples = &.{
        "var              # List all variables",
        "var x            # Show value of x",
        "var x hello      # Set x to 'hello'",
        "var x = hello    # Set x to 'hello' (with =)",
        "var x a b c      # Set x to list (a b c)",
    },
});

pub const builtin = builtins.fromSpec(spec, run);
pub const set_builtin = builtins.alias(builtin, "set");

fn run(state: *builtins.State, r: spec.Result) u8 {
    // No args: list all variables
    if (r.name.len == 0) {
        var iter = state.global_scope.iterator();
        while (iter.next()) |entry| {
            builtins.io.printStdout("{s} = ", .{entry.key_ptr.*});
            printValue(entry.value_ptr.*);
            builtins.io.writeStdout("\n");
        }
        return 0;
    }

    // Skip optional '=' after variable name
    const values = if (r.values.len > 0 and std.mem.eql(u8, r.values[0], "="))
        r.values[1..]
    else
        r.values;

    // Name only: show single variable
    if (values.len == 0) {
        if (state.getVar(r.name)) |value| {
            builtins.io.writeStdout(value);
            builtins.io.writeStdout("\n");
            return 0;
        }
        return 1;
    }

    // Name + values: set variable
    state.setVarList(r.name, values) catch |err| {
        builtins.io.printError("var: {}\n", .{err});
        return 1;
    };
    return 0;
}

fn printValue(value: ScopeValue) void {
    switch (value) {
        .scalar => |s| builtins.io.writeStdout(s),
        .list => |list| {
            for (list, 0..) |v, i| {
                if (i > 0) builtins.io.writeStdout(" ");
                builtins.io.writeStdout(v);
            }
        },
    }
}
