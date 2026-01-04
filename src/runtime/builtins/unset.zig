//! unset builtin - remove shell variables and environment variables
const builtins = @import("../builtins.zig");
const env = @import("../env.zig");
const args = @import("../../terminal/args.zig");

const spec = args.Spec("unset", .{
    .desc = "Remove shell variables and environment variables.",
    .args = .{
        .names = args.Rest(.{ .desc = "Variable names to unset", .required = true }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    for (r.names) |name| {
        state.unsetVar(name);
        env.unset(state.allocator, name) catch {};
    }

    return 0;
}
