//! unalias builtin - remove command aliases
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");

const spec = args.Spec("unalias", .{
    .desc = "Remove command aliases.",
    .args = .{
        .names = args.Rest(.{ .desc = "Alias names to remove", .required = true }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    for (r.names) |name| {
        state.unsetAlias(name);
    }
    return 0;
}
