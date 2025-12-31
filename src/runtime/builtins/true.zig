//! true builtin - always succeeds
const builtins = @import("../builtins.zig");

pub const builtin = builtins.Builtin{
    .name = "true",
    .run = run,
    .help = "true - Return success (exit status 0)",
};

fn run(_: *builtins.State, _: builtins.ExpandedCommand) u8 {
    return 0;
}
