//! false builtin - always fails
const builtins = @import("../builtins.zig");

pub const builtin = builtins.Builtin{
    .name = "false",
    .run = run,
    .help = "false - Return failure (exit status 1)",
};

fn run(_: *builtins.State, _: builtins.ExpandedCommand) u8 {
    return 1;
}
