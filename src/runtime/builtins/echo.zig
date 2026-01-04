//! echo builtin - print arguments with escape sequence support

const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const Writer = @import("../../terminal/io.zig").Writer;

const spec = args.Spec("echo", .{
    .desc = "Print arguments to stdout. Supports escape sequences (\\n, \\t, \\e for ESC).",
    .args = .{
        .n = args.Flag(.{ .short = "n", .long = "n", .desc = "No trailing newline" }),
        .values = args.Rest(.{ .desc = "Values to print" }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(_: *builtins.State, r: spec.Result) u8 {
    var w = Writer{};
    for (r.values, 0..) |val, i| {
        if (i > 0) w.writeByte(' ');
        w.writeEscaped(val);
    }
    if (!r.n) w.writeByte('\n');
    w.flush();
    return 0;
}
