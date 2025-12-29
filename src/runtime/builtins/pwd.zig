const std = @import("std");
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");

const spec = args.Spec("pwd", .{
    .desc = "Print current working directory.",
    .args = .{
        .tilde = args.Flag(.{ .short = "t", .long = "tilde", .desc = "Replace $HOME with ~" }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    const cwd = state.getCwd() catch |err| {
        builtins.io.printError("pwd: {}\n", .{err});
        return 1;
    };

    if (r.tilde) {
        if (builtins.env.getHome()) |home| {
            if (std.mem.startsWith(u8, cwd, home)) {
                builtins.io.writeStdout("~");
                builtins.io.writeStdout(cwd[home.len..]);
                builtins.io.writeStdout("\n");
                return 0;
            }
        }
    }

    builtins.io.writeStdout(cwd);
    builtins.io.writeStdout("\n");
    return 0;
}
