const std = @import("std");
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const tui = @import("../../terminal/tui.zig");

const spec = args.Spec("cd", .{
    .desc = "Change current directory.",
    .args = .{
        .dir = args.StringPositional(.{ .desc = "Target directory", .default = "" }),
    },
    .examples = &.{
        "cd           # Go to $HOME",
        "cd /tmp      # Go to /tmp",
        "cd -         # Go to previous directory",
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    // Determine target directory
    const target = if (r.dir.len == 0) state.home orelse {
        builtins.io.writeStderr("cd: HOME not set\n");
        return 1;
    } else if (std.mem.eql(u8, r.dir, "-")) state.prev_cwd orelse {
        builtins.io.writeStderr("cd: OLDPWD not set\n");
        return 1;
    } else r.dir;

    state.chdir(target) catch |err| {
        builtins.io.printError("cd: {s}: {}\n", .{ target, err });
        return 1;
    };

    // Emit OSC 7 to notify terminal of new working directory (only in interactive mode)
    if (state.interactive) {
        if (state.getCwd() catch null) |cwd| {
            tui.emitOsc7(cwd);
        }
    }

    return 0;
}
