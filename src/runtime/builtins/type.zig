//! type builtin - show how a command would be interpreted
const builtins = @import("../builtins.zig");
const resolve = @import("../resolve.zig");
const args = @import("../../terminal/args.zig");

const spec = args.Spec("type", .{
    .desc = "Show how a command would be interpreted.",
    .args = .{
        .names = args.Rest(.{ .desc = "Command names to look up", .required = true }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    var status: u8 = 0;
    for (r.names) |name| {
        if (!showType(state, name)) {
            status = 1;
        }
    }
    return status;
}

fn showType(state: *builtins.State, name: []const u8) bool {
    switch (resolve.resolveCommand(state, name, true)) {
        .alias => |expansion| {
            builtins.io.printStdout("{s} is aliased to '{s}'\n", .{ name, expansion });
            return true;
        },
        .builtin => {
            builtins.io.printStdout("{s} is a builtin\n", .{name});
            return true;
        },
        .function => {
            builtins.io.printStdout("{s} is a function\n", .{name});
            return true;
        },
        .external => |path| {
            builtins.io.printStdout("{s} is {s}\n", .{ name, path });
            return true;
        },
        .not_found => {
            builtins.io.printError("type: {s}: not found\n", .{name});
            return false;
        },
    }
}
