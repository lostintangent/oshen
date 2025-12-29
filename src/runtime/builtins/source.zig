//! source builtin - execute commands from a file
const std = @import("std");
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const interpreter = @import("../../interpreter/interpreter.zig");

const spec = args.Spec("source", .{
    .desc = "Execute commands from a file.",
    .args = .{
        .file = args.StringPositional(.{ .desc = "Script file to execute" }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    const path = r.file;

    // Expand tilde if present
    var expanded_path: []const u8 = path;
    var needs_free = false;

    if (path.len > 0 and path[0] == '~') {
        if (state.home) |home| {
            expanded_path = std.fmt.allocPrint(state.allocator, "{s}{s}", .{ home, path[1..] }) catch {
                builtins.io.writeStderr("source: out of memory\n");
                return 1;
            };
            needs_free = true;
        }
    }
    defer if (needs_free) state.allocator.free(expanded_path);

    return interpreter.executeFile(state.allocator, state, expanded_path) catch |err| {
        builtins.io.printError("source: {s}: {}\n", .{ expanded_path, err });
        return 1;
    };
}
