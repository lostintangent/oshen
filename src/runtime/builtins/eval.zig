//! eval builtin - execute a string as shell code

const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const interpreter = @import("../../interpreter/interpreter.zig");

const spec = args.Spec("eval", .{
    .desc = "Execute arguments as shell code.",
    .args = .{
        .code = args.Rest(.{ .desc = "Code to execute" }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    // eval with no args succeeds (like bash)
    if (r.code.len == 0) return 0;

    const code = builtins.joinArgs(state.allocator, r.code) catch {
        return builtins.reportOOM("eval");
    };
    defer state.allocator.free(code);

    return interpreter.execute(state.allocator, state, code) catch |err| {
        builtins.io.printError("eval: {}\n", .{err});
        return 1;
    };
}
