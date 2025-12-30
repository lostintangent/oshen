//! bg builtin - continue job in background
//!
//! NOTE: No unit or E2E tests - requires TTY and real process groups.
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const signals = @import("../../interpreter/execution/signals.zig");

const spec = args.Spec("bg", .{
    .desc = "Continue a stopped job in the background.",
    .args = .{
        .job = args.StringPositional(.{ .desc = "Job ID", .default = "" }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    const job_id = state.jobs.resolveJob(r.job, "bg", true) orelse return 1;
    return signals.continueJobBackground(state, job_id);
}
