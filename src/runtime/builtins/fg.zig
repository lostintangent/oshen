//! fg builtin - bring job to foreground
//!
//! NOTE: No unit or E2E tests - requires TTY and real process groups.
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const signals = @import("../../interpreter/execution/signals.zig");

const spec = args.Spec("fg", .{
    .desc = "Bring a job to the foreground.",
    .args = .{
        .job = args.StringPositional(.{ .desc = "Job ID", .default = "" }),
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    const job_id = state.jobs.resolveJob(r.job, "fg", false) orelse return 1;
    return signals.continueJobForeground(state, job_id);
}
