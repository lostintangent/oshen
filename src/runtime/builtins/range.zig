//! range builtin - print a sequence of numbers

const std = @import("std");
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");
const Writer = @import("../../terminal/io.zig").Writer;

/// Command spec: single source of truth for parsing AND help text
const spec = args.Spec("range", .{
    .desc = "Print a sequence of numbers. If FIRST > LAST, counts down automatically.",
    .args = .{
        .step = args.I64Option(.{ .short = "s", .long = "step", .desc = "Increment (default: 1 or -1)", .default = 1 }),
        .separator = args.StringOption(.{ .short = "S", .long = "separator", .desc = "Output separator", .default = "\n" }),
        .first = args.I64Positional(.{ .desc = "Starting number", .default = 1 }),
        .last = args.I64Positional(.{ .desc = "Ending number (inclusive)" }),
    },
    .examples = &.{
        "range 5              # 1, 2, 3, 4, 5",
        "range 3 7            # 3, 4, 5, 6, 7",
        "range 10 1           # 10, 9, 8, ..., 1",
        "range --step 2 1 10  # 1, 3, 5, 7, 9",
        "range -S ', ' 1 3    # 1, 2, 3",
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(_: *builtins.State, r: spec.Result) u8 {
    if (r.step == 0) {
        builtins.io.writeStderr("range: step cannot be zero\n");
        return 1;
    }

    // Auto-detect direction: if first > last and step is positive (default), negate it
    const step = if (r.first > r.last and r.step > 0) -r.step else r.step;

    // Check for impossible ranges
    if ((r.first < r.last and step < 0) or (r.first > r.last and step > 0)) return 0;

    // Output sequence
    var w = Writer{};
    var is_first = true;
    var n = r.first;
    while (if (step > 0) n <= r.last else n >= r.last) {
        if (!is_first) w.write(r.separator);
        is_first = false;
        w.writeSignedInt(n);
        // Use checked arithmetic to prevent overflow on extreme values
        n = std.math.add(i64, n, step) catch break;
    }
    w.flush();
    return 0;
}
