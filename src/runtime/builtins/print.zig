//! print builtin - echo with inline color support
//!
//! Behaves like echo for plain text, but supports color flags that can be
//! interleaved with text arguments. Automatically resets color at end.
//! Uses buffered output for minimal syscalls.
//!
//! Usage: print [-n] [--color]... [text]...
//! Examples:
//!   print --green "success"                     # Green text, auto-reset
//!   print --bold --red "error" --reset "normal" # Mixed styles
//!   print -n --blue "status: "                  # No newline, still resets
//!   print --nl --yellow "section"               # Extra newline before text

const std = @import("std");
const builtins = @import("../builtins.zig");
const ansi = @import("../../terminal/ansi.zig");
const Writer = @import("../../terminal/io.zig").Writer;

pub const builtin = builtins.Builtin{
    .name = "print",
    .run = run,
    .help = "print [-n] [--color]... [text]... - Print with colors (--nl, --green, --red, --yellow, --blue, --magenta, --purple, --cyan, --gray, --bold, --dim, --reset)",
};

/// O(1) lookup table for color/style flags
const styles = std.StaticStringMap([]const u8).initComptime(.{
    // Formatting
    .{ "--nl", "\r\n" },
    // Colors
    .{ "--red", ansi.red },
    .{ "--green", ansi.green },
    .{ "--yellow", ansi.yellow },
    .{ "--blue", ansi.blue },
    .{ "--magenta", ansi.magenta },
    .{ "--purple", ansi.magenta }, // alias
    .{ "--cyan", ansi.cyan },
    .{ "--gray", ansi.gray },
    // Styles
    .{ "--bold", ansi.bold },
    .{ "--dim", ansi.dim },
    .{ "--reset", ansi.reset },
});

fn run(_: *builtins.State, cmd: builtins.ExpandedCmd) u8 {
    var args = cmd.argv[1..];
    var newline = true;
    var need_space = false;

    // Check for -n flag (must be first, like echo)
    if (args.len > 0 and std.mem.eql(u8, args[0], "-n")) {
        newline = false;
        args = args[1..];
    }

    var w = Writer{};

    for (args) |arg| {
        if (styles.get(arg)) |code| {
            w.write(code);
        } else {
            if (need_space) w.writeByte(' ');
            w.writeEscaped(arg);
            need_space = true;
        }
    }

    // Always reset to prevent color bleed
    w.write(ansi.reset);
    if (newline) w.writeByte('\n');

    w.flush();
    return 0;
}
