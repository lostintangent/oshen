//! print builtin - echo with inline color support
//!
//! Behaves like echo for plain text, but supports color flags that can be
//! interleaved with text arguments. Automatically resets color at end.
//! Uses buffered output for minimal syscalls.
//!
//! NOTE: Does not use Args API - simple loop over args with StaticStringMap lookup
//! is clearer and more direct than Args wrapper for this use case.

const std = @import("std");
const builtins = @import("../builtins.zig");
const binding = @import("../binding.zig");
const ansi = @import("../../terminal/ansi.zig");
const Writer = @import("../../terminal/io.zig").Writer;

pub const builtin = builtins.Builtin{
    .name = "print",
    .run = run,
    .help =
    \\print [-n] [--color]... [text|@varname]...
    \\
    \\Print text with inline color/style support. Behaves like echo but supports
    \\ANSI color codes via flags that can be mixed with text arguments.
    \\Automatically resets formatting at end to prevent color bleed.
    \\
    \\Options:
    \\  -n          Omit trailing newline (like echo -n)
    \\
    \\Variable references:
    \\  @varname    Directly read variable value (efficient for large strings)
    \\
    \\Colors:
    \\  --red       Red text
    \\  --green     Green text
    \\  --yellow    Yellow text
    \\  --blue      Blue text
    \\  --cyan      Cyan text
    \\  --magenta   Magenta text
    \\  --purple    Alias for magenta
    \\  --gray      Gray text
    \\
    \\Styles:
    \\  --bold      Bold/bright text
    \\  --dim       Dim/faint text
    \\  --reset     Reset to default (useful mid-output)
    \\
    \\Formatting:
    \\  --nl        Emit a newline (useful for spacing)
    \\
    \\Examples:
    \\  print --green "success"                    # Green text, auto-reset
    \\  print --bold --red "error" --reset normal  # Mixed styles
    \\  print -n --blue "status: "                 # No newline, color resets
    \\  print --nl --yellow "Section"              # Blank line before output
    \\  print @message                             # Print variable directly
    \\  print --green @status --reset "complete"   # Mix variables and colors
    ,
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

fn run(state: *builtins.State, in_args: []const []const u8) u8 {
    var args = in_args;
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
            // Resolve @varname bindings to their values
            const text = binding.resolveBindableArg(state, "print", arg) orelse return 1;

            if (need_space) w.writeByte(' ');
            w.writeEscaped(text);
            need_space = true;
        }
    }

    // Always reset to prevent color bleed
    w.write(ansi.reset);
    if (newline) w.writeByte('\n');

    w.flush();
    return 0;
}
