//! alias builtin - define command aliases

const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");

const spec = args.Spec("alias", .{
    .desc = "Define or list command aliases.",
    .args = .{
        .name = args.StringPositional(.{ .desc = "Alias name", .default = "" }),
        .expansion = args.Rest(.{ .desc = "Command expansion" }),
    },
    .examples = &.{
        "alias              # List all aliases",
        "alias ll           # Show 'll' alias",
        "alias ll ls -la    # Define 'll' as 'ls -la'",
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    // No args: list all aliases
    if (r.name.len == 0) {
        var iter = state.aliases.iterator();
        while (iter.next()) |entry| {
            builtins.io.printStdout("alias {s} '{s}'\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
        return 0;
    }

    // Name only: show single alias
    if (r.expansion.len == 0) {
        if (state.getAlias(r.name)) |expansion| {
            builtins.io.printStdout("alias {s} '{s}'\n", .{ r.name, expansion });
            return 0;
        }
        builtins.io.printError("alias: {s}: not found\n", .{r.name});
        return 1;
    }

    // Name + expansion: define alias
    const expansion = builtins.joinArgs(state.allocator, r.expansion) catch {
        return builtins.reportOOM("alias");
    };
    defer state.allocator.free(expansion);

    state.setAlias(r.name, expansion) catch {
        return builtins.reportOOM("alias");
    };

    return 0;
}
