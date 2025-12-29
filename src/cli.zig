const build_options = @import("build_options");
const args = @import("terminal/args.zig");
const io = @import("terminal/io.zig");

pub const RunMode = union(enum) {
    interactive,
    command: []const u8,
    script: []const u8,
};

pub const Options = struct {
    mode: RunMode = .interactive,
    is_login: bool = false,
};

/// CLI argument spec - single source of truth for parsing and help
const spec = args.Spec("oshen", .{
    .usage = "oshen [OPTIONS] [SCRIPT]",
    .desc = "Oshen Shell (v" ++ build_options.version ++ ")",
    .args = .{
        .help = args.Flag(.{ .short = "h", .long = "help", .desc = "Show this help" }),
        .version = args.Flag(.{ .short = "v", .long = "version", .desc = "Show version" }),
        .login = args.Flag(.{ .short = "l", .long = "login", .desc = "Run as login shell" }),
        .interactive = args.Flag(.{ .short = "i", .long = "interactive", .desc = "Force interactive mode" }),
        .command = args.StringOption(.{ .short = "c", .long = "command", .desc = "Execute command and exit" }),
        .script = args.StringPositional(.{ .desc = "Script file to execute", .default = "" }),
    },
    .examples = &.{
        "oshen                   Start interactive REPL",
        "oshen -c 'echo hello'   Run a single command",
        "oshen script.osh        Execute a script file",
    },
});

pub fn parseArgs(argv: []const []const u8) args.ParseError!?Options {
    const r = try spec.parse(argv);

    if (r.help) {
        io.writeStdout(spec.help);
        return null;
    }

    if (r.version) {
        io.writeStdout(build_options.version ++ "\n");
        return null;
    }

    // Check if invoked as login shell (argv[0] starts with '-')
    const is_login_invocation = argv.len > 0 and argv[0].len > 0 and argv[0][0] == '-';
    return .{
        .is_login = r.login or is_login_invocation,
        .mode = if (r.command) |cmd|
            .{ .command = cmd }
        else if (r.script.len > 0)
            .{ .script = r.script }
        else
            .interactive,
    };
}
