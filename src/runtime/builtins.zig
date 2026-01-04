//! Builtin command dispatcher
//!
//! Central registry for all shell builtin commands, with compile-time
//! optimized lookup and shared utilities for builtin implementations.

const std = @import("std");

/// Prefix for explicitly referencing builtins (e.g., "oshen:echo" bypasses aliases/functions)
pub const builtin_prefix = "oshen:";

/// Normalize a command name by stripping the builtin prefix if present.
/// This allows users to explicitly reference builtins when shadowed by aliases/functions.
inline fn normalizeBuiltinName(name: []const u8) []const u8 {
    if (name.len > builtin_prefix.len and
        std.mem.eql(u8, name[0..builtin_prefix.len], builtin_prefix))
    {
        return name[builtin_prefix.len..];
    }
    return name;
}

// Re-export types commonly needed by builtin implementations
pub const State = @import("state.zig").State;
pub const io = @import("../terminal/io.zig");
pub const env = @import("env.zig");

// =============================================================================
// Shared Utilities
// =============================================================================

/// Check if a string looks like a number (for argument parsing).
/// Handles negative numbers: "-5" returns true, "--foo" returns false.
pub const isNumeric = @import("../terminal/args.zig").isNumeric;

/// Standard signature for all builtin commands.
pub const BuiltinFn = *const fn (*State, []const []const u8) u8;

/// Builtin command definition
pub const Builtin = struct {
    name: []const u8,
    run: BuiltinFn,
    help: []const u8,
};

/// Create a Builtin from a Spec type, which handles arg parsing automatically.
pub fn fromSpec(comptime S: type, comptime run_fn: *const fn (*State, S.Result) u8) Builtin {
    const wrapper = struct {
        fn run(st: *State, args: []const []const u8) u8 {
            const parsed = S.parse(args) catch return 1;
            return run_fn(st, parsed);
        }
    };
    return .{ .name = S.name, .run = wrapper.run, .help = S.help };
}

/// Create an alias for an existing builtin with a different name.
pub fn alias(comptime b: Builtin, comptime name: []const u8) Builtin {
    return .{ .name = name, .run = b.run, .help = b.help };
}

// Import individual builtin modules
const cd_builtin = @import("builtins/cd.zig");
const pwd_builtin = @import("builtins/pwd.zig");
const jobs_builtin = @import("builtins/jobs.zig");
const fg_builtin = @import("builtins/fg.zig");
const bg_builtin = @import("builtins/bg.zig");
const var_builtin = @import("builtins/var.zig");
const unset_builtin = @import("builtins/unset.zig");
const export_builtin = @import("builtins/export.zig");
const source_builtin = @import("builtins/source.zig");
const eval_builtin = @import("builtins/eval.zig");
const true_builtin = @import("builtins/true.zig");
const false_builtin = @import("builtins/false.zig");
const type_builtin = @import("builtins/type.zig");
const echo_builtin = @import("builtins/echo.zig");
const print_builtin = @import("builtins/print.zig");
const alias_builtin = @import("builtins/alias.zig");
const unalias_builtin = @import("builtins/unalias.zig");
const test_builtin = @import("builtins/test.zig");
const path_builtin = @import("builtins/path.zig");
const calc_builtin = @import("builtins/calc.zig");
const increment_builtin = @import("builtins/increment.zig");
const terminal_builtin = @import("builtins/terminal.zig");
const string_builtin = @import("builtins/string.zig");
const list_builtin = @import("builtins/list.zig");

/// All registered builtins - single source of truth
const all_builtins = [_]Builtin{
    cd_builtin.builtin,
    pwd_builtin.builtin,
    jobs_builtin.builtin,
    fg_builtin.builtin,
    bg_builtin.builtin,
    var_builtin.builtin,
    var_builtin.set_builtin,
    unset_builtin.builtin,
    export_builtin.builtin,
    source_builtin.builtin,
    eval_builtin.builtin,
    true_builtin.builtin,
    false_builtin.builtin,
    type_builtin.builtin,
    echo_builtin.builtin,
    print_builtin.builtin,
    alias_builtin.builtin,
    unalias_builtin.builtin,
    test_builtin.builtin,
    test_builtin.bracket_builtin,
    path_builtin.builtin,
    calc_builtin.builtin,
    calc_builtin.equals_builtin,
    increment_builtin.builtin,
    terminal_builtin.builtin,
    string_builtin.builtin,
    list_builtin.builtin,
};

/// Compile-time map for O(1) builtin lookup (built from all_builtins)
const builtin_map = blk: {
    var entries: [all_builtins.len]struct { []const u8, Builtin } = undefined;
    for (all_builtins, 0..) |b, i| {
        entries[i] = .{ b.name, b };
    }
    break :blk std.StaticStringMap(Builtin).initComptime(entries);
};

/// Compile-time array of builtin names (for tab completion)
const builtin_names = blk: {
    var names: [all_builtins.len][]const u8 = undefined;
    for (all_builtins, 0..) |b, i| {
        names[i] = b.name;
    }
    break :blk names;
};

/// Get all builtin names (for tab completion)
pub fn getNames() []const []const u8 {
    return &builtin_names;
}

/// Try to run a builtin command. Returns null if not a builtin.
pub fn tryRun(st: *State, args: []const []const u8) ?u8 {
    if (args.len == 0) return null;

    const name = normalizeBuiltinName(args[0]);

    if (builtin_map.get(name)) |builtin| {
        // Slice off command name
        const builtin_args = args[1..];

        // Check for -h or --help as first argument
        if (builtin_args.len > 0) {
            const arg = builtin_args[0];
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                io.writeStdout(builtin.help);
                io.writeStdout("\n");
                return 0;
            }
        }
        return builtin.run(st, builtin_args);
    }

    return null; // Not a builtin
}

/// Check if a command name is a builtin (handles osh: prefix)
pub fn isBuiltin(name: []const u8) bool {
    return builtin_map.has(normalizeBuiltinName(name));
}

// =============================================================================
// Shared Utilities for Builtins
// =============================================================================

/// Join arguments with spaces into a single allocated string.
pub fn joinArgs(allocator: std.mem.Allocator, args: []const []const u8) ![]const u8 {
    if (args.len == 0) return "";
    if (args.len == 1) return allocator.dupe(u8, args[0]);

    // Calculate total size needed
    var total: usize = 0;
    for (args) |arg| total += arg.len;
    total += args.len - 1; // spaces between args

    const result = try allocator.alloc(u8, total);
    var pos: usize = 0;
    for (args, 0..) |arg, i| {
        @memcpy(result[pos..][0..arg.len], arg);
        pos += arg.len;
        if (i < args.len - 1) {
            result[pos] = ' ';
            pos += 1;
        }
    }
    return result;
}

/// Report an out-of-memory error for a builtin command.
pub fn reportOOM(comptime cmd_name: []const u8) u8 {
    io.writeStderr(cmd_name ++ ": out of memory\n");
    return 1;
}

/// Join arguments with spaces into a stack buffer (zero-allocation).
/// Returns the joined slice, or null if buffer overflow.
pub fn joinArgsToBuffer(args: []const []const u8, buf: []u8) ?[]const u8 {
    var pos: usize = 0;
    for (args, 0..) |arg, i| {
        if (i > 0) {
            if (pos >= buf.len) return null;
            buf[pos] = ' ';
            pos += 1;
        }
        if (pos + arg.len > buf.len) return null;
        @memcpy(buf[pos..][0..arg.len], arg);
        pos += arg.len;
    }
    return buf[0..pos];
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "normalizeBuiltinName: strips oshen: prefix" {
    try testing.expectEqualStrings("echo", normalizeBuiltinName("oshen:echo"));
    try testing.expectEqualStrings("string", normalizeBuiltinName("oshen:string"));
    try testing.expectEqualStrings("cd", normalizeBuiltinName("oshen:cd"));
}

test "normalizeBuiltinName: preserves names without prefix" {
    try testing.expectEqualStrings("echo", normalizeBuiltinName("echo"));
    try testing.expectEqualStrings("string", normalizeBuiltinName("string"));
    try testing.expectEqualStrings("my-command", normalizeBuiltinName("my-command"));
}

test "normalizeBuiltinName: handles edge cases" {
    // Too short to have prefix
    try testing.expectEqualStrings("oshen", normalizeBuiltinName("oshen"));
    try testing.expectEqualStrings("oshen:", normalizeBuiltinName("oshen:"));
    // Different prefix
    try testing.expectEqualStrings("fish:echo", normalizeBuiltinName("fish:echo"));
    // Empty string
    try testing.expectEqualStrings("", normalizeBuiltinName(""));
}

test "isBuiltin: recognizes prefixed builtins" {
    try testing.expect(isBuiltin("oshen:echo"));
    try testing.expect(isBuiltin("oshen:cd"));
    try testing.expect(isBuiltin("oshen:string"));
    try testing.expect(isBuiltin("oshen:pwd"));
}

test "isBuiltin: recognizes unprefixed builtins" {
    try testing.expect(isBuiltin("echo"));
    try testing.expect(isBuiltin("cd"));
    try testing.expect(isBuiltin("string"));
    try testing.expect(isBuiltin("pwd"));
}

test "isBuiltin: rejects non-builtins" {
    try testing.expect(!isBuiltin("notabuiltin"));
    try testing.expect(!isBuiltin("oshen:notabuiltin"));
    try testing.expect(!isBuiltin("ls"));
    try testing.expect(!isBuiltin("oshen:ls"));
}
