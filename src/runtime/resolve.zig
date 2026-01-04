//! Command resolution utilities
//!
//! Shared logic for determining how a command name resolves:
//! alias, builtin, function, or external executable.

const std = @import("std");
const builtins = @import("builtins.zig");
const State = @import("state.zig").State;
const env = @import("env.zig");

/// How a command name resolves
pub const CommandResolution = union(enum) {
    alias: []const u8, // expansion text
    builtin,
    function,
    external: []const u8, // full path
    not_found,
};

/// Resolve how a command name would be interpreted.
/// Resolution order: aliases (if included) → functions → builtins → external.
///
/// By default, aliases are included. Pass `false` for execution contexts
/// where aliases have already been expanded.
pub fn resolveCommand(state: ?*State, name: []const u8, include_aliases: bool) CommandResolution {
    // Check aliases first (unless skipped for execution)
    if (include_aliases) {
        if (state) |s| {
            if (s.getAlias(name)) |expansion| {
                return .{ .alias = expansion };
            }
        }
    }

    // Check user-defined functions (allows shadowing builtins)
    if (state) |s| {
        if (s.getFunction(name) != null) {
            return .function;
        }
    }

    // Check builtins
    if (builtins.isBuiltin(name)) {
        return .builtin;
    }

    // Check PATH
    if (findInPath(name)) |path| {
        return .{ .external = path };
    }

    return .not_found;
}

/// Check if a command name resolves to something valid (includes aliases)
pub fn isValidCommand(state: ?*State, name: []const u8) bool {
    return resolveCommand(state, name, true) != .not_found;
}

/// Find a command in PATH, returns full path or null
pub fn findInPath(name: []const u8) ?[]const u8 {
    // If it contains a slash, it's a direct path
    if (std.mem.indexOfScalar(u8, name, '/') != null) {
        std.fs.cwd().access(name, .{}) catch return null;
        return name;
    }

    const path_env = env.getPath() orelse return null;
    var path_iter = std.mem.splitScalar(u8, path_env, ':');

    // Use a static buffer for the result (valid until next call)
    const S = struct {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
    };

    while (path_iter.next()) |dir| {
        if (dir.len == 0) continue;

        const full_path = std.fmt.bufPrint(&S.buf, "{s}/{s}", .{ dir, name }) catch continue;
        std.fs.cwd().access(full_path, .{}) catch continue;
        return full_path;
    }
    return null;
}
