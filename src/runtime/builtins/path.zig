//! path builtin - prepend paths to a variable with deduplication

// TODO: Add other commands

const std = @import("std");
const builtins = @import("../builtins.zig");
const args = @import("../../terminal/args.zig");

const spec = args.Spec("path", .{
    .desc = "Prepend paths to a variable with deduplication.",
    .args = .{
        .variable = args.StringPositional(.{ .desc = "Variable name (e.g., PATH)" }),
        .paths = args.Rest(.{ .desc = "Paths to prepend", .required = true }),
    },
    .examples = &.{
        "path PATH /usr/local/bin   # Prepend to PATH",
        "path PATH ~/bin ~/.local/bin",
    },
});

pub const builtin = builtins.fromSpec(spec, run);

fn run(state: *builtins.State, r: spec.Result) u8 {
    const current = state.getVar(r.variable) orelse "";

    const new_value = buildPathList(state.allocator, r.paths, current) catch {
        return builtins.reportOOM("path");
    };
    defer state.allocator.free(new_value);

    state.setVar(r.variable, new_value) catch return builtins.reportOOM("path");
    storeExport(state, r.variable, new_value) catch return builtins.reportOOM("path");
    builtins.env.set(state.allocator, r.variable, new_value) catch return builtins.reportOOM("path");

    return 0;
}

// =============================================================================
// Path Building
// =============================================================================

/// Build a colon-separated path list with deduplication.
fn buildPathList(
    allocator: std.mem.Allocator,
    new_paths: []const []const u8,
    current: []const u8,
) ![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(allocator);

    var seen: std.StringHashMapUnmanaged(void) = .empty;
    defer seen.deinit(allocator);

    // Add new paths first, then existing paths (deduplicating)
    try appendPaths(allocator, &result, &seen, new_paths);
    try appendPaths(allocator, &result, &seen, &.{current});

    return result.toOwnedSlice(allocator);
}

/// Append paths to result list, skipping duplicates.
fn appendPaths(
    allocator: std.mem.Allocator,
    result: *std.ArrayListUnmanaged(u8),
    seen: *std.StringHashMapUnmanaged(void),
    paths: []const []const u8,
) !void {
    for (paths) |path_or_list| {
        var iter = std.mem.splitScalar(u8, path_or_list, ':');
        while (iter.next()) |path| {
            if (path.len == 0) continue;
            if (seen.contains(path)) continue;

            try seen.put(allocator, path, {});
            if (result.items.len > 0) try result.append(allocator, ':');
            try result.appendSlice(allocator, path);
        }
    }
}

// =============================================================================
// Storage
// =============================================================================

/// Store an export in state, freeing any old entry.
fn storeExport(state: *builtins.State, name: []const u8, value: []const u8) !void {
    if (state.exports.fetchRemove(name)) |old| {
        state.freeStringEntry(old);
    }

    const key = try state.allocator.dupe(u8, name);
    errdefer state.allocator.free(key);

    const val = try state.allocator.dupe(u8, value);
    errdefer state.allocator.free(val);

    try state.exports.put(key, val);
}
