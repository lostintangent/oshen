//! Environment variable operations
//!
//! Provides a unified interface for setting/unsetting environment variables,
//! handling null-terminated string conversion internally.

const std = @import("std");

// =============================================================================
// C Library Interface
// =============================================================================

extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
extern "c" fn unsetenv(name: [*:0]const u8) c_int;

// =============================================================================
// Public API
// =============================================================================

/// Get an environment variable value.
/// This is a thin inline wrapper around std.posix.getenv for consistency.
pub inline fn get(name: []const u8) ?[]const u8 {
    return std.posix.getenv(name);
}

/// Get the HOME directory from the environment.
pub inline fn getHome() ?[]const u8 {
    return std.posix.getenv("HOME");
}

/// Get the PATH environment variable.
pub inline fn getPath() ?[]const u8 {
    return std.posix.getenv("PATH");
}

/// Set an environment variable, allocating null-terminated copies internally.
pub fn set(allocator: std.mem.Allocator, name: []const u8, value: []const u8) !void {
    const name_z = try allocator.dupeZ(u8, name);
    defer allocator.free(name_z);
    const value_z = try allocator.dupeZ(u8, value);
    defer allocator.free(value_z);
    _ = setenv(name_z, value_z, 1);
}

/// Unset an environment variable.
pub fn unset(allocator: std.mem.Allocator, name: []const u8) !void {
    const name_z = try allocator.dupeZ(u8, name);
    defer allocator.free(name_z);
    _ = unsetenv(name_z);
}
