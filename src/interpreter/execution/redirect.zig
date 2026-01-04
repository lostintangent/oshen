//! File descriptor redirection for shell commands.
//!
//! Handles `<`, `>`, `>>`, `2>`, `2>>`, `&>`, and `2>&1` redirections
//! by opening files and using dup2() to redirect standard file descriptors.
//!
//! Called in child processes after fork() but before exec() to set up
//! the file descriptor table for the command being executed.

const std = @import("std");
const expansion_pipeline = @import("../expansion/pipeline.zig");
const io = @import("../../terminal/io.zig");

const ExpandedRedirect = expansion_pipeline.ExpandedRedirect;

// =============================================================================
// Public API
// =============================================================================

/// Apply file redirections for a command.
///
/// Called in child process after fork() to redirect stdin/stdout/stderr
/// before exec(). Redirections are applied in order, which matters for
/// cases like `>out 2>&1` (redirect stdout, then dup stderr to stdout).
///
/// ## Errors
/// Returns error if file cannot be opened or dup2 fails.
pub fn apply(redirs: []const ExpandedRedirect) !void {
    for (redirs) |redir| {
        switch (redir.kind) {
            .read => |path| {
                try openAndDup(path, .{ .ACCMODE = .RDONLY }, 0, redir.from_fd);
            },
            .write_truncate => |path| {
                try openAndDup(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true }, 0o644, redir.from_fd);
            },
            .write_append => |path| {
                try openAndDup(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true }, 0o644, redir.from_fd);
            },
            .dup => |to_fd| {
                try std.posix.dup2(to_fd, redir.from_fd);
            },
        }
    }
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Open a file and dup2 it to target fd.
fn openAndDup(path: []const u8, flags: std.posix.O, mode: std.posix.mode_t, target_fd: u8) !void {
    const fd = std.posix.open(path, flags, mode) catch |err| {
        io.printError("oshen: {s}: {}\n", .{ path, err });
        return err;
    };
    defer std.posix.close(fd);
    try std.posix.dup2(fd, target_fd);
}
