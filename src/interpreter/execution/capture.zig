//! Output capture utilities.
//!
//! Provides two capture strategies:
//! - `captureBuiltin()`: In-process capture for builtins (fast, no fork)
//! - `forkWithPipe()`: Fork-based capture for external commands
//!
//! Also provides `tryExpandSimpleBuiltin()` to detect when the fast path is usable.
//!
//! Used by:
//! - Output capture operators (`=>`, `=>@`)
//! - Command substitution `$(...)`
//! - Custom prompt function execution

const std = @import("std");
const io = @import("../../terminal/io.zig");
const builtins = @import("../../runtime/builtins.zig");
const State = @import("../../runtime/state.zig").State;
const ExpandedCmd = @import("../expansion/expanded.zig").ExpandedCmd;
const ast = @import("../../language/ast.zig");
const expand = @import("../expansion/word.zig");
const expansion_statement = @import("../expansion/statement.zig");

// C library functions
const c = struct {
    extern "c" fn waitpid(pid: std.posix.pid_t, status: ?*c_int, options: c_int) std.posix.pid_t;
};

pub const CaptureResult = struct {
    output: []const u8,
    status: u8,
};

// =============================================================================
// Simple Builtin Detection
// =============================================================================

/// A simple builtin command that can be captured in-process.
/// Contains both the expanded command and the slice it came from (for cleanup).
pub const ExpandedBuiltin = struct {
    cmd: ExpandedCmd,
    slice: []const ExpandedCmd,

    /// Free all memory allocated for the expanded commands.
    pub fn deinit(self: ExpandedBuiltin, allocator: std.mem.Allocator) void {
        for (self.slice) |cmd| {
            allocator.free(cmd.argv);
            allocator.free(cmd.env);
            allocator.free(cmd.redirects);
        }
        allocator.free(self.slice);
    }
};

/// Try to expand a CommandStatement as a simple builtin (single command, no redirects).
///
/// Returns null if the statement is too complex for in-process capture:
/// - Multiple chains (&&, ||)
/// - Pipelines (|)
/// - Redirects (>, <, etc.)
/// - External commands (not a builtin)
///
/// On success, caller owns the returned ExpandedBuiltin and must call deinit().
pub fn tryExpandSimpleBuiltin(allocator: std.mem.Allocator, state: *State, stmt: ast.CommandStatement) ?ExpandedBuiltin {
    // Must be: single chain, single command, no redirects in AST
    if (stmt.chains.len != 1) return null;
    const chain = stmt.chains[0];
    if (chain.pipeline.commands.len != 1) return null;
    if (chain.pipeline.commands[0].redirects.len != 0) return null;

    // Expand and verify it's a builtin
    var ctx = expand.ExpandContext.init(allocator, state);
    defer ctx.deinit();

    const expanded = expansion_statement.expandPipeline(allocator, &ctx, chain.pipeline) catch return null;

    // Verify: single command, has argv, no redirects after expansion, is a builtin
    if (expanded.len != 1 or expanded[0].argv.len == 0 or expanded[0].redirects.len != 0) {
        freeExpandedSlice(allocator, expanded);
        return null;
    }
    const name = expanded[0].argv[0];
    if (!builtins.isBuiltin(name)) {
        freeExpandedSlice(allocator, expanded);
        return null;
    }
    // eval and source execute arbitrary code that may have redirects/pipes
    // They must use fork-based capture to honor those correctly
    if (std.mem.eql(u8, name, "eval") or std.mem.eql(u8, name, "source")) {
        freeExpandedSlice(allocator, expanded);
        return null;
    }

    return .{ .cmd = expanded[0], .slice = expanded };
}

fn freeExpandedSlice(allocator: std.mem.Allocator, cmds: []const ExpandedCmd) void {
    for (cmds) |cmd| {
        allocator.free(cmd.argv);
        allocator.free(cmd.env);
        allocator.free(cmd.redirects);
    }
    allocator.free(cmds);
}

// =============================================================================
// In-Process Builtin Capture
// =============================================================================

/// Capture a builtin's stdout output in-process without forking.
///
/// This is the fastest capture path - pure in-memory with zero syscalls
/// and zero heap allocations. Reuses Writer for buffering (no separate impl).
///
/// Only works for builtins (not external commands or pipelines).
pub fn captureBuiltin(allocator: std.mem.Allocator, state: *State, cmd: ExpandedCmd) !CaptureResult {
    // Use a Writer as the capture buffer - same type builtins use internally
    var w = io.Writer{};

    // Enable capture mode, saving previous state for nested captures
    const prev = io.startCapture(&w);
    defer io.endCapture(prev);

    // Run the builtin (output goes to Writer)
    const status = builtins.tryRun(state, cmd) orelse 1;

    // Get captured output and trim trailing newlines
    const trimmed = std.mem.trimRight(u8, w.buf[0..w.pos], "\n");
    return .{ .output = try allocator.dupe(u8, trimmed), .status = status };
}

/// Store captured output into a shell variable.
pub fn storeCapture(allocator: std.mem.Allocator, state: *State, output: []const u8, variable: []const u8, as_lines: bool) !void {
    if (as_lines) {
        var lines: std.ArrayListUnmanaged([]const u8) = .empty;
        defer lines.deinit(allocator);

        var iter = std.mem.splitScalar(u8, output, '\n');
        while (iter.next()) |line| {
            try lines.append(allocator, try allocator.dupe(u8, line));
        }
        try state.setVarList(variable, lines.items);
    } else {
        try state.setVar(variable, output);
    }
}

// =============================================================================
// Fork-Based Capture
// =============================================================================

/// Result of forkWithPipe - tells caller which process they're in.
pub const ForkResult = union(enum) {
    /// We're in the child - stdout and stderr are redirected to the pipe.
    /// Run your code and call std.posix.exit() when done.
    child: void,

    /// We're in the parent. Use readAndWait() to get the captured output.
    parent: ParentHandle,
};

pub const ParentHandle = struct {
    read_fd: std.posix.fd_t,
    child_pid: std.posix.pid_t,

    /// Read all output from the child and wait for it to exit.
    /// Returns the captured output (trimmed) and exit status.
    pub fn readAndWait(self: ParentHandle, allocator: std.mem.Allocator) !CaptureResult {
        var output: std.ArrayListUnmanaged(u8) = .empty;
        defer output.deinit(allocator);

        var buf: [4096]u8 = undefined;
        while (true) {
            const n = std.posix.read(self.read_fd, &buf) catch break;
            if (n == 0) break;
            try output.appendSlice(allocator, buf[0..n]);
        }
        std.posix.close(self.read_fd);

        // Wait for child
        var status: c_int = 0;
        _ = c.waitpid(self.child_pid, &status, 0);

        const exit_status: u8 = if (std.posix.W.IFEXITED(@bitCast(status)))
            std.posix.W.EXITSTATUS(@bitCast(status))
        else
            1;

        // Trim trailing newlines
        const trimmed = std.mem.trimRight(u8, output.items, "\n");
        const owned_output = try allocator.dupe(u8, trimmed);

        return CaptureResult{
            .output = owned_output,
            .status = exit_status,
        };
    }
};

/// Fork a child process with stdout and stderr redirected to a pipe.
///
/// Returns `.child` in the child process (stdout and stderr already redirected),
/// or `.parent` with a handle to read the output and wait.
///
/// Usage:
/// ```zig
/// switch (try process.forkWithPipe()) {
///     .child => {
///         // Run code that writes to stdout/stderr
///         const status = doWork();
///         std.posix.exit(status);
///     },
///     .parent => |handle| {
///         const result = try handle.readAndWait(allocator);
///         // result.output contains captured stdout and stderr
///         // result.status contains exit code
///     },
/// }
/// ```
pub fn forkWithPipe() !ForkResult {
    const pipe_fds = try std.posix.pipe();
    const read_fd = pipe_fds[0];
    const write_fd = pipe_fds[1];

    const pid = try std.posix.fork();
    if (pid == 0) {
        // Child: redirect stdout and stderr to pipe
        std.posix.close(read_fd);
        std.posix.dup2(write_fd, std.posix.STDOUT_FILENO) catch std.posix.exit(1);
        std.posix.dup2(write_fd, std.posix.STDERR_FILENO) catch std.posix.exit(1);
        std.posix.close(write_fd);
        return .child;
    }

    // Parent: close write end, return handle for reading
    std.posix.close(write_fd);
    return .{ .parent = .{
        .read_fd = read_fd,
        .child_pid = pid,
    } };
}
