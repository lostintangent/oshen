//! Pipeline execution: fork/exec, pipe wiring, and process management
//!
//! This module handles the low-level execution of command pipelines:
//! - Forking child processes
//! - Setting up pipes between commands
//! - exec() system call
//! - Waiting for child processes

const std = @import("std");
const expansion_pipeline = @import("../expansion/pipeline.zig");
const state_mod = @import("../../runtime/state.zig");
const State = state_mod.State;
const builtins = @import("../../runtime/builtins.zig");
const resolve = @import("../../runtime/resolve.zig");
const io = @import("../../terminal/io.zig");
const redirect = @import("redirect.zig");
const signals = @import("signals.zig");

const ExpandedCommand = expansion_pipeline.ExpandedCommand;
const posix = signals.posix;

// =============================================================================
// Types
// =============================================================================

/// Function pointer type for trying to run commands as user-defined functions.
/// Returns exit status if command was a function, null if not a function.
pub const FunctionExecutor = *const fn (std.mem.Allocator, *State, ExpandedCommand) ?u8;

// =============================================================================
// Public API
// =============================================================================

/// Build null-terminated argv array for execvpe
pub fn buildArgv(allocator: std.mem.Allocator, cmd: ExpandedCommand) !std.ArrayListUnmanaged(?[*:0]const u8) {
    var argv: std.ArrayListUnmanaged(?[*:0]const u8) = .empty;
    errdefer argv.deinit(allocator);

    for (cmd.args) |arg| {
        const z = try allocator.dupeZ(u8, arg);
        try argv.append(allocator, z);
    }
    try argv.append(allocator, null);

    return argv;
}

/// Execute a pipeline in foreground, checking functions then builtins (aliases already expanded)
pub fn executePipelineForeground(allocator: std.mem.Allocator, state: *State, commands: []const ExpandedCommand, tryRunFunction: FunctionExecutor) !u8 {
    if (commands.len == 0) return 0;

    // Single command - try to run in-process if possible
    if (commands.len == 1) {
        const cmd = commands[0];
        if (cmd.args.len > 0) {
            // Use centralized resolution (functions before builtins, aliases already expanded)
            switch (resolve.resolveCommand(state, cmd.args[0], false)) {
                .function => {
                    if (cmd.redirects.len > 0) {
                        return try executeFunctionWithRedirects(allocator, state, cmd, tryRunFunction);
                    }
                    if (tryRunFunction(allocator, state, cmd)) |status| {
                        return status;
                    }
                },
                .builtin => {
                    if (cmd.redirects.len > 0) {
                        return try executeBuiltinWithRedirects(allocator, state, cmd);
                    }
                    if (builtins.tryRun(state, cmd.args)) |status| {
                        return status;
                    }
                },
                .alias, .external, .not_found => {},
            }
        }
        return try forkPipeline(allocator, state, commands, tryRunFunction);
    }

    return try forkPipeline(allocator, state, commands, tryRunFunction);
}

/// Execute a builtin command with file redirections.
/// Uses in-process redirect for output-only redirects (avoids fork overhead).
/// Falls back to fork for stdin redirects which are more complex.
fn executeBuiltinWithRedirects(_: std.mem.Allocator, state: *State, cmd: ExpandedCommand) !u8 {
    // Input redirects require fork because builtins may read from stdin
    for (cmd.redirects) |redir| {
        if (redir.kind == .read) return forkBuiltinWithRedirects(state, cmd);
    }

    // Fast path: output-only redirects can be handled in-process (~30x faster)
    return executeBuiltinInProcess(state, cmd);
}

/// Execute a builtin with output redirects in-process (no fork).
/// Handles `> file`, `2> file`, `> file 2>&1`, `>> file`, etc.
fn executeBuiltinInProcess(state: *State, cmd: ExpandedCommand) !u8 {
    // Save stdout/stderr for restoration after builtin completes
    const saved = SavedFds.init() catch return forkBuiltinWithRedirects(state, cmd);
    defer saved.restore();

    // Apply redirects in order (order matters for `>out 2>&1` vs `2>&1 >out`)
    for (cmd.redirects) |redir| {
        switch (redir.kind) {
            .write_truncate, .write_append => |path| {
                const fd = openForWrite(path, redir.kind == .write_append) orelse return 1;
                defer std.posix.close(fd); // Close source fd after dup2; target fd keeps file open
                std.posix.dup2(fd, redir.from_fd) catch return 1;
            },
            .dup => |to_fd| std.posix.dup2(to_fd, redir.from_fd) catch return 1,
            .read => unreachable, // Filtered out in caller
        }
    }

    return builtins.tryRun(state, cmd.args) orelse 127;
}

/// Saved stdout/stderr file descriptors for restoration after in-process redirects.
const SavedFds = struct {
    stdout: std.posix.fd_t,
    stderr: std.posix.fd_t,

    fn init() !SavedFds {
        const stdout = try std.posix.dup(std.posix.STDOUT_FILENO);
        errdefer std.posix.close(stdout);
        const stderr = try std.posix.dup(std.posix.STDERR_FILENO);
        return .{ .stdout = stdout, .stderr = stderr };
    }

    fn restore(self: SavedFds) void {
        std.posix.dup2(self.stdout, std.posix.STDOUT_FILENO) catch {};
        std.posix.dup2(self.stderr, std.posix.STDERR_FILENO) catch {};
        std.posix.close(self.stdout);
        std.posix.close(self.stderr);
    }
};

/// Open a file for write redirect (truncate or append mode).
fn openForWrite(path: []const u8, append: bool) ?std.posix.fd_t {
    return std.posix.open(path, .{
        .ACCMODE = .WRONLY,
        .CREAT = true,
        .TRUNC = !append,
        .APPEND = append,
    }, 0o644) catch |err| {
        io.printError("oshen: {s}: {}\n", .{ path, err });
        return null;
    };
}

/// Fork and execute a builtin with redirects (fallback for stdin redirects).
fn forkBuiltinWithRedirects(state: *State, cmd: ExpandedCommand) !u8 {
    const pid = try std.posix.fork();

    if (pid == 0) {
        redirect.apply(cmd.redirects) catch std.posix.exit(1);
        std.posix.exit(builtins.tryRun(state, cmd.args) orelse 127);
    }

    return waitForChild(pid);
}

/// Execute a user-defined function with redirects in a forked child
fn executeFunctionWithRedirects(allocator: std.mem.Allocator, state: *State, cmd: ExpandedCommand, tryRunFunction: FunctionExecutor) !u8 {
    const pid = try std.posix.fork();

    if (pid == 0) {
        redirect.apply(cmd.redirects) catch {
            std.posix.exit(1);
        };

        const status = tryRunFunction(allocator, state, cmd) orelse 127;
        std.posix.exit(status);
    }

    return waitForChild(pid);
}

// =============================================================================
// Child Process Execution
// =============================================================================

/// Execute a pipeline in a child process (no job control needed)
pub fn executePipelineInChild(allocator: std.mem.Allocator, state: ?*State, commands: []const ExpandedCommand, tryRunFunction: ?FunctionExecutor) !u8 {
    if (commands.len == 0) return 0;

    // In child, we don't need job control - just execute
    if (commands.len == 1) {
        if (state) |s| {
            return try executeSingleCommand(allocator, s, commands[0], tryRunFunction);
        }
        return try executeCommandSimple(allocator, commands[0]);
    }

    // Multi-command pipeline
    const n = commands.len;
    var pipes: std.ArrayListUnmanaged([2]std.posix.fd_t) = .empty;
    defer {
        for (pipes.items) |pipe| {
            std.posix.close(pipe[0]);
            std.posix.close(pipe[1]);
        }
        pipes.deinit(allocator);
    }

    for (0..n - 1) |_| {
        const pipe = try std.posix.pipe();
        try pipes.append(allocator, pipe);
    }

    var pids: std.ArrayListUnmanaged(std.posix.pid_t) = .empty;
    defer pids.deinit(allocator);

    for (commands, 0..) |cmd, i| {
        const stdin_fd: ?std.posix.fd_t = if (i == 0) null else pipes.items[i - 1][0];
        const stdout_fd: ?std.posix.fd_t = if (i == n - 1) null else pipes.items[i][1];

        const child_pid = try std.posix.fork();

        if (child_pid == 0) {
            setupPipeRedirects(stdin_fd, stdout_fd);
            for (pipes.items) |pipe| {
                std.posix.close(pipe[0]);
                std.posix.close(pipe[1]);
            }
            execCommandWithState(allocator, state, tryRunFunction, cmd);
        }

        try pids.append(allocator, child_pid);
    }

    for (pipes.items) |pipe| {
        std.posix.close(pipe[0]);
        std.posix.close(pipe[1]);
    }
    pipes.clearRetainingCapacity();

    var last_status: u8 = 0;
    for (pids.items) |child_pid| {
        last_status = waitForChild(child_pid);
    }

    return last_status;
}

fn executeSingleCommand(allocator: std.mem.Allocator, state: *State, cmd: ExpandedCommand, tryRunFunction: ?FunctionExecutor) !u8 {
    if (cmd.args.len == 0) return 0;

    // Use centralized resolution (functions before builtins, aliases already expanded)
    switch (resolve.resolveCommand(state, cmd.args[0], false)) {
        .function => {
            if (tryRunFunction) |f| {
                if (cmd.redirects.len > 0) {
                    return try executeFunctionWithRedirects(allocator, state, cmd, f);
                }
                if (f(allocator, state, cmd)) |status| {
                    return status;
                }
            }
        },
        .builtin => {
            if (cmd.redirects.len > 0) {
                return try executeBuiltinWithRedirects(allocator, state, cmd);
            }
            if (builtins.tryRun(state, cmd.args)) |status| {
                return status;
            }
        },
        .alias, .external, .not_found => {},
    }

    return try executeCommandSimple(allocator, cmd);
}

// =============================================================================
// Pipeline Orchestration
// =============================================================================

/// Fork and execute a pipeline with process groups and terminal handling.
///
/// This is the core process-creation path. It:
/// 1. Creates pipes between commands
/// 2. Forks child processes into a shared process group
/// 3. Gives terminal control to the process group (if foreground)
/// 4. Waits for all children to complete
///
/// Used for external commands and pipelines that can't run in-process.
pub fn forkPipeline(allocator: std.mem.Allocator, state: *State, commands: []const ExpandedCommand, tryRunFunction: FunctionExecutor) !u8 {
    const n = commands.len;

    // Create pipes for multi-command pipeline
    var pipes: std.ArrayListUnmanaged([2]std.posix.fd_t) = .empty;
    defer {
        for (pipes.items) |pipe| {
            if (pipe[0] != -1) std.posix.close(pipe[0]);
            if (pipe[1] != -1) std.posix.close(pipe[1]);
        }
        pipes.deinit(allocator);
    }

    for (0..n - 1) |_| {
        const pipe = try std.posix.pipe();
        try pipes.append(allocator, pipe);
    }

    // Fork all processes, putting them in the same process group
    var pids: std.ArrayListUnmanaged(std.posix.pid_t) = .empty;
    defer pids.deinit(allocator);

    var pgid: std.posix.pid_t = 0;

    // Check if first command can run in-process (builtin, no redirects, pipeline has >1 command)
    // Use centralized resolution to respect function shadowing
    const first_cmd = commands[0];
    const first_is_inline_builtin = n > 1 and
        first_cmd.args.len > 0 and
        first_cmd.redirects.len == 0 and
        resolve.resolveCommand(state, first_cmd.args[0], false) == .builtin;

    // If first command is a simple builtin, run it in-process writing to the pipe
    if (first_is_inline_builtin) {
        const write_fd = pipes.items[0][1];

        // Redirect stdout to pipe
        const saved_stdout = try std.posix.dup(std.posix.STDOUT_FILENO);
        defer std.posix.close(saved_stdout);
        try std.posix.dup2(write_fd, std.posix.STDOUT_FILENO);

        // Run builtin (ignore status - pipeline status comes from last command)
        _ = builtins.tryRun(state, first_cmd.args) orelse 0;

        // Restore stdout (use catch to ensure we always attempt restoration)
        std.posix.dup2(saved_stdout, std.posix.STDOUT_FILENO) catch {};

        // Close write end so downstream sees EOF
        std.posix.close(write_fd);
        pipes.items[0][1] = -1; // Mark as closed
    }

    const start_idx: usize = if (first_is_inline_builtin) 1 else 0;

    for (commands[start_idx..], start_idx..) |cmd, i| {
        const stdin_fd: ?std.posix.fd_t = if (i == 0) null else pipes.items[i - 1][0];
        const stdout_fd: ?std.posix.fd_t = if (i == n - 1) null else pipes.items[i][1];

        const pid = try std.posix.fork();

        if (pid == 0) {
            // Child process
            // Set process group (first child becomes group leader)
            const child_pgid = if (pgid == 0) posix.getpid() else pgid;
            _ = posix.setpgid(0, child_pgid);

            // Reset signal handlers to default
            signals.resetToDefault();

            setupPipeRedirects(stdin_fd, stdout_fd);

            for (pipes.items) |pipe| {
                if (pipe[0] != -1) std.posix.close(pipe[0]);
                if (pipe[1] != -1) std.posix.close(pipe[1]);
            }

            execCommandWithState(allocator, state, tryRunFunction, cmd);
        }

        // Parent: set process group (race with child)
        if (pgid == 0) pgid = pid;
        _ = posix.setpgid(pid, pgid);

        try pids.append(allocator, pid);
    }

    // Parent: close all pipe fds
    for (pipes.items) |*pipe| {
        if (pipe[0] != -1) {
            std.posix.close(pipe[0]);
            pipe[0] = -1;
        }
        if (pipe[1] != -1) {
            std.posix.close(pipe[1]);
            pipe[1] = -1;
        }
    }
    pipes.clearRetainingCapacity();

    // Give terminal to the job's process group (foreground only)
    if (state.interactive and pgid != 0) {
        _ = posix.tcsetpgrp(state.terminal_fd, pgid);
    }

    // Wait for all children
    var last_status: u8 = 0;
    for (pids.items) |pid| {
        last_status = signals.waitForeground(pid);
    }

    // Take back terminal control
    if (state.interactive) {
        _ = posix.tcsetpgrp(state.terminal_fd, state.shell_pgid);
    }

    // If child was killed by SIGINT, propagate interrupt to shell
    // (SIGINT goes to foreground process group, which is the child, not the shell)
    if (last_status == 130) {
        state.interrupted = true;
    }

    return last_status;
}

// =============================================================================
// Simple Execution
// =============================================================================

/// Execute a single command (fork + exec + wait)
pub fn executeCommandSimple(allocator: std.mem.Allocator, cmd: ExpandedCommand) !u8 {
    if (cmd.args.len == 0) return 0;

    const pid = try std.posix.fork();

    if (pid == 0) {
        execCommand(allocator, cmd);
    }

    return waitForChild(pid);
}

/// Set up pipeline stdin/stdout redirects (called in child)
pub fn setupPipeRedirects(stdin_fd: ?std.posix.fd_t, stdout_fd: ?std.posix.fd_t) void {
    if (stdin_fd) |fd| {
        std.posix.dup2(fd, std.posix.STDIN_FILENO) catch {
            std.posix.exit(1);
        };
    }
    if (stdout_fd) |fd| {
        std.posix.dup2(fd, std.posix.STDOUT_FILENO) catch {
            std.posix.exit(1);
        };
    }
}

// =============================================================================
// Exec Helpers
// =============================================================================

/// Execute command in child process (does not return)
pub fn execCommand(allocator: std.mem.Allocator, cmd: ExpandedCommand) noreturn {
    execCommandWithState(allocator, null, null, cmd);
}

/// Execute command in child process with optional state for builtins (does not return)
pub fn execCommandWithState(allocator: std.mem.Allocator, state: ?*State, tryRunFunction: ?FunctionExecutor, cmd: ExpandedCommand) noreturn {
    // Apply file redirections
    redirect.apply(cmd.redirects) catch {
        std.posix.exit(1);
    };

    // If we have state, use centralized resolution (functions before builtins, aliases already expanded)
    if (state) |s| {
        if (cmd.args.len > 0) {
            switch (resolve.resolveCommand(s, cmd.args[0], false)) {
                .function => {
                    if (tryRunFunction) |f| {
                        if (f(allocator, s, cmd)) |status| {
                            std.posix.exit(status);
                        }
                    }
                },
                .builtin => {
                    const status = builtins.tryRun(s, cmd.args) orelse 127;
                    std.posix.exit(status);
                },
                .alias, .external, .not_found => {},
            }
        }
    }

    const argv = buildArgv(allocator, cmd) catch {
        std.posix.exit(127);
    };

    const envp = std.c.environ;
    const argv_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(argv.items.ptr);

    const err = std.posix.execvpeZ(argv.items[0].?, argv_ptr, envp);

    // If we get here, exec failed
    const msg = if (err == error.FileNotFound) "Command not found" else @errorName(err);
    io.printError("{s}: {s}\n", .{ msg, cmd.args[0] });
    std.posix.exit(127);
}

/// Wait for child process and return exit status
pub fn waitForChild(pid: std.posix.pid_t) u8 {
    const result = std.posix.waitpid(pid, 0);

    if (std.posix.W.IFEXITED(result.status)) {
        return std.posix.W.EXITSTATUS(result.status);
    } else if (std.posix.W.IFSIGNALED(result.status)) {
        return 128 + @as(u8, @intCast(std.posix.W.TERMSIG(result.status)));
    }

    return 1;
}
