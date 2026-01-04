//! Signal handling and job continuation for the shell.
//!
//! This module handles:
//! - Shell initialization (process group, terminal ownership)
//! - Signal handler setup (SIGCHLD, SIGINT, SIGTSTP)
//! - Continuing stopped jobs (fg/bg builtins)
//!
//! Signal handling only matters in interactive mode (when connected to a TTY).

const std = @import("std");
const state_mod = @import("../../runtime/state.zig");
const State = state_mod.State;
const io = @import("../../terminal/io.zig");

// =============================================================================
// POSIX C Bindings
// =============================================================================

/// POSIX functions not available in Zig stdlib
pub const posix = struct {
    pub extern "c" fn getpgrp() std.posix.pid_t;
    pub extern "c" fn setpgid(pid: std.posix.pid_t, pgid: std.posix.pid_t) c_int;
    pub extern "c" fn tcgetpgrp(fd: std.posix.fd_t) std.posix.pid_t;
    pub extern "c" fn tcsetpgrp(fd: std.posix.fd_t, pgrp: std.posix.pid_t) c_int;
    pub extern "c" fn getpid() std.posix.pid_t;
    pub extern "c" fn kill(pid: std.posix.pid_t, sig: c_int) c_int;
    pub extern "c" fn waitpid(pid: std.posix.pid_t, status: ?*u32, options: c_int) std.posix.pid_t;

    // Wait flags
    pub const WNOHANG: c_int = 1;
    pub const WUNTRACED: c_int = 2;
};

// =============================================================================
// Child Process Helpers
// =============================================================================

/// Reset signals to default handlers in child processes.
///
/// Called after fork() but before exec() to restore normal signal behavior.
/// Without this, child processes would inherit the shell's signal handlers
/// (which ignore SIGTSTP, etc.).
pub fn resetToDefault() void {
    const default_act = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.DFL },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.INT, &default_act, null);
    std.posix.sigaction(std.posix.SIG.TSTP, &default_act, null);
    std.posix.sigaction(std.posix.SIG.TTIN, &default_act, null);
    std.posix.sigaction(std.posix.SIG.TTOU, &default_act, null);
    std.posix.sigaction(std.posix.SIG.CHLD, &default_act, null);
}

// =============================================================================
// Global State
// =============================================================================

/// Global state pointer for signal handlers.
///
/// RATIONALE: POSIX signal handlers have a C ABI constraint that prevents passing
/// context through the handler signature. This global is the standard pattern for
/// allowing the signal handler to update shell job state when child processes
/// terminate or stop. Set once during `init()` and remains valid for the shell's
/// lifetime.
///
/// SAFETY: Only accessed from signal handler context (single-threaded signal delivery)
/// and from `init`. The shell is single-threaded, so no synchronization needed.
var g_state: ?*State = null;

// =============================================================================
// Public API
// =============================================================================

/// Initialize signal handling for interactive shell mode.
///
/// Only called once at shell startup when running interactively. Sets up:
/// - Shell's process group
/// - Terminal ownership (via tcsetpgrp)
/// - Signal handlers (SIGINT, SIGCHLD, SIGTSTP, etc.)
///
/// Not needed for non-interactive script execution.
pub fn initInteractive(state: *State) void {
    g_state = state;

    // Check if we're actually connected to a terminal
    const is_tty = std.posix.isatty(state.terminal_fd);
    if (!is_tty) {
        state.interactive = false;
    }

    // Get our process group
    state.shell_pgid = posix.getpgrp();

    // Make sure we're in the foreground (only if interactive)
    if (state.interactive) {
        // Loop until we're in the foreground
        while (posix.tcgetpgrp(state.terminal_fd) != state.shell_pgid) {
            _ = posix.kill(-state.shell_pgid, std.posix.SIG.TTIN);
        }

        // Put ourselves in our own process group
        _ = posix.setpgid(0, state.shell_pgid);

        // Grab control of terminal
        _ = posix.tcsetpgrp(state.terminal_fd, state.shell_pgid);
    }

    // Set up signal handlers
    installHandlers();
}

// =============================================================================
// Signal Handlers
// =============================================================================

/// Install signal handlers for interactive shell operation.
fn installHandlers() void {
    // Ignore job control signals in the shell itself
    const ignore_act = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.IGN },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.TSTP, &ignore_act, null);
    std.posix.sigaction(std.posix.SIG.TTIN, &ignore_act, null);
    std.posix.sigaction(std.posix.SIG.TTOU, &ignore_act, null);

    // Set up SIGINT handler to allow interrupting loops and builtins
    const int_act = std.posix.Sigaction{
        .handler = .{ .handler = sigintHandler },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.INT, &int_act, null);

    // Set up SIGCHLD handler to reap children and update job status
    const chld_act = std.posix.Sigaction{
        .handler = .{ .handler = sigchldHandler },
        .mask = std.posix.sigemptyset(),
        .flags = std.posix.SA.RESTART | std.posix.SA.NOCLDSTOP,
    };
    std.posix.sigaction(std.posix.SIG.CHLD, &chld_act, null);
}

fn sigchldHandler(_: c_int) callconv(.c) void {
    // Reap terminated background jobs only (non-blocking).
    // We specifically waitpid on each known background job PID rather than
    // using waitpid(-1), which would also reap foreground children and steal
    // their exit status from the foreground waiter.
    const state = g_state orelse return;

    var iter = state.jobs.iter();
    while (iter.next()) |job| {
        if (job.status == .done) continue;

        for (job.pids) |pid| {
            var status: u32 = 0;
            const result = posix.waitpid(pid, &status, posix.WNOHANG | posix.WUNTRACED);
            if (result > 0) {
                state.jobs.updateStatus(pid, status);
            }
        }
    }
}

fn sigintHandler(_: c_int) callconv(.c) void {
    // Set the interrupted flag so loops can check and break
    if (g_state) |state| {
        state.interrupted = true;
    }
}

// =============================================================================
// Job Continuation (fg/bg builtins)
// =============================================================================

/// Continue a stopped job in the foreground.
///
/// Used by the `fg` builtin. Gives terminal control to the job,
/// sends SIGCONT, and waits for the job to complete or stop again.
pub fn continueJobForeground(state: *State, job_id: u16) u8 {
    const job = state.jobs.get(job_id) orelse {
        io.printError("oshen: fg: no such job: %{d}\n", .{job_id});
        return 1;
    };

    io.printStdout("{s}\n", .{job.cmd});
    job.status = .running;

    // Give terminal to job
    if (state.interactive) {
        _ = posix.tcsetpgrp(state.terminal_fd, job.pgid);
    }

    // Send SIGCONT
    _ = posix.kill(-job.pgid, std.posix.SIG.CONT);

    // Wait for job to complete or stop
    var last_status: u8 = 0;
    for (job.pids) |pid| {
        last_status = waitForeground(pid);
    }

    // Take back terminal
    if (state.interactive) {
        _ = posix.tcsetpgrp(state.terminal_fd, state.shell_pgid);
    }

    // Check if job stopped again
    var status: u32 = 0;
    const result = posix.waitpid(job.pgid, &status, posix.WNOHANG | posix.WUNTRACED);
    if (result > 0 and std.posix.W.IFSTOPPED(status)) {
        job.status = .stopped;
        io.printStdout("\n[{d}]+  Stopped                 {s}\n", .{ job.id, job.cmd });
    } else {
        state.jobs.remove(job_id);
    }

    return last_status;
}

/// Continue a stopped job in the background.
///
/// Used by the `bg` builtin. Sends SIGCONT without giving terminal control,
/// allowing the job to run in the background.
pub fn continueJobBackground(state: *State, job_id: u16) u8 {
    const job = state.jobs.get(job_id) orelse {
        io.printError("oshen: bg: no such job: %{d}\n", .{job_id});
        return 1;
    };

    io.printStdout("[{d}] {s} &\n", .{ job.id, job.cmd });
    job.status = .running;

    // Send SIGCONT
    _ = posix.kill(-job.pgid, std.posix.SIG.CONT);

    return 0;
}

// =============================================================================
// Wait Helpers
// =============================================================================

/// Wait for a foreground process to exit or stop.
///
/// Blocks until the process exits normally, is killed by a signal, or is
/// stopped (Ctrl-Z). This is used for foreground pipeline execution.
///
/// Returns exit status:
/// - Normal exit: the exit code (0-255)
/// - Killed by signal: 128 + signal number
/// - Stopped (Ctrl-Z): 128 + stop signal number
pub fn waitForeground(pid: std.posix.pid_t) u8 {
    var status: u32 = 0;
    _ = posix.waitpid(pid, &status, posix.WUNTRACED);

    if (std.posix.W.IFEXITED(status)) {
        return std.posix.W.EXITSTATUS(status);
    } else if (std.posix.W.IFSIGNALED(status)) {
        return 128 + @as(u8, @intCast(std.posix.W.TERMSIG(status)));
    } else if (std.posix.W.IFSTOPPED(status)) {
        // Process was stopped (Ctrl-Z)
        return 128 + @as(u8, @intCast(std.posix.W.STOPSIG(status)));
    }

    return 1;
}
