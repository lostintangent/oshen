//! jobs builtin - list and select background/stopped jobs
//!
//! In interactive mode, displays a TUI for selecting jobs.
//! In non-interactive mode (pipes, redirects), prints a simple list.
//!
//! NOTE: Does not use Args API - takes zero arguments, simply lists jobs.
//! NOTE: No unit or E2E tests - requires TTY and real process groups.

const std = @import("std");
const builtins = @import("../builtins.zig");
const tui = @import("../../terminal/tui.zig");
const ansi = @import("../../terminal/ansi.zig");
const signals = @import("../../interpreter/execution/signals.zig");
const runtime_jobs = @import("../jobs.zig");

pub const builtin = builtins.Builtin{
    .name = "jobs",
    .run = run,
    .help = "jobs - List background and stopped jobs",
};

fn run(state: *builtins.State, _: []const []const u8) u8 {
    // Interactive mode requires TTY on both stdin and stdout
    const stdin_tty = std.posix.isatty(std.posix.STDIN_FILENO);
    const stdout_tty = std.posix.isatty(std.posix.STDOUT_FILENO);

    if (state.interactive and stdin_tty and stdout_tty) {
        if (selectJob(state)) |job_id| {
            return signals.continueJobForeground(state, job_id);
        }
        return 0;
    }

    // Non-interactive: simple list output
    var iter = state.jobs.iter();
    while (iter.next()) |job| {
        const marker: u8 = if (job.status == .running) '+' else '-';
        builtins.io.printStdout("[{d}]{c}  {s}  {s}\n", .{ job.id, marker, job.status.str(), job.cmd });
    }
    return 0;
}

fn printJob(job: *const runtime_jobs.Job, highlighted: bool) void {
    const marker: u8 = if (job.status == .running) '+' else '-';

    if (highlighted) {
        builtins.io.printStdout(ansi.bg_dark_gray, .{});
    }

    builtins.io.printStdout("[{d: >2}]{c}  {s: <7}    {s}", .{ job.id, marker, job.status.str(), job.cmd });
    builtins.io.printStdout(ansi.clear_line_right ++ ansi.reset ++ "\n", .{});
}

fn selectJob(state: *builtins.State) ?u16 {
    var jobs_list: std.ArrayListUnmanaged(*runtime_jobs.Job) = .empty;
    defer jobs_list.deinit(state.allocator);

    var iter = state.jobs.iter();
    while (iter.next()) |job| {
        jobs_list.append(state.allocator, job) catch return null;
    }

    if (jobs_list.items.len == 0) return null;

    const fd = std.posix.STDIN_FILENO;
    const orig_termios = tui.enableRawMode(fd) catch return null;

    defer {
        builtins.io.writeStdout(ansi.cursor_show);
        tui.restoreTerminal(fd, orig_termios);
    }

    builtins.io.writeStdout(ansi.cursor_hide);

    var selected_index: usize = 0;
    var first_frame: bool = true;

    while (true) {
        if (!first_frame) {
            // Move cursor up N lines and to column 1 to overwrite previous frame
            var move_buf: [16]u8 = undefined;
            const move_up = std.fmt.bufPrint(&move_buf, "\x1b[{d}A\r", .{jobs_list.items.len}) catch "";
            builtins.io.writeStdout(move_up);
        }
        first_frame = false;

        for (jobs_list.items, 0..) |job, i| {
            builtins.io.writeStdout("\r\x1b[2K");
            printJob(job, i == selected_index);
        }

        const key = tui.readKey(fd) catch break;
        switch (key) {
            .up => {
                if (selected_index > 0) selected_index -= 1;
            },
            .down => {
                if (selected_index < jobs_list.items.len - 1) selected_index += 1;
            },
            .enter => {
                clearJobLines(jobs_list.items.len);
                return jobs_list.items[selected_index].id;
            },
            .escape, .interrupt, .eof => {
                clearJobLines(jobs_list.items.len);
                return null;
            },
            .char => |c| {
                if (c == 'q') {
                    clearJobLines(jobs_list.items.len);
                    return null;
                }
            },
            else => {},
        }
    }
    return null;
}

fn clearJobLines(count: usize) void {
    var buf: [16]u8 = undefined;
    const move_up = std.fmt.bufPrint(&buf, "\x1b[{d}A", .{count}) catch return;
    builtins.io.writeStdout(move_up);
    for (0..count) |_| {
        builtins.io.writeStdout("\r\x1b[2K\n");
    }
    builtins.io.writeStdout(move_up);
}
