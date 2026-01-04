//! Raw terminal I/O - direct syscall wrappers
//!
//! Low-level, unbuffered I/O operations. Each call results in a syscall.
//! For high-throughput output, use StdoutWriter instead.
//!
//! Supports capture mode for zero-syscall builtin output capture.
//! When active, writeStdout redirects to a StdoutWriter instead of stdout.
//!
//! Thread Safety: This module uses global state for capture mode. It is designed
//! for single-threaded shell execution. Do not use from multiple threads.

const std = @import("std");
const StdoutWriter = @import("writer.zig").StdoutWriter;

// =============================================================================
// Capture Mode
// =============================================================================

/// When set, writeStdout redirects to this writer instead of stdout.
/// This reuses StdoutWriter's buffering logic - no separate implementation needed.
///
/// Note: This is global state, safe only in single-threaded context.
/// The shell is single-threaded by design (like bash, zsh, fish).
var capture_writer: ?*StdoutWriter = null;

/// Enable capture mode - all writeStdout calls will write to this writer.
/// Returns the previous capture writer (for nested capture support).
pub fn startCapture(w: *StdoutWriter) ?*StdoutWriter {
    const prev = capture_writer;
    capture_writer = w;
    return prev;
}

/// Restore previous capture mode (or disable if prev is null).
pub fn endCapture(prev: ?*StdoutWriter) void {
    capture_writer = prev;
}

/// Check if capture mode is currently active.
pub fn isCaptureActive() bool {
    return capture_writer != null;
}

// =============================================================================
// Basic I/O
// =============================================================================

/// Check if stdout is connected to a TTY
pub fn isStdoutTty() bool {
    return std.posix.isatty(std.posix.STDOUT_FILENO);
}

/// Write data to a file descriptor, ignoring errors
pub fn writeToFd(fd: std.posix.fd_t, data: []const u8) void {
    _ = std.posix.write(fd, data) catch {};
}

/// Write data to stdout. In capture mode, writes to the capture Writer instead.
pub fn writeStdout(data: []const u8) void {
    if (capture_writer) |w| {
        w.writeNoFlush(data);
    } else {
        writeToFd(std.posix.STDOUT_FILENO, data);
    }
}

/// Write data to stderr, ignoring errors
pub fn writeStderr(data: []const u8) void {
    writeToFd(std.posix.STDERR_FILENO, data);
}

/// Print a formatted error message to stderr
pub fn printError(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch {
        writeStderr("oshen: format error\n");
        return;
    };
    writeStderr(msg);
}

/// Print a formatted message to stdout. Respects capture mode.
pub fn printStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch {
        writeStderr("oshen: format error\n");
        return;
    };
    writeStdout(msg);
}

// =============================================================================
// Escape Sequence Handling
// =============================================================================

/// Parse an octal escape sequence (\0, \033, etc.)
/// Returns the decoded value and number of characters consumed.
pub fn parseOctal(s: []const u8) struct { value: u8, len: usize } {
    var value: u8 = 0;
    var len: usize = 0;

    for (s[0..@min(s.len, 3)]) |c| {
        if (c >= '0' and c <= '7') {
            value = value *| 8 +| (c - '0');
            len += 1;
        } else break;
    }

    return .{ .value = value, .len = len };
}

/// Write a string to stdout interpreting escape sequences: \e, \n, \t, \r, \\, \0NNN
/// This is the unbuffered version - for performance-critical code, use StdoutWriter.writeEscaped()
pub fn writeEscaped(s: []const u8) void {
    var i: usize = 0;
    while (i < s.len) {
        if (s[i] == '\\' and i + 1 < s.len) {
            switch (s[i + 1]) {
                'e' => {
                    writeStdout("\x1b");
                    i += 2;
                },
                'n' => {
                    writeStdout("\n");
                    i += 2;
                },
                't' => {
                    writeStdout("\t");
                    i += 2;
                },
                'r' => {
                    writeStdout("\r");
                    i += 2;
                },
                '\\' => {
                    writeStdout("\\");
                    i += 2;
                },
                '0' => {
                    const octal = parseOctal(s[i + 1 ..]);
                    if (octal.len > 0) {
                        writeStdout(&[1]u8{octal.value});
                        i += 1 + octal.len;
                    } else {
                        writeStdout("\\");
                        i += 1;
                    }
                },
                else => {
                    writeStdout(&[1]u8{s[i]});
                    i += 1;
                },
            }
        } else {
            writeStdout(&[1]u8{s[i]});
            i += 1;
        }
    }
}
