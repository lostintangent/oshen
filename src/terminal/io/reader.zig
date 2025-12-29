//! Buffered input reader for stdin
//!
//! Provides efficient line-by-line reading from stdin with minimal syscalls.
//! Used by string and other builtins that process piped input.
//!
//! Performance characteristics:
//!   - Reads in 4KB chunks to minimize syscalls
//!   - Zero heap allocations
//!   - Handles partial lines at EOF correctly

const std = @import("std");

/// Maximum line length supported (64KB)
pub const MAX_LINE_SIZE = 65536;

/// Internal buffer size for stdin reads.
/// 4KB matches the typical pipe buffer size on Linux/macOS, providing a good
/// balance between syscall reduction and memory usage. Larger buffers show
/// diminishing returns for line-oriented input.
const READ_BUF_SIZE = 4096;

/// Buffered line reader for stdin.
///
/// Usage:
/// ```zig
/// var reader = StdinReader.init();
/// var line_buf: [MAX_LINE_SIZE]u8 = undefined;
/// while (reader.readLine(&line_buf)) |line| {
///     // process line (without trailing newline)
/// }
/// ```
pub const StdinReader = struct {
    buf: [READ_BUF_SIZE]u8 = undefined,
    pos: usize = 0,
    len: usize = 0,
    eof: bool = false,

    pub fn init() StdinReader {
        return .{};
    }

    /// Read the next line from stdin into the provided buffer.
    /// Returns the line content (without trailing newline), or null at EOF.
    pub fn readLine(self: *StdinReader, out: *[MAX_LINE_SIZE]u8) ?[]const u8 {
        var out_pos: usize = 0;

        while (true) {
            // Scan buffer for newline
            while (self.pos < self.len) {
                const c = self.buf[self.pos];
                self.pos += 1;
                if (c == '\n') return out[0..out_pos];
                if (out_pos < MAX_LINE_SIZE) {
                    out[out_pos] = c;
                    out_pos += 1;
                }
            }

            // EOF reached previously - return any remaining data
            if (self.eof) return if (out_pos > 0) out[0..out_pos] else null;

            // Refill buffer from stdin
            const n = std.posix.read(std.posix.STDIN_FILENO, &self.buf) catch return null;
            if (n == 0) {
                self.eof = true;
                return if (out_pos > 0) out[0..out_pos] else null;
            }
            self.pos = 0;
            self.len = n;
        }
    }
};
