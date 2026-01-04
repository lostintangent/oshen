//! Buffered output writer for stdout
//!
//! Coalesces multiple writes into a single syscall for performance.
//! Used by string, print, echo, range, and other builtins that produce output.
//!
//! Performance characteristics:
//!   - Single syscall for outputs under 64KB
//!   - Automatic flush on overflow
//!   - Zero heap allocations

const std = @import("std");
const raw = @import("raw.zig");

/// Buffer size for stdout operations (64KB)
pub const BUF_SIZE = 65536;

/// Buffered writer that coalesces output into minimal syscalls.
///
/// Usage:
/// ```zig
/// var w = StdoutWriter{};
/// w.write("hello ");
/// w.writeInt(42);
/// w.writeByte('\n');
/// w.flush();
/// ```
pub const StdoutWriter = struct {
    buf: [BUF_SIZE]u8 = undefined,
    pos: usize = 0,

    pub fn init() StdoutWriter {
        return .{};
    }

    /// Write a slice to the buffer, flushing if needed
    pub fn write(self: *@This(), data: []const u8) void {
        if (data.len == 0) return;
        if (self.pos + data.len <= BUF_SIZE) {
            @memcpy(self.buf[self.pos..][0..data.len], data);
            self.pos += data.len;
        } else {
            self.flush();
            if (data.len <= BUF_SIZE) {
                @memcpy(self.buf[0..data.len], data);
                self.pos = data.len;
            } else {
                // Data larger than buffer - write directly
                raw.writeStdout(data);
            }
        }
    }

    /// Write without flushing - used by capture mode to accumulate output.
    /// Silently truncates if buffer is full (capture has fixed capacity).
    pub fn writeNoFlush(self: *@This(), data: []const u8) void {
        const available = BUF_SIZE - self.pos;
        const to_copy = @min(data.len, available);
        @memcpy(self.buf[self.pos..][0..to_copy], data[0..to_copy]);
        self.pos += to_copy;
    }

    /// Write a single byte
    pub fn writeByte(self: *@This(), byte: u8) void {
        if (self.pos < BUF_SIZE) {
            self.buf[self.pos] = byte;
            self.pos += 1;
        } else {
            self.flush();
            self.buf[0] = byte;
            self.pos = 1;
        }
    }

    /// Write multiple copies of a byte
    pub fn writeByteN(self: *@This(), byte: u8, count: usize) void {
        for (0..count) |_| self.writeByte(byte);
    }

    /// Write an unsigned integer as decimal
    pub fn writeInt(self: *@This(), n: usize) void {
        var tmp: [20]u8 = undefined;
        const s = std.fmt.bufPrint(&tmp, "{d}", .{n}) catch return;
        self.write(s);
    }

    /// Write a signed integer as decimal
    pub fn writeSignedInt(self: *@This(), n: i64) void {
        var tmp: [21]u8 = undefined;
        const s = std.fmt.bufPrint(&tmp, "{d}", .{n}) catch return;
        self.write(s);
    }

    /// Flush buffer to stdout
    pub fn flush(self: *@This()) void {
        if (self.pos > 0) {
            raw.writeStdout(self.buf[0..self.pos]);
            self.pos = 0;
        }
    }

    /// Write with escape sequence interpretation (\e, \0NNN, \n, \t, etc.)
    /// This is the buffered version - more efficient than raw.writeEscaped for
    /// high-throughput output.
    pub fn writeEscaped(self: *@This(), s: []const u8) void {
        var i: usize = 0;
        while (i < s.len) {
            if (s[i] == '\\' and i + 1 < s.len) {
                switch (s[i + 1]) {
                    'e' => {
                        self.writeByte('\x1b');
                        i += 2;
                    },
                    'n' => {
                        self.writeByte('\n');
                        i += 2;
                    },
                    't' => {
                        self.writeByte('\t');
                        i += 2;
                    },
                    'r' => {
                        self.writeByte('\r');
                        i += 2;
                    },
                    '\\' => {
                        self.writeByte('\\');
                        i += 2;
                    },
                    '0' => {
                        const octal = raw.parseOctal(s[i + 1 ..]);
                        if (octal.len > 0) {
                            self.writeByte(octal.value);
                            i += 1 + octal.len;
                        } else {
                            self.writeByte('\\');
                            i += 1;
                        }
                    },
                    else => {
                        self.writeByte(s[i]);
                        i += 1;
                    },
                }
            } else {
                self.writeByte(s[i]);
                i += 1;
            }
        }
    }
};

// Re-export parseOctal for backwards compatibility (used by ansi.zig)
pub const parseOctal = raw.parseOctal;

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "StdoutWriter: basic operations" {
    var w = StdoutWriter{};
    w.write("hello");
    w.writeByte(' ');
    w.write("world");
    try testing.expectEqualStrings("hello world", w.buf[0..w.pos]);
}

test "StdoutWriter: writeInt" {
    var w = StdoutWriter{};
    w.writeInt(12345);
    try testing.expectEqualStrings("12345", w.buf[0..w.pos]);
}

test "StdoutWriter: writeSignedInt" {
    var w = StdoutWriter{};
    w.writeSignedInt(-42);
    try testing.expectEqualStrings("-42", w.buf[0..w.pos]);
}

test "StdoutWriter: writeByteN" {
    var w = StdoutWriter{};
    w.writeByteN('=', 5);
    try testing.expectEqualStrings("=====", w.buf[0..w.pos]);
}

test "parseOctal" {
    const r1 = parseOctal("033");
    try testing.expectEqual(@as(u8, 27), r1.value);
    try testing.expectEqual(@as(usize, 3), r1.len);

    const r2 = parseOctal("0");
    try testing.expectEqual(@as(u8, 0), r2.value);
    try testing.expectEqual(@as(usize, 1), r2.len);

    const r3 = parseOctal("777");
    try testing.expectEqual(@as(u8, 255), r3.value);
    try testing.expectEqual(@as(usize, 3), r3.len);
}
