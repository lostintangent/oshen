//! Terminal I/O module
//!
//! Provides buffered and unbuffered I/O primitives for the shell.
//!
//! ## Architecture
//!
//! ```
//! ┌─────────────────────────────────────────────────────────┐
//! │                    terminal/io.zig                      │
//! │                   (this file - index)                   │
//! ├─────────────────────────────────────────────────────────┤
//! │  ┌─────────┐    ┌──────────┐    ┌──────────┐           │
//! │  │ io/     │    │ io/      │    │ io/      │           │
//! │  │ raw.zig │    │ writer   │    │ reader   │           │
//! │  │         │    │ .zig     │    │ .zig     │           │
//! │  │ Direct  │    │ Stdout   │    │ Stdin    │           │
//! │  │ syscall │───▶│ Writer   │    │ Reader   │           │
//! │  │ wrappers│    │ (64KB)   │    │ (4KB)    │           │
//! │  └─────────┘    └──────────┘    └──────────┘           │
//! └─────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Usage
//!
//! ```zig
//! const io = @import("terminal/io.zig");
//!
//! // Low-level (unbuffered)
//! io.writeStdout("hello");
//! io.writeStderr("error");
//!
//! // Buffered stdout
//! var w = io.StdoutWriter{};
//! w.write("hello");
//! w.writeInt(42);
//! w.flush();
//!
//! // Buffered stdin
//! var r = io.StdinReader.init();
//! var buf: [io.reader.MAX_LINE_SIZE]u8 = undefined;
//! while (r.readLine(&buf)) |line| {
//!     // process line
//! }
//! ```

// Re-export all public APIs
pub const raw = @import("io/raw.zig");
pub const writer = @import("io/writer.zig");
pub const reader = @import("io/reader.zig");

// Convenience re-exports for common types
pub const StdoutWriter = writer.StdoutWriter;
pub const StdinReader = reader.StdinReader;

// Backwards compatibility alias
pub const Writer = StdoutWriter;

// Shared constants
pub const BUF_SIZE = 65536;

// Direct re-exports from raw for backwards compatibility
pub const writeStdout = raw.writeStdout;
pub const writeStderr = raw.writeStderr;
pub const writeToFd = raw.writeToFd;
pub const printError = raw.printError;
pub const printStdout = raw.printStdout;
pub const isStdoutTty = raw.isStdoutTty;

// Capture mode - redirects writeStdout to a StdoutWriter
pub const startCapture = raw.startCapture;
pub const endCapture = raw.endCapture;

// Escape sequence handling
pub const writeEscaped = raw.writeEscaped;
pub const parseOctal = raw.parseOctal;
