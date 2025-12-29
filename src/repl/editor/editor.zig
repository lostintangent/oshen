//! Editor: interactive line editor with raw mode terminal handling.
//!
//! Provides a readline-like editing experience with:
//! - Cursor movement (arrows, Home/End, word-by-word with Ctrl+arrows)
//! - Text manipulation (insert, delete, kill word/line)
//! - History navigation (up/down arrows)
//! - Syntax highlighting (via highlight.zig)
//! - Autosuggestions from history (via suggest.zig)
//! - Tab completion (via complete.zig)
//!
//! The editor operates in raw terminal mode to capture individual keystrokes.
//! Terminal state is saved on init and restored on deinit or when executing commands.

const std = @import("std");

const history = @import("history.zig");
const highlight = @import("ui/highlight.zig");
const suggest = @import("ui/suggest.zig");
const complete = @import("ui/complete.zig");
const repl = @import("../repl.zig");
const State = @import("../../runtime/state.zig").State;
const ansi = @import("../../terminal/ansi.zig");
const tui = @import("../../terminal/tui.zig");

const History = history.History;
const posix = std.posix;
const Termios = std.posix.termios;
const system = std.posix.system;

// =============================================================================
// Word Boundaries
// =============================================================================

/// Find the position of the previous word boundary (moving left).
/// Skips trailing whitespace, then skips the word.
fn findWordBoundaryLeft(buf: []const u8, cursor: usize) usize {
    var pos = cursor;
    while (pos > 0 and repl.isWordBreak(buf[pos - 1])) pos -= 1;
    while (pos > 0 and !repl.isWordBreak(buf[pos - 1])) pos -= 1;
    return pos;
}

/// Find the position of the next word boundary (moving right).
/// Skips the current word, then skips whitespace.
fn findWordBoundaryRight(buf: []const u8, cursor: usize) usize {
    var pos = cursor;
    while (pos < buf.len and !repl.isWordBreak(buf[pos])) pos += 1;
    while (pos < buf.len and repl.isWordBreak(buf[pos])) pos += 1;
    return pos;
}

// =============================================================================
// Terminal I/O
// =============================================================================

/// Write bytes directly to stdout.
fn writeToTerminal(bytes: []const u8) !void {
    _ = try posix.write(posix.STDOUT_FILENO, bytes);
}

/// Get the current terminal width, or null if unavailable
fn getTerminalWidth() ?usize {
    var ws: system.winsize = undefined;
    const rc = system.ioctl(posix.STDOUT_FILENO, system.T.IOCGWINSZ, @intFromPtr(&ws));
    if (rc == 0 and ws.col > 0) {
        return ws.col;
    }
    return null;
}

// =============================================================================
// Editor
// =============================================================================

/// Interactive line editor state.
pub const Editor = struct {
    allocator: std.mem.Allocator,
    /// The line buffer
    buf: std.ArrayListUnmanaged(u8),
    /// Cursor position in the buffer
    cursor: usize,
    /// Original terminal settings (to restore on exit)
    orig_termios: ?Termios,
    /// History manager
    hist: history.History,
    /// Current history navigation index (null = editing new line)
    hist_index: ?usize,
    /// Saved current line when navigating history
    saved_line: ?[]u8,
    /// Terminal file descriptor
    tty_fd: posix.fd_t,
    /// Current prompt (stored for refresh)
    prompt: []const u8,
    /// Enable syntax highlighting
    highlighting: bool,
    /// Enable autosuggestions
    suggestions: bool,
    /// Shell state (for alias checking in highlighting)
    state: ?*State,
    /// Whether the terminal currently has focus (controls redraw and suggestions)
    has_focus: bool,
    /// Whether the display has been initialized (safe to render). Currently driven
    /// by focus events: first focus signals the terminal is visible and properly sized.
    display_initialized: bool,

    pub fn init(allocator: std.mem.Allocator) Editor {
        return .{
            .allocator = allocator,
            .buf = .empty,
            .cursor = 0,
            .orig_termios = null,
            .hist = history.History.init(allocator),
            .hist_index = null,
            .saved_line = null,
            .tty_fd = posix.STDIN_FILENO,
            .prompt = "",
            .highlighting = true,
            .suggestions = true,
            .state = null,
            .has_focus = false,
            .display_initialized = false,
        };
    }

    pub fn deinit(self: *Editor) void {
        self.restoreTerminal();
        self.buf.deinit(self.allocator);
        self.hist.deinit();
        if (self.saved_line) |line| {
            self.allocator.free(line);
        }
    }

    /// Load history from file
    pub fn loadHistory(self: *Editor, path: []const u8) void {
        self.hist.load(path);
    }

    /// Save history to file
    pub fn saveHistory(self: *Editor, path: []const u8) void {
        self.hist.save(path);
    }

    /// Enable raw mode for the terminal
    pub fn enableRawMode(self: *Editor) !void {
        if (self.orig_termios != null) return; // Already in raw mode

        self.orig_termios = try tui.enableRawMode(self.tty_fd);

        // Enable focus reporting so we can suppress suggestions when unfocused
        tui.enableFocusReporting(self.tty_fd);
    }

    /// Restore terminal to original settings
    pub fn restoreTerminal(self: *Editor) void {
        if (self.tty_fd < 0) return; // Skip for test instances
        if (self.orig_termios) |orig| {
            // Disable focus reporting before restoring cooked mode to avoid stray focus escapes
            tui.disableFocusReporting(self.tty_fd);

            tui.restoreTerminal(self.tty_fd, orig);
            self.orig_termios = null;
        }
    }

    /// Read a line from the user. Returns null on EOF (ctrl+d on empty line).
    /// Assumes raw mode is already enabled (call enableRawMode before first use).
    pub fn readLine(self: *Editor, prompt_text: []const u8) !?[]const u8 {
        self.prompt = prompt_text;
        self.buf.clearRetainingCapacity();
        self.cursor = 0;
        self.hist_index = null;
        if (self.saved_line) |line| {
            self.allocator.free(line);
            self.saved_line = null;
        }

        try writeToTerminal(prompt_text);

        while (true) {
            const key = try tui.readKey(self.tty_fd);

            switch (key) {
                .enter => {
                    // Clear suggestion text before newline by rewriting line without suggestion
                    try writeToTerminal("\r\x1b[K");
                    try writeToTerminal(self.prompt);
                    try writeToTerminal(self.buf.items);
                    try writeToTerminal("\r\n");
                    return try self.allocator.dupe(u8, self.buf.items);
                },
                .eof => {
                    if (self.buf.items.len == 0) {
                        try writeToTerminal("\r\n");
                        return null;
                    }
                },
                .char => |c| try self.insertChar(c),
                .backspace => try self.deleteBackward(),
                .delete => try self.deleteForward(),
                .left => try self.moveCursorLeft(),
                .right => try self.acceptSuggestionOrMoveRight(),
                .home => try self.moveCursorHome(),
                .end => try self.acceptSuggestionOrMoveEnd(),
                .word_left => try self.moveCursorWordLeft(),
                .word_right => try self.moveCursorWordRight(),
                .up => try self.historyPrev(),
                .down => try self.historyNext(),
                .kill_line => try self.killLine(),
                .kill_word => try self.killWord(),
                .clear_screen => try self.clearScreen(),
                .interrupt => {
                    try writeToTerminal("^C\r\n");
                    self.buf.clearRetainingCapacity();
                    self.cursor = 0;
                    try writeToTerminal(self.prompt);
                },
                .ctrl => |c| switch (c) {
                    'a' => try self.moveCursorHome(),
                    'e' => try self.acceptSuggestionOrMoveEnd(),
                    'b' => try self.moveCursorLeft(),
                    'f' => try self.acceptSuggestionOrMoveRight(),
                    'h' => try self.deleteBackward(),
                    'u' => try self.killToStart(),
                    else => {},
                },
                .tab => try self.handleTab(),
                .focus_in => {
                    self.has_focus = true;
                    self.display_initialized = true;
                    try self.refreshLine();
                },
                .focus_out => {
                    self.has_focus = false;
                },
                else => {},
            }
        }
    }

    /// Insert a character at the cursor position
    fn insertChar(self: *Editor, c: u8) !void {
        try self.buf.insert(self.allocator, self.cursor, c);
        self.cursor += 1;
        try self.refreshLine();
    }

    /// Delete the character before the cursor
    fn deleteBackward(self: *Editor) !void {
        if (self.cursor > 0 and self.buf.items.len > 0) {
            _ = self.buf.orderedRemove(self.cursor - 1);
            self.cursor -= 1;
            try self.refreshLine();
        }
    }

    /// Delete the character at the cursor
    fn deleteForward(self: *Editor) !void {
        if (self.cursor < self.buf.items.len) {
            _ = self.buf.orderedRemove(self.cursor);
            try self.refreshLine();
        }
    }

    /// Move cursor left
    fn moveCursorLeft(self: *Editor) !void {
        if (self.cursor > 0) {
            self.cursor -= 1;
            try writeToTerminal("\x1b[D");
        }
    }

    /// Move cursor right
    fn moveCursorRight(self: *Editor) !void {
        if (self.cursor < self.buf.items.len) {
            self.cursor += 1;
            try writeToTerminal("\x1b[C");
        }
    }

    /// Move cursor to start of line
    fn moveCursorHome(self: *Editor) !void {
        self.cursor = 0;
        try self.refreshLine();
    }

    /// Move cursor to end of line
    fn moveCursorEnd(self: *Editor) !void {
        self.cursor = self.buf.items.len;
        try self.refreshLine();
    }

    /// Move cursor to previous word boundary
    fn moveCursorWordLeft(self: *Editor) !void {
        self.cursor = findWordBoundaryLeft(self.buf.items, self.cursor);
        try self.refreshLine();
    }

    /// Move cursor to next word boundary
    fn moveCursorWordRight(self: *Editor) !void {
        self.cursor = findWordBoundaryRight(self.buf.items, self.cursor);
        try self.refreshLine();
    }

    /// Delete from cursor to end of line
    fn killLine(self: *Editor) !void {
        self.buf.shrinkRetainingCapacity(self.cursor);
        try self.refreshLine();
    }

    /// Delete from start to cursor
    fn killToStart(self: *Editor) !void {
        if (self.cursor > 0) {
            std.mem.copyForwards(u8, self.buf.items[0..], self.buf.items[self.cursor..]);
            self.buf.shrinkRetainingCapacity(self.buf.items.len - self.cursor);
            self.cursor = 0;
            try self.refreshLine();
        }
    }

    /// Delete the word before cursor
    fn killWord(self: *Editor) !void {
        if (self.cursor == 0) return;

        const end = self.cursor;
        self.cursor = findWordBoundaryLeft(self.buf.items, self.cursor);

        // Shift remaining content left and shrink (O(n) instead of O(n²))
        const remaining = self.buf.items.len - end;
        std.mem.copyForwards(u8, self.buf.items[self.cursor..], self.buf.items[end..][0..remaining]);
        self.buf.shrinkRetainingCapacity(self.cursor + remaining);
        try self.refreshLine();
    }

    /// Clear screen and redraw
    fn clearScreen(self: *Editor) !void {
        try writeToTerminal(ansi.cursor_home ++ ansi.clear_screen);
        try writeToTerminal(self.prompt);
        try self.refreshLine();
    }

    /// Navigate to previous history entry
    fn historyPrev(self: *Editor) !void {
        if (self.hist.entries.items.len == 0) return;

        if (self.hist_index == null) {
            if (self.saved_line) |old| self.allocator.free(old);
            self.saved_line = try self.allocator.dupe(u8, self.buf.items);
            self.hist_index = self.hist.entries.items.len;
        }

        if (self.hist_index.? > 0) {
            self.hist_index.? -= 1;
            try self.setLineContent(self.hist.entries.items[self.hist_index.?].command);
            try self.refreshLine();
        }
    }

    /// Navigate to next history entry
    fn historyNext(self: *Editor) !void {
        if (self.hist_index == null) return;

        if (self.hist_index.? < self.hist.entries.items.len - 1) {
            self.hist_index.? += 1;
            try self.setLineContent(self.hist.entries.items[self.hist_index.?].command);
        } else {
            self.hist_index = null;
            if (self.saved_line) |saved| {
                try self.setLineContent(saved);
                self.allocator.free(saved);
                self.saved_line = null;
            } else {
                self.buf.clearRetainingCapacity();
                self.cursor = 0;
            }
        }
        try self.refreshLine();
    }

    /// Set line content from history
    fn setLineContent(self: *Editor, content: []const u8) !void {
        self.buf.clearRetainingCapacity();
        try self.buf.appendSlice(self.allocator, content);
        self.cursor = self.buf.items.len;
    }

    /// Refresh the display of the current line
    fn refreshLine(self: *Editor) !void {
        // Skip redraws until display is initialized (terminal visible and sized).
        // Once initialized, refresh even when unfocused to support programmatic PTY input.
        if (!self.has_focus and !self.display_initialized) return;

        var out_buf: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&out_buf);
        const writer = stream.writer();

        // Clear line and write prompt
        try writer.writeAll("\r\x1b[K");
        try writer.writeAll(self.prompt);

        // Write input (with optional highlighting; skip highlighting when unfocused for perf)
        if (self.highlighting and self.has_focus and self.buf.items.len > 0) {
            highlight.render(self.allocator, self.buf.items, writer, self.state) catch {
                // Fallback to plain text on error
                try writer.writeAll(self.buf.items);
            };
        } else {
            try writer.writeAll(self.buf.items);
        }

        // Write suggestion suffix (dimmed) if cursor is at end and focused
        if (self.suggestions and self.has_focus and self.cursor == self.buf.items.len) {
            const cwd = if (self.state) |s| s.getCwd() catch "" else "";
            if (suggest.fromHistory(self.buf.items, &self.hist, cwd)) |suffix| {
                // Truncate suggestion to fit terminal width
                const display_suffix = if (getTerminalWidth()) |term_width| blk: {
                    const used = ansi.displayLength(self.prompt) + self.buf.items.len;
                    if (used >= term_width) break :blk suffix[0..0];
                    const available = term_width - used - 1; // -1 to prevent cursor at edge
                    if (suffix.len <= available) {
                        break :blk suffix;
                    } else if (available > 3) {
                        break :blk suffix[0 .. available - 3]; // Leave room for "..."
                    } else {
                        break :blk suffix[0..0];
                    }
                } else suffix;

                if (display_suffix.len > 0) {
                    const truncated = display_suffix.len < suffix.len;
                    try writer.writeAll(ansi.dim);
                    try writer.writeAll(display_suffix);
                    if (truncated) try writer.writeAll("...");
                    try writer.writeAll(ansi.reset);
                    // Move cursor back to end of actual input
                    const move_back = display_suffix.len + if (truncated) @as(usize, 3) else 0;
                    try writer.print("\x1b[{d}D", .{move_back});
                }
            }
        }

        // Position cursor
        if (self.cursor < self.buf.items.len) {
            const back = self.buf.items.len - self.cursor;
            try writer.print("\x1b[{d}D", .{back});
        }

        try writeToTerminal(stream.getWritten());
    }

    /// Try to accept the current suggestion. Returns true if accepted.
    fn tryAcceptSuggestion(self: *Editor) !bool {
        if (!self.suggestions or self.cursor != self.buf.items.len) return false;
        const cwd = if (self.state) |s| s.getCwd() catch "" else "";
        if (suggest.fromHistory(self.buf.items, &self.hist, cwd)) |suffix| {
            try self.buf.appendSlice(self.allocator, suffix);
            self.cursor = self.buf.items.len;
            try self.refreshLine();
            return true;
        }
        return false;
    }

    /// Accept suggestion if at end of line, otherwise move cursor right
    fn acceptSuggestionOrMoveRight(self: *Editor) !void {
        if (try self.tryAcceptSuggestion()) return;
        if (self.cursor < self.buf.items.len) {
            self.cursor += 1;
            try writeToTerminal("\x1b[C");
        }
    }

    /// Accept suggestion if at end of line, otherwise move to end
    fn acceptSuggestionOrMoveEnd(self: *Editor) !void {
        if (try self.tryAcceptSuggestion()) return;
        if (self.cursor < self.buf.items.len) {
            self.cursor = self.buf.items.len;
            try self.refreshLine();
        }
    }

    /// Handle tab key for completion
    fn handleTab(self: *Editor) !void {
        var result = complete.complete(self.allocator, self.buf.items, self.cursor) catch return;
        if (result == null) return;
        defer result.?.deinit(self.allocator);

        const completions = result.?.completions;
        if (completions.len == 0) return;

        // Get the common prefix
        const prefix = result.?.commonPrefix();
        const word_start = result.?.word_start;
        const word_end = result.?.word_end;
        const current_word = self.buf.items[word_start..word_end];

        // If common prefix is longer than current word, insert it
        if (prefix.len > current_word.len) {
            // Replace current word with common prefix
            try self.replaceWord(word_start, word_end, prefix);
        } else if (completions.len == 1) {
            // Single completion - insert it with trailing space
            const comp_with_space = try std.fmt.allocPrint(self.allocator, "{s} ", .{completions[0]});
            defer self.allocator.free(comp_with_space);
            try self.replaceWord(word_start, word_end, comp_with_space);
        } else {
            // Multiple completions - show them
            try self.showCompletions(completions);
        }
    }

    /// Replace a word in the buffer
    fn replaceWord(self: *Editor, start: usize, end: usize, replacement: []const u8) !void {
        // Calculate new buffer size
        const old_len = end - start;
        const new_len = replacement.len;
        const buf_len = self.buf.items.len;

        if (new_len > old_len) {
            // Need to grow - insert extra space
            const extra = new_len - old_len;
            for (0..extra) |_| {
                try self.buf.insert(self.allocator, end, ' ');
            }
        } else if (new_len < old_len) {
            // Need to shrink - remove extra
            const remove = old_len - new_len;
            for (0..remove) |_| {
                _ = self.buf.orderedRemove(start);
            }
        }

        // Copy replacement into position
        @memcpy(self.buf.items[start .. start + new_len], replacement);

        // Update cursor
        self.cursor = start + new_len;

        // Recalculate buffer length
        _ = buf_len;

        try self.refreshLine();
    }

    /// Display multiple completions below the prompt
    fn showCompletions(self: *Editor, completions: []const []const u8) !void {
        // Move to new line and show completions
        try writeToTerminal("\r\n");

        // Display completions in columns
        for (completions) |c| {
            try writeToTerminal(c);
            try writeToTerminal("  ");
        }
        try writeToTerminal("\r\n");

        // Redraw prompt and line
        try writeToTerminal(self.prompt);
        try self.refreshLine();
    }

    // =========================================================================
    // Test helpers
    // =========================================================================

    fn initForTest(allocator: std.mem.Allocator) Editor {
        return .{
            .allocator = allocator,
            .buf = .{},
            .cursor = 0,
            .prompt = "$ ",
            .hist = History.init(allocator),
            .hist_index = null,
            .saved_line = null,
            .orig_termios = null,
            .tty_fd = -1,
            .highlighting = false,
            .suggestions = false,
            .state = null,
            .has_focus = true,
            .display_initialized = true,
        };
    }

    fn setBuffer(self: *Editor, content: []const u8) !void {
        self.buf.clearRetainingCapacity();
        try self.buf.appendSlice(self.allocator, content);
        self.cursor = content.len;
    }

    fn getBuffer(self: *const Editor) []const u8 {
        return self.buf.items;
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

const TestContext = struct {
    editor: Editor,

    fn init() TestContext {
        return .{ .editor = Editor.initForTest(testing.allocator) };
    }

    fn deinit(self: *TestContext) void {
        self.editor.deinit();
    }

    fn setBuffer(self: *TestContext, content: []const u8) !void {
        try self.editor.setBuffer(content);
    }

    fn getBuffer(self: *TestContext) []const u8 {
        return self.editor.getBuffer();
    }

    fn insert(self: *TestContext, pos: usize, char: u8) !void {
        try self.editor.buf.insert(testing.allocator, pos, char);
    }
};

// -----------------------------------------------------------------------------
// Buffer Operations
// -----------------------------------------------------------------------------

test "Buffer: insert operations" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Insert at end
    try ctx.insert(0, 'h');
    ctx.editor.cursor = 1;
    try ctx.insert(1, 'i');
    ctx.editor.cursor = 2;
    try testing.expectEqualStrings("hi", ctx.getBuffer());

    // Insert in middle
    try ctx.setBuffer("hllo");
    ctx.editor.cursor = 1;
    try ctx.insert(1, 'e');
    try testing.expectEqualStrings("hello", ctx.getBuffer());
}

test "Buffer: delete operations" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Delete backward
    try ctx.setBuffer("hello");
    _ = ctx.editor.buf.orderedRemove(ctx.editor.cursor - 1);
    ctx.editor.cursor -= 1;
    try testing.expectEqualStrings("hell", ctx.getBuffer());

    // Delete forward
    try ctx.setBuffer("hello");
    ctx.editor.cursor = 0;
    _ = ctx.editor.buf.orderedRemove(0);
    try testing.expectEqualStrings("ello", ctx.getBuffer());
}

// -----------------------------------------------------------------------------
// Word Boundaries
// -----------------------------------------------------------------------------

test "findWordBoundaryLeft: moves to previous word start" {
    const cases = [_]struct { []const u8, usize, usize }{
        .{ "hello world", 11, 6 }, // end of second word
        .{ "hello world", 6, 0 }, // start of second word
        .{ "hello", 5, 0 }, // single word
        .{ "hello", 0, 0 }, // already at start
    };
    for (cases) |c| try testing.expectEqual(c[2], findWordBoundaryLeft(c[0], c[1]));
}

test "findWordBoundaryRight: moves to next word start" {
    const cases = [_]struct { []const u8, usize, usize }{
        .{ "hello world", 0, 6 }, // from start
        .{ "hello world", 3, 6 }, // from middle of first word
        .{ "hello world", 6, 11 }, // from second word
        .{ "hello", 5, 5 }, // already at end
    };
    for (cases) |c| try testing.expectEqual(c[2], findWordBoundaryRight(c[0], c[1]));
}

// -----------------------------------------------------------------------------
// Kill Operations
// -----------------------------------------------------------------------------

test "Kill: to end of line" {
    var ctx = TestContext.init();
    defer ctx.deinit();
    try ctx.setBuffer("hello world");
    ctx.editor.cursor = 5;
    ctx.editor.buf.shrinkRetainingCapacity(ctx.editor.cursor);
    try testing.expectEqualStrings("hello", ctx.getBuffer());
}

test "Kill: to start of line" {
    var ctx = TestContext.init();
    defer ctx.deinit();
    try ctx.setBuffer("hello world");
    ctx.editor.cursor = 6;
    std.mem.copyForwards(u8, ctx.editor.buf.items[0..], ctx.editor.buf.items[ctx.editor.cursor..]);
    ctx.editor.buf.shrinkRetainingCapacity(ctx.editor.buf.items.len - ctx.editor.cursor);
    ctx.editor.cursor = 0;
    try testing.expectEqualStrings("world", ctx.getBuffer());
}

test "Kill: previous word" {
    var ctx = TestContext.init();
    defer ctx.deinit();
    try ctx.setBuffer("hello world");
    ctx.editor.cursor = 11;
    const end = ctx.editor.cursor;
    ctx.editor.cursor = findWordBoundaryLeft(ctx.editor.buf.items, ctx.editor.cursor);
    for (0..(end - ctx.editor.cursor)) |_| {
        _ = ctx.editor.buf.orderedRemove(ctx.editor.cursor);
    }
    try testing.expectEqualStrings("hello ", ctx.getBuffer());
    try testing.expectEqual(@as(usize, 6), ctx.editor.cursor);
}

