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

const History = @import("history.zig").History;
const highlight = @import("ui/highlight.zig");
const suggest = @import("ui/suggest.zig");
const complete = @import("ui/complete.zig");
const repl = @import("../repl.zig");
const State = @import("../../runtime/state.zig").State;
const ansi = @import("../../terminal/ansi.zig");
const tui = @import("../../terminal/tui.zig");
const io = @import("../../terminal/io.zig");

const posix = std.posix;

// =============================================================================
// Editor
// =============================================================================

/// Interactive line editor state.
pub const Editor = struct {
    allocator: std.mem.Allocator,
    buf: std.ArrayListUnmanaged(u8),
    cursor: usize,
    orig_termios: ?posix.termios,
    hist: History,
    hist_index: ?usize,
    saved_line: ?[]u8,
    tty_fd: posix.fd_t,
    prompt: []const u8,
    highlighting: bool,
    suggestions: bool,
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
            .hist = History.init(allocator),
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
        if (self.saved_line) |line| self.allocator.free(line);
    }

    pub fn loadHistory(self: *Editor, path: []const u8) void {
        self.hist.load(path);
    }

    pub fn saveHistory(self: *Editor, path: []const u8) void {
        self.hist.save(path);
    }

    pub fn enableRawMode(self: *Editor) !void {
        if (self.orig_termios != null) return;
        self.orig_termios = try tui.enableRawMode(self.tty_fd);
        tui.enableFocusReporting(self.tty_fd);
    }

    pub fn restoreTerminal(self: *Editor) void {
        if (self.tty_fd < 0) return;
        if (self.orig_termios) |orig| {
            tui.disableFocusReporting(self.tty_fd);
            tui.restoreTerminal(self.tty_fd, orig);
            self.orig_termios = null;
        }
    }

    /// Read a line from the user. Returns null on EOF (Ctrl+D on empty line).
    pub fn readLine(self: *Editor, prompt_text: []const u8) !?[]const u8 {
        self.prompt = prompt_text;
        self.buf.clearRetainingCapacity();
        self.cursor = 0;
        self.hist_index = null;
        if (self.saved_line) |line| {
            self.allocator.free(line);
            self.saved_line = null;
        }

        io.writeStdout(prompt_text);

        while (true) {
            switch (try tui.readKey(self.tty_fd)) {
                .enter => return try self.submitLine(),
                .eof => if (self.buf.items.len == 0) {
                    io.writeStdout("\r\n");
                    return null;
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
                .tab => try self.handleTab(),
                .interrupt => {
                    io.writeStdout("^C\r\n");
                    self.buf.clearRetainingCapacity();
                    self.cursor = 0;
                    io.writeStdout(self.prompt);
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
                .focus_in => {
                    self.has_focus = true;
                    self.display_initialized = true;
                    try self.refreshLine();
                },
                .focus_out => self.has_focus = false,
                else => {},
            }
        }
    }

    // =========================================================================
    // Line submission
    // =========================================================================

    /// Submit the current line with syntax highlighting preserved.
    fn submitLine(self: *Editor) ![]const u8 {
        io.writeStdout(ansi.clear_line);
        io.writeStdout(self.prompt);

        if (self.highlighting and self.buf.items.len > 0) {
            var buf: [4096]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            highlight.render(self.allocator, self.buf.items, stream.writer(), self.state) catch {
                io.writeStdout(self.buf.items);
            };
            io.writeStdout(stream.getWritten());
        } else {
            io.writeStdout(self.buf.items);
        }

        io.writeStdout("\r\n");
        return try self.allocator.dupe(u8, self.buf.items);
    }

    // =========================================================================
    // Text manipulation
    // =========================================================================

    fn insertChar(self: *Editor, c: u8) !void {
        try self.buf.insert(self.allocator, self.cursor, c);
        self.cursor += 1;
        try self.refreshLine();
    }

    fn deleteBackward(self: *Editor) !void {
        if (self.cursor > 0 and self.buf.items.len > 0) {
            _ = self.buf.orderedRemove(self.cursor - 1);
            self.cursor -= 1;
            try self.refreshLine();
        }
    }

    fn deleteForward(self: *Editor) !void {
        if (self.cursor < self.buf.items.len) {
            _ = self.buf.orderedRemove(self.cursor);
            try self.refreshLine();
        }
    }

    fn killLine(self: *Editor) !void {
        self.buf.shrinkRetainingCapacity(self.cursor);
        try self.refreshLine();
    }

    fn killToStart(self: *Editor) !void {
        if (self.cursor > 0) {
            std.mem.copyForwards(u8, self.buf.items[0..], self.buf.items[self.cursor..]);
            self.buf.shrinkRetainingCapacity(self.buf.items.len - self.cursor);
            self.cursor = 0;
            try self.refreshLine();
        }
    }

    fn killWord(self: *Editor) !void {
        if (self.cursor == 0) return;
        const end = self.cursor;
        self.cursor = findWordBoundaryLeft(self.buf.items, self.cursor);
        const remaining = self.buf.items.len - end;
        std.mem.copyForwards(u8, self.buf.items[self.cursor..], self.buf.items[end..][0..remaining]);
        self.buf.shrinkRetainingCapacity(self.cursor + remaining);
        try self.refreshLine();
    }

    fn replaceRange(self: *Editor, start: usize, end: usize, replacement: []const u8) !void {
        const old_len = end - start;
        const new_len = replacement.len;

        if (new_len > old_len) {
            // Grow: insert placeholder bytes, then overwrite
            const extra = new_len - old_len;
            try self.buf.insertSlice(self.allocator, end, replacement[old_len..][0..extra]);
        } else if (new_len < old_len) {
            // Shrink: remove excess bytes
            const tail = self.buf.items[end..];
            std.mem.copyForwards(u8, self.buf.items[start + new_len ..], tail);
            self.buf.shrinkRetainingCapacity(self.buf.items.len - (old_len - new_len));
        }

        @memcpy(self.buf.items[start..][0..new_len], replacement);
        self.cursor = start + new_len;
        try self.refreshLine();
    }

    // =========================================================================
    // Cursor movement
    // =========================================================================

    fn moveCursorLeft(self: *Editor) !void {
        if (self.cursor > 0) {
            self.cursor -= 1;
            io.writeStdout(ansi.cursor_left);
        }
    }

    fn moveCursorHome(self: *Editor) !void {
        self.cursor = 0;
        try self.refreshLine();
    }

    fn moveCursorWordLeft(self: *Editor) !void {
        self.cursor = findWordBoundaryLeft(self.buf.items, self.cursor);
        try self.refreshLine();
    }

    fn moveCursorWordRight(self: *Editor) !void {
        self.cursor = findWordBoundaryRight(self.buf.items, self.cursor);
        try self.refreshLine();
    }

    fn acceptSuggestionOrMoveRight(self: *Editor) !void {
        if (try self.tryAcceptSuggestion()) return;
        if (self.cursor < self.buf.items.len) {
            self.cursor += 1;
            io.writeStdout(ansi.cursor_right);
        }
    }

    fn acceptSuggestionOrMoveEnd(self: *Editor) !void {
        if (try self.tryAcceptSuggestion()) return;
        if (self.cursor < self.buf.items.len) {
            self.cursor = self.buf.items.len;
            try self.refreshLine();
        }
    }

    // =========================================================================
    // History navigation
    // =========================================================================

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

    fn setLineContent(self: *Editor, content: []const u8) !void {
        self.buf.clearRetainingCapacity();
        try self.buf.appendSlice(self.allocator, content);
        self.cursor = self.buf.items.len;
    }

    // =========================================================================
    // Tab completion
    // =========================================================================

    fn handleTab(self: *Editor) !void {
        var result = complete.complete(self.allocator, self.buf.items, self.cursor) catch return orelse return;
        defer result.deinit(self.allocator);

        if (result.completions.len == 0) return;

        const prefix = result.commonPrefix();
        const current_word = self.buf.items[result.word_start..result.word_end];

        if (prefix.len > current_word.len) {
            try self.replaceRange(result.word_start, result.word_end, prefix);
        } else if (result.completions.len == 1) {
            const with_space = try std.fmt.allocPrint(self.allocator, "{s} ", .{result.completions[0]});
            defer self.allocator.free(with_space);
            try self.replaceRange(result.word_start, result.word_end, with_space);
        } else {
            try self.showCompletions(result.completions);
        }
    }

    fn showCompletions(self: *Editor, completions: []const []const u8) !void {
        io.writeStdout("\r\n");
        for (completions) |c| {
            io.writeStdout(c);
            io.writeStdout("  ");
        }
        io.writeStdout("\r\n");
        io.writeStdout(self.prompt);
        try self.refreshLine();
    }

    // =========================================================================
    // Suggestions
    // =========================================================================

    fn tryAcceptSuggestion(self: *Editor) !bool {
        if (!self.suggestions or self.cursor != self.buf.items.len) return false;
        if (suggest.fromHistory(self.buf.items, &self.hist, self.getCwd())) |suffix| {
            try self.buf.appendSlice(self.allocator, suffix);
            self.cursor = self.buf.items.len;
            try self.refreshLine();
            return true;
        }
        return false;
    }

    fn getCwd(self: *const Editor) []const u8 {
        return if (self.state) |s| s.getCwd() catch "" else "";
    }

    // =========================================================================
    // Display
    // =========================================================================

    fn clearScreen(self: *Editor) !void {
        io.writeStdout(ansi.cursor_home ++ ansi.clear_screen);
        io.writeStdout(self.prompt);
        try self.refreshLine();
    }

    fn refreshLine(self: *Editor) !void {
        if (!self.has_focus and !self.display_initialized) return;

        var out_buf: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&out_buf);
        const writer = stream.writer();

        try writer.writeAll(ansi.clear_line);
        try writer.writeAll(self.prompt);

        // Write input with optional highlighting (skip when unfocused for perf)
        if (self.highlighting and self.has_focus and self.buf.items.len > 0) {
            highlight.render(self.allocator, self.buf.items, writer, self.state) catch {
                try writer.writeAll(self.buf.items);
            };
        } else {
            try writer.writeAll(self.buf.items);
        }

        // Write suggestion suffix if cursor is at end and focused
        if (self.suggestions and self.has_focus and self.cursor == self.buf.items.len) {
            try self.writeSuggestion(writer);
        }

        // Position cursor
        if (self.cursor < self.buf.items.len) {
            try writer.print("\x1b[{d}D", .{self.buf.items.len - self.cursor});
        }

        io.writeStdout(stream.getWritten());
    }

    fn writeSuggestion(self: *Editor, writer: anytype) !void {
        const suffix = suggest.fromHistory(self.buf.items, &self.hist, self.getCwd()) orelse return;
        const display_suffix = truncateSuggestion(suffix, self.prompt, self.buf.items.len);
        if (display_suffix.len == 0) return;

        const truncated = display_suffix.len < suffix.len;
        try writer.writeAll(ansi.dim);
        try writer.writeAll(display_suffix);
        if (truncated) try writer.writeAll("...");
        try writer.writeAll(ansi.reset);

        const move_back = display_suffix.len + if (truncated) @as(usize, 3) else 0;
        try writer.print("\x1b[{d}D", .{move_back});
    }
};

// =============================================================================
// Helpers
// =============================================================================

fn findWordBoundaryLeft(buf: []const u8, cursor: usize) usize {
    var pos = cursor;
    while (pos > 0 and repl.isWordBreak(buf[pos - 1])) pos -= 1;
    while (pos > 0 and !repl.isWordBreak(buf[pos - 1])) pos -= 1;
    return pos;
}

fn findWordBoundaryRight(buf: []const u8, cursor: usize) usize {
    var pos = cursor;
    while (pos < buf.len and !repl.isWordBreak(buf[pos])) pos += 1;
    while (pos < buf.len and repl.isWordBreak(buf[pos])) pos += 1;
    return pos;
}

fn truncateSuggestion(suffix: []const u8, prompt: []const u8, input_len: usize) []const u8 {
    const term_width = tui.getTerminalWidth() orelse return suffix;
    const used = ansi.displayLength(prompt) + input_len;
    if (used >= term_width) return suffix[0..0];

    const available = term_width - used - 1; // -1 to prevent cursor at edge
    if (suffix.len <= available) return suffix;
    if (available > 3) return suffix[0 .. available - 3]; // Leave room for "..."
    return suffix[0..0];
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

const TestContext = struct {
    editor: Editor,

    fn init() TestContext {
        return .{
            .editor = .{
                .allocator = testing.allocator,
                .buf = .empty,
                .cursor = 0,
                .orig_termios = null,
                .hist = History.init(testing.allocator),
                .hist_index = null,
                .saved_line = null,
                .tty_fd = -1,
                .prompt = "$ ",
                .highlighting = false,
                .suggestions = false,
                .state = null,
                .has_focus = true,
                .display_initialized = true,
            },
        };
    }

    fn deinit(self: *TestContext) void {
        self.editor.deinit();
    }

    /// Set buffer content and cursor position.
    fn set(self: *TestContext, text: []const u8, pos: usize) !void {
        self.editor.buf.clearRetainingCapacity();
        try self.editor.buf.appendSlice(testing.allocator, text);
        self.editor.cursor = pos;
    }

    /// Get buffer content.
    fn content(self: *const TestContext) []const u8 {
        return self.editor.buf.items;
    }

    /// Get cursor position.
    fn cursor(self: *const TestContext) usize {
        return self.editor.cursor;
    }

    /// Insert a character at the given position.
    fn insert(self: *TestContext, pos: usize, char: u8) !void {
        try self.editor.buf.insert(testing.allocator, pos, char);
    }

    /// Delete character at the given position.
    fn delete(self: *TestContext, pos: usize) void {
        _ = self.editor.buf.orderedRemove(pos);
    }

    /// Kill from cursor to end of line.
    fn killToEnd(self: *TestContext) void {
        self.editor.buf.shrinkRetainingCapacity(self.editor.cursor);
    }

    /// Kill from start to cursor.
    fn killToStart(self: *TestContext) void {
        std.mem.copyForwards(u8, self.editor.buf.items[0..], self.editor.buf.items[self.editor.cursor..]);
        self.editor.buf.shrinkRetainingCapacity(self.editor.buf.items.len - self.editor.cursor);
        self.editor.cursor = 0;
    }

    /// Kill word before cursor.
    fn killWord(self: *TestContext) void {
        const end = self.editor.cursor;
        self.editor.cursor = findWordBoundaryLeft(self.editor.buf.items, self.editor.cursor);
        const remaining = self.editor.buf.items.len - end;
        std.mem.copyForwards(u8, self.editor.buf.items[self.editor.cursor..], self.editor.buf.items[end..][0..remaining]);
        self.editor.buf.shrinkRetainingCapacity(self.editor.cursor + remaining);
    }
};

// -----------------------------------------------------------------------------
// Word boundaries
// -----------------------------------------------------------------------------

test "findWordBoundaryLeft" {
    const cases = .{
        .{ "hello world", 11, 6 }, // end of second word
        .{ "hello world", 6, 0 }, // start of second word
        .{ "hello", 5, 0 }, // single word
        .{ "hello", 0, 0 }, // already at start
    };
    inline for (cases) |c| try testing.expectEqual(c[2], findWordBoundaryLeft(c[0], c[1]));
}

test "findWordBoundaryRight" {
    const cases = .{
        .{ "hello world", 0, 6 }, // from start
        .{ "hello world", 3, 6 }, // from middle of first word
        .{ "hello world", 6, 11 }, // from second word
        .{ "hello", 5, 5 }, // already at end
    };
    inline for (cases) |c| try testing.expectEqual(c[2], findWordBoundaryRight(c[0], c[1]));
}

// -----------------------------------------------------------------------------
// Buffer operations
// -----------------------------------------------------------------------------

test "buffer insert" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.insert(0, 'h');
    try ctx.insert(1, 'i');
    try testing.expectEqualStrings("hi", ctx.content());
}

test "buffer delete" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.set("hello", 5);
    ctx.delete(4);
    try testing.expectEqualStrings("hell", ctx.content());

    ctx.delete(0);
    try testing.expectEqualStrings("ell", ctx.content());
}

// -----------------------------------------------------------------------------
// Kill operations
// -----------------------------------------------------------------------------

test "kill to end" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.set("hello world", 5);
    ctx.killToEnd();
    try testing.expectEqualStrings("hello", ctx.content());
}

test "kill to start" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.set("hello world", 6);
    ctx.killToStart();
    try testing.expectEqualStrings("world", ctx.content());
}

test "kill word" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.set("hello world", 11);
    ctx.killWord();
    try testing.expectEqualStrings("hello ", ctx.content());
    try testing.expectEqual(@as(usize, 6), ctx.cursor());
}

// -----------------------------------------------------------------------------
// Suggestion truncation
// -----------------------------------------------------------------------------

test "truncateSuggestion without terminal" {
    // When no terminal width available, returns full suffix
    const result = truncateSuggestion("suggestion", "$ ", 5);
    try testing.expectEqualStrings("suggestion", result);
}
