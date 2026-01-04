//! Lexer: transforms shell input into a stream of tokens.
//!
//! Tokenization proceeds left-to-right, at each position trying (in order):
//! 1. Skip whitespace (spaces, tabs)
//! 2. Skip comments (`#` to end of line)
//! 3. Read separator tokens (newline, semicolon)
//! 4. Read operator tokens (longest match first: `2>&1`, `=>@`, `&&`, etc.)
//! 5. Read word tokens (bare words, quoted strings, command substitutions)
//!
//! Operators are checked before words so that `|>foo` tokenizes as `|>` + `foo`,
//! not as a single word. Operators are checked longest-first in `peekOperator`
//! to ensure correct matching (e.g., `2>&1` before `2>`).
//!
//! Each token includes a `TokenSpan` with source location for error reporting.

const std = @import("std");
const token_types = @import("tokens.zig");

const Token = token_types.Token;
const WordPart = token_types.WordPart;
const QuoteKind = token_types.QuoteKind;
const TokenSpan = token_types.TokenSpan;
const Operator = token_types.Operator;
const Separator = token_types.Separator;

pub const LexError = error{
    UnterminatedString,
    UnterminatedCommandSubstitution,
    UnterminatedBrace,
};

pub const Lexer = struct {
    input: []const u8,
    pos: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, input: []const u8) Lexer {
        return .{
            .input = input,
            .pos = 0,
            .allocator = allocator,
        };
    }

    // =========================================================================
    // Input navigation helpers
    // =========================================================================

    /// Checks if the position plus offset is within bounds of the input.
    inline fn isInBounds(self: *const Lexer, offset: usize) bool {
        return self.pos + offset < self.input.len;
    }

    /// Returns the character at the current position, or null if at end of input.
    inline fn peek(self: *const Lexer) ?u8 {
        return self.peekAt(0);
    }

    /// Returns the character at `offset` positions ahead, or null if out of bounds.
    inline fn peekAt(self: *const Lexer, offset: usize) ?u8 {
        return if (self.isInBounds(offset)) self.input[self.pos + offset] else null;
    }

    /// Returns a slice of `len` characters starting at current position, or null if not enough input.
    inline fn peekSlice(self: *const Lexer, len: usize) ?[]const u8 {
        return if (len == 0 or !self.isInBounds(len - 1))
            null
        else
            self.input[self.pos .. self.pos + len];
    }

    /// Advances position by one character.
    inline fn advance(self: *Lexer) void {
        self.advanceBy(1);
    }

    /// Advances position by `n` characters.
    inline fn advanceBy(self: *Lexer, n: usize) void {
        self.pos = @min(self.pos + n, self.input.len);
    }

    /// Creates a TokenSpan from `start` to current position.
    inline fn makeSpan(self: *const Lexer, start: usize) TokenSpan {
        return .{ .start = start, .end = self.pos };
    }

    // =========================================================================
    // Whitespace and comment handling
    // =========================================================================

    /// Skips spaces and tabs (not newlines - those are separators).
    fn skipWhitespace(self: *Lexer) void {
        while (self.peek()) |c| {
            if (!token_types.isWhitespace(c)) break;
            self.advance();
        }
    }

    /// Skips a comment if present (from `#` to end of line). Returns true if skipped.
    fn trySkipComment(self: *Lexer) bool {
        if (self.peek() != '#') return false;
        while (self.peek()) |c| {
            if (c == '\n') break;
            self.advance();
        }
        return true;
    }

    // =========================================================================
    // Operator handling
    // =========================================================================

    /// Checks if the input at current position matches the given string.
    inline fn matchesStr(self: *const Lexer, s: []const u8) bool {
        const slice = self.peekSlice(s.len) orelse return false;
        return std.mem.eql(u8, slice, s);
    }

    /// Checks if an operator starts at the current position. Returns the operator or null.
    /// Dispatches on first character, then checks longer matches first within each group.
    fn peekOperator(self: *const Lexer) ?Operator {
        const c = self.peek() orelse return null;
        return switch (c) {
            '|' => if (self.matchesStr("|>")) .pipe_arrow else if (self.matchesStr("||")) .@"or" else .pipe,
            '&' => if (self.matchesStr("&>")) .redirect_both else if (self.matchesStr("&&")) .@"and" else .background,
            '>' => if (self.matchesStr(">>")) .redirect_stdout_append else .redirect_stdout,
            '<' => .redirect_stdin,
            '=' => if (self.matchesStr("=>@")) .capture_lines else if (self.matchesStr("=>")) .capture else null,
            '2' => blk: {
                if (self.matchesStr("2>&1")) break :blk .redirect_stderr_to_stdout;
                if (self.matchesStr("2>>")) break :blk .redirect_stderr_append;
                if (self.matchesStr("2>")) break :blk .redirect_stderr;
                break :blk null;
            },
            else => null,
        };
    }

    /// Reads an operator token if present. Returns true if an operator was read.
    fn tryReadOperator(self: *Lexer, tokens: *std.ArrayListUnmanaged(Token)) error{OutOfMemory}!bool {
        const op = self.peekOperator() orelse return false;
        const start = self.pos;
        self.advanceBy(op.len());
        try tokens.append(self.allocator, Token.initOp(op, self.makeSpan(start)));
        return true;
    }

    /// Reads a separator token (newline or semicolon) if present. Returns true if read.
    fn tryReadSeparator(self: *Lexer, tokens: *std.ArrayListUnmanaged(Token)) error{OutOfMemory}!bool {
        const c = self.peek() orelse return false;
        if (!token_types.isSeparator(c)) return false;
        const sep: Separator = if (c == '\n') .newline else .semicolon;
        const start = self.pos;
        self.advance();
        try tokens.append(self.allocator, Token.initSep(sep, self.makeSpan(start)));
        return true;
    }

    // =========================================================================
    // Escape sequence handling
    // =========================================================================

    /// Handles escape sequences in double-quoted strings.
    /// - `\<newline>` is line continuation (removed from output)
    /// - `\n`, `\t` are converted to literal newline/tab
    /// - `\"`, `\\` are converted to the literal character
    /// - `\$` is preserved for the expander
    /// - Other escapes preserve both backslash and character
    fn handleDoubleQuotedEscape(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) error{OutOfMemory}!void {
        const next = self.peek() orelse {
            try buf.append(self.allocator, '\\');
            return;
        };
        self.advance();

        switch (next) {
            '\n' => self.skipWhitespace(), // Line continuation
            '"', '\\' => try buf.append(self.allocator, next),
            'n' => try buf.append(self.allocator, '\n'),
            't' => try buf.append(self.allocator, '\t'),
            '$' => try buf.appendSlice(self.allocator, "\\$"),
            else => try buf.appendSlice(self.allocator, &.{ '\\', next }),
        }
    }

    /// Handles escape sequences in bare words.
    /// - `\<newline>` is line continuation (removed, skips following indentation)
    /// - `\$` is preserved for the expander
    /// - Other escapes include just the escaped character
    fn handleBareWordEscape(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) error{OutOfMemory}!void {
        const next = self.peek() orelse {
            try buf.append(self.allocator, '\\');
            return;
        };
        self.advance();

        switch (next) {
            '\n' => self.skipWhitespace(), // Line continuation
            '$' => try buf.appendSlice(self.allocator, "\\$"),
            else => try buf.append(self.allocator, next),
        }
    }

    // =========================================================================
    // Command substitution handling
    // =========================================================================

    /// Flushes pending buffer content as a word part if non-empty.
    fn flushPendingContent(
        self: *Lexer,
        parts: *std.ArrayListUnmanaged(WordPart),
        buf: *std.ArrayListUnmanaged(u8),
        quote: QuoteKind,
    ) error{OutOfMemory}!void {
        if (buf.items.len > 0) {
            const content = try self.allocator.dupe(u8, buf.items);
            try parts.append(self.allocator, .{ .quotes = quote, .text = content });
            buf.clearRetainingCapacity();
        }
    }

    /// Reads content between matched delimiters (parens or braces) with nesting support.
    /// Returns the content string (without the surrounding delimiters).
    /// Assumes we're positioned after the opening delimiter.
    fn readDelimitedContent(
        self: *Lexer,
        comptime open: u8,
        comptime close: u8,
        comptime unterminated_error: LexError,
    ) (error{OutOfMemory} || LexError)![]const u8 {
        const start = self.pos;
        var depth: usize = 1;

        while (self.peek()) |ch| {
            if (ch == open) {
                depth += 1;
            } else if (ch == close) {
                depth -= 1;
                if (depth == 0) {
                    const content = try self.allocator.dupe(u8, self.input[start..self.pos]);
                    self.advance(); // skip closing delimiter
                    return content;
                }
            }
            self.advance();
        }
        return unterminated_error;
    }

    /// Reads command substitution `$(...)` or `(...)` and adds as a `.command` word part.
    fn readCommandPart(
        self: *Lexer,
        parts: *std.ArrayListUnmanaged(WordPart),
        buf: *std.ArrayListUnmanaged(u8),
        pending_quote: QuoteKind,
    ) (error{OutOfMemory} || LexError)!void {
        try self.flushPendingContent(parts, buf, pending_quote);
        if (self.peek() == '$') self.advanceBy(2) else self.advance();
        const content = try self.readDelimitedContent('(', ')', LexError.UnterminatedCommandSubstitution);
        try parts.append(self.allocator, .{ .quotes = .command, .text = content });
    }

    /// Reads brace expansion `{...}` and adds as a `.brace` word part.
    fn readBracePart(
        self: *Lexer,
        parts: *std.ArrayListUnmanaged(WordPart),
        buf: *std.ArrayListUnmanaged(u8),
        pending_quote: QuoteKind,
    ) (error{OutOfMemory} || LexError)!void {
        try self.flushPendingContent(parts, buf, pending_quote);
        self.advance(); // skip opening '{'
        const content = try self.readDelimitedContent('{', '}', LexError.UnterminatedBrace);
        try parts.append(self.allocator, .{ .quotes = .brace, .text = content });
    }

    // =========================================================================
    // Word reading (quoted and bare)
    // =========================================================================

    /// Reads a single-quoted string (no escape processing, literal content).
    fn readSingleQuoted(self: *Lexer, parts: *std.ArrayListUnmanaged(WordPart)) (error{OutOfMemory} || LexError)!void {
        self.advance(); // skip opening quote
        const start = self.pos;

        while (self.peek()) |c| {
            if (c == '\'') {
                const content = self.input[start..self.pos];
                self.advance(); // skip closing quote
                try parts.append(self.allocator, .{ .quotes = .single, .text = content });
                return;
            }
            self.advance();
        }
        return LexError.UnterminatedString;
    }

    /// Reads a double-quoted string (with escape processing).
    /// Command substitutions are split into separate `.command` parts.
    fn readDoubleQuoted(self: *Lexer, parts: *std.ArrayListUnmanaged(WordPart), buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!void {
        self.advance(); // skip opening quote
        buf.clearRetainingCapacity();
        const start_parts = parts.items.len;

        while (self.peek()) |c| {
            switch (c) {
                '"' => {
                    self.advance(); // skip closing quote
                    // Flush any remaining content, or produce empty part if this is a pure ""
                    if (buf.items.len > 0 or parts.items.len == start_parts) {
                        const content = try self.allocator.dupe(u8, buf.items);
                        try parts.append(self.allocator, .{ .quotes = .double, .text = content });
                    }
                    return;
                },
                '\\' => {
                    self.advance();
                    try self.handleDoubleQuotedEscape(buf);
                },
                '$' => {
                    if (self.peekAt(1) == '(') {
                        try self.readCommandPart(parts, buf, .double);
                    } else {
                        try buf.append(self.allocator, c);
                        self.advance();
                    }
                },
                else => {
                    try buf.append(self.allocator, c);
                    self.advance();
                },
            }
        }
        return LexError.UnterminatedString;
    }

    /// Reads an unquoted (bare) word part.
    /// Returns true if any content was read.
    /// Command substitutions and brace expansions are split into separate parts.
    fn readBareWord(self: *Lexer, parts: *std.ArrayListUnmanaged(WordPart), buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!bool {
        buf.clearRetainingCapacity();
        const start_parts = parts.items.len;

        while (self.peek()) |c| {
            // Stop at word breaks and operators
            if (token_types.isWordBreak(c)) break;
            if (self.peekOperator() != null) break;

            switch (c) {
                '\\' => {
                    self.advance();
                    try self.handleBareWordEscape(buf);
                },
                '"', '\'' => break, // Quote starts new part
                '$' => {
                    if (self.peekAt(1) == '(') {
                        try self.readCommandPart(parts, buf, .none);
                    } else {
                        try buf.append(self.allocator, c);
                        self.advance();
                    }
                },
                '(' => {
                    try self.readCommandPart(parts, buf, .none);
                },
                '{' => {
                    // Check if this is ${var} syntax (not brace expansion)
                    if (buf.items.len > 0 and buf.items[buf.items.len - 1] == '$') {
                        try buf.append(self.allocator, c);
                        self.advance();
                    } else {
                        try self.readBracePart(parts, buf, .none);
                    }
                },
                '}' => {
                    // Closing brace outside ${var} - include in buffer for ${var} syntax
                    try buf.append(self.allocator, c);
                    self.advance();
                },
                else => {
                    try buf.append(self.allocator, c);
                    self.advance();
                },
            }
        }

        // Flush any remaining content as a .none part
        if (buf.items.len > 0) {
            const content = try self.allocator.dupe(u8, buf.items);
            try parts.append(self.allocator, .{ .quotes = .none, .text = content });
        }

        // Return true if we added any parts
        return parts.items.len > start_parts;
    }

    /// Reads a complete word token (may contain multiple quoted/unquoted parts).
    fn readWord(self: *Lexer, tokens: *std.ArrayListUnmanaged(Token)) (error{OutOfMemory} || LexError)!void {
        var parts: std.ArrayListUnmanaged(WordPart) = .empty;
        var buffer: std.ArrayListUnmanaged(u8) = .empty;
        defer buffer.deinit(self.allocator);

        const start = self.pos;

        while (self.peek()) |c| {
            switch (c) {
                '"' => try self.readDoubleQuoted(&parts, &buffer),
                '\'' => try self.readSingleQuoted(&parts),
                else => {
                    if (token_types.isWordBreak(c)) break;
                    if (self.peekOperator() != null) break;
                    if (!try self.readBareWord(&parts, &buffer)) break;
                },
            }
        }

        if (parts.items.len > 0) {
            const parts_slice = try parts.toOwnedSlice(self.allocator);
            try tokens.append(self.allocator, Token.initWord(parts_slice, self.makeSpan(start)));
        }
    }

    // =========================================================================
    // Main tokenization entry point
    // =========================================================================

    pub fn tokenize(self: *Lexer) (error{OutOfMemory} || LexError)![]Token {
        var tokens: std.ArrayListUnmanaged(Token) = .empty;

        while (self.pos < self.input.len) {
            self.skipWhitespace();
            if (self.peek() == null) break;
            if (self.trySkipComment()) continue;

            if (try self.tryReadSeparator(&tokens)) continue;
            if (try self.tryReadOperator(&tokens)) continue;

            try self.readWord(&tokens);
        }

        return try tokens.toOwnedSlice(self.allocator);
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

const TestContext = struct {
    arena: std.heap.ArenaAllocator,

    fn init() TestContext {
        return .{ .arena = std.heap.ArenaAllocator.init(testing.allocator) };
    }

    fn deinit(self: *TestContext) void {
        self.arena.deinit();
    }

    fn tokenize(self: *TestContext, input: []const u8) ![]Token {
        var lex = Lexer.init(self.arena.allocator(), input);
        return try lex.tokenize();
    }

    fn expectError(self: *TestContext, input: []const u8, expected: LexError) !void {
        var lex = Lexer.init(self.arena.allocator(), input);
        try testing.expectError(expected, lex.tokenize());
    }

    /// Asserts that a word token has a single part with the expected quote kind and text.
    fn expectWord(parts: []const WordPart, quote: QuoteKind, expected: []const u8) !void {
        if (parts.len == 1 and parts[0].quotes == quote) {
            try testing.expectEqualStrings(expected, parts[0].text);
            return;
        }
        return error.TestExpectedEqual;
    }

    /// Asserts that a word token has a single bare part with the expected text.
    fn expectBareWord(parts: []const WordPart, expected: []const u8) !void {
        return expectWord(parts, .none, expected);
    }

    /// Asserts that a word token has a single command substitution part with the expected text.
    fn expectCommandWord(parts: []const WordPart, expected: []const u8) !void {
        return expectWord(parts, .command, expected);
    }
};

// -----------------------------------------------------------------------------
// Words and Quoting
// -----------------------------------------------------------------------------

test "Words: simple bare words" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const tokens = try ctx.tokenize("echo hello world");

    try testing.expectEqual(@as(usize, 3), tokens.len);
    try TestContext.expectBareWord(tokens[0].kind.word, "echo");
    try TestContext.expectBareWord(tokens[1].kind.word, "hello");
    try TestContext.expectBareWord(tokens[2].kind.word, "world");
}

test "Words: quoted strings preserve spaces" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Test both quote styles in one test
    const single = try ctx.tokenize("echo 'hello world'");
    try testing.expectEqual(@as(usize, 2), single.len);
    try testing.expectEqual(QuoteKind.single, single[1].kind.word[0].quotes);
    try testing.expectEqualStrings("hello world", single[1].kind.word[0].text);

    const double = try ctx.tokenize("echo \"hello world\"");
    try testing.expectEqual(@as(usize, 2), double.len);
    try testing.expectEqual(QuoteKind.double, double[1].kind.word[0].quotes);
    try testing.expectEqualStrings("hello world", double[1].kind.word[0].text);

    // Empty quoted strings
    const emptySingle = try ctx.tokenize("echo ''");
    try testing.expectEqual(@as(usize, 2), emptySingle.len);
    try testing.expectEqual(QuoteKind.single, emptySingle[1].kind.word[0].quotes);
    try testing.expectEqualStrings("", emptySingle[1].kind.word[0].text);

    const emptyDouble = try ctx.tokenize("echo \"\"");
    try testing.expectEqual(@as(usize, 2), emptyDouble.len);
    try testing.expectEqual(QuoteKind.double, emptyDouble[1].kind.word[0].quotes);
    try testing.expectEqualStrings("", emptyDouble[1].kind.word[0].text);
}

test "Words: mixed quote parts combine into single token" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const tokens = try ctx.tokenize("echo hello\"world\"'!'");

    try testing.expectEqual(@as(usize, 2), tokens.len);
    const parts = tokens[1].kind.word;
    try testing.expectEqual(@as(usize, 3), parts.len);

    // Each part preserves its quote context
    try testing.expectEqual(QuoteKind.none, parts[0].quotes);
    try testing.expectEqualStrings("hello", parts[0].text);
    try testing.expectEqual(QuoteKind.double, parts[1].quotes);
    try testing.expectEqualStrings("world", parts[1].text);
    try testing.expectEqual(QuoteKind.single, parts[2].quotes);
    try testing.expectEqualStrings("!", parts[2].text);
}

// -----------------------------------------------------------------------------
// Escape Sequences
// -----------------------------------------------------------------------------

test "Escapes: double quotes process escape sequences" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Standard escapes: \n, \t
    const tokens = try ctx.tokenize("echo \"hello\\nworld\\t!\"");
    try testing.expectEqualStrings("hello\nworld\t!", tokens[1].kind.word[0].text);

    // Escaped quotes and backslashes
    const quoted = try ctx.tokenize("echo \"say \\\"hi\\\"\"");
    try testing.expectEqualStrings("say \"hi\"", quoted[1].kind.word[0].text);

    const backslash = try ctx.tokenize("echo \"a\\\\b\"");
    try testing.expectEqualStrings("a\\b", backslash[1].kind.word[0].text);

    // Escaped $ preserved for expander
    const dollar = try ctx.tokenize("echo \"\\$literal\"");
    try testing.expectEqualStrings("\\$literal", dollar[1].kind.word[0].text);

    // Unknown escapes preserved as-is
    const unknown = try ctx.tokenize("echo \"\\x\"");
    try testing.expectEqualStrings("\\x", unknown[1].kind.word[0].text);
}

test "Escapes: bare words preserve backslash for expander" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // \$ preserved for expander
    const tokens = try ctx.tokenize("echo \\$HOME");
    try testing.expectEqualStrings("\\$HOME", tokens[1].kind.word[0].text);

    // Other escapes: just the escaped char (backslash consumed)
    const space = try ctx.tokenize("echo hello\\ world");
    try testing.expectEqual(@as(usize, 2), space.len);
    try testing.expectEqualStrings("hello world", space[1].kind.word[0].text);
}

test "Escapes: line continuation in bare words" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Basic line continuation - backslash-newline is removed
    const basic = try ctx.tokenize("echo hello\\\nworld");
    try testing.expectEqual(@as(usize, 2), basic.len);
    try testing.expectEqualStrings("helloworld", basic[1].kind.word[0].text);

    // Line continuation with indentation - leading whitespace on next line is skipped
    const indented = try ctx.tokenize("cmd \\\n    arg");
    try testing.expectEqual(@as(usize, 2), indented.len);
    try testing.expectEqualStrings("arg", indented[1].kind.word[0].text);

    // Line continuation before quoted string
    const before_quote = try ctx.tokenize("cmd \\\n    \"arg\"");
    try testing.expectEqual(@as(usize, 2), before_quote.len);
    try testing.expectEqual(QuoteKind.double, before_quote[1].kind.word[0].quotes);
    try testing.expectEqualStrings("arg", before_quote[1].kind.word[0].text);

    // Multiple arguments across lines
    const multi = try ctx.tokenize("cmd \\\n    arg1 \\\n    arg2");
    try testing.expectEqual(@as(usize, 3), multi.len);
    try testing.expectEqualStrings("arg1", multi[1].kind.word[0].text);
    try testing.expectEqualStrings("arg2", multi[2].kind.word[0].text);
}

test "Escapes: line continuation in double-quoted strings" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Line continuation inside double quotes
    const inside = try ctx.tokenize("echo \"hello\\\nworld\"");
    try testing.expectEqual(@as(usize, 2), inside.len);
    try testing.expectEqualStrings("helloworld", inside[1].kind.word[0].text);

    // Line continuation with indentation inside double quotes
    const indented = try ctx.tokenize("echo \"line1\\\n    line2\"");
    try testing.expectEqual(@as(usize, 2), indented.len);
    try testing.expectEqualStrings("line1line2", indented[1].kind.word[0].text);
}

// -----------------------------------------------------------------------------
// Command Substitution
// -----------------------------------------------------------------------------

test "Command substitution: $() and bare () forms produce .command parts" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // $() syntax - produces .command part with just the command (no wrapper)
    const dollar = try ctx.tokenize("echo $(whoami)");
    try testing.expectEqual(@as(usize, 2), dollar.len);
    try TestContext.expectCommandWord(dollar[1].kind.word, "whoami");

    // Bare () also produces .command part
    const bare = try ctx.tokenize("echo (whoami)");
    try testing.expectEqual(@as(usize, 2), bare.len);
    try TestContext.expectCommandWord(bare[1].kind.word, "whoami");
}

test "Command substitution: nested substitutions" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Nested substitution - outer is .command, inner stays as text for re-lexing
    const nested = try ctx.tokenize("echo $(dirname $(pwd))");
    try TestContext.expectCommandWord(nested[1].kind.word, "dirname $(pwd)");

    // Bare parens nested
    const bareNested = try ctx.tokenize("echo (dirname (pwd))");
    try TestContext.expectCommandWord(bareNested[1].kind.word, "dirname (pwd)");

    // Pipe inside parens stays inside
    const withPipe = try ctx.tokenize("echo (ls | head)");
    try TestContext.expectCommandWord(withPipe[1].kind.word, "ls | head");
}

test "Command substitution: concatenation splits into multiple parts" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // file_(date).txt becomes 3 parts: "file_" + cmdsub("date") + ".txt"
    const tokens = try ctx.tokenize("file_(date).txt");
    try testing.expectEqual(@as(usize, 1), tokens.len);
    const parts = tokens[0].kind.word;
    try testing.expectEqual(@as(usize, 3), parts.len);
    try testing.expectEqual(QuoteKind.none, parts[0].quotes);
    try testing.expectEqualStrings("file_", parts[0].text);
    try testing.expectEqual(QuoteKind.command, parts[1].quotes);
    try testing.expectEqualStrings("date", parts[1].text);
    try testing.expectEqual(QuoteKind.none, parts[2].quotes);
    try testing.expectEqualStrings(".txt", parts[2].text);
}

test "Command substitution: inside double quotes" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // "hello $(cmd) world" splits into: .double "hello ", .command "cmd", .double " world"
    const tokens = try ctx.tokenize("echo \"hello $(whoami) there\"");
    try testing.expectEqual(@as(usize, 2), tokens.len);
    const parts = tokens[1].kind.word;
    try testing.expectEqual(@as(usize, 3), parts.len);
    try testing.expectEqual(QuoteKind.double, parts[0].quotes);
    try testing.expectEqualStrings("hello ", parts[0].text);
    try testing.expectEqual(QuoteKind.command, parts[1].quotes);
    try testing.expectEqualStrings("whoami", parts[1].text);
    try testing.expectEqual(QuoteKind.double, parts[2].quotes);
    try testing.expectEqualStrings(" there", parts[2].text);

    // Bare () inside double quotes is NOT command substitution (literal)
    const literal = try ctx.tokenize("echo \"hello (world)\"");
    try testing.expectEqual(@as(usize, 2), literal.len);
    try TestContext.expectWord(literal[1].kind.word, .double, "hello (world)");
}

// -----------------------------------------------------------------------------
// Brace Expansion
// -----------------------------------------------------------------------------

test "Brace expansion: simple braces produce .brace parts" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Simple comma list
    const comma = try ctx.tokenize("echo {a,b,c}");
    try testing.expectEqual(@as(usize, 2), comma.len);
    try TestContext.expectWord(comma[1].kind.word, .brace, "a,b,c");

    // Range
    const range = try ctx.tokenize("echo {1..5}");
    try testing.expectEqual(@as(usize, 2), range.len);
    try TestContext.expectWord(range[1].kind.word, .brace, "1..5");
}

test "Brace expansion: concatenation splits into multiple parts" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // prefix_{a,b}_suffix becomes 3 parts
    const tokens = try ctx.tokenize("prefix_{a,b}_suffix");
    try testing.expectEqual(@as(usize, 1), tokens.len);
    const parts = tokens[0].kind.word;
    try testing.expectEqual(@as(usize, 3), parts.len);
    try testing.expectEqual(QuoteKind.none, parts[0].quotes);
    try testing.expectEqualStrings("prefix_", parts[0].text);
    try testing.expectEqual(QuoteKind.brace, parts[1].quotes);
    try testing.expectEqualStrings("a,b", parts[1].text);
    try testing.expectEqual(QuoteKind.none, parts[2].quotes);
    try testing.expectEqualStrings("_suffix", parts[2].text);
}

test "Brace expansion: nested braces" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Nested braces stay as single brace part
    const nested = try ctx.tokenize("echo {a,{b,c}}");
    try TestContext.expectWord(nested[1].kind.word, .brace, "a,{b,c}");
}

test "Brace expansion: ${var} syntax is NOT brace expansion" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // ${var} should be a single .none part, not .brace
    const varSyntax = try ctx.tokenize("echo ${foo}");
    try testing.expectEqual(@as(usize, 2), varSyntax.len);
    try TestContext.expectBareWord(varSyntax[1].kind.word, "${foo}");

    // ${var}suffix stays as single .none part
    const withSuffix = try ctx.tokenize("echo ${foo}bar");
    try TestContext.expectBareWord(withSuffix[1].kind.word, "${foo}bar");
}

test "Brace expansion: in double quotes is literal" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Braces inside double quotes are literal (no brace expansion)
    const quoted = try ctx.tokenize("echo \"{a,b,c}\"");
    try testing.expectEqual(@as(usize, 2), quoted.len);
    try TestContext.expectWord(quoted[1].kind.word, .double, "{a,b,c}");
}

test "Brace expansion: escaped braces are literal" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Escaped opening brace - backslash consumed, brace is literal
    const escaped = try ctx.tokenize("echo \\{a,b\\}");
    try testing.expectEqual(@as(usize, 2), escaped.len);
    try TestContext.expectBareWord(escaped[1].kind.word, "{a,b}");

    // In single quotes, braces are literal
    const single = try ctx.tokenize("echo '{a,b,c}'");
    try testing.expectEqual(@as(usize, 2), single.len);
    try TestContext.expectWord(single[1].kind.word, .single, "{a,b,c}");
}

// -----------------------------------------------------------------------------
// Operators
// -----------------------------------------------------------------------------

test "Operators: all types tokenize correctly" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Each input is "a <op> b" or "a <op>", operator is always token[1]
    const cases = [_]struct { []const u8, Operator }{
        // Pipes
        .{ "a | b", .pipe },
        .{ "a |> b", .pipe_arrow },
        // Logical
        .{ "a && b", .@"and" },
        .{ "a || b", .@"or" },
        // Background
        .{ "a &", .background },
        // Capture
        .{ "a => b", .capture },
        .{ "a =>@ b", .capture_lines },
        // Redirects
        .{ "a < b", .redirect_stdin },
        .{ "a > b", .redirect_stdout },
        .{ "a >> b", .redirect_stdout_append },
        .{ "a 2> b", .redirect_stderr },
        .{ "a 2>> b", .redirect_stderr_append },
        .{ "a &> b", .redirect_both },
        .{ "a 2>&1", .redirect_stderr_to_stdout },
    };

    for (cases) |case| {
        const tokens = try ctx.tokenize(case[0]);
        try testing.expectEqual(case[1], tokens[1].kind.operator);
    }
}

test "Operators: text 'and'/'or' tokenize as words" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Text operators are lexed as words; parser handles them contextually
    const tokens = try ctx.tokenize("true and echo ok or echo fail");
    try TestContext.expectBareWord(tokens[1].kind.word, "and");
    try TestContext.expectBareWord(tokens[4].kind.word, "or");
}

test "Operators: work adjacent to words without whitespace" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const tokens = try ctx.tokenize("echo|cat");

    try testing.expectEqual(@as(usize, 3), tokens.len);
    try TestContext.expectBareWord(tokens[0].kind.word, "echo");
    try testing.expectEqual(Operator.pipe, tokens[1].kind.operator);
    try TestContext.expectBareWord(tokens[2].kind.word, "cat");
}

// -----------------------------------------------------------------------------
// Separators
// -----------------------------------------------------------------------------

test "Separators: semicolon and newline" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const semi = try ctx.tokenize("echo a; echo b");
    try testing.expectEqual(@as(usize, 5), semi.len);
    try testing.expectEqual(Separator.semicolon, semi[2].kind.separator);

    const newline = try ctx.tokenize("echo a\necho b");
    try testing.expectEqual(@as(usize, 5), newline.len);
    try testing.expectEqual(Separator.newline, newline[2].kind.separator);
}

// -----------------------------------------------------------------------------
// Comments
// -----------------------------------------------------------------------------

test "Comments: trailing comment ignored, hash in word preserved" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Trailing comment is stripped
    const trailing = try ctx.tokenize("echo hello # this is a comment");
    try testing.expectEqual(@as(usize, 2), trailing.len);

    // Hash inside word is preserved
    const inside = try ctx.tokenize("echo foo#bar");
    try testing.expectEqual(@as(usize, 2), inside.len);
    try TestContext.expectBareWord(inside[1].kind.word, "foo#bar");
}

// -----------------------------------------------------------------------------
// Edge Cases
// -----------------------------------------------------------------------------

test "Edge cases: empty and whitespace-only input" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const empty = try ctx.tokenize("");
    try testing.expectEqual(@as(usize, 0), empty.len);

    const whitespace = try ctx.tokenize("   \t  ");
    try testing.expectEqual(@as(usize, 0), whitespace.len);
}

// -----------------------------------------------------------------------------
// Error Cases
// -----------------------------------------------------------------------------

test "Errors: unterminated strings, substitutions, and braces" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("echo 'hello", LexError.UnterminatedString);
    try ctx.expectError("echo \"hello", LexError.UnterminatedString);
    try ctx.expectError("echo $(whoami", LexError.UnterminatedCommandSubstitution);
    try ctx.expectError("echo {a,b", LexError.UnterminatedBrace);
}
