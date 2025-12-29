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
    UnterminatedCmdSub,
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
    /// Operators are checked longest-first to ensure correct matching (e.g., "2>&1" before "2>").
    fn peekOperator(self: *const Lexer) ?Operator {
        // 4-character operators
        if (self.matchesStr("2>&1")) return .redirect_stderr_to_stdout;
        // 3-character operators
        if (self.matchesStr("=>@")) return .capture_lines;
        if (self.matchesStr("2>>")) return .redirect_stderr_append;
        // 2-character operators
        if (self.matchesStr("&>")) return .redirect_both;
        if (self.matchesStr("|>")) return .pipe_arrow;
        if (self.matchesStr("&&")) return .@"and";
        if (self.matchesStr("||")) return .@"or";
        if (self.matchesStr("=>")) return .capture;
        if (self.matchesStr("2>")) return .redirect_stderr;
        if (self.matchesStr(">>")) return .redirect_stdout_append;
        // 1-character operators
        if (self.matchesStr("|")) return .pipe;
        if (self.matchesStr("&")) return .background;
        if (self.matchesStr("<")) return .redirect_stdin;
        if (self.matchesStr(">")) return .redirect_stdout;
        return null;
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
    /// Processes the character after a backslash and appends the result to buf.
    fn handleDoubleQuotedEscape(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) error{OutOfMemory}!void {
        const next = self.peek() orelse {
            try buf.append(self.allocator, '\\');
            return;
        };

        switch (next) {
            '"', '\\' => {
                try buf.append(self.allocator, next);
                self.advance();
            },
            'n' => {
                try buf.append(self.allocator, '\n');
                self.advance();
            },
            't' => {
                try buf.append(self.allocator, '\t');
                self.advance();
            },
            '$' => {
                // Preserve escape so expansion treats `$` literally
                try buf.appendSlice(self.allocator, "\\$");
                self.advance();
            },
            else => {
                // Unknown escape: preserve both characters
                try buf.appendSlice(self.allocator, &.{ '\\', next });
                self.advance();
            },
        }
    }

    /// Handles escape sequences in bare words.
    /// Simpler than double-quoted: only `\$` is special (preserved for expander).
    fn handleBareWordEscape(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) error{OutOfMemory}!void {
        const next = self.peek() orelse {
            try buf.append(self.allocator, '\\');
            return;
        };

        if (next == '$') {
            // Preserve escape so expansion treats `$` literally
            try buf.appendSlice(self.allocator, "\\$");
        } else {
            // Other escapes: just append the escaped character
            try buf.append(self.allocator, next);
        }
        self.advance();
    }

    // =========================================================================
    // Command substitution handling
    // =========================================================================

    /// Reads a command substitution `$(...)` with nested parenthesis matching.
    /// Assumes we're positioned at the `$` of `$(`.
    fn readCommandSubstitution(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!void {
        std.debug.assert(self.peek() == '$');
        std.debug.assert(self.peekAt(1) == '(');

        try buf.appendSlice(self.allocator, "$(");
        self.advanceBy(2); // skip `$(`

        try self.readParenContent(buf);
    }

    /// Reads a bare command substitution `(...)` and normalizes to `$(...)`.
    /// Assumes we're positioned at the opening `(`.
    fn readBareCommandSubstitution(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!void {
        std.debug.assert(self.peek() == '(');

        // Normalize to $(...) so expander can use the same logic
        try buf.appendSlice(self.allocator, "$(");
        self.advance(); // skip `(`

        try self.readParenContent(buf);
    }

    /// Reads the content inside parentheses with nested paren matching.
    /// Assumes we're positioned after the opening `(` and `$(` has been written.
    fn readParenContent(self: *Lexer, buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!void {
        var depth: usize = 1;
        while (self.peek()) |ch| {
            switch (ch) {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if (depth == 0) {
                        try buf.append(self.allocator, ')');
                        self.advance();
                        return;
                    }
                },
                // TODO: Handle escaped parens and quoted strings containing parens
                else => {},
            }
            try buf.append(self.allocator, ch);
            self.advance();
        }
        return LexError.UnterminatedCmdSub;
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
    fn readDoubleQuoted(self: *Lexer, parts: *std.ArrayListUnmanaged(WordPart), buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!void {
        self.advance(); // skip opening quote
        buf.clearRetainingCapacity();

        while (self.peek()) |c| {
            switch (c) {
                '"' => {
                    self.advance(); // skip closing quote
                    const content = try self.allocator.dupe(u8, buf.items);
                    try parts.append(self.allocator, .{ .quotes = .double, .text = content });
                    return;
                },
                '\\' => {
                    self.advance();
                    try self.handleDoubleQuotedEscape(buf);
                },
                else => {
                    try buf.append(self.allocator, c);
                    self.advance();
                },
            }
        }
        return LexError.UnterminatedString;
    }

    /// Reads an unquoted (bare) word segment.
    /// Returns true if any content was read.
    fn readBareWord(self: *Lexer, parts: *std.ArrayListUnmanaged(WordPart), buf: *std.ArrayListUnmanaged(u8)) (error{OutOfMemory} || LexError)!bool {
        buf.clearRetainingCapacity();

        while (self.peek()) |c| {
            // Stop at word breaks and operators
            if (token_types.isWordBreak(c)) break;
            if (self.peekOperator() != null) break;

            switch (c) {
                '\\' => {
                    self.advance();
                    try self.handleBareWordEscape(buf);
                },
                '"', '\'' => break, // Quote starts new segment
                '$' => {
                    if (self.peekAt(1) == '(') {
                        try self.readCommandSubstitution(buf);
                    } else {
                        try buf.append(self.allocator, c);
                        self.advance();
                    }
                },
                '(' => {
                    // Bare paren command substitution: (cmd) → normalized to $(cmd)
                    try self.readBareCommandSubstitution(buf);
                },
                else => {
                    try buf.append(self.allocator, c);
                    self.advance();
                },
            }
        }

        if (buf.items.len > 0) {
            const content = try self.allocator.dupe(u8, buf.items);
            try parts.append(self.allocator, .{ .quotes = .none, .text = content });
            return true;
        }
        return false;
    }

    /// Reads a complete word token (may contain multiple quoted/unquoted segments).
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

    /// Asserts that a word token has a single bare segment with the expected text.
    fn expectBareWord(segs: []const WordPart, expected: []const u8) !void {
        if (segs.len == 1 and segs[0].quotes == .none) {
            try testing.expectEqualStrings(expected, segs[0].text);
            return;
        }
        return error.TestExpectedEqual;
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

test "Words: mixed quote segments combine into single token" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const tokens = try ctx.tokenize("echo hello\"world\"'!'");

    try testing.expectEqual(@as(usize, 2), tokens.len);
    const segs = tokens[1].kind.word;
    try testing.expectEqual(@as(usize, 3), segs.len);

    // Each segment preserves its quote context
    try testing.expectEqual(QuoteKind.none, segs[0].quotes);
    try testing.expectEqualStrings("hello", segs[0].text);
    try testing.expectEqual(QuoteKind.double, segs[1].quotes);
    try testing.expectEqualStrings("world", segs[1].text);
    try testing.expectEqual(QuoteKind.single, segs[2].quotes);
    try testing.expectEqualStrings("!", segs[2].text);
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

// -----------------------------------------------------------------------------
// Command Substitution
// -----------------------------------------------------------------------------

test "Command substitution: $() and bare () forms" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // $() syntax
    const dollar = try ctx.tokenize("echo $(whoami)");
    try testing.expectEqual(@as(usize, 2), dollar.len);
    try TestContext.expectBareWord(dollar[1].kind.word, "$(whoami)");

    // Bare () gets normalized to $()
    const bare = try ctx.tokenize("echo (whoami)");
    try testing.expectEqual(@as(usize, 2), bare.len);
    try TestContext.expectBareWord(bare[1].kind.word, "$(whoami)");
}

test "Command substitution: nested and with pipes" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Nested substitution
    const nested = try ctx.tokenize("echo $(dirname $(pwd))");
    try TestContext.expectBareWord(nested[1].kind.word, "$(dirname $(pwd))");

    // Bare parens nested
    const bareNested = try ctx.tokenize("echo (dirname (pwd))");
    try TestContext.expectBareWord(bareNested[1].kind.word, "$(dirname (pwd))");

    // Pipe inside parens stays inside
    const withPipe = try ctx.tokenize("echo (ls | head)");
    try TestContext.expectBareWord(withPipe[1].kind.word, "$(ls | head)");
}

test "Command substitution: concatenation with text" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const tokens = try ctx.tokenize("file_(date).txt");
    try testing.expectEqual(@as(usize, 1), tokens.len);
    try TestContext.expectBareWord(tokens[0].kind.word, "file_$(date).txt");
}

// -----------------------------------------------------------------------------
// Operators (table-driven)
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

test "Errors: unterminated strings and substitutions" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("echo 'hello", LexError.UnterminatedString);
    try ctx.expectError("echo \"hello", LexError.UnterminatedString);
    try ctx.expectError("echo $(whoami", LexError.UnterminatedCmdSub);
}
