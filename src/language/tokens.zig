//! Token types and related definitions for the Oshen shell lexer.
//!
//! Type hierarchy:
//! - `Token`: A lexical unit produced by the lexer, containing:
//!   - `TokenKind`: A union of either a word, operator, or separator
//!   - `TokenSpan`: The source position of the token (byte indices) in the script/command
//! - `WordPart`: A part of a word token with its quoting context
//!       (e.g., `hello"world"` produces two WordParts: one unquoted, one double-quoted)
//! - `Operator`: Enum of all shell operators (pipes, redirects, logical, capture, background)
//! - `Separator`: Enum of command separators (newline, semicolon)
//!
//! ## Keywords
//!
//! Unlike most programming languages, keywords (if, for, while, etc.) are
//! not a distinct token type. This is because shell keywords are context-sensitive:
//! `if` in command position starts a conditional, but `echo if` just prints "if".
//! Additionally, quoting escapes keyword interpretation: `"if"` is always a word.
//! The parser determines keyword semantics based on position; the lexer just
//! produces generic word tokens.

const std = @import("std");

/// Indicates how a word part was quoted in the source.
/// Used by the expander to determine which expansions apply:
/// - `none`: bare word, all expansions apply (variables, globs, escapes)
/// - `double`: double-quoted, variable expansion only (no globs)
/// - `single`: single-quoted, no expansion (literal)
/// - `command`: command substitution $(...) or (...), executed and result expanded
/// - `brace`: brace expansion {...}, expands to multiple values
pub const QuoteKind = enum {
    none,
    double,
    single,
    command,
    brace,
};

/// A part of a word with its quoting context.
///
/// Words are composed of one or more parts when quote boundaries are present.
/// This enables Cartesian product expansion where each part can have different
/// expansion behavior based on its quote type.
///
/// Examples:
/// - `hello` → single part: { .quotes = .none, .text = "hello" }
/// - `"hello"` → single part: { .quotes = .double, .text = "hello" }
/// - `hello"world"` → two parts:
///   - { .quotes = .none, .text = "hello" }
///   - { .quotes = .double, .text = "world" }
/// - `$var"_suffix"` → two parts enabling Cartesian product:
///   - { .quotes = .none, .text = "$var" } (expands to list)
///   - { .quotes = .double, .text = "_suffix" } (literal)
///   Result: if $var = [a, b] then expansion produces [a_suffix, b_suffix]
pub const WordPart = struct {
    quotes: QuoteKind,
    text: []const u8,

    /// Compares two word parts for semantic equality.
    /// Note: Compares text content (not pointer identity) and quote kind.
    /// Two word parts are equal if they have the same quote type and text content.
    pub fn eql(self: WordPart, other: WordPart) bool {
        return self.quotes == other.quotes and std.mem.eql(u8, self.text, other.text);
    }
};

/// Returns the text if parts is a single bare (unquoted) word, null otherwise.
/// This is a common pattern for keyword detection and simple word extraction.
pub fn getBareText(parts: []const WordPart) ?[]const u8 {
    if (parts.len == 1 and parts[0].quotes == .none) {
        return parts[0].text;
    }
    return null;
}

/// Source location for a token as byte indices for O(1) source slicing.
/// Line/column positions can be computed on-demand from byte indices when needed
/// for error reporting (see `getLineCol`).
pub const TokenSpan = struct {
    start: usize,
    end: usize,

    /// Computes line and column numbers from a byte position.
    /// Useful for error messages - only computed when actually needed.
    pub fn getLineCol(input: []const u8, byte_pos: usize) struct { line: usize, col: usize } {
        var line: usize = 1;
        var col: usize = 1;
        for (input[0..@min(byte_pos, input.len)]) |c| {
            if (c == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        return .{ .line = line, .col = col };
    }
};

/// Shell operators recognized by the lexer.
/// Ordered conceptually by category for clarity.
pub const Operator = enum {
    // Pipe operators
    pipe, // |
    pipe_arrow, // |>

    // Logical operators
    @"and", // &&
    @"or", // ||

    // Redirect operators
    redirect_stdin, // <
    redirect_stdout, // >
    redirect_stdout_append, // >>
    redirect_stderr, // 2>
    redirect_stderr_append, // 2>>
    redirect_both, // &>
    redirect_stderr_to_stdout, // 2>&1

    // Capture operators
    capture, // =>
    capture_lines, // =>@

    // Background
    background, // &

    /// Returns the character length of this operator in source text.
    pub fn len(self: Operator) usize {
        return switch (self) {
            .redirect_stderr_to_stdout => 4, // 2>&1
            .capture_lines, .redirect_stderr_append => 3, // =>@, 2>>
            .pipe_arrow, .@"and", .@"or", .capture, .redirect_stdout_append, .redirect_stderr, .redirect_both => 2,
            .pipe, .redirect_stdin, .redirect_stdout, .background => 1,
        };
    }

    /// Returns true if this operator is a redirection operator.
    pub fn isRedirect(self: Operator) bool {
        return switch (self) {
            .redirect_stdin, .redirect_stdout, .redirect_stdout_append, .redirect_stderr, .redirect_stderr_append, .redirect_both, .redirect_stderr_to_stdout => true,
            else => false,
        };
    }

    /// Returns true if this operator is a pipe operator.
    pub fn isPipe(self: Operator) bool {
        return self == .pipe or self == .pipe_arrow;
    }

    /// Returns true if this operator is a logical operator.
    pub fn isLogical(self: Operator) bool {
        return self == .@"and" or self == .@"or";
    }

    /// Returns true if this operator is a capture operator.
    pub fn isCapture(self: Operator) bool {
        return self == .capture or self == .capture_lines;
    }
};

/// Command separators.
pub const Separator = enum {
    newline, // \n
    semicolon, // ;
};

pub const TokenKind = union(enum) {
    word: []const WordPart,
    operator: Operator,
    separator: Separator,
};

pub const Token = struct {
    kind: TokenKind,
    span: TokenSpan,

    pub fn initWord(word_parts: []const WordPart, tok_span: TokenSpan) Token {
        return .{ .kind = .{ .word = word_parts }, .span = tok_span };
    }

    pub fn initOp(op: Operator, tok_span: TokenSpan) Token {
        return .{ .kind = .{ .operator = op }, .span = tok_span };
    }

    pub fn initSep(sep: Separator, tok_span: TokenSpan) Token {
        return .{ .kind = .{ .separator = sep }, .span = tok_span };
    }
};

/// Helper to create a compile-time string set from a simple list of strings.
/// Avoids the verbose `.{ "str", {} }` syntax required by StaticStringMap.
fn stringSet(comptime strings: []const []const u8) std.StaticStringMap(void) {
    comptime {
        var kvs: [strings.len]struct { []const u8, void } = undefined;
        for (strings, 0..) |s, i| {
            kvs[i] = .{ s, {} };
        }
        return std.StaticStringMap(void).initComptime(&kvs);
    }
}

const keywords = stringSet(&.{ "and", "or", "fun", "end", "if", "else", "for", "each", "in", "while", "break", "continue", "return", "defer", "exit" });

/// Returns true if the word is any shell keyword.
pub fn isKeyword(word: []const u8) bool {
    return keywords.has(word);
}

// Keyword subsets for parser use
const logical_keywords = stringSet(&.{ "and", "or" });
const block_keywords = stringSet(&.{ "if", "for", "each", "while", "fun" });

/// Returns true if the word is a logical keyword (and, or).
pub fn isLogicalKeyword(word: []const u8) bool {
    return logical_keywords.has(word);
}

/// Returns true if the word starts a block (if, for, each, while, fun).
pub fn isBlockKeyword(word: []const u8) bool {
    return block_keywords.has(word);
}

/// Returns true if `c` is a valid identifier character (alphanumeric or underscore).
pub fn isIdentChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

/// Returns true if `c` is a valid identifier start character (alphabetic or underscore).
pub fn isIdentStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

/// Validates that a string is a valid identifier.
/// Must start with alphabetic or underscore, and contain only alphanumeric or underscores.
pub fn isValidIdentifier(text: []const u8) bool {
    if (text.len == 0) return false;
    if (!isIdentStart(text[0])) return false;
    for (text[1..]) |c| {
        if (!isIdentChar(c)) return false;
    }
    return true;
}

/// Returns true if text is a variable reference ($identifier or $N).
pub fn isVariable(text: []const u8) bool {
    if (text.len < 2 or text[0] != '$') return false;
    const name = text[1..];

    // Named variable ($foo, $HOME)
    if (isValidIdentifier(name)) return true;

    // Positional ($1, $2, etc.)
    for (name) |c| {
        if (!std.ascii.isDigit(c)) return false;
    }
    return true;
}

/// Returns true if `c` is horizontal whitespace (space or tab).
/// Note: newlines are not whitespace - they are command separators.
pub fn isWhitespace(c: u8) bool {
    return c == ' ' or c == '\t';
}

/// Returns true if `c` is a command separator (newline or semicolon).
pub fn isSeparator(c: u8) bool {
    return c == '\n' or c == ';';
}

/// Returns true if `c` is a word boundary (whitespace or command separator).
pub fn isWordBreak(c: u8) bool {
    return isWhitespace(c) or isSeparator(c);
}
