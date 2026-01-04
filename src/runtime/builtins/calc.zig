//! calc builtin - evaluate arithmetic expressions
//!
//! NOTE: Does not use Args API - joins all arguments into a single expression
//! string and uses recursive descent parser to evaluate with proper precedence.

const std = @import("std");
const builtins = @import("../builtins.zig");

pub const builtin = builtins.Builtin{
    .name = "calc",
    .run = run,
    .help =
    \\calc EXPRESSION
    \\= EXPRESSION
    \\
    \\Evaluate arithmetic expressions and print the result.
    \\All arguments are joined with spaces to form the expression.
    \\
    \\Operators (in order of precedence):
    \\  ( )         Grouping (highest precedence)
    \\  - (unary)   Negation
    \\  * x / %     Multiplication, division, modulo
    \\  + -         Addition, subtraction (lowest precedence)
    \\
    \\Notes:
    \\  - Use 'x' for multiplication without quotes: calc 2 x 3
    \\  - Use '*' for multiplication with quotes: calc "2 * 3"
    \\  - Operators are left-associative: 10 - 5 - 2 = (10 - 5) - 2 = 3
    \\  - Integer arithmetic only (i64)
    \\  - Division by zero returns an error
    \\  - Overflow detection enabled
    \\
    \\Examples:
    \\  calc 2 + 3                  # Simple addition → 5
    \\  = 2 + 3 x 4                 # Precedence → 14
    \\  calc "(2 + 3) * 4"          # Grouping → 20
    \\  = 10 / 3                    # Integer division → 3
    \\  calc 17 % 5                 # Modulo → 2
    \\  = -5 + 3                    # Unary minus → -2
    \\  calc 2 x 3 x 4              # Left associative → 24
    \\  = $x + 1                    # Using variables
    ,
};

pub const equals_builtin = builtins.Builtin{
    .name = "=",
    .run = run,
    .help = "= EXPR - Evaluate arithmetic expression (alias for calc)",
};

fn run(_: *builtins.State, args: []const []const u8) u8 {
    if (args.len == 0) {
        builtins.io.writeStderr("calc: missing expression\n");
        return 1;
    }

    // Join arguments with spaces: ["2", "+", "3"] → "2 + 3"
    var buf: [4096]u8 = undefined;
    const expr = builtins.joinArgsToBuffer(args, &buf) orelse {
        builtins.io.writeStderr("calc: expression too long\n");
        return 1;
    };

    // Parse and evaluate
    var parser = Parser.init(expr);
    const result = parser.parse() catch |err| {
        builtins.io.printError("calc: {s}\n", .{errorMessage(err)});
        return 1;
    };

    // Print result
    var out: [32]u8 = undefined;
    const str = std.fmt.bufPrint(&out, "{d}\n", .{result}) catch return 1;
    builtins.io.writeStdout(str);
    return 0;
}

fn errorMessage(err: ParseError) []const u8 {
    return switch (err) {
        error.DivisionByZero => "division by zero",
        error.InvalidNumber => "invalid number",
        error.UnexpectedEnd => "unexpected end of expression",
        error.UnmatchedParen => "unmatched parenthesis",
        error.Overflow => "integer overflow",
    };
}

// =============================================================================
// Recursive descent parser for arithmetic expressions
// =============================================================================
//
// Grammar:
//   expr   → term (('+' | '-') term)*
//   term   → factor (('*' | 'x' | '/' | '%') factor)*
//   factor → NUMBER | '(' expr ')' | '-' factor
//
// The 'x' operator is a shell-friendly alias for '*' (avoids glob expansion).

pub const ParseError = error{
    DivisionByZero,
    InvalidNumber,
    UnexpectedEnd,
    UnmatchedParen,
    Overflow,
};

pub const Parser = struct {
    input: []const u8,
    pos: usize = 0,

    pub fn init(input: []const u8) Parser {
        return .{ .input = input };
    }

    /// Parse and evaluate the full expression, ensuring no trailing content.
    pub fn parse(self: *Parser) ParseError!i64 {
        const result = try self.expr();
        self.skipWhitespace();
        if (self.pos < self.input.len) return error.InvalidNumber;
        return result;
    }

    fn expr(self: *Parser) ParseError!i64 {
        var left = try self.term();
        while (true) {
            self.skipWhitespace();
            const op = self.peek() orelse break;
            if (op != '+' and op != '-') break;
            self.advance();
            const right = try self.term();
            left = switch (op) {
                '+' => std.math.add(i64, left, right) catch return error.Overflow,
                '-' => std.math.sub(i64, left, right) catch return error.Overflow,
                else => unreachable,
            };
        }
        return left;
    }

    fn term(self: *Parser) ParseError!i64 {
        var left = try self.factor();
        while (true) {
            self.skipWhitespace();
            const op = self.peek() orelse break;

            // 'x' is multiplication only if followed by whitespace/digit/paren (not part of word)
            if (op == 'x') {
                const next = if (self.pos + 1 < self.input.len) self.input[self.pos + 1] else 0;
                if (next != ' ' and next != 0 and !std.ascii.isDigit(next) and next != '(' and next != '-') {
                    break; // Not a standalone 'x', treat as end of expression
                }
            }

            if (op != '*' and op != 'x' and op != '/' and op != '%') break;

            self.advance();
            const right = try self.factor();
            left = switch (op) {
                '*', 'x' => std.math.mul(i64, left, right) catch return error.Overflow,
                '/' => if (right == 0) return error.DivisionByZero else @divTrunc(left, right),
                '%' => if (right == 0) return error.DivisionByZero else @mod(left, right),
                else => unreachable,
            };
        }
        return left;
    }

    fn factor(self: *Parser) ParseError!i64 {
        self.skipWhitespace();
        const c = self.peek() orelse return error.UnexpectedEnd;

        if (c == '-') {
            self.advance();
            return std.math.negate(try self.factor()) catch return error.Overflow;
        }
        if (c == '(') {
            self.advance();
            const result = try self.expr();
            self.skipWhitespace();
            if (self.peek() != ')') return error.UnmatchedParen;
            self.advance();
            return result;
        }
        if (c == ')') return error.UnmatchedParen;
        return self.number();
    }

    fn number(self: *Parser) ParseError!i64 {
        self.skipWhitespace();
        const start = self.pos;
        while (self.peek()) |c| {
            if (!std.ascii.isDigit(c)) break;
            self.advance();
        }
        if (self.pos == start) return error.InvalidNumber;
        return std.fmt.parseInt(i64, self.input[start..self.pos], 10) catch error.InvalidNumber;
    }

    inline fn peek(self: *const Parser) ?u8 {
        return if (self.pos < self.input.len) self.input[self.pos] else null;
    }

    inline fn advance(self: *Parser) void {
        self.pos += @intFromBool(self.pos < self.input.len);
    }

    fn skipWhitespace(self: *Parser) void {
        while (self.pos < self.input.len and self.input[self.pos] == ' ') self.pos += 1;
    }
};

// =============================================================================
// Tests
// =============================================================================
//
// NOTE: Basic arithmetic, precedence, x operator, and unary minus are covered
// by e2e.wave (lines 642-680). These unit tests focus on:
// - Parser edge cases (whitespace, deeply nested parens)
// - Error handling (division by zero, unmatched parens, invalid input, overflow)
// - Left-to-right associativity

const testing = std.testing;

fn eval(input: []const u8) ParseError!i64 {
    var p = Parser.init(input);
    return p.parse();
}

// -----------------------------------------------------------------------------
// Calc: associativity and precedence
// -----------------------------------------------------------------------------

test "Calc: associativity and precedence" {
    const cases = .{
        // left-to-right same-level operators
        .{ "10 - 5 - 3", @as(i64, 2) }, // (10 - 5) - 3 = 2
        .{ "24 / 4 / 3", @as(i64, 2) }, // (24 / 4) / 3 = 2
        // modulo same as multiplication/division
        .{ "2 * 7 % 4", @as(i64, 2) }, // (2 * 7) % 4 = 14 % 4 = 2
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], try eval(case[0]));
    }
}

// -----------------------------------------------------------------------------
// Calc: parser edge cases
// -----------------------------------------------------------------------------

test "Calc: parser edge cases" {
    const cases = .{
        // deeply nested parens
        .{ "(((10)))", @as(i64, 10) },
        .{ "((1 + 2) * (1 + 1))", @as(i64, 6) },
        // whitespace variations
        .{ "  2 + 3  ", @as(i64, 5) },
        .{ "2+3", @as(i64, 5) },
        .{ "2+3*4", @as(i64, 14) },
        // unary minus
        .{ "--5", @as(i64, 5) }, // double minus
        .{ "---5", @as(i64, -5) }, // triple minus
        .{ "-(2 + 3)", @as(i64, -5) }, // negate parenthesized
        // x multiplication whitespace
        .{ "2x3", @as(i64, 6) },
        .{ "2 x3", @as(i64, 6) },
        .{ "2x 3", @as(i64, 6) },
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], try eval(case[0]));
    }
}

// -----------------------------------------------------------------------------
// Calc: error cases
// -----------------------------------------------------------------------------

test "Calc: error cases" {
    const cases = .{
        // division by zero
        .{ "5 / 0", error.DivisionByZero },
        .{ "5 % 0", error.DivisionByZero },
        // unmatched parentheses
        .{ "(2 + 3", error.UnmatchedParen },
        .{ ")", error.UnmatchedParen },
        .{ "((2 + 3)", error.UnmatchedParen },
        // invalid input
        .{ "abc", error.InvalidNumber },
        .{ "2 + 3 abc", error.InvalidNumber }, // trailing garbage
        // empty input
        .{ "", error.UnexpectedEnd },
        .{ "   ", error.UnexpectedEnd },
        // incomplete expression
        .{ "2 +", error.UnexpectedEnd },
    };
    inline for (cases) |case| {
        try testing.expectError(case[1], eval(case[0]));
    }
}

// -----------------------------------------------------------------------------
// Calc: overflow handling
// -----------------------------------------------------------------------------

test "Calc: overflow handling" {
    // i64 max = 9223372036854775807
    // Note: the parser handles negative numbers as unary minus + positive number,
    // so we test overflow through arithmetic operations
    const cases = .{
        .{ "9223372036854775807 + 1", error.Overflow }, // max + 1
        .{ "9223372036854775807 * 2", error.Overflow }, // max * 2
        .{ "0 - 9223372036854775807 - 2", error.Overflow }, // min - 1 (via subtraction)
    };
    inline for (cases) |case| {
        try testing.expectError(case[1], eval(case[0]));
    }

    // These should NOT overflow
    try testing.expectEqual(@as(i64, 9223372036854775807), try eval("9223372036854775807"));
    try testing.expectEqual(@as(i64, -9223372036854775807), try eval("0 - 9223372036854775807"));
}
