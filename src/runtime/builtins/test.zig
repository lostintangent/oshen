//! test/[ builtin - evaluate conditional expressions
//!
//! NOTE: Does not use Args API - implements custom recursive expression evaluator
//! for handling complex boolean logic with operators, parentheses, and precedence.

const std = @import("std");
const builtins = @import("../builtins.zig");

pub const builtin = builtins.Builtin{
    .name = "test",
    .run = run,
    .help =
    \\test EXPRESSION
    \\[ EXPRESSION ]
    \\
    \\Evaluate conditional expressions for use in if/while statements.
    \\Returns 0 (true) if expression evaluates to true, 1 (false) otherwise.
    \\
    \\File tests:
    \\  -e FILE         True if file exists
    \\  -f FILE         True if file exists and is regular file
    \\  -d FILE         True if file exists and is directory
    \\  -r FILE         True if file exists and is readable
    \\  -w FILE         True if file exists and is writable
    \\  -x FILE         True if file exists and is executable
    \\  -s FILE         True if file exists and has size > 0
    \\  -L FILE         True if file exists and is symbolic link
    \\  -h FILE         Alias for -L
    \\
    \\String tests:
    \\  -z STRING       True if string is empty
    \\  -n STRING       True if string is non-empty
    \\  STR1 = STR2     True if strings are equal
    \\  STR1 == STR2    Alias for =
    \\  STR1 != STR2    True if strings are not equal
    \\
    \\Numeric tests:
    \\  N1 -eq N2       True if integers are equal
    \\  N1 -ne N2       True if integers are not equal
    \\  N1 -lt N2       True if N1 is less than N2
    \\  N1 -le N2       True if N1 is less than or equal to N2
    \\  N1 -gt N2       True if N1 is greater than N2
    \\  N1 -ge N2       True if N1 is greater than or equal to N2
    \\
    \\Logical operators:
    \\  ! EXPR          True if EXPR is false (negation)
    \\  EXPR1 -a EXPR2  True if both expressions are true (and)
    \\  EXPR1 -o EXPR2  True if either expression is true (or)
    \\  ( EXPR )        Group expressions (requires spaces around parens)
    \\
    \\Examples:
    \\  test -f file.txt              # Check if file exists
    \\  [ -d /tmp ]                   # Check if directory exists
    \\  test -n "$var"                # Check if variable is non-empty
    \\  [ "$a" = "$b" ]               # String equality
    \\  test 5 -gt 3                  # Numeric comparison
    \\  [ -f file.txt -a -r file.txt ]  # File exists and is readable
    \\  test ! -e file.txt            # File does not exist
    \\  [ \( -f a -o -f b \) -a -x c ]  # Complex expression with grouping
    ,
};

pub const bracket_builtin = builtins.Builtin{
    .name = "[",
    .run = runBracket,
    .help = "[ EXPR ] - Evaluate conditional expression (alias for test)",
};

// =============================================================================
// Entry Points
// =============================================================================

fn runBracket(_: *builtins.State, args: []const []const u8) u8 {
    if (args.len == 0) {
        builtins.io.writeStderr("[: missing ']'\n");
        return 2;
    }

    const last = args[args.len - 1];
    if (!std.mem.eql(u8, last, "]")) {
        builtins.io.writeStderr("[: missing ']'\n");
        return 2;
    }

    // Just "[ ]" - no expression, returns false
    if (args.len == 1) return 1;

    return evaluateExpr(args[0 .. args.len - 1]);
}

fn run(_: *builtins.State, args: []const []const u8) u8 {
    if (args.len == 0) return 1; // No expression = false
    return evaluateExpr(args);
}

// =============================================================================
// Expression Evaluation
// =============================================================================

/// Evaluate a test expression. Returns 0 (true), 1 (false), or 2 (error).
fn evaluateExpr(args: []const []const u8) u8 {
    if (args.len == 0) return 1;

    // Handle negation: ! EXPR
    if (std.mem.eql(u8, args[0], "!")) {
        if (args.len == 1) return 0; // "!" alone is true (negation of empty)
        return negateResult(evaluateExpr(args[1..]));
    }

    // Handle parentheses: ( EXPR )
    if (std.mem.eql(u8, args[0], "(")) {
        return evaluateParenExpr(args);
    }

    // Dispatch by argument count
    return switch (args.len) {
        1 => if (args[0].len > 0) 0 else 1, // Non-empty string = true
        2 => evalUnary(args[0], args[1]),
        else => evaluateMultiArg(args),
    };
}

/// Negate a result: 0 <-> 1, preserve 2 (error).
fn negateResult(result: u8) u8 {
    return if (result == 0) 1 else if (result == 1) 0 else result;
}

/// Evaluate parenthesized expression: ( EXPR ) [OP EXPR...]
fn evaluateParenExpr(args: []const []const u8) u8 {
    const close_idx = findMatchingParen(args) orelse {
        builtins.io.printError("test: missing ')'\n", .{});
        return 2;
    };

    const inner_result = evaluateExpr(args[1..close_idx]);

    // Check for chained logical operators after )
    if (close_idx + 1 < args.len) {
        return evaluateBinaryLogical(inner_result, args[close_idx + 1 ..]);
    }
    return inner_result;
}

/// Find the index of the matching closing paren.
fn findMatchingParen(args: []const []const u8) ?usize {
    var depth: usize = 1;
    for (args[1..], 1..) |arg, i| {
        if (std.mem.eql(u8, arg, "(")) {
            depth += 1;
        } else if (std.mem.eql(u8, arg, ")")) {
            depth -= 1;
            if (depth == 0) return i;
        }
    }
    return null;
}

/// Evaluate 3+ argument expressions (binary operators or chained).
fn evaluateMultiArg(args: []const []const u8) u8 {
    if (args.len >= 3) {
        if (evalBinary(args[0], args[1], args[2])) |result| {
            if (args.len == 3) return result;
            return evaluateBinaryLogical(result, args[3..]);
        }
    }

    if (args.len >= 2) {
        const unary_result = evalUnary(args[0], args[1]);
        if (unary_result == 2) return 2;
        if (args.len == 2) return unary_result;
        return evaluateBinaryLogical(unary_result, args[2..]);
    }

    builtins.io.printError("test: too many arguments\n", .{});
    return 2;
}

/// Handle -a (and) / -o (or) chaining.
fn evaluateBinaryLogical(left_result: u8, remaining: []const []const u8) u8 {
    if (remaining.len == 0) return left_result;

    const op = remaining[0];

    if (std.mem.eql(u8, op, "-a")) {
        if (left_result != 0) return 1; // Short-circuit: false AND x = false
        if (remaining.len < 2) {
            builtins.io.printError("test: argument expected after -a\n", .{});
            return 2;
        }
        return evaluateExpr(remaining[1..]);
    }

    if (std.mem.eql(u8, op, "-o")) {
        if (left_result == 0) return 0; // Short-circuit: true OR x = true
        if (remaining.len < 2) {
            builtins.io.printError("test: argument expected after -o\n", .{});
            return 2;
        }
        return evaluateExpr(remaining[1..]);
    }

    builtins.io.printError("test: unknown operator: {s}\n", .{op});
    return 2;
}

// =============================================================================
// Unary Operators
// =============================================================================

/// Evaluate unary operators: -e, -f, -d, -r, -w, -x, -s, -L, -z, -n
fn evalUnary(op: []const u8, arg: []const u8) u8 {
    if (op.len < 2 or op[0] != '-') {
        builtins.io.printError("test: {s}: unary operator expected\n", .{op});
        return 2;
    }

    return switch (op[1]) {
        // File tests
        'e' => testFileStat(arg, null), // exists
        'f' => testFileStat(arg, .file), // regular file
        'd' => testFileStat(arg, .directory), // directory
        'L', 'h' => testSymlink(arg), // symlink
        's' => testFileHasSize(arg), // non-empty file

        // Permission tests
        'r' => testFileAccess(arg, .read_only),
        'w' => testFileAccess(arg, .write_only),
        'x' => testExecutable(arg),

        // String tests
        'z' => if (arg.len == 0) 0 else 1, // true if empty
        'n' => if (arg.len > 0) 0 else 1, // true if non-empty

        else => blk: {
            builtins.io.printError("test: {s}: unknown operator\n", .{op});
            break :blk 2;
        },
    };
}

// =============================================================================
// Binary Operators
// =============================================================================

/// Evaluate binary operators: =, ==, !=, -eq, -ne, -lt, -le, -gt, -ge
fn evalBinary(left: []const u8, op: []const u8, right: []const u8) ?u8 {
    // String comparisons
    if (std.mem.eql(u8, op, "=") or std.mem.eql(u8, op, "==")) {
        return if (std.mem.eql(u8, left, right)) 0 else 1;
    }
    if (std.mem.eql(u8, op, "!=")) {
        return if (!std.mem.eql(u8, left, right)) 0 else 1;
    }

    // Numeric comparisons (all start with -)
    if (op.len < 2 or op[0] != '-') return null;

    const left_num = std.fmt.parseInt(i64, left, 10) catch {
        builtins.io.printError("test: {s}: integer expression expected\n", .{left});
        return 2;
    };
    const right_num = std.fmt.parseInt(i64, right, 10) catch {
        builtins.io.printError("test: {s}: integer expression expected\n", .{right});
        return 2;
    };

    const cmp = op[1..];
    if (std.mem.eql(u8, cmp, "eq")) return if (left_num == right_num) 0 else 1;
    if (std.mem.eql(u8, cmp, "ne")) return if (left_num != right_num) 0 else 1;
    if (std.mem.eql(u8, cmp, "lt")) return if (left_num < right_num) 0 else 1;
    if (std.mem.eql(u8, cmp, "le")) return if (left_num <= right_num) 0 else 1;
    if (std.mem.eql(u8, cmp, "gt")) return if (left_num > right_num) 0 else 1;
    if (std.mem.eql(u8, cmp, "ge")) return if (left_num >= right_num) 0 else 1;

    return null;
}

// =============================================================================
// File Test Helpers
// =============================================================================

/// Test file stat with optional kind check.
fn testFileStat(path: []const u8, kind: ?std.fs.File.Kind) u8 {
    const stat = std.fs.cwd().statFile(path) catch return 1;
    if (kind) |k| {
        return if (stat.kind == k) 0 else 1;
    }
    return 0;
}

/// Test if path is a symlink.
fn testSymlink(path: []const u8) u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    _ = std.posix.readlink(path, &buf) catch return 1;
    return 0;
}

/// Test if file has non-zero size.
fn testFileHasSize(path: []const u8) u8 {
    const stat = std.fs.cwd().statFile(path) catch return 1;
    return if (stat.size > 0) 0 else 1;
}

/// Test file access mode (read/write).
fn testFileAccess(path: []const u8, mode: std.fs.File.OpenMode) u8 {
    const file = std.fs.cwd().openFile(path, .{ .mode = mode }) catch return 1;
    file.close();
    return 0;
}

/// Test if file is executable (any execute bit set).
fn testExecutable(path: []const u8) u8 {
    const stat = std.fs.cwd().statFile(path) catch return 1;
    const exec_bits = 0o111; // owner, group, other execute
    return if (stat.mode & exec_bits != 0) 0 else 1;
}

// =============================================================================
// Tests
// =============================================================================
//
// NOTE: Basic operator tests (string equality, numeric comparisons, file tests)
// are covered by e2e.wave. These unit tests focus on:
// - Parser edge cases not easily tested via E2E
// - Error handling and malformed expressions
// - Logical operators (-a, -o) and parentheses (complex parsing)

const testing = std.testing;

// -----------------------------------------------------------------------------
// Test: edge cases and logical operators
// -----------------------------------------------------------------------------

test "Test: edge cases and negation" {
    const cases = .{
        // Edge cases (not covered by E2E)
        .{ &[_][]const u8{}, @as(u8, 1) }, // no args returns false
        .{ &[_][]const u8{""}, @as(u8, 1) }, // single empty string returns false
        .{ &[_][]const u8{"!"}, @as(u8, 0) }, // "!" alone is truthy (non-empty string)
        // Double negation
        .{ &[_][]const u8{ "!", "!", "" }, @as(u8, 1) }, // !!empty = false
        .{ &[_][]const u8{ "!", "!", "hello" }, @as(u8, 0) }, // !!nonempty = true
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], run(undefined, case[0]));
    }
}

test "Test: logical operators (-a and -o)" {
    const cases = .{
        // -a (and)
        .{ &[_][]const u8{ "-n", "a", "-a", "-n", "b" }, @as(u8, 0) }, // true AND true
        .{ &[_][]const u8{ "-z", "a", "-a", "-n", "b" }, @as(u8, 1) }, // false AND true (short-circuit)
        .{ &[_][]const u8{ "-n", "a", "-a", "-z", "b" }, @as(u8, 1) }, // true AND false
        // -o (or)
        .{ &[_][]const u8{ "-n", "a", "-o", "-n", "b" }, @as(u8, 0) }, // true OR true (short-circuit)
        .{ &[_][]const u8{ "-z", "a", "-o", "-n", "b" }, @as(u8, 0) }, // false OR true
        .{ &[_][]const u8{ "-z", "a", "-o", "-z", "b" }, @as(u8, 1) }, // false OR false
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], run(undefined, case[0]));
    }
}

// -----------------------------------------------------------------------------
// Test: parentheses (complex parsing)
// -----------------------------------------------------------------------------

test "Test: parentheses grouping" {
    const cases = .{
        // Simple grouping
        .{ &[_][]const u8{ "(", "-n", "hello", ")" }, @as(u8, 0) },
        .{ &[_][]const u8{ "(", "-z", "hello", ")" }, @as(u8, 1) },
        // With operators inside
        .{ &[_][]const u8{ "(", "-n", "a", "-a", "-z", "a", ")" }, @as(u8, 1) },
        .{ &[_][]const u8{ "(", "-n", "a", "-o", "-z", "a", ")" }, @as(u8, 0) },
        // Nested
        .{ &[_][]const u8{ "(", "(", "-n", "x", ")", ")" }, @as(u8, 0) },
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], run(undefined, case[0]));
    }
}

// -----------------------------------------------------------------------------
// Test: bracket syntax and error cases
// -----------------------------------------------------------------------------

test "Test: bracket syntax errors" {
    const cases = .{
        .{ &[_][]const u8{ "-n", "hello" }, @as(u8, 2) }, // missing closing bracket
        .{ &[_][]const u8{}, @as(u8, 2) }, // empty args
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], runBracket(undefined, case[0]));
    }
}

test "Test: error cases" {
    const cases = .{
        .{ &[_][]const u8{ "abc", "-eq", "5" }, @as(u8, 2) }, // invalid numeric comparison
        .{ &[_][]const u8{ "-X", "foo" }, @as(u8, 2) }, // unknown unary operator
        .{ &[_][]const u8{ "(", "-n", "x" }, @as(u8, 2) }, // missing closing paren
        .{ &[_][]const u8{ "-n", "x", "-a" }, @as(u8, 2) }, // missing argument after -a
        .{ &[_][]const u8{ "-z", "x", "-o" }, @as(u8, 2) }, // missing argument after -o
    };
    inline for (cases) |case| {
        try testing.expectEqual(case[1], run(undefined, case[0]));
    }
}
