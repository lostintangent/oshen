//! Core interpreter module for the Oshen shell.
//!
//! This module orchestrates the shell's execution pipeline:
//!
//!     input string → lex → parse → expand → execute → exit status
//!
//! The pipeline is split into composable functions to support different use cases:
//!
//! - `execute()`: One-shot execution for REPL input, scripts, and command substitution
//! - `executeAndCapture()`: Execute and capture stdout (for `$(...)` and prompts)
//! - `parseInput()` + `executeAst()`: Separated parsing for loop optimization (parse once, execute many)
//! - `executeFile()`: Convenience wrapper for sourcing script files
//!
//! ## Memory Management
//!
//! All functions use arena allocators internally. Callers pass a backing allocator
//! (typically `std.heap.page_allocator` or another arena), and each execution creates
//! its own arena that's freed when the function returns.

const std = @import("std");
const State = @import("../runtime/state.zig").State;
const statement = @import("execution/statement.zig");
const capture = @import("execution/capture.zig");
const lexer = @import("../language/lexer.zig");
const parser = @import("../language/parser.zig");
const ast_mod = @import("../language/ast.zig");
const tokens = @import("../language/tokens.zig");
const io = @import("../terminal/io.zig");
const ansi = @import("../terminal/ansi.zig");

// =============================================================================
// Re-exports
// =============================================================================

/// Execute shell input and capture stdout.
/// Re-exported from execution/capture.zig for a unified interpreter API.
pub const executeAndCapture = capture.executeAndCapture;

/// Maximum file size for script execution (prevents accidental memory exhaustion)
const MAX_SCRIPT_SIZE: usize = 1024 * 1024; // 1MB

// =============================================================================
// Error Reporting
// =============================================================================

/// Formats a PascalCase error name as readable text: "InvalidEach" → "invalid each"
fn formatErrorName(buf: []u8, name: []const u8) []const u8 {
    var len: usize = 0;
    for (name, 0..) |c, i| {
        // Add space before uppercase letters (except at start)
        if (i > 0 and std.ascii.isUpper(c)) {
            if (len < buf.len) {
                buf[len] = ' ';
                len += 1;
            }
        }
        // Lowercase all characters
        if (len < buf.len) {
            buf[len] = std.ascii.toLower(c);
            len += 1;
        }
    }
    return buf[0..len];
}

fn printSyntaxError(input: []const u8, byte_pos: usize, err: anyerror) void {
    const lc = tokens.TokenSpan.getLineCol(input, byte_pos);
    var buf: [64]u8 = undefined;
    const msg = formatErrorName(&buf, @errorName(err));
    io.printError(
        ansi.bold ++ ansi.red ++ "Syntax error" ++ ansi.reset ++
            ansi.dim ++ " ({d}:{d})" ++ ansi.reset ++ ": {s}\n",
        .{ lc.line, lc.col, msg },
    );
}

// =============================================================================
// Types
// =============================================================================

/// A parsed program ready for execution.
///
/// Separating parsing from execution enables an important optimization: loop bodies
/// can be parsed once and executed many times, avoiding redundant lexing/parsing
/// on each iteration. The AST is immutable after parsing; only expansion (variable
/// substitution, globbing) happens per-execution.
pub const ParsedInput = struct {
    /// The parsed abstract syntax tree
    ast: ast_mod.Program,
    /// Original source text, retained for:
    /// - Nested control flow bodies (stored as source slices, parsed on-demand)
    /// - Error messages with source context
    input: []const u8,
};

// =============================================================================
// Public API
// =============================================================================

/// Parse shell input into an AST without executing it.
///
/// Use this when you need to execute the same code multiple times (e.g., loop bodies).
/// For one-shot execution, prefer `execute()` which handles both steps.
///
/// ## Example: Loop optimization
/// ```zig
/// // Parse body once before the loop
/// const body_ast = try parseInput(arena, for_stmt.body);
///
/// // Execute many times without re-parsing
/// for (items) |item| {
///     state.setVar("i", item);
///     _ = try executeAst(allocator, state, body_ast);
/// }
/// ```
///
/// ## Memory
/// The returned `ParsedInput` contains slices into memory owned by `allocator`.
/// The caller must keep the allocator alive for the lifetime of the `ParsedInput`.
pub fn parseInput(allocator: std.mem.Allocator, input: []const u8) !ParsedInput {
    var lex = lexer.Lexer.init(allocator, input);
    const toks = lex.tokenize() catch |err| {
        printSyntaxError(input, lex.pos, err);
        return err;
    };

    if (toks.len == 0) {
        return ParsedInput{
            .ast = ast_mod.Program{ .statements = &[_]ast_mod.Statement{} },
            .input = input,
        };
    }

    var p = parser.Parser.initWithInput(allocator, toks, input);
    const ast = p.parse() catch |err| {
        printSyntaxError(input, p.errorPos(), err);
        return err;
    };

    return ParsedInput{
        .ast = ast,
        .input = input,
    };
}

/// Execute a pre-parsed AST.
///
/// This is the core execution engine. It iterates through statements, expands each
/// (variable substitution, glob patterns, command substitution), and executes them.
/// Each call creates its own arena for expansion temporaries.
///
/// ## When to use
/// - Loop bodies: Parse once with `parseInput()`, then call `executeAst()` each iteration
/// - The `execute()` function calls this internally after parsing
///
/// ## Execution model
/// Statements execute sequentially. Variable assignments in earlier statements are
/// visible when expanding later statements (e.g., `set x 1; echo $x` works correctly).
///
/// ## Returns
/// The exit status of the last executed statement (0-255).
pub fn executeAst(allocator: std.mem.Allocator, state: *State, parsed: ParsedInput) !u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    return executeAstWithArena(arena.allocator(), state, parsed);
}

/// Execute a pre-parsed AST using a provided allocator (for loop optimization).
///
/// This variant allows callers to manage their own arena and reuse it across
/// multiple executions, avoiding the overhead of arena init/deinit per iteration.
/// Used by while/each loops to dramatically reduce allocation overhead.
///
/// ## When to use
/// - Loop bodies where the caller resets an arena between iterations
/// - Any case where you want to control the allocator lifetime
///
/// ## Returns
/// The exit status of the last executed statement (0-255).
pub fn executeAstWithArena(arena_alloc: std.mem.Allocator, state: *State, parsed: ParsedInput) !u8 {
    var last_status: u8 = 0;

    for (parsed.ast.statements) |stmt| {
        last_status = try statement.executeStatement(arena_alloc, state, stmt, parsed.input);

        // Exit signals bubble up to terminate the entire shell
        if (state.should_exit) {
            return state.exit_code;
        }

        // Interrupt signals bubble up to abort the current command
        if (state.interrupted) {
            return last_status;
        }

        // Break/continue signals bubble up to the enclosing loop
        if (state.loop_break or state.loop_continue) {
            return last_status;
        }

        // Return signals bubble up to the enclosing function
        if (state.fn_return) {
            return state.status;
        }
    }

    return last_status;
}

/// Execute shell input as a single operation (parse + execute).
///
/// This is the primary entry point for most execution contexts:
/// - REPL: Each line the user types
/// - Scripts: The entire file content (via `executeFile()`)
/// - Command substitution: The captured command string
/// - Function bodies: When a user-defined function is invoked
///
/// For repeated execution of the same code (loop bodies), use `parseInput()` +
/// `executeAst()` to avoid redundant parsing.
///
/// ## Returns
/// The exit status of the last executed statement (0-255).
pub fn execute(allocator: std.mem.Allocator, state: *State, input: []const u8) !u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const parsed = try parseInput(arena.allocator(), input);
    return executeAst(arena.allocator(), state, parsed);
}

/// Execute an Oshen script file.
///
/// Reads the file into memory and executes it via `execute()`. Used by:
/// - The `source` builtin to load configuration files
/// - Script execution when Oshen is invoked with a file argument
///
/// ## Limits
/// Files larger than 1MB are rejected to prevent accidental memory exhaustion.
///
/// ## Returns
/// The exit status of the last executed statement in the file (0-255).
pub fn executeFile(allocator: std.mem.Allocator, state: *State, path: []const u8) !u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, MAX_SCRIPT_SIZE);
    defer allocator.free(content);

    return execute(allocator, state, content);
}
