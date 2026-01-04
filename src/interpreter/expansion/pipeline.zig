//! Pipeline expansion: transforms parsed pipelines into expanded form ready for execution.
//!
//! This module bridges the parser and executor by expanding pipeline commands:
//! - Command arguments are expanded (variables, globs, command substitution)
//! - Aliases are resolved
//! - Redirections are evaluated
//!
//! Types defined here:
//! - ExpandedCommand - fully expanded command with argv, env, and redirects
//! - ExpandedRedirect - expanded I/O redirection

const std = @import("std");
const ast = @import("../../language/ast.zig");
const token_types = @import("../../language/tokens.zig");
const expand = @import("word.zig");
const lexer_mod = @import("../../language/lexer.zig");
const State = @import("../../runtime/state.zig").State;

// =============================================================================
// Expanded Types
// =============================================================================

/// The type of I/O redirection after expansion (paths are strings).
pub const ExpandedRedirectKind = union(enum) {
    /// Redirect input from a file to fd (defaults to stdin)
    read: []const u8,
    /// Redirect output to a file (truncate) from fd (defaults to stdout)
    write_truncate: []const u8,
    /// Redirect output to a file (append) from fd (defaults to stdout)
    write_append: []const u8,
    /// Duplicate one fd to another (e.g., 2>&1)
    dup: u8,
};

/// A single I/O redirection after expansion (paths are resolved strings).
pub const ExpandedRedirect = struct {
    /// File descriptor being redirected (0=stdin, 1=stdout, 2=stderr)
    from_fd: u8,
    /// The type and target of the redirection
    kind: ExpandedRedirectKind,
};

/// A fully expanded command ready for execution.
/// AST Command has words with $vars, globs, quotes.
/// ExpandedCommand has flat args strings ready for exec.
pub const ExpandedCommand = struct {
    args: []const []const u8,
    env: []const ast.Assignment,
    redirects: []const ExpandedRedirect,
};

// Re-export AST types needed by other modules
pub const CaptureMode = ast.CaptureMode;
pub const Capture = ast.Capture;

// =============================================================================
// Type Aliases (internal)
// =============================================================================

const Pipeline = ast.Pipeline;
const Command = ast.Command;
const Redirect = ast.Redirect;
const WordPart = token_types.WordPart;

pub const ExpandError = error{
    EmptyCommand,
    ExpansionError,
};

// =============================================================================
// Public API
// =============================================================================

/// Expand all commands in a pipeline, returning owned slice of ExpandedCommand.
pub fn expandPipeline(allocator: std.mem.Allocator, ctx: *expand.ExpandContext, pipeline: Pipeline) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)![]const ExpandedCommand {
    var cmd_expanded: std.ArrayListUnmanaged(ExpandedCommand) = .empty;

    for (pipeline.commands) |cmd| {
        const expanded_result = try expandCommand(allocator, ctx, cmd);
        try cmd_expanded.append(allocator, expanded_result);
    }

    return try cmd_expanded.toOwnedSlice(allocator);
}

// =============================================================================
// Internal Expansion Functions
// =============================================================================

fn expandCommand(allocator: std.mem.Allocator, ctx: *expand.ExpandContext, cmd: Command) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)!ExpandedCommand {
    var env_list: std.ArrayListUnmanaged(ast.Assignment) = .empty;

    const words_with_alias = try applyAliasExpansion(allocator, ctx, cmd.words);

    for (cmd.assignments) |assign| {
        const expanded_value = try expandAssignmentValue(allocator, ctx, assign.value);
        try env_list.append(allocator, .{ .key = assign.key, .value = expanded_value });
    }

    const expanded_argv = try expand.expandWords(ctx, words_with_alias);

    var redir_expanded: std.ArrayListUnmanaged(ExpandedRedirect) = .empty;
    for (cmd.redirects) |redir| {
        const redir_result = try expandRedirect(ctx, redir);
        try redir_expanded.append(allocator, redir_result);
    }

    return ExpandedCommand{
        .args = expanded_argv,
        .env = try env_list.toOwnedSlice(allocator),
        .redirects = try redir_expanded.toOwnedSlice(allocator),
    };
}

// =============================================================================
// Alias Expansion
// =============================================================================

/// Apply alias expansion to the first word of the command, if any.
/// Expands only once to avoid recursive alias loops.
fn applyAliasExpansion(allocator: std.mem.Allocator, ctx: *expand.ExpandContext, words: []const []const WordPart) ![]const []const WordPart {
    if (words.len == 0) return words;
    if (words[0].len == 0 or words[0][0].quotes != .none) return words;

    const alias_name = words[0][0].text;
    const alias_text = ctx.state.getAlias(alias_name) orelse return words;

    var lex = lexer_mod.Lexer.init(allocator, alias_text);
    const tokens = lex.tokenize() catch return words; // On lex error, leave unchanged

    var alias_words: std.ArrayListUnmanaged([]const WordPart) = .empty;
    defer alias_words.deinit(allocator);

    for (tokens) |tok| {
        if (tok.kind != .word) break;
        try alias_words.append(allocator, tok.kind.word);
    }

    if (alias_words.items.len == 0) return words;

    var combined: std.ArrayListUnmanaged([]const WordPart) = .empty;
    errdefer combined.deinit(allocator);

    try combined.appendSlice(allocator, alias_words.items);
    try combined.appendSlice(allocator, words[1..]);

    return try combined.toOwnedSlice(allocator);
}

// =============================================================================
// Redirect and Helper Functions
// =============================================================================

fn expandRedirect(ctx: *expand.ExpandContext, redirect: Redirect) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)!ExpandedRedirect {
    const expanded_kind: ExpandedRedirectKind = switch (redirect.kind) {
        .dup => |to_fd| .{ .dup = to_fd },
        .read => |parts| .{ .read = try expandPathParts(ctx, parts) },
        .write_truncate => |parts| .{ .write_truncate = try expandPathParts(ctx, parts) },
        .write_append => |parts| .{ .write_append = try expandPathParts(ctx, parts) },
    };

    return ExpandedRedirect{
        .from_fd = redirect.from_fd,
        .kind = expanded_kind,
    };
}

/// Expand word parts for a redirect path, returning the first expanded value.
/// Respects quoting: single-quoted stays literal, double-quoted expands vars, bare expands all.
fn expandPathParts(ctx: *expand.ExpandContext, parts: []const WordPart) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)![]const u8 {
    const expanded_values = try expand.expandWord(ctx, parts);
    if (expanded_values.len > 0) {
        return expanded_values[0];
    }
    return "";
}

fn expandAssignmentValue(allocator: std.mem.Allocator, ctx: *expand.ExpandContext, value: []const u8) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)![]const u8 {
    // Tokenize the assignment value and expand it
    var lexer = lexer_mod.Lexer.init(allocator, value);
    const tokens = lexer.tokenize() catch {
        // If tokenization fails, use the literal value
        return value;
    };

    if (tokens.len > 0 and tokens[0].kind == .word) {
        const expanded_values = try expand.expandWord(ctx, tokens[0].kind.word);
        return try joinValues(allocator, expanded_values);
    }

    // Fall back to literal value
    return value;
}

fn joinValues(allocator: std.mem.Allocator, values: []const []const u8) std.mem.Allocator.Error![]const u8 {
    if (values.len == 0) return "";
    if (values.len == 1) return values[0];

    var total_len: usize = 0;
    for (values) |v| {
        total_len += v.len;
    }
    total_len += values.len - 1;

    const result = try allocator.alloc(u8, total_len);
    var pos: usize = 0;

    for (values, 0..) |v, i| {
        @memcpy(result[pos .. pos + v.len], v);
        pos += v.len;
        if (i < values.len - 1) {
            result[pos] = ' ';
            pos += 1;
        }
    }

    return result;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;
const parser = @import("../../language/parser.zig");

/// Test helpers for pipeline expansion tests.
const TestHelper = struct {
    /// Parse input and expand the first command in the pipeline.
    fn expandFirst(arena: *std.heap.ArenaAllocator, state: *State, input: []const u8) !ExpandedCommand {
        const cmds = try expandAll(arena, state, input);
        return cmds[0];
    }

    /// Parse input and expand all commands in the first pipeline.
    fn expandAll(arena: *std.heap.ArenaAllocator, state: *State, input: []const u8) ![]const ExpandedCommand {
        const alloc = arena.allocator();
        const prog = try parse(alloc, input);
        const pl = prog.statements[0].command.chains[0].pipeline;
        var ctx = expand.ExpandContext.init(alloc, state);
        defer ctx.deinit();
        return try expandPipeline(alloc, &ctx, pl);
    }

    /// Parse input without expansion.
    fn parse(alloc: std.mem.Allocator, input: []const u8) !ast.Program {
        var lex = lexer_mod.Lexer.init(alloc, input);
        const tokens = try lex.tokenize();
        var p = parser.Parser.init(alloc, tokens);
        return try p.parse();
    }
};

test "simple command" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    const cmd = try TestHelper.expandFirst(&arena, &state, "echo hello world");

    try testing.expectEqual(@as(usize, 3), cmd.args.len);
    try testing.expectEqualStrings("echo", cmd.args[0]);
    try testing.expectEqualStrings("hello", cmd.args[1]);
    try testing.expectEqualStrings("world", cmd.args[2]);
}

test "pipeline: multiple commands" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    const cmds = try TestHelper.expandAll(&arena, &state, "cat file | grep foo");

    try testing.expectEqual(@as(usize, 2), cmds.len);
    try testing.expectEqualStrings("cat", cmds[0].args[0]);
    try testing.expectEqualStrings("grep", cmds[1].args[0]);
}

test "variable expansion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    try state.setVar("name", "world");
    const cmd = try TestHelper.expandFirst(&arena, &state, "echo $name");

    try testing.expectEqual(@as(usize, 2), cmd.args.len);
    try testing.expectEqualStrings("echo", cmd.args[0]);
    try testing.expectEqualStrings("world", cmd.args[1]);
}

test "env prefix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    const cmd = try TestHelper.expandFirst(&arena, &state, "FOO=bar env");

    try testing.expectEqual(@as(usize, 1), cmd.env.len);
    try testing.expectEqualStrings("FOO", cmd.env[0].key);
    try testing.expectEqualStrings("bar", cmd.env[0].value);
}

test "capture preserved" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const prog = try TestHelper.parse(arena.allocator(), "whoami => user");

    try testing.expectEqualStrings("user", prog.statements[0].command.capture.?.variable);
    try testing.expectEqual(ast.CaptureMode.string, prog.statements[0].command.capture.?.mode);
}

test "background preserved" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const prog = try TestHelper.parse(arena.allocator(), "sleep 10 &");

    try testing.expectEqual(true, prog.statements[0].command.background);
}

test "redirect: variable expansion in path" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    try state.setVar("outfile", "output.txt");
    const cmd = try TestHelper.expandFirst(&arena, &state, "echo test > $outfile");

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    try testing.expectEqualStrings("output.txt", cmd.redirects[0].kind.write_truncate);
}

test "redirect: quoted path with spaces" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    const cmd = try TestHelper.expandFirst(&arena, &state, "echo test > \"foo bar.txt\"");

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    try testing.expectEqualStrings("foo bar.txt", cmd.redirects[0].kind.write_truncate);
}

test "redirect: single quotes prevent expansion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    try state.setVar("var", "should_not_appear");
    const cmd = try TestHelper.expandFirst(&arena, &state, "echo test > '$var.txt'");

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    try testing.expectEqualStrings("$var.txt", cmd.redirects[0].kind.write_truncate);
}

test "redirect: double quotes expand variables" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    try state.setVar("name", "myfile");
    const cmd = try TestHelper.expandFirst(&arena, &state, "echo test > \"$name.txt\"");

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    try testing.expectEqualStrings("myfile.txt", cmd.redirects[0].kind.write_truncate);
}
