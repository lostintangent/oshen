//! AST expansion: transforms parsed statements into expanded form ready for execution.
//!
//! This module bridges the parser and executor by expanding AST nodes:
//! - Command arguments are expanded (variables, globs, command substitution)
//! - Aliases are resolved
//! - Redirections are evaluated
//! - Control flow statements pass through unchanged (expanded at execution time)

const std = @import("std");
const ast = @import("../../language/ast.zig");
const expansion_types = @import("expanded.zig");
const token_types = @import("../../language/tokens.zig");
const expand = @import("word.zig");
const lexer_mod = @import("../../language/lexer.zig");
const State = @import("../../runtime/state.zig").State;

// =============================================================================
// Type Aliases
// =============================================================================

const Program = ast.Program;
const Stmt = ast.Statement;
const ChainItem = ast.ChainItem;
const Pipeline = ast.Pipeline;
const Command = ast.Command;
const Redirect = ast.Redirect;
const WordPart = token_types.WordPart;
const CaptureMode = ast.CaptureMode;

const ExpandedCmd = expansion_types.ExpandedCmd;
const ExpandedRedirect = expansion_types.ExpandedRedirect;
const ExpandedRedirectKind = expansion_types.ExpandedRedirectKind;
const Capture = expansion_types.Capture;

pub const ExpandError = error{
    EmptyCommand,
    ExpansionError,
};

// =============================================================================
// Public API
// =============================================================================

/// Expand all commands in a pipeline, returning owned slice of ExpandedCmd
pub fn expandPipeline(allocator: std.mem.Allocator, ctx: *expand.ExpandContext, pipeline: Pipeline) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)![]const ExpandedCmd {
    var cmd_expanded: std.ArrayListUnmanaged(ExpandedCmd) = .empty;

    for (pipeline.commands) |cmd| {
        const expanded_result = try expandCommand(allocator, ctx, cmd);
        try cmd_expanded.append(allocator, expanded_result);
    }

    return try cmd_expanded.toOwnedSlice(allocator);
}

// =============================================================================
// Internal Expansion Functions
// =============================================================================

fn expandCommand(allocator: std.mem.Allocator, ctx: *expand.ExpandContext, cmd: Command) (ExpandError || expand.ExpandError || std.mem.Allocator.Error)!ExpandedCmd {
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

    return ExpandedCmd{
        .argv = expanded_argv,
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

fn expandInput(allocator: std.mem.Allocator, input: []const u8) !Program {
    var lex = lexer_mod.Lexer.init(allocator, input);
    const tokens = try lex.tokenize();
    var p = parser.Parser.init(allocator, tokens);
    return try p.parse();
}

test "simple command" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    const prog = try expandInput(arena.allocator(), "echo hello world");

    try testing.expectEqual(@as(usize, 1), prog.statements.len);
    // Expand the pipeline at execution time
    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd_expanded = expanded_cmds[0];
    try testing.expectEqual(@as(usize, 3), cmd_expanded.argv.len);
    try testing.expectEqualStrings("echo", cmd_expanded.argv[0]);
    try testing.expectEqualStrings("hello", cmd_expanded.argv[1]);
    try testing.expectEqualStrings("world", cmd_expanded.argv[2]);
}

test "pipeline normalizes |> to |" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const prog = try expandInput(arena.allocator(), "cat file |> grep foo");

    const pipeline = prog.statements[0].command.chains[0].pipeline;
    try testing.expectEqual(@as(usize, 2), pipeline.commands.len);
}

test "with variable expansion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    const name_values = [_][]const u8{"world"};
    try ctx.setVar("name", &name_values);

    const prog = try expandInput(arena.allocator(), "echo $name");

    // Expand the pipeline at execution time
    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd_expanded = expanded_cmds[0];
    try testing.expectEqual(@as(usize, 2), cmd_expanded.argv.len);
    try testing.expectEqualStrings("echo", cmd_expanded.argv[0]);
    try testing.expectEqualStrings("world", cmd_expanded.argv[1]);
}

test "with env prefix" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    const prog = try expandInput(arena.allocator(), "FOO=bar env");

    // Expand the pipeline at execution time
    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd_expanded = expanded_cmds[0];
    try testing.expectEqual(@as(usize, 1), cmd_expanded.env.len);
    try testing.expectEqualStrings("FOO", cmd_expanded.env[0].key);
    try testing.expectEqualStrings("bar", cmd_expanded.env[0].value);
}

test "capture preserved" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const prog = try expandInput(arena.allocator(), "whoami => user");

    try testing.expectEqualStrings("user", prog.statements[0].command.capture.?.variable);
    try testing.expectEqual(ast.CaptureMode.string, prog.statements[0].command.capture.?.mode);
}

test "background preserved" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const prog = try expandInput(arena.allocator(), "sleep 10 &");

    try testing.expectEqual(true, prog.statements[0].command.background);
}

test "redirect: variable expansion in path" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    const outfile_values = [_][]const u8{"output.txt"};
    try ctx.setVar("outfile", &outfile_values);

    const prog = try expandInput(arena.allocator(), "echo test > $outfile");

    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd = expanded_cmds[0];

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    switch (cmd.redirects[0].kind) {
        .write_truncate => |path| {
            try testing.expectEqualStrings("output.txt", path);
        },
        else => return error.TestExpectedEqual,
    }
}

test "redirect: double-quoted path with spaces" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    const prog = try expandInput(arena.allocator(), "echo test > \"foo bar.txt\"");

    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd = expanded_cmds[0];

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    switch (cmd.redirects[0].kind) {
        .write_truncate => |path| {
            try testing.expectEqualStrings("foo bar.txt", path);
        },
        else => return error.TestExpectedEqual,
    }
}

test "redirect: single-quoted path prevents expansion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    // Set a variable that should NOT be expanded due to single quotes
    const var_values = [_][]const u8{"should_not_appear"};
    try ctx.setVar("var", &var_values);

    const prog = try expandInput(arena.allocator(), "echo test > '$var.txt'");

    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd = expanded_cmds[0];

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    switch (cmd.redirects[0].kind) {
        .write_truncate => |path| {
            // Single quotes should preserve the literal $var.txt
            try testing.expectEqualStrings("$var.txt", path);
        },
        else => return error.TestExpectedEqual,
    }
}

test "redirect: double-quoted path expands variables" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();
    var ctx = expand.ExpandContext.init(arena.allocator(), &state);
    defer ctx.deinit();

    const name_values = [_][]const u8{"myfile"};
    try ctx.setVar("name", &name_values);

    const prog = try expandInput(arena.allocator(), "echo test > \"$name.txt\"");

    const ast_pipeline = prog.statements[0].command.chains[0].pipeline;
    const expanded_cmds = try expandPipeline(arena.allocator(), &ctx, ast_pipeline);
    const cmd = expanded_cmds[0];

    try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
    switch (cmd.redirects[0].kind) {
        .write_truncate => |path| {
            try testing.expectEqualStrings("myfile.txt", path);
        },
        else => return error.TestExpectedEqual,
    }
}
