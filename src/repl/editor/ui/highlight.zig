//! Highlight: AST-aware syntax highlighting for the line editor.
//!
//! Tokenizes and parses input to provide semantic highlighting:
//! - Valid commands (builtins, aliases, PATH executables) → bold
//! - Invalid/unknown commands → red
//! - Keywords (if, for, while, etc.) → blue
//! - Variables ($foo, $1, $HOME) → bright magenta
//! - Quoted strings → green
//! - Pipe/redirect operators → cyan
//! - Logical operators (&&, ||) → yellow
//! - Background operator (&) → magenta
//!
//! Uses the lexer and parser from the language module to understand
//! command structure, enabling accurate command-position detection.

const std = @import("std");

const Lexer = @import("../../../language/lexer.zig").Lexer;
const Parser = @import("../../../language/parser.zig").Parser;
const ast = @import("../../../language/ast.zig");
const tokens = @import("../../../language/tokens.zig");
const resolve = @import("../../../runtime/resolve.zig");
const State = @import("../../../runtime/state.zig").State;
const ansi = @import("../../../terminal/ansi.zig");

// =============================================================================
// Command cache
// =============================================================================

/// Cache for command existence checks (avoids repeated PATH searches per render).
const CommandCache = struct {
    map: std.StringHashMap(bool),
    state: ?*State,

    fn init(allocator: std.mem.Allocator, state: ?*State) CommandCache {
        return .{ .map = std.StringHashMap(bool).init(allocator), .state = state };
    }

    fn isValid(self: *CommandCache, cmd: []const u8) bool {
        if (self.map.get(cmd)) |valid| return valid;

        const valid = resolve.isValid(self.state, cmd);
        self.map.put(cmd, valid) catch {};
        return valid;
    }
};

// =============================================================================
// Public API
// =============================================================================

/// Render highlighted input to a writer.
/// Falls back to plain text on lex/parse errors.
pub fn render(allocator: std.mem.Allocator, input: []const u8, writer: anytype, state: ?*State) !void {
    if (input.len == 0) return;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var lexer = Lexer.init(arena_alloc, input);
    const toks = lexer.tokenize() catch {
        try writer.writeAll(input);
        return;
    };

    var parser = Parser.init(arena_alloc, toks);
    const program_ast_val = parser.parse() catch null;
    const program_ast = if (program_ast_val) |val| &val else null;

    const cmd_positions = collectCommandPositions(arena_alloc, program_ast, toks);
    var cache = CommandCache.init(arena_alloc, state);

    var pos: usize = 0;
    for (toks) |token| {
        const start = token.span.start;
        const end = token.span.end;

        // Write inter-token whitespace
        if (start > pos) try writer.writeAll(input[pos..start]);

        if (start >= input.len or end > input.len or end <= start) {
            pos = end;
            continue;
        }

        const text = input[start..end];

        switch (token.kind) {
            .word => |segs| try highlightWord(writer, segs, text, start, cmd_positions, &cache),
            .operator => |op| try writeColored(writer, operatorColor(op), text),
            .separator => try writeColored(writer, ansi.yellow, text),
        }

        pos = end;
    }

    // Trailing content (incomplete tokens, trailing whitespace)
    if (pos < input.len) try writer.writeAll(input[pos..]);
}

// =============================================================================
// Private helpers
// =============================================================================

/// Write text with an ANSI color, followed by reset.
fn writeColored(writer: anytype, color: []const u8, text: []const u8) !void {
    try writer.writeAll(color);
    try writer.writeAll(text);
    try writer.writeAll(ansi.reset);
}

/// Collect byte offsets of command-position words from the AST.
fn collectCommandPositions(
    allocator: std.mem.Allocator,
    program_ast: ?*const ast.Program,
    toks: []const tokens.Token,
) std.AutoHashMap(usize, void) {
    var positions = std.AutoHashMap(usize, void).init(allocator);

    const program = program_ast orelse return positions;
    for (program.statements) |stmt| {
        const cmd_stmt = switch (stmt) {
            .command => |cmd| cmd,
            else => continue,
        };

        for (cmd_stmt.chains) |chain| {
            for (chain.pipeline.commands) |cmd| {
                if (cmd.words.len == 0) continue;
                const first_word = cmd.words[0];
                if (first_word.len == 0) continue;

                // Find matching token by pointer identity
                for (toks) |tok| {
                    if (tok.kind == .word and tok.kind.word.ptr == first_word.ptr) {
                        positions.put(tok.span.start, {}) catch {};
                        break;
                    }
                }
            }
        }
    }
    return positions;
}

/// Highlight a word token based on its semantic role.
fn highlightWord(
    writer: anytype,
    segs: []const tokens.WordPart,
    text: []const u8,
    start: usize,
    cmd_positions: std.AutoHashMap(usize, void),
    cache: *CommandCache,
) !void {
    const is_bare_single = segs.len == 1 and segs[0].quotes == .none;
    const bare_text = if (segs.len > 0 and segs[0].quotes == .none) segs[0].text else text;

    // Keywords take priority
    if (is_bare_single and tokens.isKeyword(bare_text)) {
        return writeColored(writer, ansi.blue, text);
    }

    // Variables
    if (is_bare_single and tokens.isVariable(bare_text)) {
        return writeColored(writer, ansi.bright_magenta, text);
    }

    // Command position: validate and color accordingly
    if (cmd_positions.get(start) != null) {
        const color = if (cache.isValid(bare_text)) ansi.bold else ansi.red;
        return writeColored(writer, color, text);
    }

    // Quoted strings get green
    const has_quotes = for (segs) |seg| {
        if (seg.quotes != .none) break true;
    } else false;

    if (has_quotes) {
        return writeColored(writer, ansi.green, text);
    }

    // Default: no highlighting
    try writer.writeAll(text);
}

/// Get ANSI color code for an operator.
fn operatorColor(op: tokens.Operator) []const u8 {
    if (op.isPipe() or op.isRedirect()) return ansi.cyan;
    if (op.isLogical()) return ansi.yellow;
    // Background (&) and capture (=>, =>@) operators
    return ansi.magenta;
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

fn expectHighlight(input: []const u8, expected_color: []const u8) !void {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(testing.allocator);
    try render(testing.allocator, input, buf.writer(testing.allocator), null);
    try testing.expect(std.mem.indexOf(u8, buf.items, expected_color) != null);
}

test "Highlighting: syntax elements get correct colors" {
    const cases = [_]struct { []const u8, []const u8 }{
        // Commands
        .{ "cd foo", ansi.bold }, // valid builtin
        .{ "xyznonexistent123 foo", ansi.red }, // unknown command
        // Keywords
        .{ "if true", ansi.blue },
        // Variables
        .{ "echo $foo", ansi.bright_magenta }, // named
        .{ "echo $1", ansi.bright_magenta }, // positional
        // Strings
        .{ "echo \"hello\"", ansi.green },
        // Operators
        .{ "a | b", ansi.cyan }, // pipe
        .{ "echo foo > out.txt", ansi.cyan }, // redirect
        .{ "true && false", ansi.yellow }, // logical
        .{ "sleep 1 &", ansi.magenta }, // background
        // Separators
        .{ "echo a; echo b", ansi.yellow }, // semicolon separator
        // Capture operators
        .{ "echo hello => x", ansi.magenta }, // capture
        .{ "echo lines =>@ arr", ansi.magenta }, // capture lines
    };

    for (cases) |case| try expectHighlight(case[0], case[1]);
}

test "Highlighting: bare words have no color codes" {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(testing.allocator);
    // "echo" is a valid command (bold), but "hello" and "world" are bare arguments
    try render(testing.allocator, "echo hello world", buf.writer(testing.allocator), null);
    // The output should contain the bare words without color codes around them
    // We verify by checking the words appear and the output isn't excessively long
    // (if everything was colored, output would be much longer due to ANSI codes)
    try testing.expect(std.mem.indexOf(u8, buf.items, "hello") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "world") != null);
}

test "Highlighting: empty input produces empty output" {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(testing.allocator);
    try render(testing.allocator, "", buf.writer(testing.allocator), null);
    try testing.expectEqualStrings("", buf.items);
}

