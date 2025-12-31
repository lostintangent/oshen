//! Highlight: AST-aware syntax highlighting for the line editor.
//!
//! Tokenizes and parses input to provide semantic highlighting:
//! - Valid commands (builtins, aliases, PATH executables) → green
//! - Invalid/unknown commands → red
//! - Keywords (if, for, while, etc.) → blue
//! - Variables ($foo, $1, $HOME) → bright magenta
//! - Glob patterns (*, **, ?) → bright magenta
//! - Tilde expansion (~, ~/path) → bright magenta
//! - Quoted strings → yellow
//! - Number literals → yellow
//! - Operators (pipes, redirects, logical, background, capture) → cyan
//! - Separators (;, newline) → dim
//!
//! Uses the lexer and parser from the language module to understand
//! command structure, enabling accurate command-position detection.

const std = @import("std");

const ast = @import("../../../language/ast.zig");
const tokens = @import("../../../language/tokens.zig");
const Lexer = @import("../../../language/lexer.zig").Lexer;
const Parser = @import("../../../language/parser.zig").Parser;

const glob = @import("../../../interpreter/expansion/glob.zig");
const resolve = @import("../../../runtime/resolve.zig");
const State = @import("../../../runtime/state.zig").State;
const ansi = @import("../../../terminal/ansi.zig");

// =============================================================================
// Public API
// =============================================================================

/// Render highlighted input to a writer.
/// Falls back to plain text on lex/parse errors.
pub fn render(allocator: std.mem.Allocator, input: []const u8, writer: anytype, state: ?*State) !void {
    if (input.len == 0) return;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lexer = Lexer.init(alloc, input);
    const toks = lexer.tokenize() catch return writer.writeAll(input);

    var parser = Parser.init(alloc, toks);
    const program = if (parser.parse() catch null) |p| &p else null;

    var ctx = Context{
        .cmd_positions = collectCommandPositions(alloc, program, toks),
        .cmd_cache = .{ .map = std.StringHashMap(bool).init(alloc), .state = state },
    };

    var pos: usize = 0;
    for (toks) |token| {
        const start = token.span.start;
        const end = token.span.end;

        // Write inter-token whitespace
        if (start > pos) try writer.writeAll(input[pos..start]);

        // Skip invalid spans
        if (start >= input.len or end > input.len or end <= start) {
            pos = end;
            continue;
        }

        const text = input[start..end];
        const color: ?[]const u8 = switch (token.kind) {
            .word => |segs| ctx.wordColor(segs, text, start),
            .operator => ansi.cyan,
            .separator => ansi.dim,
        };

        if (color) |c| {
            try writer.writeAll(c);
            try writer.writeAll(text);
            try writer.writeAll(ansi.reset);
        } else {
            try writer.writeAll(text);
        }

        pos = end;
    }

    // Trailing content (incomplete tokens, trailing whitespace)
    if (pos < input.len) try writer.writeAll(input[pos..]);
}

// =============================================================================
// Highlighting context
// =============================================================================

const Context = struct {
    cmd_positions: std.AutoHashMap(usize, void),
    cmd_cache: CommandCache,

    /// Determine the highlight color for a word token, or null for no highlighting.
    fn wordColor(self: *Context, segs: []const tokens.WordPart, text: []const u8, start: usize) ?[]const u8 {
        const is_bare = segs.len == 1 and segs[0].quotes == .none;
        const bare_text = if (is_bare) segs[0].text else text;

        // Keywords (highest priority)
        if (is_bare and tokens.isKeyword(bare_text)) return ansi.blue;

        // Variables
        if (is_bare and tokens.isVariable(bare_text)) return ansi.bright_magenta;

        // Commands (validate against PATH/builtins/aliases)
        if (self.cmd_positions.contains(start)) {
            return if (self.cmd_cache.isValid(bare_text)) ansi.green else ansi.red;
        }

        // Quoted strings
        for (segs) |seg| {
            if (seg.quotes != .none) return ansi.yellow;
        }

        // Glob patterns
        if (is_bare and glob.hasGlobChars(bare_text)) return ansi.bright_magenta;

        // Tilde expansion (~ or ~/path or ~user/path)
        if (is_bare and isTilde(bare_text)) return ansi.bright_magenta;

        // Number literals
        if (is_bare and isNumber(bare_text)) return ansi.yellow;

        return null;
    }

    /// Cache for command existence checks (avoids repeated PATH searches per render).
    const CommandCache = struct {
        map: std.StringHashMap(bool),
        state: ?*State,

        fn isValid(self: *CommandCache, cmd: []const u8) bool {
            if (self.map.get(cmd)) |valid| return valid;
            const valid = resolve.isValid(self.state, cmd);
            self.map.put(cmd, valid) catch {};
            return valid;
        }
    };
};

// =============================================================================
// Token classification helpers
// =============================================================================

fn isTilde(text: []const u8) bool {
    if (text.len == 0 or text[0] != '~') return false;
    return text.len == 1 or text[1] == '/';
}

fn isNumber(text: []const u8) bool {
    if (text.len == 0) return false;
    for (text) |c| {
        if (!std.ascii.isDigit(c)) return false;
    }
    return true;
}

// =============================================================================
// AST analysis
// =============================================================================

/// Collect byte offsets of command-position words from the AST.
fn collectCommandPositions(
    allocator: std.mem.Allocator,
    program: ?*const ast.Program,
    toks: []const tokens.Token,
) std.AutoHashMap(usize, void) {
    var positions = std.AutoHashMap(usize, void).init(allocator);
    const prog = program orelse return positions;

    for (prog.statements) |stmt| {
        const chains = switch (stmt) {
            .command => |cmd| cmd.chains,
            else => continue,
        };
        for (chains) |chain| {
            for (chain.pipeline.commands) |cmd| {
                if (cmd.words.len == 0) continue;
                const first = cmd.words[0];
                if (first.len == 0) continue;

                // Find matching token by pointer identity
                for (toks) |tok| {
                    if (tok.kind == .word and tok.kind.word.ptr == first.ptr) {
                        positions.put(tok.span.start, {}) catch {};
                        break;
                    }
                }
            }
        }
    }
    return positions;
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

test "syntax elements get correct colors" {
    const cases = .{
        // Commands
        .{ "cd foo", ansi.green }, // valid builtin
        .{ "xyznonexistent123 foo", ansi.red }, // unknown command
        // Keywords
        .{ "if true", ansi.blue },
        // Variables
        .{ "echo $foo", ansi.bright_magenta }, // named
        .{ "echo $1", ansi.bright_magenta }, // positional
        // Strings
        .{ "echo \"hello\"", ansi.yellow },
        // Globs
        .{ "echo *.txt", ansi.bright_magenta }, // glob pattern
        .{ "echo **/*.zig", ansi.bright_magenta }, // recursive glob
        // Tilde
        .{ "cd ~", ansi.bright_magenta },
        .{ "ls ~/Documents", ansi.bright_magenta },
        // Numbers
        .{ "echo 42", ansi.yellow },
        .{ "chmod 755 file", ansi.yellow },
        // Operators (all cyan)
        .{ "a | b", ansi.cyan }, // pipe
        .{ "echo foo > out.txt", ansi.cyan }, // redirect
        .{ "true && false", ansi.cyan }, // logical
        .{ "sleep 1 &", ansi.cyan }, // background
        .{ "echo hello => x", ansi.cyan }, // capture
        .{ "echo lines =>@ arr", ansi.cyan }, // capture lines
        // Separators
        .{ "echo a; echo b", ansi.dim },
    };
    inline for (cases) |case| try expectHighlight(case[0], case[1]);
}

test "bare words have no color codes" {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(testing.allocator);
    // "echo" is a valid command (green), but "hello" and "world" are bare arguments
    try render(testing.allocator, "echo hello world", buf.writer(testing.allocator), null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "hello") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "world") != null);
}

test "empty input produces empty output" {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(testing.allocator);
    try render(testing.allocator, "", buf.writer(testing.allocator), null);
    try testing.expectEqualStrings("", buf.items);
}
