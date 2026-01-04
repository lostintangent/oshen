//! Parser: transforms a token stream into an abstract syntax tree (AST).
//!
//! The parser recognizes Oshen's grammar:
//! - Commands with arguments, redirections, and environment prefixes
//! - Pipelines (`cmd1 | cmd2`) and logical chains (`&&`, `||`, `and`, `or`)
//! - Control flow: `if`/`else`, `for`/`in`, `each`, `while`, `break`, `continue`
//! - Function definitions (`fun name ... end`)
//! - Output capture (`=> var`, `=>@ var`)
//! - Background execution (`&`)
//!
//! Control flow bodies are stored as source slices (not recursively parsed)
//! and parsed on-demand during execution, enabling loop body caching.

const std = @import("std");
const token_types = @import("tokens.zig");
const ast = @import("ast.zig");

const Token = token_types.Token;
const TokenSpan = token_types.TokenSpan;
const WordPart = token_types.WordPart;
const QuoteKind = token_types.QuoteKind;
const Operator = token_types.Operator;
const Program = ast.Program;
const Statement = ast.Statement;
const CommandStatement = ast.CommandStatement;
const FunctionDefinition = ast.FunctionDefinition;
const IfStatement = ast.IfStatement;
const IfBranch = ast.IfBranch;
const EachStatement = ast.EachStatement;
const WhileStatement = ast.WhileStatement;
const ChainItem = ast.ChainItem;
const Pipeline = ast.Pipeline;
const Command = ast.Command;
const RedirectKind = ast.RedirectKind;
const ChainOperator = ast.ChainOperator;
const Assignment = ast.Assignment;
const Redirect = ast.Redirect;
const Capture = ast.Capture;
const CaptureMode = ast.CaptureMode;

pub const ParseError = error{
    InvalidFunctionName,
    InvalidEach,
    InvalidCapture,
    UnterminatedFunction,
    UnterminatedIf,
    UnterminatedEach,
    UnterminatedWhile,
    MissingSeparator,
    MissingCondition,
    MissingBody,
    ConditionWithBackground,
    ConditionWithCapture,
    CaptureWithBackground,
    UnexpectedEOF,
    OutOfMemory,
};

pub const Parser = struct {
    tokens: []const Token,
    pos: usize,
    allocator: std.mem.Allocator,

    /// Original input source (needed for capturing function bodies)
    input: []const u8,

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return initWithInput(allocator, tokens, "");
    }

    pub fn initWithInput(allocator: std.mem.Allocator, tokens: []const Token, input: []const u8) Parser {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
            .input = input,
        };
    }

    /// Get the byte position for error reporting.
    pub fn errorPos(self: *const Parser) usize {
        if (self.pos < self.tokens.len) {
            return self.tokens[self.pos].span.start;
        } else if (self.tokens.len > 0) {
            return self.tokens[self.tokens.len - 1].span.end;
        }
        return 0;
    }

    // =========================================================================
    // Token navigation
    // =========================================================================

    /// Returns the current token without advancing, or null if at end.
    inline fn peek(self: *const Parser) ?Token {
        return if (self.pos < self.tokens.len) self.tokens[self.pos] else null;
    }

    /// Advances position by one token and returns the consumed token.
    inline fn advance(self: *Parser) ?Token {
        if (self.pos >= self.tokens.len) return null;
        defer self.pos += 1;
        return self.tokens[self.pos];
    }

    fn skipSeparators(self: *Parser) void {
        while (self.isSeparator()) {
            _ = self.advance();
        }
    }

    // =========================================================================
    // Token type predicates
    // =========================================================================

    /// Returns the current operator if the token is an operator, null otherwise.
    fn peekOperator(self: *const Parser) ?Operator {
        const tok = self.peek() orelse return null;
        return if (tok.kind == .operator) tok.kind.operator else null;
    }

    /// Returns true if current token is the specified operator.
    fn isOperator(self: *const Parser, op: Operator) bool {
        return self.peekOperator() == op;
    }

    /// Returns true if current token is an unquoted bare word matching the given text.
    /// Used for checking keywords that can appear in command position (if, for, while, etc.).
    /// Note: Keywords are lexed as words, not operators, so this only checks words.
    fn isKeyword(self: *const Parser, keyword: []const u8) bool {
        const text = self.peekBareWord() orelse return false;
        return std.mem.eql(u8, text, keyword);
    }

    /// Returns the text of the current token if it's a simple unquoted bare word.
    /// This is the fast path for keyword checking - extract once, compare many.
    fn peekBareWord(self: *const Parser) ?[]const u8 {
        const tok = self.peek() orelse return null;
        if (tok.kind != .word) return null;
        return token_types.getBareText(tok.kind.word);
    }

    /// Returns true if current token is a separator (newline or semicolon).
    fn isSeparator(self: *const Parser) bool {
        const tok = self.peek() orelse return false;
        return tok.kind == .separator;
    }

    /// Returns true if the previous token is a separator (or we're at start of input).
    /// Used to check if current position is in "command position" (start of a statement).
    fn isPrevTokenSeparator(self: *const Parser) bool {
        if (self.pos == 0) return true;
        return self.tokens[self.pos - 1].kind == .separator;
    }

    /// Returns true if current token is a word.
    fn isWord(self: *const Parser) bool {
        const tok = self.peek() orelse return false;
        return tok.kind == .word;
    }

    /// Returns true if current word token is a logical keyword (and, or).
    fn isLogicalKeyword(self: *const Parser) bool {
        const text = self.peekBareWord() orelse return false;
        return token_types.isLogicalKeyword(text);
    }

    /// Returns true if current token starts a block (if, for, each, while, fun).
    fn isBlockStart(self: *const Parser) bool {
        const text = self.peekBareWord() orelse return false;
        return token_types.isBlockKeyword(text);
    }

    /// Consumes 'end' keyword and validates that a separator or EOF follows.
    fn consumeEnd(self: *Parser) ParseError!void {
        _ = self.advance(); // consume 'end'
        if (self.pos < self.tokens.len and !self.isSeparator()) {
            return ParseError.MissingSeparator;
        }
    }

    // =========================================================================
    // Word analysis helpers
    // =========================================================================

    /// Extracts a simple unquoted word's text from a token.
    /// Returns null if the token is not a single unquoted word part.
    fn extractSimpleWord(tok: Token) ?[]const u8 {
        return token_types.getBareText(tok.kind.word);
    }

    /// Result of parsing an environment variable assignment.
    const EnvAssignment = struct { key: []const u8, value: []const u8 };

    /// Attempts to parse a word as an environment variable assignment (KEY=value).
    /// Returns the key-value pair if valid, null otherwise.
    fn tryParseEnvAssignment(word_parts: []const WordPart) ?EnvAssignment {
        // Only unquoted single-part words can be assignments
        if (word_parts.len != 1 or word_parts[0].quotes != .none) return null;

        const text = word_parts[0].text;
        const eq_pos = std.mem.indexOf(u8, text, "=") orelse return null;
        if (eq_pos == 0) return null; // Empty key

        const key = text[0..eq_pos];
        if (!token_types.isValidIdentifier(key)) return null;

        return .{ .key = key, .value = text[eq_pos + 1 ..] };
    }

    /// Attempts to parse a logical operator (&&, ||, and, or) at current position.
    /// Returns the operator type if found, null otherwise.
    fn tryParseLogicalOp(self: *const Parser) ?ChainOperator {
        // Check for operator tokens (&&, ||)
        if (self.peekOperator()) |op| {
            return switch (op) {
                .@"and" => .@"and",
                .@"or" => .@"or",
                else => null,
            };
        }
        // Check for keyword tokens (and, or)
        if (self.isKeyword("and")) return .@"and";
        if (self.isKeyword("or")) return .@"or";
        return null;
    }

    // =========================================================================
    // Redirect parsing
    // =========================================================================

    /// Parses a redirect operator and its target. Returns null if no redirect present.
    fn parseRedirect(self: *Parser) ParseError!?Redirect {
        const op = self.peekOperator() orelse return null;
        if (!op.isRedirect()) return null;

        _ = self.advance();

        // Determine source file descriptor
        const fd: u8 = switch (op) {
            .redirect_stdin => 0,
            .redirect_stderr, .redirect_stderr_append, .redirect_stderr_to_stdout => 2,
            .redirect_stdout, .redirect_stdout_append, .redirect_both => 1,
            else => unreachable,
        };

        // Handle fd duplication (2>&1)
        if (op == .redirect_stderr_to_stdout) {
            return Redirect{ .from_fd = fd, .kind = .{ .dup = 1 } };
        }

        // All other redirects need a target word
        if (!self.isWord()) return ParseError.UnexpectedEOF;
        const word_tok = self.advance().?;
        const parts = word_tok.kind.word;

        // Preserve word parts for proper expansion (respects quoting)
        const kind: RedirectKind = switch (op) {
            .redirect_stdin => .{ .read = parts },
            .redirect_stdout_append, .redirect_stderr_append => .{ .write_append = parts },
            .redirect_stdout, .redirect_stderr, .redirect_both => .{ .write_truncate = parts },
            else => unreachable,
        };

        return Redirect{ .from_fd = fd, .kind = kind };
    }

    // =========================================================================
    // Command parsing
    // =========================================================================

    /// Parses leading environment variable assignments (KEY=value format).
    fn parseAssignments(self: *Parser, assignments: *std.ArrayListUnmanaged(Assignment)) ParseError!void {
        while (self.isWord() and !self.isLogicalKeyword()) {
            const tok = self.peek().?;
            const parsed = tryParseEnvAssignment(tok.kind.word) orelse break;

            _ = self.advance();
            try assignments.append(self.allocator, .{ .key = parsed.key, .value = parsed.value });
        }
    }

    /// Parses command words and redirections.
    fn parseWordsAndRedirects(
        self: *Parser,
        words: *std.ArrayListUnmanaged([]const WordPart),
        redirects: *std.ArrayListUnmanaged(Redirect),
    ) ParseError!void {
        while (true) {
            if (try self.parseRedirect()) |redirect| {
                try redirects.append(self.allocator, redirect);
                continue;
            }

            // Stop at logical keywords (and, or) - they belong to parseLogical
            if (self.isWord() and !self.isLogicalKeyword()) {
                const tok = self.advance().?;
                try words.append(self.allocator, tok.kind.word);
                continue;
            }

            break;
        }
    }

    /// Parses a single command with optional assignments, words, and redirects.
    fn parseCommand(self: *Parser) ParseError!?Command {
        var assignments: std.ArrayListUnmanaged(Assignment) = .empty;
        var words: std.ArrayListUnmanaged([]const WordPart) = .empty;
        var redirects: std.ArrayListUnmanaged(Redirect) = .empty;

        try self.parseAssignments(&assignments);
        try self.parseWordsAndRedirects(&words, &redirects);

        if (words.items.len == 0 and assignments.items.len == 0) {
            return null;
        }

        return Command{
            .assignments = try assignments.toOwnedSlice(self.allocator),
            .words = try words.toOwnedSlice(self.allocator),
            .redirects = try redirects.toOwnedSlice(self.allocator),
        };
    }

    // =========================================================================
    // Pipeline and logical chain parsing
    // =========================================================================

    /// Parses a pipeline of commands connected by `|`.
    fn parsePipeline(self: *Parser) ParseError!?Pipeline {
        var commands: std.ArrayListUnmanaged(Command) = .empty;

        const first_cmd = try self.parseCommand() orelse return null;
        try commands.append(self.allocator, first_cmd);

        while (self.peekOperator()) |op| {
            if (!op.isPipe()) break;
            _ = self.advance();
            self.skipSeparators();
            const cmd = try self.parseCommand() orelse return ParseError.UnexpectedEOF;
            try commands.append(self.allocator, cmd);
        }

        return Pipeline{
            .commands = try commands.toOwnedSlice(self.allocator),
        };
    }

    /// Parses a logical chain of pipelines connected by `&&`, `||`, `and`, or `or`.
    fn parseLogical(self: *Parser) ParseError!?[]ChainItem {
        var chains: std.ArrayListUnmanaged(ChainItem) = .empty;

        const first_pipeline = try self.parsePipeline() orelse return null;
        try chains.append(self.allocator, .{ .op = .none, .pipeline = first_pipeline });

        while (self.tryParseLogicalOp()) |op| {
            _ = self.advance();
            self.skipSeparators();
            const pipeline = try self.parsePipeline() orelse return ParseError.UnexpectedEOF;
            try chains.append(self.allocator, .{ .op = op, .pipeline = pipeline });
        }

        return try chains.toOwnedSlice(self.allocator);
    }

    // =========================================================================
    // Block scanning helpers
    // =========================================================================

    /// Checks if the previous token is "else", used to detect "else if" patterns.
    /// Returns true if the token before the current position is an unquoted "else" word.
    fn isPrevTokenElse(self: *const Parser) bool {
        if (self.pos == 0) return false;
        const prev_tok = self.tokens[self.pos - 1];
        if (prev_tok.kind != .word) return false;
        const text = token_types.getBareText(prev_tok.kind.word) orelse return false;
        return std.mem.eql(u8, text, "else");
    }

    /// Scans forward to find a block terminator, handling nested blocks.
    /// Returns the position of the terminator, leaving the parser AT the terminator (not past it).
    ///
    /// `terminators` specifies which keywords (besides "end" at depth 0) can end the scan.
    /// For example, parseIfBranch passes &.{"else"} to stop at "else" at depth 1.
    ///
    /// Keywords are only recognized in command position (after a separator or at start).
    /// This allows `echo end` to work correctly - "end" as an argument is not a terminator.
    fn scanToBlockEnd(self: *Parser, terminators: []const []const u8) ?usize {
        var depth: usize = 1;

        while (self.pos < self.tokens.len) {
            const in_command_position = self.isPrevTokenSeparator();

            // Skip non-keyword positions
            if (!in_command_position) {
                _ = self.advance();
                continue;
            }

            // Handle block-starting keywords (increases nesting depth)
            if (self.isBlockStart()) {
                // "else if" is a continuation, not a new block
                if (!(self.isKeyword("if") and self.isPrevTokenElse())) {
                    depth += 1;
                }
                _ = self.advance();
                continue;
            }

            // Handle "end" keyword (decreases nesting depth)
            if (self.isKeyword("end")) {
                depth -= 1;
                if (depth == 0) return self.pos;
                _ = self.advance();
                continue;
            }

            // Check custom terminators at outermost level
            if (depth == 1) {
                for (terminators) |term| {
                    if (self.isKeyword(term)) return self.pos;
                }
            }

            _ = self.advance();
        }
        return null;
    }

    // =========================================================================
    // Source extraction utilities
    // =========================================================================

    /// Extracts source text between two token positions using byte indices.
    /// Returns empty string if positions are invalid or input is unavailable.
    ///
    /// Note: Uses start_byte of token at end_pos (not the end_byte), which
    /// effectively captures text up to (but not including) the end_pos token.
    /// This is useful for extracting source between keywords (e.g., "if" body before "else").
    fn extractSourceRange(self: *const Parser, start_pos: usize, end_pos: usize) []const u8 {
        if (self.input.len == 0) return "";
        if (start_pos >= end_pos or start_pos >= self.tokens.len) return "";

        const start_byte = self.tokens[start_pos].span.start;

        // End byte is either the start of the end_pos token, or end of input
        const end_byte = if (end_pos < self.tokens.len)
            self.tokens[end_pos].span.start
        else
            self.input.len;

        if (start_byte >= end_byte) return "";

        return self.input[start_byte..@min(end_byte, self.input.len)];
    }

    /// Captures a block body (the content between a construct's header and its terminator).
    ///
    /// Enforces two requirements:
    /// 1. A separator (`;` or newline) must precede the body
    /// 2. The body must be non-empty
    ///
    /// Scans until reaching `end` or any keyword in `terminators` (e.g., `else`).
    /// Returns the trimmed body source, or fails with `MissingSeparator`/`MissingBody`/`err`.
    fn captureBlockBody(self: *Parser, terminators: []const []const u8, err: ParseError) ParseError![]const u8 {
        if (!self.isSeparator()) return ParseError.MissingSeparator;
        self.skipSeparators();

        const body_start = self.pos;
        const end_pos = self.scanToBlockEnd(terminators) orelse return err;
        const body = std.mem.trim(u8, self.extractSourceRange(body_start, end_pos), " \t\n");

        if (body.len == 0) return ParseError.MissingBody;
        return body;
    }

    /// Captures condition source (tokens until separator).
    /// Advances past the condition, leaving parser at the separator.
    fn captureCondition(self: *Parser) []const u8 {
        const start = self.pos;
        while (self.pos < self.tokens.len and !self.isSeparator()) {
            _ = self.advance();
        }
        return std.mem.trim(u8, self.extractSourceRange(start, self.pos), " \t\n");
    }

    // =========================================================================
    // Control flow statement parsing
    // =========================================================================

    /// Parses a function definition: `fun name ... end`
    fn parseFunctionDefinition(self: *Parser) ParseError!Statement {
        _ = self.advance(); // consume 'fun'

        if (!self.isWord()) return ParseError.InvalidFunctionName;
        const name = extractSimpleWord(self.advance().?) orelse return ParseError.InvalidFunctionName;
        const body = try self.captureBlockBody(&.{}, ParseError.UnterminatedFunction);
        try self.consumeEnd();

        return .{ .function = .{ .name = name, .body = body } };
    }

    /// Parses an if statement with optional else-if chains:
    ///   `if cond1; body1; else if cond2; body2; else; body3; end`
    fn parseIfStatement(self: *Parser) ParseError!Statement {
        var branches: std.ArrayListUnmanaged(IfBranch) = .empty;

        _ = self.advance(); // consume 'if'
        const first_branch = try self.parseIfBranch();
        try branches.append(self.allocator, first_branch);

        // Parse else-if chains and final else
        var else_body: ?[]const u8 = null;
        while (self.pos < self.tokens.len) {
            if (self.isKeyword("else")) {
                _ = self.advance(); // consume 'else'

                if (self.isKeyword("if")) {
                    // else if
                    _ = self.advance(); // consume 'if'
                    const branch = try self.parseIfBranch();
                    try branches.append(self.allocator, branch);
                } else {
                    // Final else
                    else_body = try self.captureBlockBody(&.{}, ParseError.UnterminatedIf);
                    try self.consumeEnd();
                    break;
                }
            } else if (self.isKeyword("end")) {
                try self.consumeEnd();
                break;
            } else {
                return ParseError.UnterminatedIf;
            }
        }

        return .{ .@"if" = .{
            .branches = try branches.toOwnedSlice(self.allocator),
            .else_body = else_body,
        } };
    }

    /// Parses a single if/else-if branch (condition + body).
    /// Expects parser positioned after 'if' keyword.
    fn parseIfBranch(self: *Parser) ParseError!IfBranch {
        // Condition must immediately follow 'if' - no separator allowed
        if (!self.isWord()) return ParseError.MissingCondition;
        const condition = try self.parseSimpleCommand() orelse return ParseError.UnexpectedEOF;
        const body = try self.captureBlockBody(&.{"else"}, ParseError.UnterminatedIf);
        return .{ .condition = condition, .body = body };
    }

    /// Parses an each loop (for is an alias).
    ///
    /// Supported forms:
    ///   - `each items... end`         (variable defaults to "item")
    ///   - `each var in items... end`  (explicit variable name)
    ///   - `for var in items... end`   (alias)
    ///   - `for items... end`          (alias)
    ///
    /// Sets $item (or custom var) and $index (1-based) on each iteration.
    fn parseEachStatement(self: *Parser) ParseError!Statement {
        _ = self.advance(); // consume 'each' or 'for'

        // Items/variable must immediately follow 'each'/'for' - no separator allowed
        if (!self.isWord()) return ParseError.InvalidEach;

        // Look ahead: is this `VAR in ITEMS` or just `ITEMS`?
        const first_word_pos = self.pos;
        const first_word_tok = self.advance().?;

        const variable: []const u8 = if (self.isKeyword("in")) blk: {
            // `var in items` form - extract variable name
            _ = self.advance(); // consume 'in'
            if (!self.isWord()) return ParseError.InvalidEach;
            break :blk extractSimpleWord(first_word_tok) orelse return ParseError.InvalidEach;
        } else blk: {
            // `items` form - rewind and use default variable name
            self.pos = first_word_pos;
            break :blk "item";
        };

        const items_source = self.captureCondition();
        const body = try self.captureBlockBody(&.{}, ParseError.UnterminatedEach);
        try self.consumeEnd();

        return .{ .each = .{
            .variable = variable,
            .items_source = items_source,
            .body = body,
        } };
    }

    /// Parses a while loop: `while condition ... end`
    fn parseWhileStatement(self: *Parser) ParseError!Statement {
        _ = self.advance(); // consume 'while'

        // Condition must immediately follow 'while' - no separator allowed
        if (!self.isWord()) return ParseError.MissingCondition;
        const condition = try self.parseSimpleCommand() orelse return ParseError.UnexpectedEOF;
        const body = try self.captureBlockBody(&.{}, ParseError.UnterminatedWhile);
        try self.consumeEnd();

        return .{ .@"while" = .{ .condition = condition, .body = body } };
    }

    // =========================================================================
    // Statement parsing
    // =========================================================================

    /// Parses a return statement with optional status.
    fn parseReturnStatement(self: *Parser) Statement {
        _ = self.advance(); // consume 'return'
        return .{ .@"return" = self.tryParseOptionalWord() };
    }

    /// Parses an exit statement with optional status code.
    fn parseExitStatement(self: *Parser) Statement {
        _ = self.advance(); // consume 'exit'
        return .{ .exit = self.tryParseOptionalWord() };
    }

    /// Captures the next word if present, returns null otherwise.
    fn tryParseOptionalWord(self: *Parser) ?[]const u8 {
        return if (self.isWord()) self.captureWord() else null;
    }

    /// Captures a single word token as trimmed source text.
    fn captureWord(self: *Parser) []const u8 {
        const start = self.pos;
        _ = self.advance();
        return std.mem.trim(u8, self.extractSourceRange(start, self.pos), " \t\n");
    }

    /// Parses a simple command: a command without background or capture.
    /// Used for if/while conditions and defer statements.
    fn parseSimpleCommand(self: *Parser) ParseError!?CommandStatement {
        const cmd_stmt = try self.parseCommandStatement() orelse return null;

        // Simple commands cannot be backgrounded or capture output
        if (cmd_stmt.background) return ParseError.ConditionWithBackground;
        if (cmd_stmt.capture != null) return ParseError.ConditionWithCapture;

        return cmd_stmt;
    }

    /// Parses a defer statement.
    /// Stores the source text of the deferred command (parsed on-demand during execution).
    fn parseDeferStatement(self: *Parser) ParseError!Statement {
        _ = self.advance(); // consume 'defer'
        const start = self.pos;

        // Parse and validate the command (parseSimpleCommand rejects background/capture)
        _ = try self.parseSimpleCommand() orelse return ParseError.UnexpectedEOF;

        // Extract the source text of the command (will be parsed again during execution)
        const source = std.mem.trim(u8, self.extractSourceRange(start, self.pos), " \t\n");
        return .{ .@"defer" = source };
    }

    /// Parses an output capture (=> or =>@).
    fn parseCapture(self: *Parser) ParseError!Capture {
        const op = self.advance().?.kind.operator;
        const mode: CaptureMode = if (op == .capture_lines) .lines else .string;

        if (!self.isWord()) return ParseError.InvalidCapture;
        const name = extractSimpleWord(self.advance().?) orelse return ParseError.InvalidCapture;

        return .{ .mode = mode, .variable = name };
    }

    /// Parses a command statement: logical chains with optional background and capture.
    fn parseCommandStatement(self: *Parser) ParseError!?CommandStatement {
        const chains = try self.parseLogical() orelse return null;
        var background = false;
        var capture: ?Capture = null;

        if (self.isOperator(.background)) {
            background = true;
            _ = self.advance();
        }

        if (self.peekOperator()) |op| {
            if (op.isCapture()) {
                if (background) return ParseError.CaptureWithBackground;
                capture = try self.parseCapture();
            }
        }

        return .{
            .background = background,
            .capture = capture,
            .chains = chains,
        };
    }

    /// Parses a single statement (command, control flow, or function definition).
    fn parseStatement(self: *Parser) ParseError!?Statement {
        // Control flow and function definitions
        if (self.isKeyword("fun")) return try self.parseFunctionDefinition();
        if (self.isKeyword("if")) return try self.parseIfStatement();
        if (self.isKeyword("each") or self.isKeyword("for")) return try self.parseEachStatement();
        if (self.isKeyword("while")) return try self.parseWhileStatement();

        // Simple statements
        if (self.isKeyword("break")) {
            _ = self.advance();
            return .@"break";
        }
        if (self.isKeyword("continue")) {
            _ = self.advance();
            return .@"continue";
        }
        if (self.isKeyword("return")) return self.parseReturnStatement();
        if (self.isKeyword("defer")) return try self.parseDeferStatement();
        if (self.isKeyword("exit")) return self.parseExitStatement();

        // Command statement
        const cmd_stmt = try self.parseCommandStatement() orelse return null;
        return .{ .command = cmd_stmt };
    }

    // =========================================================================
    // Main parsing entry point
    // =========================================================================

    pub fn parse(self: *Parser) ParseError!Program {
        var statements: std.ArrayListUnmanaged(Statement) = .empty;

        self.skipSeparators();
        while (self.pos < self.tokens.len) {
            const stmt = try self.parseStatement() orelse break;
            try statements.append(self.allocator, stmt);
            self.skipSeparators();
        }

        return .{ .statements = try statements.toOwnedSlice(self.allocator) };
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;
const lexer = @import("lexer.zig");

const TestContext = struct {
    arena: std.heap.ArenaAllocator,

    fn init() TestContext {
        return .{ .arena = std.heap.ArenaAllocator.init(testing.allocator) };
    }

    fn deinit(self: *TestContext) void {
        self.arena.deinit();
    }

    /// Parse input and return the AST. Use this for most tests.
    fn parse(self: *TestContext, input: []const u8) !Program {
        const allocator = self.arena.allocator();

        var lex = lexer.Lexer.init(allocator, input);
        const tokens = try lex.tokenize();

        var p = Parser.initWithInput(allocator, tokens, input);
        return try p.parse();
    }

    /// Expect a parse error.
    fn expectError(self: *TestContext, input: []const u8, expected: ParseError) !void {
        const allocator = self.arena.allocator();
        var lex = lexer.Lexer.init(allocator, input);
        const tokens = try lex.tokenize();
        var p = Parser.initWithInput(allocator, tokens, input);
        try testing.expectError(expected, p.parse());
    }

    /// Get the first command from a parsed program.
    fn firstCommand(prog: Program) Command {
        return prog.statements[0].command.chains[0].pipeline.commands[0];
    }
};

// -----------------------------------------------------------------------------
// Commands and Pipelines
// -----------------------------------------------------------------------------

test "Commands: simple command with arguments" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("echo hello");
    const cmd_stmt = prog.statements[0].command;

    try testing.expectEqual(@as(usize, 1), prog.statements.len);
    try testing.expectEqual(false, cmd_stmt.background);
    try testing.expectEqual(@as(?Capture, null), cmd_stmt.capture);
    try testing.expectEqual(@as(usize, 1), cmd_stmt.chains.len);
    try testing.expectEqual(@as(usize, 2), TestContext.firstCommand(prog).words.len);
}

test "Commands: pipeline chains multiple commands" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("cat file | grep foo | wc -l");
    const pipeline = prog.statements[0].command.chains[0].pipeline;

    try testing.expectEqual(@as(usize, 3), pipeline.commands.len);
}

test "Commands: background execution" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("sleep 10 &");
    try testing.expectEqual(true, prog.statements[0].command.background);
}

// -----------------------------------------------------------------------------
// Output Capture
// -----------------------------------------------------------------------------

test "Capture: string and lines modes" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const cases = [_]struct { []const u8, CaptureMode, []const u8 }{
        .{ "whoami => user", .string, "user" },
        .{ "ls =>@ files", .lines, "files" },
    };

    for (cases) |case| {
        const prog = try ctx.parse(case[0]);
        const cap = prog.statements[0].command.capture orelse return error.TestExpectedEqual;
        try testing.expectEqual(case[1], cap.mode);
        try testing.expectEqualStrings(case[2], cap.variable);
    }
}

// -----------------------------------------------------------------------------
// Logical Operators
// -----------------------------------------------------------------------------

test "Logical: operator and keyword forms" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Operator form: && and ||
    const ops = try ctx.parse("true && echo ok || echo fail");
    try testing.expectEqual(@as(usize, 3), ops.statements[0].command.chains.len);
    try testing.expectEqual(ast.ChainOperator.@"and", ops.statements[0].command.chains[1].op);
    try testing.expectEqual(ast.ChainOperator.@"or", ops.statements[0].command.chains[2].op);

    // Keyword form: and, or
    const kws = try ctx.parse("true and echo ok or echo fail");
    try testing.expectEqual(@as(usize, 3), kws.statements[0].command.chains.len);
    try testing.expectEqual(ast.ChainOperator.@"and", kws.statements[0].command.chains[1].op);
    try testing.expectEqual(ast.ChainOperator.@"or", kws.statements[0].command.chains[2].op);
}

// -----------------------------------------------------------------------------
// Redirections
// -----------------------------------------------------------------------------

test "Redirections: all types parse correctly" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Table-driven test for redirect types
    const cases = [_]struct {
        input: []const u8,
        from_fd: u8,
        kind_tag: enum { read, write_truncate, write_append, dup },
    }{
        .{ .input = "cmd < in.txt", .from_fd = 0, .kind_tag = .read },
        .{ .input = "cmd > out.txt", .from_fd = 1, .kind_tag = .write_truncate },
        .{ .input = "cmd >> log.txt", .from_fd = 1, .kind_tag = .write_append },
        .{ .input = "cmd 2> err.txt", .from_fd = 2, .kind_tag = .write_truncate },
        .{ .input = "cmd 2>> err.txt", .from_fd = 2, .kind_tag = .write_append },
        .{ .input = "cmd &> all.txt", .from_fd = 1, .kind_tag = .write_truncate },
        .{ .input = "cmd 2>&1", .from_fd = 2, .kind_tag = .dup },
    };

    for (cases) |case| {
        const prog = try ctx.parse(case.input);
        const cmd = TestContext.firstCommand(prog);
        try testing.expectEqual(@as(usize, 1), cmd.redirects.len);
        try testing.expectEqual(case.from_fd, cmd.redirects[0].from_fd);

        const matches = switch (cmd.redirects[0].kind) {
            .read => case.kind_tag == .read,
            .write_truncate => case.kind_tag == .write_truncate,
            .write_append => case.kind_tag == .write_append,
            .dup => case.kind_tag == .dup,
        };
        try testing.expect(matches);
    }
}

test "Redirections: quoting affects path parsing" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const cases = [_]struct { []const u8, []const u8, QuoteKind }{
        .{ "echo > \"foo bar.txt\"", "foo bar.txt", .double },
        .{ "echo > '$var.txt'", "$var.txt", .single },
        .{ "echo > $outfile", "$outfile", .none },
    };

    for (cases) |case| {
        const prog = try ctx.parse(case[0]);
        const cmd = TestContext.firstCommand(prog);
        switch (cmd.redirects[0].kind) {
            .write_truncate => |parts| {
                try testing.expectEqualStrings(case[1], parts[0].text);
                try testing.expectEqual(case[2], parts[0].quotes);
            },
            else => return error.TestExpectedEqual,
        }
    }
}

// -----------------------------------------------------------------------------
// Assignments
// -----------------------------------------------------------------------------

test "Assignments: environment prefix" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Single assignment
    const single = try ctx.parse("FOO=bar env");
    const cmd1 = TestContext.firstCommand(single);
    try testing.expectEqual(@as(usize, 1), cmd1.assignments.len);
    try testing.expectEqualStrings("FOO", cmd1.assignments[0].key);
    try testing.expectEqualStrings("bar", cmd1.assignments[0].value);

    // Multiple assignments
    const multi = try ctx.parse("FOO=a BAR=b BAZ=c cmd");
    const cmd2 = TestContext.firstCommand(multi);
    try testing.expectEqual(@as(usize, 3), cmd2.assignments.len);
    try testing.expectEqualStrings("FOO", cmd2.assignments[0].key);
    try testing.expectEqualStrings("BAR", cmd2.assignments[1].key);
    try testing.expectEqualStrings("BAZ", cmd2.assignments[2].key);
}

// -----------------------------------------------------------------------------
// Functions
// -----------------------------------------------------------------------------

test "Functions: definition styles" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Multi-line definition
    const multiline = try ctx.parse("fun greet\n  echo hello\nend");
    try testing.expectEqualStrings("greet", multiline.statements[0].function.name);
    try testing.expect(std.mem.indexOf(u8, multiline.statements[0].function.body, "echo hello") != null);

    // Inline definition
    const inline_def = try ctx.parse("fun greet; echo hello; end");
    try testing.expectEqualStrings("greet", inline_def.statements[0].function.name);
    try testing.expect(std.mem.indexOf(u8, inline_def.statements[0].function.body, "echo hello") != null);
}

test "Functions: nested definitions" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("fun outer\n  fun inner\n    echo inner\n  end\n  inner\nend");
    const fun_def = prog.statements[0].function;

    try testing.expectEqualStrings("outer", fun_def.name);
    try testing.expect(std.mem.indexOf(u8, fun_def.body, "fun inner") != null);
    try testing.expect(std.mem.indexOf(u8, fun_def.body, "end") != null);
}

test "Functions: errors" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("fun greet\n  echo hello", ParseError.UnterminatedFunction);
    try ctx.expectError("fun \"quoted\" body end", ParseError.InvalidFunctionName);
    try ctx.expectError("fun greet; end", ParseError.MissingBody);
}

// -----------------------------------------------------------------------------
// If Statements
// -----------------------------------------------------------------------------

test "If: simple and inline forms" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Multi-line
    const multiline = try ctx.parse("if true\n  echo yes\nend");
    const if1 = multiline.statements[0].@"if";
    try testing.expectEqual(@as(usize, 1), if1.branches.len);
    try testing.expectEqual(@as(usize, 1), if1.branches[0].condition.chains.len);
    try testing.expect(std.mem.indexOf(u8, if1.branches[0].body, "echo yes") != null);
    try testing.expectEqual(@as(?[]const u8, null), if1.else_body);

    // Inline
    const inline_if = try ctx.parse("if true; echo yes; end");
    try testing.expectEqual(@as(usize, 1), inline_if.statements[0].@"if".branches.len);
}

test "If: with else branch" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("if false\n  echo no\nelse\n  echo yes\nend");
    const if_stmt = prog.statements[0].@"if";

    try testing.expectEqual(@as(usize, 1), if_stmt.branches.len);
    try testing.expect(std.mem.indexOf(u8, if_stmt.branches[0].body, "echo no") != null);
    try testing.expect(if_stmt.else_body != null);
    try testing.expect(std.mem.indexOf(u8, if_stmt.else_body.?, "echo yes") != null);
}

test "If: else-if chains" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // With final else
    const with_else = try ctx.parse("if test $x -eq 1\n  echo one\nelse if test $x -eq 2\n  echo two\nelse\n  echo other\nend");
    const if1 = with_else.statements[0].@"if";
    try testing.expectEqual(@as(usize, 2), if1.branches.len);
    try testing.expectEqual(@as(usize, 4), if1.branches[0].condition.chains[0].pipeline.commands[0].words.len);
    try testing.expect(if1.else_body != null);

    // Without final else
    const without_else = try ctx.parse("if false; echo a; else if true; echo b; end");
    const if2 = without_else.statements[0].@"if";
    try testing.expectEqual(@as(usize, 2), if2.branches.len);
    try testing.expectEqual(@as(?[]const u8, null), if2.else_body);
}

test "If: nested statements" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("if true\n  if false\n    echo inner\n  end\nend");
    const if_stmt = prog.statements[0].@"if";

    try testing.expect(std.mem.indexOf(u8, if_stmt.branches[0].body, "if false") != null);
    try testing.expect(std.mem.indexOf(u8, if_stmt.branches[0].body, "end") != null);
}

test "If: unterminated error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("if true\n  echo yes", ParseError.UnterminatedIf);
}

test "If: missing condition error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("if; echo hello; end", ParseError.MissingCondition);
    try ctx.expectError("if\necho hello\nend", ParseError.MissingCondition);
    try ctx.expectError("if false; echo no; else if; echo yes; end", ParseError.MissingCondition);
}

test "If: missing body error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("if true; end", ParseError.MissingBody);
    try ctx.expectError("if true; echo a; else; end", ParseError.MissingBody);
    try ctx.expectError("if true; echo a; else if true; end", ParseError.MissingBody);
}

test "If: else requires separator before body" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("if true; echo a; else echo b; end", ParseError.MissingSeparator);
}

// -----------------------------------------------------------------------------
// While Loops
// -----------------------------------------------------------------------------

test "While: simple and inline forms" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Multi-line
    const multiline = try ctx.parse("while test -f file\n  sleep 1\nend");
    const while1 = multiline.statements[0].@"while";
    try testing.expectEqual(@as(usize, 3), while1.condition.chains[0].pipeline.commands[0].words.len);
    try testing.expect(std.mem.indexOf(u8, while1.body, "sleep 1") != null);

    // Inline
    const inline_while = try ctx.parse("while true; echo loop; end");
    try testing.expectEqual(@as(usize, 1), inline_while.statements[0].@"while".condition.chains.len);
}

test "While: nested in if" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("if true\n  while false\n    echo inner\n  end\nend");
    const if_stmt = prog.statements[0].@"if";

    try testing.expect(std.mem.indexOf(u8, if_stmt.branches[0].body, "while false") != null);
}

test "While: unterminated error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("while true\n  echo loop", ParseError.UnterminatedWhile);
}

test "While: missing condition error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("while; echo loop; end", ParseError.MissingCondition);
    try ctx.expectError("while\necho loop\nend", ParseError.MissingCondition);
}

test "While: missing body error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("while true; end", ParseError.MissingBody);
}

// -----------------------------------------------------------------------------
// Control Flow (break, continue, return)
// -----------------------------------------------------------------------------

test "Control: break and continue" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const break_prog = try ctx.parse("break");
    try testing.expect(break_prog.statements[0] == .@"break");

    const continue_prog = try ctx.parse("continue");
    try testing.expect(continue_prog.statements[0] == .@"continue");
}

test "Return and Exit: with and without status" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Return statements
    const ret = try ctx.parse("return");
    try testing.expect(ret.statements[0] == .@"return");
    try testing.expectEqual(@as(?[]const u8, null), ret.statements[0].@"return");

    const ret1 = try ctx.parse("return 1");
    try testing.expectEqualStrings("1", ret1.statements[0].@"return".?);

    const retVar = try ctx.parse("return $status");
    try testing.expectEqualStrings("$status", retVar.statements[0].@"return".?);

    // Exit statements (same structure as return)
    const exit = try ctx.parse("exit");
    try testing.expect(exit.statements[0] == .exit);
    try testing.expectEqual(@as(?[]const u8, null), exit.statements[0].exit);

    const exit0 = try ctx.parse("exit 0");
    try testing.expectEqualStrings("0", exit0.statements[0].exit.?);

    const exitVar = try ctx.parse("exit $code");
    try testing.expectEqualStrings("$code", exitVar.statements[0].exit.?);
}

// -----------------------------------------------------------------------------
// Defer
// -----------------------------------------------------------------------------

test "Defer: command forms" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Simple command - now stores source text instead of parsed AST
    const simple = try ctx.parse("defer rm -rf $tmpdir");
    const defer1 = simple.statements[0].@"defer";
    try testing.expectEqualStrings("rm -rf $tmpdir", defer1);

    // With pipeline
    const pipeline_prog = try ctx.parse("defer cat file | grep foo");
    try testing.expectEqualStrings("cat file | grep foo", pipeline_prog.statements[0].@"defer");

    // With logical chain
    const logical = try ctx.parse("defer rm file && echo done");
    try testing.expectEqualStrings("rm file && echo done", logical.statements[0].@"defer");
}

test "Defer: in function body" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("fun cleanup\n  defer echo done\n  echo working\nend");
    const fun_def = prog.statements[0].function;

    try testing.expectEqualStrings("cleanup", fun_def.name);
    try testing.expect(std.mem.indexOf(u8, fun_def.body, "defer echo done") != null);
}

test "Defer: errors for invalid modifiers" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("defer rm file &", ParseError.ConditionWithBackground);
    try ctx.expectError("defer whoami => user", ParseError.ConditionWithCapture);
}

// -----------------------------------------------------------------------------
// Each/For Loops
// -----------------------------------------------------------------------------

test "Each: implicit and explicit variable" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // Implicit $item variable
    const implicit = try ctx.parse("each a b c\n  echo $item\nend");
    const each1 = implicit.statements[0].each;
    try testing.expectEqualStrings("item", each1.variable);
    try testing.expectEqualStrings("a b c", each1.items_source);

    // Explicit variable with 'in'
    const explicit = try ctx.parse("each x in 1 2 3\n  echo $x\nend");
    const each2 = explicit.statements[0].each;
    try testing.expectEqualStrings("x", each2.variable);
    try testing.expectEqualStrings("1 2 3", each2.items_source);
}

test "Each: inline form" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("each a b c; echo $item; end");
    try testing.expectEqualStrings("item", prog.statements[0].each.variable);
}

test "Each: 'for' keyword is an alias" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    // 'for' works exactly like 'each'
    const implicit = try ctx.parse("for a b c\n  echo $item\nend");
    try testing.expectEqualStrings("item", implicit.statements[0].each.variable);

    const explicit = try ctx.parse("for x in 1 2 3\n  echo $x\nend");
    try testing.expectEqualStrings("x", explicit.statements[0].each.variable);
}

test "Each: nested loops" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    const prog = try ctx.parse("each a b\n  each 1 2\n    echo $item\n  end\nend");
    try testing.expect(std.mem.indexOf(u8, prog.statements[0].each.body, "each 1 2") != null);
}

test "Each: unterminated error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("each a b c\n  echo $item", ParseError.UnterminatedEach);
}

test "Each: missing body error" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.expectError("each a b c; end", ParseError.MissingBody);
    try ctx.expectError("for x in 1 2 3; end", ParseError.MissingBody);
}
