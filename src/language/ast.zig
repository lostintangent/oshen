//! Abstract Syntax Tree types produced by the parser.
//!
//! The AST is the syntax-only view of a parsed program—no expansion, no runtime
//! state. Key shapes:
//! - `Program`: Top-level list of `Statement`. There's always exactly one.
//! - `Statement`: A tagged union that represents commands, function definitions, and
//!   control flow statements.
//! - `CommandStatement`: A logical chain of pipelines with optional capture or
//!   background execution.
//! - `Pipeline`/`Command`/`Assignment`/`Redirect`: The components of a simple shell
//!   command (argv, env prefixes, redirects).
//! - Control flow: `IfStatement`, `ForStatement`, `WhileStatement`,
//!   `FunctionDefinition`

const std = @import("std");
const tokens = @import("tokens.zig");

pub const WordPart = tokens.WordPart;
pub const QuoteKind = tokens.QuoteKind;

// =============================================================================
// Function and Control Flow Definitions
// =============================================================================

/// A function definition: `fun name ... end`
pub const FunctionDefinition = struct {
    /// The function name (must be an unquoted bare word)
    name: []const u8,
    /// The raw source of the function body (parsed on-demand during execution)
    body: []const u8,
};

/// A single if/else-if branch with its condition and body.
pub const IfBranch = struct {
    /// The pre-parsed condition (always has background=false, capture=null)
    condition: CommandStatement,
    /// The body source to execute if condition is true
    body: []const u8,
};

/// An if statement with optional else-if chains and else clause.
pub const IfStatement = struct {
    /// First element is the "if" branch, rest are "else if" branches
    branches: []const IfBranch,
    /// Final "else" body if present (no condition)
    else_body: ?[]const u8,
};

/// An each loop: `each items... end` or `each var in items... end`
/// The `for` keyword is an alias for `each`.
///
/// Features:
/// - Optional variable naming (defaults to "item")
/// - Implicit $index variable (1-based)
pub const EachStatement = struct {
    /// The loop variable name (defaults to "item" if not specified)
    variable: []const u8,
    /// The raw source of items to iterate over (parsed on-demand)
    items_source: []const u8,
    /// The loop body source
    body: []const u8,
};

/// A while loop: `while condition... end`
pub const WhileStatement = struct {
    /// The pre-parsed condition (always has background=false, capture=null)
    condition: CommandStatement,
    /// The loop body source
    body: []const u8,
};

// =============================================================================
// Output Capture
// =============================================================================

/// How to capture command output.
pub const CaptureMode = enum {
    /// Capture as a single string (trims trailing newline)
    string,
    /// Capture as an array of lines
    lines,
};

/// Output capture specification: `command => var` or `command =>@ var`
pub const Capture = struct {
    /// Whether to capture as string or lines
    mode: CaptureMode,
    /// The variable name to store the captured output
    variable: []const u8,
};

// =============================================================================
// Redirections
// =============================================================================

/// The type of I/O redirection.
pub const RedirectKind = union(enum) {
    /// Redirect input from a file to fd (defaults to stdin)
    read: []const WordPart,
    /// Redirect output to a file (truncate) from fd (defaults to stdout)
    write_truncate: []const WordPart,
    /// Redirect output to a file (append) from fd (defaults to stdout)
    write_append: []const WordPart,
    /// Duplicate one fd to another (e.g., 2>&1)
    dup: u8,
};

/// A single I/O redirection (e.g., `< input.txt`, `> output.txt`, `2>&1`)
pub const Redirect = struct {
    /// File descriptor being redirected (0=stdin, 1=stdout, 2=stderr)
    from_fd: u8,
    /// The type and target of the redirection
    kind: RedirectKind,
};

// =============================================================================
// Commands and Pipelines
// =============================================================================

/// An environment variable assignment prefix (e.g., `FOO=bar cmd`)
/// Can represent both unexpanded (AST) and expanded forms.
/// In AST: value is the literal string from source
/// In ExpandedCmd: value is the expanded string
pub const Assignment = struct {
    /// The variable name
    key: []const u8,
    /// The value as a string (literal in AST, expanded in ExpandedCmd)
    value: []const u8,
};

/// A single command with optional assignments, words, and redirects.
pub const Command = struct {
    /// Environment variable assignments (KEY=value) that prefix the command
    assignments: []const Assignment,
    /// Command name and arguments (may contain variables to expand)
    words: []const []const WordPart,
    /// I/O redirections (<, >, >>, 2>, 2>&1, etc.)
    redirects: []const Redirect,
};

/// Logical chain operator between pipelines.
pub const ChainOperator = enum {
    /// First command in chain (no preceding operator)
    none,
    /// Execute if previous succeeded (&&, and)
    @"and",
    /// Execute if previous failed (||, or)
    @"or",
};

/// A pipeline of commands connected by pipes.
pub const Pipeline = struct {
    /// Commands in the pipeline (connected by `|`)
    commands: []const Command,
};

/// A single element in a logical chain.
pub const ChainItem = struct {
    /// The operator preceding this pipeline (none for first)
    op: ChainOperator,
    /// The pipeline to execute
    pipeline: Pipeline,
};

/// A command statement with logical chains, background, and capture options.
pub const CommandStatement = struct {
    /// The chain of pipelines (connected by &&, ||, and, or)
    chains: []const ChainItem,
    /// Whether to run in background (&)
    background: bool,
    /// Optional output capture (=> or =>@)
    capture: ?Capture,
};

// =============================================================================
// Statements and Program
// =============================================================================

/// A single statement in the AST.
pub const Statement = union(enum) {
    /// A command statement (may include pipelines, chains, capture)
    command: CommandStatement,
    /// A function definition
    function: FunctionDefinition,
    /// An if/else-if/else statement
    @"if": IfStatement,
    /// An each loop (for is an alias)
    each: EachStatement,
    /// A while loop
    @"while": WhileStatement,
    /// Break out of the innermost loop
    @"break",
    /// Continue to next iteration of innermost loop
    @"continue",
    /// Return from current function with optional status (raw source, expanded at runtime)
    @"return": ?[]const u8,
    /// Defer a command to run when the current function exits.
    /// Stores the raw source text (parsed on-demand during execution).
    /// This ensures the deferred command survives arena cleanup of nested blocks.
    @"defer": []const u8,
    /// Exit the shell with optional status code (raw source, expanded at runtime)
    exit: ?[]const u8,
};

/// The top-level AST node representing a complete program.
pub const Program = struct {
    /// All statements in the program, in order
    statements: []const Statement,
};
