//! Output capture: execute commands and capture their stdout.
//!
//! This module handles all output capture scenarios in the shell:
//! - Command substitution `$(...)`
//! - Output capture operators (`=>`, `=>@`)
//! - Custom prompt function execution
//!
//! Performance is critical since command substitutions are extremely common
//! in shell scripts (e.g., loops with `$(= $i + 1)`). We use a tiered approach:
//!
//!   1. Direct paths - Skip lexing/parsing entirely for simple patterns
//!      - tryCaptureCalcDirect:    $(= $x + 1), $(calc 2 * 3)
//!      - tryCaptureBuiltinDirect: $(pwd), $(echo $var), $(true)
//!
//!   2. Full path - Use complete lex → parse → expand pipeline, but capture in-process
//!      - tryCaptureBuiltinFull:   $(echo "hello $name"), $(type git)
//!
//!   3. Fork path - Fork a child process for complex commands
//!      - External commands, pipelines, redirects, etc.
//!      - tryExecExternalDirect: Simple external commands exec directly (no double-fork)
//!
//! The direct paths achieve ~1000x speedup over forking by avoiding process
//! creation overhead entirely. Even the full path is ~100x faster than forking
//! since it runs the builtin in-process.

const std = @import("std");
const io = @import("../../terminal/io.zig");
const builtins = @import("../../runtime/builtins.zig");
const State = @import("../../runtime/state.zig").State;
const expansion_pipeline = @import("../expansion/pipeline.zig");
const ExpandedCommand = expansion_pipeline.ExpandedCommand;
const ast = @import("../../language/ast.zig");
const expand = @import("../expansion/word.zig");
const calc = @import("../../runtime/builtins/calc.zig");
const interpreter = @import("../interpreter.zig");
const pipeline = @import("pipeline.zig");

// C library functions
const c = struct {
    extern "c" fn waitpid(pid: std.posix.pid_t, status: ?*c_int, options: c_int) std.posix.pid_t;
};

// =============================================================================
// Public API
// =============================================================================

pub const CaptureResult = struct {
    output: []const u8,
    status: u8,
};

/// Execute shell input and capture stdout.
///
/// Tries increasingly expensive strategies until one succeeds:
/// 1. Direct calc evaluation (no parsing)
/// 2. Direct builtin execution (no parsing)
/// 3. Full parse + in-process builtin capture
/// 4. Fork child process (fallback for external commands)
///
/// Used by:
/// - Command substitution `$(...)` during word expansion
/// - Custom prompt functions
pub fn executeAndCapture(allocator: std.mem.Allocator, state: *State, input: []const u8) ![]const u8 {
    // Direct path: evaluate calc expressions without any parsing
    if (tryCaptureCalcDirect(allocator, state, input)) |result| {
        return result;
    }

    // Direct path: run simple builtins without parsing
    if (tryCaptureBuiltinDirect(allocator, state, input)) |result| {
        return result;
    }

    // Full path: parse and expand, then capture builtin in-process
    if (tryCaptureBuiltinFull(allocator, state, input)) |result| {
        defer result.arena.deinit();
        const captured = try captureBuiltin(allocator, state, result.expanded.cmd.args);
        return captured.output;
    }

    // Fork path: spawn child process for external commands, pipelines, etc.
    switch (try forkWithPipe()) {
        .child => {
            // Try to exec simple external commands directly (avoids double-fork)
            tryExecExternalDirect(allocator, state, input);
            // If tryExecExternalDirect returns, fall back to full interpreter
            const status = interpreter.execute(allocator, state, input) catch 1;
            std.posix.exit(status);
        },
        .parent => |handle| {
            const result = try handle.readAndWait(allocator);
            return result.output;
        },
    }
}

/// Store captured output into a shell variable.
pub fn storeCapture(allocator: std.mem.Allocator, state: *State, output: []const u8, variable: []const u8, as_lines: bool) !void {
    if (as_lines) {
        var lines: std.ArrayListUnmanaged([]const u8) = .empty;
        defer lines.deinit(allocator);

        var iter = std.mem.splitScalar(u8, output, '\n');
        while (iter.next()) |line| {
            try lines.append(allocator, try allocator.dupe(u8, line));
        }
        try state.setVarList(variable, lines.items);
    } else {
        try state.setVar(variable, output);
    }
}

// =============================================================================
// Direct Capture Paths (no parsing)
// =============================================================================

/// Direct capture path for calc expressions.
///
/// Handles patterns like `$(= $x + 1)` and `$(calc 2 * 3)` by:
/// 1. Detecting the `= ` or `calc ` prefix
/// 2. Rejecting expressions with complex shell syntax ($(), pipes, etc.)
/// 3. Expanding simple `$var` references
/// 4. Evaluating the math expression directly
///
/// Returns null if the input doesn't match or is too complex.
///
/// ## Performance
/// ~1000x faster than forking - no process creation, no lexer/parser, no command dispatch.
/// The calc parser operates directly on the expression string.
fn tryCaptureCalcDirect(allocator: std.mem.Allocator, state: *State, input: []const u8) ?[]const u8 {
    // Check for "calc " or "= " prefix
    const expr_start: usize = if (std.mem.startsWith(u8, input, "calc "))
        5
    else if (std.mem.startsWith(u8, input, "= "))
        2
    else
        return null;

    const expr_raw = input[expr_start..];

    // Bail if expression contains complex shell metacharacters
    // We can handle simple $var but not $(), ``, pipes, redirects, etc.
    for (expr_raw) |char| {
        switch (char) {
            '`', '|', '>', '<', '&', ';', '\n' => return null,
            else => {},
        }
    }

    // Expand simple variables in the expression
    // Use a stack buffer to avoid allocation in common cases
    var expanded_buf: [512]u8 = undefined;
    const expanded = expandSimpleVars(state, expr_raw, &expanded_buf) orelse return null;

    // Parse and evaluate the expression directly
    var parser_inst = calc.Parser.init(expanded);
    const result = parser_inst.parse() catch return null;

    // Format result
    return std.fmt.allocPrint(allocator, "{d}", .{result}) catch null;
}

/// Direct capture path for simple builtin commands.
///
/// Handles patterns like `$(pwd)`, `$(true)`, `$(echo $var)` by:
/// 1. Rejecting input with shell metacharacters (quotes, pipes, redirects, etc.)
/// 2. Splitting on whitespace into argv
/// 3. Expanding simple `$var` references
/// 4. Running the builtin and capturing output
///
/// Returns null if the input is too complex, falling through to slower paths.
///
/// ## Performance
/// ~1000x faster than forking - no process creation, no lexer/parser overhead.
/// Uses stack buffers to avoid heap allocation in the common case.
fn tryCaptureBuiltinDirect(allocator: std.mem.Allocator, state: *State, input: []const u8) ?[]const u8 {
    // Bail on any shell metacharacters that require full parsing
    for (input) |char| {
        switch (char) {
            '`', '|', '>', '<', '&', ';', '\n', '"', '\'', '(', ')', '{', '}', '*', '?', '[', '\\' => return null,
            else => {},
        }
    }

    // Split into argv on whitespace
    var argv_buf: [64][]const u8 = undefined;
    var expanded_storage: [2048]u8 = undefined;
    var storage_pos: usize = 0;
    var argc: usize = 0;

    var iter = std.mem.tokenizeAny(u8, input, " \t");
    while (iter.next()) |token| {
        if (argc >= argv_buf.len) return null; // Too many args

        // Expand simple $var in this token
        if (std.mem.indexOfScalar(u8, token, '$')) |_| {
            const remaining = expanded_storage[storage_pos..];
            const expanded = expandSimpleVars(state, token, remaining) orelse return null;
            argv_buf[argc] = expanded_storage[storage_pos..][0..expanded.len];
            storage_pos += expanded.len;
        } else {
            argv_buf[argc] = token;
        }
        argc += 1;
    }

    if (argc == 0) return null;

    const argv = argv_buf[0..argc];

    // Check if it's a builtin (and not shadowed by a function)
    if (state.getFunction(argv[0]) != null) return null;
    if (!builtins.isBuiltin(argv[0])) return null;

    // Run the builtin with capture
    const result = captureBuiltin(allocator, state, argv) catch return null;
    return result.output;
}

/// Expand simple $var references in a string.
/// Returns null if the string contains complex syntax we can't handle.
/// Uses a stack buffer to avoid allocation.
fn expandSimpleVars(state: *State, input: []const u8, buf: []u8) ?[]const u8 {
    var out_pos: usize = 0;
    var i: usize = 0;

    while (i < input.len) {
        if (input[i] == '$') {
            // Check for complex syntax we can't handle
            if (i + 1 >= input.len) {
                // Trailing $, just copy it
                if (out_pos >= buf.len) return null;
                buf[out_pos] = '$';
                out_pos += 1;
                i += 1;
                continue;
            }

            const next = input[i + 1];
            if (next == '(' or next == '{') {
                // Complex syntax like $() or ${} - bail
                return null;
            }

            // Simple variable: $name
            const var_start = i + 1;
            var var_end = var_start;
            while (var_end < input.len and isVarChar(input[var_end])) {
                var_end += 1;
            }

            if (var_end == var_start) {
                // Just a lone $, copy it
                if (out_pos >= buf.len) return null;
                buf[out_pos] = '$';
                out_pos += 1;
                i += 1;
                continue;
            }

            const var_name = input[var_start..var_end];
            const var_value = state.getVar(var_name) orelse "";

            // Copy variable value to output
            if (out_pos + var_value.len > buf.len) return null;
            @memcpy(buf[out_pos..][0..var_value.len], var_value);
            out_pos += var_value.len;
            i = var_end;
        } else {
            if (out_pos >= buf.len) return null;
            buf[out_pos] = input[i];
            out_pos += 1;
            i += 1;
        }
    }

    return buf[0..out_pos];
}

fn isVarChar(char: u8) bool {
    return std.ascii.isAlphanumeric(char) or char == '_';
}

// =============================================================================
// Full Capture Path (with parsing)
// =============================================================================

/// Result of successfully parsing and expanding a simple builtin command.
const CapturedBuiltin = struct {
    expanded: ExpandedSimple,
    arena: std.heap.ArenaAllocator,
};

/// Full capture path for builtin commands.
///
/// Handles patterns like `$(echo "hello $name")` and `$(type git)` by:
/// 1. Lexing and parsing the input through the full pipeline
/// 2. Validating it's a single, simple command (no pipelines, no background)
/// 3. Expanding variables, globs, and other shell expansions
/// 4. Running the builtin and capturing output in-process
///
/// Returns null if parsing fails or the command is too complex (external command,
/// pipeline, etc.), falling through to the fork path.
///
/// ## Performance
/// ~100x faster than forking - runs the builtin in-process, avoiding fork/exec.
/// Slower than direct paths due to full lex/parse/expand overhead.
fn tryCaptureBuiltinFull(backing: std.mem.Allocator, state: *State, input: []const u8) ?CapturedBuiltin {
    var arena = std.heap.ArenaAllocator.init(backing);
    const alloc = arena.allocator();

    // Parse input and extract command statement
    const parsed = interpreter.parseInput(alloc, input) catch {
        arena.deinit();
        return null;
    };
    const cmd_stmt = getSimpleCommandStatement(parsed) orelse {
        arena.deinit();
        return null;
    };

    // Use shared builtin detection/expansion
    const expanded = tryExpandSimpleBuiltin(alloc, state, cmd_stmt) orelse {
        arena.deinit();
        return null;
    };

    return .{ .expanded = expanded, .arena = arena };
}

/// Extract a simple command statement from a parsed program, or null if too complex.
/// This does structural validation before the more expensive expansion step.
fn getSimpleCommandStatement(parsed: interpreter.ParsedInput) ?ast.CommandStatement {
    if (parsed.ast.statements.len != 1) return null;
    const stmt = parsed.ast.statements[0];
    if (stmt != .command) return null;

    const cmd = stmt.command;
    // Reject background commands - they can't be captured in-process
    if (cmd.background) return null;

    return cmd;
}

/// A simple expanded command (single command, no pipes, no redirects).
/// Contains both the command and the backing slice (for cleanup).
pub const ExpandedSimple = struct {
    cmd: ExpandedCommand,
    slice: []const ExpandedCommand,

    pub fn deinit(self: ExpandedSimple, allocator: std.mem.Allocator) void {
        freeExpandedSlice(allocator, self.slice);
    }
};

/// Try to expand a CommandStatement as a simple command (single, no pipes, no redirects).
/// Returns null if the statement is too complex. Caller must check if the result
/// is a builtin, function, or external command as needed.
fn tryExpandSimpleCommand(allocator: std.mem.Allocator, state: *State, stmt: ast.CommandStatement) ?ExpandedSimple {
    // Must be: single chain, single command, no redirects
    if (stmt.chains.len != 1) return null;
    const chain = stmt.chains[0];
    if (chain.pipeline.commands.len != 1) return null;
    if (chain.pipeline.commands[0].redirects.len != 0) return null;

    // Expand the command
    var ctx = expand.ExpandContext.init(allocator, state);
    defer ctx.deinit();
    const expanded = expansion_pipeline.expandPipeline(allocator, &ctx, chain.pipeline) catch return null;

    // Verify: single command, has argv, no redirects after expansion
    if (expanded.len != 1 or expanded[0].args.len == 0 or expanded[0].redirects.len != 0) {
        freeExpandedSlice(allocator, expanded);
        return null;
    }

    return .{ .cmd = expanded[0], .slice = expanded };
}

/// Try to expand a CommandStatement as a simple builtin (single command, no redirects).
///
/// Returns null if the statement is too complex for in-process capture:
/// - Multiple chains (&&, ||)
/// - Pipelines (|)
/// - Redirects (>, <, etc.)
/// - External commands (not a builtin)
///
/// On success, caller owns the returned ExpandedSimple and must call deinit().
pub fn tryExpandSimpleBuiltin(allocator: std.mem.Allocator, state: *State, stmt: ast.CommandStatement) ?ExpandedSimple {
    const result = tryExpandSimpleCommand(allocator, state, stmt) orelse return null;
    const name = result.cmd.args[0];

    // Must be a builtin, not shadowed by a function
    if (state.getFunction(name) != null or !builtins.isBuiltin(name)) {
        result.deinit(allocator);
        return null;
    }

    // eval and source execute arbitrary code - must use fork-based capture
    if (std.mem.eql(u8, name, "eval") or std.mem.eql(u8, name, "source")) {
        result.deinit(allocator);
        return null;
    }

    return result;
}

fn freeExpandedSlice(allocator: std.mem.Allocator, cmds: []const ExpandedCommand) void {
    for (cmds) |cmd| {
        allocator.free(cmd.args);
        allocator.free(cmd.env);
        allocator.free(cmd.redirects);
    }
    allocator.free(cmds);
}

// =============================================================================
// Direct External Exec (avoids double-fork)
// =============================================================================

/// Try to exec a simple external command directly from the capture child.
///
/// Called inside the forked capture child. If the input is a simple external
/// command (single command, no pipes, no redirects), we exec it directly
/// instead of going through the full interpreter (which would fork again).
///
/// This eliminates the "double-fork" overhead for simple external command
/// substitutions like `$(date +%s)` or `$(uname)`.
///
/// If the command is complex (pipeline, redirects, builtin, function) or
/// parsing fails, this function returns and the caller falls back to the
/// full interpreter.
fn tryExecExternalDirect(allocator: std.mem.Allocator, state: *State, input: []const u8) void {
    // Parse and validate it's a simple command statement
    const parsed = interpreter.parseInput(allocator, input) catch return;
    const cmd_stmt = getSimpleCommandStatement(parsed) orelse return;

    // Nested capture not supported in direct path
    if (cmd_stmt.capture != null) return;

    // Expand to a simple command
    const result = tryExpandSimpleCommand(allocator, state, cmd_stmt) orelse return;
    const name = result.cmd.args[0];

    // Only exec external commands - builtins/functions handled by interpreter
    if (builtins.isBuiltin(name) or state.getFunction(name) != null) {
        result.deinit(allocator);
        return;
    }

    // It's a simple external command - exec directly (does not return)
    pipeline.execCommand(allocator, result.cmd);
}

// =============================================================================
// In-Process Builtin Capture
// =============================================================================

/// Capture a builtin's stdout output in-process without forking.
///
/// This is the fastest capture path - pure in-memory with zero syscalls
/// and zero heap allocations. Reuses Writer for buffering (no separate impl).
///
/// Only works for builtins (not external commands or pipelines).
pub fn captureBuiltin(allocator: std.mem.Allocator, state: *State, args: []const []const u8) !CaptureResult {
    // Use a Writer as the capture buffer - same type builtins use internally
    var w = io.Writer{};

    // Enable capture mode, saving previous state for nested captures
    const prev = io.startCapture(&w);
    defer io.endCapture(prev);

    // Run the builtin (output goes to Writer)
    const status = builtins.tryRun(state, args) orelse 1;

    // Get captured output and trim trailing newlines
    const trimmed = std.mem.trimRight(u8, w.buf[0..w.pos], "\n");
    return .{ .output = try allocator.dupe(u8, trimmed), .status = status };
}

// =============================================================================
// Fork-Based Capture
// =============================================================================

/// Result of forkWithPipe - tells caller which process they're in.
pub const ForkResult = union(enum) {
    /// We're in the child - stdout and stderr are redirected to the pipe.
    /// Run your code and call std.posix.exit() when done.
    child: void,

    /// We're in the parent. Use readAndWait() to get the captured output.
    parent: ParentHandle,
};

pub const ParentHandle = struct {
    read_fd: std.posix.fd_t,
    child_pid: std.posix.pid_t,

    /// Read all output from the child and wait for it to exit.
    /// Returns the captured output (trimmed) and exit status.
    pub fn readAndWait(self: ParentHandle, allocator: std.mem.Allocator) !CaptureResult {
        var output: std.ArrayListUnmanaged(u8) = .empty;
        defer output.deinit(allocator);

        var buf: [4096]u8 = undefined;
        while (true) {
            const n = std.posix.read(self.read_fd, &buf) catch break;
            if (n == 0) break;
            try output.appendSlice(allocator, buf[0..n]);
        }
        std.posix.close(self.read_fd);

        // Wait for child
        var status: c_int = 0;
        _ = c.waitpid(self.child_pid, &status, 0);

        const exit_status: u8 = if (std.posix.W.IFEXITED(@bitCast(status)))
            std.posix.W.EXITSTATUS(@bitCast(status))
        else
            1;

        // Trim trailing newlines
        const trimmed = std.mem.trimRight(u8, output.items, "\n");
        const owned_output = try allocator.dupe(u8, trimmed);

        return CaptureResult{
            .output = owned_output,
            .status = exit_status,
        };
    }
};

/// Fork a child process with stdout and stderr redirected to a pipe.
///
/// Returns `.child` in the child process (stdout and stderr already redirected),
/// or `.parent` with a handle to read the output and wait.
///
/// Usage:
/// ```zig
/// switch (try forkWithPipe()) {
///     .child => {
///         // Run code that writes to stdout/stderr
///         const status = doWork();
///         std.posix.exit(status);
///     },
///     .parent => |handle| {
///         const result = try handle.readAndWait(allocator);
///         // result.output contains captured stdout and stderr
///         // result.status contains exit code
///     },
/// }
/// ```
pub fn forkWithPipe() !ForkResult {
    const pipe_fds = try std.posix.pipe();
    const read_fd = pipe_fds[0];
    const write_fd = pipe_fds[1];

    const pid = try std.posix.fork();
    if (pid == 0) {
        // Child: redirect stdout and stderr to pipe
        std.posix.close(read_fd);
        std.posix.dup2(write_fd, std.posix.STDOUT_FILENO) catch std.posix.exit(1);
        std.posix.dup2(write_fd, std.posix.STDERR_FILENO) catch std.posix.exit(1);
        std.posix.close(write_fd);
        return .child;
    }

    // Parent: close write end, return handle for reading
    std.posix.close(write_fd);
    return .{ .parent = .{
        .read_fd = read_fd,
        .child_pid = pid,
    } };
}
