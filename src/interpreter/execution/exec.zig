//! Statement execution: the main orchestrator for running shell statements
//!
//! This module handles high-level execution flow:
//! - Statement dispatch (commands, functions, control flow)
//! - Control flow statements (if, for, each, while, break, continue)
//! - Background jobs and output capture
//!
//! Pipeline execution and job control are delegated to separate modules.

const std = @import("std");
const expansion_types = @import("../expansion/expanded.zig");
const state_mod = @import("../../runtime/state.zig");
const State = state_mod.State;
const Scope = @import("../../runtime/scope.zig").Scope;
const builtins = @import("../../runtime/builtins.zig");
const io = @import("../../terminal/io.zig");
const interpreter_mod = @import("../interpreter.zig");
const expand = @import("../expansion/word.zig");
const lexer_mod = @import("../../language/lexer.zig");
const expansion_statement = @import("../expansion/statement.zig");

// Delegate to specialized modules
const jobs = @import("jobs.zig");
const pipeline = @import("pipeline.zig");
const capture_mod = @import("capture.zig");

const ast = @import("../../language/ast.zig");
const ExpandedCmd = expansion_types.ExpandedCmd;
const posix = jobs.posix;

// =============================================================================
// Re-exports
// =============================================================================

pub const initJobControl = jobs.initJobControl;
pub const initSignals = jobs.initSignals;
pub const continueJobForeground = jobs.continueJobForeground;
pub const continueJobBackground = jobs.continueJobBackground;

// =============================================================================
// Public API
// =============================================================================

/// Execute a parsed program
pub fn execute(allocator: std.mem.Allocator, state: *State, prog: ast.Program, cmd_str: []const u8) !u8 {
    var last_status: u8 = 0;

    for (prog.statements) |stmt| {
        last_status = try executeStatement(allocator, state, stmt, cmd_str);
        // Check if exit was requested
        if (state.should_exit) {
            return state.exit_code;
        }
    }

    return last_status;
}

pub fn executeStatement(allocator: std.mem.Allocator, state: *State, stmt: ast.Statement, cmd_str: []const u8) !u8 {
    return switch (stmt) {
        .command => |cmd_stmt| try executeCmdStatement(allocator, state, cmd_stmt, cmd_str),
        .function => |fun_def| {
            // Register the function in state
            try state.setFunction(fun_def.name, fun_def.body);
            return 0;
        },
        .@"if" => |if_stmt| executeIfStatement(allocator, state, if_stmt),
        .each => |each_stmt| executeEachStatement(allocator, state, each_stmt),
        .@"while" => |while_stmt| executeWhileStatement(allocator, state, while_stmt),
        .@"break" => {
            state.loop_break = true;
            return 0;
        },
        .@"continue" => {
            state.loop_continue = true;
            return 0;
        },
        .@"return" => |opt_status_str| {
            state.fn_return = true;
            if (opt_status_str) |status_str| {
                // Tokenize, expand, and parse the status value at runtime
                var lexer = lexer_mod.Lexer.init(allocator, status_str);
                const tokens = lexer.tokenize() catch {
                    io.printError("return: invalid argument\n", .{});
                    state.setStatus(1);
                    return 1;
                };
                if (tokens.len > 0 and tokens[0].kind == .word) {
                    var expand_ctx = expand.ExpandContext.init(allocator, state);
                    defer expand_ctx.deinit();
                    const expanded = expand.expandWord(&expand_ctx, tokens[0].kind.word) catch {
                        io.printError("return: expansion error\n", .{});
                        state.setStatus(1);
                        return 1;
                    };
                    if (expanded.len > 0) {
                        const parsed = std.fmt.parseInt(u8, expanded[0], 10) catch blk: {
                            io.printError("return: {s}: numeric argument required\n", .{expanded[0]});
                            break :blk 1;
                        };
                        state.setStatus(parsed);
                    }
                }
            }
            // If no argument, status is already the last command's exit status
            return state.status;
        },
        .@"defer" => |cmd_stmt| {
            // Push the pre-parsed command onto the defer stack (will be executed LIFO on function exit)
            state.pushDefer(cmd_stmt) catch {
                return 1;
            };
            return 0;
        },
        .exit => |opt_status_str| {
            state.should_exit = true;
            if (opt_status_str) |status_str| {
                // Tokenize, expand, and parse the exit code at runtime
                var lexer = lexer_mod.Lexer.init(allocator, status_str);
                const tokens = lexer.tokenize() catch {
                    io.printError("exit: invalid argument\n", .{});
                    state.exit_code = 1;
                    return 1;
                };
                if (tokens.len > 0 and tokens[0].kind == .word) {
                    var expand_ctx = expand.ExpandContext.init(allocator, state);
                    defer expand_ctx.deinit();
                    const expanded = expand.expandWord(&expand_ctx, tokens[0].kind.word) catch {
                        io.printError("exit: expansion error\n", .{});
                        state.exit_code = 1;
                        return 1;
                    };
                    if (expanded.len > 0) {
                        const parsed = std.fmt.parseInt(u8, expanded[0], 10) catch blk: {
                            io.printError("exit: {s}: numeric argument required\n", .{expanded[0]});
                            break :blk 1;
                        };
                        state.exit_code = parsed;
                    }
                }
            } else {
                // If no argument, use the last command's exit status
                state.exit_code = state.status;
            }
            return state.exit_code;
        },
    };
}

// =============================================================================
// Control Flow
// =============================================================================

/// Execute a shell body string, catching errors and returning a status code.
/// Used by control flow statements (if, for) to break the error set cycle.
fn executeBody(allocator: std.mem.Allocator, state: *State, body: []const u8, context: []const u8) u8 {
    return interpreter_mod.execute(allocator, state, body) catch |err| {
        io.printError("{s}: {}\n", .{ context, err });
        return 1;
    };
}

const LoopSignal = enum { break_, continue_, ret, interrupt };

fn consumeLoopSignal(state: *State) ?LoopSignal {
    if (state.fn_return) return .ret;
    // Note: interrupted is NOT cleared here - it propagates through all loops
    // and is only cleared by the REPL after command execution completes
    if (state.interrupted) return .interrupt;
    if (state.loop_break) {
        state.loop_break = false;
        return .break_;
    }
    if (state.loop_continue) {
        state.loop_continue = false;
        return .continue_;
    }
    return null;
}

/// Execute an if statement by evaluating condition branches and running appropriate branch.
/// Each branch body gets its own scope - variables created inside are block-local.
/// Uses scope pooling to avoid allocation overhead in hot loops.
fn executeIfStatement(allocator: std.mem.Allocator, state: *State, if_stmt: expansion_types.ast.IfStatement) u8 {
    // Try each branch in order (first is "if", rest are "else if")
    for (if_stmt.branches) |branch| {
        // Execute the pre-parsed condition directly
        const cond_status = executeSimpleCommand(allocator, state, branch.condition) catch |err| {
            io.printError("if: condition error: {}\n", .{err});
            return 1;
        };

        // Check for return during condition evaluation
        if (state.fn_return) return state.status;

        // Exit status 0 means true (success) - execute this branch's body
        if (cond_status == 0) {
            // Acquire a scope from pool (or allocate if pool empty)
            _ = state.acquireScope() catch {
                io.printError("if: out of memory\n", .{});
                return 1;
            };
            defer state.releaseScope(); // Returns scope to pool for reuse

            const body_status = executeBody(allocator, state, branch.body, "if: body error");
            // fn_return propagates automatically since we return the status
            return body_status;
        }
    }

    // No branch condition was true - try else body if present
    if (if_stmt.else_body) |else_body| {
        // Acquire a scope from pool for else branch
        _ = state.acquireScope() catch {
            io.printError("if: out of memory\n", .{});
            return 1;
        };
        defer state.releaseScope();

        return executeBody(allocator, state, else_body, "if: else error");
    }

    return 0;
}

/// Execute an each loop (for is an alias).
///
/// Sets $item (or custom var) and $index (1-based) on each iteration.
/// Loop variables are scoped to the loop body and automatically cleaned up.
///
/// OPTIMIZATION: Uses scope-based variable management with arena reset per iteration.
/// - Loop scope is pushed once, reset each iteration (O(1) variable cleanup)
/// - Body AST is parsed once and reused for all iterations
/// - Arena allocations are amortized across iterations
fn executeEachStatement(allocator: std.mem.Allocator, state: *State, stmt: expansion_types.ast.EachStatement) u8 {
    // Parse arena - lives for entire loop, holds cached body AST and expanded items
    var parse_arena = std.heap.ArenaAllocator.init(allocator);
    defer parse_arena.deinit();
    const parse_alloc = parse_arena.allocator();

    // Expand items_source into a list of strings (lives for loop duration)
    const items = expandItems(parse_alloc, state, stmt.items_source) orelse return 1;

    // Parse body once (cached for all iterations)
    const body_ast = interpreter_mod.parseInput(parse_alloc, stmt.body) catch |err| {
        io.printError("each: body parse error: {}\n", .{err});
        return 1;
    };

    // Push a scope for loop variables - they're automatically cleaned up on pop
    const loop_scope = state.pushScope() catch {
        io.printError("each: out of memory\n", .{});
        return 1;
    };
    defer state.popScope();

    // Execute body for each item
    var index_buf: [20]u8 = undefined;
    var last_status: u8 = 0;

    for (items, 0..) |item, i| {
        // Reset scope arena for this iteration (O(1) cleanup of previous iteration's vars)
        loop_scope.reset();

        // Set loop variables in loop scope (arena-allocated, freed on reset)
        loop_scope.setLocalScalar(stmt.variable, item) catch |err| {
            io.printError("each: set var error: {}\n", .{err});
            return 1;
        };

        // Set $index (1-based, matching Oshen's 1-based array indexing)
        const index_str = std.fmt.bufPrint(&index_buf, "{d}", .{i + 1}) catch unreachable;
        loop_scope.setLocalScalar("index", index_str) catch |err| {
            io.printError("each: set index error: {}\n", .{err});
            return 1;
        };

        last_status = interpreter_mod.executeAstWithArena(loop_scope.allocator(), state, body_ast) catch |err| {
            io.printError("each: body error: {}\n", .{err});
            return 1;
        };

        if (consumeLoopSignal(state)) |signal| switch (signal) {
            .ret => return state.status,
            .break_ => break,
            .continue_ => continue,
            .interrupt => return 130,
        };
    }

    return last_status;
}

/// Expand items_source into a list of strings. Returns null on error.
fn expandItems(arena_alloc: std.mem.Allocator, state: *State, items_source: []const u8) ?[]const []const u8 {
    var items: std.ArrayListUnmanaged([]const u8) = .empty;

    var lexer = lexer_mod.Lexer.init(arena_alloc, items_source);
    const tokens = lexer.tokenize() catch |err| {
        io.printError("each: items parse error: {}\n", .{err});
        return null;
    };

    var expand_ctx = expand.ExpandContext.init(arena_alloc, state);
    defer expand_ctx.deinit();

    for (tokens) |tok| {
        if (tok.kind == .word) {
            const expanded = expand.expandWord(&expand_ctx, tok.kind.word) catch |err| {
                io.printError("each: expand error: {}\n", .{err});
                return null;
            };
            for (expanded) |word| {
                items.append(arena_alloc, word) catch |err| {
                    io.printError("each: append error: {}\n", .{err});
                    return null;
                };
            }
        }
    }

    return items.items;
}

/// Execute a while loop by repeatedly checking condition and running body.
///
/// The condition is pre-parsed at parse time. The body is parsed once on first
/// iteration, then re-executed each iteration (only expansion happens per-iteration).
///
/// OPTIMIZATION: Uses scope-based variable management with arena reset per iteration.
/// - Loop scope is pushed once, reset each iteration (O(1) cleanup)
/// - Body AST is parsed once and reused for all iterations
/// - Variables created in loop body are scoped to the loop
fn executeWhileStatement(allocator: std.mem.Allocator, state: *State, while_stmt: expansion_types.ast.WhileStatement) u8 {
    // Parse arena - lives for entire loop duration, holds the cached body AST
    var parse_arena = std.heap.ArenaAllocator.init(allocator);
    defer parse_arena.deinit();
    const parse_alloc = parse_arena.allocator();

    // Parse body once upfront
    const body_parsed = interpreter_mod.parseInput(parse_alloc, while_stmt.body) catch |err| {
        io.printError("while: body parse error: {}\n", .{err});
        return 1;
    };

    // Push a scope for loop body - variables created inside are block-local
    const loop_scope = state.pushScope() catch {
        io.printError("while: out of memory\n", .{});
        return 1;
    };
    defer state.popScope();

    var last_status: u8 = 0;

    while (true) {
        // Reset scope arena for this iteration (O(1) cleanup of previous iteration's vars)
        loop_scope.reset();

        // Execute the pre-parsed condition using the loop scope's arena
        const cond_status = executeSimpleCommand(loop_scope.allocator(), state, while_stmt.condition) catch |err| {
            io.printError("while: condition error: {}\n", .{err});
            return 1;
        };

        // Check for return during condition evaluation
        if (state.fn_return) return state.status;

        // Exit status 0 means true (continue), non-zero means false (stop)
        if (cond_status != 0) break;

        // Execute pre-parsed body using the loop scope's arena
        last_status = interpreter_mod.executeAstWithArena(loop_scope.allocator(), state, body_parsed) catch |err| {
            io.printError("while: body error: {}\n", .{err});
            return 1;
        };

        if (consumeLoopSignal(state)) |signal| switch (signal) {
            .ret => return state.status,
            .break_ => break,
            .continue_ => continue,
            .interrupt => return 130,
        };
    }

    return last_status;
}

// =============================================================================
// Command Execution
// =============================================================================

/// Execute a simple command (no background, no capture) and return the exit status.
/// Used for if/while conditions and defer statements.
fn executeSimpleCommand(allocator: std.mem.Allocator, state: *State, stmt: ast.CommandStatement) !u8 {
    var last_status: u8 = 0;
    var should_continue = true;

    for (stmt.chains) |chain| {
        // Check conditional logic using explicit operator enum
        switch (chain.op) {
            .none => {},
            .@"and" => if (last_status != 0) {
                should_continue = false;
            },
            .@"or" => if (last_status == 0) {
                should_continue = false;
            },
        }

        if (!should_continue) {
            should_continue = true; // Reset for next chain
            continue;
        }

        // Expand pipeline with current state/cwd
        const expanded_cmds = try expandPipeline(allocator, state, chain.pipeline);
        defer freeCommands(allocator, expanded_cmds);

        last_status = try pipeline.executePipelineForeground(allocator, state, expanded_cmds, &tryRunFunction);
    }

    state.setStatus(last_status);
    return last_status;
}

fn executeCmdStatement(allocator: std.mem.Allocator, state: *State, stmt: expansion_types.ast.CommandStatement, cmd_str: []const u8) !u8 {
    // For background jobs, we run the pipeline in a process group
    if (stmt.background) {
        return executeBackgroundJob(allocator, state, stmt, cmd_str);
    }

    // Handle capture: redirect stdout to a pipe and read the output
    if (stmt.capture) |capture| {
        return executeCommandWithCapture(allocator, state, stmt, capture);
    }

    // Foreground execution - delegate to simple command executor
    return executeSimpleCommand(allocator, state, stmt);
}

/// Free memory allocated for expanded commands
fn freeCommands(allocator: std.mem.Allocator, cmds: []const ExpandedCmd) void {
    for (cmds) |cmd| {
        allocator.free(cmd.argv);
        allocator.free(cmd.env);
        allocator.free(cmd.redirects);
    }
    allocator.free(cmds);
}

/// Expand a pipeline with current state
fn expandPipeline(allocator: std.mem.Allocator, state: *State, ast_pipeline: expansion_types.ast.Pipeline) ![]const ExpandedCmd {
    var ctx = expand.ExpandContext.init(allocator, state);
    defer ctx.deinit();
    return expansion_statement.expandPipeline(allocator, &ctx, ast_pipeline);
}

/// Expand a pipeline in a child process context (exits on error instead of returning)
fn expandPipelineInChild(allocator: std.mem.Allocator, state: *State, ast_pipeline: expansion_types.ast.Pipeline) []const ExpandedCmd {
    var ctx = expand.ExpandContext.init(allocator, state);
    defer ctx.deinit();
    return expansion_statement.expandPipeline(allocator, &ctx, ast_pipeline) catch {
        std.posix.exit(1);
    };
}

/// Execute a command statement with output capture (=> or =>@).
///
/// For simple builtins, captures output in-process (~100x faster).
/// For external commands or pipelines, forks a child process.
fn executeCommandWithCapture(allocator: std.mem.Allocator, state: *State, command: expansion_types.ast.CommandStatement, capture: expansion_types.Capture) !u8 {
    // Fast path: single builtin without redirects
    if (capture_mod.tryExpandSimpleBuiltin(allocator, state, command)) |expanded| {
        defer expanded.deinit(allocator);
        const result = try capture_mod.captureBuiltin(allocator, state, expanded.cmd);
        defer allocator.free(result.output);
        try capture_mod.storeCapture(allocator, state, result.output, capture.variable, capture.mode == .lines);
        state.setStatus(result.status);
        return result.status;
    }

    // Slow path: fork for external commands, pipelines, or complex cases
    const result = switch (try capture_mod.forkWithPipe()) {
        .child => {
            var last_status: u8 = 0;
            for (command.chains) |chain| {
                const expanded = expandPipelineInChild(allocator, state, chain.pipeline);
                if (expanded.len == 1 and expanded[0].argv.len > 0) {
                    if (builtins.tryRun(state, expanded[0])) |s| {
                        last_status = s;
                        continue;
                    }
                }
                last_status = pipeline.executePipelineInChild(allocator, state, expanded, &tryRunFunction) catch 1;
            }
            std.posix.exit(last_status);
        },
        .parent => |handle| try handle.readAndWait(allocator),
    };
    defer allocator.free(result.output);

    try capture_mod.storeCapture(allocator, state, result.output, capture.variable, capture.mode == .lines);
    state.setStatus(result.status);
    return result.status;
}

fn executeBackgroundJob(allocator: std.mem.Allocator, state: *State, stmt: expansion_types.ast.CommandStatement, cmd_str: []const u8) !u8 {
    // Fork a child to be the process group leader
    const pid = try std.posix.fork();

    if (pid == 0) {
        // Child: create new process group with self as leader
        _ = posix.setpgid(0, 0);

        // Reset signal handlers to default in child
        jobs.resetSignalsToDefault();

        // Execute the statement chains
        var last_status: u8 = 0;
        for (stmt.chains) |chain| {
            // Expand pipeline (exits on error)
            const expanded_cmds = expandPipelineInChild(allocator, state, chain.pipeline);

            last_status = pipeline.executePipelineInChild(allocator, state, expanded_cmds, &tryRunFunction) catch 1;
        }
        std.posix.exit(last_status);
    }

    // Parent: set process group (race with child doing same)
    _ = posix.setpgid(pid, pid);

    // Add to job table
    const pids = try allocator.alloc(std.posix.pid_t, 1);
    errdefer allocator.free(pids);
    pids[0] = pid;
    const job_id = state.jobs.add(pid, pids, cmd_str, .running) catch {
        io.printStdout("[bg] {d}\n", .{pid});
        state.setStatus(0);
        return 0;
    };

    io.printStdout("[{d}] {d}\n", .{ job_id, pid });
    state.setStatus(0);
    return 0;
}

// =============================================================================
// Functions
// =============================================================================

/// Execute deferred commands from a given index in LIFO order.
/// This allows nested functions to only run their own defers.
fn runDeferredCommandsFromIndex(allocator: std.mem.Allocator, state: *State, from_index: usize) void {
    // Pop and execute in reverse order (LIFO), but only commands added after from_index
    while (state.deferred.items.len > from_index) {
        const cmd_stmt = state.popDeferred().?;
        // Execute the pre-parsed simple command directly (no re-parsing needed)
        _ = executeSimpleCommand(allocator, state, cmd_stmt) catch {};
    }
}

fn runFunctionWithArgs(allocator: std.mem.Allocator, state: *State, cmd: ExpandedCmd) ?u8 {
    if (cmd.argv.len == 0) return null;

    const name = cmd.argv[0];
    const func = state.getFunction(name) orelse return null;

    // Remember how many deferred commands exist before this function
    const defer_count_before = state.deferred.items.len;

    // Acquire a function scope from pool (or allocate if pool empty)
    // This is much faster than pushScope() in hot loops
    const fn_scope = state.acquireScope() catch {
        io.printError("function {s}: out of memory\n", .{name});
        return 1;
    };

    // Guaranteed cleanup: release scope to pool and run defers on all exit paths
    defer state.releaseScope();
    defer runDeferredCommandsFromIndex(allocator, state, defer_count_before);

    // Set $argv in the function scope (automatically restored when scope is released)
    fn_scope.setLocalList("argv", if (cmd.argv.len > 1) cmd.argv[1..] else &.{}) catch {
        io.printError("function {s}: out of memory\n", .{name});
        return 1;
    };

    // Get cached parse or parse on first call, then execute
    const parsed = func.getParsed() catch |err| {
        io.printError("function {s}: {}\n", .{ name, err });
        state.fn_return = false;
        return 1;
    };

    const status = interpreter_mod.executeAst(allocator, state, parsed) catch |err| {
        io.printError("function {s}: {}\n", .{ name, err });
        state.fn_return = false;
        return 1;
    };

    // Check if exit was requested - propagate without clearing flag
    if (state.should_exit) {
        return state.exit_code;
    }

    if (state.fn_return) {
        state.fn_return = false;
        return state.status;
    }
    return status;
}

/// Try to execute a user-defined function.
/// Returns the exit status if it was a function, null otherwise.
/// Order: builtins > functions > external commands
///
/// Note: This function catches errors internally and returns a status code
/// to break the error set cycle (exec → pipeline → exec).
fn tryRunFunction(allocator: std.mem.Allocator, state: *State, cmd: ExpandedCmd) ?u8 {
    return runFunctionWithArgs(allocator, state, cmd);
}
