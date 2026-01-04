//! Shell runtime state
//!
//! Central state management for the shell including variables, exports,
//! functions, aliases, job control, and working directory.
//!
//! Variables use a scope chain for proper lexical scoping:
//! - Variables defined in blocks (if/while/each/fun) are block-local
//! - Setting an existing variable updates it in the scope where it was defined
//! - Setting a new variable creates it in the current (innermost) scope

const std = @import("std");
const jobs = @import("jobs.zig");
const env = @import("env.zig");
const interpreter = @import("../interpreter/interpreter.zig");
const ast = @import("../language/ast.zig");
const Scope = @import("scope.zig").Scope;
const ScopeValue = @import("scope.zig").Value;

pub const JobTable = jobs.JobTable;
pub const Job = jobs.Job;
pub const JobStatus = jobs.JobStatus;

/// Maximum number of scopes to keep in the pool (see releaseScope).
const max_pooled_scopes = 16;

/// A user-defined function with lazy-parsed AST caching.
pub const Function = struct {
    /// The raw source text of the function body (owned by State's allocator)
    source: []const u8,

    /// Cached parse result; arena owns the AST memory, backed by page_allocator for stability
    cached: ?struct {
        arena: std.heap.ArenaAllocator,
        parsed: interpreter.ParsedInput,
    } = null,

    /// Get the parsed AST, parsing on first call.
    /// Uses page_allocator for the arena to ensure it outlives any execution arenas.
    pub fn getParsed(self: *Function) !interpreter.ParsedInput {
        if (self.cached) |c| return c.parsed;

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const parsed = try interpreter.parseInput(arena.allocator(), self.source);
        self.cached = .{ .arena = arena, .parsed = parsed };
        return parsed;
    }

    /// Free all memory owned by this function.
    pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
        if (self.cached) |*c| c.arena.deinit();
        allocator.free(self.source);
    }
};

/// Shell state: variables, status, cwd, options
pub const State = struct {
    allocator: std.mem.Allocator,

    /// Last command exit status
    status: u8 = 0,

    /// Global scope for variables (always exists, never popped)
    global_scope: Scope,

    /// Current (innermost) scope for variable lookups and assignments
    current_scope: *Scope,

    /// Exported environment variables
    exports: std.StringHashMap([]const u8),

    /// User-defined functions (name -> Function with cached AST)
    functions: std.StringHashMap(Function),

    /// Aliases (name -> expansion text)
    aliases: std.StringHashMap([]const u8),

    /// Current working directory (cached)
    cwd: ?[]const u8 = null,

    /// Previous working directory (for `cd -`)
    prev_cwd: ?[]const u8 = null,

    /// Home directory
    home: ?[]const u8 = null,

    /// Job table for background/stopped jobs
    jobs: JobTable,

    /// Shell's process group ID (for terminal control)
    shell_pgid: std.posix.pid_t = 0,

    /// Terminal file descriptor (for tcsetpgrp)
    terminal_fd: std.posix.fd_t = std.posix.STDIN_FILENO,

    /// Whether we're an interactive shell
    interactive: bool = false,

    // =========================================================================
    // Control Flow Flags
    //
    // Five boolean flags signal the interpreter/REPL how to continue execution.
    // They form a hierarchy from narrowest to broadest scope:
    //
    // | Flag            | Scope                           | Triggered By       |
    // |-----------------|---------------------------------|--------------------|
    // | `loop_continue` | One iteration                   | `continue`         |
    // | `loop_break`    | One loop                        | `break`            |
    // | `fn_return`     | One function (all loops inside) | `return`           |
    // | `interrupted`   | One command (all functions)     | Ctrl+C (SIGINT)    |
    // | `should_exit`   | Entire shell                    | `exit` builtin     |
    // =========================================================================

    loop_break: bool = false,
    loop_continue: bool = false,
    fn_return: bool = false,
    interrupted: bool = false,
    should_exit: bool = false,
    exit_code: u8 = 0,

    /// Stack of deferred command source strings (LIFO execution order).
    /// Stores source text that is parsed on-demand during execution.
    /// Each string is allocated with state.allocator for long-term storage.
    deferred: std.ArrayListUnmanaged([]const u8),

    /// Pool of reusable scopes to avoid allocation overhead in hot loops.
    /// When a scope is released, it's reset and added here for reuse.
    /// This dramatically improves performance for if statements and function calls in loops.
    scope_pool: std.ArrayListUnmanaged(*Scope),

    // =========================================================================
    // Initialization
    // =========================================================================

    pub fn init(allocator: std.mem.Allocator) State {
        var state = State{
            .allocator = allocator,
            .global_scope = Scope.init(null, allocator),
            .current_scope = undefined, // Set by caller via initCurrentScope()
            .exports = std.StringHashMap([]const u8).init(allocator),
            .functions = std.StringHashMap(Function).init(allocator),
            .aliases = std.StringHashMap([]const u8).init(allocator),
            .jobs = JobTable.init(allocator),
            .deferred = .empty,
            .scope_pool = .empty,
        };

        // Initialize HOME from environment
        if (env.getHome()) |home| {
            state.home = home;
        }

        return state;
    }

    /// Must be called after init() once the State is in its final location.
    /// This fixes up the self-referential current_scope pointer.
    pub fn initCurrentScope(self: *State) void {
        self.current_scope = &self.global_scope;
    }

    pub fn deinit(self: *State) void {
        // Pop any remaining child scopes (shouldn't happen in normal operation)
        while (self.current_scope != &self.global_scope) {
            const old_scope = self.current_scope;
            self.current_scope = old_scope.parent orelse &self.global_scope;
            old_scope.deinit();
            self.allocator.destroy(old_scope);
        }

        // Free global scope's arena (but not the struct itself - it's embedded in State)
        self.global_scope.deinit();

        // Free all export keys and values
        var exp_iter = self.exports.iterator();
        while (exp_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.exports.deinit();

        // Free all function names and cached parse state
        var fn_iter = self.functions.iterator();
        while (fn_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.functions.deinit();

        // Free all alias names and expansions
        var alias_iter = self.aliases.iterator();
        while (alias_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.aliases.deinit();

        if (self.cwd) |cwd| {
            self.allocator.free(cwd);
        }

        if (self.prev_cwd) |prev| {
            self.allocator.free(prev);
        }

        // Clean up jobs
        self.jobs.deinit();

        // Free deferred command sources (they were duplicated into state.allocator)
        for (self.deferred.items) |source| {
            self.allocator.free(source);
        }
        self.deferred.deinit(self.allocator);

        // Free pooled scopes
        for (self.scope_pool.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.scope_pool.deinit(self.allocator);
    }

    // =========================================================================
    // Scope Management
    // =========================================================================

    /// Push a new scope for a block (loop, function, if statement).
    /// Returns a pointer to the new scope for direct manipulation.
    pub fn pushScope(self: *State) !*Scope {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.current_scope, self.allocator);
        self.current_scope = new_scope;
        return new_scope;
    }

    /// Pop the current scope, restoring the parent as current.
    /// Frees all memory allocated in the popped scope.
    pub fn popScope(self: *State) void {
        const old_scope = self.current_scope;

        // Don't pop the global scope
        if (old_scope == &self.global_scope) return;

        // Restore parent as current
        self.current_scope = old_scope.parent orelse &self.global_scope;

        // Free the old scope
        old_scope.deinit();
        self.allocator.destroy(old_scope);
    }

    /// Acquire a scope from the pool or allocate a new one.
    /// This is faster than pushScope() in hot loops because it reuses memory.
    /// The returned scope has its parent set to the current scope and becomes current.
    pub fn acquireScope(self: *State) !*Scope {
        if (self.scope_pool.items.len > 0) {
            // Reuse pooled scope - it's already been reset
            const scope = self.scope_pool.items[self.scope_pool.items.len - 1];
            self.scope_pool.items.len -= 1;
            scope.parent = self.current_scope;
            self.current_scope = scope;
            return scope;
        }
        // No pooled scope available, allocate a new one
        return self.pushScope();
    }

    /// Release a scope back to the pool for reuse.
    /// This is faster than popScope() because it avoids deallocation.
    /// The scope is reset (variables cleared) and added to the pool.
    pub fn releaseScope(self: *State) void {
        const old_scope = self.current_scope;

        // Don't release the global scope
        if (old_scope == &self.global_scope) return;

        // Restore parent as current
        self.current_scope = old_scope.parent orelse &self.global_scope;

        // Reset the scope for reuse (clears vars, retains memory)
        old_scope.reset();

        // Add to pool if there's room (limit pool size to avoid unbounded growth)
        if (self.scope_pool.items.len < max_pooled_scopes) {
            self.scope_pool.append(self.allocator, old_scope) catch {
                // Pool append failed, just free the scope
                old_scope.deinit();
                self.allocator.destroy(old_scope);
            };
        } else {
            // Pool is full, free the scope
            old_scope.deinit();
            self.allocator.destroy(old_scope);
        }
    }

    // =========================================================================
    // Variable Operations
    // =========================================================================

    /// Find the scope where a variable should be set:
    /// - If it exists anywhere in the chain, return that scope (for updates)
    /// - Otherwise, return the current scope (for new variables)
    fn targetScope(self: *State, name: []const u8) *Scope {
        return self.current_scope.findScope(name) orelse self.current_scope;
    }

    /// Get a variable value as a string (first element for lists).
    /// Walks the scope chain, then falls back to environment.
    pub fn getVar(self: *State, name: []const u8) ?[]const u8 {
        // Walk scope chain
        if (self.current_scope.get(name)) |value| {
            return value.asScalar();
        }
        // Fall back to environment
        return env.get(name);
    }

    /// Get a variable as a list.
    /// Walks the scope chain. Does NOT fall back to environment (env vars are scalars).
    pub fn getVarList(self: *State, name: []const u8) ?[]const []const u8 {
        if (self.current_scope.get(name)) |value| {
            return switch (value) {
                .list => |l| l,
                .scalar => null, // Scalars don't convert to lists for getVarList
            };
        }
        return null;
    }

    /// Set a variable as a single value.
    /// Updates in the scope where it's defined, or creates in current scope.
    pub fn setVar(self: *State, name: []const u8, value: []const u8) !void {
        try self.targetScope(name).setLocalScalar(name, value);
    }

    /// Set a variable as a list.
    /// Updates in the scope where it's defined, or creates in current scope.
    pub fn setVarList(self: *State, name: []const u8, values: []const []const u8) !void {
        try self.targetScope(name).setLocalList(name, values);
    }

    /// Set a variable in the CURRENT scope only (for loop variables).
    /// This always creates/updates in the innermost scope, enabling proper shadowing.
    pub fn setLocalVar(self: *State, name: []const u8, value: []const u8) !void {
        try self.current_scope.setLocalScalar(name, value);
    }

    /// Set a list variable in the CURRENT scope only.
    pub fn setLocalVarList(self: *State, name: []const u8, values: []const []const u8) !void {
        try self.current_scope.setLocalList(name, values);
    }

    /// Remove a variable from the scope where it's defined.
    pub fn unsetVar(self: *State, name: []const u8) void {
        if (self.current_scope.findScope(name)) |scope| {
            _ = scope.removeLocal(name);
        }
    }

    // =========================================================================
    // Functions
    // =========================================================================

    /// Get a function by name (returns mutable pointer for lazy caching)
    pub fn getFunction(self: *State, name: []const u8) ?*Function {
        return self.functions.getPtr(name);
    }

    /// Define or redefine a function
    pub fn setFunction(self: *State, name: []const u8, body: []const u8) !void {
        if (self.functions.fetchRemove(name)) |old| {
            self.allocator.free(old.key);
            var func = old.value;
            func.deinit(self.allocator);
        }

        const key = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(key);
        const source = try self.allocator.dupe(u8, body);
        errdefer self.allocator.free(source);

        try self.functions.put(key, .{ .source = source });
    }

    // =========================================================================
    // Aliases
    // =========================================================================

    /// Get an alias expansion by name
    pub fn getAlias(self: *State, name: []const u8) ?[]const u8 {
        return self.aliases.get(name);
    }

    /// Define or redefine an alias
    pub fn setAlias(self: *State, name: []const u8, expansion: []const u8) !void {
        if (self.aliases.fetchRemove(name)) |old| {
            self.allocator.free(old.key);
            self.allocator.free(old.value);
        }

        const key = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(key);
        const value = try self.allocator.dupe(u8, expansion);
        errdefer self.allocator.free(value);

        try self.aliases.put(key, value);
    }

    /// Remove an alias
    pub fn unsetAlias(self: *State, name: []const u8) void {
        if (self.aliases.fetchRemove(name)) |old| {
            self.allocator.free(old.key);
            self.allocator.free(old.value);
        }
    }

    // =========================================================================
    // Exports
    // =========================================================================

    /// Free an export entry (key-value pair) - used by builtins
    pub fn freeStringEntry(self: *State, entry: std.StringHashMap([]const u8).KV) void {
        self.allocator.free(entry.key);
        self.allocator.free(entry.value);
    }

    // =========================================================================
    // Status and CWD
    // =========================================================================

    /// Set last exit status
    pub fn setStatus(self: *State, status: u8) void {
        self.status = status;
    }

    /// Get current working directory
    pub fn getCwd(self: *State) ![]const u8 {
        if (self.cwd) |cwd| return cwd;

        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd = try std.posix.getcwd(&buf);
        self.cwd = try self.allocator.dupe(u8, cwd);
        return self.cwd.?;
    }

    /// Change directory and update prev_cwd
    pub fn chdir(self: *State, path: []const u8) !void {
        // Get current directory before changing (to save as prev_cwd)
        const old_cwd = self.getCwd() catch null;

        // Change to the new directory
        try std.posix.chdir(path);

        // Save old directory as prev_cwd after successful change
        if (old_cwd) |old| {
            // Free existing prev_cwd if any
            if (self.prev_cwd) |prev| {
                self.allocator.free(prev);
            }
            // Move the old cwd to prev_cwd (it's already allocated)
            self.prev_cwd = old;
        }

        // Invalidate cached cwd since we changed directories
        self.cwd = null;
    }

    // =========================================================================
    // Deferred Commands
    // =========================================================================

    /// Push a deferred command source onto the stack (executed LIFO on function exit).
    /// The source string is duplicated into state's allocator for long-term storage.
    pub fn pushDefer(self: *State, source: []const u8) !void {
        const duped = try self.allocator.dupe(u8, source);
        try self.deferred.append(self.allocator, duped);
    }

    /// Pop and return the last deferred command source, or null if empty.
    /// Caller should free the returned slice after use.
    pub fn popDeferred(self: *State) ?[]const u8 {
        if (self.deferred.items.len == 0) return null;
        return self.deferred.pop();
    }

    /// Free a deferred command source string
    pub fn freeDeferred(self: *State, source: []const u8) void {
        self.allocator.free(source);
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

const TestContext = struct {
    state: State,

    fn init() TestContext {
        return .{ .state = State.init(testing.allocator) };
    }

    fn initScope(self: *TestContext) void {
        self.state.initCurrentScope();
    }

    fn deinit(self: *TestContext) void {
        self.state.deinit();
    }
};

// -----------------------------------------------------------------------------
// Variables
// -----------------------------------------------------------------------------

test "Variables: set, get, and update" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    // Basic set and get
    try ctx.state.setVar("foo", "bar");
    try testing.expectEqualStrings("bar", ctx.state.getVar("foo").?);

    // List variable
    const values = [_][]const u8{ "a", "b", "c" };
    try ctx.state.setVarList("xs", &values);
    try testing.expectEqual(@as(usize, 3), ctx.state.getVarList("xs").?.len);
    try testing.expectEqualStrings("a", ctx.state.getVar("xs").?); // getVar returns first element

    // Unset
    ctx.state.unsetVar("foo");
    try testing.expect(ctx.state.getVar("foo") == null);

    // Unset nonexistent is safe
    ctx.state.unsetVar("nonexistent");
}

test "Variables: scope semantics" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    // Set in global scope
    try ctx.state.setVar("x", "outer");

    // Push scope - setVar updates existing in outer scope
    _ = try ctx.state.pushScope();
    try ctx.state.setVar("x", "modified");
    try testing.expectEqualStrings("modified", ctx.state.getVar("x").?);
    ctx.state.popScope();
    try testing.expectEqualStrings("modified", ctx.state.getVar("x").?);

    // New var in inner scope is local
    _ = try ctx.state.pushScope();
    try ctx.state.setVar("local", "value");
    try testing.expectEqualStrings("value", ctx.state.getVar("local").?);
    ctx.state.popScope();
    try testing.expect(ctx.state.getVar("local") == null);

    // setLocalVar always creates in current scope (shadowing)
    try ctx.state.setVar("y", "outer");
    _ = try ctx.state.pushScope();
    try ctx.state.setLocalVar("y", "shadowed");
    try testing.expectEqualStrings("shadowed", ctx.state.getVar("y").?);
    ctx.state.popScope();
    try testing.expectEqualStrings("outer", ctx.state.getVar("y").?);
}

// -----------------------------------------------------------------------------
// Functions
// -----------------------------------------------------------------------------

test "Functions: set, get, and redefine" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    // Set and get
    try ctx.state.setFunction("greet", "echo hello");
    try testing.expectEqualStrings("echo hello", ctx.state.getFunction("greet").?.source);

    // Nonexistent returns null
    try testing.expect(ctx.state.getFunction("nonexistent") == null);

    // Redefinition replaces body
    try ctx.state.setFunction("greet", "echo goodbye");
    try testing.expectEqualStrings("echo goodbye", ctx.state.getFunction("greet").?.source);

    // Multiple functions
    try ctx.state.setFunction("foo", "echo foo");
    try ctx.state.setFunction("bar", "echo bar");
    try testing.expectEqualStrings("echo foo", ctx.state.getFunction("foo").?.source);
    try testing.expectEqualStrings("echo bar", ctx.state.getFunction("bar").?.source);
}

// -----------------------------------------------------------------------------
// Aliases
// -----------------------------------------------------------------------------

test "Aliases: set, get, and unset" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    // Set and get
    try ctx.state.setAlias("ll", "ls -la");
    try testing.expectEqualStrings("ls -la", ctx.state.getAlias("ll").?);

    // Nonexistent returns null
    try testing.expect(ctx.state.getAlias("nonexistent") == null);

    // Redefinition replaces expansion
    try ctx.state.setAlias("ll", "ls -lah");
    try testing.expectEqualStrings("ls -lah", ctx.state.getAlias("ll").?);

    // Unset
    ctx.state.unsetAlias("ll");
    try testing.expect(ctx.state.getAlias("ll") == null);

    // Unset nonexistent is safe
    ctx.state.unsetAlias("nonexistent");
}

// -----------------------------------------------------------------------------
// Scope Management
// -----------------------------------------------------------------------------

test "Scope: push and pop" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    const global = ctx.state.current_scope;
    try testing.expect(global == &ctx.state.global_scope);

    const inner = try ctx.state.pushScope();
    try testing.expect(ctx.state.current_scope == inner);
    try testing.expect(inner.parent == global);

    ctx.state.popScope();
    try testing.expect(ctx.state.current_scope == global);
}

test "Scope: reset for loop optimization" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    const scope = try ctx.state.pushScope();

    for (0..3) |i| {
        scope.reset();
        var buf: [16]u8 = undefined;
        const idx = std.fmt.bufPrint(&buf, "{d}", .{i}) catch unreachable;
        try scope.setLocalScalar("i", idx);
    }

    try testing.expectEqualStrings("2", scope.getLocal("i").?.asScalar().?);
    ctx.state.popScope();
}

// -----------------------------------------------------------------------------
// Scope Pool
// -----------------------------------------------------------------------------

test "Scope pool: acquire and release" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    // Pool starts empty, acquire allocates new scope
    try testing.expectEqual(@as(usize, 0), ctx.state.scope_pool.items.len);
    const scope1 = try ctx.state.acquireScope();
    try testing.expect(scope1 != &ctx.state.global_scope);

    // Release adds to pool
    ctx.state.releaseScope();
    try testing.expectEqual(@as(usize, 1), ctx.state.scope_pool.items.len);

    // Acquire reuses pooled scope
    const scope2 = try ctx.state.acquireScope();
    try testing.expect(scope2 == scope1);
    try testing.expectEqual(@as(usize, 0), ctx.state.scope_pool.items.len);

    ctx.state.releaseScope();
}

test "Scope pool: released scope is cleared" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    const scope1 = try ctx.state.acquireScope();
    try scope1.setLocalScalar("foo", "bar");
    try testing.expect(scope1.getLocal("foo") != null);

    ctx.state.releaseScope();

    // Reacquired scope should be empty
    const scope2 = try ctx.state.acquireScope();
    try testing.expect(scope2.getLocal("foo") == null);

    ctx.state.releaseScope();
}

test "Scope pool: parent chain preserved after reuse" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    try ctx.state.setVar("global_var", "value");

    _ = try ctx.state.acquireScope();
    ctx.state.releaseScope();

    const scope = try ctx.state.acquireScope();
    try testing.expect(scope.parent == &ctx.state.global_scope);
    try testing.expectEqualStrings("value", scope.get("global_var").?.asScalar().?);

    ctx.state.releaseScope();
}

test "Scope pool: nested scopes and size limit" {
    var ctx = TestContext.init();
    ctx.initScope();
    defer ctx.deinit();

    // Nested acquire/release
    const outer = try ctx.state.acquireScope();
    try outer.setLocalScalar("outer_var", "outer");

    const inner = try ctx.state.acquireScope();
    try testing.expect(inner.get("outer_var") != null);

    ctx.state.releaseScope();
    try testing.expect(ctx.state.current_scope == outer);

    ctx.state.releaseScope();
    try testing.expectEqual(@as(usize, 2), ctx.state.scope_pool.items.len);

    // Pool size is limited
    for (0..20) |_| {
        _ = try ctx.state.acquireScope();
        ctx.state.releaseScope();
    }
    try testing.expect(ctx.state.scope_pool.items.len <= max_pooled_scopes);

    // Release on global scope is safe
    ctx.state.releaseScope();
    try testing.expect(ctx.state.current_scope == &ctx.state.global_scope);
}
