//! Lexical scope for shell variables.
//!
//! Scopes form a chain from innermost (current) to outermost (global).
//! Each scope owns an arena allocator that holds all its variable data.
//! When a scope is popped, its arena is freed, automatically cleaning up
//! all variables defined in that scope.
//!
//! This enables:
//! - Block-local variables (if/while/each blocks don't leak vars)
//! - O(1) cleanup on scope exit (just reset/free the arena)
//! - O(1) variable writes within a scope (arena bump allocation)
//! - Proper shadowing (inner scope can shadow outer variables)

const std = @import("std");

/// A value stored in a scope — either a single string or a list of strings.
pub const Value = union(enum) {
    scalar: []const u8,
    list: []const []const u8,

    /// Get as a single string (first element for lists, or the scalar value)
    pub fn asScalar(self: Value) ?[]const u8 {
        return switch (self) {
            .scalar => |s| s,
            .list => |l| if (l.len > 0) l[0] else null,
        };
    }

    /// Get as a list (wraps scalar in single-element slice).
    /// Note: For scalars, returns a pointer into self, so self must remain valid.
    pub fn asList(self: *const Value) []const []const u8 {
        return switch (self.*) {
            .scalar => @as(*const [1][]const u8, &self.scalar),
            .list => |l| l,
        };
    }
};

/// A lexical scope containing variables and a link to its parent scope.
pub const Scope = struct {
    /// Variables defined in this scope.
    /// The HashMap itself uses the backing allocator for its bucket storage.
    /// Keys and values are owned by `arena`.
    vars: std.StringHashMap(Value),

    /// Parent scope, or null if this is the global scope.
    parent: ?*Scope,

    /// Arena allocator that owns all variable keys and values in this scope.
    /// When the scope is popped, the arena is freed, cleaning up all var data.
    arena: std.heap.ArenaAllocator,

    /// Backing allocator (for the hashmap's internal storage).
    backing_allocator: std.mem.Allocator,

    /// Create a new scope with the given parent.
    pub fn init(parent: ?*Scope, backing_allocator: std.mem.Allocator) Scope {
        return .{
            .vars = std.StringHashMap(Value).init(backing_allocator),
            .parent = parent,
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .backing_allocator = backing_allocator,
        };
    }

    /// Free all memory owned by this scope.
    pub fn deinit(self: *Scope) void {
        // Free the hashmap's bucket storage
        self.vars.deinit();
        // Free the arena (owns all keys and values)
        self.arena.deinit();
    }

    /// Reset the scope for reuse (e.g., between loop iterations).
    /// Clears all variables but retains allocated memory for performance.
    pub fn reset(self: *Scope) void {
        // Clear hashmap entries but keep bucket storage
        self.vars.clearRetainingCapacity();
        // Reset arena but keep allocated pages
        _ = self.arena.reset(.retain_capacity);
    }

    /// Get the arena allocator for this scope.
    pub fn allocator(self: *Scope) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Set a variable in THIS scope only (does not walk parent chain).
    /// The name and value are copied into the scope's arena.
    pub fn setLocal(self: *Scope, name: []const u8, value: Value) !void {
        const alloc = self.arena.allocator();

        // Dupe the value into our arena
        const duped_value: Value = switch (value) {
            .scalar => |s| .{ .scalar = try alloc.dupe(u8, s) },
            .list => |l| blk: {
                const list_copy = try alloc.alloc([]const u8, l.len);
                for (l, 0..) |item, i| {
                    list_copy[i] = try alloc.dupe(u8, item);
                }
                break :blk .{ .list = list_copy };
            },
        };

        // If key already exists in this scope, we can reuse the key
        // (arena memory isn't freed until scope exits anyway)
        if (self.vars.getPtr(name)) |existing| {
            existing.* = duped_value;
        } else {
            // New key - dupe it into our arena
            const key = try alloc.dupe(u8, name);
            try self.vars.put(key, duped_value);
        }
    }

    /// Set a scalar variable in this scope.
    pub fn setLocalScalar(self: *Scope, name: []const u8, value: []const u8) !void {
        try self.setLocal(name, .{ .scalar = value });
    }

    /// Set a list variable in this scope.
    pub fn setLocalList(self: *Scope, name: []const u8, values: []const []const u8) !void {
        try self.setLocal(name, .{ .list = values });
    }

    /// Check if a variable exists in THIS scope only.
    pub fn contains(self: *const Scope, name: []const u8) bool {
        return self.vars.contains(name);
    }

    /// Get a variable from THIS scope only (no chain walk).
    pub fn getLocal(self: *const Scope, name: []const u8) ?Value {
        return self.vars.get(name);
    }

    /// Get a variable, walking up the scope chain.
    pub fn get(self: *const Scope, name: []const u8) ?Value {
        if (self.vars.get(name)) |v| return v;
        if (self.parent) |p| return p.get(name);
        return null;
    }

    /// Find the scope where a variable is defined (for updates).
    /// Returns null if the variable doesn't exist in any scope.
    pub fn findScope(self: *Scope, name: []const u8) ?*Scope {
        if (self.vars.contains(name)) return self;
        if (self.parent) |p| return p.findScope(name);
        return null;
    }

    /// Remove a variable from THIS scope only.
    /// Returns true if the variable existed and was removed.
    pub fn removeLocal(self: *Scope, name: []const u8) bool {
        // Note: We can't actually free the memory (it's in an arena),
        // but we remove it from the hashmap so lookups won't find it.
        return self.vars.remove(name);
    }

    /// Get an iterator over this scope's variables only (no parent walk).
    pub fn iterator(self: *const Scope) std.StringHashMap(Value).Iterator {
        return self.vars.iterator();
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

// -----------------------------------------------------------------------------
// Basic Operations
// -----------------------------------------------------------------------------

test "Scope: scalar and list values" {
    var scope = Scope.init(null, testing.allocator);
    defer scope.deinit();

    // Scalar
    try scope.setLocalScalar("foo", "bar");
    try testing.expectEqualStrings("bar", scope.get("foo").?.asScalar().?);

    // List
    const items = [_][]const u8{ "a", "b", "c" };
    try scope.setLocalList("xs", &items);
    var value = scope.get("xs").?;
    const list = value.asList();
    try testing.expectEqual(@as(usize, 3), list.len);
    try testing.expectEqualStrings("a", list[0]);

    // Scalar asList returns single element
    var scalar = scope.get("foo").?;
    try testing.expectEqual(@as(usize, 1), scalar.asList().len);

    // Update existing value
    try scope.setLocalScalar("foo", "updated");
    try testing.expectEqualStrings("updated", scope.get("foo").?.asScalar().?);
}

test "Scope: reset clears variables" {
    var scope = Scope.init(null, testing.allocator);
    defer scope.deinit();

    try scope.setLocalScalar("foo", "bar");
    try testing.expect(scope.get("foo") != null);

    scope.reset();
    try testing.expect(scope.get("foo") == null);
}

// -----------------------------------------------------------------------------
// Scope Chain
// -----------------------------------------------------------------------------

test "Scope: parent chain lookup and shadowing" {
    var parent = Scope.init(null, testing.allocator);
    defer parent.deinit();
    var child = Scope.init(&parent, testing.allocator);
    defer child.deinit();

    try parent.setLocalScalar("outer", "parent_value");
    try child.setLocalScalar("inner", "child_value");

    // Child sees both scopes
    try testing.expectEqualStrings("child_value", child.get("inner").?.asScalar().?);
    try testing.expectEqualStrings("parent_value", child.get("outer").?.asScalar().?);

    // Parent cannot see child's var
    try testing.expect(parent.get("inner") == null);

    // Shadowing: child can shadow parent's variable
    try child.setLocalScalar("outer", "shadowed");
    try testing.expectEqualStrings("shadowed", child.get("outer").?.asScalar().?);
    try testing.expectEqualStrings("parent_value", parent.get("outer").?.asScalar().?);
}

test "Scope: findScope locates defining scope" {
    var parent = Scope.init(null, testing.allocator);
    defer parent.deinit();
    var child = Scope.init(&parent, testing.allocator);
    defer child.deinit();

    try parent.setLocalScalar("outer", "value");
    try child.setLocalScalar("inner", "value");

    try testing.expect(child.findScope("inner") == &child);
    try testing.expect(child.findScope("outer") == &parent);
    try testing.expect(child.findScope("nonexistent") == null);
}
