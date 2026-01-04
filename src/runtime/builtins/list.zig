//! list builtin - create and manipulate lists
//!
//! Design principles:
//!   - Parallel to `string` as a fundamental value-manipulation builtin
//!   - Supports both stdout output and direct variable writes via --into
//!   - Variable references via @varname for efficient large-list handling
//!   - Composable transforms applied in fixed order

const std = @import("std");
const builtins = @import("../builtins.zig");
const binding = @import("../binding.zig");
const Args = @import("../../terminal/args.zig");

const io = builtins.io;

pub const builtin = builtins.Builtin{
    .name = "list",
    .run = run,
    .help =
    \\list [OPTIONS] [ITEMS...] [@VARNAME]
    \\
    \\Create, transform, and output lists. Parallel to `string` for collections.
    \\
    \\Input sources (mutually exclusive):
    \\  ITEMS...         Literal items or ranges (1..10, a..z)
    \\  @VARNAME         Two-way binding: read from variable, write result back
    \\  (stdin)          When no items given, reads one item per line
    \\
    \\Range creation:
    \\  START..END       Numeric or alphabetic range (inclusive, auto-direction)
    \\  -s, --step N     Step size for numeric ranges (default: 1 or -1)
    \\
    \\Mutation flags:
    \\  --append ITEM    Append item(s) to the list
    \\  --prepend ITEM   Prepend item(s) to the list
    \\
    \\Transform flags (applied in order: unique -> sort -> reverse):
    \\  -u, --unique     Remove duplicates (preserves first occurrence)
    \\  --sort           Sort lexicographically
    \\  -n, --numeric    Sort numerically
    \\  -r, --reverse    Reverse the list
    \\
    \\Output control:
    \\  -S, --separator S  Output separator (default: newline)
    \\  -l, --length     Output count instead of items
    \\
    \\Test flags (exit code only, no output):
    \\  --contains ITEM  Exit 0 if ITEM is in the list
    \\  --empty          Exit 0 if list is empty
    \\
    \\Examples:
    \\  list 1..10                        # 1 through 10
    \\  list 1..10 --step 2               # 1, 3, 5, 7, 9
    \\  list a..z                         # a through z
    \\  list c b a --sort                 # a, b, c
    \\  list 10 2 5 --numeric             # 2, 5, 10
    \\  list a b b c --unique             # a, b, c
    \\  list @items --sort                # sort variable in-place
    \\  list @items --contains foo        # exit 0 if foo in items
    \\  list @items --length              # count items (output, no mutation)
    \\  list @items --append x y z        # append items in-place
    \\  list $items --sort                # output sorted list (no mutation)
    \\  list a b c -S ', '                # a, b, c
    ,
};

// =============================================================================
// Configuration
// =============================================================================

const Mode = enum { output, length, contains, empty };

const Config = struct {
    // Range options
    step: i64 = 1,

    // Transforms (applied in order)
    unique: bool = false,
    sort: bool = false,
    numeric_sort: bool = false,
    reverse: bool = false,

    // Mutations
    append_items: []const []const u8 = &.{},
    prepend_items: []const []const u8 = &.{},

    // Output
    separator: []const u8 = "\n",

    // Mode
    mode: Mode = .output,
    contains_item: []const u8 = "",

    // Source variable (set when using @varname for auto two-way binding)
    source_var: ?[]const u8 = null,
};

// =============================================================================
// Entry Point
// =============================================================================

fn run(state: *builtins.State, args: []const []const u8) u8 {
    var cfg = Config{};

    // Parse arguments and get positionals for output detection
    var positionals: []const []const u8 = &.{};
    if (!parseArgs(state, args, &cfg, &positionals)) {
        return 1;
    }

    // Create output writer with automatic binding detection
    var output = binding.OutputWriter.init(state, .{
        .args = positionals,
        .readonly = cfg.mode == .contains or cfg.mode == .empty or cfg.mode == .length,
    });

    // Collect items into a dynamic list
    var items: std.ArrayListUnmanaged([]const u8) = .empty;
    const allocator = state.allocator;

    // Add prepended items first
    items.appendSlice(allocator, cfg.prepend_items) catch return oom();

    // Collect main input (handles @varname expansion, stdin)
    // Then expand ranges for literal args
    if (collectItems(state, positionals, &cfg, &items)) |_| {} else |err| switch (err) {
        error.OutOfMemory => return oom(),
        error.VariableNotFound, error.ZeroStep => return 1, // Already printed error
    }

    // Add appended items
    items.appendSlice(allocator, cfg.append_items) catch return oom();

    // Apply transforms
    const result = applyTransforms(state.allocator, items.items, &cfg) catch return oom();

    // Execute based on mode
    return executeMode(state, result, &cfg, &output);
}

/// Centralized OOM error reporting.
fn oom() u8 {
    return builtins.reportOOM("list");
}

// =============================================================================
// Argument Parsing
// =============================================================================

fn parseArgs(state: *builtins.State, args: []const []const u8, cfg: *Config, positionals_out: *[]const []const u8) bool {
    _ = state;
    var p = Args.Args("list").init(args);

    // Track indices for slicing args later
    var positional_start: usize = 0;
    var positional_end: usize = 0;
    var append_start: usize = 0;
    var append_end: usize = 0;
    var prepend_start: usize = 0;
    var prepend_end: usize = 0;
    var collecting_append = false;
    var collecting_prepend = false;
    var found_positional = false;

    while (p.next()) |arg| {
        const arg_idx = p.idx - 1; // Current argument index

        // Stop collecting append/prepend on new flag
        if (arg.len > 0 and arg[0] == '-' and !Args.isNumeric(arg)) {
            collecting_append = false;
            collecting_prepend = false;
        }

        // Handle end-of-flags
        if (std.mem.eql(u8, arg, "--")) {
            const rest = p.rest();
            if (rest.len > 0) {
                if (!found_positional) {
                    positional_start = p.idx;
                    found_positional = true;
                }
                positional_end = p.idx + rest.len;
            }
            break;
        }

        // Flags with values (use p.option for cleaner parsing)
        if (p.option("-s", "--step")) |val| {
            cfg.step = std.fmt.parseInt(i64, val, 10) catch {
                _ = p.errArg("invalid step: ", val);
                return false;
            };
        } else if (p.option("-S", "--separator")) |val| {
            cfg.separator = val;
        } else if (p.option(null, "--contains")) |val| {
            cfg.mode = .contains;
            cfg.contains_item = val;
        }
        // Boolean flags
        else if (p.flag("-u", "--unique")) {
            cfg.unique = true;
        } else if (p.flag(null, "--sort")) {
            cfg.sort = true;
        } else if (p.flag("-n", "--numeric")) {
            cfg.numeric_sort = true;
        } else if (p.flag("-r", "--reverse")) {
            cfg.reverse = true;
        } else if (p.flag("-l", "--length")) {
            cfg.mode = .length;
        } else if (p.flag(null, "--empty")) {
            cfg.mode = .empty;
        } else if (p.flag(null, "--append")) {
            collecting_append = true;
            collecting_prepend = false;
            append_start = p.idx;
            append_end = p.idx;
        } else if (p.flag(null, "--prepend")) {
            collecting_prepend = true;
            collecting_append = false;
            prepend_start = p.idx;
            prepend_end = p.idx;
        } else if (arg.len > 0 and arg[0] == '-' and !Args.isNumeric(arg)) {
            _ = p.errArg("unknown option: ", arg);
            return false;
        } else if (collecting_append) {
            append_end = arg_idx + 1;
        } else if (collecting_prepend) {
            prepend_end = arg_idx + 1;
        } else {
            // Positional argument
            if (!found_positional) {
                positional_start = arg_idx;
                found_positional = true;
            }
            positional_end = arg_idx + 1;
        }
    }

    // Set slices from args
    if (append_end > append_start) {
        cfg.append_items = args[append_start..append_end];
    }
    if (prepend_end > prepend_start) {
        cfg.prepend_items = args[prepend_start..prepend_end];
    }

    // Extract positional args slice
    positionals_out.* = if (positional_end > positional_start) args[positional_start..positional_end] else &[_][]const u8{};

    return true;
}

const CollectError = error{ OutOfMemory, VariableNotFound, ZeroStep };

/// Collect items from positionals, handling @varname, stdin, and range expansion.
fn collectItems(state: *builtins.State, positionals: []const []const u8, cfg: *const Config, items: *std.ArrayListUnmanaged([]const u8)) CollectError!void {
    const allocator = state.allocator;

    // Check if this is a single @varname or stdin case (no range expansion needed)
    if (positionals.len == 0) {
        // stdin case
        binding.collectInputs(state, .{ .args = positionals, .stdin_if_empty = true }, allocator, items) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.VariableNotFound => return error.VariableNotFound,
        };
        return;
    }

    if (positionals.len == 1 and positionals[0].len > 1 and positionals[0][0] == '@') {
        // Single @varname case - use binding.collectInputs
        binding.collectInputs(state, .{ .args = positionals, .stdin_if_empty = false }, allocator, items) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.VariableNotFound => return error.VariableNotFound,
        };
        return;
    }

    // Literal items - expand ranges
    for (positionals) |item| {
        if (parseRange(item)) |range| {
            try expandRange(allocator, range, cfg.step, items);
        } else {
            items.append(allocator, item) catch return error.OutOfMemory;
        }
    }
}

// =============================================================================
// Range Parsing and Expansion
// =============================================================================

const Range = union(enum) {
    numeric: struct { start: i64, end: i64 },
    alpha: struct { start: u8, end: u8 },
};

fn parseRange(s: []const u8) ?Range {
    // Look for ".."
    const dot_pos = std.mem.indexOf(u8, s, "..") orelse return null;
    if (dot_pos == 0 or dot_pos + 2 >= s.len) return null;

    const start_str = s[0..dot_pos];
    const end_str = s[dot_pos + 2 ..];

    // Try numeric range first
    if (std.fmt.parseInt(i64, start_str, 10)) |start| {
        if (std.fmt.parseInt(i64, end_str, 10)) |end| {
            return .{ .numeric = .{ .start = start, .end = end } };
        } else |_| {}
    } else |_| {}

    // Try alphabetic range (single chars only)
    if (start_str.len == 1 and end_str.len == 1) {
        const start_c = start_str[0];
        const end_c = end_str[0];
        if ((std.ascii.isAlphabetic(start_c) and std.ascii.isAlphabetic(end_c)) and
            ((std.ascii.isLower(start_c) and std.ascii.isLower(end_c)) or
                (std.ascii.isUpper(start_c) and std.ascii.isUpper(end_c))))
        {
            return .{ .alpha = .{ .start = start_c, .end = end_c } };
        }
    }

    return null;
}

const ExpandError = error{ OutOfMemory, ZeroStep };

fn expandRange(allocator: std.mem.Allocator, range: Range, custom_step: i64, items: *std.ArrayListUnmanaged([]const u8)) ExpandError!void {
    switch (range) {
        .numeric => |r| {
            if (custom_step == 0) {
                io.writeStderr("list: step cannot be zero\n");
                return error.ZeroStep;
            }
            // Auto-detect direction if step is default positive but range is descending
            const step = if (r.start > r.end and custom_step > 0) -custom_step else custom_step;

            // Check for impossible ranges
            if ((r.start < r.end and step < 0) or (r.start > r.end and step > 0)) {
                return; // Empty range, not an error
            }

            var n = r.start;
            while (if (step > 0) n <= r.end else n >= r.end) {
                var buf: [21]u8 = undefined;
                const s = std.fmt.bufPrint(&buf, "{d}", .{n}) catch break;
                try appendDuped(allocator, items, s);
                n = std.math.add(i64, n, step) catch break;
            }
        },
        .alpha => |r| {
            // Alphabetic ranges always step by 1
            const step: i8 = if (r.start <= r.end) 1 else -1;
            var c: i16 = r.start;
            const end: i16 = r.end;
            while (if (step > 0) c <= end else c >= end) {
                const buf: [1]u8 = .{@intCast(c)};
                try appendDuped(allocator, items, &buf);
                c += step;
            }
        },
    }
}

/// Helper to duplicate a string and append to list.
fn appendDuped(allocator: std.mem.Allocator, items: *std.ArrayListUnmanaged([]const u8), s: []const u8) !void {
    const duped = try allocator.dupe(u8, s);
    try items.append(allocator, duped);
}

// =============================================================================
// Transforms
// =============================================================================

fn applyTransforms(allocator: std.mem.Allocator, items: []const []const u8, cfg: *const Config) ![]const []const u8 {
    var result = items;

    // 1. Unique (preserves first occurrence)
    if (cfg.unique) {
        result = try dedup(allocator, result);
    }

    // 2. Sort
    if (cfg.sort or cfg.numeric_sort) {
        const mutable = try allocator.dupe([]const u8, result);
        if (cfg.numeric_sort) {
            std.mem.sort([]const u8, mutable, {}, numericLessThan);
        } else {
            std.mem.sort([]const u8, mutable, {}, stringLessThan);
        }
        result = mutable;
    }

    // 3. Reverse
    if (cfg.reverse) {
        const mutable = try allocator.dupe([]const u8, result);
        std.mem.reverse([]const u8, mutable);
        result = mutable;
    }

    return result;
}

fn dedup(allocator: std.mem.Allocator, items: []const []const u8) ![]const []const u8 {
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();

    var result: std.ArrayListUnmanaged([]const u8) = .empty;
    for (items) |item| {
        if (!seen.contains(item)) {
            try seen.put(item, {});
            try result.append(allocator, item);
        }
    }
    return result.toOwnedSlice(allocator);
}

fn stringLessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

fn numericLessThan(_: void, a: []const u8, b: []const u8) bool {
    const a_num = std.fmt.parseInt(i64, a, 10) catch return stringLessThan({}, a, b);
    const b_num = std.fmt.parseInt(i64, b, 10) catch return stringLessThan({}, a, b);
    return a_num < b_num;
}

// =============================================================================
// Output
// =============================================================================

fn executeMode(state: *builtins.State, items: []const []const u8, cfg: *const Config, output: *binding.OutputWriter) u8 {
    _ = state;
    switch (cfg.mode) {
        .contains => {
            for (items) |item| {
                if (std.mem.eql(u8, item, cfg.contains_item)) {
                    return 0; // Found
                }
            }
            return 1; // Not found
        },
        .empty => {
            return if (items.len == 0) 0 else 1;
        },
        .length => {
            output.writeInt(items.len) catch return oom();
            return 0;
        },
        .output => {
            if (std.mem.eql(u8, cfg.separator, "\n")) {
                output.writeList(items) catch return oom();
            } else {
                output.writeListWithSeparator(items, cfg.separator);
            }
            return 0;
        },
    }
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;
const State = @import("../state.zig").State;

fn runList(state: *State, args: []const []const u8) u8 {
    return run(state, args);
}

fn expectList(state: *State, name: []const u8, expected: []const []const u8) !void {
    const actual = state.getVarList(name) orelse return error.VariableNotFound;
    try testing.expectEqual(expected.len, actual.len);
    for (expected, actual) |e, a| {
        try testing.expectEqualStrings(e, a);
    }
}

// -----------------------------------------------------------------------------
// Range: parsing
// -----------------------------------------------------------------------------

test "Range: numeric ranges parse correctly" {
    // Basic ascending, negative numbers, descending
    const cases = .{
        .{ "1..10", @as(i64, 1), @as(i64, 10) },
        .{ "-5..5", @as(i64, -5), @as(i64, 5) },
        .{ "10..1", @as(i64, 10), @as(i64, 1) },
    };
    inline for (cases) |case| {
        const r = parseRange(case[0]).?.numeric;
        try testing.expectEqual(case[1], r.start);
        try testing.expectEqual(case[2], r.end);
    }
}

test "Range: alphabetic ranges parse correctly" {
    // Lowercase ascending, uppercase descending
    const r1 = parseRange("a..z").?.alpha;
    try testing.expectEqual(@as(u8, 'a'), r1.start);
    try testing.expectEqual(@as(u8, 'z'), r1.end);

    const r2 = parseRange("Z..A").?.alpha;
    try testing.expectEqual(@as(u8, 'Z'), r2.start);
    try testing.expectEqual(@as(u8, 'A'), r2.end);
}

test "Range: invalid patterns return null" {
    const invalid = .{ "abc", "..5", "5..", "a..1", "a..Z" };
    inline for (invalid) |case| {
        try testing.expect(parseRange(case) == null);
    }
}

// -----------------------------------------------------------------------------
// Range: expansion
// -----------------------------------------------------------------------------

test "Range: numeric expansion ascending and descending" {
    var items: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (items.items) |s| testing.allocator.free(s);
        items.deinit(testing.allocator);
    }

    // Ascending 1..5
    try expandRange(testing.allocator, .{ .numeric = .{ .start = 1, .end = 5 } }, 1, &items);
    try testing.expectEqual(@as(usize, 5), items.items.len);
    try testing.expectEqualStrings("1", items.items[0]);
    try testing.expectEqualStrings("5", items.items[4]);

    // Clear and test descending 5..1 (auto-negates step)
    for (items.items) |s| testing.allocator.free(s);
    items.clearRetainingCapacity();

    try expandRange(testing.allocator, .{ .numeric = .{ .start = 5, .end = 1 } }, 1, &items);
    try testing.expectEqual(@as(usize, 5), items.items.len);
    try testing.expectEqualStrings("5", items.items[0]);
    try testing.expectEqualStrings("1", items.items[4]);
}

test "Range: numeric expansion with custom step" {
    var items: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (items.items) |s| testing.allocator.free(s);
        items.deinit(testing.allocator);
    }

    try expandRange(testing.allocator, .{ .numeric = .{ .start = 1, .end = 10 } }, 2, &items);
    try testing.expectEqual(@as(usize, 5), items.items.len);
    try testing.expectEqualStrings("1", items.items[0]);
    try testing.expectEqualStrings("3", items.items[1]);
    try testing.expectEqualStrings("9", items.items[4]);
}

test "Range: alphabetic expansion" {
    var items: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (items.items) |s| testing.allocator.free(s);
        items.deinit(testing.allocator);
    }

    try expandRange(testing.allocator, .{ .alpha = .{ .start = 'a', .end = 'e' } }, 1, &items);
    try testing.expectEqual(@as(usize, 5), items.items.len);
    try testing.expectEqualStrings("a", items.items[0]);
    try testing.expectEqualStrings("e", items.items[4]);
}

// -----------------------------------------------------------------------------
// Transforms: sort and dedup
// -----------------------------------------------------------------------------

test "Transforms: dedup preserves first occurrence" {
    const items = [_][]const u8{ "a", "b", "a", "c", "b", "c" };
    const result = try dedup(testing.allocator, &items);
    defer testing.allocator.free(result);

    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqualStrings("a", result[0]);
    try testing.expectEqualStrings("b", result[1]);
    try testing.expectEqualStrings("c", result[2]);
}

test "Transforms: lexicographic sort" {
    var items = [_][]const u8{ "banana", "apple", "cherry" };
    std.mem.sort([]const u8, &items, {}, stringLessThan);

    try testing.expectEqualStrings("apple", items[0]);
    try testing.expectEqualStrings("banana", items[1]);
    try testing.expectEqualStrings("cherry", items[2]);
}

test "Transforms: numeric sort handles multi-digit numbers" {
    var items = [_][]const u8{ "10", "2", "1", "20" };
    std.mem.sort([]const u8, &items, {}, numericLessThan);

    try testing.expectEqualStrings("1", items[0]);
    try testing.expectEqualStrings("2", items[1]);
    try testing.expectEqualStrings("10", items[2]);
    try testing.expectEqualStrings("20", items[3]);
}

// -----------------------------------------------------------------------------
// Command: variable reference, transforms, and output
// -----------------------------------------------------------------------------

test "Command: @varname two-way binding" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    // Test @varname with --sort (mutates in-place)
    try state.setVarList("items", &.{ "c", "a", "b" });
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "@items", "--sort" }));
    try expectList(&state, "items", &.{ "a", "b", "c" });

    // Test @varname with --reverse (mutates in-place)
    try state.setVarList("nums", &.{ "1", "2", "3" });
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "@nums", "--reverse" }));
    try expectList(&state, "nums", &.{ "3", "2", "1" });
}

// -----------------------------------------------------------------------------
// Command: test modes (--contains, --empty)
// -----------------------------------------------------------------------------

test "Command: --contains and --empty exit codes" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    // --contains: found=0, not found=1
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "a", "b", "c", "--contains", "b" }));
    try testing.expectEqual(@as(u8, 1), runList(&state, &.{ "a", "b", "c", "--contains", "x" }));

    // --empty: empty=0, non-empty=1
    try state.setVarList("empty", &.{});
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "@empty", "--empty" }));

    try state.setVarList("nonempty", &.{"x"});
    try testing.expectEqual(@as(u8, 1), runList(&state, &.{ "@nonempty", "--empty" }));
}

// -----------------------------------------------------------------------------
// Command: mutations (--append, --prepend)
// -----------------------------------------------------------------------------

test "Command: --append and --prepend mutations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    // --append mutates in-place
    try state.setVarList("items", &.{ "a", "b" });
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "@items", "--append", "c", "d" }));
    try expectList(&state, "items", &.{ "a", "b", "c", "d" });

    // --prepend mutates in-place
    try state.setVarList("items2", &.{ "c", "d" });
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "@items2", "--prepend", "a", "b" }));
    try expectList(&state, "items2", &.{ "a", "b", "c", "d" });
}

test "Command: read-only modes preserve variables" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var state = State.init(arena.allocator());
    state.initCurrentScope();
    defer state.deinit();

    // Initialize items
    try state.setVarList("items", &.{ "a", "b", "c" });

    // --contains shouldn't mutate
    try testing.expectEqual(@as(u8, 0), runList(&state, &.{ "@items", "--contains", "b" }));
    try expectList(&state, "items", &.{ "a", "b", "c" });

    // --empty shouldn't mutate
    try testing.expectEqual(@as(u8, 1), runList(&state, &.{ "@items", "--empty" }));
    try expectList(&state, "items", &.{ "a", "b", "c" });
}
