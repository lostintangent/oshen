//! Variable binding utilities for @varname syntax
//!
//! This module provides the infrastructure for two-way binding in builtin commands:
//!   @varname    - Two-way binding: read variable, mutate with result
//!   $varname    - One-way: value expanded, no mutation (use for read-only access)
//!
//! ## Public API
//!
//! **OutputWriter** - Automatically write results to variables or stdout:
//!   var output = OutputWriter.init(state, .{
//!       .args = positionals,
//!       .readonly = cfg.mode == .match,
//!   });
//!   try output.writeScalar(result);
//!
//! **resolveBindableArg()** - Read arguments that support @varname syntax:
//!   const arg = resolveBindableArg(state, "string", positionals[0]) orelse return 1;

const std = @import("std");
const State = @import("state.zig").State;
const io = @import("../terminal/io.zig");

// =============================================================================
// Public API
// =============================================================================

/// Configuration for InputSource - controls how inputs are collected.
pub const InputSource = struct {
    /// The positional arguments (literals or @varname references)
    args: []const []const u8 = &.{},
    /// Read from stdin if args is empty (disable for modes like --join)
    stdin_if_empty: bool = true,
};

/// Configuration for OutputWriter.
pub const OutputWriterConfig = struct {
    /// The positional arguments being processed
    args: []const []const u8,
    /// Whether this is a read-only operation (--length, --match, --contains, etc.)
    readonly: bool = false,
};

/// Automatically routes output to a variable or stdout based on binding context.
///
/// Simplifies builtin implementations by eliminating output branching logic.
/// Builtins just write to OutputWriter, which handles the destination automatically.
///
/// Usage:
/// ```zig
/// var output = OutputWriter.init(state, .{
///     .args = positionals,
///     .stdout = cfg.stdout,
///     .readonly = cfg.mode == .match or cfg.mode == .contains,
/// });
/// try output.writeScalar(result);
/// ```
pub const OutputWriter = struct {
    state: *State,
    mutate_var: ?[]const u8,
    writer: io.StdoutWriter,

    /// Create an OutputWriter with automatic binding detection.
    ///
    /// Mutation is enabled when ALL of these conditions are met:
    /// - Exactly one positional argument
    /// - That argument is @varname (a binding)
    /// - Operation is NOT read-only
    /// - NOT in capture mode (capture needs stdout output)
    pub fn init(state: *State, cfg: OutputWriterConfig) OutputWriter {
        // Don't mutate if capture mode is active - output needs to go to stdout for capture
        const in_capture_mode = io.raw.isCaptureActive();
        const mutate_var = if (cfg.args.len == 1 and !cfg.readonly and !in_capture_mode)
            parseVarRef(cfg.args[0])
        else
            null;

        return .{
            .state = state,
            .mutate_var = mutate_var,
            .writer = io.StdoutWriter{},
        };
    }

    /// Write a scalar value (string) to the binding or stdout.
    ///
    /// - If mutate_var is set: updates the variable
    /// - Otherwise: writes to stdout with trailing newline
    pub fn writeScalar(self: *OutputWriter, value: []const u8) !void {
        if (self.mutate_var) |varname| {
            try self.state.setVar(varname, value);
        } else {
            self.writer.write(value);
            self.writer.writeByte('\n');
            self.writer.flush();
        }
    }

    /// Write a list of items to the binding or stdout.
    ///
    /// - If mutate_var is set: updates the variable as a list
    /// - Otherwise: writes items to stdout with newline separator
    pub fn writeList(self: *OutputWriter, items: []const []const u8) !void {
        if (self.mutate_var) |varname| {
            try self.state.setVarList(varname, items);
        } else {
            for (items, 0..) |item, i| {
                if (i > 0) self.writer.writeByte('\n');
                self.writer.write(item);
            }
            if (items.len > 0) self.writer.writeByte('\n');
            self.writer.flush();
        }
    }

    /// Write a list with a custom separator to stdout (bypasses binding).
    ///
    /// Used by list builtin when custom separator is specified.
    /// Always writes to stdout regardless of binding context.
    pub fn writeListWithSeparator(self: *OutputWriter, items: []const []const u8, separator: []const u8) void {
        for (items, 0..) |item, i| {
            if (i > 0) self.writer.write(separator);
            self.writer.write(item);
        }
        if (items.len > 0) self.writer.writeByte('\n');
        self.writer.flush();
    }

    /// Write an integer value to the binding or stdout.
    ///
    /// - If mutate_var is set: formats as string and updates variable
    /// - Otherwise: writes to stdout with trailing newline
    pub fn writeInt(self: *OutputWriter, value: usize) !void {
        if (self.mutate_var) |varname| {
            var buf: [21]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
            try self.state.setVar(varname, str);
        } else {
            self.writer.writeInt(value);
            self.writer.writeByte('\n');
            self.writer.flush();
        }
    }

    /// Write items as scalar (1 item) or list (multiple items) to binding or stdout.
    ///
    /// - If mutate_var is set: stores as scalar if 1 item, list if multiple
    /// - Otherwise: writes items space-separated to stdout
    ///
    /// Used by string builtin when processing multiple arguments.
    pub fn writeScalarOrList(self: *OutputWriter, items: []const []const u8, separator: []const u8) !void {
        if (self.mutate_var) |varname| {
            if (items.len == 1) {
                try self.state.setVar(varname, items[0]);
            } else {
                try self.state.setVarList(varname, items);
            }
        } else {
            for (items, 0..) |item, i| {
                if (i > 0) self.writer.write(separator);
                self.writer.write(item);
            }
            self.writer.writeByte('\n');
            self.writer.flush();
        }
    }
};

/// Error type for collectInputs.
pub const CollectError = error{OutOfMemory, VariableNotFound};

/// Collect inputs from positional arguments, expanding @varname references.
///
/// Handles three input sources:
/// - Literal arguments: appended directly
/// - @varname references: expanded (lists become multiple items, scalars become one)
/// - stdin: read line-by-line if args is empty and stdin_if_empty is true
///
/// Usage:
/// ```zig
/// var items = std.ArrayListUnmanaged([]const u8){};
/// try binding.collectInputs(state, .{
///     .args = positionals,
///     .stdin_if_empty = true,
/// }, allocator, &items);
/// ```
pub fn collectInputs(
    state: *State,
    source: InputSource,
    allocator: std.mem.Allocator,
    items: *std.ArrayListUnmanaged([]const u8),
) CollectError!void {
    if (source.args.len == 0) {
        if (source.stdin_if_empty) {
            var reader = io.StdinReader.init();
            var line_buf: [io.BUF_SIZE]u8 = undefined;
            while (reader.readLine(&line_buf)) |line| {
                const duped = allocator.dupe(u8, line) catch return error.OutOfMemory;
                items.append(allocator, duped) catch return error.OutOfMemory;
            }
        }
        return;
    }

    for (source.args) |arg| {
        if (parseVarRef(arg)) |varname| {
            if (state.getVarList(varname)) |list| {
                items.appendSlice(allocator, list) catch return error.OutOfMemory;
            } else if (state.getVar(varname)) |scalar| {
                items.append(allocator, scalar) catch return error.OutOfMemory;
            } else {
                io.writeStderr("variable '");
                io.writeStderr(varname);
                io.writeStderr("' not found\n");
                return error.VariableNotFound;
            }
        } else {
            items.append(allocator, arg) catch return error.OutOfMemory;
        }
    }
}

/// Resolve an argument that may be a literal or @varname binding.
///
/// Returns the resolved string, or null if variable not found.
/// Reports error to stderr and returns null on failure.
///
/// Use this for arguments that support the @varname binding syntax.
/// The function handles both literal values and variable bindings.
///
/// Examples:
///   resolveBindableArg(state, "print", "hello")   -> "hello" (literal)
///   resolveBindableArg(state, "print", "@text")   -> value of text (binding)
///   resolveBindableArg(state, "print", "@missing") -> null + stderr message
pub fn resolveBindableArg(state: *State, comptime builtin_name: []const u8, arg: []const u8) ?[]const u8 {
    if (parseVarRef(arg)) |varname| {
        return getVarScalar(state, varname) orelse {
            _ = reportVarNotFound(builtin_name, varname);
            return null;
        };
    }
    return arg;
}

// =============================================================================
// Internal Helpers
// =============================================================================

/// Parse variable reference syntax (@varname).
/// Returns the variable name (without @) if valid, null otherwise.
///
/// Internal helper used by OutputWriter.init() and resolveBindableArg().
/// Most code should use resolveBindableArg() instead.
fn parseVarRef(arg: []const u8) ?[]const u8 {
    if (arg.len > 1 and arg[0] == '@') {
        return arg[1..];
    }
    return null;
}

/// Retrieve a variable's scalar value.
/// - Lists return first element (or "" if empty)
/// - Scalars return their value
/// - Missing vars return null
///
/// Internal helper used by resolveBindableArg().
/// Most code should use resolveBindableArg() instead.
fn getVarScalar(state: *State, varname: []const u8) ?[]const u8 {
    // Try list first, get first element
    if (state.getVarList(varname)) |list| {
        return if (list.len > 0) list[0] else "";
    }
    // Fall back to scalar (includes env vars)
    return state.getVar(varname);
}

/// Report a variable-not-found error to stderr.
/// Returns 1 for convenient `return reportVarNotFound(...)` usage.
///
/// Internal helper used by resolveBindableArg().
/// Most code should use resolveBindableArg() instead.
fn reportVarNotFound(comptime builtin_name: []const u8, varname: []const u8) u8 {
    io.writeStderr(builtin_name ++ ": variable '");
    io.writeStderr(varname);
    io.writeStderr("' not found\n");
    return 1;
}

// =============================================================================
// Tests
// =============================================================================
