//! args.zig - Declarative argument parsing with auto-generated help
//!
//! Two parsing APIs:
//!   1. Declarative: Comptime spec → parsed struct + generated help
//!   2. Imperative: Manual iteration for complex parsing needs
//!
//! Design principles:
//!   - Zero heap allocations
//!   - Single source of truth: spec defines parsing AND help text
//!   - Comptime type generation for type-safe configs
//!   - Elegant, minimal API surface

const std = @import("std");
const io = @import("io.zig");

/// Error returned when argument parsing fails.
/// The error message has already been printed to stderr.
pub const ParseError = error{ParseFailed};

// =============================================================================
// Command Spec - Complete command definition for parsing + help generation
// =============================================================================

/// Complete command specification: defines parsing behavior AND generates help.
/// Usage:
/// ```
/// const spec = Spec("range", .{
///     .desc = "Print a sequence of numbers",
///     .args = .{
///         .step = Option.I64(.{ .short = "s", .long = "step", .desc = "Increment", .default = 1 }),
///         .first = Positional.I64Default(.{ .desc = "Starting number", .default = 1 }),
///         .last = Positional.I64(.{ .desc = "Ending number" }),
///     },
/// });
/// const r = spec.parse(args) catch return 1;
/// ```
pub fn Spec(comptime cmd: []const u8, comptime def: anytype) type {
    // Validate spec at comptime
    comptime {
        const fields = std.meta.fields(@TypeOf(def.args));
        var rest_count: usize = 0;
        var rest_idx: usize = 0;
        var last_positional_idx: usize = 0;

        for (fields, 0..) |f, idx| {
            const is_rest = @hasDecl(f.type, "is_rest") and f.type.is_rest;
            const is_positional = @hasDecl(f.type, "is_positional") and f.type.is_positional;
            if (is_rest) {
                rest_count += 1;
                rest_idx = idx;
            }
            if (is_positional) {
                last_positional_idx = idx;
            }
        }

        if (rest_count > 1) {
            @compileError("Spec can only have one Rest field");
        }
        if (rest_count == 1 and rest_idx != last_positional_idx) {
            @compileError("Rest must be the last positional argument");
        }
    }

    return struct {
        pub const name = cmd;
        const ArgsParser = Args(cmd);
        pub const Result = ArgsParser.Result(@TypeOf(def.args));

        pub fn parse(args: []const []const u8) ParseError!Result {
            return ArgsParser.parse(args, def.args);
        }

        pub const help = generateHelp(cmd, def);
    };
}

/// Generate help text from a command spec at comptime
fn generateHelp(comptime cmd: []const u8, comptime def: anytype) []const u8 {
    comptime {
        var help: []const u8 = "";

        // Usage line
        if (@hasField(@TypeOf(def), "usage")) {
            help = help ++ def.usage ++ "\n\n";
        } else {
            help = help ++ cmd ++ " [OPTIONS]";
            // Add positional placeholders
            const fields = std.meta.fields(@TypeOf(def.args));
            for (fields) |f| {
                const FieldType = f.type;
                if (@hasDecl(FieldType, "is_positional") and FieldType.is_positional) {
                    const is_rest = @hasDecl(FieldType, "is_rest") and FieldType.is_rest;
                    const has_default = @hasDecl(FieldType, "has_default_value") and FieldType.has_default_value;
                    const name = fieldToArgName(f.name);
                    if (is_rest) {
                        const rest_required = @hasDecl(FieldType, "is_required") and FieldType.is_required;
                        if (rest_required) {
                            help = help ++ " " ++ name ++ "...";
                        } else {
                            help = help ++ " [" ++ name ++ "...]";
                        }
                    } else if (has_default) {
                        help = help ++ " [" ++ name ++ "]";
                    } else {
                        help = help ++ " " ++ name;
                    }
                }
            }
            help = help ++ "\n\n";
        }

        // Description
        if (@hasField(@TypeOf(def), "desc") and def.desc.len > 0) {
            help = help ++ def.desc ++ "\n";
        }

        // Arguments section (positionals)
        const arg_fields = std.meta.fields(@TypeOf(def.args));
        var has_positional = false;
        for (arg_fields) |f| {
            const FT = f.type;
            if (@hasDecl(FT, "is_positional") and FT.is_positional) {
                has_positional = true;
                break;
            }
        }
        if (has_positional) {
            help = help ++ "\nArguments:\n";
            for (arg_fields) |f| {
                const FT = f.type;
                if (@hasDecl(FT, "is_positional") and FT.is_positional) {
                    const spec_val = @field(def.args, f.name);
                    const desc_text = if (@hasField(@TypeOf(spec_val), "desc")) spec_val.desc else "";
                    help = help ++ "  " ++ padRight(fieldToArgName(f.name), 14) ++ desc_text ++ "\n";
                }
            }
        }

        // Options section
        var has_option = false;
        for (arg_fields) |f| {
            const FT = f.type;
            if ((@hasDecl(FT, "is_flag") and FT.is_flag) or (@hasDecl(FT, "is_option") and FT.is_option)) {
                has_option = true;
                break;
            }
        }
        if (has_option) {
            help = help ++ "\nOptions:\n";
            for (arg_fields) |f| {
                const FT = f.type;
                const spec_val = @field(def.args, f.name);
                if (@hasDecl(FT, "is_flag") and FT.is_flag) {
                    help = help ++ formatFlagHelp(spec_val);
                } else if (@hasDecl(FT, "is_option") and FT.is_option) {
                    help = help ++ formatOptionHelp(spec_val, FT);
                }
            }
        }

        // Examples
        if (@hasField(@TypeOf(def), "examples")) {
            help = help ++ "\nExamples:\n";
            for (def.examples) |ex| {
                help = help ++ "  " ++ ex ++ "\n";
            }
        }

        return help;
    }
}

fn padRight(comptime s: []const u8, comptime width: usize) []const u8 {
    if (s.len >= width) return s ++ "  ";
    return s ++ (" " ** (width - s.len));
}

fn fieldToArgName(comptime name: []const u8) []const u8 {
    comptime {
        var upper: [name.len]u8 = undefined;
        for (name, 0..) |c, i| {
            upper[i] = if (c >= 'a' and c <= 'z') c - 32 else if (c == '_') '-' else c;
        }
        return &upper;
    }
}

fn formatFlagHelp(comptime spec: anytype) []const u8 {
    comptime {
        var line: []const u8 = "  ";
        if (spec.short) |s| {
            line = line ++ s ++ ", ";
        } else {
            line = line ++ "    ";
        }
        line = line ++ spec.long;
        const desc = if (@hasField(@TypeOf(spec), "desc")) spec.desc else "";
        line = line ++ makePadding(line.len, 22) ++ desc ++ "\n";
        return line;
    }
}

fn formatOptionHelp(comptime spec: anytype, comptime FT: type) []const u8 {
    comptime {
        var line: []const u8 = "  ";
        if (spec.short) |s| {
            line = line ++ s ++ ", ";
        } else {
            line = line ++ "    ";
        }
        line = line ++ spec.long;
        // Add value placeholder based on type
        if (FT.ValueType == i64 or FT.ValueType == usize) {
            line = line ++ " N";
        } else {
            line = line ++ " S";
        }
        const desc = if (@hasField(@TypeOf(spec), "desc")) spec.desc else "";
        line = line ++ makePadding(line.len, 22) ++ desc ++ "\n";
        return line;
    }
}

fn makePadding(comptime current: usize, comptime target: usize) []const u8 {
    if (current >= target) return "  ";
    return " " ** (target - current);
}

// =============================================================================
// Spec Types - Used to define command arguments
// =============================================================================

/// Marker for boolean flags
pub const FlagSpec = struct {
    short: ?[]const u8,
    long: []const u8,
    desc: []const u8 = "",
    pub const is_flag = true;
    pub const is_option = false;
    pub const is_positional = false;
    pub const T = bool;
    pub const default: T = false;
};

/// Generic option spec - works for any value type
pub fn OptionSpec(comptime ValueT: type, comptime default_val: ?ValueT) type {
    return struct {
        short: ?[]const u8,
        long: []const u8,
        desc: []const u8 = "",
        pub const is_flag = false;
        pub const is_option = true;
        pub const is_positional = false;
        pub const T = if (default_val != null) ValueT else ?ValueT;
        pub const ValueType = ValueT;
        pub const default: T = default_val orelse null;
    };
}

/// Marker for i64 options
pub fn I64OptionSpec(comptime default_val: ?i64) type {
    return OptionSpec(i64, default_val);
}

/// Marker for usize options
pub fn UsizeOptionSpec(comptime default_val: ?usize) type {
    return OptionSpec(usize, default_val);
}

/// Marker for string options
pub fn StringOptionSpec(comptime default_val: ?[]const u8) type {
    return OptionSpec([]const u8, default_val);
}

/// Generic positional spec - works like OptionSpec with optional default
pub fn PositionalSpec(comptime ValueT: type, comptime default_val: ?ValueT) type {
    return struct {
        desc: []const u8 = "",
        pub const is_flag = false;
        pub const is_option = false;
        pub const is_positional = true;
        pub const is_typed_positional = true;
        pub const has_default_value = default_val != null;
        pub const T = ValueT;
        pub const default: T = default_val orelse switch (@typeInfo(ValueT)) {
            .int => 0,
            .pointer => "", // for []const u8
            else => @compileError("unsupported positional type"),
        };
    };
}

/// Marker for i64 positional argument
pub fn I64PositionalSpec(comptime default_val: ?i64) type {
    return PositionalSpec(i64, default_val);
}

/// Marker for string positional argument
pub fn StringPositionalSpec(comptime default_val: ?[]const u8) type {
    return PositionalSpec([]const u8, default_val);
}

/// Create a boolean flag spec
pub fn Flag(comptime opts: struct {
    short: ?[]const u8 = null,
    long: []const u8,
    desc: []const u8 = "",
}) FlagSpec {
    return .{
        .short = if (opts.short) |s| "-" ++ s else null,
        .long = "--" ++ opts.long,
        .desc = opts.desc,
    };
}

/// Create an i64 option spec
pub fn I64Option(comptime opts: struct {
    short: ?[]const u8 = null,
    long: []const u8,
    desc: []const u8 = "",
    default: ?i64 = null,
}) I64OptionSpec(opts.default) {
    return .{
        .short = if (opts.short) |s| "-" ++ s else null,
        .long = "--" ++ opts.long,
        .desc = opts.desc,
    };
}

/// Create a usize option spec
pub fn UsizeOption(comptime opts: struct {
    short: ?[]const u8 = null,
    long: []const u8,
    desc: []const u8 = "",
    default: ?usize = null,
}) UsizeOptionSpec(opts.default) {
    return .{
        .short = if (opts.short) |s| "-" ++ s else null,
        .long = "--" ++ opts.long,
        .desc = opts.desc,
    };
}

/// Create a string option spec
pub fn StringOption(comptime opts: struct {
    short: ?[]const u8 = null,
    long: []const u8,
    desc: []const u8 = "",
    default: ?[]const u8 = null,
}) StringOptionSpec(opts.default) {
    return .{
        .short = if (opts.short) |s| "-" ++ s else null,
        .long = "--" ++ opts.long,
        .desc = opts.desc,
    };
}

// =============================================================================
// Positional Argument Helpers (mirror the Option pattern)
// =============================================================================

/// Create an i64 positional argument (required if no default, optional if default provided)
pub fn I64Positional(comptime opts: struct {
    desc: []const u8 = "",
    default: ?i64 = null,
}) I64PositionalSpec(opts.default) {
    return .{ .desc = opts.desc };
}

/// Create a string positional argument (required if no default, optional if default provided)
pub fn StringPositional(comptime opts: struct {
    desc: []const u8 = "",
    default: ?[]const u8 = null,
}) StringPositionalSpec(opts.default) {
    return .{ .desc = opts.desc };
}

/// Marker for variadic positional arguments (collects all remaining args).
/// Constraints (enforced at comptime):
///   - Only one Rest field allowed per spec
///   - Must be the last positional argument in the spec
pub fn RestSpec(comptime required: bool) type {
    return struct {
        desc: []const u8 = "",
        pub const is_flag = false;
        pub const is_option = false;
        pub const is_positional = true;
        pub const is_rest = true;
        pub const is_required = required;
        pub const T = []const []const u8;
    };
}

/// Create a variadic "rest" positional that collects all remaining arguments.
/// Must be the last positional in the spec; only one Rest allowed.
/// Set required=true to fail parsing if no arguments are provided.
pub fn Rest(comptime opts: struct { desc: []const u8 = "", required: bool = false }) RestSpec(opts.required) {
    return .{ .desc = opts.desc };
}

// =============================================================================
// Args - Main parser type
// =============================================================================

pub fn Args(comptime cmd: []const u8) type {
    return struct {
        const Self = @This();

        args: []const []const u8,
        idx: usize = 0,
        current: []const u8 = "",

        pub fn init(args: []const []const u8) Self {
            return .{ .args = args };
        }

        // =================================================================
        // Declarative API
        // =================================================================

        /// Generate a Result type from a spec and parse args into it.
        ///
        /// Usage:
        /// ```
        /// const r = Args("range").parse(cmd.args, .{
        ///     .step = I64Option(.{ .short = "-s", .long = "--step" }),
        ///     .first = I64Positional(.{ .default = 1 }),
        ///     .last = I64Positional(.{}),
        /// }) catch return 1;
        /// // r.step: ?i64, r.first: i64, r.last: i64
        /// ```
        pub fn parse(args: []const []const u8, comptime spec: anytype) ParseError!Result(@TypeOf(spec)) {
            var self = Self.init(args);
            return self.parseSpec(spec);
        }

        /// Result type generated from a spec
        pub fn Result(comptime SpecT: type) type {
            const spec_fields = std.meta.fields(SpecT);

            // Count fields and detect positional types
            comptime var field_count: usize = 0;
            comptime var has_typed_positional = false;
            comptime var has_rest = false;
            inline for (spec_fields) |sf| {
                const is_typed_pos = @hasDecl(sf.type, "is_typed_positional") and sf.type.is_typed_positional;
                const is_rest = @hasDecl(sf.type, "is_rest") and sf.type.is_rest;
                if (is_typed_pos) has_typed_positional = true;
                if (is_rest) has_rest = true;
                // Include: flags, options, typed positionals, and rest
                if (!sf.type.is_positional or is_typed_pos or is_rest) field_count += 1;
            }

            // Add implicit positional field only if no typed positionals AND no explicit Rest
            const extra_fields: usize = if (has_typed_positional or has_rest) 0 else 1;

            // Build struct fields from spec
            var fields: [field_count + extra_fields]std.builtin.Type.StructField = undefined;
            var i: usize = 0;

            inline for (spec_fields) |sf| {
                const FieldSpec = sf.type;
                const is_typed_pos = @hasDecl(FieldSpec, "is_typed_positional") and FieldSpec.is_typed_positional;
                const is_rest = @hasDecl(FieldSpec, "is_rest") and FieldSpec.is_rest;

                if (!FieldSpec.is_positional or is_typed_pos or is_rest) {
                    if (is_rest) {
                        // Rest field: slice of remaining args
                        fields[i] = .{
                            .name = sf.name,
                            .type = []const []const u8,
                            .default_value_ptr = null,
                            .is_comptime = false,
                            .alignment = @alignOf([]const []const u8),
                        };
                    } else {
                        fields[i] = .{
                            .name = sf.name,
                            .type = FieldSpec.T,
                            .default_value_ptr = @ptrCast(&FieldSpec.default),
                            .is_comptime = false,
                            .alignment = @alignOf(FieldSpec.T),
                        };
                    }
                    i += 1;
                }
            }

            // Add implicit positional field (only if no typed positionals and no Rest)
            if (!has_typed_positional and !has_rest) {
                fields[i] = .{
                    .name = "positional",
                    .type = []const []const u8,
                    .default_value_ptr = null,
                    .is_comptime = false,
                    .alignment = @alignOf([]const []const u8),
                };
                i += 1;
            }

            return @Type(.{
                .@"struct" = .{
                    .layout = .auto,
                    .fields = fields[0..i],
                    .decls = &.{},
                    .is_tuple = false,
                },
            });
        }

        /// Print error message and return exit code 1
        pub fn errMsg(comptime msg: []const u8) u8 {
            io.writeStderr(cmd ++ ": " ++ msg ++ "\n");
            return 1;
        }

        /// Parse string as i64, printing error on failure
        pub fn parseI64(s: []const u8) ?i64 {
            return std.fmt.parseInt(i64, s, 10) catch {
                io.writeStderr(cmd ++ ": invalid number: ");
                io.writeStderr(s);
                io.writeStderr("\n");
                return null;
            };
        }

        fn parseSpec(self: *Self, comptime spec: anytype) ParseError!Result(@TypeOf(spec)) {
            const SpecT = @TypeOf(spec);
            const ResultType = Result(SpecT);
            var result: ResultType = undefined;

            // Detect positional field types at comptime
            const has_typed_positional = comptime blk: {
                for (std.meta.fields(SpecT)) |sf| {
                    if (@hasDecl(sf.type, "is_typed_positional") and sf.type.is_typed_positional) {
                        break :blk true;
                    }
                }
                break :blk false;
            };

            const has_rest = comptime blk: {
                for (std.meta.fields(SpecT)) |sf| {
                    if (@hasDecl(sf.type, "is_rest") and sf.type.is_rest) {
                        break :blk true;
                    }
                }
                break :blk false;
            };

            // Initialize implicit positional field if present
            if (!has_typed_positional and !has_rest) {
                result.positional = &.{};
            }

            // Initialize all non-positional fields to defaults (and Rest fields to empty)
            inline for (std.meta.fields(SpecT)) |sf| {
                const FieldSpec = sf.type;
                const is_typed_pos = @hasDecl(FieldSpec, "is_typed_positional") and FieldSpec.is_typed_positional;
                const is_rest = @hasDecl(FieldSpec, "is_rest") and FieldSpec.is_rest;
                if (is_rest) {
                    @field(result, sf.name) = &.{};
                } else if (!FieldSpec.is_positional or is_typed_pos) {
                    @field(result, sf.name) = FieldSpec.default;
                }
            }

            // Parse flags/options
            while (self.next()) |arg| {
                // End of flags
                if (std.mem.eql(u8, arg, "--")) {
                    break;
                }

                // Not a flag - start of positional args
                // Single "-" is treated as positional (common convention for stdin/previous)
                if (arg.len == 0 or arg[0] != '-' or arg.len == 1 or isNumeric(arg)) {
                    self.idx -= 1; // Rewind
                    break;
                }

                // Try to match against spec
                var matched = false;
                inline for (std.meta.fields(SpecT)) |sf| {
                    const FieldSpec = sf.type;
                    if (FieldSpec.is_positional) continue;

                    const field_val = @field(spec, sf.name);

                    if (FieldSpec.is_flag) {
                        if (self.matchFlag(field_val.short, field_val.long, arg)) {
                            @field(result, sf.name) = true;
                            matched = true;
                        }
                    } else if (FieldSpec.is_option) {
                        if (self.matchOption(field_val.short, field_val.long, arg)) |val| {
                            if (FieldSpec.ValueType == []const u8) {
                                @field(result, sf.name) = val;
                            } else if (FieldSpec.ValueType == i64) {
                                @field(result, sf.name) = std.fmt.parseInt(i64, val, 10) catch
                                    return self.parseFail("invalid number: ", val);
                            } else if (FieldSpec.ValueType == usize) {
                                @field(result, sf.name) = std.fmt.parseInt(usize, val, 10) catch
                                    return self.parseFail("invalid number: ", val);
                            }
                            matched = true;
                        }
                    }
                }

                if (!matched) {
                    return self.parseFail("unknown option: ", arg);
                }
            }

            // Get remaining positional args
            const pos_args = self.rest();

            if (has_typed_positional) {
                // Count typed positionals (not including Rest)
                comptime var positional_count: usize = 0;
                inline for (std.meta.fields(SpecT)) |sf| {
                    const FieldSpec = sf.type;
                    if (@hasDecl(FieldSpec, "is_typed_positional") and FieldSpec.is_typed_positional) {
                        positional_count += 1;
                    }
                }

                // Check minimum args (all typed positionals without defaults are required)
                comptime var required_count: usize = 0;
                inline for (std.meta.fields(SpecT)) |sf| {
                    const FieldSpec = sf.type;
                    if (@hasDecl(FieldSpec, "is_typed_positional") and FieldSpec.is_typed_positional) {
                        if (!FieldSpec.has_default_value) required_count += 1;
                    }
                }

                if (pos_args.len < required_count) {
                    return self.parseFailStatic(if (required_count == 1) "missing operand" else "not enough arguments");
                }

                // Without Rest, check for too many args
                if (!has_rest and pos_args.len > positional_count) {
                    return self.parseFailStatic("too many arguments");
                }

                // Args consumed by typed positionals (up to positional_count)
                const typed_arg_count = @min(pos_args.len, positional_count);
                const optional_args_provided = typed_arg_count -| required_count;

                // Assign to typed positionals
                comptime var opt_idx: usize = 0;
                comptime var req_idx: usize = 0;
                inline for (std.meta.fields(SpecT)) |sf| {
                    const FieldSpec = sf.type;
                    if (@hasDecl(FieldSpec, "is_typed_positional") and FieldSpec.is_typed_positional) {
                        if (FieldSpec.has_default_value) {
                            if (opt_idx < optional_args_provided) {
                                const arg_idx = opt_idx;
                                if (FieldSpec.T == i64) {
                                    @field(result, sf.name) = std.fmt.parseInt(i64, pos_args[arg_idx], 10) catch
                                        return self.parseFail("invalid number: ", pos_args[arg_idx]);
                                } else if (FieldSpec.T == []const u8) {
                                    @field(result, sf.name) = pos_args[arg_idx];
                                }
                            }
                            opt_idx += 1;
                        } else {
                            const arg_idx = optional_args_provided + req_idx;
                            if (FieldSpec.T == i64) {
                                @field(result, sf.name) = std.fmt.parseInt(i64, pos_args[arg_idx], 10) catch
                                    return self.parseFail("invalid number: ", pos_args[arg_idx]);
                            } else if (FieldSpec.T == []const u8) {
                                @field(result, sf.name) = pos_args[arg_idx];
                            }
                            req_idx += 1;
                        }
                    }
                }

                // Assign remaining args to Rest (if present)
                if (has_rest) {
                    const rest_args = pos_args[typed_arg_count..];
                    inline for (std.meta.fields(SpecT)) |sf| {
                        if (@hasDecl(sf.type, "is_rest") and sf.type.is_rest) {
                            if (@hasDecl(sf.type, "is_required") and sf.type.is_required and rest_args.len == 0) {
                                return self.parseFailStatic("missing operand");
                            }
                            @field(result, sf.name) = rest_args;
                        }
                    }
                }
            } else if (has_rest) {
                // Rest only (no typed positionals)
                inline for (std.meta.fields(SpecT)) |sf| {
                    if (@hasDecl(sf.type, "is_rest") and sf.type.is_rest) {
                        if (@hasDecl(sf.type, "is_required") and sf.type.is_required and pos_args.len == 0) {
                            return self.parseFailStatic("missing operand");
                        }
                        @field(result, sf.name) = pos_args;
                    }
                }
            } else {
                // Store raw positional args (no constraints)
                result.positional = pos_args;
            }

            return result;
        }

        /// Print error with dynamic argument and return ParseError
        fn parseFail(_: *const Self, comptime msg: []const u8, arg: []const u8) ParseError {
            io.writeStderr(cmd ++ ": " ++ msg);
            io.writeStderr(arg);
            io.writeStderr("\n");
            return ParseError.ParseFailed;
        }

        /// Print error with static message and return ParseError
        fn parseFailStatic(_: *const Self, comptime msg: []const u8) ParseError {
            io.writeStderr(cmd ++ ": " ++ msg ++ "\n");
            return ParseError.ParseFailed;
        }

        fn matchOption(self: *Self, comptime short: ?[]const u8, comptime long: []const u8, arg: []const u8) ?[]const u8 {
            // Check --flag=value form
            if (std.mem.startsWith(u8, arg, long ++ "=")) {
                return arg[long.len + 1 ..];
            }
            if (short) |s| {
                if (std.mem.startsWith(u8, arg, s ++ "=")) {
                    return arg[s.len + 1 ..];
                }
            }
            // Check --flag value form
            if (self.matchFlag(short, long, arg)) {
                return self.next();
            }
            return null;
        }

        // =================================================================
        // Imperative API (original)
        // =================================================================

        pub fn next(self: *Self) ?[]const u8 {
            if (self.idx >= self.args.len) return null;
            self.current = self.args[self.idx];
            self.idx += 1;
            return self.current;
        }

        pub fn peek(self: *const Self) ?[]const u8 {
            return if (self.idx < self.args.len) self.args[self.idx] else null;
        }

        /// Peek at next arg and consume it if it's a valid i64, returning the value.
        /// Returns default if no next arg, next arg is a flag, or parse fails.
        pub fn peekI64(self: *Self, default: i64) i64 {
            if (self.peek()) |next_arg| {
                if (isNumeric(next_arg)) {
                    _ = self.next();
                    return std.fmt.parseInt(i64, next_arg, 10) catch default;
                }
            }
            return default;
        }

        /// Peek at next arg and consume it if it's a valid usize, returning the value.
        /// Returns default if no next arg, next arg is a flag, or parse fails.
        pub fn peekUsize(self: *Self, default: usize) usize {
            if (self.peek()) |next_arg| {
                if (isNumeric(next_arg) and next_arg[0] != '-') {
                    _ = self.next();
                    return std.fmt.parseInt(usize, next_arg, 10) catch default;
                }
            }
            return default;
        }

        /// Peek at next arg and consume it if it doesn't look like a flag.
        /// Returns the arg or null if no next arg or next arg is a flag.
        pub fn peekString(self: *Self) ?[]const u8 {
            if (self.peek()) |next_arg| {
                if (next_arg.len > 0 and next_arg[0] != '-') {
                    return self.next();
                }
            }
            return null;
        }

        pub fn rest(self: *const Self) []const []const u8 {
            return if (self.idx <= self.args.len) self.args[self.idx..] else &.{};
        }

        pub fn flag(self: *Self, comptime short: ?[]const u8, comptime long: []const u8) bool {
            return self.matchFlag(short, long, self.current);
        }

        pub fn option(self: *Self, comptime short: ?[]const u8, comptime long: []const u8) ?[]const u8 {
            // Check --flag=value form
            if (std.mem.startsWith(u8, self.current, long ++ "=")) {
                return self.current[long.len + 1 ..];
            }
            if (short) |s| {
                if (std.mem.startsWith(u8, self.current, s ++ "=")) {
                    return self.current[s.len + 1 ..];
                }
            }
            // Check --flag value form
            if (self.matchFlag(short, long, self.current)) {
                return self.next();
            }
            return null;
        }

        pub fn isFlag(self: *const Self) bool {
            return self.current.len > 0 and
                self.current[0] == '-' and
                !isNumeric(self.current);
        }

        pub fn isEndOfFlags(self: *const Self) bool {
            return std.mem.eql(u8, self.current, "--");
        }

        fn matchFlag(_: *const Self, comptime short: ?[]const u8, comptime long: []const u8, arg: []const u8) bool {
            if (std.mem.eql(u8, arg, long)) return true;
            if (short) |s| return std.mem.eql(u8, arg, s);
            return false;
        }

        // =================================================================
        // Value Parsing
        // =================================================================

        pub fn asI64(self: *Self, s: []const u8) ?i64 {
            return std.fmt.parseInt(i64, s, 10) catch {
                _ = self.errArg("invalid number: ", s);
                return null;
            };
        }

        pub fn asUsize(self: *Self, s: []const u8) ?usize {
            return std.fmt.parseInt(usize, s, 10) catch {
                _ = self.errArg("invalid number: ", s);
                return null;
            };
        }

        pub fn asI64For(self: *Self, s: []const u8, comptime context: []const u8) ?i64 {
            return std.fmt.parseInt(i64, s, 10) catch {
                _ = self.err(context ++ " must be a number");
                return null;
            };
        }

        pub fn asUsizeFor(self: *Self, s: []const u8, comptime context: []const u8) ?usize {
            return std.fmt.parseInt(usize, s, 10) catch {
                _ = self.err(context ++ " must be a number");
                return null;
            };
        }

        // =================================================================
        // Error Reporting
        // =================================================================

        pub fn err(_: *const Self, comptime msg: []const u8) u8 {
            io.writeStderr(cmd ++ ": " ++ msg ++ "\n");
            return 1;
        }

        pub fn errArg(_: *const Self, comptime msg: []const u8, arg: []const u8) u8 {
            io.writeStderr(cmd ++ ": " ++ msg);
            io.writeStderr(arg);
            io.writeStderr("\n");
            return 1;
        }

        pub fn fail(_: *const Self) u8 {
            return 1;
        }
    };
}

// =============================================================================
// Shared Utilities
// =============================================================================

pub fn isNumeric(s: []const u8) bool {
    if (s.len == 0) return false;
    const start: usize = if (s[0] == '-' and s.len > 1) 1 else 0;
    return std.ascii.isDigit(s[start]);
}

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

test "Args: imperative - basic iteration" {
    const argv = [_][]const u8{ "a", "b", "c" };
    var p = Args("test").init(&argv);

    try testing.expectEqualStrings("a", p.next().?);
    try testing.expectEqualStrings("b", p.next().?);
    try testing.expectEqualStrings("c", p.next().?);
    try testing.expect(p.next() == null);
}

test "Args: imperative - flag matching" {
    const argv = [_][]const u8{ "-v", "--verbose", "arg" };
    var p = Args("test").init(&argv);

    _ = p.next();
    try testing.expect(p.flag("-v", "--verbose"));

    _ = p.next();
    try testing.expect(p.flag("-v", "--verbose"));

    _ = p.next();
    try testing.expect(!p.flag("-v", "--verbose"));
}

test "Args: imperative - option with value" {
    const argv = [_][]const u8{ "--step", "5", "--count=10" };
    var p = Args("test").init(&argv);

    _ = p.next();
    const step = p.option("-s", "--step");
    try testing.expectEqualStrings("5", step.?);

    _ = p.next();
    const count = p.option("-c", "--count");
    try testing.expectEqualStrings("10", count.?);
}

test "Args: declarative - flags" {
    const argv = [_][]const u8{ "-v", "--quiet", "arg1", "arg2" };

    const result = try Args("test").parse(&argv, .{
        .verbose = Flag(.{ .short = "v", .long = "verbose" }),
        .quiet = Flag(.{ .short = "q", .long = "quiet" }),
    });

    try testing.expect(result.verbose == true);
    try testing.expect(result.quiet == true);
    try testing.expectEqual(@as(usize, 2), result.positional.len);
    try testing.expectEqualStrings("arg1", result.positional[0]);
    try testing.expectEqualStrings("arg2", result.positional[1]);
}

test "Args: declarative - options" {
    const argv = [_][]const u8{ "--step", "5", "-n", "10", "arg" };

    const result = try Args("test").parse(&argv, .{
        .step = I64Option(.{ .short = "s", .long = "step" }),
        .count = I64Option(.{ .short = "n", .long = "count" }),
    });

    try testing.expectEqual(@as(?i64, 5), result.step);
    try testing.expectEqual(@as(?i64, 10), result.count);
    try testing.expectEqual(@as(usize, 1), result.positional.len);
}

test "Args: declarative - string options" {
    const argv = [_][]const u8{ "--name=hello", "arg" };

    const result = try Args("test").parse(&argv, .{
        .name = StringOption(.{ .long = "name" }),
    });

    try testing.expectEqualStrings("hello", result.name.?);
}

test "Args: declarative - unknown flag error" {
    const argv = [_][]const u8{"--unknown"};

    const result = Args("test").parse(&argv, .{
        .verbose = Flag(.{ .long = "verbose" }),
    });

    try testing.expectError(ParseError.ParseFailed, result);
}

test "Args: declarative - end of flags" {
    const argv = [_][]const u8{ "-v", "--", "-not-a-flag" };

    const result = try Args("test").parse(&argv, .{
        .verbose = Flag(.{ .short = "v", .long = "verbose" }),
    });

    try testing.expect(result.verbose == true);
    try testing.expectEqual(@as(usize, 1), result.positional.len);
    try testing.expectEqualStrings("-not-a-flag", result.positional[0]);
}

test "Args: declarative - negative numbers as positional" {
    const argv = [_][]const u8{ "-5", "10" };

    const result = try Args("test").parse(&argv, .{
        .verbose = Flag(.{ .short = "v", .long = "verbose" }),
    });

    try testing.expect(result.verbose == false);
    try testing.expectEqual(@as(usize, 2), result.positional.len);
    try testing.expectEqualStrings("-5", result.positional[0]);
}

test "isNumeric" {
    try testing.expect(isNumeric("42"));
    try testing.expect(isNumeric("-5"));
    try testing.expect(isNumeric("0"));
    try testing.expect(!isNumeric(""));
    try testing.expect(!isNumeric("-"));
    try testing.expect(!isNumeric("--foo"));
    try testing.expect(!isNumeric("-abc"));
}

test "Args: peekI64 - valid number" {
    const argv = [_][]const u8{ "--flag", "42", "rest" };
    var p = Args("test").init(&argv);
    // idx starts at 0
    _ = p.next(); // consume --flag, idx becomes 1
    try testing.expectEqual(@as(i64, 42), p.peekI64(0));
    try testing.expectEqual(@as(usize, 2), p.idx); // consumed "42"
}

test "Args: peekI64 - negative number" {
    const argv = [_][]const u8{ "-5" };
    var p = Args("test").init(&argv);
    // idx starts at 0, so peek sees "-5"
    try testing.expectEqual(@as(i64, -5), p.peekI64(0));
}

test "Args: peekI64 - not a number returns default" {
    const argv = [_][]const u8{"--next"};
    var p = Args("test").init(&argv);
    // idx starts at 0, peek sees "--next" which is not numeric
    try testing.expectEqual(@as(i64, 99), p.peekI64(99));
    try testing.expectEqual(@as(usize, 0), p.idx); // not consumed
}

test "Args: peekI64 - no next arg returns default" {
    const argv = [_][]const u8{};
    var p = Args("test").init(&argv);
    // idx starts at 0, no more args
    try testing.expectEqual(@as(i64, 99), p.peekI64(99));
}

test "Args: peekUsize - valid positive number" {
    const argv = [_][]const u8{ "42" };
    var p = Args("test").init(&argv);
    // idx starts at 0, peek sees "42"
    try testing.expectEqual(@as(usize, 42), p.peekUsize(0));
}

test "Args: peekUsize - negative returns default" {
    const argv = [_][]const u8{"-5"};
    var p = Args("test").init(&argv);
    // idx starts at 0, peek sees "-5" which is negative
    try testing.expectEqual(@as(usize, 99), p.peekUsize(99));
    try testing.expectEqual(@as(usize, 0), p.idx); // not consumed
}

test "Args: peekString - valid non-flag" {
    const argv = [_][]const u8{ "hello" };
    var p = Args("test").init(&argv);
    // idx starts at 0, peek sees "hello"
    try testing.expectEqualStrings("hello", p.peekString().?);
}

test "Args: peekString - flag returns null" {
    const argv = [_][]const u8{"--flag"};
    var p = Args("test").init(&argv);
    // idx starts at 0, peek sees "--flag" which looks like a flag
    try testing.expect(p.peekString() == null);
    try testing.expectEqual(@as(usize, 0), p.idx); // not consumed
}

// =============================================================================
// Typed Positional Tests
// =============================================================================

test "Args: typed positionals - required only" {
    const argv = [_][]const u8{ "42" };

    const result = try Args("test").parse(&argv, .{
        .value = I64Positional(.{}),
    });

    try testing.expectEqual(@as(i64, 42), result.value);
}

test "Args: typed positionals - optional with default" {
    const argv = [_][]const u8{};

    const result = try Args("test").parse(&argv, .{
        .value = I64Positional(.{ .default = 100 }),
    });

    try testing.expectEqual(@as(i64, 100), result.value);
}

test "Args: typed positionals - optional provided" {
    const argv = [_][]const u8{ "42" };

    const result = try Args("test").parse(&argv, .{
        .value = I64Positional(.{ .default = 100 }),
    });

    try testing.expectEqual(@as(i64, 42), result.value);
}

test "Args: typed positionals - optional then required (1 arg)" {
    // range 5 -> first=1 (default), last=5
    const argv = [_][]const u8{ "5" };

    const result = try Args("test").parse(&argv, .{
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectEqual(@as(i64, 1), result.first); // default
    try testing.expectEqual(@as(i64, 5), result.last); // provided
}

test "Args: typed positionals - optional then required (2 args)" {
    // range 3 7 -> first=3, last=7
    const argv = [_][]const u8{ "3", "7" };

    const result = try Args("test").parse(&argv, .{
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectEqual(@as(i64, 3), result.first);
    try testing.expectEqual(@as(i64, 7), result.last);
}

test "Args: typed positionals - missing required" {
    const argv = [_][]const u8{};

    const result = Args("test").parse(&argv, .{
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectError(ParseError.ParseFailed, result);
}

test "Args: typed positionals - too many" {
    const argv = [_][]const u8{ "1", "2", "3" };

    const result = Args("test").parse(&argv, .{
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectError(ParseError.ParseFailed, result);
}

test "Args: typed positionals - string args" {
    const argv = [_][]const u8{ "hello" };

    const result = try Args("test").parse(&argv, .{
        .name = StringPositional(.{}),
    });

    try testing.expectEqualStrings("hello", result.name);
}

test "Args: typed positionals - string with default" {
    const argv = [_][]const u8{};

    const result = try Args("test").parse(&argv, .{
        .name = StringPositional(.{ .default = "world" }),
    });

    try testing.expectEqualStrings("world", result.name);
}

test "Args: typed positionals - with options" {
    const argv = [_][]const u8{ "--step", "2", "5" };

    const result = try Args("test").parse(&argv, .{
        .step = I64Option(.{ .short = "s", .long = "step" }),
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectEqual(@as(?i64, 2), result.step);
    try testing.expectEqual(@as(i64, 1), result.first);
    try testing.expectEqual(@as(i64, 5), result.last);
}

test "Args: typed positionals - with options and both args" {
    const argv = [_][]const u8{ "-s", "2", "3", "10" };

    const result = try Args("test").parse(&argv, .{
        .step = I64Option(.{ .short = "s", .long = "step" }),
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectEqual(@as(?i64, 2), result.step);
    try testing.expectEqual(@as(i64, 3), result.first);
    try testing.expectEqual(@as(i64, 10), result.last);
}

test "Args: typed positionals - negative numbers" {
    const argv = [_][]const u8{ "-5", "10" };

    const result = try Args("test").parse(&argv, .{
        .first = I64Positional(.{ .default = 1 }),
        .last = I64Positional(.{}),
    });

    try testing.expectEqual(@as(i64, -5), result.first);
    try testing.expectEqual(@as(i64, 10), result.last);
}

// =============================================================================
// Rest (variadic) Tests
// =============================================================================

test "Args: Rest - collects all args" {
    const argv = [_][]const u8{ "a", "b", "c" };

    const result = try Args("test").parse(&argv, .{
        .args = Rest(.{}),
    });

    try testing.expectEqual(@as(usize, 3), result.args.len);
    try testing.expectEqualStrings("a", result.args[0]);
    try testing.expectEqualStrings("b", result.args[1]);
    try testing.expectEqualStrings("c", result.args[2]);
}

test "Args: Rest - with flags" {
    const argv = [_][]const u8{ "-n", "hello", "world" };

    const result = try Args("test").parse(&argv, .{
        .n = Flag(.{ .short = "n", .long = "n" }),
        .values = Rest(.{}),
    });

    try testing.expect(result.n == true);
    try testing.expectEqual(@as(usize, 2), result.values.len);
    try testing.expectEqualStrings("hello", result.values[0]);
    try testing.expectEqualStrings("world", result.values[1]);
}

test "Args: Rest - empty" {
    const argv = [_][]const u8{};

    const result = try Args("test").parse(&argv, .{
        .args = Rest(.{}),
    });

    try testing.expectEqual(@as(usize, 0), result.args.len);
}

test "Args: Rest - flag only, no rest args" {
    const argv = [_][]const u8{ "-v" };

    const result = try Args("test").parse(&argv, .{
        .v = Flag(.{ .short = "v", .long = "verbose" }),
        .args = Rest(.{}),
    });

    try testing.expect(result.v == true);
    try testing.expectEqual(@as(usize, 0), result.args.len);
}
