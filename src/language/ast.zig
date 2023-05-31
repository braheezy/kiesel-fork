const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const tokenizer = @import("tokenizer.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");
const line_terminators = tokenizer.line_terminators;

const Agent = execution.Agent;
const BigInt = types.BigInt;
const Environment = execution.Environment;
const Executable = bytecode.Executable;
const Object = types.Object;
const PrivateEnvironment = execution.PrivateEnvironment;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const noexcept = utils.noexcept;
const ordinaryFunctionCreate = builtins.ordinaryFunctionCreate;
const setFunctionName = builtins.setFunctionName;

const BytecodeError = error{ OutOfMemory, IndexOutOfRange };

pub const BytecodeContext = struct {
    agent: *Agent,
    contained_in_strict_mode_code: bool = false,
};

fn printIndentation(writer: anytype, indentation: usize) !void {
    var i: usize = 0;
    while (i < indentation) : (i += 1)
        try writer.print("  ", .{});
}

fn printString(string: []const u8, writer: anytype, indentation: usize) !void {
    try printIndentation(writer, indentation);
    try writer.print("{s}\n", .{string});
}

const AnalyzeQuery = enum {
    is_reference,
    is_string_literal,
};

/// https://tc39.es/ecma262/#prod-ParenthesizedExpression
pub const ParenthesizedExpression = struct {
    const Self = @This();

    expression: *Expression,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => self.expression.analyze(query),
            .is_string_literal => false,
        };
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        try self.expression.generateBytecode(executable, ctx);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("ParenthesizedExpression", writer, indentation);
        try self.expression.print(writer, indentation + 1);
    }
};

/// https://tc39.es/ecma262/#prod-IdentifierReference
pub const IdentifierReference = struct {
    const Self = @This();

    identifier: Identifier,

    /// 13.1.3 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-identifiers-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // IdentifierReference : Identifier
        // IdentifierReference : yield
        // IdentifierReference : await
        // 1. Return ? ResolveBinding(StringValue of Identifier).
        try executable.addInstructionWithIdentifier(.resolve_binding, self.identifier);
        const strict = ctx.contained_in_strict_mode_code;
        try executable.addIndex(@boolToInt(strict));
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("IdentifierReference", writer, indentation);
        try printString(self.identifier, writer, indentation + 1);
    }
};

/// https://tc39.es/ecma262/#prod-Identifier
pub const Identifier = []const u8;

/// https://tc39.es/ecma262/#prod-PrimaryExpression
pub const PrimaryExpression = union(enum) {
    const Self = @This();

    this,
    identifier_reference: IdentifierReference,
    literal: Literal,
    parenthesized_expression: ParenthesizedExpression,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => switch (self) {
                .identifier_reference => true,
                .parenthesized_expression => |parenthesized_expression| parenthesized_expression.analyze(query),
                .this, .literal => false,
            },
            .is_string_literal => switch (self) {
                .literal => |literal| literal.analyze(query),
                else => false,
            },
        };
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) BytecodeError!void {
        switch (self) {
            // PrimaryExpression : this
            .this => {
                // 1. Return ? ResolveThisBinding().
                try executable.addInstruction(.resolve_this_binding);
            },

            .identifier_reference => |identifier_reference| try identifier_reference.generateBytecode(executable, ctx),
            .literal => |literal| try literal.generateBytecode(executable, ctx),
            .parenthesized_expression => |parenthesized_expression| try parenthesized_expression.generateBytecode(executable, ctx),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        // Omit printing 'PrimaryExpression' here, it's implied and only adds nesting.
        switch (self) {
            .this => try printString("this", writer, indentation),
            .identifier_reference => |identifier_reference| try identifier_reference.print(
                writer,
                indentation,
            ),
            .literal => |literal| try literal.print(writer, indentation),
            .parenthesized_expression => |parenthesized_expression| try parenthesized_expression.print(
                writer,
                indentation,
            ),
        }
    }
};

/// https://tc39.es/ecma262/#prod-MemberExpression
pub const MemberExpression = struct {
    const Self = @This();

    pub const Property = union(enum) {
        expression: *Expression,
        identifier: Identifier,
    };

    expression: *Expression,
    property: Property,

    /// 13.3.2.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-property-accessors-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // 1. Let baseReference be ? Evaluation of MemberExpression.
        try self.expression.generateBytecode(executable, ctx);

        // 2. Let baseValue be ? GetValue(baseReference).
        if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);

        // 3. If the source text matched by this MemberExpression is strict mode code, let strict
        //    be true; else let strict be false.
        const strict = ctx.contained_in_strict_mode_code;

        switch (self.property) {
            // MemberExpression : MemberExpression [ Expression ]
            .expression => |expression| {
                // 4. Return ? EvaluatePropertyAccessWithExpressionKey(baseValue, Expression, strict).
                try expression.generateBytecode(executable, ctx);
                try executable.addInstruction(.load);
                try executable.addInstruction(.evaluate_property_access_with_expression_key);
                try executable.addIndex(@boolToInt(strict));
            },

            // MemberExpression : MemberExpression . IdentifierName
            .identifier => |identifier| {
                // 4. Return EvaluatePropertyAccessWithIdentifierKey(baseValue, IdentifierName, strict).
                try executable.addInstructionWithIdentifier(
                    .evaluate_property_access_with_identifier_key,
                    identifier,
                );
                try executable.addIndex(@boolToInt(strict));
            },
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("MemberExpression", writer, indentation);
        try printString("expression:", writer, indentation + 1);
        try self.expression.print(writer, indentation + 2);
        try printString("property:", writer, indentation + 1);
        switch (self.property) {
            .expression => |expression| try expression.print(writer, indentation + 2),
            .identifier => |identifier| try printString(identifier, writer, indentation + 2),
        }
    }
};

/// https://tc39.es/ecma262/#prod-CallExpression
pub const CallExpression = struct {
    const Self = @This();

    expression: *Expression,
    arguments: Arguments,

    /// 13.3.6.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-function-calls-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // CallExpression : CallExpression Arguments
        // 1. Let ref be ? Evaluation of CallExpression.
        try self.expression.generateBytecode(executable, ctx);

        try executable.addInstruction(.set_evaluation_context_reference);

        // 2. Let func be ? GetValue(ref).
        if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);

        // TODO: 3. Let thisCall be this CallExpression.
        // TODO: 4. Let tailCall be IsInTailPosition(thisCall).

        try executable.addInstruction(.load_this_value);

        for (self.arguments) |argument| {
            try argument.generateBytecode(executable, ctx);
            if (argument.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);
        }

        const strict = ctx.contained_in_strict_mode_code;

        // 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
        try executable.addInstruction(.evaluate_call);
        try executable.addIndex(self.arguments.len);
        try executable.addIndex(@boolToInt(strict));
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("CallExpression", writer, indentation);
        try printString("expression:", writer, indentation + 1);
        try self.expression.print(writer, indentation + 2);
        try printString("arguments:", writer, indentation + 1);
        for (self.arguments) |argument| {
            try argument.print(writer, indentation + 2);
        }
    }
};

/// https://tc39.es/ecma262/#prod-Arguments
pub const Arguments = []const Expression;

/// https://tc39.es/ecma262/#prod-Literal
pub const Literal = union(enum) {
    const Self = @This();

    null,
    boolean: bool,
    numeric: NumericLiteral,
    string: StringLiteral,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => false,
            .is_string_literal => self == .string,
        };
    }

    /// 13.2.3.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, _: *BytecodeContext) !void {
        switch (self) {
            // Literal : NullLiteral
            .null => {
                // 1. Return null.
                try executable.addInstructionWithConstant(.store_constant, .null);
            },

            // Literal : BooleanLiteral
            .boolean => |boolean| {
                // 1. If BooleanLiteral is the token false, return false.
                // 2. If BooleanLiteral is the token true, return true.
                try executable.addInstructionWithConstant(.store_constant, Value.from(boolean));
            },

            // Literal : NumericLiteral
            .numeric => |numeric_literal| {
                // 1. Return the NumericValue of NumericLiteral as defined in 12.9.3.
                const value = try numeric_literal.numericValue(executable.allocator);
                try executable.addInstructionWithConstant(.store_constant, value);
            },

            // Literal : StringLiteral
            .string => |string_literal| {
                // 1. Return the SV of StringLiteral as defined in 12.9.4.2.
                const value = try string_literal.stringValue(executable.allocator);
                try executable.addInstructionWithConstant(.store_constant, value);
            },
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("Literal", writer, indentation);
        switch (self) {
            .null => try printString("null", writer, indentation + 1),
            .boolean => |boolean| try printString(
                if (boolean) "true" else "false",
                writer,
                indentation + 1,
            ),
            .numeric => |numeric_literal| try printString(
                numeric_literal.text,
                writer,
                indentation + 1,
            ),
            .string => |string_literal| try printString(
                string_literal.text,
                writer,
                indentation + 1,
            ),
        }
    }
};

/// https://tc39.es/ecma262/#prod-NumericLiteral
pub const NumericLiteral = struct {
    const Self = @This();

    pub const System = enum {
        binary,
        octal,
        decimal,
        hexadecimal,
    };

    pub const Production = enum {
        /// Anything else
        regular,
        /// https://tc39.es/ecma262/#prod-LegacyOctalIntegerLiteral
        legacy_octal_integer_literal,
        /// https://tc39.es/ecma262/#prod-NonOctalDecimalIntegerLiteral
        non_octal_decimal_integer_literal,
    };

    pub const Type = enum {
        number,
        big_int,
    };

    text: []const u8,
    system: System,
    production: Production,
    type: Type,

    /// 12.9.3.3 Static Semantics: NumericValue
    /// https://tc39.es/ecma262/#sec-numericvalue
    pub fn numericValue(self: Self, allocator: Allocator) !Value {
        const base: u8 = switch (self.system) {
            .binary => 2,
            .octal => 8,
            .decimal => 10,
            .hexadecimal => 16,
        };
        const start: usize = if (self.production == .legacy_octal_integer_literal)
            1 // Strip leading zero
        else if (self.system != .decimal)
            2 // Strip 0b/0o/0x prefix
        else
            0;
        const end: usize = if (self.type == .big_int)
            self.text.len - 1 // Strip trailing 'n'
        else
            self.text.len;
        const str = self.text[start..end];
        switch (self.type) {
            .number => {
                const number = switch (self.system) {
                    .decimal => std.fmt.parseFloat(f64, str) catch unreachable,
                    else => @intToFloat(
                        f64,
                        std.fmt.parseInt(i128, str, base) catch unreachable,
                    ),
                };
                return Value.from(number);
            },
            .big_int => {
                var big_int = BigInt{ .value = try BigInt.Value.init(allocator) };
                big_int.value.setString(base, str) catch |err| switch (err) {
                    error.InvalidBase, error.InvalidCharacter => unreachable,
                    error.OutOfMemory => return error.OutOfMemory,
                };
                return Value.from(big_int);
            },
        }
    }
};

/// https://tc39.es/ecma262/#prod-StringLiteral
pub const StringLiteral = struct {
    const Self = @This();

    text: []const u8,

    /// 12.9.4.2 Static Semantics: SV
    /// https://tc39.es/ecma262/#sec-static-semantics-sv
    pub fn stringValue(self: Self, allocator: Allocator) !Value {
        // TODO: Implement remaining escape sequence types
        std.debug.assert(self.text.len >= 2);
        var str = try std.ArrayList(u8).initCapacity(allocator, self.text.len - 2);
        var i: usize = 1;
        while (i <= self.text.len - 2) : (i += 1) {
            const c = self.text[i];
            if (c == '\\') {
                for (line_terminators) |line_terminator| {
                    if (std.mem.startsWith(u8, self.text[i + 1 ..], line_terminator)) {
                        i += line_terminator.len;
                        break;
                    }
                } else {
                    const next_c = self.text[i + 1];
                    i += 1;
                    switch (next_c) {
                        '0' => str.appendAssumeCapacity(0),
                        'b' => str.appendAssumeCapacity(0x08),
                        'f' => str.appendAssumeCapacity(0x0c),
                        'n' => str.appendAssumeCapacity('\n'),
                        'r' => str.appendAssumeCapacity('\r'),
                        't' => str.appendAssumeCapacity('\t'),
                        'v' => str.appendAssumeCapacity(0x0b),
                        else => str.appendAssumeCapacity(next_c),
                    }
                }
            } else str.appendAssumeCapacity(c);
        }
        return Value.from(str.items);
    }
};

/// https://tc39.es/ecma262/#prod-UnaryExpression
pub const UnaryExpression = struct {
    const Self = @This();

    pub const Operator = enum {
        void,
        typeof,
        @"+",
        @"-",
        @"~",
        @"!",
    };

    operator: Operator,
    expression: *Expression,

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) BytecodeError!void {
        switch (self.operator) {
            // 13.5.2.1 Runtime Semantics: Evaluation
            // https://tc39.es/ecma262/#sec-void-operator-runtime-semantics-evaluation
            // UnaryExpression : void UnaryExpression
            .void => {
                // 1. Let expr be ? Evaluation of UnaryExpression.
                try self.expression.generateBytecode(executable, ctx);

                // 2. Perform ? GetValue(expr).
                if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

                // 3. Return undefined.
                try executable.addInstructionWithConstant(.store_constant, .undefined);
            },

            // 13.5.3.1 Runtime Semantics: Evaluation
            // https://tc39.es/ecma262/#sec-typeof-operator-runtime-semantics-evaluation
            // UnaryExpression : typeof UnaryExpression
            .typeof => {
                // NOTE: get_value is intentionally omitted here, typeof needs to do it conditionally.
                try self.expression.generateBytecode(executable, ctx);
                try executable.addInstruction(.typeof);
            },

            // 13.5.4.1 Runtime Semantics: Evaluation
            // https://tc39.es/ecma262/#sec-unary-plus-operator-runtime-semantics-evaluation
            // UnaryExpression : + UnaryExpression
            .@"+" => {
                // 1. Let expr be ? Evaluation of UnaryExpression.
                try self.expression.generateBytecode(executable, ctx);

                // 2. Return ? ToNumber(? GetValue(expr)).
                if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.to_number);
            },

            // 13.5.5.1 Runtime Semantics: Evaluation
            // https://tc39.es/ecma262/#sec-unary-minus-operator-runtime-semantics-evaluation
            // UnaryExpression : - UnaryExpression
            .@"-" => {
                // 1. Let expr be ? Evaluation of UnaryExpression.
                try self.expression.generateBytecode(executable, ctx);

                // 2. Let oldValue be ? ToNumeric(? GetValue(expr)).
                if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.to_numeric);

                // 3. If oldValue is a Number, then
                //     a. Return Number::unaryMinus(oldValue).
                // 4. Else,
                //     a. Assert: oldValue is a BigInt.
                //     b. Return BigInt::unaryMinus(oldValue).
                try executable.addInstruction(.unary_minus);
            },

            // 13.5.6.1 Runtime Semantics: Evaluation
            // https://tc39.es/ecma262/#sec-bitwise-not-operator-runtime-semantics-evaluation
            // UnaryExpression : ~ UnaryExpression
            .@"~" => {
                // 1. Let expr be ? Evaluation of UnaryExpression.
                try self.expression.generateBytecode(executable, ctx);

                // 2. Let oldValue be ? ToNumeric(? GetValue(expr)).
                if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.to_numeric);

                // 3. If oldValue is a Number, then
                //     a. Return Number::bitwiseNOT(oldValue).
                // 4. Else,
                //     a. Assert: oldValue is a BigInt.
                //     b. Return BigInt::bitwiseNOT(oldValue).
                try executable.addInstruction(.bitwise_not);
            },

            // 13.5.7.1 Runtime Semantics: Evaluation
            // https://tc39.es/ecma262/#sec-logical-not-operator-runtime-semantics-evaluation
            // UnaryExpression : ! UnaryExpression
            .@"!" => {
                // 1. Let expr be ? Evaluation of UnaryExpression.
                try self.expression.generateBytecode(executable, ctx);

                // 2. Let oldValue be ToBoolean(? GetValue(expr)).
                if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

                // 3. If oldValue is true, return false.
                // 4. Return true.
                try executable.addInstruction(.logical_not);
            },
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        try printString("UnaryExpression", writer, indentation);
        try printString("operator:", writer, indentation + 1);
        try printString(@tagName(self.operator), writer, indentation + 2);
        try printString("expression:", writer, indentation + 1);
        try self.expression.print(writer, indentation + 2);
    }
};

/// https://tc39.es/ecma262/#prod-Expression
pub const Expression = union(enum) {
    const Self = @This();

    primary_expression: PrimaryExpression,
    member_expression: MemberExpression,
    call_expression: CallExpression,
    unary_expression: UnaryExpression,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => switch (self) {
                .primary_expression => |primary_expression| primary_expression.analyze(query),
                .member_expression => true,
                .call_expression => false,
                .unary_expression => false,
            },
            .is_string_literal => switch (self) {
                .primary_expression => |primary_expression| primary_expression.analyze(query),
                else => false,
            },
        };
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) BytecodeError!void {
        switch (self) {
            .primary_expression => |primary_expression| try primary_expression.generateBytecode(executable, ctx),
            .member_expression => |member_expression| try member_expression.generateBytecode(executable, ctx),
            .call_expression => |call_expression| try call_expression.generateBytecode(executable, ctx),
            .unary_expression => |unary_expression| try unary_expression.generateBytecode(executable, ctx),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        try printString("Expression", writer, indentation);
        switch (self) {
            .primary_expression => |primary_expression| try primary_expression.print(
                writer,
                indentation + 1,
            ),
            .member_expression => |member_expression| try member_expression.print(
                writer,
                indentation + 1,
            ),
            .call_expression => |call_expression| try call_expression.print(
                writer,
                indentation + 1,
            ),
            .unary_expression => |unary_expression| try unary_expression.print(
                writer,
                indentation + 1,
            ),
        }
    }
};

/// https://tc39.es/ecma262/#prod-Statement
pub const Statement = union(enum) {
    const Self = @This();

    block_statement: BlockStatement,
    empty_statement,
    expression_statement: ExpressionStatement,
    if_statement: IfStatement,
    breakable_statement: BreakableStatement,
    throw_statement: ThrowStatement,
    debugger_statement,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => switch (self) {
                .expression_statement => |expression_statement| expression_statement.analyze(query),
                else => false,
            },
            .is_string_literal => switch (self) {
                .expression_statement => |expression_statement| expression_statement.analyze(query),
                else => false,
            },
        };
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) BytecodeError!void {
        switch (self) {
            .block_statement => |block_statement| {
                try block_statement.generateBytecode(executable, ctx);
            },

            // EmptyStatement : ;
            .empty_statement => {
                // 1. Return empty.
            },

            .expression_statement => |expression_statement| {
                try expression_statement.generateBytecode(executable, ctx);
            },
            .if_statement => |if_statement| try if_statement.generateBytecode(executable, ctx),
            .breakable_statement => |breakable_statement| {
                try breakable_statement.generateBytecode(executable, ctx);
            },
            .throw_statement => |throw_statement| {
                try throw_statement.generateBytecode(executable, ctx);
            },

            // DebuggerStatement : debugger ;
            .debugger_statement => {
                // 1.If an implementation-defined debugging facility is available and enabled, then
                //     a. Perform an implementation-defined debugging action.
                //     b. Return a new implementation-defined Completion Record.
                // 2. Else,
                //     a. Return empty.
            },
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        try printString("Statement", writer, indentation);
        switch (self) {
            .block_statement => |block_statement| try block_statement.print(
                writer,
                indentation + 1,
            ),
            .empty_statement => try printString("empty", writer, indentation + 1),
            .expression_statement => |expression_statement| try expression_statement.print(
                writer,
                indentation + 1,
            ),
            .if_statement => |if_statement| try if_statement.print(writer, indentation + 1),
            .breakable_statement => |breakable_statement| try breakable_statement.print(
                writer,
                indentation + 1,
            ),
            .throw_statement => |throw_statement| try throw_statement.print(
                writer,
                indentation + 1,
            ),
            .debugger_statement => try printString("debugger", writer, indentation + 1),
        }
    }
};

/// https://tc39.es/ecma262/#prod-Declaration
pub const Declaration = union(enum) {
    const Self = @This();

    hoistable_declaration: HoistableDeclaration,

    pub fn analyze(_: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => false,
            .is_string_literal => false,
        };
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        switch (self) {
            .hoistable_declaration => |hoistable_declaration| try hoistable_declaration.generateBytecode(executable, ctx),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("Declaration", writer, indentation);
        switch (self) {
            .hoistable_declaration => |hoistable_declaration| try hoistable_declaration.print(
                writer,
                indentation + 1,
            ),
        }
    }
};

/// https://tc39.es/ecma262/#prod-HoistableDeclaration
pub const HoistableDeclaration = union(enum) {
    const Self = @This();

    function_declaration: FunctionDeclaration,

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        switch (self) {
            .function_declaration => |function_declaration| try function_declaration.generateBytecode(executable, ctx),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        // Omit printing 'HoistableDeclaration' here, it's implied and only adds nesting.
        switch (self) {
            .function_declaration => |function_declaration| try function_declaration.print(
                writer,
                indentation,
            ),
        }
    }
};

/// https://tc39.es/ecma262/#prod-BreakableStatement
pub const BreakableStatement = union(enum) {
    const Self = @This();

    iteration_statement: IterationStatement,

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        switch (self) {
            .iteration_statement => |iteration_statement| try iteration_statement.generateBytecode(executable, ctx),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'BreakableStatement' here, it's implied and only adds nesting.
        switch (self) {
            .iteration_statement => |iteration_statement| try iteration_statement.print(
                writer,
                indentation,
            ),
        }
    }
};

/// https://tc39.es/ecma262/#prod-BlockStatement
pub const BlockStatement = struct {
    const Self = @This();

    block: Block,

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        try self.block.generateBytecode(executable, ctx);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'BlockStatement' here, it's implied and only adds nesting.
        try self.block.print(writer, indentation);
    }
};

/// https://tc39.es/ecma262/#prod-Block
pub const Block = struct {
    const Self = @This();

    statement_list: StatementList,

    /// 14.2.2 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-block-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) BytecodeError!void {
        // Block : { }
        if (self.statement_list.items.len == 0) {
            // 1. Return empty.
            return;
        }

        // Block : { StatementList }
        // TODO: 1-4, 6
        // 5. Let blockValue be Completion(Evaluation of StatementList).
        // 7. Return ? blockValue.
        try self.statement_list.generateBytecode(executable, ctx);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        try printString("Block", writer, indentation);
        try self.statement_list.print(writer, indentation + 1);
    }
};

/// https://tc39.es/ecma262/#prod-StatementList
pub const StatementList = struct {
    const Self = @This();

    items: []const StatementListItem,

    /// 11.2.1 Directive Prologues and the Use Strict Directive
    /// https://tc39.es/ecma262/#sec-directive-prologues-and-the-use-strict-directive
    pub fn containsDirective(self: Self, directive: []const u8) bool {
        for (self.items) |item| {
            if (!item.analyze(.is_string_literal)) break;
            const string_literal = item.statement.expression_statement.expression.primary_expression.literal.string;
            const raw_string_value = string_literal.text[1 .. string_literal.text.len - 1];
            if (std.mem.eql(u8, raw_string_value, directive)) return true;
        }
        return false;
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // StatementList : StatementList StatementListItem
        // 1. Let sl be ? Evaluation of StatementList.
        // 2. Let s be Completion(Evaluation of StatementListItem).
        // 3. Return ? UpdateEmpty(s, sl).
        for (self.items) |item| {
            try item.generateBytecode(executable, ctx);
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'StatementList' here, it's implied and only adds nesting.
        for (self.items) |item| {
            try item.print(writer, indentation);
        }
    }
};

/// https://tc39.es/ecma262/#prod-StatementListItem
pub const StatementListItem = union(enum) {
    const Self = @This();

    statement: *Statement,
    declaration: *Declaration,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (self) {
            .statement => |statement| statement.analyze(query),
            .declaration => |declaration| declaration.analyze(query),
        };
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        switch (self) {
            .statement => |statement| try statement.generateBytecode(executable, ctx),
            .declaration => |declaration| try declaration.generateBytecode(executable, ctx),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'StatementListItem' here, it's implied and only adds nesting.
        switch (self) {
            .statement => |statement| try statement.print(writer, indentation),
            .declaration => |declaration| try declaration.print(writer, indentation),
        }
    }
};

/// https://tc39.es/ecma262/#prod-BindingElement
pub const BindingElement = struct {
    const Self = @This();

    identifier: Identifier,
    // TODO: Binding patterns, initializers

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("BindingElement", writer, indentation);
        try printString(self.identifier, writer, indentation + 1);
    }
};

/// https://tc39.es/ecma262/#prod-ExpressionStatement
pub const ExpressionStatement = struct {
    const Self = @This();

    expression: Expression,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return self.expression.analyze(query);
    }

    /// 14.5.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-expression-statement-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // ExpressionStatement : Expression ;
        // 1. Let exprRef be ? Evaluation of Expression.
        try self.expression.generateBytecode(executable, ctx);

        // 2. Return ? GetValue(exprRef).
        if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'ExpressionStatement' here, it's implied and only adds nesting.
        try self.expression.print(writer, indentation);
    }
};

/// https://tc39.es/ecma262/#prod-IfStatement
pub const IfStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,
    alternate_statement: ?*Statement,

    /// 14.6.2 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-if-statement-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // 1. Let exprRef be ? Evaluation of Expression.
        try self.test_expression.generateBytecode(executable, ctx);

        // 2. Let exprValue be ToBoolean(? GetValue(exprRef)).
        if (self.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const alternate_jump = try executable.addJumpIndex();

        // 3. If exprValue is true, then
        try consequent_jump.setTargetHere();
        try executable.addInstructionWithConstant(.store_constant, .undefined);

        // a. Let stmtCompletion be Completion(Evaluation of the first Statement).
        try self.consequent_statement.generateBytecode(executable, ctx);
        try executable.addInstruction(.jump);
        const end_jump = try executable.addJumpIndex();

        // 4. Else,
        try alternate_jump.setTargetHere();
        try executable.addInstructionWithConstant(.store_constant, .undefined);

        if (self.alternate_statement) |alternate_statement| {
            // a. Let stmtCompletion be Completion(Evaluation of the second Statement).
            try alternate_statement.generateBytecode(executable, ctx);
        }

        // 5. Return ? UpdateEmpty(stmtCompletion, undefined).
        // NOTE: This is handled by the store_constant before the consequent/alternate statements.

        try end_jump.setTargetHere();
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("IfStatement", writer, indentation);
        try printString("test:", writer, indentation + 1);
        try self.test_expression.print(writer, indentation + 2);
        try printString("consequent:", writer, indentation + 1);
        try self.consequent_statement.print(writer, indentation + 2);
        if (self.alternate_statement) |alternate_statement| {
            try printString("alternate:", writer, indentation + 1);
            try alternate_statement.print(writer, indentation + 2);
        }
    }
};

/// https://tc39.es/ecma262/#prod-IterationStatement
pub const IterationStatement = union(enum) {
    const Self = @This();

    do_while_statement: DoWhileStatement,
    while_statement: WhileStatement,

    /// 14.7.1.2 Runtime Semantics: LoopEvaluation
    /// https://tc39.es/ecma262/#sec-runtime-semantics-loopevaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        switch (self) {
            // IterationStatement : DoWhileStatement
            .do_while_statement => |do_while_statement| {
                // 1. Return ? DoWhileLoopEvaluation of DoWhileStatement with argument labelSet.
                try do_while_statement.generateBytecode(executable, ctx);
            },
            // IterationStatement : WhileStatement
            .while_statement => |while_statement| {
                // 1. Return ? WhileLoopEvaluation of WhileStatement with argument labelSet.
                try while_statement.generateBytecode(executable, ctx);
            },
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'IterationStatement' here, it's implied and only adds nesting.
        switch (self) {
            .do_while_statement => |do_while_statement| try do_while_statement.print(writer, indentation),
            .while_statement => |while_statement| try while_statement.print(writer, indentation),
        }
    }
};

/// https://tc39.es/ecma262/#prod-DoWhileStatement
pub const DoWhileStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,

    /// 14.7.2.2 Runtime Semantics: DoWhileLoopEvaluation
    /// https://tc39.es/ecma262/#sec-runtime-semantics-dowhileloopevaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // DoWhileStatement : do Statement while ( Expression ) ;
        // 1. Let V be undefined.
        try executable.addInstructionWithConstant(.load_constant, .undefined);

        // 2. Repeat,
        const start_index = executable.instructions.items.len;

        // a. Let stmtResult be Completion(Evaluation of Statement).
        try executable.addInstruction(.store);
        try self.consequent_statement.generateBytecode(executable, ctx);
        try executable.addInstruction(.load);

        // TODO: b. If LoopContinues(stmtResult, labelSet) is false, return ? UpdateEmpty(stmtResult, V).

        // c. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
        // NOTE: This is done by the store/load sequence around each consequent execution.

        // d. Let exprRef be ? Evaluation of Expression.
        try self.test_expression.generateBytecode(executable, ctx);

        // e. Let exprValue be ? GetValue(exprRef).
        if (self.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // f. If ToBoolean(exprValue) is false, return V.
        try executable.addInstruction(.load);
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const end_jump = try executable.addJumpIndex();

        try consequent_jump.setTarget(start_index);

        try end_jump.setTargetHere();
        try executable.addInstruction(.store);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("DoWhileStatement", writer, indentation);
        try printString("consequent:", writer, indentation + 1);
        try self.consequent_statement.print(writer, indentation + 2);
        try printString("test:", writer, indentation + 1);
        try self.test_expression.print(writer, indentation + 2);
    }
};

/// https://tc39.es/ecma262/#prod-WhileStatement
pub const WhileStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,

    /// 14.7.3.2 Runtime Semantics: WhileLoopEvaluation
    /// https://tc39.es/ecma262/#sec-runtime-semantics-whileloopevaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // WhileStatement : while ( Expression ) Statement
        // 1. Let V be undefined.
        try executable.addInstructionWithConstant(.load_constant, .undefined);

        // 2. Repeat,
        const start_index = executable.instructions.items.len;

        // a. Let exprRef be ? Evaluation of Expression.
        try self.test_expression.generateBytecode(executable, ctx);

        // b. Let exprValue be ? GetValue(exprRef).
        if (self.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // c. If ToBoolean(exprValue) is false, return V.
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const end_jump = try executable.addJumpIndex();

        // d. Let stmtResult be Completion(Evaluation of Statement).
        try consequent_jump.setTargetHere();
        try executable.addInstruction(.store);
        try self.consequent_statement.generateBytecode(executable, ctx);
        try executable.addInstruction(.load);

        // TODO: e. If LoopContinues(stmtResult, labelSet) is false, return ? UpdateEmpty(stmtResult, V).

        try executable.addInstruction(.jump);
        const start_jump = try executable.addJumpIndex();
        try start_jump.setTarget(start_index);

        // f. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
        // NOTE: This is done by the store/load sequence around each consequent execution.

        try end_jump.setTargetHere();
        try executable.addInstruction(.store);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("WhileStatement", writer, indentation);
        try printString("test:", writer, indentation + 1);
        try self.test_expression.print(writer, indentation + 2);
        try printString("consequent:", writer, indentation + 1);
        try self.consequent_statement.print(writer, indentation + 2);
    }
};

/// https://tc39.es/ecma262/#prod-ThrowStatement
pub const ThrowStatement = struct {
    const Self = @This();

    expression: Expression,

    /// 14.14.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-throw-statement-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        // ThrowStatement : throw Expression ;
        // 1. Let exprRef be ? Evaluation of Expression.
        try self.expression.generateBytecode(executable, ctx);

        // 2. Let exprValue be ? GetValue(exprRef).
        if (self.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // 3. Return ThrowCompletion(exprValue).
        try executable.addInstruction(.throw);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("ThrowStatement", writer, indentation);
        try self.expression.print(writer, indentation + 1);
    }
};

pub const FormalParametersItem = union(enum) {
    formal_parameter: FormalParameter,
    // TODO: FunctionRestParameter
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameters = struct {
    const Self = @This();

    items: []const FormalParametersItem,

    /// 15.1.5 Static Semantics: ExpectedArgumentCount
    /// https://tc39.es/ecma262/#sec-static-semantics-expectedargumentcount
    pub fn expectedArgumentCount(self: Self) usize {
        return self.items.len;
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'FormalParameters' here, it's implied and only adds nesting.
        for (self.items) |item| {
            switch (item) {
                .formal_parameter => |formal_parameter| try formal_parameter.print(writer, indentation),
            }
        }
    }
};

/// https://tc39.es/ecma262/#prod-FormalParameter
pub const FormalParameter = struct {
    const Self = @This();

    binding_element: BindingElement,

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("FormalParameter", writer, indentation);
        try self.binding_element.print(writer, indentation + 1);
    }
};

/// https://tc39.es/ecma262/#prod-FunctionDeclaration
pub const FunctionDeclaration = struct {
    const Self = @This();

    identifier: Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,

    /// 15.2.2 Static Semantics: FunctionBodyContainsUseStrict
    /// https://tc39.es/ecma262/#sec-static-semantics-functionbodycontainsusestrict
    pub fn functionBodyContainsUseStrict(self: Self) bool {
        // 1. If the Directive Prologue of FunctionBody contains a Use Strict Directive, return
        //    true; otherwise, return false.
        return self.function_body.statement_list.containsDirective("use strict");
    }

    // 15.2.4 Runtime Semantics: InstantiateOrdinaryFunctionObject
    // https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionobject
    fn instantiateOrdinaryFunctionObject(
        self: Self,
        agent: *Agent,
        env: Environment,
        private_env: ?*PrivateEnvironment,
    ) !Object {
        const realm = agent.currentRealm();

        // 1. Let name be StringValue of BindingIdentifier.
        const name = self.identifier;

        // TODO: 2. Let sourceText be the source text matched by FunctionDeclaration.
        const source_text = "";

        // 3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            self.formal_parameters,
            self.function_body,
            .non_lexical_this,
            env,
            private_env,
            self.functionBodyContainsUseStrict(),
        );

        // 4. Perform SetFunctionName(F, name).
        try setFunctionName(function, PropertyKey.from(name), null);

        // TODO: 5. Perform MakeConstructor(F).

        // 6. Return F.
        return function;
    }

    /// 15.2.6 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-function-definitions-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, _: *Executable, ctx: *BytecodeContext) !void {
        // FIXME: This should be called in the various FooDeclarationInstantiation AOs instead.
        //        It's enough to get ECMAScript Functions Objects up and running however :^)
        const realm = ctx.agent.currentRealm();
        const env = Environment{ .global_environment = realm.global_env };
        const function = try self.instantiateOrdinaryFunctionObject(ctx.agent, env, null);
        realm.global_env.object_record.binding_object.set(
            PropertyKey.from(self.identifier),
            Value.from(function),
            .ignore,
        ) catch |err| try noexcept(err);

        // 1. Return empty.
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("FunctionDeclaration", writer, indentation);
        try printString("identifier:", writer, indentation + 1);
        try printString(self.identifier, writer, indentation + 2);
        try printString("formal_parameters:", writer, indentation + 1);
        try self.formal_parameters.print(writer, indentation + 2);
        try printString("function_body:", writer, indentation + 1);
        try self.function_body.print(writer, indentation + 2);
    }
};

/// https://tc39.es/ecma262/#prod-FunctionBody
pub const FunctionBody = struct {
    const Self = @This();

    statement_list: StatementList,

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        try self.statement_list.generateBytecode(executable, ctx);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'FunctionBody' here, it's implied and only adds nesting.
        try self.statement_list.print(writer, indentation);
    }
};

/// https://tc39.es/ecma262/#prod-Script
pub const Script = struct {
    const Self = @This();

    statement_list: StatementList,

    /// 16.1.2 Static Semantics: IsStrict
    /// https://tc39.es/ecma262/#sec-static-semantics-isstrict
    pub fn isStrict(self: Self) bool {
        // 1. If ScriptBody is present and the Directive Prologue of ScriptBody contains a Use
        //    Strict Directive, return true; otherwise, return false.
        return self.statement_list.containsDirective("use strict");
    }

    pub fn generateBytecode(self: Self, executable: *Executable, ctx: *BytecodeContext) !void {
        ctx.contained_in_strict_mode_code = self.isStrict();
        try self.statement_list.generateBytecode(executable, ctx);
    }

    pub fn print(self: Self, writer: anytype) !void {
        const indentation: usize = 0;
        try printString("Script", writer, indentation);
        try printString("strict:", writer, indentation + 1);
        try printString(if (self.isStrict()) "true" else "false", writer, indentation + 2);
        try self.statement_list.print(writer, indentation + 1);
    }
};
