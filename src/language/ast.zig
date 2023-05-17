const std = @import("std");

const bytecode = @import("bytecode.zig");
const types = @import("../types.zig");

const Executable = bytecode.Executable;
const Value = types.Value;

const BytecodeError = error{ OutOfMemory, BytecodeGenerationFailed };

fn printIndentation(writer: anytype, indentation: usize) !void {
    var i: usize = 0;
    while (i < indentation) : (i += 1)
        try writer.print("  ", .{});
}

fn printString(string: []const u8, writer: anytype, indentation: usize) !void {
    try printIndentation(writer, indentation);
    try writer.print("{s}\n", .{string});
}

/// https://tc39.es/ecma262/#prod-ParenthesizedExpression
pub const ParenthesizedExpression = struct {
    const Self = @This();

    expression: *Expression,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        try self.expression.generateBytecode(executable);
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
    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        // IdentifierReference : Identifier
        // IdentifierReference : yield
        // IdentifierReference : await
        // 1. Return ? ResolveBinding(StringValue of Identifier).
        try executable.addInstructionWithConstant(.resolve_binding, Value.from(self.identifier));
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

    pub fn generateBytecode(self: Self, executable: *Executable) BytecodeError!void {
        switch (self) {
            // PrimaryExpression : this
            .this => {
                // 1. Return ? ResolveThisBinding().
                try executable.addInstruction(.resolve_this_binding);
            },

            .identifier_reference => |identifier_reference| try identifier_reference.generateBytecode(
                executable,
            ),
            .literal => |literal| try literal.generateBytecode(executable),
            .parenthesized_expression => |parenthesized_expression| try parenthesized_expression.generateBytecode(
                executable,
            ),
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

/// https://tc39.es/ecma262/#prod-Literal
pub const Literal = union(enum) {
    const Self = @This();

    null,
    boolean: bool,
    numeric,
    string,

    /// 13.2.3.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable) !void {
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
            .numeric => {
                // 1. Return the NumericValue of NumericLiteral as defined in 12.9.3.
                unreachable;
            },

            // Literal : StringLiteral
            .string => {
                // 1. Return the SV of StringLiteral as defined in 12.9.4.2.
                unreachable;
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
            .numeric => unreachable,
            .string => unreachable,
        }
    }
};

/// https://tc39.es/ecma262/#prod-Expression
pub const Expression = union(enum) {
    const Self = @This();

    primary_expression: PrimaryExpression,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        switch (self) {
            .primary_expression => |primary_expression| try primary_expression.generateBytecode(
                executable,
            ),
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        try printString("Expression", writer, indentation);
        switch (self) {
            .primary_expression => |primary_expression| try primary_expression.print(
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
    debugger_statement,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        switch (self) {
            .block_statement => |block_statement| {
                try block_statement.generateBytecode(executable);
            },

            // EmptyStatement : ;
            .empty_statement => {
                // 1. Return empty.
            },

            .expression_statement => |expression_statement| {
                try expression_statement.generateBytecode(executable);
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

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
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
            .debugger_statement => try printString("debugger", writer, indentation + 1),
        }
    }
};

/// https://tc39.es/ecma262/#prod-Declaration
pub const Declaration = union(enum) {
    const Self = @This();

    dummy,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        _ = executable;
        _ = self;
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        _ = writer;
        _ = indentation;
        _ = self;
    }
};

/// https://tc39.es/ecma262/#prod-BlockStatement
pub const BlockStatement = struct {
    const Self = @This();

    block: Block,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        try self.block.generateBytecode(executable);
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
    pub fn generateBytecode(self: Self, executable: *Executable) BytecodeError!void {
        // StatementList : StatementList StatementListItem
        // 1. Let sl be ? Evaluation of StatementList.
        // 2. Let s be Completion(Evaluation of StatementListItem).
        // 3. Return ? UpdateEmpty(s, sl).
        for (self.statement_list) |statement_list_item| {
            try statement_list_item.generateBytecode(executable);
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) std.os.WriteError!void {
        try printString("Block", writer, indentation);
        for (self.statement_list) |statement_list_item| {
            try statement_list_item.print(writer, indentation + 1);
        }
    }
};

/// https://tc39.es/ecma262/#prod-StatementList
pub const StatementList = []const StatementListItem;

/// https://tc39.es/ecma262/#prod-StatementListItem
pub const StatementListItem = union(enum) {
    const Self = @This();

    statement: *Statement,
    declaration: *Declaration,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        switch (self) {
            .statement => |statement| try statement.generateBytecode(executable),
            .declaration => |declaration| try declaration.generateBytecode(executable),
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

/// https://tc39.es/ecma262/#prod-ExpressionStatement
pub const ExpressionStatement = struct {
    const Self = @This();

    expression: Expression,

    /// 14.5.1 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-expression-statement-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        // ExpressionStatement : Expression ;
        // 1. Let exprRef be ? Evaluation of Expression.
        // 2. Return ? GetValue(exprRef).
        try self.expression.generateBytecode(executable);
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'ExpressionStatement' here, it's implied and only adds nesting.
        try self.expression.print(writer, indentation);
    }
};

/// https://tc39.es/ecma262/#prod-Script
pub const Script = struct {
    const Self = @This();

    statement_list: StatementList,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        for (self.statement_list) |statement_list_item| {
            try statement_list_item.generateBytecode(executable);
        }
    }

    pub fn print(self: Self, writer: anytype) !void {
        const indentation: usize = 0;
        try printString("Script", writer, indentation);
        for (self.statement_list) |statement_list_item| {
            try statement_list_item.print(writer, indentation + 1);
        }
    }
};
