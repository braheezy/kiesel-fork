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
    if_statement: IfStatement,
    breakable_statement: BreakableStatement,
    debugger_statement,

    pub fn generateBytecode(self: Self, executable: *Executable) BytecodeError!void {
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
            .if_statement => |if_statement| try if_statement.generateBytecode(executable),
            .breakable_statement => |breakable_statement| {
                try breakable_statement.generateBytecode(executable);
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

/// https://tc39.es/ecma262/#prod-BreakableStatement
pub const BreakableStatement = union(enum) {
    const Self = @This();

    iteration_statement: IterationStatement,

    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        switch (self) {
            .iteration_statement => |iteration_statement| try iteration_statement.generateBytecode(
                executable,
            ),
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

/// https://tc39.es/ecma262/#prod-IfStatement
pub const IfStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,
    alternate_statement: ?*Statement,

    /// 14.6.2 Runtime Semantics: Evaluation
    /// https://tc39.es/ecma262/#sec-if-statement-runtime-semantics-evaluation
    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        // 1. Let exprRef be ? Evaluation of Expression.
        try self.test_expression.generateBytecode(executable);

        // 2. Let exprValue be ToBoolean(? GetValue(exprRef)).
        // 3. If exprValue is true, then
        try executable.addInstruction(.load);
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const alternate_jump = try executable.addJumpIndex();

        // a. Let stmtCompletion be Completion(Evaluation of the first Statement).
        try consequent_jump.setTargetHere();
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try self.consequent_statement.generateBytecode(executable);

        if (self.alternate_statement) |alternate_statement| {
            try executable.addInstruction(.jump);
            const end_jump = try executable.addJumpIndex();

            // 4. Else,
            // a. Let stmtCompletion be Completion(Evaluation of the second Statement).
            try alternate_jump.setTargetHere();
            try executable.addInstructionWithConstant(.store_constant, .undefined);
            try alternate_statement.generateBytecode(executable);

            try end_jump.setTargetHere();
        } else {
            try alternate_jump.setTargetHere();
            try executable.addInstructionWithConstant(.store_constant, .undefined);
        }

        // 5. Return ? UpdateEmpty(stmtCompletion, undefined).
        // NOTE: This is handled by the store_constant before the consequent/alternate statements.
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

    while_statement: WhileStatement,

    /// 14.7.1.2 Runtime Semantics: LoopEvaluation
    /// https://tc39.es/ecma262/#sec-runtime-semantics-loopevaluation
    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        switch (self) {
            // IterationStatement : WhileStatement
            .while_statement => |while_statement| {
                // 1. Return ? WhileLoopEvaluation of WhileStatement with argument labelSet.
                try while_statement.generateBytecode(executable);
            },
        }
    }

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'IterationStatement' here, it's implied and only adds nesting.
        switch (self) {
            .while_statement => |while_statement| try while_statement.print(writer, indentation),
        }
    }
};

/// https://tc39.es/ecma262/#prod-WhileStatement
pub const WhileStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,

    /// 14.7.3.2 Runtime Semantics: WhileLoopEvaluation
    /// https://tc39.es/ecma262/#sec-runtime-semantics-whileloopevaluation
    pub fn generateBytecode(self: Self, executable: *Executable) !void {
        // WhileStatement : while ( Expression ) Statement
        // 1. Let V be undefined.
        try executable.addInstructionWithConstant(.load_constant, .undefined);

        // 2. Repeat,
        const start_index = executable.instructions.items.len;

        // a. Let exprRef be ? Evaluation of Expression.
        // b. Let exprValue be ? GetValue(exprRef).
        try self.test_expression.generateBytecode(executable);

        // c. If ToBoolean(exprValue) is false, return V.
        try executable.addInstruction(.load);
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const end_jump = try executable.addJumpIndex();

        // d. Let stmtResult be Completion(Evaluation of Statement).
        try consequent_jump.setTargetHere();
        try executable.addInstruction(.store);
        try self.consequent_statement.generateBytecode(executable);
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
