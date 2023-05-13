const std = @import("std");

fn printIndentation(writer: anytype, indentation: usize) !void {
    var i: usize = 0;
    while (i < indentation) : (i += 1)
        try writer.print("  ", .{});
}

fn printString(string: []const u8, writer: anytype, indentation: usize) !void {
    try printIndentation(writer, indentation);
    try writer.print("{s}\n", .{string});
}

/// https://tc39.es/ecma262/#prod-PrimaryExpression
pub const PrimaryExpression = union(enum) {
    const Self = @This();

    literal: Literal,

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'PrimaryExpression' here, it's implied and only adds nesting.
        switch (self) {
            .literal => |literal| try literal.print(writer, indentation),
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

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'BlockStatement' here, it's implied and only adds nesting.
        try self.block.print(writer, indentation);
    }
};

/// https://tc39.es/ecma262/#prod-Block
pub const Block = struct {
    const Self = @This();

    statement_list: StatementList,

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

    pub fn print(self: Self, writer: anytype, indentation: usize) !void {
        // Omit printing 'ExpressionStatement' here, it's implied and only adds nesting.
        try self.expression.print(writer, indentation);
    }
};

/// https://tc39.es/ecma262/#prod-Script
pub const Script = struct {
    const Self = @This();

    statement_list: ?StatementList,

    pub fn print(self: Self, writer: anytype) !void {
        const indentation: usize = 0;
        try printString("Script", writer, indentation);
        if (self.statement_list) |statement_list| {
            for (statement_list) |statement_list_item| {
                try statement_list_item.print(writer, indentation + 1);
            }
        }
    }
};
