const ptk = @import("ptk");
const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const tokenizer_ = @import("tokenizer.zig");

const Tokenizer = tokenizer_.Tokenizer;
const line_terminators = tokenizer_.line_terminators;

const Self = @This();

pub const Error = error{
    ParseError,
    OutOfMemory,
};

pub const Context = struct {
    diagnostics: *ptk.Diagnostics,
    file_name: ?[]const u8 = null,
};

allocator: Allocator,
core: ParserCore,
diagnostics: *ptk.Diagnostics,

const RuleSet = ptk.RuleSet(Tokenizer.TokenType);
const ParserCore = ptk.ParserCore(Tokenizer, .{ .whitespace, .comment });

pub fn parse(
    comptime T: type,
    allocator: Allocator,
    source_text: []const u8,
    ctx: Context,
) Error!T {
    if (T != ast.Script)
        @compileError("Parser.parse() is only implemented for ast.Script");

    var tokenizer = Tokenizer.init(source_text, ctx.file_name);
    var core = ParserCore.init(&tokenizer);
    var parser = Self{
        .allocator = allocator,
        .core = core,
        .diagnostics = ctx.diagnostics,
    };

    const statement_list = try parser.acceptStatementList();
    if (parser.core.peek()) |maybe_next_token| {
        if (maybe_next_token) |next_token| {
            try parser.diagnostics.emit(
                tokenizer.current_location,
                .@"error",
                "Expected statement or declaration, got '{s}'",
                .{@tagName(next_token.type)},
            );
            return error.ParseError;
        }
    } else |_| {
        try parser.diagnostics.emit(
            tokenizer.current_location,
            .@"error",
            "Expected statement or declaration, got unexpected character",
            .{},
        );
        return error.ParseError;
    }
    return ast.Script{ .statement_list = statement_list };
}

/// 12.10 Automatic Semicolon Insertion
/// https://tc39.es/ecma262/#sec-automatic-semicolon-insertion
fn acceptOrInsertSemicolon(self: *Self) !void {
    // Next token is ';', consume semicolon
    if (self.core.accept(RuleSet.is(.semicolon))) |_|
        return
    else |_| {}

    // Same as peek() but without immediately restoring the state; we need to look at what's
    // between the current and next token.
    const state = self.core.saveState();
    defer self.core.restoreState(state);
    const maybe_next_token = try self.core.nextToken();

    // Next token is EOF, insert semicolon
    if (maybe_next_token == null)
        return;

    const next_token = maybe_next_token.?;

    // Next token is '}', insert semicolon
    if (next_token.type == .@"}")
        return;

    const start_offset = state.offset;
    const end_offset = self.core.tokenizer.offset - next_token.text.len;
    const whitespace_and_comments = self.core.tokenizer.source[start_offset..end_offset];

    // Next token is separated by a newline, insert semicolon
    for (line_terminators) |needle| {
        if (std.mem.indexOf(u8, whitespace_and_comments, needle)) |_|
            return;
    }

    return error.UnexpectedToken;
}

fn acceptPrimaryExpression(self: *Self) !ast.PrimaryExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptLiteral()) |literal|
        return .{ .literal = literal }
    else |_|
        return error.UnexpectedToken;
}

fn acceptLiteral(self: *Self) !ast.Literal {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{
        .true,
        .false,
        .null,
    }));
    switch (token.type) {
        .true => return .{ .boolean = true },
        .false => return .{ .boolean = false },
        .null => return .null,
        else => unreachable,
    }
}

fn acceptExpression(self: *Self) !ast.Expression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptPrimaryExpression()) |primary_expression|
        return .{ .primary_expression = primary_expression }
    else |_|
        return error.UnexpectedToken;
}

fn acceptStatement(self: *Self) !*ast.Statement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const statement = try self.allocator.create(ast.Statement);

    if (self.acceptBlockStatement()) |block_statement|
        statement.* = .{ .block_statement = block_statement }
    else |_| if (self.core.accept(RuleSet.is(.semicolon))) |_|
        statement.* = .empty_statement
    else |_| if (self.acceptExpressionStatement()) |expression_statement|
        statement.* = .{ .expression_statement = expression_statement }
    else |_| if (self.core.accept(RuleSet.is(.debugger))) |_|
        statement.* = .debugger_statement
    else |_|
        return error.UnexpectedToken;
    return statement;
}

fn acceptDeclaration(self: *Self) !*ast.Declaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const declaration = try self.allocator.create(ast.Declaration);
    _ = declaration;

    return error.UnexpectedToken;
}

fn acceptBlockStatement(self: *Self) !ast.BlockStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    return .{ .block = try self.acceptBlock() };
}

fn acceptBlock(self: *Self) !ast.Block {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"{"));
    const block = .{ .statement_list = try self.acceptStatementList() };
    _ = try self.core.accept(RuleSet.is(.@"}"));
    return block;
}

fn acceptStatementList(self: *Self) error{OutOfMemory}!ast.StatementList {
    var statement_list = std.ArrayList(ast.StatementListItem).init(self.allocator);
    while (self.acceptStatementListItem()) |statement_list_item|
        try statement_list.append(statement_list_item)
    else |_| {}
    return statement_list.toOwnedSlice();
}

fn acceptStatementListItem(self: *Self) !ast.StatementListItem {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const statement_list_item: ast.StatementListItem = blk: {
        if (self.acceptStatement()) |statement|
            break :blk .{ .statement = statement }
        else |_| if (self.acceptDeclaration()) |declaration|
            break :blk .{ .declaration = declaration }
        else |_|
            return error.UnexpectedToken;
    };
    // ASI only applies if the parsed item was not an empty statement
    if (statement_list_item != .statement or statement_list_item.statement.* != .empty_statement)
        try self.acceptOrInsertSemicolon();
    return statement_list_item;
}

fn acceptExpressionStatement(self: *Self) !ast.ExpressionStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    return .{ .expression = try self.acceptExpression() };
}
