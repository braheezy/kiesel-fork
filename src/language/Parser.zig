const ptk = @import("ptk");
const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const literals = @import("literals.zig");
const tokenizer_ = @import("tokenizer.zig");

const Tokenizer = tokenizer_.Tokenizer;
const line_terminators = tokenizer_.line_terminators;
const containsLineTerminator = tokenizer_.containsLineTerminator;
const parseNumericLiteral = literals.parseNumericLiteral;
const parseStringLiteral = literals.parseStringLiteral;

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
    if (parser.diagnostics.hasErrors()) {
        return error.ParseError;
    } else if (parser.core.peek()) |maybe_next_token| {
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

/// 5.1.5.8 [no LineTerminator here]
/// https://tc39.es/ecma262/#sec-no-lineterminator-here
fn noLineTerminatorHere(self: *Self) !void {
    // Same as peek() but without immediately restoring the state; we need to look at what's
    // between the current and next token.
    const state = self.core.saveState();
    defer self.core.restoreState(state);
    if (try self.core.nextToken()) |next_token| {
        const start_offset = state.offset;
        const end_offset = self.core.tokenizer.offset - next_token.text.len;
        const whitespace_and_comments = self.core.tokenizer.source[start_offset..end_offset];

        if (containsLineTerminator(whitespace_and_comments)) {
            try self.diagnostics.emit(state.location, .@"error", "Unexpected newline", .{});
            return error.UnexpectedToken;
        }
    }
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
    if (maybe_next_token == null) return;

    const next_token = maybe_next_token.?;

    // Next token is '}', insert semicolon
    if (next_token.type == .@"}") return;

    const start_offset = state.offset;
    const end_offset = self.core.tokenizer.offset - next_token.text.len;
    const whitespace_and_comments = self.core.tokenizer.source[start_offset..end_offset];

    // Next token is separated by a newline, insert semicolon
    if (containsLineTerminator(whitespace_and_comments)) return;

    return error.UnexpectedToken;
}

fn acceptParenthesizedExpression(self: *Self) !ast.ParenthesizedExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"("));
    const expression = try self.allocator.create(ast.Expression);
    expression.* = try self.acceptExpression();
    _ = try self.core.accept(RuleSet.is(.@")"));
    return .{ .expression = expression };
}

fn acceptIdentifierReference(self: *Self) !ast.IdentifierReference {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.identifier));
    return .{ .identifier = token.text };
}

fn acceptPrimaryExpression(self: *Self) !ast.PrimaryExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.core.accept(RuleSet.is(.this))) |_|
        return .this
    else |_| if (self.acceptIdentifierReference()) |identifier_reference|
        return .{ .identifier_reference = identifier_reference }
    else |_| if (self.acceptLiteral()) |literal|
        return .{ .literal = literal }
    else |_| if (self.acceptParenthesizedExpression()) |parenthesized_expression|
        return .{ .parenthesized_expression = parenthesized_expression }
    else |_|
        return error.UnexpectedToken;
}

fn acceptLiteral(self: *Self) !ast.Literal {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{
        .null,
        .true,
        .false,
        .numeric,
        .string,
    }));
    switch (token.type) {
        .null => return .null,
        .true => return .{ .boolean = true },
        .false => return .{ .boolean = false },
        .numeric => return .{ .numeric = parseNumericLiteral(token.text, .complete) catch unreachable },
        .string => return .{ .string = parseStringLiteral(token.text, .complete) catch unreachable },
        else => unreachable,
    }
}

fn acceptExpression(self: *Self) ParserCore.AcceptError!ast.Expression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptPrimaryExpression()) |primary_expression|
        return .{ .primary_expression = primary_expression }
    else |_|
        return error.UnexpectedToken;
}

fn acceptStatement(self: *Self) (ParserCore.AcceptError || error{OutOfMemory})!*ast.Statement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const statement = try self.allocator.create(ast.Statement);

    if (self.acceptBlockStatement()) |block_statement|
        statement.* = .{ .block_statement = block_statement }
    else |_| if (self.core.accept(RuleSet.is(.semicolon))) |_|
        statement.* = .empty_statement
    else |_| if (self.acceptExpressionStatement()) |expression_statement|
        statement.* = .{ .expression_statement = expression_statement }
    else |_| if (self.acceptIfStatement()) |if_statement|
        statement.* = .{ .if_statement = if_statement }
    else |_| if (self.acceptBreakableStatement()) |breakable_statement|
        statement.* = .{ .breakable_statement = breakable_statement }
    else |_| if (self.acceptThrowStatement()) |throw_statement|
        statement.* = .{ .throw_statement = throw_statement }
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

fn acceptBreakableStatement(self: *Self) !ast.BreakableStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptIterationStatement()) |iteration_statement|
        return .{ .iteration_statement = iteration_statement }
    else |_|
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

fn acceptIfStatement(self: *Self) !ast.IfStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"if"));
    _ = try self.core.accept(RuleSet.is(.@"("));
    const test_expression = try self.acceptExpression();
    _ = try self.core.accept(RuleSet.is(.@")"));
    const consequent_statement = try self.acceptStatement();
    const alternate_statement = if (self.core.accept(RuleSet.is(.@"else"))) |_|
        try self.acceptStatement()
    else |_|
        null;
    return .{
        .test_expression = test_expression,
        .consequent_statement = consequent_statement,
        .alternate_statement = alternate_statement,
    };
}

fn acceptIterationStatement(self: *Self) !ast.IterationStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptDoWhileStatement()) |do_while_statement|
        return .{ .do_while_statement = do_while_statement }
    else |_| if (self.acceptWhileStatement()) |while_statement|
        return .{ .while_statement = while_statement }
    else |_|
        return error.UnexpectedToken;
}

fn acceptDoWhileStatement(self: *Self) !ast.DoWhileStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.do));
    const consequent_statement = try self.acceptStatement();
    _ = try self.core.accept(RuleSet.is(.@"while"));
    _ = try self.core.accept(RuleSet.is(.@"("));
    const test_expression = try self.acceptExpression();
    _ = try self.core.accept(RuleSet.is(.@")"));
    return .{
        .test_expression = test_expression,
        .consequent_statement = consequent_statement,
    };
}

fn acceptWhileStatement(self: *Self) !ast.WhileStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"while"));
    _ = try self.core.accept(RuleSet.is(.@"("));
    const test_expression = try self.acceptExpression();
    _ = try self.core.accept(RuleSet.is(.@")"));
    const consequent_statement = try self.acceptStatement();
    return .{
        .test_expression = test_expression,
        .consequent_statement = consequent_statement,
    };
}

fn acceptThrowStatement(self: *Self) !ast.ThrowStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.throw));
    try self.noLineTerminatorHere();
    const expression = try self.acceptExpression();
    return .{ .expression = expression };
}
