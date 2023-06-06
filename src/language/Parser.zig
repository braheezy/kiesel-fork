const ptk = @import("ptk");
const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const literals = @import("literals.zig");
const tokenizer_ = @import("tokenizer.zig");
const utils = @import("../utils.zig");

const Tokenizer = tokenizer_.Tokenizer;
const containsLineTerminator = tokenizer_.containsLineTerminator;
const parseNumericLiteral = literals.parseNumericLiteral;
const parseStringLiteral = literals.parseStringLiteral;
const temporaryChange = utils.temporaryChange;

const Self = @This();

allocator: Allocator,
core: ParserCore,
diagnostics: *ptk.Diagnostics,
in_function_body: bool = false,

const RuleSet = ptk.RuleSet(Tokenizer.TokenType);
const ParserCore = ptk.ParserCore(Tokenizer, .{ .whitespace, .comment });

pub const Error = error{
    ParseError,
    OutOfMemory,
};

pub const ParseContext = struct {
    diagnostics: *ptk.Diagnostics,
    file_name: ?[]const u8 = null,
};

const AcceptContext = struct {
    precedence: Precedence = 0,
    associativity: ?Associativity = null,
};

const Precedence = u5;
const Associativity = enum {
    left_to_right,
    right_to_left,
};

const PrecedenceAndAssociativityAltFlag = enum {
    new_args,
    function_args,
    postfix_increment,
    postfix_decrement,
    unary_plus,
    unary_minus,
};

fn getPrecedenceAndAssociativity(token_type: Tokenizer.TokenType) struct { Precedence, ?Associativity } {
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table
    return switch (token_type) {
        .@"(" => .{ 18, null },
        .@".",
        .@"?.",
        => .{ 17, .left_to_right },
        .@"[" => .{ 17, null },
        .new => .{ 16, null },
        .@"!",
        .@"~",
        .typeof,
        .void,
        .delete,
        .@"await",
        => .{ 14, null },
        .@"**" => .{ 13, .right_to_left },
        .@"*",
        .@"/",
        .@"%",
        => .{ 12, .left_to_right },
        .@"+",
        .@"-",
        => .{ 11, .left_to_right },
        .@"<<",
        .@">>",
        .@">>>",
        => .{ 10, .left_to_right },
        .@"<",
        .@"<=",
        .@">",
        .@">=",
        .in,
        .instanceof,
        => .{ 9, .left_to_right },
        .@"==",
        .@"!=",
        .@"===",
        .@"!==",
        => .{ 8, .left_to_right },
        .@"&" => .{ 7, .left_to_right },
        .@"^" => .{ 6, .left_to_right },
        .@"|" => .{ 5, .left_to_right },
        .@"&&" => .{ 4, .left_to_right },
        .@"||",
        .@"??",
        => .{ 3, .left_to_right },
        .@"=",
        .@"+=",
        .@"-=",
        .@"**=",
        .@"*=",
        .@"/=",
        .@"%=",
        .@"<<=",
        .@">>=",
        .@">>>=",
        .@"&=",
        .@"^=",
        .@"|=",
        .@"&&=",
        .@"||=",
        .@"??=",
        => .{ 2, .right_to_left },
        .@"?" => .{ 2, .right_to_left },
        .@"=>" => .{ 2, .right_to_left },
        .yield,
        .@"yield*",
        .@"...",
        => .{ 2, null },
        .@"," => .{ 1, .left_to_right },
        else => .{ 0, null },
    };
}

fn getPrecedenceAndAssociativityAlt(flag: PrecedenceAndAssociativityAltFlag) struct { Precedence, ?Associativity } {
    return switch (flag) {
        .new_args, .function_args => .{ 17, null },
        .postfix_increment, .postfix_decrement => .{ 15, null },
        .unary_plus, .unary_minus => .{ 14, null },
    };
}

fn getPrecedence(token_type: Tokenizer.TokenType) Precedence {
    return getPrecedenceAndAssociativity(token_type)[0];
}

fn getPrecedenceAlt(flag: PrecedenceAndAssociativityAltFlag) Precedence {
    return getPrecedenceAndAssociativityAlt(flag)[0];
}

fn getAssociativity(token_type: Tokenizer.TokenType) ?Associativity {
    return getPrecedenceAndAssociativity(token_type)[1];
}

fn getAssociativityAlt(flag: PrecedenceAndAssociativityAltFlag) ?Associativity {
    return getPrecedenceAndAssociativityAlt(flag)[1];
}

pub fn parse(
    comptime T: type,
    allocator: Allocator,
    source_text: []const u8,
    ctx: ParseContext,
) Error!T {
    if (T != ast.Script)
        @compileError("Parser.parse() is only implemented for ast.Script");

    return ast.Script{
        .statement_list = try parseNode(
            ast.StatementList,
            "acceptStatementList",
            allocator,
            source_text,
            ctx,
        ),
    };
}

pub fn parseNode(
    comptime T: type,
    comptime accept_function_name: []const u8,
    allocator: Allocator,
    source_text: []const u8,
    ctx: ParseContext,
) Error!T {
    var tokenizer = Tokenizer.init(source_text, ctx.file_name);
    var core = ParserCore.init(&tokenizer);
    var parser = Self{
        .allocator = allocator,
        .core = core,
        .diagnostics = ctx.diagnostics,
    };
    const ast_node = @field(Self, accept_function_name)(&parser) catch |err| blk: {
        if (err == error.OutOfMemory) return error.OutOfMemory;
        break :blk null;
    };
    if (parser.diagnostics.hasErrors()) {
        return error.ParseError;
    } else if (parser.core.peek()) |maybe_next_token| {
        if (maybe_next_token) |next_token| {
            try parser.diagnostics.emit(
                tokenizer.current_location,
                .@"error",
                "Unexpected token '{s}'",
                .{@tagName(next_token.type)},
            );
            return error.ParseError;
        }
    } else |_| {
        const source = parser.core.tokenizer.source;
        const offset = parser.core.tokenizer.offset;
        std.debug.assert(offset < source.len);
        try parser.diagnostics.emit(
            tokenizer.current_location,
            .@"error",
            "Invalid character '{s}'",
            .{source[offset .. offset + 1]},
        );
        return error.ParseError;
    }
    return ast_node.?;
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
    if (self.core.accept(RuleSet.is(.@";"))) |_|
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
    expression.* = try self.acceptExpression(.{});
    _ = try self.core.accept(RuleSet.is(.@")"));
    return .{ .expression = expression };
}

fn acceptIdentifierReference(self: *Self) !ast.IdentifierReference {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .identifier, .yield, .@"await" }));
    return .{ .identifier = try self.allocator.dupe(u8, token.text) };
}

fn acceptBindingIdentifier(self: *Self) !ast.Identifier {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .identifier, .yield, .@"await" }));
    return self.allocator.dupe(u8, token.text);
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
    else |_| if (self.acceptArrayLiteral()) |array_literal|
        return .{ .array_literal = array_literal }
    else |_| if (self.acceptObjectLiteral()) |object_literal|
        return .{ .object_literal = object_literal }
    else |_| if (self.acceptFunctionExpression()) |function_expression|
        return .{ .function_expression = function_expression }
    else |_| if (self.acceptParenthesizedExpression()) |parenthesized_expression|
        return .{ .parenthesized_expression = parenthesized_expression }
    else |_|
        return error.UnexpectedToken;
}

fn acceptSecondaryExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) !ast.Expression {
    if (self.acceptMemberExpression(primary_expression)) |member_expression|
        return .{ .member_expression = member_expression }
    else |_| if (self.acceptCallExpression(primary_expression)) |call_expression|
        return .{ .call_expression = call_expression }
    else |_| if (self.acceptRelationalExpression(primary_expression, ctx)) |relational_expression|
        return .{ .relational_expression = relational_expression }
    else |_| if (self.acceptEqualityExpression(primary_expression, ctx)) |equality_expression|
        return .{ .equality_expression = equality_expression }
    else |_|
        return error.UnexpectedToken;
}

fn acceptMemberExpression(self: *Self, primary_expression: ast.Expression) !ast.MemberExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .@"[", .@"." }));
    const property: ast.MemberExpression.Property = switch (token.type) {
        .@"[" => blk: {
            const ctx = AcceptContext{ .precedence = getPrecedence(.@"[") };
            const property_expression = try self.allocator.create(ast.Expression);
            property_expression.* = try self.acceptExpression(ctx);
            _ = try self.core.accept(RuleSet.is(.@"]"));
            break :blk .{ .expression = property_expression };
        },
        .@"." => blk: {
            const identifier_token = try self.core.accept(RuleSet.is(.identifier));
            break :blk .{ .identifier = try self.allocator.dupe(u8, identifier_token.text) };
        },
        else => unreachable,
    };
    // Defer heap allocation of expression until we know this is a MemberExpression
    const expression = try self.allocator.create(ast.Expression);
    expression.* = primary_expression;
    return .{ .expression = expression, .property = property };
}

fn acceptCallExpression(self: *Self, primary_expression: ast.Expression) !ast.CallExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const arguments = try self.acceptArguments();
    // Defer heap allocation of expression until we know this is a CallExpression
    const expression = try self.allocator.create(ast.Expression);
    expression.* = primary_expression;
    return .{ .expression = expression, .arguments = arguments };
}

fn acceptArguments(self: *Self) !ast.Arguments {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var arguments = std.ArrayList(ast.Expression).init(self.allocator);
    _ = try self.core.accept(RuleSet.is(.@"("));
    const ctx = AcceptContext{ .precedence = getPrecedenceAlt(.function_args) };
    while (self.acceptExpression(ctx)) |argument| {
        try arguments.append(argument);
        _ = self.core.accept(RuleSet.is(.@",")) catch break;
    } else |_| {}
    _ = try self.core.accept(RuleSet.is(.@")"));
    return arguments.toOwnedSlice();
}

fn acceptLiteral(self: *Self) !ast.Literal {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptNumericLiteral()) |numeric_literal|
        return .{ .numeric = numeric_literal }
    else |_| if (self.acceptStringLiteral()) |string_literal|
        return .{ .string = string_literal }
    else |_| if (self.core.accept(RuleSet.is(.null))) |_|
        return .null
    else |_| if (self.core.accept(RuleSet.is(.true))) |_|
        return .{ .boolean = true }
    else |_| if (self.core.accept(RuleSet.is(.false))) |_|
        return .{ .boolean = false }
    else |_|
        return error.UnexpectedToken;
}

fn acceptNumericLiteral(self: *Self) !ast.NumericLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.numeric));
    var numeric_literal = parseNumericLiteral(token.text, .complete) catch unreachable;
    numeric_literal.text = try self.allocator.dupe(u8, numeric_literal.text);
    return numeric_literal;
}

fn acceptStringLiteral(self: *Self) !ast.StringLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.string));
    var string_literal = parseStringLiteral(token.text, .complete) catch unreachable;
    string_literal.text = try self.allocator.dupe(u8, string_literal.text);
    return string_literal;
}

fn acceptArrayLiteral(self: *Self) !ast.ArrayLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"["));
    var elements = std.ArrayList(ast.ArrayLiteral.Element).init(self.allocator);
    const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
    while (true) {
        if (self.acceptExpression(ctx)) |expression| {
            try elements.append(.{ .expression = expression });
            _ = self.core.accept(RuleSet.is(.@",")) catch break;
        } else |_| if (self.core.accept(RuleSet.is(.@","))) |_| {
            try elements.append(.elision);
        } else |_| break;
    }
    _ = try self.core.accept(RuleSet.is(.@"]"));
    return .{ .element_list = try elements.toOwnedSlice() };
}

fn acceptObjectLiteral(self: *Self) !ast.ObjectLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"{"));
    const property_definition_list = try self.acceptPropertyDefinitionList();
    _ = try self.core.accept(RuleSet.is(.@"}"));
    return .{ .property_definition_list = property_definition_list };
}

fn acceptPropertyDefinitionList(self: *Self) !ast.PropertyDefinitionList {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var property_definitions = std.ArrayList(ast.PropertyDefinition).init(self.allocator);
    while (true) {
        if (self.acceptPropertyDefinition()) |property_definition| {
            try property_definitions.append(property_definition);
            _ = self.core.accept(RuleSet.is(.@",")) catch break;
        } else |_| break;
    }
    return .{ .items = try property_definitions.toOwnedSlice() };
}

fn acceptPropertyDefinition(self: *Self) !ast.PropertyDefinition {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var property_name: ast.PropertyName = undefined;
    if (self.acceptIdentifierReference()) |identifier_reference| {
        const token = self.core.peek() catch null;
        // IdentifierReference
        if (token == null or token.?.type != .@":")
            return .{ .identifier_reference = identifier_reference };
        // LiteralPropertyName : IdentifierName
        property_name = .{
            .literal_property_name = .{ .identifier = identifier_reference.identifier },
        };
    } else |_| if (self.acceptStringLiteral()) |string_literal|
        // LiteralPropertyName : StringLiteral
        property_name = .{ .literal_property_name = .{ .string_literal = string_literal } }
    else |_| if (self.acceptNumericLiteral()) |numeric_literal|
        // LiteralPropertyName : NumericLiteral
        property_name = .{ .literal_property_name = .{ .numeric_literal = numeric_literal } }
    else |_| if (self.core.accept(RuleSet.is(.@"["))) |_| {
        // ComputedPropertyName
        const ctx = AcceptContext{ .precedence = getPrecedence(.@"[") };
        const computed_property_name = try self.acceptExpression(ctx);
        property_name = ast.PropertyName{
            .computed_property_name = computed_property_name,
        };
        _ = try self.core.accept(RuleSet.is(.@"]"));
    } else |_| return error.UnexpectedToken;

    _ = try self.core.accept(RuleSet.is(.@":"));
    const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
    const expression = try self.acceptExpression(ctx);
    return .{
        .property_name_and_expression = .{
            .property_name = property_name,
            .expression = expression,
        },
    };
}

fn acceptUnaryExpression(self: *Self, operator_token: Tokenizer.Token) !ast.UnaryExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const operator: ast.UnaryExpression.Operator = switch (operator_token.type) {
        .void => .void,
        .typeof => .typeof,
        .@"+" => .@"+",
        .@"-" => .@"-",
        .@"~" => .@"~",
        .@"!" => .@"!",
        else => unreachable,
    };
    const ctx: AcceptContext = switch (operator_token.type) {
        .@"+" => .{ .precedence = getPrecedenceAlt(.unary_plus), .associativity = getAssociativityAlt(.unary_plus) },
        .@"-" => .{ .precedence = getPrecedenceAlt(.unary_minus), .associativity = getAssociativityAlt(.unary_minus) },
        else => .{ .precedence = getPrecedence(operator_token.type), .associativity = getAssociativity(operator_token.type) },
    };
    const expression = try self.allocator.create(ast.Expression);
    expression.* = try self.acceptExpression(ctx);
    return .{ .operator = operator, .expression = expression };
}

fn acceptRelationalExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) !ast.RelationalExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .@">", .@"<", .@">=", .@"<=", .instanceof, .in }));
    const operator: ast.RelationalExpression.Operator = switch (token.type) {
        .@">" => .@">",
        .@"<" => .@"<",
        .@">=" => .@">=",
        .@"<=" => .@"<=",
        .instanceof => .instanceof,
        .in => .in,
        else => unreachable,
    };
    // Defer heap allocation of expression until we know this is a RelationalExpression
    const lhs_expression = try self.allocator.create(ast.Expression);
    lhs_expression.* = primary_expression;
    const rhs_expression = try self.allocator.create(ast.Expression);
    rhs_expression.* = try self.acceptExpression(ctx);
    return .{
        .operator = operator,
        .lhs_expression = lhs_expression,
        .rhs_expression = rhs_expression,
    };
}

fn acceptEqualityExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) !ast.EqualityExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .@"==", .@"!=", .@"===", .@"!==" }));
    const operator: ast.EqualityExpression.Operator = switch (token.type) {
        .@"==" => .@"==",
        .@"!=" => .@"!=",
        .@"===" => .@"===",
        .@"!==" => .@"!==",
        else => unreachable,
    };
    // Defer heap allocation of expression until we know this is an EqualityExpression
    const lhs_expression = try self.allocator.create(ast.Expression);
    lhs_expression.* = primary_expression;
    const rhs_expression = try self.allocator.create(ast.Expression);
    rhs_expression.* = try self.acceptExpression(ctx);
    return .{
        .operator = operator,
        .lhs_expression = lhs_expression,
        .rhs_expression = rhs_expression,
    };
}

fn acceptExpression(self: *Self, ctx: AcceptContext) (ParserCore.AcceptError || error{OutOfMemory})!ast.Expression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.core.accept(RuleSet.oneOf(.{ .void, .typeof, .@"+", .@"-", .@"~", .@"!" }))) |operator_token| {
        const unary_expression = try self.acceptUnaryExpression(operator_token);
        return .{ .unary_expression = unary_expression };
    } else |_| {}

    const primary_expression = try self.acceptPrimaryExpression();
    var expression = ast.Expression{ .primary_expression = primary_expression };
    while (true) {
        const next_token = try self.core.peek() orelse break;
        const new_ctx = AcceptContext{
            .precedence = getPrecedence(next_token.type),
            .associativity = getAssociativity(next_token.type),
        };
        if (new_ctx.precedence < ctx.precedence) break;
        if (new_ctx.precedence == ctx.precedence and
            ctx.associativity != null and
            ctx.associativity.? == .left_to_right) break;
        expression = self.acceptSecondaryExpression(expression, new_ctx) catch break;
    }
    return expression;
}

fn acceptStatement(self: *Self) (ParserCore.AcceptError || error{OutOfMemory})!*ast.Statement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const statement = try self.allocator.create(ast.Statement);

    if (self.acceptBlockStatement()) |block_statement|
        statement.* = .{ .block_statement = block_statement }
    else |_| if (self.core.accept(RuleSet.is(.@";"))) |_|
        statement.* = .empty_statement
    else |_| if (self.acceptExpressionStatement()) |expression_statement|
        statement.* = .{ .expression_statement = expression_statement }
    else |_| if (self.acceptIfStatement()) |if_statement|
        statement.* = .{ .if_statement = if_statement }
    else |_| if (self.acceptBreakableStatement()) |breakable_statement|
        statement.* = .{ .breakable_statement = breakable_statement }
    else |_| if (self.acceptReturnStatement()) |return_statement|
        statement.* = .{ .return_statement = return_statement }
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

    if (self.acceptHoistableDeclaration()) |hoistable_declaration|
        declaration.* = .{ .hoistable_declaration = hoistable_declaration }
    else |_|
        return error.UnexpectedToken;
    return declaration;
}

fn acceptHoistableDeclaration(self: *Self) !ast.HoistableDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptFunctionDeclaration()) |function_declaration|
        return .{ .function_declaration = function_declaration }
    else |_|
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
    var statement_list_items = std.ArrayList(ast.StatementListItem).init(self.allocator);
    while (self.acceptStatementListItem()) |statement_list_item|
        try statement_list_items.append(statement_list_item)
    else |_| {}
    return .{ .items = try statement_list_items.toOwnedSlice() };
}

fn acceptStatementListItem(self: *Self) !ast.StatementListItem {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const statement_list_item: ast.StatementListItem = blk: {
        // NOTE: Declarations need to be tried first since function declarations could also be expressions
        if (self.acceptDeclaration()) |declaration|
            break :blk .{ .declaration = declaration }
        else |_| if (self.acceptStatement()) |statement|
            break :blk .{ .statement = statement }
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

    return .{ .expression = try self.acceptExpression(.{}) };
}

fn acceptIfStatement(self: *Self) !ast.IfStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"if"));
    _ = try self.core.accept(RuleSet.is(.@"("));
    const test_expression = try self.acceptExpression(.{});
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
    const test_expression = try self.acceptExpression(.{});
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
    const test_expression = try self.acceptExpression(.{});
    _ = try self.core.accept(RuleSet.is(.@")"));
    const consequent_statement = try self.acceptStatement();
    return .{
        .test_expression = test_expression,
        .consequent_statement = consequent_statement,
    };
}

fn acceptReturnStatement(self: *Self) !ast.ReturnStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"return"));

    if (!self.in_function_body) {
        try self.diagnostics.emit(
            state.location,
            .@"error",
            "Return statement is only allowed in functions",
            .{},
        );
        return error.UnexpectedToken;
    }

    if (self.noLineTerminatorHere()) |_| {
        if (self.acceptExpression(.{})) |expression| {
            return .{ .expression = expression };
        } else |_| {}
    } else |_| {
        // Drop emitted 'unexpected newline' error
        _ = self.diagnostics.errors.pop();
    }
    return .{ .expression = null };
}

fn acceptThrowStatement(self: *Self) !ast.ThrowStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.throw));
    try self.noLineTerminatorHere();
    const expression = try self.acceptExpression(.{});
    return .{ .expression = expression };
}

fn acceptFormalParameters(self: *Self) !ast.FormalParameters {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var formal_parameters_items = std.ArrayList(ast.FormalParameters.Item).init(self.allocator);
    while (self.acceptBindingIdentifier()) |identifier| {
        const formal_parameter = ast.FormalParameter{
            .binding_element = .{ .identifier = identifier },
        };
        try formal_parameters_items.append(.{ .formal_parameter = formal_parameter });
        _ = self.core.accept(RuleSet.is(.@",")) catch break;
    } else |_| {}
    return .{ .items = try formal_parameters_items.toOwnedSlice() };
}

fn acceptFunctionDeclaration(self: *Self) !ast.FunctionDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.function));
    // We need to do this after consuming the 'function' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "function".len);
    const identifier = self.acceptBindingIdentifier() catch |err| {
        try self.diagnostics.emit(
            self.core.tokenizer.current_location,
            .@"error",
            "Function declaration must have a binding identifier",
            .{},
        );
        return err;
    };
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody();
    _ = try self.core.accept(RuleSet.is(.@"}"));
    const end_offset = self.core.tokenizer.offset;
    const source_text = try self.allocator.dupe(
        u8,
        self.core.tokenizer.source[start_offset..end_offset],
    );
    return .{
        .identifier = identifier,
        .formal_parameters = formal_parameters,
        .function_body = function_body,
        .source_text = source_text,
    };
}

fn acceptFunctionExpression(self: *Self) !ast.FunctionExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.function));
    // We need to do this after consuming the 'function' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "function".len);
    const identifier = self.acceptBindingIdentifier() catch null;
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody();
    _ = try self.core.accept(RuleSet.is(.@"}"));
    const end_offset = self.core.tokenizer.offset;
    const source_text = try self.allocator.dupe(
        u8,
        self.core.tokenizer.source[start_offset..end_offset],
    );
    return .{
        .identifier = identifier,
        .formal_parameters = formal_parameters,
        .function_body = function_body,
        .source_text = source_text,
    };
}

fn acceptFunctionBody(self: *Self) !ast.FunctionBody {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const tmp = temporaryChange(self, "in_function_body", true);
    defer tmp.restore();

    const statement_list = try self.acceptStatementList();
    return .{ .statement_list = statement_list };
}
