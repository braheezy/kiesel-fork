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
const parseRegularExpressionLiteral = literals.parseRegularExpressionLiteral;
const parseStringLiteral = literals.parseStringLiteral;
const temporaryChange = utils.temporaryChange;
const reserved_words = tokenizer_.reserved_words;

const Self = @This();

allocator: Allocator,
core: ParserCore,
diagnostics: *ptk.Diagnostics,
state: struct {
    in_function_body: bool = false,
    call_expression_forbidden: bool = false,
} = .{},

const RuleSet = ptk.RuleSet(Tokenizer.TokenType);
const ParserCore = ptk.ParserCore(Tokenizer, .{ .whitespace, .comment });

pub const AcceptError = error{OutOfMemory} || ParserCore.AcceptError;

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
    prefix_increment,
    prefix_decrement,
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
        .@"++",
        .@"--",
        => .{ 15, null },
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
        .@"...",
        => .{ 2, null },
        .@"," => .{ 1, .left_to_right },
        else => .{ 0, null },
    };
}

fn getPrecedenceAndAssociativityAlt(flag: PrecedenceAndAssociativityAltFlag) struct { Precedence, ?Associativity } {
    return switch (flag) {
        .prefix_increment,
        .prefix_decrement,
        .unary_plus,
        .unary_minus,
        => .{ 14, null },
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

    return parseNode(
        ast.Script,
        struct {
            fn accept(parser: *Self) AcceptError!ast.Script {
                return parser.acceptScript();
            }
        }.accept,
        allocator,
        source_text,
        ctx,
    );
}

pub fn parseNode(
    comptime T: type,
    comptime acceptFn: fn (*Self) anyerror!T,
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
    tokenizer_.state.tokenizer = &tokenizer;
    const ast_node: ?T = acceptFn(&parser) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => null,
    };
    if (parser.diagnostics.hasErrors()) {
        return error.ParseError;
    } else if (parser.core.peek()) |maybe_next_token| {
        if (maybe_next_token) |next_token| {
            try parser.emitError("Unexpected token '{s}'", .{@tagName(next_token.type)});
            return error.ParseError;
        }
    } else |_| {
        // If peek() returned UnexpectedCharacter, the tokenizer state is restored to the last
        // 'good' position, which means we also have to re-consume any whitespace and other ignored
        // tokens preceding the invalid character.
        while (parser.core.tokenizer.next() catch null) |_| {}
        const source = parser.core.tokenizer.source;
        const offset = parser.core.tokenizer.offset;
        std.debug.assert(offset < source.len);
        try parser.emitError("Invalid character '{s}'", .{source[offset .. offset + 1]});
        return error.ParseError;
    }
    return ast_node.?;
}

fn emitError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    try self.diagnostics.emit(self.core.tokenizer.current_location, .@"error", fmt, args);
}

fn emitErrorAt(self: *Self, location: ptk.Location, comptime fmt: []const u8, args: anytype) !void {
    try self.diagnostics.emit(location, .@"error", fmt, args);
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
            try self.emitErrorAt(state.location, "Unexpected newline", .{});
            return error.UnexpectedToken;
        }
    }
}

/// 12.10 Automatic Semicolon Insertion
/// https://tc39.es/ecma262/#sec-automatic-semicolon-insertion
pub fn acceptOrInsertSemicolon(self: *Self) !void {
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

pub fn acceptParenthesizedExpression(self: *Self) AcceptError!ast.ParenthesizedExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"("));
    const expression = try self.allocator.create(ast.Expression);
    expression.* = try self.acceptExpression(.{});
    _ = try self.core.accept(RuleSet.is(.@")"));
    return .{ .expression = expression };
}

pub fn acceptIdentifierName(self: *Self) AcceptError!ast.Identifier {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const reserved_word_token_types = comptime blk: {
        var reserved_word_token_types: [reserved_words.len]Tokenizer.TokenType = undefined;
        for (reserved_words, 0..) |reserved_word, i| {
            reserved_word_token_types[i] = @field(Tokenizer.TokenType, reserved_word);
        }
        break :blk reserved_word_token_types;
    };
    const token = try self.core.accept(RuleSet.oneOf(.{.identifier} ++ reserved_word_token_types));
    return self.allocator.dupe(u8, token.text);
}

pub fn acceptIdentifierReference(self: *Self) AcceptError!ast.IdentifierReference {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .identifier, .yield, .@"await" }));
    if (self.core.peek() catch null) |next_token| {
        if (next_token.type == .@"=>") return error.UnexpectedToken;
        if (std.mem.eql(u8, token.text, "async")) {
            if (self.noLineTerminatorHere()) {
                // AsyncFunction{Expression,Declaration}
                if (next_token.type == .function) return error.UnexpectedToken;
                // AsyncArrowFunction
                if (next_token.type == .@"(" or next_token.type == .identifier) return error.UnexpectedToken;
            } else |_| {}
        }
    }
    return .{ .identifier = try self.allocator.dupe(u8, token.text) };
}

pub fn acceptBindingIdentifier(self: *Self) AcceptError!ast.Identifier {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .identifier, .yield, .@"await" }));
    return self.allocator.dupe(u8, token.text);
}

pub fn acceptPrimaryExpression(self: *Self) AcceptError!ast.PrimaryExpression {
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
    else |_| if (self.acceptGeneratorExpression()) |generator_expression|
        return .{ .generator_expression = generator_expression }
    else |_| if (self.acceptAsyncFunctionExpression()) |async_function_expression|
        return .{ .async_function_expression = async_function_expression }
    else |_| if (self.acceptAsyncGeneratorExpression()) |async_generator_expression|
        return .{ .async_generator_expression = async_generator_expression }
    else |_| if (self.acceptRegularExpressionLiteral()) |regular_expression_literal|
        return .{ .regular_expression_literal = regular_expression_literal }
    else |_| if (self.acceptArrowFunction()) |arrow_function|
        return .{ .arrow_function = arrow_function }
    else |_| if (self.acceptAsyncArrowFunction()) |async_arrow_function|
        return .{ .async_arrow_function = async_arrow_function }
    else |_| if (self.acceptParenthesizedExpression()) |parenthesized_expression|
        return .{ .parenthesized_expression = parenthesized_expression }
    else |_|
        return error.UnexpectedToken;
}

pub fn acceptSecondaryExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.Expression {
    if (self.acceptMemberExpression(primary_expression)) |member_expression|
        return .{ .member_expression = member_expression }
    else |_| if (self.acceptCallExpression(primary_expression)) |call_expression|
        return .{ .call_expression = call_expression }
    else |_| if (self.acceptUpdateExpression(primary_expression)) |update_expression|
        return .{ .update_expression = update_expression }
    else |_| if (self.acceptBinaryExpression(primary_expression, ctx)) |binary_expression|
        return .{ .binary_expression = binary_expression }
    else |_| if (self.acceptRelationalExpression(primary_expression, ctx)) |relational_expression|
        return .{ .relational_expression = relational_expression }
    else |_| if (self.acceptEqualityExpression(primary_expression, ctx)) |equality_expression|
        return .{ .equality_expression = equality_expression }
    else |_| if (self.acceptLogicalExpression(primary_expression, ctx)) |logical_expression|
        return .{ .logical_expression = logical_expression }
    else |_| if (self.acceptConditionalExpression(primary_expression, ctx)) |conditional_expression|
        return .{ .conditional_expression = conditional_expression }
    else |_| if (self.acceptAssignmentExpression(primary_expression, ctx)) |assignment_expression|
        return .{ .assignment_expression = assignment_expression }
    else |_| if (self.acceptSequenceExpression(primary_expression)) |sequence_expression|
        return .{ .sequence_expression = sequence_expression }
    else |_|
        return error.UnexpectedToken;
}

pub fn acceptMemberExpression(self: *Self, primary_expression: ast.Expression) AcceptError!ast.MemberExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .@"[", .@"." }));
    const property: ast.MemberExpression.Property = switch (token.type) {
        .@"[" => blk: {
            const property_expression = try self.allocator.create(ast.Expression);
            property_expression.* = try self.acceptExpression(.{});
            _ = try self.core.accept(RuleSet.is(.@"]"));
            break :blk .{ .expression = property_expression };
        },
        .@"." => .{ .identifier = try self.acceptIdentifierName() },
        else => unreachable,
    };
    // Defer heap allocation of expression until we know this is a MemberExpression
    const expression = try self.allocator.create(ast.Expression);
    expression.* = primary_expression;
    return .{ .expression = expression, .property = property };
}

pub fn acceptMetaProperty(self: *Self) AcceptError!ast.MetaProperty {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptNewTarget())
        return .new_target
    else |_|
        return error.UnexpectedToken;
}

pub fn acceptNewTarget(self: *Self) AcceptError!void {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.new));
    _ = try self.core.accept(RuleSet.is(.@"."));
    const identifier = try self.core.accept(RuleSet.is(.identifier));
    if (!std.mem.eql(u8, identifier.text, "target")) return error.UnexpectedToken;

    if (!self.state.in_function_body) {
        try self.emitErrorAt(token.location, "new.target is only allowed in functions", .{});
        return error.UnexpectedToken;
    }
}

pub fn acceptNewExpression(self: *Self) AcceptError!ast.NewExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const tmp = temporaryChange(&self.state, "call_expression_forbidden", true);
    defer tmp.restore();

    _ = try self.core.accept(RuleSet.is(.new));
    const ctx = AcceptContext{ .precedence = getPrecedence(.new) };
    const expression = try self.allocator.create(ast.Expression);
    expression.* = try self.acceptExpression(ctx);
    tmp.restore();
    const arguments = self.acceptArguments() catch &[_]ast.Expression{};
    return .{ .expression = expression, .arguments = arguments };
}

pub fn acceptCallExpression(self: *Self, primary_expression: ast.Expression) AcceptError!ast.CallExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.state.call_expression_forbidden) return error.UnexpectedToken;

    const arguments = try self.acceptArguments();
    // Defer heap allocation of expression until we know this is a CallExpression
    const expression = try self.allocator.create(ast.Expression);
    expression.* = primary_expression;
    return .{ .expression = expression, .arguments = arguments };
}

pub fn acceptArguments(self: *Self) AcceptError!ast.Arguments {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var arguments = std.ArrayList(ast.Expression).init(self.allocator);
    defer arguments.deinit();
    _ = try self.core.accept(RuleSet.is(.@"("));
    const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
    while (self.acceptExpression(ctx)) |argument| {
        try arguments.append(argument);
        _ = self.core.accept(RuleSet.is(.@",")) catch break;
    } else |_| {}
    _ = try self.core.accept(RuleSet.is(.@")"));
    return arguments.toOwnedSlice();
}

pub fn acceptUpdateExpression(self: *Self, primary_expression: ?ast.Expression) AcceptError!ast.UpdateExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const operator_token = try self.core.accept(RuleSet.oneOf(.{ .@"++", .@"--" }));
    const operator: ast.UpdateExpression.Operator = switch (operator_token.type) {
        .@"++" => .@"++",
        .@"--" => .@"--",
        else => unreachable,
    };

    var @"type": ast.UpdateExpression.Type = undefined;

    // Defer heap allocation of expression until we know this is an UpdateExpression
    const expression = try self.allocator.create(ast.Expression);
    if (primary_expression == null) {
        const ctx: AcceptContext = switch (operator_token.type) {
            .@"++" => .{ .precedence = getPrecedenceAlt(.prefix_increment), .associativity = getAssociativityAlt(.prefix_increment) },
            .@"--" => .{ .precedence = getPrecedenceAlt(.prefix_decrement), .associativity = getAssociativityAlt(.prefix_decrement) },
            else => unreachable,
        };
        expression.* = try self.acceptExpression(ctx);
        @"type" = .prefix;
    } else {
        expression.* = primary_expression.?;
        @"type" = .postfix;
    }

    // It is an early Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
    if (@"type" == .prefix and expression.assignmentTargetType() != .simple) {
        try self.emitErrorAt(state.location, "Invalid left-hand side in update expression", .{});
        return error.UnexpectedToken;
    }

    // It is an early Syntax Error if AssignmentTargetType of UnaryExpression is not simple.
    if (@"type" == .postfix and expression.assignmentTargetType() != .simple) {
        try self.emitErrorAt(state.location, "Invalid right-hand side in update expression", .{});
        return error.UnexpectedToken;
    }

    return .{
        .type = @"type",
        .operator = operator,
        .expression = expression,
    };
}

pub fn acceptLiteral(self: *Self) AcceptError!ast.Literal {
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

pub fn acceptNumericLiteral(self: *Self) AcceptError!ast.NumericLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.numeric));
    var numeric_literal = parseNumericLiteral(token.text, .complete) catch unreachable;
    numeric_literal.text = try self.allocator.dupe(u8, numeric_literal.text);
    return numeric_literal;
}

pub fn acceptStringLiteral(self: *Self) AcceptError!ast.StringLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.string));
    var string_literal = parseStringLiteral(token.text, .complete) catch unreachable;
    string_literal.text = try self.allocator.dupe(u8, string_literal.text);
    return string_literal;
}

pub fn acceptArrayLiteral(self: *Self) AcceptError!ast.ArrayLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"["));
    var elements = std.ArrayList(ast.ArrayLiteral.Element).init(self.allocator);
    defer elements.deinit();
    const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
    while (true) {
        if (self.core.accept(RuleSet.is(.@"..."))) |_| {
            const expression = try self.acceptExpression(ctx);
            try elements.append(.{ .spread = expression });
            _ = self.core.accept(RuleSet.is(.@",")) catch break;
        } else |_| if (self.acceptExpression(ctx)) |expression| {
            try elements.append(.{ .expression = expression });
            _ = self.core.accept(RuleSet.is(.@",")) catch break;
        } else |_| if (self.core.accept(RuleSet.is(.@","))) |_| {
            try elements.append(.elision);
        } else |_| break;
    }
    _ = try self.core.accept(RuleSet.is(.@"]"));
    return .{ .element_list = try elements.toOwnedSlice() };
}

pub fn acceptObjectLiteral(self: *Self) AcceptError!ast.ObjectLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"{"));
    const property_definition_list = try self.acceptPropertyDefinitionList();
    _ = try self.core.accept(RuleSet.is(.@"}"));
    return .{ .property_definition_list = property_definition_list };
}

pub fn acceptPropertyDefinitionList(self: *Self) AcceptError!ast.PropertyDefinitionList {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var property_definitions = std.ArrayList(ast.PropertyDefinition).init(self.allocator);
    defer property_definitions.deinit();
    while (true) {
        if (self.acceptPropertyDefinition(null)) |property_definition| {
            try property_definitions.append(property_definition);
            _ = self.core.accept(RuleSet.is(.@",")) catch break;
        } else |_| break;
    }
    return .{ .items = try property_definitions.toOwnedSlice() };
}

pub fn acceptPropertyDefinition(
    self: *Self,
    method_type: ?ast.MethodDefinition.Type,
) AcceptError!ast.PropertyDefinition {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
    var property_name: ast.PropertyName = undefined;
    if (self.core.accept(RuleSet.is(.@"..."))) |_| {
        const expression = try self.acceptExpression(ctx);
        return .{ .spread = expression };
    } else |_| if (self.acceptIdentifierReference()) |identifier_reference| {
        if (method_type == null) {
            const get = std.mem.eql(u8, identifier_reference.identifier, "get");
            const set = std.mem.eql(u8, identifier_reference.identifier, "set");
            if (get or set) {
                if (acceptPropertyDefinition(self, if (get) .get else .set)) |property_definition|
                    return property_definition
                else |_| {}
            }

            var followed_by_valid_punctuation = false;
            var followed_by_identifier_reference = false;
            if (try self.core.peek()) |next_token| {
                followed_by_valid_punctuation = switch (next_token.type) {
                    .@":", .@"(" => true,
                    else => false,
                };
                followed_by_identifier_reference = switch (next_token.type) {
                    .identifier, .yield, .@"await" => true,
                    else => false,
                };
            }

            // IdentifierReference
            if (!followed_by_valid_punctuation and !followed_by_identifier_reference) {
                return .{ .identifier_reference = identifier_reference };
            }
        }

        // LiteralPropertyName : IdentifierName
        // MethodDefinition
        const identifier = identifier_reference.identifier;
        property_name = .{ .literal_property_name = .{ .identifier = identifier } };
    } else |_| if (self.acceptIdentifierName()) |identifier|
        // LiteralPropertyName : IdentifierName
        property_name = .{ .literal_property_name = .{ .identifier = identifier } }
    else |_| if (self.acceptStringLiteral()) |string_literal|
        // LiteralPropertyName : StringLiteral
        property_name = .{ .literal_property_name = .{ .string_literal = string_literal } }
    else |_| if (self.acceptNumericLiteral()) |numeric_literal|
        // LiteralPropertyName : NumericLiteral
        property_name = .{ .literal_property_name = .{ .numeric_literal = numeric_literal } }
    else |_| if (self.core.accept(RuleSet.is(.@"["))) |_| {
        // ComputedPropertyName
        const computed_property_name = try self.acceptExpression(.{});
        property_name = .{ .computed_property_name = computed_property_name };
        _ = try self.core.accept(RuleSet.is(.@"]"));
    } else |_| return error.UnexpectedToken;

    if (self.core.accept(RuleSet.is(.@":"))) |_| {
        const expression = try self.acceptExpression(ctx);
        return .{
            .property_name_and_expression = .{
                .property_name = property_name,
                .expression = expression,
            },
        };
    } else |_| if (self.core.accept(RuleSet.is(.@"("))) |_| {
        // We need to do this after consuming the '(' token to skip preceeding whitespace.
        const start_offset = self.core.tokenizer.offset - (comptime "(".len);
        const formal_parameters = try self.acceptFormalParameters();
        _ = try self.core.accept(RuleSet.is(.@")"));
        _ = try self.core.accept(RuleSet.is(.@"{"));
        const function_body = try self.acceptFunctionBody(.normal);
        _ = try self.core.accept(RuleSet.is(.@"}"));
        const end_offset = self.core.tokenizer.offset;
        const source_text = try self.allocator.dupe(
            u8,
            self.core.tokenizer.source[start_offset..end_offset],
        );
        const function_expression = ast.FunctionExpression{
            .identifier = null,
            .formal_parameters = formal_parameters,
            .function_body = function_body,
            .source_text = source_text,
        };
        return .{
            .method_definition = .{
                .property_name = property_name,
                .function_expression = function_expression,
                .type = method_type orelse .method,
            },
        };
    } else |_| return error.UnexpectedToken;
}

pub fn acceptUnaryExpression(self: *Self) AcceptError!ast.UnaryExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const operator_token = try self.core.accept(RuleSet.oneOf(.{
        .delete,
        .void,
        .typeof,
        .@"+",
        .@"-",
        .@"~",
        .@"!",
    }));
    const operator: ast.UnaryExpression.Operator = switch (operator_token.type) {
        .delete => .delete,
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

pub fn acceptBinaryExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.BinaryExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{
        .@"**",
        .@"*",
        .@"/",
        .@"%",
        .@"+",
        .@"-",
        .@"<<",
        .@">>",
        .@">>>",
        .@"&",
        .@"^",
        .@"|",
    }));
    const operator: ast.BinaryExpression.Operator = switch (token.type) {
        .@"**" => .@"**",
        .@"*" => .@"*",
        .@"/" => .@"/",
        .@"%" => .@"%",
        .@"+" => .@"+",
        .@"-" => .@"-",
        .@"<<" => .@"<<",
        .@">>" => .@">>",
        .@">>>" => .@">>>",
        .@"&" => .@"&",
        .@"^" => .@"^",
        .@"|" => .@"|",
        else => unreachable,
    };
    // Defer heap allocation of expression until we know this is a BinaryExpression
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

pub fn acceptRelationalExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.RelationalExpression {
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

pub fn acceptEqualityExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.EqualityExpression {
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

pub fn acceptLogicalExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.LogicalExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{ .@"&&", .@"||", .@"??" }));
    const operator: ast.LogicalExpression.Operator = switch (token.type) {
        .@"&&" => .@"&&",
        .@"||" => .@"||",
        .@"??" => .@"??",
        else => unreachable,
    };
    // Defer heap allocation of expression until we know this is a LogicalExpression
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

pub fn acceptConditionalExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.ConditionalExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"?"));
    // Defer heap allocation of expression until we know this is a ConditionalExpression
    const test_expression = try self.allocator.create(ast.Expression);
    test_expression.* = primary_expression;
    const consequent_expression = try self.allocator.create(ast.Expression);
    consequent_expression.* = try self.acceptExpression(ctx);
    _ = try self.core.accept(RuleSet.is(.@":"));
    const alternate_expression = try self.allocator.create(ast.Expression);
    alternate_expression.* = try self.acceptExpression(ctx);
    return .{
        .test_expression = test_expression,
        .consequent_expression = consequent_expression,
        .alternate_expression = alternate_expression,
    };
}

pub fn acceptAssignmentExpression(self: *Self, primary_expression: ast.Expression, ctx: AcceptContext) AcceptError!ast.AssignmentExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.oneOf(.{
        .@"=",
        .@"*=",
        .@"/=",
        .@"%=",
        .@"+=",
        .@"-=",
        .@"<<=",
        .@">>=",
        .@">>>=",
        .@"&=",
        .@"^=",
        .@"|=",
        .@"**=",
        .@"&&=",
        .@"||=",
        .@"??=",
    }));
    const operator: ast.AssignmentExpression.Operator = switch (token.type) {
        .@"=" => .@"=",
        .@"*=" => .@"*=",
        .@"/=" => .@"/=",
        .@"%=" => .@"%=",
        .@"+=" => .@"+=",
        .@"-=" => .@"-=",
        .@"<<=" => .@"<<=",
        .@">>=" => .@">>=",
        .@">>>=" => .@">>>=",
        .@"&=" => .@"&=",
        .@"^=" => .@"^=",
        .@"|=" => .@"|=",
        .@"**=" => .@"**=",
        .@"&&=" => .@"&&=",
        .@"||=" => .@"||=",
        .@"??=" => .@"??=",
        else => unreachable,
    };

    // It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
    if (primary_expression.assignmentTargetType() != .simple) {
        try self.emitErrorAt(state.location, "Invalid left-hand side in assignment expression", .{});
        return error.UnexpectedToken;
    }

    // Defer heap allocation of expression until we know this is an AssignmentExpression
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

pub fn acceptSequenceExpression(self: *Self, primary_expression: ast.Expression) AcceptError!ast.SequenceExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var expressions = std.ArrayList(ast.Expression).init(self.allocator);
    defer expressions.deinit();
    while (self.core.accept(RuleSet.is(.@","))) |_| {
        const expression = try self.acceptExpression(.{});
        try expressions.append(expression);
    } else |_| if (expressions.items.len == 0) return error.UnexpectedToken;
    try expressions.insert(0, primary_expression);
    return .{ .expressions = try expressions.toOwnedSlice() };
}

pub fn acceptExpression(self: *Self, ctx: AcceptContext) (ParserCore.AcceptError || error{OutOfMemory})!ast.Expression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var expression: ast.Expression = if (self.acceptUnaryExpression()) |unary_expression|
        .{ .unary_expression = unary_expression }
    else |_| if (self.acceptUpdateExpression(null)) |update_expression|
        .{ .update_expression = update_expression }
    else |_| if (self.acceptMetaProperty()) |meta_property|
        .{ .meta_property = meta_property }
    else |_| if (self.acceptNewExpression()) |new_expression|
        .{ .new_expression = new_expression }
    else |_| if (self.acceptPrimaryExpression()) |primary_expression|
        .{ .primary_expression = primary_expression }
    else |_|
        return error.UnexpectedToken;

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

pub fn acceptStatement(self: *Self) (ParserCore.AcceptError || error{OutOfMemory})!*ast.Statement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const statement = try self.allocator.create(ast.Statement);

    if (self.acceptBlockStatement()) |block_statement|
        statement.* = .{ .block_statement = block_statement }
    else |_| if (self.acceptVariableStatement(false)) |variable_statement|
        statement.* = .{ .variable_statement = variable_statement }
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
    else |_| if (self.acceptTryStatement()) |try_statement|
        statement.* = .{ .try_statement = try_statement }
    else |_| if (self.acceptDebuggerStatement()) |_|
        statement.* = .debugger_statement
    else |_|
        return error.UnexpectedToken;
    return statement;
}

pub fn acceptDeclaration(self: *Self) !*ast.Declaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const declaration = try self.allocator.create(ast.Declaration);

    if (self.acceptHoistableDeclaration()) |hoistable_declaration|
        declaration.* = .{ .hoistable_declaration = hoistable_declaration }
    else |_| if (self.acceptLexicalDeclaration(false)) |lexical_declaration|
        declaration.* = .{ .lexical_declaration = lexical_declaration }
    else |_|
        return error.UnexpectedToken;
    return declaration;
}

pub fn acceptHoistableDeclaration(self: *Self) AcceptError!ast.HoistableDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptFunctionDeclaration()) |function_declaration|
        return .{ .function_declaration = function_declaration }
    else |_| if (self.acceptGeneratorDeclaration()) |generator_declaration|
        return .{ .generator_declaration = generator_declaration }
    else |_| if (self.acceptAsyncFunctionDeclaration()) |async_function_declaration|
        return .{ .async_function_declaration = async_function_declaration }
    else |_| if (self.acceptAsyncGeneratorDeclaration()) |async_generator_declaration|
        return .{ .async_generator_declaration = async_generator_declaration }
    else |_|
        return error.UnexpectedToken;
}

pub fn acceptBreakableStatement(self: *Self) AcceptError!ast.BreakableStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptIterationStatement()) |iteration_statement|
        return .{ .iteration_statement = iteration_statement }
    else |_|
        return error.UnexpectedToken;
}

pub fn acceptBlockStatement(self: *Self) AcceptError!ast.BlockStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    return .{ .block = try self.acceptBlock() };
}

pub fn acceptBlock(self: *Self) AcceptError!ast.Block {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"{"));
    const block = .{ .statement_list = try self.acceptStatementList() };
    _ = try self.core.accept(RuleSet.is(.@"}"));
    return block;
}

pub fn acceptStatementList(self: *Self) AcceptError!ast.StatementList {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var statement_list_items = std.ArrayList(ast.StatementListItem).init(self.allocator);
    defer statement_list_items.deinit();
    while (self.acceptStatementListItem()) |statement_list_item|
        try statement_list_items.append(statement_list_item)
    else |_| {}
    return .{ .items = try statement_list_items.toOwnedSlice() };
}

pub fn acceptStatementListItem(self: *Self) AcceptError!ast.StatementListItem {
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
    return statement_list_item;
}

pub fn acceptLexicalDeclaration(self: *Self, for_initializer: bool) AcceptError!ast.LexicalDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const let_or_const = self.core.accept(RuleSet.is(.@"const")) catch blk: {
        const identifier = try self.core.accept(RuleSet.is(.identifier));
        if (!std.mem.eql(u8, identifier.text, "let")) return error.UnexpectedToken;
        break :blk identifier;
    };
    const @"type": ast.LexicalDeclaration.Type = switch (let_or_const.type) {
        .identifier => .let,
        .@"const" => .@"const",
        else => unreachable,
    };
    const binding_list = try self.acceptBindingList();
    if (!for_initializer) try self.acceptOrInsertSemicolon();
    return .{ .type = @"type", .binding_list = binding_list };
}

pub fn acceptBindingList(self: *Self) AcceptError!ast.BindingList {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var lexical_bindings = std.ArrayList(ast.LexicalBinding).init(self.allocator);
    while (self.acceptLexicalBinding()) |lexical_binding| {
        try lexical_bindings.append(lexical_binding);
        _ = self.core.accept(RuleSet.is(.@",")) catch break;
    } else |_| {}
    return .{ .items = try lexical_bindings.toOwnedSlice() };
}

pub fn acceptLexicalBinding(self: *Self) AcceptError!ast.LexicalBinding {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const binding_identifier = try self.acceptBindingIdentifier();
    var initializer: ?ast.Expression = null;
    if (self.core.accept(RuleSet.is(.@"="))) |_| {
        const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
        initializer = try self.acceptExpression(ctx);
    } else |_| {}
    return .{ .binding_identifier = binding_identifier, .initializer = initializer };
}

pub fn acceptVariableStatement(self: *Self, for_initializer: bool) AcceptError!ast.VariableStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"var"));
    const variable_declaration_list = try self.acceptVariableDeclarationList();
    if (!for_initializer) try self.acceptOrInsertSemicolon();
    return .{ .variable_declaration_list = variable_declaration_list };
}

pub fn acceptVariableDeclarationList(self: *Self) AcceptError!ast.VariableDeclarationList {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var variable_declarations = std.ArrayList(ast.VariableDeclaration).init(self.allocator);
    defer variable_declarations.deinit();
    while (self.acceptVariableDeclaration()) |variable_declaration| {
        try variable_declarations.append(variable_declaration);
        _ = self.core.accept(RuleSet.is(.@",")) catch break;
    } else |_| {}
    return .{ .items = try variable_declarations.toOwnedSlice() };
}

pub fn acceptVariableDeclaration(self: *Self) AcceptError!ast.VariableDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const binding_identifier = try self.acceptBindingIdentifier();
    var initializer: ?ast.Expression = null;
    if (self.core.accept(RuleSet.is(.@"="))) |_| {
        const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
        initializer = try self.acceptExpression(ctx);
    } else |_| {}
    return .{ .binding_identifier = binding_identifier, .initializer = initializer };
}

pub fn acceptExpressionStatement(self: *Self) AcceptError!ast.ExpressionStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const expression = try self.acceptExpression(.{});
    try self.acceptOrInsertSemicolon();
    return .{ .expression = expression };
}

pub fn acceptIfStatement(self: *Self) AcceptError!ast.IfStatement {
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

pub fn acceptIterationStatement(self: *Self) AcceptError!ast.IterationStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    if (self.acceptDoWhileStatement()) |do_while_statement|
        return .{ .do_while_statement = do_while_statement }
    else |_| if (self.acceptWhileStatement()) |while_statement|
        return .{ .while_statement = while_statement }
    else |_| if (self.acceptForStatement()) |for_statement|
        return .{ .for_statement = for_statement }
    else |_|
        return error.UnexpectedToken;
}

pub fn acceptDoWhileStatement(self: *Self) AcceptError!ast.DoWhileStatement {
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

pub fn acceptWhileStatement(self: *Self) AcceptError!ast.WhileStatement {
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

pub fn acceptForStatement(self: *Self) AcceptError!ast.ForStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"for"));
    _ = try self.core.accept(RuleSet.is(.@"("));
    var initializer: ?ast.ForStatement.Initializer = null;
    if (self.acceptExpression(.{})) |expression|
        initializer = .{ .expression = expression }
    else |_| if (self.acceptVariableStatement(true)) |variable_statement|
        initializer = .{ .variable_statement = variable_statement }
    else |_| if (self.acceptLexicalDeclaration(true)) |lexical_declaration|
        initializer = .{ .lexical_declaration = lexical_declaration }
    else |_| {}
    _ = try self.core.accept(RuleSet.is(.@";"));
    const test_expression = self.acceptExpression(.{}) catch null;
    _ = try self.core.accept(RuleSet.is(.@";"));
    const increment_expression = self.acceptExpression(.{}) catch null;
    _ = try self.core.accept(RuleSet.is(.@")"));
    const consequent_statement = try self.acceptStatement();
    return .{
        .initializer = initializer,
        .test_expression = test_expression,
        .increment_expression = increment_expression,
        .consequent_statement = consequent_statement,
    };
}

pub fn acceptReturnStatement(self: *Self) AcceptError!ast.ReturnStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"return"));

    if (!self.state.in_function_body) {
        try self.emitErrorAt(state.location, "Return statement is only allowed in functions", .{});
        return error.UnexpectedToken;
    }

    if (self.noLineTerminatorHere()) |_| {
        if (self.acceptExpression(.{})) |expression| {
            try self.acceptOrInsertSemicolon();
            return .{ .expression = expression };
        } else |_| {}
    } else |err| switch (err) {
        // Drop emitted 'unexpected newline' error
        error.UnexpectedToken => _ = self.diagnostics.errors.pop(),
        else => {},
    }
    try self.acceptOrInsertSemicolon();
    return .{ .expression = null };
}

pub fn acceptThrowStatement(self: *Self) AcceptError!ast.ThrowStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.throw));
    try self.noLineTerminatorHere();
    const expression = try self.acceptExpression(.{});
    try self.acceptOrInsertSemicolon();
    return .{ .expression = expression };
}

pub fn acceptTryStatement(self: *Self) AcceptError!ast.TryStatement {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.@"try"));
    const try_block = try self.acceptBlock();
    var catch_parameter: ?[]const u8 = null;
    const catch_block = if (self.core.accept(RuleSet.is(.@"catch"))) |_| blk: {
        if (self.core.accept(RuleSet.is(.@"("))) |_| {
            catch_parameter = try self.acceptBindingIdentifier();
            _ = try self.core.accept(RuleSet.is(.@")"));
        } else |_| {}
        break :blk try self.acceptBlock();
    } else |_| null;
    const finally_block = if (self.core.accept(RuleSet.is(.finally))) |_|
        try self.acceptBlock()
    else |_|
        null;
    if (catch_block == null and finally_block == null) {
        try self.emitError("Try statement requires catch or finally block", .{});
        return error.UnexpectedToken;
    }
    return .{
        .try_block = try_block,
        .catch_parameter = catch_parameter,
        .catch_block = catch_block,
        .finally_block = finally_block,
    };
}

pub fn acceptDebuggerStatement(self: *Self) !void {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.debugger));
    try self.acceptOrInsertSemicolon();
}

pub fn acceptFormalParameters(self: *Self) AcceptError!ast.FormalParameters {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var formal_parameters_items = std.ArrayList(ast.FormalParameters.Item).init(self.allocator);
    defer formal_parameters_items.deinit();
    while (self.acceptBindingIdentifier()) |identifier| {
        const formal_parameter = ast.FormalParameter{
            .binding_element = .{ .identifier = identifier },
        };
        try formal_parameters_items.append(.{ .formal_parameter = formal_parameter });
        _ = self.core.accept(RuleSet.is(.@",")) catch break;
    } else |_| {}
    return .{ .items = try formal_parameters_items.toOwnedSlice() };
}

pub fn acceptFunctionDeclaration(self: *Self) AcceptError!ast.FunctionDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.function));
    // We need to do this after consuming the 'function' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "function".len);
    const identifier = self.acceptBindingIdentifier() catch |err| {
        if (self.core.peek() catch null) |next_token| if (next_token.type == .@"(") {
            try self.emitError("Function declaration must have a binding identifier", .{});
        };
        return err;
    };
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.normal);
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

pub fn acceptFunctionExpression(self: *Self) AcceptError!ast.FunctionExpression {
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
    const function_body = try self.acceptFunctionBody(.normal);
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

pub fn acceptFunctionBody(self: *Self, @"type": ast.FunctionBody.Type) AcceptError!ast.FunctionBody {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const tmp = temporaryChange(&self.state, "in_function_body", true);
    defer tmp.restore();

    const statement_list = try self.acceptStatementList();
    return .{ .type = @"type", .statement_list = statement_list };
}

pub fn acceptArrowFunction(self: *Self) AcceptError!ast.ArrowFunction {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    var start_offset: usize = undefined;
    var formal_parameters: ast.FormalParameters = undefined;
    if (self.acceptBindingIdentifier()) |identifier| {
        // We need to do this after consuming the identifier token to skip preceeding whitespace.
        start_offset = self.core.tokenizer.offset - identifier.len;
        var formal_parameters_items = try std.ArrayList(ast.FormalParameters.Item).initCapacity(
            self.allocator,
            1,
        );
        formal_parameters_items.appendAssumeCapacity(.{
            .formal_parameter = .{ .binding_element = .{ .identifier = identifier } },
        });
        formal_parameters = .{ .items = try formal_parameters_items.toOwnedSlice() };
    } else |_| {
        _ = try self.core.accept(RuleSet.is(.@"("));
        // We need to do this after consuming the '(' token to skip preceeding whitespace.
        start_offset = self.core.tokenizer.offset - (comptime "(".len);
        formal_parameters = try self.acceptFormalParameters();
        _ = try self.core.accept(RuleSet.is(.@")"));
    }
    _ = try self.core.accept(RuleSet.is(.@"=>"));
    try self.noLineTerminatorHere();
    const function_body = if (self.core.accept(RuleSet.is(.@"{"))) |_| blk: {
        const function_body = try self.acceptFunctionBody(.normal);
        _ = try self.core.accept(RuleSet.is(.@"}"));
        break :blk function_body;
    } else |_| blk: {
        const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
        const expression_body = try self.acceptExpression(ctx);
        // Synthesize a FunctionBody with return statement
        const statement = try self.allocator.create(ast.Statement);
        statement.* = ast.Statement{ .return_statement = .{ .expression = expression_body } };
        const items = try self.allocator.alloc(ast.StatementListItem, 1);
        items[0] = .{ .statement = statement };
        const statement_list = ast.StatementList{ .items = items };
        break :blk ast.FunctionBody{ .type = .normal, .statement_list = statement_list };
    };
    const end_offset = self.core.tokenizer.offset;
    const source_text = try self.allocator.dupe(
        u8,
        self.core.tokenizer.source[start_offset..end_offset],
    );
    return .{
        .formal_parameters = formal_parameters,
        .function_body = function_body,
        .source_text = source_text,
    };
}

fn acceptGeneratorDeclaration(self: *Self) AcceptError!ast.GeneratorDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.function));
    // We need to do this after consuming the 'function' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "function".len);
    _ = try self.core.accept(RuleSet.is(.@"*"));
    const identifier = self.acceptBindingIdentifier() catch |err| {
        try self.emitError("Generator declaration must have a binding identifier", .{});
        return err;
    };
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.generator);
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

pub fn acceptGeneratorExpression(self: *Self) AcceptError!ast.GeneratorExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = try self.core.accept(RuleSet.is(.function));
    // We need to do this after consuming the 'function' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "function".len);
    _ = try self.core.accept(RuleSet.is(.@"*"));
    const identifier = self.acceptBindingIdentifier() catch null;
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.generator);
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

fn acceptAsyncGeneratorDeclaration(self: *Self) AcceptError!ast.AsyncGeneratorDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.identifier));
    if (!std.mem.eql(u8, token.text, "async")) return error.UnexpectedToken;
    // We need to do this after consuming the 'async' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "async".len);
    try self.noLineTerminatorHere();
    _ = try self.core.accept(RuleSet.is(.function));
    _ = try self.core.accept(RuleSet.is(.@"*"));
    const identifier = self.acceptBindingIdentifier() catch |err| {
        try self.emitError("Async generator declaration must have a binding identifier", .{});
        return err;
    };
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.async_generator);
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

pub fn acceptAsyncGeneratorExpression(self: *Self) AcceptError!ast.AsyncGeneratorExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.identifier));
    if (!std.mem.eql(u8, token.text, "async")) return error.UnexpectedToken;
    // We need to do this after consuming the 'async' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "async".len);
    try self.noLineTerminatorHere();
    _ = try self.core.accept(RuleSet.is(.function));
    _ = try self.core.accept(RuleSet.is(.@"*"));
    const identifier = self.acceptBindingIdentifier() catch null;
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.async_generator);
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

fn acceptRegularExpressionLiteral(self: *Self) AcceptError!ast.RegularExpressionLiteral {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const tmp = temporaryChange(&tokenizer_.state, "parsing_regular_expression", true);
    defer tmp.restore();

    const token = try self.core.accept(RuleSet.is(.regular_expression));
    var regular_expression_literal = parseRegularExpressionLiteral(token.text, .complete) catch unreachable;
    switch (regular_expression_literal.isValidRegularExpressionLiteral()) {
        .invalid_pattern => {
            try self.emitErrorAt(token.location, "Invalid RegExp pattern", .{});
            return error.UnexpectedToken;
        },
        .invalid_flags => {
            var flags_location = token.location;
            flags_location.column += @intCast(regular_expression_literal.pattern.len + 2);
            try self.emitErrorAt(flags_location, "Invalid RegExp flags", .{});
            return error.UnexpectedToken;
        },
        .valid => {},
    }
    regular_expression_literal.pattern = try self.allocator.dupe(u8, regular_expression_literal.pattern);
    regular_expression_literal.flags = try self.allocator.dupe(u8, regular_expression_literal.flags);
    return regular_expression_literal;
}

fn acceptAsyncFunctionDeclaration(self: *Self) AcceptError!ast.AsyncFunctionDeclaration {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.identifier));
    if (!std.mem.eql(u8, token.text, "async")) return error.UnexpectedToken;
    // We need to do this after consuming the 'async' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "async".len);
    try self.noLineTerminatorHere();
    _ = try self.core.accept(RuleSet.is(.function));
    const identifier = self.acceptBindingIdentifier() catch |err| {
        if (self.core.peek() catch null) |next_token| if (next_token.type == .@"(") {
            try self.emitError("Async function declaration must have a binding identifier", .{});
        };
        return err;
    };
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.@"async");
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

pub fn acceptAsyncFunctionExpression(self: *Self) AcceptError!ast.AsyncFunctionExpression {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.identifier));
    if (!std.mem.eql(u8, token.text, "async")) return error.UnexpectedToken;
    // We need to do this after consuming the 'async' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "async".len);
    try self.noLineTerminatorHere();
    _ = try self.core.accept(RuleSet.is(.function));
    const identifier = self.acceptBindingIdentifier() catch null;
    _ = try self.core.accept(RuleSet.is(.@"("));
    const formal_parameters = try self.acceptFormalParameters();
    _ = try self.core.accept(RuleSet.is(.@")"));
    _ = try self.core.accept(RuleSet.is(.@"{"));
    const function_body = try self.acceptFunctionBody(.@"async");
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

pub fn acceptAsyncArrowFunction(self: *Self) AcceptError!ast.AsyncArrowFunction {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    const token = try self.core.accept(RuleSet.is(.identifier));
    if (!std.mem.eql(u8, token.text, "async")) return error.UnexpectedToken;
    // We need to do this after consuming the 'async' token to skip preceeding whitespace.
    const start_offset = self.core.tokenizer.offset - (comptime "async".len);
    try self.noLineTerminatorHere();
    var formal_parameters: ast.FormalParameters = undefined;
    if (self.acceptBindingIdentifier()) |identifier| {
        var formal_parameters_items = try std.ArrayList(ast.FormalParameters.Item).initCapacity(
            self.allocator,
            1,
        );
        formal_parameters_items.appendAssumeCapacity(.{
            .formal_parameter = .{ .binding_element = .{ .identifier = identifier } },
        });
        formal_parameters = .{ .items = try formal_parameters_items.toOwnedSlice() };
    } else |_| {
        _ = try self.core.accept(RuleSet.is(.@"("));
        formal_parameters = try self.acceptFormalParameters();
        _ = try self.core.accept(RuleSet.is(.@")"));
    }
    _ = try self.core.accept(RuleSet.is(.@"=>"));
    try self.noLineTerminatorHere();
    const function_body = if (self.core.accept(RuleSet.is(.@"{"))) |_| blk: {
        const function_body = try self.acceptFunctionBody(.@"async");
        _ = try self.core.accept(RuleSet.is(.@"}"));
        break :blk function_body;
    } else |_| blk: {
        const ctx = AcceptContext{ .precedence = getPrecedence(.@",") + 1 };
        const expression_body = try self.acceptExpression(ctx);
        // Synthesize a FunctionBody with return statement
        const statement = try self.allocator.create(ast.Statement);
        statement.* = ast.Statement{ .return_statement = .{ .expression = expression_body } };
        const items = try self.allocator.alloc(ast.StatementListItem, 1);
        items[0] = .{ .statement = statement };
        const statement_list = ast.StatementList{ .items = items };
        break :blk ast.FunctionBody{ .type = .@"async", .statement_list = statement_list };
    };
    const end_offset = self.core.tokenizer.offset;
    const source_text = try self.allocator.dupe(
        u8,
        self.core.tokenizer.source[start_offset..end_offset],
    );
    return .{
        .formal_parameters = formal_parameters,
        .function_body = function_body,
        .source_text = source_text,
    };
}

pub fn acceptScript(self: *Self) AcceptError!ast.Script {
    const state = self.core.saveState();
    errdefer self.core.restoreState(state);

    _ = self.core.accept(RuleSet.is(.hashbang_comment)) catch {};
    const statement_list = try self.acceptStatementList();
    return .{ .statement_list = statement_list };
}
