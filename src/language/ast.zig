const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const reg_exp = @import("../builtins/reg_exp.zig");
const tokenizer = @import("tokenizer.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");
const line_terminators = tokenizer.line_terminators;

const Agent = execution.Agent;
const BigInt = types.BigInt;
const Environment = execution.Environment;
const ExportEntry = language.ExportEntry;
const Object = types.Object;
const PrivateEnvironment = execution.PrivateEnvironment;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const escapeSequenceMatcher = tokenizer.escapeSequenceMatcher;
const makeConstructor = builtins.makeConstructor;
const noexcept = utils.noexcept;
const ordinaryFunctionCreate = builtins.ordinaryFunctionCreate;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const setFunctionName = builtins.setFunctionName;

const AnalyzeQuery = enum {
    is_reference,
    is_string_literal,
};

const VarScopedDeclaration = union(enum) {
    variable_declaration: VariableDeclaration,
    hoistable_declaration: HoistableDeclaration,
};

const LexicallyScopedDeclaration = union(enum) {
    const Self = @This();

    hoistable_declaration: HoistableDeclaration,
    class_declaration: ClassDeclaration,
    lexical_declaration: LexicalDeclaration,
    export_declaration: Expression,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        switch (self) {
            // ExportDeclaration : export default AssignmentExpression ;
            .export_declaration => {
                // 1. Return « "*default*" ».
                var bound_names = try std.ArrayList(Identifier).initCapacity(allocator, 1);
                bound_names.appendAssumeCapacity("*default*");
                return bound_names.toOwnedSlice();
            },
            inline else => |node| return node.boundNames(allocator),
        }
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(self: Self) bool {
        switch (self) {
            // ExportDeclaration :
            //     export ExportFromClause FromClause ;
            //     export NamedExports ;
            //     export default AssignmentExpression ;
            .export_declaration => {
                // 1. Return false.
                return false;
            },
            inline else => |node| return node.isConstantDeclaration(),
        }
    }
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
};

/// https://tc39.es/ecma262/#prod-IdentifierReference
pub const IdentifierReference = []const u8;

/// https://tc39.es/ecma262/#prod-Identifier
pub const Identifier = []const u8;

/// https://tc39.es/ecma262/#prod-PrivateIdentifier
pub const PrivateIdentifier = []const u8;

/// https://tc39.es/ecma262/#prod-PrimaryExpression
pub const PrimaryExpression = union(enum) {
    const Self = @This();

    this,
    identifier_reference: IdentifierReference,
    literal: Literal,
    array_literal: ArrayLiteral,
    object_literal: ObjectLiteral,
    function_expression: FunctionExpression,
    class_expression: ClassExpression,
    generator_expression: GeneratorExpression,
    async_function_expression: AsyncFunctionExpression,
    async_generator_expression: AsyncGeneratorExpression,
    regular_expression_literal: RegularExpressionLiteral,
    template_literal: TemplateLiteral,
    arrow_function: ArrowFunction,
    async_arrow_function: AsyncArrowFunction,
    parenthesized_expression: ParenthesizedExpression,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => switch (self) {
                .identifier_reference => true,
                .parenthesized_expression => |parenthesized_expression| parenthesized_expression.analyze(query),
                else => false,
            },
            .is_string_literal => switch (self) {
                .literal => |literal| literal.analyze(query),
                else => false,
            },
        };
    }
};

/// https://tc39.es/ecma262/#prod-MemberExpression
pub const MemberExpression = struct {
    pub const Property = union(enum) {
        expression: *Expression,
        identifier: Identifier,
        private_identifier: PrivateIdentifier,
    };

    expression: *Expression,
    property: Property,
};

/// https://tc39.es/ecma262/#prod-SuperProperty
pub const SuperProperty = union(enum) {
    expression: *Expression,
    identifier: Identifier,
};

/// https://tc39.es/ecma262/#prod-MetaProperty
pub const MetaProperty = union(enum) {
    new_target,
    import_meta,
};

/// https://tc39.es/ecma262/#prod-NewExpression
pub const NewExpression = struct {
    expression: *Expression,
    arguments: Arguments,
};

/// https://tc39.es/ecma262/#prod-CallExpression
pub const CallExpression = struct {
    expression: *Expression,
    arguments: Arguments,
};

/// https://tc39.es/ecma262/#prod-SuperCall
pub const SuperCall = struct {
    arguments: Arguments,
};

/// https://tc39.es/ecma262/#prod-ImportCall
pub const ImportCall = struct {
    expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-Arguments
pub const Arguments = []const Expression;

/// https://tc39.es/ecma262/#prod-OptionalExpression
pub const OptionalExpression = struct {
    pub const Property = union(enum) {
        arguments: Arguments,
        expression: *Expression,
        identifier: Identifier,
        private_identifier: PrivateIdentifier,
    };

    expression: *Expression,
    property: Property,
};

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
    pub fn numericValue(self: Self, allocator: Allocator) Allocator.Error!Value {
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
                const number: f64 = switch (self.system) {
                    .decimal => std.fmt.parseFloat(f64, str) catch unreachable,
                    else => @floatFromInt(std.fmt.parseInt(i128, str, base) catch unreachable),
                };
                return Value.from(number);
            },
            .big_int => {
                var big_int = try BigInt.from(allocator, 0);
                big_int.value.setString(base, str) catch |err| switch (err) {
                    error.InvalidBase, error.InvalidCharacter => unreachable,
                    error.OutOfMemory => return error.OutOfMemory,
                };
                return Value.from(big_int);
            },
        }
    }
};

fn stringValueImpl(allocator: Allocator, text: []const u8) Allocator.Error![]const u8 {
    var str = try std.ArrayList(u8).initCapacity(allocator, text.len);
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        const c = text[i];
        if (c == '\\') {
            for (line_terminators) |line_terminator| {
                if (std.mem.startsWith(u8, text[i + 1 ..], line_terminator)) {
                    i += line_terminator.len;
                    break;
                }
            } else {
                switch (text[i + 1]) {
                    '0' => str.appendAssumeCapacity(0),
                    'b' => str.appendAssumeCapacity(0x08),
                    'f' => str.appendAssumeCapacity(0x0c),
                    'n' => str.appendAssumeCapacity('\n'),
                    'r' => str.appendAssumeCapacity('\r'),
                    't' => str.appendAssumeCapacity('\t'),
                    'v' => str.appendAssumeCapacity(0x0b),
                    'x' => {
                        const byte = std.fmt.parseInt(
                            u8,
                            text[i + 2 .. i + 4],
                            16,
                        ) catch unreachable;
                        str.appendAssumeCapacity(byte);
                        i += 2;
                    },
                    'u' => {
                        const chars = switch (text[i + 2]) {
                            '{' => text[i + 3 .. i + escapeSequenceMatcher(text[i..]).? - 1],
                            else => text[i + 2 .. i + 6],
                        };
                        const code_point = std.fmt.parseInt(u21, chars, 16) catch unreachable;
                        var out: [4]u8 = undefined;
                        const len = std.unicode.utf8Encode(code_point, &out) catch |err| switch (err) {
                            error.CodepointTooLarge => unreachable,
                            // TODO: Handle surrogate halfs
                            error.Utf8CannotEncodeSurrogateHalf => @as(u3, 0),
                        };
                        str.appendSliceAssumeCapacity(out[0..len]);
                        i += switch (text[i + 2]) {
                            '{' => chars.len + 2,
                            else => 4,
                        };
                    },
                    else => |next_c| str.appendAssumeCapacity(next_c),
                }
                i += 1;
            }
        } else str.appendAssumeCapacity(c);
    }
    return str.toOwnedSlice();
}

/// https://tc39.es/ecma262/#prod-StringLiteral
pub const StringLiteral = struct {
    const Self = @This();

    text: []const u8,

    /// 12.9.4.2 Static Semantics: SV
    /// https://tc39.es/ecma262/#sec-static-semantics-sv
    pub fn stringValue(self: Self, allocator: Allocator) Allocator.Error!Value {
        std.debug.assert(self.text.len >= 2);
        return Value.from(try stringValueImpl(allocator, self.text[1 .. self.text.len - 1]));
    }
};

/// https://tc39.es/ecma262/#prod-ArrayLiteral
pub const ArrayLiteral = struct {
    pub const Element = union(enum) {
        elision,
        expression: Expression,
        spread: Expression,
    };

    element_list: []const Element,
};

/// https://tc39.es/ecma262/#prod-ObjectLiteral
pub const ObjectLiteral = struct {
    property_definition_list: PropertyDefinitionList,
};

/// https://tc39.es/ecma262/#prod-PropertyDefinitionList
pub const PropertyDefinitionList = struct {
    items: []const PropertyDefinition,
};

/// https://tc39.es/ecma262/#prod-PropertyDefinition
pub const PropertyDefinition = union(enum) {
    pub const PropertyNameAndExpression = struct {
        property_name: PropertyName,
        expression: Expression,
    };

    spread: Expression,
    identifier_reference: IdentifierReference,
    property_name_and_expression: PropertyNameAndExpression,
    method_definition: MethodDefinition,
};

/// https://tc39.es/ecma262/#prod-PropertyName
pub const PropertyName = union(enum) {
    literal_property_name: union(enum) {
        identifier: Identifier,
        string_literal: StringLiteral,
        numeric_literal: NumericLiteral,
    },
    computed_property_name: Expression,
};

/// https://tc39.es/ecma262/#prod-RegularExpressionLiteral
pub const RegularExpressionLiteral = struct {
    const Self = @This();

    pattern: []const u8,
    flags: []const u8,

    /// 13.2.7.2 Static Semantics: IsValidRegularExpressionLiteral ( literal )
    /// https://tc39.es/ecma262/#sec-isvalidregularexpressionliteral
    pub fn isValidRegularExpressionLiteral(self: Self) enum {
        valid,
        invalid_pattern,
        invalid_flags,
    } {
        // 1. Let flags be FlagText of literal.
        // 2. If flags contains any code points other than d, g, i, m, s, u, v, or y, or if flags
        //    contains any code point more than once, return false.
        _ = reg_exp.ParsedFlags.from(self.flags) orelse return .invalid_flags;

        // TODO: 3-8.
        // The only way we have right now to validate a regex pattern is `libregexp.lre_compile()`,
        // which would be wasteful to call here. At the very least we'd have to reuse the bytecode.
        return .valid;
    }
};

/// https://tc39.es/ecma262/#prod-TemplateLiteral
pub const TemplateLiteral = struct {
    pub const Span = union(enum) {
        const Self = @This();

        text: []const u8,
        expression: Expression,

        /// https://tc39.es/ecma262/#sec-static-semantics-tv
        /// 12.9.6.1 Static Semantics: TV
        pub fn templateValue(self: Self, allocator: Allocator) Allocator.Error!Value {
            std.debug.assert(self.text[0] == '`' or self.text[0] == '}');
            return if (self.text[self.text.len - 1] == '`')
                Value.from(try stringValueImpl(allocator, self.text[1 .. self.text.len - 1]))
            else
                Value.from(try stringValueImpl(allocator, self.text[1 .. self.text.len - 2]));
        }
    };

    spans: []const Span,
};

/// https://tc39.es/ecma262/#prod-UpdateExpression
pub const UpdateExpression = struct {
    pub const Type = enum {
        prefix,
        postfix,
    };

    pub const Operator = enum {
        @"++",
        @"--",
    };

    type: Type,
    operator: Operator,
    expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-UnaryExpression
pub const UnaryExpression = struct {
    pub const Operator = enum {
        delete,
        void,
        typeof,
        @"+",
        @"-",
        @"~",
        @"!",
    };

    operator: Operator,
    expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-ExponentiationExpression
/// https://tc39.es/ecma262/#prod-MultiplicativeExpression
/// https://tc39.es/ecma262/#prod-AdditiveExpression#
/// https://tc39.es/ecma262/#prod-ShiftExpression
/// https://tc39.es/ecma262/#prod-BitwiseANDExpression
/// https://tc39.es/ecma262/#prod-BitwiseXORExpression
/// https://tc39.es/ecma262/#prod-BitwiseORExpression
pub const BinaryExpression = struct {
    pub const Operator = enum {
        @"**",
        @"*",
        @"/",
        @"%",
        @"+",
        @"-",
        @"<<",
        @">>",
        @">>>",
        @"&",
        @"^",
        @"|",
    };

    operator: Operator,
    lhs_expression: *Expression,
    rhs_expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-RelationalExpression
pub const RelationalExpression = struct {
    pub const Operator = enum {
        @"<",
        @">",
        @"<=",
        @">=",
        instanceof,
        in,
    };

    pub const Lhs = union(enum) {
        expression: *Expression,
        private_identifier: PrivateIdentifier,
    };

    operator: Operator,
    lhs: Lhs,
    rhs_expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-EqualityExpression
pub const EqualityExpression = struct {
    pub const Operator = enum {
        @"==",
        @"!=",
        @"===",
        @"!==",
    };

    operator: Operator,
    lhs_expression: *Expression,
    rhs_expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-LogicalExpression
pub const LogicalExpression = struct {
    pub const Operator = enum {
        @"&&",
        @"||",
        @"??",
    };

    operator: Operator,
    lhs_expression: *Expression,
    rhs_expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-ConditionalExpression
pub const ConditionalExpression = struct {
    test_expression: *Expression,
    consequent_expression: *Expression,
    alternate_expression: *Expression,
};

/// https://tc39.es/ecma262/#prod-AssignmentExpression
pub const AssignmentExpression = struct {
    pub const Operator = enum {
        @"=",
        @"*=",
        @"/=",
        @"%=",
        @"+=",
        @"-=",
        @"<<=",
        @">>=",
        @">>>=",
        @"&=",
        @"^=",
        @"|=",
        @"**=",
        @"&&=",
        @"||=",
        @"??=",
    };

    operator: Operator,
    lhs_expression: *Expression,
    rhs_expression: *Expression,
};

pub const SequenceExpression = struct {
    expressions: []const Expression,
};

/// https://tc39.es/ecma262/#prod-Expression
pub const Expression = union(enum) {
    const Self = @This();

    primary_expression: PrimaryExpression,
    member_expression: MemberExpression,
    super_property: SuperProperty,
    meta_property: MetaProperty,
    new_expression: NewExpression,
    call_expression: CallExpression,
    super_call: SuperCall,
    import_call: ImportCall,
    optional_expression: OptionalExpression,
    update_expression: UpdateExpression,
    unary_expression: UnaryExpression,
    binary_expression: BinaryExpression,
    relational_expression: RelationalExpression,
    equality_expression: EqualityExpression,
    logical_expression: LogicalExpression,
    conditional_expression: ConditionalExpression,
    assignment_expression: AssignmentExpression,
    sequence_expression: SequenceExpression,

    /// 8.6.4 Static Semantics: AssignmentTargetType
    /// https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype
    pub fn assignmentTargetType(self: Self) enum { simple, invalid } {
        switch (self) {
            .primary_expression => |primary_expression| switch (primary_expression) {
                .identifier_reference => |identifier_reference| {
                    // TODO: 1. If this IdentifierReference is contained in strict mode code and StringValue
                    //    of Identifier is either "eval" or "arguments", return invalid.
                    if (false and
                        (std.mem.eql(u8, identifier_reference.identifier, "eval") or
                        std.mem.eql(u8, identifier_reference.identifier, "arguments")))
                        return .invalid;

                    // 2. Return simple.
                    return .simple;
                },
                .parenthesized_expression => |parenthesized_expression| {
                    // 1. Let expr be the ParenthesizedExpression that is covered by
                    //    CoverParenthesizedExpressionAndArrowParameterList.
                    // 2. Return AssignmentTargetType of expr.
                    return parenthesized_expression.expression.assignmentTargetType();
                },
                else => {},
            },
            .member_expression,
            .super_property,
            => {
                // 1. Return simple.
                return .simple;
            },
            else => {},
        }

        // 1. Return invalid.
        return .invalid;
    }

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => switch (self) {
                .primary_expression => |primary_expression| primary_expression.analyze(query),
                .member_expression,
                .super_property,
                .optional_expression,
                => true,
                else => false,
            },
            .is_string_literal => switch (self) {
                .primary_expression => |primary_expression| primary_expression.analyze(query),
                else => false,
            },
        };
    }
};

/// https://tc39.es/ecma262/#prod-Statement
pub const Statement = union(enum) {
    const Self = @This();

    block_statement: BlockStatement,
    variable_statement: VariableStatement,
    empty_statement,
    expression_statement: ExpressionStatement,
    if_statement: IfStatement,
    breakable_statement: BreakableStatement,
    continue_statement: ContinueStatement,
    break_statement: BreakStatement,
    return_statement: ReturnStatement,
    throw_statement: ThrowStatement,
    try_statement: TryStatement,
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

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        switch (self) {
            // Statement :
            //     EmptyStatement
            //     ExpressionStatement
            //     ContinueStatement
            //     BreakStatement
            //     ReturnStatement
            //     ThrowStatement
            //     DebuggerStatement
            // 1. Return a new empty List.
            .empty_statement,
            .expression_statement,
            .continue_statement,
            .break_statement,
            .return_statement,
            .throw_statement,
            .debugger_statement,
            => return &.{},

            .block_statement => |block_statement| return block_statement.block.statement_list.varDeclaredNames(allocator),
            .variable_statement => |variable_statement| return variable_statement.variable_declaration_list.varDeclaredNames(allocator),
            .if_statement => |if_statement| return if_statement.varDeclaredNames(allocator),
            .breakable_statement => |breakable_statement| switch (breakable_statement.iteration_statement) {
                inline else => |node| return node.varDeclaredNames(allocator),
            },
            .try_statement => |try_statement| return try_statement.varDeclaredNames(allocator),
        }
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        switch (self) {
            // Statement :
            //     EmptyStatement
            //     ExpressionStatement
            //     ContinueStatement
            //     BreakStatement
            //     ReturnStatement
            //     ThrowStatement
            //     DebuggerStatement
            // 1. Return a new empty List.
            .empty_statement,
            .expression_statement,
            .continue_statement,
            .break_statement,
            .return_statement,
            .throw_statement,
            .debugger_statement,
            => return &.{},

            .block_statement => |block_statement| return block_statement.block.statement_list.varScopedDeclarations(allocator),
            .variable_statement => |variable_statement| return variable_statement.variable_declaration_list.varScopedDeclarations(allocator),
            .if_statement => |if_statement| return if_statement.varScopedDeclarations(allocator),
            .breakable_statement => |breakable_statement| switch (breakable_statement.iteration_statement) {
                inline else => |node| return node.varScopedDeclarations(allocator),
            },
            .try_statement => |try_statement| return try_statement.varScopedDeclarations(allocator),
        }
    }
};

/// https://tc39.es/ecma262/#prod-Declaration
pub const Declaration = union(enum) {
    const Self = @This();

    hoistable_declaration: HoistableDeclaration,
    class_declaration: ClassDeclaration,
    lexical_declaration: LexicalDeclaration,

    pub fn analyze(_: Self, query: AnalyzeQuery) bool {
        return switch (query) {
            .is_reference => false,
            .is_string_literal => false,
        };
    }

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        switch (self) {
            inline else => |node| return node.boundNames(allocator),
        }
    }
};

/// https://tc39.es/ecma262/#prod-HoistableDeclaration
pub const HoistableDeclaration = union(enum) {
    const Self = @This();

    function_declaration: FunctionDeclaration,
    generator_declaration: GeneratorDeclaration,
    async_function_declaration: AsyncFunctionDeclaration,
    async_generator_declaration: AsyncGeneratorDeclaration,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        switch (self) {
            inline else => |node| return node.boundNames(allocator),
        }
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(self: Self) bool {
        switch (self) {
            inline else => |node| return node.isConstantDeclaration(),
        }
    }
};

/// https://tc39.es/ecma262/#prod-BreakableStatement
pub const BreakableStatement = union(enum) {
    iteration_statement: IterationStatement,
};

/// https://tc39.es/ecma262/#prod-BlockStatement
pub const BlockStatement = struct {
    block: Block,
};

/// https://tc39.es/ecma262/#prod-Block
pub const Block = struct {
    statement_list: StatementList,
};

/// https://tc39.es/ecma262/#prod-StatementList
pub const StatementList = struct {
    const Self = @This();

    items: []const StatementListItem,

    /// 8.2.5 Static Semantics: LexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations
    pub fn lexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        // StatementList : StatementList StatementListItem
        // 1. Let declarations1 be LexicallyScopedDeclarations of StatementList.
        // 2. Let declarations2 be LexicallyScopedDeclarations of StatementListItem.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        var declarations = std.ArrayList(LexicallyScopedDeclaration).init(allocator);
        for (self.items) |item| {
            try declarations.appendSlice(try item.lexicallyScopedDeclarations(allocator));
        }
        return declarations.toOwnedSlice();
    }

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // StatementList : StatementList StatementListItem
        // 1. Let names1 be VarDeclaredNames of StatementList.
        // 2. Let names2 be VarDeclaredNames of StatementListItem.
        // 3. Return the list-concatenation of names1 and names2.
        // StatementListItem : Declaration
        // 1. Return a new empty List.
        var var_declared_names = std.ArrayList(Identifier).init(allocator);
        for (self.items) |item| {
            try var_declared_names.appendSlice(try item.varDeclaredNames(allocator));
        }
        return var_declared_names.toOwnedSlice();
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // StatementList : StatementList StatementListItem
        // 1. Let declarations1 be VarScopedDeclarations of StatementList.
        // 2. Let declarations2 be VarScopedDeclarations of StatementListItem.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        // StatementListItem : Declaration
        // 1. Return a new empty List.
        var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
        for (self.items) |item| {
            try variable_declarations.appendSlice(try item.varScopedDeclarations(allocator));
        }
        return variable_declarations.toOwnedSlice();
    }

    /// 8.2.9 Static Semantics: TopLevelLexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-toplevellexicallyscopeddeclarations
    pub fn topLevelLexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        // StatementList : StatementList StatementListItem
        // 1. Let declarations1 be TopLevelLexicallyScopedDeclarations of StatementList.
        // 2. Let declarations2 be TopLevelLexicallyScopedDeclarations of StatementListItem.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        var declarations = std.ArrayList(LexicallyScopedDeclaration).init(allocator);
        for (self.items) |item| {
            try declarations.appendSlice(try item.topLevelLexicallyScopedDeclarations(allocator));
        }
        return declarations.toOwnedSlice();
    }

    /// 8.2.10 Static Semantics: TopLevelVarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-toplevelvardeclarednames
    pub fn topLevelVarDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // StatementList : StatementList StatementListItem
        // 1. Let names1 be TopLevelVarDeclaredNames of StatementList.
        // 2. Let names2 be TopLevelVarDeclaredNames of StatementListItem.
        // 3. Return the list-concatenation of names1 and names2.
        var var_declared_names = std.ArrayList(Identifier).init(allocator);
        for (self.items) |item| switch (item) {
            // StatementListItem : Statement
            .statement => |statement| {
                // TODO: 1. If Statement is Statement : LabelledStatement , return
                //          TopLevelVarDeclaredNames of Statement.
                // 2. Return VarDeclaredNames of Statement.
                try var_declared_names.appendSlice(try statement.varDeclaredNames(allocator));
            },
            // StatementListItem : Declaration
            .declaration => |declaration| {
                switch (declaration.*) {
                    // 1. If Declaration is Declaration : HoistableDeclaration , then
                    .hoistable_declaration => |hoistable_declaration| {
                        // a. Return the BoundNames of HoistableDeclaration.
                        try var_declared_names.appendSlice(try hoistable_declaration.boundNames(allocator));
                    },

                    // HACK: Emit lexical declarations too while they're codegen'd as var decls
                    .lexical_declaration => |lexical_declaration| {
                        for (lexical_declaration.binding_list.items) |lexical_binding| {
                            try var_declared_names.append(lexical_binding.binding_identifier);
                        }
                    },

                    // 2. Return a new empty List.
                    else => {},
                }
            },
        };
        return var_declared_names.toOwnedSlice();
    }

    /// 8.2.11 Static Semantics: TopLevelVarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-toplevelvarscopeddeclarations
    pub fn topLevelVarScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // StatementList : StatementList StatementListItem
        // 1. Let declarations1 be TopLevelVarScopedDeclarations of StatementList.
        // 2. Let declarations2 be TopLevelVarScopedDeclarations of StatementListItem.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
        for (self.items) |item| switch (item) {
            // StatementListItem : Statement
            .statement => |statement| {
                // TODO: 1. If Statement is Statement : LabelledStatement , return
                //          TopLevelVarScopedDeclarations of Statement.
                // 2. Return VarScopedDeclarations of Statement.
                try variable_declarations.appendSlice(try statement.varScopedDeclarations(allocator));
            },
            // StatementListItem : Declaration
            .declaration => |declaration| {
                switch (declaration.*) {
                    // 1. If Declaration is Declaration : HoistableDeclaration , then
                    .hoistable_declaration => |hoistable_declaration| {
                        // a. Let declaration be DeclarationPart of HoistableDeclaration.
                        // b. Return « declaration ».
                        try variable_declarations.append(.{ .hoistable_declaration = hoistable_declaration });
                    },

                    // HACK: Emit lexical declarations too while they're codegen'd as var decls
                    .lexical_declaration => |lexical_declaration| {
                        for (lexical_declaration.binding_list.items) |lexical_binding| {
                            try variable_declarations.append(.{
                                .variable_declaration = .{
                                    .binding_identifier = lexical_binding.binding_identifier,
                                    .initializer = lexical_binding.initializer,
                                },
                            });
                        }
                    },

                    // 2. Return a new empty List.
                    else => {},
                }
            },
        };
        return variable_declarations.toOwnedSlice();
    }

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
};

/// https://tc39.es/ecma262/#prod-StatementListItem
pub const StatementListItem = union(enum) {
    const Self = @This();

    statement: *Statement,
    declaration: *Declaration,

    /// 8.2.5 Static Semantics: LexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations
    pub fn lexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        var declarations = std.ArrayList(LexicallyScopedDeclaration).init(allocator);
        switch (self) {
            // StatementListItem : Statement
            .statement => {
                // TODO: 1. If Statement is Statement : LabelledStatement , return
                //          LexicallyScopedDeclarations of LabelledStatement.
                // 2. Return a new empty List.
            },

            // StatementListItem : Declaration
            .declaration => |declaration| switch (declaration.*) {
                // 1. Return a List whose sole element is DeclarationPart of Declaration.
                .hoistable_declaration => |hoistable_declaration| try declarations.append(.{ .hoistable_declaration = hoistable_declaration }),
                .class_declaration => |class_declaration| try declarations.append(.{ .class_declaration = class_declaration }),
                .lexical_declaration => |lexical_declaration| try declarations.append(.{ .lexical_declaration = lexical_declaration }),
            },
        }
        return declarations.toOwnedSlice();
    }

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        switch (self) {
            .statement => |statement| return try statement.varDeclaredNames(allocator),
            .declaration => |declaration| {
                var var_declared_names = std.ArrayList(Identifier).init(allocator);
                switch (declaration.*) {
                    // HACK: Emit lexical declarations too while they're codegen'd as var decls
                    .lexical_declaration => |lexical_declaration| {
                        for (lexical_declaration.binding_list.items) |lexical_binding| {
                            try var_declared_names.append(lexical_binding.binding_identifier);
                        }
                    },
                    else => {},
                }
                return var_declared_names.toOwnedSlice();
            },
        }
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        switch (self) {
            .statement => |statement| return try statement.varScopedDeclarations(allocator),
            .declaration => |declaration| {
                var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
                switch (declaration.*) {
                    // HACK: Emit lexical declarations too while they're codegen'd as var decls
                    .lexical_declaration => |lexical_declaration| {
                        for (lexical_declaration.binding_list.items) |lexical_binding| {
                            try variable_declarations.append(.{
                                .variable_declaration = .{
                                    .binding_identifier = lexical_binding.binding_identifier,
                                    .initializer = lexical_binding.initializer,
                                },
                            });
                        }
                    },
                    else => {},
                }
                return variable_declarations.toOwnedSlice();
            },
        }
    }

    /// 8.2.9 Static Semantics: TopLevelLexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-toplevellexicallyscopeddeclarations
    pub fn topLevelLexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        var declarations = std.ArrayList(LexicallyScopedDeclaration).init(allocator);
        switch (self) {
            // StatementListItem : Statement
            .statement => {
                // 1. Return a new empty List.
            },

            // StatementListItem : Declaration
            .declaration => |declaration| switch (declaration.*) {
                // 1. If Declaration is Declaration : HoistableDeclaration , then
                .hoistable_declaration => {
                    // a. Return a new empty List.
                },

                // 2. Return « Declaration ».
                .class_declaration => |class_declaration| try declarations.append(.{ .class_declaration = class_declaration }),
                .lexical_declaration => |lexical_declaration| try declarations.append(.{ .lexical_declaration = lexical_declaration }),
            },
        }
        return declarations.toOwnedSlice();
    }

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return switch (self) {
            inline else => |node| node.analyze(query),
        };
    }
};

/// https://tc39.es/ecma262/#prod-LexicalDeclaration
pub const LexicalDeclaration = struct {
    const Self = @This();

    pub const Type = enum {
        let,
        @"const",
    };

    type: Type,
    binding_list: BindingList,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // LexicalDeclaration : LetOrConst BindingList ;
        // 1. Return the BoundNames of BindingList.
        return self.binding_list.boundNames(allocator);
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(self: Self) bool {
        // LexicalDeclaration : LetOrConst BindingList ;
        // 1. Return IsConstantDeclaration of LetOrConst.
        switch (self.type) {
            // LetOrConst : let
            .let => {
                // 1. Return false.
                return false;
            },
            // LetOrConst : const
            .@"const" => {
                // 1. Return true.
                return true;
            },
        }
    }
};

/// https://tc39.es/ecma262/#prod-BindingList
pub const BindingList = struct {
    const Self = @This();

    items: []const LexicalBinding,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // BindingList : BindingList , LexicalBinding
        // 1. Let names1 be the BoundNames of BindingList.
        // 2. Let names2 be the BoundNames of LexicalBinding.
        // 3. Return the list-concatenation of names1 and names2.
        var bound_names = std.ArrayList(Identifier).init(allocator);
        for (self.items) |lexical_binding| {
            try bound_names.appendSlice(try lexical_binding.boundNames(allocator));
        }
        return bound_names.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-LexicalBinding
pub const LexicalBinding = struct {
    const Self = @This();

    binding_identifier: Identifier,
    initializer: ?Expression,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // LexicalBinding : BindingIdentifier Initializer[opt]
        // 1. Return the BoundNames of BindingIdentifier.
        // LexicalBinding : BindingPattern Initializer
        // TODO: 1. Return the BoundNames of BindingPattern.
        var bound_names = std.ArrayList(Identifier).init(allocator);
        try bound_names.append(self.binding_identifier);
        return bound_names.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-VariableStatement
pub const VariableStatement = struct {
    variable_declaration_list: VariableDeclarationList,
};

/// https://tc39.es/ecma262/#prod-VariableDeclarationList
pub const VariableDeclarationList = struct {
    const Self = @This();

    items: []const VariableDeclaration,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // VariableStatement : var VariableDeclarationList ;
        // 1. Return BoundNames of VariableDeclarationList.
        return self.boundNames(allocator);
    }

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // VariableDeclarationList : VariableDeclarationList , VariableDeclaration
        // 1. Let names1 be BoundNames of VariableDeclarationList.
        // 2. Let names2 be BoundNames of VariableDeclaration.
        // 3. Return the list-concatenation of names1 and names2.
        // VariableDeclaration : BindingIdentifier Initializer[opt]
        // 1. Return the BoundNames of BindingIdentifier.
        var bound_names = try std.ArrayList(Identifier).initCapacity(
            allocator,
            self.items.len,
        );
        for (self.items) |variable_declaration| {
            bound_names.appendAssumeCapacity(variable_declaration.binding_identifier);
        }
        return bound_names.toOwnedSlice();
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // VariableDeclarationList : VariableDeclaration
        // 1. Return « VariableDeclaration ».
        // VariableDeclarationList : VariableDeclarationList , VariableDeclaration
        // 1. Let declarations1 be VarScopedDeclarations of VariableDeclarationList.
        // 2. Return the list-concatenation of declarations1 and « VariableDeclaration ».
        var variable_declarations = try std.ArrayList(VarScopedDeclaration).initCapacity(
            allocator,
            self.items.len,
        );
        for (self.items) |variable_declaration| {
            variable_declarations.appendAssumeCapacity(.{ .variable_declaration = variable_declaration });
        }
        return variable_declarations.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-VariableDeclaration
pub const VariableDeclaration = struct {
    binding_identifier: Identifier,
    initializer: ?Expression,
};

/// https://tc39.es/ecma262/#prod-BindingElement
pub const BindingElement = struct {
    const Self = @This();

    identifier: Identifier,
    initializer: ?Expression,
    // TODO: Binding patterns

    /// 15.1.2 Static Semantics: ContainsExpression
    /// https://tc39.es/ecma262/#sec-static-semantics-containsexpression
    pub fn containsExpression(self: Self) bool {
        // TODO: BindingElement : BindingPattern Initializer
        // 1. Return true.
        // SingleNameBinding : BindingIdentifier
        // 1. Return false.
        // SingleNameBinding : BindingIdentifier Initializer
        // 1. Return true.
        return self.initializer != null;
    }

    /// 15.1.3 Static Semantics: IsSimpleParameterList
    /// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
    pub fn isSimpleParameterList(self: Self) bool {
        // TODO: BindingElement : BindingPattern
        // 1. Return false.
        // TODO: BindingElement : BindingPattern Initializer
        // 1. Return false.
        // SingleNameBinding : BindingIdentifier
        // 1. Return true.
        // SingleNameBinding : BindingIdentifier Initializer
        // 1. Return false.
        return self.initializer == null;
    }
};

/// https://tc39.es/ecma262/#prod-BindingRestElement
pub const BindingRestElement = struct {
    const Self = @This();

    identifier: Identifier,
    // TODO: Binding patterns

    /// 15.1.2 Static Semantics: ContainsExpression
    /// https://tc39.es/ecma262/#sec-static-semantics-containsexpression
    pub fn containsExpression(_: Self) bool {
        // BindingRestElement : ... BindingIdentifier
        // 1. Return false.
        return false;

        // TODO: BindingRestElement : ... BindingPattern
        // 1. Return ContainsExpression of BindingPattern.
    }
};

/// https://tc39.es/ecma262/#prod-ExpressionStatement
pub const ExpressionStatement = struct {
    const Self = @This();

    expression: Expression,

    pub fn analyze(self: Self, query: AnalyzeQuery) bool {
        return self.expression.analyze(query);
    }
};

/// https://tc39.es/ecma262/#prod-IfStatement
pub const IfStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,
    alternate_statement: ?*Statement,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // IfStatement : if ( Expression ) Statement else Statement
        // 1. Let names1 be VarDeclaredNames of the first Statement.
        // 2. Let names2 be VarDeclaredNames of the second Statement.
        // 3. Return the list-concatenation of names1 and names2.
        if (self.alternate_statement) |alternate_statement| {
            var var_declared_names = std.ArrayList(Identifier).init(allocator);
            try var_declared_names.appendSlice(try self.consequent_statement.varDeclaredNames(allocator));
            try var_declared_names.appendSlice(try alternate_statement.varDeclaredNames(allocator));
            return var_declared_names.toOwnedSlice();
        }

        // IfStatement : if ( Expression ) Statement
        // 1. Return the VarDeclaredNames of Statement.
        return self.consequent_statement.varDeclaredNames(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // IfStatement : if ( Expression ) Statement else Statement
        // 1. Let declarations1 be VarScopedDeclarations of the first Statement.
        // 2. Let declarations2 be VarScopedDeclarations of the second Statement.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        if (self.alternate_statement) |alternate_statement| {
            var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
            try variable_declarations.appendSlice(try self.consequent_statement.varScopedDeclarations(allocator));
            try variable_declarations.appendSlice(try alternate_statement.varScopedDeclarations(allocator));
            return variable_declarations.toOwnedSlice();
        }

        // IfStatement : if ( Expression ) Statement
        // 1. Return the VarScopedDeclarations of Statement.
        return self.consequent_statement.varScopedDeclarations(allocator);
    }
};

/// https://tc39.es/ecma262/#prod-IterationStatement
pub const IterationStatement = union(enum) {
    do_while_statement: DoWhileStatement,
    while_statement: WhileStatement,
    for_statement: ForStatement,
    for_in_of_statement: ForInOfStatement,
};

/// https://tc39.es/ecma262/#prod-DoWhileStatement
pub const DoWhileStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // DoWhileStatement : do Statement while ( Expression ) ;
        // 1. Return the VarDeclaredNames of Statement.
        return self.consequent_statement.varDeclaredNames(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // DoWhileStatement : do Statement while ( Expression ) ;
        // 1. Return the VarScopedDeclarations of Statement.
        return self.consequent_statement.varScopedDeclarations(allocator);
    }
};

/// https://tc39.es/ecma262/#prod-WhileStatement
pub const WhileStatement = struct {
    const Self = @This();

    test_expression: Expression,
    consequent_statement: *Statement,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // WhileStatement : while ( Expression ) Statement
        // 1. Return the VarDeclaredNames of Statement.
        return self.consequent_statement.varDeclaredNames(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // WhileStatement : while ( Expression ) Statement
        // 1. Return the VarScopedDeclarations of Statement.
        return self.consequent_statement.varScopedDeclarations(allocator);
    }
};

/// https://tc39.es/ecma262/#prod-ForStatement
pub const ForStatement = struct {
    const Self = @This();

    pub const Initializer = union(enum) {
        expression: Expression,
        variable_statement: VariableStatement,
        lexical_declaration: LexicalDeclaration,
    };

    initializer: ?Initializer,
    test_expression: ?Expression,
    increment_expression: ?Expression,
    consequent_statement: *Statement,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // ForStatement : for ( Expression[opt] ; Expression[opt] ; Expression[opt] ) Statement
        // 1. Return the VarDeclaredNames of Statement.
        // ForStatement : for ( var VariableDeclarationList ; Expression[opt] ; Expression[opt] ) Statement
        // 1. Let names1 be BoundNames of VariableDeclarationList.
        // 2. Let names2 be VarDeclaredNames of Statement.
        // 3. Return the list-concatenation of names1 and names2.
        if (self.initializer) |initializer| switch (initializer) {
            .variable_statement => {
                var var_declared_names = std.ArrayList(Identifier).init(allocator);
                try var_declared_names.appendSlice(try self.initializer.?.variable_statement.variable_declaration_list.boundNames(allocator));
                try var_declared_names.appendSlice(try self.consequent_statement.varDeclaredNames(allocator));
                return var_declared_names.toOwnedSlice();
            },
            // HACK: Emit lexical declarations too while they're codegen'd as var decls
            .lexical_declaration => |lexical_declaration| {
                var var_declared_names = std.ArrayList(Identifier).init(allocator);
                for (lexical_declaration.binding_list.items) |lexical_binding| {
                    try var_declared_names.append(lexical_binding.binding_identifier);
                }
                try var_declared_names.appendSlice(try self.consequent_statement.varDeclaredNames(allocator));
                return var_declared_names.toOwnedSlice();
            },
            else => {},
        };

        // ForStatement : for ( LexicalDeclaration Expression[opt] ; Expression[opt] ) Statement
        // 1. Return the VarDeclaredNames of Statement.
        return self.consequent_statement.varDeclaredNames(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // ForStatement : for ( var VariableDeclarationList ; Expression[opt] ; Expression[opt] ) Statement
        // 1. Let declarations1 be VarScopedDeclarations of VariableDeclarationList.
        // 2. Let declarations2 be VarScopedDeclarations of Statement.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        if (self.initializer) |initializer| switch (initializer) {
            .variable_statement => {
                var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
                try variable_declarations.appendSlice(try self.initializer.?.variable_statement.variable_declaration_list.varScopedDeclarations(allocator));
                try variable_declarations.appendSlice(try self.consequent_statement.varScopedDeclarations(allocator));
                return variable_declarations.toOwnedSlice();
            },
            // HACK: Emit lexical declarations too while they're codegen'd as var decls
            .lexical_declaration => |lexical_declaration| {
                var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
                for (lexical_declaration.binding_list.items) |lexical_binding| {
                    try variable_declarations.append(.{
                        .variable_declaration = .{
                            .binding_identifier = lexical_binding.binding_identifier,
                            .initializer = lexical_binding.initializer,
                        },
                    });
                }
                try variable_declarations.appendSlice(try self.consequent_statement.varScopedDeclarations(allocator));
                return variable_declarations.toOwnedSlice();
            },
            else => {},
        };

        // ForStatement : for ( Expression[opt] ; Expression[opt] ; Expression[opt] ) Statement
        // 1. Return the VarScopedDeclarations of Statement.
        // ForStatement : for ( LexicalDeclaration Expression[opt] ; Expression[opt] ) Statement
        // 1. Return the VarScopedDeclarations of Statement.
        return self.consequent_statement.varScopedDeclarations(allocator);
    }
};

/// https://tc39.es/ecma262/#prod-ForInOfStatement
pub const ForInOfStatement = struct {
    const Self = @This();

    pub const Type = enum {
        in,
        of,
        async_of,
    };

    pub const Initializer = union(enum) {
        expression: Expression,
        for_binding: Identifier,
        for_declaration: LexicalDeclaration,
    };

    type: Type,
    initializer: Initializer,
    expression: Expression,
    consequent_statement: *Statement,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // ForInOfStatement :
        //     for ( LeftHandSideExpression in Expression ) Statement
        //     for ( ForDeclaration in Expression ) Statement
        //     for ( LeftHandSideExpression of AssignmentExpression ) Statement
        //     for ( ForDeclaration of AssignmentExpression ) Statement
        //     for await ( LeftHandSideExpression of AssignmentExpression ) Statement
        //     for await ( ForDeclaration of AssignmentExpression ) Statement
        if (self.initializer != .for_binding) {
            var var_declared_names = std.ArrayList(Identifier).init(allocator);
            switch (self.initializer) {
                // HACK: Emit lexical declarations too while they're codegen'd as var decls
                .for_declaration => |lexical_declaration| {
                    const lexical_binding = lexical_declaration.binding_list.items[0];
                    try var_declared_names.append(lexical_binding.binding_identifier);
                },
                else => {},
            }

            // 1. Return the VarDeclaredNames of Statement.
            try var_declared_names.appendSlice(try self.consequent_statement.varDeclaredNames(allocator));
            return var_declared_names.toOwnedSlice();
        }
        // ForInOfStatement :
        //     for ( var ForBinding in Expression ) Statement
        //     for ( var ForBinding of AssignmentExpression ) Statement
        //     for await ( var ForBinding of AssignmentExpression ) Statement
        else {
            // 1. Let names1 be the BoundNames of ForBinding.
            // 2. Let names2 be the VarDeclaredNames of Statement.
            // 3. Return the list-concatenation of names1 and names2.
            var var_declared_names = std.ArrayList(Identifier).init(allocator);
            try var_declared_names.append(self.initializer.for_binding);
            try var_declared_names.appendSlice(try self.consequent_statement.varDeclaredNames(allocator));
            return var_declared_names.toOwnedSlice();
        }
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // ForInOfStatement :
        //     for ( LeftHandSideExpression in Expression ) Statement
        //     for ( ForDeclaration in Expression ) Statement
        //     for ( LeftHandSideExpression of AssignmentExpression ) Statement
        //     for ( ForDeclaration of AssignmentExpression ) Statement
        //     for await ( LeftHandSideExpression of AssignmentExpression ) Statement
        //     for await ( ForDeclaration of AssignmentExpression ) Statement
        if (self.initializer != .for_binding) {
            var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
            switch (self.initializer) {
                // HACK: Emit lexical declarations too while they're codegen'd as var decls
                .for_declaration => |lexical_declaration| {
                    const lexical_binding = lexical_declaration.binding_list.items[0];
                    try variable_declarations.append(.{
                        .variable_declaration = .{
                            .binding_identifier = lexical_binding.binding_identifier,
                            .initializer = lexical_binding.initializer,
                        },
                    });
                },
                else => {},
            }

            // 1. Return the VarScopedDeclarations of Statement.
            try variable_declarations.appendSlice(try self.consequent_statement.varScopedDeclarations(allocator));
            return variable_declarations.toOwnedSlice();
        }
        // ForInOfStatement :
        //     for ( var ForBinding in Expression ) Statement
        //     for ( var ForBinding of AssignmentExpression ) Statement
        //     for await ( var ForBinding of AssignmentExpression ) Statement
        else {
            // 1. Let declarations1 be « ForBinding ».
            // 2. Let declarations2 be VarScopedDeclarations of Statement.
            // 3. Return the list-concatenation of declarations1 and declarations2.
            var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
            try variable_declarations.append(.{
                .variable_declaration = .{ .binding_identifier = self.initializer.for_binding, .initializer = null },
            });
            try variable_declarations.appendSlice(try self.consequent_statement.varScopedDeclarations(allocator));
            return variable_declarations.toOwnedSlice();
        }
    }
};

/// https://tc39.es/ecma262/#prod-ContinueStatement
pub const ContinueStatement = struct {
    label: ?Identifier,
};

/// https://tc39.es/ecma262/#prod-BreakStatement
pub const BreakStatement = struct {
    label: ?Identifier,
};

/// https://tc39.es/ecma262/#prod-ReturnStatement
pub const ReturnStatement = struct {
    expression: ?Expression,
};

/// https://tc39.es/ecma262/#prod-ThrowStatement
pub const ThrowStatement = struct {
    expression: Expression,
};

/// https://tc39.es/ecma262/#prod-TryStatement
pub const TryStatement = struct {
    const Self = @This();

    try_block: Block,
    catch_parameter: ?Identifier, // TODO: Binding patterns
    catch_block: ?Block,
    finally_block: ?Block,

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // TryStatement : try Block Catch
        // 1. Let names1 be VarDeclaredNames of Block.
        // 2. Let names2 be VarDeclaredNames of Catch.
        // 3. Return the list-concatenation of names1 and names2.
        // TryStatement : try Block Finally
        // 1. Let names1 be VarDeclaredNames of Block.
        // 2. Let names2 be VarDeclaredNames of Finally.
        // 3. Return the list-concatenation of names1 and names2.
        // TryStatement : try Block Catch Finally
        // 1. Let names1 be VarDeclaredNames of Block.
        // 2. Let names2 be VarDeclaredNames of Catch.
        // 3. Let names3 be VarDeclaredNames of Finally.
        // 4. Return the list-concatenation of names1, names2, and names3.
        // Catch : catch ( CatchParameter ) Block
        // 1. Return the VarDeclaredNames of Block.
        var var_declared_names = std.ArrayList(Identifier).init(allocator);
        try var_declared_names.appendSlice(try self.try_block.statement_list.varDeclaredNames(allocator));
        if (self.catch_block) |catch_block| {
            try var_declared_names.appendSlice(try catch_block.statement_list.varDeclaredNames(allocator));
        }
        if (self.finally_block) |finally_block| {
            try var_declared_names.appendSlice(try finally_block.statement_list.varDeclaredNames(allocator));
        }
        return var_declared_names.toOwnedSlice();
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // TryStatement : try Block Catch
        // 1. Let declarations1 be VarScopedDeclarations of Block.
        // 2. Let declarations2 be VarScopedDeclarations of Catch.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        // TryStatement : try Block Finally
        // 1. Let declarations1 be VarScopedDeclarations of Block.
        // 2. Let declarations2 be VarScopedDeclarations of Finally.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        // TryStatement : try Block Catch Finally
        // 1. Let declarations1 be VarScopedDeclarations of Block.
        // 2. Let declarations2 be VarScopedDeclarations of Catch.
        // 3. Let declarations3 be VarScopedDeclarations of Finally.
        // 4. Return the list-concatenation of declarations1, declarations2, and declarations3.
        // Catch : catch ( CatchParameter ) Block
        // 1. Return the VarScopedDeclarations of Block.
        var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
        try variable_declarations.appendSlice(try self.try_block.statement_list.varScopedDeclarations(allocator));
        if (self.catch_block) |catch_block| {
            try variable_declarations.appendSlice(try catch_block.statement_list.varScopedDeclarations(allocator));
        }
        if (self.finally_block) |finally_block| {
            try variable_declarations.appendSlice(try finally_block.statement_list.varScopedDeclarations(allocator));
        }
        return variable_declarations.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameters = struct {
    const Self = @This();

    pub const Item = union(enum) {
        formal_parameter: FormalParameter,
        function_rest_parameter: FunctionRestParameter,
    };

    items: []const Item,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(self: Self, allocator: Allocator) Allocator.Error![]const []const u8 {
        var bound_names = try std.ArrayList([]const u8).initCapacity(allocator, self.items.len);
        // FormalParameterList : FormalParameterList , FormalParameter
        // 1. Let names1 be BoundNames of FormalParameterList.
        // 2. Let names2 be BoundNames of FormalParameter.
        // 3. Return the list-concatenation of names1 and names2.
        for (self.items) |item| {
            // BindingElement : BindingPattern Initializer[opt]
            // 1. Return the BoundNames of BindingPattern.
            // SingleNameBinding : BindingIdentifier Initializer[opt]
            // 1. Return the BoundNames of BindingIdentifier.
            const name = switch (item) {
                .formal_parameter => |formal_parameter| formal_parameter.binding_element.identifier,
                .function_rest_parameter => |function_rest_parameter| function_rest_parameter.binding_rest_element.identifier,
            };
            bound_names.appendAssumeCapacity(name);
        }
        return bound_names.toOwnedSlice();
    }

    /// 15.1.2 Static Semantics: ContainsExpression
    /// https://tc39.es/ecma262/#sec-static-semantics-containsexpression
    pub fn containsExpression(self: Self) bool {
        // FormalParameters : [empty]
        // 1. Return false.
        // FormalParameters : FormalParameterList , FunctionRestParameter
        // 1. If ContainsExpression of FormalParameterList is true, return true.
        // 2. Return ContainsExpression of FunctionRestParameter.
        // FormalParameterList : FormalParameterList , FormalParameter
        // 1. If ContainsExpression of FormalParameterList is true, return true.
        // 2. Return ContainsExpression of FormalParameter.
        for (self.items) |item| switch (item) {
            .formal_parameter => |formal_parameter| {
                if (formal_parameter.binding_element.containsExpression()) return true;
            },
            .function_rest_parameter => |function_rest_parameter| {
                if (function_rest_parameter.binding_rest_element.containsExpression()) return true;
            },
        };
        return false;
    }

    /// 15.1.3 Static Semantics: IsSimpleParameterList
    /// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
    pub fn isSimpleParameterList(self: Self) bool {
        // FormalParameters : [empty]
        // 1. Return true.
        // FormalParameters : FunctionRestParameter
        // 1. Return false.
        // FormalParameters : FormalParameterList , FunctionRestParameter
        // 1. Return false.
        // FormalParameterList : FormalParameterList , FormalParameter
        // 1. If IsSimpleParameterList of FormalParameterList is false, return false.
        // 2. Return IsSimpleParameterList of FormalParameter.
        for (self.items) |item| switch (item) {
            .formal_parameter => |formal_parameter| {
                //  FormalParameter : BindingElement
                // 1. Return IsSimpleParameterList of BindingElement.
                if (!formal_parameter.binding_element.isSimpleParameterList()) return false;
            },
            .function_rest_parameter => return false,
        };
        return true;
    }

    /// 15.1.5 Static Semantics: ExpectedArgumentCount
    /// https://tc39.es/ecma262/#sec-static-semantics-expectedargumentcount
    pub fn expectedArgumentCount(self: Self) usize {
        if (self.items.len != 0 and self.items[self.items.len - 1] == .function_rest_parameter)
            return self.items.len - 1
        else
            return self.items.len;
    }
};

/// https://tc39.es/ecma262/#prod-FunctionRestParameter
pub const FunctionRestParameter = struct {
    binding_rest_element: BindingRestElement,
};

/// https://tc39.es/ecma262/#prod-FormalParameter
pub const FormalParameter = struct {
    binding_element: BindingElement,
};

/// https://tc39.es/ecma262/#prod-FunctionDeclaration
pub const FunctionDeclaration = struct {
    const Self = @This();

    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
        // 1. Return the BoundNames of BindingIdentifier.
        // FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
        // 1. Return « "*default*" ».
        var bound_names = try std.ArrayList(Identifier).initCapacity(allocator, 1);
        bound_names.appendAssumeCapacity(self.identifier orelse "*default*");
        return bound_names.toOwnedSlice();
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(_: Self) bool {
        // FunctionDeclaration :
        //     function BindingIdentifier ( FormalParameters ) { FunctionBody }
        //     function ( FormalParameters ) { FunctionBody }
        // 1. Return false.
        return false;
    }

    /// 15.2.4 Runtime Semantics: InstantiateOrdinaryFunctionObject
    /// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionobject
    pub fn instantiateOrdinaryFunctionObject(
        self: Self,
        agent: *Agent,
        env: Environment,
        private_env: ?*PrivateEnvironment,
    ) Allocator.Error!Object {
        const realm = agent.currentRealm();

        // FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
        if (self.identifier) |identifier| {
            // 1. Let name be StringValue of BindingIdentifier.
            const name = identifier;

            // 2. Let sourceText be the source text matched by FunctionDeclaration.
            const source_text = self.source_text;

            // 3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
            //    FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%Function.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 4. Perform SetFunctionName(F, name).
            try setFunctionName(function, PropertyKey.from(name), null);

            // 5. Perform MakeConstructor(F).
            try makeConstructor(function, .{});

            // 6. Return F.
            return function;
        }
        // FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
        // NOTE: An anonymous FunctionDeclaration can only occur as part of an export default
        // declaration, and its function code is therefore always strict mode code.
        else {
            // 1. Let sourceText be the source text matched by FunctionDeclaration.
            const source_text = self.source_text;

            // 2. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
            //    FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%Function.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 3. Perform SetFunctionName(F, "default").
            try setFunctionName(function, PropertyKey.from("default"), null);

            // 4. Perform MakeConstructor(F).
            try makeConstructor(function, .{});

            // 5. Return F.
            return function;
        }
    }
};

/// https://tc39.es/ecma262/#prod-FunctionExpression
pub const FunctionExpression = struct {
    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-FunctionBody
pub const FunctionBody = struct {
    const Self = @This();

    pub const Type = enum {
        normal,
        generator,
        @"async",
        async_generator,
    };

    type: Type,
    statement_list: StatementList,
    strict: ?bool = null, // Unassigned until bytecode generation

    /// 8.2.5 Static Semantics: LexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations
    pub fn lexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        // FunctionStatementList : [empty]
        // 1. Return a new empty List.
        // FunctionStatementList : StatementList
        // 1. Return the TopLevelLexicallyScopedDeclarations of StatementList.
        return self.statement_list.topLevelLexicallyScopedDeclarations(allocator);
    }

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // FunctionStatementList : [empty]
        // 1. Return a new empty List.
        // FunctionStatementList : StatementList
        // 1. Return TopLevelVarDeclaredNames of StatementList.
        return self.statement_list.varDeclaredNames(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // FunctionStatementList : [empty]
        // 1. Return a new empty List.
        // FunctionStatementList : StatementList
        // 1. Return the TopLevelVarScopedDeclarations of StatementList.
        return self.statement_list.topLevelVarScopedDeclarations(allocator);
    }

    /// 15.2.2 Static Semantics: FunctionBodyContainsUseStrict
    /// https://tc39.es/ecma262/#sec-static-semantics-functionbodycontainsusestrict
    pub fn functionBodyContainsUseStrict(self: Self) bool {
        // 1. If the Directive Prologue of FunctionBody contains a Use Strict Directive, return
        //    true; otherwise, return false.
        return self.statement_list.containsDirective("use strict");
    }
};

/// https://tc39.es/ecma262/#prod-ArrowFunction
pub const ArrowFunction = struct {
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-MethodDefinition
pub const MethodDefinition = struct {
    pub const Type = enum {
        method,
        get,
        set,
        generator,
        @"async",
        async_generator,
    };

    pub const Method = union(Type) {
        method: FunctionExpression,
        get: FunctionExpression,
        set: FunctionExpression,
        generator: GeneratorExpression,
        @"async": AsyncFunctionExpression,
        async_generator: AsyncGeneratorExpression,
    };

    class_element_name: ClassElementName,
    method: Method,
};

/// https://tc39.es/ecma262/#prod-GeneratorDeclaration
pub const GeneratorDeclaration = struct {
    const Self = @This();

    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // GeneratorDeclaration : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        // 1. Return the BoundNames of BindingIdentifier.
        // GeneratorDeclaration : function * ( FormalParameters ) { GeneratorBody }
        // 1. Return « "*default*" ».
        var bound_names = try std.ArrayList(Identifier).initCapacity(allocator, 1);
        bound_names.appendAssumeCapacity(self.identifier orelse "*default*");
        return bound_names.toOwnedSlice();
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(_: Self) bool {
        // GeneratorDeclaration :
        //     function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        //     function * ( FormalParameters ) { GeneratorBody }
        // 1. Return false.
        return false;
    }

    /// 15.5.3 Runtime Semantics: InstantiateGeneratorFunctionObject
    /// https://tc39.es/ecma262/#sec-runtime-semantics-instantiategeneratorfunctionobject
    pub fn instantiateGeneratorFunctionObject(
        self: Self,
        agent: *Agent,
        env: Environment,
        private_env: ?*PrivateEnvironment,
    ) Allocator.Error!Object {
        const realm = agent.currentRealm();

        // GeneratorDeclaration : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        if (self.identifier) |identifier| {
            // 1. Let name be StringValue of BindingIdentifier.
            const name = identifier;

            // 2. Let sourceText be the source text matched by GeneratorDeclaration.
            const source_text = self.source_text;

            // 3. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
            //    FormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 4. Perform SetFunctionName(F, name).
            try setFunctionName(function, PropertyKey.from(name), null);

            // 5. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%GeneratorPrototype%"(),
            );

            // 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);

            // 7. Return F.
            return function;
        }
        // GeneratorDeclaration : function * ( FormalParameters ) { GeneratorBody }
        // NOTE: An anonymous GeneratorDeclaration can only occur as part of an export default
        // declaration, and its function code is therefore always strict mode code.
        else {
            // 1. Let sourceText be the source text matched by GeneratorDeclaration.
            const source_text = self.source_text;

            // 2. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
            //    FormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 3. Perform SetFunctionName(F, "default").
            try setFunctionName(function, PropertyKey.from("default"), null);

            // 4. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%GeneratorPrototype%"(),
            );

            // 5. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);

            // 6. Return F.
            return function;
        }
    }
};

/// https://tc39.es/ecma262/#prod-GeneratorExpression
pub const GeneratorExpression = struct {
    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-AsyncGeneratorDeclaration
pub const AsyncGeneratorDeclaration = struct {
    const Self = @This();

    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // AsyncGeneratorDeclaration : async function * BindingIdentifier ( FormalParameters ) { AsyncGeneratorBody }
        // 1. Return the BoundNames of BindingIdentifier.
        // AsyncGeneratorDeclaration : async function * ( FormalParameters ) { AsyncGeneratorBody }
        // 1. Return « "*default*" ».
        var bound_names = try std.ArrayList(Identifier).initCapacity(allocator, 1);
        bound_names.appendAssumeCapacity(self.identifier orelse "*default*");
        return bound_names.toOwnedSlice();
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(_: Self) bool {
        // AsyncGeneratorDeclaration :
        //     async function * BindingIdentifier ( FormalParameters ) { AsyncGeneratorBody }
        //     async function * ( FormalParameters ) { AsyncGeneratorBody }
        // 1. Return false.
        return false;
    }

    /// 15.6.3 Runtime Semantics: InstantiateAsyncGeneratorFunctionObject
    /// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncgeneratorfunctionobject
    pub fn instantiateAsyncGeneratorFunctionObject(
        self: Self,
        agent: *Agent,
        env: Environment,
        private_env: ?*PrivateEnvironment,
    ) Allocator.Error!Object {
        const realm = agent.currentRealm();

        // AsyncGeneratorDeclaration : async function * BindingIdentifier ( FormalParameters ) { AsyncGeneratorBody }
        if (self.identifier) |identifier| {
            // 1. Let name be StringValue of BindingIdentifier.
            const name = identifier;

            // 2. Let sourceText be the source text matched by AsyncGeneratorDeclaration.
            const source_text = self.source_text;

            // 3. Let F be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
            //    FormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 4. Perform SetFunctionName(F, name).
            try setFunctionName(function, PropertyKey.from(name), null);

            // 5. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorFunction.prototype.prototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
            );

            // 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);

            // 7. Return F.
            return function;
        }
        // AsyncGeneratorDeclaration : async function * ( FormalParameters ) { AsyncGeneratorBody }
        // NOTE: An anonymous AsyncGeneratorDeclaration can only occur as part of an `export default` declaration.
        else {
            // 1. Let sourceText be the source text matched by AsyncGeneratorDeclaration.
            const source_text = self.source_text;

            // 2. Let F be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
            //    FormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 3. Perform SetFunctionName(F, "default").
            try setFunctionName(function, PropertyKey.from("default"), null);

            // 4. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorFunction.prototype.prototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
            );

            // 5. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);

            // 6. Return F.
            return function;
        }
    }
};

/// https://tc39.es/ecma262/#prod-AsyncGeneratorExpression
pub const AsyncGeneratorExpression = struct {
    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-ClassDeclaration
pub const ClassDeclaration = struct {
    const Self = @This();

    identifier: ?Identifier,
    class_tail: ClassTail,
    source_text: []const u8,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // ClassDeclaration : class BindingIdentifier ClassTail
        // 1. Return the BoundNames of BindingIdentifier.
        // ClassDeclaration : class ClassTail
        // 1. Return « "*default*" ».
        var bound_names = try std.ArrayList(Identifier).initCapacity(allocator, 1);
        bound_names.appendAssumeCapacity(self.identifier orelse "*default*");
        return bound_names.toOwnedSlice();
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(_: Self) bool {
        // ClassDeclaration :
        //     class BindingIdentifier ClassTail
        //     class ClassTail
        // 1. Return false.
        return false;
    }
};

/// https://tc39.es/ecma262/#prod-ClassExpression
pub const ClassExpression = struct {
    identifier: ?Identifier,
    class_tail: ClassTail,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-ClassTail
pub const ClassTail = struct {
    class_heritage: ?*Expression,
    class_body: ClassBody,
};

/// https://tc39.es/ecma262/#prod-ClassBody
pub const ClassBody = struct {
    const Self = @This();

    class_element_list: ClassElementList,

    /// 15.7.3 Static Semantics: ConstructorMethod
    /// https://tc39.es/ecma262/#sec-static-semantics-constructormethod
    pub fn constructorMethod(self: Self) ?MethodDefinition {
        // ClassElementList : ClassElement
        // 1. If ClassElementKind of ClassElement is constructor-method, return ClassElement.
        // 2. Return empty.
        // ClassElementList : ClassElementList ClassElement
        // 1. Let head be ConstructorMethod of ClassElementList.
        // 2. If head is not empty, return head.
        // 3. If ClassElementKind of ClassElement is constructor-method, return ClassElement.
        // 4. Return empty.
        for (self.class_element_list.items) |class_element| {
            if (class_element.classElementKind() == .constructor_method) {
                return class_element.method_definition;
            }
        }
        return null;
    }

    /// 15.7.5 Static Semantics: NonConstructorElements
    /// https://tc39.es/ecma262/#sec-static-semantics-nonconstructorelements
    pub fn nonConstructorElements(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const ClassElement {
        // ClassElementList : ClassElement
        // 1. If ClassElementKind of ClassElement is non-constructor-method, then
        //     a. Return « ClassElement ».
        // 2. Return a new empty List.
        // ClassElementList : ClassElementList ClassElement
        // 1. Let list be NonConstructorElements of ClassElementList.
        // 2. If ClassElementKind of ClassElement is non-constructor-method, then
        //     a. Append ClassElement to the end of list.
        // 3. Return list.
        var class_elements = try std.ArrayList(ClassElement).initCapacity(
            allocator,
            self.class_element_list.items.len,
        );
        for (self.class_element_list.items) |class_element| {
            if (class_element.classElementKind() == .non_constructor_method) {
                class_elements.appendAssumeCapacity(class_element);
            }
        }
        return class_elements.toOwnedSlice();
    }

    /// 15.7.8 Static Semantics: PrivateBoundIdentifiers
    /// https://tc39.es/ecma262/#sec-static-semantics-privateboundidentifiers
    pub fn privateBoundIdentifiers(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const PrivateIdentifier {
        // ClassElementList : ClassElementList ClassElement
        // 1. Let names1 be PrivateBoundIdentifiers of ClassElementList.
        // 2. Let names2 be PrivateBoundIdentifiers of ClassElement.
        // 3. Return the list-concatenation of names1 and names2.
        var private_bound_identifiers = std.ArrayList(PrivateIdentifier).init(allocator);
        for (self.class_element_list.items) |class_element| {
            switch (class_element) {
                // ClassElement :
                //     ClassStaticBlock
                //     ;
                .class_static_block, .empty_statement => {
                    // 1. Return a new empty List.
                },

                // MethodDefinition :
                //     ClassElementName ( UniqueFormalParameters ) { FunctionBody }
                //     get ClassElementName ( ) { FunctionBody }
                //     set ClassElementName ( PropertySetParameterList ) { FunctionBody }
                // GeneratorMethod :
                //     * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
                // AsyncMethod :
                //     async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
                // AsyncGeneratorMethod :
                //     async * ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
                .method_definition, .static_method_definition => |method_definition| {
                    // 1. Return PrivateBoundIdentifiers of ClassElementName.
                    switch (method_definition.class_element_name) {
                        // ClassElementName : PrivateIdentifier
                        .private_identifier => |private_identifier| {
                            // 1. Return a List whose sole element is the StringValue of PrivateIdentifier.
                            try private_bound_identifiers.append(private_identifier);
                        },
                        // ClassElementName : PropertyName
                        .property_name => {
                            // 1. Return a new empty List.
                        },
                    }
                },

                // FieldDefinition : ClassElementName Initializer[opt]
                .field_definition, .static_field_definition => |field_definition| {
                    // 1. Return PrivateBoundIdentifiers of ClassElementName.
                    switch (field_definition.class_element_name) {
                        // ClassElementName : PrivateIdentifier
                        .private_identifier => |private_identifier| {
                            // 1. Return a List whose sole element is the StringValue of PrivateIdentifier.
                            try private_bound_identifiers.append(private_identifier);
                        },
                        // ClassElementName : PropertyName
                        .property_name => {
                            // 1. Return a new empty List.
                        },
                    }
                },
            }
        }
        return private_bound_identifiers.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-ClassElementList
pub const ClassElementList = struct {
    items: []const ClassElement,
};

/// https://tc39.es/ecma262/#prod-ClassElement
pub const ClassElement = union(enum) {
    const Self = @This();

    pub const Kind = enum {
        constructor_method,
        non_constructor_method,
        empty,
    };

    empty_statement,
    method_definition: MethodDefinition,
    static_method_definition: MethodDefinition,
    field_definition: FieldDefinition,
    static_field_definition: FieldDefinition,
    class_static_block: ClassStaticBlock,

    /// 15.7.2 Static Semantics: ClassElementKind
    /// https://tc39.es/ecma262/#sec-static-semantics-classelementkind
    pub fn classElementKind(self: Self) Kind {
        switch (self) {
            // ClassElement : MethodDefinition
            .method_definition => |method_definition| {
                // 1. If PropName of MethodDefinition is "constructor", return constructor-method.
                if (method_definition.class_element_name == .property_name and
                    method_definition.class_element_name.property_name == .literal_property_name and
                    method_definition.class_element_name.property_name.literal_property_name == .identifier and
                    std.mem.eql(u8, method_definition.class_element_name.property_name.literal_property_name.identifier, "constructor"))
                {
                    return .constructor_method;
                }

                // 2. Return non-constructor-method.
                return .non_constructor_method;
            },

            // ClassElement :
            //     static MethodDefinition
            //     FieldDefinition ;
            //     static FieldDefinition ;
            // ClassElement : ClassStaticBlock
            .static_method_definition,
            .field_definition,
            .static_field_definition,
            .class_static_block,
            => {
                // 1. Return non-constructor-method.
                return .non_constructor_method;
            },

            // ClassElement : ;
            .empty_statement => {
                // 1. Return empty.
                return .empty;
            },
        }
    }

    /// 15.7.4 Static Semantics: IsStatic
    /// https://tc39.es/ecma262/#sec-static-semantics-isstatic
    pub fn isStatic(self: Self) bool {
        switch (self) {
            // ClassElement : MethodDefinition
            // ClassElement : FieldDefinition ;
            // ClassElement : ;
            .method_definition,
            .field_definition,
            .empty_statement,
            => {
                // 1. Return false.
                return false;
            },

            // ClassElement : static MethodDefinition
            // ClassElement : static FieldDefinition ;
            // ClassElement : ClassStaticBlock
            .static_method_definition,
            .static_field_definition,
            .class_static_block,
            => {
                // 1. Return true.
                return true;
            },
        }
    }
};

/// https://tc39.es/ecma262/#prod-FieldDefinition
pub const FieldDefinition = struct {
    class_element_name: ClassElementName,
    initializer: ?Expression,
};

/// https://tc39.es/ecma262/#prod-ClassElementName
pub const ClassElementName = union(enum) {
    property_name: PropertyName,
    private_identifier: PrivateIdentifier,
};

/// https://tc39.es/ecma262/#prod-ClassStaticBlock
pub const ClassStaticBlock = struct {
    statement_list: StatementList,
};

/// https://tc39.es/ecma262/#prod-AsyncFunctionDeclaration
pub const AsyncFunctionDeclaration = struct {
    const Self = @This();

    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,

    /// 8.2.1 Static Semantics: BoundNames
    /// https://tc39.es/ecma262/#sec-static-semantics-boundnames
    pub fn boundNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // AsyncFunctionDeclaration : async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
        // 1. Return the BoundNames of BindingIdentifier.
        // AsyncFunctionDeclaration : async function ( FormalParameters ) { AsyncFunctionBody }
        // 1. Return « "*default*" ».
        var bound_names = try std.ArrayList(Identifier).initCapacity(allocator, 1);
        bound_names.appendAssumeCapacity(self.identifier orelse "*default*");
        return bound_names.toOwnedSlice();
    }

    /// 8.2.3 Static Semantics: IsConstantDeclaration
    /// https://tc39.es/ecma262/#sec-static-semantics-isconstantdeclaration
    pub fn isConstantDeclaration(_: Self) bool {
        // AsyncFunctionDeclaration :
        //     async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
        //     async function ( FormalParameters ) { AsyncFunctionBody }
        // 1. Return false.
        return false;
    }

    /// 15.8.2 Runtime Semantics: InstantiateAsyncFunctionObject
    /// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncfunctionobject
    pub fn instantiateAsyncFunctionObject(
        self: Self,
        agent: *Agent,
        env: Environment,
        private_env: ?*PrivateEnvironment,
    ) Allocator.Error!Object {
        const realm = agent.currentRealm();

        // AsyncFunctionDeclaration : async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
        if (self.identifier) |identifier| {
            // 1. Let name be StringValue of BindingIdentifier.
            const name = identifier;

            // 2. Let sourceText be the source text matched by AsyncFunctionDeclaration.
            const source_text = self.source_text;

            // 3. Let F be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
            //    FormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncFunction.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 4. Perform SetFunctionName(F, name).
            try setFunctionName(function, PropertyKey.from(name), null);

            // 5. Return F.
            return function;
        }
        // AsyncFunctionDeclaration : async function ( FormalParameters ) { AsyncFunctionBody }
        else {
            // 1. Let sourceText be the source text matched by AsyncFunctionDeclaration.
            const source_text = self.source_text;

            // 2. Let F be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
            //    FormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
            const function = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncFunction.prototype%"(),
                source_text,
                self.formal_parameters,
                self.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 3. Perform SetFunctionName(F, "default").
            try setFunctionName(function, PropertyKey.from("default"), null);

            // 4. Return F.
            return function;
        }
    }
};

/// https://tc39.es/ecma262/#prod-AsyncFunctionExpression
pub const AsyncFunctionExpression = struct {
    identifier: ?Identifier,
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-AsyncArrowFunction
pub const AsyncArrowFunction = struct {
    formal_parameters: FormalParameters,
    function_body: FunctionBody,
    source_text: []const u8,
};

/// https://tc39.es/ecma262/#prod-Script
pub const Script = struct {
    const Self = @This();

    statement_list: StatementList,

    /// 8.2.5 Static Semantics: LexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations
    pub fn lexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        // Script : [empty]
        // 1. Return a new empty List.
        // ScriptBody : StatementList
        // 1. Return TopLevelLexicallyScopedDeclarations of StatementList.
        return self.statement_list.topLevelLexicallyScopedDeclarations(allocator);
    }

    /// 8.2.6 Static Semantics: VarDeclaredNames
    /// https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames
    pub fn varDeclaredNames(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const Identifier {
        // Script : [empty]
        // 1. Return a new empty List.
        // ScriptBody : StatementList
        // 1. Return TopLevelVarDeclaredNames of StatementList.
        return self.statement_list.topLevelVarDeclaredNames(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // Script : [empty]
        // 1. Return a new empty List.
        // ScriptBody : StatementList
        // 1. Return TopLevelVarScopedDeclarations of StatementList.
        return self.statement_list.topLevelVarScopedDeclarations(allocator);
    }

    /// 16.1.2 Static Semantics: IsStrict
    /// https://tc39.es/ecma262/#sec-static-semantics-isstrict
    pub fn isStrict(self: Self) bool {
        // 1. If ScriptBody is present and the Directive Prologue of ScriptBody contains a Use
        //    Strict Directive, return true; otherwise, return false.
        return self.statement_list.containsDirective("use strict");
    }
};

/// https://tc39.es/ecma262/#prod-Module
pub const Module = struct {
    const Self = @This();

    module_item_list: ModuleItemList,

    /// 8.2.5 Static Semantics: LexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations
    pub fn lexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        // Module : [empty]
        // 1. Return a new empty List.
        // ModuleItemList : ModuleItemList ModuleItem
        // 1. Let declarations1 be LexicallyScopedDeclarations of ModuleItemList.
        // 2. Let declarations2 be LexicallyScopedDeclarations of ModuleItem.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        return self.module_item_list.lexicallyScopedDeclarations(allocator);
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        return self.module_item_list.varScopedDeclarations(allocator);
    }

    /// 16.2.3.4 Static Semantics: ExportEntries
    /// https://tc39.es/ecma262/#sec-static-semantics-exportentries
    pub fn exportEntries(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const ExportEntry {
        // Module : [empty]
        // 1. Return a new empty List.
        // ModuleItemList : ModuleItemList ModuleItem
        // 1. Let entries1 be ExportEntries of ModuleItemList.
        // 2. Let entries2 be ExportEntries of ModuleItem.
        // 3. Return the list-concatenation of entries1 and entries2.
        var export_entries = std.ArrayList(ExportEntry).init(allocator);
        for (self.module_item_list.items) |module_item| switch (module_item) {
            // ModuleItem :
            //     ImportDeclaration
            //     StatementListItem
            .import_declaration, .statement_list_item => {
                // 1. Return a new empty List.
            },
            .export_declaration => |export_declaration| switch (export_declaration) {
                // ExportDeclaration : export ExportFromClause FromClause ;
                .export_from => {
                    // TODO: 1. Let module be the sole element of ModuleRequests of FromClause.
                    // TODO: 2. Return ExportEntriesForModule of ExportFromClause with argument module.
                },

                // ExportDeclaration : export NamedExports ;
                .named_exports => {
                    // TODO: 1. Return ExportEntriesForModule of NamedExports with argument null.
                },

                // ExportDeclaration : export VariableStatement
                .variable_statement => |variable_statement| {
                    // 1. Let entries be a new empty List.

                    // 2. Let names be the BoundNames of VariableStatement.
                    const names = try variable_statement.variable_declaration_list.boundNames(allocator);
                    defer allocator.free(names);

                    // 3. For each element name of names, do
                    for (names) |name| {
                        // a. Append the ExportEntry Record {
                        //      [[ModuleRequest]]: null, [[ImportName]]: null, [[LocalName]]: name,
                        //      [[ExportName]]: name
                        //    } to entries.
                        try export_entries.append(.{
                            .module_request = null,
                            .import_name = null,
                            .local_name = name,
                            .export_name = name,
                        });
                    }

                    // 4. Return entries.
                },

                // ExportDeclaration : export Declaration
                .declaration => |declaration| {
                    // 1. Let entries be a new empty List.

                    // 2. Let names be the BoundNames of Declaration.
                    const names = try declaration.boundNames(allocator);
                    defer allocator.free(names);

                    // 3. For each element name of names, do
                    for (names) |name| {
                        // a. Append the ExportEntry Record {
                        //      [[ModuleRequest]]: null, [[ImportName]]: null,
                        //      [[LocalName]]: name, [[ExportName]]: name
                        //    } to entries.
                        try export_entries.append(.{
                            .module_request = null,
                            .import_name = null,
                            .local_name = name,
                            .export_name = name,
                        });
                    }

                    // 4. Return entries.
                },

                // ExportDeclaration : export default HoistableDeclaration
                .default_hoistable_declaration => |hoistable_declaration| {
                    // 1. Let names be BoundNames of HoistableDeclaration.
                    const names = try hoistable_declaration.boundNames(allocator);
                    defer allocator.free(names);

                    // 2. Let localName be the sole element of names.
                    const local_name = names[0];

                    // 3. Return a List whose sole element is a new ExportEntry Record {
                    //      [[ModuleRequest]]: null, [[ImportName]]: null,
                    //      [[LocalName]]: localName, [[ExportName]]: "default"
                    //    }.
                    try export_entries.append(.{
                        .module_request = null,
                        .import_name = null,
                        .local_name = local_name,
                        .export_name = "default",
                    });
                },

                // ExportDeclaration : export default ClassDeclaration
                .default_class_declaration => |class_declaration| {
                    // 1. Let names be BoundNames of ClassDeclaration.
                    const names = try class_declaration.boundNames(allocator);
                    defer allocator.free(names);

                    // 2. Let localName be the sole element of names.
                    const local_name = names[0];

                    // 3. Return a List whose sole element is a new ExportEntry Record {
                    //      [[ModuleRequest]]: null, [[ImportName]]: null,
                    //      [[LocalName]]: localName, [[ExportName]]: "default"
                    //    }.
                    try export_entries.append(.{
                        .module_request = null,
                        .import_name = null,
                        .local_name = local_name,
                        .export_name = "default",
                    });
                },

                // ExportDeclaration : export default AssignmentExpression ;
                .default_expression => {
                    // 1. Let entry be the ExportEntry Record {
                    //      [[ModuleRequest]]: null, [[ImportName]]: null,
                    //      [[LocalName]]: "*default*", [[ExportName]]: "default"
                    //    }.
                    // 2. Return « entry ».
                    try export_entries.append(.{
                        .module_request = null,
                        .import_name = null,
                        .local_name = "*default*",
                        .export_name = "default",
                    });
                },
            },
        };
        return export_entries.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-ModuleItemList
pub const ModuleItemList = struct {
    const Self = @This();

    items: []const ModuleItem,

    /// 8.2.5 Static Semantics: LexicallyScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations
    pub fn lexicallyScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const LexicallyScopedDeclaration {
        var declarations = std.ArrayList(LexicallyScopedDeclaration).init(allocator);
        for (self.items) |item| switch (item) {
            .statement_list_item => |statement_list_item| {
                try declarations.appendSlice(try statement_list_item.lexicallyScopedDeclarations(allocator));
            },

            // ModuleItem : ImportDeclaration
            .import_declaration => {
                // 1. Return a new empty List.
            },

            .export_declaration => |export_declaration| switch (export_declaration) {
                // ExportDeclaration :
                //     export ExportFromClause FromClause ;
                //     export NamedExports ;
                //     export VariableStatement
                .export_from, .named_exports, .variable_statement => {
                    // 1. Return a new empty List.
                },

                // ExportDeclaration : export Declaration
                .declaration => |declaration| {
                    // 1. Return a List whose sole element is DeclarationPart of Declaration.
                    switch (declaration.*) {
                        .hoistable_declaration => |hoistable_declaration| try declarations.append(.{ .hoistable_declaration = hoistable_declaration }),
                        .class_declaration => |class_declaration| try declarations.append(.{ .class_declaration = class_declaration }),
                        .lexical_declaration => |lexical_declaration| try declarations.append(.{ .lexical_declaration = lexical_declaration }),
                    }
                },

                // ExportDeclaration : export default HoistableDeclaration
                .default_hoistable_declaration => |hoistable_declaration| {
                    // 1. Return a List whose sole element is DeclarationPart of HoistableDeclaration.
                    try declarations.append(.{ .hoistable_declaration = hoistable_declaration });
                },

                // ExportDeclaration : export default ClassDeclaration
                .default_class_declaration => |class_declaration| {
                    // 1. Return a List whose sole element is ClassDeclaration.
                    try declarations.append(.{ .class_declaration = class_declaration });
                },

                // ExportDeclaration : export default AssignmentExpression ;
                .default_expression => |expression| {
                    // 1. Return a List whose sole element is this ExportDeclaration.
                    try declarations.append(.{ .export_declaration = expression });
                },
            },
        };
        return declarations.toOwnedSlice();
    }

    /// 8.2.7 Static Semantics: VarScopedDeclarations
    /// https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations
    pub fn varScopedDeclarations(
        self: Self,
        allocator: Allocator,
    ) Allocator.Error![]const VarScopedDeclaration {
        // ModuleItemList : ModuleItemList ModuleItem
        // 1. Let declarations1 be VarScopedDeclarations of ModuleItemList.
        // 2. Let declarations2 be VarScopedDeclarations of ModuleItem.
        // 3. Return the list-concatenation of declarations1 and declarations2.
        // ModuleItem : ImportDeclaration
        // 1. Return a new empty List.
        // ModuleItem : ExportDeclaration
        // 1. If ExportDeclaration is export VariableStatement, return VarScopedDeclarations of VariableStatement.
        // 2. Return a new empty List.
        var variable_declarations = std.ArrayList(VarScopedDeclaration).init(allocator);
        for (self.items) |item| switch (item) {
            .statement_list_item => |statement_list_item| {
                try variable_declarations.appendSlice(try statement_list_item.varScopedDeclarations(allocator));
            },
            .export_declaration => |export_declaration| switch (export_declaration) {
                .variable_statement => |variable_statement| try variable_declarations.appendSlice(
                    try variable_statement.variable_declaration_list.varScopedDeclarations(allocator),
                ),
                else => {},
            },
            .import_declaration => {},
        };
        return variable_declarations.toOwnedSlice();
    }
};

/// https://tc39.es/ecma262/#prod-ModuleItem
pub const ModuleItem = union(enum) {
    import_declaration: ImportDeclaration,
    export_declaration: ExportDeclaration,
    statement_list_item: StatementListItem,
};

/// https://tc39.es/ecma262/#prod-ModuleExportName
pub const ModuleExportName = union(enum) {
    identifier: Identifier,
    string_literal: StringLiteral,
};

/// https://tc39.es/ecma262/#prod-ImportDeclaration
pub const ImportDeclaration = struct {
    import_clause: ?ImportClause,
    module_specifier: StringLiteral,
};

/// https://tc39.es/ecma262/#prod-ImportClause
pub const ImportClause = union(enum) {
    imported_default_binding: struct {
        binding_identifier: Identifier,
    },
    // TODO: NameSpaceImport
    // TODO: NamedImports
    // TODO: ImportedDefaultBinding , NameSpaceImport
    // TODO: ImportedDefaultBinding , NamedImports
};

/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportDeclaration = union(enum) {
    pub const ExportFrom = struct {
        export_from_clause: ExportFromClause,
        module_specifier: StringLiteral,
    };

    export_from: ExportFrom,
    named_exports: NamedExports,
    declaration: *Declaration,
    variable_statement: VariableStatement,
    default_hoistable_declaration: HoistableDeclaration,
    default_class_declaration: ClassDeclaration,
    default_expression: Expression,
};

/// https://tc39.es/ecma262/#prod-ExportFromClause
pub const ExportFromClause = union(enum) {
    star,
    star_as: ModuleExportName,
    named_exports: NamedExports,
};

/// https://tc39.es/ecma262/#prod-NamedExports
pub const NamedExports = struct {
    exports_list: ExportsList,
};

/// https://tc39.es/ecma262/#prod-ExportsList
pub const ExportsList = struct {
    items: []const ExportSpecifier,
};

/// https://tc39.es/ecma262/#prod-ExportSpecifier
pub const ExportSpecifier = struct {
    name: ModuleExportName,
    alias: ?ModuleExportName,
};
