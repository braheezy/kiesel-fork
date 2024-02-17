const std = @import("std");

const ast = @import("../ast.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const Executable = @import("Executable.zig");
const IteratorKind = types.IteratorKind;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const noexcept = utils.noexcept;
const temporaryChange = utils.temporaryChange;

pub const Context = struct {
    agent: *Agent,
    contained_in_strict_mode_code: bool = false,
    environment_lookup_cache_index: Executable.IndexType = 0,
    continue_jumps: std.ArrayList(Executable.JumpIndex),
    break_jumps: std.ArrayList(Executable.JumpIndex),
};

pub fn codegenParenthesizedExpression(
    node: ast.ParenthesizedExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    try codegenExpression(node.expression.*, executable, ctx);
}

/// 13.1.3 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-identifiers-runtime-semantics-evaluation
pub fn codegenIdentifierReference(
    node: ast.IdentifierReference,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // IdentifierReference : Identifier
    // IdentifierReference : yield
    // IdentifierReference : await
    // 1. Return ? ResolveBinding(StringValue of Identifier).
    try executable.addInstructionWithIdentifier(.resolve_binding, node);
    const strict = ctx.contained_in_strict_mode_code;
    try executable.addIndex(@intFromBool(strict));
    try executable.addIndex(ctx.environment_lookup_cache_index);
    ctx.environment_lookup_cache_index += 1;
}

pub fn codegenPrimaryExpression(
    node: ast.PrimaryExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        // PrimaryExpression : this
        .this => {
            // 1. Return ? ResolveThisBinding().
            try executable.addInstruction(.resolve_this_binding);
        },
        .identifier_reference => |x| try codegenIdentifierReference(x, executable, ctx),
        .literal => |x| try codegenLiteral(x, executable, ctx),
        .array_literal => |x| try codegenArrayLiteral(x, executable, ctx),
        .object_literal => |x| try codegenObjectLiteral(x, executable, ctx),
        .function_expression => |x| try codegenFunctionExpression(x, executable, ctx),
        .class_expression => |x| try codegenClassExpression(x, executable, ctx),
        .generator_expression => |x| try codegenGeneratorExpression(x, executable, ctx),
        .async_function_expression => |x| try codegenAsyncFunctionExpression(x, executable, ctx),
        .async_generator_expression => |x| try codegenAsyncGeneratorExpression(x, executable, ctx),
        .regular_expression_literal => |x| try codegenRegularExpressionLiteral(x, executable, ctx),
        .template_literal => |x| try codegenTemplateLiteral(x, executable, ctx),
        .arrow_function => |x| try codegenArrowFunction(x, executable, ctx),
        .async_arrow_function => |x| try codegenAsyncArrowFunction(x, executable, ctx),
        .parenthesized_expression => |x| try codegenParenthesizedExpression(x, executable, ctx),
    }
}

/// 13.3.2.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-property-accessors-runtime-semantics-evaluation
pub fn codegenMemberExpression(
    node: ast.MemberExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // 1. Let baseReference be ? Evaluation of MemberExpression.
    try codegenExpression(node.expression.*, executable, ctx);

    // 2. Let baseValue be ? GetValue(baseReference).
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // 3. If the source text matched by this MemberExpression is strict mode code, let strict
    //    be true; else let strict be false.
    const strict = ctx.contained_in_strict_mode_code;

    switch (node.property) {
        // MemberExpression : MemberExpression [ Expression ]
        .expression => |expression| {
            // 4. Return ? EvaluatePropertyAccessWithExpressionKey(baseValue, Expression, strict).
            try codegenExpression(expression.*, executable, ctx);
            if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);
            try executable.addInstruction(.evaluate_property_access_with_expression_key);
            try executable.addIndex(@intFromBool(strict));
        },

        // MemberExpression : MemberExpression . IdentifierName
        .identifier => |identifier| {
            // 4. Return EvaluatePropertyAccessWithIdentifierKey(baseValue, IdentifierName, strict).
            try executable.addInstructionWithIdentifier(
                .evaluate_property_access_with_identifier_key,
                identifier,
            );
            try executable.addIndex(@intFromBool(strict));
        },
    }
}

/// 13.3.7.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-super-keyword-runtime-semantics-evaluation
pub fn codegenSuperProperty(
    node: ast.SuperProperty,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        // SuperProperty : super [ Expression ]
        .expression => |expression| {
            // 1. Let env be GetThisEnvironment().
            // 2. Let actualThis be ? env.GetThisBinding().
            try executable.addInstruction(.load_this_value_for_make_super_property_reference);

            // 3. Let propertyNameReference be ? Evaluation of Expression.
            try codegenExpression(expression.*, executable, ctx);

            // 4. Let propertyNameValue be ? GetValue(propertyNameReference).
            if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);

            // 6. If the source text matched by this SuperProperty is strict mode code, let
            //    strict be true; else let strict be false.
            const strict = ctx.contained_in_strict_mode_code;

            // 7. Return ? MakeSuperPropertyReference(actualThis, propertyKey, strict).
            try executable.addInstruction(.make_super_property_reference);
            try executable.addIndex(@intFromBool(strict));
        },

        // SuperProperty : super . IdentifierName
        .identifier => |identifier| {
            // 1. Let env be GetThisEnvironment().
            // 2. Let actualThis be ? env.GetThisBinding().
            try executable.addInstruction(.load_this_value_for_make_super_property_reference);

            // 3. Let propertyKey be StringValue of IdentifierName.
            try executable.addInstructionWithConstant(.load_constant, Value.from(identifier));

            // 4. If the source text matched by this SuperProperty is strict mode code, let
            //    strict be true; else let strict be false.
            const strict = ctx.contained_in_strict_mode_code;

            // 5. Return ? MakeSuperPropertyReference(actualThis, propertyKey, strict).
            try executable.addInstruction(.make_super_property_reference);
            try executable.addIndex(@intFromBool(strict));
        },
    }
}

/// 13.3.12.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-meta-properties-runtime-semantics-evaluation
pub fn codegenMetaProperty(
    node: ast.MetaProperty,
    executable: *Executable,
    _: *Context,
) Executable.Error!void {
    switch (node) {
        // NewTarget : new . target
        .new_target => {
            // 1. Return GetNewTarget().
            try executable.addInstruction(.get_new_target);
        },

        // ImportMeta : import . meta
        .import_meta => {
            // 1-5.
            try executable.addInstruction(.get_or_create_import_meta);
        },
    }
}

/// 13.3.5.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-new-operator-runtime-semantics-evaluation
pub fn codegenNewExpression(
    node: ast.NewExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // NewExpression : new NewExpression
    // 1. Return ? EvaluateNew(NewExpression, empty).
    // MemberExpression : new MemberExpression Arguments
    // 1. Return ? EvaluateNew(MemberExpression, Arguments).
    try codegenExpression(node.expression.*, executable, ctx);
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    for (node.arguments) |argument| {
        try codegenExpression(argument, executable, ctx);
        if (argument.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);
    }

    try executable.addInstruction(.evaluate_new);
    try executable.addIndex(node.arguments.len);
}

/// 13.3.6.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-function-calls-runtime-semantics-evaluation
pub fn codegenCallExpression(
    node: ast.CallExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // CallExpression : CallExpression Arguments
    // 1. Let ref be ? Evaluation of CallExpression.
    try codegenExpression(node.expression.*, executable, ctx);

    try executable.addInstruction(.push_reference);

    // 2. Let func be ? GetValue(ref).
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // TODO: 3. Let thisCall be this CallExpression.
    // TODO: 4. Let tailCall be IsInTailPosition(thisCall).

    try executable.addInstruction(.load_this_value_for_evaluate_call);

    for (node.arguments) |argument| {
        try codegenExpression(argument, executable, ctx);
        if (argument.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);
    }

    const strict = ctx.contained_in_strict_mode_code;

    // 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
    try executable.addInstruction(.evaluate_call);
    try executable.addIndex(node.arguments.len);
    try executable.addIndex(@intFromBool(strict));

    // TODO: We should probably also clean this up if something throws beforehand...
    try executable.addInstruction(.pop_reference);
}

/// 13.3.7.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-super-keyword-runtime-semantics-evaluation
pub fn codegenSuperCall(
    node: ast.SuperCall,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    for (node.arguments) |argument| {
        try codegenExpression(argument, executable, ctx);
        if (argument.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);
    }
    try executable.addInstruction(.evaluate_super_call);
    try executable.addIndex(node.arguments.len);
}

/// 13.3.10.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-import-call-runtime-semantics-evaluation
pub fn codegenImportCall(
    node: ast.ImportCall,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    try codegenExpression(node.expression.*, executable, ctx);
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);
    try executable.addInstruction(.evaluate_import_call);
}

/// 13.3.9.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-optional-chaining-evaluation
/// 13.3.9.2 Runtime Semantics: ChainEvaluation
/// https://tc39.es/ecma262/#sec-optional-chaining-chain-evaluation
pub fn codegenOptionalExpression(
    node: ast.OptionalExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // 1. Let baseReference be ? Evaluation of OptionalExpression.
    try codegenExpression(node.expression.*, executable, ctx);

    if (node.property == .arguments) try executable.addInstruction(.push_reference);

    // 2. Let baseValue be ? GetValue(baseReference).
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // 3. If baseValue is either undefined or null, then
    try executable.addInstruction(.load);
    try executable.addInstructionWithConstant(.load_constant, .undefined);
    try executable.addInstruction(.is_loosely_equal);

    try executable.addInstruction(.jump_conditional);
    const consequent_jump = try executable.addJumpIndex();
    const alternate_jump = try executable.addJumpIndex();

    // a. Return undefined.
    try consequent_jump.setTargetHere();
    try executable.addInstruction(.store); // Drop baseValue from the stack
    try executable.addInstructionWithConstant(.store_constant, .undefined);
    try executable.addInstruction(.jump);
    const end_jump = try executable.addJumpIndex();

    // 4. Return ? ChainEvaluation of OptionalChain with arguments baseValue and baseReference.
    try alternate_jump.setTargetHere();

    // 1. If the source text matched by this OptionalChain is strict mode code, let strict be
    //    true; else let strict be false.
    const strict = ctx.contained_in_strict_mode_code;

    switch (node.property) {
        // OptionalChain : ?. Arguments
        .arguments => |arguments| {
            try executable.addInstruction(.load_this_value_for_evaluate_call);

            for (arguments) |argument| {
                try codegenExpression(argument, executable, ctx);
                if (argument.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.load);
            }

            // TODO: 1. Let thisChain be this OptionalChain.
            // TODO: 2. Let tailCall be IsInTailPosition(thisChain).

            // 3. Return ? EvaluateCall(baseValue, baseReference, Arguments, tailCall).
            try executable.addInstruction(.evaluate_call);
            try executable.addIndex(arguments.len);
            try executable.addIndex(@intFromBool(strict));

            // TODO: We should probably also clean this up if something throws beforehand...
            try executable.addInstruction(.pop_reference);
        },

        // OptionalChain : ?. [ Expression ]
        .expression => |expression| {
            // 2. Return ? EvaluatePropertyAccessWithExpressionKey(baseValue, Expression, strict).
            try codegenExpression(expression.*, executable, ctx);
            if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);
            try executable.addInstruction(.evaluate_property_access_with_expression_key);
            try executable.addIndex(@intFromBool(strict));
        },

        // OptionalChain : ?. IdentifierName
        .identifier => |identifier| {
            // 2. Return EvaluatePropertyAccessWithIdentifierKey(baseValue, IdentifierName, strict).
            try executable.addInstructionWithIdentifier(
                .evaluate_property_access_with_identifier_key,
                identifier,
            );
            try executable.addIndex(@intFromBool(strict));
        },
    }

    try end_jump.setTargetHere();
}

/// 13.2.3.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation
pub fn codegenLiteral(
    node: ast.Literal,
    executable: *Executable,
    _: *Context,
) Executable.Error!void {
    switch (node) {
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

/// 13.2.4.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-array-initializer-runtime-semantics-evaluation
pub fn codegenArrayLiteral(
    node: ast.ArrayLiteral,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    try executable.addInstruction(.array_create);
    try executable.addInstruction(.load);
    for (node.element_list, 0..) |element, i| {
        switch (element) {
            // Elision : ,
            .elision => {
                try executable.addInstruction(.store);
                try executable.addInstruction(.array_set_length);
                try executable.addIndex(i + 1);
                try executable.addInstruction(.load);
            },

            // ElementList : Elision[opt] AssignmentExpression
            .expression => |expression| {
                // 1. If Elision is present, then
                // NOTE: This is handled above.

                // 2. Let initResult be ? Evaluation of AssignmentExpression.
                try codegenExpression(expression, executable, ctx);

                // 3. Let initValue be ? GetValue(initResult).
                if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.load);

                // 4. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), initValue).
                try executable.addInstruction(.array_push_value);
                try executable.addInstruction(.load);

                // 5. Return nextIndex + 1.
            },

            // SpreadElement : ... AssignmentExpression
            .spread => |expression| {
                // 1. Let spreadRef be ? Evaluation of AssignmentExpression.
                try codegenExpression(expression, executable, ctx);

                // 2. Let spreadObj be ? GetValue(spreadRef).
                if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.load);

                // 3-4.
                try executable.addInstruction(.array_spread_value);
                try executable.addInstruction(.load);
            },
        }
    }
    try executable.addInstruction(.store);
}

/// 13.2.5.4 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-object-initializer-runtime-semantics-evaluation
pub fn codegenObjectLiteral(
    node: ast.ObjectLiteral,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // ObjectLiteral : { }
    if (node.property_definition_list.items.len == 0) {
        // 1. Return OrdinaryObjectCreate(%Object.prototype%).
        try executable.addInstruction(.object_create);
        return;
    }

    // ObjectLiteral :
    //     { PropertyDefinitionList }
    //     { PropertyDefinitionList , }
    // 1. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    try executable.addInstruction(.object_create);

    // 2. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with argument obj.
    try codegenPropertyDefinitionList(node.property_definition_list, executable, ctx);

    // 3. Return obj.
}

/// 13.2.5.5 Runtime Semantics: PropertyDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-propertydefinitionevaluation
pub fn codegenPropertyDefinitionList(
    node: ast.PropertyDefinitionList,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
    // 1. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with argument object.
    // 2. Perform ? PropertyDefinitionEvaluation of PropertyDefinition with argument object.
    for (node.items) |property_definition| {
        // Load object onto the stack again before each property definition is evaluated
        try executable.addInstruction(.load);
        try codegenPropertyDefinition(property_definition, executable, ctx);
    }

    // 3. Return unused.
}

/// 13.2.5.5 Runtime Semantics: PropertyDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-propertydefinitionevaluation
pub fn codegenPropertyDefinition(
    node: ast.PropertyDefinition,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        // PropertyDefinition : ... AssignmentExpression
        .spread => |expression| {
            // 1. Let exprValue be ? Evaluation of AssignmentExpression.
            try codegenExpression(expression, executable, ctx);

            // 2. Let fromValue be ? GetValue(exprValue).
            if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);

            // 3. Let excludedNames be a new empty List.
            // 4. Perform ? CopyDataProperties(object, fromValue, excludedNames).
            try executable.addInstruction(.object_spread_value);

            // 5. Return unused.
        },

        // PropertyDefinition : IdentifierReference
        .identifier_reference => |identifier_reference| {
            // 1. Let propName be StringValue of IdentifierReference.
            try executable.addInstructionWithConstant(.load_constant, Value.from(identifier_reference));

            // 2. Let exprValue be ? Evaluation of IdentifierReference.
            try codegenIdentifierReference(identifier_reference, executable, ctx);

            // 3. Let propValue be ? GetValue(exprValue).
            try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);

            // 4. Assert: object is an ordinary, extensible object with no non-configurable properties.
            // 5. Perform ! CreateDataPropertyOrThrow(object, propName, propValue).
            try executable.addInstruction(.object_set_property);

            // 6. Return unused.
        },

        // PropertyDefinition : PropertyName : AssignmentExpression
        .property_name_and_expression => |property_name_and_expression| {
            // 1. Let propKey be ? Evaluation of PropertyName.
            try codegenPropertyName(property_name_and_expression.property_name, executable, ctx);
            try executable.addInstruction(.load);

            // TODO: 2-4.

            // TODO: 5. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and isProtoSetter is false, then
            // 6. Else,

            // a. Let exprValueRef be ? Evaluation of AssignmentExpression.
            try codegenExpression(property_name_and_expression.expression, executable, ctx);

            // b. Let propValue be ? GetValue(exprValueRef).
            if (property_name_and_expression.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.load);

            // TODO: 7. If isProtoSetter is true, then

            // 8. Assert: object is an ordinary, extensible object with no non-configurable properties.
            // 9. Perform ! CreateDataPropertyOrThrow(object, propKey, propValue).
            try executable.addInstruction(.object_set_property);

            // 10. Return unused.
        },

        // PropertyDefinition : MethodDefinition
        .method_definition => |method_definition| {
            // 1. Perform ? MethodDefinitionEvaluation of MethodDefinition with arguments object and true.
            try codegenMethodDefinition(method_definition, executable, ctx);

            // 2. Return unused.
        },
    }
}

/// 13.2.5.4 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-object-initializer-runtime-semantics-evaluation
pub fn codegenPropertyName(
    node: ast.PropertyName,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        .literal_property_name => |literal| switch (literal) {
            // LiteralPropertyName : IdentifierName
            .identifier => |identifier| {
                // 1. Return StringValue of IdentifierName.
                try executable.addInstructionWithConstant(.store_constant, Value.from(identifier));
            },

            // LiteralPropertyName : StringLiteral
            .string_literal => |string_literal| {
                // 1. Return the SV of StringLiteral.
                try executable.addInstructionWithConstant(
                    .store_constant,
                    try string_literal.stringValue(executable.allocator),
                );
            },

            // LiteralPropertyName : NumericLiteral
            .numeric_literal => |numeric_literal| {
                // 1. Let nbr be the NumericValue of NumericLiteral.
                // 2. Return ! ToString(nbr).
                try executable.addInstructionWithConstant(
                    .store_constant,
                    try numeric_literal.numericValue(executable.allocator),
                );
            },
        },

        // ComputedPropertyName : [ AssignmentExpression ]
        .computed_property_name => |expression| {
            // 1. Let exprValue be ? Evaluation of AssignmentExpression.
            try codegenExpression(expression, executable, ctx);

            // 2. Let propName be ? GetValue(exprValue).
            if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            // 3. Return ? ToPropertyKey(propName).
            // NOTE: This is done in object_set_property
        },
    }
}

/// 13.2.7.3 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-regular-expression-literals-runtime-semantics-evaluation
pub fn codegenRegularExpressionLiteral(
    node: ast.RegularExpressionLiteral,
    executable: *Executable,
    _: *Context,
) Executable.Error!void {
    // 1. Let pattern be CodePointsToString(BodyText of RegularExpressionLiteral).
    try executable.addInstructionWithConstant(.load_constant, Value.from(node.pattern));

    // 2. Let flags be CodePointsToString(FlagText of RegularExpressionLiteral).
    try executable.addInstructionWithConstant(.load_constant, Value.from(node.flags));

    // 3. Return ! RegExpCreate(pattern, flags).
    try executable.addInstruction(.reg_exp_create);
}

/// 13.2.8.6 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-template-literals-runtime-semantics-evaluation
pub fn codegenTemplateLiteral(
    node: ast.TemplateLiteral,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // TemplateLiteral : NoSubstitutionTemplate
    // 1. Return the TV of NoSubstitutionTemplate as defined in 12.9.6.
    if (node.spans.len == 1) {
        const span = node.spans[0];
        std.debug.assert(span == .text);
        try executable.addInstructionWithConstant(
            .store_constant,
            try span.templateValue(executable.allocator),
        );
        return;
    }

    // SubstitutionTemplate : TemplateHead Expression TemplateSpans
    // 1. Let head be the TV of TemplateHead as defined in 12.9.6.
    // 2. Let subRef be ? Evaluation of Expression.
    // 3. Let sub be ? GetValue(subRef).
    // 4. Let middle be ? ToString(sub).
    // 5. Let tail be ? Evaluation of TemplateSpans.
    // 6. Return the string-concatenation of head, middle, and tail.
    for (node.spans, 0..) |span, i| {
        std.debug.assert(if (i % 2 == 0) span == .text else span == .expression);
        switch (span) {
            .expression => |expression| {
                try codegenExpression(expression, executable, ctx);
                if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
                try executable.addInstruction(.to_string);
                try executable.addInstruction(.load);
            },
            .text => {
                try executable.addInstructionWithConstant(
                    .load_constant,
                    try span.templateValue(executable.allocator),
                );
            },
        }
        if (i != 0) {
            try executable.addInstruction(.apply_string_or_numeric_binary_operator);
            try executable.addIndex(@intFromEnum(ast.BinaryExpression.Operator.@"+"));
            if (i < node.spans.len - 1) try executable.addInstruction(.load);
        }
    }
}

/// 13.4.2.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-postfix-increment-operator-runtime-semantics-evaluation
/// 13.4.3.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-postfix-decrement-operator-runtime-semantics-evaluation
/// 13.4.4.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-prefix-increment-operator-runtime-semantics-evaluation
/// 13.4.5.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-prefix-decrement-operator-runtime-semantics-evaluation
pub fn codegenUpdateExpression(
    node: ast.UpdateExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // UpdateExpression : LeftHandSideExpression ++
    if (node.type == .postfix and node.operator == .@"++") {
        // 1. Let lhs be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let oldValue be ? ToNumeric(? GetValue(lhs)).
        if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.to_numeric);

        try executable.addInstruction(.load);

        // 3. If oldValue is a Number, then
        //     a. Let newValue be Number::add(oldValue, 1𝔽).
        // 4. Else,
        //     a. Assert: oldValue is a BigInt.
        //     b. Let newValue be BigInt::add(oldValue, 1ℤ).
        try executable.addInstruction(.increment);

        // 5. Perform ? PutValue(lhs, newValue).
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);

        // 6. Return oldValue.
        try executable.addInstruction(.store);
    }
    // UpdateExpression : LeftHandSideExpression --
    else if (node.type == .postfix and node.operator == .@"--") {
        // 1. Let lhs be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let oldValue be ? ToNumeric(? GetValue(lhs)).
        if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.to_numeric);

        try executable.addInstruction(.load);

        // 3. If oldValue is a Number, then
        //     a. Let newValue be Number::subtract(oldValue, 1𝔽).
        // 4. Else,
        //     a. Assert: oldValue is a BigInt.
        //     b. Let newValue be BigInt::subtract(oldValue, 1ℤ).
        try executable.addInstruction(.decrement);

        // 5. Perform ? PutValue(lhs, newValue).
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);

        // 6. Return oldValue.
        try executable.addInstruction(.store);
    }
    // UpdateExpression : ++ UnaryExpression
    else if (node.type == .prefix and node.operator == .@"++") {
        // 1. Let expr be ? Evaluation of UnaryExpression.
        try codegenExpression(node.expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let oldValue be ? ToNumeric(? GetValue(expr)).
        if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.to_numeric);

        // 3. If oldValue is a Number, then
        //     a. Let newValue be Number::add(oldValue, 1𝔽).
        // 4. Else,
        //     a. Assert: oldValue is a BigInt.
        //     b. Let newValue be BigInt::add(oldValue, 1ℤ).
        try executable.addInstruction(.increment);

        // 5. Perform ? PutValue(expr, newValue).
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);

        // 6. Return newValue.
    }
    // UpdateExpression : -- UnaryExpression
    else if (node.type == .prefix and node.operator == .@"--") {
        // 1. Let expr be ? Evaluation of UnaryExpression.
        try codegenExpression(node.expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let oldValue be ? ToNumeric(? GetValue(expr)).
        if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.to_numeric);

        // 3. If oldValue is a Number, then
        //     a. Let newValue be Number::subtract(oldValue, 1𝔽).
        // 4. Else,
        //     a. Assert: oldValue is a BigInt.
        //     b. Let newValue be BigInt::subtract(oldValue, 1ℤ).
        try executable.addInstruction(.decrement);

        // 5. Perform ? PutValue(expr, newValue).
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);

        // 6. Return newValue.
    } else unreachable;
}

/// 13.5.1.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-delete-operator-runtime-semantics-evaluation
/// 13.5.2.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-void-operator-runtime-semantics-evaluation
/// 13.5.3.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-typeof-operator-runtime-semantics-evaluation
/// 13.5.4.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-unary-plus-operator-runtime-semantics-evaluation
/// 13.5.5.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-unary-minus-operator-runtime-semantics-evaluation
/// 13.5.6.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-bitwise-not-operator-runtime-semantics-evaluation
/// 13.5.7.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-logical-not-operator-runtime-semantics-evaluation
pub fn codegenUnaryExpression(
    node: ast.UnaryExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node.operator) {
        // UnaryExpression : delete UnaryExpression
        .delete => {
            // 1. Let ref be ? Evaluation of UnaryExpression.
            try codegenExpression(node.expression.*, executable, ctx);

            if (!node.expression.analyze(.is_reference))
                // 2. If ref is not a Reference Record, return true.
                try executable.addInstructionWithConstant(.store_constant, Value.from(true))
            else
                // 3-5.
                try executable.addInstruction(.delete);
        },

        // UnaryExpression : void UnaryExpression
        .void => {
            // 1. Let expr be ? Evaluation of UnaryExpression.
            try codegenExpression(node.expression.*, executable, ctx);

            // 2. Perform ? GetValue(expr).
            if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            // 3. Return undefined.
            try executable.addInstructionWithConstant(.store_constant, .undefined);
        },

        // UnaryExpression : typeof UnaryExpression
        .typeof => {
            // NOTE: get_value is intentionally omitted here, typeof needs to do it conditionally.
            try codegenExpression(node.expression.*, executable, ctx);
            try executable.addInstruction(.typeof);
        },

        // UnaryExpression : + UnaryExpression
        .@"+" => {
            // 1. Let expr be ? Evaluation of UnaryExpression.
            try codegenExpression(node.expression.*, executable, ctx);

            // 2. Return ? ToNumber(? GetValue(expr)).
            if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.to_number);
        },

        // UnaryExpression : - UnaryExpression
        .@"-" => {
            // 1. Let expr be ? Evaluation of UnaryExpression.
            try codegenExpression(node.expression.*, executable, ctx);

            // 2. Let oldValue be ? ToNumeric(? GetValue(expr)).
            if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.to_numeric);

            // 3. If oldValue is a Number, then
            //     a. Return Number::unaryMinus(oldValue).
            // 4. Else,
            //     a. Assert: oldValue is a BigInt.
            //     b. Return BigInt::unaryMinus(oldValue).
            try executable.addInstruction(.unary_minus);
        },

        // UnaryExpression : ~ UnaryExpression
        .@"~" => {
            // 1. Let expr be ? Evaluation of UnaryExpression.
            try codegenExpression(node.expression.*, executable, ctx);

            // 2. Let oldValue be ? ToNumeric(? GetValue(expr)).
            if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
            try executable.addInstruction(.to_numeric);

            // 3. If oldValue is a Number, then
            //     a. Return Number::bitwiseNOT(oldValue).
            // 4. Else,
            //     a. Assert: oldValue is a BigInt.
            //     b. Return BigInt::bitwiseNOT(oldValue).
            try executable.addInstruction(.bitwise_not);
        },

        // UnaryExpression : ! UnaryExpression
        .@"!" => {
            // 1. Let expr be ? Evaluation of UnaryExpression.
            try codegenExpression(node.expression.*, executable, ctx);

            // 2. Let oldValue be ToBoolean(? GetValue(expr)).
            if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            // 3. If oldValue is true, return false.
            // 4. Return true.
            try executable.addInstruction(.logical_not);
        },
    }
}

/// 13.15.4 EvaluateStringOrNumericBinaryExpression ( leftOperand, opText, rightOperand )
/// https://tc39.es/ecma262/#sec-evaluatestringornumericbinaryexpression
pub fn codegenBinaryExpression(
    node: ast.BinaryExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // 1. Let lref be ? Evaluation of leftOperand.
    try codegenExpression(node.lhs_expression.*, executable, ctx);

    // 2. Let lval be ? GetValue(lref).
    if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // 3. Let rref be ? Evaluation of rightOperand.
    try codegenExpression(node.rhs_expression.*, executable, ctx);

    // 4. Let rval be ? GetValue(rref).
    if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // 5. Return ? ApplyStringOrNumericBinaryOperator(lval, opText, rval).
    try executable.addInstruction(.apply_string_or_numeric_binary_operator);
    try executable.addIndex(@intFromEnum(node.operator));
}

/// 13.10.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-relational-operators-runtime-semantics-evaluation
pub fn codegenRelationalExpression(
    node: ast.RelationalExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // RelationalExpression : RelationalExpression < ShiftExpression
    // RelationalExpression : RelationalExpression > ShiftExpression
    // RelationalExpression : RelationalExpression <= ShiftExpression
    // RelationalExpression : RelationalExpression >= ShiftExpression
    // RelationalExpression : RelationalExpression instanceof ShiftExpression
    // RelationalExpression : RelationalExpression in ShiftExpression
    // 1. Let lref be ? Evaluation of RelationalExpression.
    try codegenExpression(node.lhs_expression.*, executable, ctx);

    // 2. Let lval be ? GetValue(lref).
    if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // 3. Let rref be ? Evaluation of ShiftExpression.
    try codegenExpression(node.rhs_expression.*, executable, ctx);

    // 4. Let rval be ? GetValue(rref).
    if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    switch (node.operator) {
        .@"<" => try executable.addInstruction(.less_than),
        .@">" => try executable.addInstruction(.greater_than),
        .@"<=" => try executable.addInstruction(.less_than_equals),
        .@">=" => try executable.addInstruction(.greater_than_equals),
        .instanceof => try executable.addInstruction(.instanceof_operator),
        .in => try executable.addInstruction(.has_property),
    }
}

/// 13.11.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-equality-operators-runtime-semantics-evaluation
pub fn codegenEqualityExpression(
    node: ast.EqualityExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // EqualityExpression : EqualityExpression == RelationalExpression
    // EqualityExpression : EqualityExpression != RelationalExpression
    // EqualityExpression : EqualityExpression === RelationalExpression
    // EqualityExpression : EqualityExpression !== RelationalExpression
    // 1. Let lref be ? Evaluation of EqualityExpression.
    try codegenExpression(node.lhs_expression.*, executable, ctx);

    // 2. Let lval be ? GetValue(lref).
    if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    // 3. Let rref be ? Evaluation of RelationalExpression.
    try codegenExpression(node.rhs_expression.*, executable, ctx);

    // 4. Let rval be ? GetValue(rref).
    if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.load);

    switch (node.operator) {
        .@"==" => {
            // 5. Return ? IsLooselyEqual(rval, lval).
            try executable.addInstruction(.is_loosely_equal);
        },
        .@"!=" => {
            // 5. Let r be ? IsLooselyEqual(rval, lval).
            try executable.addInstruction(.is_loosely_equal);

            // 6. If r is true, return false. Otherwise, return true.
            try executable.addInstruction(.logical_not);
        },
        .@"===" => {
            // 5. Return IsStrictlyEqual(rval, lval).
            try executable.addInstruction(.is_strictly_equal);
        },
        .@"!==" => {
            // 5. Let r be IsStrictlyEqual(rval, lval).
            try executable.addInstruction(.is_strictly_equal);

            // 6. If r is true, return false. Otherwise, return true.
            try executable.addInstruction(.logical_not);
        },
    }
}

/// 13.13.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-binary-logical-operators-runtime-semantics-evaluation
pub fn codegenLogicalExpression(
    node: ast.LogicalExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node.operator) {
        // LogicalANDExpression : LogicalANDExpression && BitwiseORExpression
        .@"&&" => {
            // 1. Let lref be ? Evaluation of LogicalANDExpression.
            try codegenExpression(node.lhs_expression.*, executable, ctx);

            // 2. Let lval be ? GetValue(lref).
            if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            // 3. Let lbool be ToBoolean(lval).
            // 4. If lbool is false, return lval.
            try executable.addInstruction(.jump_conditional);
            const consequent_jump = try executable.addJumpIndex();
            const alternate_jump = try executable.addJumpIndex();
            try consequent_jump.setTargetHere();

            // 5. Let rref be ? Evaluation of BitwiseORExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // 6. Return ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            try alternate_jump.setTargetHere();
        },

        // LogicalORExpression : LogicalORExpression || LogicalANDExpression
        .@"||" => {
            // 1. Let lref be ? Evaluation of LogicalORExpression.
            try codegenExpression(node.lhs_expression.*, executable, ctx);

            // 2. Let lval be ? GetValue(lref).
            if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            // 3. Let lbool be ToBoolean(lval).
            // 4. If lbool is true, return lval.
            try executable.addInstruction(.jump_conditional);
            const consequent_jump = try executable.addJumpIndex();
            const alternate_jump = try executable.addJumpIndex();
            try alternate_jump.setTargetHere();

            // 5. Let rref be ? Evaluation of LogicalANDExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // 6. Return ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            try consequent_jump.setTargetHere();
        },

        // CoalesceExpression : CoalesceExpressionHead ?? BitwiseORExpression
        .@"??" => {
            // 1. Let lref be ? Evaluation of CoalesceExpressionHead.
            try codegenExpression(node.lhs_expression.*, executable, ctx);

            // 2. Let lval be ? GetValue(lref).
            if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            try executable.addInstruction(.load);

            try executable.addInstruction(.load);
            try executable.addInstructionWithConstant(.load_constant, .undefined);
            try executable.addInstruction(.is_loosely_equal);

            try executable.addInstruction(.jump_conditional);
            const consequent_jump = try executable.addJumpIndex();
            const alternate_jump = try executable.addJumpIndex();

            // 3. If lval is either undefined or null, then
            try consequent_jump.setTargetHere();
            try executable.addInstruction(.store); // Drop lval from the stack

            // a. Let rref be ? Evaluation of BitwiseORExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // b. Return ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            try executable.addInstruction(.jump);
            const end_jump = try executable.addJumpIndex();

            // 4. Else,
            try alternate_jump.setTargetHere();

            // a. Return lval.
            try executable.addInstruction(.store);

            try end_jump.setTargetHere();
        },
    }
}

/// 13.14.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-conditional-operator-runtime-semantics-evaluation
pub fn codegenConditionalExpression(
    node: ast.ConditionalExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // ConditionalExpression : ShortCircuitExpression ? AssignmentExpression : AssignmentExpression
    // 1. Let lref be ? Evaluation of ShortCircuitExpression.
    try codegenExpression(node.test_expression.*, executable, ctx);

    // 2. Let lval be ToBoolean(? GetValue(lref)).
    if (node.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    try executable.addInstruction(.jump_conditional);
    const consequent_jump = try executable.addJumpIndex();
    const alternate_jump = try executable.addJumpIndex();

    // 3. If lval is true, then
    try consequent_jump.setTargetHere();

    // a. Let trueRef be ? Evaluation of the first AssignmentExpression.
    try codegenExpression(node.consequent_expression.*, executable, ctx);

    // b. Return ? GetValue(trueRef).
    if (node.consequent_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    try executable.addInstruction(.jump);
    const end_jump = try executable.addJumpIndex();

    // 4. Else,
    try alternate_jump.setTargetHere();

    // a. Let falseRef be ? Evaluation of the second AssignmentExpression.
    try codegenExpression(node.alternate_expression.*, executable, ctx);

    // b. Return ? GetValue(falseRef).
    if (node.alternate_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    try end_jump.setTargetHere();
}

/// 13.15.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-assignment-operators-runtime-semantics-evaluation
pub fn codegenAssignmentExpression(
    node: ast.AssignmentExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // AssignmentExpression : LeftHandSideExpression = AssignmentExpression
    if (node.operator == .@"=") {
        // 1. If LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral, then

        // a. Let lref be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.lhs_expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // TODO: b. If IsAnonymousFunctionDefinition(AssignmentExpression) and IsIdentifierRef of
        //          LeftHandSideExpression are both true, then
        if (false) {
            // i. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
        }
        // c. Else,
        else {
            // i. Let rref be ? Evaluation of AssignmentExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // ii. Let rval be ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        }

        // d. Perform ? PutValue(lref, rval).
        // e. Return rval.
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);
    }
    // AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
    else if (node.operator != .@"&&=" and node.operator != .@"||=" and node.operator != .@"??=") {
        // 1. Let lref be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.lhs_expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let lval be ? GetValue(lref).
        if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);

        // 3. Let rref be ? Evaluation of AssignmentExpression.
        try codegenExpression(node.rhs_expression.*, executable, ctx);

        // 4. Let rval be ? GetValue(rref).
        if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);

        // 5. Let assignmentOpText be the source text matched by AssignmentOperator.
        // 6. Let opText be the sequence of Unicode code points associated with assignmentOpText
        //    in the following table:
        const operator: ast.BinaryExpression.Operator = switch (node.operator) {
            .@"*=" => .@"*",
            .@"/=" => .@"/",
            .@"%=" => .@"%",
            .@"+=" => .@"+",
            .@"-=" => .@"-",
            .@"<<=" => .@"<<",
            .@">>=" => .@">>",
            .@">>>=" => .@">>>",
            .@"&=" => .@"&",
            .@"^=" => .@"^",
            .@"|=" => .@"|",
            .@"**=" => .@"**",
            else => unreachable,
        };

        // 7. Let r be ? ApplyStringOrNumericBinaryOperator(lval, opText, rval).
        try executable.addInstruction(.apply_string_or_numeric_binary_operator);
        try executable.addIndex(@intFromEnum(operator));

        // 8. Perform ? PutValue(lref, r).
        // 9. Return r.
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);
    }
    // AssignmentExpression : LeftHandSideExpression &&= AssignmentExpression
    else if (node.operator == .@"&&=") {
        // 1. Let lref be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.lhs_expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let lval be ? GetValue(lref).
        if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        try executable.addInstruction(.load);

        // 3. Let lbool be ToBoolean(lval).
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const alternate_jump = try executable.addJumpIndex();

        try consequent_jump.setTargetHere();

        // TODO: 5. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and IsIdentifierRef
        //          of LeftHandSideExpression is true, then
        if (false) {
            // a. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
        }
        // 6. Else,
        else {
            // a. Let rref be ? Evaluation of AssignmentExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // b. Let rval be ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        }

        // 7. Perform ? PutValue(lref, rval).
        // 8. Return rval.
        try executable.addInstruction(.put_value);

        try executable.addInstruction(.jump);
        const end_jump = try executable.addJumpIndex();

        // 4. If lbool is false, return lval.
        try alternate_jump.setTargetHere();
        try executable.addInstruction(.store); // Restore lval as the result value

        try end_jump.setTargetHere();
        try executable.addInstruction(.pop_reference);
    }
    // AssignmentExpression : LeftHandSideExpression ||= AssignmentExpression
    else if (node.operator == .@"||=") {
        // 1. Let lref be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.lhs_expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let lval be ? GetValue(lref).
        if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // 3. Let lbool be ToBoolean(lval).
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const alternate_jump = try executable.addJumpIndex();

        try alternate_jump.setTargetHere();

        // TODO: 5. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and IsIdentifierRef
        //          of LeftHandSideExpression is true, then
        if (false) {
            // a. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
        }
        // 6. Else,
        else {
            // a. Let rref be ? Evaluation of AssignmentExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // b. Let rval be ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        }

        // 7. Perform ? PutValue(lref, rval).
        // 8. Return rval.
        try executable.addInstruction(.put_value);

        // 4. If lbool is true, return lval.
        try consequent_jump.setTargetHere();

        try executable.addInstruction(.pop_reference);
    }
    // AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
    else if (node.operator == .@"??=") {
        // 1. Let lref be ? Evaluation of LeftHandSideExpression.
        try codegenExpression(node.lhs_expression.*, executable, ctx);
        try executable.addInstruction(.push_reference);

        // 2. Let lval be ? GetValue(lref).
        if (node.lhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        try executable.addInstruction(.load);

        // 3. If lval is neither undefined nor null, return lval.
        try executable.addInstruction(.load);
        try executable.addInstructionWithConstant(.load_constant, .undefined);
        try executable.addInstruction(.is_loosely_equal);

        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const alternate_jump = try executable.addJumpIndex();

        try consequent_jump.setTargetHere();
        try executable.addInstruction(.store); // Drop lval from the stack

        // TODO: 4. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and
        //          IsIdentifierRef of LeftHandSideExpression is true, then
        if (false) {
            // a. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
        }
        // 5. Else,
        else {
            // a. Let rref be ? Evaluation of AssignmentExpression.
            try codegenExpression(node.rhs_expression.*, executable, ctx);

            // b. Let rval be ? GetValue(rref).
            if (node.rhs_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
        }

        // 6. Perform ? PutValue(lref, rval).
        // 7. Return rval.
        try executable.addInstruction(.put_value);

        try executable.addInstruction(.jump);
        const end_jump = try executable.addJumpIndex();

        try alternate_jump.setTargetHere();
        try executable.addInstruction(.store); // Restore lval as the result value

        try end_jump.setTargetHere();
        try executable.addInstruction(.pop_reference);
    } else unreachable;
}

/// 13.16.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-comma-operator-runtime-semantics-evaluation
pub fn codegenSequenceExpression(
    node: ast.SequenceExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // 1. Let lref be ? Evaluation of Expression.
    // 2. Perform ? GetValue(lref).
    // 3. Let rref be ? Evaluation of AssignmentExpression.
    // 4. Return ? GetValue(rref).
    for (node.expressions) |expression| {
        try codegenExpression(expression, executable, ctx);
        if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    }
}

pub fn codegenExpression(
    node: ast.Expression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        .primary_expression => |x| try codegenPrimaryExpression(x, executable, ctx),
        .member_expression => |x| try codegenMemberExpression(x, executable, ctx),
        .super_property => |x| try codegenSuperProperty(x, executable, ctx),
        .meta_property => |x| try codegenMetaProperty(x, executable, ctx),
        .new_expression => |x| try codegenNewExpression(x, executable, ctx),
        .call_expression => |x| try codegenCallExpression(x, executable, ctx),
        .super_call => |x| try codegenSuperCall(x, executable, ctx),
        .import_call => |x| try codegenImportCall(x, executable, ctx),
        .optional_expression => |x| try codegenOptionalExpression(x, executable, ctx),
        .update_expression => |x| try codegenUpdateExpression(x, executable, ctx),
        .unary_expression => |x| try codegenUnaryExpression(x, executable, ctx),
        .binary_expression => |x| try codegenBinaryExpression(x, executable, ctx),
        .relational_expression => |x| try codegenRelationalExpression(x, executable, ctx),
        .equality_expression => |x| try codegenEqualityExpression(x, executable, ctx),
        .logical_expression => |x| try codegenLogicalExpression(x, executable, ctx),
        .conditional_expression => |x| try codegenConditionalExpression(x, executable, ctx),
        .assignment_expression => |x| try codegenAssignmentExpression(x, executable, ctx),
        .sequence_expression => |x| try codegenSequenceExpression(x, executable, ctx),
    }
}

pub fn codegenStatement(
    node: ast.Statement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        // EmptyStatement : ;
        .empty_statement => {
            // 1. Return empty.
        },

        // DebuggerStatement : debugger ;
        .debugger_statement => {
            // 1.If an implementation-defined debugging facility is available and enabled, then
            //     a. Perform an implementation-defined debugging action.
            //     b. Return a new implementation-defined Completion Record.
            // 2. Else,
            //     a. Return empty.
        },

        .block_statement => |x| try codegenBlockStatement(x, executable, ctx),
        .variable_statement => |x| try codegenVariableStatement(x, executable, ctx),
        .expression_statement => |x| try codegenExpressionStatement(x, executable, ctx),
        .if_statement => |x| try codegenIfStatement(x, executable, ctx),
        .breakable_statement => |x| try codegenBreakableStatement(x, executable, ctx),
        .continue_statement => |x| try codegenContinueStatement(x, executable, ctx),
        .break_statement => |x| try codegenBreakStatement(x, executable, ctx),
        .return_statement => |x| try codegenReturnStatement(x, executable, ctx),
        .throw_statement => |x| try codegenThrowStatement(x, executable, ctx),
        .try_statement => |x| try codegenTryStatement(x, executable, ctx),
    }
}

pub fn codegenDeclaration(
    node: ast.Declaration,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        .hoistable_declaration => |x| try codegenHoistableDeclaration(x, executable, ctx),
        .class_declaration => |x| try codegenClassDeclaration(x, executable, ctx),
        .lexical_declaration => |x| try codegenLexicalDeclaration(x, executable, ctx),
    }
}

/// 14.1.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-statement-semantics-runtime-semantics-evaluation
pub fn codegenHoistableDeclaration(
    _: ast.HoistableDeclaration,
    _: *Executable,
    _: *Context,
) Executable.Error!void {
    // HoistableDeclaration :
    //     GeneratorDeclaration
    //     AsyncFunctionDeclaration
    //     AsyncGeneratorDeclaration
    // 1. Return empty.
    // HoistableDeclaration : FunctionDeclaration
    // 1. Return ? Evaluation of FunctionDeclaration.
}

pub fn codegenBreakableStatement(
    node: ast.BreakableStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        .iteration_statement => |iteration_statement| {
            try codegenIterationStatement(iteration_statement, executable, ctx);
        },
    }
}

pub fn codegenBlockStatement(
    node: ast.BlockStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    try codegenBlock(node.block, executable, ctx);
}

/// 14.2.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-block-runtime-semantics-evaluation
pub fn codegenBlock(
    node: ast.Block,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // Block : { }
    if (node.statement_list.items.len == 0) {
        // 1. Return empty.
        return;
    }

    // Block : { StatementList }
    // TODO: 1-4, 6
    // 5. Let blockValue be Completion(Evaluation of StatementList).
    // 7. Return ? blockValue.
    try codegenStatementList(node.statement_list, executable, ctx);
}

pub fn codegenStatementList(
    node: ast.StatementList,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // StatementList : StatementList StatementListItem
    // 1. Let sl be ? Evaluation of StatementList.
    // 2. Let s be Completion(Evaluation of StatementListItem).
    // 3. Return ? UpdateEmpty(s, sl).
    for (node.items) |item| {
        try codegenStatementListItem(item, executable, ctx);
    }
}

pub fn codegenStatementListItem(
    node: ast.StatementListItem,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    if (node == .declaration and node.declaration.* == .hoistable_declaration) {
        switch (node.declaration.hoistable_declaration) {
            inline else => |*function_declaration| {
                // Assign the function body's strictness, which is needed for the deferred bytecode generation.
                // FIXME: This should ideally happen at parse time.
                const strict = ctx.contained_in_strict_mode_code or
                    function_declaration.function_body.functionBodyContainsUseStrict();
                function_declaration.function_body.strict = strict;
            },
        }
    }

    switch (node) {
        .statement => |statement| try codegenStatement(statement.*, executable, ctx),
        .declaration => |declaration| try codegenDeclaration(declaration.*, executable, ctx),
    }
}

/// 14.3.1.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-let-and-const-declarations-runtime-semantics-evaluation
pub fn codegenLexicalDeclaration(
    node: ast.LexicalDeclaration,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // LexicalDeclaration : LetOrConst BindingList ;
    // 1. Perform ? Evaluation of BindingList.
    try codegenBindingList(node.binding_list, executable, ctx);

    // 2. Return empty.
}

/// 14.3.1.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-let-and-const-declarations-runtime-semantics-evaluation
pub fn codegenBindingList(
    node: ast.BindingList,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // BindingList : BindingList , LexicalBinding
    // 1. Perform ? Evaluation of BindingList.
    // 2. Return ? Evaluation of LexicalBinding.
    for (node.items) |lexical_binding| {
        try codegenLexicalBinding(lexical_binding, executable, ctx);
    }
}

/// 14.3.1.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-let-and-const-declarations-runtime-semantics-evaluation
pub fn codegenLexicalBinding(
    node: ast.LexicalBinding,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // TODO: Implement this properly, we just codegen a VariableDeclaration for now
    const variable_declaration = ast.VariableDeclaration{
        .binding_identifier = node.binding_identifier,
        .initializer = node.initializer,
    };
    try codegenVariableDeclaration(variable_declaration, executable, ctx);
}

/// 14.3.2.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-variable-statement-runtime-semantics-evaluation
pub fn codegenVariableStatement(
    node: ast.VariableStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // VariableStatement : var VariableDeclarationList ;
    // 1. Perform ? Evaluation of VariableDeclarationList.
    try codegenVariableDeclarationList(node.variable_declaration_list, executable, ctx);

    // 2. Return empty.
}

/// 14.3.2.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-variable-statement-runtime-semantics-evaluation
pub fn codegenVariableDeclarationList(
    node: ast.VariableDeclarationList,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // VariableDeclarationList : VariableDeclarationList , VariableDeclaration
    // 1. Perform ? Evaluation of VariableDeclarationList.
    // 2. Return ? Evaluation of VariableDeclaration.
    for (node.items) |variable_declaration| {
        try codegenVariableDeclaration(variable_declaration, executable, ctx);
    }
}

/// 14.3.2.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-variable-statement-runtime-semantics-evaluation
pub fn codegenVariableDeclaration(
    node: ast.VariableDeclaration,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // VariableDeclaration : BindingIdentifier Initializer
    if (node.initializer) |initializer| {
        // 1. Let bindingId be StringValue of BindingIdentifier.
        // 2. Let lhs be ? ResolveBinding(bindingId).
        try executable.addInstruction(.load);
        try executable.addInstructionWithIdentifier(.resolve_binding, node.binding_identifier);
        const strict = ctx.contained_in_strict_mode_code;
        try executable.addIndex(@intFromBool(strict));
        try executable.addIndex(ctx.environment_lookup_cache_index);
        ctx.environment_lookup_cache_index += 1;
        try executable.addInstruction(.push_reference);

        // TODO: 3. If IsAnonymousFunctionDefinition(Initializer) is true, then
        // 4. Else,

        // a. Let rhs be ? Evaluation of Initializer.
        try codegenExpression(initializer, executable, ctx);

        // b. Let value be ? GetValue(rhs).
        // FIXME: This clobbers the result value and we don't have a good way of restoring it.
        //        Should probably use the stack more and have explicit result store instructions.
        if (node.initializer.?.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // 5. Perform ? PutValue(lhs, value).
        try executable.addInstruction(.put_value);
        try executable.addInstruction(.pop_reference);

        // 6. Return empty.
        try executable.addInstruction(.store);
    }
    // VariableDeclaration : BindingIdentifier
    else {
        // 1. Return empty.
    }
}

/// 14.5.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-expression-statement-runtime-semantics-evaluation
pub fn codegenExpressionStatement(
    node: ast.ExpressionStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // ExpressionStatement : Expression ;
    // 1. Let exprRef be ? Evaluation of Expression.
    try codegenExpression(node.expression, executable, ctx);

    // 2. Return ? GetValue(exprRef).
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
}

/// 14.6.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-if-statement-runtime-semantics-evaluation
pub fn codegenIfStatement(
    node: ast.IfStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // 1. Let exprRef be ? Evaluation of Expression.
    try codegenExpression(node.test_expression, executable, ctx);

    // 2. Let exprValue be ToBoolean(? GetValue(exprRef)).
    if (node.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    try executable.addInstruction(.jump_conditional);
    const consequent_jump = try executable.addJumpIndex();
    const alternate_jump = try executable.addJumpIndex();

    // 3. If exprValue is true, then
    try consequent_jump.setTargetHere();
    try executable.addInstructionWithConstant(.store_constant, .undefined);

    // a. Let stmtCompletion be Completion(Evaluation of the first Statement).
    try codegenStatement(node.consequent_statement.*, executable, ctx);
    try executable.addInstruction(.jump);
    const end_jump = try executable.addJumpIndex();

    // 4. Else,
    try alternate_jump.setTargetHere();
    try executable.addInstructionWithConstant(.store_constant, .undefined);

    if (node.alternate_statement) |alternate_statement| {
        // a. Let stmtCompletion be Completion(Evaluation of the second Statement).
        try codegenStatement(alternate_statement.*, executable, ctx);
    }

    // 5. Return ? UpdateEmpty(stmtCompletion, undefined).
    // NOTE: This is handled by the store_constant before the consequent/alternate statements.

    try end_jump.setTargetHere();
}

/// 14.7.1.2 Runtime Semantics: LoopEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-loopevaluation
pub fn codegenIterationStatement(
    node: ast.IterationStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        // IterationStatement : DoWhileStatement
        .do_while_statement => |do_while_statement| {
            // 1. Return ? DoWhileLoopEvaluation of DoWhileStatement with argument labelSet.
            try codegenDoWhileStatement(do_while_statement, executable, ctx);
        },

        // IterationStatement : WhileStatement
        .while_statement => |while_statement| {
            // 1. Return ? WhileLoopEvaluation of WhileStatement with argument labelSet.
            try codegenWhileStatement(while_statement, executable, ctx);
        },

        // IterationStatement : ForStatement
        .for_statement => |for_statement| {
            // 1. Return ? ForLoopEvaluation of ForStatement with argument labelSet.
            try codegenForStatement(for_statement, executable, ctx);
        },

        // IterationStatement : ForInOfStatement
        .for_in_of_statement => |for_in_of_statement| {
            // 1. Return ? ForInOfLoopEvaluation of ForInOfStatement with argument labelSet.
            try codegenForInOfStatement(for_in_of_statement, executable, ctx);
        },
    }
}

/// 14.7.2.2 Runtime Semantics: DoWhileLoopEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-dowhileloopevaluation
pub fn codegenDoWhileStatement(
    node: ast.DoWhileStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // DoWhileStatement : do Statement while ( Expression ) ;
    // 1. Let V be undefined.
    try executable.addInstructionWithConstant(.load_constant, .undefined);

    // 2. Repeat,
    const start_index = executable.instructions.items.len;

    // a. Let stmtResult be Completion(Evaluation of Statement).
    // b. If LoopContinues(stmtResult, labelSet) is false, return ? UpdateEmpty(stmtResult, V).
    try executable.addInstruction(.store);
    try codegenStatement(node.consequent_statement.*, executable, ctx);
    const continue_index = executable.instructions.items.len;
    try executable.addInstruction(.load);

    // c. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
    // NOTE: This is done by the store/load sequence around each consequent execution.

    // d. Let exprRef be ? Evaluation of Expression.
    try codegenExpression(node.test_expression, executable, ctx);

    // e. Let exprValue be ? GetValue(exprRef).
    if (node.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    // f. If ToBoolean(exprValue) is false, return V.
    try executable.addInstruction(.jump_conditional);
    const consequent_jump = try executable.addJumpIndex();
    const end_jump = try executable.addJumpIndex();

    try consequent_jump.setTarget(start_index);

    try end_jump.setTargetHere();
    try executable.addInstruction(.store);

    while (ctx.continue_jumps.popOrNull()) |jump_index| {
        try jump_index.setTarget(continue_index);
    }
    while (ctx.break_jumps.popOrNull()) |jump_index| {
        try jump_index.setTargetHere();
    }
}

/// 14.7.3.2 Runtime Semantics: WhileLoopEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-whileloopevaluation
pub fn codegenWhileStatement(
    node: ast.WhileStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // WhileStatement : while ( Expression ) Statement
    // 1. Let V be undefined.
    try executable.addInstructionWithConstant(.load_constant, .undefined);

    // 2. Repeat,
    const start_index = executable.instructions.items.len;

    // a. Let exprRef be ? Evaluation of Expression.
    try codegenExpression(node.test_expression, executable, ctx);

    // b. Let exprValue be ? GetValue(exprRef).
    if (node.test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    // c. If ToBoolean(exprValue) is false, return V.
    try executable.addInstruction(.jump_conditional);
    const consequent_jump = try executable.addJumpIndex();
    const end_jump = try executable.addJumpIndex();

    // d. Let stmtResult be Completion(Evaluation of Statement).
    // e. If LoopContinues(stmtResult, labelSet) is false, return ? UpdateEmpty(stmtResult, V).
    try consequent_jump.setTargetHere();
    try executable.addInstruction(.store);
    try codegenStatement(node.consequent_statement.*, executable, ctx);
    const continue_index = executable.instructions.items.len;
    try executable.addInstruction(.load);

    try executable.addInstruction(.jump);
    const start_jump = try executable.addJumpIndex();
    try start_jump.setTarget(start_index);

    // f. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
    // NOTE: This is done by the store/load sequence around each consequent execution.

    try end_jump.setTargetHere();
    try executable.addInstruction(.store);

    while (ctx.continue_jumps.popOrNull()) |jump_index| {
        try jump_index.setTarget(continue_index);
    }
    while (ctx.break_jumps.popOrNull()) |jump_index| {
        try jump_index.setTargetHere();
    }
}

/// 14.7.4.2 Runtime Semantics: ForLoopEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-forloopevaluation
/// 14.7.4.3 ForBodyEvaluation ( test, increment, stmt, perIterationBindings, labelSet )
/// https://tc39.es/ecma262/#sec-forbodyevaluation
pub fn codegenForStatement(
    node: ast.ForStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    if (node.initializer) |initializer| switch (initializer) {
        // ForStatement : for ( Expression[opt] ; Expression[opt] ; Expression[opt] ) Statement
        .expression => |expression| {
            // 1. If the first Expression is present, then
            //     a. Let exprRef be ? Evaluation of the first Expression.
            //     b. Perform ? GetValue(exprRef).
            try codegenExpression(expression, executable, ctx);
            if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

            // 2. If the second Expression is present, let test be the second Expression;
            //    otherwise, let test be empty.
            // 3. If the third Expression is present, let increment be the third Expression;
            //    otherwise, let increment be empty.
            // 4. Return ? ForBodyEvaluation(test, increment, Statement, « », labelSet).
        },

        // ForStatement : for ( var VariableDeclarationList ; Expression[opt] ; Expression[opt] ) Statement
        .variable_statement => |variable_statement| {
            // 1. Perform ? Evaluation of VariableDeclarationList.
            try codegenVariableStatement(variable_statement, executable, ctx);

            // 2. If the first Expression is present, let test be the first Expression;
            //    otherwise, let test be empty.
            // 3. If the second Expression is present, let increment be the second Expression;
            //    otherwise, let increment be empty.
            // 4. Return ? ForBodyEvaluation(test, increment, Statement, « », labelSet).
        },

        // ForStatement : for ( LexicalDeclaration Expression[opt] ; Expression[opt] ) Statement
        .lexical_declaration => |lexical_declaration| {
            // TODO: Implement this fully once lexical declarations behave different than var decls
            try codegenLexicalDeclaration(lexical_declaration, executable, ctx);
        },
    };

    // 1. Let V be undefined.
    try executable.addInstructionWithConstant(.load_constant, .undefined);

    // TODO: 2. Perform ? CreatePerIterationEnvironment(perIterationBindings).

    // 3. Repeat,
    const start_index = executable.instructions.items.len;

    var end_jump: Executable.JumpIndex = undefined;

    // a. If test is not empty, then
    if (node.test_expression) |test_expression| {
        // i. Let testRef be ? Evaluation of test.
        try codegenExpression(test_expression, executable, ctx);

        // ii. Let testValue be ? GetValue(testRef).
        if (test_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // iii. If ToBoolean(testValue) is false, return V.
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        end_jump = try executable.addJumpIndex();
        try consequent_jump.setTargetHere();
    }

    // b. Let result be Completion(Evaluation of stmt).
    // c. If LoopContinues(result, labelSet) is false, return ? UpdateEmpty(result, V).
    try executable.addInstruction(.store);
    try codegenStatement(node.consequent_statement.*, executable, ctx);
    const continue_index = executable.instructions.items.len;
    try executable.addInstruction(.load);

    // d. If result.[[Value]] is not empty, set V to result.[[Value]].
    // NOTE: This is done by the store/load sequence around each consequent execution.

    // TODO: e. Perform ? CreatePerIterationEnvironment(perIterationBindings).

    // f. If increment is not empty, then
    if (node.increment_expression) |increment_expression| {
        // i. Let incRef be ? Evaluation of increment.
        try codegenExpression(increment_expression, executable, ctx);

        // ii. Perform ? GetValue(incRef).
        if (increment_expression.analyze(.is_reference)) try executable.addInstruction(.get_value);
    }

    try executable.addInstruction(.jump);
    const start_jump = try executable.addJumpIndex();
    try start_jump.setTarget(start_index);

    if (node.test_expression != null) try end_jump.setTargetHere();
    try executable.addInstruction(.store);

    while (ctx.continue_jumps.popOrNull()) |jump_index| {
        try jump_index.setTarget(continue_index);
    }
    while (ctx.break_jumps.popOrNull()) |jump_index| {
        try jump_index.setTargetHere();
    }
}

const ForInOfIterationKind = enum { enumerate, iterate, async_iterate };
const ForInOfLhsKind = enum { assignment, var_binding, lexical_binding };

/// 14.7.5.5 Runtime Semantics: ForInOfLoopEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-forinofloopevaluation
pub fn codegenForInOfStatement(
    node: ast.ForInOfStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const iteration_kind: ForInOfIterationKind = switch (node.type) {
        .in => .enumerate,
        .of => .iterate,
        .async_of => .async_iterate,
    };
    const lhs_kind: ForInOfLhsKind = switch (node.initializer) {
        .expression => .assignment,
        .for_binding => .var_binding,
        .for_declaration => .lexical_binding,
    };
    const break_jump = try forInOfHeadEvaluation(
        executable,
        ctx,
        node.expression,
        iteration_kind,
    );
    try forInOfBodyEvaluation(
        executable,
        ctx,
        node.initializer,
        node.consequent_statement.*,
        break_jump,
        iteration_kind,
        lhs_kind,
        null,
    );
}

/// 14.7.5.6 ForIn/OfHeadEvaluation ( uninitializedBoundNames, expr, iterationKind )
/// https://tc39.es/ecma262/#sec-runtime-semantics-forinofheadevaluation
fn forInOfHeadEvaluation(
    executable: *Executable,
    ctx: *Context,
    expression: ast.Expression,
    iteration_kind: ForInOfIterationKind,
) Executable.Error!?Executable.JumpIndex {
    // TODO: 1-2.

    // 3. Let exprRef be Completion(Evaluation of expr).
    try codegenExpression(expression, executable, ctx);

    // TODO: 4. Set the running execution context's LexicalEnvironment to oldEnv.

    // 5. Let exprValue be ? GetValue(? exprRef).
    if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    // 6. If iterationKind is enumerate, then
    if (iteration_kind == .enumerate) {
        try executable.addInstruction(.load); // Store RHS object for the iterator

        // a. If exprValue is either undefined or null, then
        //     i. Return Completion Record { [[Type]]: break, [[Value]]: empty, [[Target]]: empty }.
        try executable.addInstruction(.load);
        try executable.addInstructionWithConstant(.load_constant, .undefined);
        try executable.addInstruction(.is_loosely_equal);
        try executable.addInstruction(.jump_conditional);
        const consequent_jump = try executable.addJumpIndex();
        const alternate_jump = try executable.addJumpIndex();
        try alternate_jump.setTargetHere();

        // b. Let obj be ! ToObject(exprValue).
        // c. Let iterator be EnumerateObjectProperties(obj).
        // d. Let nextMethod be ! GetV(iterator, "next").
        // e. Return the Iterator Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false }.
        try executable.addInstruction(.store);
        try executable.addInstruction(.create_object_property_iterator);

        return consequent_jump;
    }
    // 7. Else,
    else {
        // a. Assert: iterationKind is either iterate or async-iterate.
        std.debug.assert(iteration_kind == .iterate or iteration_kind == .async_iterate);

        // b. If iterationKind is async-iterate, let iteratorKind be async.
        // c. Else, let iteratorKind be sync.
        const iterator_kind: IteratorKind = if (iteration_kind == .async_iterate)
            .@"async"
        else
            .sync;

        // d. Return ? GetIterator(exprValue, iteratorKind).
        try executable.addInstruction(.get_iterator);
        try executable.addIndex(@intFromEnum(iterator_kind));

        return null;
    }
}

/// 14.7.5.7 ForIn/OfBodyEvaluation ( lhs, stmt, iteratorRecord, iterationKind, lhsKind, labelSet [ , iteratorKind ] )
/// https://tc39.es/ecma262/#sec-runtime-semantics-forin-div-ofbodyevaluation-lhs-stmt-iterator-lhskind-labelset
fn forInOfBodyEvaluation(
    executable: *Executable,
    ctx: *Context,
    lhs: ast.ForInOfStatement.Initializer,
    statement: ast.Statement,
    break_jump: ?Executable.JumpIndex,
    iteration_kind: ForInOfIterationKind,
    lhs_kind: ForInOfLhsKind,
    // TODO: label_set
    maybe_iterator_kind: ?IteratorKind,
) Executable.Error!void {
    _ = iteration_kind;
    // NOTE: The iterator is created by forInOfHeadEvaluation() and needs to be pushed onto the
    //       stack. If this isn't applicable a break completion jump to the end of this function
    //       is emitted.
    try executable.addInstruction(.push_iterator);

    // 1. If iteratorKind is not present, set iteratorKind to sync.
    const iterator_kind = maybe_iterator_kind orelse .sync;
    _ = iterator_kind;

    // TODO: 2. Let oldEnv be the running execution context's LexicalEnvironment.

    // 3. Let V be undefined.
    try executable.addInstructionWithConstant(.load_constant, .undefined);

    // TODO: 4. Let destructuring be IsDestructuring of lhs.
    const destructuring = false;

    // 5. If destructuring is true and lhsKind is assignment, then
    if (destructuring and lhs_kind == .assignment) {
        // TODO: a-b.
    }

    // 6. Repeat,
    const start_index = executable.instructions.items.len;

    // a. Let nextResult be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
    try executable.addInstruction(.load_iterator_next_args);
    try executable.addInstruction(.evaluate_call);
    try executable.addIndex(0); // No arguments
    try executable.addIndex(0); // Strictness doesn't matter here

    // TODO: b. If iteratorKind is async, set nextResult to ? Await(nextResult).
    // TODO: c. If nextResult is not an Object, throw a TypeError exception.

    // Store result object on the stack for later `.value` access
    try executable.addInstruction(.load);

    // d. Let done be ? IteratorComplete(nextResult).
    try executable.addInstruction(.load);
    try executable.addInstructionWithIdentifier(
        .evaluate_property_access_with_identifier_key,
        "done",
    );
    try executable.addIndex(0); // Strictness doesn't matter here
    try executable.addInstruction(.get_value);

    // e. If done is true, return V.
    try executable.addInstruction(.jump_conditional);
    const consequent_jump = try executable.addJumpIndex();
    const alternate_jump = try executable.addJumpIndex();

    try alternate_jump.setTargetHere();

    // f. Let nextValue be ? IteratorValue(nextResult).
    try executable.addInstructionWithIdentifier(
        .evaluate_property_access_with_identifier_key,
        "value",
    );
    try executable.addIndex(0); // Strictness doesn't matter here
    try executable.addInstruction(.get_value);

    // g. If lhsKind is either assignment or var-binding, then
    if (lhs_kind == .assignment or lhs_kind == .var_binding) {
        // i. If destructuring is true, then
        if (destructuring) {
            // 1. If lhsKind is assignment, then
            if (lhs_kind == .assignment) {
                // TODO: a. Let status be Completion(DestructuringAssignmentEvaluation of
                //          assignmentPattern with argument nextValue).
            }
            // 2. Else,
            else {
                // a. Assert: lhsKind is var-binding.
                std.debug.assert(lhs_kind == .var_binding);

                // b. Assert: lhs is a ForBinding.
                std.debug.assert(lhs == .for_binding);

                // TODO: c. Let status be Completion(BindingInitialization of lhs with arguments
                //          nextValue and undefined).
            }
        }
        // ii. Else,
        else {
            // 1. Let lhsRef be Completion(Evaluation of lhs). (It may be evaluated repeatedly.)
            switch (lhs) {
                .expression => |expression| try codegenExpression(expression, executable, ctx),
                .for_binding => |identifier| try codegenIdentifierReference(identifier, executable, ctx),
                .for_declaration => unreachable,
            }

            // TODO: 2. If lhsRef is an abrupt completion, then
            //     a. Let status be lhsRef.
            // 3. Else,
            //     a. Let status be Completion(PutValue(lhsRef.[[Value]], nextValue)).
            try executable.addInstruction(.push_reference);
            try executable.addInstruction(.put_value);
            try executable.addInstruction(.pop_reference);
        }
    }
    // h. Else,
    else {
        // i. Assert: lhsKind is lexical-binding.
        std.debug.assert(lhs_kind == .lexical_binding);

        // ii. Assert: lhs is a ForDeclaration.
        std.debug.assert(lhs == .for_declaration);

        // TODO: iii-v.

        // vi. If destructuring is true, then
        if (destructuring) {
            // TODO: 1. Let status be Completion(ForDeclarationBindingInitialization of lhs with
            //          arguments nextValue and iterationEnv).
        }
        // vii. Else,
        else {
            // 1. Assert: lhs binds a single name.
            // 2. Let lhsName be the sole element of BoundNames of lhs.
            const lhs_name = lhs.for_declaration.binding_list.items[0].binding_identifier;

            // TODO: 3. Let lhsRef be ! ResolveBinding(lhsName).
            // TODO: 4. Let status be Completion(InitializeReferencedBinding(lhsRef, nextValue)).
            try codegenIdentifierReference(lhs_name, executable, ctx);
            try executable.addInstruction(.push_reference);
            try executable.addInstruction(.put_value);
            try executable.addInstruction(.pop_reference);
        }
    }

    // TODO: i. If status is an abrupt completion, then

    // j. Let result be Completion(Evaluation of stmt).
    try executable.addInstruction(.store);
    try codegenStatement(statement, executable, ctx);
    const continue_index = executable.instructions.items.len;
    try executable.addInstruction(.load);

    // TODO: k. Set the running execution context's LexicalEnvironment to oldEnv.
    // TODO: l. If LoopContinues(result, labelSet) is false, then

    // m. If result.[[Value]] is not empty, set V to result.[[Value]].
    // NOTE: This is done by the store/load sequence around each consequent execution.

    try executable.addInstruction(.jump);
    try executable.addIndex(start_index);

    try consequent_jump.setTargetHere();

    try executable.addInstruction(.store); // Pop last iterator result object

    try executable.addInstruction(.store);

    while (ctx.continue_jumps.popOrNull()) |jump_index| {
        try jump_index.setTarget(continue_index);
    }
    while (ctx.break_jumps.popOrNull()) |jump_index| {
        try jump_index.setTargetHere();
    }

    // TODO: We should probably also clean this up if something throws beforehand...
    try executable.addInstruction(.pop_iterator);

    if (break_jump) |jump_index| {
        try executable.addInstruction(.jump);
        const skip_break_jump = try executable.addJumpIndex();

        // If this is for a for-in loop and the RHS is nullish we jump here and clean up the result
        // value that was created for the jump_conditional instruction.
        try jump_index.setTargetHere();
        try executable.addInstruction(.store); // Pop RHS object
        try executable.addInstructionWithConstant(.store_constant, .undefined);

        try skip_break_jump.setTargetHere();
    }
}

/// 14.8.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-continue-statement-runtime-semantics-evaluation
pub fn codegenContinueStatement(
    node: ast.ContinueStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // TODO: ContinueStatement : continue LabelIdentifier ;
    if (node.label) |label| {
        _ = label;
        // 1. Let label be the StringValue of LabelIdentifier.
        // 2. Return Completion Record { [[Type]]: continue, [[Value]]: empty, [[Target]]: label }.
        try executable.addInstruction(.jump);
        const jump_index = try executable.addJumpIndex();
        try ctx.continue_jumps.append(jump_index);
    }
    // ContinueStatement : continue ;
    else {
        // 1. Return Completion Record { [[Type]]: continue, [[Value]]: empty, [[Target]]: empty }.
        try executable.addInstruction(.jump);
        const jump_index = try executable.addJumpIndex();
        try ctx.continue_jumps.append(jump_index);
    }
}

/// 14.9.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-break-statement-runtime-semantics-evaluation
pub fn codegenBreakStatement(
    node: ast.BreakStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // TODO: BreakStatement : break LabelIdentifier ;
    if (node.label) |label| {
        _ = label;
        // 1. Let label be the StringValue of LabelIdentifier.
        // 2. Return Completion Record { [[Type]]: break, [[Value]]: empty, [[Target]]: label }.
        try executable.addInstruction(.jump);
        const jump_index = try executable.addJumpIndex();
        try ctx.break_jumps.append(jump_index);
    }
    // BreakStatement : break ;
    else {
        // 1. Return Completion Record { [[Type]]: break, [[Value]]: empty, [[Target]]: empty }.
        try executable.addInstruction(.jump);
        const jump_index = try executable.addJumpIndex();
        try ctx.break_jumps.append(jump_index);
    }
}

/// 14.10.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-return-statement-runtime-semantics-evaluation
pub fn codegenReturnStatement(
    node: ast.ReturnStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // ReturnStatement : return Expression ;
    if (node.expression) |expression| {
        // 1. Let exprRef be ? Evaluation of Expression.
        try codegenExpression(expression, executable, ctx);

        // 2. Let exprValue be ? GetValue(exprRef).
        if (expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

        // TODO: 3. If GetGeneratorKind() is async, set exprValue to ? Await(exprValue).

        // 4. Return Completion Record { [[Type]]: return, [[Value]]: exprValue, [[Target]]: empty }.
        try executable.addInstruction(.@"return");
    }
    // ReturnStatement : return ;
    else {
        // 1. Return Completion Record { [[Type]]: return, [[Value]]: undefined, [[Target]]: empty }.
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try executable.addInstruction(.@"return");
    }
}

/// 14.14.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-throw-statement-runtime-semantics-evaluation
pub fn codegenThrowStatement(
    node: ast.ThrowStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // ThrowStatement : throw Expression ;
    // 1. Let exprRef be ? Evaluation of Expression.
    try codegenExpression(node.expression, executable, ctx);

    // 2. Let exprValue be ? GetValue(exprRef).
    if (node.expression.analyze(.is_reference)) try executable.addInstruction(.get_value);

    // 3. Return ThrowCompletion(exprValue).
    try executable.addInstruction(.throw);
}

/// 14.15.3 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-try-statement-runtime-semantics-evaluation
pub fn codegenTryStatement(
    node: ast.TryStatement,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // TryStatement : try Block Catch
    if (node.finally_block == null) {
        try executable.addInstruction(.push_exception_jump_target);
        const exception_jump_to_catch = try executable.addJumpIndex();

        // 1. Let B be Completion(Evaluation of Block).
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.try_block, executable, ctx);
        try executable.addInstruction(.pop_exception_jump_target);
        try executable.addInstruction(.jump);
        const end_jump = try executable.addJumpIndex();

        // 2. If B is a throw completion, let C be Completion(CatchClauseEvaluation of Catch with
        //    argument B.[[Value]]).
        // TODO: Create a new lexical environment
        try exception_jump_to_catch.setTargetHere();
        try executable.addInstruction(.pop_exception_jump_target);
        if (node.catch_parameter) |catch_parameter| try executable.addInstructionWithIdentifier(
            .create_catch_binding,
            catch_parameter,
        );
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.catch_block.?, executable, ctx);

        // 3. Else, let C be B.
        // 4. Return ? UpdateEmpty(C, undefined).
        try end_jump.setTargetHere();
    }
    // TryStatement : try Block Finally
    else if (node.catch_block == null) {
        try executable.addInstruction(.push_exception_jump_target);
        const exception_jump_to_finally = try executable.addJumpIndex();

        // 1. Let B be Completion(Evaluation of Block).
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.try_block, executable, ctx);

        // 2. Let F be Completion(Evaluation of Finally).
        try exception_jump_to_finally.setTargetHere();
        try executable.addInstruction(.pop_exception_jump_target);
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.finally_block.?, executable, ctx);
        try executable.addInstruction(.rethrow_exception_if_any);

        // 3. If F is a normal completion, set F to B.
        // 4. Return ? UpdateEmpty(F, undefined).
    }
    // TryStatement : try Block Catch Finally
    else {
        try executable.addInstruction(.push_exception_jump_target);
        const exception_jump_to_catch = try executable.addJumpIndex();

        // 1. Let B be Completion(Evaluation of Block).
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.try_block, executable, ctx);
        try executable.addInstruction(.jump);
        const finally_jump = try executable.addJumpIndex();

        // 2. If B is a throw completion, let C be Completion(CatchClauseEvaluation of Catch with argument B.[[Value]]).
        // 3. Else, let C be B.
        // TODO: Create a new lexical environment
        try exception_jump_to_catch.setTargetHere();
        try executable.addInstruction(.pop_exception_jump_target);
        try executable.addInstruction(.push_exception_jump_target);
        const exception_jump_to_finally = try executable.addJumpIndex();
        if (node.catch_parameter) |catch_parameter| try executable.addInstructionWithIdentifier(
            .create_catch_binding,
            catch_parameter,
        );
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.catch_block.?, executable, ctx);

        // 4. Let F be Completion(Evaluation of Finally).
        try finally_jump.setTargetHere();
        try exception_jump_to_finally.setTargetHere();
        try executable.addInstruction(.pop_exception_jump_target);
        try executable.addInstructionWithConstant(.store_constant, .undefined);
        try codegenBlock(node.finally_block.?, executable, ctx);
        try executable.addInstruction(.rethrow_exception_if_any);

        // 5. If F is a normal completion, set F to C.
        // 6. Return ? UpdateEmpty(F, undefined).
    }
}

/// 15.2.6 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-function-definitions-runtime-semantics-evaluation
pub fn codegenFunctionExpression(
    node: ast.FunctionExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or node.function_body.functionBodyContainsUseStrict();

    // Copy `node` so that we can assign the function body's strictness, which is needed for the
    // deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var function_expression = node;
    function_expression.function_body.strict = strict;

    // 1. Return InstantiateOrdinaryFunctionExpression of FunctionExpression.
    try executable.addInstructionWithFunctionOrClass(
        .instantiate_ordinary_function_expression,
        .{ .function_expression = function_expression },
    );
}

pub fn codegenFunctionBody(
    node: ast.FunctionBody,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const tmp = temporaryChange(&ctx.contained_in_strict_mode_code, node.strict.?);
    defer tmp.restore();
    try codegenStatementList(node.statement_list, executable, ctx);
}

/// 15.3.5 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-function-definitions-runtime-semantics-evaluation
pub fn codegenArrowFunction(
    node: ast.ArrowFunction,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or node.function_body.functionBodyContainsUseStrict();

    // Copy `node` so that we can assign the function body's strictness, which is needed for the
    // deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var arrow_function = node;
    arrow_function.function_body.strict = strict;

    // 1. Return InstantiateArrowFunctionExpression of ArrowFunction.
    try executable.addInstructionWithFunctionOrClass(
        .instantiate_arrow_function_expression,
        .{ .arrow_function = arrow_function },
    );
}

/// 15.4.5 Runtime Semantics: MethodDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-methoddefinitionevaluation
pub fn codegenMethodDefinition(
    node: ast.MethodDefinition,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or switch (node.method) {
        inline else => |expression| expression.function_body.functionBodyContainsUseStrict(),
    };

    // Copy `method` so that we can assign the function body's strictness,
    // which is needed for the deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var method = node.method;
    switch (method) {
        inline else => |*expression| expression.function_body.strict = strict,
    }

    try codegenPropertyName(node.property_name, executable, ctx);
    try executable.addInstruction(.load);

    try executable.addInstructionWithFunctionOrClass(
        .object_define_method,
        switch (method) {
            .method, .get, .set => |function_expression| .{ .function_expression = function_expression },
            .generator => |generator_expression| .{ .generator_expression = generator_expression },
            .@"async" => |async_function_expression| .{ .async_function_expression = async_function_expression },
            .async_generator => |async_generator_expression| .{ .async_generator_expression = async_generator_expression },
        },
    );
    try executable.addIndex(@intFromEnum(std.meta.activeTag(node.method)));
}

/// 15.5.5 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-generator-function-definitions-runtime-semantics-evaluation
pub fn codegenGeneratorExpression(
    node: ast.GeneratorExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or node.function_body.functionBodyContainsUseStrict();

    // Copy `node` so that we can assign the function body's strictness, which is needed for the
    // deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var generator_expression = node;
    generator_expression.function_body.strict = strict;

    // 1. Return InstantiateGeneratorFunctionExpression of GeneratorExpression.
    try executable.addInstructionWithFunctionOrClass(
        .instantiate_generator_function_expression,
        .{ .generator_expression = generator_expression },
    );
}

/// 15.6.5 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-asyncgenerator-definitions-evaluation
pub fn codegenAsyncGeneratorExpression(
    node: ast.AsyncGeneratorExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or node.function_body.functionBodyContainsUseStrict();

    // Copy `node` so that we can assign the function body's strictness, which is needed for the
    // deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var async_generator_expression = node;
    async_generator_expression.function_body.strict = strict;

    // 1. Return InstantiateAsyncGeneratorFunctionExpression of AsyncGeneratorExpression.
    try executable.addInstructionWithFunctionOrClass(
        .instantiate_async_generator_function_expression,
        .{ .async_generator_expression = async_generator_expression },
    );
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
pub fn codegenClassDeclaration(
    node: ast.ClassDeclaration,
    executable: *Executable,
    _: *Context,
) Executable.Error!void {
    // ClassDeclaration : class BindingIdentifier ClassTail
    // 1. Perform ? BindingClassDeclarationEvaluation of this ClassDeclaration.
    try executable.addInstruction(.load);
    try executable.addInstructionWithFunctionOrClass(
        .binding_class_declaration_evaluation,
        .{ .class_declaration = node },
    );

    // 2. Return empty.
    try executable.addInstruction(.store);
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
pub fn codegenClassExpression(
    node: ast.ClassExpression,
    executable: *Executable,
    _: *Context,
) Executable.Error!void {
    // ClassExpression : class ClassTail
    // ClassExpression : class BindingIdentifier ClassTail
    try executable.addInstructionWithFunctionOrClass(
        .class_definition_evaluation,
        .{ .class_expression = node },
    );
}

/// 15.8.5 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-async-function-definitions-runtime-semantics-evaluation
pub fn codegenAsyncFunctionExpression(
    node: ast.AsyncFunctionExpression,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or node.function_body.functionBodyContainsUseStrict();

    // Copy `node` so that we can assign the function body's strictness, which is needed for the
    // deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var async_function_expression = node;
    async_function_expression.function_body.strict = strict;

    // 1. Return InstantiateAsyncFunctionExpression of AsyncFunctionExpression.
    try executable.addInstructionWithFunctionOrClass(
        .instantiate_async_function_expression,
        .{ .async_function_expression = async_function_expression },
    );
}

/// 15.9.5 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-async-arrow-function-definitions-runtime-semantics-evaluation
pub fn codegenAsyncArrowFunction(
    node: ast.AsyncArrowFunction,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    const strict = ctx.contained_in_strict_mode_code or node.function_body.functionBodyContainsUseStrict();

    // Copy `node` so that we can assign the function body's strictness, which is needed for the
    // deferred bytecode generation.
    // FIXME: This should ideally happen at parse time.
    var async_arrow_function = node;
    async_arrow_function.function_body.strict = strict;

    // 1. Return InstantiateAsyncArrowFunctionExpression of AsyncArrowFunction.
    try executable.addInstructionWithFunctionOrClass(
        .instantiate_async_arrow_function_expression,
        .{ .async_arrow_function = async_arrow_function },
    );
}

pub fn codegenScript(
    node: ast.Script,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    ctx.contained_in_strict_mode_code = node.isStrict();
    try codegenStatementList(node.statement_list, executable, ctx);
}

pub fn codegenModule(
    node: ast.Module,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    ctx.contained_in_strict_mode_code = true;
    try codegenModuleItemList(node.module_item_list, executable, ctx);
}

/// 16.2.1.11 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-module-semantics-runtime-semantics-evaluation
pub fn codegenModuleItemList(
    node: ast.ModuleItemList,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    // ModuleItemList : ModuleItemList ModuleItem
    // 1. Let sl be ? Evaluation of ModuleItemList.
    // 2. Let s be Completion(Evaluation of ModuleItem).
    // 3. Return ? UpdateEmpty(s, sl).
    for (node.items) |item| {
        try codegenModuleItem(item, executable, ctx);
    }
}

pub fn codegenModuleItem(
    node: ast.ModuleItem,
    executable: *Executable,
    ctx: *Context,
) Executable.Error!void {
    switch (node) {
        .statement_list_item => |statement_list_item| {
            try codegenStatementListItem(statement_list_item, executable, ctx);
        },
        else => {},
    }
}
