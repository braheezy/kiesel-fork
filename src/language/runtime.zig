const std = @import("std");

const ast = @import("ast.zig");
const builtins = @import("../builtins.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ClassConstructorFields = builtins.builtin_function.ClassConstructorFields;
const ClassFieldDefinition = types.ClassFieldDefinition;
const ClassStaticBlockDefinition = types.ClassStaticBlockDefinition;
const Environment = execution.Environment;
const ImportAttribute = language.ImportAttribute;
const ImportedModuleReferrer = language.ImportedModuleReferrer;
const ModuleRequest = language.ModuleRequest;
const Object = types.Object;
const PrivateElement = types.PrivateElement;
const PrivateEnvironment = execution.PrivateEnvironment;
const PrivateMethodDefinition = types.PrivateMethodDefinition;
const PrivateName = types.PrivateName;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const PropertyKeyOrPrivateName = types.PropertyKeyOrPrivateName;
const Reference = types.Reference;
const String = types.String;
const Value = types.Value;
const allImportAttributesSupported = language.allImportAttributesSupported;
const arrayCreate = builtins.arrayCreate;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineMethodProperty = builtins.defineMethodProperty;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const makeClassConstructor = builtins.makeClassConstructor;
const makeConstructor = builtins.makeConstructor;
const makeMethod = builtins.makeMethod;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const newPrivateEnvironment = execution.newPrivateEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryFunctionCreate = builtins.ordinaryFunctionCreate;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const performEval = builtins.performEval;
const setFunctionName = builtins.setFunctionName;

/// 8.6.2.1 InitializeBoundName ( name, value, environment )
/// https://tc39.es/ecma262/#sec-initializeboundname
pub fn initializeBoundName(
    agent: *Agent,
    name: *const String,
    value: Value,
    environment_or_strict: union(enum) {
        environment: Environment,
        strict: bool,
    },
) Agent.Error!void {
    switch (environment_or_strict) {
        // 1. If environment is not undefined, then
        .environment => |environment| {
            // a. Perform ! environment.InitializeBinding(name, value).
            environment.initializeBinding(agent, name, value) catch |err| try noexcept(err);

            // b. Return unused.
        },

        // 2. Else,
        .strict => |strict| {
            // a. Let lhs be ? ResolveBinding(name).
            const lhs = try agent.resolveBinding(name, null, strict, null);

            // b. Return ? PutValue(lhs, value).
            try lhs.putValue(agent, value);
        },
    }
}

/// 13.2.8.4 GetTemplateObject ( templateLiteral )
/// https://tc39.es/ecma262/#sec-gettemplateobject
pub fn getTemplateObject(
    agent: *Agent,
    template_literal: *ast.TemplateLiteral,
) std.mem.Allocator.Error!*Object {
    // 1. Let realm be the current Realm Record.
    const realm = agent.currentRealm();

    // 2. Let templateRegistry be realm.[[TemplateMap]].
    // 3. For each element e of templateRegistry, do
    if (realm.template_map.get(template_literal)) |template| {
        // a. If e.[[Site]] is the same Parse Node as templateLiteral, then
        //     i. Return e.[[Array]].
        return template;
    }

    // 4. Let rawStrings be the TemplateStrings of templateLiteral with argument true.
    // 5. Assert: rawStrings is a List of Strings.
    const raw_strings = try template_literal.templateStrings(agent.gc_allocator, true);
    defer agent.gc_allocator.free(raw_strings);

    // 6. Let cookedStrings be the TemplateStrings of templateLiteral with argument false.
    const cooked_strings = try template_literal.templateStrings(agent.gc_allocator, false);
    defer agent.gc_allocator.free(cooked_strings);

    // 7. Let count be the number of elements in the List cookedStrings.
    // 8. Assert: count ≤ 2**32 - 1.
    const count: u32 = @intCast(cooked_strings.len);

    // 9. Let template be ! ArrayCreate(count).
    const template = arrayCreate(agent, count, null) catch |err| try noexcept(err);

    // 10. Let rawObj be ! ArrayCreate(count).
    const raw_obj = arrayCreate(agent, count, null) catch |err| try noexcept(err);

    // 11. Let index be 0.
    var index: u53 = 0;

    // 12. Repeat, while index < count,
    while (index < count) : (index += 1) {
        // a. Let prop be ! ToString(𝔽(index)).
        const property_key = PropertyKey.from(index);

        // b. Let cookedValue be cookedStrings[index].
        const cooked_value = cooked_strings[@intCast(index)];

        // c. Perform ! DefinePropertyOrThrow(template, prop, PropertyDescriptor {
        //      [[Value]]: cookedValue, [[Writable]]: false, [[Enumerable]]: true, [[Configurable]]: false
        //    }).
        try template.definePropertyDirect(agent, property_key, .{
            .value_or_accessor = .{
                .value = Value.from(cooked_value),
            },
            .attributes = .{
                .writable = false,
                .enumerable = true,
                .configurable = false,
            },
        });

        // d. Let rawValue be the String value rawStrings[index].
        const raw_value = raw_strings[@intCast(index)];

        // e. Perform ! DefinePropertyOrThrow(rawObj, prop, PropertyDescriptor {
        //      [[Value]]: rawValue, [[Writable]]: false, [[Enumerable]]: true, [[Configurable]]: false
        //    }).
        try raw_obj.definePropertyDirect(agent, property_key, .{
            .value_or_accessor = .{
                .value = Value.from(raw_value),
            },
            .attributes = .{
                .writable = false,
                .enumerable = true,
                .configurable = false,
            },
        });

        // f. Set index to index + 1.
    }

    // 13. Perform ! SetIntegrityLevel(rawObj, frozen).
    _ = raw_obj.setIntegrityLevel(agent, .frozen) catch |err| try noexcept(err);

    // 14. Perform ! DefinePropertyOrThrow(template, "raw", PropertyDescriptor {
    //       [[Value]]: rawObj, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
    //     }).
    try template.definePropertyDirect(agent, PropertyKey.from("raw"), .{
        .value_or_accessor = .{
            .value = Value.from(raw_obj),
        },
        .attributes = .none,
    });

    // 15. Perform ! SetIntegrityLevel(template, frozen).
    _ = template.setIntegrityLevel(agent, .frozen) catch |err| try noexcept(err);

    // 16. Append the Record { [[Site]]: templateLiteral, [[Array]]: template } to realm.[[TemplateMap]].
    try realm.template_map.putNoClobber(agent.gc_allocator, template_literal, template);

    // 17. Return template.
    return template;
}

/// 13.3.5.1.1 EvaluateNew ( constructExpr, arguments )
/// https://tc39.es/ecma262/#sec-evaluatenew
pub fn evaluateNew(agent: *Agent, constructor: Value, arguments: []const Value) Agent.Error!Value {
    // 1. Let ref be ? Evaluation of constructExpr.
    // 2. Let constructor be ? GetValue(ref).
    // 3. If arguments is empty, then
    //     a. Let argList be a new empty List.
    // 4. Else,
    //     a. Let argList be ? ArgumentListEvaluation of arguments.
    // NOTE: This is part of the generated bytecode.

    // 5. If IsConstructor(constructor) is false, throw a TypeError exception.
    if (!constructor.isConstructor()) {
        return agent.throwException(.type_error, "{} is not a constructor", .{constructor});
    }

    // 6. Return ? Construct(constructor, argList).
    return Value.from(try constructor.asObject().construct(agent, arguments, null));
}

/// 13.3.6.2 EvaluateCall ( func, ref, arguments, tailPosition )
/// https://tc39.es/ecma262/#sec-evaluatecall
pub fn evaluateCallGetThisValue(reference: Reference) Value {
    // 1. If ref is a Reference Record, then
    // a. If IsPropertyReference(ref) is true, then
    if (reference.isPropertyReference()) {
        // i. Let thisValue be GetThisValue(ref).
        return reference.getThisValue();
    } else {
        // b. Else,
        // i. Let refEnv be ref.[[Base]].
        // ii. Assert: refEnv is an Environment Record.
        const reference_environment = reference.base.environment;

        // iii. Let thisValue be refEnv.WithBaseObject().
        return if (reference_environment.withBaseObject()) |object|
            Value.from(object)
        else
            .undefined;
    }
}

/// 13.3.6.2 EvaluateCall ( func, ref, arguments, tailPosition )
/// https://tc39.es/ecma262/#sec-evaluatecall
pub fn evaluateCall(
    agent: *Agent,
    function: Value,
    this_value: Value,
    arguments: []const Value,
) Agent.Error!Value {
    // 1-2.
    // NOTE: These are done in the function above, which is called by the prepare_call instruction.

    // 3. Let argList be ? ArgumentListEvaluation of arguments.
    // NOTE: This is part of the generated bytecode.

    // 4. If func is not an Object, throw a TypeError exception.
    if (!function.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{function});
    }

    // 5. If IsCallable(func) is false, throw a TypeError exception.
    if (!function.isCallable()) {
        return agent.throwException(.type_error, "{} is not callable", .{function});
    }

    // 6. If tailPosition is true, perform PrepareForTailCall().
    // 7. Return ? Call(func, thisValue, argList).
    return function.callAssumeCallable(agent, this_value, arguments);
}

/// 13.3.7.2 GetSuperConstructor ( )
/// https://tc39.es/ecma262/#sec-getsuperconstructor
pub fn getSuperConstructor(agent: *Agent) std.mem.Allocator.Error!Value {
    // 1. Let envRec be GetThisEnvironment().
    const env = agent.getThisEnvironment();

    // 2. Assert: envRec is a Function Environment Record.
    std.debug.assert(env == .function_environment);

    // 3. Let activeFunction be envRec.[[FunctionObject]].
    // 4. Assert: activeFunction is an ECMAScript function object.
    const active_function = &env.function_environment.function_object.object;

    // 5. Let superConstructor be ! activeFunction.[[GetPrototypeOf]]().
    const super_constructor = active_function.internal_methods.getPrototypeOf(
        agent,
        active_function,
    ) catch |err| try noexcept(err);

    // 6. Return superConstructor.
    return if (super_constructor) |object| Value.from(object) else .null;
}

/// 13.3.10.2 EvaluateImportCall ( specifierExpression [ , optionsExpression ] )
/// https://tc39.es/ecma262/#sec-evaluate-import-call
pub fn evaluateImportCall(agent: *Agent, specifier: Value, options: Value) Agent.Error!Value {
    const realm = agent.currentRealm();

    // 1. Let referrer be GetActiveScriptOrModule().
    // 2. If referrer is null, set referrer to the current Realm Record.
    const referrer: ImportedModuleReferrer = if (agent.getActiveScriptOrModule()) |script_or_module|
        switch (script_or_module) {
            .script => |script| .{ .script = script },
            .module => |module| switch (module) {
                .source_text_module => |source_text_module| .{ .module = source_text_module },
                .synthetic_module => unreachable,
            },
        }
    else
        .{ .realm = realm };

    // 3. Let specifierRef be ? Evaluation of specifierExpression.
    // 4. Let specifier be ? GetValue(specifierRef).
    // 5. If optionsExpression is present, then
    //     a. Let optionsRef be ? Evaluation of optionsExpression.
    //     b. Let options be ? GetValue(optionsRef).
    // 6. Else,
    //     a. Let options be undefined.
    // NOTE: This is part of the generated bytecode.

    // 7. Let promiseCapability be ! NewPromiseCapability(%Promise%).
    const promise_capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 8. Let specifierString be Completion(ToString(specifier)).
    const specifier_string = specifier.toString(agent) catch |err| {
        // 9. IfAbruptRejectPromise(specifierString, promiseCapability).
        return Value.from(try promise_capability.rejectPromise(agent, err));
    };

    // 10. Let attributes be a new empty List.
    var attributes: std.ArrayListUnmanaged(ImportAttribute) = .empty;

    // 11. If options is not undefined, then
    if (!options.isUndefined()) {
        // a. If options is not an Object, then
        if (!options.isObject()) {
            const @"error" = try agent.createErrorObject(
                .type_error,
                "Options value is not an object",
                .{},
            );

            // i. Perform ! Call(promiseCapability.[[Reject]], undefined, « a newly created
            //    TypeError object »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                agent,
                .undefined,
                &.{Value.from(@"error")},
            ) catch |err| try noexcept(err);

            // ii. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // b. Let attributesObj be Completion(Get(options, "with")).
        const attributes_object = options.asObject().get(agent, PropertyKey.from("with")) catch |err| {
            // c. IfAbruptRejectPromise(attributesObj, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // d. If attributesObj is not undefined, then
        if (!attributes_object.isUndefined()) {
            // i. If attributesObj is not an Object, the n
            if (!attributes_object.isObject()) {
                const @"error" = try agent.createErrorObject(
                    .type_error,
                    "'with' property is not an object",
                    .{},
                );

                // 1. Perform ! Call(promiseCapability.[[Reject]], undefined, « a newly created
                //    TypeError object »).
                _ = Value.from(promise_capability.reject).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{Value.from(@"error")},
                ) catch |err| try noexcept(err);

                // 2. Return promiseCapability.[[Promise]].
                return Value.from(promise_capability.promise);
            }

            // ii. Let entries be Completion(EnumerableOwnProperties(attributesObj, key+value)).
            const entries = attributes_object.asObject().enumerableOwnProperties(
                agent,
                .@"key+value",
            ) catch |err| {
                // iii. IfAbruptRejectPromise(entries, promiseCapability).
                return Value.from(try promise_capability.rejectPromise(agent, err));
            };

            // iv. For each element entry of entries, do
            for (entries.items) |entry| {
                // 1. Let key be ! Get(entry, "0").
                const key = entry.get(agent, PropertyKey.from(0)) catch |err| try noexcept(err);

                // 2. Let value be ! Get(entry, "1").
                const value = entry.get(agent, PropertyKey.from(1)) catch |err| try noexcept(err);

                // 3. If key is a String, then
                if (key.isString()) {
                    // a. If value is not a String, then
                    if (!value.isString()) {
                        const @"error" = try agent.createErrorObject(
                            .type_error,
                            "Import attribute '{}' value is not a string",
                            .{key.asString()},
                        );

                        // i. Perform ! Call(promiseCapability.[[Reject]], undefined, « a newly
                        //    created TypeError object »).
                        _ = Value.from(promise_capability.reject).callAssumeCallable(
                            agent,
                            .undefined,
                            &.{Value.from(@"error")},
                        ) catch |err| try noexcept(err);

                        // ii. Return promiseCapability.[[Promise]].
                        return Value.from(promise_capability.promise);
                    }

                    // b. Append the ImportAttribute Record { [[Key]]: key, [[Value]]: value } to
                    //    attributes.
                    try attributes.append(agent.gc_allocator, .{
                        .key = key.asString(),
                        .value = value.asString(),
                    });
                }
            }
        }

        // e. If AllImportAttributesSupported(attributes) is false, then
        if (try allImportAttributesSupported(agent, attributes.items)) |unsupported| {
            const @"error" = try agent.createErrorObject(
                .type_error,
                "Import attribute '{}' is not supported",
                .{unsupported},
            );

            // i. Perform ! Call(promiseCapability.[[Reject]], undefined, « a newly created
            //    TypeError object »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                agent,
                .undefined,
                &.{Value.from(@"error")},
            ) catch |err| try noexcept(err);

            // ii. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // f. Sort attributes according to the lexicographic order of their [[Key]] field, treating the value of each such field as a sequence of UTF-16 code unit values. NOTE: This sorting is observable only in that hosts are prohibited from changing behaviour based on the order in which attributes are enumerated.
    }

    // 12. Let moduleRequest be a new ModuleRequest Record {
    //       [[Specifier]]: specifierString, [[Attributes]]: attributes
    //     }.
    const module_request: ModuleRequest = .{
        .specifier = specifier_string,
        .attributes = try attributes.toOwnedSlice(agent.gc_allocator),
    };

    // 13. Perform HostLoadImportedModule(referrer, moduleRequest, empty, promiseCapability).
    try agent.host_hooks.hostLoadImportedModule(
        agent,
        referrer,
        module_request,
        .null_pointer,
        .{ .promise_capability = promise_capability },
    );

    // 14. Return promiseCapability.[[Promise]].
    return Value.from(promise_capability.promise);
}

/// CallExpression : CoverCallExpressionAndAsyncArrowHead
/// Step 6.a.
pub fn directEval(agent: *Agent, arguments: []const Value, strict: bool) Agent.Error!Value {
    // i. Let argList be ? ArgumentListEvaluation of arguments.
    // NOTE: This is part of the generated bytecode.

    // ii. If argList has no elements, return undefined.
    if (arguments.len == 0) return .undefined;

    // iii. Let evalArg be the first element of argList.
    const eval_arg = arguments[0];

    // iv. If IsStrict(this CallExpression) is true, let strictCaller be true; otherwise let
    //     strictCaller be false.
    const strict_caller = strict;

    // v. Return ? PerformEval(evalArg, strictCaller, true).
    return performEval(agent, eval_arg, strict_caller, true);
}

/// 13.15.3 ApplyStringOrNumericBinaryOperator ( lVal, opText, rVal )
/// https://tc39.es/ecma262/#sec-applystringornumericbinaryoperator
pub fn applyStringOrNumericBinaryOperator(
    agent: *Agent,
    l_val: Value,
    comptime operator: ast.BinaryExpression.Operator,
    r_val: Value,
) Agent.Error!Value {
    var final_l_val = l_val;
    var final_r_val = r_val;

    // 1. If opText is +, then
    if (operator == .@"+") {
        // a. Let lPrim be ? ToPrimitive(lVal).
        const l_prim = try l_val.toPrimitive(agent, null);

        // b. Let rPrim be ? ToPrimitive(rVal).
        const r_prim = try r_val.toPrimitive(agent, null);

        // c. If lPrim is a String or rPrim is a String, then
        if (l_prim.isString() or r_prim.isString()) {
            // i. Let lStr be ? ToString(lPrim).
            const l_str = try l_prim.toString(agent);

            // ii. Let rStr be ? ToString(rPrim).
            const r_str = try r_prim.toString(agent);

            // iii. Return the string-concatenation of lStr and rStr.
            return Value.from(try String.concat(agent, &.{ l_str, r_str }));
        }

        // d. Set lVal to lPrim.
        final_l_val = l_prim;

        // e. Set rVal to rPrim.
        final_r_val = r_prim;
    }

    // 2. NOTE: At this point, it must be a numeric operation.

    // 3. Let lNum be ? ToNumeric(lVal).
    const l_num = try final_l_val.toNumeric(agent);

    // 4. Let rNum be ? ToNumeric(rVal).
    const r_num = try final_r_val.toNumeric(agent);

    // 5. If SameType(lNum, rNum) is false, throw a TypeError exception.
    if (!Value.Numeric.sameType(l_num, r_num)) {
        return agent.throwException(
            .type_error,
            "Left-hand side and right-hand side of numeric binary expression must have the same type",
            .{},
        );
    }

    // 6. If lNum is a BigInt, then
    return if (l_num == .big_int) switch (operator) {
        // a. If opText is **, return ? BigInt::exponentiate(lNum, rNum).
        .@"**" => Value.from(try l_num.big_int.exponentiate(agent, r_num.big_int)),

        // b. If opText is /, return ? BigInt::divide(lNum, rNum).
        .@"/" => Value.from(try l_num.big_int.divide(agent, r_num.big_int)),

        // c. If opText is %, return ? BigInt::remainder(lNum, rNum).
        .@"%" => Value.from(try l_num.big_int.remainder(agent, r_num.big_int)),

        // d. If opText is >>>, return ? BigInt::unsignedRightShift(lNum, rNum).
        .@">>>" => Value.from(try l_num.big_int.unsignedRightShift(agent, r_num.big_int)),

        // e. Let operation be the abstract operation associated with opText in the following table:
        .@"*" => Value.from(try l_num.big_int.multiply(agent, r_num.big_int)),
        .@"+" => Value.from(try l_num.big_int.add(agent, r_num.big_int)),
        .@"-" => Value.from(try l_num.big_int.subtract(agent, r_num.big_int)),
        .@"<<" => Value.from(try l_num.big_int.leftShift(agent, r_num.big_int)),
        .@">>" => Value.from(try l_num.big_int.signedRightShift(agent, r_num.big_int)),
        .@"&" => Value.from(try l_num.big_int.bitwiseAND(agent, r_num.big_int)),
        .@"^" => Value.from(try l_num.big_int.bitwiseXOR(agent, r_num.big_int)),
        .@"|" => Value.from(try l_num.big_int.bitwiseOR(agent, r_num.big_int)),
    }
    // 7. Else,
    else switch (operator) {
        // a. Assert: lNum is a Number.
        // b. Let operation be the abstract operation associated with opText in the following table:
        .@"**" => Value.from(l_num.number.exponentiate(r_num.number)),
        .@"*" => Value.from(l_num.number.multiply(r_num.number)),
        .@"/" => Value.from(l_num.number.divide(r_num.number)),
        .@"%" => Value.from(l_num.number.remainder(r_num.number)),
        .@"+" => Value.from(l_num.number.add(r_num.number)),
        .@"-" => Value.from(l_num.number.subtract(r_num.number)),
        .@"<<" => Value.from(l_num.number.leftShift(r_num.number)),
        .@">>" => Value.from(l_num.number.signedRightShift(r_num.number)),
        .@">>>" => Value.from(l_num.number.unsignedRightShift(r_num.number)),
        .@"&" => Value.from(l_num.number.bitwiseAND(r_num.number)),
        .@"^" => Value.from(l_num.number.bitwiseXOR(r_num.number)),
        .@"|" => Value.from(l_num.number.bitwiseOR(r_num.number)),
    };

    // 8. Return operation(lNum, rNum).
}

pub const BlockDeclarationInstantiationType = enum {
    statement_list,
    case_block,
};

/// 14.2.3 BlockDeclarationInstantiation ( code, env )
/// https://tc39.es/ecma262/#sec-blockdeclarationinstantiation
pub fn blockDeclarationInstantiation(
    agent: *Agent,
    code: union(BlockDeclarationInstantiationType) {
        statement_list: ast.StatementList,
        case_block: ast.CaseBlock,
    },
    env: Environment,
) std.mem.Allocator.Error!void {
    // NOTE: Keeping this wrapped in a generic `Environment` makes a bunch of stuff below easier.
    std.debug.assert(env == .declarative_environment);

    // 1. let declarations be the LexicallyScopedDeclarations of code.
    var declarations: std.ArrayListUnmanaged(ast.LexicallyScopedDeclaration) = .empty;
    defer declarations.deinit(agent.gc_allocator);
    switch (code) {
        .statement_list => |node| try node.collectLexicallyScopedDeclarations(agent.gc_allocator, &declarations),
        .case_block => |node| try node.collectLexicallyScopedDeclarations(agent.gc_allocator, &declarations),
    }

    // 2. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

    var bound_names: std.ArrayListUnmanaged(ast.Identifier) = .empty;
    defer bound_names.deinit(agent.gc_allocator);

    // 3. For each element d of declarations, do
    for (declarations.items) |declaration| {
        bound_names.clearRetainingCapacity();
        try declaration.collectBoundNames(agent.gc_allocator, &bound_names);

        // a. For each element dn of the BoundNames of d, do
        for (bound_names.items) |name_utf8| {
            const name = try String.fromUtf8(agent, name_utf8);

            // i. If IsConstantDeclaration of d is true, then
            if (declaration.isConstantDeclaration()) {
                // 1. Perform ! env.CreateImmutableBinding(dn, true).
                env.createImmutableBinding(agent, name, true) catch |err| try noexcept(err);
            } else {
                // ii. Else,
                // 1. If the host is a web browser or otherwise supports Block-Level Function
                //    Declarations Web Legacy Compatibility Semantics, then
                //     a. If ! env.HasBinding(dn) is false, then
                //         i. Perform ! env.CreateMutableBinding(dn, false).
                // 2. Else,
                //     a. Perform ! env.CreateMutableBinding(dn, false).
                env.createMutableBinding(agent, name, false) catch |err| try noexcept(err);
            }
        }

        // b. If d is either a FunctionDeclaration, a GeneratorDeclaration, an
        //    AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration, then
        if (declaration == .hoistable_declaration) {
            const hoistable_declaration = declaration.hoistable_declaration;

            // i. Let fn be the sole element of the BoundNames of d.
            const function_name = switch (hoistable_declaration) {
                inline else => |function_declaration| try String.fromUtf8(agent, function_declaration.identifier.?),
            };

            // ii. Let fo be InstantiateFunctionObject of d with arguments env and privateEnv.
            const function_object = try switch (hoistable_declaration) {
                .function_declaration => |function_declaration| instantiateOrdinaryFunctionObject(agent, function_declaration, env, private_env),
                .generator_declaration => |generator_declaration| instantiateGeneratorFunctionObject(agent, generator_declaration, env, private_env),
                .async_function_declaration => |async_function_declaration| instantiateAsyncFunctionObject(agent, async_function_declaration, env, private_env),
                .async_generator_declaration => |async_generator_declaration| instantiateAsyncGeneratorFunctionObject(agent, async_generator_declaration, env, private_env),
            };

            // iii. If the host is a web browser or otherwise supports Block-Level Function
            //      Declarations Web Legacy Compatibility Semantics, then
            //     1. If the binding for fn in env is an uninitialized binding, then
            //         a. Perform ! env.InitializeBinding(fn, fo).
            //     2. Else,
            //         a. Assert: d is a FunctionDeclaration.
            //         b. Perform ! env.SetMutableBinding(fn, fo, false).
            // iv. Else,
            //     1. Perform ! env.InitializeBinding(fn, fo).
            env.initializeBinding(
                agent,
                function_name,
                Value.from(function_object),
            ) catch |err| try noexcept(err);
        }
    }

    // 4. Return unused.
}

/// 15.2.4 Runtime Semantics: InstantiateOrdinaryFunctionObject
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionobject
pub fn instantiateOrdinaryFunctionObject(
    agent: *Agent,
    function_declaration: ast.FunctionDeclaration,
    env: Environment,
    private_env: ?*PrivateEnvironment,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
    if (function_declaration.identifier) |identifier| {
        // 1. Let name be the StringValue of BindingIdentifier.
        const name = identifier;

        // 2. Let sourceText be the source text matched by FunctionDeclaration.
        const source_text = function_declaration.source_text;

        // 3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            function_declaration.formal_parameters,
            function_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 4. Perform SetFunctionName(F, name).
        try setFunctionName(
            agent,
            function,
            PropertyKey.from(try String.fromUtf8(agent, name)),
            null,
        );

        // 5. Perform MakeConstructor(F).
        try makeConstructor(agent, function, .{});

        // 6. Return F.
        return function;
    }
    // FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
    // NOTE: An anonymous FunctionDeclaration can only occur as part of an export default
    // declaration, and its function code is therefore always strict mode code.
    else {
        // 1. Let sourceText be the source text matched by FunctionDeclaration.
        const source_text = function_declaration.source_text;

        // 2. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            function_declaration.formal_parameters,
            function_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 3. Perform SetFunctionName(F, "default").
        try setFunctionName(agent, function, PropertyKey.from("default"), null);

        // 4. Perform MakeConstructor(F).
        try makeConstructor(agent, function, .{});

        // 5. Return F.
        return function;
    }
}

/// 15.2.5 Runtime Semantics: InstantiateOrdinaryFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionexpression
pub fn instantiateOrdinaryFunctionExpression(
    agent: *Agent,
    function_expression: ast.FunctionExpression,
    default_name: ?[]const u8,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // FunctionExpression : function BindingIdentifier ( FormalParameters ) { FunctionBody }
    if (function_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 7. Let sourceText be the source text matched by FunctionExpression.
        const source_text = function_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    FormalParameters, FunctionBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            function_expression.formal_parameters,
            function_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 10. Perform MakeConstructor(closure).
        try makeConstructor(agent, closure, .{});

        // 11. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(name, Value.from(closure));

        // 12. Return closure.
        return closure;
    }
    // FunctionExpression : function ( FormalParameters ) { FunctionBody }
    else {
        // 1. If name is not present, set name to "".
        const name: *const String = if (default_name) |name|
            try String.fromUtf8(agent, name)
        else
            .empty;

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 4. Let sourceText be the source text matched by FunctionExpression.
        const source_text = function_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            function_expression.formal_parameters,
            function_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 7. Perform MakeConstructor(closure).
        try makeConstructor(agent, closure, .{});

        // 8. Return closure.
        return closure;
    }
}

/// 15.3.4 Runtime Semantics: InstantiateArrowFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiatearrowfunctionexpression
pub fn instantiateArrowFunctionExpression(
    agent: *Agent,
    arrow_function: ast.ArrowFunction,
    default_name: ?[]const u8,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If name is not present, set name to "".
    const name: *const String = if (default_name) |name|
        try String.fromUtf8(agent, name)
    else
        .empty;

    // 2. Let env be the LexicalEnvironment of the running execution context.
    const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

    // 3. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

    // 4. Let sourceText be the source text matched by ArrowFunction.
    const source_text = arrow_function.source_text;

    // 5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, ArrowParameters,
    //    ConciseBody, lexical-this, env, privateEnv).
    const closure = try ordinaryFunctionCreate(
        agent,
        try realm.intrinsics.@"%Function.prototype%"(),
        source_text,
        arrow_function.formal_parameters,
        arrow_function.function_body,
        .lexical_this,
        env,
        private_env,
    );

    // 6. Perform SetFunctionName(closure, name).
    try setFunctionName(agent, closure, PropertyKey.from(name), null);

    // 7. Return closure.
    return closure;
}

/// 15.4.4 Runtime Semantics: DefineMethod
/// https://tc39.es/ecma262/#sec-runtime-semantics-definemethod
fn defineMethod(
    agent: *Agent,
    function_expression: ast.FunctionExpression,
    property_name: Value,
    object: *Object,
    function_prototype: ?*Object,
) Agent.Error!struct { key: PropertyKeyOrPrivateName, closure: *Object } {
    const realm = agent.currentRealm();

    // 1. Let propKey be ? Evaluation of ClassElementName.
    const property_key: PropertyKeyOrPrivateName = if (property_name.toPrivateName()) |private_name|
        .{ .private_name = private_name }
    else if (property_name.toPropertyKey(agent)) |property_key|
        .{ .property_key = property_key }
    else |err|
        return err;

    // 2. Let env be the running execution context's LexicalEnvironment.
    const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

    // 3. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

    // 4. If functionPrototype is present, then
    //     a. Let prototype be functionPrototype.
    // 5. Else,
    //     a. Let prototype be %Function.prototype%.
    const prototype = function_prototype orelse try realm.intrinsics.@"%Function.prototype%"();

    // 6. Let sourceText be the source text matched by MethodDefinition.
    const source_text = function_expression.source_text;

    // 7. Let closure be OrdinaryFunctionCreate(prototype, sourceText, UniqueFormalParameters,
    //    FunctionBody, non-lexical-this, env, privateEnv).
    const closure = try ordinaryFunctionCreate(
        agent,
        prototype,
        source_text,
        function_expression.formal_parameters,
        function_expression.function_body,
        .non_lexical_this,
        env,
        private_env,
    );

    // 8. Perform MakeMethod(closure, object).
    makeMethod(closure.as(builtins.ECMAScriptFunction), object);

    // 9. Return the Record { [[Key]]: propKey, [[Closure]]: closure }.
    return .{ .key = property_key, .closure = closure };
}

/// 15.4.5 Runtime Semantics: MethodDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-methoddefinitionevaluation
pub fn methodDefinitionEvaluation(
    agent: *Agent,
    // Like ast.MethodDefinition but with an evaluated ast.PropertyName
    method_definition: struct {
        property_name: Value,
        method: ast.MethodDefinition.Method,
    },
    object: *Object,
    enumerable: bool,
) Agent.Error!?PrivateMethodDefinition {
    const realm = agent.currentRealm();
    switch (method_definition.method) {
        // MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
        .method => |function_expression| {
            // 1. Let methodDef be ? DefineMethod of MethodDefinition with argument object.
            const method_def = try defineMethod(
                agent,
                function_expression,
                method_definition.property_name,
                object,
                null,
            );

            // 2. Perform SetFunctionName(methodDef.[[Closure]], methodDef.[[Key]]).
            try setFunctionName(agent, method_def.closure, method_def.key, null);

            // 3. Return ? DefineMethodProperty(object, methodDef.[[Key]], methodDef.[[Closure]], enumerable).
            return defineMethodProperty(
                agent,
                object,
                method_def.key,
                method_def.closure,
                enumerable,
            );
        },

        // MethodDefinition : get ClassElementName ( ) { FunctionBody }
        .get => |function_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

            // 4. Let sourceText be the source text matched by MethodDefinition.
            const source_text = function_expression.source_text;

            // 5. Let formalParameterList be an instance of the production FormalParameters : [empty] .
            const formal_parameter_list: ast.FormalParameters = .{
                .items = &.{},
                .arguments_object_needed = false,
            };

            // 6. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
            //    formalParameterList, FunctionBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%Function.prototype%"(),
                source_text,
                formal_parameter_list,
                function_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 7. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 8. Perform SetFunctionName(closure, propKey, "get").
            try setFunctionName(agent, closure, property_key_or_private_name, "get");

            switch (property_key_or_private_name) {
                // 9. If propKey is a Private Name, then
                .private_name => |private_name| {
                    // a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: accessor, [[Get]]: closure, [[Set]]: undefined }.
                    const private_element: PrivateElement = .{
                        .accessor = .{ .get = closure, .set = null },
                    };
                    return .{ .private_name = private_name, .private_element = private_element };
                },
                // 10. Else,
                .property_key => |property_key| {
                    // a. Let desc be the PropertyDescriptor {
                    //      [[Get]]: closure, [[Enumerable]]: enumerable, [[Configurable]]: true
                    //    }.
                    const property_descriptor: PropertyDescriptor = .{
                        .get = closure,
                        .enumerable = enumerable,
                        .configurable = true,
                    };

                    // b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
                    try object.definePropertyOrThrow(agent, property_key, property_descriptor);

                    // c. Return unused.
                    return null;
                },
            }
        },

        // MethodDefinition : set ClassElementName ( PropertySetParameterList ) { FunctionBody }
        .set => |function_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

            // 4. Let sourceText be the source text matched by MethodDefinition.
            const source_text = function_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
            //    PropertySetParameterList, FunctionBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%Function.prototype%"(),
                source_text,
                function_expression.formal_parameters,
                function_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey, "set").
            try setFunctionName(agent, closure, property_key_or_private_name, "set");

            switch (property_key_or_private_name) {
                // 8. If propKey is a Private Name, then
                .private_name => |private_name| {
                    // a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: accessor, [[Get]]: undefined, [[Set]]: closure }.
                    const private_element: PrivateElement = .{
                        .accessor = .{ .get = null, .set = closure },
                    };
                    return .{ .private_name = private_name, .private_element = private_element };
                },
                // 9. Else,
                .property_key => |property_key| {
                    // a. Let desc be the PropertyDescriptor {
                    //      [[Set]]: closure, [[Enumerable]]: enumerable, [[Configurable]]: true
                    //    }.
                    const property_descriptor: PropertyDescriptor = .{
                        .set = closure,
                        .enumerable = enumerable,
                        .configurable = true,
                    };

                    // b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
                    try object.definePropertyOrThrow(agent, property_key, property_descriptor);

                    // c. Return unused.
                    return null;
                },
            }
        },

        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        .generator => |generator_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

            // 4. Let sourceText be the source text matched by GeneratorMethod.
            const source_text = generator_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
            //    UniqueFormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
                source_text,
                generator_expression.formal_parameters,
                generator_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey).
            try setFunctionName(agent, closure, property_key_or_private_name, null);

            // 8. Let prototype be OrdinaryObjectCreate(%GeneratorPrototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%GeneratorPrototype%"(),
            );

            // 9. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            try closure.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
                .value_or_accessor = .{
                    .value = Value.from(prototype),
                },
                .attributes = .{
                    .writable = true,
                    .enumerable = false,
                    .configurable = false,
                },
            });

            // 10. Return ? DefineMethodProperty(object, propKey, closure, enumerable).
            return defineMethodProperty(
                agent,
                object,
                property_key_or_private_name,
                closure,
                enumerable,
            );
        },

        // AsyncGeneratorMethod : async * ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        .async_generator => |async_generator_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

            // 4. Let sourceText be the source text matched by AsyncGeneratorMethod.
            const source_text = async_generator_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%,
            //    sourceText, UniqueFormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
                source_text,
                async_generator_expression.formal_parameters,
                async_generator_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey).
            try setFunctionName(agent, closure, property_key_or_private_name, null);

            // 8. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorPrototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
            );

            // 9. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            try closure.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
                .value_or_accessor = .{
                    .value = Value.from(prototype),
                },
                .attributes = .{
                    .writable = true,
                    .enumerable = false,
                    .configurable = false,
                },
            });

            // 10. Return ? DefineMethodProperty(object, propKey, closure, enumerable).
            return defineMethodProperty(
                agent,
                object,
                property_key_or_private_name,
                closure,
                enumerable,
            );
        },

        // AsyncMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
        .@"async" => |async_function_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the LexicalEnvironment of the running execution context.
            const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

            // 4. Let sourceText be the source text matched by AsyncMethod.
            const source_text = async_function_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
            //    UniqueFormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncFunction.prototype%"(),
                source_text,
                async_function_expression.formal_parameters,
                async_function_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey).
            try setFunctionName(agent, closure, property_key_or_private_name, null);

            // 8. Return ? DefineMethodProperty(object, propKey, closure, enumerable).
            return defineMethodProperty(
                agent,
                object,
                property_key_or_private_name,
                closure,
                enumerable,
            );
        },
    }
}

/// 15.5.3 Runtime Semantics: InstantiateGeneratorFunctionObject
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiategeneratorfunctionobject
pub fn instantiateGeneratorFunctionObject(
    agent: *Agent,
    generator_declaration: ast.GeneratorDeclaration,
    env: Environment,
    private_env: ?*PrivateEnvironment,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // GeneratorDeclaration : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
    if (generator_declaration.identifier) |identifier| {
        // 1. Let name be the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 2. Let sourceText be the source text matched by GeneratorDeclaration.
        const source_text = generator_declaration.source_text;

        // 3. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
        //    FormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
            source_text,
            generator_declaration.formal_parameters,
            generator_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 4. Perform SetFunctionName(F, name).
        try setFunctionName(agent, function, PropertyKey.from(name), null);

        // 5. Let prototype be OrdinaryObjectCreate(%GeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%GeneratorPrototype%"(),
        );

        // 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        try function.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 7. Return F.
        return function;
    }
    // GeneratorDeclaration : function * ( FormalParameters ) { GeneratorBody }
    // NOTE: An anonymous GeneratorDeclaration can only occur as part of an export default
    // declaration, and its function code is therefore always strict mode code.
    else {
        // 1. Let sourceText be the source text matched by GeneratorDeclaration.
        const source_text = generator_declaration.source_text;

        // 2. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
        //    FormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
            source_text,
            generator_declaration.formal_parameters,
            generator_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 3. Perform SetFunctionName(F, "default").
        try setFunctionName(agent, function, PropertyKey.from("default"), null);

        // 4. Let prototype be OrdinaryObjectCreate(%GeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%GeneratorPrototype%"(),
        );

        // 5. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        try function.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 6. Return F.
        return function;
    }
}

/// 15.5.4 Runtime Semantics: InstantiateGeneratorFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiategeneratorfunctionexpression
pub fn instantiateGeneratorFunctionExpression(
    agent: *Agent,
    generator_expression: ast.GeneratorExpression,
    default_name: ?[]const u8,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // GeneratorExpression : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
    if (generator_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 7. Let sourceText be the source text matched by GeneratorExpression.
        const source_text = generator_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
        //    FormalParameters, GeneratorBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
            source_text,
            generator_expression.formal_parameters,
            generator_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 10. Let prototype be OrdinaryObjectCreate(%GeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%GeneratorPrototype%"(),
        );

        // 11. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //       [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //     }).
        try closure.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 12. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(name, Value.from(closure));

        // 13. Return closure.
        return closure;
    }
    // GeneratorExpression : function * ( FormalParameters ) { GeneratorBody }
    else {
        // 1. If name is not present, set name to "".
        const name: *const String = if (default_name) |name|
            try String.fromUtf8(agent, name)
        else
            .empty;

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 4. Let sourceText be the source text matched by GeneratorExpression.
        const source_text = generator_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
        //    FormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
            source_text,
            generator_expression.formal_parameters,
            generator_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 7. Let prototype be OrdinaryObjectCreate(%GeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%GeneratorPrototype%"(),
        );

        // 8. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        try closure.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 9. Return closure.
        return closure;
    }
}

/// 15.6.3 Runtime Semantics: InstantiateAsyncGeneratorFunctionObject
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncgeneratorfunctionobject
pub fn instantiateAsyncGeneratorFunctionObject(
    agent: *Agent,
    async_generator_declaration: ast.AsyncGeneratorDeclaration,
    env: Environment,
    private_env: ?*PrivateEnvironment,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // AsyncGeneratorDeclaration : async function * BindingIdentifier ( FormalParameters ) { AsyncGeneratorBody }
    if (async_generator_declaration.identifier) |identifier| {
        // 1. Let name be the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 2. Let sourceText be the source text matched by AsyncGeneratorDeclaration.
        const source_text = async_generator_declaration.source_text;

        // 3. Let F be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
        //    FormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
            source_text,
            async_generator_declaration.formal_parameters,
            async_generator_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 4. Perform SetFunctionName(F, name).
        try setFunctionName(agent, function, PropertyKey.from(name), null);

        // 5. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
        );

        // 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        try function.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 7. Return F.
        return function;
    }
    // AsyncGeneratorDeclaration : async function * ( FormalParameters ) { AsyncGeneratorBody }
    // NOTE: An anonymous AsyncGeneratorDeclaration can only occur as part of an `export default` declaration.
    else {
        // 1. Let sourceText be the source text matched by AsyncGeneratorDeclaration.
        const source_text = async_generator_declaration.source_text;

        // 2. Let F be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
        //    FormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
            source_text,
            async_generator_declaration.formal_parameters,
            async_generator_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 3. Perform SetFunctionName(F, "default").
        try setFunctionName(agent, function, PropertyKey.from("default"), null);

        // 4. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
        );

        // 5. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        try function.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 6. Return F.
        return function;
    }
}

/// 15.6.4 Runtime Semantics: InstantiateAsyncGeneratorFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncgeneratorfunctionexpression
pub fn instantiateAsyncGeneratorFunctionExpression(
    agent: *Agent,
    async_generator_expression: ast.AsyncGeneratorExpression,
    default_name: ?[]const u8,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // GeneratorExpression : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
    if (async_generator_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 7. Let sourceText be the source text matched by AsyncGeneratorExpression.
        const source_text = async_generator_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
        //    FormalParameters, AsyncGeneratorBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
            source_text,
            async_generator_expression.formal_parameters,
            async_generator_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 10. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
        );

        // 11. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //       [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //     }).
        try closure.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 12. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(name, Value.from(closure));

        // 13. Return closure.
        return closure;
    }
    // AsyncGeneratorExpression : async function * ( FormalParameters ) { AsyncGeneratorBody }
    else {
        // 1. If name is not present, set name to "".
        const name: *const String = if (default_name) |name|
            try String.fromUtf8(agent, name)
        else
            .empty;

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 4. Let sourceText be the source text matched by AsyncGeneratorExpression.
        const source_text = async_generator_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
        //    FormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
            source_text,
            async_generator_expression.formal_parameters,
            async_generator_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 7. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorPrototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
        );

        // 8. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        try closure.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
            .value_or_accessor = .{
                .value = Value.from(prototype),
            },
            .attributes = .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
        });

        // 9. Return closure.
        return closure;
    }
}

/// 15.7.10 Runtime Semantics: ClassFieldDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-classfielddefinitionevaluation
fn classFieldDefinitionEvaluation(
    agent: *Agent,
    field_definition: ast.FieldDefinition,
    home_object: *Object,
) Agent.Error!ClassFieldDefinition {
    const realm = agent.currentRealm();

    // 1. Let name be ? Evaluation of ClassElementName.
    const name: PropertyKeyOrPrivateName = blk: {
        const value = (try generateAndRunBytecode(
            agent,
            field_definition.class_element_name,
            .{},
        )).value.?;
        if (value.toPrivateName()) |private_name| {
            break :blk .{ .private_name = private_name };
        }
        const property_key = try value.toPropertyKey(agent);
        break :blk .{ .property_key = property_key };
    };

    // 2. If Initializer is present, then
    const initializer = if (field_definition.initializer) |initializer| blk: {
        // a. Let formalParameterList be an instance of the production FormalParameters : [empty] .
        const formal_parameter_list: ast.FormalParameters = .{
            .items = &.{},
            .arguments_object_needed = false,
        };

        // b. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // c. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // d. Let sourceText be the empty sequence of Unicode code points.
        const source_text = "";

        // e. Let initializer be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    formalParameterList, Initializer, non-lexical-this, env, privateEnv).
        const body: ast.FunctionBody = blk_body: {
            const statement = try agent.gc_allocator.create(ast.Statement);
            statement.* = .{ .return_statement = .{ .expression = initializer } };
            const items = try agent.gc_allocator.alloc(ast.StatementListItem, 1);
            items[0] = .{ .statement = statement };
            const statement_list: ast.StatementList = .{ .items = items };
            break :blk_body .{
                .type = .normal,
                .statement_list = statement_list,
                .strict = true,
                .arguments_object_needed = false,
            };
        };
        const initializer_function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            formal_parameter_list,
            body,
            .non_lexical_this,
            env,
            private_env,
        );

        // f. Perform MakeMethod(initializer, homeObject).
        makeMethod(initializer_function.as(builtins.ECMAScriptFunction), home_object);

        // g. Set initializer.[[ClassFieldInitializerName]] to name.
        initializer_function.as(builtins.ECMAScriptFunction).fields.class_field_initializer_name = name;

        break :blk initializer_function;
    } else blk: {
        // 3. Else,
        // a. Let initializer be empty.
        break :blk null;
    };

    // 4. Return the ClassFieldDefinition Record { [[Name]]: name, [[Initializer]]: initializer }.
    return .{
        .name = name,
        .initializer = if (initializer) |i| i.as(builtins.ECMAScriptFunction) else null,
    };
}

/// 15.7.11 Runtime Semantics: ClassStaticBlockDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-classstaticblockdefinitionevaluation
fn classStaticBlockDefinitionEvaluation(
    agent: *Agent,
    class_static_block: ast.ClassStaticBlock,
    home_object: *Object,
) std.mem.Allocator.Error!ClassStaticBlockDefinition {
    const realm = agent.currentRealm();

    // 1. Let lex be the running execution context's LexicalEnvironment.
    const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

    // 2. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

    // 3. Let sourceText be the empty sequence of Unicode code points.
    const source_text = "";

    // 4. Let formalParameters be an instance of the production FormalParameters : [empty] .
    const formal_parameter_list: ast.FormalParameters = .{
        .items = &.{},
        .arguments_object_needed = false,
    };

    // 5. Let bodyFunction be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
    //    formalParameters, ClassStaticBlockBody, non-lexical-this, lex, privateEnv).
    const body: ast.FunctionBody = blk_body: {
        // NOTE: This serves as a replacement for EvaluateClassStaticBlockBody, which invokes
        //       FunctionDeclarationInstantiation before evaluating the statement list.
        break :blk_body .{
            .type = .normal,
            .statement_list = class_static_block.statement_list,
            .strict = true,
            .arguments_object_needed = false,
        };
    };
    const body_function = try ordinaryFunctionCreate(
        agent,
        try realm.intrinsics.@"%Function.prototype%"(),
        source_text,
        formal_parameter_list,
        body,
        .non_lexical_this,
        env,
        private_env,
    );

    // 6. Perform MakeMethod(bodyFunction, homeObject).
    makeMethod(body_function.as(builtins.ECMAScriptFunction), home_object);

    // 7. Return the ClassStaticBlockDefinition Record { [[BodyFunction]]: bodyFunction }.
    return .{ .body_function = body_function.as(builtins.ECMAScriptFunction) };
}

/// 15.7.13 Runtime Semantics: ClassElementEvaluation
/// https://tc39.es/ecma262/#sec-static-semantics-classelementevaluation
fn classElementEvaluation(
    agent: *Agent,
    class_element: ast.ClassElement,
    object: *Object,
) Agent.Error!?union(enum) {
    class_field_definition: ClassFieldDefinition,
    class_static_block_definition: ClassStaticBlockDefinition,
    private_method_definition: PrivateMethodDefinition,
} {
    switch (class_element) {
        // ClassElement :
        //     FieldDefinition ;
        //     static FieldDefinition ;
        .field_definition,
        .static_field_definition,
        => |field_definition| {
            // 1. Return ? ClassFieldDefinitionEvaluation of FieldDefinition with argument object.
            return .{
                .class_field_definition = try classFieldDefinitionEvaluation(
                    agent,
                    field_definition,
                    object,
                ),
            };
        },

        // ClassElement :
        //     MethodDefinition
        //     static MethodDefinition
        .method_definition,
        .static_method_definition,
        => |method_definition| {
            // 1. Return ? MethodDefinitionEvaluation of MethodDefinition with arguments object and false.
            const property_name = (try generateAndRunBytecode(
                agent,
                method_definition.class_element_name,
                .{},
            )).value.?;
            if (try methodDefinitionEvaluation(
                agent,
                .{ .property_name = property_name, .method = method_definition.method },
                object,
                false,
            )) |private_method_definition|
                return .{ .private_method_definition = private_method_definition }
            else
                return null;
        },

        // ClassElement : ClassStaticBlock
        .class_static_block => |class_static_block| {
            // 1. Return ClassStaticBlockDefinitionEvaluation of ClassStaticBlock with argument object.
            return .{
                .class_static_block_definition = try classStaticBlockDefinitionEvaluation(
                    agent,
                    class_static_block,
                    object,
                ),
            };
        },

        // ClassElement : ;
        .empty_statement => {
            // 1. Return unused.
            return null;
        },
    }
}

/// 15.7.14 Runtime Semantics: ClassDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-classdefinitionevaluation
pub fn classDefinitionEvaluation(
    agent: *Agent,
    class_tail: ast.ClassTail,
    class_binding: ?*const String,
    class_name: *const String,
    source_text: []const u8,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Let env be the LexicalEnvironment of the running execution context.
    const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

    // 2. Let classEnv be NewDeclarativeEnvironment(env).
    const class_env = try newDeclarativeEnvironment(agent.gc_allocator, env);

    // 3. If classBinding is not undefined, then
    if (class_binding != null) {
        // a. Perform ! classEnv.CreateImmutableBinding(classBinding, true).
        class_env.createImmutableBinding(
            agent,
            class_binding.?,
            true,
        ) catch |err| try noexcept(err);
    }

    // 4. Let outerPrivateEnvironment be the running execution context's PrivateEnvironment.
    const outer_private_environment = agent.runningExecutionContext().ecmascript_code.private_environment;

    // 5. Let classPrivateEnvironment be NewPrivateEnvironment(outerPrivateEnvironment).
    const class_private_environment = try newPrivateEnvironment(
        agent.gc_allocator,
        outer_private_environment,
    );

    // 6. If ClassBody is present, then
    if (class_tail.class_body.class_element_list.items.len != 0) {
        const private_bound_identifiers = try class_tail.class_body.privateBoundIdentifiers(agent.gc_allocator);
        defer agent.gc_allocator.free(private_bound_identifiers);

        // a. For each String dn of the PrivateBoundIdentifiers of ClassBody, do
        for (private_bound_identifiers) |declared_name| {
            // i. If classPrivateEnvironment.[[Names]] contains a Private Name pn such that pn.[[Description]] is dn, then
            if (class_private_environment.names.contains(declared_name)) {
                // 1. Assert: This is only possible for getter/setter pairs.
            } else {
                // ii. Else,
                // 1. Let name be a new Private Name whose [[Description]] is dn.
                const description = try String.fromUtf8(agent, declared_name);
                const name = try PrivateName.init(agent.gc_allocator, description);

                // 2. Append name to classPrivateEnvironment.[[Names]].
                try class_private_environment.names.putNoClobber(agent.gc_allocator, declared_name, name);
            }
        }
    }

    var prototype_parent: ?*Object = undefined;
    var constructor_parent: *Object = undefined;

    // 7. If ClassHeritage is not present, then
    if (class_tail.class_heritage == null) {
        // a. Let protoParent be %Object.prototype%.
        prototype_parent = try realm.intrinsics.@"%Object.prototype%"();

        // b. Let constructorParent be %Function.prototype%.
        constructor_parent = try realm.intrinsics.@"%Function.prototype%"();
    } else {
        // 8. Else,
        // a. Set the running execution context's LexicalEnvironment to classEnv.
        agent.runningExecutionContext().ecmascript_code.lexical_environment = .{ .declarative_environment = class_env };

        // b. NOTE: The running execution context's PrivateEnvironment is outerPrivateEnvironment
        //    when evaluating ClassHeritage.
        // c. Let superclassRef be Completion(Evaluation of ClassHeritage).
        const superclass_ref = generateAndRunBytecode(
            agent,
            ast.ExpressionStatement{ .expression = class_tail.class_heritage.?.* },
            .{},
        );

        // d. Set the running execution context's LexicalEnvironment to env.
        agent.runningExecutionContext().ecmascript_code.lexical_environment = env;

        // e. Let superclass be ? GetValue(? superclassRef).
        // NOTE: Wrapping the Expression node in a ExpressionStatement above ensures a get_value
        //       instruction is emitted for references.
        const superclass = (try superclass_ref).value.?;

        // f. If superclass is null, then
        if (superclass.isNull()) {
            // i. Let protoParent be null.
            prototype_parent = null;

            // ii. Let constructorParent be %Function.prototype%.
            constructor_parent = try realm.intrinsics.@"%Function.prototype%"();
        }
        // g. Else if IsConstructor(superclass) is false, then
        else if (!superclass.isConstructor()) {
            // i. Throw a TypeError exception.
            return agent.throwException(.type_error, "{} is not a constructor", .{superclass});
        } else {
            // h. Else,
            // i. Let protoParent be ? Get(superclass, "prototype").
            const prototype_parent_value = try superclass.asObject().get(agent, PropertyKey.from("prototype"));

            // ii. If protoParent is not an Object and protoParent is not null, throw a TypeError
            //     exception.
            if (!prototype_parent_value.isObject() and !prototype_parent_value.isNull()) {
                return agent.throwException(
                    .type_error,
                    "{} is not an Object or null",
                    .{prototype_parent_value},
                );
            }

            prototype_parent = if (prototype_parent_value.isObject())
                prototype_parent_value.asObject()
            else
                null;

            // iii. Let constructorParent be superclass.
            constructor_parent = superclass.asObject();
        }
    }

    // 9. Let proto be OrdinaryObjectCreate(protoParent).
    const prototype = try ordinaryObjectCreate(agent, prototype_parent);

    // 10. If ClassBody is not present, let constructor be empty.
    // 11. Else, let constructor be the ConstructorMethod of ClassBody.
    const constructor = class_tail.class_body.constructorMethod();

    // 12. Set the running execution context's LexicalEnvironment to classEnv.
    agent.runningExecutionContext().ecmascript_code.lexical_environment = .{ .declarative_environment = class_env };

    // 13. Set the running execution context's PrivateEnvironment to classPrivateEnvironment.
    agent.runningExecutionContext().ecmascript_code.private_environment = class_private_environment;

    // 14. If constructor is empty, then
    var function = if (constructor == null) blk: {
        // a. Let defaultConstructor be a new Abstract Closure with no parameters that captures
        //    nothing and performs the following steps when called:
        const default_constructor = struct {
            fn func(agent_: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
                // i. Let args be the List of arguments that was passed to this function by [[Call]]
                //    or [[Construct]].
                const args = arguments.values;

                // ii. If NewTarget is undefined, throw a TypeError exception.
                if (new_target == null) {
                    return agent_.throwException(.type_error, "Class must be constructed with 'new'", .{});
                }

                // iii. Let F be the active function object.
                const function = agent_.activeFunctionObject();
                const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);

                // iv. If F.[[ConstructorKind]] is derived, then
                var result = if (class_constructor_fields.constructor_kind == .derived) blk: {
                    // 1. NOTE: This branch behaves similarly to constructor(...args) { super(...args); }.
                    //    The most notable distinction is that while the aforementioned ECMAScript
                    //    source text observably calls the %Symbol.iterator% method on %Array.prototype%,
                    //    this function does not.

                    // 2. Let func be ! F.[[GetPrototypeOf]]().
                    const prototype_function = function.internal_methods.getPrototypeOf(
                        agent_,
                        function,
                    ) catch |err| try noexcept(err);

                    // 3. If IsConstructor(func) is false, throw a TypeError exception.
                    if (prototype_function == null or !Value.from(prototype_function.?).isConstructor()) {
                        return agent_.throwException(
                            .type_error,
                            "{} is not a constructor",
                            .{
                                if (prototype_function == null)
                                    Value.undefined
                                else
                                    Value.from(prototype_function.?),
                            },
                        );
                    }

                    // 4. Let result be ? Construct(func, args, NewTarget).
                    break :blk try prototype_function.?.construct(agent_, args, new_target.?);
                } else blk: {
                    // v. Else,
                    // 1. NOTE: This branch behaves similarly to constructor() {}.
                    // 2. Let result be ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
                    break :blk try ordinaryCreateFromConstructor(
                        builtins.Object,
                        agent_,
                        new_target.?,
                        "%Object.prototype%",
                        {},
                    );
                };

                // vi. Perform ? InitializeInstanceElements(result, F).
                try result.initializeInstanceElements(agent_, function);

                // vii. Return NormalCompletion(result).
                return Value.from(result);
            }
        }.func;

        // b. Let F be CreateBuiltinFunction(defaultConstructor, 0, className, « [[ConstructorKind]],
        //    [[SourceText]] », the current Realm Record, constructorParent).
        const class_constructor_fields = try agent.gc_allocator.create(ClassConstructorFields);
        const function = try createBuiltinFunction(
            agent,
            .{ .constructor = default_constructor },
            0,
            null,
            .{
                .realm = agent.currentRealm(),
                .prototype = constructor_parent,
                .additional_fields = .make(*ClassConstructorFields, class_constructor_fields),
            },
        );
        try setFunctionName(agent, function, PropertyKey.from(class_name), null);
        break :blk function;
    } else blk: {
        // 15. Else,
        // a. Let constructorInfo be ! DefineMethod of constructor with arguments proto and
        //    constructorParent.
        const constructor_info = defineMethod(
            agent,
            constructor.?.method.method,
            Value.from("constructor"),
            prototype,
            constructor_parent,
        ) catch |err| try noexcept(err);

        // b. Let F be constructorInfo.[[Closure]].
        const function = constructor_info.closure;

        // c. Perform MakeClassConstructor(F).
        makeClassConstructor(function.as(builtins.ECMAScriptFunction));

        // d. Perform SetFunctionName(F, className).
        try setFunctionName(
            agent,
            function,
            PropertyKey.from(class_name),
            null,
        );

        break :blk function;
    };

    // FIXME: The spec sets [[SourceText]] after calling ClassDefinitionEvaluation, which doesn't
    //        work for static blocks. See https://github.com/tc39/ecma262/issues/2669.
    if (function.is(builtins.ECMAScriptFunction)) {
        function.as(builtins.ECMAScriptFunction).fields.source_text = source_text;
    } else if (function.is(builtins.BuiltinFunction)) {
        const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        class_constructor_fields.source_text = source_text;
    } else unreachable;

    // 16. Perform MakeConstructor(F, false, proto).
    try makeConstructor(agent, function, .{ .writable_prototype = false, .prototype = prototype });

    // 17. If ClassHeritage is present, set F.[[ConstructorKind]] to derived.
    if (class_tail.class_heritage != null) {
        if (function.is(builtins.ECMAScriptFunction)) {
            function.as(builtins.ECMAScriptFunction).fields.constructor_kind = .derived;
        } else if (function.is(builtins.BuiltinFunction)) {
            const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
            class_constructor_fields.constructor_kind = .derived;
        } else unreachable;
    }

    // 18. Perform ! DefineMethodProperty(proto, "constructor", F, false).
    _ = defineMethodProperty(
        agent,
        prototype,
        .{ .property_key = PropertyKey.from("constructor") },
        function,
        false,
    ) catch |err| try noexcept(err);

    // 19. If ClassBody[opt] is not present, let elements be a new empty List.
    // 20. Else, let elements be the NonConstructorElements of ClassBody.
    const elements = try class_tail.class_body.nonConstructorElements(agent.gc_allocator);
    defer agent.gc_allocator.free(elements);

    // 21. Let instancePrivateMethods be a new empty List.
    var instance_private_methods: std.ArrayListUnmanaged(PrivateMethodDefinition) = .empty;
    // Converted to owned slice, no `deinit()` needed

    // 22. Let staticPrivateMethods be a new empty List.
    var static_private_methods: std.ArrayListUnmanaged(PrivateMethodDefinition) = .empty;
    defer static_private_methods.deinit(agent.gc_allocator);

    // 23. Let instanceFields be a new empty List.
    var instance_fields: std.ArrayListUnmanaged(ClassFieldDefinition) = .empty;
    // Converted to owned slice, no `deinit()` needed

    // 24. Let staticElements be a new empty List.
    var static_elements: std.ArrayListUnmanaged(union(enum) {
        class_field_definition: ClassFieldDefinition,
        class_static_block_definition: ClassStaticBlockDefinition,
    }) = .empty;
    defer static_elements.deinit(agent.gc_allocator);

    // 25. For each ClassElement e of elements, do
    for (elements) |class_element| {
        // a. If IsStatic of e is false, then
        const element_or_error = if (!class_element.isStatic()) blk: {
            // i. Let element be Completion(ClassElementEvaluation of e with argument proto).
            break :blk classElementEvaluation(agent, class_element, prototype);
        } else blk: {
            // b. Else,
            // i. Let element be Completion(ClassElementEvaluation of e with argument F).
            break :blk classElementEvaluation(agent, class_element, function);
        };

        // c. If element is an abrupt completion, then
        // d. Set element to ! element.
        const element = element_or_error catch |err| {
            // i. Set the running execution context's LexicalEnvironment to env.
            agent.runningExecutionContext().ecmascript_code.lexical_environment = env;

            // ii. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
            agent.runningExecutionContext().ecmascript_code.private_environment = outer_private_environment;

            // iii. Return ? element.
            return err;
        };

        if (element != null) switch (element.?) {
            // e. If element is a PrivateElement, then
            .private_method_definition => |private_method_definition| {
                // i. Assert: element.[[Kind]] is either method or accessor.
                std.debug.assert(
                    private_method_definition.private_element == .method or
                        private_method_definition.private_element == .accessor,
                );

                // ii. If IsStatic of e is false, let container be instancePrivateMethods.
                // iii. Else, let container be staticPrivateMethods.
                const container = if (!class_element.isStatic())
                    &instance_private_methods
                else
                    &static_private_methods;

                // iv. If container contains a PrivateElement pe such that pe.[[Key]] is element.[[Key]], then
                if (for (container.items, 0..) |p, index| {
                    if (p.private_name.eql(private_method_definition.private_name)) {
                        break .{ .private_element = p.private_element, .index = index };
                    }
                } else null) |found| {
                    // 1. Assert: element.[[Kind]] and pe.[[Kind]] are both accessor.
                    std.debug.assert(
                        private_method_definition.private_element == .accessor and
                            found.private_element == .accessor,
                    );

                    var combined = private_method_definition;

                    // 2. If element.[[Get]] is undefined, then
                    if (private_method_definition.private_element.accessor.get == null) {
                        // a. Let combined be PrivateElement {
                        //      [[Key]]: element.[[Key]], [[Kind]]: accessor, [[Get]]: pe.[[Get]], [[Set]]: element.[[Set]]
                        //    }.
                        combined.private_element.accessor.get = found.private_element.accessor.get;
                    } else {
                        // 3. Else,
                        // a. Let combined be PrivateElement {
                        //      [[Key]]: element.[[Key]], [[Kind]]: accessor, [[Get]]: element.[[Get]], [[Set]]: pe.[[Set]]
                        //    }.
                        combined.private_element.accessor.set = found.private_element.accessor.set;
                    }

                    // 4. Replace pe in container with combined.
                    container.items[found.index] = combined;
                } else {
                    // v. Else,
                    // 1. Append element to container.
                    try container.append(agent.gc_allocator, private_method_definition);
                }
            },

            // f. Else if element is a ClassFieldDefinition Record, then
            .class_field_definition => |class_field_definition| {
                // i. If IsStatic of e is false, append element to instanceFields.
                // ii. Else, append element to staticElements.
                if (!class_element.isStatic())
                    try instance_fields.append(agent.gc_allocator, class_field_definition)
                else
                    try static_elements.append(
                        agent.gc_allocator,
                        .{ .class_field_definition = class_field_definition },
                    );
            },

            // g. Else if element is a ClassStaticBlockDefinition Record, then
            .class_static_block_definition => |class_static_block_definition| {
                // i. Append element to staticElements.
                try static_elements.append(
                    agent.gc_allocator,
                    .{ .class_static_block_definition = class_static_block_definition },
                );
            },
        };
    }

    // 26. Set the running execution context's LexicalEnvironment to env.
    agent.runningExecutionContext().ecmascript_code.lexical_environment = env;

    // 27. If classBinding is not undefined, then
    if (class_binding != null) {
        // a. Perform ! classEnv.InitializeBinding(classBinding, F).
        class_env.initializeBinding(class_binding.?, Value.from(function));
    }

    // 28. Set F.[[PrivateMethods]] to instancePrivateMethods.
    if (function.is(builtins.ECMAScriptFunction)) {
        function.as(builtins.ECMAScriptFunction).fields.private_methods = try instance_private_methods.toOwnedSlice(agent.gc_allocator);
    } else if (function.is(builtins.BuiltinFunction)) {
        const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        class_constructor_fields.private_methods = try instance_private_methods.toOwnedSlice(agent.gc_allocator);
    } else unreachable;

    // 29. Set F.[[Fields]] to instanceFields.
    if (function.is(builtins.ECMAScriptFunction)) {
        function.as(builtins.ECMAScriptFunction).fields.fields = try instance_fields.toOwnedSlice(agent.gc_allocator);
    } else if (function.is(builtins.BuiltinFunction)) {
        const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        class_constructor_fields.fields = try instance_fields.toOwnedSlice(agent.gc_allocator);
    } else unreachable;

    // 30. For each PrivateElement method of staticPrivateMethods, do
    for (static_private_methods.items) |method| {
        // a. Perform ! PrivateMethodOrAccessorAdd(F, method).
        function.privateMethodOrAccessorAdd(
            agent,
            method.private_name,
            method.private_element,
        ) catch |err| try noexcept(err);
    }

    // 31. For each element elementRecord of staticElements, do
    for (static_elements.items) |element| {
        const result = switch (element) {
            // a. If elementRecord is a ClassFieldDefinition Record, then
            .class_field_definition => |class_field_definition| blk: {
                // i. Let result be Completion(DefineField(F, elementRecord)).
                break :blk function.defineField(agent, class_field_definition);
            },
            // b. Else,
            .class_static_block_definition => |class_static_block_definition| blk: {
                // i. Assert: elementRecord is a ClassStaticBlockDefinition Record.
                // ii. Let result be Completion(Call(elementRecord.[[BodyFunction]], F)).
                const body_function = &class_static_block_definition.body_function.object;
                _ = Value.from(body_function).callAssumeCallable(
                    agent,
                    Value.from(function),
                    &.{},
                ) catch |err| break :blk err;
            },
        };

        // c. If result is an abrupt completion, then
        _ = result catch |err| {
            // i. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
            agent.runningExecutionContext().ecmascript_code.private_environment = outer_private_environment;

            // ii. Return ? result.
            return err;
        };
    }

    // 32. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
    agent.runningExecutionContext().ecmascript_code.private_environment = outer_private_environment;

    // 33. Return F.
    return function;
}

/// 15.7.15 Runtime Semantics: BindingClassDeclarationEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-bindingclassdeclarationevaluation
pub fn bindingClassDeclarationEvaluation(
    agent: *Agent,
    class_declaration: ast.ClassDeclaration,
) Agent.Error!*Object {
    // ClassDeclaration : class BindingIdentifier ClassTail
    if (class_declaration.identifier) |identifier| {
        // 1. Let className be the StringValue of BindingIdentifier.
        const class_name = try String.fromUtf8(agent, identifier);

        // 2. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments className and className.
        // 3. Set value.[[SourceText]] to the source text matched by ClassDeclaration.
        const value = try classDefinitionEvaluation(
            agent,
            class_declaration.class_tail,
            class_name,
            class_name,
            class_declaration.source_text,
        );

        // 4. Let env be the running execution context's LexicalEnvironment.
        const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 5. Perform ? InitializeBoundName(className, value, env).
        try initializeBoundName(agent, class_name, Value.from(value), .{ .environment = env });

        // 6. Return value.
        return value;
    }
    // ClassDeclaration : class ClassTail
    else {
        // 1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and
        //    "default".
        // 2. Set value.[[SourceText]] to the source text matched by ClassDeclaration.
        // 3. Return value.
        return classDefinitionEvaluation(
            agent,
            class_declaration.class_tail,
            null,
            String.fromLiteral("default"),
            class_declaration.source_text,
        );
    }
}

/// 15.8.2 Runtime Semantics: InstantiateAsyncFunctionObject
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncfunctionobject
pub fn instantiateAsyncFunctionObject(
    agent: *Agent,
    async_function_declaration: ast.AsyncFunctionDeclaration,
    env: Environment,
    private_env: ?*PrivateEnvironment,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // AsyncFunctionDeclaration : async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
    if (async_function_declaration.identifier) |identifier| {
        // 1. Let name be the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 2. Let sourceText be the source text matched by AsyncFunctionDeclaration.
        const source_text = async_function_declaration.source_text;

        // 3. Let F be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
        //    FormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncFunction.prototype%"(),
            source_text,
            async_function_declaration.formal_parameters,
            async_function_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 4. Perform SetFunctionName(F, name).
        try setFunctionName(agent, function, PropertyKey.from(name), null);

        // 5. Return F.
        return function;
    }
    // AsyncFunctionDeclaration : async function ( FormalParameters ) { AsyncFunctionBody }
    else {
        // 1. Let sourceText be the source text matched by AsyncFunctionDeclaration.
        const source_text = async_function_declaration.source_text;

        // 2. Let F be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
        //    FormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
        const function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncFunction.prototype%"(),
            source_text,
            async_function_declaration.formal_parameters,
            async_function_declaration.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 3. Perform SetFunctionName(F, "default").
        try setFunctionName(agent, function, PropertyKey.from("default"), null);

        // 4. Return F.
        return function;
    }
}

/// 15.8.3 Runtime Semantics: InstantiateAsyncFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncfunctionexpression
pub fn instantiateAsyncFunctionExpression(
    agent: *Agent,
    async_function_expression: ast.AsyncFunctionExpression,
    default_name: ?[]const u8,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // AsyncFunctionExpression : async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
    if (async_function_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to the StringValue of BindingIdentifier.
        const name = try String.fromUtf8(agent, identifier);

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 7. Let sourceText be the source text matched by AsyncFunctionExpression.
        const source_text = async_function_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
        //    FormalParameters, AsyncFunctionBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncFunction.prototype%"(),
            source_text,
            async_function_expression.formal_parameters,
            async_function_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 10. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(name, Value.from(closure));

        // 11. Return closure.
        return closure;
    }
    // AsyncFunctionExpression : async function ( FormalParameters ) { AsyncFunctionBody }
    else {
        // 1. If name is not present, set name to "".
        const name: *const String = if (default_name) |name|
            try String.fromUtf8(agent, name)
        else
            .empty;

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

        // 4. Let sourceText be the source text matched by AsyncFunctionExpression.
        const source_text = async_function_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
        //    FormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncFunction.prototype%"(),
            source_text,
            async_function_expression.formal_parameters,
            async_function_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(agent, closure, PropertyKey.from(name), null);

        // 7. Return closure.
        return closure;
    }
}

/// 15.9.4 Runtime Semantics: InstantiateAsyncArrowFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncarrowfunctionexpression
pub fn instantiateAsyncArrowFunctionExpression(
    agent: *Agent,
    async_arrow_function: ast.AsyncArrowFunction,
    default_name: ?[]const u8,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If name is not present, set name to "".
    const name: *const String = if (default_name) |name|
        try String.fromUtf8(agent, name)
    else
        .empty;

    // 2. Let env be the LexicalEnvironment of the running execution context.
    const env = agent.runningExecutionContext().ecmascript_code.lexical_environment;

    // 3. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.private_environment;

    // 4. Let sourceText be the source text matched by AsyncArrowFunction.
    const source_text = async_arrow_function.source_text;

    // 5. Let head be the AsyncArrowHead that is covered by CoverCallExpressionAndAsyncArrowHead.
    // 6. Let parameters be the ArrowFormalParameters of head.
    const parameters = async_arrow_function.formal_parameters;

    // 7. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText, parameters,
    //    AsyncConciseBody, lexical-this, env, privateEnv).
    const closure = try ordinaryFunctionCreate(
        agent,
        try realm.intrinsics.@"%AsyncFunction.prototype%"(),
        source_text,
        parameters,
        async_arrow_function.function_body,
        .lexical_this,
        env,
        private_env,
    );

    // 8. Perform SetFunctionName(closure, name).
    try setFunctionName(agent, closure, PropertyKey.from(name), null);

    // 9. Return closure.
    return closure;
}
