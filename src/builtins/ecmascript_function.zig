//! 10.2 ECMAScript Function Objects
//! https://tc39.es/ecma262/#sec-ecmascript-function-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("../language/ast.zig");
const builtin_function = @import("./builtin_function.zig");
const builtins = @import("../builtins.zig");
const bytecode = @import("../language/bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const BuiltinFunction = builtins.BuiltinFunction;
const ClassConstructorFields = builtins.ClassConstructorFields;
const ClassFieldDefinition = types.ClassFieldDefinition;
const Completion = types.Completion;
const Environment = execution.Environment;
const Executable = bytecode.Executable;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PrivateEnvironment = execution.PrivateEnvironment;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const ScriptOrModule = execution.ScriptOrModule;
const String = types.String;
const Symbol = types.Symbol;
const Value = types.Value;
const Vm = bytecode.Vm;
const arrayCreate = builtins.arrayCreate;
const containsSlice = utils.containsSlice;
const createMappedArgumentsObject = builtins.createMappedArgumentsObject;
const createUnmappedArgumentsObject = builtins.createUnmappedArgumentsObject;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const generateBytecode = bytecode.generateBytecode;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const newFunctionEnvironment = execution.newFunctionEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

// Non-standard helper to get the name property of a function
pub fn getFunctionName(function: Object) []const u8 {
    const property_descriptor = function.data.property_storage.get(PropertyKey.from("name")).?;
    const value = property_descriptor.value.?;
    return value.string.utf8;
}

pub const ConstructorKind = enum {
    base,
    derived,
};

pub const ThisMode = enum {
    lexical,
    strict,
    global,
};

pub const ECMAScriptFunction = MakeObject(.{
    .Fields = struct {
        /// [[Environment]]
        environment: Environment,

        /// [[PrivateEnvironment]]
        private_environment: ?*PrivateEnvironment,

        /// [[FormalParameters]]
        formal_parameters: ast.FormalParameters,

        /// [[ECMAScriptCode]]
        ecmascript_code: ast.FunctionBody,

        /// [[ConstructorKind]]
        constructor_kind: ConstructorKind,

        /// [[Realm]]
        realm: *Realm,

        /// [[ScriptOrModule]]
        script_or_module: ScriptOrModule,

        /// [[ThisMode]]
        this_mode: ThisMode,

        /// [[Strict]]
        strict: bool,

        /// [[HomeObject]]
        home_object: ?Object,

        /// [[SourceText]]
        source_text: []const u8,

        /// [[ClassFieldInitializerName]]
        class_field_initializer_name: ?union(enum) {
            property_key: PropertyKey,
            private_name: void, // TODO: Implement private names
        },

        /// [[Fields]]
        fields: []const ClassFieldDefinition,

        // TODO: [[PrivateMethods]]

        /// [[IsClassConstructor]]
        is_class_constructor: bool,

        cached_executable: ?Executable = null,
    },
    .tag = .ecmascript_function,
});

/// 10.2.1 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-ecmascript-function-objects-call-thisargument-argumentslist
fn call(object: Object, this_argument: Value, arguments_list: ArgumentsList) Agent.Error!Value {
    const agent = object.agent();

    const function = object.as(ECMAScriptFunction);

    // 1. Let callerContext be the running execution context.
    // NOTE: This is only used to restore the context, which is a simple pop().

    // 2. Let calleeContext be PrepareForOrdinaryCall(F, undefined).
    const callee_context = try prepareForOrdinaryCall(agent, function, null);

    // 3. Assert: calleeContext is now the running execution context.
    std.debug.assert(callee_context == agent.runningExecutionContext());

    // 4. If F.[[IsClassConstructor]] is true, then
    if (function.fields.is_class_constructor) {
        // a. Let error be a newly created TypeError object.
        // b. NOTE: error is created in calleeContext with F's associated Realm Record.
        const err = agent.throwException(.type_error, "{} is not callable", .{object});

        // c. Remove calleeContext from the execution context stack and restore callerContext as
        //    the running execution context.
        _ = agent.execution_context_stack.pop();

        // d. Return ThrowCompletion(error).
        return err;
    }

    // 5. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
    try ordinaryCallBindThis(agent, function, callee_context, this_argument);

    // 6. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
    const result = ordinaryCallEvaluateBody(agent, function, arguments_list);

    // 7. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
    _ = agent.execution_context_stack.pop();

    // 8. If result is a return completion, return result.[[Value]].
    if (result) |completion| {
        if (completion.type == .@"return")
            return completion.value.?;
    }
    // 9. ReturnIfAbrupt(result).
    else |err| return err;

    // 10. Return undefined.
    return .undefined;
}

/// 10.2.1.1 PrepareForOrdinaryCall ( F, newTarget )
/// https://tc39.es/ecma262/#sec-prepareforordinarycall
fn prepareForOrdinaryCall(
    agent: *Agent,
    function: *ECMAScriptFunction,
    new_target: ?Object,
) Allocator.Error!*ExecutionContext {
    // 1. Let callerContext be the running execution context.
    // NOTE: This is only used to suspend the context, which we don't do yet.

    // 7. Let localEnv be NewFunctionEnvironment(F, newTarget).
    const local_env = try newFunctionEnvironment(agent.gc_allocator, function, new_target);

    // 2. Let calleeContext be a new ECMAScript code execution context.
    const callee_context = ExecutionContext{
        // 3. Set the Function of calleeContext to F.
        .function = function.object(),

        // 4. Let calleeRealm be F.[[Realm]].
        // 5. Set the Realm of calleeContext to calleeRealm.
        .realm = function.fields.realm,

        // 6. Set the ScriptOrModule of calleeContext to F.[[ScriptOrModule]].
        .script_or_module = function.fields.script_or_module,

        .ecmascript_code = .{
            // 8. Set the LexicalEnvironment of calleeContext to localEnv.
            .lexical_environment = .{ .function_environment = local_env },

            // 9. Set the VariableEnvironment of calleeContext to localEnv.
            .variable_environment = .{ .function_environment = local_env },

            // 10. Set the PrivateEnvironment of calleeContext to F.[[PrivateEnvironment]].
            .private_environment = function.fields.private_environment,
        },
    };

    // TODO: 11. If callerContext is not already suspended, suspend callerContext.

    // 12. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
    try agent.execution_context_stack.append(callee_context);

    // 13. NOTE: Any exception objects produced after this point are associated with calleeRealm.
    // 14. Return calleeContext.
    return &agent.execution_context_stack.items[agent.execution_context_stack.items.len - 1];
}

/// 10.2.1.2 OrdinaryCallBindThis ( F, calleeContext, thisArgument )
/// https://tc39.es/ecma262/#sec-ordinarycallbindthis
pub fn ordinaryCallBindThis(
    agent: *Agent,
    function: *ECMAScriptFunction,
    callee_context: *ExecutionContext,
    this_argument: Value,
) Allocator.Error!void {
    // 1. Let thisMode be F.[[ThisMode]].
    const this_mode = function.fields.this_mode;

    // 2. If thisMode is lexical, return unused.
    if (this_mode == .lexical) return;

    // 3. Let calleeRealm be F.[[Realm]].
    const callee_realm = function.fields.realm;

    // 4. Let localEnv be the LexicalEnvironment of calleeContext.
    const local_env = callee_context.ecmascript_code.?.lexical_environment;

    const this_value = blk: {
        // 5. If thisMode is strict, then
        if (this_mode == .strict) {
            // a. Let thisValue be thisArgument.
            break :blk this_argument;
        }
        // 6. Else,
        else {
            // a. If thisArgument is either undefined or null, then
            if (this_argument == .undefined or this_argument == .null) {
                // i. Let globalEnv be calleeRealm.[[GlobalEnv]].
                const global_env = callee_realm.global_env;

                // ii. Assert: globalEnv is a Global Environment Record.
                // iii. Let thisValue be globalEnv.[[GlobalThisValue]].
                break :blk Value.from(global_env.global_this_value);
            }
            // b. Else,
            else {
                // i. Let thisValue be ! ToObject(thisArgument).
                // ii. NOTE: ToObject produces wrapper objects using calleeRealm.
                break :blk Value.from(this_argument.toObject(agent) catch |err| try noexcept(err));
            }
        }
    };

    // 7. Assert: localEnv is a Function Environment Record.
    std.debug.assert(local_env == .function_environment);

    // 8. Assert: The next step never returns an abrupt completion because localEnv.[[ThisBindingStatus]] is not initialized.
    // 9. Perform ! localEnv.BindThisValue(thisValue).
    _ = local_env.bindThisValue(this_value) catch unreachable;

    // 10. Return unused.
}

/// 10.2.1.4 OrdinaryCallEvaluateBody ( F, argumentsList )
/// https://tc39.es/ecma262/#sec-ordinarycallevaluatebody
pub fn ordinaryCallEvaluateBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: ArgumentsList,
) Agent.Error!Completion {
    // 1. Return ? EvaluateBody of F.[[ECMAScriptCode]] with arguments F and argumentsList.
    const function_body = function.fields.ecmascript_code;

    // 10.2.1.3 Runtime Semantics: EvaluateBody
    // https://tc39.es/ecma262/#sec-runtime-semantics-evaluatebody
    return switch (function_body.type) {
        // FunctionBody : FunctionStatementList
        // 1. Return ? EvaluateFunctionBody of FunctionBody with arguments functionObject and argumentsList.
        // ConciseBody : ExpressionBody
        // 1. Return ? EvaluateConciseBody of ConciseBody with arguments functionObject and argumentsList.
        .normal => evaluateFunctionBody(agent, function, arguments_list),

        // GeneratorBody : FunctionBody
        // 1. Return ? EvaluateGeneratorBody of GeneratorBody with arguments functionObject and argumentsList.
        .generator => evaluateGeneratorBody(agent, function, arguments_list),

        // AsyncGeneratorBody : FunctionBody
        // 1. Return ? EvaluateAsyncGeneratorBody of AsyncGeneratorBody with arguments functionObject and argumentsList.
        .async_generator => evaluateAsyncGeneratorBody(agent, function, arguments_list),

        // AsyncFunctionBody : FunctionBody
        // 1. Return ? EvaluateAsyncFunctionBody of AsyncFunctionBody with arguments functionObject and argumentsList.
        // AsyncConciseBody : ExpressionBody
        // 1. Return ? EvaluateAsyncConciseBody of AsyncConciseBody with arguments functionObject and argumentsList.
        .@"async" => evaluateAsyncFunctionBody(agent, function, arguments_list),
    };
}

/// 15.2.3 Runtime Semantics: EvaluateFunctionBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluatefunctionbody
fn evaluateFunctionBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: ArgumentsList,
) Agent.Error!Completion {
    // FunctionBody : FunctionStatementList
    // 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
    try functionDeclarationInstantiation(agent, function, arguments_list);

    // 2. Return ? Evaluation of FunctionStatementList.
    const executable = function.fields.cached_executable orelse blk: {
        const executable = try generateBytecode(agent, function.fields.ecmascript_code, .{});
        function.fields.cached_executable = executable;
        break :blk executable;
    };
    var vm = try Vm.init(agent);
    defer vm.deinit();
    return vm.run(executable);
}

/// 15.5.2 Runtime Semantics: EvaluateGeneratorBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluategeneratorbody
fn evaluateGeneratorBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: ArgumentsList,
) Agent.Error!Completion {
    // GeneratorBody : FunctionBody
    // 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
    try functionDeclarationInstantiation(agent, function, arguments_list);

    // 2. Let G be ? OrdinaryCreateFromConstructor(functionObject, "%GeneratorFunction.prototype.prototype%",
    //    « [[GeneratorState]], [[GeneratorContext]], [[GeneratorBrand]] »).
    // 3. Set G.[[GeneratorBrand]] to empty.
    const generator = try ordinaryCreateFromConstructor(
        builtins.Generator,
        agent,
        function.object(),
        "%GeneratorPrototype%",
    );

    // TODO: 4. Perform GeneratorStart(G, FunctionBody).

    // 5. Return Completion Record { [[Type]]: return, [[Value]]: G, [[Target]]: empty }.
    return .{ .type = .@"return", .value = Value.from(generator), .target = null };
}

/// 15.6.2 Runtime Semantics: EvaluateAsyncGeneratorBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluateasyncgeneratorbody
fn evaluateAsyncGeneratorBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: ArgumentsList,
) Agent.Error!Completion {
    // AsyncGeneratorBody : FunctionBody
    // 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
    try functionDeclarationInstantiation(agent, function, arguments_list);

    // 2. Let generator be ? OrdinaryCreateFromConstructor(functionObject, "%AsyncGeneratorFunction.prototype.prototype%",
    //    « [[AsyncGeneratorState]], [[AsyncGeneratorContext]], [[AsyncGeneratorQueue]], [[GeneratorBrand]] »).
    // 3. Set generator.[[GeneratorBrand]] to empty.
    const generator = try ordinaryCreateFromConstructor(
        builtins.AsyncGenerator,
        agent,
        function.object(),
        "%AsyncGeneratorPrototype%",
    );

    // TODO: 4. Perform AsyncGeneratorStart(generator, FunctionBody).

    // 5. Return Completion Record { [[Type]]: return, [[Value]]: generator, [[Target]]: empty }.
    return .{ .type = .@"return", .value = Value.from(generator), .target = null };
}

/// 15.8.4 Runtime Semantics: EvaluateAsyncFunctionBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluateasyncfunctionbody
fn evaluateAsyncFunctionBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: ArgumentsList,
) Allocator.Error!Completion {
    // AsyncFunctionBody : FunctionBody
    const realm = agent.currentRealm();

    // 1. Let promiseCapability be ! NewPromiseCapability(%Promise%).
    const promise_capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 2. Let declResult be Completion(FunctionDeclarationInstantiation(functionObject, argumentsList)).
    functionDeclarationInstantiation(agent, function, arguments_list) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,

        // 3. If declResult is an abrupt completion, then
        error.ExceptionThrown => {
            const exception = agent.clearException();

            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « declResult.[[Value]] »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                .undefined,
                &.{exception},
            ) catch |err_| try noexcept(err_);
        },
    };

    // 4. Else,
    //     TODO: a. Perform AsyncFunctionStart(promiseCapability, FunctionBody).

    // 5. Return Completion Record { [[Type]]: return, [[Value]]: promiseCapability.[[Promise]], [[Target]]: empty }.
    return .{ .type = .@"return", .value = Value.from(promise_capability.promise), .target = null };
}

/// 10.2.2 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-ecmascript-function-objects-construct-argumentslist-newtarget
pub fn construct(
    object: Object,
    arguments_list: ArgumentsList,
    new_target: Object,
) Agent.Error!Object {
    const agent = object.agent();
    const function = object.as(ECMAScriptFunction);

    // 1. Let callerContext be the running execution context.
    // NOTE: This is only used to restore the context, which is a simple pop().

    // 2. Let kind be F.[[ConstructorKind]].
    const kind = function.fields.constructor_kind;

    var this_argument: Object = undefined;

    // 3. If kind is base, then
    if (kind == .base) {
        // a. Let thisArgument be ? OrdinaryCreateFromConstructor(newTarget, "%Object.prototype%").
        this_argument = try ordinaryCreateFromConstructor(
            builtins.Object,
            agent,
            new_target,
            "%Object.prototype%",
        );
    }

    // 4. Let calleeContext be PrepareForOrdinaryCall(F, newTarget).
    const callee_context = try prepareForOrdinaryCall(agent, function, new_target);

    // 5. Assert: calleeContext is now the running execution context.
    std.debug.assert(callee_context == agent.runningExecutionContext());

    // 6. If kind is base, then
    if (kind == .base) {
        // a. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
        try ordinaryCallBindThis(agent, function, callee_context, Value.from(this_argument));

        // b. Let initializeResult be Completion(InitializeInstanceElements(thisArgument, F)).
        const initialize_result = this_argument.initializeInstanceElements(function.object());

        // c. If initializeResult is an abrupt completion, then
        initialize_result catch |err| {
            // i. Remove calleeContext from the execution context stack and restore callerContext
            //    as the running execution context.
            _ = agent.execution_context_stack.pop();

            // ii. Return ? initializeResult.
            return err;
        };
    }

    // 7. Let constructorEnv be the LexicalEnvironment of calleeContext.
    const constructor_env = callee_context.ecmascript_code.?.lexical_environment;

    // 8. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
    const result = ordinaryCallEvaluateBody(agent, function, arguments_list);

    // 9. Remove calleeContext from the execution context stack and restore callerContext as the
    //    running execution context.
    _ = agent.execution_context_stack.pop();

    // 10. If result is a return completion, then
    if (result) |completion| {
        if (completion.type == .@"return") {
            // a. If result.[[Value]] is an Object, return result.[[Value]].
            if (completion.value.? == .object) return completion.value.?.object;

            // b. If kind is base, return thisArgument.
            if (kind == .base) return this_argument;

            // c. If result.[[Value]] is not undefined, throw a TypeError exception.
            if (completion.value.? != .undefined) {
                return agent.throwException(
                    .type_error,
                    "Constructor must return an object or undefined",
                    .{},
                );
            }
        }
    }
    // 11. Else,
    else |err| {
        // a. ReturnIfAbrupt(result).
        return err;
    }

    // 12. Let thisBinding be ? constructorEnv.GetThisBinding().
    const this_binding = try constructor_env.getThisBinding();

    // 13. Assert: thisBinding is an Object.
    std.debug.assert(this_binding == .object);

    // 14. Return thisBinding.
    return this_binding.object;
}

/// 10.2.3 OrdinaryFunctionCreate ( functionPrototype, sourceText, ParameterList, Body, thisMode, env, privateEnv )
/// https://tc39.es/ecma262/#sec-ordinaryfunctioncreate
pub fn ordinaryFunctionCreate(
    agent: *Agent,
    function_prototype: Object,
    source_text: []const u8,
    parameter_list: ast.FormalParameters,
    body: ast.FunctionBody,
    this_mode: enum { lexical_this, non_lexical_this },
    env: Environment,
    private_env: ?*PrivateEnvironment,
) Allocator.Error!Object {
    // 7. If the source text matched by Body is strict mode code, let Strict be true; else let
    //    Strict be false.
    const strict = body.strict.?;

    // 1. Let internalSlotsList be the internal slots listed in Table 30.
    // 2. Let F be OrdinaryObjectCreate(functionPrototype, internalSlotsList).
    const function = try ECMAScriptFunction.create(agent, .{
        .internal_methods = .{
            // 3. Set F.[[Call]] to the definition specified in 10.2.1.
            .call = call,
        },
        .prototype = function_prototype,
        .fields = .{
            // 4. Set F.[[SourceText]] to sourceText.
            .source_text = source_text,

            // 5. Set F.[[FormalParameters]] to ParameterList.
            .formal_parameters = parameter_list,

            // 6. Set F.[[ECMAScriptCode]] to Body.
            .ecmascript_code = body,

            // 8. Set F.[[Strict]] to Strict.
            .strict = strict,

            // 9. If thisMode is lexical-this, set F.[[ThisMode]] to lexical.
            // 10. Else if Strict is true, set F.[[ThisMode]] to strict.
            // 11. Else, set F.[[ThisMode]] to global.
            .this_mode = switch (this_mode) {
                .lexical_this => .lexical,
                else => if (strict) .strict else .global,
            },

            // 12. Set F.[[IsClassConstructor]] to false.
            .is_class_constructor = false,

            // 13. Set F.[[Environment]] to env.
            .environment = env,

            // 14. Set F.[[PrivateEnvironment]] to privateEnv.
            .private_environment = private_env,

            // 15. Set F.[[ScriptOrModule]] to GetActiveScriptOrModule().
            .script_or_module = agent.getActiveScriptOrModule().?,

            // 16. Set F.[[Realm]] to the current Realm Record.
            .realm = agent.currentRealm(),

            // 17. Set F.[[HomeObject]] to undefined.
            .home_object = null,

            // 18. Set F.[[Fields]] to a new empty List.
            .fields = &.{},

            // TODO: 19. Set F.[[PrivateMethods]] to a new empty List.

            // 20. Set F.[[ClassFieldInitializerName]] to empty.
            .class_field_initializer_name = null,

            // NOTE: Not in the spec but we need to provide a value
            .constructor_kind = .base,
        },
    });

    // 21. Let len be the ExpectedArgumentCount of ParameterList.
    const len = parameter_list.expectedArgumentCount();

    // 22. Perform SetFunctionLength(F, len).
    try setFunctionLength(function, @floatFromInt(len));

    // 23. Return F.
    return function;
}

/// 10.2.4 AddRestrictedFunctionProperties ( F, realm )
/// https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
pub fn addRestrictedFunctionProperties(function: Object, realm: *Realm) Allocator.Error!void {
    // 1. Assert: realm.[[Intrinsics]].[[%ThrowTypeError%]] exists and has been initialized.
    // 2. Let thrower be realm.[[Intrinsics]].[[%ThrowTypeError%]].
    const thrower = try realm.intrinsics.@"%ThrowTypeError%"();

    const property_descriptor = PropertyDescriptor{
        .get = thrower,
        .set = thrower,
        .enumerable = false,
        .configurable = true,
    };

    // 3. Perform ! DefinePropertyOrThrow(F, "caller", PropertyDescriptor {
    //      [[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(
        PropertyKey.from("caller"),
        property_descriptor,
    ) catch |err| try noexcept(err);

    // 4. Perform ! DefinePropertyOrThrow(F, "arguments", PropertyDescriptor {
    //      [[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(
        PropertyKey.from("arguments"),
        property_descriptor,
    ) catch |err| try noexcept(err);

    // 5. Return unused.
}

/// 10.2.5 MakeConstructor ( F [ , writablePrototype [ , prototype ] ] )
/// https://tc39.es/ecma262/#sec-makeconstructor
pub fn makeConstructor(
    function: Object,
    args: struct {
        writable_prototype: bool = true,
        prototype: ?Object = null,
    },
) Allocator.Error!void {
    const agent = function.agent();
    const realm = agent.currentRealm();

    // 1. If F is an ECMAScript function object, then
    if (function.is(ECMAScriptFunction)) {
        // a. Assert: IsConstructor(F) is false.
        std.debug.assert(!Value.from(function).isConstructor());

        // b. Assert: F is an extensible object that does not have a "prototype" own property.
        std.debug.assert(
            function.extensible().* and !function.propertyStorage().has(PropertyKey.from("prototype")),
        );

        // c. Set F.[[Construct]] to the definition specified in 10.2.2.
        function.internalMethods().construct = construct;
    }
    // 2. Else,
    else {
        // a. Set F.[[Construct]] to the definition specified in 10.3.2.
        function.internalMethods().construct = builtin_function.construct;
    }

    // 3. Set F.[[ConstructorKind]] to base.
    if (function.is(ECMAScriptFunction)) {
        function.as(ECMAScriptFunction).fields.constructor_kind = .base;
    } else if (function.is(BuiltinFunction)) {
        if (function.as(BuiltinFunction).fields.additional_fields.tryCast(*ClassConstructorFields)) |class_constructor_fields| {
            class_constructor_fields.constructor_kind = .base;
        }
    }

    // 4. If writablePrototype is not present, set writablePrototype to true.
    // NOTE: This is done via the default argument.

    // 5. If prototype is not present, then
    const prototype = args.prototype orelse blk: {
        // a. Set prototype to OrdinaryObjectCreate(%Object.prototype%).
        const prototype = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

        // b. Perform ! DefinePropertyOrThrow(prototype, "constructor", PropertyDescriptor {
        //      [[Value]]: F, [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: true
        //    }).
        prototype.definePropertyOrThrow(PropertyKey.from("constructor"), .{
            .value = Value.from(function),
            .writable = args.writable_prototype,
            .enumerable = false,
            .configurable = true,
        }) catch |err| try noexcept(err);

        break :blk prototype;
    };

    // 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
    //      [[Value]]: prototype, [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
        .value = Value.from(prototype),
        .writable = args.writable_prototype,
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 7. Return unused.
}

/// 10.2.6 MakeClassConstructor ( F )
/// https://tc39.es/ecma262/#sec-makeclassconstructor
pub fn makeClassConstructor(function: *ECMAScriptFunction) void {
    // 1. Assert: F.[[IsClassConstructor]] is false.
    std.debug.assert(!function.fields.is_class_constructor);

    // 2. Set F.[[IsClassConstructor]] to true.
    function.fields.is_class_constructor = true;

    // 3. Return unused.
}

/// 10.2.7 MakeMethod ( F, homeObject )
/// https://tc39.es/ecma262/#sec-makemethod
pub fn makeMethod(
    function: *ECMAScriptFunction,
    home_object: Object,
) void {
    // 1. Set F.[[HomeObject]] to homeObject.
    function.fields.home_object = home_object;

    // 2. Return unused.
}

/// 10.2.8 DefineMethodProperty ( homeObject, key, closure, enumerable )
/// https://tc39.es/ecma262/#sec-definemethodproperty
pub fn defineMethodProperty(
    home_object: Object,
    property_key: PropertyKey,
    closure: Object,
    enumerable: bool,
) Agent.Error!void {
    // 1. Assert: homeObject is an ordinary, extensible object.

    // TODO: 2. If key is a Private Name, then
    if (false) {
        // a. Return PrivateElement { [[Key]]: key, [[Kind]]: method, [[Value]]: closure }.
    }
    // 3. Else,
    else {
        // a. Let desc be the PropertyDescriptor {
        //      [[Value]]: closure, [[Writable]]: true, [[Enumerable]]: enumerable, [[Configurable]]: true
        //    }.
        const property_descriptor = PropertyDescriptor{
            .value = Value.from(closure),
            .writable = true,
            .enumerable = enumerable,
            .configurable = true,
        };

        // b. Perform ? DefinePropertyOrThrow(homeObject, key, desc).
        try home_object.definePropertyOrThrow(
            property_key,
            property_descriptor,
        );

        // c. NOTE: DefinePropertyOrThrow only returns an abrupt completion when attempting to
        //    define a class static method whose key is "prototype".

        // d. Return unused.
    }
}

/// 10.2.9 SetFunctionName ( F, name [ , prefix ] )
/// https://tc39.es/ecma262/#sec-setfunctionname
pub fn setFunctionName(
    function: Object,
    name_property_key: PropertyKey,
    prefix: ?[]const u8,
) Allocator.Error!void {
    const agent = function.agent();

    // 1. Assert: F is an extensible object that does not have a "name" own property.
    std.debug.assert(
        function.extensible().* and !function.propertyStorage().has(PropertyKey.from("name")),
    );

    var name = switch (try name_property_key.toStringOrSymbol(agent)) {
        .string => |string| String.from(string),

        // 2. If name is a Symbol, then
        .symbol => |symbol| blk: {
            // a. Let description be name's [[Description]] value.
            const description = symbol.description;

            // b. If description is undefined, set name to the empty String.
            if (description == null) break :blk String.from("");

            // c. Else, set name to the string-concatenation of "[", description, and "]".
            break :blk String.from(try std.fmt.allocPrint(
                agent.gc_allocator,
                "[{s}]",
                .{description.?.utf8},
            ));
        },

        // TODO: 3. Else if name is a Private Name, then
        //     a. Set name to name.[[Description]].
    };

    // 4. If F has an [[InitialName]] internal slot, then
    if (function.is(BuiltinFunction)) {
        // a. Set F.[[InitialName]] to name.
        function.as(BuiltinFunction).fields.initial_name = name;
    }

    // 5. If prefix is present, then
    if (prefix != null) {
        // a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and
        //    name.
        name = String.from(
            try std.fmt.allocPrint(agent.gc_allocator, "{s} {}", .{ prefix.?, name }),
        );

        // b. If F has an [[InitialName]] internal slot, then
        if (function.is(BuiltinFunction)) {
            // i. Optionally, set F.[[InitialName]] to name.
            function.as(BuiltinFunction).fields.initial_name = name;
        }
    }

    // 6. Perform ! DefinePropertyOrThrow(F, "name", PropertyDescriptor {
    //      [[Value]]: name, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(PropertyKey.from("name"), PropertyDescriptor{
        .value = Value.from(name),
        .writable = false,
        .enumerable = false,
        .configurable = true,
    }) catch |err| try noexcept(err);

    // 7. Return unused.
}

/// 10.2.10 SetFunctionLength ( F, length )
/// https://tc39.es/ecma262/#sec-setfunctionlength
pub fn setFunctionLength(function: Object, length: f64) Allocator.Error!void {
    std.debug.assert(
        std.math.isPositiveInf(length) or
            (std.math.isFinite(length) and std.math.trunc(length) == length and length >= 0),
    );

    // 1. Assert: F is an extensible object that does not have a "length" own property.
    std.debug.assert(
        function.extensible().* and !function.propertyStorage().has(PropertyKey.from("length")),
    );

    // 2. Perform ! DefinePropertyOrThrow(F, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(PropertyKey.from("length"), PropertyDescriptor{
        .value = Value.from(length),
        .writable = false,
        .enumerable = false,
        .configurable = true,
    }) catch |err| try noexcept(err);

    // 3. Return unused.
}

/// 10.2.11 FunctionDeclarationInstantiation ( func, argumentsList )
/// https://tc39.es/ecma262/#sec-functiondeclarationinstantiation
fn functionDeclarationInstantiation(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: ArgumentsList,
) Agent.Error!void {
    // 1. Let calleeContext be the running execution context.
    var callee_context = agent.runningExecutionContext();

    // 2. Let code be func.[[ECMAScriptCode]].
    const code = function.fields.ecmascript_code;

    // 3. Let strict be func.[[Strict]].
    const strict = function.fields.strict;

    // 4. Let formals be func.[[FormalParameters]].
    const formals = function.fields.formal_parameters;

    // 5. Let parameterNames be the BoundNames of formals.
    const parameter_names = try formals.boundNames(agent.gc_allocator);
    defer agent.gc_allocator.free(parameter_names);

    // 6. If parameterNames has any duplicate entries, let hasDuplicates be true. Otherwise, let
    //    hasDuplicates be false.
    const has_duplicates = blk: {
        var unique_names = std.StringHashMap(void).init(agent.gc_allocator);
        defer unique_names.deinit();
        if (parameter_names.len > std.math.maxInt(u32)) return error.OutOfMemory;
        try unique_names.ensureTotalCapacity(@intCast(parameter_names.len));
        for (parameter_names) |parameter_name| {
            if (unique_names.contains(parameter_name)) break :blk true;
            unique_names.putAssumeCapacityNoClobber(parameter_name, {});
        }
        break :blk false;
    };

    // 7. Let simpleParameterList be IsSimpleParameterList of formals.
    const simple_parameter_list = formals.isSimpleParameterList();

    // 8. Let hasParameterExpressions be ContainsExpression of formals.
    const has_parameter_expressions = formals.containsExpression();

    // TODO: 9. Let varNames be the VarDeclaredNames of code.

    // 10. Let varDeclarations be the VarScopedDeclarations of code.
    const var_declarations = try code.varScopedDeclarations(agent.gc_allocator);
    defer agent.gc_allocator.free(var_declarations);

    // TODO: 11. Let lexicalNames be the LexicallyDeclaredNames of code.
    const lexical_names = &[_][]const u8{};

    // 12. Let functionNames be a new empty List.
    var function_names = std.StringHashMap(void).init(agent.gc_allocator);
    defer function_names.deinit();

    // 13. Let functionsToInitialize be a new empty List.
    var functions_to_initialize = std.ArrayList(ast.HoistableDeclaration).init(agent.gc_allocator);
    defer functions_to_initialize.deinit();

    // 14. For each element d of varDeclarations, in reverse List order, do
    var it = std.mem.reverseIterator(var_declarations);
    while (it.next()) |var_declaration| {
        // a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
        if (var_declaration == .hoistable_declaration) {
            // i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an
            //    AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.

            // ii. Let fn be the sole element of the BoundNames of d.
            const function_name = switch (var_declaration.hoistable_declaration) {
                inline else => |function_declaration| function_declaration.identifier,
            }.?;

            // iii. If functionNames does not contain fn, then
            if (!function_names.contains(function_name)) {
                // 1. Insert fn as the first element of functionNames.
                try function_names.put(function_name, {});

                // 2. NOTE: If there are multiple function declarations for the same name, the last
                //    declaration is used.
                // 3. Insert d as the first element of functionsToInitialize.
                // NOTE: AFAICT the order isn't observable, so we can append.
                try functions_to_initialize.append(var_declaration.hoistable_declaration);
            }
        }
    }

    // 15. Let argumentsObjectNeeded be true.
    var arguments_object_needed = true;

    // 16. If func.[[ThisMode]] is lexical, then
    if (function.fields.this_mode == .lexical) {
        // a. NOTE: Arrow functions never have an arguments object.
        // b. Set argumentsObjectNeeded to false.
        arguments_object_needed = false;
    }
    // 17. Else if parameterNames contains "arguments", then
    else if (containsSlice(parameter_names, "arguments")) {
        // a. Set argumentsObjectNeeded to false.
        arguments_object_needed = false;
    }
    // 18. Else if hasParameterExpressions is false, then
    else if (!has_parameter_expressions) {
        // a. If functionNames contains "arguments" or lexicalNames contains "arguments", then
        if (function_names.contains("arguments") or containsSlice(lexical_names, "arguments")) {
            // i. Set argumentsObjectNeeded to false.
            arguments_object_needed = false;
        }
    }

    // 19. If strict is true or hasParameterExpressions is false, then
    const env = if (strict or !has_parameter_expressions) blk: {
        // a. NOTE: Only a single Environment Record is needed for the parameters, since calls
        //    to eval in strict mode code cannot create new bindings which are visible outside
        //    of the eval.

        // b. Let env be the LexicalEnvironment of calleeContext.
        break :blk callee_context.ecmascript_code.?.lexical_environment;
    }
    // 20. Else,
    else blk: {
        // a. NOTE: A separate Environment Record is needed to ensure that bindings created by
        //    direct eval calls in the formal parameter list are outside the environment where
        //    parameters are declared.

        // b. Let calleeEnv be the LexicalEnvironment of calleeContext.
        const callee_env = callee_context.ecmascript_code.?.lexical_environment;

        // c. Let env be NewDeclarativeEnvironment(calleeEnv).
        const env = Environment{
            .declarative_environment = try newDeclarativeEnvironment(
                agent.gc_allocator,
                callee_env,
            ),
        };

        // d. Assert: The VariableEnvironment of calleeContext and calleeEnv are the same Environment Record.
        std.debug.assert(
            std.meta.eql(callee_context.ecmascript_code.?.variable_environment, callee_env),
        );

        // e. Set the LexicalEnvironment of calleeContext to env.
        callee_context.ecmascript_code.?.lexical_environment = env;

        break :blk env;
    };

    // 21. For each String paramName of parameterNames, do
    for (parameter_names) |parameter_name| {
        // a. Let alreadyDeclared be ! env.HasBinding(paramName).
        const already_declared = env.hasBinding(parameter_name) catch |err| try noexcept(err);

        // b. NOTE: Early errors ensure that duplicate parameter names can only occur in non-strict
        //    functions that do not have parameter default values or rest parameters.

        // c. If alreadyDeclared is false, then
        if (!already_declared) {
            // i. Perform ! env.CreateMutableBinding(paramName, false).
            env.createMutableBinding(agent, parameter_name, false) catch |err| try noexcept(err);

            // ii. If hasDuplicates is true, then
            if (has_duplicates) {
                // 1. Perform ! env.InitializeBinding(paramName, undefined).
                env.initializeBinding(
                    agent,
                    parameter_name,
                    .undefined,
                ) catch |err| try noexcept(err);
            }
        }
    }

    // 22. If argumentsObjectNeeded is true, then
    const parameter_bindings = if (arguments_object_needed) blk: {
        // a. If strict is true or simpleParameterList is false, then
        const arguments_object = if (strict or !simple_parameter_list) ao_blk: {
            // i. Let ao be CreateUnmappedArgumentsObject(argumentsList).
            break :ao_blk try createUnmappedArgumentsObject(agent, arguments_list.values);
        }
        // b. Else,
        else ao_blk: {
            // i. NOTE: A mapped argument object is only provided for non-strict functions that
            //    don't have a rest parameter, any parameter default value initializers, or any
            //    destructured parameters.

            // ii. Let ao be CreateMappedArgumentsObject(func, formals, argumentsList, env).
            break :ao_blk try createMappedArgumentsObject(
                agent,
                function.object(),
                formals,
                arguments_list.values,
                env,
            );
        };

        // c. If strict is true, then
        if (strict) {
            // i. Perform ! env.CreateImmutableBinding("arguments", false).
            env.createImmutableBinding(agent, "arguments", false) catch |err| try noexcept(err);

            // ii. NOTE: In strict mode code early errors prevent attempting to assign to this
            //     binding, so its mutability is not observable.
        }
        // d. Else,
        else {
            // i. Perform ! env.CreateMutableBinding("arguments", false).
            env.createMutableBinding(agent, "arguments", false) catch |err| try noexcept(err);
        }

        // e. Perform ! env.InitializeBinding("arguments", ao).
        env.initializeBinding(
            agent,
            "arguments",
            Value.from(arguments_object),
        ) catch |err| try noexcept(err);

        // f. Let parameterBindings be the list-concatenation of parameterNames and « "arguments" ».
        var parameter_bindings = try std.ArrayList([]const u8).initCapacity(
            agent.gc_allocator,
            parameter_names.len + 1,
        );
        parameter_bindings.appendSliceAssumeCapacity(parameter_names);
        parameter_bindings.appendAssumeCapacity("arguments");
        break :blk try parameter_bindings.toOwnedSlice();
    }
    // 23. Else,
    else blk: {
        // a. Let parameterBindings be parameterNames.
        break :blk parameter_names;
    };
    defer if (arguments_object_needed) agent.gc_allocator.free(parameter_bindings);

    // TODO: 24-26.
    // NOTE: Ad-hoc implementation of IteratorBindingInitialization for SingleNameBinding and BindingRestElement
    const environment = if (has_duplicates) null else env;
    for (formals.items, 0..) |item, i| {
        switch (item) {
            .formal_parameter => |formal_parameter| {
                const name = formal_parameter.binding_element.identifier;
                const initializer = formal_parameter.binding_element.initializer;
                var value = arguments_list.get(i);
                const reference = try agent.resolveBinding(name, environment, strict, null);
                if (initializer != null and value == .undefined) {
                    value = (try generateAndRunBytecode(
                        agent,
                        ast.ExpressionStatement{ .expression = initializer.? },
                        .{},
                    )).value.?;
                }
                if (environment == null)
                    try reference.putValue(agent, value)
                else
                    try reference.initializeReferencedBinding(agent, value);
            },
            .function_rest_parameter => |function_rest_parameter| {
                const name = function_rest_parameter.binding_rest_element.identifier;
                const reference = try agent.resolveBinding(name, environment, strict, null);
                const array = arrayCreate(agent, 0, null) catch |err| try noexcept(err);
                const rest = arguments_list.values[@min(i, arguments_list.values.len)..];
                for (rest, 0..) |value, n| {
                    array.createDataPropertyOrThrow(
                        PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(n))),
                        value,
                    ) catch |err| try noexcept(err);
                }
                if (environment == null)
                    try reference.putValue(agent, Value.from(array))
                else
                    try reference.initializeReferencedBinding(agent, Value.from(array));
            },
        }
    }

    // 27. If hasParameterExpressions is false, then
    const var_env = if (!has_parameter_expressions) blk: {
        // a. NOTE: Only a single Environment Record is needed for the parameters and top-level vars.

        // b. Let instantiatedVarNames be a copy of the List parameterBindings.
        var instantiated_var_names = std.StringHashMap(void).init(agent.gc_allocator);
        defer instantiated_var_names.deinit();

        if (parameter_bindings.len > std.math.maxInt(u32)) return error.OutOfMemory;
        try instantiated_var_names.ensureTotalCapacity(@intCast(parameter_bindings.len));
        for (parameter_bindings) |parameter_binding| {
            instantiated_var_names.putAssumeCapacity(parameter_binding, {});
        }

        // c. For each element n of varNames, do
        // TODO: This should use VarDeclaredNames
        for (var_declarations) |var_declaration| {
            const var_name = switch (var_declaration) {
                .variable_declaration => |variable_declaration| variable_declaration.binding_identifier,
                .hoistable_declaration => |hoistable_declaration| switch (hoistable_declaration) {
                    inline else => |function_declaration| function_declaration.identifier,
                },
            }.?;

            // i. If instantiatedVarNames does not contain n, then
            if (!instantiated_var_names.contains(var_name)) {
                // 1. Append n to instantiatedVarNames.
                try instantiated_var_names.putNoClobber(var_name, {});

                // 2. Perform ! env.CreateMutableBinding(n, false).
                env.createMutableBinding(agent, var_name, false) catch |err| try noexcept(err);

                // 3. Perform ! env.InitializeBinding(n, undefined).
                env.initializeBinding(agent, var_name, .undefined) catch |err| try noexcept(err);
            }
        }

        // d. Let varEnv be env.
        break :blk env;
    }
    // 28. Else,
    else blk: {
        // a. NOTE: A separate Environment Record is needed to ensure that closures created by
        //    expressions in the formal parameter list do not have visibility of declarations in
        //    the function body.

        // b. Let varEnv be NewDeclarativeEnvironment(env).
        const var_env = Environment{
            .declarative_environment = try newDeclarativeEnvironment(agent.gc_allocator, env),
        };

        // c. Set the VariableEnvironment of calleeContext to varEnv.
        callee_context.ecmascript_code.?.variable_environment = var_env;

        // d. Let instantiatedVarNames be a new empty List.
        var instantiated_var_names = std.StringHashMap(void).init(agent.gc_allocator);
        defer instantiated_var_names.deinit();

        // e. For each element n of varNames, do
        // TODO: This should use VarDeclaredNames
        for (var_declarations) |var_declaration| {
            const var_name = switch (var_declaration) {
                .variable_declaration => |variable_declaration| variable_declaration.binding_identifier,
                .hoistable_declaration => |hoistable_declaration| switch (hoistable_declaration) {
                    inline else => |function_declaration| function_declaration.identifier,
                },
            }.?;

            // i. If instantiatedVarNames does not contain n, then
            if (!instantiated_var_names.contains(var_name)) {
                // 1. Append n to instantiatedVarNames.
                try instantiated_var_names.putNoClobber(var_name, {});

                // 2. Perform ! varEnv.CreateMutableBinding(n, false).
                var_env.createMutableBinding(agent, var_name, false) catch |err| try noexcept(err);

                // 3. If parameterBindings does not contain n, or if functionNames contains n, then
                //     a. Let initialValue be undefined.
                // 4. Else,
                //     a. Let initialValue be ! env.GetBindingValue(n, false).
                const initial_value = if (!containsSlice(parameter_names, var_name) or
                    function_names.contains(var_name))
                    .undefined
                else
                    env.getBindingValue(agent, var_name, false) catch |err| try noexcept(err);

                // 5. Perform ! varEnv.InitializeBinding(n, initialValue).
                var_env.initializeBinding(agent, var_name, initial_value) catch |err| try noexcept(err);

                // 6. NOTE: A var with the same name as a formal parameter initially has the same
                //    value as the corresponding initialized parameter.
            }
        }

        break :blk var_env;
    };

    // 29. NOTE: Annex B.3.2.1 adds additional steps at this point.

    // 30. If strict is false, then
    const lex_env = if (!strict) blk: {
        // a. Let lexEnv be NewDeclarativeEnvironment(varEnv).
        break :blk Environment{
            .declarative_environment = try newDeclarativeEnvironment(agent.gc_allocator, var_env),
        };

        // b. NOTE: Non-strict functions use a separate Environment Record for top-level lexical
        //    declarations so that a direct eval can determine whether any var scoped declarations
        //    introduced by the eval code conflict with pre-existing top-level lexically scoped
        //    declarations. This is not needed for strict functions because a strict direct eval
        //    always places all declarations into a new Environment Record.
    }
    // 31. Else,
    else blk: {
        // a. Let lexEnv be varEnv.
        break :blk var_env;
    };

    // 32. Set the LexicalEnvironment of calleeContext to lexEnv.
    callee_context.ecmascript_code.?.lexical_environment = lex_env;

    // TODO: 33-34.

    // 35. Let privateEnv be the PrivateEnvironment of calleeContext.
    const private_env = callee_context.ecmascript_code.?.private_environment;

    // 36. For each Parse Node f of functionsToInitialize, do
    for (functions_to_initialize.items) |*hoistable_declaration| {
        // a. Let fn be the sole element of the BoundNames of f.
        const function_name = switch (hoistable_declaration.*) {
            inline else => |function_declaration| function_declaration.identifier,
        }.?;

        switch (hoistable_declaration.*) {
            inline else => |*function_declaration| {
                // Assign the function body's strictness, which is needed for the deferred bytecode generation.
                // FIXME: This should ideally happen at parse time.
                function_declaration.function_body.strict = strict or
                    function_declaration.function_body.functionBodyContainsUseStrict();
            },
        }

        // b. Let fo be InstantiateFunctionObject of f with arguments lexEnv and privateEnv.
        const function_object = try switch (hoistable_declaration.*) {
            .function_declaration => |function_declaration| function_declaration.instantiateOrdinaryFunctionObject(agent, lex_env, private_env),
            .generator_declaration => |generator_declaration| generator_declaration.instantiateGeneratorFunctionObject(agent, lex_env, private_env),
            .async_function_declaration => |async_function_declaration| async_function_declaration.instantiateAsyncFunctionObject(agent, lex_env, private_env),
            .async_generator_declaration => |async_generator_declaration| async_generator_declaration.instantiateAsyncGeneratorFunctionObject(agent, lex_env, private_env),
        };

        // c. Perform ! varEnv.SetMutableBinding(fn, fo, false).
        var_env.setMutableBinding(
            agent,
            function_name,
            Value.from(function_object),
            false,
        ) catch |err| try noexcept(err);
    }

    // 37. Return unused.
}
