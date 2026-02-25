//! 10.2 ECMAScript Function Objects
//! https://tc39.es/ecma262/#sec-ecmascript-function-objects

const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const interpreter = @import("../interpreter.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BuiltinFunction = builtins.BuiltinFunction;
const ClassConstructorFields = builtins.builtin_function.ClassConstructorFields;
const ClassFieldDefinition = types.ClassFieldDefinition;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PrivateElement = types.PrivateElement;
const PrivateEnvironment = execution.PrivateEnvironment;
const PrivateMethodDefinition = types.PrivateMethodDefinition;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const PropertyKeyOrPrivateName = types.PropertyKeyOrPrivateName;
const Realm = execution.Realm;
const ScriptOrModule = execution.ScriptOrModule;
const String = types.String;
const Value = types.Value;
const asyncFunctionStart = builtins.asyncFunctionStart;
const asyncGeneratorStart = builtins.asyncGeneratorStart;
const generatorStart = builtins.generatorStart;
const newFunctionEnvironment = execution.newFunctionEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

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
        home_object: ?*Object,

        /// [[SourceText]]
        source_text: []const u8,

        /// [[ClassFieldInitializerName]]
        class_field_initializer_name: ?PropertyKeyOrPrivateName,

        /// [[Fields]]
        fields: []const ClassFieldDefinition,

        /// [[PrivateMethods]]
        private_methods: []const PrivateMethodDefinition,

        /// [[IsClassConstructor]]
        is_class_constructor: bool,

        cached_bytecode: ?*const interpreter.Bytecode = null,

        pub fn compile(self: *@This(), agent: *Agent) std.mem.Allocator.Error!*const interpreter.Bytecode {
            if (self.cached_bytecode) |bc| return bc;

            const function: *ECMAScriptFunction = @fieldParentPtr("fields", self);
            const object = &function.object;
            const name_value = object.getPropertyValueDirect(PropertyKey.from("name"));
            const name = try name_value.asString().toUtf8(agent.gc_allocator);
            defer agent.gc_allocator.free(name);

            if (function.fields.this_mode == .lexical) {
                // FDI IR lowering does not have [[ThisMode]] information and instead relies on the
                // parser setting these.
                std.debug.assert(!self.formal_parameters.arguments_object_needed);
                std.debug.assert(!self.ecmascript_code.arguments_object_needed);
            }

            const bc = try agent.gc_allocator.create(interpreter.Bytecode);
            errdefer agent.gc_allocator.destroy(bc);
            bc.* = try interpreter.compile(agent, name, .{
                .function = .{
                    .parameters = &self.formal_parameters,
                    .body = &self.ecmascript_code,
                },
            });

            self.cached_bytecode = bc;
            return bc;
        }
    },
    .tag = .ecmascript_function,
    .display_name = "Function",
});

/// 10.2.1 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-ecmascript-function-objects-call-thisargument-argumentslist
fn call(
    agent: *Agent,
    object: *Object,
    this_argument: Value,
    arguments_list: Arguments,
) Agent.Error!Value {
    const function = object.as(ECMAScriptFunction);

    try agent.checkStackOverflow();

    // 1. Let callerContext be the running execution context.
    // NOTE: This is only used to restore the context, which is a simple pop().

    // 2. Let calleeContext be PrepareForOrdinaryCall(F, undefined).
    var callee_context: ExecutionContext = undefined;
    try prepareForOrdinaryCall(agent, function, null, &callee_context);

    // 3. Assert: calleeContext is now the running execution context.
    std.debug.assert(&callee_context == agent.runningExecutionContext());

    // 4. If F.[[IsClassConstructor]] is true, then
    if (function.fields.is_class_constructor) {
        // a. Let error be a newly created TypeError object.
        // b. NOTE: error is created in calleeContext with F's associated Realm Record.
        const err = agent.throwException(.type_error, "{f} is not callable", .{object});

        // c. Remove calleeContext from the execution context stack and restore callerContext as
        //    the running execution context.
        _ = agent.execution_context_stack.pop().?;

        // d. Return ThrowCompletion(error).
        return err;
    }

    // 5. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
    try ordinaryCallBindThis(agent, function, &callee_context, this_argument);

    // 6. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
    const result = ordinaryCallEvaluateBody(agent, function, arguments_list);

    // 7. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
    _ = agent.execution_context_stack.pop().?;

    // 8. If result is a return completion, return result.[[Value]].
    // 9. Assert: result is a throw completion.
    // 10. Return ? result.
    return result;
}

/// 10.2.1.1 PrepareForOrdinaryCall ( F, newTarget )
/// https://tc39.es/ecma262/#sec-prepareforordinarycall
fn prepareForOrdinaryCall(
    agent: *Agent,
    function: *ECMAScriptFunction,
    new_target: ?*Object,
    callee_context: *ExecutionContext,
) std.mem.Allocator.Error!void {
    // 1. Let callerContext be the running execution context.
    // NOTE: This is only used to suspend the context, which we don't do yet.

    // 7. Let localEnv be NewFunctionEnvironment(F, newTarget).
    const local_env = try newFunctionEnvironment(agent.gc_allocator, function, new_target);

    // 2. Let calleeContext be a new ECMAScript code execution context.
    callee_context.* = .{
        // 3. Set the Function of calleeContext to F.
        .origin = .{ .function = &function.object },

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

    // 11. If callerContext is not already suspended, suspend callerContext.

    // 12. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
    try agent.execution_context_stack.append(agent.gc_allocator, callee_context);

    // 13. NOTE: Any exception objects produced after this point are associated with calleeRealm.
    // 14. Return calleeContext.
}

/// 10.2.1.2 OrdinaryCallBindThis ( F, calleeContext, thisArgument )
/// https://tc39.es/ecma262/#sec-ordinarycallbindthis
pub fn ordinaryCallBindThis(
    agent: *Agent,
    function: *ECMAScriptFunction,
    callee_context: *ExecutionContext,
    this_argument: Value,
) std.mem.Allocator.Error!void {
    // 1. Let thisMode be F.[[ThisMode]].
    const this_mode = function.fields.this_mode;

    // 2. If thisMode is lexical, return unused.
    if (this_mode == .lexical) return;

    // 3. Let calleeRealm be F.[[Realm]].
    const callee_realm = function.fields.realm;

    // 4. Let localEnv be the LexicalEnvironment of calleeContext.
    const local_env = callee_context.ecmascript_code.lexical_environment;

    const this_value = blk: {
        // 5. If thisMode is strict, then
        if (this_mode == .strict) {
            // a. Let thisValue be thisArgument.
            break :blk this_argument;
        } else {
            // 6. Else,
            // a. If thisArgument is either undefined or null, then
            if (this_argument.isUndefined() or this_argument.isNull()) {
                // i. Let globalEnv be calleeRealm.[[GlobalEnv]].
                const global_env = callee_realm.global_env;

                // ii. Assert: globalEnv is a Global Environment Record.
                // iii. Let thisValue be globalEnv.[[GlobalThisValue]].
                break :blk Value.from(global_env.global_this_value);
            } else {
                // b. Else,
                // i. Let thisValue be ! ToObject(thisArgument).
                // ii. NOTE: ToObject produces wrapper objects using calleeRealm.
                break :blk Value.from(this_argument.toObject(agent) catch |err| try noexcept(err));
            }
        }
    };

    // 7. Assert: localEnv is a Function Environment Record.
    // 8. Assert: The next step never returns an abrupt completion because
    //    localEnv.[[ThisBindingStatus]] is not initialized.
    // 9. Perform ! BindThisValue(localEnv, thisValue).
    local_env.function_environment.bindThisValue(agent, this_value) catch unreachable;

    // 10. Return unused.
}

/// 10.2.1.4 OrdinaryCallEvaluateBody ( F, argumentsList )
/// https://tc39.es/ecma262/#sec-ordinarycallevaluatebody
pub fn ordinaryCallEvaluateBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: Arguments,
) Agent.Error!Value {
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
        .async => evaluateAsyncFunctionBody(agent, function, arguments_list),
    };
}

/// 15.2.3 Runtime Semantics: EvaluateFunctionBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluatefunctionbody
fn evaluateFunctionBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: Arguments,
) Agent.Error!Value {
    // FunctionBody : FunctionStatementList

    const bc = try function.fields.compile(agent);
    var temp_vm: ?interpreter.Vm = null;
    defer if (temp_vm) |*vm| vm.deinit();

    const vm = agent.active_vm orelse blk: {
        // Create a temporary VM if none is active. This happens when draining the job queue
        // for example.
        temp_vm = try interpreter.Vm.init(agent, bc);
        break :blk &temp_vm.?;
    };

    // 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
    // NOTE: FDI is handled via the generated bytecode.

    // 2. Perform ? Evaluation of FunctionStatementList.
    // 3. NOTE: If the previous step resulted in a normal completion, then evaluation finished by
    //    proceeding past the end of the FunctionStatementList.
    try vm.pushCallFrame(bc, arguments_list.values);
    errdefer vm.popCallFrame();
    const result = try vm.run(.{});

    // 4. Return ReturnCompletion(undefined).
    return switch (result) {
        .@"return" => |value| value orelse .undefined,
        .yield => unreachable,
    };
}

/// 15.5.2 Runtime Semantics: EvaluateGeneratorBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluategeneratorbody
fn evaluateGeneratorBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: Arguments,
) Agent.Error!Value {
    // GeneratorBody : FunctionBody
    // 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
    const bc = try function.fields.compile(agent);
    var temp_vm: ?interpreter.Vm = null;
    defer if (temp_vm) |*vm| vm.deinit();

    const vm = agent.active_vm orelse blk: {
        // Create a temporary VM if none is active. This happens when draining the job queue
        // for example.
        temp_vm = try interpreter.Vm.init(agent, bc);
        break :blk &temp_vm.?;
    };

    try vm.pushCallFrame(bc, arguments_list.values);
    errdefer vm.popCallFrame();
    const result = try vm.run(.{});
    const initial_suspension = switch (result) {
        .yield => |suspension| suspension,
        .@"return" => unreachable,
    };
    errdefer agent.gc_allocator.free(initial_suspension.stack);
    std.debug.assert(initial_suspension.yield_reg == .none);

    // 2. Let G be ? OrdinaryCreateFromConstructor(functionObject, "%GeneratorPrototype%",
    //    « [[GeneratorState]], [[GeneratorContext]], [[GeneratorBrand]] »).
    const generator = try ordinaryCreateFromConstructor(
        builtins.Generator,
        agent,
        &function.object,
        "%GeneratorPrototype%",
        .{
            // 3. Set G.[[GeneratorBrand]] to empty.
            // 4. Set G.[[GeneratorState]] to suspended-start.
            .generator_state = .suspended_start,
            .generator_context = undefined,
            .evaluation_state = undefined,
        },
    );

    // 5. Perform GeneratorStart(G, FunctionBody).
    try generatorStart(agent, generator, function, initial_suspension);

    // 6. Return ReturnCompletion(G).
    return Value.from(&generator.object);
}

/// 15.6.2 Runtime Semantics: EvaluateAsyncGeneratorBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluateasyncgeneratorbody
fn evaluateAsyncGeneratorBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: Arguments,
) Agent.Error!Value {
    // AsyncGeneratorBody : FunctionBody
    // 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
    const bc = try function.fields.compile(agent);
    var temp_vm: ?interpreter.Vm = null;
    defer if (temp_vm) |*vm| vm.deinit();

    const vm = agent.active_vm orelse blk: {
        // Create a temporary VM if none is active. This happens when draining the job queue
        // for example.
        temp_vm = try interpreter.Vm.init(agent, bc);
        break :blk &temp_vm.?;
    };

    try vm.pushCallFrame(bc, arguments_list.values);
    errdefer vm.popCallFrame();
    const result = try vm.run(.{});
    const initial_suspension = switch (result) {
        .yield => |suspension| suspension,
        .@"return" => unreachable,
    };
    errdefer agent.gc_allocator.free(initial_suspension.stack);
    std.debug.assert(initial_suspension.yield_reg == .none);

    // 2. Let generator be ? OrdinaryCreateFromConstructor(functionObject,
    //    "%AsyncGeneratorPrototype%", « [[AsyncGeneratorState]], [[AsyncGeneratorContext]],
    //    [[AsyncGeneratorQueue]], [[GeneratorBrand]] »).
    const generator = try ordinaryCreateFromConstructor(
        builtins.AsyncGenerator,
        agent,
        &function.object,
        "%AsyncGeneratorPrototype%",
        .{
            // 3. Set generator.[[GeneratorBrand]] to empty.
            // 4. Set generator.[[AsyncGeneratorState]] to suspended-start.
            .async_generator_state = .suspended_start,
            .async_generator_context = undefined,
            .async_generator_queue = undefined,
            .evaluation_state = undefined,
        },
    );

    // 5. Perform AsyncGeneratorStart(generator, FunctionBody).
    try asyncGeneratorStart(agent, generator, function, initial_suspension);

    // 6. Return ReturnCompletion(generator).
    return Value.from(&generator.object);
}

/// 15.8.4 Runtime Semantics: EvaluateAsyncFunctionBody
/// https://tc39.es/ecma262/#sec-runtime-semantics-evaluateasyncfunctionbody
fn evaluateAsyncFunctionBody(
    agent: *Agent,
    function: *ECMAScriptFunction,
    arguments_list: Arguments,
) std.mem.Allocator.Error!Value {
    // AsyncFunctionBody : FunctionBody
    const realm = agent.currentRealm();

    // 1. Let promiseCapability be ! NewPromiseCapability(%Promise%).
    const promise_capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 2. Let completion be Completion(FunctionDeclarationInstantiation(functionObject, argumentsList)).
    // 3. If completion is an abrupt completion, then
    //     a. Perform ! Call(promiseCapability.[[Reject]], undefined, « completion.[[Value]] »).
    // 4. Else,
    //     a. Perform AsyncFunctionStart(promiseCapability, FunctionBody).
    // NOTE: FDI is handled via the generated bytecode.
    try asyncFunctionStart(agent, promise_capability, .{ .ecmascript_function = .{
        .function = function,
        .arguments = arguments_list.values,
    } });

    // 5. Return ReturnCompletion(promiseCapability.[[Promise]]).
    return Value.from(promise_capability.promise);
}

/// 10.2.2 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-ecmascript-function-objects-construct-argumentslist-newtarget
fn construct(
    agent: *Agent,
    object: *Object,
    arguments_list: Arguments,
    new_target: *Object,
) Agent.Error!*Object {
    const function = object.as(ECMAScriptFunction);

    try agent.checkStackOverflow();

    // 1. Let callerContext be the running execution context.
    // NOTE: This is only used to restore the context, which is a simple pop().

    // 2. Let kind be F.[[ConstructorKind]].
    const kind = function.fields.constructor_kind;

    var this_argument: *Object = undefined;

    // 3. If kind is base, then
    if (kind == .base) {
        // a. Let thisArgument be ? OrdinaryCreateFromConstructor(newTarget, "%Object.prototype%").
        const this_argument_object = try ordinaryCreateFromConstructor(
            builtins.Object,
            agent,
            new_target,
            "%Object.prototype%",
            {},
        );
        this_argument = &this_argument_object.object;
    }

    // 4. Let calleeContext be PrepareForOrdinaryCall(F, newTarget).
    var callee_context: ExecutionContext = undefined;
    try prepareForOrdinaryCall(agent, function, new_target, &callee_context);

    // 5. Assert: calleeContext is now the running execution context.
    std.debug.assert(&callee_context == agent.runningExecutionContext());

    // 6. If kind is base, then
    if (kind == .base) {
        // a. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
        try ordinaryCallBindThis(agent, function, &callee_context, Value.from(this_argument));

        // b. Let initializeResult be Completion(InitializeInstanceElements(thisArgument, F)).
        const initialize_result = this_argument.initializeInstanceElements(
            agent,
            &function.object,
        );

        // c. If initializeResult is an abrupt completion, then
        initialize_result catch |err| {
            // i. Remove calleeContext from the execution context stack and restore callerContext
            //    as the running execution context.
            _ = agent.execution_context_stack.pop().?;

            // ii. Return ? initializeResult.
            return err;
        };
    }

    // 7. Let constructorEnv be the LexicalEnvironment of calleeContext.
    const constructor_env = callee_context.ecmascript_code.lexical_environment;

    // 8. Let result be Completion(OrdinaryCallEvaluateBody(F, argumentsList)).
    const result = ordinaryCallEvaluateBody(agent, function, arguments_list);

    // 9. Remove calleeContext from the execution context stack and restore callerContext as the
    //    running execution context.
    _ = agent.execution_context_stack.pop().?;

    // 10. If result is a throw completion, then
    //     a. Return ? result.
    // 11. Assert: result is a return completion.
    const value = try result;

    // 12. If result.[[Value]] is an Object, return result.[[Value]].
    if (value.isObject()) return value.asObject();

    // 13. If kind is base, return thisArgument.
    if (kind == .base) return this_argument;

    // 14. If result.[[Value]] is not undefined, throw a TypeError exception.
    if (!value.isUndefined()) {
        return agent.throwException(
            .type_error,
            "Constructor must return an object or undefined",
            .{},
        );
    }

    // 15. Let thisBinding be ? constructorEnv.GetThisBinding().
    const this_binding = try constructor_env.getThisBinding(agent);

    // 16. Assert: thisBinding is an Object.
    std.debug.assert(this_binding.isObject());

    // 17. Return thisBinding.
    return this_binding.asObject();
}

/// 10.2.3 OrdinaryFunctionCreate ( functionPrototype, sourceText, ParameterList, Body, thisMode, env, privateEnv )
/// https://tc39.es/ecma262/#sec-ordinaryfunctioncreate
pub fn ordinaryFunctionCreate(
    agent: *Agent,
    function_prototype: *Object,
    source_text: []const u8,
    parameter_list: ast.FormalParameters,
    body: ast.FunctionBody,
    this_mode: enum { lexical_this, non_lexical_this },
    env: Environment,
    private_env: ?*PrivateEnvironment,
) std.mem.Allocator.Error!*ECMAScriptFunction {
    // 7. Let Strict be IsStrict(Body).
    const strict = body.strict;

    // 1. Let internalSlotsList be the internal slots listed in Table 30.
    // 2. Let F be OrdinaryObjectCreate(functionPrototype, internalSlotsList).
    const function = try ECMAScriptFunction.create(agent, .{
        .internal_methods = .initComptime(.{
            // 3. Set F.[[Call]] to the definition specified in 10.2.1.
            .call = call,
        }),
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

            // 19. Set F.[[PrivateMethods]] to a new empty List.
            .private_methods = &.{},

            // 20. Set F.[[ClassFieldInitializerName]] to empty.
            .class_field_initializer_name = null,

            // NOTE: Not in the spec but we need to provide a value
            .constructor_kind = .base,
        },
    });

    // 21. Let len be the ExpectedArgumentCount of ParameterList.
    const len = parameter_list.expectedArgumentCount();

    // 22. Perform SetFunctionLength(F, len).
    try setFunctionLength(agent, &function.object, @floatFromInt(len));

    // 23. Return F.
    return function;
}

pub fn ordinaryFunctionCreateFast(
    agent: *Agent,
    source_text: []const u8,
    parameter_list: ast.FormalParameters,
    body: ast.FunctionBody,
    env: Environment,
    private_env: ?*PrivateEnvironment,
    name: *const String,
) std.mem.Allocator.Error!*ECMAScriptFunction {
    const length: u53 = @intCast(parameter_list.expectedArgumentCount());
    const strict = body.strict;

    const realm = agent.currentRealm();
    const function_shape, const function_indices = try realm.shapes.ordinaryFunction();
    const prototype_shape, const prototype_indices = try realm.shapes.ordinaryFunctionPrototype();

    const function = try ECMAScriptFunction.createWithShape(agent, .{
        .shape = function_shape,
        .internal_methods = .initComptime(.{
            .call = call,
            .construct = construct,
        }),
        .fields = .{
            .source_text = source_text,
            .formal_parameters = parameter_list,
            .ecmascript_code = body,
            .strict = strict,
            .this_mode = if (strict) .strict else .global,
            .is_class_constructor = false,
            .environment = env,
            .private_environment = private_env,
            .script_or_module = agent.getActiveScriptOrModule().?,
            .realm = realm,
            .home_object = null,
            .fields = &.{},
            .private_methods = &.{},
            .class_field_initializer_name = null,
            .constructor_kind = .base,
        },
    });

    const prototype = try builtins.Object.createWithShape(agent, .{ .shape = prototype_shape });
    prototype.object.setValueAtPropertyIndex(prototype_indices.constructor, Value.from(&function.object));

    function.object.setValueAtPropertyIndex(function_indices.length, Value.from(length));
    function.object.setValueAtPropertyIndex(function_indices.name, Value.from(name));
    function.object.setValueAtPropertyIndex(function_indices.prototype, Value.from(&prototype.object));

    return function;
}

/// 10.2.4 AddRestrictedFunctionProperties ( F, realm )
/// https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
pub fn addRestrictedFunctionProperties(
    agent: *Agent,
    function: *Object,
    realm: *Realm,
) std.mem.Allocator.Error!void {
    // 1. Assert: realm.[[Intrinsics]].[[%ThrowTypeError%]] exists and has been initialized.
    // 2. Let thrower be realm.[[Intrinsics]].[[%ThrowTypeError%]].
    const thrower = try realm.intrinsics.@"%ThrowTypeError%"();

    const property_descriptor: Object.PropertyStorage.CompletePropertyDescriptor = .{
        .value_or_accessor = .{
            .accessor = .{
                .get = thrower,
                .set = thrower,
            },
        },
        .attributes = .builtin_default,
    };

    // 3. Perform ! DefinePropertyOrThrow(F, "caller", PropertyDescriptor {
    //      [[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    try function.definePropertyDirect(
        agent,
        PropertyKey.from("caller"),
        property_descriptor,
    );

    // 4. Perform ! DefinePropertyOrThrow(F, "arguments", PropertyDescriptor {
    //      [[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    try function.definePropertyDirect(
        agent,
        PropertyKey.from("arguments"),
        property_descriptor,
    );

    // 5. Return unused.
}

/// 10.2.5 MakeConstructor ( F [ , writablePrototype [ , prototype ] ] )
/// https://tc39.es/ecma262/#sec-makeconstructor
pub fn makeConstructor(
    agent: *Agent,
    function: *Object,
    args: struct {
        writable_prototype: bool = true,
        prototype: ?*Object = null,
    },
) std.mem.Allocator.Error!void {
    const realm = agent.currentRealm();

    // 1. If F is an ECMAScript function object, then
    if (function.is(ECMAScriptFunction)) {
        // a. Assert: IsConstructor(F) is false.
        std.debug.assert(!Value.from(function).isConstructor());

        // b. Assert: F is an extensible object that does not have a "prototype" own property.
        std.debug.assert(
            function.extensible() and !function.property_storage.contains(PropertyKey.from("prototype")),
        );

        // c. Set F.[[Construct]] to the definition specified in 10.2.2.
        function.internal_methods = try .init(agent.gc_allocator, function.internal_methods, .{ .construct = construct });
    } else {
        // 2. Else,
        // a. Set F.[[Construct]] to the definition specified in 10.3.2.
        function.internal_methods = try .init(agent.gc_allocator, function.internal_methods, .{ .construct = builtins.builtin_function.construct });
    }

    // 3. Set F.[[ConstructorKind]] to base.
    if (function.cast(ECMAScriptFunction)) |ecmascript_function| {
        ecmascript_function.fields.constructor_kind = .base;
    } else if (function.cast(BuiltinFunction)) |builtin_function| {
        if (builtin_function.fields.additional_fields.tryCast(*ClassConstructorFields)) |class_constructor_fields| {
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
        try prototype.definePropertyDirect(agent, PropertyKey.from("constructor"), .{
            .value_or_accessor = .{
                .value = Value.from(function),
            },
            .attributes = .{
                .writable = args.writable_prototype,
                .enumerable = false,
                .configurable = true,
            },
        });

        break :blk prototype;
    };

    // 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
    //      [[Value]]: prototype, [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    try function.definePropertyDirect(agent, PropertyKey.from("prototype"), .{
        .value_or_accessor = .{
            .value = Value.from(prototype),
        },
        .attributes = .{
            .writable = args.writable_prototype,
            .enumerable = false,
            .configurable = false,
        },
    });

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
pub fn makeMethod(function: *ECMAScriptFunction, home_object: *Object) void {
    // 1. Set F.[[HomeObject]] to homeObject.
    function.fields.home_object = home_object;

    // 2. Return unused.
}

/// 10.2.8 DefineMethodProperty ( homeObject, key, closure, enumerable )
/// https://tc39.es/ecma262/#sec-definemethodproperty
pub fn defineMethodProperty(
    agent: *Agent,
    home_object: *Object,
    key: PropertyKeyOrPrivateName,
    closure: *Object,
    enumerable: bool,
) Agent.Error!?PrivateMethodDefinition {
    // 1. Assert: homeObject is an ordinary, extensible object.

    switch (key) {
        // 2. If key is a Private Name, then
        .private_name => |private_name| {
            // a. Return PrivateElement { [[Key]]: key, [[Kind]]: method, [[Value]]: closure }.
            const private_element: PrivateElement = .{ .method = closure };
            return .{ .private_name = private_name, .private_element = private_element };
        },
        // 3. Else,
        .property_key => |property_key| {
            // a. Let desc be the PropertyDescriptor {
            //      [[Value]]: closure, [[Writable]]: true, [[Enumerable]]: enumerable, [[Configurable]]: true
            //    }.
            const property_descriptor: PropertyDescriptor = .{
                .value = Value.from(closure),
                .writable = true,
                .enumerable = enumerable,
                .configurable = true,
            };

            // b. Perform ? DefinePropertyOrThrow(homeObject, key, desc).
            try home_object.definePropertyOrThrow(
                agent,
                property_key,
                property_descriptor,
            );

            // c. NOTE: DefinePropertyOrThrow only returns an abrupt completion when attempting to
            //    define a class static method whose key is "prototype".

            // d. Return unused.
            return null;
        },
    }
}

/// 10.2.9 SetFunctionName ( F, name [ , prefix ] )
/// https://tc39.es/ecma262/#sec-setfunctionname
pub fn setFunctionName(
    agent: *Agent,
    function: *Object,
    key: anytype,
    prefix: ?[]const u8,
) std.mem.Allocator.Error!void {
    comptime std.debug.assert(@TypeOf(key) == PropertyKey or @TypeOf(key) == PropertyKeyOrPrivateName);

    // 1. Assert: F is an extensible object that does not have a "name" own property.
    std.debug.assert(
        function.extensible() and !function.property_storage.contains(PropertyKey.from("name")),
    );

    var name: *const String = switch (if (@TypeOf(key) == PropertyKey) PropertyKeyOrPrivateName{ .property_key = key } else key) {
        .property_key => |property_key| switch (try property_key.toStringOrSymbol(agent)) {
            .string => |string| string,

            // 2. If name is a Symbol, then
            .symbol => |symbol| blk: {
                // a. Let description be name.[[Description]].
                const description = symbol.description orelse {
                    // b. If description is undefined, set name to the empty String.
                    break :blk .empty;
                };

                // c. Else, set name to the string-concatenation of "[", description, and "]".
                break :blk try String.concat(agent, &.{
                    String.fromLiteral("["),
                    description,
                    String.fromLiteral("]"),
                });
            },
        },
        // 3. Else if name is a Private Name, then
        .private_name => |private_name| blk: {
            // a. Set name to name.[[Description]].
            break :blk private_name.symbol.description.?;
        },
    };

    // 4. If F has an [[InitialName]] internal slot, then
    if (function.cast(BuiltinFunction)) |builtin_function| {
        // a. Set F.[[InitialName]] to name.
        builtin_function.fields.initial_name = name;
    }

    // 5. If prefix is present, then
    if (prefix != null) {
        // a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and
        //    name.
        name = try String.concat(agent, &.{
            try String.fromAscii(agent, prefix.?),
            String.fromLiteral(" "),
            name,
        });

        // b. If F has an [[InitialName]] internal slot, then
        if (function.cast(BuiltinFunction)) |builtin_function| {
            // i. NOTE: The choice in the following step is made independently each time this
            //    Abstract Operation is invoked.
            // ii. Optionally, set F.[[InitialName]] to name.
            builtin_function.fields.initial_name = name;
        }
    }

    // 6. Perform ! DefinePropertyOrThrow(F, "name", PropertyDescriptor {
    //      [[Value]]: name, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    try function.definePropertyDirect(agent, PropertyKey.from("name"), .{
        .value_or_accessor = .{
            .value = Value.from(name),
        },
        .attributes = .{
            .writable = false,
            .enumerable = false,
            .configurable = true,
        },
    });

    // 7. Return unused.
}

/// 10.2.10 SetFunctionLength ( F, length )
/// https://tc39.es/ecma262/#sec-setfunctionlength
pub fn setFunctionLength(agent: *Agent, function: *Object, length: f64) std.mem.Allocator.Error!void {
    std.debug.assert(
        std.math.isPositiveInf(length) or
            (std.math.isFinite(length) and std.math.trunc(length) == length and length >= 0),
    );

    // 1. Assert: F is an extensible object that does not have a "length" own property.
    std.debug.assert(
        function.extensible() and !function.property_storage.contains(PropertyKey.from("length")),
    );

    // 2. Perform ! DefinePropertyOrThrow(F, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    try function.definePropertyDirect(agent, PropertyKey.from("length"), .{
        .value_or_accessor = .{
            .value = Value.from(length),
        },
        .attributes = .{
            .writable = false,
            .enumerable = false,
            .configurable = true,
        },
    });

    // 3. Return unused.
}
