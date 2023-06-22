//! 10.2 ECMAScript Function Objects
//! https://tc39.es/ecma262/#sec-ecmascript-function-objects

const std = @import("std");

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
const Completion = types.Completion;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const Object = types.Object;
const PrivateEnvironment = execution.PrivateEnvironment;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const ScriptOrModule = execution.ScriptOrModule;
const Value = types.Value;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const newFunctionEnvironment = execution.newFunctionEnvironment;
const noexcept = utils.noexcept;
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

pub const ECMAScriptFunction = Object.Factory(.{
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

        ///  [[SourceText]]
        source_text: []const u8,

        // TODO: [[Fields]],  [[PrivateMethods]], [[ClassFieldInitializerName]]

        /// [[IsClassConstructor]]
        is_class_constructor: bool,
    },
    .tag = .ecmascript_function,
});

/// 10.2.1 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-ecmascript-function-objects-call-thisargument-argumentslist
fn call(object: Object, this_argument: Value, arguments_list: ArgumentsList) !Value {
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
        const err = agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{object}),
        );

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

    // 8. If result.[[Type]] is return, return result.[[Value]].
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
fn prepareForOrdinaryCall(agent: *Agent, function: *ECMAScriptFunction, new_target: ?Object) !*ExecutionContext {
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
) !void {
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
) !Completion {
    _ = arguments_list;
    // 1. Return ? EvaluateBody of F.[[ECMAScriptCode]] with arguments F and argumentsList.
    // TODO: Implement this closer to spec and support arguments :^)
    return generateAndRunBytecode(agent, function.fields.ecmascript_code);
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
) !Object {
    // 7. If the source text matched by Body is strict mode code, let Strict be true; else let
    //    Strict be false.
    const strict = body.functionBodyContainsUseStrict();

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

            // TODO: 18. Set F.[[Fields]] to a new empty List.
            // TODO: 19. Set F.[[PrivateMethods]] to a new empty List.
            // TODO: 20. Set F.[[ClassFieldInitializerName]] to empty.

            // NOTE: Not in the spec but we need to provide a value
            .constructor_kind = .base,
        },
    });

    // 21. Let len be the ExpectedArgumentCount of ParameterList.
    const len = parameter_list.expectedArgumentCount();

    // 22. Perform SetFunctionLength(F, len).
    try setFunctionLength(function, @floatFromInt(f64, len));

    // 23. Return F.
    return function;
}

/// 10.2.4 AddRestrictedFunctionProperties ( F, realm )
/// https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
pub fn addRestrictedFunctionProperties(function: Object, realm: *Realm) !void {
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
) !void {
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

        // TODO: c. Set F.[[Construct]] to the definition specified in 10.2.2.
    }
    // 2. Else,
    else {
        // a. Set F.[[Construct]] to the definition specified in 10.3.2.
        function.internalMethods().construct = builtin_function.construct;
    }

    // 3. Set F.[[ConstructorKind]] to base.
    if (function.is(ECMAScriptFunction)) function.as(ECMAScriptFunction).fields.constructor_kind = .base;

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

/// 10.2.9 SetFunctionName ( F, name [ , prefix ] )
/// https://tc39.es/ecma262/#sec-setfunctionname
pub fn setFunctionName(
    function: Object,
    name_property_key: PropertyKey,
    prefix: ?[]const u8,
) !void {
    const agent = function.agent();

    // 1. Assert: F is an extensible object that does not have a "name" own property.
    std.debug.assert(
        function.extensible().* and !function.propertyStorage().has(PropertyKey.from("name")),
    );

    var name = switch (name_property_key) {
        .string => |string| string,
        .integer_index => |integer_index| try std.fmt.allocPrint(
            agent.gc_allocator,
            "{d}",
            .{integer_index},
        ),

        // 2. If name is a Symbol, then
        .symbol => |symbol| blk: {
            // a. Let description be name's [[Description]] value.
            const description = symbol.description;

            // b. If description is undefined, set name to the empty String.
            if (description == null) break :blk "";

            // c. Else, set name to the string-concatenation of "[", description, and "]".
            break :blk try std.fmt.allocPrint(agent.gc_allocator, "[{s}]", .{description.?});
        },

        // TODO: 3. Else if name is a Private Name, then
        //     a. Set name to name.[[Description]].
    };

    // 4. If F has an [[InitialName]] internal slot, then
    //     a. Set F.[[InitialName]] to name.
    function.as(BuiltinFunction).fields.initial_name = name;

    // 5. If prefix is present, then
    if (prefix != null) {
        // a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and
        //    name.
        name = try std.fmt.allocPrint(agent.gc_allocator, "{s} {s}", .{ prefix.?, name });

        // b. If F has an [[InitialName]] internal slot, then
        //     i. Optionally, set F.[[InitialName]] to name.
        function.as(BuiltinFunction).fields.initial_name = name;
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
pub fn setFunctionLength(function: Object, length: f64) !void {
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
