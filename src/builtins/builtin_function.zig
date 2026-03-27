//! 10.3 Built-in Function Objects
//! https://tc39.es/ecma262/#sec-built-in-function-objects-call-thisargument-argumentslist

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ClassFieldDefinition = types.ClassFieldDefinition;
const Completion = types.Completion;
const ConstructorKind = builtins.ecmascript_function.ConstructorKind;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PrivateMethodDefinition = types.PrivateMethodDefinition;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const SourceText = builtins.ecmascript_function.SourceText;
const String = types.String;
const Value = types.Value;
const asyncFunctionStart = builtins.asyncFunctionStart;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const setFunctionLength = builtins.setFunctionLength;
const setFunctionName = builtins.setFunctionName;

pub const Behaviour = union(enum) {
    pub const Function = fn (*Agent, Value, Arguments) Agent.Error!Value;
    pub const Constructor = fn (*Agent, Arguments, ?*Object) Agent.Error!Value;

    function: *const Function,
    constructor: *const Constructor,
};

pub const ClassConstructorFields = struct {
    /// [[ConstructorKind]]
    constructor_kind: ConstructorKind,

    /// [[SourceText]]
    source_text: SourceText,

    /// [[PrivateMethods]]
    private_methods: []const PrivateMethodDefinition,

    /// [[Fields]]
    fields: []const ClassFieldDefinition,
};

pub const BuiltinFunction = MakeObject(.{
    .Fields = struct {
        behaviour: Behaviour,

        /// [[Realm]]
        realm: *Realm,

        /// [[InitialName]]
        initial_name: ?*const String,

        /// [[Async]]
        async: bool,

        additional_fields: SafePointer,
    },
    .tag = .builtin_function,
    .display_name = "Builtin Function",
});

/// 10.3.1 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-built-in-function-objects-call-thisargument-argumentslist
fn call(
    agent: *Agent,
    object: *Object,
    this_argument: Value,
    arguments_list: Arguments,
) Agent.Error!Value {
    const function = object.as(BuiltinFunction);

    // 1. Return ? BuiltinCallOrConstruct(F, thisArgument, argumentsList, undefined).
    return builtinCallOrConstruct(agent, function, this_argument, arguments_list, null);
}

/// 10.3.2 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-built-in-function-objects-construct-argumentslist-newtarget
pub fn construct(
    agent: *Agent,
    object: *Object,
    arguments_list: Arguments,
    new_target: *Object,
) Agent.Error!*Object {
    const function = object.as(BuiltinFunction);

    // 1. Let result be ? BuiltinCallOrConstruct(F, uninitialized, argumentsList, newTarget).
    const result = try builtinCallOrConstruct(agent, function, null, arguments_list, new_target);

    // 2. Assert: result is an Object.
    // 3. Return result.
    return result.asObject();
}

/// 10.3.3 BuiltinCallOrConstruct ( F, thisArgument, argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-builtincallorconstruct
pub fn builtinCallOrConstruct(
    agent: *Agent,
    builtin_function: *BuiltinFunction,
    this_argument: ?Value,
    arguments_list: Arguments,
    new_target: ?*Object,
) Agent.Error!Value {
    // 1. Let callerContext be the running execution context.
    // 2. If callerContext is not already suspended, suspend callerContext.

    // 3. Let calleeContext be a new execution context.
    var callee_context: ExecutionContext = .{
        // 4. Set the Function of calleeContext to F.
        .origin = .{ .function = &builtin_function.object },

        // 5. Let calleeRealm be F.[[Realm]].
        // 6. Set the Realm of calleeContext to calleeRealm.
        .realm = builtin_function.fields.realm,

        // 7. Set the ScriptOrModule of calleeContext to null.
        .script_or_module = null,

        .ecmascript_code = undefined,
    };

    // 8. Perform any necessary implementation-defined initialization of calleeContext.

    // 9. Push calleeContext onto the execution context stack; calleeContext is now the running
    //    execution context.
    try agent.execution_context_stack.append(agent.gc_allocator, &callee_context);

    // 10. If F.[[Async]] is true, then
    if (builtin_function.fields.async) {
        const realm = agent.currentRealm();

        // a. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        const Captures = struct {
            builtin_function: *BuiltinFunction,
            this_argument: ?Value,
            arguments_list: Arguments,
            new_target: ?*Object,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .builtin_function = builtin_function,
            .this_argument = this_argument,
            .arguments_list = arguments_list,
            .new_target = new_target,
        };

        // b. Let resultsClosure be a new Abstract Closure with no parameters that captures F,
        //    thisArgument, argumentsList, and newTarget and performs the following steps when
        //    called:
        const resultsClosure = struct {
            fn func(agent_: *Agent, captures_: SafePointer) Agent.Error!Completion {
                const builtin_function_ = captures_.cast(*Captures).builtin_function;
                const this_argument_ = captures_.cast(*Captures).this_argument;
                const arguments_list_ = captures_.cast(*Captures).arguments_list;
                const new_target_ = captures_.cast(*Captures).new_target;

                // i. Let result be the Completion Record that is the result of evaluating F in a
                //    manner that conforms to the specification of F. If thisArgument is
                //    uninitialized, the this value is uninitialized; else thisArgument provides
                //    the this value. argumentsList provides the named parameters. newTarget
                //    provides the NewTarget value.
                // ii. NOTE: If F is defined in this document, “the specification of F” is the
                //     behaviour specified for it via algorithm steps or other means.
                const result = switch (builtin_function_.fields.behaviour) {
                    .function => |function| function(agent_, this_argument_.?, arguments_list_),
                    .constructor => |constructor| constructor(agent_, arguments_list_, new_target_),
                };

                // iii. Return Completion(result).
                return .@"return"(try result);
            }
        }.func;

        // c. Perform AsyncFunctionStart(promiseCapability, resultsClosure).
        try asyncFunctionStart(agent, promise_capability, .{
            .abstract_closure = .{
                .func = resultsClosure,
                .captures = .make(*Captures, captures),
            },
        });

        // d. Remove calleeContext from the execution context stack and restore callerContext as
        //    the running execution context.
        _ = agent.execution_context_stack.pop().?;

        // e. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    // 11. Let result be the Completion Record that is the result of evaluating F in a manner that
    //     conforms to the specification of F. If thisArgument is uninitialized, the this value is
    //     uninitialized; else thisArgument provides the this value. argumentsList provides the
    //     named parameters. newTarget provides the NewTarget value.
    // 12. NOTE: If F is defined in this document, “the specification of F” is the behaviour
    //     specified for it via algorithm steps or other means.
    const result = switch (builtin_function.fields.behaviour) {
        .function => |function| function(agent, this_argument.?, arguments_list),
        .constructor => |constructor| constructor(agent, arguments_list, new_target),
    };

    // 13. Remove calleeContext from the execution context stack and restore callerContext as
    //     the running execution context.
    _ = agent.execution_context_stack.pop().?;

    // 14. Return ? result.
    return result;
}

/// 10.3.4 CreateBuiltinFunction ( behaviour, length, name, additionalInternalSlotsList [ , realm [ , prototype [ , prefix [ , async ] ] ] ] )
/// https://tc39.es/ecma262/#sec-createbuiltinfunction
pub fn createBuiltinFunction(
    agent: *Agent,
    comptime behaviour: Behaviour,
    comptime length: u32,
    comptime maybe_name: ?[]const u8,
    args: struct {
        realm: ?*Realm = null,
        // NOTE: I don't think any builtin functions are created with a null prototype,
        //       so the null state can serve as 'not present'.
        prototype: ?*Object = null,
        prefix: ?[]const u8 = null,
        async: ?bool = null,
        additional_fields: SafePointer = .null_pointer,
    },
) std.mem.Allocator.Error!*BuiltinFunction {
    // 1. If realm is not present, set realm to the current Realm Record.
    const realm = args.realm orelse agent.currentRealm();

    // 2. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%Function.prototype%]].
    const prototype = args.prototype orelse try realm.intrinsics.@"%Function.prototype%"();

    // 3. If async is not present, set async to false.
    const async = args.async orelse false;

    // 4. Let internalSlotsList be a List containing the names of all the internal slots that 10.3
    //    requires for the built-in function object that is about to be created.
    // 5. Append to internalSlotsList the elements of additionalInternalSlotsList.

    // 6. Let func be a new built-in function object that, when called, performs the action
    //    described by behaviour using the provided arguments as the values of the corresponding
    //    parameters specified by behaviour. The new function object has internal slots whose names
    //    are the elements of internalSlotsList, and an [[InitialName]] internal slot.
    const function = try BuiltinFunction.create(agent, .{
        .internal_methods = .initComptime(.{
            .call = call,
            .construct = if (behaviour == .constructor) construct else null,
        }),

        // 8. Set func.[[Prototype]] to prototype.
        .prototype = prototype,

        // 9. Set func.[[Extensible]] to true.
        .extensible = true,

        .fields = .{
            .behaviour = behaviour,

            // 10. Set func.[[Realm]] to realm.
            .realm = realm,

            // 11. Set func.[[InitialName]] to null.
            .initial_name = null,

            // 7. Set func.[[Async]] to async.
            .async = async,

            .additional_fields = args.additional_fields,
        },
    });

    // 12. Perform SetFunctionLength(func, length).
    try setFunctionLength(agent, &function.object, @floatFromInt(length));

    // 13. If prefix is not present, then
    //     a. Perform SetFunctionName(func, name).
    // 14. Else,
    //     a. Perform SetFunctionName(func, name, prefix).
    // NOTE: We make the name optional because classDefinitionEvaluation() calls createBuiltinFunction()
    //       with a runtime-known name and we want to keep the assertion in setFunctionName() to
    //       ensure it only gets called once. It's the caller's responsibility to install the
    //       function name after the fact.
    if (maybe_name) |name| {
        try setFunctionName(agent, &function.object, PropertyKey.from(name), args.prefix);
    }

    // 15. Return func.
    return function;
}
