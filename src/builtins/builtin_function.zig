//! 10.3 Built-in Function Objects
//! https://tc39.es/ecma262/#sec-built-in-function-objects-call-thisargument-argumentslist

const std = @import("std");

const ecmascript_function = @import("ecmascript_function.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ClassFieldDefinition = types.ClassFieldDefinition;
const ConstructorKind = ecmascript_function.ConstructorKind;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PrivateMethodDefinition = types.PrivateMethodDefinition;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const setFunctionLength = ecmascript_function.setFunctionLength;
const setFunctionName = ecmascript_function.setFunctionName;

pub const Behaviour = union(enum) {
    pub const Function = fn (*Agent, Value, Arguments) Agent.Error!Value;
    pub const Constructor = fn (*Agent, Arguments, ?Object) Agent.Error!Value;

    function: *const Function,
    constructor: *const Constructor,
};

pub const ClassConstructorFields = struct {
    /// [[ConstructorKind]]
    constructor_kind: ConstructorKind,

    /// [[SourceText]]
    source_text: []const u8,

    // TODO: These are not in the spec yet - https://github.com/tc39/ecma262/issues/3204

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
        initial_name: ?String,

        additional_fields: SafePointer,
    },
    .tag = .builtin_function,
});

/// 10.3.1 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-built-in-function-objects-call-thisargument-argumentslist
fn call(
    object: Object,
    this_argument: Value,
    arguments_list: Arguments,
) Agent.Error!Value {
    const agent = object.agent();
    const function = object.as(BuiltinFunction);

    // 1. Return ? BuiltinCallOrConstruct(F, thisArgument, argumentsList, undefined).
    return builtinCallOrConstruct(agent, function, this_argument, arguments_list, null);
}

/// 10.3.2 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-built-in-function-objects-construct-argumentslist-newtarget
pub fn construct(
    object: Object,
    arguments_list: Arguments,
    new_target: Object,
) Agent.Error!Object {
    const agent = object.agent();
    const function = object.as(BuiltinFunction);

    // 1. Return ? BuiltinCallOrConstruct(F, uninitialized, argumentsList, newTarget).
    return (try builtinCallOrConstruct(agent, function, null, arguments_list, new_target)).asObject();
}

/// 10.3.3 BuiltinCallOrConstruct ( F, thisArgument, argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-builtincallorconstruct
pub fn builtinCallOrConstruct(
    agent: *Agent,
    builtin_function: *BuiltinFunction,
    this_argument: ?Value,
    arguments_list: Arguments,
    new_target: ?Object,
) Agent.Error!Value {
    // 1. Let callerContext be the running execution context.
    const caller_context = agent.runningExecutionContext();

    // TODO: 2. If callerContext is not already suspended, suspend callerContext.
    _ = caller_context;

    // 3. Let calleeContext be a new execution context.
    const callee_context: ExecutionContext = .{
        // 4. Set the Function of calleeContext to F.
        .function = builtin_function.object(),

        // 5. Let calleeRealm be F.[[Realm]].
        // 6. Set the Realm of calleeContext to calleeRealm.
        .realm = builtin_function.fields.realm,

        // 7. Set the ScriptOrModule of calleeContext to null.
        .script_or_module = null,
    };

    // 8. Perform any necessary implementation-defined initialization of calleeContext.

    // 9. Push calleeContext onto the execution context stack; calleeContext is now the running
    //    execution context.
    try agent.execution_context_stack.append(callee_context);

    // 10. Let result be the Completion Record that is the result of evaluating F in a manner that
    //     conforms to the specification of F. If thisArgument is uninitialized, the this value is
    //     uninitialized; otherwise, thisArgument provides the this value. argumentsList provides
    //     the named parameters. newTarget provides the NewTarget value.
    // 11. NOTE: If F is defined in this document, “the specification of F” is the behaviour
    //     specified for it via algorithm steps or other means.
    const result = switch (builtin_function.fields.behaviour) {
        .function => |function| function(agent, this_argument.?, arguments_list),
        .constructor => |constructor| constructor(agent, arguments_list, new_target),
    };

    // 12. Remove calleeContext from the execution context stack and restore callerContext as the
    //     running execution context.
    _ = agent.execution_context_stack.pop();

    // 13. Return ? result.
    return result;
}

/// 10.3.4 CreateBuiltinFunction ( behaviour, length, name, additionalInternalSlotsList [ , realm [ , prototype [ , prefix ] ] ] )
/// https://tc39.es/ecma262/#sec-createbuiltinfunction
pub fn createBuiltinFunction(
    agent: *Agent,
    behaviour: Behaviour,
    args: struct {
        length: u32,
        name: []const u8,
        realm: ?*Realm = null,
        // NOTE: I don't think any builtin functions are created with a null prototype,
        //       so the null state can serve as 'not present'.
        prototype: ?Object = null,
        prefix: ?[]const u8 = null,
        additional_fields: SafePointer = .null_pointer,
    },
) std.mem.Allocator.Error!Object {
    // 1. If realm is not present, set realm to the current Realm Record.
    const realm = args.realm orelse agent.currentRealm();

    // 2. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%Function.prototype%]].
    const prototype = args.prototype orelse try realm.intrinsics.@"%Function.prototype%"();

    // 3. Let internalSlotsList be a List containing the names of all the internal slots that 10.3
    //    requires for the built-in function object that is about to be created.
    // 4. Append to internalSlotsList the elements of additionalInternalSlotsList.

    // 5. Let func be a new built-in function object that, when called, performs the action
    //    described by behaviour using the provided arguments as the values of the corresponding
    //    parameters specified by behaviour. The new function object has internal slots whose names
    //    are the elements of internalSlotsList, and an [[InitialName]] internal slot.
    const function = try BuiltinFunction.create(agent, .{
        .internal_methods = if (behaviour == .constructor) &.{
            .call = call,
            .construct = construct,
        } else &.{ .call = call },

        // 6. Set func.[[Prototype]] to prototype.
        .prototype = prototype,

        // 7. Set func.[[Extensible]] to true.
        .extensible = true,

        .fields = .{
            .behaviour = behaviour,

            // 8. Set func.[[Realm]] to realm.
            .realm = realm,

            // 9. Set func.[[InitialName]] to null.
            .initial_name = null,

            .additional_fields = args.additional_fields,
        },
    });

    // 10. Perform SetFunctionLength(func, length).
    try setFunctionLength(function, @floatFromInt(args.length));

    // 11. If prefix is not present, then
    //     a. Perform SetFunctionName(func, name).
    // 12. Else,
    //     a. Perform SetFunctionName(func, name, prefix).
    try setFunctionName(
        function,
        PropertyKey.from(try String.fromAscii(agent.gc_allocator, args.name)),
        args.prefix,
    );

    // 13. Return func.
    return function;
}
