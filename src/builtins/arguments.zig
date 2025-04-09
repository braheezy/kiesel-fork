//! 10.4.4 Arguments Exotic Objects
//! https://tc39.es/ecma262/#sec-arguments-exotic-objects

const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments_ = types.Arguments;
const Environment = execution.Environment;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryDelete = builtins.ordinaryDelete;
const ordinaryGet = builtins.ordinaryGet;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const ordinarySet = builtins.ordinarySet;
const sameValue = types.sameValue;

/// 10.4.4.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-getownproperty-p
fn getOwnProperty(
    _: *Agent,
    object: *Object,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    // 1. Let desc be OrdinaryGetOwnProperty(args, P).
    var descriptor = (ordinaryGetOwnProperty(object, property_key) catch unreachable) orelse {
        // 2. If desc is undefined, return undefined.
        return null;
    };

    // 3. Let map be args.[[ParameterMap]].
    const map = object.as(Arguments).fields.parameter_map;

    // 4. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.hasOwnProperty(property_key) catch |err| try noexcept(err);

    // 5. If isMapped is true, then
    if (is_mapped) {
        // a. Set desc.[[Value]] to ! Get(map, P).
        descriptor.value = map.get(property_key) catch |err| try noexcept(err);
    }

    // 6. Return desc.
    return descriptor;
}

/// 10.4.4.2 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-defineownproperty-p-desc
fn defineOwnProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) std.mem.Allocator.Error!bool {
    // 1. Let map be args.[[ParameterMap]].
    const map = object.as(Arguments).fields.parameter_map;

    // 2. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.hasOwnProperty(property_key) catch |err| try noexcept(err);

    // 3. Let newArgDesc be Desc.
    var new_arg_property_descriptor = property_descriptor;

    // 4. If isMapped is true and IsDataDescriptor(Desc) is true, then
    if (is_mapped and property_descriptor.isDataDescriptor()) {
        // a. If Desc does not have a [[Value]] field, Desc has a [[Writable]] field, and
        //    Desc.[[Writable]] is false, then
        if (property_descriptor.value == null and property_descriptor.writable == false) {
            // i. Set newArgDesc to a copy of Desc.

            // ii. Set newArgDesc.[[Value]] to ! Get(map, P).
            new_arg_property_descriptor.value = map.get(property_key) catch |err| try noexcept(err);
        }
    }

    // 5. Let allowed be ! OrdinaryDefineOwnProperty(args, P, newArgDesc).
    const allowed = ordinaryDefineOwnProperty(
        agent,
        object,
        property_key,
        new_arg_property_descriptor,
    ) catch |err| try noexcept(err);

    // 6. If allowed is false, return false.
    if (!allowed) return false;

    // 7. If isMapped is true, then
    if (is_mapped) {
        // a. If IsAccessorDescriptor(Desc) is true, then
        if (property_descriptor.isAccessorDescriptor()) {
            // i. Perform ! map.[[Delete]](P).
            _ = map.internal_methods.delete(
                agent,
                map,
                property_key,
            ) catch |err| try noexcept(err);
        } else {
            // b. Else,
            // i. If Desc has a [[Value]] field, then
            if (property_descriptor.value) |value| {
                // 1. Assert: The following Set will succeed, since formal parameters mapped by
                //    arguments objects are always writable.
                // 2. Perform ! Set(map, P, Desc.[[Value]], false).
                map.set(property_key, value, .ignore) catch |err| try noexcept(err);
            }

            // ii. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, then
            if (property_descriptor.writable == false) {
                // 1. Perform ! map.[[Delete]](P).
                _ = map.internal_methods.delete(
                    agent,
                    map,
                    property_key,
                ) catch |err| try noexcept(err);
            }
        }
    }

    // 8. Return true.
    return true;
}

/// 10.4.4.3 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-get-p-receiver
fn get(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    receiver: Value,
) Agent.Error!Value {
    // 1. Let map be args.[[ParameterMap]].
    const map = object.as(Arguments).fields.parameter_map;

    // 2. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.hasOwnProperty(property_key) catch |err| try noexcept(err);

    // 3. If isMapped is false, then
    if (!is_mapped) {
        // a. Return ? OrdinaryGet(args, P, Receiver).
        return ordinaryGet(agent, object, property_key, receiver);
    } else {
        // 4. Else,
        // a. Assert: map contains a formal parameter mapping for P.
        // b. Return ! Get(map, P).
        return map.get(property_key) catch |err| try noexcept(err);
    }
}

/// 10.4.4.4 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-set-p-v-receiver
fn set(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool {
    // 1. If SameValue(args, Receiver) is false, then
    //     a. Let isMapped be false.
    // 2. Else,
    if (sameValue(Value.from(object), receiver)) {
        // a. Let map be args.[[ParameterMap]].
        const map = object.as(Arguments).fields.parameter_map;

        // b. Let isMapped be ! HasOwnProperty(map, P).
        const is_mapped = map.hasOwnProperty(property_key) catch |err| try noexcept(err);

        // 3. If isMapped is true, then
        if (is_mapped) {
            // a. Assert: The following Set will succeed, since formal parameters mapped by
            //    arguments objects are always writable.
            // b. Perform ! Set(map, P, V, false).
            map.set(property_key, value, .ignore) catch |err| try noexcept(err);
        }
    }

    // 4. Return ? OrdinarySet(args, P, V, Receiver).
    return ordinarySet(agent, object, property_key, value, receiver);
}

/// 10.4.4.5 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-delete-p
fn delete(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Let map be args.[[ParameterMap]].
    const map = object.as(Arguments).fields.parameter_map;

    // 2. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.hasOwnProperty(property_key) catch |err| try noexcept(err);

    // 3. Let result be ? OrdinaryDelete(args, P).
    const result = try ordinaryDelete(agent, object, property_key);

    // 4. If result is true and isMapped is true, then
    if (result and is_mapped) {
        // a. Perform ! map.[[Delete]](P).
        _ = map.internal_methods.delete(agent, map, property_key) catch |err| try noexcept(err);
    }

    // 5. Return result.
    return result;
}

/// 10.4.4.6 CreateUnmappedArgumentsObject ( argumentsList )
/// https://tc39.es/ecma262/#sec-createunmappedargumentsobject
pub fn createUnmappedArgumentsObject(
    agent: *Agent,
    arguments_list: []const Value,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Let len be the number of elements in argumentsList.
    const len = arguments_list.len;

    // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%, « [[ParameterMap]] »).
    const object = try Arguments.create(agent, .{
        .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        .fields = .{
            // 3. Set obj.[[ParameterMap]] to undefined.
            .parameter_map = undefined,
        },
    });

    // 4. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(len), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    try object.definePropertyDirect(PropertyKey.from("length"), .{
        .value = Value.from(@as(u53, @intCast(len))),
        .writable = true,
        .enumerable = false,
        .configurable = true,
    });

    // 5. Let index be 0.
    // 6. Repeat, while index < len,
    for (arguments_list, 0..) |value, index| {
        // a. Let val be argumentsList[index].
        // b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
        try object.createDataPropertyDirect(
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index))),
            value,
        );

        // c. Set index to index + 1.
    }

    // 7. Perform ! DefinePropertyOrThrow(obj, %Symbol.iterator%, PropertyDescriptor {
    //      [[Value]]: %Array.prototype.values%, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    try object.definePropertyDirect(PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"), .{
        .value = Value.from(try realm.intrinsics.@"%Array.prototype.values%"()),
        .writable = true,
        .enumerable = false,
        .configurable = true,
    });

    // 8. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor {
    //      [[Get]]: %ThrowTypeError%, [[Set]]: %ThrowTypeError%, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    try object.definePropertyDirect(PropertyKey.from("callee"), .{
        .get = try realm.intrinsics.@"%ThrowTypeError%"(),
        .set = try realm.intrinsics.@"%ThrowTypeError%"(),
        .enumerable = false,
        .configurable = false,
    });

    // 9. Return obj.
    return object;
}

/// 10.4.4.7 CreateMappedArgumentsObject ( func, formals, argumentsList, env )
/// https://tc39.es/ecma262/#sec-createmappedargumentsobject
pub fn createMappedArgumentsObject(
    agent: *Agent,
    function: *Object,
    formals: ast.FormalParameters,
    arguments_list: []const Value,
    env: Environment,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Assert: formals does not contain a rest parameter, any binding patterns, or any
    //    initializers. It may contain duplicate identifiers.
    if (std.debug.runtime_safety) for (formals.items) |item| {
        std.debug.assert(item == .formal_parameter);
    };

    // 2. Let len be the number of elements in argumentsList.
    const len = arguments_list.len;

    // 3. Let obj be MakeBasicObject(« [[Prototype]], [[Extensible]], [[ParameterMap]] »).
    const object = try Arguments.create(agent, .{
        .internal_methods = &.{
            // 4. Set obj.[[GetOwnProperty]] as specified in 10.4.4.1.
            .getOwnProperty = getOwnProperty,

            // 5. Set obj.[[DefineOwnProperty]] as specified in 10.4.4.2.
            .defineOwnProperty = defineOwnProperty,

            // 6. Set obj.[[Get]] as specified in 10.4.4.3.
            .get = get,

            // 7. Set obj.[[Set]] as specified in 10.4.4.4.
            .set = set,

            // 8. Set obj.[[Delete]] as specified in 10.4.4.5.
            .delete = delete,
        },

        // 9. Set obj.[[Prototype]] to %Object.prototype%.
        .prototype = try realm.intrinsics.@"%Object.prototype%"(),

        .fields = .{ .parameter_map = undefined },
    });

    // 10. Let map be OrdinaryObjectCreate(null).
    const map = try ordinaryObjectCreate(agent, null);

    // 11. Set obj.[[ParameterMap]] to map.
    object.as(Arguments).fields.parameter_map = map;

    // 12. Let parameterNames be the BoundNames of formals.
    var parameter_names: std.ArrayListUnmanaged(ast.Identifier) = .empty;
    defer parameter_names.deinit(agent.gc_allocator);
    try formals.collectBoundNames(agent.gc_allocator, &parameter_names);

    // 13. Let numberOfParameters be the number of elements in parameterNames.
    const number_of_parameters = parameter_names.items.len;

    // 14. Let index be 0.
    // 15. Repeat, while index < len,
    for (arguments_list, 0..) |value, index| {
        // a. Let val be argumentsList[index].
        // b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
        try object.createDataPropertyDirect(
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index))),
            value,
        );

        // c. Set index to index + 1.
    }

    // 16. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor {
    //       [[Value]]: 𝔽(len), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //     }).
    try object.definePropertyDirect(PropertyKey.from("length"), .{
        .value = Value.from(@as(u53, @intCast(len))),
        .writable = true,
        .enumerable = false,
        .configurable = true,
    });

    // 17. Let mappedNames be a new empty List.
    var mapped_names: std.StringHashMapUnmanaged(void) = .empty;
    defer mapped_names.deinit(agent.gc_allocator);

    // 18. Set index to numberOfParameters - 1.
    var index: ?u53 = std.math.sub(u53, @intCast(number_of_parameters), 1) catch null;

    // 19. Repeat, while index ≥ 0,
    while (index != null) : (index = (std.math.sub(u53, index.?, 1) catch null)) {
        // a. Let name be parameterNames[index].
        const name = parameter_names.items[@intCast(index.?)];

        // b. If mappedNames does not contain name, then
        if (!mapped_names.contains(name)) {
            // i. Append name to mappedNames.
            try mapped_names.put(agent.gc_allocator, name, {});

            // ii. If index < len, then
            if (index.? < len) {
                // 1. Let g be MakeArgGetter(name, env).
                const getter = try makeArgGetter(
                    agent,
                    try String.fromUtf8(agent, name),
                    env,
                );

                // 2. Let p be MakeArgSetter(name, env).
                const setter = try makeArgSetter(
                    agent,
                    try String.fromUtf8(agent, name),
                    env,
                );

                // 3. Perform ! map.[[DefineOwnProperty]](! ToString(𝔽(index)), PropertyDescriptor {
                //      [[Set]]: p, [[Get]]: g, [[Enumerable]]: false, [[Configurable]]: true
                //    }).
                _ = map.internal_methods.defineOwnProperty(agent, map, PropertyKey.from(index.?), .{
                    .set = setter,
                    .get = getter,
                    .enumerable = false,
                    .configurable = true,
                }) catch |err| try noexcept(err);
            }
        }

        // c. Set index to index - 1.
    }

    // 20. Perform ! DefinePropertyOrThrow(obj, %Symbol.iterator%, PropertyDescriptor {
    //       [[Value]]: %Array.prototype.values%, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //     }).
    try object.definePropertyDirect(PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"), .{
        .value = Value.from(try realm.intrinsics.@"%Array.prototype.values%"()),
        .writable = true,
        .enumerable = false,
        .configurable = true,
    });

    // 21. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor {
    //       [[Value]]: func, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //     }).
    try object.definePropertyDirect(PropertyKey.from("callee"), .{
        .value = Value.from(function),
        .writable = true,
        .enumerable = false,
        .configurable = true,
    });

    // 22. Return obj.
    return object;
}

/// 10.4.4.7.1 MakeArgGetter ( name, env )
/// https://tc39.es/ecma262/#sec-makearggetter
fn makeArgGetter(
    agent: *Agent,
    name: *const String,
    env: Environment,
) std.mem.Allocator.Error!*Object {
    const Captures = struct {
        name: *const String,
        env: Environment,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .name = name, .env = env };

    // 1. Let getterClosure be a new Abstract Closure with no parameters that captures name and env
    //    and performs the following steps when called:
    const getter_closure = struct {
        fn func(agent_: *Agent, _: Value, _: Arguments_) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const name_ = captures_.name;
            const env_ = captures_.env;

            // a. Return env.GetBindingValue(name, false).
            return env_.getBindingValue(agent_, name_, false);
        }
    }.func;

    // 2. Let getter be CreateBuiltinFunction(getterClosure, 0, "", « »).
    // 3. NOTE: getter is never directly accessible to ECMAScript code.
    const getter = try createBuiltinFunction(
        agent,
        .{ .function = getter_closure },
        1,
        "",
        .{ .additional_fields = .make(*Captures, captures) },
    );

    // 4. Return getter.
    return getter;
}

/// 10.4.4.7.2 MakeArgSetter ( name, env )
/// https://tc39.es/ecma262/#sec-makeargsetter
fn makeArgSetter(
    agent: *Agent,
    name: *const String,
    env: Environment,
) std.mem.Allocator.Error!*Object {
    const Captures = struct {
        name: *const String,
        env: Environment,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .name = name, .env = env };

    // 1. Let setterClosure be a new Abstract Closure with parameters (value) that captures name
    //    and env and performs the following steps when called:
    const setter_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments: Arguments_) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const name_ = captures_.name;
            const env_ = captures_.env;
            const value = arguments.get(0);

            // a. Return ! env.SetMutableBinding(name, value, false).
            env_.setMutableBinding(agent_, name_, value, false) catch |err| try noexcept(err);
            return .undefined;
        }
    }.func;

    // 2. Let setter be CreateBuiltinFunction(setterClosure, 1, "", « »).
    // 3. NOTE: setter is never directly accessible to ECMAScript code.
    const setter = try createBuiltinFunction(
        agent,
        .{ .function = setter_closure },
        1,
        "",
        .{ .additional_fields = .make(*Captures, captures) },
    );

    // 4. Return setter.
    return setter;
}

pub const Arguments = MakeObject(.{
    .Fields = struct {
        /// [[ParameterMap]]
        parameter_map: *Object,
    },
    .tag = .arguments,
});
