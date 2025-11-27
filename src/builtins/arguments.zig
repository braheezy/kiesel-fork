//! 10.4.4 Arguments Exotic Objects
//! https://tc39.es/ecma262/#sec-arguments-exotic-objects

const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryDelete = builtins.ordinaryDelete;
const ordinaryGet = builtins.ordinaryGet;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const ordinarySet = builtins.ordinarySet;
const sameValue = types.sameValue;

/// 10.4.4.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-getownproperty-p
fn getOwnProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    // 1. Let desc be OrdinaryGetOwnProperty(args, P).
    var descriptor = (ordinaryGetOwnProperty(object, property_key) catch unreachable) orelse {
        // 2. If desc is undefined, return undefined.
        return null;
    };

    // 3. Let map be args.[[ParameterMap]].
    const map = &object.as(Arguments).fields.parameter_map;

    // 4. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.has(property_key);

    // 5. If isMapped is true, then
    if (is_mapped) {
        // a. Set desc.[[Value]] to ! Get(map, P).
        descriptor.value = map.get(agent, property_key);
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
    const map = &object.as(Arguments).fields.parameter_map;

    // 2. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.has(property_key);

    // 3. Let newArgDesc be Desc.
    var new_arg_property_descriptor = property_descriptor;

    // 4. If isMapped is true and IsDataDescriptor(Desc) is true, then
    if (is_mapped and property_descriptor.isDataDescriptor()) {
        // a. If Desc does not have a [[Value]] field, Desc has a [[Writable]] field, and
        //    Desc.[[Writable]] is false, then
        if (property_descriptor.value == null and property_descriptor.writable == false) {
            // i. Set newArgDesc to a copy of Desc.

            // ii. Set newArgDesc.[[Value]] to ! Get(map, P).
            new_arg_property_descriptor.value = map.get(agent, property_key);
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
            map.delete(property_key);
        } else {
            // b. Else,
            // i. If Desc has a [[Value]] field, then
            if (property_descriptor.value) |value| {
                // 1. Assert: The following Set will succeed, since formal parameters mapped by
                //    arguments objects are always writable.
                // 2. Perform ! Set(map, P, Desc.[[Value]], false).
                map.set(agent, property_key, value);
            }

            // ii. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, then
            if (property_descriptor.writable == false) {
                // 1. Perform ! map.[[Delete]](P).
                map.delete(property_key);
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
    const map = &object.as(Arguments).fields.parameter_map;

    // 2. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.has(property_key);

    // 3. If isMapped is false, then
    if (!is_mapped) {
        // a. Return ? OrdinaryGet(args, P, Receiver).
        return ordinaryGet(agent, object, property_key, receiver);
    } else {
        // 4. Else,
        // a. Assert: map contains a formal parameter mapping for P.
        // b. Return ! Get(map, P).
        return map.get(agent, property_key);
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
        const map = &object.as(Arguments).fields.parameter_map;

        // b. Let isMapped be ! HasOwnProperty(map, P).
        const is_mapped = map.has(property_key);

        // 3. If isMapped is true, then
        if (is_mapped) {
            // a. Assert: The following Set will succeed, since formal parameters mapped by
            //    arguments objects are always writable.
            // b. Perform ! Set(map, P, V, false).
            map.set(agent, property_key, value);
        }
    }

    // 4. Return ? OrdinarySet(args, P, V, Receiver).
    return ordinarySet(agent, object, property_key, value, receiver);
}

/// 10.4.4.5 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-arguments-exotic-objects-delete-p
fn delete(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Let map be args.[[ParameterMap]].
    const map = &object.as(Arguments).fields.parameter_map;

    // 2. Let isMapped be ! HasOwnProperty(map, P).
    const is_mapped = map.has(property_key);

    // 3. Let result be ? OrdinaryDelete(args, P).
    const result = try ordinaryDelete(agent, object, property_key);

    // 4. If result is true and isMapped is true, then
    if (result and is_mapped) {
        // a. Perform ! map.[[Delete]](P).
        map.delete(property_key);
    }

    // 5. Return result.
    return result;
}

/// 10.4.4.6 CreateUnmappedArgumentsObject ( argumentsList )
/// https://tc39.es/ecma262/#sec-createunmappedargumentsobject
pub fn createUnmappedArgumentsObject(
    agent: *Agent,
    arguments_list: []const Value,
) std.mem.Allocator.Error!*Arguments {
    const realm = agent.currentRealm();

    // 1. Let len be the number of elements in argumentsList.
    const len = arguments_list.len;

    // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%, « [[ParameterMap]] »).
    const shape, const indices = try realm.shapes.unmappedArgumentsObject();
    const arguments = try Arguments.createWithShape(agent, .{
        .shape = shape,
        .fields = .{
            // 3. Set obj.[[ParameterMap]] to undefined.
            .parameter_map = undefined,
        },
    });

    // 4. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(len), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    arguments.object.setValueAtPropertyIndex(indices.length, Value.from(@as(u53, @intCast(len))));

    // 5. Let index be 0.
    // 6. Repeat, while index < len,
    for (arguments_list, 0..) |value, index| {
        // a. Let val be argumentsList[index].
        // b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
        try arguments.object.createDataPropertyDirect(
            agent,
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index))),
            value,
        );

        // c. Set index to index + 1.
    }

    // 7. Perform ! DefinePropertyOrThrow(obj, %Symbol.iterator%, PropertyDescriptor {
    //      [[Value]]: %Array.prototype.values%, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    arguments.object.setValueAtPropertyIndex(
        indices.@"%Symbol.iterator%",
        Value.from(try realm.intrinsics.@"%Array.prototype.values%"()),
    );

    // 8. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor {
    //      [[Get]]: %ThrowTypeError%, [[Set]]: %ThrowTypeError%, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    arguments.object.setAccessorAtPropertyIndex(indices.callee, .{
        .get = try realm.intrinsics.@"%ThrowTypeError%"(),
        .set = try realm.intrinsics.@"%ThrowTypeError%"(),
    });

    // 9. Return obj.
    return arguments;
}

/// 10.4.4.7 CreateMappedArgumentsObject ( func, formals, argumentsList, env )
/// https://tc39.es/ecma262/#sec-createmappedargumentsobject
pub fn createMappedArgumentsObject(
    agent: *Agent,
    function: *Object,
    formals: ast.FormalParameters,
    arguments_list: []const Value,
    env: Environment,
) std.mem.Allocator.Error!*Arguments {
    const realm = agent.currentRealm();

    // 1. Assert: formals does not contain a rest parameter, any binding patterns, or any
    //    initializers. It may contain duplicate identifiers.
    if (std.debug.runtime_safety) for (formals.items) |item| {
        std.debug.assert(item == .formal_parameter);
    };

    // 2. Let len be the number of elements in argumentsList.
    const len = arguments_list.len;

    // 3. Let obj be MakeBasicObject(« [[Prototype]], [[Extensible]], [[ParameterMap]] »).
    const shape, const indices = try realm.shapes.mappedArgumentsObject();
    const arguments = try Arguments.createWithShape(agent, .{
        .shape = shape,
        .internal_methods = .initComptime(.{
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
        }),

        // 9. Set obj.[[Prototype]] to %Object.prototype%.
        // NOTE: This is done via the shape.

        .fields = .{
            // 10. Let map be OrdinaryObjectCreate(null).
            // 11. Set obj.[[ParameterMap]] to map.
            .parameter_map = .{
                // Temporarily set to empty so that creating properties below doesn't invoke IB.
                .items = &.{},
                .environment = env,
            },
        },
    });

    // 12. Let parameterNames be the BoundNames of formals.
    var parameter_names: std.ArrayList(ast.Identifier) = .empty;
    defer parameter_names.deinit(agent.gc_allocator);
    try formals.collectBoundNames(agent.gc_allocator, &parameter_names);

    // 13. Let numberOfParameters be the number of elements in parameterNames.
    const number_of_parameters = parameter_names.items.len;

    // 14. Let index be 0.
    // 15. Repeat, while index < len,
    for (arguments_list, 0..) |value, index| {
        // a. Let val be argumentsList[index].
        // b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
        try arguments.object.createDataPropertyDirect(
            agent,
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index))),
            value,
        );

        // c. Set index to index + 1.
    }

    // 16. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor {
    //       [[Value]]: 𝔽(len), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //     }).
    arguments.object.setValueAtPropertyIndex(indices.length, Value.from(@as(u53, @intCast(len))));

    // 17. Let mappedNames be a new empty List.
    var mapped_names: String.HashMapUnmanaged(void) = .empty;
    defer mapped_names.deinit(agent.gc_allocator);

    const map = &arguments.fields.parameter_map;
    map.items = try agent.gc_allocator.alloc(?*const String, @min(number_of_parameters, len));
    @memset(map.items, null);

    // 18. Set index to numberOfParameters - 1.
    var maybe_index: ?u53 = std.math.sub(u53, @intCast(number_of_parameters), 1) catch null;

    // 19. Repeat, while index ≥ 0,
    while (maybe_index != null) : (maybe_index = (std.math.sub(u53, maybe_index.?, 1) catch null)) {
        // a. Let name be parameterNames[index].
        const index: usize = @intCast(maybe_index.?);
        const name = try String.fromUtf8(agent, parameter_names.items[index]);

        const gop = try mapped_names.getOrPut(agent.gc_allocator, name);

        // b. If mappedNames does not contain name, then
        if (!gop.found_existing) {
            // i. Append name to mappedNames.

            // ii. If index < len, then
            if (index < len) {
                // 1. Let g be MakeArgGetter(name, env).
                // 2. Let p be MakeArgSetter(name, env).
                // 3. Perform ! map.[[DefineOwnProperty]](! ToString(𝔽(index)), PropertyDescriptor {
                //      [[Set]]: p, [[Get]]: g, [[Enumerable]]: false, [[Configurable]]: true
                //    }).
                // NOTE: The getter and setter are implemented via the ParameterMap methods.
                map.items[index] = name;
            }
        }

        // c. Set index to index - 1.
    }

    // 20. Perform ! DefinePropertyOrThrow(obj, %Symbol.iterator%, PropertyDescriptor {
    //       [[Value]]: %Array.prototype.values%, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //     }).
    arguments.object.setValueAtPropertyIndex(
        indices.@"%Symbol.iterator%",
        Value.from(try realm.intrinsics.@"%Array.prototype.values%"()),
    );

    // 21. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor {
    //       [[Value]]: func, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //     }).
    arguments.object.setValueAtPropertyIndex(indices.callee, Value.from(function));

    // 22. Return obj.
    return arguments;
}

const ParameterMap = struct {
    items: []?*const String,
    environment: Environment,

    pub fn has(self: *const ParameterMap, property_key: PropertyKey) bool {
        if (property_key != .integer_index) return false;
        const index: usize = @intCast(property_key.integer_index);
        return index < self.items.len and self.items[index] != null;
    }

    pub fn get(self: *const ParameterMap, agent: *Agent, property_key: PropertyKey) Value {
        const index: usize = @intCast(property_key.integer_index);
        const name = self.items[index].?;
        // Bindings are always initialized before this is called, so this can't fail.
        return self.environment.getBindingValue(agent, name, false) catch unreachable;
    }

    pub fn set(self: *ParameterMap, agent: *Agent, property_key: PropertyKey, value: Value) void {
        const index: usize = @intCast(property_key.integer_index);
        const name = self.items[index].?;
        // Bindings are always initialized before this is called, so this can't fail.
        self.environment.setMutableBinding(agent, name, value, false) catch unreachable;
    }

    pub fn delete(self: *ParameterMap, property_key: PropertyKey) void {
        const index: usize = @intCast(property_key.integer_index);
        self.items[index] = null;
    }
};

pub const Arguments = MakeObject(.{
    .Fields = struct {
        /// [[ParameterMap]]
        parameter_map: ParameterMap,
    },
    .tag = .arguments,
});
