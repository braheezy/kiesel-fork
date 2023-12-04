//! 27.7 AsyncFunction Objects
//! https://tc39.es/ecma262/#sec-async-function-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createDynamicFunction = builtins.createDynamicFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 27.7.2 Properties of the AsyncFunction Constructor
/// https://tc39.es/ecma262/#sec-async-function-constructor-properties
pub const AsyncFunctionConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "AsyncFunction",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function%"(),
        });

        // 27.7.2.1 AsyncFunction.prototype
        // https://tc39.es/ecma262/#sec-async-function-constructor-prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AsyncFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 27.7.3.1 AsyncFunction.prototype.constructor
        // https://tc39.es/ecma262/#sec-async-function-prototype-properties-constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%AsyncFunction.prototype%"() catch unreachable,
            "constructor",
            PropertyDescriptor{
                .value = Value.from(object),
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        return object;
    }

    /// 27.7.1.1 AsyncFunction ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-async-function-constructor-arguments
    fn behaviour(agent: *Agent, arguments: ArgumentsList, new_target: ?Object) Agent.Error!Value {
        const parameter_args = ArgumentsList.from(arguments.values[0..arguments.count() -| 1]);
        const maybe_body_arg = arguments.getOrNull(arguments.count() -| 1);

        // 1. Let C be the active function object.
        const constructor = agent.activeFunctionObject();

        // 2. If bodyArg is not present, set bodyArg to the empty String.
        const body_arg = maybe_body_arg orelse Value.from("");

        // 3. Return ? CreateDynamicFunction(C, NewTarget, async, parameterArgs, bodyArg).
        return Value.from(try createDynamicFunction(
            agent,
            constructor,
            new_target,
            .@"async",
            parameter_args,
            body_arg,
        ));
    }
};

/// 27.7.3 Properties of the AsyncFunction Prototype Object
/// https://tc39.es/ecma262/#sec-async-function-prototype-properties
pub const AsyncFunctionPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 27.7.3.2 AsyncFunction.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-async-function-prototype-properties-toStringTag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("AsyncFunction"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};
