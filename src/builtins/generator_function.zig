//! 27.3 GeneratorFunction Objects
//! https://tc39.es/ecma262/#sec-generatorfunction-objects

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

/// 27.3.1 The GeneratorFunction Constructor
/// https://tc39.es/ecma262/#sec-generatorfunction-constructor
pub const GeneratorFunctionConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "GeneratorFunction",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function%"(),
        });

        // 27.3.2.1 GeneratorFunction.prototype
        // https://tc39.es/ecma262/#sec-generatorfunction.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%GeneratorFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        return object;
    }

    /// 27.3.1.1 GeneratorFunction ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-generatorfunction
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const parameter_args = ArgumentsList.from(arguments.values[0..arguments.count() -| 1]);
        const maybe_body_arg = arguments.getOrNull(arguments.count() -| 1);

        // 1. Let C be the active function object.
        const constructor = agent.activeFunctionObject();

        // 2. If bodyArg is not present, set bodyArg to the empty String.
        const body_arg = maybe_body_arg orelse Value.from("");

        // 3. Return ? CreateDynamicFunction(C, NewTarget, generator, parameterArgs, bodyArg).
        return Value.from(try createDynamicFunction(
            agent,
            constructor,
            new_target,
            .generator,
            parameter_args,
            body_arg,
        ));
    }
};

/// 27.3.3 Properties of the GeneratorFunction Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-generatorfunction-prototype-object
pub const GeneratorFunctionPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createNoinit(realm);
        init(realm, object);
        return object;
    }

    pub fn createNoinit(realm: *Realm) !Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) !void {
        // 27.3.3.1 GeneratorFunction.prototype.constructor
        // https://tc39.es/ecma262/#sec-generatorfunction.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%GeneratorFunction%"()),
        );

        // 27.3.3.2 GeneratorFunction.prototype.prototype
        // https://tc39.es/ecma262/#sec-generatorfunction.prototype.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%GeneratorFunction.prototype.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 27.3.3.3 GeneratorFunction.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-generatorfunction.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("GeneratorFunction"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }
};
