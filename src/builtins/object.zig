//! 20.1 Object Objects
//! https://tc39.es/ecma262/#sec-object-objects

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const immutable_prototype = @import("immutable_prototype.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Object_ = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 20.1.1 The Object Constructor
/// https://tc39.es/ecma262/#sec-object-constructor
pub const ObjectConstructor = struct {
    pub fn create(realm: *Realm) !Object_ {
        // 20.1.2 Properties of the Object Constructor
        // https://tc39.es/ecma262/#sec-properties-of-the-object-constructor
        const object = try createBuiltinFunction(realm.agent, behaviour, .{
            .length = 1,
            .name = "Object",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
            .is_constructor = true,
        });

        // 20.1.2.20 Object.prototype
        // https://tc39.es/ecma262/#sec-object.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Boolean.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.1.3.1 Object.prototype.constructor
        // https://tc39.es/ecma262/#sec-object.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Object.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.1.1.1 Object ( [ value ] )
    /// https://tc39.es/ecma262/#sec-object-value
    fn behaviour(agent: *Agent, _: Value, arguments: []const Value, new_target: ?Object_) !Value {
        const realm = agent.currentRealm();
        const value = if (arguments.len > 0) arguments[0] else .undefined;

        // 1. If NewTarget is neither undefined nor the active function object, then
        if (new_target != null and new_target.?.ptr != agent.activeFunctionObject().ptr) {
            // a. Return ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
            return Value.from(try ordinaryCreateFromConstructor(
                Object,
                agent,
                new_target.?,
                "%Object.prototype%",
            ));
        }

        // 2. If value is either undefined or null, return OrdinaryObjectCreate(%Object.prototype%).
        if (value == .undefined or value == .null) return Value.from(try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        ));

        // 3. Return ! ToObject(value).
        // TODO: Use `catch |err| try noexcept(err)` once Value.toObject() is fully implemented
        return Value.from(try value.toObject(agent));
    }
};

/// 20.1.3 Properties of the Object Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object
pub const ObjectPrototype = struct {
    pub fn create(realm: *Realm) !Object_ {
        return try Object.create(realm.agent, .{
            .prototype = null,
            .internal_methods = .{
                .setPrototypeOf = immutable_prototype.setPrototypeOf,
            },
        });
    }
};

/// 20.1.4 Properties of Object Instances
/// https://tc39.es/ecma262/#sec-properties-of-object-instances
pub const Object = Object_.Factory(.{});
