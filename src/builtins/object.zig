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
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

/// 20.1.1 The Object Constructor
/// https://tc39.es/ecma262/#sec-object-constructor
pub const ObjectConstructor = struct {
    pub fn create(realm: *Realm) !Object_ {
        // 20.1.2 Properties of the Object Constructor
        // https://tc39.es/ecma262/#sec-properties-of-the-object-constructor
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Object",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "freeze", freeze, 1, realm);
        try defineBuiltinFunction(object, "is", is, 2, realm);
        try defineBuiltinFunction(object, "isFrozen", isFrozen, 1, realm);
        try defineBuiltinFunction(object, "isSealed", isSealed, 1, realm);
        try defineBuiltinFunction(object, "seal", seal, 1, realm);

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

    /// 20.1.2.6 Object.freeze ( O )
    /// https://tc39.es/ecma262/#sec-object.freeze
    fn freeze(agent: *Agent, _: Value, arguments: []const Value) !Value {
        const object = if (arguments.len > 0) arguments[0] else .undefined;

        // 1. If O is not an Object, return O.
        if (object != .object) return object;

        // 2. Let status be ? SetIntegrityLevel(O, frozen).
        const status = try object.object.setIntegrityLevel(.frozen);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not freeze object");

        // 4. Return O.
        return object;
    }

    /// 20.1.2.14 Object.is ( value1, value2 )
    /// https://tc39.es/ecma262/#sec-object.is
    fn is(_: *Agent, _: Value, arguments: []const Value) !Value {
        const value1 = if (arguments.len > 0) arguments[0] else .undefined;
        const value2 = if (arguments.len > 1) arguments[1] else .undefined;

        // 1. Return SameValue(value1, value2).
        return Value.from(sameValue(value1, value2));
    }

    /// 20.1.2.16 Object.isFrozen ( O )
    /// https://tc39.es/ecma262/#sec-object.isfrozen
    fn isFrozen(_: *Agent, _: Value, arguments: []const Value) !Value {
        const object = if (arguments.len > 0) arguments[0] else .undefined;

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, frozen).
        return Value.from(try object.object.testIntegrityLevel(.frozen));
    }

    /// 20.1.2.17 Object.isSealed ( O )
    /// https://tc39.es/ecma262/#sec-object.issealed
    fn isSealed(_: *Agent, _: Value, arguments: []const Value) !Value {
        const object = if (arguments.len > 0) arguments[0] else .undefined;

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, sealed).
        return Value.from(try object.object.testIntegrityLevel(.sealed));
    }

    /// 20.1.2.21 Object.seal ( O )
    /// https://tc39.es/ecma262/#sec-object.seal
    fn seal(agent: *Agent, _: Value, arguments: []const Value) !Value {
        const object = if (arguments.len > 0) arguments[0] else .undefined;

        // 1. If O is not an Object, return O.
        if (object != .object) return Value.from(true);

        // 2. Let status be ? SetIntegrityLevel(O, sealed).
        const status = try object.object.setIntegrityLevel(.sealed);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not seal object");

        // 4. Return O.
        return object;
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
