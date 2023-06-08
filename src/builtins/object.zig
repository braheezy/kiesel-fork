//! 20.1 Object Objects
//! https://tc39.es/ecma262/#sec-object-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const immutable_prototype = @import("immutable_prototype.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object_ = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
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
        try defineBuiltinFunction(object, "isExtensible", isExtensible, 1, realm);
        try defineBuiltinFunction(object, "isFrozen", isFrozen, 1, realm);
        try defineBuiltinFunction(object, "isSealed", isSealed, 1, realm);
        try defineBuiltinFunction(object, "preventExtensions", preventExtensions, 1, realm);
        try defineBuiltinFunction(object, "seal", seal, 1, realm);

        // 20.1.2.20 Object.prototype
        // https://tc39.es/ecma262/#sec-object.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Object.prototype%"()),
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
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object_) !Value {
        const realm = agent.currentRealm();
        const value = arguments.get(0);

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
    fn freeze(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

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
    fn is(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const value1 = arguments.get(0);
        const value2 = arguments.get(1);

        // 1. Return SameValue(value1, value2).
        return Value.from(sameValue(value1, value2));
    }

    /// 20.1.2.15 Object.isExtensible ( O )
    /// https://tc39.es/ecma262/#sec-object.isextensible
    fn isExtensible(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? IsExtensible(O).
        return Value.from(try object.object.isExtensible());
    }

    /// 20.1.2.16 Object.isFrozen ( O )
    /// https://tc39.es/ecma262/#sec-object.isfrozen
    fn isFrozen(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, frozen).
        return Value.from(try object.object.testIntegrityLevel(.frozen));
    }

    /// 20.1.2.17 Object.isSealed ( O )
    /// https://tc39.es/ecma262/#sec-object.issealed
    fn isSealed(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, sealed).
        return Value.from(try object.object.testIntegrityLevel(.sealed));
    }

    /// 20.1.2.21 Object.seal ( O )
    /// https://tc39.es/ecma262/#sec-object.seal
    fn seal(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (object != .object) return Value.from(true);

        // 2. Let status be ? SetIntegrityLevel(O, sealed).
        const status = try object.object.setIntegrityLevel(.sealed);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not seal object");

        // 4. Return O.
        return object;
    }

    /// 20.1.2.19 Object.preventExtensions ( O )
    /// https://tc39.es/ecma262/#sec-object.preventextensions
    fn preventExtensions(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (object != .object) return object;

        // 2. Let status be ? O.[[PreventExtensions]]().
        const status = try object.object.internalMethods().preventExtensions(object.object);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not prevent extensions");

        // 4. Return O.
        return object;
    }
};

/// 20.1.3 Properties of the Object Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object
pub const ObjectPrototype = struct {
    pub fn create(realm: *Realm) !Object_ {
        const object = try createNoinit(realm);
        init(realm, object);
        return object;
    }

    pub fn createNoinit(realm: *Realm) !Object_ {
        return Object.create(realm.agent, .{
            .prototype = null,
            .internal_methods = .{
                .setPrototypeOf = immutable_prototype.setPrototypeOf,
            },
        });
    }

    pub fn init(realm: *Realm, object: Object_) !void {
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);
    }

    /// 20.1.3.6 Object.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-object.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. If the this value is undefined, return "[object Undefined]".
        if (this_value == .undefined) return Value.from("[object Undefined]");

        // 2. If the this value is null, return "[object Null]".
        if (this_value == .null) return Value.from("[object Null]");

        // 3. Let O be ! ToObject(this value).
        const object = this_value.toObject(agent) catch |err| try noexcept(err);

        // 4. Let isArray be ? IsArray(O).
        const is_array = try this_value.isArray();

        // zig fmt: off
        // 5. If isArray is true, let builtinTag be "Array".
        const builtin_tag = if (is_array)
            "Array"
        // TODO: 6. Else if O has a [[ParameterMap]] internal slot, let builtinTag be "Arguments".
        // 7. Else if O has a [[Call]] internal method, let builtinTag be "Function".
        else if (object.internalMethods().call) |_|
            "Function"
        // TODO: 8. Else if O has an [[ErrorData]] internal slot, let builtinTag be "Error".
        // 9. Else if O has a [[BooleanData]] internal slot, let builtinTag be "Boolean".
        else if (object.is(builtins.Boolean))
            "Boolean"
        // TODO: 10. Else if O has a [[NumberData]] internal slot, let builtinTag be "Number".
        // TODO: 11. Else if O has a [[StringData]] internal slot, let builtinTag be "String".
        // TODO: 12. Else if O has a [[DateValue]] internal slot, let builtinTag be "Date".
        // TODO: 13. Else if O has a [[RegExpMatcher]] internal slot, let builtinTag be "RegExp".
        // 14. Else, let builtinTag be "Object".
        else
            "Object";
        // zig fmt: on

        // 15. Let tag be ? Get(O, @@toStringTag).
        const tag_value = try object.get(PropertyKey.from(agent.well_known_symbols.@"@@toStringTag"));

        // 16. If tag is not a String, set tag to builtinTag.
        const tag = switch (tag_value) {
            .string => |string| string,
            else => builtin_tag,
        };

        // 17. Return the string-concatenation of "[object ", tag, and "]".
        return Value.from(try std.fmt.allocPrint(agent.gc_allocator, "[object {s}]", .{tag}));
    }

    /// 20.1.3.7 Object.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-object.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? ToObject(this value).
        return Value.from(try this_value.toObject(agent));
    }
};

/// 20.1.4 Properties of Object Instances
/// https://tc39.es/ecma262/#sec-properties-of-object-instances
pub const Object = Object_.Factory(.{});
