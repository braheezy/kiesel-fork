//! 20.3 Boolean Objects
//! https://tc39.es/ecma262/#sec-boolean-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 20.3.2 Properties of the Boolean Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-boolean-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Boolean",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.3.2.1 Boolean.prototype
        // https://tc39.es/ecma262/#sec-boolean.prototype
        try object.defineBuiltinProperty(agent, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Boolean.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 20.3.1.1 Boolean ( value )
    /// https://tc39.es/ecma262/#sec-boolean-constructor-boolean-value
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Let b be ToBoolean(value).
        const b = value.toBoolean();

        // 2. If NewTarget is undefined, return b.
        if (new_target == null) return Value.from(b);

        // 3. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Boolean.prototype%", « [[BooleanData]] »).
        const object = try ordinaryCreateFromConstructor(
            Boolean,
            agent,
            new_target.?,
            "%Boolean.prototype%",
            .{
                // 4. Set O.[[BooleanData]] to b.
                .boolean_data = b,
            },
        );

        // 5. Return O.
        return Value.from(object);
    }
};

/// 20.3.3 Properties of the Boolean Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-boolean-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return Boolean.create(agent, .{
            .fields = .{
                .boolean_data = false,
            },
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

        // 20.3.3.1 Boolean.prototype.constructor
        // https://tc39.es/ecma262/#sec-boolean.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Boolean%"()),
        );
    }

    /// 20.3.3.3.1 ThisBooleanValue ( value )
    /// https://tc39.es/ecma262/#sec-thisbooleanvalue
    fn thisBooleanValue(agent: *Agent, value: Value) error{ExceptionThrown}!bool {
        // 1. If value is a Boolean, return value.
        if (value.isBoolean()) return value.asBoolean();

        // 2. If value is an Object and value has a [[BooleanData]] internal slot, then
        if (value.isObject() and value.asObject().is(Boolean)) {
            // a. Let b be value.[[BooleanData]].
            // b. Assert: b is a Boolean.
            const b = value.asObject().as(Boolean).fields.boolean_data;

            // c. Return b.
            return b;
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a boolean or Boolean object",
            .{},
        );
    }

    /// 20.3.3.2 Boolean.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-boolean.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let b be ? ThisBooleanValue(this value).
        const b = try thisBooleanValue(agent, this_value);

        // 2. If b is true, return "true"; else return "false".
        return if (b) Value.from("true") else Value.from("false");
    }

    /// 20.3.3.3 Boolean.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-boolean.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisBooleanValue(this value).
        return Value.from(try thisBooleanValue(agent, this_value));
    }
};

/// 20.3.4 Properties of Boolean Instances
/// https://tc39.es/ecma262/#sec-properties-of-boolean-instances
pub const Boolean = MakeObject(.{
    .Fields = struct {
        /// [[BooleanData]]
        boolean_data: bool,
    },
    .tag = .boolean,
});
