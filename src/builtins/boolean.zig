//! 20.3 Boolean Objects
//! https://tc39.es/ecma262/#sec-boolean-objects

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 20.3.2 Properties of the Boolean Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-boolean-constructor
pub const BooleanConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Boolean",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 20.3.2.1 Boolean.prototype
        // https://tc39.es/ecma262/#sec-boolean.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Boolean.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.3.3.1 Boolean.prototype.constructor
        // https://tc39.es/ecma262/#sec-boolean.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Boolean.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.3.1.1 Boolean ( value )
    /// https://tc39.es/ecma262/#sec-boolean-constructor-boolean-value
    fn behaviour(agent: *Agent, _: Value, arguments: []const Value, new_target: ?Object) !Value {
        const value = if (arguments.len > 0) arguments[0] else .undefined;

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
        );

        // 4. Set O.[[BooleanData]] to b.
        object.as(Boolean).fields = .{ .boolean_data = b };

        // 5. Return O.
        return Value.from(object);
    }
};

/// 20.3.3 Properties of the Boolean Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-boolean-prototype-object
pub const BooleanPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try Boolean.create(realm.agent, .{
            .fields = .{
                .boolean_data = false,
            },
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);

        return object;
    }

    /// https://tc39.es/ecma262/#thisbooleanvalue
    fn thisBooleanValue(agent: *Agent, value: Value) !bool {
        switch (value) {
            // 1. If value is a Boolean, return value.
            .boolean => |boolean| return boolean,

            // 2. If value is an Object and value has a [[BooleanData]] internal slot, then
            .object => |object| if (object.is(Boolean)) {
                // a. Let b be value.[[BooleanData]].
                // b. Assert: b is a Boolean.
                const b = object.as(Boolean).fields.boolean_data;

                // c. Return b.
                return b;
            },

            else => {},
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a boolean or Boolean object",
        );
    }

    /// 20.3.3.2 Boolean.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-boolean.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: []const Value) !Value {
        // 1. Let b be ? thisBooleanValue(this value).
        const b = try thisBooleanValue(agent, this_value);

        // 2. If b is true, return "true"; else return "false".
        return Value.from(if (b) "true" else "false");
    }

    /// 20.3.3.3 Boolean.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-boolean.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: []const Value) !Value {
        // 1. Return ? thisBooleanValue(this value).
        return Value.from(try thisBooleanValue(agent, this_value));
    }
};

/// 20.3.4 Properties of Boolean Instances
/// https://tc39.es/ecma262/#sec-properties-of-boolean-instances
pub const Boolean = Object.Factory(.{
    .Fields = struct {
        /// [[BooleanData]]
        boolean_data: bool,
    },
    .tag = .boolean,
});
