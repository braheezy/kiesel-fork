//! 21.1 Number Objects
//! https://tc39.es/ecma262/#sec-number-objects

const std = @import("std");

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
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 21.1.2 Properties of the Number Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-number-constructor
pub const NumberConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Number",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 21.1.2.1 Number.EPSILON
        // https://tc39.es/ecma262/#sec-number.epsilon
        try defineBuiltinProperty(object, "EPSILON", PropertyDescriptor{
            .value = Value.from(std.math.floatEps(f64)),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.2.6 Number.MAX_SAFE_INTEGER
        // https://tc39.es/ecma262/#sec-number.max_safe_integer
        try defineBuiltinProperty(object, "MAX_SAFE_INTEGER", PropertyDescriptor{
            .value = Value.from(std.math.maxInt(u53)),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        try defineBuiltinFunction(object, "isFinite", isFinite, 1, realm);
        try defineBuiltinFunction(object, "isInteger", isInteger, 1, realm);
        try defineBuiltinFunction(object, "isNaN", isNaN, 1, realm);

        // 21.1.2.15 Number.prototype
        // https://tc39.es/ecma262/#sec-number.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Number.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.3.1 Number.prototype.constructor
        // https://tc39.es/ecma262/#sec-number.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Number.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 21.1.1.1 Number ( value )
    /// https://tc39.es/ecma262/#sec-number-constructor-number-value
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const value = arguments.get(0);

        const n = blk: {
            // 1. If value is present, then
            if (arguments.count() != 0) {
                // a. Let prim be ? ToNumeric(value).
                const primitive = try value.toNumeric(agent);

                // b. If prim is a BigInt, let n be 𝔽(ℝ(prim)).
                if (primitive == .big_int) break :blk types.Number.from(
                    try primitive.big_int.asFloat(agent),
                );

                // c. Otherwise, let n be prim.
                break :blk primitive.number;
            }
            // 2. Else,
            else {
                // a. Let n be +0𝔽.
                break :blk types.Number.from(0);
            }
        };

        // 3. If NewTarget is undefined, return n.
        if (new_target == null) return Value.from(n);

        // 4. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Number.prototype%", « [[NumberData]] »).
        const object = try ordinaryCreateFromConstructor(
            Number,
            agent,
            new_target.?,
            "%Number.prototype%",
        );

        // 5. Set O.[[NumberData]] to n.
        object.as(Number).fields = .{ .number_data = n };

        // 6. Return O.
        return Value.from(object);
    }

    /// 21.1.2.2 Number.isFinite ( number )
    /// https://tc39.es/ecma262/#sec-number.isfinite
    fn isFinite(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const number = arguments.get(0);

        // 1. If number is not a Number, return false.
        if (number != .number) return Value.from(false);

        // 2. If number is not finite, return false.
        if (!number.number.isFinite()) return Value.from(false);

        // 3. Otherwise, return true.
        return Value.from(true);
    }

    /// 21.1.2.3 Number.isInteger ( number )
    /// https://tc39.es/ecma262/#sec-number.isinteger
    fn isInteger(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const number = arguments.get(0);

        // 1. Return IsIntegralNumber(number).
        return Value.from(number.isIntegralNumber());
    }

    /// 21.1.2.4 Number.isNaN ( number )
    /// https://tc39.es/ecma262/#sec-number.isnan
    fn isNaN(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const number = arguments.get(0);

        // 1. If number is not a Number, return false.
        if (number != .number) return Value.from(false);

        // 2. If number is NaN, return true.
        if (number.number.isNan()) return Value.from(true);

        // 3. Otherwise, return false.
        return Value.from(false);
    }
};

/// 21.1.3 Properties of the Number Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-number-prototype-object
pub const NumberPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try Number.create(realm.agent, .{
            .fields = .{
                .number_data = types.Number.from(0),
            },
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);

        return object;
    }

    /// https://tc39.es/ecma262/#thisnumbervalue
    fn thisNumberValue(agent: *Agent, value: Value) !types.Number {
        switch (value) {
            // 1. If value is a Number, return value.
            .number => |number| return number,

            // 2. If value is an Object and value has a [[NumberData]] internal slot, then
            .object => |object| if (object.is(Number)) {
                // a. Let n be value.[[NumberData]].
                // b. Assert: n is a Number.
                const n = object.as(Number).fields.number_data;

                // c. Return n.
                return n;
            },

            else => {},
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a number or Number object",
        );
    }

    /// 21.1.3.6 Number.prototype.toString ( [ radix ] )
    /// https://tc39.es/ecma262/#sec-number.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const radix = arguments.get(0);

        // 1. Let x be ? thisNumberValue(this value).
        const x = try thisNumberValue(agent, this_value);

        // 2. If radix is undefined, let radixMV be 10.
        // 3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
        const radix_mv = if (radix == .undefined) 10 else try radix.toIntegerOrInfinity(agent);

        // 4. If radixMV is not in the inclusive interval from 2 to 36, throw a RangeError exception.
        if (radix_mv < 2 or radix_mv > 36) {
            return agent.throwException(.range_error, "Radix must be in range 2-36");
        }

        // 5. Return Number::toString(x, radixMV).
        return Value.from(try x.toString(agent.gc_allocator, @intFromFloat(radix_mv)));
    }

    /// 21.1.3.7 Number.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-number.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? thisNumberValue(this value).
        return Value.from(try thisNumberValue(agent, this_value));
    }
};

/// 21.1.4 Properties of Number Instances
/// https://tc39.es/ecma262/#sec-properties-of-number-instances
pub const Number = Object.Factory(.{
    .Fields = struct {
        /// [[NumberData]]
        number_data: types.Number,
    },
    .tag = .number,
});
