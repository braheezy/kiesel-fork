//! 21.2 BigInt Objects
//! https://tc39.es/ecma262/#sec-bigint-objects

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Number = types.Number;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 21.2.2 Properties of the BigInt Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-bigint-constructor
pub const BigIntConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "BigInt",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 21.2.2.3 BigInt.prototype
        // https://tc39.es/ecma262/#sec-bigint.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%BigInt.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.2.3.1 BigInt.prototype.constructor
        // https://tc39.es/ecma262/#sec-bigint.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%BigInt.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 21.2.1.1 BigInt ( value )
    /// https://tc39.es/ecma262/#sec-bigint-constructor-number-value
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const value = arguments.get(0);

        // 1. If NewTarget is not undefined, throw a TypeError exception.
        if (new_target != null) {
            return agent.throwException(.type_error, "BigInt is not a constructor");
        }

        // 2. Let prim be ? ToPrimitive(value, number).
        const primitive = try value.toPrimitive(agent, .number);

        // 3. If prim is a Number, return ? NumberToBigInt(prim).
        if (primitive == .number) return Value.from(try numberToBigInt(agent, primitive.number));

        // 4. Otherwise, return ? ToBigInt(prim).
        return Value.from(try primitive.toBigInt(agent));
    }
};

/// 21.2.1.1.1 NumberToBigInt ( number )
/// https://tc39.es/ecma262/#sec-numbertobigint
fn numberToBigInt(agent: *Agent, number: Number) !types.BigInt {
    // 1. If IsIntegralNumber(number) is false, throw a RangeError exception.
    if (!Value.from(number).isIntegralNumber()) {
        return agent.throwException(.range_error, "Cannot convert non-integral number to BigInt");
    }

    // 2. Return ℤ(ℝ(number)).
    const string = try number.toString(agent.gc_allocator, 10);
    var value = try types.BigInt.Value.init(agent.gc_allocator);
    value.setString(10, string.utf8) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidBase, error.InvalidCharacter => unreachable,
    };
    return .{ .value = value };
}

/// 21.2.3 Properties of the BigInt Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-bigint-prototype-object
pub const BigIntPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);

        // 21.2.3.5 BigInt.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-bigint.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("BigInt"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 21.2.3.4.1 ThisBigIntValue ( value )
    /// https://tc39.es/ecma262/#sec-thisbigintvalue
    fn thisBigIntValue(agent: *Agent, value: Value) !types.BigInt {
        switch (value) {
            // 1. If value is a BigInt, return value.
            .big_int => |big_int| return big_int,

            // 2. If value is an Object and value has a [[BigIntData]] internal slot, then
            .object => |object| if (object.is(BigInt)) {
                // a. Assert: value.[[BigIntData]] is a BigInt.
                // b. Return value.[[BigIntData]].
                return object.as(BigInt).fields.big_int_data;
            },

            else => {},
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a BigInt or BigInt object",
        );
    }

    /// 21.2.3.3 BigInt.prototype.toString ( [ radix ] )
    /// https://tc39.es/ecma262/#sec-bigint.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const radix = arguments.get(0);

        // 1. Let x be ? ThisBigIntValue(this value).
        const x = try thisBigIntValue(agent, this_value);

        // 2. If radix is undefined, let radixMV be 10.
        // 3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
        const radix_mv = if (radix == .undefined) 10 else try radix.toIntegerOrInfinity(agent);

        // 4. If radixMV is not in the inclusive interval from 2 to 36, throw a RangeError exception.
        if (radix_mv < 2 or radix_mv > 36) {
            return agent.throwException(.range_error, "Radix must be in range 2-36");
        }

        // 5. Return BigInt::toString(x, radixMV).
        return Value.from(try x.toString(agent.gc_allocator, @intFromFloat(radix_mv)));
    }

    /// 21.2.3.4 BigInt.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-bigint.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? ThisBigIntValue(this value).
        return Value.from(try thisBigIntValue(agent, this_value));
    }
};

/// 21.2.4 Properties of BigInt Instances
/// https://tc39.es/ecma262/#sec-properties-of-bigint-instances
pub const BigInt = Object.Factory(.{
    .Fields = struct {
        /// [[BigIntData]]
        big_int_data: types.BigInt,
    },
    .tag = .big_int,
});
