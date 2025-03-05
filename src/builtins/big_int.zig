//! 21.2 BigInt Objects
//! https://tc39.es/ecma262/#sec-bigint-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Number = types.Number;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 21.2.2 Properties of the BigInt Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-bigint-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            realm.agent,
            .{ .constructor = impl },
            1,
            "BigInt",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "asIntN", asIntN, 2, realm);
        try defineBuiltinFunction(object, "asUintN", asUintN, 2, realm);

        // 21.2.2.3 BigInt.prototype
        // https://tc39.es/ecma262/#sec-bigint.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%BigInt.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 21.2.1.1 BigInt ( value )
    /// https://tc39.es/ecma262/#sec-bigint-constructor-number-value
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. If NewTarget is not undefined, throw a TypeError exception.
        if (new_target != null) {
            return agent.throwException(.type_error, "BigInt is not a constructor", .{});
        }

        // 2. Let prim be ? ToPrimitive(value, number).
        const primitive = try value.toPrimitive(agent, .number);

        // 3. If prim is a Number, return ? NumberToBigInt(prim).
        if (primitive.isNumber()) return Value.from(try numberToBigInt(agent, primitive.asNumber()));

        // 4. Otherwise, return ? ToBigInt(prim).
        return Value.from(try primitive.toBigInt(agent));
    }

    /// 21.2.2.1 BigInt.asIntN ( bits, bigint )
    /// https://tc39.es/ecma262/#sec-bigint.asintn
    fn asIntN(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const bits_value = arguments.get(0);
        const big_int_value = arguments.get(1);

        // 1. Set bits to ? ToIndex(bits).
        const bits = try bits_value.toIndex(agent);

        // 2. Set bigint to ? ToBigInt(bigint).
        const big_int = try big_int_value.toBigInt(agent);

        // 3. Let mod be ℝ(bigint) modulo 2**bits.
        // 4. If mod ≥ 2**(bits - 1), return ℤ(mod - 2**bits); otherwise, return ℤ(mod).
        const result = try types.BigInt.from(agent.gc_allocator, 0);
        try result.managed.truncate(&big_int.managed, .signed, @intCast(bits));
        return Value.from(result);
    }

    /// 21.2.2.2 BigInt.asUintN ( bits, bigint )
    /// https://tc39.es/ecma262/#sec-bigint.asuintn
    fn asUintN(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const bits_value = arguments.get(0);
        const big_int_value = arguments.get(1);

        // 1. Set bits to ? ToIndex(bits).
        const bits = try bits_value.toIndex(agent);

        // 2. Set bigint to ? ToBigInt(bigint).
        const big_int = try big_int_value.toBigInt(agent);

        // 3. Return ℤ(ℝ(bigint) modulo 2**bits).
        const result = try types.BigInt.from(agent.gc_allocator, 0);
        try result.managed.truncate(&big_int.managed, .unsigned, @intCast(bits));
        return Value.from(result);
    }
};

/// 21.2.1.1.1 NumberToBigInt ( number )
/// https://tc39.es/ecma262/#sec-numbertobigint
fn numberToBigInt(agent: *Agent, number: Number) Agent.Error!*const types.BigInt {
    // 1. If number is not an integral Number, throw a RangeError exception.
    if (!number.isIntegral()) {
        return agent.throwException(
            .range_error,
            "Cannot convert non-integral number to BigInt",
            .{},
        );
    }

    // 2. Return ℤ(ℝ(number)).
    const string = try number.toString(agent.gc_allocator, 10);
    const big_int = try types.BigInt.from(agent.gc_allocator, 0);
    big_int.managed.setString(10, string.slice.ascii) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidBase, error.InvalidCharacter => unreachable,
    };
    return big_int;
}

/// 21.2.3 Properties of the BigInt Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-bigint-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "toLocaleString", toLocaleString, 0, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);

        // 21.2.3.1 BigInt.prototype.constructor
        // https://tc39.es/ecma262/#sec-bigint.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%BigInt%"()),
        );

        // 21.2.3.5 BigInt.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-bigint.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("BigInt"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 21.2.3.4.1 ThisBigIntValue ( value )
    /// https://tc39.es/ecma262/#sec-thisbigintvalue
    fn thisBigIntValue(agent: *Agent, value: Value) error{ExceptionThrown}!*const types.BigInt {
        // 1. If value is a BigInt, return value.
        if (value.isBigInt()) return value.asBigInt();

        // 2. If value is an Object and value has a [[BigIntData]] internal slot, then
        if (value.isObject() and value.asObject().is(BigInt)) {
            // a. Assert: value.[[BigIntData]] is a BigInt.
            // b. Return value.[[BigIntData]].
            return value.asObject().as(BigInt).fields.big_int_data;
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a BigInt or BigInt object",
            .{},
        );
    }

    /// 21.2.3.2 BigInt.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-bigint.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const x = try thisBigIntValue(agent, this_value);
        return Value.from(try x.toString(agent.gc_allocator, 10));
    }

    /// 21.2.3.3 BigInt.prototype.toString ( [ radix ] )
    /// https://tc39.es/ecma262/#sec-bigint.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const radix = arguments.get(0);

        // 1. Let x be ? ThisBigIntValue(this value).
        const x = try thisBigIntValue(agent, this_value);

        // 2. If radix is undefined, let radixMV be 10.
        // 3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
        const radix_mv = if (radix.isUndefined()) 10 else try radix.toIntegerOrInfinity(agent);

        // 4. If radixMV is not in the inclusive interval from 2 to 36, throw a RangeError exception.
        if (radix_mv < 2 or radix_mv > 36) {
            return agent.throwException(.range_error, "Radix must be in range 2-36", .{});
        }

        // 5. Return BigInt::toString(x, radixMV).
        return Value.from(try x.toString(agent.gc_allocator, @intFromFloat(radix_mv)));
    }

    /// 21.2.3.4 BigInt.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-bigint.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisBigIntValue(this value).
        return Value.from(try thisBigIntValue(agent, this_value));
    }
};

/// 21.2.4 Properties of BigInt Instances
/// https://tc39.es/ecma262/#sec-properties-of-bigint-instances
pub const BigInt = MakeObject(.{
    .Fields = struct {
        /// [[BigIntData]]
        big_int_data: *const types.BigInt,
    },
    .tag = .big_int,
});
