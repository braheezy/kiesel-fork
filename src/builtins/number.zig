//! 21.1 Number Objects
//! https://tc39.es/ecma262/#sec-number-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 21.1.2 Properties of the Number Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-number-constructor
pub const NumberConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
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

        // 21.1.2.7 Number.MAX_VALUE
        // https://tc39.es/ecma262/#sec-number.max_value
        try defineBuiltinProperty(object, "MAX_VALUE", PropertyDescriptor{
            .value = Value.from(std.math.floatMax(f64)),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.2.8 Number.MIN_SAFE_INTEGER
        // https://tc39.es/ecma262/#sec-number.min_safe_integer
        try defineBuiltinProperty(object, "MIN_SAFE_INTEGER", PropertyDescriptor{
            .value = Value.from(-@as(f64, @floatFromInt(std.math.maxInt(u53)))),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.2.8 Number.MIN_VALUE
        // https://tc39.es/ecma262/#sec-number.min_value
        try defineBuiltinProperty(object, "MIN_VALUE", PropertyDescriptor{
            .value = Value.from(std.math.floatTrueMin(f64)),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.2.10 Number.NaN
        // https://tc39.es/ecma262/#sec-number.nan
        try defineBuiltinProperty(object, "NaN", PropertyDescriptor{
            .value = Value.nan(),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.2.11 Number.NEGATIVE_INFINITY
        // https://tc39.es/ecma262/#sec-number.negative_infinity
        try defineBuiltinProperty(object, "NEGATIVE_INFINITY", PropertyDescriptor{
            .value = Value.negativeInfinity(),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.1.2.12 Number.parseFloat ( string )
        // https://tc39.es/ecma262/#sec-number.parsefloat
        try defineBuiltinProperty(object, "parseFloat", Value.from(
            try realm.intrinsics.@"%parseFloat%"(),
        ));

        // 21.1.2.13 Number.parseInt ( string, radix )
        // https://tc39.es/ecma262/#sec-number.parseint
        try defineBuiltinProperty(object, "parseInt", Value.from(
            try realm.intrinsics.@"%parseInt%"(),
        ));

        // 21.1.2.14 Number.POSITIVE_INFINITY
        // https://tc39.es/ecma262/#sec-number.positive_infinity
        try defineBuiltinProperty(object, "POSITIVE_INFINITY", PropertyDescriptor{
            .value = Value.infinity(),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        try defineBuiltinFunction(object, "isFinite", isFinite, 1, realm);
        try defineBuiltinFunction(object, "isInteger", isInteger, 1, realm);
        try defineBuiltinFunction(object, "isNaN", isNaN, 1, realm);
        try defineBuiltinFunction(object, "isSafeInteger", isSafeInteger, 1, realm);

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
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
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
            .{
                // 5. Set O.[[NumberData]] to n.
                .number_data = n,
            },
        );

        // 6. Return O.
        return Value.from(object);
    }

    /// 21.1.2.2 Number.isFinite ( number )
    /// https://tc39.es/ecma262/#sec-number.isfinite
    fn isFinite(_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
    fn isInteger(_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const number = arguments.get(0);

        // 1. If number is an integral Number, return true.
        if (number == .number and number.number.isIntegral()) return Value.from(true);

        // 2. Return false.
        return Value.from(false);
    }

    /// 21.1.2.4 Number.isNaN ( number )
    /// https://tc39.es/ecma262/#sec-number.isnan
    fn isNaN(_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const number = arguments.get(0);

        // 1. If number is not a Number, return false.
        if (number != .number) return Value.from(false);

        // 2. If number is NaN, return true.
        if (number.number.isNan()) return Value.from(true);

        // 3. Otherwise, return false.
        return Value.from(false);
    }

    /// 21.1.2.5 Number.isSafeInteger ( number )
    /// https://tc39.es/ecma262/#sec-number.issafeinteger
    fn isSafeInteger(_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const number = arguments.get(0);

        // 1. If number is an integral Number, then
        if (number == .number and number.number.isIntegral()) {
            // a. If abs(ℝ(number)) ≤ 2**53 - 1, return true.
            if (@abs(number.number.asFloat()) <= @as(f64, @floatFromInt(std.math.maxInt(u53)))) {
                return Value.from(true);
            }
        }

        // 2. Return false.
        return Value.from(false);
    }
};

/// 21.1.3 Properties of the Number Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-number-prototype-object
pub const NumberPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try Number.create(realm.agent, .{
            .fields = .{
                .number_data = types.Number.from(0),
            },
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "toFixed", toFixed, 1, realm);
        try defineBuiltinFunction(object, "toLocaleString", toLocaleString, 0, realm);
        try defineBuiltinFunction(object, "toPrecision", toPrecision, 1, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);

        return object;
    }

    /// 21.1.3.7.1 ThisNumberValue ( value )
    /// https://tc39.es/ecma262/#sec-thisnumbervalue
    fn thisNumberValue(agent: *Agent, value: Value) error{ExceptionThrown}!types.Number {
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
            .{},
        );
    }

    /// 21.1.3.3 Number.prototype.toFixed ( fractionDigits )
    /// https://tc39.es/ecma262/#sec-number.prototype.toprecision
    fn toFixed(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const fraction_digits_value = arguments.get(0);

        // 1. Let x be ? ThisNumberValue(this value).
        const x_number = try thisNumberValue(agent, this_value);

        // 2. Let f be ? ToIntegerOrInfinity(fractionDigits).
        // 3. Assert: If fractionDigits is undefined, then f is 0.
        const fraction_digits_f64 = try fraction_digits_value.toIntegerOrInfinity(agent);

        // 4. If f is not finite, throw a RangeError exception.
        if (!std.math.isFinite(fraction_digits_f64)) {
            return agent.throwException(.range_error, "Fraction digits must be a finite number", .{});
        }

        // 5. If f < 0 or f > 100, throw a RangeError exception.
        if (fraction_digits_f64 < 0 or fraction_digits_f64 > 100) {
            return agent.throwException(.range_error, "Fraction digits must be in range 0-100", .{});
        }
        const fraction_digits: usize = @intFromFloat(fraction_digits_f64);

        // 6. If x is not finite, return Number::toString(x, 10).
        if (!x_number.isFinite()) {
            return Value.from(try x_number.toString(agent.gc_allocator, 10));
        }

        // 7. Set x to ℝ(x).
        var x = x_number.asFloat();

        // 8. Let s be the empty String.
        var sign: []const u8 = "";

        // 9. If x < 0, then
        if (x < 0) {
            // a. Set s to "-".
            sign = "-";

            // b. Set x to -x.
            x = -x;
        }

        // 10. If x ≥ 10**21, then
        if (x >= 10e21) {
            // a. Let m be ! ToString(𝔽(x)).
            return Value.from(Value.from(x).toString(agent) catch |err| try noexcept(err));
        }

        // 11. Else,
        //     a. Let n be an integer for which n / 10**f - x is as close to zero as possible. If
        //        there are two such n, pick the larger n.
        //     b. If n = 0, let m be "0". Otherwise, let m be the String value consisting of the
        //        digits of the decimal representation of n (in order, with no leading zeroes).
        //     c. If f ≠ 0, then
        //         i. Let k be the length of m.
        //         ii. If k ≤ f, then
        //             1. Let z be the String value consisting of f + 1 - k occurrences of the code
        //                unit 0x0030 (DIGIT ZERO).
        //             2. Set m to the string-concatenation of z and m.
        //             3. Set k to f + 1.
        //         iii. Let a be the first k - f code units of m.
        //         iv. Let b be the other f code units of m.
        //         v. Set m to the string-concatenation of a, ".", and b.
        // 12. Return the string-concatenation of s and m.
        return Value.from(
            String.fromAscii(
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "{s}{d:.[2]}",
                    .{ sign, x, fraction_digits },
                ),
            ),
        );
    }

    /// 21.1.3.4 Number.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-number.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const x = try thisNumberValue(agent, this_value);
        return Value.from(try x.toString(agent.gc_allocator, 10));
    }

    /// 21.1.3.5 Number.prototype.toPrecision ( precision )
    /// https://tc39.es/ecma262/#sec-number.prototype.toprecision
    fn toPrecision(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const precision_value = arguments.get(0);

        // 1. Let x be ? ThisNumberValue(this value).
        const x_number = try thisNumberValue(agent, this_value);

        // 2. If precision is undefined, return ! ToString(x).
        if (precision_value == .undefined) {
            return Value.from(Value.from(x_number).toString(agent) catch |err| try noexcept(err));
        }

        // 3. Let p be ? ToIntegerOrInfinity(precision).
        const precision_f64 = try precision_value.toIntegerOrInfinity(agent);

        // 4. If x is not finite, return Number::toString(x, 10).
        if (!x_number.isFinite()) {
            return Value.from(try x_number.toString(agent.gc_allocator, 10));
        }

        // 5. If p < 1 or p > 100, throw a RangeError exception.
        if (precision_f64 < 1 or precision_f64 > 100) {
            return agent.throwException(.range_error, "Precision must be in range 1-100", .{});
        }
        const precision: usize = @intFromFloat(precision_f64);

        // 6. Set x to ℝ(x).
        var x = x_number.asFloat();

        // 7. Let s be the empty String.
        var sign: []const u8 = "";

        // 8. If x < 0, then
        if (x < 0) {
            // a. Set s to the code unit 0x002D (HYPHEN-MINUS).
            sign = "-";

            // b. Set x to -x.
            x = -x;
        }

        var exponent: i64 = undefined;
        var number_string: []const u8 = undefined;

        // 9. If x = 0, then
        if (x == 0) {
            // a. Let m be the String value consisting of p occurrences of the code unit 0x0030 (DIGIT ZERO).
            number_string = try std.fmt.allocPrint(
                agent.gc_allocator,
                "{s:0>[1]}",
                .{ "", precision },
            );

            // b. Let e be 0.
            exponent = 0;
        }
        // 10. Else,
        else {
            // a. Let e and n be integers such that 10**(p - 1) ≤ n < 10**p and for which
            //    n × 10**(e - p + 1) - x is as close to zero as possible. If there are two such
            //    sets of e and n, pick the e and n for which n × 10**(e - p + 1) is larger.
            exponent = @intFromFloat(@floor(std.math.log10(x)));
            const number = @round(
                x / std.math.pow(
                    f64,
                    10,
                    @floatFromInt(exponent - @as(i64, @intCast(precision)) + 1),
                ),
            );

            // b. Let m be the String value consisting of the digits of the decimal representation
            //    of n (in order, with no leading zeroes).
            number_string = try std.fmt.allocPrint(agent.gc_allocator, "{d}", .{number});

            // c. If e < -6 or e ≥ p, then
            if (exponent < -6 or exponent >= precision) {
                // i. Assert: e ≠ 0.
                std.debug.assert(exponent != 0);

                // ii. If p ≠ 1, then
                if (precision != 1) {
                    // 1. Let a be the first code unit of m.
                    const a = number_string[0..1];

                    // 2. Let b be the other p - 1 code units of m.
                    const b = number_string[1..];

                    // 3. Set m to the string-concatenation of a, ".", and b.
                    number_string = try std.fmt.allocPrint(
                        agent.gc_allocator,
                        "{s}.{s}",
                        .{ a, b },
                    );
                }

                var exponent_sign: u8 = undefined;

                // iii. If e > 0, then
                if (exponent > 0) {
                    // 1. Let c be the code unit 0x002B (PLUS SIGN).
                    exponent_sign = '+';
                }
                // iv. Else,
                else {
                    // 1. Assert: e < 0.
                    std.debug.assert(exponent < 0);

                    // 2. Let c be the code unit 0x002D (HYPHEN-MINUS).
                    exponent_sign = '-';

                    // 3. Set e to -e.
                    exponent = -exponent;
                }

                // v. Let d be the String value consisting of the digits of the decimal
                //    representation of e (in order, with no leading zeroes).
                // vi. Return the string-concatenation of s, m, the code unit 0x0065 (LATIN SMALL
                //     LETTER E), c, and d.
                return Value.from(
                    String.fromAscii(
                        try std.fmt.allocPrint(
                            agent.gc_allocator,
                            "{s}{s}e{c}{d}",
                            .{ sign, number_string, exponent_sign, exponent },
                        ),
                    ),
                );
            }
        }

        // 11. If e = p - 1, return the string-concatenation of s and m.
        if (exponent == precision - 1) {
            return Value.from(
                String.fromAscii(
                    try std.fmt.allocPrint(agent.gc_allocator, "{s}{s}", .{ sign, number_string }),
                ),
            );
        }

        // 12. If e ≥ 0, then
        if (exponent >= 0) {
            // a. Set m to the string-concatenation of the first e + 1 code units of m, the code
            //    unit 0x002E (FULL STOP), and the remaining p - (e + 1) code units of m.
            number_string = try std.fmt.allocPrint(
                agent.gc_allocator,
                "{s}.{s}",
                .{ number_string[0..@intCast(exponent + 1)], number_string[@intCast(exponent + 1)..] },
            );
        }
        // 13. Else,
        else {
            // a. Set m to the string-concatenation of the code unit 0x0030 (DIGIT ZERO), the code
            //    unit 0x002E (FULL STOP), -(e + 1) occurrences of the code unit 0x0030 (DIGIT ZERO),
            //    and the String m.
            number_string = try std.fmt.allocPrint(
                agent.gc_allocator,
                "0.{s:0>[2]}{s}",
                .{ "", number_string, @as(usize, @intCast(-(exponent + 1))) },
            );
        }

        // 14. Return the string-concatenation of s and m.
        return Value.from(
            String.fromAscii(
                try std.fmt.allocPrint(agent.gc_allocator, "{s}{s}", .{ sign, number_string }),
            ),
        );
    }

    /// 21.1.3.6 Number.prototype.toString ( [ radix ] )
    /// https://tc39.es/ecma262/#sec-number.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const radix = arguments.get(0);

        // 1. Let x be ? ThisNumberValue(this value).
        const x = try thisNumberValue(agent, this_value);

        // 2. If radix is undefined, let radixMV be 10.
        // 3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
        const radix_mv = if (radix == .undefined) 10 else try radix.toIntegerOrInfinity(agent);

        // 4. If radixMV is not in the inclusive interval from 2 to 36, throw a RangeError exception.
        if (radix_mv < 2 or radix_mv > 36) {
            return agent.throwException(.range_error, "Radix must be in range 2-36", .{});
        }

        // 5. Return Number::toString(x, radixMV).
        return Value.from(try x.toString(agent.gc_allocator, @intFromFloat(radix_mv)));
    }

    /// 21.1.3.7 Number.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-number.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisNumberValue(this value).
        return Value.from(try thisNumberValue(agent, this_value));
    }
};

/// 21.1.4 Properties of Number Instances
/// https://tc39.es/ecma262/#sec-properties-of-number-instances
pub const Number = MakeObject(.{
    .Fields = struct {
        /// [[NumberData]]
        number_data: types.Number,
    },
    .tag = .number,
});
