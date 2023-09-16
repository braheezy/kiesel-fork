//! 21.3 The Math Object
//! https://tc39.es/ecma262/#sec-math-object

const std = @import("std");

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
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 21.3.1 Value Properties of the Math Object
/// https://tc39.es/ecma262/#sec-value-properties-of-the-math-object
pub const Math = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 21.3.1.1 Math.E
        // https://tc39.es/ecma262/#sec-math.e
        try defineBuiltinProperty(object, "E", PropertyDescriptor{
            .value = Value.from(std.math.e),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.2 Math.LN10
        // https://tc39.es/ecma262/#sec-math.ln10
        try defineBuiltinProperty(object, "LN10", PropertyDescriptor{
            .value = Value.from(std.math.ln10),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.2 Math.LN2
        // https://tc39.es/ecma262/#sec-math.ln2
        try defineBuiltinProperty(object, "LN2", PropertyDescriptor{
            .value = Value.from(std.math.ln2),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.4 Math.LOG10E
        // https://tc39.es/ecma262/#sec-math.log10e
        try defineBuiltinProperty(object, "LOG10E", PropertyDescriptor{
            .value = Value.from(std.math.log10e),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.5 Math.LOG2E
        // https://tc39.es/ecma262/#sec-math.log2e
        try defineBuiltinProperty(object, "LOG2E", PropertyDescriptor{
            .value = Value.from(std.math.log2e),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.6 Math.PI
        // https://tc39.es/ecma262/#sec-math.pi
        try defineBuiltinProperty(object, "PI", PropertyDescriptor{
            .value = Value.from(std.math.pi),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.7 Math.SQRT1_2
        // https://tc39.es/ecma262/#sec-math.sqrt1_2
        try defineBuiltinProperty(object, "SQRT1_2", PropertyDescriptor{
            .value = Value.from(std.math.sqrt1_2),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.8 Math.SQRT2
        // https://tc39.es/ecma262/#sec-math.sqrt2
        try defineBuiltinProperty(object, "SQRT2", PropertyDescriptor{
            .value = Value.from(std.math.sqrt2),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.3.1.9 Math [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-math-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Math"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        try defineBuiltinFunction(object, "abs", abs, 1, realm);
        try defineBuiltinFunction(object, "acos", acos, 1, realm);
        try defineBuiltinFunction(object, "acosh", acosh, 1, realm);
        try defineBuiltinFunction(object, "asin", asin, 1, realm);
        try defineBuiltinFunction(object, "asinh", asinh, 1, realm);
        try defineBuiltinFunction(object, "atan", atan, 1, realm);
        try defineBuiltinFunction(object, "atanh", atanh, 1, realm);
        try defineBuiltinFunction(object, "cbrt", cbrt, 1, realm);
        try defineBuiltinFunction(object, "ceil", ceil, 1, realm);
        try defineBuiltinFunction(object, "clz32", clz32, 1, realm);
        try defineBuiltinFunction(object, "cos", cos, 1, realm);
        try defineBuiltinFunction(object, "cosh", cosh, 1, realm);
        try defineBuiltinFunction(object, "exp", exp, 1, realm);
        try defineBuiltinFunction(object, "expm1", expm1, 1, realm);
        try defineBuiltinFunction(object, "floor", floor, 1, realm);
        try defineBuiltinFunction(object, "log", log, 1, realm);
        try defineBuiltinFunction(object, "log1p", log1p, 1, realm);
        try defineBuiltinFunction(object, "log10", log10, 1, realm);
        try defineBuiltinFunction(object, "log2", log2, 1, realm);
        try defineBuiltinFunction(object, "max", max, 2, realm);
        try defineBuiltinFunction(object, "pow", pow, 2, realm);
        try defineBuiltinFunction(object, "random", random, 0, realm);
        try defineBuiltinFunction(object, "round", round, 1, realm);
        try defineBuiltinFunction(object, "sign", sign, 1, realm);
        try defineBuiltinFunction(object, "sin", sin, 1, realm);
        try defineBuiltinFunction(object, "sinh", sinh, 1, realm);
        try defineBuiltinFunction(object, "sqrt", sqrt, 1, realm);
        try defineBuiltinFunction(object, "tan", tan, 1, realm);
        try defineBuiltinFunction(object, "tanh", tanh, 1, realm);
        try defineBuiltinFunction(object, "trunc", trunc, 1, realm);

        return object;
    }

    /// 21.3.2.1 Math.abs ( x )
    /// https://tc39.es/ecma262/#sec-math.abs
    fn abs(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, return NaN.
        // 3. If n is -0𝔽, return +0𝔽.
        // 4. If n is -∞𝔽, return +∞𝔽.
        // 5. If n < -0𝔽, return -n.
        // 6. Return n.
        return Value.from(@fabs(n.asFloat()));
    }

    /// 21.3.2.2 Math.acos ( x )
    /// https://tc39.es/ecma262/#sec-math.acos
    fn acos(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, n > 1𝔽, or n < -1𝔽, return NaN.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    inverse cosine of ℝ(n).
        return Value.from(std.math.acos(n.asFloat()));
    }

    /// 21.3.2.3 Math.acosh ( x )
    /// https://tc39.es/ecma262/#sec-math.acosh
    fn acosh(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n < 1𝔽, return NaN.
        // 5. Return an implementation-approximated Number value representing the result of the
        //    inverse hyperbolic cosine of ℝ(n).
        return Value.from(std.math.acosh(n.asFloat()));
    }

    /// 21.3.2.4 Math.asin ( x )
    /// https://tc39.es/ecma262/#sec-math.asin
    fn asin(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n > 1𝔽 or n < -1𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    inverse sine of ℝ(n).
        return Value.from(std.math.asin(n.asFloat()));
    }

    /// 21.3.2.5 Math.asinh ( x )
    /// https://tc39.es/ecma262/#sec-math.asinh
    fn asinh(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n > 1𝔽 or n < -1𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    inverse sine of ℝ(n).
        return Value.from(std.math.asinh(n.asFloat()));
    }

    /// 21.3.2.6 Math.atan ( x )
    /// https://tc39.es/ecma262/#sec-math.atan
    fn atan(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is +∞𝔽, return an implementation-approximated Number value representing π / 2.
        // 4. If n is -∞𝔽, return an implementation-approximated Number value representing -π / 2.
        // 5. Return an implementation-approximated Number value representing the result of the
        //    inverse tangent of ℝ(n).
        return Value.from(std.math.atan(n.asFloat()));
    }

    /// 21.3.2.7 Math.atanh ( x )
    /// https://tc39.es/ecma262/#sec-math.atanh
    fn atanh(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n > 1𝔽 or n < -1𝔽, return NaN.
        // 4. If n is 1𝔽, return +∞𝔽.
        // 5. If n is -1𝔽, return -∞𝔽.
        // 6. Return an implementation-approximated Number value representing the result of the
        //    inverse hyperbolic tangent of ℝ(n).
        return Value.from(std.math.atanh(n.asFloat()));
    }

    /// 21.3.2.9 Math.cbrt ( x )
    /// https://tc39.es/ecma262/#sec-math.cbrt
    fn cbrt(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. Return an implementation-approximated Number value representing the result of the
        //    cube root of ℝ(n).
        return Value.from(std.math.cbrt(n.asFloat()));
    }

    /// 21.3.2.10 Math.ceil ( x )
    /// https://tc39.es/ecma262/#sec-math.ceil
    fn ceil(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. If n < -0𝔽 and n > -1𝔽, return -0𝔽.
        // 4. If n is an integral Number, return n.
        // 5. Return the smallest (closest to -∞) integral Number value that is not less than n.
        return Value.from(n.ceil());
    }

    /// 21.3.2.11 Math.clz32 ( x )
    /// https://tc39.es/ecma262/#sec-math.clz32
    fn clz32(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToUint32(x).
        const n = try x.toUint32(agent);

        // 2. Let p be the number of leading zero bits in the unsigned 32-bit binary representation of n.
        const p = @clz(n);

        // 3. Return 𝔽(p).
        return Value.from(p);
    }

    /// 21.3.2.12 Math.cos ( x )
    /// https://tc39.es/ecma262/#sec-math.cos
    fn cos(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite, return NaN.
        // 3. If n is either +0𝔽 or -0𝔽, return 1𝔽.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    cosine of ℝ(n).
        return Value.from(@cos(n.asFloat()));
    }

    /// 21.3.2.13 Math.cosh ( x )
    /// https://tc39.es/ecma262/#sec-math.cosh
    fn cosh(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, return NaN.
        // 3. If n is either +∞𝔽 or -∞𝔽, return +∞𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return 1𝔽.
        // 5. Return an implementation-approximated Number value representing the result of the
        //    hyperbolic cosine of ℝ(n).
        return Value.from(std.math.cosh(n.asFloat()));
    }

    /// 21.3.2.14 Math.exp ( x )
    /// https://tc39.es/ecma262/#sec-math.exp
    fn exp(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is either +0𝔽 or -0𝔽, return 1𝔽.
        // 4. If n is -∞𝔽, return +0𝔽.
        // 5. Return an implementation-approximated Number value representing the result of the
        //    exponential function of ℝ(n).
        return Value.from(@exp(n.asFloat()));
    }

    /// 21.3.2.15 Math.expm1 ( x )
    /// https://tc39.es/ecma262/#sec-math.expm1
    fn expm1(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
        // 3. If n is -∞𝔽, return -1𝔽.
        // 4. Return an implementation-approximated Number value representing the result of
        //    subtracting 1 from the exponential function of ℝ(n).
        if (n.isNegativeZero()) return Value.from(n);
        return Value.from(@exp(n.asFloat()) - 1);
    }

    /// 21.3.2.16 Math.floor ( x )
    /// https://tc39.es/ecma262/#sec-math.floor
    fn floor(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. If n < 1𝔽 and n > +0𝔽, return +0𝔽.
        // 4. If n is an integral Number, return n.
        // 5. Return the greatest (closest to +∞) integral Number value that is not greater than n.
        return Value.from(n.floor());
    }

    /// 21.3.2.20 Math.log ( x )
    /// https://tc39.es/ecma262/#sec-math.log
    fn log(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
        // 5. If n < -0𝔽, return NaN.
        // 6. Return an implementation-approximated Number value representing the result of the
        //    natural logarithm of ℝ(n).
        return Value.from(@log(n.asFloat()));
    }

    /// 21.3.2.21 Math.log1p ( x )
    /// https://tc39.es/ecma262/#sec-math.log1p
    fn log1p(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
        // 3. If n is -1𝔽, return -∞𝔽.
        // 4. If n < -1𝔽, return NaN.
        // 5. Return an implementation-approximated Number value representing the result of the
        //    natural logarithm of 1 + ℝ(n).
        if (n.isNegativeZero()) return Value.from(n);
        return Value.from(@log(1 + n.asFloat()));
    }

    /// 21.3.2.22 Math.log10 ( x )
    /// https://tc39.es/ecma262/#sec-math.log10
    fn log10(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
        // 5. If n < -0𝔽, return NaN.
        // 6. Return an implementation-approximated Number value representing the result of the
        //    base 10 logarithm of ℝ(n).
        return Value.from(@log10(n.asFloat()));
    }

    /// 21.3.2.23 Math.log2 ( x )
    /// https://tc39.es/ecma262/#sec-math.log2
    fn log2(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
        // 5. If n < -0𝔽, return NaN.
        // 6. Return an implementation-approximated Number value representing the result of the
        //    base 2 logarithm of ℝ(n).
        return Value.from(@log2(n.asFloat()));
    }

    /// 21.3.2.24 Math.max ( ...args )
    /// https://tc39.es/ecma262/#sec-math.max
    fn max(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        // 1. Let coerced be a new empty List.
        var coerced = try std.ArrayList(Number).initCapacity(agent.gc_allocator, arguments.count());
        defer coerced.deinit();

        // 2. For each element arg of args, do
        for (arguments.values) |arg| {
            // a. Let n be ? ToNumber(arg).
            const n = try arg.toNumber(agent);

            // b. Append n to coerced.
            coerced.appendAssumeCapacity(n);
        }

        // 3. Let highest be -∞𝔽.
        var highest = Number.from(-std.math.inf(f64));

        // 4. For each element number of coerced, do
        for (coerced.items) |number| {
            // a. If number is NaN, return NaN.
            if (number.isNan()) return Value.nan();

            // b. If number is +0𝔽 and highest is -0𝔽, set highest to +0𝔽.
            if (number.isPositiveZero() and highest.isNegativeZero()) {
                highest = Number.from(0);
                continue;
            }

            // c. If number > highest, set highest to number.
            if (number.asFloat() > highest.asFloat()) highest = number;
        }

        // 5. Return highest.
        return Value.from(highest);
    }

    /// 21.3.2.26 Math.pow ( base, exponent )
    /// https://tc39.es/ecma262/#sec-math.pow
    fn pow(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const base_value = arguments.get(0);
        const exponent_value = arguments.get(1);

        // 1. Set base to ? ToNumber(base).
        const base = try base_value.toNumber(agent);

        // 2. Set exponent to ? ToNumber(exponent).
        const exponent = try exponent_value.toNumber(agent);

        // 3. Return Number::exponentiate(base, exponent).
        return Value.from(base.exponentiate(exponent));
    }

    /// 21.3.2.27 Math.random ( )
    /// https://tc39.es/ecma262/#sec-math.random
    fn random(agent: *Agent, _: Value, _: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const value = @min(realm.rng.random().float(f64), 1 - std.math.floatEps(f64));
        return Value.from(value);
    }

    /// 21.3.2.28 Math.round ( x )
    /// https://tc39.es/ecma262/#sec-math.round
    fn round(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is an integral Number, return n.
        // 3. If n < 0.5𝔽 and n > +0𝔽, return +0𝔽.
        // 4. If n < -0𝔽 and n ≥ -0.5𝔽, return -0𝔽.
        // 5. Return the integral Number closest to n, preferring the Number closer to +∞ in the case of a tie.
        var rounded = n.ceil();
        if (rounded.asFloat() - 0.5 > n.asFloat()) {
            rounded = rounded.subtract(.{ .i32 = 1 });
        }
        return Value.from(rounded);
    }

    /// 21.3.2.29 Math.sign ( x )
    /// https://tc39.es/ecma262/#sec-math.sign
    fn sign(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        if (n.isNan() or n.isZero()) return Value.from(n);

        // 3. If n < -0𝔽, return -1𝔽.
        // 4. Return 1𝔽.
        return Value.from(std.math.sign(n.asFloat()));
    }

    /// 21.3.2.30 Math.sin ( x )
    /// https://tc39.es/ecma262/#sec-math.sin
    fn sin(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is either +∞𝔽 or -∞𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    sine of ℝ(n).
        return Value.from(@sin(n.asFloat()));
    }

    /// 21.3.2.31 Math.sinh ( x )
    /// https://tc39.es/ecma262/#sec-math.sinh
    fn sinh(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. Return an implementation-approximated Number value representing the result of the
        //    hyperbolic sine of ℝ(n).
        return Value.from(std.math.sinh(n.asFloat()));
    }

    /// 21.3.2.32 Math.sqrt ( x )
    /// https://tc39.es/ecma262/#sec-math.sqrt
    fn sqrt(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
        // 3. If n < -0𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    square root of ℝ(n).
        return Value.from(@sqrt(n.asFloat()));
    }

    /// 21.3.2.33 Math.tan ( x )
    /// https://tc39.es/ecma262/#sec-math.tan
    fn tan(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is either +∞𝔽 or -∞𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the result of the
        //    tangent of ℝ(n).
        return Value.from(@tan(n.asFloat()));
    }

    /// 21.3.2.34 Math.tanh ( x )
    /// https://tc39.es/ecma262/#sec-math.tanh
    fn tanh(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is +∞𝔽, return 1𝔽.
        // 4. If n is -∞𝔽, return -1𝔽.
        // 5. Return an implementation-approximated Number value representing the result of the
        //    hyperbolic tangent of ℝ(n).
        return Value.from(std.math.tanh(n.asFloat()));
    }

    /// 21.3.2.35 Math.trunc ( x )
    /// https://tc39.es/ecma262/#sec-math.trunc
    fn trunc(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. If n < 1𝔽 and n > +0𝔽, return +0𝔽.
        // 4. If n < -0𝔽 and n > -1𝔽, return -0𝔽.
        // 5. Return the integral Number nearest n in the direction of +0𝔽.
        return Value.from(n.truncate());
    }
};
