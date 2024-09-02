//! 21.3 The Math Object
//! https://tc39.es/ecma262/#sec-math-object

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Number = types.Number;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;

/// 21.3.1 Value Properties of the Math Object
/// https://tc39.es/ecma262/#sec-value-properties-of-the-math-object
pub const Math = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
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

        // 21.3.1.9 Math [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-math-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
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
        try defineBuiltinFunction(object, "atan2", atan2, 2, realm);
        try defineBuiltinFunction(object, "cbrt", cbrt, 1, realm);
        try defineBuiltinFunction(object, "ceil", ceil, 1, realm);
        try defineBuiltinFunction(object, "clz32", clz32, 1, realm);
        try defineBuiltinFunction(object, "cos", cos, 1, realm);
        try defineBuiltinFunction(object, "cosh", cosh, 1, realm);
        try defineBuiltinFunction(object, "exp", exp, 1, realm);
        try defineBuiltinFunction(object, "expm1", expm1, 1, realm);
        try defineBuiltinFunction(object, "f16round", f16round, 1, realm);
        try defineBuiltinFunction(object, "floor", floor, 1, realm);
        try defineBuiltinFunction(object, "fround", fround, 1, realm);
        try defineBuiltinFunction(object, "hypot", hypot, 2, realm);
        try defineBuiltinFunction(object, "imul", imul, 2, realm);
        try defineBuiltinFunction(object, "log", log, 1, realm);
        try defineBuiltinFunction(object, "log1p", log1p, 1, realm);
        try defineBuiltinFunction(object, "log10", log10, 1, realm);
        try defineBuiltinFunction(object, "log2", log2, 1, realm);
        try defineBuiltinFunction(object, "max", max, 2, realm);
        try defineBuiltinFunction(object, "min", min, 2, realm);
        try defineBuiltinFunction(object, "pow", pow, 2, realm);
        try defineBuiltinFunction(object, "random", random, 0, realm);
        try defineBuiltinFunction(object, "round", round, 1, realm);
        try defineBuiltinFunction(object, "sign", sign, 1, realm);
        try defineBuiltinFunction(object, "sin", sin, 1, realm);
        try defineBuiltinFunction(object, "sinh", sinh, 1, realm);
        try defineBuiltinFunction(object, "sqrt", sqrt, 1, realm);
        try defineBuiltinFunction(object, "sumPrecise", sumPrecise, 1, realm);
        try defineBuiltinFunction(object, "tan", tan, 1, realm);
        try defineBuiltinFunction(object, "tanh", tanh, 1, realm);
        try defineBuiltinFunction(object, "trunc", trunc, 1, realm);
    }

    /// 21.3.2.1 Math.abs ( x )
    /// https://tc39.es/ecma262/#sec-math.abs
    fn abs(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, return NaN.
        // 3. If n is -0𝔽, return +0𝔽.
        // 4. If n is -∞𝔽, return +∞𝔽.
        // 5. If n < -0𝔽, return -n.
        // 6. Return n.
        return Value.from(@abs(n.asFloat()));
    }

    /// 21.3.2.2 Math.acos ( x )
    /// https://tc39.es/ecma262/#sec-math.acos
    fn acos(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, n > 1𝔽, or n < -1𝔽, return NaN.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. Return an implementation-approximated Number value representing the inverse cosine of
        //    ℝ(n).
        return Value.from(std.math.acos(n.asFloat()));
    }

    /// 21.3.2.3 Math.acosh ( x )
    /// https://tc39.es/ecma262/#sec-math.acosh
    fn acosh(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n < 1𝔽, return NaN.
        // 5. Return an implementation-approximated Number value representing the inverse
        //    hyperbolic cosine of ℝ(n).
        return Value.from(std.math.acosh(n.asFloat()));
    }

    /// 21.3.2.4 Math.asin ( x )
    /// https://tc39.es/ecma262/#sec-math.asin
    fn asin(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n > 1𝔽 or n < -1𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the inverse sine of
        //    ℝ(n).
        return Value.from(std.math.asin(n.asFloat()));
    }

    /// 21.3.2.5 Math.asinh ( x )
    /// https://tc39.es/ecma262/#sec-math.asinh
    fn asinh(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n > 1𝔽 or n < -1𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the inverse sine of
        //    ℝ(n).
        return Value.from(std.math.asinh(n.asFloat()));
    }

    /// 21.3.2.6 Math.atan ( x )
    /// https://tc39.es/ecma262/#sec-math.atan
    fn atan(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is +∞𝔽, return an implementation-approximated Number value representing π / 2.
        // 4. If n is -∞𝔽, return an implementation-approximated Number value representing -π / 2.
        // 5. Return an implementation-approximated Number value representing the inverse tangent
        //    of ℝ(n).
        return Value.from(std.math.atan(n.asFloat()));
    }

    /// 21.3.2.7 Math.atanh ( x )
    /// https://tc39.es/ecma262/#sec-math.atanh
    fn atanh(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n > 1𝔽 or n < -1𝔽, return NaN.
        // 4. If n is 1𝔽, return +∞𝔽.
        // 5. If n is -1𝔽, return -∞𝔽.
        // 6. Return an implementation-approximated Number value representing the inverse
        //    hyperbolic tangent of ℝ(n).
        return Value.from(std.math.atanh(n.asFloat()));
    }

    /// 21.3.2.8 Math.atan2 ( y, x )
    /// https://tc39.es/ecma262/#sec-math.atan2
    fn atan2(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const y = arguments.get(0);
        const x = arguments.get(1);

        // 1. Let ny be ? ToNumber(y).
        const ny = try y.toNumber(agent);

        // 2. Let nx be ? ToNumber(x).
        const nx = try x.toNumber(agent);

        // 3. If ny is NaN or nx is NaN, return NaN.
        // 4. If ny is +∞𝔽, then
        //     a. If nx is +∞𝔽, return an implementation-approximated Number value representing π / 4.
        //     b. If nx is -∞𝔽, return an implementation-approximated Number value representing 3π / 4.
        //     c. Return an implementation-approximated Number value representing π / 2.
        // 5. If ny is -∞𝔽, then
        //     a. If nx is +∞𝔽, return an implementation-approximated Number value representing -π / 4.
        //     b. If nx is -∞𝔽, return an implementation-approximated Number value representing -3π / 4.
        //     c. Return an implementation-approximated Number value representing -π / 2.
        // 6. If ny is +0𝔽, then
        //     a. If nx > +0𝔽 or nx is +0𝔽, return +0𝔽.
        //     b. Return an implementation-approximated Number value representing π.
        // 7. If ny is -0𝔽, then
        //     a. If nx > +0𝔽 or nx is +0𝔽, return -0𝔽.
        //     b. Return an implementation-approximated Number value representing -π.
        // 8. Assert: ny is finite and is neither +0𝔽 nor -0𝔽.
        // 9. If ny > +0𝔽, then
        //     a. If nx is +∞𝔽, return +0𝔽.
        //     b. If nx is -∞𝔽, return an implementation-approximated Number value representing π.
        //     c. If nx is either +0𝔽 or -0𝔽, return an implementation-approximated Number value
        //        representing π / 2.
        // 10. If ny < -0𝔽, then
        //     a. If nx is +∞𝔽, return -0𝔽.
        //     b. If nx is -∞𝔽, return an implementation-approximated Number value representing -π.
        //     c. If nx is either +0𝔽 or -0𝔽, return an implementation-approximated Number value
        //        representing -π / 2.
        // 11. Assert: nx is finite and is neither +0𝔽 nor -0𝔽.
        // 12. Let r be the inverse tangent of abs(ℝ(ny) / ℝ(nx)).
        // 13. If nx < -0𝔽, then
        //     a. If ny > +0𝔽, set r to π - r.
        //     b. Else, set r to -π + r.
        // 14. Else,
        //     a. If ny < -0𝔽, set r to -r.
        // 15. Return an implementation-approximated Number value representing r.
        return Value.from(std.math.atan2(ny.asFloat(), nx.asFloat()));
    }

    /// 21.3.2.9 Math.cbrt ( x )
    /// https://tc39.es/ecma262/#sec-math.cbrt
    fn cbrt(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. Return an implementation-approximated Number value representing the cube root of
        //    ℝ(n).
        return Value.from(std.math.cbrt(n.asFloat()));
    }

    /// 21.3.2.10 Math.ceil ( x )
    /// https://tc39.es/ecma262/#sec-math.ceil
    fn ceil(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
    fn clz32(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
    fn cos(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite, return NaN.
        // 3. If n is either +0𝔽 or -0𝔽, return 1𝔽.
        // 4. Return an implementation-approximated Number value representing the cosine of ℝ(n).
        return Value.from(@cos(n.asFloat()));
    }

    /// 21.3.2.13 Math.cosh ( x )
    /// https://tc39.es/ecma262/#sec-math.cosh
    fn cosh(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, return NaN.
        // 3. If n is either +∞𝔽 or -∞𝔽, return +∞𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return 1𝔽.
        // 5. Return an implementation-approximated Number value representing the hyperbolic cosine
        //    of ℝ(n).
        return Value.from(std.math.cosh(n.asFloat()));
    }

    /// 21.3.2.14 Math.exp ( x )
    /// https://tc39.es/ecma262/#sec-math.exp
    fn exp(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is either +0𝔽 or -0𝔽, return 1𝔽.
        // 4. If n is -∞𝔽, return +0𝔽.
        // 5. Return an implementation-approximated Number value representing the exponential
        //    function of ℝ(n).
        return Value.from(@exp(n.asFloat()));
    }

    /// 21.3.2.15 Math.expm1 ( x )
    /// https://tc39.es/ecma262/#sec-math.expm1
    fn expm1(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
        // 3. If n is -∞𝔽, return -1𝔽.
        // 4. Let exp be the exponential function of ℝ(n).
        // 5. Return an implementation-approximated Number value representing exp - 1.
        if (n.isNegativeZero()) return Value.from(n);
        return Value.from(@exp(n.asFloat()) - 1);
    }

    /// 3.1 Math.f16round ( x )
    /// https://tc39.es/proposal-float16array/#sec-math.f16round
    fn f16round(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, return NaN.
        // 3. If n is one of +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return n.
        // 4. Let n16 be the result of converting n to IEEE 754-2019 binary16 format using
        //    roundTiesToEven mode.
        const n_16 = n.toFloat16();

        // 5. Let n64 be the result of converting n16 to IEEE 754-2019 binary64 format.
        const n_64: f64 = @floatCast(n_16);

        // 6. Return the ECMAScript Number value corresponding to n64.
        return Value.from(n_64);
    }

    /// 21.3.2.16 Math.floor ( x )
    /// https://tc39.es/ecma262/#sec-math.floor
    fn floor(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. If n < 1𝔽 and n > +0𝔽, return +0𝔽.
        // 4. If n is an integral Number, return n.
        // 5. Return the greatest (closest to +∞) integral Number value that is not greater than n.
        return Value.from(n.floor());
    }

    /// 21.3.2.17 Math.fround ( x )
    /// https://tc39.es/ecma262/#sec-math.fround
    fn fround(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is NaN, return NaN.
        // 3. If n is one of +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return n.
        if (n.isZero() or !n.isFinite()) return Value.from(n);

        // 4. Let n32 be the result of converting n to IEEE 754-2019 binary32 format using roundTiesToEven mode.
        const n32: f32 = @floatCast(n.asFloat());

        // 5. Let n64 be the result of converting n32 to IEEE 754-2019 binary64 format.
        const n64: f64 = @floatCast(n32);

        // 6. Return the ECMAScript Number value corresponding to n64.
        return Value.from(n64);
    }

    /// 21.3.2.18 Math.hypot ( ...args )
    /// https://tc39.es/ecma262/#sec-math.hypot
    fn hypot(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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

        // 3. For each element number of coerced, do
        for (coerced.items) |number| {
            // a. If number is either +∞𝔽 or -∞𝔽, return +∞𝔽.
            if (number.isPositiveInf() or number.isNegativeInf()) return .infinity;
        }

        // 4. Let onlyZero be true.
        var only_zero = true;

        // 5. For each element number of coerced, do
        for (coerced.items) |number| {
            // a. If number is NaN, return NaN.
            if (number.isNan()) return .nan;

            // b. If number is neither +0𝔽 nor -0𝔽, set onlyZero to false.
            if (!number.isZero()) only_zero = false;
        }

        // 6. If onlyZero is true, return +0𝔽.
        if (only_zero) return Value.from(0);

        // 7. Return an implementation-approximated Number value representing the square root of
        //    the sum of squares of the mathematical values of the elements of coerced.
        var sum_of_squares: f64 = 0;
        for (coerced.items) |number| {
            sum_of_squares += number.asFloat() * number.asFloat();
        }
        return Value.from(@sqrt(sum_of_squares));
    }

    /// 21.3.2.19 Math.imul ( x, y )
    /// https://tc39.es/ecma262/#sec-math.imul
    fn imul(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);
        const y = arguments.get(1);

        // 1. Let a be ℝ(? ToUint32(x)).
        const a = try x.toUint32(agent);

        // 2. Let b be ℝ(? ToUint32(y)).
        const b = try y.toUint32(agent);

        // 3. Let product be (a × b) modulo 2**32.
        const product = a *% b;

        // 4. If product ≥ 2**31, return 𝔽(product - 2**32); otherwise return 𝔽(product).
        return Value.from(@as(i32, @bitCast(product)));
    }

    /// 21.3.2.20 Math.log ( x )
    /// https://tc39.es/ecma262/#sec-math.log
    fn log(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
        // 5. If n < -0𝔽, return NaN.
        // 6. Return an implementation-approximated Number value representing the natural logarithm
        //    of ℝ(n).
        return Value.from(@log(n.asFloat()));
    }

    /// 21.3.2.21 Math.log1p ( x )
    /// https://tc39.es/ecma262/#sec-math.log1p
    fn log1p(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
        // 3. If n is -1𝔽, return -∞𝔽.
        // 4. If n < -1𝔽, return NaN.
        // 5. Return an implementation-approximated Number value representing the natural logarithm
        //    of 1 + ℝ(n).
        if (n.isNegativeZero()) return Value.from(n);
        return Value.from(@log(1 + n.asFloat()));
    }

    /// 21.3.2.22 Math.log10 ( x )
    /// https://tc39.es/ecma262/#sec-math.log10
    fn log10(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
        // 5. If n < -0𝔽, return NaN.
        // 6. Return an implementation-approximated Number value representing the base 10 logarithm
        //    of ℝ(n).
        return Value.from(@log10(n.asFloat()));
    }

    /// 21.3.2.23 Math.log2 ( x )
    /// https://tc39.es/ecma262/#sec-math.log2
    fn log2(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is either NaN or +∞𝔽, return n.
        // 3. If n is 1𝔽, return +0𝔽.
        // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
        // 5. If n < -0𝔽, return NaN.
        // 6. Return an implementation-approximated Number value representing the base 2 logarithm of ℝ(n).
        return Value.from(@log2(n.asFloat()));
    }

    /// 21.3.2.24 Math.max ( ...args )
    /// https://tc39.es/ecma262/#sec-math.max
    fn max(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
            if (number.isNan()) return .nan;

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

    /// 21.3.2.25 Math.min ( ...args )
    /// https://tc39.es/ecma262/#sec-math.min
    fn min(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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

        // 3. Let lowest be +∞𝔽.
        var lowest = Number.from(std.math.inf(f64));

        // 4. For each element number of coerced, do
        for (coerced.items) |number| {
            // a. If number is NaN, return NaN.
            if (number.isNan()) return .nan;

            // b. If number is -0𝔽 and lowest is +0𝔽, set lowest to -0𝔽.
            if (number.isNegativeZero() and lowest.isPositiveZero()) {
                lowest = Number.from(-0.0);
                continue;
            }

            // c. If number < lowest, set lowest to number.
            if (number.asFloat() < lowest.asFloat()) lowest = number;
        }

        // 5. Return lowest.
        return Value.from(lowest);
    }

    /// 21.3.2.26 Math.pow ( base, exponent )
    /// https://tc39.es/ecma262/#sec-math.pow
    fn pow(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
    fn random(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const value = @min(realm.rng.random().float(f64), 1 - std.math.floatEps(f64));
        return Value.from(value);
    }

    /// 21.3.2.28 Math.round ( x )
    /// https://tc39.es/ecma262/#sec-math.round
    fn round(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
    fn sign(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
    fn sin(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is either +∞𝔽 or -∞𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the sine of ℝ(n).
        return Value.from(@sin(n.asFloat()));
    }

    /// 21.3.2.31 Math.sinh ( x )
    /// https://tc39.es/ecma262/#sec-math.sinh
    fn sinh(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
        // 3. Return an implementation-approximated Number value representing the hyperbolic sine
        //    of ℝ(n).
        return Value.from(std.math.sinh(n.asFloat()));
    }

    /// 21.3.2.32 Math.sqrt ( x )
    /// https://tc39.es/ecma262/#sec-math.sqrt
    fn sqrt(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
        // 3. If n < -0𝔽, return NaN.
        // 4. Return 𝔽(the square root of ℝ(n)).
        return Value.from(@sqrt(n.asFloat()));
    }

    fn twoSum(x: f64, y: f64) struct { f64, f64 } {
        const hi = x + y;
        const lo = y - (hi - x);
        return .{ hi, lo };
    }

    /// 2 Math.sumPrecise ( items )
    /// https://tc39.es/proposal-math-sum/#sec-math.sumprecise
    fn sumPrecise(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // The implementation for higher precision summation in steps 4 and 7.b.vi.4.b are based on
        // the JavaScript polyfill in the proposal's repository by Kevin Gibbons (bakkot on GitHub).
        // See the polyfill directory in https://github.com/tc39/proposal-math-sum for the licenses.

        const items_ = arguments.get(0);

        // 1. Perform ? RequireObjectCoercible(items).
        const items = try items_.requireObjectCoercible(agent);

        // 2. Let iteratorRecord be ? GetIterator(items, SYNC).
        var iterator = try getIterator(agent, items, .sync);

        // 3. Let state be MINUS-ZERO.
        var state: enum {
            finite,
            minus_infinity,
            minus_zero,
            not_a_number,
            plus_infinity,
        } = .minus_zero;

        // 4. Let sum be 0.
        const initial_partials_count = 32;

        var stack_fallback = std.heap.stackFallback(@sizeOf(f64) * initial_partials_count, agent.gc_allocator);
        const allocator = stack_fallback.get();

        var partials = try std.ArrayList(f64).initCapacity(allocator, initial_partials_count);
        defer partials.deinit();
        var overflow: f64 = 0.0;

        const max_f64 = std.math.floatMax(f64);
        const penultimate_f64 = 1.79769313486231550856e+308;
        const max_ulp = max_f64 - penultimate_f64;
        const @"2^1023" = std.math.pow(f64, 2, 1023);

        // 5. Let count be 0.
        var count: u53 = 0;

        // 6. Let next be NOT-STARTED.
        // 7. Repeat, while next is not DONE,
        //    a. Set next to ? IteratorStepValue(iteratorRecord).
        //    b. If next is not DONE, then
        while (try iterator.stepValue()) |next| {
            // i. Set count to count + 1.
            // ii. If count ≥ 2**53, then
            count = std.math.add(u53, count, 1) catch {
                // 1. Let error be ThrowCompletion(a newly created RangeError object).
                const @"error" = agent.throwException(
                    .range_error,
                    "Maximum number of values exceeded",
                    .{},
                );

                // 2. Return ? IteratorClose(iteratorRecord, error).
                return iterator.close(@as(Agent.Error!Value, @"error"));
            };

            // iii. NOTE: The above case is not expected to be reached in practice and is included
            //            only so that implementations may rely on inputs being "reasonably sized"
            //            without violating this specification.

            // iv. If next is not a Number, then
            if (!next.isNumber()) {
                // 1. Let error be ThrowCompletion(a newly created TypeError object).
                const @"error" = agent.throwException(
                    .type_error,
                    "Value is not a number",
                    .{},
                );

                // 2. Return ? IteratorClose(iteratorRecord, error).
                return iterator.close(@as(Agent.Error!Value, @"error"));
            }

            // v. Let n be next.
            const n = next.asNumber();

            // vi. If state is not NOT-A-NUMBER, then
            if (state != .not_a_number) {
                // 1. If n is NaN, then
                if (n.isNan()) {
                    // a. Set state to NOT-A-NUMBER.
                    state = .not_a_number;
                }
                // 2. Else if n is +∞𝔽, then
                else if (n.isPositiveInf()) {
                    // a. If state is MINUS-INFINITY, set state to NOT-A-NUMBER.
                    // b. Else, set state to PLUS-INFINITY.
                    state = if (state == .minus_infinity) .not_a_number else .plus_infinity;
                }
                // 3. Else if n is -∞𝔽, then
                else if (n.isNegativeInf()) {
                    // a. If state is PLUS-INFINITY, set state to NOT-A-NUMBER.
                    // b. Else, set state to MINUS-INFINITY.
                    state = if (state == .plus_infinity) .not_a_number else .minus_infinity;
                }
                // 4. Else if n is not -0𝔽 and state is either MINUS-ZERO or FINITE, then
                else if (!n.isNegativeZero() and (state == .minus_zero or state == .finite)) {
                    // a. Set state to FINITE.
                    state = .finite;

                    // b. Set sum to sum + ℝ(n).
                    var x = n.asFloat();
                    var written_partials_len: usize = 0;
                    for (partials.items) |*y| {
                        if (@abs(x) < @abs(y.*)) std.mem.swap(f64, &x, y);

                        var hi, var lo = twoSum(x, y.*);
                        if (std.math.isInf(hi)) {
                            const sign_ = std.math.sign(hi);
                            overflow += sign_;
                            std.debug.assert(@abs(overflow) < std.math.pow(f64, 2, 53));
                            x = (x - sign_ * @"2^1023") - sign_ * @"2^1023";
                            if (@abs(x) < @abs(y.*)) std.mem.swap(f64, &x, y);
                            hi, lo = twoSum(x, y.*);
                        }

                        if (lo != 0.0) {
                            partials.items[written_partials_len] = lo;
                            written_partials_len += 1;
                        }

                        x = hi;
                    }

                    partials.shrinkRetainingCapacity(written_partials_len);

                    if (x != 0.0) {
                        try partials.append(x);
                    }
                }
            }
        }

        switch (state) {
            // 8. If state is NOT-A-NUMBER, return NaN.
            .not_a_number => return .nan,
            // 9. If state is PLUS-INFINITY, return +∞𝔽.
            .plus_infinity => return .infinity,
            // 10. If state is MINUS-INFINITY, return -∞𝔽.
            .minus_infinity => return .negative_infinity,
            // 11. If state is MINUS-ZERO, return -0𝔽.
            .minus_zero => return Value.from(-0.0),
            // 12. Return 𝔽(sum).
            // 13. NOTE: The value of sum can be computed without arbitrary-precision arithmetic by
            //           a variety of algorithms. One such is the "Grow-Expansion" algorithm given
            //           in Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric
            //           Predicates by Jonathan Richard Shewchuk. A more recent algorithm is given
            //           in "Fast exact summation using small and large superaccumulators", code for
            //           which is available at https://gitlab.com/radfordneal/xsum.
            else => {
                var n = partials.items.len;
                var hi: f64, var lo: f64 = .{ 0.0, 0.0 };

                if (overflow != 0.0) {
                    const next = if (partials.items.len > 0) blk: {
                        n -= 1;
                        break :blk partials.items[n];
                    } else 0.0;

                    if (@abs(overflow) > 1.0 or std.math.sign(overflow) == std.math.sign(next)) {
                        return if (overflow > 0.0) .infinity else .negative_infinity;
                    }

                    hi, lo = twoSum(overflow * @"2^1023", next / 2.0);
                    lo *= 2.0;

                    if (std.math.isInf(hi * 2.0)) {
                        if (hi > 0.0) {
                            if (hi == @"2^1023" and lo == -(max_ulp / 2) and n > 0 and partials.items[n - 1] < 0.0) {
                                return Value.from(max_f64);
                            }
                            return .infinity;
                        } else if (hi == -@"2^1023" and lo == max_ulp / 2 and n > 0 and partials.items[n - 1] > 0.0) {
                            return Value.from(-max_f64);
                        }
                        return .negative_infinity;
                    }

                    if (lo != 0.0) {
                        partials.items[n] = lo;
                        n += 1;
                        lo = 0.0;
                    }

                    hi *= 2.0;
                }

                while (n > 0) {
                    const x = hi;
                    n -= 1;
                    const y = partials.items[n];
                    hi, lo = twoSum(x, y);
                    if (lo != 0.0) {
                        break;
                    }
                }

                if (n > 0 and lo != 0.0 and std.math.sign(lo) == std.math.sign(partials.items[n - 1])) {
                    const y = lo * 2.0;
                    const x = hi + y;
                    if (y == x - hi) {
                        hi = x;
                    }
                }

                return Value.from(hi);
            },
        }
    }

    /// 21.3.2.33 Math.tan ( x )
    /// https://tc39.es/ecma262/#sec-math.tan
    fn tan(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is either +∞𝔽 or -∞𝔽, return NaN.
        // 4. Return an implementation-approximated Number value representing the tangent of ℝ(n).
        return Value.from(@tan(n.asFloat()));
    }

    /// 21.3.2.34 Math.tanh ( x )
    /// https://tc39.es/ecma262/#sec-math.tanh
    fn tanh(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const x = arguments.get(0);

        // 1. Let n be ? ToNumber(x).
        const n = try x.toNumber(agent);

        // 2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
        // 3. If n is +∞𝔽, return 1𝔽.
        // 4. If n is -∞𝔽, return -1𝔽.
        // 5. Return an implementation-approximated Number value representing the hyperbolic
        //    tangent of ℝ(n).
        return Value.from(std.math.tanh(n.asFloat()));
    }

    /// 21.3.2.35 Math.trunc ( x )
    /// https://tc39.es/ecma262/#sec-math.trunc
    fn trunc(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
