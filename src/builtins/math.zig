//! 21.3 The Math Object
//! https://tc39.es/ecma262/#sec-math-object

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
        try defineBuiltinFunction(object, "ceil", ceil, 1, realm);
        try defineBuiltinFunction(object, "random", random, 0, realm);

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

    /// 21.3.2.27 Math.random ( )
    /// https://tc39.es/ecma262/#sec-math.random
    fn random(agent: *Agent, _: Value, _: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const value = @min(realm.rng.random().float(f64), 1 - std.math.floatEps(f64));
        return Value.from(value);
    }
};
