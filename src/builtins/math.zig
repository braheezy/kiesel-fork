//! 21.3 The Math Object
//! https://tc39.es/ecma262/#sec-math-object

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
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

        return object;
    }
};
