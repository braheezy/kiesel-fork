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

        return object;
    }
};
