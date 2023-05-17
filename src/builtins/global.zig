//! 19 The Global Object
//! https://tc39.es/ecma262/#sec-global-object

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;

const NameAndPropertyDescriptor = struct {
    []const u8,
    PropertyDescriptor,
};

pub fn globalObjectProperties(realm: *Realm) ![5]NameAndPropertyDescriptor {
    // NOTE: For the sake of compactness we're breaking the line length recommendations here.
    return [_]NameAndPropertyDescriptor{
        // 19.1.1 globalThis
        // https://tc39.es/ecma262/#sec-globalthis
        .{ "globalThis", .{ .value = Value.from(realm.global_env.global_this_value), .writable = true, .enumerable = false, .configurable = true } },

        // 19.1.2 Infinity
        // https://tc39.es/ecma262/#sec-value-properties-of-the-global-object-infinity
        .{ "Infinity", .{ .value = Value.infinity(), .writable = false, .enumerable = false, .configurable = false } },

        // 19.1.3 NaN
        // https://tc39.es/ecma262/#sec-value-properties-of-the-global-object-nan
        .{ "NaN", .{ .value = Value.nan(), .writable = false, .enumerable = false, .configurable = false } },

        // 19.1.4 undefined
        // https://tc39.es/ecma262/#sec-undefined
        .{ "undefined", .{ .value = .undefined, .writable = false, .enumerable = false, .configurable = false } },

        // 19.3.7 Boolean ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-boolean
        .{ "Boolean", .{ .value = Value.from(try realm.intrinsics.@"%Boolean%"()), .writable = true, .enumerable = false, .configurable = true } },
    };
}
