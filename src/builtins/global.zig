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
        // 19.3.7 Boolean ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-boolean
        .{ "Boolean", .{ .value = Value.from(try realm.intrinsics.@"%Boolean%"()), .writable = true, .enumerable = false, .configurable = true } },
    };
}
