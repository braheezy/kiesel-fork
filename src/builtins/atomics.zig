//! 25.4 The Atomics Object
//! https://tc39.es/ecma262/#sec-atomics-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const defineBuiltinProperty = utils.defineBuiltinProperty;

pub const Atomics = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 25.4.17 Atomics [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-atomics-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Atomics"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};
