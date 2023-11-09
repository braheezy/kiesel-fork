//! 27.6 AsyncGenerator Objects
//! https://tc39.es/ecma262/#sec-asyncgenerator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 27.6.1 Properties of the AsyncGenerator Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-prototype
pub const AsyncGeneratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%AsyncIteratorPrototype%"(),
        });

        // 27.6.1.1 AsyncGenerator.prototype.constructor
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-constructor
        try defineBuiltinProperty(object, "constructor", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 27.6.1.5 AsyncGenerator.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("AsyncGenerator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

/// 27.6.2 Properties of AsyncGenerator Instances
/// https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-intances
pub const AsyncGenerator = MakeObject(.{
    .tag = .async_generator,
});
