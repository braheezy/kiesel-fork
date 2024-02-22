//! 27.5 Generator Objects
//! https://tc39.es/ecma262/#sec-generator-objects

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

/// 27.5.1 The %GeneratorPrototype% Object
/// https://tc39.es/ecma262/#sec-properties-of-generator-prototype
pub const GeneratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });

        // 27.5.1.1 %GeneratorPrototype%.constructor
        // https://tc39.es/ecma262/#sec-generator.prototype.constructor
        try defineBuiltinProperty(object, "constructor", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%GeneratorFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 27.5.1.5 %GeneratorPrototype% [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-generator.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Generator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

/// 27.5.2 Properties of Generator Instances
/// https://tc39.es/ecma262/#sec-properties-of-generator-instances
pub const Generator = MakeObject(.{
    .tag = .generator,
});
