//! 25.4 The Atomics Object
//! https://tc39.es/ecma262/#sec-atomics-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Object = types.Object;
const Realm = execution.Realm;

pub const Atomics = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        return object;
    }
};
