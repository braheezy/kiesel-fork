//! 8 The Intl Object
//! https://tc39.es/ecma402/#intl-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Object = types.Object;
const Realm = execution.Realm;

comptime {
    const build_options = @import("build-options");
    if (!build_options.enable_intl) @compileError("Intl is not enabled");
}

pub const Intl = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        return object;
    }
};
