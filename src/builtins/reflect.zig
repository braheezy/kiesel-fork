//! 28.1 The Reflect Object
//! https://tc39.es/ecma262/#sec-reflect-object

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Object = types.Object;
const Realm = execution.Realm;

pub const Reflect = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        return object;
    }
};
