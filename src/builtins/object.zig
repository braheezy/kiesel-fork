//! 20.1 Object Objects
//! https://tc39.es/ecma262/#sec-object-objects

const execution = @import("../execution.zig");
const immutable_prototype = @import("immutable_prototype.zig");
const types = @import("../types.zig");

const Object_ = types.Object;
const Realm = execution.Realm;

/// 20.1.3 Properties of the Object Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object
pub const ObjectPrototype = struct {
    pub fn create(realm: *Realm) !Object_ {
        return try Object.create(realm.agent, .{
            .prototype = null,
            .internal_methods = .{
                .setPrototypeOf = immutable_prototype.setPrototypeOf,
            },
        });
    }
};

/// 20.1.4 Properties of Object Instances
/// https://tc39.es/ecma262/#sec-properties-of-object-instances
pub const Object = Object_.Factory(.{});
