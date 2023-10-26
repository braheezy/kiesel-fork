//! 25.5 The JSON Object
//! https://tc39.es/ecma262/#sec-json-object

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const defineBuiltinProperty = utils.defineBuiltinProperty;

pub const JSON = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 25.5.3 JSON [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-json-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("JSON"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};
