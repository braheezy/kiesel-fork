const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const defineBuiltinFunction = utils.defineBuiltinFunction;

/// 27.1.2 The %IteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%iteratorprototype%-object
pub const IteratorPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "@@iterator", @"@@iterator", 0, realm);

        return object;
    }

    /// 27.1.2.1 %IteratorPrototype% [ @@iterator ] ( )
    /// https://tc39.es/ecma262/#sec-%iteratorprototype%-@@iterator
    fn @"@@iterator"(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return the this value.
        return this_value;
    }
};
