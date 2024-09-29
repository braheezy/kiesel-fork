const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const defineBuiltinFunction = utils.defineBuiltinFunction;

/// 27.1.2 The %IteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%iteratorprototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "%Symbol.iterator%", @"%Symbol.iterator%", 0, realm);
    }

    /// 27.1.2.1 %IteratorPrototype% [ %Symbol.iterator% ] ( )
    /// https://tc39.es/ecma262/#sec-%iteratorprototype%-%symbol.iterator%
    fn @"%Symbol.iterator%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};
