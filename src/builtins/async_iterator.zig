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

/// 27.1.3 The %AsyncIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-asynciteratorprototype
pub const AsyncIteratorPrototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "%Symbol.asyncIterator%", @"%Symbol.asyncIterator%", 0, realm);
    }

    /// 27.1.3.1 %AsyncIteratorPrototype% [ %Symbol.asyncIterator% ] ( )
    /// https://tc39.es/ecma262/#sec-%asynciteratorprototype%-%symbol.asynciterator%
    fn @"%Symbol.asyncIterator%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};
