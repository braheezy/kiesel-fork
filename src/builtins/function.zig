//! 20.2 Function Objects
//! https://tc39.es/ecma262/#sec-function-objects

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;

/// 20.2.3 Properties of the Function Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
pub const FunctionPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        return createBuiltinFunction(realm.agent, .{ .regular = behaviour }, .{
            .length = 0,
            .name = "",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    fn behaviour(_: *Agent, _: Value, _: ArgumentsList) !Value {
        return .undefined;
    }
};
