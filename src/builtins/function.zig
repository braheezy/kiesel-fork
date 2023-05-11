//! 20.2 Function Objects
//! https://tc39.es/ecma262/#sec-function-objects

const builtin_function = @import("builtin_function.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtin_function.createBuiltinFunction;

/// 20.2.3 Properties of the Function Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
pub const FunctionPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        return createBuiltinFunction(
            realm.agent,
            behaviour,
            .{
                .length = 0,
                .name = "",
                .realm = realm,
                .prototype = try realm.intrinsics.@"%Object.prototype%"(),
            },
        );
    }

    fn behaviour(agent: *Agent, this_value: Value, arguments: []const Value, new_target: ?Object) !Value {
        _ = agent;
        _ = this_value;
        _ = arguments;
        _ = new_target;
        return .undefined;
    }
};
