//! 9.1.1.4 Global Environment Records
//! https://tc39.es/ecma262/#sec-global-environment-records

const std = @import("std");

const environments = @import("../environments.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const DeclarativeEnvironment = environments.DeclarativeEnvironment;
const Environment = environments.Environment;
const Object = types.Object;
const ObjectEnvironment = environments.ObjectEnvironment;
const Value = types.Value;

const Self = @This();

/// [[ObjectRecord]]
object_record: *ObjectEnvironment,

/// [[GlobalThisValue]]
global_this_value: Object,

/// [[DeclarativeRecord]]
declarative_record: *DeclarativeEnvironment,

/// [[VarNames]]
var_names: std.ArrayList([]const u8),

/// [[OuterEnv]]
outer_env: ?Environment,

/// 9.1.1.4.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-global-environment-records-hasbinding-n
pub fn hasBinding(self: Self, name: []const u8) !bool {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, return true.
    if (self.declarative_record.hasBinding(name)) return true;

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Return ? ObjRec.HasBinding(N).
    return self.object_record.hasBinding(name);
}

/// 9.1.1.4.5 SetMutableBinding ( N, V, S )
/// https://tc39.es/ecma262/#sec-global-environment-records-setmutablebinding-n-v-s
pub fn setMutableBinding(self: Self, agent: *Agent, name: []const u8, value: Value, strict: bool) !void {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, then
    if (self.declarative_record.hasBinding(name)) {
        // a. Return ? DclRec.SetMutableBinding(N, V, S).
        return self.declarative_record.setMutableBinding(agent, name, value, strict);
    }

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Return ? ObjRec.SetMutableBinding(N, V, S).
    return self.object_record.setMutableBinding(agent, name, value, strict);
}

/// 9.1.1.4.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-global-environment-records-getbindingvalue-n-s
pub fn getBindingValue(self: Self, agent: *Agent, name: []const u8, strict: bool) !Value {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, then
    if (self.declarative_record.hasBinding(name)) {
        // a. Return ? DclRec.GetBindingValue(N, S).
        return self.declarative_record.getBindingValue(agent, name, strict);
    }

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Return ? ObjRec.GetBindingValue(N, S).
    return self.object_record.getBindingValue(name, strict);
}

/// 9.1.1.4.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return true.
    return true;
}

/// 9.1.1.4.10 WithBaseObject ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-withbaseobject
pub fn withBaseObject(_: Self) ?Object {
    // 1. Return undefined.
    return null;
}

/// 9.1.1.4.11 GetThisBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-getthisbinding
pub fn getThisBinding(self: Self) Object {
    // 1. Return envRec.[[GlobalThisValue]].
    return self.global_this_value;
}
