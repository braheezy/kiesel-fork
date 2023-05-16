//! 9.1.1.4 Global Environment Records
//! https://tc39.es/ecma262/#sec-global-environment-records

const std = @import("std");

const environments = @import("../environments.zig");
const types = @import("../../types.zig");

const DeclarativeEnvironment = environments.DeclarativeEnvironment;
const Environment = environments.Environment;
const Object = types.Object;
const ObjectEnvironment = environments.ObjectEnvironment;

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

/// 9.1.1.4.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return true.
    return true;
}

/// 9.1.1.4.11 GetThisBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-getthisbinding
pub fn getThisBinding(self: Self) Object {
    // 1. Return envRec.[[GlobalThisValue]].
    return self.global_this_value;
}
