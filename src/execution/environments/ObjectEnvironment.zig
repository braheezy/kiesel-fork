//! 9.1.1.2 Object Environment Records
//! https://tc39.es/ecma262/#sec-object-environment-records

const environments = @import("../environments.zig");
const types = @import("../../types.zig");

const Environment = environments.Environment;
const Object = types.Object;

const Self = @This();

/// [[BindingObject]]
binding_object: Object,

/// [[IsWithEnvironment]]
is_with_environment: bool,

/// [[OuterEnv]]
outer_env: ?Environment,

/// 9.1.1.2.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-object-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}
