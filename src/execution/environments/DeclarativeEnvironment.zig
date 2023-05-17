//! 9.1.1.1 Declarative Environment Records
//! https://tc39.es/ecma262/#sec-declarative-environment-records

const environments = @import("../environments.zig");

const Environment = environments.Environment;

const Self = @This();

/// [[OuterEnv]]
outer_env: ?Environment,

/// 9.1.1.1.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasbinding-n
pub fn hasBinding(self: Self, name: []const u8) bool {
    // TODO: 1. If envRec has a binding for N, return true.
    _ = self;
    _ = name;

    // 2. Return false.
    return false;
}

/// 9.1.1.1.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}
