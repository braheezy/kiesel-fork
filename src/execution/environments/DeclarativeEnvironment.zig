//! 9.1.1.1 Declarative Environment Records
//! https://tc39.es/ecma262/#sec-declarative-environment-records

const environments = @import("../environments.zig");
const types = @import("../../types.zig");

const Environment = environments.Environment;
const Value = types.Value;

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

/// 9.1.1.1.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-getbindingvalue-n-s
pub fn getBindingValue(self: Self, name: []const u8, strict: bool) Value {
    _ = self;
    _ = name;
    _ = strict;
    @panic("Not implemented");
}

/// 9.1.1.1.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}
