//! 9.1.1.1 Declarative Environment Records
//! https://tc39.es/ecma262/#sec-declarative-environment-records

const std = @import("std");

const environments = @import("../environments.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Environment = environments.Environment;
const Object = types.Object;
const Value = types.Value;

const Self = @This();

/// [[OuterEnv]]
outer_env: ?Environment,

bindings: std.StringArrayHashMap(Binding),

pub const Binding = struct {
    value: ?Value,
    strict: bool,
    mutable: bool,
    deletable: bool,
};

/// 9.1.1.1.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasbinding-n
pub fn hasBinding(self: Self, name: []const u8) bool {
    // 1. If envRec has a binding for N, return true.
    // 2. Return false.
    return self.bindings.contains(name);
}

/// 9.1.1.1.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-getbindingvalue-n-s
pub fn getBindingValue(self: Self, agent: *Agent, name: []const u8, _: bool) !Value {
    // 1. Assert: envRec has a binding for N.
    const binding = self.bindings.get(name).?;

    // 2. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError exception.
    // 3. Return the value currently bound to N in envRec.
    return binding.value orelse agent.throwException(
        .reference_error,
        try std.fmt.allocPrint(
            agent.gc_allocator,
            "Binding for '{s}' is not initialized",
            .{name},
        ),
    );
}

/// 9.1.1.1.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}

/// 9.1.1.1.10 WithBaseObject ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-withbaseobject
pub fn withBaseObject(_: Self) ?Object {
    // 1. Return undefined.
    return null;
}
