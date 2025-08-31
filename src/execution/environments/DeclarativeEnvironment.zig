//! 9.1.1.1 Declarative Environment Records
//! https://tc39.es/ecma262/#sec-declarative-environment-records

const builtin = @import("builtin");
const std = @import("std");

const environments = @import("../environments.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Environment = environments.Environment;
const Object = types.Object;
const String = types.String;
const Value = types.Value;

const DeclarativeEnvironment = @This();

/// [[OuterEnv]]
outer_env: ?Environment,

bindings: String.HashMapUnmanaged(Binding),

pub const Binding = struct {
    value: Value,
    initialized: bool,
    strict: bool,
    mutable: bool,
    deletable: bool,
};

comptime {
    switch (builtin.target.ptrBitWidth()) {
        // Only some 32-bit platforms have certain bitpacking optimizations applied
        32 => std.debug.assert(@sizeOf(Binding) == @sizeOf(Value) + 4 or @sizeOf(Binding) == @sizeOf(Value) + 8),
        64 => std.debug.assert(@sizeOf(Binding) == @sizeOf(Value) + 8),
        else => unreachable,
    }
}

/// 9.1.1.1.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasbinding-n
pub fn hasBinding(self: DeclarativeEnvironment, name: *const String) bool {
    // 1. If envRec has a binding for N, return true.
    // 2. Return false.
    return self.bindings.contains(name);
}

/// 9.1.1.2.2 CreateMutableBinding ( N, D )
/// https://tc39.es/ecma262/#sec-object-environment-records-createmutablebinding-n-d
pub fn createMutableBinding(
    self: *DeclarativeEnvironment,
    agent: *Agent,
    name: *const String,
    deletable: bool,
) std.mem.Allocator.Error!void {
    // 1. Assert: envRec does not already have a binding for N.
    // 2. Create a mutable binding in envRec for N and record that it is uninitialized. If D is
    //    true, record that the newly created binding may be deleted by a subsequent DeleteBinding
    //    call.
    try self.bindings.putNoClobber(agent.gc_allocator, name, .{
        .value = undefined,
        .initialized = false,
        .strict = false,
        .mutable = true,
        .deletable = deletable,
    });

    // 3. Return unused.
}

/// 9.1.1.1.3 CreateImmutableBinding ( N, S )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-createimmutablebinding-n-s
pub fn createImmutableBinding(
    self: *DeclarativeEnvironment,
    agent: *Agent,
    name: *const String,
    strict: bool,
) std.mem.Allocator.Error!void {
    // 1. Assert: envRec does not already have a binding for N.
    // 2. Create an immutable binding in envRec for N and record that it is uninitialized. If S is
    //    true, record that the newly created binding is a strict binding.
    try self.bindings.putNoClobber(agent.gc_allocator, name, .{
        .value = undefined,
        .initialized = false,
        .strict = strict,
        .mutable = false,
        .deletable = false,
    });

    // 3. Return unused.
}

/// 9.1.1.1.4 InitializeBinding ( N, V )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-initializebinding-n-v
pub fn initializeBinding(
    self: DeclarativeEnvironment,
    name: *const String,
    value: Value,
) void {
    var binding = self.bindings.getPtr(name).?;

    // 1. Assert: envRec must have an uninitialized binding for N.
    std.debug.assert(binding.initialized == false);

    // 2. Set the bound value for N in envRec to V.
    binding.value = value;

    // 3. Record that the binding for N in envRec has been initialized.
    binding.initialized = true;

    // 4. Return unused.
}

/// 9.1.1.1.5 SetMutableBinding ( N, V, S )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-setmutablebinding-n-v-s
pub fn setMutableBinding(
    self: *DeclarativeEnvironment,
    agent: *Agent,
    name: *const String,
    value: Value,
    strict: bool,
) Agent.Error!void {
    const maybe_binding = self.bindings.getPtr(name);

    // 1. If envRec does not have a binding for N, then
    const binding = maybe_binding orelse {
        // a. If S is true, throw a ReferenceError exception.
        if (strict) {
            return agent.throwException(
                .reference_error,
                "'{f}' is not defined",
                .{name.fmtUnquoted()},
            );
        }

        // b. Perform ! envRec.CreateMutableBinding(N, true).
        try self.createMutableBinding(agent, name, true);

        // c. Perform ! envRec.InitializeBinding(N, V).
        self.initializeBinding(name, value);

        // d. Return unused.
        return;
    };

    // 2. If the binding for N in envRec is a strict binding, set S to true.
    const final_strict = if (binding.strict) true else strict;

    // 3. If the binding for N in envRec has not yet been initialized, then
    if (!binding.initialized) {
        // a. Throw a ReferenceError exception.
        return agent.throwException(
            .reference_error,
            "Binding for '{f}' is not initialized",
            .{name.fmtUnquoted()},
        );
    }

    // 4. Else if the binding for N in envRec is a mutable binding, then
    if (binding.mutable) {
        // a. Change its bound value to V.
        binding.value = value;
    } else {
        // 5. Else,
        // a. Assert: This is an attempt to change the value of an immutable binding.
        // b. If S is true, throw a TypeError exception.
        if (final_strict) {
            return agent.throwException(
                .type_error,
                "Binding for '{f}' is immutable",
                .{name.fmtUnquoted()},
            );
        }
    }

    // 6. Return unused.
}

/// 9.1.1.1.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-getbindingvalue-n-s
pub fn getBindingValue(
    self: DeclarativeEnvironment,
    agent: *Agent,
    name: *const String,
    _: bool,
) error{ExceptionThrown}!Value {
    // 1. Assert: envRec has a binding for N.
    const binding = self.bindings.get(name).?;

    // 2. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError exception.
    if (!binding.initialized) {
        return agent.throwException(
            .reference_error,
            "Binding for '{f}' is not initialized",
            .{name.fmtUnquoted()},
        );
    }

    // 3. Return the value currently bound to N in envRec.
    return binding.value;
}

/// 9.1.1.1.7 DeleteBinding ( N )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-deletebinding-n
pub fn deleteBinding(self: *DeclarativeEnvironment, name: *const String) bool {
    // 1. Assert: envRec has a binding for N.
    const binding = self.bindings.get(name).?;

    // 2. If the binding for N in envRec cannot be deleted, return false.
    if (!binding.deletable) return false;

    // 3. Remove the binding for N from envRec.
    _ = self.bindings.remove(name);

    // 4. Return true.
    return true;
}

/// 9.1.1.1.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hasthisbinding
pub fn hasThisBinding(_: DeclarativeEnvironment) bool {
    // 1. Return false.
    return false;
}

/// 9.1.1.1.9 HasSuperBinding ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-hassuperbinding
pub fn hasSuperBinding(_: DeclarativeEnvironment) bool {
    // 1. Return false.
    return false;
}

/// 9.1.1.1.10 WithBaseObject ( )
/// https://tc39.es/ecma262/#sec-declarative-environment-records-withbaseobject
pub fn withBaseObject(_: DeclarativeEnvironment) ?*Object {
    // 1. Return undefined.
    return null;
}
