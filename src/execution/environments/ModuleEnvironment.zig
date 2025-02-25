//! 9.1.1.5 Module Environment Records
//! https://tc39.es/ecma262/#sec-module-environment-records

const std = @import("std");

const environments = @import("../environments.zig");
const execution = @import("../../execution.zig");
const language = @import("../../language.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const DeclarativeEnvironment = environments.DeclarativeEnvironment;
const Module = language.Module;
const String = types.String;
const Value = types.Value;

const ModuleEnvironment = @This();

indirect_bindings: String.HashMapUnmanaged(IndirectBinding),

// NOTE: This is how we implement the spec's inheritance of module environments.
declarative_environment: DeclarativeEnvironment,

pub const IndirectBinding = struct {
    module: Module,
    binding_name: *const String,
};

pub fn hasBinding(self: ModuleEnvironment, name: *const String) bool {
    // Handled via DeclarativeEnvironment in the spec but with a vague "has a binding", so we need
    // to override the implementation and check the indirect bindings as well.
    return self.indirect_bindings.contains(name) or self.declarative_environment.bindings.contains(name);
}

/// 9.1.1.5.1 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-module-environment-records-getbindingvalue-n-s
pub fn getBindingValue(
    self: ModuleEnvironment,
    agent: *Agent,
    name: *const String,
    strict: bool,
) error{ ExceptionThrown, OutOfMemory }!Value {
    // 1. Assert: S is true.
    std.debug.assert(strict);

    // 2. Assert: envRec has a binding for N.
    std.debug.assert(self.hasBinding(name));

    // 3. If the binding for N is an indirect binding, then
    if (self.indirect_bindings.get(name)) |indirect_binding| {
        // a. Let M and N2 be the indirection values provided when this binding for N was created.
        const module = indirect_binding.module;
        const binding_name = indirect_binding.binding_name;

        // b. Let targetEnv be M.[[Environment]].
        const target_env = switch (module) {
            inline else => |m| m.environment,
        };

        // c. If targetEnv is empty, throw a ReferenceError exception.
        if (target_env == null) {
            return agent.throwException(
                .reference_error,
                "Module environment is not initialized",
                .{},
            );
        }

        // d. Return ? targetEnv.GetBindingValue(N2, true).
        return target_env.?.getBindingValue(agent, binding_name, true);
    }

    // 4. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError
    //    exception.
    // 5. Return the value currently bound to N in envRec.
    const binding = self.declarative_environment.bindings.get(name).?;
    return binding.value orelse agent.throwException(
        .reference_error,
        "Binding for '{}' is not initialized",
        .{name},
    );
}

/// 9.1.1.5.2 DeleteBinding ( N )
/// https://tc39.es/ecma262/#sec-module-environment-records-deletebinding-n
pub fn deleteBinding(_: ModuleEnvironment, _: *const String) bool {
    // The DeleteBinding concrete method of a Module Environment Record is never used within this
    // specification.
    unreachable;
}

/// 9.1.1.5.3 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-module-environment-records-hasthisbinding
pub fn hasThisBinding(_: ModuleEnvironment) bool {
    // 1. Return true.
    return true;
}

/// 9.1.1.5.4 GetThisBinding ( )
/// https://tc39.es/ecma262/#sec-module-environment-records-getthisbinding
pub fn getThisBinding(_: ModuleEnvironment) Value {
    // 1. Return undefined.
    return .undefined;
}

/// 9.1.1.5.5 CreateImportBinding ( envRec, N, M, N2 )
/// https://tc39.es/ecma262/#sec-createimportbinding
pub fn createImportBinding(
    self: *ModuleEnvironment,
    agent: *Agent,
    name: *const String,
    module: Module,
    binding_name: *const String,
) std.mem.Allocator.Error!void {
    // 1. Assert: envRec does not already have a binding for N.
    // 2. Assert: When M.[[Environment]] is instantiated, it will have a direct binding for N2.
    // 3. Create an immutable indirect binding in envRec for N that references M and N2 as its
    //    target binding and record that the binding is initialized.
    try self.indirect_bindings.putNoClobber(agent.gc_allocator, name, .{
        .module = module,
        .binding_name = binding_name,
    });

    // 4. Return unused.
}
