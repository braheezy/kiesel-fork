//! 9.1.1.3 Function Environment Records
//! https://tc39.es/ecma262/#sec-function-environment-records

const std = @import("std");

const builtins = @import("../../builtins.zig");
const environments = @import("../environments.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const DeclarativeEnvironment = environments.DeclarativeEnvironment;
const ECMAScriptFunction = builtins.ECMAScriptFunction;
const Object = types.Object;
const Value = types.Value;
const noexcept = utils.noexcept;

const FunctionEnvironment = @This();

pub const ThisBindingStatus = enum {
    lexical,
    initialized,
    uninitialized,
};

/// [[ThisValue]]
this_value: Value,

/// [[ThisBindingStatus]]
this_binding_status: ThisBindingStatus,

/// [[FunctionObject]]
function_object: *ECMAScriptFunction,

/// [[NewTarget]]
new_target: ?*Object,

// NOTE: This is how we implement the spec's inheritance of function environments.
declarative_environment: *DeclarativeEnvironment,

/// 9.1.1.3.1 BindThisValue ( V )
/// https://tc39.es/ecma262/#sec-bindthisvalue
pub fn bindThisValue(self: *FunctionEnvironment, value: Value) error{ExceptionThrown}!Value {
    const agent = self.function_object.object.agent;

    // 1. Assert: envRec.[[ThisBindingStatus]] is not lexical.
    std.debug.assert(self.this_binding_status != .lexical);

    // 2. If envRec.[[ThisBindingStatus]] is initialized, throw a ReferenceError exception.
    if (self.this_binding_status == .initialized) {
        return agent.throwException(
            .reference_error,
            "Function this binding is already initialized",
            .{},
        );
    }

    // 3. Set envRec.[[ThisValue]] to V.
    self.this_value = value;

    // 4. Set envRec.[[ThisBindingStatus]] to initialized.
    self.this_binding_status = .initialized;

    // 5. Return V.
    return value;
}

/// 9.1.1.3.2 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-function-environment-records-hasthisbinding
pub fn hasThisBinding(self: FunctionEnvironment) bool {
    // 1. If envRec.[[ThisBindingStatus]] is lexical, return false; otherwise, return true.
    return self.this_binding_status != .lexical;
}

/// 9.1.1.3.3 HasSuperBinding ( )
/// https://tc39.es/ecma262/#sec-function-environment-records-hassuperbinding
pub fn hasSuperBinding(self: FunctionEnvironment) bool {
    // 1. If envRec.[[ThisBindingStatus]] is lexical, return false.
    if (self.this_binding_status == .lexical) return false;

    // 2. If envRec.[[FunctionObject]].[[HomeObject]] is undefined, return false; otherwise, return true.
    return self.function_object.fields.home_object != null;
}

/// 9.1.1.3.4 GetThisBinding ( )
/// https://tc39.es/ecma262/#sec-function-environment-records-getthisbinding
pub fn getThisBinding(self: FunctionEnvironment) error{ExceptionThrown}!Value {
    const agent = self.function_object.object.agent;

    // 1. Assert: envRec.[[ThisBindingStatus]] is not lexical.
    std.debug.assert(self.this_binding_status != .lexical);

    // 2. If envRec.[[ThisBindingStatus]] is uninitialized, throw a ReferenceError exception.
    if (self.this_binding_status == .uninitialized) {
        return agent.throwException(
            .reference_error,
            "Function this binding is uninitialized",
            .{},
        );
    }

    // 3. Return envRec.[[ThisValue]].
    return self.this_value;
}

/// 9.1.1.3.5 GetSuperBase ( )
/// https://tc39.es/ecma262/#sec-getsuperbase
pub fn getSuperBase(self: FunctionEnvironment) std.mem.Allocator.Error!Value {
    // 1. Let home be envRec.[[FunctionObject]].[[HomeObject]].
    const home = self.function_object.fields.home_object;

    // 2. If home is undefined, return undefined.
    if (home == null) return .undefined;

    // 3. Assert: home is an ordinary object.
    // 4. Return ! home.[[GetPrototypeOf]]().
    return if (home.?.internal_methods.getPrototypeOf(
        home.?,
    ) catch |err| try noexcept(err)) |prototype|
        Value.from(prototype)
    else
        .null;
}
