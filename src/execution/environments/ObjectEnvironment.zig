//! 9.1.1.2 Object Environment Records
//! https://tc39.es/ecma262/#sec-object-environment-records

const std = @import("std");

const environments = @import("../environments.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Environment = environments.Environment;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

const Self = @This();

/// [[BindingObject]]
binding_object: Object,

/// [[IsWithEnvironment]]
is_with_environment: bool,

/// [[OuterEnv]]
outer_env: ?Environment,

/// 9.1.1.2.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-object-environment-records-hasbinding-n
pub fn hasBinding(self: Self, name: []const u8) !bool {
    const agent = self.binding_object.agent();

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let foundBinding be ? HasProperty(bindingObject, N).
    const found_binding = try self.binding_object.hasProperty(PropertyKey.from(name));

    // 3. If foundBinding is false, return false.
    if (!found_binding) return false;

    // 4. If envRec.[[IsWithEnvironment]] is false, return true.
    if (!self.is_with_environment) return true;

    // 5. Let unscopables be ? Get(bindingObject, @@unscopables).
    const unscopables = try self.binding_object.get(
        PropertyKey.from(agent.well_known_symbols.@"@@unscopables"),
    );

    // 6. If unscopables is an Object, then
    if (unscopables == .object) {
        // a. Let blocked be ToBoolean(? Get(unscopables, N)).
        const blocked = (try unscopables.object.get(PropertyKey.from(name))).toBoolean();

        // b. If blocked is true, return false.
        if (blocked) return false;
    }

    // 7. Return true.
    return true;
}

/// 9.1.1.2.5 SetMutableBinding ( N, V, S )
/// https://tc39.es/ecma262/#sec-object-environment-records-setmutablebinding-n-v-s
pub fn setMutableBinding(self: Self, agent: *Agent, name: []const u8, value: Value, strict: bool) !void {
    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let stillExists be ? HasProperty(bindingObject, N).
    const still_exists = try self.binding_object.hasProperty(PropertyKey.from(name));

    // 3. If stillExists is false and S is true, throw a ReferenceError exception.
    if (!still_exists and strict) {
        return agent.throwException(
            .reference_error,
            try std.fmt.allocPrint(agent.gc_allocator, "'{s}' is not defined", .{name}),
        );
    }

    // 4. Perform ? Set(bindingObject, N, V, S).
    try self.binding_object.set(PropertyKey.from(name), value, if (strict) .throw else .ignore);

    // 5. Return unused.
}

/// 9.1.1.2.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-object-environment-records-getbindingvalue-n-s
pub fn getBindingValue(self: Self, name: []const u8, strict: bool) !Value {
    const agent = self.binding_object.agent();

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let value be ? HasProperty(bindingObject, N).
    const value = try self.binding_object.hasProperty(PropertyKey.from(name));

    // 3. If value is false, then
    if (!value) {
        // a. If S is false, return undefined; otherwise throw a ReferenceError exception.
        if (!strict) return .undefined;
        return agent.throwException(
            .reference_error,
            try std.fmt.allocPrint(agent.gc_allocator, "'{s}' is not defined", .{name}),
        );
    }

    // 4. Return ? Get(bindingObject, N).
    return self.binding_object.get(PropertyKey.from(name));
}

/// 9.1.1.2.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-object-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}

/// 9.1.1.2.10 WithBaseObject ( )
/// https://tc39.es/ecma262/#sec-object-environment-records-withbaseobject
pub fn withBaseObject(self: Self) ?Object {
    // 1. If envRec.[[IsWithEnvironment]] is true, return envRec.[[BindingObject]].
    if (self.is_with_environment) return self.binding_object;

    // 2. Otherwise, return undefined.
    return null;
}
