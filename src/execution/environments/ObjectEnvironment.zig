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
const String = types.String;
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
pub fn hasBinding(self: Self, name: []const u8) Agent.Error!bool {
    const agent = self.binding_object.agent();
    const property_key = PropertyKey.from(try String.fromUtf8(agent.gc_allocator, name));

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let foundBinding be ? HasProperty(bindingObject, N).
    const found_binding = try self.binding_object.hasProperty(property_key);

    // 3. If foundBinding is false, return false.
    if (!found_binding) return false;

    // 4. If envRec.[[IsWithEnvironment]] is false, return true.
    if (!self.is_with_environment) return true;

    // 5. Let unscopables be ? Get(bindingObject, %Symbol.unscopables%).
    const unscopables = try self.binding_object.get(
        PropertyKey.from(agent.well_known_symbols.@"%Symbol.unscopables%"),
    );

    // 6. If unscopables is an Object, then
    if (unscopables == .object) {
        // a. Let blocked be ToBoolean(? Get(unscopables, N)).
        const blocked = (try unscopables.object.get(property_key)).toBoolean();

        // b. If blocked is true, return false.
        if (blocked) return false;
    }

    // 7. Return true.
    return true;
}

/// 9.1.1.2.2 CreateMutableBinding ( N, D )
/// https://tc39.es/ecma262/#sec-object-environment-records-createmutablebinding-n-d
pub fn createMutableBinding(
    self: Self,
    agent: *Agent,
    name: []const u8,
    deletable: bool,
) Agent.Error!void {
    const property_key = PropertyKey.from(try String.fromUtf8(agent.gc_allocator, name));

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Perform ? DefinePropertyOrThrow(bindingObject, N, PropertyDescriptor {
    //      [[Value]]: undefined, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: D
    //    }).
    try self.binding_object.definePropertyOrThrow(property_key, .{
        .value = .undefined,
        .writable = true,
        .enumerable = true,
        .configurable = deletable,
    });

    // 3. Return unused.
}

/// 9.1.1.2.3 CreateImmutableBinding ( N, S )
/// https://tc39.es/ecma262/#sec-object-environment-records-createimmutablebinding-n-s
pub fn createImmutableBinding(_: Self, _: *Agent, _: []const u8, _: bool) noreturn {
    // The CreateImmutableBinding concrete method of an Object Environment Record is never used
    // within this specification.
    @panic("ObjectEnvironment.createImmutableBinding() must not be called");
}

/// 9.1.1.2.4 InitializeBinding ( N, V )
/// https://tc39.es/ecma262/#sec-object-environment-records-initializebinding-n-v
pub fn initializeBinding(
    self: Self,
    agent: *Agent,
    name: []const u8,
    value: Value,
) Agent.Error!void {
    // 1. Perform ? envRec.SetMutableBinding(N, V, false).
    try self.setMutableBinding(agent, name, value, false);

    // 2. Return unused.
}

/// 9.1.1.2.5 SetMutableBinding ( N, V, S )
/// https://tc39.es/ecma262/#sec-object-environment-records-setmutablebinding-n-v-s
pub fn setMutableBinding(
    self: Self,
    agent: *Agent,
    name: []const u8,
    value: Value,
    strict: bool,
) Agent.Error!void {
    const property_key = PropertyKey.from(try String.fromUtf8(agent.gc_allocator, name));

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let stillExists be ? HasProperty(bindingObject, N).
    const still_exists = try self.binding_object.hasProperty(property_key);

    // 3. If stillExists is false and S is true, throw a ReferenceError exception.
    if (!still_exists and strict) {
        return agent.throwException(.reference_error, "'{s}' is not defined", .{name});
    }

    // 4. Perform ? Set(bindingObject, N, V, S).
    try self.binding_object.set(property_key, value, if (strict) .throw else .ignore);

    // 5. Return unused.
}

/// 9.1.1.2.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-object-environment-records-getbindingvalue-n-s
pub fn getBindingValue(
    self: Self,
    agent: *Agent,
    name: []const u8,
    strict: bool,
) Agent.Error!Value {
    const property_key = PropertyKey.from(try String.fromUtf8(agent.gc_allocator, name));

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let value be ? HasProperty(bindingObject, N).
    const value = try self.binding_object.hasProperty(property_key);

    // 3. If value is false, then
    if (!value) {
        // a. If S is false, return undefined; otherwise throw a ReferenceError exception.
        if (!strict) return .undefined;
        return agent.throwException(.reference_error, "'{s}' is not defined", .{name});
    }

    // 4. Return ? Get(bindingObject, N).
    return self.binding_object.get(property_key);
}

/// 9.1.1.2.7 DeleteBinding ( N )
/// https://tc39.es/ecma262/#sec-object-environment-records-deletebinding-n
pub fn deleteBinding(self: Self, name: []const u8) Agent.Error!bool {
    const agent = self.binding_object.agent();
    const property_key = PropertyKey.from(try String.fromUtf8(agent.gc_allocator, name));

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Return ? bindingObject.[[Delete]](N).
    return self.binding_object.internalMethods().delete(self.binding_object, property_key);
}

/// 9.1.1.2.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-object-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}

/// 9.1.1.2.9 HasSuperBinding ( )
/// https://tc39.es/ecma262/#sec-object-environment-records-hassuperbinding
pub fn hasSuperBinding(_: Self) bool {
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
