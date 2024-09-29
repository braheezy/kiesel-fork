//! 6.1.7.2 Object Internal Methods and Internal Slots
//! https://tc39.es/ecma262/#sec-object-internal-methods-and-internal-slots

const std = @import("std");

const builtins = @import("../../../builtins.zig");
const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Value = types.Value;

const InternalMethods = @This();

/// [[GetPrototypeOf]]
getPrototypeOf: *const fn (
    object: Object,
) Agent.Error!?Object = builtins.ordinary.internal_methods.getPrototypeOf,

/// [[SetPrototypeOf]]
setPrototypeOf: *const fn (
    object: Object,
    prototype: ?Object,
) Agent.Error!bool = builtins.ordinary.internal_methods.setPrototypeOf,

/// [[IsExtensible]]
isExtensible: *const fn (
    object: Object,
) Agent.Error!bool = builtins.ordinary.internal_methods.isExtensible,

/// [[PreventExtensions]]
preventExtensions: *const fn (
    object: Object,
) Agent.Error!bool = builtins.ordinary.internal_methods.preventExtensions,

/// [[GetOwnProperty]]
getOwnProperty: *const fn (
    object: Object,
    property_key: PropertyKey,
) Agent.Error!?PropertyDescriptor = builtins.ordinary.internal_methods.getOwnProperty,

/// [[DefineOwnProperty]]
defineOwnProperty: *const fn (
    object: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool = builtins.ordinary.internal_methods.defineOwnProperty,

/// [[HasProperty]]
hasProperty: *const fn (
    object: Object,
    property_key: PropertyKey,
) Agent.Error!bool = builtins.ordinary.internal_methods.hasProperty,

/// [[Get]]
get: *const fn (
    object: Object,
    property_key: PropertyKey,
    receiver: Value,
) Agent.Error!Value = builtins.ordinary.internal_methods.get,

/// [[Set]]
set: *const fn (
    object: Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool = builtins.ordinary.internal_methods.set,

/// [[Delete]]
delete: *const fn (
    object: Object,
    property_key: PropertyKey,
) Agent.Error!bool = builtins.ordinary.internal_methods.delete,

// [[OwnPropertyKeys]]
ownPropertyKeys: *const fn (
    object: Object,
) Agent.Error!std.ArrayList(PropertyKey) = builtins.ordinary.internal_methods.ownPropertyKeys,

// [[Call]]
call: ?*const fn (
    object: Object,
    this_value: Value,
    arguments: Arguments,
) Agent.Error!Value = null,

// [[Construct]]
construct: ?*const fn (
    object: Object,
    arguments: Arguments,
    new_target: Object,
) Agent.Error!Object = null,

pub fn create(
    allocator: std.mem.Allocator,
    initial: *const InternalMethods,
    to_insert: *const InternalMethods,
) std.mem.Allocator.Error!*const InternalMethods {
    const ordinary: *const InternalMethods = &.{};
    if (initial == ordinary) {
        return to_insert;
    }
    const methods = try allocator.create(InternalMethods);
    methods.* = initial.*;
    inline for (comptime std.meta.fieldNames(InternalMethods)) |field_name| {
        if (@field(to_insert, field_name) != @field(ordinary, field_name)) {
            @field(methods, field_name) = @field(to_insert, field_name);
        }
    }
    return methods;
}
