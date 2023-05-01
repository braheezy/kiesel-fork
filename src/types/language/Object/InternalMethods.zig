//! 6.1.7.2 Object Internal Methods and Internal Slots
//! https://tc39.es/ecma262/#sec-object-internal-methods-and-internal-slots

const std = @import("std");

const builtins = @import("../../../builtins.zig");
const execution = @import("../../../execution.zig");
const spec = @import("../../spec.zig");

const Agent = execution.Agent;
const Object = @import("../Object.zig");
const PropertyDescriptor = spec.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Value = @import("../value.zig").Value;

/// [[GetPrototypeOf]]
getPrototypeOf: *const fn (
    object: Object,
) Agent.Error!?Object = builtins.ordinary.internalGetPrototypeOf,

/// [[SetPrototypeOf]]
setPrototypeOf: *const fn (
    object: Object,
    prototype: ?Object,
) Agent.Error!bool = builtins.ordinary.internalSetPrototypeOf,

/// [[IsExtensible]]
isExtensible: *const fn (
    object: Object,
) Agent.Error!bool = builtins.ordinary.internalIsExtensible,

/// [[PreventExtensions]]
preventExtensions: *const fn (
    object: Object,
) Agent.Error!bool = builtins.ordinary.internalPreventExtensions,

/// [[GetOwnProperty]]
getOwnProperty: *const fn (
    object: Object,
    property_key: PropertyKey,
) Agent.Error!?PropertyDescriptor = builtins.ordinary.internalGetOwnProperty,

/// [[DefineOwnProperty]]
defineOwnProperty: *const fn (
    object: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool = builtins.ordinary.internalDefineOwnProperty,

/// [[HasProperty]]
hasProperty: *const fn (
    object: Object,
    property_key: PropertyKey,
) Agent.Error!bool = builtins.ordinary.internalHasProperty,

/// [[Get]]
get: *const fn (
    object: Object,
    property_key: PropertyKey,
    receiver: Value,
) Agent.Error!Value = builtins.ordinary.internalGet,

/// [[Set]]
set: *const fn (
    object: Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool = builtins.ordinary.internalSet,

/// [[Delete]]
delete: *const fn (
    object: Object,
    property_key: PropertyKey,
) Agent.Error!bool = builtins.ordinary.internalDelete,

// [[OwnPropertyKeys]]
ownPropertyKeys: *const fn (
    object: Object,
) Agent.Error!std.ArrayList(PropertyKey) = builtins.ordinary.internalOwnPropertyKeys,

// [[Call]]
call: ?*const fn (
    object: Object,
    this_value: Value,
    arguments: []const Value,
) Agent.Error!Value = null,

// [[Construct]]
construct: ?*const fn (
    object: Object,
    arguments: []const Value,
    new_target: Object,
) Agent.Error!Object = null,
