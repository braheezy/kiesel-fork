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

pub const GetPrototypeOf = *const fn (agent: *Agent, object: *Object) Agent.Error!?*Object;
pub const SetPrototypeOf = *const fn (agent: *Agent, object: *Object, prototype: ?*Object) Agent.Error!bool;
pub const IsExtensible = *const fn (agent: *Agent, object: *Object) Agent.Error!bool;
pub const PreventExtensions = *const fn (agent: *Agent, object: *Object) Agent.Error!bool;
pub const GetOwnProperty = *const fn (agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!?PropertyDescriptor;
pub const DefineOwnProperty = *const fn (agent: *Agent, object: *Object, property_key: PropertyKey, property_descriptor: PropertyDescriptor) Agent.Error!bool;
pub const HasProperty = *const fn (agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool;
pub const Get = *const fn (agent: *Agent, object: *Object, property_key: PropertyKey, receiver: Value) Agent.Error!Value;
pub const Set = *const fn (agent: *Agent, object: *Object, property_key: PropertyKey, value: Value, receiver: Value) Agent.Error!bool;
pub const Delete = *const fn (agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool;
pub const OwnPropertyKeys = *const fn (agent: *Agent, object: *Object) Agent.Error![]PropertyKey;
pub const Call = *const fn (agent: *Agent, object: *Object, this_value: Value, arguments: Arguments) Agent.Error!Value;
pub const Construct = *const fn (agent: *Agent, object: *Object, arguments: Arguments, new_target: *Object) Agent.Error!*Object;

/// [[GetPrototypeOf]]
getPrototypeOf: GetPrototypeOf,

/// [[SetPrototypeOf]]
setPrototypeOf: SetPrototypeOf,

/// [[IsExtensible]]
isExtensible: IsExtensible,

/// [[PreventExtensions]]
preventExtensions: PreventExtensions,

/// [[GetOwnProperty]]
getOwnProperty: GetOwnProperty,

/// [[DefineOwnProperty]]
defineOwnProperty: DefineOwnProperty,

/// [[HasProperty]]
hasProperty: HasProperty,

/// [[Get]]
get: Get,

/// [[Set]]
set: Set,

/// [[Delete]]
delete: Delete,

/// [[OwnPropertyKeys]]
ownPropertyKeys: OwnPropertyKeys,

/// [[Call]]
call: ?Call,

/// [[Construct]]
construct: ?Construct,

pub const default: *const InternalMethods = &.{
    .getPrototypeOf = builtins.ordinary.internal_methods.getPrototypeOf,
    .setPrototypeOf = builtins.ordinary.internal_methods.setPrototypeOf,
    .isExtensible = builtins.ordinary.internal_methods.isExtensible,
    .preventExtensions = builtins.ordinary.internal_methods.preventExtensions,
    .getOwnProperty = builtins.ordinary.internal_methods.getOwnProperty,
    .defineOwnProperty = builtins.ordinary.internal_methods.defineOwnProperty,
    .hasProperty = builtins.ordinary.internal_methods.hasProperty,
    .get = builtins.ordinary.internal_methods.get,
    .set = builtins.ordinary.internal_methods.set,
    .delete = builtins.ordinary.internal_methods.delete,
    .ownPropertyKeys = builtins.ordinary.internal_methods.ownPropertyKeys,
    .call = null,
    .construct = null,
};

const Overwrites = struct {
    getPrototypeOf: ?GetPrototypeOf = null,
    setPrototypeOf: ?SetPrototypeOf = null,
    isExtensible: ?IsExtensible = null,
    preventExtensions: ?PreventExtensions = null,
    getOwnProperty: ?GetOwnProperty = null,
    defineOwnProperty: ?DefineOwnProperty = null,
    hasProperty: ?HasProperty = null,
    get: ?Get = null,
    set: ?Set = null,
    delete: ?Delete = null,
    ownPropertyKeys: ?OwnPropertyKeys = null,
    call: ?Call = null,
    construct: ?Construct = null,
};

pub fn init(
    allocator: std.mem.Allocator,
    initial: *const InternalMethods,
    overwrites: Overwrites,
) std.mem.Allocator.Error!*const InternalMethods {
    var internal_methods = try allocator.create(InternalMethods);
    internal_methods.* = initial.*;
    inline for (comptime std.meta.fieldNames(Overwrites)) |field_name| {
        if (@field(overwrites, field_name)) |internal_method| {
            @field(internal_methods, field_name) = internal_method;
        }
    }
    return internal_methods;
}

pub inline fn initComptime(comptime overwrites: Overwrites) *const InternalMethods {
    comptime {
        var internal_methods = default.*;
        for (std.meta.fieldNames(Overwrites)) |field_name| {
            if (@field(overwrites, field_name)) |internal_method| {
                @field(internal_methods, field_name) = internal_method;
            }
        }
        const final = internal_methods;
        return &final;
    }
}
