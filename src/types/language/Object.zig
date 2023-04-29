//! 6.1.7 The Object Type
//! https://tc39.es/ecma262/#sec-object-type

const std = @import("std");

const execution = @import("../../execution.zig");

const Agent = execution.Agent;
const Value = @import("value.zig").Value;

pub const Data = @import("Object/Data.zig");
pub const Factory = @import("Object/Factory.zig").Factory;
pub const InternalMethods = @import("Object/InternalMethods.zig");
pub const PropertyKey = @import("Object/PropertyKey.zig").PropertyKey;
pub const PropertyStorage = @import("Object/PropertyStorage.zig");

const Object = @This();

ptr: *anyopaque,
data: *Object.Data,

pub fn as(object: Object, comptime T: type) *T {
    return @ptrCast(*T, @alignCast(@alignOf(T), object.ptr));
}

// Helper functions so we don't have to say 'data' all the time

pub fn prototype(self: Object) *?Object {
    return &self.data.prototype;
}

pub fn extensible(self: Object) *bool {
    return &self.data.extensible;
}

pub fn agent(self: Object) *Agent {
    return self.data.agent;
}

pub fn internalMethods(self: Object) *InternalMethods {
    return &self.data.internal_methods;
}

pub fn propertyStorage(self: Object) *PropertyStorage {
    return &self.data.property_storage;
}

/// 7.2.5 IsExtensible ( O )
/// https://tc39.es/ecma262/#sec-isextensible-o
pub fn isExtensible(object: Object) !bool {
    // 1. Return ? O.[[IsExtensible]]().
    return object.internalMethods().isExtensible(object);
}

/// 7.3.2 Get ( O, P )
/// https://tc39.es/ecma262/#sec-get-o-p
pub fn get(object: Object, property_key: PropertyKey) !Value {
    // 1. Return ? O.[[Get]](P, O).
    return object.internalMethods().get(object, property_key, Value.fromObject(object));
}
