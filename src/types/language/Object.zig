//! 6.1.7 The Object Type
//! https://tc39.es/ecma262/#sec-object-type

const std = @import("std");

const execution = @import("../../execution.zig");
const spec = @import("../spec.zig");

const Agent = execution.Agent;
const PreferredType = Value.PreferredType;
const PropertyDescriptor = spec.PropertyDescriptor;
const Value = @import("value.zig").Value;

pub const Data = @import("Object/Data.zig");
pub const Factory = @import("Object/Factory.zig").Factory;
pub const InternalMethods = @import("Object/InternalMethods.zig");
pub const PropertyKey = @import("Object/PropertyKey.zig").PropertyKey;
pub const PropertyStorage = @import("Object/PropertyStorage.zig");

const Self = @This();

ptr: *anyopaque,
data: *Data,

pub fn as(self: Self, comptime T: type) *T {
    return @ptrCast(*T, @alignCast(@alignOf(T), self.ptr));
}

// Helper functions so we don't have to say 'data' all the time

pub fn prototype(self: Self) *?Self {
    return &self.data.prototype;
}

pub fn extensible(self: Self) *bool {
    return &self.data.extensible;
}

pub fn agent(self: Self) *Agent {
    return self.data.agent;
}

pub fn internalMethods(self: Self) *InternalMethods {
    return &self.data.internal_methods;
}

pub fn propertyStorage(self: Self) *PropertyStorage {
    return &self.data.property_storage;
}

/// 7.1.1.1 OrdinaryToPrimitive ( O, hint )
/// https://tc39.es/ecma262/#sec-ordinarytoprimitive
pub fn ordinaryToPrimitive(self: Self, hint: PreferredType) !Value {
    const method_names = switch (hint) {
        // 1. If hint is string, then
        //     a. Let methodNames be « "toString", "valueOf" ».
        .string => [_][]const u8{ "toString", "valueOf" },
        // 2. Else,
        //     a. Let methodNames be « "valueOf", "toString" ».
        else => [_][]const u8{ "valueOf", "toString" },
    };

    // 3. For each element name of methodNames, do
    for (method_names) |name| {
        // a. Let method be ? Get(O, name).
        const method = try self.get(PropertyKey.fromString(name));

        // b. If IsCallable(method) is true, then
        if (method.isCallable()) {
            // i. Let result be ? Call(method, O).
            const result = try method.callAssumeCallableNoArgs(Value.fromObject(self));

            // ii. If result is not an Object, return result.
            if (result != .object)
                return result;
        }
    }

    // 4. Throw a TypeError exception.
    return error.ExceptionThrown;
}

/// 7.2.5 IsExtensible ( O )
/// https://tc39.es/ecma262/#sec-isextensible-o
pub fn isExtensible(self: Self) !bool {
    // 1. Return ? O.[[IsExtensible]]().
    return self.internalMethods().isExtensible(self);
}

/// 7.3.2 Get ( O, P )
/// https://tc39.es/ecma262/#sec-get-o-p
pub fn get(self: Self, property_key: PropertyKey) !Value {
    // 1. Return ? O.[[Get]](P, O).
    return self.internalMethods().get(self, property_key, Value.fromObject(self));
}

/// 7.3.4 Set ( O, P, V, Throw )
/// https://tc39.es/ecma262/#sec-set-o-p-v-throw
pub fn set(self: Self, property_key: PropertyKey, value: Value, throw: enum { throw, ignore }) !void {
    // 1. Let success be ? O.[[Set]](P, V, O).
    const success = try self.internalMethods().set(self, property_key, value, Value.fromObject(self));

    // 2. If success is false and Throw is true, throw a TypeError exception.
    if (!success and throw == .throw)
        return error.ExceptionThrown;

    // 3. Return unused.
}

/// 7.3.5 CreateDataProperty ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdataproperty
pub fn createDataProperty(self: Self, property_key: PropertyKey, value: Value) !bool {
    // 1. Let newDesc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }.
    const new_descriptor = PropertyDescriptor{ .value = value, .writable = true, .enumerable = true, .configurable = true };

    // 2. Return ? O.[[DefineOwnProperty]](P, newDesc).
    return self.internalMethods().defineOwnProperty(self, property_key, new_descriptor);
}
