//! 6.1.7 The Object Type
//! https://tc39.es/ecma262/#sec-object-type

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const spec = @import("../spec.zig");

const Agent = execution.Agent;
const PreferredType = Value.PreferredType;
const PropertyDescriptor = spec.PropertyDescriptor;
const Realm = execution.Realm;
const Value = @import("value.zig").Value;

pub const Data = @import("Object/Data.zig");
pub const Factory = @import("Object/Factory.zig").Factory;
pub const InternalMethods = @import("Object/InternalMethods.zig");
pub const PropertyKey = @import("Object/PropertyKey.zig").PropertyKey;
pub const PropertyStorage = @import("Object/PropertyStorage.zig");

const Self = @This();

ptr: *anyopaque,
data: *Data,

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = self;
    _ = fmt;
    _ = options;
    // TODO: Print the actual object type.
    try writer.writeAll("[object Object]");
}

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
    const message = try std.fmt.allocPrint(
        self.agent().allocator,
        "Could not convert object to {s}",
        .{@tagName(hint)},
    );
    return self.agent().throwException(.type_error, message);
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
        return self.agent().throwException(.type_error, "Could not set property");

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

/// 7.3.9 DefinePropertyOrThrow ( O, P, desc )
/// https://tc39.es/ecma262/#sec-definepropertyorthrow
pub fn definePropertyOrThrow(self: Self, property_key: PropertyKey, property_descriptor: PropertyDescriptor) !void {
    // 1. Let success be ? O.[[DefineOwnProperty]](P, desc).
    const success = try self.internalMethods().defineOwnProperty(self, property_key, property_descriptor);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent().throwException(.type_error, "Could not define property");

    // 3. Return unused.
}

/// 7.3.10 DeletePropertyOrThrow ( O, P )
/// https://tc39.es/ecma262/#sec-deletepropertyorthrow
pub fn deletePropertyOrThrow(self: Self, property_key: PropertyKey) !void {
    // 1. Let success be ? O.[[Delete]](P).
    const success = try self.internalMethods().delete(self, property_key);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent().throwException(.type_error, "Could not delete property");

    // 3. Return unused.
}

/// 7.3.15 Construct ( F [ , argumentsList [ , newTarget ] ] )
/// https://tc39.es/ecma262/#sec-construct
pub fn construct(self: Self, args: struct { arguments_list: []const Value = &[_]Value{}, new_target: ?Self = null }) !Self {
    // 1. If newTarget is not present, set newTarget to F.
    const new_target = args.new_target orelse self;

    // 2. If argumentsList is not present, set argumentsList to a new empty List.
    const arguments_list = args.arguments_list;

    // 3. Return ? F.[[Construct]](argumentsList, newTarget).
    return self.internalMethods().construct.?(self, arguments_list, new_target);
}

/// 7.3.25 GetFunctionRealm ( obj )
/// https://tc39.es/ecma262/#sec-getfunctionrealm
pub fn getFunctionRealm(self: Self) !*Realm {
    // 1. If obj has a [[Realm]] internal slot, then
    if (self.internalMethods().call != null) {
        // a. Return obj.[[Realm]].
        return self.as(builtins.BuiltinFunction).fields.realm;
    }

    // TODO: 2. If obj is a bound function exotic object, then
    //     a. Let boundTargetFunction be obj.[[BoundTargetFunction]].
    //     b. Return ? GetFunctionRealm(boundTargetFunction).

    // TODO: 3. If obj is a Proxy exotic object, then
    //     a. Perform ? ValidateNonRevokedProxy(obj).
    //     b. Let proxyTarget be obj.[[ProxyTarget]].
    //     c. Return ? GetFunctionRealm(proxyTarget).

    // 4. Return the current Realm Record.
    return self.agent().currentRealm();
}

test "format" {
    var agent_ = try Agent.init();
    const object = (try builtins.Object.create(&agent_, .{
        .prototype = null,
    })).object();
    const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{object});
    defer std.testing.allocator.free(string);
    try std.testing.expectEqualStrings(string, "[object Object]");
}
