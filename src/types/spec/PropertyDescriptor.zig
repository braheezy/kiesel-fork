//! 6.2.6 The Property Descriptor Specification Type
//! https://tc39.es/ecma262/#sec-property-descriptor-specification-type

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const language = @import("../language.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Object = language.Object;
const PropertyKey = language.Object.PropertyKey;
const Value = language.Value;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

const Self = @This();

/// [[Value]]
value: ?Value = null,

/// [[Writable]]
writable: ?bool = null,

/// [[Get]]
get: ?Object = null,

/// [[Set]]
set: ?Object = null,

/// [[Enumerable]]
enumerable: ?bool = null,

/// [[Configurable]]
configurable: ?bool = null,

/// 6.2.6.1 IsAccessorDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isaccessordescriptor
pub inline fn isAccessorDescriptor(self: Self) bool {
    // 1. If Desc is undefined, return false.
    // 2. If Desc has a [[Get]] field, return true.
    // 3. If Desc has a [[Set]] field, return true.
    // 4. Return false.
    return self.get != null or self.set != null;
}

/// 6.2.6.2 IsDataDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isdatadescriptor
pub inline fn isDataDescriptor(self: Self) bool {
    // 1. If Desc is undefined, return false.
    // 2. If Desc has a [[Value]] field, return true.
    // 3. If Desc has a [[Writable]] field, return true.
    // 4. Return false.
    return self.value != null or self.writable != null;
}

/// 6.2.6.3 IsGenericDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isgenericdescriptor
pub inline fn isGenericDescriptor(self: Self) bool {
    // 1. If Desc is undefined, return false.
    // 2. If IsAccessorDescriptor(Desc) is true, return false.
    // 3. If IsDataDescriptor(Desc) is true, return false.
    // 4. Return true.
    return !(self.isAccessorDescriptor() or self.isDataDescriptor());
}

/// 6.2.6.4 FromPropertyDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-frompropertydescriptor
pub fn fromPropertyDescriptor(self: Self, agent: *Agent) !Object {
    const realm = agent.currentRealm();

    // 1. If Desc is undefined, return undefined.

    // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    // 3. Assert: obj is an extensible ordinary object with no own properties.
    const object = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

    // 4. If Desc has a [[Value]] field, then
    if (self.value) |value| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "value", Desc.[[Value]]).
        object.createDataPropertyOrThrow(
            PropertyKey.from("value"),
            value,
        ) catch |err| try noexcept(err);
    }

    // 5. If Desc has a [[Writable]] field, then
    if (self.writable) |writable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "writable", Desc.[[Writable]]).
        object.createDataPropertyOrThrow(
            PropertyKey.from("writable"),
            Value.from(writable),
        ) catch |err| try noexcept(err);
    }

    // 6. If Desc has a [[Get]] field, then
    if (self.get) |get| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "get", Desc.[[Get]]).
        object.createDataPropertyOrThrow(
            PropertyKey.from("get"),
            Value.from(get),
        ) catch |err| try noexcept(err);
    }

    // 7. If Desc has a [[Set]] field, then
    if (self.set) |set| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "set", Desc.[[Set]]).
        object.createDataPropertyOrThrow(
            PropertyKey.from("set"),
            Value.from(set),
        ) catch |err| try noexcept(err);
    }

    // 8. If Desc has an [[Enumerable]] field, then
    if (self.enumerable) |enumerable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "enumerable", Desc.[[Enumerable]]).
        object.createDataPropertyOrThrow(
            PropertyKey.from("enumerable"),
            Value.from(enumerable),
        ) catch |err| try noexcept(err);
    }

    // 9. If Desc has a [[Configurable]] field, then
    if (self.configurable) |configurable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "configurable", Desc.[[Configurable]]).
        object.createDataPropertyOrThrow(
            PropertyKey.from("configurable"),
            Value.from(configurable),
        ) catch |err| try noexcept(err);
    }

    // 10. Return obj.
    return object;
}

pub inline fn isFullyPopulated(self: Self) bool {
    return ((self.value != null and self.writable != null) or
        (self.get != null or self.set != null)) and
        self.enumerable != null and
        self.configurable != null;
}

pub inline fn hasFields(self: Self) bool {
    return self.value != null or
        self.writable != null or
        self.get != null or
        self.set != null or
        self.enumerable != null or
        self.configurable != null;
}

test "isAccessorDescriptor" {
    var agent = try Agent.init(.{});
    defer agent.deinit();
    const getter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    const setter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    try std.testing.expect((Self{ .get = getter }).isAccessorDescriptor());
    try std.testing.expect((Self{ .set = setter }).isAccessorDescriptor());
    try std.testing.expect((Self{ .get = getter, .set = setter }).isAccessorDescriptor());
    try std.testing.expect(!(Self{ .value = .undefined }).isAccessorDescriptor());
    try std.testing.expect(!(Self{}).isAccessorDescriptor());
}

test "isDataDescriptor" {
    try std.testing.expect((Self{ .value = .undefined }).isDataDescriptor());
    try std.testing.expect((Self{ .writable = true }).isDataDescriptor());
    try std.testing.expect(!(Self{ .writable = null }).isDataDescriptor());
    try std.testing.expect(!(Self{}).isDataDescriptor());
}

test "isGenericDescriptor" {
    var agent = try Agent.init(.{});
    defer agent.deinit();
    const setter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    try std.testing.expect((Self{ .writable = null }).isGenericDescriptor());
    try std.testing.expect(!(Self{ .set = setter }).isGenericDescriptor());
    try std.testing.expect((Self{}).isGenericDescriptor());
}
