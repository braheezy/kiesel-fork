//! 6.2.6 The Property Descriptor Specification Type
//! https://tc39.es/ecma262/#sec-property-descriptor-specification-type

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const language = @import("../language.zig");

const Agent = execution.Agent;
const Object = language.Object;
const PropertyKey = language.Object.PropertyKey;
const Value = language.Value;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

const PropertyDescriptor = @This();

/// [[Value]]
value: ?Value = null,

/// [[Writable]]
writable: ?bool = null,

/// [[Get]]
get: ??*Object = null,

/// [[Set]]
set: ??*Object = null,

/// [[Enumerable]]
enumerable: ?bool = null,

/// [[Configurable]]
configurable: ?bool = null,

/// 6.2.6.1 IsAccessorDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isaccessordescriptor
pub fn isAccessorDescriptor(self: PropertyDescriptor) bool {
    // 1. If Desc is undefined, return false.
    // 2. If Desc has a [[Get]] field, return true.
    // 3. If Desc has a [[Set]] field, return true.
    // 4. Return false.
    return self.get != null or self.set != null;
}

/// 6.2.6.2 IsDataDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isdatadescriptor
pub fn isDataDescriptor(self: PropertyDescriptor) bool {
    // 1. If Desc is undefined, return false.
    // 2. If Desc has a [[Value]] field, return true.
    // 3. If Desc has a [[Writable]] field, return true.
    // 4. Return false.
    return self.value != null or self.writable != null;
}

/// 6.2.6.3 IsGenericDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isgenericdescriptor
pub fn isGenericDescriptor(self: PropertyDescriptor) bool {
    // 1. If Desc is undefined, return false.
    // 2. If IsAccessorDescriptor(Desc) is true, return false.
    // 3. If IsDataDescriptor(Desc) is true, return false.
    // 4. Return true.
    return !(self.isAccessorDescriptor() or self.isDataDescriptor());
}

/// 6.2.6.4 FromPropertyDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-frompropertydescriptor
pub fn fromPropertyDescriptor(
    self: PropertyDescriptor,
    agent: *Agent,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If Desc is undefined, return undefined.

    // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    // 3. Assert: obj is an extensible ordinary object with no own properties.
    const object = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

    // 4. If Desc has a [[Value]] field, then
    if (self.value) |value| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "value", Desc.[[Value]]).
        try object.createDataPropertyDirect(
            PropertyKey.from("value"),
            value,
        );
    }

    // 5. If Desc has a [[Writable]] field, then
    if (self.writable) |writable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "writable", Desc.[[Writable]]).
        try object.createDataPropertyDirect(
            PropertyKey.from("writable"),
            Value.from(writable),
        );
    }

    // 6. If Desc has a [[Get]] field, then
    if (self.get) |get| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "get", Desc.[[Get]]).
        try object.createDataPropertyDirect(
            PropertyKey.from("get"),
            if (get) |o| Value.from(o) else .undefined,
        );
    }

    // 7. If Desc has a [[Set]] field, then
    if (self.set) |set| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "set", Desc.[[Set]]).
        try object.createDataPropertyDirect(
            PropertyKey.from("set"),
            if (set) |o| Value.from(o) else .undefined,
        );
    }

    // 8. If Desc has an [[Enumerable]] field, then
    if (self.enumerable) |enumerable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "enumerable", Desc.[[Enumerable]]).
        try object.createDataPropertyDirect(
            PropertyKey.from("enumerable"),
            Value.from(enumerable),
        );
    }

    // 9. If Desc has a [[Configurable]] field, then
    if (self.configurable) |configurable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "configurable", Desc.[[Configurable]]).
        try object.createDataPropertyDirect(
            PropertyKey.from("configurable"),
            Value.from(configurable),
        );
    }

    // 10. Return obj.
    return object;
}

/// 6.2.6.6 CompletePropertyDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-completepropertydescriptor
pub fn completePropertyDescriptor(self: *PropertyDescriptor) void {
    // 1. Let like be the Record {
    //      [[Value]]: undefined,
    //      [[Writable]]: false,
    //      [[Get]]: undefined,
    //      [[Set]]: undefined,
    //      [[Enumerable]]: false,
    //      [[Configurable]]: false
    //    }.
    const like: PropertyDescriptor = .{
        .value = .undefined,
        .writable = false,
        .enumerable = false,
        .configurable = false,
    };

    // 2. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
    if (self.isGenericDescriptor() or self.isDataDescriptor()) {
        // a. If Desc does not have a [[Value]] field, set Desc.[[Value]] to like.[[Value]].
        if (self.value == null) self.value = like.value;

        // b. If Desc does not have a [[Writable]] field, set Desc.[[Writable]] to like.[[Writable]].
        if (self.writable == null) self.writable = like.writable;
    } else {
        // 3. Else,
        // a. If Desc does not have a [[Get]] field, set Desc.[[Get]] to like.[[Get]].
        // b. If Desc does not have a [[Set]] field, set Desc.[[Set]] to like.[[Set]].
        // NOTE: These are no-ops, the fields can't be missing.
    }

    // 4. If Desc does not have an [[Enumerable]] field, set Desc.[[Enumerable]] to like.[[Enumerable]].
    if (self.enumerable == null) self.enumerable = like.enumerable;

    // 5. If Desc does not have a [[Configurable]] field, set Desc.[[Configurable]] to like.[[Configurable]].
    if (self.configurable == null) self.configurable = like.configurable;

    // 6. Return unused.
}

pub fn isFullyPopulated(self: PropertyDescriptor) bool {
    return ((self.value != null and self.writable != null) or
        (self.get != null or self.set != null)) and
        self.enumerable != null and
        self.configurable != null;
}

pub fn hasFields(self: PropertyDescriptor) bool {
    return self.value != null or
        self.writable != null or
        self.get != null or
        self.set != null or
        self.enumerable != null or
        self.configurable != null;
}

test "isAccessorDescriptor" {
    const gc = @import("../../gc.zig");
    var agent = try Agent.init(gc.allocator(), .{});
    defer agent.deinit();
    const getter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    const setter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    try std.testing.expect((PropertyDescriptor{ .get = getter }).isAccessorDescriptor());
    try std.testing.expect((PropertyDescriptor{ .set = setter }).isAccessorDescriptor());
    try std.testing.expect((PropertyDescriptor{ .get = getter, .set = setter }).isAccessorDescriptor());
    try std.testing.expect(!(PropertyDescriptor{ .value = .undefined }).isAccessorDescriptor());
    try std.testing.expect(!(PropertyDescriptor{}).isAccessorDescriptor());
}

test "isDataDescriptor" {
    try std.testing.expect((PropertyDescriptor{ .value = .undefined }).isDataDescriptor());
    try std.testing.expect((PropertyDescriptor{ .writable = true }).isDataDescriptor());
    try std.testing.expect(!(PropertyDescriptor{ .writable = null }).isDataDescriptor());
    try std.testing.expect(!(PropertyDescriptor{}).isDataDescriptor());
}

test "isGenericDescriptor" {
    const gc = @import("../../gc.zig");
    var agent = try Agent.init(gc.allocator(), .{});
    defer agent.deinit();
    const setter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    try std.testing.expect((PropertyDescriptor{ .writable = null }).isGenericDescriptor());
    try std.testing.expect(!(PropertyDescriptor{ .set = setter }).isGenericDescriptor());
    try std.testing.expect((PropertyDescriptor{}).isGenericDescriptor());
}
