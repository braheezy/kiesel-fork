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

/// 6.2.6.1 IsAccessorDescriptor ( propertyDesc )
/// https://tc39.es/ecma262/#sec-isaccessordescriptor
pub fn isAccessorDescriptor(property_desc: *const PropertyDescriptor) bool {
    // 1. If propertyDesc has a [[Get]] field, return true.
    // 2. If propertyDesc has a [[Set]] field, return true.
    // 3. Return false.
    return property_desc.get != null or property_desc.set != null;
}

/// 6.2.6.2 IsDataDescriptor ( propertyDesc )
/// https://tc39.es/ecma262/#sec-isdatadescriptor
pub fn isDataDescriptor(property_desc: *const PropertyDescriptor) bool {
    // 1. If propertyDesc has a [[Value]] field, return true.
    // 2. If propertyDesc has a [[Writable]] field, return true.
    // 3. Return false.
    return property_desc.value != null or property_desc.writable != null;
}

/// 6.2.6.3 IsGenericDescriptor ( propertyDesc )
/// https://tc39.es/ecma262/#sec-isgenericdescriptor
pub fn isGenericDescriptor(property_desc: *const PropertyDescriptor) bool {
    // 1. If IsAccessorDescriptor(propertyDesc) is true, return false.
    // 2. If IsDataDescriptor(propertyDesc) is true, return false.
    // 3. Return true.
    return !(property_desc.isAccessorDescriptor() or property_desc.isDataDescriptor());
}

/// 6.2.6.4 FromPropertyDescriptor ( propertyDesc )
/// https://tc39.es/ecma262/#sec-frompropertydescriptor
pub fn fromPropertyDescriptor(
    property_desc: *const PropertyDescriptor,
    agent: *Agent,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If propertyDesc is undefined, return undefined.

    // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    // 3. Assert: obj is an extensible ordinary object with no own properties.
    const obj = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

    // 4. If propertyDesc has a [[Value]] field, then
    if (property_desc.value) |value| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "value", propertyDesc.[[Value]]).
        try obj.createDataPropertyDirect(
            agent,
            PropertyKey.from("value"),
            value,
        );
    }

    // 5. If propertyDesc has a [[Writable]] field, then
    if (property_desc.writable) |writable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "writable", propertyDesc.[[Writable]]).
        try obj.createDataPropertyDirect(
            agent,
            PropertyKey.from("writable"),
            Value.from(writable),
        );
    }

    // 6. If propertyDesc has a [[Get]] field, then
    if (property_desc.get) |get| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "get", propertyDesc.[[Get]]).
        try obj.createDataPropertyDirect(
            agent,
            PropertyKey.from("get"),
            if (get) |o| Value.from(o) else .undefined,
        );
    }

    // 7. If propertyDesc has a [[Set]] field, then
    if (property_desc.set) |set| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "set", propertyDesc.[[Set]]).
        try obj.createDataPropertyDirect(
            agent,
            PropertyKey.from("set"),
            if (set) |o| Value.from(o) else .undefined,
        );
    }

    // 8. If propertyDesc has an [[Enumerable]] field, then
    if (property_desc.enumerable) |enumerable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "enumerable", propertyDesc.[[Enumerable]]).
        try obj.createDataPropertyDirect(
            agent,
            PropertyKey.from("enumerable"),
            Value.from(enumerable),
        );
    }

    // 9. If propertyDesc has a [[Configurable]] field, then
    if (property_desc.configurable) |configurable| {
        // a. Perform ! CreateDataPropertyOrThrow(obj, "configurable",
        //    propertyDesc.[[Configurable]]).
        try obj.createDataPropertyDirect(
            agent,
            PropertyKey.from("configurable"),
            Value.from(configurable),
        );
    }

    // 10. Return obj.
    return obj;
}

/// 6.2.6.6 CompletePropertyDescriptor ( propertyDesc )
/// https://tc39.es/ecma262/#sec-completepropertydescriptor
pub fn completePropertyDescriptor(property_desc: *PropertyDescriptor) void {
    // 1. Let like be the Record { [[Value]]: undefined, [[Writable]]: false, [[Get]]: undefined,
    //    [[Set]]: undefined, [[Enumerable]]: false, [[Configurable]]: false }.
    const like: PropertyDescriptor = .{
        .value = .undefined,
        .writable = false,
        .enumerable = false,
        .configurable = false,
    };

    // 2. If IsGenericDescriptor(propertyDesc) is true or IsDataDescriptor(propertyDesc) is true,
    //    then
    if (property_desc.isGenericDescriptor() or property_desc.isDataDescriptor()) {
        // a. If propertyDesc does not have a [[Value]] field, set propertyDesc.[[Value]] to
        //    like.[[Value]].
        if (property_desc.value == null) property_desc.value = like.value;

        // b. If propertyDesc does not have a [[Writable]] field, set propertyDesc.[[Writable]] to
        //    like.[[Writable]].
        if (property_desc.writable == null) property_desc.writable = like.writable;
    } else {
        // 3. Else,
        // a. If propertyDesc does not have a [[Get]] field, set propertyDesc.[[Get]] to
        //    like.[[Get]].
        // b. If propertyDesc does not have a [[Set]] field, set propertyDesc.[[Set]] to
        //    like.[[Set]].
        // NOTE: These are no-ops, the fields can't be missing.
    }

    // 4. If propertyDesc does not have an [[Enumerable]] field, set propertyDesc.[[Enumerable]] to
    //    like.[[Enumerable]].
    if (property_desc.enumerable == null) property_desc.enumerable = like.enumerable;

    // 5. If propertyDesc does not have a [[Configurable]] field, set propertyDesc.[[Configurable]]
    //    to like.[[Configurable]].
    if (property_desc.configurable == null) property_desc.configurable = like.configurable;

    // 6. Return unused.
}

pub fn isFullyPopulated(property_desc: *const PropertyDescriptor) bool {
    return ((property_desc.value != null and property_desc.writable != null) or
        (property_desc.get != null or property_desc.set != null)) and
        property_desc.enumerable != null and
        property_desc.configurable != null;
}

pub fn hasFields(property_desc: *const PropertyDescriptor) bool {
    return property_desc.value != null or
        property_desc.writable != null or
        property_desc.get != null or
        property_desc.set != null or
        property_desc.enumerable != null or
        property_desc.configurable != null;
}

test isAccessorDescriptor {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var environ_map = try std.process.Environ.createMap(.empty, gpa);
    defer environ_map.deinit();
    const platform: Agent.Platform = .default(io, &environ_map);
    defer platform.deinit();
    var agent = try Agent.init(gpa, io, &platform, .{});
    defer agent.deinit();
    const getter = try ordinaryObjectCreate(&agent, null);
    const setter = try ordinaryObjectCreate(&agent, null);
    try std.testing.expect((PropertyDescriptor{ .get = getter }).isAccessorDescriptor());
    try std.testing.expect((PropertyDescriptor{ .set = setter }).isAccessorDescriptor());
    try std.testing.expect((PropertyDescriptor{ .get = getter, .set = setter }).isAccessorDescriptor());
    try std.testing.expect(!(PropertyDescriptor{ .value = .undefined }).isAccessorDescriptor());
    try std.testing.expect(!(PropertyDescriptor{}).isAccessorDescriptor());
}

test isDataDescriptor {
    try std.testing.expect((PropertyDescriptor{ .value = .undefined }).isDataDescriptor());
    try std.testing.expect((PropertyDescriptor{ .writable = true }).isDataDescriptor());
    try std.testing.expect(!(PropertyDescriptor{ .writable = null }).isDataDescriptor());
    try std.testing.expect(!(PropertyDescriptor{}).isDataDescriptor());
}

test isGenericDescriptor {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var environ_map = try std.process.Environ.createMap(.empty, gpa);
    defer environ_map.deinit();
    const platform: Agent.Platform = .default(io, &environ_map);
    defer platform.deinit();
    var agent = try Agent.init(gpa, io, &platform, .{});
    defer agent.deinit();
    const setter = try ordinaryObjectCreate(&agent, null);
    try std.testing.expect((PropertyDescriptor{ .writable = null }).isGenericDescriptor());
    try std.testing.expect(!(PropertyDescriptor{ .set = setter }).isGenericDescriptor());
    try std.testing.expect((PropertyDescriptor{}).isGenericDescriptor());
}
