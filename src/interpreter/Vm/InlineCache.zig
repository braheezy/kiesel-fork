const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

const InlineCache = @This();

shape: ?*Object.Shape,
offset: Object.Shape.Property.Offset,
type: Object.Shape.Property.Type,

pub fn get(
    ic: *const InlineCache,
    agent: *Agent,
    base_object: *Object,
    base_value: Value,
) Agent.Error!?Value {
    const shape = ic.shape orelse return null;
    if (base_object.shape != shape) return null;

    const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
        .ordinary_get,
        .ordinary_get_own_property,
    }));
    // Assert IC update invariants are uphold
    std.debug.assert(has_ordinary_internal_methods);
    std.debug.assert(!base_object.shape.isUnique());

    switch (ic.type) {
        .value => return base_object.getValueAtPropertyOffset(ic.offset),
        .accessor => {
            const getter_value = base_object.getValueAtPropertyOffset(ic.offset);
            if (getter_value.isNull()) {
                @branchHint(.unlikely);
                return .undefined;
            }
            std.debug.assert(getter_value.isObject());
            return try getter_value.callAssumeCallable(agent, base_value, &.{});
        },
    }
}

pub fn set(
    ic: *const InlineCache,
    agent: *Agent,
    base_object: *Object,
    base_value: Value,
    value: Value,
) Agent.Error!bool {
    const shape = ic.shape orelse return false;
    if (base_object.shape != shape) return false;

    const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
        .ordinary_set,
        .ordinary_get_own_property,
    }));
    // Assert IC update invariants are uphold
    std.debug.assert(has_ordinary_internal_methods);
    std.debug.assert(!base_object.shape.isUnique());

    switch (ic.type) {
        .value => {
            base_object.setValueAtPropertyOffset(ic.offset, value);
            return true;
        },
        .accessor => {
            const setter_value = base_object.getValueAtPropertyOffset(
                @enumFromInt(@intFromEnum(ic.offset) + 1),
            );
            if (setter_value.isNull()) {
                @branchHint(.unlikely);
                return false;
            }
            std.debug.assert(setter_value.isObject());
            _ = try setter_value.callAssumeCallable(agent, base_value, &.{value});
            return true;
        },
    }
}

pub fn update(
    ic: *InlineCache,
    base_object: *Object,
    property_key: PropertyKey,
    comptime kind: enum { get, set },
) void {
    const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
        switch (kind) {
            .get => .ordinary_get,
            .set => .ordinary_set,
        },
        .ordinary_get_own_property,
    }));
    if (!has_ordinary_internal_methods) return;
    if (base_object.shape.isUnique()) return;

    if (base_object.shape.properties.get(property_key)) |property| {
        if (kind == .set and property.type == .value and !property.attributes.writable) return;
        ic.* = .{
            .shape = base_object.shape,
            .offset = property.offset,
            .type = property.type,
        };
    }
}
