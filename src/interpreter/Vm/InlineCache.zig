const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

const InlineCache = @This();

shape: ?*Object.Shape,
offset: Object.Shape.PropertyOffset,
type: Object.PropertyStorage.PropertyType,

pub fn get(
    ic: *const InlineCache,
    agent: *Agent,
    base_object: *Object,
    base_value: Value,
) Agent.Error!?Value {
    const shape = ic.shape orelse return null;
    if (base_object.shape != shape) return null;

    if (!base_object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_get,
        .ordinary_get_own_property,
    }))) return null;

    switch (ic.type) {
        .value => return base_object.property_storage.properties.items[@intFromEnum(ic.offset)].value,
        .accessor => {
            const getter = base_object.property_storage.properties.items[@intFromEnum(ic.offset)].getter_or_setter orelse {
                @branchHint(.unlikely);
                return .undefined;
            };
            return try Value.from(getter).callAssumeCallable(agent, base_value, &.{});
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

    if (!base_object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_set,
        .ordinary_get_own_property,
    }))) return false;

    switch (ic.type) {
        .value => {
            base_object.property_storage.properties.items[@intFromEnum(ic.offset)] = .{ .value = value };
            return true;
        },
        .accessor => {
            const setter = base_object.property_storage.properties.items[@intFromEnum(ic.offset) + 1].getter_or_setter orelse return false;
            _ = try Value.from(setter).callAssumeCallable(agent, base_value, &.{value});
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
    if (!base_object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        switch (kind) {
            .get => .ordinary_get,
            .set => .ordinary_set,
        },
        .ordinary_get_own_property,
    }))) return;

    if (base_object.shape.isUnique()) return;

    if (base_object.shape.properties.get(property_key)) |property_metadata| {
        if (kind == .set and property_metadata.type == .value and !property_metadata.attributes.writable) return;
        ic.* = .{
            .shape = base_object.shape,
            .offset = property_metadata.offset,
            .type = property_metadata.type,
        };
    }
}
