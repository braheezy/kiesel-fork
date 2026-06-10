const std = @import("std");

const types = @import("../../../types.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

const PropertyStorage = @This();

/// Stores a single value for data properties and a pair of object/null for accessors.
properties: std.ArrayList(Value),

pub const PropertyType = enum {
    value,
    accessor,
};

pub const Accessor = struct {
    get: ?*Object,
    set: ?*Object,
};

pub const ValueOrAccessor = union(PropertyType) {
    value: Value,
    accessor: Accessor,
};

pub const Attributes = packed struct(u3) {
    writable: bool,
    enumerable: bool,
    configurable: bool,

    pub const all: Attributes = .{
        .writable = true,
        .enumerable = true,
        .configurable = true,
    };

    pub const none: Attributes = .{
        .writable = false,
        .enumerable = false,
        .configurable = false,
    };

    pub const builtin_default: Attributes = .{
        .writable = true,
        .enumerable = false,
        .configurable = true,
    };

    pub fn fromPropertyDescriptor(property_descriptor: *const PropertyDescriptor) Attributes {
        return .{
            .writable = property_descriptor.writable orelse false,
            .enumerable = property_descriptor.enumerable orelse false,
            .configurable = property_descriptor.configurable orelse false,
        };
    }
};

/// Like the regular `PropertyDescriptor` but only with complete states - either value or
/// accessor, all attributes set - representable.
pub const CompletePropertyDescriptor = struct {
    value_or_accessor: ValueOrAccessor,
    attributes: Attributes,

    pub fn fromPropertyDescriptor(descriptor: *const PropertyDescriptor) CompletePropertyDescriptor {
        if (descriptor.isAccessorDescriptor()) {
            return .{
                .value_or_accessor = .{
                    .accessor = .{
                        .get = descriptor.get orelse @as(?*Object, null),
                        .set = descriptor.set orelse @as(?*Object, null),
                    },
                },
                .attributes = .{
                    .writable = false,
                    .enumerable = descriptor.enumerable orelse false,
                    .configurable = descriptor.configurable orelse false,
                },
            };
        } else {
            std.debug.assert(descriptor.isDataDescriptor());
            return .{
                .value_or_accessor = .{
                    .value = descriptor.value orelse .undefined,
                },
                .attributes = .{
                    .writable = descriptor.writable orelse false,
                    .enumerable = descriptor.enumerable orelse false,
                    .configurable = descriptor.configurable orelse false,
                },
            };
        }
    }

    pub fn toPropertyDescriptor(self: *const CompletePropertyDescriptor) PropertyDescriptor {
        return switch (self.value_or_accessor) {
            .value => |value| .{
                .value = value,
                .writable = self.attributes.writable,
                .enumerable = self.attributes.enumerable,
                .configurable = self.attributes.configurable,
            },
            .accessor => |accessor| .{
                .get = if (accessor.get) |object| object else @as(?*Object, null),
                .set = if (accessor.set) |object| object else @as(?*Object, null),
                .enumerable = self.attributes.enumerable,
                .configurable = self.attributes.configurable,
            },
        };
    }
};

pub fn contains(
    _: *const PropertyStorage,
    object: *Object,
    property_key: PropertyKey,
) bool {
    if (property_key.isArrayIndex()) {
        const extra_data = object.extra_data orelse return false;
        return extra_data.indexed_properties.contains(@intCast(property_key.integer_index));
    }
    return object.shape.properties.contains(property_key);
}

pub fn getCreateLazyIfNeeded(
    self: *PropertyStorage,
    object: *Object,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?CompletePropertyDescriptor {
    if (property_key.isArrayIndex()) {
        const extra_data = object.extra_data orelse return null;
        return extra_data.indexed_properties.get(@intCast(property_key.integer_index));
    }
    const property_metadata = object.shape.properties.get(property_key) orelse return null;
    switch (property_metadata.type) {
        .value => {
            var value = &self.properties.items[@intFromEnum(property_metadata.offset)];
            if (value.isUninitialized()) {
                @branchHint(.unlikely);
                const extra_data = object.extra_data.?;
                const lazy_property = extra_data.lazy_properties.fetchRemove(property_key).?.value;
                const realm = lazy_property.realm;
                const agent = realm.agent;
                value.* = try lazy_property.initializer.value(agent, realm);
            }
            return .{
                .value_or_accessor = .{ .value = value.* },
                .attributes = property_metadata.attributes,
            };
        },
        .accessor => {
            var getter_value = &self.properties.items[@intFromEnum(property_metadata.offset)];
            var setter_value = &self.properties.items[@intFromEnum(property_metadata.offset) + 1];
            if (getter_value.isUninitialized()) {
                @branchHint(.unlikely);
                std.debug.assert(setter_value.isUninitialized());
                const extra_data = object.extra_data.?;
                const lazy_property = extra_data.lazy_properties.fetchRemove(property_key).?.value;
                const realm = lazy_property.realm;
                const agent = realm.agent;
                const accessor = try lazy_property.initializer.accessor(agent, realm);
                getter_value.* = if (accessor.get) |getter| Value.from(getter) else .null;
                setter_value.* = if (accessor.set) |setter| Value.from(setter) else .null;
            }
            return .{
                .value_or_accessor = .{ .accessor = .{
                    .get = if (getter_value.isObject()) getter_value.asObject() else null,
                    .set = if (setter_value.isObject()) setter_value.asObject() else null,
                } },
                .attributes = property_metadata.attributes,
            };
        },
    }
}

pub fn set(
    self: *PropertyStorage,
    object: *Object,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
    property_descriptor: CompletePropertyDescriptor,
) std.mem.Allocator.Error!void {
    if (property_key.isArrayIndex()) {
        const indexed_properties = try object.ensureIndexedProperties(allocator);
        return indexed_properties.set(allocator, @intCast(property_key.integer_index), property_descriptor);
    }
    const value_or_accessor = property_descriptor.value_or_accessor;
    const attributes = property_descriptor.attributes;
    if (object.shape.properties.get(property_key)) |property_metadata| {
        const property_attributes_change = property_metadata.attributes != attributes;
        const property_type_change = property_metadata.type != std.meta.activeTag(value_or_accessor);
        if (property_attributes_change or property_type_change) {
            object.shape = try object.shape.setProperty(
                allocator,
                property_key,
                attributes,
                std.meta.activeTag(value_or_accessor),
            );
        }
        if (property_type_change) {
            // Clear value in the previous storage list
            switch (property_metadata.type) {
                .value => {
                    self.properties.items[@intFromEnum(property_metadata.offset)] = undefined;
                },
                .accessor => {
                    self.properties.items[@intFromEnum(property_metadata.offset)] = undefined;
                    self.properties.items[@intFromEnum(property_metadata.offset) + 1] = undefined;
                },
            }
            switch (value_or_accessor) {
                .value => |value| {
                    try self.properties.append(allocator, value);
                },
                .accessor => |accessor| {
                    const getter_value: Value = if (accessor.get) |getter| Value.from(getter) else .null;
                    const setter_value: Value = if (accessor.set) |setter| Value.from(setter) else .null;
                    try self.properties.appendSlice(allocator, &.{ getter_value, setter_value });
                },
            }
        } else {
            switch (value_or_accessor) {
                .value => |value| {
                    self.properties.items[@intFromEnum(property_metadata.offset)] = value;
                },
                .accessor => |accessor| {
                    const getter_value: Value = if (accessor.get) |getter| Value.from(getter) else .null;
                    const setter_value: Value = if (accessor.set) |setter| Value.from(setter) else .null;
                    self.properties.items[@intFromEnum(property_metadata.offset)] = getter_value;
                    self.properties.items[@intFromEnum(property_metadata.offset) + 1] = setter_value;
                },
            }
        }
    } else {
        object.shape = try object.shape.setProperty(
            allocator,
            property_key,
            attributes,
            std.meta.activeTag(value_or_accessor),
        );
        switch (value_or_accessor) {
            .value => |value| {
                try self.properties.append(allocator, value);
            },
            .accessor => |accessor| {
                const getter_value: Value = if (accessor.get) |getter| Value.from(getter) else .null;
                const setter_value: Value = if (accessor.set) |setter| Value.from(setter) else .null;
                try self.properties.appendSlice(allocator, &.{ getter_value, setter_value });
            },
        }
    }
}
pub fn remove(
    self: *PropertyStorage,
    object: *Object,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
) std.mem.Allocator.Error!void {
    if (property_key.isArrayIndex()) {
        const extra_data = object.extra_data.?;
        return extra_data.indexed_properties.remove(allocator, @intCast(property_key.integer_index));
    }
    const property_metadata = object.shape.properties.get(property_key).?;
    object.shape = try object.shape.deleteProperty(allocator, property_key);
    // By overwriting the value and keeping subsequent offsets intact we can make property
    // deletions part of the regular transition chain without making them unique and invalidating
    // ICs. Additionally we save the cost of moving all elements after this one around, at the
    // memory cost of wasting one element.
    switch (property_metadata.type) {
        .value => {
            self.properties.items[@intFromEnum(property_metadata.offset)] = undefined;
        },
        .accessor => {
            self.properties.items[@intFromEnum(property_metadata.offset)] = undefined;
            self.properties.items[@intFromEnum(property_metadata.offset) + 1] = undefined;
        },
    }
}
