const std = @import("std");

const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PrivateElement = types.PrivateElement;
const PrivateName = types.PrivateName;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;

const PropertyStorage = @This();

shape: *Object.Shape,
properties: std.ArrayList(union {
    value: Value,
    getter_or_setter: ?*Object,
}),
indexed_properties: Object.IndexedProperties,
lazy_properties: PropertyKey.HashMapUnmanaged(LazyProperty),

/// [[PrivateElements]]
private_elements: PrivateName.HashMapUnmanaged(PrivateElement),

pub const LazyProperty = struct {
    pub const Initializer = union(PropertyType) {
        value: *const fn (*Agent, *Realm) std.mem.Allocator.Error!Value,
        accessor: *const fn (*Agent, *Realm) std.mem.Allocator.Error!Accessor,
    };

    realm: *Realm,
    initializer: Initializer,
};

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

    pub fn fromPropertyDescriptor(property_descriptor: PropertyDescriptor) Attributes {
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

    pub fn fromPropertyDescriptor(descriptor: PropertyDescriptor) CompletePropertyDescriptor {
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

    pub fn toPropertyDescriptor(self: CompletePropertyDescriptor) PropertyDescriptor {
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
    self: PropertyStorage,
    property_key: PropertyKey,
) bool {
    if (property_key.isArrayIndex()) {
        return self.indexed_properties.contains(@intCast(property_key.integer_index));
    }
    return self.shape.properties.contains(property_key);
}

pub fn get(
    self: PropertyStorage,
    property_key: PropertyKey,
) ?CompletePropertyDescriptor {
    if (property_key.isArrayIndex()) {
        return self.indexed_properties.get(@intCast(property_key.integer_index));
    }
    const property_metadata = self.shape.properties.get(property_key) orelse return null;
    std.debug.assert(!self.lazy_properties.contains(property_key));
    const value_or_accessor: ValueOrAccessor = switch (property_metadata.index) {
        .value => |index| .{ .value = self.values.items[@intFromEnum(index)] },
        .accessor => |index| .{ .accessor = self.accessors.items[@intFromEnum(index)] },
    };
    return .{
        .value_or_accessor = value_or_accessor,
        .attributes = property_metadata.attributes,
    };
}

pub fn getCreateIntrinsicIfNeeded(
    self: *PropertyStorage,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?CompletePropertyDescriptor {
    if (property_key.isArrayIndex()) {
        return self.indexed_properties.get(@intCast(property_key.integer_index));
    }
    const property_metadata = self.shape.properties.get(property_key) orelse return null;
    if (self.lazy_properties.fetchRemove(property_key)) |kv| {
        const lazy_property = kv.value;
        const realm = lazy_property.realm;
        const agent = realm.agent;
        switch (property_metadata.type) {
            .value => {
                const value = try lazy_property.initializer.value(agent, realm);
                self.properties.items[@intFromEnum(property_metadata.index)] = .{ .value = value };
            },
            .accessor => {
                const accessor = try lazy_property.initializer.accessor(agent, realm);
                self.properties.items[@intFromEnum(property_metadata.index)] = .{ .getter_or_setter = accessor.get };
                self.properties.items[@intFromEnum(property_metadata.index) + 1] = .{ .getter_or_setter = accessor.set };
            },
        }
    }
    const value_or_accessor: ValueOrAccessor = switch (property_metadata.type) {
        .value => .{
            .value = self.properties.items[@intFromEnum(property_metadata.index)].value,
        },
        .accessor => .{
            .accessor = .{
                .get = self.properties.items[@intFromEnum(property_metadata.index)].getter_or_setter,
                .set = self.properties.items[@intFromEnum(property_metadata.index) + 1].getter_or_setter,
            },
        },
    };
    return .{
        .value_or_accessor = value_or_accessor,
        .attributes = property_metadata.attributes,
    };
}

pub fn set(
    self: *PropertyStorage,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
    property_descriptor: CompletePropertyDescriptor,
) std.mem.Allocator.Error!void {
    if (property_key.isArrayIndex()) {
        return self.indexed_properties.set(allocator, @intCast(property_key.integer_index), property_descriptor);
    }
    const value_or_accessor = property_descriptor.value_or_accessor;
    const attributes = property_descriptor.attributes;
    if (self.shape.properties.get(property_key)) |property_metadata| {
        const property_attributes_change = property_metadata.attributes != attributes;
        const property_type_change = property_metadata.type != std.meta.activeTag(value_or_accessor);
        if (property_attributes_change or property_type_change) {
            self.shape = try self.shape.setProperty(
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
                    self.properties.items[@intFromEnum(property_metadata.index)] = undefined;
                },
                .accessor => {
                    self.properties.items[@intFromEnum(property_metadata.index)] = undefined;
                    self.properties.items[@intFromEnum(property_metadata.index) + 1] = undefined;
                },
            }
            switch (value_or_accessor) {
                .value => |value| {
                    try self.properties.append(allocator, .{ .value = value });
                },
                .accessor => |accessor| {
                    try self.properties.append(allocator, .{ .getter_or_setter = accessor.get });
                    try self.properties.append(allocator, .{ .getter_or_setter = accessor.set });
                },
            }
        } else {
            switch (value_or_accessor) {
                .value => |value| {
                    self.properties.items[@intFromEnum(property_metadata.index)] = .{ .value = value };
                },
                .accessor => |accessor| {
                    self.properties.items[@intFromEnum(property_metadata.index)] = .{ .getter_or_setter = accessor.get };
                    self.properties.items[@intFromEnum(property_metadata.index) + 1] = .{ .getter_or_setter = accessor.set };
                },
            }
        }
    } else {
        self.shape = try self.shape.setProperty(
            allocator,
            property_key,
            attributes,
            std.meta.activeTag(value_or_accessor),
        );
        switch (value_or_accessor) {
            .value => |value| {
                try self.properties.append(allocator, .{ .value = value });
            },
            .accessor => |accessor| {
                try self.properties.append(allocator, .{ .getter_or_setter = accessor.get });
                try self.properties.append(allocator, .{ .getter_or_setter = accessor.set });
            },
        }
    }
}

pub fn remove(
    self: *PropertyStorage,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
) std.mem.Allocator.Error!void {
    if (property_key.isArrayIndex()) {
        return self.indexed_properties.remove(allocator, @intCast(property_key.integer_index));
    }
    const property_metadata = self.shape.properties.get(property_key).?;
    self.shape = try self.shape.deleteProperty(allocator, property_key);
    // By overwriting the value and keeping subsequent indices intact we can make property
    // deletions part of the regular transition chain without making them unique and invalidating
    // ICs. Additionally we save the cost of moving all elements after this one around, at the
    // memory cost of wasting one element.
    switch (property_metadata.type) {
        .value => {
            self.properties.items[@intFromEnum(property_metadata.index)] = undefined;
        },
        .accessor => {
            self.properties.items[@intFromEnum(property_metadata.index)] = undefined;
            self.properties.items[@intFromEnum(property_metadata.index) + 1] = undefined;
        },
    }
}
