const std = @import("std");

const types = @import("../../../types.zig");

const Attributes = Object.PropertyStorage.Attributes;
const CompletePropertyDescriptor = Object.PropertyStorage.CompletePropertyDescriptor;
const Object = types.Object;
const Value = types.Value;

const IndexedProperties = @This();

storage: Storage,

pub const empty: IndexedProperties = .{
    .storage = .none,
};

pub const Index = u32;

pub const Storage = union(Type) {
    pub const Type = enum {
        none,
        dense_i32,
        dense_f64,
        dense_value,
        sparse,
    };

    none,
    dense_i32: std.ArrayListUnmanaged(i32),
    dense_f64: std.ArrayListUnmanaged(f64),
    dense_value: std.ArrayListUnmanaged(Value),
    sparse: std.AutoHashMapUnmanaged(Index, CompletePropertyDescriptor),
};

fn propertyDescriptorFromValue(value: Value) CompletePropertyDescriptor {
    return .{
        .value_or_accessor = .{
            .value = value,
        },
        .attributes = .all,
    };
}

fn migrateStorageIfNeeded(
    self: *IndexedProperties,
    allocator: std.mem.Allocator,
    index: Index,
    maybe_property_descriptor: ?CompletePropertyDescriptor,
) std.mem.Allocator.Error!void {
    const old_storage_type = std.meta.activeTag(self.storage);
    const new_storage_type: Storage.Type = blk: {
        // Property set or addition
        if (maybe_property_descriptor) |property_descriptor| {
            if (old_storage_type == .sparse or // No downgrades
                index > self.count() or // Created an array hole
                property_descriptor.value_or_accessor == .accessor or // Accessor property
                property_descriptor.attributes != Attributes.all // Non-default properties
            ) {
                break :blk .sparse;
            }
            if (property_descriptor.value_or_accessor.value.__isI32()) {
                switch (old_storage_type) {
                    .none, .dense_i32 => break :blk .dense_i32,
                    else => {},
                }
            }
            if (property_descriptor.value_or_accessor.value.__isF64()) {
                switch (old_storage_type) {
                    .none, .dense_i32, .dense_f64 => break :blk .dense_f64,
                    else => {},
                }
            }
            break :blk .dense_value;
        }
        // Property removal
        else {
            std.debug.assert(old_storage_type != .none);
            if (old_storage_type != .sparse and index + 1 == self.count()) {
                if (index == 0) break :blk .none;
                break :blk old_storage_type;
            }
            break :blk .sparse;
        }
    };
    if (old_storage_type != new_storage_type) {
        try self.migrateStorage(allocator, new_storage_type);
    }
}

pub fn migrateStorage(
    self: *IndexedProperties,
    allocator: std.mem.Allocator,
    new_storage_type: Storage.Type,
) std.mem.Allocator.Error!void {
    const old_storage_type = std.meta.activeTag(self.storage);
    std.debug.assert(@intFromEnum(old_storage_type) < @intFromEnum(new_storage_type) or
        new_storage_type == .none);
    self.storage = switch (new_storage_type) {
        .none => blk: {
            switch (self.storage) {
                .none => unreachable,
                .dense_i32 => |*dense_i32| dense_i32.deinit(allocator),
                .dense_f64 => |*dense_f64| dense_f64.deinit(allocator),
                .dense_value => |*dense_value| dense_value.deinit(allocator),
                .sparse => |*sparse| sparse.deinit(allocator),
            }
            break :blk .none;
        },
        .dense_i32 => blk: {
            const dense_i32: std.ArrayListUnmanaged(i32) = .empty;
            switch (self.storage) {
                .none => {},
                .dense_i32, .dense_f64, .dense_value, .sparse => unreachable,
            }
            break :blk .{ .dense_i32 = dense_i32 };
        },
        .dense_f64 => blk: {
            var dense_f64: std.ArrayListUnmanaged(f64) = .empty;
            switch (self.storage) {
                .none => {},
                .dense_i32 => |*dense_i32| {
                    try dense_f64.resize(allocator, dense_i32.items.len);
                    for (dense_i32.items, 0..) |value, i| {
                        dense_f64.items[i] = @floatFromInt(value);
                    }
                    dense_i32.deinit(allocator);
                },
                .dense_f64, .dense_value, .sparse => unreachable,
            }
            break :blk .{ .dense_f64 = dense_f64 };
        },
        .dense_value => blk: {
            var dense_value: std.ArrayListUnmanaged(Value) = .empty;
            switch (self.storage) {
                .none => {},
                .dense_i32 => |*dense_i32| {
                    try dense_value.resize(allocator, dense_i32.items.len);
                    for (dense_i32.items, 0..) |value, i| {
                        dense_value.items[i] = Value.from(value);
                    }
                    dense_i32.deinit(allocator);
                },
                .dense_f64 => |*dense_f64| {
                    try dense_value.resize(allocator, dense_f64.items.len);
                    for (dense_f64.items, 0..) |value, i| {
                        dense_value.items[i] = Value.from(value);
                    }
                    dense_f64.deinit(allocator);
                },
                .dense_value, .sparse => unreachable,
            }
            break :blk .{ .dense_value = dense_value };
        },
        .sparse => blk: {
            var sparse: std.AutoHashMapUnmanaged(Index, CompletePropertyDescriptor) = .empty;
            switch (self.storage) {
                .none => {},
                .dense_i32 => |*dense_i32| {
                    try sparse.ensureTotalCapacity(allocator, @intCast(dense_i32.items.len));
                    for (dense_i32.items, 0..) |value, i| {
                        sparse.putAssumeCapacity(
                            @intCast(i),
                            propertyDescriptorFromValue(Value.from(value)),
                        );
                    }
                    dense_i32.deinit(allocator);
                },
                .dense_f64 => |*dense_f64| {
                    try sparse.ensureTotalCapacity(allocator, @intCast(dense_f64.items.len));
                    for (dense_f64.items, 0..) |value, i| {
                        sparse.putAssumeCapacity(
                            @intCast(i),
                            propertyDescriptorFromValue(Value.from(value)),
                        );
                    }
                    dense_f64.deinit(allocator);
                },
                .dense_value => |*dense_value| {
                    try sparse.ensureTotalCapacity(allocator, @intCast(dense_value.items.len));
                    for (dense_value.items, 0..) |value, i| {
                        sparse.putAssumeCapacity(
                            @intCast(i),
                            propertyDescriptorFromValue(value),
                        );
                    }
                    dense_value.deinit(allocator);
                },
                .sparse => unreachable,
            }
            break :blk .{ .sparse = sparse };
        },
    };
}

pub fn count(self: IndexedProperties) usize {
    switch (self.storage) {
        .none => return 0,
        .dense_i32 => |dense_i32| return dense_i32.items.len,
        .dense_f64 => |dense_f64| return dense_f64.items.len,
        .dense_value => |dense_value| return dense_value.items.len,
        .sparse => |sparse| return sparse.size,
    }
}

pub fn contains(self: IndexedProperties, index: Index) bool {
    switch (self.storage) {
        .none => return false,
        .dense_i32 => |dense_i32| return index < dense_i32.items.len,
        .dense_f64 => |dense_f64| return index < dense_f64.items.len,
        .dense_value => |dense_value| return index < dense_value.items.len,
        .sparse => |sparse| return sparse.contains(index),
    }
}

pub fn get(self: IndexedProperties, index: Index) ?CompletePropertyDescriptor {
    switch (self.storage) {
        .none => return null,
        .dense_i32 => |dense_i32| {
            if (dense_i32.items.len <= index) return null;
            return propertyDescriptorFromValue(Value.from(dense_i32.items[index]));
        },
        .dense_f64 => |dense_f64| {
            if (dense_f64.items.len <= index) return null;
            return propertyDescriptorFromValue(Value.from(dense_f64.items[index]));
        },
        .dense_value => |dense_value| {
            if (dense_value.items.len <= index) return null;
            return propertyDescriptorFromValue(dense_value.items[index]);
        },
        .sparse => |sparse| {
            return sparse.get(index);
        },
    }
}

pub fn set(
    self: *IndexedProperties,
    allocator: std.mem.Allocator,
    index: Index,
    property_descriptor: CompletePropertyDescriptor,
) std.mem.Allocator.Error!void {
    try self.migrateStorageIfNeeded(allocator, index, property_descriptor);
    switch (self.storage) {
        .none => unreachable,
        .dense_i32 => |*dense_i32| {
            if (index >= dense_i32.items.len) try dense_i32.resize(allocator, index + 1);
            dense_i32.items[index] = property_descriptor.value_or_accessor.value.__asI32();
        },
        .dense_f64 => |*dense_f64| {
            if (index >= dense_f64.items.len) try dense_f64.resize(allocator, index + 1);
            dense_f64.items[index] = property_descriptor.value_or_accessor.value.__asF64();
        },
        .dense_value => |*dense_value| {
            if (index >= dense_value.items.len) try dense_value.resize(allocator, index + 1);
            dense_value.items[index] = property_descriptor.value_or_accessor.value;
        },
        .sparse => |*sparse| {
            try sparse.put(allocator, index, property_descriptor);
        },
    }
}

pub fn remove(
    self: *IndexedProperties,
    allocator: std.mem.Allocator,
    index: Index,
) std.mem.Allocator.Error!void {
    try self.migrateStorageIfNeeded(allocator, index, null);
    switch (self.storage) {
        .none => {},
        .dense_i32 => |*dense_i32| _ = dense_i32.pop().?,
        .dense_f64 => |*dense_f64| _ = dense_f64.pop().?,
        .dense_value => |*dense_value| _ = dense_value.pop().?,
        .sparse => |*sparse| {
            const removed = sparse.remove(index);
            std.debug.assert(removed);
        },
    }
}
