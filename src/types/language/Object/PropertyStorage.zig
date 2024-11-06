const builtin = @import("builtin");
const std = @import("std");

const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;

const PropertyStorage = @This();

/// `PropertyDescriptor` is designed to have an empty or partial state as well, which makes it
/// unnecessarily large. Within the property storage hash map we store data using this more compact
/// representation and convert it from/to `PropertyDesccriptor`s on the fly - it's okay for them to
/// be large on the stack :^)
const Entry = union(enum) {
    accessor: struct {
        get: ?*Object,
        set: ?*Object,
        attributes: Attributes,
    },
    data: struct {
        value: Value,
        attributes: Attributes,
    },

    const Attributes = packed struct(u3) {
        writable: bool,
        enumerable: bool,
        configurable: bool,
    };

    fn fromPropertyDescriptor(property_descriptor: PropertyDescriptor) Entry {
        if (property_descriptor.isAccessorDescriptor()) {
            return .{
                .accessor = .{
                    .get = if (property_descriptor.get.?) |object| object else null,
                    .set = if (property_descriptor.set.?) |object| object else null,
                    .attributes = .{
                        .writable = false,
                        .enumerable = property_descriptor.enumerable.?,
                        .configurable = property_descriptor.configurable.?,
                    },
                },
            };
        } else if (property_descriptor.isDataDescriptor()) {
            return .{
                .data = .{
                    .value = property_descriptor.value.?,
                    .attributes = .{
                        .writable = property_descriptor.writable.?,
                        .enumerable = property_descriptor.enumerable.?,
                        .configurable = property_descriptor.configurable.?,
                    },
                },
            };
        } else unreachable;
    }

    fn toPropertyDescriptor(self: Entry) PropertyDescriptor {
        return switch (self) {
            .accessor => |accessor| .{
                .get = if (accessor.get) |object| object else @as(?*Object, null),
                .set = if (accessor.set) |object| object else @as(?*Object, null),
                .enumerable = accessor.attributes.enumerable,
                .configurable = accessor.attributes.configurable,
            },
            .data => |data| .{
                .value = data.value,
                .writable = data.attributes.writable,
                .enumerable = data.attributes.enumerable,
                .configurable = data.attributes.configurable,
            },
        };
    }
};

comptime {
    // Let's make sure the size doesn't quietly change
    switch (builtin.target.ptrBitWidth()) {
        // Only some 32-bit platforms have certain bitpacking optimizations applied
        32 => std.debug.assert(@sizeOf(Entry) == 20 or @sizeOf(Entry) == 32),
        64 => std.debug.assert(@sizeOf(Entry) == 32),
        else => unreachable,
    }
}

const LazyIntrinsic = struct {
    realm: *Realm,
    lazyIntrinsicFn: *const fn (*Realm.Intrinsics) std.mem.Allocator.Error!*Object,
};

// TODO: Shapes, linear storage for arrays, etc. Gotta start somewhere :^)
hash_map: PropertyKey.ArrayHashMap(Entry),
lazy_intrinsics: String.HashMap(LazyIntrinsic),

pub fn init(allocator: std.mem.Allocator) PropertyStorage {
    return .{
        .hash_map = .init(allocator),
        .lazy_intrinsics = .init(allocator),
    };
}

pub fn has(self: PropertyStorage, property_key: PropertyKey) bool {
    return self.hash_map.contains(property_key);
}

pub fn get(self: PropertyStorage, property_key: PropertyKey) ?PropertyDescriptor {
    if (property_key == .string) {
        std.debug.assert(!self.lazy_intrinsics.contains(property_key.string));
    }
    if (self.hash_map.get(property_key)) |entry| {
        return entry.toPropertyDescriptor();
    }
    return null;
}

pub fn getCreateIntrinsicIfNeeded(
    self: *PropertyStorage,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    if (self.hash_map.getPtr(property_key)) |entry| {
        if (property_key == .string) {
            if (self.lazy_intrinsics.get(property_key.string)) |lazy_intrinsic| {
                const object = try lazy_intrinsic.lazyIntrinsicFn(&lazy_intrinsic.realm.intrinsics);
                entry.data.value = Value.from(object);
                _ = self.lazy_intrinsics.remove(property_key.string);
            }
        }
        return entry.toPropertyDescriptor();
    }
    return null;
}

pub fn set(
    self: *PropertyStorage,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) std.mem.Allocator.Error!void {
    const entry = Entry.fromPropertyDescriptor(property_descriptor);
    try self.hash_map.put(property_key, entry);
    if (property_key == .string) {
        _ = self.lazy_intrinsics.remove(property_key.string);
    }
}

pub fn remove(self: *PropertyStorage, property_key: PropertyKey) void {
    const removed = self.hash_map.orderedRemove(property_key);
    std.debug.assert(removed);
    if (property_key == .string) {
        _ = self.lazy_intrinsics.remove(property_key.string);
    }
}
