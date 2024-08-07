const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Symbol = types.Symbol;
const Value = types.Value;

const Self = @This();

/// `PropertyDescriptor` is designed to have an empty or partial state as well, which makes it
/// unnecessarily large. Within the property storage hash map we store data using this more compact
/// representation and convert it from/to `PropertyDesccriptor`s on the fly - it's okay for them to
/// be large on the stack :^)
const Entry = union(enum) {
    accessor: struct {
        get: ?Object,
        set: ?Object,
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
                    .get = property_descriptor.get.?,
                    .set = property_descriptor.set.?,
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
                .get = accessor.get,
                .set = accessor.set,
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
    // Let's make sure the size doesn't quietly change (yes, it's complicated)
    switch (builtin.target.ptrBitWidth()) {
        32 => switch (builtin.mode) {
            .Debug, .ReleaseSafe => std.debug.assert(@sizeOf(Entry) == 56),
            .ReleaseFast, .ReleaseSmall => std.debug.assert(@sizeOf(Entry) == 48),
        },
        64 => switch (builtin.mode) {
            .Debug, .ReleaseSafe => std.debug.assert(@sizeOf(Entry) == 96),
            .ReleaseFast, .ReleaseSmall => std.debug.assert(@sizeOf(Entry) == 80),
        },
        else => unreachable,
    }
}

const LazyIntrinsic = struct {
    realm: *Realm,
    lazyIntrinsicFn: *const fn (*Realm.Intrinsics) Allocator.Error!Object,
};

// TODO: Shapes, linear storage for arrays, etc. Gotta start somewhere :^)
hash_map: PropertyKeyArrayHashMap(Entry),
lazy_intrinsics: std.StringHashMap(LazyIntrinsic),

pub fn init(allocator: Allocator) Self {
    return .{
        .hash_map = PropertyKeyArrayHashMap(Entry).init(allocator),
        .lazy_intrinsics = std.StringHashMap(LazyIntrinsic).init(allocator),
    };
}

pub fn has(self: Self, property_key: PropertyKey) bool {
    return self.hash_map.contains(property_key);
}

pub fn get(self: Self, property_key: PropertyKey) ?PropertyDescriptor {
    if (property_key == .string and property_key.string == .ascii) {
        const name = property_key.string.ascii;
        std.debug.assert(!self.lazy_intrinsics.contains(name));
    }
    if (self.hash_map.get(property_key)) |entry| {
        return entry.toPropertyDescriptor();
    }
    return null;
}

pub fn getCreateIntrinsicIfNeeded(self: *Self, property_key: PropertyKey) Allocator.Error!?PropertyDescriptor {
    if (self.hash_map.getPtr(property_key)) |entry| {
        if (property_key == .string and property_key.string == .ascii) {
            const name = property_key.string.ascii;
            if (self.lazy_intrinsics.get(name)) |lazy_intrinsic| {
                const object = try lazy_intrinsic.lazyIntrinsicFn(&lazy_intrinsic.realm.intrinsics);
                entry.data.value = Value.from(object);
                _ = self.lazy_intrinsics.remove(name);
            }
        }
        return entry.toPropertyDescriptor();
    }
    return null;
}

pub fn set(
    self: *Self,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Allocator.Error!void {
    const entry = Entry.fromPropertyDescriptor(property_descriptor);
    try self.hash_map.put(property_key, entry);
    if (property_key == .string and property_key.string == .ascii) {
        const name = property_key.string.ascii;
        _ = self.lazy_intrinsics.remove(name);
    }
}

pub fn remove(self: *Self, property_key: PropertyKey) void {
    const removed = self.hash_map.orderedRemove(property_key);
    std.debug.assert(removed);
    if (property_key == .string and property_key.string == .ascii) {
        const name = property_key.string.ascii;
        _ = self.lazy_intrinsics.remove(name);
    }
}

pub const PropertyKeyArrayHashMapContext = struct {
    pub fn hash(_: anytype, property_key: PropertyKey) u64 {
        return switch (property_key) {
            .string => |string| string.hash(),
            .symbol => |symbol| std.hash_map.getAutoHashFn(Symbol.Id, void)({}, symbol.id),
            .integer_index => |integer_index| std.hash_map.getAutoHashFn(PropertyKey.IntegerIndex, void)({}, integer_index),
        };
    }

    pub fn eql(_: anytype, a: PropertyKey, b: PropertyKey) bool {
        return a.eql(b);
    }
};

pub fn PropertyKeyArrayHashMap(comptime V: type) type {
    return std.ArrayHashMap(PropertyKey, V, struct {
        pub fn hash(self: @This(), property_key: PropertyKey) u32 {
            return @truncate(PropertyKeyArrayHashMapContext.hash(self, property_key));
        }

        pub fn eql(self: @This(), a: PropertyKey, b: PropertyKey, _: usize) bool {
            return PropertyKeyArrayHashMapContext.eql(self, a, b);
        }
    }, false);
}
