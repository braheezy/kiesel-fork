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

const LazyIntrinsic = struct {
    realm: *Realm,
    lazyIntrinsicFn: *const fn (*Realm.Intrinsics) Allocator.Error!Object,
};

// TODO: Shapes, linear storage for arrays, etc. Gotta start somewhere :^)
hash_map: PropertyKeyArrayHashMap(PropertyDescriptor),
lazy_intrinsics: std.StringHashMap(LazyIntrinsic),

pub fn init(allocator: Allocator) Self {
    return .{
        .hash_map = PropertyKeyArrayHashMap(PropertyDescriptor).init(allocator),
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
    return self.hash_map.get(property_key);
}

pub fn getCreateIntrinsicIfNeeded(self: *Self, property_key: PropertyKey) Allocator.Error!?PropertyDescriptor {
    if (self.hash_map.getPtr(property_key)) |property_descriptor| {
        if (property_key == .string and property_key.string == .ascii) {
            const name = property_key.string.ascii;
            if (self.lazy_intrinsics.get(name)) |lazy_intrinsic| {
                const object = try lazy_intrinsic.lazyIntrinsicFn(&lazy_intrinsic.realm.intrinsics);
                property_descriptor.value = Value.from(object);
                _ = self.lazy_intrinsics.remove(name);
            }
        }
        return property_descriptor.*;
    }
    return null;
}

pub fn set(
    self: *Self,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Allocator.Error!void {
    try self.hash_map.put(property_key, property_descriptor);
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
