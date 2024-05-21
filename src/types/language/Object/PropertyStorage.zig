const std = @import("std");

const Allocator = std.mem.Allocator;

const spec = @import("../../spec.zig");

const Object = @import("../Object.zig");
const PropertyDescriptor = spec.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Symbol = @import("../Symbol.zig");

const Self = @This();

// TODO: Shapes, linear storage for arrays, etc. Gotta start somewhere :^)
hash_map: PropertyKeyArrayHashMap(PropertyDescriptor),

pub fn init(allocator: Allocator) Self {
    return .{
        .hash_map = PropertyKeyArrayHashMap(PropertyDescriptor).init(allocator),
    };
}

pub fn has(self: Self, property_key: PropertyKey) bool {
    return self.hash_map.contains(property_key);
}

pub fn get(self: Self, property_key: PropertyKey) ?PropertyDescriptor {
    return self.hash_map.get(property_key);
}

pub fn set(
    self: *Self,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Allocator.Error!void {
    try self.hash_map.put(property_key, property_descriptor);
}

pub fn remove(self: *Self, property_key: PropertyKey) void {
    const removed = self.hash_map.orderedRemove(property_key);
    std.debug.assert(removed);
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
