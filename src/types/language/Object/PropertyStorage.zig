const std = @import("std");

const Allocator = std.mem.Allocator;

const spec = @import("../../spec.zig");

const Object = @import("../Object.zig");
const PropertyDescriptor = spec.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Symbol = @import("../Symbol.zig");

const Self = @This();

// TODO: Shapes, linear storage for arrays, etc. Gotta start somewhere :^)
hash_map: PropertyKeyHashMap(PropertyDescriptor),

pub fn init(allocator: Allocator) Self {
    return .{
        .hash_map = PropertyKeyHashMap(PropertyDescriptor).init(allocator),
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
    const property_key_copy = switch (property_key) {
        // Copy the string since it might outlive the source text - identifiers are just slices of that.
        .string => |string| PropertyKey.from(try self.hash_map.allocator.dupe(u8, string.utf8)),
        else => property_key,
    };
    try self.hash_map.put(property_key_copy, property_descriptor);
}

pub fn remove(self: *Self, property_key: PropertyKey) void {
    const removed = self.hash_map.orderedRemove(property_key);
    std.debug.assert(removed);
}

pub const PropertyKeyHashMapContext = struct {
    const hashString = std.hash_map.hashString;
    const hashSymbol = std.hash_map.getAutoHashFn(Symbol.Id, void);
    const hashIntegerIndex = std.hash_map.getAutoHashFn(PropertyKey.IntegerIndex, void);

    pub fn hash(_: anytype, property_key: PropertyKey) u64 {
        return switch (property_key) {
            .string => |string| hashString(string.utf8),
            .symbol => |symbol| hashSymbol({}, symbol.id),
            .integer_index => |integer_index| hashIntegerIndex({}, integer_index),
        };
    }

    pub fn eql(_: anytype, a: PropertyKey, b: PropertyKey) bool {
        return a.eql(b);
    }
};

pub fn PropertyKeyHashMap(comptime V: type) type {
    return std.ArrayHashMap(PropertyKey, V, struct {
        pub fn hash(self: @This(), property_key: PropertyKey) u32 {
            return @truncate(PropertyKeyHashMapContext.hash(self, property_key));
        }

        pub fn eql(self: @This(), a: PropertyKey, b: PropertyKey, _: usize) bool {
            return PropertyKeyHashMapContext.eql(self, a, b);
        }
    }, false);
}
