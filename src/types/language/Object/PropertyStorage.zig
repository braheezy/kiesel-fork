const std = @import("std");

const Allocator = std.mem.Allocator;

const spec = @import("../../spec.zig");

const Object = @import("../Object.zig");
const PropertyDescriptor = spec.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Symbol = @import("../Symbol.zig");

const Self = @This();

// TODO: Shapes, linear storage for arrays, etc. Gotta start somewhere :^)
hash_map: PropertyHashMap,

pub fn init(allocator: Allocator) Self {
    return .{
        .hash_map = PropertyHashMap.init(allocator),
    };
}

pub fn has(self: Self, property_key: PropertyKey) bool {
    return self.hash_map.contains(property_key);
}

pub fn get(self: Self, property_key: PropertyKey) ?PropertyDescriptor {
    return self.hash_map.get(property_key);
}

pub fn set(self: *Self, property_key: PropertyKey, property_descriptor: PropertyDescriptor) !void {
    try self.hash_map.put(property_key, property_descriptor);
}

pub fn remove(self: *Self, property_key: PropertyKey) void {
    const removed = self.hash_map.orderedRemove(property_key);
    std.debug.assert(removed);
}

const PropertyHashMap = std.ArrayHashMap(PropertyKey, PropertyDescriptor, struct {
    const hashString = std.hash_map.hashString;
    const hashSymbol = std.hash_map.getAutoHashFn(Symbol.Id, @This());
    const hashIntegerIndex = std.hash_map.getAutoHashFn(PropertyKey.IntegerIndex, @This());

    const eqlString = std.hash_map.eqlString;

    fn eqlStringAndIntegerIndex(string: []const u8, index: PropertyKey.IntegerIndex) bool {
        const len = comptime std.fmt.count("{d}", .{std.math.maxInt(PropertyKey.IntegerIndex)});
        var index_string: [len]u8 = undefined;
        _ = std.fmt.bufPrint(&index_string, "{d}", .{index}) catch unreachable;
        return eqlString(string, &index_string);
    }

    pub fn hash(self: @This(), k: PropertyKey) u32 {
        return switch (k) {
            .string => |string| @truncate(u32, hashString(string)),
            .symbol => |symbol| @truncate(u32, hashSymbol(self, symbol.id)),
            .integer_index => |integer_index| @truncate(u32, hashIntegerIndex(self, integer_index)),
        };
    }

    pub fn eql(self: @This(), a: PropertyKey, b: PropertyKey, b_index_in_hashmap: usize) bool {
        _ = b_index_in_hashmap;
        _ = self;
        return switch (a) {
            .string => |a_string| switch (b) {
                .string => |b_string| eqlString(a_string, b_string),
                .symbol => false,
                .integer_index => |b_integer_index| eqlStringAndIntegerIndex(a_string, b_integer_index),
            },
            .symbol => |a_symbol| switch (b) {
                .string => false,
                .symbol => |b_symbol| a_symbol.id == b_symbol.id,
                .integer_index => false,
            },
            .integer_index => |a_integer_index| switch (b) {
                .string => |b_string| eqlStringAndIntegerIndex(b_string, a_integer_index),
                .symbol => false,
                .integer_index => |b_integer_index| a_integer_index == b_integer_index,
            },
        };
    }
}, false);
