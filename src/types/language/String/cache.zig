pub const std = @import("std");

const types = @import("../../../types.zig");

const String = types.String;

pub const Cache = std.HashMapUnmanaged(Key, *const String, struct {
    pub fn hash(_: @This(), key: Key) u64 {
        return key.hash();
    }

    pub fn eql(_: @This(), a: Key, b: Key) bool {
        return a.eql(b);
    }
}, std.hash_map.default_max_load_percentage);

pub const Key = union(enum) {
    utf8: []const u8,
    utf16: []const u16,

    pub fn eql(self: Key, other: Key) bool {
        if (self == .utf8 and other == .utf8) {
            return std.mem.eql(u8, self.utf8, other.utf8);
        } else if (self == .utf16 and other == .utf16) {
            return std.mem.eql(u16, self.utf16, other.utf16);
        }
        return false;
    }

    pub fn hash(self: Key) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&std.meta.activeTag(self)));
        switch (self) {
            .utf8 => |utf8| hasher.update(utf8),
            .utf16 => |utf16| hasher.update(std.mem.sliceAsBytes(utf16)),
        }
        return hasher.final();
    }
};
