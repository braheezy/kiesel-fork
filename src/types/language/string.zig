//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const std = @import("std");

const Allocator = std.mem.Allocator;

pub const String = union(enum) {
    const Self = @This();

    value: []const u8,

    pub inline fn from(value: []const u8) Self {
        return .{ .value = value };
    }

    pub inline fn utf16Length(self: Self) usize {
        return std.unicode.calcUtf16LeLen(self.value) catch unreachable;
    }

    pub inline fn utf16CodeUnits(self: Self, allocator: Allocator) ![]const u16 {
        return std.unicode.utf8ToUtf16LeWithNull(allocator, self.value) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidUtf8 => unreachable,
        };
    }

    pub inline fn substring(self: Self, allocator: Allocator, start: usize, end: usize) ![]const u8 {
        const code_units = try self.utf16CodeUnits(allocator);
        defer allocator.free(code_units);
        return std.unicode.utf16leToUtf8Alloc(allocator, code_units[start..end]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DanglingSurrogateHalf,
            error.ExpectedSecondSurrogateHalf,
            error.UnexpectedSecondSurrogateHalf,
            => unreachable,
        };
    }
};
