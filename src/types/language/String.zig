//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const std = @import("std");

const Self = @This();

value: []const u8,

pub inline fn from(value: []const u8) Self {
    return .{ .value = value };
}

pub inline fn utf16Length(self: Self) usize {
    return std.unicode.calcUtf16LeLen(self.value) catch unreachable;
}
