//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const Self = @This();

value: []const u8,

pub inline fn from(value: []const u8) Self {
    return .{ .value = value };
}
