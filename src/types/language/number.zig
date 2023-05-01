//! 6.1.6.1 The Number Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-number-type

const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Number = union(enum) {
    const Self = @This();

    f64: f64,
    // OPTIMIZATION: Instead of always storing floats we also have a Number type that stores an
    // i32 internally.
    i32: i32,

    pub fn from(number: anytype) Self {
        switch (@typeInfo(@TypeOf(number))) {
            .Int, .ComptimeInt => return .{ .i32 = @as(i32, number) },
            .Float, .ComptimeFloat => {
                const truncated = std.math.trunc(number);
                if (std.math.isFinite(@as(f64, number)) and
                    truncated == number and
                    truncated <= std.math.maxInt(i32))
                {
                    return .{ .i32 = @floatToInt(i32, truncated) };
                }
                return .{ .f64 = @as(f64, number) };
            },
            else => @compileError("Value.fromNumber() called with incompatible type " ++ @typeName(@TypeOf(number))),
        }
    }

    /// 6.1.6.1.20 Number::toString ( x, radix )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-tostring
    pub fn toString(self: Self, allocator: Allocator, radix: u8) ![]const u8 {
        _ = radix;
        // TODO: Implement according to spec!
        return switch (self) {
            .f64 => |x| std.fmt.allocPrint(allocator, "{}", .{x}),
            .i32 => |x| std.fmt.allocPrint(allocator, "{}", .{x}),
        };
    }
};
