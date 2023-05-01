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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .f64 => |x| {
                if (std.math.isNan(x)) {
                    try writer.writeAll("NaN");
                } else if (std.math.isPositiveInf(x)) {
                    try writer.writeAll("Infinity");
                } else if (std.math.isNegativeInf(x)) {
                    try writer.writeAll("-Infinity");
                } else {
                    try writer.print("{d}", .{x});
                }
            },
            .i32 => |x| try writer.print("{d}", .{x}),
        }
    }

    pub fn from(number: anytype) Self {
        switch (@typeInfo(@TypeOf(number))) {
            .Int, .ComptimeInt => return .{ .i32 = @as(i32, number) },
            .Float, .ComptimeFloat => {
                const truncated = std.math.trunc(number);
                if (std.math.isFinite(@as(f64, number)) and
                    !std.math.signbit(@as(f64, number)) and
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

    pub fn asFloat(self: Self) f64 {
        return switch (self) {
            .f64 => |x| x,
            .i32 => |x| @intToFloat(f64, x),
        };
    }

    pub fn isNan(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isNan(x),
            .i32 => false,
        };
    }

    pub fn isPositiveInf(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isPositiveInf(x),
            .i32 => false,
        };
    }

    pub fn isNegativeInf(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isNegativeInf(x),
            .i32 => false,
        };
    }

    pub fn isFinite(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isFinite(x),
            .i32 => true,
        };
    }

    pub fn truncate(self: Self) Self {
        return switch (self) {
            .f64 => |x| .{ .f64 = @trunc(x) },
            .i32 => |x| .{ .i32 = x },
        };
    }

    pub fn floor(self: Self) Self {
        return switch (self) {
            .f64 => |x| .{ .f64 = @floor(x) },
            .i32 => |x| .{ .i32 = x },
        };
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

test "format" {
    const test_cases = [_]struct { Number, []const u8 }{
        .{ Number.from(0), "0" },
        .{ Number.from(-0.0), "-0" },
        .{ Number.from(123), "123" },
        .{ Number.from(123.456), "123.456" },
        .{ Number.from(-42), "-42" },
        .{ Number.from(std.math.nan(f64)), "NaN" },
        .{ Number.from(std.math.inf(f64)), "Infinity" },
        .{ Number.from(-std.math.inf(f64)), "-Infinity" },
    };
    for (test_cases) |test_case| {
        const number = test_case[0];
        const expected = test_case[1];
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{number});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}
