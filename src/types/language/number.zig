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

    pub inline fn from(number: anytype) Self {
        const T = @TypeOf(number);
        switch (@typeInfo(T)) {
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
            else => @compileError("Number.from() called with incompatible type " ++ @typeName(T)),
        }
    }

    pub inline fn asFloat(self: Self) f64 {
        return switch (self) {
            .f64 => |x| x,
            .i32 => |x| @intToFloat(f64, x),
        };
    }

    pub inline fn isNan(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isNan(x),
            .i32 => false,
        };
    }

    pub inline fn isPositiveInf(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isPositiveInf(x),
            .i32 => false,
        };
    }

    pub inline fn isNegativeInf(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isNegativeInf(x),
            .i32 => false,
        };
    }

    pub inline fn isPositiveZero(self: Self) bool {
        return switch (self) {
            .f64 => |x| x == 0 and !std.math.signbit(x),
            .i32 => |x| x == 0,
        };
    }

    pub inline fn isNegativeZero(self: Self) bool {
        return switch (self) {
            .f64 => |x| x == 0 and std.math.signbit(x),
            .i32 => false,
        };
    }

    pub inline fn isFinite(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isFinite(x),
            .i32 => true,
        };
    }

    pub inline fn truncate(self: Self) Self {
        return switch (self) {
            .f64 => |x| .{ .f64 = @trunc(x) },
            .i32 => |x| .{ .i32 = x },
        };
    }

    pub inline fn floor(self: Self) Self {
        return switch (self) {
            .f64 => |x| .{ .f64 = @floor(x) },
            .i32 => |x| .{ .i32 = x },
        };
    }

    /// 6.1.6.1.14 Number::sameValue ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-sameValue
    pub fn sameValue(x: Self, y: Self) bool {
        // 1. If x is NaN and y is NaN, return true.
        if (x.isNan() and y.isNan())
            return true;

        // 2. If x is +0𝔽 and y is -0𝔽, return false.
        if (x.isPositiveZero() and y.isNegativeZero())
            return false;

        // 3. If x is -0𝔽 and y is +0𝔽, return false.
        if (x.isNegativeZero() and y.isPositiveZero())
            return false;

        // 4. If x is y, return true.
        // 5. Return false.
        return x.asFloat() == y.asFloat();
    }

    /// 6.1.6.1.15 Number::sameValueZero ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-sameValueZero
    pub fn sameValueZero(x: Self, y: Self) bool {
        // 1. If x is NaN and y is NaN, return true.
        if (x.isNan() and y.isNan())
            return true;

        // 2. If x is +0𝔽 and y is -0𝔽, return true.
        // 3. If x is -0𝔽 and y is +0𝔽, return true.
        // 4. If x is y, return true.
        // 5. Return false.
        return x.asFloat() == y.asFloat();
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
