//! 6.1.6.1 The Number Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-number-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const pow_2_31 = std.math.pow(f64, 2, 31);
const pow_2_32 = std.math.pow(f64, 2, 32);

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
            .Int, .ComptimeInt => {
                if (@intToFloat(f64, number) <= @intToFloat(f64, std.math.maxInt(i32))) {
                    return .{ .i32 = @intCast(i32, number) };
                }
                return .{ .f64 = @intToFloat(f64, number) };
            },
            .Float, .ComptimeFloat => {
                const truncated = std.math.trunc(number);
                if (std.math.isFinite(@as(f64, number)) and
                    !std.math.signbit(@as(f64, number)) and
                    truncated == number and
                    truncated <= @intToFloat(f64, std.math.maxInt(i32)))
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

    /// 6.1.6.1.1 Number::unaryMinus ( x )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-unaryMinus
    pub fn unaryMinus(self: Self) Self {
        // 1. If x is NaN, return NaN.
        if (self.isNan()) return self;

        // 2. Return the result of negating x; that is, compute a Number with the same magnitude
        //    but opposite sign.
        return switch (self) {
            .f64 => |x| .{ .f64 = -x },
            .i32 => |x| .{ .i32 = -x },
        };
    }

    /// 6.1.6.1.2 Number::bitwiseNOT ( x )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-bitwiseNOT
    pub fn bitwiseNOT(self: Self) Self {
        // 1. Let oldValue be ! ToInt32(x).
        const old_value = switch (self) {
            .f64 => |x| blk: {
                // Excerpt from Value.toInt32()
                if (!std.math.isFinite(x) or x == 0) break :blk 0;
                const int = @trunc(x);
                const int32bit = @mod(int, pow_2_32);
                break :blk @floatToInt(
                    i32,
                    if (int32bit >= pow_2_31) int32bit - pow_2_32 else int32bit,
                );
            },
            .i32 => |x| x,
        };

        // 2. Return the result of applying bitwise complement to oldValue. The mathematical value
        //    of the result is exactly representable as a 32-bit two's complement bit string.
        return .{ .i32 = ~old_value };
    }

    /// 6.1.6.1.12 Number::lessThan ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-lessThan
    pub fn lessThan(x: Self, y: Self) ?bool {
        // 1. If x is NaN, return undefined.
        if (x.isNan()) return null;

        // 2. If y is NaN, return undefined.
        if (y.isNan()) return null;

        // 3. If x is y, return false.
        if (x.sameValue(y)) return false;

        // 4. If x is +0𝔽 and y is -0𝔽, return false.
        if (x.isPositiveZero() and y.isNegativeZero()) return false;

        // 5. If x is -0𝔽 and y is +0𝔽, return false.
        if (x.isNegativeZero() and y.isPositiveZero()) return false;

        // 6. If x is +∞𝔽, return false.
        if (x.isPositiveInf()) return false;

        // 7. If y is +∞𝔽, return true.
        if (y.isPositiveInf()) return true;

        // 8. If y is -∞𝔽, return false.
        if (x.isNegativeInf()) return false;

        // 9. If x is -∞𝔽, return true.
        if (y.isNegativeInf()) return true;

        // 10. Assert: x and y are finite and non-zero.
        std.debug.assert(std.math.isFinite(x.asFloat()) and x.asFloat() != 0);
        std.debug.assert(std.math.isFinite(y.asFloat()) and y.asFloat() != 0);

        // 11. If ℝ(x) < ℝ(y), return true. Otherwise, return false.
        return x.asFloat() < y.asFloat();
    }

    /// 6.1.6.1.14 Number::sameValue ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-sameValue
    pub fn sameValue(x: Self, y: Self) bool {
        // 1. If x is NaN and y is NaN, return true.
        if (x.isNan() and y.isNan()) return true;

        // 2. If x is +0𝔽 and y is -0𝔽, return false.
        if (x.isPositiveZero() and y.isNegativeZero()) return false;

        // 3. If x is -0𝔽 and y is +0𝔽, return false.
        if (x.isNegativeZero() and y.isPositiveZero()) return false;

        // 4. If x is y, return true.
        // 5. Return false.
        return x.asFloat() == y.asFloat();
    }

    /// 6.1.6.1.15 Number::sameValueZero ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-sameValueZero
    pub fn sameValueZero(x: Self, y: Self) bool {
        // 1. If x is NaN and y is NaN, return true.
        if (x.isNan() and y.isNan()) return true;

        // 2. If x is +0𝔽 and y is -0𝔽, return true.
        // 3. If x is -0𝔽 and y is +0𝔽, return true.
        // 4. If x is y, return true.
        // 5. Return false.
        return x.asFloat() == y.asFloat();
    }

    /// 6.1.6.1.20 Number::toString ( x, radix )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-tostring
    pub fn toString(self: Self, allocator: Allocator, radix: u8) ![]const u8 {
        // 1. If x is NaN, return "NaN".
        if (self.isNan()) return "NaN";

        // 2. If x is either +0𝔽 or -0𝔽, return "0".
        if (self.isPositiveZero() or self.isNegativeZero()) return "0";

        // 3. If x < -0𝔽, return the string-concatenation of "-" and Number::toString(-x, radix).
        if (self.asFloat() < 0) return std.fmt.allocPrint(allocator, "-{s}", .{
            try self.unaryMinus().toString(allocator, radix),
        });

        // 4. If x is +∞𝔽, return "Infinity".
        if (self.isPositiveInf()) return "Infinity";

        // TODO: Implement steps 5-12 according to spec!
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
