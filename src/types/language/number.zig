//! 6.1.6.1 The Number Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-number-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const String = types.String;

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
    ) @TypeOf(writer).Error!void {
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
                if (@typeInfo(T) == .Int and @typeInfo(T).Int.bits > 53) {
                    @compileError("Number.from() is only safe up to 53 bit integers");
                }
                if (std.math.cast(i32, number)) |x| {
                    return .{ .i32 = x };
                }
                return .{ .f64 = @floatFromInt(number) };
            },
            .Float, .ComptimeFloat => {
                const truncated = std.math.trunc(number);
                if (std.math.isFinite(@as(f64, number)) and
                    !std.math.signbit(@as(f64, number)) and
                    truncated == number and
                    truncated <= @as(f64, @floatFromInt(std.math.maxInt(i32))) and
                    truncated >= @as(f64, @floatFromInt(std.math.minInt(i32))))
                {
                    return .{ .i32 = @intFromFloat(truncated) };
                }
                return .{ .f64 = @as(f64, number) };
            },
            else => @compileError("Number.from() called with incompatible type " ++ @typeName(T)),
        }
    }

    pub inline fn asFloat(self: Self) f64 {
        return switch (self) {
            .f64 => |x| x,
            .i32 => |x| @as(f64, @floatFromInt(x)),
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

    pub inline fn isZero(self: Self) bool {
        return switch (self) {
            .f64 => |x| x == 0,
            .i32 => |x| x == 0,
        };
    }

    pub inline fn isPositiveZero(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isPositiveZero(x),
            .i32 => |x| x == 0,
        };
    }

    pub inline fn isNegativeZero(self: Self) bool {
        return switch (self) {
            .f64 => |x| std.math.isNegativeZero(x),
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

    pub inline fn ceil(self: Self) Self {
        return switch (self) {
            .f64 => |x| .{ .f64 = @ceil(x) },
            .i32 => |x| .{ .i32 = x },
        };
    }

    pub inline fn floor(self: Self) Self {
        return switch (self) {
            .f64 => |x| .{ .f64 = @floor(x) },
            .i32 => |x| .{ .i32 = x },
        };
    }

    inline fn toInt32(self: Self) i32 {
        return switch (self) {
            .f64 => |x| blk: {
                // Excerpt from Value.toInt32()
                if (!std.math.isFinite(x) or x == 0) break :blk 0;
                const int = @trunc(x);
                const int32bit = @mod(int, pow_2_32);
                break :blk @as(
                    i32,
                    @intFromFloat(if (int32bit >= pow_2_31) int32bit - pow_2_32 else int32bit),
                );
            },
            .i32 => |x| x,
        };
    }

    inline fn toUint32(self: Self) u32 {
        return switch (self) {
            .f64 => |x| blk: {
                // Excerpt from Value.toUint32()
                if (!std.math.isFinite(x) or x == 0) break :blk 0;
                const int = @trunc(x);
                const int32bit = @mod(int, pow_2_32);
                break :blk @as(u32, @intFromFloat(int32bit));
            },
            .i32 => |x| blk: {
                const int = @as(i64, x);
                const int32bit = @mod(int, comptime @as(i64, @intFromFloat(pow_2_32)));
                break :blk @as(u32, @intCast(int32bit));
            },
        };
    }

    pub fn toFloat16(self: Self) f16 {
        return utils.float16.__truncdfhf2(self.asFloat());
    }

    /// 6.1.6.1.1 Number::unaryMinus ( x )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-unaryMinus
    pub fn unaryMinus(self: Self) Self {
        // 1. If x is NaN, return NaN.
        if (self.isNan()) return self;

        // 2. Return the result of negating x; that is, compute a Number with the same magnitude
        //    but opposite sign.
        return if (self.isZero())
            .{ .f64 = -self.asFloat() }
        else switch (self) {
            .f64 => |x| .{ .f64 = -x },
            .i32 => |x| .{ .i32 = -x },
        };
    }

    /// 6.1.6.1.2 Number::bitwiseNOT ( x )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-bitwiseNOT
    pub fn bitwiseNOT(self: Self) Self {
        // 1. Let oldValue be ! ToInt32(x).
        const old_value = self.toInt32();

        // 2. Return the result of applying bitwise complement to oldValue. The mathematical value
        //    of the result is exactly representable as a 32-bit two's complement bit string.
        return .{ .i32 = ~old_value };
    }

    /// 6.1.6.1.3 Number::exponentiate ( base, exponent )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-exponentiate
    pub fn exponentiate(base: Self, exponent: Self) Self {
        // 1. If exponent is NaN, return NaN.
        if (exponent.isNan()) return .{ .f64 = std.math.nan(f64) };

        // 2. If exponent is either +0𝔽 or -0𝔽, return 1𝔽.
        if (exponent.isZero()) return .{ .i32 = 1 };

        // 3. If base is NaN, return NaN.
        if (base.isNan()) return .{ .f64 = std.math.nan(f64) };

        // 4. If base is +∞𝔽, then
        if (base.isPositiveInf()) {
            // a. If exponent > +0𝔽, return +∞𝔽. Otherwise, return +0𝔽.
            if (exponent.asFloat() > 0)
                return .{ .f64 = std.math.inf(f64) }
            else
                return .{ .i32 = 0 };
        }

        // 5. If base is -∞𝔽, then
        if (base.isNegativeInf()) {
            // a. If exponent > +0𝔽, then
            if (exponent.asFloat() > 0) {
                // i. If exponent is an odd integral Number, return -∞𝔽. Otherwise, return +∞𝔽.
                if (std.math.isFinite(exponent.asFloat()) and
                    @trunc(exponent.asFloat()) == exponent.asFloat() and
                    @mod(exponent.asFloat(), 2) != 0)
                    return .{ .f64 = -std.math.inf(f64) }
                else
                    return .{ .f64 = std.math.inf(f64) };
            }
            // b. Else,
            else {
                // i. If exponent is an odd integral Number, return -0𝔽. Otherwise, return +0𝔽.
                if (std.math.isFinite(exponent.asFloat()) and
                    @trunc(exponent.asFloat()) == exponent.asFloat() and
                    @mod(exponent.asFloat(), 2) != 0)
                    return .{ .f64 = -0.0 }
                else
                    return .{ .i32 = 0 };
            }
        }

        // 6. If base is +0𝔽, then
        if (base.isPositiveZero()) {
            // a. If exponent > +0𝔽, return +0𝔽. Otherwise, return +∞𝔽.
            if (exponent.asFloat() > 0)
                return .{ .i32 = 0 }
            else
                return .{ .f64 = std.math.inf(f64) };
        }

        // 7. If base is -0𝔽, then
        if (base.isNegativeZero()) {
            // a. If exponent > +0𝔽, then
            if (exponent.asFloat() > 0) {
                // i. If exponent is an odd integral Number, return -0𝔽. Otherwise, return +0𝔽.
                if (std.math.isFinite(exponent.asFloat()) and
                    @trunc(exponent.asFloat()) == exponent.asFloat() and
                    @mod(exponent.asFloat(), 2) != 0)
                    return .{ .f64 = -0.0 }
                else
                    return .{ .i32 = 0 };
            }
            // b. Else,
            else {
                // i. If exponent is an odd integral Number, return -∞𝔽. Otherwise, return +∞𝔽.
                if (std.math.isFinite(exponent.asFloat()) and
                    @trunc(exponent.asFloat()) == exponent.asFloat() and
                    @mod(exponent.asFloat(), 2) != 0)
                    return .{ .f64 = -std.math.inf(f64) }
                else
                    return .{ .f64 = std.math.inf(f64) };
            }
        }

        // 8. Assert: base is finite and is neither +0𝔽 nor -0𝔽.
        std.debug.assert(base.isFinite() and !base.isZero());

        // 9. If exponent is +∞𝔽, then
        if (exponent.isPositiveInf()) {
            // a. If abs(ℝ(base)) > 1, return +∞𝔽.
            if (@abs(base.asFloat()) > 1) return .{ .f64 = std.math.inf(f64) };

            // b. If abs(ℝ(base)) = 1, return NaN.
            if (@abs(base.asFloat()) == 1) return .{ .f64 = std.math.nan(f64) };

            // c. If abs(ℝ(base)) < 1, return +0𝔽.
            if (@abs(base.asFloat()) < 1) return .{ .i32 = 0 };
        }

        // 10. If exponent is -∞𝔽, then
        if (exponent.isNegativeInf()) {
            // a. If abs(ℝ(base)) > 1, return +0𝔽.
            if (@abs(base.asFloat()) > 1) return .{ .i32 = 0 };

            // b. If abs(ℝ(base)) = 1, return NaN.
            if (@abs(base.asFloat()) == 1) return .{ .f64 = std.math.nan(f64) };

            // c. If abs(ℝ(base)) < 1, return +∞𝔽.
            if (@abs(base.asFloat()) < 1) return .{ .f64 = std.math.inf(f64) };
        }

        // 11. Assert: exponent is finite and is neither +0𝔽 nor -0𝔽.
        std.debug.assert(exponent.isFinite() and !exponent.isZero());

        // 12. If base < -0𝔽 and exponent is not an integral Number, return NaN.
        if (base.asFloat() < 0 and @trunc(exponent.asFloat()) != exponent.asFloat())
            return .{ .f64 = std.math.nan(f64) };

        // 13. Return an implementation-approximated Number value representing the result of
        //     raising ℝ(base) to the ℝ(exponent) power.
        return from(std.math.pow(f64, base.asFloat(), exponent.asFloat()));
    }

    /// 6.1.6.1.4 Number::multiply ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-multiply
    pub fn multiply(x: Self, y: Self) Self {
        if (x == .i32 and y == .i32) {
            if (std.math.mul(i32, x.i32, y.i32) catch null) |result| return .{ .i32 = result };
        }
        return from(x.asFloat() * y.asFloat());
    }

    /// 6.1.6.1.5 Number::divide ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-divide
    pub fn divide(x: Self, y: Self) Self {
        return from(x.asFloat() / y.asFloat());
    }

    /// 6.1.6.1.6 Number::remainder ( n, d )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-remainder
    pub fn remainder(n: Self, d: Self) Self {
        // 1. If n is NaN or d is NaN, return NaN.
        if (n.isNan() or d.isNan()) return .{ .f64 = std.math.nan(f64) };

        // 2. If n is either +∞𝔽 or -∞𝔽, return NaN.
        if (n.isPositiveInf() or n.isNegativeInf()) return .{ .f64 = std.math.nan(f64) };

        // 3. If d is either +∞𝔽 or -∞𝔽, return n.
        if (d.isPositiveInf() or d.isNegativeInf()) return n;

        // 4. If d is either +0𝔽 or -0𝔽, return NaN.
        if (d.isZero()) return .{ .f64 = std.math.nan(f64) };

        // 5. If n is either +0𝔽 or -0𝔽, return n.
        if (n.isZero()) return n;

        // 6. Assert: n and d are finite and non-zero.
        std.debug.assert(n.isFinite() and n.asFloat() != 0);
        std.debug.assert(d.isFinite() and d.asFloat() != 0);

        // 7. Let quotient be ℝ(n) / ℝ(d).
        const quotient = n.asFloat() / d.asFloat();

        // 8. Let q be truncate(quotient).
        const q = @trunc(quotient);

        // 9. Let r be ℝ(n) - (ℝ(d) × q).
        const r = n.asFloat() - (d.asFloat() * q);

        // 10. If r = 0 and n < -0𝔽, return -0𝔽.
        if (r == 0 and n.asFloat() < 0) return .{ .f64 = -0.0 };

        // 11. Return 𝔽(r).
        return from(r);
    }

    /// 6.1.6.1.7 Number::add ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-add
    pub fn add(x: Self, y: Self) Self {
        if (x == .i32 and y == .i32) {
            if (std.math.add(i32, x.i32, y.i32) catch null) |result| return .{ .i32 = result };
        }
        return from(x.asFloat() + y.asFloat());
    }

    /// 6.1.6.1.8 Number::subtract ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-subtract
    pub fn subtract(x: Self, y: Self) Self {
        // 1. Return Number::add(x, Number::unaryMinus(y)).
        if (x == .i32 and y == .i32) {
            if (std.math.sub(i32, x.i32, y.i32) catch null) |result| return .{ .i32 = result };
        }
        return from(x.asFloat() - y.asFloat());
    }

    /// 6.1.6.1.9 Number::leftShift ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-leftShift
    pub fn leftShift(x: Self, y: Self) Self {
        // 1. Let lnum be ! ToInt32(x).
        const lnum = x.toInt32();

        // 2. Let rnum be ! ToUint32(y).
        const rnum = y.toUint32();

        // 3. Let shiftCount be ℝ(rnum) modulo 32.
        const shift_count: u5 = @intCast(@mod(rnum, 32));

        // 4. Return the result of left shifting lnum by shiftCount bits. The mathematical value of
        //    the result is exactly representable as a 32-bit two's complement bit string.
        return .{ .i32 = lnum << shift_count };
    }

    /// 6.1.6.1.10 Number::signedRightShift ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-signedRightShift
    pub fn signedRightShift(x: Self, y: Self) Self {
        // 1. Let lnum be ! ToInt32(x).
        const lnum = x.toInt32();

        // 2. Let rnum be ! ToUint32(y).
        const rnum = y.toUint32();

        // 3. Let shiftCount be ℝ(rnum) modulo 32.
        const shift_count: u5 = @intCast(@mod(rnum, 32));

        // 4. Return the result of performing a sign-extending right shift of lnum by shiftCount
        //    bits. The most significant bit is propagated. The mathematical value of the result
        //    is exactly representable as a 32-bit two's complement bit string.
        return .{ .i32 = lnum >> shift_count };
    }

    /// 6.1.6.1.11 Number::unsignedRightShift ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-unsignedRightShift
    pub fn unsignedRightShift(x: Self, y: Self) Self {
        // 1. Let lnum be ! ToUint32(x).
        const lnum = x.toUint32();

        // 2. Let rnum be ! ToUint32(y).
        const rnum = y.toUint32();

        // 3. Let shiftCount be ℝ(rnum) modulo 32.
        const shift_count: u5 = @intCast(@mod(rnum, 32));

        // 4. Return the result of performing a zero-filling right shift of lnum by shiftCount
        //    bits. Vacated bits are filled with zero. The mathematical value of the result is
        //    exactly representable as a 32-bit unsigned bit string.
        return from(lnum >> shift_count);
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
        if (y.isNegativeInf()) return false;

        // 9. If x is -∞𝔽, return true.
        if (x.isNegativeInf()) return true;

        // 10. Assert: x and y are finite.
        std.debug.assert(std.math.isFinite(x.asFloat()) and std.math.isFinite(y.asFloat()));

        // 11. If ℝ(x) < ℝ(y), return true. Otherwise, return false.
        return x.asFloat() < y.asFloat();
    }

    /// 6.1.6.1.13 Number::equal ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-equal
    pub fn equal(x: Self, y: Self) bool {
        // 1. If x is NaN, return false.
        if (x.isNan()) return false;

        // 2. If y is NaN, return false.
        if (y.isNan()) return false;

        // 3. If x is y, return true.
        // 4. If x is +0𝔽 and y is -0𝔽, return true.
        // 5. If x is -0𝔽 and y is +0𝔽, return true.
        // 6. Return false.
        return x.asFloat() == y.asFloat();
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

    /// 6.1.6.1.16 NumberBitwiseOp ( op, x, y )
    /// https://tc39.es/ecma262/#sec-numberbitwiseop
    inline fn numberBitwiseOp(comptime op: enum { @"&", @"^", @"|" }, x: Self, y: Self) i32 {
        // 1. Let lnum be ! ToInt32(x).
        const lnum = x.toInt32();

        // 2. Let rnum be ! ToInt32(y).
        const rnum = y.toInt32();

        // 3. Let lbits be the 32-bit two's complement bit string representing ℝ(lnum).
        // 4. Let rbits be the 32-bit two's complement bit string representing ℝ(rnum).

        const result = switch (op) {
            // 5. If op is &, then
            // a. Let result be the result of applying the bitwise AND operation to lbits and rbits.
            .@"&" => lnum & rnum,

            // 6. Else if op is ^, then
            // a. Let result be the result of applying the bitwise exclusive OR (XOR) operation to
            //    lbits and rbits.
            .@"^" => lnum ^ rnum,

            // 7. Else,
            // a. Assert: op is |.
            // b. Let result be the result of applying the bitwise inclusive OR operation to lbits
            //    and rbits.
            .@"|" => lnum | rnum,
        };

        // 8. Return the Number value for the integer represented by the 32-bit two's complement
        //    bit string result.
        return result;
    }

    /// 6.1.6.1.17 Number::bitwiseAND ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-bitwiseAND
    pub fn bitwiseAND(x: Self, y: Self) Self {
        // 1. Return NumberBitwiseOp(&, x, y).
        return .{ .i32 = numberBitwiseOp(.@"&", x, y) };
    }

    /// 6.1.6.1.18 Number::bitwiseXOR ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-bitwiseXOR
    pub fn bitwiseXOR(x: Self, y: Self) Self {
        // 1. Return NumberBitwiseOp(^, x, y).
        return .{ .i32 = numberBitwiseOp(.@"^", x, y) };
    }

    /// 6.1.6.1.19 Number::bitwiseOR ( x, y )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-bitwiseOR
    pub fn bitwiseOR(x: Self, y: Self) Self {
        // 1. Return NumberBitwiseOp(|, x, y).
        return .{ .i32 = numberBitwiseOp(.@"|", x, y) };
    }

    /// 6.1.6.1.20 Number::toString ( x, radix )
    /// https://tc39.es/ecma262/#sec-numeric-types-number-tostring
    pub fn toString(self: Self, allocator: Allocator, radix: u8) Allocator.Error!String {
        // 1. If x is NaN, return "NaN".
        if (self.isNan()) return String.fromLiteral("NaN");

        // 2. If x is either +0𝔽 or -0𝔽, return "0".
        if (self.isPositiveZero() or self.isNegativeZero()) return String.fromLiteral("0");

        // 3. If x < -0𝔽, return the string-concatenation of "-" and Number::toString(-x, radix).
        if (self.asFloat() < 0) {
            return String.fromAscii(
                try std.fmt.allocPrint(allocator, "-{}", .{
                    try self.unaryMinus().toString(allocator, radix),
                }),
            );
        }

        // 4. If x is +∞𝔽, return "Infinity".
        if (self.isPositiveInf()) return String.fromLiteral("Infinity");

        // TODO: Implement steps 5-12 according to spec!
        return String.fromAscii(try switch (self) {
            .f64 => |x| if (x > @as(f64, 1e-7) and x < @as(f64, 1e21))
                std.fmt.allocPrint(allocator, "{0d}", .{x})
            else
                std.fmt.allocPrint(allocator, "{0e}", .{x}),
            .i32 => |x| std.fmt.allocPrint(allocator, "{}", .{x}),
        });
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
        const number, const expected = test_case;
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{number});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}
