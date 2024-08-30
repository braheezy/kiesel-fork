//! 6.1.6.2 The BigInt Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;

const BigInt = @This();

managed: *std.math.big.int.Managed,

pub fn format(
    self: BigInt,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    try writer.print("{}", .{self.managed.*});
    try writer.writeAll("n");
}

pub fn from(allocator: Allocator, value: anytype) Allocator.Error!BigInt {
    const managed = try allocator.create(std.math.big.int.Managed);
    managed.* = switch (@TypeOf(value)) {
        std.math.big.int.Managed => value,
        else => try std.math.big.int.Managed.initSet(allocator, value),
    };
    return .{ .managed = managed };
}

/// For tests not using the GC allocator.
pub fn deinit(self: BigInt, allocator: Allocator) void {
    self.managed.deinit();
    allocator.destroy(self.managed);
}

pub fn asFloat(self: BigInt, agent: *Agent) Allocator.Error!f64 {
    // NOTE: We could also use to(i1024) here, which should cover the largest possible int for
    //       an f64, but that fails to codegen on the Zig side for at least aarch64-macos and
    //       wasm32-wasi. Going via toString() and parsing that into a float isn't great but
    //       works for now.
    return std.fmt.parseFloat(
        f64,
        (try self.toString(agent.gc_allocator, 10)).data.slice.ascii,
    ) catch unreachable;
}

/// 6.1.6.2.1 BigInt::unaryMinus ( x )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-unaryMinus
pub fn unaryMinus(x: BigInt, agent: *Agent) Allocator.Error!BigInt {
    // 1.If x = 0ℤ, return 0ℤ.
    if (x.managed.eqlZero()) return x;

    // 2. Return -x.
    var result = try x.managed.clone();
    result.negate();
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.2 BigInt::bitwiseNOT ( x )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseNOT
pub fn bitwiseNOT(self: BigInt, agent: *Agent) Allocator.Error!BigInt {
    const one = agent.pre_allocated.one;

    // 1. Return -x - 1ℤ.
    var result = try self.managed.clone();
    result.negate();
    try result.sub(&result, one.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.3 BigInt::exponentiate ( base, exponent )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-exponentiate
pub fn exponentiate(base: BigInt, agent: *Agent, exponent: BigInt) Agent.Error!BigInt {
    const one = agent.pre_allocated.one;

    // 1. If exponent < 0ℤ, throw a RangeError exception.
    if (!exponent.managed.isPositive() and !exponent.managed.eqlZero()) {
        return agent.throwException(.range_error, "Exponent must be positive", .{});
    }

    // 2. If base = 0ℤ and exponent = 0ℤ, return 1ℤ.
    // NOTE: This also applies if the base is not zero.
    if (exponent.managed.eqlZero()) return one;

    // 3. Return base raised to the power exponent.
    var result = try base.managed.clone();
    var exponent_cloned = try exponent.managed.clone();
    defer exponent_cloned.deinit();
    exponent_cloned.abs();
    while (exponent_cloned.order(one.managed.*) == .gt) {
        try result.mul(&result, base.managed);
        try exponent_cloned.sub(&exponent_cloned, one.managed);
    }
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.4 BigInt::multiply ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-multiply
pub fn multiply(x: BigInt, agent: *Agent, y: BigInt) Allocator.Error!BigInt {
    // 1. Return x × y.
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.mul(x.managed, y.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.5 BigInt::divide ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-divide
pub fn divide(x: BigInt, agent: *Agent, y: BigInt) Agent.Error!BigInt {
    // 1. If y = 0ℤ, throw a RangeError exception.
    if (y.managed.eqlZero()) return agent.throwException(.range_error, "Division by zero", .{});

    // 2. Let quotient be ℝ(x) / ℝ(y).
    // 3. Return ℤ(truncate(quotient)).
    var quotient = try std.math.big.int.Managed.init(agent.gc_allocator);
    var r = try std.math.big.int.Managed.init(agent.gc_allocator);
    try quotient.divTrunc(&r, x.managed, y.managed);
    return from(agent.gc_allocator, quotient);
}

/// 6.1.6.2.6 BigInt::remainder ( n, d )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-remainder
pub fn remainder(n: BigInt, agent: *Agent, d: BigInt) Agent.Error!BigInt {
    // 1. If d = 0ℤ, throw a RangeError exception.
    if (d.managed.eqlZero()) return agent.throwException(.range_error, "Division by zero", .{});

    // 2. If n = 0ℤ, return 0ℤ.
    if (n.managed.eqlZero()) return n;

    // 3. Let quotient be ℝ(n) / ℝ(d).
    // 4. Let q be ℤ(truncate(quotient)).
    // 5. Return n - (d × q).
    var quotient = try std.math.big.int.Managed.init(agent.gc_allocator);
    defer quotient.deinit();
    var r = try std.math.big.int.Managed.init(agent.gc_allocator);
    try quotient.divTrunc(&r, n.managed, d.managed);
    return from(agent.gc_allocator, r);
}

/// 6.1.6.2.7 BigInt::add ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-add
pub fn add(x: BigInt, agent: *Agent, y: BigInt) Allocator.Error!BigInt {
    // 1. Return x + y.
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.add(x.managed, y.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.8 BigInt::subtract ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-subtract
pub fn subtract(x: BigInt, agent: *Agent, y: BigInt) Allocator.Error!BigInt {
    // 1. Return x - y.
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.sub(x.managed, y.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.9 BigInt::leftShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-leftShift
pub fn leftShift(x: BigInt, agent: *Agent, y: BigInt) Agent.Error!BigInt {
    // 1. If y < 0ℤ, then
    //     a. Return ℤ(floor(ℝ(x) / 2**(-ℝ(y)))).
    // 2. Return x × 2ℤ^y.
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.shiftLeft(
        x.managed,
        y.managed.to(usize) catch return agent.throwException(
            .internal_error,
            "Cannot left-shift BigInt by more than {} bits",
            .{std.math.maxInt(usize)},
        ),
    );
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.10 BigInt::signedRightShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-signedRightShift
pub fn signedRightShift(x: BigInt, agent: *Agent, y: BigInt) Agent.Error!BigInt {
    // 1. Return BigInt::leftShift(x, -y).
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.shiftRight(
        x.managed,
        y.managed.to(usize) catch return agent.throwException(
            .internal_error,
            "Cannot right-shift BigInt by more than {} bits",
            .{std.math.maxInt(usize)},
        ),
    );
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.11 BigInt::unsignedRightShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-unsignedRightShift
pub fn unsignedRightShift(_: BigInt, agent: *Agent, _: BigInt) error{ExceptionThrown}!BigInt {
    // 1. Throw a TypeError exception.
    return agent.throwException(
        .type_error,
        "Unsigned right-shift is not supported for BigInts",
        .{},
    );
}

/// 6.1.6.2.12 BigInt::lessThan ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-lessThan
pub fn lessThan(x: BigInt, y: BigInt) bool {
    // 1. If ℝ(x) < ℝ(y), return true; otherwise return false.
    return x.managed.order(y.managed.*) == .lt;
}

/// 6.1.6.2.13 BigInt::equal ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-equal
pub fn equal(x: BigInt, y: BigInt) bool {
    // 1. If ℝ(x) = ℝ(y), return true; otherwise return false.
    return x.managed.eql(y.managed.*);
}

/// 6.1.6.2.18 BigInt::bitwiseAND ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseAND
pub fn bitwiseAND(x: BigInt, agent: *Agent, y: BigInt) Allocator.Error!BigInt {
    // 1. Return BigIntBitwiseOp(&, x, y).
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.bitAnd(x.managed, y.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.19 BigInt::bitwiseXOR ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseXOR
pub fn bitwiseXOR(x: BigInt, agent: *Agent, y: BigInt) Allocator.Error!BigInt {
    // 1. Return BigIntBitwiseOp(^, x, y).
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.bitXor(x.managed, y.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.20 BigInt::bitwiseOR ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseOR
pub fn bitwiseOR(x: BigInt, agent: *Agent, y: BigInt) Allocator.Error!BigInt {
    // 1. Return BigIntBitwiseOp(|, x, y).
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.bitOr(x.managed, y.managed);
    return from(agent.gc_allocator, result);
}

/// 6.1.6.2.21 BigInt::toString ( x, radix )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-tostring
pub fn toString(self: BigInt, allocator: Allocator, radix: u8) Allocator.Error!String {
    // 1. If x < 0ℤ, return the string-concatenation of "-" and BigInt::toString(-x, radix).
    // 2. Return the String value consisting of the representation of x using radix radix.
    return String.fromAscii(allocator, self.managed.toString(allocator, radix, .lower) catch |err| switch (err) {
        // This is an internal API, the base should always be valid.
        error.InvalidBase => @panic("BigInt.toString() called with invalid base"),
        error.OutOfMemory => return error.OutOfMemory,
    });
}

test format {
    inline for (.{
        .{ 0, "0n" },
        .{ 123, "123n" },
        .{ -42, "-42n" },
    }) |test_case| {
        const value, const expected = test_case;
        const big_int = try from(std.testing.allocator, value);
        defer std.testing.allocator.destroy(big_int.managed);
        defer big_int.managed.deinit();
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{big_int});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}
