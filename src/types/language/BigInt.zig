//! 6.1.6.2 The BigInt Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;

const Self = @This();

pub const Value = std.math.big.int.Managed;

value: Value,

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try writer.print("{}", .{self.value});
    try writer.writeAll("n");
}

pub inline fn from(allocator: Allocator, value: anytype) !Self {
    return if (@typeInfo(@TypeOf(value)) == .ComptimeInt and value == 0)
        .{ .value = try Value.init(allocator) }
    else
        .{ .value = try Value.initSet(allocator, value) };
}

pub inline fn clone(self: Self) !Self {
    return .{ .value = try self.value.clone() };
}

pub fn asFloat(self: Self, agent: *Agent) !f64 {
    // NOTE: We could also use to(i1024) here, which should cover the largest possible int for
    //       an f64, but that fails to codegen on the Zig side for at least aarch64-macos and
    //       wasm32-wasi. Going via toString() and parsing that into a float isn't great but
    //       works for now.
    return std.fmt.parseFloat(
        f64,
        (try self.toString(agent.gc_allocator, 10)).utf8,
    ) catch unreachable;
}

/// 6.1.6.2.1 BigInt::unaryMinus ( x )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-unaryMinus
pub fn unaryMinus(self: Self) !Self {
    // 1.If x is 0ℤ, return 0ℤ.
    if (self.value.eqlZero()) return self;

    // 2. Return -x.
    var result = try self.clone();
    result.value.negate();
    return result;
}

/// 6.1.6.2.2 BigInt::bitwiseNOT ( x )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseNOT
pub fn bitwiseNOT(self: Self, agent: *Agent) !Self {
    const one = agent.pre_allocated.one;

    // 1. Return -x - 1ℤ.
    var result = try self.clone();
    result.value.negate();
    try result.value.sub(&result.value, &one.value);
    return result;
}

/// 6.1.6.2.3 BigInt::exponentiate ( base, exponent )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-exponentiate
pub fn exponentiate(base: Self, agent: *Agent, exponent: Self) !Self {
    const one = agent.pre_allocated.one;

    // 1. If exponent < 0ℤ, throw a RangeError exception.
    if (!exponent.value.isPositive() and !exponent.value.eqlZero()) {
        return agent.throwException(.range_error, "Exponent must be positive", .{});
    }

    // 2. If base is 0ℤ and exponent is 0ℤ, return 1ℤ.
    // NOTE: This also applies if the base is not zero.
    if (exponent.value.eqlZero()) return one;

    // 3. Return base raised to the power exponent.
    var result = try base.clone();
    var cloned_exponent = try exponent.clone();
    cloned_exponent.value.abs();
    while (cloned_exponent.value.order(one.value) == .gt) {
        try result.value.mul(&result.value, &base.value);
        try cloned_exponent.value.sub(&cloned_exponent.value, &one.value);
    }
    return result;
}

/// 6.1.6.2.4 BigInt::multiply ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-multiply
pub fn multiply(x: Self, agent: *Agent, y: Self) !Self {
    // 1. Return x × y.
    var result = try from(agent.gc_allocator, 0);
    try result.value.mul(&x.value, &y.value);
    return result;
}

/// 6.1.6.2.5 BigInt::divide ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-divide
pub fn divide(x: Self, agent: *Agent, y: Self) !Self {
    // 1. If y is 0ℤ, throw a RangeError exception.
    if (y.value.eqlZero()) return agent.throwException(.range_error, "Division by zero", .{});

    // 2. Let quotient be ℝ(x) / ℝ(y).
    // 3. Return ℤ(truncate(quotient)).
    var quotient = try from(agent.gc_allocator, 0);
    var r = try from(agent.gc_allocator, 0);
    try quotient.value.divTrunc(&r.value, &x.value, &y.value);
    return quotient;
}

/// 6.1.6.2.6 BigInt::remainder ( n, d )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-remainder
pub fn remainder(n: Self, agent: *Agent, d: Self) !Self {
    // 1. If d is 0ℤ, throw a RangeError exception.
    if (d.value.eqlZero()) return agent.throwException(.range_error, "Division by zero", .{});

    // 2. If n is 0ℤ, return 0ℤ.
    if (n.value.eqlZero()) return n;

    // 3. Let quotient be ℝ(n) / ℝ(d).
    // 4. Let q be ℤ(truncate(quotient)).
    // 5. Return n - (d × q).
    var quotient = try from(agent.gc_allocator, 0);
    var r = try from(agent.gc_allocator, 0);
    try quotient.value.divTrunc(&r.value, &n.value, &d.value);
    return r;
}

/// 6.1.6.2.7 BigInt::add ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-add
pub fn add(x: Self, agent: *Agent, y: Self) !Self {
    // 1. Return x + y.
    var result = try from(agent.gc_allocator, 0);
    try result.value.add(&x.value, &y.value);
    return result;
}

/// 6.1.6.2.8 BigInt::subtract ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-subtract
pub fn subtract(x: Self, agent: *Agent, y: Self) !Self {
    // 1. Return x - y.
    var result = try from(agent.gc_allocator, 0);
    try result.value.sub(&x.value, &y.value);
    return result;
}

/// 6.1.6.2.9 BigInt::leftShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-leftShift
pub fn leftShift(_: Self, agent: *Agent, _: Self) !Self {
    // 1. If y < 0ℤ, then
    //     a. Return ℤ(floor(ℝ(x) / 2**(-ℝ(y)))).
    // 2. Return x × 2ℤ^y.
    // TODO: Figure out how to do this with built-in functionality, shiftLeft() only accepts an usize
    return agent.throwException(.internal_error, "Left-shift for BigInts is not implemented", .{});
}

/// 6.1.6.2.10 BigInt::signedRightShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-signedRightShift
pub fn signedRightShift(x: Self, agent: *Agent, y: Self) !Self {
    const error_message = std.fmt.comptimePrint(
        "Cannot right-shift BigInt by more than {} bits",
        .{std.math.maxInt(usize)},
    );

    // 1. Return BigInt::leftShift(x, -y).
    var result = try from(agent.gc_allocator, 0);
    try result.value.shiftRight(
        &x.value,
        y.value.to(usize) catch return agent.throwException(.internal_error, error_message, .{}),
    );
    return result;
}

/// 6.1.6.2.11 BigInt::unsignedRightShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-unsignedRightShift
pub fn unsignedRightShift(_: Self, agent: *Agent, _: Self) !Self {
    // 1. Throw a TypeError exception.
    return agent.throwException(
        .type_error,
        "Unsigned right-shift is not supported for BigInts",
        .{},
    );
}

/// 6.1.6.2.12 BigInt::lessThan ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-lessThan
pub fn lessThan(x: Self, y: Self) bool {
    // 1. If ℝ(x) < ℝ(y), return true; otherwise return false.
    return x.value.order(y.value) == .lt;
}

/// 6.1.6.2.13 BigInt::equal ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-equal
pub fn equal(x: Self, y: Self) bool {
    // 1. If ℝ(x) = ℝ(y), return true; otherwise return false.
    return x.value.eql(y.value);
}

/// 6.1.6.2.18 BigInt::bitwiseAND ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseAND
pub fn bitwiseAND(x: Self, agent: *Agent, y: Self) !Self {
    // 1. Return BigIntBitwiseOp(&, x, y).
    var result = try from(agent.gc_allocator, 0);
    try result.value.bitAnd(&x.value, &y.value);
    return result;
}

/// 6.1.6.2.19 BigInt::bitwiseXOR ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseXOR
pub fn bitwiseXOR(x: Self, agent: *Agent, y: Self) !Self {
    // 1. Return BigIntBitwiseOp(^, x, y).
    var result = try from(agent.gc_allocator, 0);
    try result.value.bitXor(&x.value, &y.value);
    return result;
}

/// 6.1.6.2.20 BigInt::bitwiseOR ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseOR
pub fn bitwiseOR(x: Self, agent: *Agent, y: Self) !Self {
    // 1. Return BigIntBitwiseOp(|, x, y).
    var result = try from(agent.gc_allocator, 0);
    try result.value.bitOr(&x.value, &y.value);
    return result;
}

/// 6.1.6.2.21 BigInt::toString ( x, radix )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-tostring
pub fn toString(self: Self, allocator: Allocator, radix: u8) !String {
    // 1. If x < 0ℤ, return the string-concatenation of "-" and BigInt::toString(-x, radix).
    // 2. Return the String value consisting of the representation of x using radix radix.
    return String.from(self.value.toString(allocator, radix, .lower) catch |err| switch (err) {
        // This is an internal API, the base should always be valid.
        error.InvalidBase => @panic("BigInt.toString() called with invalid base"),
        error.OutOfMemory => return error.OutOfMemory,
    });
}

test "format" {
    const test_cases = [_]struct { Self, []const u8 }{
        .{ try from(std.testing.allocator, 0), "0n" },
        .{ try from(std.testing.allocator, 123), "123n" },
        .{ try from(std.testing.allocator, -42), "-42n" },
    };
    for (test_cases) |test_case| {
        const big_int, const expected = test_case;
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{big_int});
        defer std.testing.allocator.free(string);
        defer @constCast(&big_int.value).deinit();
        try std.testing.expectEqualStrings(expected, string);
    }
}
