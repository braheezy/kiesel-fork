//! 6.1.6.2 The BigInt Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;

const Self = @This();

value: *std.math.big.int.Const,

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    try writer.print("{}", .{self.value});
    try writer.writeAll("n");
}

pub fn from(allocator: Allocator, value: anytype) Allocator.Error!Self {
    const value_ptr = try allocator.create(std.math.big.int.Const);
    value_ptr.* = switch (@TypeOf(value)) {
        std.math.big.int.Const => value,
        else => (try std.math.big.int.Managed.initSet(allocator, value)).toConst(),
    };
    return .{ .value = value_ptr };
}

pub fn asFloat(self: Self, agent: *Agent) Allocator.Error!f64 {
    // NOTE: We could also use to(i1024) here, which should cover the largest possible int for
    //       an f64, but that fails to codegen on the Zig side for at least aarch64-macos and
    //       wasm32-wasi. Going via toString() and parsing that into a float isn't great but
    //       works for now.
    return std.fmt.parseFloat(
        f64,
        (try self.toString(agent.gc_allocator, 10)).ascii,
    ) catch unreachable;
}

/// 6.1.6.2.1 BigInt::unaryMinus ( x )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-unaryMinus
pub fn unaryMinus(x: Self, agent: *Agent) Allocator.Error!Self {
    // 1.If x = 0ℤ, return 0ℤ.
    if (x.value.eqlZero()) return x;

    // 2. Return -x.
    return from(agent.gc_allocator, x.value.negate());
}

/// 6.1.6.2.2 BigInt::bitwiseNOT ( x )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseNOT
pub fn bitwiseNOT(self: Self, agent: *Agent) Allocator.Error!Self {
    const one = agent.pre_allocated.one;

    // 1. Return -x - 1ℤ.
    var one_managed = try one.value.toManaged(agent.gc_allocator);
    defer one_managed.deinit();
    var result = try self.value.toManaged(agent.gc_allocator);
    result.negate();
    try result.sub(&result, &one_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.3 BigInt::exponentiate ( base, exponent )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-exponentiate
pub fn exponentiate(base: Self, agent: *Agent, exponent: Self) Agent.Error!Self {
    const one = agent.pre_allocated.one;

    // 1. If exponent < 0ℤ, throw a RangeError exception.
    if (!exponent.value.positive and !exponent.value.eqlZero()) {
        return agent.throwException(.range_error, "Exponent must be positive", .{});
    }

    // 2. If base = 0ℤ and exponent = 0ℤ, return 1ℤ.
    // NOTE: This also applies if the base is not zero.
    if (exponent.value.eqlZero()) return one;

    // 3. Return base raised to the power exponent.
    var base_managed = try base.value.toManaged(agent.gc_allocator);
    defer base_managed.deinit();
    var one_managed = try one.value.toManaged(agent.gc_allocator);
    defer one_managed.deinit();
    var result = try base.value.toManaged(agent.gc_allocator);
    var exponent_managed = try exponent.value.toManaged(agent.gc_allocator);
    defer exponent_managed.deinit();
    exponent_managed.abs();
    while (exponent_managed.order(one_managed) == .gt) {
        try result.mul(&result, &base_managed);
        try exponent_managed.sub(&exponent_managed, &one_managed);
    }
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.4 BigInt::multiply ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-multiply
pub fn multiply(x: Self, agent: *Agent, y: Self) Allocator.Error!Self {
    // 1. Return x × y.
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.mul(&x_managed, &y_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.5 BigInt::divide ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-divide
pub fn divide(x: Self, agent: *Agent, y: Self) Agent.Error!Self {
    // 1. If y = 0ℤ, throw a RangeError exception.
    if (y.value.eqlZero()) return agent.throwException(.range_error, "Division by zero", .{});

    // 2. Let quotient be ℝ(x) / ℝ(y).
    // 3. Return ℤ(truncate(quotient)).
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var quotient = try std.math.big.int.Managed.init(agent.gc_allocator);
    var r = try std.math.big.int.Managed.init(agent.gc_allocator);
    try quotient.divTrunc(&r, &x_managed, &y_managed);
    return from(agent.gc_allocator, quotient.toConst());
}

/// 6.1.6.2.6 BigInt::remainder ( n, d )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-remainder
pub fn remainder(n: Self, agent: *Agent, d: Self) Agent.Error!Self {
    // 1. If d = 0ℤ, throw a RangeError exception.
    if (d.value.eqlZero()) return agent.throwException(.range_error, "Division by zero", .{});

    // 2. If n = 0ℤ, return 0ℤ.
    if (n.value.eqlZero()) return n;

    // 3. Let quotient be ℝ(n) / ℝ(d).
    // 4. Let q be ℤ(truncate(quotient)).
    // 5. Return n - (d × q).
    const n_managed = try n.value.toManaged(agent.gc_allocator);
    const d_managed = try d.value.toManaged(agent.gc_allocator);
    var quotient = try std.math.big.int.Managed.init(agent.gc_allocator);
    var r = try std.math.big.int.Managed.init(agent.gc_allocator);
    try quotient.divTrunc(&r, &n_managed, &d_managed);
    return from(agent.gc_allocator, r.toConst());
}

/// 6.1.6.2.7 BigInt::add ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-add
pub fn add(x: Self, agent: *Agent, y: Self) Allocator.Error!Self {
    // 1. Return x + y.
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.add(&x_managed, &y_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.8 BigInt::subtract ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-subtract
pub fn subtract(x: Self, agent: *Agent, y: Self) Allocator.Error!Self {
    // 1. Return x - y.
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.sub(&x_managed, &y_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.9 BigInt::leftShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-leftShift
pub fn leftShift(x: Self, agent: *Agent, y: Self) Agent.Error!Self {
    // 1. If y < 0ℤ, then
    //     a. Return ℤ(floor(ℝ(x) / 2**(-ℝ(y)))).
    // 2. Return x × 2ℤ^y.
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.shiftLeft(
        &x_managed,
        y.value.to(usize) catch return agent.throwException(
            .internal_error,
            "Cannot left-shift BigInt by more than {} bits",
            .{std.math.maxInt(usize)},
        ),
    );
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.10 BigInt::signedRightShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-signedRightShift
pub fn signedRightShift(x: Self, agent: *Agent, y: Self) Agent.Error!Self {
    // 1. Return BigInt::leftShift(x, -y).
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.shiftRight(
        &x_managed,
        y.value.to(usize) catch return agent.throwException(
            .internal_error,
            "Cannot right-shift BigInt by more than {} bits",
            .{std.math.maxInt(usize)},
        ),
    );
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.11 BigInt::unsignedRightShift ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-unsignedRightShift
pub fn unsignedRightShift(_: Self, agent: *Agent, _: Self) error{ExceptionThrown}!Self {
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
    return x.value.order(y.value.*) == .lt;
}

/// 6.1.6.2.13 BigInt::equal ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-equal
pub fn equal(x: Self, y: Self) bool {
    // 1. If ℝ(x) = ℝ(y), return true; otherwise return false.
    return x.value.eql(y.value.*);
}

/// 6.1.6.2.18 BigInt::bitwiseAND ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseAND
pub fn bitwiseAND(x: Self, agent: *Agent, y: Self) Allocator.Error!Self {
    // 1. Return BigIntBitwiseOp(&, x, y).
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.bitAnd(&x_managed, &y_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.19 BigInt::bitwiseXOR ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseXOR
pub fn bitwiseXOR(x: Self, agent: *Agent, y: Self) Allocator.Error!Self {
    // 1. Return BigIntBitwiseOp(^, x, y).
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.bitXor(&x_managed, &y_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.20 BigInt::bitwiseOR ( x, y )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-bitwiseOR
pub fn bitwiseOR(x: Self, agent: *Agent, y: Self) Allocator.Error!Self {
    // 1. Return BigIntBitwiseOp(|, x, y).
    const x_managed = try x.value.toManaged(agent.gc_allocator);
    const y_managed = try y.value.toManaged(agent.gc_allocator);
    var result = try std.math.big.int.Managed.init(agent.gc_allocator);
    try result.bitOr(&x_managed, &y_managed);
    return from(agent.gc_allocator, result.toConst());
}

/// 6.1.6.2.21 BigInt::toString ( x, radix )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-tostring
pub fn toString(self: Self, allocator: Allocator, radix: u8) Allocator.Error!String {
    // 1. If x < 0ℤ, return the string-concatenation of "-" and BigInt::toString(-x, radix).
    // 2. Return the String value consisting of the representation of x using radix radix.
    var managed = try self.value.toManaged(allocator);
    defer managed.deinit();
    return String.fromAscii(managed.toString(allocator, radix, .lower) catch |err| switch (err) {
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
        var managed = try std.math.big.int.Managed.initSet(std.testing.allocator, value);
        defer managed.deinit();
        const big_int = try from(std.testing.allocator, managed.toConst());
        defer std.testing.allocator.destroy(big_int.value);
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{big_int});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}
