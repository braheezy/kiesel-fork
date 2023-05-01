//! 6.1.6.2 The BigInt Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const Self = @This();

pub const Value = std.math.big.int.Managed;

value: Value,

/// 6.1.6.2.21 BigInt::toString ( x, radix )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-tostring
pub fn toString(self: Self, allocator: Allocator, radix: u8) ![]const u8 {
    // 1. If x < 0ℤ, return the string-concatenation of "-" and BigInt::toString(-x, radix).
    // 2. Return the String value consisting of the representation of x using radix radix.
    return self.value.toString(allocator, radix, .lower);
}
