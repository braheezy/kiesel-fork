//! 6.1.6.2 The BigInt Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type

const std = @import("std");

const Allocator = std.mem.Allocator;

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

/// 6.1.6.2.21 BigInt::toString ( x, radix )
/// https://tc39.es/ecma262/#sec-numeric-types-bigint-tostring
pub fn toString(self: Self, allocator: Allocator, radix: u8) ![]const u8 {
    // 1. If x < 0ℤ, return the string-concatenation of "-" and BigInt::toString(-x, radix).
    // 2. Return the String value consisting of the representation of x using radix radix.
    return self.value.toString(allocator, radix, .lower) catch |err| switch (err) {
        // This is an internal API, the base should always be valid.
        error.InvalidBase => @panic("BigInt.toString() called with invalid base"),
        error.OutOfMemory => return error.OutOfMemory,
    };
}

test "format" {
    const test_cases = [_]struct { Self, []const u8 }{
        .{ .{ .value = try Value.initSet(std.testing.allocator, 0) }, "0n" },
        .{ .{ .value = try Value.initSet(std.testing.allocator, 123) }, "123n" },
        .{ .{ .value = try Value.initSet(std.testing.allocator, -42) }, "-42n" },
    };
    for (test_cases) |test_case| {
        const big_int = test_case[0];
        const expected = test_case[1];
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{big_int});
        defer std.testing.allocator.free(string);
        defer @constCast(&big_int.value).deinit();
        try std.testing.expectEqualStrings(expected, string);
    }
}
