//! Non-standard util functions

const std = @import("std");

pub const float16 = @import("utils/float16.zig");

/// 5.2.4.3 Shorthands for Unwrapping Completion Records
/// https://tc39.es/ecma262/#sec-shorthands-for-unwrapping-completion-records
///
/// '!' in the spec, ensures that the error is not a throw completion (`error.ExceptionThrown`).
/// OOM is still propagated. The name is a nod to C++, of course :^)
pub fn noexcept(err: error{ ExceptionThrown, OutOfMemory }) std.mem.Allocator.Error!noreturn {
    switch (err) {
        error.ExceptionThrown => @panic("Throw completion was returned from '!' function call"),
        error.OutOfMemory => return error.OutOfMemory,
    }
}

pub fn TemporaryChange(comptime T: type) type {
    return struct {
        field: *T,
        previous_value: T,

        pub fn restore(self: @This()) void {
            self.field.* = self.previous_value;
        }
    };
}

pub fn temporaryChange(
    field: anytype,
    new_value: @TypeOf(field.*),
) TemporaryChange(@TypeOf(field.*)) {
    const T = @TypeOf(field);
    if (@typeInfo(T) != .pointer) {
        @compileError("temporaryChange() called with incompatible type " ++ @typeName(T));
    }
    defer field.* = new_value;
    return .{ .field = field, .previous_value = field.* };
}

// NOTE: This function is vendored from the Zig since it was removed from std.
pub inline fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .pointer) break :blk false;

        const ptr = &info.pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;

        // If it's already a slice, simple check.
        if (ptr.size == .slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.
        if (ptr.size == .one) {
            const child = @typeInfo(ptr.child);
            if (child == .array) {
                const arr = &child.array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

pub fn containsSlice(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |value| {
        if (std.mem.eql(u8, value, needle)) return true;
    }
    return false;
}

pub const ParseDigitsError = error{
    InvalidCharacter,
    Overflow,
};

pub fn parseDigits(comptime T: type, buf: []const u8, base: u8) ParseDigitsError!T {
    if (buf.len == 0) return error.InvalidCharacter;
    var result: T = 0;
    for (buf) |c| {
        const digit = try std.fmt.charToDigit(c, base);
        result = try std.math.mul(T, result, std.math.cast(T, base) orelse return error.Overflow);
        result = try std.math.add(T, result, std.math.cast(T, digit) orelse return error.Overflow);
    }
    return result;
}

pub const StringParser = struct {
    string: []const u8,
    index: usize,

    pub fn init(string: []const u8) StringParser {
        return .{ .string = string, .index = 0 };
    }

    pub fn peek(self: StringParser) ?u8 {
        if (self.index + 1 > self.string.len) return null;
        return self.string[self.index];
    }

    pub fn peekSlice(self: StringParser, count: usize) ?[]const u8 {
        if (self.index + count > self.string.len) return null;
        return self.string[self.index .. self.index + count];
    }

    pub fn consume(self: *StringParser) ?u8 {
        const char = self.peek() orelse return null;
        self.index += 1;
        return char;
    }

    pub fn consumeSlice(self: *StringParser, count: usize) ?[]const u8 {
        const slice = self.peekSlice(count) orelse return null;
        self.index += count;
        return slice;
    }

    pub fn consumeDigits(self: *StringParser, comptime T: type, count: usize) ?T {
        const slice = self.peekSlice(count) orelse return null;
        const result = parseDigits(T, slice, 10) catch return null;
        self.index += count;
        return result;
    }
};

test parseDigits {
    try std.testing.expectEqual(0, try parseDigits(u8, "0", 2));
    try std.testing.expectEqual(0, try parseDigits(u8, "0", 10));
    try std.testing.expectEqual(0, try parseDigits(u8, "0", 16));
    try std.testing.expectEqual(255, try parseDigits(u8, "11111111", 2));
    try std.testing.expectEqual(255, try parseDigits(u8, "255", 10));
    try std.testing.expectEqual(255, try parseDigits(u8, "FF", 16));
    try std.testing.expectEqual(123456789, try parseDigits(i32, "111010110111100110100010101", 2));
    try std.testing.expectEqual(123456789, try parseDigits(i32, "123456789", 10));
    try std.testing.expectEqual(123456789, try parseDigits(i32, "75bcd15", 16));
    try std.testing.expectError(error.InvalidCharacter, parseDigits(u8, "+10", 2));
    try std.testing.expectError(error.InvalidCharacter, parseDigits(u8, "1_0", 2));
    try std.testing.expectError(error.InvalidCharacter, parseDigits(u8, "A", 10));
    try std.testing.expectError(error.InvalidCharacter, parseDigits(u8, "G", 16));
    try std.testing.expectError(error.Overflow, parseDigits(u8, "256", 10));
    try std.testing.expectError(error.Overflow, parseDigits(u8, "100", 16));
}
