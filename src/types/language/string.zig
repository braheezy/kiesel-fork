//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const std = @import("std");

const Allocator = std.mem.Allocator;

pub const String = union(enum) {
    const Self = @This();

    utf8: []const u8,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .utf8 => |utf8| try writer.print("{s}", .{utf8}),
        }
    }

    pub inline fn from(utf8: []const u8) Self {
        return .{ .utf8 = utf8 };
    }

    pub inline fn utf16Length(self: Self) usize {
        return std.unicode.calcUtf16LeLen(self.utf8) catch unreachable;
    }

    pub inline fn utf16CodeUnits(self: Self, allocator: Allocator) ![]const u16 {
        return std.unicode.utf8ToUtf16LeWithNull(allocator, self.utf8) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidUtf8 => unreachable,
        };
    }

    pub inline fn substring(self: Self, allocator: Allocator, start: usize, end: usize) ![]const u8 {
        const code_units = try self.utf16CodeUnits(allocator);
        defer allocator.free(code_units);
        return std.unicode.utf16leToUtf8Alloc(allocator, code_units[start..end]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DanglingSurrogateHalf,
            error.ExpectedSecondSurrogateHalf,
            error.UnexpectedSecondSurrogateHalf,
            => unreachable,
        };
    }
};

test "format" {
    const test_cases = [_]struct { String, []const u8 }{
        .{ String.from(""), "" },
        .{ String.from("foo"), "foo" },
        .{ String.from("123"), "123" },
    };
    for (test_cases) |test_case| {
        const string = test_case[0];
        const expected = test_case[1];
        const actual = try std.fmt.allocPrint(std.testing.allocator, "{}", .{string});
        defer std.testing.allocator.free(actual);
        try std.testing.expectEqualStrings(expected, actual);
    }
}
