//! 6.1.5 The Symbol Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type

const std = @import("std");

const Self = @This();

pub const Id = usize;

/// Internal ID used for equality checks
id: Id,

/// [[Description]]
description: ?[]const u8,

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try writer.writeAll("Symbol(");
    if (self.description) |description| {
        try writer.writeAll("\"");
        try writer.writeAll(description);
        try writer.writeAll("\"");
    }
    try writer.writeAll(")");
}

test "format" {
    const test_cases = [_]struct { Self, []const u8 }{
        .{ .{ .id = 0, .description = null }, "Symbol()" },
        .{ .{ .id = 1, .description = "" }, "Symbol(\"\")" },
        .{ .{ .id = 2, .description = "foo" }, "Symbol(\"foo\")" },
    };
    for (test_cases) |test_case| {
        const symbol = test_case[0];
        const expected = test_case[1];
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{symbol});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}
