//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const std = @import("std");

const Allocator = std.mem.Allocator;

pub const String = union(enum) {
    const Self = @This();

    /// https://tc39.es/ecma262/#ASCII-word-characters
    pub const ascii_word_characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";

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

    pub inline fn eql(a: Self, b: Self) bool {
        return std.mem.eql(u8, a.utf8, b.utf8);
    }

    pub inline fn substring(self: Self, allocator: Allocator, start: usize, end: usize) ![]const u8 {
        const code_units = try self.utf16CodeUnits(allocator);
        defer allocator.free(code_units);
        return std.unicode.utf16leToUtf8Alloc(allocator, code_units[start..end]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DanglingSurrogateHalf,
            error.ExpectedSecondSurrogateHalf,
            error.UnexpectedSecondSurrogateHalf,
            => @panic("Surrogate pairs are not handled in String.substring()"),
        };
    }

    /// 6.1.4.1 StringIndexOf ( string, searchValue, fromIndex )
    /// https://tc39.es/ecma262/#sec-stringindexof
    pub fn indexOf(self: Self, search_value: String, from_index: usize) ?usize {
        // 1. Let len be the length of string.
        const len = self.utf16Length();

        // 2. If searchValue is the empty String and fromIndex ≤ len, return fromIndex.
        if (search_value.utf16Length() == 0 and from_index <= len) return from_index;

        // 3. Let searchLen be the length of searchValue.
        // 4. For each integer i such that fromIndex ≤ i ≤ len - searchLen, in ascending order, do
        //     a. Let candidate be the substring of string from i to i + searchLen.
        //     b. If candidate is searchValue, return i.
        // 5. Return -1.
        if (from_index >= len) return null;
        return if (std.mem.indexOf(u8, self.utf8[from_index..], search_value.utf8)) |index|
            index + from_index
        else
            null;
    }

    pub fn codePointAt(self: Self, index: usize) u21 {
        var it = std.unicode.Utf8View.initUnchecked(self.utf8).iterator();
        var i: usize = 0;
        while (it.nextCodepoint()) |code_point| : (i += 1) {
            if (i == index) return code_point;
        }
        unreachable;
    }
};

test "format" {
    const test_cases = [_]struct { String, []const u8 }{
        .{ String.from(""), "" },
        .{ String.from("foo"), "foo" },
        .{ String.from("123"), "123" },
    };
    for (test_cases) |test_case| {
        const string, const expected = test_case;
        const actual = try std.fmt.allocPrint(std.testing.allocator, "{}", .{string});
        defer std.testing.allocator.free(actual);
        try std.testing.expectEqualStrings(expected, actual);
    }
}
