//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const tokenizer = @import("../../language/tokenizer.zig");

pub const String = struct {
    const Self = @This();

    /// https://tc39.es/ecma262/#ASCII-word-characters
    pub const ascii_word_characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";

    /// The definition of white space is the union of WhiteSpace and LineTerminator.
    pub const whitespace = tokenizer.whitespace ++ tokenizer.line_terminators;

    pub const empty: Self = from("");

    pub const Builder = @import("String/Builder.zig");

    utf8: []const u8,
    utf16_length: usize,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.utf8});
    }

    pub inline fn from(utf8: []const u8) Self {
        const utf16_length = std.unicode.calcUtf16LeLen(utf8) catch unreachable;
        return .{ .utf8 = utf8, .utf16_length = utf16_length };
    }

    pub inline fn isEmpty(self: Self) bool {
        return self.utf8.len == 0;
    }

    pub inline fn utf16Length(self: Self) usize {
        // NOTE: Ideally we'd calulate the UTF-16 length lazily but that requires a mutable self
        //       pointer (const-casting didn't seem to work, performance-wise).
        //       Instead, we just do it once upfront now.
        return self.utf16_length;
    }

    pub inline fn utf16CodeUnits(self: Self, allocator: Allocator) Allocator.Error![]const u16 {
        return std.unicode.utf8ToUtf16LeWithNull(allocator, self.utf8) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidUtf8 => unreachable,
        };
    }

    pub inline fn eql(a: Self, b: Self) bool {
        return std.mem.eql(u8, a.utf8, b.utf8);
    }

    pub fn substring(
        self: Self,
        allocator: Allocator,
        start: usize,
        end: usize,
    ) Allocator.Error!String {
        const code_units = try self.utf16CodeUnits(allocator);
        defer allocator.free(code_units);
        return from(std.unicode.utf16leToUtf8Alloc(allocator, code_units[start..end]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DanglingSurrogateHalf,
            error.ExpectedSecondSurrogateHalf,
            error.UnexpectedSecondSurrogateHalf,
            => @panic("Surrogate pairs are not handled in String.substring()"),
        });
    }

    /// 6.1.4.1 StringIndexOf ( string, searchValue, fromIndex )
    /// https://tc39.es/ecma262/#sec-stringindexof
    pub fn indexOf(self: Self, search_value: String, from_index: usize) ?usize {
        // 1. Let len be the length of string.
        const len = self.utf16Length();

        // 2. If searchValue is the empty String and fromIndex ≤ len, return fromIndex.
        if (search_value.isEmpty() and from_index <= len) return from_index;

        // 3. Let searchLen be the length of searchValue.
        // 4. For each integer i such that fromIndex ≤ i ≤ len - searchLen, in ascending order, do
        //     a. Let candidate be the substring of string from i to i + searchLen.
        //     b. If candidate is searchValue, return i.
        // 5. Return not-found.
        if (from_index >= len) return null;
        return if (std.mem.indexOf(u8, self.utf8[from_index..], search_value.utf8)) |index|
            index + from_index
        else
            null;
    }

    /// 6.1.4.2 StringLastIndexOf ( string, searchValue, fromIndex )
    /// https://tc39.es/ecma262/#sec-stringlastindexof
    pub fn lastIndexOf(self: Self, search_value: String, from_index: usize) ?usize {
        // 1. Let len be the length of string.
        const len = self.utf16Length();

        // 2. Let searchLen be the length of searchValue.
        // 3. Assert: fromIndex + searchLen ≤ len.
        // 1. For each integer i such that 0 ≤ i ≤ fromIndex, in descending order, do
        //     a. Let candidate be the substring of string from i to i + searchLen.
        //     b. If candidate is searchValue, return i.
        // 5. Return not-found.
        if (search_value.isEmpty() and from_index <= len) return from_index;
        if (from_index >= len) return null;
        return if (std.mem.lastIndexOf(u8, self.utf8[from_index..], search_value.utf8)) |index|
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

    pub fn utf16CodeUnitAt(self: Self, index: usize) u16 {
        // NOTE: This is based on `std.unicode.utf8ToUtf16LeWithNull()`, including the SIMD fast path.

        var remaining = self.utf8;
        var i: usize = 0;
        // Need support for std.simd.interlace
        if (builtin.zig_backend != .stage2_x86_64 and comptime !builtin.cpu.arch.isMIPS()) {
            const chunk_len = std.simd.suggestVectorLength(u8) orelse 1;
            const Chunk = @Vector(chunk_len, u8);

            // Fast path. Check for and encode ASCII characters at the start of the input.
            while (remaining.len >= chunk_len) : (i += chunk_len) {
                const chunk: Chunk = remaining[0..chunk_len].*;
                const mask: Chunk = @splat(0x80);
                if (@reduce(.Or, chunk & mask == mask)) {
                    // found a non ASCII code unit
                    break;
                }
                const zeroes: Chunk = @splat(0);
                const utf16_chunk: [chunk_len * 2]u8 align(@alignOf(u16)) = std.simd.interlace(.{ chunk, zeroes });
                if (i + chunk_len > index) {
                    return std.mem.bytesAsSlice(u16, &utf16_chunk)[@mod(index, chunk_len)];
                }
                remaining = remaining[chunk_len..];
            }
        }

        var it = std.unicode.Utf8View.initUnchecked(remaining).iterator();
        while (it.nextCodepoint()) |code_point| : (i += 1) {
            if (code_point < 0x10000) {
                if (i == index) return @intCast(code_point);
            } else {
                defer i += 1;
                const high = @as(u16, @intCast((code_point - 0x10000) >> 10)) + 0xD800;
                const low = @as(u16, @intCast(code_point & 0x3FF)) + 0xDC00;
                if (i == index) return @intCast(high);
                if (i + 1 == index) return @intCast(low);
            }
        }
        unreachable;
    }

    pub fn repeat(self: Self, allocator: Allocator, n: usize) Allocator.Error!String {
        var builder = Builder.init(allocator);
        defer builder.deinit();
        for (0..n) |_| try builder.appendString(self);
        return builder.build();
    }

    pub fn concat(allocator: Allocator, strings: []const String) Allocator.Error!String {
        var builder = Builder.init(allocator);
        defer builder.deinit();
        for (strings) |string| try builder.appendString(string);
        return builder.build();
    }
};

test "format" {
    const test_cases = [_]struct { String, []const u8 }{
        .{ String.empty, "" },
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
