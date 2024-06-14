//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const icu4zig = @import("icu4zig");

const build_options = @import("build-options");
const tokenizer = @import("../../language/tokenizer.zig");

fn utf8IsAscii(utf8: []const u8) bool {
    return for (utf8) |c| {
        if (!std.ascii.isASCII(c)) break false;
    } else true;
}

pub const String = union(enum) {
    const Self = @This();

    /// https://tc39.es/ecma262/#ASCII-word-characters
    pub const ascii_word_characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";

    /// The definition of white space is the union of WhiteSpace and LineTerminator.
    pub const whitespace = tokenizer.whitespace ++ tokenizer.line_terminators;

    pub const empty: Self = fromLiteral("");

    pub const Builder = @import("String/Builder.zig");
    pub const CodeUnitIterator = @import("String/CodeUnitIterator.zig");

    ascii: []const u8,
    utf16: []const u16,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;
        switch (self) {
            .ascii => |ascii| try writer.writeAll(ascii),
            .utf16 => |utf16| try writer.print("{}", .{std.unicode.fmtUtf16Le(utf16)}),
        }
    }

    pub fn fromLiteral(comptime utf8: []const u8) Self {
        return comptime blk: {
            if (utf8IsAscii(utf8)) {
                break :blk .{ .ascii = utf8 };
            } else {
                const utf16 = std.unicode.utf8ToUtf16LeStringLiteral(utf8);
                break :blk .{ .utf16 = utf16 };
            }
        };
    }

    pub fn fromUtf8(allocator: Allocator, utf8: []const u8) Allocator.Error!Self {
        if (utf8IsAscii(utf8)) {
            return .{ .ascii = utf8 };
        } else {
            const utf16 = std.unicode.utf8ToUtf16LeAlloc(allocator, utf8) catch |err| switch (err) {
                error.InvalidUtf8 => @panic("Invalid UTF-8"),
                error.OutOfMemory => return error.OutOfMemory,
            };
            return .{ .utf16 = utf16 };
        }
    }

    pub fn fromAscii(ascii: []const u8) Self {
        return .{ .ascii = ascii };
    }

    pub fn fromUtf16(utf16: []const u16) Self {
        return .{ .utf16 = utf16 };
    }

    pub fn isEmpty(self: Self) bool {
        return self.length() == 0;
    }

    pub fn length(self: Self) usize {
        return switch (self) {
            .ascii => |ascii| ascii.len,
            .utf16 => |utf16| utf16.len,
        };
    }

    pub fn toUtf8(self: Self, allocator: Allocator) Allocator.Error![]const u8 {
        return switch (self) {
            .ascii => |ascii| allocator.dupe(u8, ascii),
            .utf16 => |utf16| std.fmt.allocPrint(
                allocator,
                "{}",
                .{std.unicode.fmtUtf16Le(utf16)},
            ),
        };
    }

    pub fn codeUnitIterator(self: Self) CodeUnitIterator {
        return .{ .index = 0, .string = self };
    }

    pub fn codeUnits(self: Self, allocator: Allocator) Allocator.Error![]const u16 {
        if (self.isEmpty()) return &.{};
        var it = self.codeUnitIterator();
        var code_units = try allocator.alloc(u16, self.length());
        while (it.next()) |c| code_units[it.index - 1] = c;
        return code_units;
    }

    pub fn eql(a: Self, b: Self) bool {
        if (a.isEmpty() and b.isEmpty()) return true;
        if (a.length() != b.length()) return false;
        var it1 = a.codeUnitIterator();
        var it2 = b.codeUnitIterator();
        for (0..a.length()) |_| {
            const c1 = it1.next().?;
            const c2 = it2.next().?;
            if (c1 != c2) return false;
        }
        return true;
    }

    pub fn hash(self: Self) u64 {
        var hasher = std.hash.Wyhash.init(0);
        var it = self.codeUnitIterator();
        while (it.next()) |code_unit| {
            hasher.update(std.mem.asBytes(&code_unit));
        }
        return hasher.final();
    }

    pub fn startsWith(self: Self, other: String) bool {
        if (other.isEmpty()) return true;
        if (self.length() < other.length()) return false;
        var it1 = self.codeUnitIterator();
        var it2 = other.codeUnitIterator();
        for (0..other.length()) |_| {
            const c1 = it1.next().?;
            const c2 = it2.next().?;
            if (c1 != c2) return false;
        }
        return true;
    }

    /// https://tc39.es/ecma262/#substring
    pub fn substring(
        self: Self,
        allocator: Allocator,
        inclusive_start: usize,
        exclusive_end: ?usize,
    ) Allocator.Error!String {
        if (inclusive_start == 0 and (exclusive_end == null or exclusive_end == self.length())) {
            return self;
        }
        if (self.isEmpty() or inclusive_start == self.length() or exclusive_end == 0) return empty;
        switch (self) {
            .ascii => |ascii| {
                const ascii_substring = ascii[inclusive_start .. exclusive_end orelse self.length()];
                return .{ .ascii = try allocator.dupe(u8, ascii_substring) };
            },
            .utf16 => |utf16| {
                const utf16_substring = utf16[inclusive_start .. exclusive_end orelse self.length()];
                const is_ascii = for (utf16_substring) |code_unit| {
                    if (code_unit > 0x7F) break false;
                } else true;
                if (is_ascii) {
                    const ascii = try allocator.alloc(u8, utf16_substring.len);
                    for (utf16_substring, 0..) |code_unit, i| {
                        ascii[i] = @intCast(code_unit);
                    }
                    return .{ .ascii = ascii };
                } else {
                    return .{ .utf16 = try allocator.dupe(u16, utf16_substring) };
                }
            },
        }
    }

    /// 6.1.4.1 StringIndexOf ( string, searchValue, fromIndex )
    /// https://tc39.es/ecma262/#sec-stringindexof
    pub fn indexOf(self: Self, search_value: String, from_index: usize) ?usize {
        // 1. Let len be the length of string.
        const len = self.length();
        const search_len = search_value.length();

        // 2. If searchValue is the empty String and fromIndex ≤ len, return fromIndex.
        if (search_value.isEmpty() and from_index <= len) return from_index;

        // 3. Let searchLen be the length of searchValue.
        // 4. For each integer i such that fromIndex ≤ i ≤ len - searchLen, in ascending order, do
        //     a. Let candidate be the substring of string from i to i + searchLen.
        //     b. If candidate is searchValue, return i.
        // 5. Return not-found.
        if (from_index >= len or search_len > len) return null;
        if (self == .ascii and search_value == .ascii) {
            return if (std.mem.indexOf(u8, self.ascii[from_index..], search_value.ascii)) |index|
                index + from_index
            else
                null;
        }
        if (self == .utf16 and search_value == .utf16) {
            return if (std.mem.indexOf(u16, self.utf16[from_index..], search_value.utf16)) |index|
                index + from_index
            else
                null;
        }
        var i: usize = from_index;
        const end = len - search_len;
        outer: while (i <= end) : (i += 1) {
            for (0..search_len) |n| {
                if (self.codeUnitAt(i + n) != search_value.codeUnitAt(n)) continue :outer;
            }
            return i;
        }
        return null;
    }

    /// 6.1.4.2 StringLastIndexOf ( string, searchValue, fromIndex )
    /// https://tc39.es/ecma262/#sec-stringlastindexof
    pub fn lastIndexOf(self: Self, search_value: String, from_index: usize) ?usize {
        // 1. Let len be the length of string.
        const len = self.length();

        // 2. Let searchLen be the length of searchValue.
        const search_len = search_value.length();

        // 3. Assert: fromIndex + searchLen ≤ len.
        // 1. For each integer i such that 0 ≤ i ≤ fromIndex, in descending order, do
        //     a. Let candidate be the substring of string from i to i + searchLen.
        //     b. If candidate is searchValue, return i.
        // 5. Return not-found.
        if (search_value.isEmpty() and from_index <= len) return from_index;
        if (from_index >= len or search_len > len) return null;
        if (self == .ascii and search_value == .ascii) {
            return if (std.mem.lastIndexOf(u8, self.ascii[0..from_index], search_value.ascii)) |index|
                index
            else
                null;
        }
        if (self == .utf16 and search_value == .utf16) {
            return if (std.mem.lastIndexOf(u16, self.utf16[0..from_index], search_value.utf16)) |index|
                index
            else
                null;
        }
        var i = std.math.sub(usize, len - search_len, from_index) catch return null;
        outer: while (true) : (i -= 1) {
            for (0..search_len) |n| {
                if (self.codeUnitAt(i + n) != search_value.codeUnitAt(n)) continue :outer;
            }
            return i;
        }
    }

    /// 7.2.8 Static Semantics: IsStringWellFormedUnicode ( string )
    /// https://tc39.es/ecma262/#sec-isstringwellformedunicode
    pub fn isWellFormedUnicode(self: Self) bool {
        if (self == .ascii) return true;

        // 1. Let len be the length of string.
        const len = self.length();

        // 2. Let k be 0.
        var k: usize = 0;

        // 3. Repeat, while k < len,
        while (k < len) {
            // a. Let cp be CodePointAt(string, k).
            const code_point = self.codePointAt(k);

            // b. If cp.[[IsUnpairedSurrogate]] is true, return false.
            if (code_point.is_unpaired_surrogate) return false;

            // c. Set k to k + cp.[[CodeUnitCount]].
            k += code_point.code_unit_count;
        }

        // 4. Return true.
        return true;
    }

    const CodePoint = struct {
        code_point: u21,
        code_unit_count: u2,
        is_unpaired_surrogate: bool,
    };

    /// 11.1.4 Static Semantics: CodePointAt ( string, position )
    /// https://tc39.es/ecma262/#sec-codepointat
    pub fn codePointAt(self: Self, position: usize) CodePoint {
        // 1. Let size be the length of string.
        const size = self.length();

        // 2. Assert: position ≥ 0 and position < size.
        std.debug.assert(position >= 0 and position < size);

        switch (self) {
            .ascii => |ascii| {
                return .{ .code_point = ascii[position], .code_unit_count = 1, .is_unpaired_surrogate = false };
            },
            .utf16 => |utf16| {
                // 3. Let first be the code unit at index position within string.
                const first = utf16[position];

                // 4. Let cp be the code point whose numeric value is the numeric value of first.
                var code_point: u21 = first;

                // 5. If first is neither a leading surrogate nor a trailing surrogate, then
                if (!std.unicode.utf16IsHighSurrogate(first) and !std.unicode.utf16IsLowSurrogate(first)) {
                    // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: false }.
                    return .{ .code_point = code_point, .code_unit_count = 1, .is_unpaired_surrogate = false };
                }

                // 6. If first is a trailing surrogate or position + 1 = size, then
                if (std.unicode.utf16IsLowSurrogate(first) or position + 1 == size) {
                    // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
                    return .{ .code_point = code_point, .code_unit_count = 1, .is_unpaired_surrogate = true };
                }

                // 7. Let second be the code unit at index position + 1 within string.
                const second = utf16[position + 1];

                // 8. If second is not a trailing surrogate, then
                if (!std.unicode.utf16IsLowSurrogate(second)) {
                    // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
                    return .{ .code_point = code_point, .code_unit_count = 1, .is_unpaired_surrogate = true };
                }

                // 9. Set cp to UTF16SurrogatePairToCodePoint(first, second).
                code_point = std.unicode.utf16DecodeSurrogatePair(&.{ first, second }) catch unreachable;

                // 10. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 2, [[IsUnpairedSurrogate]]: false }.
                return .{ .code_point = code_point, .code_unit_count = 2, .is_unpaired_surrogate = false };
            },
        }
    }

    pub fn codeUnitAt(self: Self, index: usize) u16 {
        return switch (self) {
            .ascii => |ascii| ascii[index],
            .utf16 => |utf16| utf16[index],
        };
    }

    pub fn toLowerCase(self: Self, allocator: Allocator) Allocator.Error!String {
        if (self.isEmpty()) return empty;
        switch (self) {
            .ascii => |ascii| {
                const output = try allocator.alloc(u8, ascii.len);
                for (ascii, 0..) |c, i| {
                    output[i] = std.ascii.toLower(c);
                }
                return fromAscii(output);
            },
            .utf16 => |utf16| {
                if (build_options.enable_intl) {
                    // NOTE: ICU4X only supports UTF-8 for this, so unpaired surrogates are not
                    //       handled correctly here.
                    const utf8 = try self.toUtf8(allocator);
                    defer allocator.free(utf8);
                    const data_provider = icu4zig.DataProvider.init();
                    defer data_provider.deinit();
                    const case_mapper = icu4zig.CaseMapper.init(data_provider);
                    defer case_mapper.deinit();
                    const locale = icu4zig.Locale.init(null) catch unreachable;
                    defer locale.deinit();
                    const utf8_lowercase = try case_mapper.lowercase(allocator, utf8, locale);
                    defer allocator.free(utf8_lowercase);
                    return fromUtf8(allocator, utf8_lowercase);
                }
                const output = try allocator.alloc(u16, utf16.len);
                for (utf16, 0..) |c, i| {
                    output[i] = if (c < 128) std.ascii.toLower(@intCast(c)) else c;
                }
                return fromUtf16(output);
            },
        }
    }

    pub fn toUpperCase(self: Self, allocator: Allocator) Allocator.Error!String {
        if (self.isEmpty()) return empty;
        switch (self) {
            .ascii => |ascii| {
                const output = try allocator.alloc(u8, ascii.len);
                for (ascii, 0..) |c, i| {
                    output[i] = std.ascii.toUpper(c);
                }
                return fromAscii(output);
            },
            .utf16 => |utf16| {
                if (build_options.enable_intl) {
                    // NOTE: ICU4X only supports UTF-8 for this, so unpaired surrogates are not
                    //       handled correctly here.
                    const utf8 = try self.toUtf8(allocator);
                    defer allocator.free(utf8);
                    const data_provider = icu4zig.DataProvider.init();
                    defer data_provider.deinit();
                    const case_mapper = icu4zig.CaseMapper.init(data_provider);
                    defer case_mapper.deinit();
                    const locale = icu4zig.Locale.init(null) catch unreachable;
                    defer locale.deinit();
                    const utf8_uppercase = try case_mapper.uppercase(allocator, utf8, locale);
                    defer allocator.free(utf8_uppercase);
                    return fromUtf8(allocator, utf8_uppercase);
                }
                const output = try allocator.alloc(u16, utf16.len);
                for (utf16, 0..) |c, i| {
                    output[i] = if (c < 128) std.ascii.toUpper(@intCast(c)) else c;
                }
                return fromUtf16(output);
            },
        }
    }

    pub fn trim(
        self: Self,
        allocator: Allocator,
        where: enum { start, end, @"start+end" },
    ) Allocator.Error!String {
        const whitespace_code_units = comptime blk: {
            var code_units: [whitespace.len]u21 = undefined;
            for (whitespace, 0..) |utf8, i| {
                code_units[i] = std.unicode.utf8Decode(utf8) catch unreachable;
            }
            break :blk code_units;
        };
        if (self.isEmpty()) return empty;
        switch (where) {
            .start => {
                var start: usize = 0;
                var it = self.codeUnitIterator();
                code_units: while (it.next()) |string_code_unit| {
                    for (whitespace_code_units) |whitespace_code_unit| {
                        if (whitespace_code_unit == string_code_unit) {
                            start += 1;
                            continue :code_units;
                        }
                    }
                    break;
                }
                return self.substring(allocator, start, null);
            },
            .end => {
                var end: usize = self.length();
                var it = self.codeUnitIterator();
                it.index = self.length() - 1;
                code_units: while (it.previous()) |string_code_unit| {
                    for (whitespace_code_units) |whitespace_code_unit| {
                        if (whitespace_code_unit == string_code_unit) {
                            end -= 1;
                            continue :code_units;
                        }
                    }
                    break;
                }
                return self.substring(allocator, 0, end);
            },
            .@"start+end" => {
                var start: usize = 0;
                var end: usize = self.length();
                var it = self.codeUnitIterator();
                code_units: while (it.next()) |string_code_unit| {
                    for (whitespace_code_units) |whitespace_code_unit| {
                        if (whitespace_code_unit == string_code_unit) {
                            start += 1;
                            continue :code_units;
                        }
                    }
                    break;
                }
                it.index = self.length() - 1;
                code_units: while (it.previous()) |string_code_unit| {
                    for (whitespace_code_units) |whitespace_code_unit| {
                        if (whitespace_code_unit == string_code_unit) {
                            end -= 1;
                            continue :code_units;
                        }
                    }
                    break;
                }
                return self.substring(allocator, start, end);
            },
        }
    }

    pub fn replace(
        self: Self,
        allocator: Allocator,
        needle: []const u8,
        replacement: []const u8,
    ) Allocator.Error!String {
        // For now this only deals with simple ASCII replacements.
        switch (self) {
            .ascii => |ascii| {
                const output = try std.mem.replaceOwned(
                    u8,
                    allocator,
                    ascii,
                    needle,
                    replacement,
                );
                return fromAscii(output);
            },
            .utf16 => |utf16| {
                const needle_utf16 = std.unicode.utf8ToUtf16LeAlloc(allocator, needle) catch |err| switch (err) {
                    error.InvalidUtf8 => @panic("Invalid UTF-8"),
                    error.OutOfMemory => return error.OutOfMemory,
                };
                const replacement_utf16 = std.unicode.utf8ToUtf16LeAlloc(allocator, replacement) catch |err| switch (err) {
                    error.InvalidUtf8 => @panic("Invalid UTF-8"),
                    error.OutOfMemory => return error.OutOfMemory,
                };
                const output = try std.mem.replaceOwned(
                    u16,
                    allocator,
                    utf16,
                    needle_utf16,
                    replacement_utf16,
                );
                return fromUtf16(output);
            },
        }
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

pub fn StringHashMap(comptime V: type) type {
    return std.HashMap(String, V, struct {
        pub fn hash(_: @This(), key: String) u64 {
            return key.hash();
        }

        pub fn eql(_: @This(), a: String, b: String) bool {
            return a.eql(b);
        }
    }, std.hash_map.default_max_load_percentage);
}

pub fn StringArrayHashMap(comptime V: type) type {
    return std.ArrayHashMap(String, V, struct {
        pub fn hash(_: @This(), key: String) u32 {
            return @truncate(key.hash());
        }

        pub fn eql(_: @This(), a: String, b: String, _: usize) bool {
            return a.eql(b);
        }
    }, false);
}

test "format" {
    const test_cases = [_]struct { String, []const u8 }{
        .{ String.empty, "" },
        .{ String.fromLiteral("foo"), "foo" },
        .{ String.fromLiteral("123"), "123" },
    };
    for (test_cases) |test_case| {
        const string, const expected = test_case;
        const actual = try std.fmt.allocPrint(std.testing.allocator, "{}", .{string});
        defer std.testing.allocator.free(actual);
        try std.testing.expectEqualStrings(expected, actual);
    }
}
