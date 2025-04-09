//! 6.1.4 The String Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type

const std = @import("std");

const icu4zig = @import("icu4zig");

const build_options = @import("build-options");
const execution = @import("../../execution.zig");
const language = @import("../../language.zig");

const Agent = execution.Agent;

fn utf8IsAscii(utf8: []const u8) bool {
    return for (utf8) |c| {
        if (!std.ascii.isAscii(c)) break false;
    } else true;
}

const String = @This();

/// https://tc39.es/ecma262/#ASCII-word-characters
pub const ascii_word_characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";

/// The definition of white space is the union of WhiteSpace and LineTerminator.
pub const whitespace_code_units = blk: {
    const whitespace = language.tokenizer.whitespace ++ language.tokenizer.line_terminators;
    var code_units: [whitespace.len]u21 = undefined;
    for (whitespace, 0..) |utf8, i| {
        code_units[i] = std.unicode.utf8Decode(utf8) catch unreachable;
    }
    break :blk code_units;
};

pub const empty = fromLiteral("");

pub const Builder = @import("String/Builder.zig");
pub const CodeUnitIterator = @import("String/CodeUnitIterator.zig");

pub const Slice = union(enum) {
    ascii: []const u8,
    utf16: []const u16,

    pub fn hash(self: @This()) u64 {
        return std.hash.Wyhash.hash(0, switch (self) {
            .ascii => |ascii| ascii,
            .utf16 => |utf16| std.mem.sliceAsBytes(utf16),
        });
    }
};

slice: Slice,
hash: u64,

pub fn format(
    self: *const String,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    switch (self.slice) {
        .ascii => |ascii| try writer.writeAll(ascii),
        .utf16 => |utf16| try writer.print("{}", .{std.unicode.fmtUtf16Le(utf16)}),
    }
}

pub fn fromLiteral(comptime utf8: []const u8) *const String {
    @setEvalBranchQuota(10_000);
    const slice: Slice = comptime if (utf8IsAscii(utf8)) blk: {
        break :blk .{ .ascii = utf8 };
    } else blk: {
        const utf16 = std.unicode.utf8ToUtf16LeStringLiteral(utf8);
        break :blk .{ .utf16 = utf16 };
    };
    const string: *const String = comptime &.{ .slice = slice, .hash = slice.hash() };
    return string;
}

pub fn fromUtf8(agent: *Agent, utf8: []const u8) std.mem.Allocator.Error!*const String {
    if (utf8.len == 0) return empty;
    const slice: Slice = if (utf8IsAscii(utf8)) blk: {
        break :blk .{ .ascii = utf8 };
    } else blk: {
        const utf16 = std.unicode.utf8ToUtf16LeAlloc(agent.gc_allocator, utf8) catch |err| switch (err) {
            error.InvalidUtf8 => @panic("Invalid UTF-8"),
            error.OutOfMemory => return error.OutOfMemory,
        };
        break :blk .{ .utf16 = utf16 };
    };
    const string = try agent.gc_allocator.create(String);
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

// The parser, AST, and parts of codegen need to create strings from UTF-8 slices but do not
// currently have access to the agent, so we skip caching them. This can be revisited later.
pub fn fromUtf8Alloc(allocator: std.mem.Allocator, utf8: []const u8) std.mem.Allocator.Error!*const String {
    if (utf8.len == 0) return empty;
    const slice: Slice = if (utf8IsAscii(utf8)) blk: {
        break :blk .{ .ascii = utf8 };
    } else blk: {
        const utf16 = std.unicode.utf8ToUtf16LeAlloc(allocator, utf8) catch |err| switch (err) {
            error.InvalidUtf8 => @panic("Invalid UTF-8"),
            error.OutOfMemory => return error.OutOfMemory,
        };
        break :blk .{ .utf16 = utf16 };
    };
    const string = try allocator.create(String);
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

pub fn fromAscii(agent: *Agent, ascii: []const u8) std.mem.Allocator.Error!*const String {
    if (ascii.len == 0) return empty;
    const slice: Slice = .{ .ascii = ascii };
    const string = try agent.gc_allocator.create(String);
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

pub fn fromAsciiAlloc(allocator: std.mem.Allocator, ascii: []const u8) std.mem.Allocator.Error!*const String {
    if (ascii.len == 0) return empty;
    const slice: Slice = .{ .ascii = ascii };
    const string = try allocator.create(String);
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

pub fn fromUtf16(agent: *Agent, utf16: []const u16) std.mem.Allocator.Error!*const String {
    if (utf16.len == 0) return empty;
    const slice: Slice = .{ .utf16 = utf16 };
    const string = try agent.gc_allocator.create(String);
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

pub fn fromUtf16Alloc(allocator: std.mem.Allocator, utf16: []const u16) std.mem.Allocator.Error!*const String {
    if (utf16.len == 0) return empty;
    const slice: Slice = .{ .utf16 = utf16 };
    const string = try allocator.create(String);
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

pub fn isEmpty(self: *const String) bool {
    return self.length() == 0;
}

pub fn length(self: *const String) usize {
    return switch (self.slice) {
        .ascii => |ascii| ascii.len,
        .utf16 => |utf16| utf16.len,
    };
}

pub fn toUtf8(self: *const String, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u8 {
    return switch (self.slice) {
        .ascii => |ascii| allocator.dupe(u8, ascii),
        .utf16 => |utf16| std.fmt.allocPrint(
            allocator,
            "{}",
            .{std.unicode.fmtUtf16Le(utf16)},
        ),
    };
}

pub fn toUtf16(self: *const String, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const u16 {
    return switch (self.slice) {
        .ascii => |ascii| blk: {
            const utf16 = try allocator.alloc(u16, ascii.len);
            for (ascii, 0..) |c, i| utf16[i] = c;
            break :blk utf16;
        },
        .utf16 => |utf16| allocator.dupe(u16, utf16),
    };
}

pub fn codeUnitIterator(self: *const String) CodeUnitIterator {
    return .{ .index = 0, .string = self };
}

pub fn eql(self: *const String, other: *const String) bool {
    if (self.slice == .ascii and other.slice == .ascii) {
        return self.hash == other.hash and std.mem.eql(u8, self.slice.ascii, other.slice.ascii);
    }
    if (self.slice == .utf16 and other.slice == .utf16) {
        return self.hash == other.hash and std.mem.eql(u16, self.slice.utf16, other.slice.utf16);
    }
    if (self.isEmpty() and other.isEmpty()) return true;
    if (self.length() != other.length()) return false;
    var it1 = self.codeUnitIterator();
    var it2 = other.codeUnitIterator();
    for (0..self.length()) |_| {
        const c1 = it1.next().?;
        const c2 = it2.next().?;
        if (c1 != c2) return false;
    }
    return true;
}

pub fn startsWith(self: *const String, other: *const String) bool {
    if (self.slice == .ascii and other.slice == .ascii) {
        return std.mem.startsWith(u8, self.slice.ascii, other.slice.ascii);
    }
    if (self.slice == .utf16 and other.slice == .utf16) {
        return std.mem.startsWith(u16, self.slice.utf16, other.slice.utf16);
    }
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
    self: *const String,
    agent: *Agent,
    inclusive_start: usize,
    exclusive_end: ?usize,
) std.mem.Allocator.Error!*const String {
    if (inclusive_start == 0 and (exclusive_end == null or exclusive_end == self.length())) {
        return self;
    }
    if (self.isEmpty() or inclusive_start == self.length() or exclusive_end == 0) return empty;
    switch (self.slice) {
        .ascii => |ascii| {
            const ascii_substring = ascii[inclusive_start .. exclusive_end orelse self.length()];
            return fromAscii(agent, try agent.gc_allocator.dupe(u8, ascii_substring));
        },
        .utf16 => |utf16| {
            const utf16_substring = utf16[inclusive_start .. exclusive_end orelse self.length()];
            const is_ascii = for (utf16_substring) |code_unit| {
                if (code_unit > 0x7F) break false;
            } else true;
            if (is_ascii) {
                const ascii = try agent.gc_allocator.alloc(u8, utf16_substring.len);
                for (utf16_substring, 0..) |code_unit, i| {
                    ascii[i] = @intCast(code_unit);
                }
                return fromAscii(agent, ascii);
            } else {
                return fromUtf16(agent, try agent.gc_allocator.dupe(u16, utf16_substring));
            }
        },
    }
}

/// 6.1.4.1 StringIndexOf ( string, searchValue, fromIndex )
/// https://tc39.es/ecma262/#sec-stringindexof
pub fn indexOf(self: *const String, search_value: *const String, from_index: usize) ?usize {
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
    if (self.slice == .ascii and search_value.slice == .ascii) {
        return if (std.mem.indexOf(
            u8,
            self.slice.ascii[from_index..],
            search_value.slice.ascii,
        )) |index|
            index + from_index
        else
            null;
    }
    if (self.slice == .utf16 and search_value.slice == .utf16) {
        return if (std.mem.indexOf(
            u16,
            self.slice.utf16[from_index..],
            search_value.slice.utf16,
        )) |index|
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
pub fn lastIndexOf(self: *const String, search_value: *const String, from_index: usize) ?usize {
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
    if (self.slice == .ascii and search_value.slice == .ascii) {
        const end = std.math.clamp(from_index + search_len, 0, len);
        return if (std.mem.lastIndexOf(
            u8,
            self.slice.ascii[0..end],
            search_value.slice.ascii,
        )) |index|
            index
        else
            null;
    }
    if (self.slice == .utf16 and search_value.slice == .utf16) {
        const end = std.math.clamp(from_index + search_len, 0, len);
        return if (std.mem.lastIndexOf(
            u16,
            self.slice.utf16[0..end],
            search_value.slice.utf16,
        )) |index|
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
pub fn isWellFormedUnicode(self: *const String) bool {
    if (self.slice == .ascii) return true;

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

pub const CodePoint = struct {
    code_point: u21,
    code_unit_count: u2,
    is_unpaired_surrogate: bool,
};

/// 11.1.4 Static Semantics: CodePointAt ( string, position )
/// https://tc39.es/ecma262/#sec-codepointat
pub fn codePointAt(self: *const String, position: usize) CodePoint {
    // 1. Let size be the length of string.
    const size = self.length();

    // 2. Assert: position ≥ 0 and position < size.
    std.debug.assert(position >= 0 and position < size);

    switch (self.slice) {
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

pub fn codeUnitAt(self: *const String, index: usize) u16 {
    return switch (self.slice) {
        .ascii => |ascii| ascii[index],
        .utf16 => |utf16| utf16[index],
    };
}

pub fn toLowerCase(self: *const String, agent: *Agent) std.mem.Allocator.Error!*const String {
    if (self.isEmpty()) return empty;
    switch (self.slice) {
        .ascii => |ascii| {
            const output = try agent.gc_allocator.alloc(u8, ascii.len);
            for (ascii, 0..) |c, i| {
                output[i] = std.ascii.toLower(c);
            }
            return fromAscii(agent, output);
        },
        .utf16 => |utf16| {
            if (build_options.enable_intl) {
                // NOTE: ICU4X only supports UTF-8 for this, so unpaired surrogates are not
                //       handled correctly here.
                const utf8 = try self.toUtf8(agent.gc_allocator);
                defer agent.gc_allocator.free(utf8);
                const case_mapper = icu4zig.CaseMapper.init();
                defer case_mapper.deinit();
                const locale = icu4zig.Locale.und();
                defer locale.deinit();
                const utf8_lowercase = try case_mapper.lowercase(agent.gc_allocator, utf8, locale);
                return fromUtf8(agent, utf8_lowercase);
            }
            const output = try agent.gc_allocator.alloc(u16, utf16.len);
            for (utf16, 0..) |c, i| {
                output[i] = if (c < 128) std.ascii.toLower(@intCast(c)) else c;
            }
            return fromUtf16(agent, output);
        },
    }
}

pub fn toUpperCase(self: *const String, agent: *Agent) std.mem.Allocator.Error!*const String {
    if (self.isEmpty()) return empty;
    switch (self.slice) {
        .ascii => |ascii| {
            const output = try agent.gc_allocator.alloc(u8, ascii.len);
            for (ascii, 0..) |c, i| {
                output[i] = std.ascii.toUpper(c);
            }
            return fromAscii(agent, output);
        },
        .utf16 => |utf16| {
            if (build_options.enable_intl) {
                // NOTE: ICU4X only supports UTF-8 for this, so unpaired surrogates are not
                //       handled correctly here.
                const utf8 = try self.toUtf8(agent.gc_allocator);
                defer agent.gc_allocator.free(utf8);
                const case_mapper = icu4zig.CaseMapper.init();
                defer case_mapper.deinit();
                const locale = icu4zig.Locale.und();
                defer locale.deinit();
                const utf8_uppercase = try case_mapper.uppercase(agent.gc_allocator, utf8, locale);
                return fromUtf8(agent, utf8_uppercase);
            }
            const output = try agent.gc_allocator.alloc(u16, utf16.len);
            for (utf16, 0..) |c, i| {
                output[i] = if (c < 128) std.ascii.toUpper(@intCast(c)) else c;
            }
            return fromUtf16(agent, output);
        },
    }
}

pub fn trim(
    self: *const String,
    agent: *Agent,
    where: enum { start, end, @"start+end" },
) std.mem.Allocator.Error!*const String {
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
            return self.substring(agent, start, null);
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
            return self.substring(agent, 0, end);
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
            return self.substring(agent, start, end);
        },
    }
}

pub fn replace(
    self: *const String,
    agent: *Agent,
    needle: []const u8,
    replacement: []const u8,
) std.mem.Allocator.Error!*const String {
    // For now this only deals with simple ASCII replacements.
    switch (self.slice) {
        .ascii => |ascii| {
            const output = try std.mem.replaceOwned(
                u8,
                agent.gc_allocator,
                ascii,
                needle,
                replacement,
            );
            return fromAscii(agent, output);
        },
        .utf16 => |utf16| {
            const needle_utf16 = std.unicode.utf8ToUtf16LeAlloc(agent.gc_allocator, needle) catch |err| switch (err) {
                error.InvalidUtf8 => @panic("Invalid UTF-8"),
                error.OutOfMemory => return error.OutOfMemory,
            };
            const replacement_utf16 = std.unicode.utf8ToUtf16LeAlloc(agent.gc_allocator, replacement) catch |err| switch (err) {
                error.InvalidUtf8 => @panic("Invalid UTF-8"),
                error.OutOfMemory => return error.OutOfMemory,
            };
            const output = try std.mem.replaceOwned(
                u16,
                agent.gc_allocator,
                utf16,
                needle_utf16,
                replacement_utf16,
            );
            return fromUtf16(agent, output);
        },
    }
}

pub fn repeat(
    self: *const String,
    agent: *Agent,
    n: usize,
) std.mem.Allocator.Error!*const String {
    // NOTE: This allocates the exact needed capacity upfront
    var builder = try Builder.initCapacity(agent.gc_allocator, n);
    defer builder.deinit(agent.gc_allocator);
    for (0..n) |_| builder.appendStringAssumeCapacity(self);
    return builder.build(agent);
}

pub fn concat(
    agent: *Agent,
    strings: []const *const String,
) std.mem.Allocator.Error!*const String {
    // NOTE: This allocates the exact needed capacity upfront
    var builder = try Builder.initCapacity(agent.gc_allocator, strings.len);
    defer builder.deinit(agent.gc_allocator);
    for (strings) |string| builder.appendStringAssumeCapacity(string);
    return builder.build(agent);
}

pub fn HashMapUnmanaged(comptime V: type) type {
    return std.HashMapUnmanaged(*const String, V, struct {
        pub fn hash(_: @This(), key: *const String) u64 {
            return key.hash;
        }

        pub fn eql(_: @This(), a: *const String, b: *const String) bool {
            return a.eql(b);
        }
    }, std.hash_map.default_max_load_percentage);
}

pub fn ArrayHashMapUnmanaged(comptime V: type) type {
    return std.ArrayHashMapUnmanaged(*const String, V, struct {
        pub fn hash(_: @This(), key: *const String) u32 {
            return @truncate(key.hash);
        }

        pub fn eql(_: @This(), a: *const String, b: *const String, _: usize) bool {
            return a.eql(b);
        }
    }, true);
}

test format {
    const test_cases = [_]struct { *const String, []const u8 }{
        .{ empty, "" },
        .{ fromLiteral("foo"), "foo" },
        .{ fromLiteral("123"), "123" },
    };
    for (test_cases) |test_case| {
        const string, const expected = test_case;
        try std.testing.expectFmt(expected, "{}", .{string});
    }
}
