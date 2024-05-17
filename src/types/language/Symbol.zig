//! 6.1.5 The Symbol Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;

const Self = @This();

pub const Id = usize;

/// Internal ID used for equality checks
id: Id,

/// [[Description]]
description: ?String,

/// Flag to mark this symbol as a private name.
private: bool = false,

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    try writer.writeAll("Symbol(");
    if (self.description) |description| {
        try writer.print("\"{}\"", .{description});
    }
    try writer.writeAll(")");
}

/// Shortcut for the SameValue AO applied on two symbols (i.e. id equality)
pub fn sameValue(self: Self, other: Self) bool {
    return self.id == other.id;
}

/// 20.4.3.3.1 SymbolDescriptiveString ( sym )
/// https://tc39.es/ecma262/#sec-symboldescriptivestring
pub fn descriptiveString(self: Self, agent: *Agent) Allocator.Error![]const u8 {
    // 1. Let desc be sym's [[Description]] value.
    // 2. If desc is undefined, set desc to the empty String.
    // 3. Assert: desc is a String.
    const description = self.description orelse String.empty;

    // 4. Return the string-concatenation of "Symbol(", desc, and ")".
    return std.fmt.allocPrint(agent.gc_allocator, "Symbol({s})", .{description.utf8});
}

test "format" {
    const test_cases = [_]struct { Self, []const u8 }{
        .{ .{ .id = 0, .description = null }, "Symbol()" },
        .{ .{ .id = 1, .description = String.empty }, "Symbol(\"\")" },
        .{ .{ .id = 2, .description = String.from("foo") }, "Symbol(\"foo\")" },
    };
    for (test_cases) |test_case| {
        const symbol, const expected = test_case;
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{symbol});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}
