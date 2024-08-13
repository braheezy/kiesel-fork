//! 6.1.5 The Symbol Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type

const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;

const Self = @This();

pub const Data = struct {
    /// Flag to mark this symbol as a private name.
    is_private: bool = false,

    /// [[Description]]
    description: ?String,
};

data: *Data,

pub fn init(allocator: Allocator, description: ?String) Allocator.Error!Self {
    const data = try allocator.create(Data);
    data.* = .{ .description = description };
    return .{ .data = data };
}

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    try writer.writeAll("Symbol(");
    if (self.data.description) |description| {
        try writer.print("\"{}\"", .{description});
    }
    try writer.writeAll(")");
}

/// Shortcut for the SameValue AO applied on two symbols (i.e. id equality)
pub fn sameValue(self: Self, other: Self) bool {
    return self.data == other.data;
}

/// 20.4.3.3.1 SymbolDescriptiveString ( sym )
/// https://tc39.es/ecma262/#sec-symboldescriptivestring
pub fn descriptiveString(self: Self, agent: *Agent) Allocator.Error!String {
    // 1. Let desc be sym's [[Description]] value.
    // 2. If desc is undefined, set desc to the empty String.
    // 3. Assert: desc is a String.
    const description = self.data.description orelse String.empty;

    // 4. Return the string-concatenation of "Symbol(", desc, and ")".
    return String.concat(
        agent.gc_allocator,
        &.{ String.fromLiteral("Symbol("), description, String.fromLiteral(")") },
    );
}

test "format" {
    var test_cases = [_]struct { Self.Data, []const u8 }{
        .{ .{ .description = null }, "Symbol()" },
        .{ .{ .description = String.empty }, "Symbol(\"\")" },
        .{ .{ .description = String.fromLiteral("foo") }, "Symbol(\"foo\")" },
    };
    for (&test_cases) |*test_case| {
        const symbol: Self = .{ .data = &test_case[0] };
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{symbol});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(test_case[1], string);
    }
}
