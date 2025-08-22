//! 6.1.5 The Symbol Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type

const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;

const Symbol = @This();

/// Flag to mark this symbol as a private name.
is_private: bool = false,

/// [[Description]]
description: ?*const String,

pub fn init(
    allocator: std.mem.Allocator,
    description: ?*const String,
) std.mem.Allocator.Error!*const Symbol {
    const symbol = try allocator.create(Symbol);
    symbol.* = .{ .description = description };
    return symbol;
}

/// For tests not using the GC allocator.
pub fn deinit(self: *const Symbol, allocator: std.mem.Allocator) void {
    // TODO: To deinit the description string we need to know if it was dynamically allocated.
    allocator.destroy(self);
}

pub fn format(
    self: *const Symbol,
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

/// 20.4.3.3.1 SymbolDescriptiveString ( sym )
/// https://tc39.es/ecma262/#sec-symboldescriptivestring
pub fn descriptiveString(self: *const Symbol, agent: *Agent) std.mem.Allocator.Error!*const String {
    // 1. Let desc be sym.[[Description]].
    // 2. If desc is undefined, set desc to the empty String.
    // 3. Assert: desc is a String.
    const description: *const String = self.description orelse .empty;

    // 4. Return the string-concatenation of "Symbol(", desc, and ")".
    return String.concat(agent, &.{
        String.fromLiteral("Symbol("),
        description,
        String.fromLiteral(")"),
    });
}

test format {
    const test_cases = [_]struct { Symbol, []const u8 }{
        .{ .{ .description = null }, "Symbol()" },
        .{ .{ .description = .empty }, "Symbol(\"\")" },
        .{ .{ .description = String.fromLiteral("foo") }, "Symbol(\"foo\")" },
    };
    for (test_cases) |test_case| {
        const symbol, const expected = test_case;
        try std.testing.expectFmt(expected, "{}", .{symbol});
    }
}
