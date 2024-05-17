//! 6.2.12 Private Names
//! https://tc39.es/ecma262/#sec-private-names

const std = @import("std");

const types = @import("../../types.zig");

const Symbol = types.Symbol;

const Self = @This();

pub const Id = usize;

/// As symbols already have uniqueness via an internal ID, as well as a description string, private
/// names can easily be implemented using symbols.
/// This should be considered an implementation detail and not relied upon for anything else.
symbol: Symbol,

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    try writer.print("{}", .{self.symbol.description.?});
}

pub fn eql(a: Self, b: Self) bool {
    return a.symbol.sameValue(b.symbol);
}

pub fn PrivateNameArrayHashMap(comptime V: type) type {
    return std.ArrayHashMap(Self, V, struct {
        pub fn hash(_: @This(), key: Self) u32 {
            return std.array_hash_map.getAutoHashFn(Id, void)({}, key.symbol.id);
        }

        pub fn eql(_: @This(), a: Self, b: Self, _: usize) bool {
            return a.eql(b);
        }
    }, false);
}
