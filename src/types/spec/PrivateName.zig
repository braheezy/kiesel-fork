//! 6.2.12 Private Names
//! https://tc39.es/ecma262/#sec-private-names

const std = @import("std");

const types = @import("../../types.zig");

const Symbol = types.Symbol;

const PrivateName = @This();

/// As symbols already have uniqueness via pointers, as well as a description string, private
/// names can easily be implemented using symbols.
/// This should be considered an implementation detail and not relied upon for anything else.
symbol: *const Symbol,

pub fn format(
    self: PrivateName,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;
    try writer.print("{}", .{self.symbol.description.?});
}

pub fn hash(self: PrivateName) u64 {
    return std.hash_map.getAutoHashFn(*const Symbol, void)({}, self.symbol);
}

pub fn eql(a: PrivateName, b: PrivateName) bool {
    return a.symbol == b.symbol;
}

pub fn ArrayHashMap(comptime V: type) type {
    return std.ArrayHashMap(PrivateName, V, struct {
        pub fn hash(_: @This(), private_name: PrivateName) u32 {
            return @truncate(private_name.hash());
        }

        pub fn eql(_: @This(), a: PrivateName, b: PrivateName, _: usize) bool {
            return a.eql(b);
        }
    }, true);
}
