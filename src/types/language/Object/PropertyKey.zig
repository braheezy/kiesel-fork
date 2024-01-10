const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../../execution.zig");
const utils = @import("../../../utils.zig");

const Agent = execution.Agent;
const String = @import("../string.zig").String;
const Symbol = @import("../Symbol.zig");
const Value = @import("../value.zig").Value;
const isZigString = utils.isZigString;

/// A property key is either a String or a Symbol. All Strings and Symbols, including the empty
/// String, are valid as property keys.
pub const PropertyKey = union(enum) {
    const Self = @This();

    /// An integer index is a property name n such that CanonicalNumericIndexString(n) returns an
    /// integral Number in the inclusive interval from +0𝔽 to 𝔽(2^53 - 1).
    /// https://tc39.es/ecma262/#integer-index
    pub const IntegerIndex = u53;

    string: String,
    symbol: Symbol,

    // OPTIMIZATION: If the string is known to be an integer index, store it as a number.
    integer_index: IntegerIndex,

    pub inline fn from(value: anytype) Self {
        const T = @TypeOf(value);
        if (isZigString(T)) {
            // FIXME: This should use CanonicalNumericIndexString to reject numeric strings that
            //        are not canonical.
            if (std.fmt.parseUnsigned(IntegerIndex, value, 10)) |integer_index| {
                return .{ .integer_index = integer_index };
            } else |_| {
                return .{ .string = String.from(value) };
            }
        } else if (T == String) {
            // FIXME: This should use CanonicalNumericIndexString to reject numeric strings that
            //        are not canonical.
            if (std.fmt.parseUnsigned(IntegerIndex, value.utf8, 10)) |integer_index| {
                return .{ .integer_index = integer_index };
            } else |_| {
                return .{ .string = value };
            }
        } else if (T == Symbol) {
            return .{ .symbol = value };
        } else if (T == IntegerIndex or @typeInfo(T) == .ComptimeInt) {
            return .{ .integer_index = @as(IntegerIndex, value) };
        } else {
            @compileError("PropertyKey.from() called with incompatible type " ++ @typeName(T));
        }
    }

    /// An array index is an integer index n such that CanonicalNumericIndexString(n) returns an
    /// integral Number in the inclusive interval from +0𝔽 to 𝔽(2^32 - 2).
    pub inline fn isArrayIndex(self: Self) bool {
        return self == .integer_index and self.integer_index <= (std.math.maxInt(u32) - 1);
    }

    fn eqlStringAndIntegerIndex(string: String, index: IntegerIndex) bool {
        const len = comptime std.fmt.count("{d}", .{std.math.maxInt(IntegerIndex)});
        var index_string: [len]u8 = undefined;
        _ = std.fmt.bufPrint(&index_string, "{d}", .{index}) catch unreachable;
        return string.eql(String.from(&index_string));
    }

    /// Non-standard helper to check `PropertyKey` equality without going through `sameValue()`.
    pub fn eql(a: Self, b: Self) bool {
        return switch (a) {
            .string => |a_string| switch (b) {
                .string => |b_string| a_string.eql(b_string),
                .symbol => false,
                .integer_index => |b_integer_index| eqlStringAndIntegerIndex(a_string, b_integer_index),
            },
            .symbol => |a_symbol| switch (b) {
                .string => false,
                .symbol => |b_symbol| a_symbol.id == b_symbol.id,
                .integer_index => false,
            },
            .integer_index => |a_integer_index| switch (b) {
                .string => |b_string| eqlStringAndIntegerIndex(b_string, a_integer_index),
                .symbol => false,
                .integer_index => |b_integer_index| a_integer_index == b_integer_index,
            },
        };
    }

    /// Non-standard helper to convert a `PropertyKey` to a `Value` - they *are* plain (string or
    /// symbol) values in the spec.
    pub fn toValue(self: Self, agent: *Agent) Allocator.Error!Value {
        return switch (self) {
            .string => |string| Value.from(string),
            .symbol => |symbol| Value.from(symbol),
            .integer_index => |integer_index| Value.from(try std.fmt.allocPrint(
                agent.gc_allocator,
                "{}",
                .{integer_index},
            )),
        };
    }

    /// Non-standard helper to convert a `PropertyKey` to a `[]const u8` or `Symbol` (i.e. bypassing
    /// the integer index optimization).
    pub fn toStringOrSymbol(self: Self, agent: *Agent) Allocator.Error!union(enum) {
        string: []const u8,
        symbol: Symbol,
    } {
        return switch (self) {
            .string => |string| .{ .string = string.utf8 },
            .symbol => |symbol| .{ .symbol = symbol },
            .integer_index => |integer_index| .{
                .string = try std.fmt.allocPrint(agent.gc_allocator, "{}", .{integer_index}),
            },
        };
    }
};
