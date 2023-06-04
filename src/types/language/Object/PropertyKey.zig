const std = @import("std");

const Symbol = @import("../Symbol.zig");

/// A property key value is either an ECMAScript String value or a Symbol value.
pub const PropertyKey = union(enum) {
    const Self = @This();

    pub const IntegerIndex = u53;

    string: []const u8,
    symbol: Symbol,

    // OPTIMIZATION: If the string is known to be an integer index, store it as a number.
    integer_index: IntegerIndex,

    pub inline fn from(value: anytype) Self {
        const T = @TypeOf(value);
        if (@typeInfo(T) == .Pointer) {
            // FIXME: This is not great, but for now we can let the compiler do the rest as strings
            //        are the only pointers we support here.
            // FIXME: This should use CanonicalNumericIndexString to reject numeric strings that
            //        are not canonical.
            if (std.fmt.parseUnsigned(IntegerIndex, value, 10)) |integer_index| {
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

    /// An integer index is a String-valued property key that is a canonical numeric string and
    /// whose numeric value is either +0𝔽 or a positive integral Number ≤ 𝔽(2^53 - 1).
    pub inline fn isIntegerIndex(self: Self) bool {
        return self == .integer_index;
    }

    /// An array index is an integer index whose numeric value i is in the range
    /// +0𝔽 ≤ i < 𝔽(2^32 - 1).
    pub inline fn isArrayIndex(self: Self) bool {
        return self == .integer_index and self.integer_index < (std.math.maxInt(u32) - 1);
    }
};
