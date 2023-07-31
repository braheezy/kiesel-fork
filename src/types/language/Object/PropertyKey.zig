const std = @import("std");

const String = @import("../String.zig");
const Symbol = @import("../Symbol.zig");

/// A property key is either a String or a Symbol. All Strings and Symbols, including the empty
/// String, are valid as property keys.
pub const PropertyKey = union(enum) {
    const Self = @This();

    pub const IntegerIndex = u53;

    string: String,
    symbol: Symbol,

    // OPTIMIZATION: If the string is known to be an integer index, store it as a number.
    /// An integer index is a property name n such that CanonicalNumericIndexString(n) returns an
    /// integral Number in the inclusive interval from +0𝔽 to 𝔽(2^53 - 1).
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
                return .{ .string = String.from(value) };
            }
        } else if (T == String) {
            return .{ .string = value };
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
};
