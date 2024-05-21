const string = @import("language/string.zig");
const value = @import("language/value.zig");

pub const BigInt = @import("language/BigInt.zig");
pub const MakeObject = @import("language/Object/make_object.zig").MakeObject;
pub const Number = @import("language/number.zig").Number;
pub const Object = @import("language/Object.zig");
pub const String = string.String;
pub const StringArrayHashMap = string.StringArrayHashMap;
pub const StringHashMap = string.StringHashMap;
pub const Symbol = @import("language/Symbol.zig");
pub const Value = value.Value;
pub const ValueArrayHashMap = value.ValueArrayHashMap;
pub const coerceOptionsToObject = value.coerceOptionsToObject;
pub const createArrayFromList = value.createArrayFromList;
pub const createArrayFromListMapToValue = value.createArrayFromListMapToValue;
pub const getOption = value.getOption;
pub const isLessThan = value.isLessThan;
pub const isLooselyEqual = value.isLooselyEqual;
pub const isStrictlyEqual = value.isStrictlyEqual;
pub const sameValue = value.sameValue;
pub const sameValueZero = value.sameValueZero;

test {
    _ = BigInt;
    _ = Number;
    _ = Object;
    _ = String;
    _ = Symbol;
    _ = Value;
}
