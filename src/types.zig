pub const language = @import("types/language.zig");
pub const spec = @import("types/spec.zig");

pub const BigInt = language.BigInt;
pub const Completion = spec.Completion;
pub const Number = language.Number;
pub const Object = language.Object;
pub const PreferredType = Value.PreferredType;
pub const PropertyDescriptor = spec.PropertyDescriptor;
pub const PropertyKey = Object.PropertyKey;
pub const Reference = spec.Reference;
pub const Symbol = language.Symbol;
pub const Value = language.Value;
pub const createArrayFromList = language.createArrayFromList;
pub const isLessThan = language.isLessThan;
pub const isLooselyEqual = language.isLooselyEqual;
pub const isStrictlyEqual = language.isStrictlyEqual;
pub const sameValue = language.sameValue;
pub const sameValueZero = language.sameValueZero;

test {
    _ = language;
    _ = spec;
}
