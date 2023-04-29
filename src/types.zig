pub const language = @import("types/language.zig");
pub const spec = @import("types/spec.zig");

pub const BigInt = language.BigInt;
pub const Completion = spec.Completion;
pub const Number = language.Number;
pub const Object = language.Object;
pub const PropertyDescriptor = spec.PropertyDescriptor;
pub const Symbol = language.Symbol;
pub const Value = language.Value;

test {
    _ = language;
    _ = spec;
}
