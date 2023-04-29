pub const language = @import("types/language.zig");

pub const BigInt = language.BigInt;
pub const Number = language.Number;
pub const Object = language.Object;
pub const Symbol = language.Symbol;
pub const Value = language.Value;

test {
    _ = language;
}
