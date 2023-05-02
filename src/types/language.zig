const value = @import("language/value.zig");

pub const BigInt = @import("language/BigInt.zig");
pub const Number = @import("language/number.zig").Number;
pub const Object = @import("language/Object.zig");
pub const Symbol = @import("language/Symbol.zig");
pub const Value = value.Value;
pub const sameValue = value.sameValue;
pub const sameValueZero = value.sameValueZero;

test {
    _ = BigInt;
    _ = Number;
    _ = Object;
    _ = Symbol;
    _ = Value;
}
