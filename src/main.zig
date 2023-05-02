pub const builtins = @import("builtins.zig");
const execution = @import("execution.zig");
const language = @import("language.zig");
const types = @import("types.zig");

pub const BigInt = types.BigInt;
pub const Agent = execution.Agent;
pub const Completion = types.Completion;
pub const Number = types.Number;
pub const Object = types.Object;
pub const PreferredType = types.PreferredType;
pub const PropertyDescriptor = types.PropertyDescriptor;
pub const PropertyKey = types.PropertyKey;
pub const Realm = execution.Realm;
pub const Script = language.Script;
pub const Symbol = types.Symbol;
pub const Value = types.Value;

test {
    _ = builtins;
    _ = execution;
    _ = language;
    _ = types;
}
