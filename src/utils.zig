//! Non-standard util functions

const builtins = @import("builtins.zig");
const execution = @import("execution.zig");
const types = @import("types.zig");

const BehaviourFn = builtins.BehaviourFn;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;

// NOTE: A lot of this behaviour is implied for all builtins and described at the end of
// https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects.

pub fn defineBuiltinFunction(
    object: Object,
    name: []const u8,
    behaviour: *const BehaviourFn,
    length: u32,
    realm: *Realm,
) !void {
    const function = try createBuiltinFunction(
        realm.agent,
        behaviour,
        .{ .length = length, .name = name, .realm = realm },
    );
    try object.createMethodProperty(
        PropertyKey.fromString(name),
        Value.fromObject(function),
    );
}

pub fn defineBuiltinProperty(object: Object, name: []const u8, value: anytype) !void {
    const property_key = PropertyKey.fromString(name);
    const ValueT = @TypeOf(value);
    if (ValueT == Value) {
        try object.createNonEnumerableDataPropertyOrThrow(property_key, value);
    } else if (ValueT == PropertyDescriptor) {
        object.definePropertyOrThrow(property_key, value) catch |err| switch (err) {
            error.ExceptionThrown => unreachable,
            error.OutOfMemory => return error.OutOfMemory,
        };
    } else {
        @compileError("Unsupported value type in defineBuiltinProperty(): " ++ @typeName(ValueT));
    }
}
