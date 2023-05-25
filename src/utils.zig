//! Non-standard util functions

const builtins = @import("builtins.zig");
const execution = @import("execution.zig");
const types = @import("types.zig");

const Behaviour = builtins.Behaviour;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;

/// '!' in the spec, ensures that the error is not a throw completion (`error.ExceptionThrown`).
/// OOM is still propagated. The name is a nod to C++, of course :^)
pub fn noexcept(err: error{ ExceptionThrown, OutOfMemory }) !noreturn {
    switch (err) {
        error.ExceptionThrown => @panic("Throw completion was returned from '!' function call"),
        error.OutOfMemory => return error.OutOfMemory,
    }
}

// NOTE: A lot of this behaviour is implied for all builtins and described at the end of
// https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects.

pub fn defineBuiltinFunction(
    object: Object,
    name: []const u8,
    behaviour: *const Behaviour.RegularFn,
    length: u32,
    realm: *Realm,
) !void {
    const function = try createBuiltinFunction(realm.agent, .{ .regular = behaviour }, .{
        .length = length,
        .name = name,
        .realm = realm,
    });
    try defineBuiltinProperty(object, name, Value.from(function));
}

pub fn defineBuiltinProperty(object: Object, name: []const u8, value: anytype) !void {
    const T = @TypeOf(value);
    const property_key = PropertyKey.from(name);
    const property_descriptor = if (T == Value)
        PropertyDescriptor{
            .value = value,
            .writable = true,
            .enumerable = false,
            .configurable = true,
        }
    else if (T == PropertyDescriptor)
        value
    else
        @compileError("defineBuiltinProperty() called with incompatible type " ++ @typeName(T));
    object.definePropertyOrThrow(property_key, property_descriptor) catch |err| try noexcept(err);
}
