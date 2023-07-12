//! Non-standard util functions

const ptk = @import("ptk");
const std = @import("std");

const Allocator = std.mem.Allocator;
const Error = ptk.Error;

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

fn TemporaryChange(comptime Lhs: type, comptime field_name: []const u8) type {
    const fields = switch (@typeInfo(Lhs)) {
        .Struct => |s| s.fields,
        .Pointer => |p| switch (@typeInfo(p.child)) {
            .Struct => |s| s.fields,
            else => @compileError("temporaryChange() called with incompatible type " ++ @typeName(Lhs)),
        },
        else => @compileError("temporaryChange() called with incompatible type " ++ @typeName(Lhs)),
    };
    const field = for (fields) |field| if (std.mem.eql(u8, field.name, field_name)) break field;
    return struct {
        const Self = @This();

        lhs: Lhs,
        previous_value: field.type,

        pub fn restore(self: Self) void {
            @field(self.lhs, field_name) = self.previous_value;
        }
    };
}

pub fn temporaryChange(
    lhs: anytype,
    comptime field_name: []const u8,
    new_value: anytype,
) TemporaryChange(@TypeOf(lhs), field_name) {
    defer @field(lhs, field_name) = new_value;
    return .{ .lhs = lhs, .previous_value = @field(lhs, field_name) };
}

pub fn formatParseError(allocator: Allocator, parse_error: Error) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s} ({s}:{}:{})", .{
        parse_error.message,
        parse_error.location.source orelse "<unknown>",
        parse_error.location.line,
        parse_error.location.column,
    });
}

// NOTE: A lot of this behaviour is implied for all builtins and described at the end of
// https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects.

pub fn defineBuiltinFunction(
    object: Object,
    comptime name: []const u8,
    behaviour: *const Behaviour.RegularFn,
    length: u32,
    realm: *Realm,
) !void {
    const function_name = if (comptime std.mem.startsWith(u8, name, "@@"))
        std.fmt.comptimePrint("[Symbol.{s}]", .{name[2..]})
    else
        name;
    const function = try createBuiltinFunction(realm.agent, .{ .regular = behaviour }, .{
        .length = length,
        .name = function_name,
        .realm = realm,
    });
    try defineBuiltinProperty(object, name, Value.from(function));
}

pub fn defineBuiltinProperty(object: Object, comptime name: []const u8, value: anytype) !void {
    const T = @TypeOf(value);
    const property_key = if (comptime std.mem.startsWith(u8, name, "@@"))
        PropertyKey.from(@field(object.agent().well_known_symbols, name))
    else
        PropertyKey.from(name);
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
