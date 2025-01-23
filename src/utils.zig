//! Non-standard util functions

const ptk = @import("ptk");
const std = @import("std");

const builtins = @import("builtins.zig");
const execution = @import("execution.zig");
pub const float16 = @import("utils/float16.zig");
const types = @import("types.zig");

const Agent = execution.Agent;
const Behaviour = builtins.builtin_function.Behaviour;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;

/// '!' in the spec, ensures that the error is not a throw completion (`error.ExceptionThrown`).
/// OOM is still propagated. The name is a nod to C++, of course :^)
pub fn noexcept(err: error{ ExceptionThrown, OutOfMemory }) std.mem.Allocator.Error!noreturn {
    switch (err) {
        error.ExceptionThrown => @panic("Throw completion was returned from '!' function call"),
        error.OutOfMemory => return error.OutOfMemory,
    }
}

pub fn TemporaryChange(comptime T: type) type {
    return struct {
        field: *T,
        previous_value: T,

        pub fn restore(self: @This()) void {
            self.field.* = self.previous_value;
        }
    };
}

pub fn temporaryChange(
    field: anytype,
    new_value: @TypeOf(field.*),
) TemporaryChange(@TypeOf(field.*)) {
    const T = @TypeOf(field);
    if (@typeInfo(T) != .pointer) {
        @compileError("temporaryChange() called with incompatible type " ++ @typeName(T));
    }
    defer field.* = new_value;
    return .{ .field = field, .previous_value = field.* };
}

// NOTE: This function is vendored from the Zig since it was removed from std.
pub inline fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .pointer) break :blk false;

        const ptr = &info.pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;

        // If it's already a slice, simple check.
        if (ptr.size == .slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.
        if (ptr.size == .one) {
            const child = @typeInfo(ptr.child);
            if (child == .array) {
                const arr = &child.array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

pub fn containsSlice(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |value| {
        if (std.mem.eql(u8, value, needle)) return true;
    }
    return false;
}

pub const StringParser = struct {
    string: []const u8,
    index: usize,

    pub fn init(string: []const u8) StringParser {
        return .{ .string = string, .index = 0 };
    }

    pub fn peek(self: StringParser) ?u8 {
        if (self.index + 1 > self.string.len) return null;
        return self.string[self.index];
    }

    pub fn peekSlice(self: StringParser, count: usize) ?[]const u8 {
        if (self.index + count > self.string.len) return null;
        return self.string[self.index .. self.index + count];
    }

    pub fn consume(self: *StringParser) ?u8 {
        const char = self.peek() orelse return null;
        self.index += 1;
        return char;
    }

    pub fn consumeSlice(self: *StringParser, count: usize) ?[]const u8 {
        const slice = self.peekSlice(count) orelse return null;
        self.index += count;
        return slice;
    }

    pub fn consumeDigits(self: *StringParser, comptime T: type, count: usize) ?T {
        const slice = self.peekSlice(count) orelse return null;
        if (std.mem.indexOfScalar(u8, slice, '_') != null) return null;
        const result = std.fmt.parseInt(T, slice, 10) catch return null;
        self.index += count;
        return result;
    }
};

pub fn formatParseError(
    allocator: std.mem.Allocator,
    parse_error: ptk.Error,
) std.mem.Allocator.Error![]const u8 {
    return std.fmt.allocPrint(allocator, "{s} ({s}:{}:{})", .{
        parse_error.message,
        parse_error.location.source orelse "<unknown>",
        parse_error.location.line,
        parse_error.location.column,
    });
}

pub fn formatParseErrorHint(
    allocator: std.mem.Allocator,
    parse_error: ptk.Error,
    source_text: []const u8,
) std.mem.Allocator.Error![]const u8 {
    // NOTE: parse-toolkit only uses '\n' to advance the line counter - for \r\n newlines this
    //       doesn't matter, and LS/PS are rare enough to not matter for now.
    var line_iterator = std.mem.splitScalar(u8, source_text, '\n');
    var i: usize = 0;
    const source_line = while (line_iterator.next()) |source_line| : (i += 1) {
        if (i == parse_error.location.line - 1) break source_line;
    } else unreachable;
    return std.fmt.allocPrint(allocator, "{s}\n{c: >[2]}", .{
        source_line,
        '^',
        parse_error.location.column, // 1-indexed, which is fine as this means 'width' in this context
    });
}

// NOTE: A lot of this behaviour is implied for all builtins and described at the end of
// https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects.

fn getFunctionName(comptime name: []const u8) []const u8 {
    if (comptime std.mem.startsWith(u8, name, "%Symbol.")) {
        comptime std.debug.assert(std.mem.endsWith(u8, name, "%"));
        return std.fmt.comptimePrint("[{s}]", .{name[1 .. name.len - 1]});
    } else {
        return name;
    }
}

fn getPropertyKey(comptime name: []const u8, agent: *Agent) PropertyKey {
    if (comptime std.mem.startsWith(u8, name, "%Symbol.")) {
        comptime std.debug.assert(std.mem.endsWith(u8, name, "%"));
        return PropertyKey.from(@field(agent.well_known_symbols, name));
    } else {
        return PropertyKey.from(name);
    }
}

pub fn defineBuiltinAccessor(
    object: *Object,
    comptime name: []const u8,
    getter: ?*const Behaviour.Function,
    setter: ?*const Behaviour.Function,
    realm: *Realm,
) std.mem.Allocator.Error!void {
    return defineBuiltinAccessorWithAttributes(
        object,
        name,
        getter,
        setter,
        realm,
        .{ .enumerable = false, .configurable = true },
    );
}

pub fn defineBuiltinAccessorWithAttributes(
    object: *Object,
    comptime name: []const u8,
    getter: ?*const Behaviour.Function,
    setter: ?*const Behaviour.Function,
    realm: *Realm,
    attributes: struct {
        enumerable: bool,
        configurable: bool,
    },
) std.mem.Allocator.Error!void {
    const agent = object.agent;
    std.debug.assert(getter != null or setter != null);
    const getter_function = if (getter) |function| blk: {
        const function_name = std.fmt.comptimePrint("get {s}", .{comptime getFunctionName(name)});
        break :blk try createBuiltinFunction(agent, .{ .function = function }, .{
            .length = 0,
            .name = function_name,
            .realm = realm,
        });
    } else null;
    const setter_function = if (setter) |function| blk: {
        const function_name = std.fmt.comptimePrint("set {s}", .{comptime getFunctionName(name)});
        break :blk try createBuiltinFunction(agent, .{ .function = function }, .{
            .length = 0,
            .name = function_name,
            .realm = realm,
        });
    } else null;
    const property_key = getPropertyKey(name, agent);
    const attributes_: Object.PropertyStorage.Attributes = .{
        .writable = false,
        .enumerable = attributes.enumerable,
        .configurable = attributes.configurable,
    };
    object.property_storage.shape = try object.property_storage.shape.setPropertyWithoutTransition(
        object.agent.gc_allocator,
        property_key,
        attributes_,
        .accessor,
    );
    try object.property_storage.accessors.append(agent.gc_allocator, .{
        .get = getter_function,
        .set = setter_function,
    });
}

pub fn defineBuiltinFunction(
    object: *Object,
    comptime name: []const u8,
    function: *const Behaviour.Function,
    length: u32,
    realm: *Realm,
) std.mem.Allocator.Error!void {
    const function_name = comptime getFunctionName(name);
    const builtin_function = try createBuiltinFunction(realm.agent, .{ .function = function }, .{
        .length = length,
        .name = function_name,
        .realm = realm,
    });
    try defineBuiltinProperty(object, name, Value.from(builtin_function));
}

pub fn defineBuiltinFunctionWithAttributes(
    object: *Object,
    comptime name: []const u8,
    function: *const Behaviour.Function,
    length: u32,
    realm: *Realm,
    attributes: struct {
        writable: bool,
        enumerable: bool,
        configurable: bool,
    },
) std.mem.Allocator.Error!void {
    const function_name = comptime getFunctionName(name);
    const builtin_function = try createBuiltinFunction(realm.agent, .{ .function = function }, .{
        .length = length,
        .name = function_name,
        .realm = realm,
    });
    try defineBuiltinProperty(object, name, PropertyDescriptor{
        .value = Value.from(builtin_function),
        .writable = attributes.writable,
        .enumerable = attributes.enumerable,
        .configurable = attributes.configurable,
    });
}

pub fn defineBuiltinFunctionLazy(
    object: *Object,
    comptime name: []const u8,
    comptime function: *const Behaviour.Function,
    comptime length: u32,
    realm: *Realm,
    attributes: Object.PropertyStorage.Attributes,
) std.mem.Allocator.Error!void {
    const function_name = comptime getFunctionName(name);
    try defineBuiltinPropertyLazy(
        object,
        name,
        struct {
            fn initializer(realm_: *Realm) std.mem.Allocator.Error!Value {
                const builtin_function = try createBuiltinFunction(realm_.agent, .{ .function = function }, .{
                    .length = length,
                    .name = function_name,
                    .realm = realm_,
                });
                return Value.from(builtin_function);
            }
        }.initializer,
        realm,
        attributes,
    );
}

pub fn defineBuiltinProperty(
    object: *Object,
    comptime name: []const u8,
    value_or_property_descriptor: anytype,
) std.mem.Allocator.Error!void {
    const T = @TypeOf(value_or_property_descriptor);
    const agent = object.agent;
    const property_key = getPropertyKey(name, agent);
    const value: Value, const attributes: Object.PropertyStorage.Attributes = switch (T) {
        Value => .{
            value_or_property_descriptor,
            .builtin_default,
        },
        PropertyDescriptor => .{
            value_or_property_descriptor.value.?,
            .fromPropertyDescriptor(value_or_property_descriptor),
        },
        else => @compileError("defineBuiltinProperty() called with incompatible type " ++ @typeName(T)),
    };
    object.property_storage.shape = try object.property_storage.shape.setPropertyWithoutTransition(
        object.agent.gc_allocator,
        property_key,
        attributes,
        .value,
    );
    try object.property_storage.values.append(agent.gc_allocator, value);
}

pub fn defineBuiltinPropertyLazy(
    object: *Object,
    comptime name: []const u8,
    initializer: *const fn (*Realm) std.mem.Allocator.Error!Value,
    realm: *Realm,
    attributes: Object.PropertyStorage.Attributes,
) std.mem.Allocator.Error!void {
    const agent = object.agent;
    const property_key = getPropertyKey(name, agent);
    object.property_storage.shape = try object.property_storage.shape.setPropertyWithoutTransition(
        object.agent.gc_allocator,
        property_key,
        attributes,
        .value,
    );
    try object.property_storage.values.append(agent.gc_allocator, undefined);
    try object.property_storage.lazy_properties.putNoClobber(
        agent.gc_allocator,
        property_key,
        .{
            .realm = realm,
            .initializer = .{ .value = initializer },
        },
    );
}
