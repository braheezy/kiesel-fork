const std = @import("std");

const Color = std.io.tty.Color;

const AnyPointer = @import("any-pointer").AnyPointer;

const builtins = @import("builtins.zig");
const types = @import("types.zig");

const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const getArrayLength = @import("builtins/array.zig").getArrayLength;
const getFunctionName = @import("builtins/ecmascript_function.zig").getFunctionName;
const ordinaryOwnPropertyKeys = builtins.ordinaryOwnPropertyKeys;

const SeenObjects = std.AutoHashMap(AnyPointer, usize);
const State = struct {
    seen_objects: SeenObjects,
    print_in_progress: bool,
};

var buf: [@sizeOf(*anyopaque) * 1024]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&buf);
var state = State{
    .seen_objects = SeenObjects.init(fba.allocator()),
    .print_in_progress = false,
};

fn getTtyConfigForWriter(writer: anytype) std.io.tty.Config {
    const file = if (@TypeOf(writer.context) == std.fs.File)
        writer.context
    else
        std.io.getStdOut();
    return std.io.tty.detectConfig(file);
}

fn prettyPrintArray(array: *const builtins.Array, writer: anytype) !void {
    const property_storage = array.data.property_storage;
    const length = getArrayLength(@constCast(array).object());
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("[");
    try tty_config.setColor(writer, .reset);
    if (length != 0) try writer.writeAll(" ");
    for (0..length) |i| {
        const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i)));
        if (property_storage.get(property_key)) |property_descriptor| {
            try writer.print("{pretty}", .{property_descriptor.value.?});
        } else {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<empty>");
            try tty_config.setColor(writer, .reset);
        }
        if (i + 1 < length) try writer.writeAll(", ");
    }
    if (length != 0) try writer.writeAll(" ");
    try tty_config.setColor(writer, .white);
    try writer.writeAll("]");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintArrayIterator(array_iterator: *const builtins.ArrayIterator, writer: anytype) !void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%ArrayIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (array_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(state_.array)});
        },
        .completed => {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<completed>");
            try tty_config.setColor(writer, .reset);
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintDate(date: *const builtins.Date, writer: anytype) !void {
    const date_value = date.fields.date_value;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Date(");
    try writer.print("{pretty}", .{Value.from(date_value)});
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintError(@"error": *const builtins.Error, writer: anytype) !void {
    const error_data = @"error".fields.error_data;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .red);
    if (error_data.message.utf16Length() == 0) {
        try writer.print("{}", .{error_data.name});
    } else {
        try writer.print("{}: {}", .{ error_data.name, error_data.message });
    }
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintMap(map: *const builtins.Map, writer: anytype) !void {
    const map_data = map.fields.map_data;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Map(");
    try tty_config.setColor(writer, .reset);
    var it = map_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{pretty} → {pretty}", .{ entry.key_ptr.*, entry.value_ptr.* });
        if (it.index < map_data.count()) {
            try writer.writeAll(", ");
        }
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintMapIterator(map_iterator: *const builtins.MapIterator, writer: anytype) !void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%MapIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (map_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(state_.map.object())});
        },
        .completed => {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<completed>");
            try tty_config.setColor(writer, .reset);
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintProxy(proxy: *const builtins.Proxy, writer: anytype) !void {
    const proxy_target = proxy.fields.proxy_target;
    const proxy_handler = proxy.fields.proxy_handler;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Proxy(");
    try tty_config.setColor(writer, .reset);
    if (proxy_target != null and proxy_handler != null) {
        try writer.print("{pretty}", .{Value.from(proxy_target.?)});
        try writer.writeAll(", ");
        try writer.print("{pretty}", .{Value.from(proxy_handler.?)});
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<revoked>");
        try tty_config.setColor(writer, .reset);
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintSet(set: *const builtins.Set, writer: anytype) !void {
    const set_data = set.fields.set_data;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Set(");
    try tty_config.setColor(writer, .reset);
    var it = set_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{pretty}", .{entry.key_ptr.*});
        if (it.index < set_data.count()) {
            try writer.writeAll(", ");
        }
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintSetIterator(set_iterator: *const builtins.SetIterator, writer: anytype) !void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%SetIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (set_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(state_.set.object())});
        },
        .completed => {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<completed>");
            try tty_config.setColor(writer, .reset);
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintStringIterator(string_iterator: *const builtins.StringIterator, writer: anytype) !void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%StringIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (string_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(state_.it.bytes)});
        },
        .completed => {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<completed>");
            try tty_config.setColor(writer, .reset);
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintPrimitiveWrapper(object: anytype, writer: anytype) !void {
    const tty_config = getTtyConfigForWriter(writer);

    const T = std.meta.Child(@TypeOf(object));
    const name = blk: {
        if (T == builtins.BigInt) break :blk "BigInt";
        if (T == builtins.Boolean) break :blk "Boolean";
        if (T == builtins.Number) break :blk "Number";
        if (T == builtins.String) break :blk "String";
        if (T == builtins.Symbol) break :blk "Symbol";
        @panic("Unhandled object type in prettyPrintPrimitiveWrapper()");
    };
    const value = blk: {
        if (T == builtins.BigInt) break :blk Value.from(object.fields.big_int_data);
        if (T == builtins.Boolean) break :blk Value.from(object.fields.boolean_data);
        if (T == builtins.Number) break :blk Value.from(object.fields.number_data);
        if (T == builtins.String) break :blk Value.from(object.fields.string_data);
        if (T == builtins.Symbol) break :blk Value.from(object.fields.symbol_data);
        @panic("Unhandled object type in prettyPrintPrimitiveWrapper()");
    };

    try tty_config.setColor(writer, .white);
    try writer.writeAll(name);
    try writer.writeAll("(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}", .{value});
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintFunction(object: Object, writer: anytype) !void {
    const name = getFunctionName(object);
    const tty_config = getTtyConfigForWriter(writer);

    try writer.writeAll("fn ");
    if (name.len != 0) {
        try tty_config.setColor(writer, .red);
        try writer.writeAll(name);
        try tty_config.setColor(writer, .reset);
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<anonymous>");
        try tty_config.setColor(writer, .reset);
    }
}

fn prettyPrintObject(object: Object, writer: anytype) !void {
    const property_storage = object.data.property_storage;
    const property_keys = ordinaryOwnPropertyKeys(object) catch return;
    const length = property_keys.items.len;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("{");
    try tty_config.setColor(writer, .reset);
    if (length != 0) try writer.writeAll(" ");
    for (property_keys.items, 0..) |property_key, i| {
        const property_descriptor = property_storage.get(property_key).?;
        switch (property_key) {
            .string => |string| {
                try writer.writeAll("\"");
                try tty_config.setColor(writer, .bold);
                try writer.print("{}", .{string});
                try tty_config.setColor(writer, .reset);
                try writer.writeAll("\"");
            },
            .symbol => |symbol| {
                try writer.writeAll("[");
                try tty_config.setColor(writer, .bold);
                try writer.print("{}", .{symbol});
                try tty_config.setColor(writer, .reset);
                try writer.writeAll("]");
            },
            .integer_index => |integer_index| {
                try writer.writeAll("\"");
                try tty_config.setColor(writer, .bold);
                try writer.print("{}", .{integer_index});
                try tty_config.setColor(writer, .reset);
                try writer.writeAll("\"");
            },
        }
        try writer.writeAll(": ");
        if (property_descriptor.value) |value| {
            try writer.print("{pretty}", .{value});
        } else {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<accessor>");
            try tty_config.setColor(writer, .reset);
        }
        if (i + 1 < length) try writer.writeAll(", ");
    }
    if (length != 0) try writer.writeAll(" ");
    try tty_config.setColor(writer, .white);
    try writer.writeAll("}");
    try tty_config.setColor(writer, .reset);
}

pub fn prettyPrintValue(value: Value, writer: anytype) !void {
    const print_in_progress = state.print_in_progress;
    state.print_in_progress = true;
    defer if (!print_in_progress) {
        state.seen_objects.clearRetainingCapacity();
        state.print_in_progress = false;
    };

    const tty_config = getTtyConfigForWriter(writer);

    if (value == .object) {
        const object = value.object;
        if (state.seen_objects.get(object.ptr)) |i| {
            try tty_config.setColor(writer, .dim);
            try writer.print("<ref #{}>", .{i});
            try tty_config.setColor(writer, .reset);
            return;
        }
        state.seen_objects.putNoClobber(object.ptr, state.seen_objects.count()) catch return;

        inline for (.{
            .{ builtins.Array, prettyPrintArray },
            .{ builtins.ArrayIterator, prettyPrintArrayIterator },
            .{ builtins.BigInt, prettyPrintPrimitiveWrapper },
            .{ builtins.Boolean, prettyPrintPrimitiveWrapper },
            .{ builtins.Date, prettyPrintDate },
            .{ builtins.Error, prettyPrintError },
            .{ builtins.Map, prettyPrintMap },
            .{ builtins.MapIterator, prettyPrintMapIterator },
            .{ builtins.Number, prettyPrintPrimitiveWrapper },
            .{ builtins.Proxy, prettyPrintProxy },
            .{ builtins.Set, prettyPrintSet },
            .{ builtins.SetIterator, prettyPrintSetIterator },
            .{ builtins.String, prettyPrintPrimitiveWrapper },
            .{ builtins.StringIterator, prettyPrintStringIterator },
            .{ builtins.Symbol, prettyPrintPrimitiveWrapper },
        }) |entry| {
            const T = entry[0];
            const prettyPrintFn = entry[1];
            if (object.is(T)) return prettyPrintFn(object.as(T), writer);
        }
        if (object.internalMethods().call != null)
            return prettyPrintFunction(object, writer);
        return prettyPrintObject(object, writer);
    }

    const color = switch (value) {
        .undefined => Color.bright_black,
        .null => Color.yellow,
        .boolean => Color.blue,
        .string => Color.green,
        .symbol => Color.cyan,
        .number => Color.magenta,
        .big_int => Color.magenta,
        .object => unreachable,
    };
    try tty_config.setColor(writer, color);
    try writer.print("{}", .{value});
    try tty_config.setColor(writer, .reset);
}
