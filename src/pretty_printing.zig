const builtin = @import("builtin");
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

var fba_buf: [@sizeOf(*anyopaque) * 1024]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&fba_buf);
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

fn PrettyPrintError(comptime Writer: type) type {
    return Writer.Error || if (builtin.os.tag == .windows)
        std.os.windows.SetConsoleTextAttributeError
    else
        error{};
}

fn prettyPrintArray(
    array: *const builtins.Array,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintArrayBuffer(
    array_buffer: *const builtins.ArrayBuffer,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("ArrayBuffer(");
    try tty_config.setColor(writer, .reset);
    if (array_buffer.fields.array_buffer_data) |data| {
        try writer.print("byteLength: {pretty}", .{Value.from(data.items.len)});
        if (array_buffer.fields.array_buffer_max_byte_length) |max_byte_length| {
            try writer.print(", maxByteLength: {pretty}", .{Value.from(max_byte_length)});
        }
        if (data.items.len != 0) {
            try writer.writeAll(", data: ");
            try tty_config.setColor(writer, .dim);
            // Like std.fmt.fmtSliceHexLower() but with a space between each bytes
            const charset = "0123456789abcdef";
            var buf: [2]u8 = undefined;
            for (data.items, 0..) |c, i| {
                if (i != 0) try writer.writeAll(" ");
                buf[0] = charset[c >> 4];
                buf[1] = charset[c & 15];
                try writer.writeAll(&buf);
            }
            try tty_config.setColor(writer, .reset);
        }
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<detached>");
        try tty_config.setColor(writer, .reset);
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintArrayIterator(
    array_iterator: *const builtins.ArrayIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintDataView(
    date: *const builtins.DataView,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const viewed_array_buffer = date.fields.viewed_array_buffer;
    const byte_length = date.fields.byte_length;
    const byte_offset = date.fields.byte_offset;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("DataView(");
    try tty_config.setColor(writer, .reset);
    try writer.print("arrayBuffer: {pretty}", .{Value.from(viewed_array_buffer.object())});
    if (byte_length != .auto) {
        try writer.print(", byteLength: {pretty}", .{Value.from(byte_length.value)});
    }
    if (byte_offset != 0) {
        try writer.print(", byteOffset: {pretty}", .{Value.from(byte_offset)});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintDate(
    date: *const builtins.Date,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const date_value = date.fields.date_value;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Date(");
    try writer.print("{pretty}", .{Value.from(date_value)});
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintError(
    @"error": *const builtins.Error,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintGenerator(
    _: *const builtins.Generator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Generator()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintMap(
    map: *const builtins.Map,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintMapIterator(
    map_iterator: *const builtins.MapIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintPromise(
    promise: *const builtins.Promise,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const promise_state = promise.fields.promise_state;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Promise(");
    try tty_config.setColor(writer, .reset);
    switch (promise_state) {
        .pending => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<pending>");
            try tty_config.setColor(writer, .reset);
        },
        .fulfilled => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .green);
            try writer.writeAll("<fulfilled>");
            try tty_config.setColor(writer, .reset);
            try writer.print(", result: {pretty}", .{promise.fields.promise_result});
        },
        .rejected => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .red);
            try writer.writeAll("<rejected>");
            try tty_config.setColor(writer, .reset);
            try writer.print(", result: {pretty}", .{promise.fields.promise_result});
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintProxy(
    proxy: *const builtins.Proxy,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const proxy_target = proxy.fields.proxy_target;
    const proxy_handler = proxy.fields.proxy_handler;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Proxy(");
    try tty_config.setColor(writer, .reset);
    if (proxy_target != null and proxy_handler != null) {
        try writer.print(
            "target: {pretty}, handler: {pretty}",
            .{ Value.from(proxy_target.?), Value.from(proxy_handler.?) },
        );
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<revoked>");
        try tty_config.setColor(writer, .reset);
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintRegExp(
    reg_exp: *const builtins.RegExp,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const original_source = reg_exp.fields.original_source;
    const original_flags = reg_exp.fields.original_flags;
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("RegExp(");
    try tty_config.setColor(writer, .green);
    try writer.print("/{s}/{s}", .{ original_source, original_flags });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintRegExpStringIterator(
    reg_exp_string_iterator: *const builtins.RegExpStringIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%RegExpStringIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (reg_exp_string_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}, {pretty}", .{
                Value.from(state_.reg_exp),
                Value.from(state_.string),
            });
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

fn prettyPrintSet(
    set: *const builtins.Set,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintSetIterator(
    set_iterator: *const builtins.SetIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintStringIterator(
    string_iterator: *const builtins.StringIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintAsyncGenerator(
    _: *const builtins.AsyncGenerator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("AsyncGenerator()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintPrimitiveWrapper(
    object: anytype,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
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

fn prettyPrintFunction(object: Object, writer: anytype) PrettyPrintError(@TypeOf(writer))!void {
    const name = getFunctionName(object);
    const tty_config = getTtyConfigForWriter(writer);

    if (object.is(builtins.ECMAScriptFunction)) {
        const function_body = object.as(builtins.ECMAScriptFunction).fields.ecmascript_code;
        switch (function_body.type) {
            .normal => try writer.writeAll("fn "),
            .generator => try writer.writeAll("fn* "),
            .@"async" => try writer.writeAll("async fn "),
            .async_generator => try writer.writeAll("async fn* "),
        }
    } else {
        try writer.writeAll("fn ");
    }
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

fn prettyPrintObject(object: Object, writer: anytype) PrettyPrintError(@TypeOf(writer))!void {
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

pub fn prettyPrintValue(value: Value, writer: anytype) PrettyPrintError(@TypeOf(writer))!void {
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
            .{ builtins.ArrayBuffer, prettyPrintArrayBuffer },
            .{ builtins.ArrayIterator, prettyPrintArrayIterator },
            .{ builtins.AsyncGenerator, prettyPrintAsyncGenerator },
            .{ builtins.BigInt, prettyPrintPrimitiveWrapper },
            .{ builtins.Boolean, prettyPrintPrimitiveWrapper },
            .{ builtins.DataView, prettyPrintDataView },
            .{ builtins.Date, prettyPrintDate },
            .{ builtins.Error, prettyPrintError },
            .{ builtins.Generator, prettyPrintGenerator },
            .{ builtins.Map, prettyPrintMap },
            .{ builtins.MapIterator, prettyPrintMapIterator },
            .{ builtins.Number, prettyPrintPrimitiveWrapper },
            .{ builtins.Promise, prettyPrintPromise },
            .{ builtins.Proxy, prettyPrintProxy },
            .{ builtins.RegExp, prettyPrintRegExp },
            .{ builtins.RegExpStringIterator, prettyPrintRegExpStringIterator },
            .{ builtins.Set, prettyPrintSet },
            .{ builtins.SetIterator, prettyPrintSetIterator },
            .{ builtins.String, prettyPrintPrimitiveWrapper },
            .{ builtins.StringIterator, prettyPrintStringIterator },
            .{ builtins.Symbol, prettyPrintPrimitiveWrapper },
        }) |entry| {
            const T, const prettyPrintFn = entry;
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
