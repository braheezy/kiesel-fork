const builtin = @import("builtin");
const std = @import("std");

const build_options = @import("build-options");
const builtins = @import("builtins.zig");
const types = @import("types.zig");

const BigInt = types.BigInt;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const getArrayLength = builtins.array.getArrayLength;
const makeTypedArrayWithBufferWitnessRecord = builtins.makeTypedArrayWithBufferWitnessRecord;
const ordinaryOwnPropertyKeys = builtins.ordinaryOwnPropertyKeys;
const typedArrayLength = builtins.typedArrayLength;
const weakRefDeref = builtins.weakRefDeref;

const State = struct {
    seen_objects: std.AutoHashMap(*const Object, usize),
    print_in_progress: bool,
    tty_config: std.io.tty.Config,
};

var fba_buf: [64 * 1024]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&fba_buf);
var arena = std.heap.ArenaAllocator.init(fba.allocator());
pub var state: State = .{
    .seen_objects = .init(arena.allocator()),
    .print_in_progress = false,
    .tty_config = undefined, // Set whenever an `Agent` is created
};

fn asciiString(ascii: []const u8) *const String {
    return String.fromAscii(arena.allocator(), ascii) catch String.fromLiteral("<OOM>");
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
    const length = getArrayLength(&array.object);
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("[");
    try tty_config.setColor(writer, .reset);
    if (length != 0) try writer.writeAll(" ");
    for (0..length) |i| {
        const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i)));
        if (array.object.shape.properties.get(property_key)) |property_metadata| {
            const property_value = array.object.getPropertyCreateIntrinsicIfNeeded(property_metadata.index) catch return;
            const property_descriptor = property_value.toPropertyDescriptor(property_metadata.attributes);
            if (property_descriptor.value) |value| {
                try writer.print("{pretty}", .{value});
            } else {
                try tty_config.setColor(writer, .dim);
                try writer.writeAll("<accessor>");
                try tty_config.setColor(writer, .reset);
            }
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
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("ArrayBuffer(");
    try tty_config.setColor(writer, .reset);
    if (array_buffer.fields.array_buffer_data) |data| {
        try writer.print("byteLength: {pretty}", .{Value.from(@as(u53, @intCast(data.items.len)))});
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
    const tty_config = state.tty_config;

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

fn prettyPrintAsyncGenerator(
    _: *const builtins.AsyncGenerator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("AsyncGenerator()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintDataView(
    date: *const builtins.DataView,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const viewed_array_buffer = date.fields.viewed_array_buffer;
    const byte_length = date.fields.byte_length;
    const byte_offset = date.fields.byte_offset;
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .red);
    if (error_data.message.isEmpty()) {
        try writer.print("{}", .{error_data.name});
    } else {
        try writer.print("{}: {}", .{ error_data.name, error_data.message });
    }
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintFinalizationRegistry(
    _: *const builtins.FinalizationRegistry,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("FinalizationRegistry()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintGenerator(
    _: *const builtins.Generator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Generator()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIterator(
    _: *const builtins.Iterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Iterator()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIteratorHelper(
    iterator_helper: *const builtins.IteratorHelper,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%IteratorHelper%(");
    try tty_config.setColor(writer, .reset);
    switch (iterator_helper.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(state_.underlying_iterator.iterator)});
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

fn prettyPrintMap(
    map: *const builtins.Map,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const map_data = map.fields.map_data;
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%MapIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (map_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(&state_.map.object)});
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
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

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
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%SetIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (set_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(&state_.set.object)});
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

fn prettyPrintSharedArrayBuffer(
    shared_array_buffer: *const builtins.SharedArrayBuffer,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("SharedArrayBuffer(");
    try tty_config.setColor(writer, .reset);
    const data = shared_array_buffer.fields.array_buffer_data;
    try writer.print("byteLength: {pretty}", .{Value.from(@as(u53, @intCast(data.items.len)))});
    if (shared_array_buffer.fields.array_buffer_max_byte_length) |max_byte_length| {
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
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintStringIterator(
    string_iterator: *const builtins.StringIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%StringIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (string_iterator.fields) {
        .state => |state_| {
            try writer.print("{pretty}", .{Value.from(state_.string)});
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

fn prettyPrintTypedArray(typed_array: *const builtins.TypedArray, writer: anytype) !void {
    const agent = typed_array.object.agent;
    const typed_array_name = typed_array.fields.typed_array_name;
    const viewed_array_buffer = typed_array.fields.viewed_array_buffer;
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.print("{s}(", .{typed_array_name});
    try tty_config.setColor(writer, .reset);
    if (viewed_array_buffer.arrayBufferData()) |data| {
        const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);
        const array_length = typedArrayLength(ta);
        const byte_offset = typed_array.fields.byte_offset;
        try writer.print("length: {pretty}", .{Value.from(array_length)});
        if (data.items.len != 0) {
            try writer.writeAll(", data: ");
            try tty_config.setColor(writer, .white);
            try writer.writeAll("[");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(" ");
            inline for (builtins.typed_array.element_types) |entry| {
                const name, const @"type" = entry;
                if (std.mem.eql(u8, typed_array_name, name)) {
                    const element_size = @"type".elementSize();
                    var i: u53 = 0;
                    while (i < data.items.len) : (i += element_size) {
                        const bytes: *[element_size]u8 = @ptrCast(
                            data.items[@intCast(byte_offset + i)..@intCast(byte_offset + i + element_size)],
                        );
                        const value = std.mem.bytesAsValue(@"type".T, bytes).*;
                        const numeric = if (@"type".isBigIntElementType())
                            Value.from(BigInt.from(agent.gc_allocator, value) catch return)
                        else
                            Value.from(value);
                        if (i != 0) try writer.writeAll(", ");
                        try writer.print("{pretty}", .{numeric});
                    }
                    break;
                }
            }
            try writer.writeAll(" ");
            try tty_config.setColor(writer, .white);
            try writer.writeAll("]");
            try tty_config.setColor(writer, .reset);
        }
    } else {
        // Underlying ArrayBuffer has been detached, mirror behavior of .length getter
        try writer.print("length: {pretty}", .{Value.from(0)});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWeakMap(
    map: *const builtins.WeakMap,
    writer: anytype,
) !void {
    const weak_map_data = map.fields.weak_map_data;
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("WeakMap(");
    try tty_config.setColor(writer, .reset);
    var it = weak_map_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{pretty} → {pretty}", .{ entry.key_ptr.*.get(), entry.value_ptr.* });
        if (it.index < weak_map_data.count()) {
            try writer.writeAll(", ");
        }
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWeakRef(weak_ref: *const builtins.WeakRef, writer: anytype) !void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("WeakRef(");
    try tty_config.setColor(writer, .reset);
    try prettyPrintValue(weakRefDeref(weak_ref), writer);
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWeakSet(weak_set: *const builtins.WeakSet, writer: anytype) !void {
    const weak_set_data = weak_set.fields.weak_set_data;
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("WeakSet(");
    try tty_config.setColor(writer, .reset);
    var it = weak_set_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{pretty}", .{entry.key_ptr.*.get()});
        if (it.index < weak_set_data.count()) {
            try writer.writeAll(", ");
        }
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWrapForValidIterator(
    wrap_for_valid_iterator: *const builtins.WrapForValidIterator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%WrapForValidIterator%(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}", .{Value.from(wrap_for_valid_iterator.fields.iterated.iterator)});
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlCollator(
    intl_collator: *const builtins.intl.Collator,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_collator.object.agent;
    const locale = intl_collator.fields.locale;
    const tty_config = state.tty_config;

    const resolved_options = intl_collator.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.Collator(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}, usage: {pretty}, sensitivity: {pretty}, ignorePunctuation: " ++
        "{pretty}, collation: {pretty}, numeric: {pretty}, caseFirst: {pretty}", .{
        Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
        Value.from(resolved_options.usage),
        Value.from(resolved_options.sensitivity),
        Value.from(resolved_options.ignore_punctuation),
        Value.from(resolved_options.collation),
        Value.from(resolved_options.numeric),
        Value.from(resolved_options.case_first),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}
fn prettyPrintIntlDateTimeFormat(
    intl_date_time_format: *const builtins.intl.DateTimeFormat,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_date_time_format.object.agent;
    const locale = intl_date_time_format.fields.locale;
    const tty_config = state.tty_config;

    const resolved_options = intl_date_time_format.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.DisplayNames(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}, calendar: {pretty}, numberingSystem: {pretty}, timeZone: {pretty}", .{
        Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
        Value.from(resolved_options.calendar),
        Value.from(resolved_options.numbering_system),
        Value.from(resolved_options.time_zone),
    });
    if (resolved_options.date_style) |date_style| {
        try writer.print(", dateStyle: {pretty}", .{Value.from(date_style)});
    }
    if (resolved_options.time_style) |time_style| {
        try writer.print(", timeStyle: {pretty}", .{Value.from(time_style)});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlDisplayNames(
    intl_display_names: *const builtins.intl.DisplayNames,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_display_names.object.agent;
    const locale = intl_display_names.fields.locale;
    const tty_config = state.tty_config;

    const resolved_options = intl_display_names.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.DisplayNames(");
    try tty_config.setColor(writer, .reset);
    if (intl_display_names.fields.type == .language) {
        try writer.print("{pretty}, style: {pretty}, type: {pretty}, fallback: {pretty}, languageDisplay: {pretty}", .{
            Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
            Value.from(resolved_options.style),
            Value.from(resolved_options.type),
            Value.from(resolved_options.fallback),
            Value.from(resolved_options.language_display),
        });
    } else {
        try writer.print("{pretty}, style: {pretty}, type: {pretty}, fallback: {pretty}", .{
            Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
            Value.from(resolved_options.style),
            Value.from(resolved_options.type),
            Value.from(resolved_options.fallback),
        });
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlListFormat(
    intl_list_format: *const builtins.intl.ListFormat,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_list_format.object.agent;
    const locale = intl_list_format.fields.locale;
    const tty_config = state.tty_config;

    const resolved_options = intl_list_format.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.ListFormat(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}, type: {pretty}, style: {pretty}", .{
        Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
        Value.from(resolved_options.type),
        Value.from(resolved_options.style),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlLocale(
    intl_locale: *const builtins.intl.Locale,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_locale.object.agent;
    const locale = intl_locale.fields.locale;
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.Locale(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}", .{
        Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlPluralRules(
    intl_plural_rules: *const builtins.intl.PluralRules,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_plural_rules.object.agent;
    const locale = intl_plural_rules.fields.locale;
    const tty_config = state.tty_config;

    const resolved_options = intl_plural_rules.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.PluralRules(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}, type: {pretty}", .{
        Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
        Value.from(resolved_options.type),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlSegmenter(
    intl_segmenter: *const builtins.intl.Segmenter,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const agent = intl_segmenter.object.agent;
    const locale = intl_segmenter.fields.locale;
    const tty_config = state.tty_config;

    const resolved_options = intl_segmenter.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.Segmenter(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{pretty}, granularity: {pretty}", .{
        Value.from(asciiString(locale.toString(agent.gc_allocator) catch return)),
        Value.from(resolved_options.granularity),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintPrimitiveWrapper(
    object: anytype,
    writer: anytype,
) PrettyPrintError(@TypeOf(writer))!void {
    const tty_config = state.tty_config;

    const T = std.meta.Child(@TypeOf(object));
    const name, const value = switch (T) {
        builtins.BigInt => .{ "BigInt", Value.from(object.fields.big_int_data) },
        builtins.Boolean => .{ "Boolean", Value.from(object.fields.boolean_data) },
        builtins.Number => .{ "Number", Value.from(object.fields.number_data) },
        builtins.String => .{ "String", Value.from(object.fields.string_data) },
        builtins.Symbol => .{ "Symbol", Value.from(object.fields.symbol_data) },
        else => @panic("Unhandled object type in prettyPrintPrimitiveWrapper()"),
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

fn prettyPrintFunction(object: *const Object, writer: anytype) PrettyPrintError(@TypeOf(writer))!void {
    const name = object.getPropertyValueDirect(PropertyKey.from("name")).asString();
    const tty_config = state.tty_config;

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
    if (!name.isEmpty()) {
        try tty_config.setColor(writer, .red);
        try writer.print("{}", .{name});
        try tty_config.setColor(writer, .reset);
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<anonymous>");
        try tty_config.setColor(writer, .reset);
    }
}

fn prettyPrintObject(object: *Object, writer: anytype) PrettyPrintError(@TypeOf(writer))!void {
    const property_keys = ordinaryOwnPropertyKeys(object) catch return;
    const tty_config = state.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("{");
    try tty_config.setColor(writer, .reset);

    var printed_properties: usize = 0;
    for (property_keys.items) |property_key| {
        const property_metadata = object.shape.properties.get(property_key).?;
        const property_value = object.getPropertyCreateIntrinsicIfNeeded(property_metadata.index) catch return;
        const property_descriptor = property_value.toPropertyDescriptor(property_metadata.attributes);
        if (!property_descriptor.enumerable.?) continue;

        if (printed_properties > 0) try writer.writeAll(",");
        printed_properties += 1;
        try writer.writeAll(" ");

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
    }
    if (printed_properties > 0) try writer.writeAll(" ");

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

    const tty_config = state.tty_config;

    if (value.isObject()) {
        const object = value.asObject();
        if (state.seen_objects.get(object)) |i| {
            try tty_config.setColor(writer, .dim);
            try writer.print("<ref #{}>", .{i});
            try tty_config.setColor(writer, .reset);
            return;
        }
        state.seen_objects.putNoClobber(object, state.seen_objects.count()) catch return;

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
            .{ builtins.FinalizationRegistry, prettyPrintFinalizationRegistry },
            .{ builtins.Generator, prettyPrintGenerator },
            .{ builtins.Iterator, prettyPrintIterator },
            .{ builtins.IteratorHelper, prettyPrintIteratorHelper },
            .{ builtins.Map, prettyPrintMap },
            .{ builtins.MapIterator, prettyPrintMapIterator },
            .{ builtins.Number, prettyPrintPrimitiveWrapper },
            .{ builtins.Promise, prettyPrintPromise },
            .{ builtins.Proxy, prettyPrintProxy },
            .{ builtins.RegExp, prettyPrintRegExp },
            .{ builtins.RegExpStringIterator, prettyPrintRegExpStringIterator },
            .{ builtins.Set, prettyPrintSet },
            .{ builtins.SetIterator, prettyPrintSetIterator },
            .{ builtins.SharedArrayBuffer, prettyPrintSharedArrayBuffer },
            .{ builtins.String, prettyPrintPrimitiveWrapper },
            .{ builtins.StringIterator, prettyPrintStringIterator },
            .{ builtins.Symbol, prettyPrintPrimitiveWrapper },
            .{ builtins.TypedArray, prettyPrintTypedArray },
            .{ builtins.WeakMap, prettyPrintWeakMap },
            .{ builtins.WeakRef, prettyPrintWeakRef },
            .{ builtins.WeakSet, prettyPrintWeakSet },
            .{ builtins.WrapForValidIterator, prettyPrintWrapForValidIterator },
        } ++ if (build_options.enable_intl) .{
            .{ builtins.intl.Collator, prettyPrintIntlCollator },
            .{ builtins.intl.DateTimeFormat, prettyPrintIntlDateTimeFormat },
            .{ builtins.intl.DisplayNames, prettyPrintIntlDisplayNames },
            .{ builtins.intl.ListFormat, prettyPrintIntlListFormat },
            .{ builtins.intl.Locale, prettyPrintIntlLocale },
            .{ builtins.intl.PluralRules, prettyPrintIntlPluralRules },
            .{ builtins.intl.Segmenter, prettyPrintIntlSegmenter },
        } else .{}) |entry| {
            const T, const prettyPrintFn = entry;
            if (object.is(T)) return prettyPrintFn(object.as(T), writer);
        }
        // NOTE: This needs to go before pretty-printing functions as it has [[Call]] but no name.
        if (build_options.enable_annex_b and object.is_htmldda) {
            // Keep colors in sync with undefined and null below :^)
            try tty_config.setColor(writer, .bright_black);
            try writer.writeAll("[[");
            try tty_config.setColor(writer, .yellow);
            try writer.writeAll("IsHTMLDDA");
            try tty_config.setColor(writer, .bright_black);
            try writer.writeAll("]]");
            try tty_config.setColor(writer, .reset);
            return;
        }
        if (object.internal_methods.call != null)
            return prettyPrintFunction(object, writer);
        return prettyPrintObject(object, writer);
    }

    const color: std.io.tty.Color = switch (value.type()) {
        .undefined => .bright_black,
        .null => .yellow,
        .boolean => .blue,
        .string => .green,
        .symbol => .cyan,
        .number => .magenta,
        .big_int => .magenta,
        .object => unreachable,
    };
    try tty_config.setColor(writer, color);
    try writer.print("{}", .{value});
    try tty_config.setColor(writer, .reset);
}
