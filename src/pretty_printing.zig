const std = @import("std");

const temporal_rs = @import("c/temporal_rs.zig");

const build_options = @import("build-options");
const builtins = @import("builtins.zig");
const execution = @import("execution.zig");
const types = @import("types.zig");

const Agent = execution.Agent;
const BigInt = types.BigInt;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const fmtToDateString = builtins.date.fmtToDateString;
const getArrayLength = builtins.array.getArrayLength;
const makeTypedArrayWithBufferWitnessRecord = builtins.makeTypedArrayWithBufferWitnessRecord;
const ordinaryOwnPropertyKeys = builtins.ordinaryOwnPropertyKeys;
const typedArrayLength = builtins.typedArrayLength;
const weakRefDeref = builtins.weakRefDeref;

const State = struct {
    seen_objects: std.AutoHashMapUnmanaged(*const Object, usize),
    print_in_progress: bool,
    platform: *const Agent.Platform,
};

var fba_buf: [64 * 1024]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&fba_buf);
var arena = std.heap.ArenaAllocator.init(fba.allocator());
pub var state: State = .{
    .seen_objects = .empty,
    .print_in_progress = false,
    .platform = undefined, // Set whenever an `Agent` is created
};

fn asciiString(ascii: []const u8) *const String {
    const slice: String.Slice = .{ .ascii = ascii };
    const string = arena.allocator().create(String) catch unreachable;
    string.* = .{ .slice = slice, .hash = slice.hash() };
    return string;
}

fn bigInt(value: anytype) *const BigInt {
    var managed = std.math.big.int.Managed.initSet(arena.allocator(), value) catch unreachable;
    errdefer managed.deinit();
    const big_int = arena.allocator().create(BigInt) catch unreachable;
    big_int.* = .{ .managed = managed };
    return big_int;
}

const PrettyPrintError = std.Io.Writer.Error || std.Io.tty.Config.SetColorError;

fn prettyPrintArray(
    array: *const builtins.Array,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const length = getArrayLength(&array.object);
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("[");
    try tty_config.setColor(writer, .reset);
    if (length != 0) try writer.writeAll(" ");
    for (0..length) |i| {
        if (array.object.property_storage.indexed_properties.get(@intCast(i))) |property_descriptor| {
            switch (property_descriptor.value_or_accessor) {
                .value => |value| {
                    try writer.print("{f}", .{value.fmtPretty()});
                },
                .accessor => {
                    try tty_config.setColor(writer, .dim);
                    try writer.writeAll("<accessor>");
                    try tty_config.setColor(writer, .reset);
                },
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    if (array_buffer.fields.data_block) |data_block| {
        try tty_config.setColor(writer, .white);
        if (data_block.shared) {
            try writer.writeAll("SharedArrayBuffer(");
        } else {
            try writer.writeAll("ArrayBuffer(");
        }
        try tty_config.setColor(writer, .reset);
        try writer.print("byteLength: {f}", .{
            Value.from(@intFromEnum(array_buffer.fields.byte_length)).fmtPretty(),
        });
        if (array_buffer.fields.max_byte_length.unwrap()) |max_byte_length| {
            try writer.print(", maxByteLength: {f}", .{
                Value.from(@intFromEnum(max_byte_length)).fmtPretty(),
            });
        }
        if (data_block.bytes.len != 0) {
            try writer.writeAll(", data: ");
            try tty_config.setColor(writer, .dim);
            // Like std.fmt.fmtSliceHexLower() but with a space between each bytes
            const charset = "0123456789abcdef";
            var buf: [2]u8 = undefined;
            for (data_block.bytes, 0..) |c, i| {
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
    } else {
        try tty_config.setColor(writer, .white);
        try writer.writeAll("ArrayBuffer(");
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<detached>");
        try tty_config.setColor(writer, .white);
        try writer.writeAll(")");
        try tty_config.setColor(writer, .reset);
    }
}

fn prettyPrintArrayIterator(
    array_iterator: *const builtins.ArrayIterator,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%ArrayIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (array_iterator.fields) {
        .state => |state_| {
            try writer.print("{f}", .{Value.from(state_.iterated_array_like).fmtPretty()});
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
    async_generator: *const builtins.AsyncGenerator,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("AsyncGenerator(");
    try tty_config.setColor(writer, .reset);
    switch (async_generator.fields.async_generator_state) {
        .suspended_start => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .cyan);
            try writer.writeAll("<suspended-start>");
            try tty_config.setColor(writer, .reset);
        },
        .suspended_yield => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .cyan);
            try writer.writeAll("<suspended-yield>");
            try tty_config.setColor(writer, .reset);
        },
        .executing => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .green);
            try writer.writeAll("<executing>");
            try tty_config.setColor(writer, .reset);
        },
        .draining_queue => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .cyan);
            try writer.writeAll("<draining-queue>");
            try tty_config.setColor(writer, .reset);
        },
        .completed => {
            try writer.writeAll("state: ");
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const viewed_array_buffer = date.fields.viewed_array_buffer;
    const byte_offset = date.fields.byte_offset;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("DataView(");
    try tty_config.setColor(writer, .reset);
    try writer.print("arrayBuffer: {f}", .{Value.from(&viewed_array_buffer.object).fmtPretty()});
    if (date.fields.byte_length.unwrap()) |byte_length| {
        try writer.print(", byteLength: {f}", .{Value.from(@intFromEnum(byte_length)).fmtPretty()});
    }
    if (byte_offset != .zero) {
        try writer.print(", byteOffset: {f}", .{Value.from(@intFromEnum(byte_offset)).fmtPretty()});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintDate(
    date: *const builtins.Date,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const date_value = date.fields.date_value;
    const tty_config = state.platform.tty_config;
    const platform = state.platform;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Date(");
    if (!std.math.isNan(date_value)) {
        try writer.print("{f}", .{
            Value.from(asciiString(std.fmt.allocPrint(
                arena.allocator(),
                "{f}",
                .{fmtToDateString(platform, date_value)},
            ) catch return)).fmtPretty(),
        });
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<invalid>");
        try tty_config.setColor(writer, .reset);
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintError(
    @"error": *const builtins.Error,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const error_data = @"error".fields.error_data;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .red);
    try writer.print("{f}", .{error_data.name.fmtRaw()});
    if (!error_data.message.isEmpty()) {
        try writer.print(": {f}", .{error_data.message.fmtRaw()});
    }
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintFinalizationRegistry(
    _: *const builtins.FinalizationRegistry,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("FinalizationRegistry()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintGenerator(
    generator: *const builtins.Generator,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Generator(");
    try tty_config.setColor(writer, .reset);
    switch (generator.fields.generator_state) {
        .suspended_start => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .cyan);
            try writer.writeAll("<suspended-start>");
            try tty_config.setColor(writer, .reset);
        },
        .suspended_yield => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .cyan);
            try writer.writeAll("<suspended-yield>");
            try tty_config.setColor(writer, .reset);
        },
        .executing => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .green);
            try writer.writeAll("<executing>");
            try tty_config.setColor(writer, .reset);
        },
        .completed => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<completed>");
            try tty_config.setColor(writer, .reset);
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIterator(
    _: *const builtins.Iterator,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Iterator()");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIteratorHelper(
    iterator_helper: *const builtins.IteratorHelper,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%IteratorHelper%(");
    try tty_config.setColor(writer, .reset);
    switch (iterator_helper.fields) {
        .state => |state_| {
            try writer.print("{f}", .{Value.from(state_.underlying_iterator.iterator).fmtPretty()});
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const map_data = map.fields.map_data;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Map(");
    try tty_config.setColor(writer, .reset);
    var it = map_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{f} → {f}", .{ entry.key_ptr.fmtPretty(), entry.value_ptr.fmtPretty() });
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%MapIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (map_iterator.fields) {
        .state => |state_| {
            try writer.print("{f}", .{Value.from(&state_.map.object).fmtPretty()});
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const promise_state = promise.fields.promise_state;
    const tty_config = state.platform.tty_config;

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
            try writer.print(", result: {f}", .{promise.fields.promise_result.fmtPretty()});
        },
        .rejected => {
            try writer.writeAll("state: ");
            try tty_config.setColor(writer, .red);
            try writer.writeAll("<rejected>");
            try tty_config.setColor(writer, .reset);
            try writer.print(", result: {f}", .{promise.fields.promise_result.fmtPretty()});
        },
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintProxy(
    proxy: *const builtins.Proxy,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const proxy_target = proxy.fields.proxy_target;
    const proxy_handler = proxy.fields.proxy_handler;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Proxy(");
    try tty_config.setColor(writer, .reset);
    if (proxy_target != null and proxy_handler != null) {
        try writer.print("target: {f}, handler: {f}", .{
            Value.from(proxy_target.?).fmtPretty(),
            Value.from(proxy_handler.?).fmtPretty(),
        });
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const original_source = reg_exp.fields.original_source;
    const original_flags = reg_exp.fields.original_flags;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("RegExp(");
    try tty_config.setColor(writer, .green);
    try writer.print("/{f}/{f}", .{ original_source.fmtRaw(), original_flags.fmtRaw() });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintRegExpStringIterator(
    reg_exp_string_iterator: *const builtins.RegExpStringIterator,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%RegExpStringIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (reg_exp_string_iterator.fields) {
        .state => |state_| {
            try writer.print("{f}, {f}", .{
                Value.from(state_.iterating_reg_exp).fmtPretty(),
                Value.from(state_.iterated_string).fmtPretty(),
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const set_data = set.fields.set_data;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Set(");
    try tty_config.setColor(writer, .reset);
    var it = set_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{f}", .{entry.key_ptr.fmtPretty()});
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%SetIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (set_iterator.fields) {
        .state => |state_| {
            try writer.print("{f}", .{Value.from(&state_.set.object).fmtPretty()});
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%StringIterator%(");
    try tty_config.setColor(writer, .reset);
    switch (string_iterator.fields) {
        .state => |state_| {
            try writer.print("{f}", .{Value.from(state_.string).fmtPretty()});
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

fn prettyPrintTypedArray(typed_array: *const builtins.TypedArray, writer: *std.Io.Writer) !void {
    const element_type = typed_array.fields.element_type;
    const viewed_array_buffer = typed_array.fields.viewed_array_buffer;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.print("{s}(", .{element_type.typedArrayName()});
    try tty_config.setColor(writer, .reset);
    if (viewed_array_buffer.fields.data_block) |data_block| {
        const ta = makeTypedArrayWithBufferWitnessRecord(@constCast(typed_array), .seq_cst);
        const array_length = typedArrayLength(ta);
        const byte_offset = typed_array.fields.byte_offset;
        try writer.print("length: {f}", .{Value.from(@intFromEnum(array_length)).fmtPretty()});
        if (data_block.bytes.len != 0) {
            try writer.writeAll(", data: ");
            try tty_config.setColor(writer, .white);
            try writer.writeAll("[");
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(" ");
            switch (element_type) {
                inline else => |@"type"| {
                    const element_size = @"type".elementSize();
                    var i: u53 = 0;
                    while (i < data_block.bytes.len) : (i += element_size) {
                        const bytes: *[element_size]u8 = @ptrCast(
                            data_block.bytes[@intCast(@intFromEnum(byte_offset) + i)..@intCast(@intFromEnum(byte_offset) + i + element_size)],
                        );
                        const value = std.mem.bytesAsValue(@"type".type(), bytes).*;
                        const numeric = if (@"type".isBigIntElementType())
                            Value.from(bigInt(value))
                        else
                            Value.from(value);
                        if (i != 0) try writer.writeAll(", ");
                        try writer.print("{f}", .{numeric.fmtPretty()});
                    }
                },
            }
            try writer.writeAll(" ");
            try tty_config.setColor(writer, .white);
            try writer.writeAll("]");
            try tty_config.setColor(writer, .reset);
        }
    } else {
        // Underlying ArrayBuffer has been detached, mirror behavior of .length getter
        try writer.print("length: {f}", .{Value.from(0).fmtPretty()});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWeakMap(
    map: *const builtins.WeakMap,
    writer: *std.Io.Writer,
) !void {
    const weak_map_data = map.fields.weak_map_data;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("WeakMap(");
    try tty_config.setColor(writer, .reset);
    var it = weak_map_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{f} → {f}", .{ entry.key_ptr.get().fmtPretty(), entry.value_ptr.fmtPretty() });
        if (it.index < weak_map_data.count()) {
            try writer.writeAll(", ");
        }
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWeakRef(weak_ref: *const builtins.WeakRef, writer: *std.Io.Writer) !void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("WeakRef(");
    try tty_config.setColor(writer, .reset);
    try prettyPrintValue(weakRefDeref(weak_ref), writer);
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintWeakSet(weak_set: *const builtins.WeakSet, writer: *std.Io.Writer) !void {
    const weak_set_data = weak_set.fields.weak_set_data;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("WeakSet(");
    try tty_config.setColor(writer, .reset);
    var it = weak_set_data.iterator();
    while (it.next()) |entry| {
        try writer.print("{f}", .{entry.key_ptr.get().fmtPretty()});
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
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("%WrapForValidIterator%(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}", .{Value.from(wrap_for_valid_iterator.fields.iterated.iterator).fmtPretty()});
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlCollator(
    intl_collator: *const builtins.intl.Collator,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_collator.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_collator.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.Collator(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, usage: {f}, sensitivity: {f}, ignorePunctuation: " ++
        "{f}, collation: {f}, numeric: {f}, caseFirst: {f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
        Value.from(resolved_options.usage).fmtPretty(),
        Value.from(resolved_options.sensitivity).fmtPretty(),
        Value.from(resolved_options.ignore_punctuation).fmtPretty(),
        Value.from(resolved_options.collation).fmtPretty(),
        Value.from(resolved_options.numeric).fmtPretty(),
        Value.from(resolved_options.case_first).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlDateTimeFormat(
    intl_date_time_format: *const builtins.intl.DateTimeFormat,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_date_time_format.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_date_time_format.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.DisplayNames(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, calendar: {f}, numberingSystem: {f}, timeZone: {f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
        Value.from(resolved_options.calendar).fmtPretty(),
        Value.from(resolved_options.numbering_system).fmtPretty(),
        Value.from(resolved_options.time_zone).fmtPretty(),
    });
    if (resolved_options.date_style) |date_style| {
        try writer.print(", dateStyle: {f}", .{Value.from(date_style).fmtPretty()});
    }
    if (resolved_options.time_style) |time_style| {
        try writer.print(", timeStyle: {f}", .{Value.from(time_style).fmtPretty()});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlDisplayNames(
    intl_display_names: *const builtins.intl.DisplayNames,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_display_names.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_display_names.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.DisplayNames(");
    try tty_config.setColor(writer, .reset);
    if (intl_display_names.fields.type == .language) {
        try writer.print("{f}, style: {f}, type: {f}, fallback: {f}, languageDisplay: {f}", .{
            Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
            Value.from(resolved_options.style).fmtPretty(),
            Value.from(resolved_options.type).fmtPretty(),
            Value.from(resolved_options.fallback).fmtPretty(),
            Value.from(resolved_options.language_display).fmtPretty(),
        });
    } else {
        try writer.print("{f}, style: {f}, type: {f}, fallback: {f}", .{
            Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
            Value.from(resolved_options.style).fmtPretty(),
            Value.from(resolved_options.type).fmtPretty(),
            Value.from(resolved_options.fallback).fmtPretty(),
        });
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlDurationFormat(
    intl_duration_format: *const builtins.intl.DurationFormat,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_duration_format.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_duration_format.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.DurationFormat(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, numberingSystem: {f}, style: {f}, years: {f}, " ++
        "yearsDisplay: {f}, months: {f}, monthsDisplay: {f}, weeks: {f}, " ++
        "weeksDisplay: {f}, days: {f}, daysDisplay: {f}, hours: {f}, " ++
        "hoursDisplay: {f}, minutes: {f}, minutesDisplay: {f}, seconds: {f}, " ++
        "secondsDisplay: {f}, milliseconds: {f}, millisecondsDisplay: {f}, " ++
        "microseconds: {f}, microsecondsDisplay: {f}, nanoseconds: {f}, " ++
        "nanosecondsDisplay: {f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
        Value.from(resolved_options.numbering_system).fmtPretty(),
        Value.from(resolved_options.style).fmtPretty(),
        Value.from(resolved_options.years).fmtPretty(),
        Value.from(resolved_options.years_display).fmtPretty(),
        Value.from(resolved_options.months).fmtPretty(),
        Value.from(resolved_options.months_display).fmtPretty(),
        Value.from(resolved_options.weeks).fmtPretty(),
        Value.from(resolved_options.weeks_display).fmtPretty(),
        Value.from(resolved_options.days).fmtPretty(),
        Value.from(resolved_options.days_display).fmtPretty(),
        Value.from(resolved_options.hours).fmtPretty(),
        Value.from(resolved_options.hours_display).fmtPretty(),
        Value.from(resolved_options.minutes).fmtPretty(),
        Value.from(resolved_options.minutes_display).fmtPretty(),
        Value.from(resolved_options.seconds).fmtPretty(),
        Value.from(resolved_options.seconds_display).fmtPretty(),
        Value.from(resolved_options.milliseconds).fmtPretty(),
        Value.from(resolved_options.milliseconds_display).fmtPretty(),
        Value.from(resolved_options.microseconds).fmtPretty(),
        Value.from(resolved_options.microseconds_display).fmtPretty(),
        Value.from(resolved_options.nanoseconds).fmtPretty(),
        Value.from(resolved_options.nanoseconds_display).fmtPretty(),
    });
    if (resolved_options.fractional_digits) |fractional_digits| {
        try writer.print(", fractionalDigits: {f}", .{Value.from(fractional_digits).fmtPretty()});
    }
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlListFormat(
    intl_list_format: *const builtins.intl.ListFormat,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_list_format.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_list_format.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.ListFormat(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, type: {f}, style: {f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
        Value.from(resolved_options.type).fmtPretty(),
        Value.from(resolved_options.style).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlLocale(
    intl_locale: *const builtins.intl.Locale,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_locale.fields.locale;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.Locale(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlPluralRules(
    intl_plural_rules: *const builtins.intl.PluralRules,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_plural_rules.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_plural_rules.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.PluralRules(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, type: {f}, notation: {f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
        Value.from(resolved_options.type).fmtPretty(),
        Value.from(resolved_options.notation).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintIntlSegmenter(
    intl_segmenter: *const builtins.intl.Segmenter,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const locale = intl_segmenter.fields.locale;
    const tty_config = state.platform.tty_config;

    const resolved_options = intl_segmenter.fields.resolvedOptions();

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Intl.Segmenter(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, granularity: {f}", .{
        Value.from(asciiString(locale.toString(arena.allocator()) catch return)).fmtPretty(),
        Value.from(resolved_options.granularity).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalDuration(
    temporal_duration: *const builtins.temporal.Duration,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const years = temporal_rs.c.temporal_rs_Duration_years(temporal_duration.fields.inner);
    const months = temporal_rs.c.temporal_rs_Duration_months(temporal_duration.fields.inner);
    const weeks = temporal_rs.c.temporal_rs_Duration_weeks(temporal_duration.fields.inner);
    const days = temporal_rs.c.temporal_rs_Duration_days(temporal_duration.fields.inner);
    const hours = temporal_rs.c.temporal_rs_Duration_hours(temporal_duration.fields.inner);
    const minutes = temporal_rs.c.temporal_rs_Duration_minutes(temporal_duration.fields.inner);
    const seconds = temporal_rs.c.temporal_rs_Duration_seconds(temporal_duration.fields.inner);
    const milliseconds = temporal_rs.c.temporal_rs_Duration_milliseconds(temporal_duration.fields.inner);
    const microseconds = temporal_rs.c.temporal_rs_Duration_microseconds(temporal_duration.fields.inner);
    const nanoseconds = temporal_rs.c.temporal_rs_Duration_nanoseconds(temporal_duration.fields.inner);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.Duration(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}, {f}, {f}, {f}, {f}, {f}, {f}, {f}", .{
        Value.from(@as(f64, @floatFromInt(years))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(months))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(weeks))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(days))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(hours))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(minutes))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(seconds))).fmtPretty(),
        Value.from(@as(f64, @floatFromInt(milliseconds))).fmtPretty(),
        Value.from(microseconds).fmtPretty(),
        Value.from(nanoseconds).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalInstant(
    temporal_instant: *const builtins.temporal.Instant,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const epoch_nanoseconds = temporal_rs.fromI128Nanoseconds(
        temporal_rs.c.temporal_rs_Instant_epoch_nanoseconds(temporal_instant.fields.inner),
    );

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.Instant(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}", .{
        Value.from(bigInt(epoch_nanoseconds)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalPlainDate(
    temporal_plain_date: *const builtins.temporal.PlainDate,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const year = temporal_rs.c.temporal_rs_PlainDate_year(temporal_plain_date.fields.inner);
    const month = temporal_rs.c.temporal_rs_PlainDate_month(temporal_plain_date.fields.inner);
    const day = temporal_rs.c.temporal_rs_PlainDate_day(temporal_plain_date.fields.inner);
    const calendar = temporal_rs.c.temporal_rs_PlainDate_calendar(temporal_plain_date.fields.inner);
    const calendar_id = temporal_rs.fromDiplomatStringView(temporal_rs.c.temporal_rs_Calendar_identifier(calendar));

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.PlainDate(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}, {f}", .{
        Value.from(year).fmtPretty(),
        Value.from(month).fmtPretty(),
        Value.from(day).fmtPretty(),
        Value.from(asciiString(calendar_id)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalPlainDateTime(
    temporal_plain_date_time: *const builtins.temporal.PlainDateTime,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const year = temporal_rs.c.temporal_rs_PlainDateTime_year(temporal_plain_date_time.fields.inner);
    const month = temporal_rs.c.temporal_rs_PlainDateTime_month(temporal_plain_date_time.fields.inner);
    const day = temporal_rs.c.temporal_rs_PlainDateTime_day(temporal_plain_date_time.fields.inner);
    const hour = temporal_rs.c.temporal_rs_PlainDateTime_hour(temporal_plain_date_time.fields.inner);
    const minute = temporal_rs.c.temporal_rs_PlainDateTime_minute(temporal_plain_date_time.fields.inner);
    const second = temporal_rs.c.temporal_rs_PlainDateTime_second(temporal_plain_date_time.fields.inner);
    const millisecond = temporal_rs.c.temporal_rs_PlainDateTime_millisecond(temporal_plain_date_time.fields.inner);
    const microsecond = temporal_rs.c.temporal_rs_PlainDateTime_microsecond(temporal_plain_date_time.fields.inner);
    const nanosecond = temporal_rs.c.temporal_rs_PlainDateTime_nanosecond(temporal_plain_date_time.fields.inner);
    const calendar = temporal_rs.c.temporal_rs_PlainDateTime_calendar(temporal_plain_date_time.fields.inner);
    const calendar_id = temporal_rs.fromDiplomatStringView(temporal_rs.c.temporal_rs_Calendar_identifier(calendar));

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.PlainDateTime(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}, {f}, {f}, {f}, {f}, {f}, {f}, {f}", .{
        Value.from(year).fmtPretty(),
        Value.from(month).fmtPretty(),
        Value.from(day).fmtPretty(),
        Value.from(hour).fmtPretty(),
        Value.from(minute).fmtPretty(),
        Value.from(second).fmtPretty(),
        Value.from(millisecond).fmtPretty(),
        Value.from(microsecond).fmtPretty(),
        Value.from(nanosecond).fmtPretty(),
        Value.from(asciiString(calendar_id)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalPlainMonthDay(
    temporal_plain_month_day: *const builtins.temporal.PlainMonthDay,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    var write = temporal_rs.DiplomatWrite.init(arena.allocator());
    temporal_rs.c.temporal_rs_PlainMonthDay_month_code(temporal_plain_month_day.fields.inner, &write.inner);
    const month_code = write.toOwnedSlice() catch return;
    const day = temporal_rs.c.temporal_rs_PlainMonthDay_day(temporal_plain_month_day.fields.inner);
    const calendar = temporal_rs.c.temporal_rs_PlainMonthDay_calendar(temporal_plain_month_day.fields.inner);
    const calendar_id = temporal_rs.fromDiplomatStringView(temporal_rs.c.temporal_rs_Calendar_identifier(calendar));

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.PlainMonthDay(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}", .{
        Value.from(asciiString(month_code)).fmtPretty(),
        Value.from(day).fmtPretty(),
        Value.from(asciiString(calendar_id)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalPlainTime(
    temporal_plain_time: *const builtins.temporal.PlainTime,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const hour = temporal_rs.c.temporal_rs_PlainTime_hour(temporal_plain_time.fields.inner);
    const minute = temporal_rs.c.temporal_rs_PlainTime_minute(temporal_plain_time.fields.inner);
    const second = temporal_rs.c.temporal_rs_PlainTime_second(temporal_plain_time.fields.inner);
    const millisecond = temporal_rs.c.temporal_rs_PlainTime_millisecond(temporal_plain_time.fields.inner);
    const microsecond = temporal_rs.c.temporal_rs_PlainTime_microsecond(temporal_plain_time.fields.inner);
    const nanosecond = temporal_rs.c.temporal_rs_PlainTime_nanosecond(temporal_plain_time.fields.inner);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.PlainTime(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}, {f}, {f}, {f}", .{
        Value.from(hour).fmtPretty(),
        Value.from(minute).fmtPretty(),
        Value.from(second).fmtPretty(),
        Value.from(millisecond).fmtPretty(),
        Value.from(microsecond).fmtPretty(),
        Value.from(nanosecond).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalPlainYearMonth(
    temporal_plain_year_month: *const builtins.temporal.PlainYearMonth,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const year = temporal_rs.c.temporal_rs_PlainYearMonth_year(temporal_plain_year_month.fields.inner);
    const month = temporal_rs.c.temporal_rs_PlainYearMonth_month(temporal_plain_year_month.fields.inner);
    const calendar = temporal_rs.c.temporal_rs_PlainYearMonth_calendar(temporal_plain_year_month.fields.inner);
    const calendar_id = temporal_rs.fromDiplomatStringView(temporal_rs.c.temporal_rs_Calendar_identifier(calendar));

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.PlainYearMonth(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}", .{
        Value.from(year).fmtPretty(),
        Value.from(month).fmtPretty(),
        Value.from(asciiString(calendar_id)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintTemporalZonedDateTime(
    temporal_zoned_date_time: *const builtins.temporal.ZonedDateTime,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

    const epoch_nanoseconds = temporal_rs.fromI128Nanoseconds(
        temporal_rs.c.temporal_rs_ZonedDateTime_epoch_nanoseconds(temporal_zoned_date_time.fields.inner),
    );
    const time_zone = temporal_rs.c.temporal_rs_ZonedDateTime_timezone(temporal_zoned_date_time.fields.inner);
    var write = temporal_rs.DiplomatWrite.init(arena.allocator());
    temporal_rs.c.temporal_rs_TimeZone_identifier(time_zone, &write.inner);
    const time_zone_id = write.toOwnedSlice() catch return;
    const calendar = temporal_rs.c.temporal_rs_ZonedDateTime_calendar(temporal_zoned_date_time.fields.inner);
    const calendar_id = temporal_rs.fromDiplomatStringView(temporal_rs.c.temporal_rs_Calendar_identifier(calendar));

    try tty_config.setColor(writer, .white);
    try writer.writeAll("Temporal.ZonedDateTime(");
    try tty_config.setColor(writer, .reset);
    try writer.print("{f}, {f}, {f}", .{
        Value.from(bigInt(epoch_nanoseconds)).fmtPretty(),
        Value.from(asciiString(time_zone_id)).fmtPretty(),
        Value.from(asciiString(calendar_id)).fmtPretty(),
    });
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintPrimitiveWrapper(
    object: anytype,
    writer: *std.Io.Writer,
) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;

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
    try writer.print("{f}", .{value.fmtPretty()});
    try tty_config.setColor(writer, .white);
    try writer.writeAll(")");
    try tty_config.setColor(writer, .reset);
}

fn prettyPrintFunction(object: *const Object, writer: *std.Io.Writer) PrettyPrintError!void {
    const name = object.getPropertyValueDirect(PropertyKey.from("name")).asString();
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .bold);
    try tty_config.setColor(writer, .blue);
    if (object.cast(builtins.ECMAScriptFunction)) |ecmascript_function| {
        const function_body = ecmascript_function.fields.ecmascript_code;
        switch (function_body.type) {
            .normal => try writer.writeAll("fn "),
            .generator => try writer.writeAll("fn* "),
            .async => try writer.writeAll("async fn "),
            .async_generator => try writer.writeAll("async fn* "),
        }
    } else {
        try writer.writeAll("fn ");
    }
    try tty_config.setColor(writer, .reset);
    if (!name.isEmpty()) {
        try writer.print("{f}", .{name.fmtRaw()});
    } else {
        try tty_config.setColor(writer, .dim);
        try writer.writeAll("<anonymous>");
        try tty_config.setColor(writer, .reset);
    }
}

fn prettyPrintObject(object: *Object, writer: *std.Io.Writer) PrettyPrintError!void {
    const property_keys = ordinaryOwnPropertyKeys(arena.allocator(), object) catch return;
    const tty_config = state.platform.tty_config;

    try tty_config.setColor(writer, .white);
    try writer.writeAll("{");
    try tty_config.setColor(writer, .reset);

    var printed_properties: usize = 0;
    for (property_keys) |property_key| {
        const property_descriptor = (object.property_storage.getCreateIntrinsicIfNeeded(property_key) catch return).?;
        if (!property_descriptor.attributes.enumerable) continue;

        if (printed_properties > 0) try writer.writeAll(",");
        printed_properties += 1;
        try writer.writeAll(" ");

        switch (property_key) {
            .string => |string| {
                try writer.writeAll("\"");
                try tty_config.setColor(writer, .bold);
                try writer.print("{f}", .{string.fmtEscaped()});
                try tty_config.setColor(writer, .reset);
                try writer.writeAll("\"");
            },
            .symbol => |symbol| {
                try writer.writeAll("[");
                try tty_config.setColor(writer, .bold);
                try writer.print("{f}", .{symbol});
                try tty_config.setColor(writer, .reset);
                try writer.writeAll("]");
            },
            .integer_index => |integer_index| {
                try writer.writeAll("\"");
                try tty_config.setColor(writer, .bold);
                try writer.print("{d}", .{integer_index});
                try tty_config.setColor(writer, .reset);
                try writer.writeAll("\"");
            },
        }
        try writer.writeAll(": ");

        switch (property_descriptor.value_or_accessor) {
            .value => |value| {
                try writer.print("{f}", .{value.fmtPretty()});
            },
            .accessor => {
                try tty_config.setColor(writer, .dim);
                try writer.writeAll("<accessor>");
                try tty_config.setColor(writer, .reset);
            },
        }
    }
    if (printed_properties > 0) try writer.writeAll(" ");

    try tty_config.setColor(writer, .white);
    try writer.writeAll("}");
    try tty_config.setColor(writer, .reset);
}

pub fn prettyPrintValue(value: Value, writer: *std.Io.Writer) PrettyPrintError!void {
    const print_in_progress = state.print_in_progress;
    state.print_in_progress = true;
    defer if (!print_in_progress) {
        state.seen_objects.clearAndFree(arena.allocator());
        state.print_in_progress = false;
        _ = arena.reset(.retain_capacity);
    };

    const tty_config = state.platform.tty_config;

    if (value.isObject()) {
        const object = value.asObject();
        if (state.seen_objects.get(object)) |i| {
            try tty_config.setColor(writer, .dim);
            try writer.print("<ref #{d}>", .{i});
            try tty_config.setColor(writer, .reset);
            return;
        }
        state.seen_objects.putNoClobber(arena.allocator(), object, state.seen_objects.count()) catch return;

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
            .{ builtins.String, prettyPrintPrimitiveWrapper },
            .{ builtins.StringIterator, prettyPrintStringIterator },
            .{ builtins.Symbol, prettyPrintPrimitiveWrapper },
            .{ builtins.TypedArray, prettyPrintTypedArray },
            .{ builtins.WeakMap, prettyPrintWeakMap },
            .{ builtins.WeakRef, prettyPrintWeakRef },
            .{ builtins.WeakSet, prettyPrintWeakSet },
            .{ builtins.WrapForValidIterator, prettyPrintWrapForValidIterator },
        } ++ (if (build_options.enable_intl) .{
            .{ builtins.intl.Collator, prettyPrintIntlCollator },
            .{ builtins.intl.DateTimeFormat, prettyPrintIntlDateTimeFormat },
            .{ builtins.intl.DisplayNames, prettyPrintIntlDisplayNames },
            .{ builtins.intl.DurationFormat, prettyPrintIntlDurationFormat },
            .{ builtins.intl.ListFormat, prettyPrintIntlListFormat },
            .{ builtins.intl.Locale, prettyPrintIntlLocale },
            .{ builtins.intl.PluralRules, prettyPrintIntlPluralRules },
            .{ builtins.intl.Segmenter, prettyPrintIntlSegmenter },
        } else .{}) ++ (if (build_options.enable_temporal) .{
            .{ builtins.temporal.Duration, prettyPrintTemporalDuration },
            .{ builtins.temporal.Instant, prettyPrintTemporalInstant },
            .{ builtins.temporal.PlainDate, prettyPrintTemporalPlainDate },
            .{ builtins.temporal.PlainDateTime, prettyPrintTemporalPlainDateTime },
            .{ builtins.temporal.PlainMonthDay, prettyPrintTemporalPlainMonthDay },
            .{ builtins.temporal.PlainTime, prettyPrintTemporalPlainTime },
            .{ builtins.temporal.PlainYearMonth, prettyPrintTemporalPlainYearMonth },
            .{ builtins.temporal.ZonedDateTime, prettyPrintTemporalZonedDateTime },
        } else .{})) |entry| {
            const T, const prettyPrintFn = entry;
            if (object.cast(T)) |ptr| return prettyPrintFn(ptr, writer);
        }
        // NOTE: This needs to go before pretty-printing functions as it has [[Call]] but no name.
        if (build_options.enable_annex_b and object.isHTMLDDA()) {
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

    const color: std.Io.tty.Color = switch (value.type()) {
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
    try writer.print("{f}", .{value});
    try tty_config.setColor(writer, .reset);
}

pub fn prettyPrintException(exception: Agent.Exception, writer: *std.Io.Writer) PrettyPrintError!void {
    const tty_config = state.platform.tty_config;
    try writer.print("Uncaught exception: {f}", .{exception.value.fmtPretty()});
    var it = std.mem.reverseIterator(exception.stack_trace);
    while (it.next()) |stack_frame| {
        try writer.writeAll("\n  at ");
        switch (stack_frame.origin) {
            .function => |function| {
                try writer.print("{f}", .{Value.from(function).fmtPretty()});
            },
            .eval => {
                // Keep this in sync with prettyPrintFunction()
                try tty_config.setColor(writer, .bold);
                try tty_config.setColor(writer, .blue);
                try writer.writeAll("fn");
                try tty_config.setColor(writer, .reset);
                try writer.writeAll(" eval");
            },
            // These should never be recorded
            .realm, .script, .module => unreachable,
        }
    }
}
