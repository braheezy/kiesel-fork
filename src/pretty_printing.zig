const std = @import("std");

const Color = std.io.tty.Color;

const builtins = @import("builtins.zig");
const types = @import("types.zig");

const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const getArrayLength = @import("builtins/array.zig").getArrayLength;

fn getTtyConfigForWriter(writer: anytype) std.io.tty.Config {
    const file = if (@TypeOf(writer.context) == std.fs.File)
        writer.context
    else
        std.io.getStdOut();
    return std.io.tty.detectConfig(file);
}

pub fn prettyPrintArray(array: Object, writer: anytype) !void {
    const property_storage = array.data.property_storage;
    const length = getArrayLength(array);
    const tty_config = getTtyConfigForWriter(writer);

    try tty_config.setColor(writer, .white);
    try writer.writeAll("[");
    if (length != 0) try writer.writeAll(" ");
    for (0..length) |i| {
        const property_key = PropertyKey.from(@intCast(PropertyKey.IntegerIndex, i));
        if (property_storage.get(property_key)) |property_descriptor| {
            try writer.print("{pretty}", .{property_descriptor.value.?});
        } else {
            try tty_config.setColor(writer, .dim);
            try writer.writeAll("<empty>");
            try tty_config.setColor(writer, .reset);
        }
        if (i + 1 < length) try writer.writeAll(", ");
    }
    try tty_config.setColor(writer, .white);
    if (length != 0) try writer.writeAll(" ");
    try writer.writeAll("]");
    try tty_config.setColor(writer, .reset);
}

pub fn prettyPrintValue(value: Value, writer: anytype) !void {
    if (value == .object and value.object.is(builtins.Array)) {
        return prettyPrintArray(value.object, writer);
    }
    const tty_config = getTtyConfigForWriter(writer);
    const color = switch (value) {
        .undefined => Color.bright_black,
        .null => Color.yellow,
        .boolean => Color.blue,
        .string => Color.green,
        .symbol => Color.cyan,
        .number => Color.magenta,
        .big_int => Color.magenta,
        .object => Color.white,
    };
    try tty_config.setColor(writer, color);
    try writer.print("{}", .{value});
    try tty_config.setColor(writer, .reset);
}
