const std = @import("std");

const Color = std.io.tty.Color;

const types = @import("types.zig");

const Value = types.Value;

pub fn prettyPrintValue(value: Value, writer: anytype) !void {
    const file = if (@TypeOf(writer.context) == std.fs.File)
        writer.context
    else
        std.io.getStdOut();
    const tty_config = std.io.tty.detectConfig(file);
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
