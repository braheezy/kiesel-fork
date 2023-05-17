const types = @import("types.zig");

const Value = types.Value;

/// https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
const colors = struct {
    pub const gray = "\x1b[90m";
    pub const clear = "\x1b[0m";
    pub const red = "\x1b[31m";
    pub const green = "\x1b[32m";
    pub const yellow = "\x1b[33m";
    pub const blue = "\x1b[34m";
    pub const magenta = "\x1b[35m";
    pub const cyan = "\x1b[36m";
    pub const white = "\x1b[37m";
};

pub fn prettyPrintValue(value: Value, writer: anytype) !void {
    const color = switch (value) {
        .undefined => colors.gray,
        .null => colors.yellow,
        .boolean => colors.blue,
        .string => colors.green,
        .symbol => colors.cyan,
        .number => colors.magenta,
        .big_int => colors.magenta,
        .object => colors.white,
    };
    try writer.print("{s}{}{s}", .{ color, value, colors.clear });
}
