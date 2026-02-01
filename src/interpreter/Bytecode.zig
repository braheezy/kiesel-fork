const std = @import("std");

const Bytecode = @This();

name: []const u8,
code: []const u8,
strings: []const []const u8,
big_ints: []const std.math.big.int.Const,

pub const Builder = @import("Bytecode/Builder.zig");

pub const PrintError =
    std.Io.Writer.Error ||
    std.Io.tty.Config.SetColorError;

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        end,

        jump,
        jump_if_true,
        jump_if_false,
        jump_if_nullish,

        load_undefined,
        load_null,
        load_true,
        load_false,
        load_number_i32,
        load_number_f64,
        load_string,
        load_big_int,
        move,

        add,
        sub,
        mul,
        div,

        eq,
        not_eq,
        eq_strict,
        not_eq_strict,
    };

    pub const Data = union {
        none: void,
        i32: i32,
        reg: Reg,
        reg_reg: struct { Reg, Reg },
        reg_reg_reg: struct { Reg, Reg, Reg },
        reg_i32: struct { Reg, i32 },
        reg_f64: struct { Reg, f64 },
        reg_string: struct { Reg, StringIndex },
        reg_big_int: struct { Reg, BigIntIndex },
    };

    pub const Reg = enum(u8) {
        /// Used for `end` instruction to indicate no register
        none = std.math.maxInt(u8),
        _,
    };

    pub const StringIndex = enum(u32) { _ };
    pub const BigIntIndex = enum(u32) { _ };

    pub const DecodeError = std.Io.Reader.Error || error{InvalidTag};

    pub fn decode(reader: *std.Io.Reader) DecodeError!Inst {
        const tag = try decodeTag(reader);
        const data = try decodeData(reader, tag);
        return .{ .tag = tag, .data = data };
    }

    pub inline fn decodeTag(reader: *std.Io.Reader) DecodeError!Tag {
        return reader.takeEnum(Tag, .little) catch |err| switch (err) {
            error.ReadFailed, error.EndOfStream => |e| return e,
            error.InvalidEnumTag => return error.InvalidTag,
        };
    }

    pub inline fn decodeData(reader: *std.Io.Reader, tag: Inst.Tag) std.Io.Reader.Error!Data {
        return switch (tag) {
            .end,
            .load_undefined,
            .load_null,
            .load_true,
            .load_false,
            => .{ .reg = try takeEnumNonExhaustive(Reg, reader) },
            .jump => .{ .i32 = try reader.takeInt(i32, .little) },
            .jump_if_true, .jump_if_false, .jump_if_nullish, .load_number_i32 => .{ .reg_i32 = .{
                try takeEnumNonExhaustive(Reg, reader),
                try reader.takeInt(i32, .little),
            } },
            .load_number_f64 => .{ .reg_f64 = .{
                try takeEnumNonExhaustive(Reg, reader),
                @bitCast(try reader.takeInt(u64, .little)),
            } },
            .load_string => .{ .reg_string = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
            } },
            .load_big_int => .{ .reg_big_int = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(BigIntIndex, reader),
            } },
            .move => .{ .reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .add,
            .sub,
            .mul,
            .div,
            .eq,
            .not_eq,
            .eq_strict,
            .not_eq_strict,
            => .{ .reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
        };
    }

    fn takeEnumNonExhaustive(comptime T: type, reader: *std.Io.Reader) std.Io.Reader.Error!T {
        return reader.takeEnum(T, .little) catch |err| switch (err) {
            error.InvalidEnumTag => unreachable,
            else => |e| return e,
        };
    }

    pub fn encode(inst: Inst, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeInt(u8, @intFromEnum(inst.tag), .little);
        switch (inst.tag) {
            .end,
            .load_undefined,
            .load_null,
            .load_true,
            .load_false,
            => try writer.writeInt(u8, @intFromEnum(inst.data.reg), .little),
            .jump => try writer.writeInt(i32, inst.data.i32, .little),
            .jump_if_true,
            .jump_if_false,
            .jump_if_nullish,
            .load_number_i32,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_i32[0]), .little);
                try writer.writeInt(i32, inst.data.reg_i32[1], .little);
            },
            .load_number_f64 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_f64[0]), .little);
                try writer.writeInt(u64, @bitCast(inst.data.reg_f64[1]), .little);
            },
            .load_string => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_string[0]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_string[1]), .little);
            },
            .load_big_int => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_big_int[0]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_big_int[1]), .little);
            },
            .move => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg[1]), .little);
            },
            .add,
            .sub,
            .mul,
            .div,
            .eq,
            .not_eq,
            .eq_strict,
            .not_eq_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[2]), .little);
            },
        }
    }

    pub fn encodedSize(inst: Inst) u4 {
        var buffer: [16]u8 = undefined;
        var dw: std.Io.Writer.Discarding = .init(&buffer);
        const writer = &dw.writer;
        inst.encode(writer) catch unreachable;
        return @intCast(dw.fullCount());
    }
};

pub fn deinit(bc: *const Bytecode, gpa: std.mem.Allocator) void {
    gpa.free(bc.name);
    gpa.free(bc.code);
    for (bc.strings) |string| gpa.free(string);
    gpa.free(bc.strings);
    for (bc.big_ints) |big_int| gpa.free(big_int.limbs);
    gpa.free(bc.big_ints);
}

pub fn print(
    bc: *const Bytecode,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    try tty_config.setColor(writer, .bold);
    try writer.print("Bytecode ({s})\n", .{bc.name});
    try tty_config.setColor(writer, .reset);
    var reader: std.Io.Reader = .fixed(bc.code);
    while (true) {
        const offset = reader.seek;
        const inst = Inst.decode(&reader) catch |err| switch (err) {
            error.ReadFailed => unreachable,
            error.EndOfStream => break,
            error.InvalidTag => {
                try tty_config.setColor(writer, .red);
                const tag = reader.peekByte() catch unreachable;
                try writer.print("Invalid instruction tag: {d}\n", .{tag});
                try tty_config.setColor(writer, .reset);
                break;
            },
        };
        try writer.print("{d: >4}: ", .{offset});
        try tty_config.setColor(writer, .cyan);
        try writer.print("{t}", .{inst.tag});
        try tty_config.setColor(writer, .reset);
        switch (inst.tag) {
            .load_undefined,
            .load_null,
            .load_true,
            .load_false,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg, 'r', .blue, writer, tty_config);
            },
            .jump => {
                try writer.writeByte(' ');
                try printData(inst.data.i32, null, .yellow, writer, tty_config);
            },
            .jump_if_true,
            .jump_if_false,
            .jump_if_nullish,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_i32[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_i32[1], null, .yellow, writer, tty_config);
            },
            .load_number_i32 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_i32[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_i32[1], null, .yellow, writer, tty_config);
            },
            .load_number_f64 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_f64[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_f64[1], null, .yellow, writer, tty_config);
            },
            .load_string => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_string[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_string[1]), '@', .green, writer, tty_config);
            },
            .load_big_int => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_big_int[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_big_int[1]), '@', .green, writer, tty_config);
            },
            .move => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg[1], 'r', .blue, writer, tty_config);
            },
            .add,
            .sub,
            .mul,
            .div,
            .eq,
            .not_eq,
            .eq_strict,
            .not_eq_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[2], 'r', .blue, writer, tty_config);
            },
            .end => if (inst.data.reg != .none) {
                try writer.writeByte(' ');
                try printData(inst.data.reg, 'r', .blue, writer, tty_config);
            },
        }
        try writer.writeByte('\n');
    }
}

fn printData(
    value: anytype,
    prefix: ?u8,
    color: std.Io.tty.Color,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    try tty_config.setColor(writer, color);
    if (prefix) |b| try writer.writeByte(b);
    try writer.print("{d}", .{value});
    try tty_config.setColor(writer, .reset);
}
