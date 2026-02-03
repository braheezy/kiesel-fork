const std = @import("std");

const interpreter = @import("../interpreter.zig");

const Vm = interpreter.Vm;

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

        array_create,
        array_push,
        array_set,
        array_spread,

        object_create,
        object_set,
        object_set_computed,

        to_number,
        unary_minus,
        bitwise_not,
        logical_not,
        typeof,

        add,
        sub,
        mul,
        div,
        rem,
        exp,
        shift_left,
        shift_right,
        shift_right_unsigned,
        bitwise_and,
        bitwise_or,
        bitwise_xor,

        lt,
        gt,
        lt_eq,
        gt_eq,
        instanceof,
        in,

        eq,
        not_eq,
        eq_strict,
        not_eq_strict,

        get_binding,
        get_property,
        get_property_computed,
        get_property_indexed,
        set_binding,
        set_binding_strict,
        set_property,
        set_property_strict,
        set_property_computed,
        set_property_computed_strict,
        set_property_indexed,
        set_property_indexed_strict,
        increment_binding_prefix,
        increment_binding_prefix_strict,
        increment_binding_postfix,
        increment_binding_postfix_strict,
        increment_property_prefix,
        increment_property_prefix_strict,
        increment_property_postfix,
        increment_property_postfix_strict,
        increment_property_computed_prefix,
        increment_property_computed_prefix_strict,
        increment_property_computed_postfix,
        increment_property_computed_postfix_strict,
        increment_property_indexed_prefix,
        increment_property_indexed_prefix_strict,
        increment_property_indexed_postfix,
        increment_property_indexed_postfix_strict,
        decrement_binding_prefix,
        decrement_binding_prefix_strict,
        decrement_binding_postfix,
        decrement_binding_postfix_strict,
        decrement_property_prefix,
        decrement_property_prefix_strict,
        decrement_property_postfix,
        decrement_property_postfix_strict,
        decrement_property_computed_prefix,
        decrement_property_computed_prefix_strict,
        decrement_property_computed_postfix,
        decrement_property_computed_postfix_strict,
        decrement_property_indexed_prefix,
        decrement_property_indexed_prefix_strict,
        decrement_property_indexed_postfix,
        decrement_property_indexed_postfix_strict,
        delete_binding,
        delete_property,
        delete_property_strict,
        delete_property_computed,
        delete_property_computed_strict,
        delete_property_indexed,
        delete_property_indexed_strict,
        call,
        call0,
        call1,
        call2,
        call_property,
        call_property0,
        call_property1,
        call_property2,
    };

    pub const Data = union {
        none: void,
        i32: i32,
        reg: Reg,
        reg_reg: struct { Reg, Reg },
        reg_reg_reg: struct { Reg, Reg, Reg },
        reg_reg_reg_reg: struct { Reg, Reg, Reg, Reg },
        reg_reg_reg_reg_reg: struct { Reg, Reg, Reg, Reg, Reg },
        reg_reg_u32: struct { Reg, Reg, u32 },
        reg_i32: struct { Reg, i32 },
        reg_u32: struct { Reg, u32 },
        reg_f64: struct { Reg, f64 },
        reg_string: struct { Reg, StringIndex },
        reg_reg_string: struct { Reg, Reg, StringIndex },
        reg_big_int: struct { Reg, BigIntIndex },
        reg_string_reg: struct { Reg, StringIndex, Reg },
        string_reg: struct { StringIndex, Reg },
    };

    pub const Reg = enum(u8) {
        /// Used for `end` instruction to indicate no register
        none = std.math.maxInt(u8),
        /// Scratch register for temporary values, e.g. the call arguments array
        scratch = Vm.num_regs - 1,
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
            .object_create,
            => .{ .reg = try takeEnumNonExhaustive(Reg, reader) },
            .jump => .{ .i32 = try reader.takeInt(i32, .little) },
            .jump_if_true, .jump_if_false, .jump_if_nullish, .load_number_i32 => .{ .reg_i32 = .{
                try takeEnumNonExhaustive(Reg, reader),
                try reader.takeInt(i32, .little),
            } },
            .array_create => .{ .reg_u32 = .{
                try takeEnumNonExhaustive(Reg, reader),
                try reader.takeInt(u32, .little),
            } },
            .load_number_f64 => .{ .reg_f64 = .{
                try takeEnumNonExhaustive(Reg, reader),
                @bitCast(try reader.takeInt(u64, .little)),
            } },
            .load_string,
            .get_binding,
            .delete_binding,
            .increment_binding_prefix,
            .increment_binding_prefix_strict,
            .increment_binding_postfix,
            .increment_binding_postfix_strict,
            .decrement_binding_prefix,
            .decrement_binding_prefix_strict,
            .decrement_binding_postfix,
            .decrement_binding_postfix_strict,
            => .{ .reg_string = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
            } },
            .get_property => .{ .reg_reg_string = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
            } },
            .set_property,
            .set_property_strict,
            => .{ .reg_reg_string = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
            } },
            .delete_property,
            .delete_property_strict,
            => .{ .reg_reg_string = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
            } },
            .increment_property_prefix,
            .increment_property_prefix_strict,
            .increment_property_postfix,
            .increment_property_postfix_strict,
            .decrement_property_prefix,
            .decrement_property_prefix_strict,
            .decrement_property_postfix,
            .decrement_property_postfix_strict,
            => .{ .reg_reg_string = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
            } },
            .load_big_int => .{ .reg_big_int = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(BigIntIndex, reader),
            } },
            .set_binding,
            .set_binding_strict,
            => .{ .string_reg = .{
                try takeEnumNonExhaustive(StringIndex, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .move,
            .array_push,
            .array_spread,
            .to_number,
            .unary_minus,
            .bitwise_not,
            .logical_not,
            .typeof,
            => .{ .reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .array_set,
            .get_property_indexed,
            .set_property_indexed,
            .set_property_indexed_strict,
            => .{ .reg_reg_u32 = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try reader.takeInt(u32, .little),
            } },
            .delete_property_indexed,
            .delete_property_indexed_strict,
            => .{ .reg_reg_u32 = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try reader.takeInt(u32, .little),
            } },
            .increment_property_indexed_prefix,
            .increment_property_indexed_prefix_strict,
            .increment_property_indexed_postfix,
            .increment_property_indexed_postfix_strict,
            .decrement_property_indexed_prefix,
            .decrement_property_indexed_prefix_strict,
            .decrement_property_indexed_postfix,
            .decrement_property_indexed_postfix_strict,
            => .{ .reg_reg_u32 = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try reader.takeInt(u32, .little),
            } },
            .call => .{ .reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call0 => .{ .reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call1 => .{ .reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call2 => .{ .reg_reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call_property => .{ .reg_reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call_property0 => .{ .reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call_property1 => .{ .reg_reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .call_property2 => .{ .reg_reg_reg_reg_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .object_set => .{ .reg_string_reg = .{
                try takeEnumNonExhaustive(Reg, reader),
                try takeEnumNonExhaustive(StringIndex, reader),
                try takeEnumNonExhaustive(Reg, reader),
            } },
            .object_set_computed,
            .get_property_computed,
            .set_property_computed,
            .set_property_computed_strict,
            .delete_property_computed,
            .delete_property_computed_strict,
            .increment_property_computed_prefix,
            .increment_property_computed_prefix_strict,
            .increment_property_computed_postfix,
            .increment_property_computed_postfix_strict,
            .decrement_property_computed_prefix,
            .decrement_property_computed_prefix_strict,
            .decrement_property_computed_postfix,
            .decrement_property_computed_postfix_strict,
            .add,
            .sub,
            .mul,
            .div,
            .rem,
            .exp,
            .shift_left,
            .shift_right,
            .shift_right_unsigned,
            .bitwise_and,
            .bitwise_or,
            .bitwise_xor,
            .lt,
            .gt,
            .lt_eq,
            .gt_eq,
            .instanceof,
            .in,
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
            .object_create,
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
            .array_create => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_u32[0]), .little);
                try writer.writeInt(u32, inst.data.reg_u32[1], .little);
            },
            .load_number_f64 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_f64[0]), .little);
                try writer.writeInt(u64, @bitCast(inst.data.reg_f64[1]), .little);
            },
            .load_string,
            .get_binding,
            .delete_binding,
            .increment_binding_prefix,
            .increment_binding_prefix_strict,
            .increment_binding_postfix,
            .increment_binding_postfix_strict,
            .decrement_binding_prefix,
            .decrement_binding_prefix_strict,
            .decrement_binding_postfix,
            .decrement_binding_postfix_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_string[0]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_string[1]), .little);
            },
            .get_property => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[1]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_reg_string[2]), .little);
            },
            .set_property,
            .set_property_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[1]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_reg_string[2]), .little);
            },
            .delete_property,
            .delete_property_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[1]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_reg_string[2]), .little);
            },
            .increment_property_prefix,
            .increment_property_prefix_strict,
            .increment_property_postfix,
            .increment_property_postfix_strict,
            .decrement_property_prefix,
            .decrement_property_prefix_strict,
            .decrement_property_postfix,
            .decrement_property_postfix_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_string[1]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_reg_string[2]), .little);
            },
            .load_big_int => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_big_int[0]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_big_int[1]), .little);
            },
            .move,
            .array_push,
            .array_spread,
            .to_number,
            .unary_minus,
            .bitwise_not,
            .logical_not,
            .typeof,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg[1]), .little);
            },
            .array_set,
            .get_property_indexed,
            .set_property_indexed,
            .set_property_indexed_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_u32[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_u32[1]), .little);
                try writer.writeInt(u32, inst.data.reg_reg_u32[2], .little);
            },
            .delete_property_indexed,
            .delete_property_indexed_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_u32[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_u32[1]), .little);
                try writer.writeInt(u32, inst.data.reg_reg_u32[2], .little);
            },
            .increment_property_indexed_prefix,
            .increment_property_indexed_prefix_strict,
            .increment_property_indexed_postfix,
            .increment_property_indexed_postfix_strict,
            .decrement_property_indexed_prefix,
            .decrement_property_indexed_prefix_strict,
            .decrement_property_indexed_postfix,
            .decrement_property_indexed_postfix_strict,
            => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_u32[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_u32[1]), .little);
                try writer.writeInt(u32, inst.data.reg_reg_u32[2], .little);
            },
            .call => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[2]), .little);
            },
            .call0 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg[1]), .little);
            },
            .call1 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[2]), .little);
            },
            .call2 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[2]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[3]), .little);
            },
            .call_property => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[2]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[3]), .little);
            },
            .call_property0 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg[2]), .little);
            },
            .call_property1 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[2]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg[3]), .little);
            },
            .call_property2 => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg_reg[2]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg_reg[3]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_reg_reg_reg_reg[4]), .little);
            },
            .object_set => {
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_string_reg[0]), .little);
                try writer.writeInt(u32, @intFromEnum(inst.data.reg_string_reg[1]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.reg_string_reg[2]), .little);
            },
            .set_binding,
            .set_binding_strict,
            => {
                try writer.writeInt(u32, @intFromEnum(inst.data.string_reg[0]), .little);
                try writer.writeInt(u8, @intFromEnum(inst.data.string_reg[1]), .little);
            },
            .object_set_computed,
            .get_property_computed,
            .set_property_computed,
            .set_property_computed_strict,
            .delete_property_computed,
            .delete_property_computed_strict,
            .increment_property_computed_prefix,
            .increment_property_computed_prefix_strict,
            .increment_property_computed_postfix,
            .increment_property_computed_postfix_strict,
            .decrement_property_computed_prefix,
            .decrement_property_computed_prefix_strict,
            .decrement_property_computed_postfix,
            .decrement_property_computed_postfix_strict,
            .add,
            .sub,
            .mul,
            .div,
            .rem,
            .exp,
            .shift_left,
            .shift_right,
            .shift_right_unsigned,
            .bitwise_and,
            .bitwise_or,
            .bitwise_xor,
            .lt,
            .gt,
            .lt_eq,
            .gt_eq,
            .instanceof,
            .in,
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
            .object_create,
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
            .array_create => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_u32[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_u32[1], null, .yellow, writer, tty_config);
            },
            .load_number_f64 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_f64[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_f64[1], null, .yellow, writer, tty_config);
            },
            .load_string,
            .get_binding,
            .delete_binding,
            .increment_binding_prefix,
            .increment_binding_prefix_strict,
            .increment_binding_postfix,
            .increment_binding_postfix_strict,
            .decrement_binding_prefix,
            .decrement_binding_prefix_strict,
            .decrement_binding_postfix,
            .decrement_binding_postfix_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_string[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_string[1]), '@', .green, writer, tty_config);
            },
            .get_property => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_string[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_string[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_reg_string[2]), '@', .green, writer, tty_config);
            },
            .set_property,
            .set_property_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_string[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_string[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_reg_string[2]), '@', .green, writer, tty_config);
            },
            .delete_property,
            .delete_property_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_string[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_string[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_reg_string[2]), '@', .green, writer, tty_config);
            },
            .increment_property_prefix,
            .increment_property_prefix_strict,
            .increment_property_postfix,
            .increment_property_postfix_strict,
            .decrement_property_prefix,
            .decrement_property_prefix_strict,
            .decrement_property_postfix,
            .decrement_property_postfix_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_string[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_string[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_reg_string[2]), '@', .green, writer, tty_config);
            },
            .load_big_int => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_big_int[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_big_int[1]), '@', .green, writer, tty_config);
            },
            .move,
            .array_push,
            .array_spread,
            .to_number,
            .unary_minus,
            .bitwise_not,
            .logical_not,
            .typeof,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg[1], 'r', .blue, writer, tty_config);
            },
            .array_set,
            .get_property_indexed,
            .set_property_indexed,
            .set_property_indexed_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_u32[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_u32[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_u32[2], null, .yellow, writer, tty_config);
            },
            .increment_property_indexed_prefix,
            .increment_property_indexed_prefix_strict,
            .increment_property_indexed_postfix,
            .increment_property_indexed_postfix_strict,
            .decrement_property_indexed_prefix,
            .decrement_property_indexed_prefix_strict,
            .decrement_property_indexed_postfix,
            .decrement_property_indexed_postfix_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_u32[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_u32[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_u32[2], null, .yellow, writer, tty_config);
            },
            .delete_property_indexed,
            .delete_property_indexed_strict,
            => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_u32[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_u32[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_u32[2], null, .yellow, writer, tty_config);
            },
            .call => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[2], 'r', .blue, writer, tty_config);
            },
            .call0 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg[1], 'r', .blue, writer, tty_config);
            },
            .call1 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[2], 'r', .blue, writer, tty_config);
            },
            .call2 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[2], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[3], 'r', .blue, writer, tty_config);
            },
            .call_property => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[2], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[3], 'r', .blue, writer, tty_config);
            },
            .call_property0 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg[2], 'r', .blue, writer, tty_config);
            },
            .call_property1 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[2], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg[3], 'r', .blue, writer, tty_config);
            },
            .call_property2 => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_reg_reg_reg_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg_reg[1], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg_reg[2], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg_reg[3], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_reg_reg_reg_reg[4], 'r', .blue, writer, tty_config);
            },
            .object_set => {
                try writer.writeByte(' ');
                try printData(inst.data.reg_string_reg[0], 'r', .blue, writer, tty_config);
                try writer.writeAll(", ");
                try printData(@intFromEnum(inst.data.reg_string_reg[1]), '@', .green, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.reg_string_reg[2], 'r', .blue, writer, tty_config);
            },
            .set_binding,
            .set_binding_strict,
            => {
                try writer.writeByte(' ');
                try printData(@intFromEnum(inst.data.string_reg[0]), '@', .green, writer, tty_config);
                try writer.writeAll(", ");
                try printData(inst.data.string_reg[1], 'r', .blue, writer, tty_config);
            },
            .object_set_computed,
            .get_property_computed,
            .set_property_computed,
            .set_property_computed_strict,
            .delete_property_computed,
            .delete_property_computed_strict,
            .increment_property_computed_prefix,
            .increment_property_computed_prefix_strict,
            .increment_property_computed_postfix,
            .increment_property_computed_postfix_strict,
            .decrement_property_computed_prefix,
            .decrement_property_computed_prefix_strict,
            .decrement_property_computed_postfix,
            .decrement_property_computed_postfix_strict,
            .add,
            .sub,
            .mul,
            .div,
            .rem,
            .exp,
            .shift_left,
            .shift_right,
            .shift_right_unsigned,
            .bitwise_and,
            .bitwise_or,
            .bitwise_xor,
            .lt,
            .gt,
            .lt_eq,
            .gt_eq,
            .instanceof,
            .in,
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
