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
        array_push_hole,
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

    pub const data_tags = std.enums.directEnumArray(Tag, std.meta.FieldEnum(Data), 0, .{
        .end = .reg,
        .jump = .i32,
        .jump_if_true = .reg_i32,
        .jump_if_false = .reg_i32,
        .jump_if_nullish = .reg_i32,
        .load_undefined = .reg,
        .load_null = .reg,
        .load_true = .reg,
        .load_false = .reg,
        .load_number_i32 = .reg_i32,
        .load_number_f64 = .reg_f64,
        .load_string = .reg_string,
        .load_big_int = .reg_big_int,
        .move = .reg_reg,
        .array_create = .reg_u32,
        .array_push = .reg_reg,
        .array_push_hole = .reg,
        .array_set = .reg_reg_u32,
        .array_spread = .reg_reg,
        .object_create = .reg,
        .object_set = .reg_string_reg,
        .object_set_computed = .reg_reg_reg,
        .to_number = .reg_reg,
        .unary_minus = .reg_reg,
        .bitwise_not = .reg_reg,
        .logical_not = .reg_reg,
        .typeof = .reg_reg,
        .add = .reg_reg_reg,
        .sub = .reg_reg_reg,
        .mul = .reg_reg_reg,
        .div = .reg_reg_reg,
        .rem = .reg_reg_reg,
        .exp = .reg_reg_reg,
        .shift_left = .reg_reg_reg,
        .shift_right = .reg_reg_reg,
        .shift_right_unsigned = .reg_reg_reg,
        .bitwise_and = .reg_reg_reg,
        .bitwise_or = .reg_reg_reg,
        .bitwise_xor = .reg_reg_reg,
        .lt = .reg_reg_reg,
        .gt = .reg_reg_reg,
        .lt_eq = .reg_reg_reg,
        .gt_eq = .reg_reg_reg,
        .instanceof = .reg_reg_reg,
        .in = .reg_reg_reg,
        .eq = .reg_reg_reg,
        .not_eq = .reg_reg_reg,
        .eq_strict = .reg_reg_reg,
        .not_eq_strict = .reg_reg_reg,
        .get_binding = .reg_string,
        .get_property = .reg_reg_string,
        .get_property_computed = .reg_reg_reg,
        .get_property_indexed = .reg_reg_u32,
        .set_binding = .string_reg,
        .set_binding_strict = .string_reg,
        .set_property = .reg_reg_string,
        .set_property_strict = .reg_reg_string,
        .set_property_computed = .reg_reg_reg,
        .set_property_computed_strict = .reg_reg_reg,
        .set_property_indexed = .reg_reg_u32,
        .set_property_indexed_strict = .reg_reg_u32,
        .increment_binding_prefix = .reg_string,
        .increment_binding_prefix_strict = .reg_string,
        .increment_binding_postfix = .reg_string,
        .increment_binding_postfix_strict = .reg_string,
        .increment_property_prefix = .reg_reg_string,
        .increment_property_prefix_strict = .reg_reg_string,
        .increment_property_postfix = .reg_reg_string,
        .increment_property_postfix_strict = .reg_reg_string,
        .increment_property_computed_prefix = .reg_reg_reg,
        .increment_property_computed_prefix_strict = .reg_reg_reg,
        .increment_property_computed_postfix = .reg_reg_reg,
        .increment_property_computed_postfix_strict = .reg_reg_reg,
        .increment_property_indexed_prefix = .reg_reg_u32,
        .increment_property_indexed_prefix_strict = .reg_reg_u32,
        .increment_property_indexed_postfix = .reg_reg_u32,
        .increment_property_indexed_postfix_strict = .reg_reg_u32,
        .decrement_binding_prefix = .reg_string,
        .decrement_binding_prefix_strict = .reg_string,
        .decrement_binding_postfix = .reg_string,
        .decrement_binding_postfix_strict = .reg_string,
        .decrement_property_prefix = .reg_reg_string,
        .decrement_property_prefix_strict = .reg_reg_string,
        .decrement_property_postfix = .reg_reg_string,
        .decrement_property_postfix_strict = .reg_reg_string,
        .decrement_property_computed_prefix = .reg_reg_reg,
        .decrement_property_computed_prefix_strict = .reg_reg_reg,
        .decrement_property_computed_postfix = .reg_reg_reg,
        .decrement_property_computed_postfix_strict = .reg_reg_reg,
        .decrement_property_indexed_prefix = .reg_reg_u32,
        .decrement_property_indexed_prefix_strict = .reg_reg_u32,
        .decrement_property_indexed_postfix = .reg_reg_u32,
        .decrement_property_indexed_postfix_strict = .reg_reg_u32,
        .delete_binding = .reg_string,
        .delete_property = .reg_reg_string,
        .delete_property_strict = .reg_reg_string,
        .delete_property_computed = .reg_reg_reg,
        .delete_property_computed_strict = .reg_reg_reg,
        .delete_property_indexed = .reg_reg_u32,
        .delete_property_indexed_strict = .reg_reg_u32,
        .call = .reg_reg_reg,
        .call0 = .reg_reg,
        .call1 = .reg_reg_reg,
        .call2 = .reg_reg_reg_reg,
        .call_property = .reg_reg_reg_reg,
        .call_property0 = .reg_reg_reg,
        .call_property1 = .reg_reg_reg_reg,
        .call_property2 = .reg_reg_reg_reg_reg,
    });

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

    pub inline fn decodeData(reader: *std.Io.Reader, tag: Tag) std.Io.Reader.Error!Data {
        const data_tag = data_tags[@intFromEnum(tag)];
        switch (data_tag) {
            inline else => |field| {
                const FieldType = @TypeOf(@field(@as(Data, undefined), @tagName(field)));
                const type_info = @typeInfo(FieldType);

                if (FieldType == void) {
                    return @unionInit(Data, @tagName(field), {});
                }

                if (type_info == .@"struct") {
                    var result: FieldType = undefined;
                    inline for (type_info.@"struct".fields, 0..) |struct_field, i| {
                        result[i] = try decodeField(struct_field.type, reader);
                    }
                    return @unionInit(Data, @tagName(field), result);
                }

                return @unionInit(Data, @tagName(field), try decodeField(FieldType, reader));
            },
        }
    }

    fn decodeField(comptime T: type, reader: *std.Io.Reader) std.Io.Reader.Error!T {
        return switch (T) {
            Reg,
            StringIndex,
            BigIntIndex,
            => try reader.takeEnumNonexhaustive(T, .little),
            i32 => try reader.takeInt(i32, .little),
            u32 => try reader.takeInt(u32, .little),
            f64 => @bitCast(try reader.takeInt(u64, .little)),
            void => {},
            else => comptime unreachable,
        };
    }

    pub fn encode(inst: Inst, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeInt(u8, @intFromEnum(inst.tag), .little);
        const data_tag = data_tags[@intFromEnum(inst.tag)];
        switch (data_tag) {
            inline else => |field| {
                const field_value = @field(inst.data, @tagName(field));
                const FieldType = @TypeOf(field_value);
                const type_info = @typeInfo(FieldType);

                if (FieldType == void) return;

                if (type_info == .@"struct") {
                    inline for (type_info.@"struct".fields, 0..) |struct_field, i| {
                        try encodeField(struct_field.type, field_value[i], writer);
                    }
                } else {
                    try encodeField(FieldType, field_value, writer);
                }
            },
        }
    }

    fn encodeField(comptime T: type, value: T, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (T) {
            Reg => try writer.writeInt(u8, @intFromEnum(value), .little),
            StringIndex,
            BigIntIndex,
            => try writer.writeInt(u32, @intFromEnum(value), .little),
            i32 => try writer.writeInt(i32, value, .little),
            u32 => try writer.writeInt(u32, value, .little),
            f64 => try writer.writeInt(u64, @bitCast(value), .little),
            void => {},
            else => comptime unreachable,
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

        try printData(inst.tag, inst.data, writer, tty_config);

        try writer.writeByte('\n');
    }
}

fn printData(
    tag: Inst.Tag,
    data: Inst.Data,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const data_tag = Inst.data_tags[@intFromEnum(tag)];
    if (data_tag == .none) return;
    if (tag == .end and data.reg == .none) return;

    try writer.writeByte(' ');
    switch (data_tag) {
        .none => {},
        inline .i32,
        .reg,
        => |dt| {
            const field_data = @field(data, @tagName(dt));
            try printField(field_data, writer, tty_config);
        },
        inline else => |dt| {
            const field_data = @field(data, @tagName(dt));
            const field_type = @typeInfo(@TypeOf(field_data)).@"struct";
            inline for (field_type.fields, 0..) |struct_field, idx| {
                if (idx > 0) try writer.writeAll(", ");
                const value = @field(field_data, struct_field.name);
                try printField(value, writer, tty_config);
            }
        },
    }
}

fn printField(
    value: anytype,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const T = @TypeOf(value);
    switch (T) {
        i32,
        u32,
        f64,
        => {
            try tty_config.setColor(writer, .yellow);
            try writer.print("{}", .{value});
            try tty_config.setColor(writer, .reset);
        },
        Inst.Reg => {
            try tty_config.setColor(writer, .blue);
            try writer.print("r{d}", .{@intFromEnum(value)});
            try tty_config.setColor(writer, .reset);
        },
        Inst.StringIndex,
        Inst.BigIntIndex,
        => {
            try tty_config.setColor(writer, .green);
            try writer.print("@{d}", .{@intFromEnum(value)});
            try tty_config.setColor(writer, .reset);
        },
        else => comptime unreachable,
    }
}
