const std = @import("std");

const ast = @import("../language/ast.zig");
const interpreter = @import("../interpreter.zig");

const Vm = interpreter.Vm;

const Bytecode = @This();

name: []const u8,
code: []const u8,
strings: []const []const u8,
big_ints: []const std.math.big.int.Const,
functions: []const Function,
classes: []const Class,

pub const Builder = @import("Bytecode/Builder.zig");

pub const PrintError =
    std.Io.Writer.Error ||
    std.Io.tty.Config.SetColorError;

pub const Function = struct {
    source_text: Inst.StringIndex,
    name: Name,
    parameters: ast.FormalParameters,
    body: ast.FunctionBody,
    kind: Kind,

    pub const Name = union(enum) {
        none,
        identifier: Inst.StringIndex,
        default: Inst.StringIndex,
    };

    pub const Kind = enum {
        normal,
        arrow,
        generator,
        async,
        async_arrow,
        async_generator,
    };
};

pub const Class = struct {
    source_text: Inst.StringIndex,
    name: Name,
    class_tail: ast.ClassTail,
    heritage: Inst.Reg,
    element_names: []const Inst.Reg,

    pub const Name = union(enum) {
        none,
        identifier: Inst.StringIndex,
        default: Inst.StringIndex,
    };
};

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
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
        object_set_getter,
        object_set_getter_computed,
        object_set_setter,
        object_set_setter_computed,
        object_spread,

        reg_exp_create,

        resolve_this_binding,

        to_number,
        to_numeric,
        to_string,
        to_object,
        negate,
        bitwise_not,
        logical_not,
        typeof,
        typeof_binding,

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

        push_scope,
        push_var_scope,
        push_with_scope,
        pop_scope,
        create_mutable_binding,
        create_immutable_binding,
        initialize_binding,

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
        copy_data_properties,

        call,
        call0,
        call1,
        call2,
        call_property,
        call_property0,
        call_property1,
        call_property2,
        call_direct_eval,
        call_direct_eval_strict,
        construct,
        construct0,
        construct1,
        construct2,

        get_template_object,

        get_iterator,
        get_async_iterator,
        get_for_in_iterator,
        iterator_step,
        iterator_step_value,
        iterator_step_value_async,
        iterator_close,
        iterator_is_done,
        iterator_collect,

        throw,
        throw_reference_error,
        @"return",
        await,
        yield,

        create_function,
        create_class,
        set_home_object,
        create_unmapped_arguments_object,
        create_mapped_arguments_object,
        get_argument,
        get_rest_arguments,
        get_new_target,

        super_call,
        get_super_property,
        get_super_property_computed,
        set_super_property,
        set_super_property_strict,
        set_super_property_computed,
        set_super_property_computed_strict,

        push_private_scope,
        pop_private_scope,
        create_private_element,
        get_private_element,
        set_private_element,
        has_private_element,

        import_call,
        get_import_meta,
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
        reg_reg_reg_u16: struct { Reg, Reg, Reg, u16 },
        reg_u16: struct { Reg, u16 },
        reg_i32: struct { Reg, i32 },
        reg_u32: struct { Reg, u32 },
        reg_f64: struct { Reg, f64 },
        reg_string: struct { Reg, StringIndex },
        reg_reg_string: struct { Reg, Reg, StringIndex },
        reg_big_int: struct { Reg, BigIntIndex },
        reg_string_reg: struct { Reg, StringIndex, Reg },
        reg_string_string: struct { Reg, StringIndex, StringIndex },
        reg_function: struct { Reg, FunctionIndex },
        reg_class: struct { Reg, ClassIndex },
        string: StringIndex,
        string_reg: struct { StringIndex, Reg },
    };

    pub const data_tags = blk: {
        @setEvalBranchQuota(2_000);
        break :blk std.enums.directEnumArray(Tag, std.meta.FieldEnum(Data), 0, .{
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
            .object_set_getter = .reg_string_reg,
            .object_set_getter_computed = .reg_reg_reg,
            .object_set_setter = .reg_string_reg,
            .object_set_setter_computed = .reg_reg_reg,
            .object_spread = .reg_reg,
            .reg_exp_create = .reg_string_string,
            .resolve_this_binding = .reg,
            .to_number = .reg_reg,
            .to_numeric = .reg_reg,
            .to_string = .reg_reg,
            .to_object = .reg_reg,
            .negate = .reg_reg,
            .bitwise_not = .reg_reg,
            .logical_not = .reg_reg,
            .typeof = .reg_reg,
            .typeof_binding = .reg_string,
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
            .push_scope = .none,
            .push_var_scope = .none,
            .push_with_scope = .reg,
            .pop_scope = .none,
            .create_mutable_binding = .string,
            .create_immutable_binding = .string,
            .initialize_binding = .string_reg,
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
            .copy_data_properties = .reg_reg_reg,
            .call = .reg_reg_reg,
            .call0 = .reg_reg,
            .call1 = .reg_reg_reg,
            .call2 = .reg_reg_reg_reg,
            .call_property = .reg_reg_reg_reg,
            .call_property0 = .reg_reg_reg,
            .call_property1 = .reg_reg_reg_reg,
            .call_property2 = .reg_reg_reg_reg_reg,
            .call_direct_eval = .reg_reg_reg,
            .call_direct_eval_strict = .reg_reg_reg,
            .construct = .reg_reg_reg,
            .construct0 = .reg_reg,
            .construct1 = .reg_reg_reg,
            .construct2 = .reg_reg_reg_reg,
            .get_template_object = .reg_reg_reg_u16,
            .get_iterator = .reg_reg,
            .get_async_iterator = .reg_reg,
            .get_for_in_iterator = .reg_reg,
            .iterator_step = .reg_reg,
            .iterator_step_value = .reg_reg,
            .iterator_step_value_async = .reg_reg,
            .iterator_close = .reg,
            .iterator_is_done = .reg_reg,
            .iterator_collect = .reg_reg,
            .throw = .reg,
            .throw_reference_error = .none,
            .@"return" = .reg,
            .await = .reg,
            .yield = .reg,
            .create_function = .reg_function,
            .create_class = .reg_class,
            .set_home_object = .reg_reg,
            .create_unmapped_arguments_object = .reg,
            .create_mapped_arguments_object = .reg,
            .get_argument = .reg_u16,
            .get_rest_arguments = .reg_u16,
            .get_new_target = .reg,
            .super_call = .reg_reg,
            .get_super_property = .reg_string,
            .get_super_property_computed = .reg_reg,
            .set_super_property = .reg_string,
            .set_super_property_strict = .reg_string,
            .set_super_property_computed = .reg_reg,
            .set_super_property_computed_strict = .reg_reg,
            .push_private_scope = .none,
            .pop_private_scope = .none,
            .create_private_element = .reg_string,
            .get_private_element = .reg_reg_string,
            .set_private_element = .reg_string_reg,
            .has_private_element = .reg_reg_reg,
            .import_call = .reg_reg_reg,
            .get_import_meta = .reg,
        });
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
    pub const FunctionIndex = enum(u32) { _ };
    pub const ClassIndex = enum(u32) { _ };

    pub const Format = struct {
        inst: Inst,
        bc: *const Bytecode,
        tty_config: std.Io.tty.Config,

        pub fn format(f: Format, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            f.tty_config.setColor(writer, .cyan) catch {};
            try writer.print("{t}", .{f.inst.tag});
            f.tty_config.setColor(writer, .reset) catch {};
            printData(f.bc, f.inst.tag, f.inst.data, writer, f.tty_config) catch |err| switch (err) {
                error.Unexpected => {},
                error.WriteFailed => return error.WriteFailed,
            };
        }
    };

    pub fn fmt(inst: Inst, bc: *const Bytecode, tty_config: std.Io.tty.Config) Format {
        return .{ .inst = inst, .bc = bc, .tty_config = tty_config };
    }

    /// Decode an instruction from the given bytecode slice.
    ///
    /// Returns a tuple containing the decoded instruction and the number of
    /// consumed bytes, or `null` if decoding fails.
    pub fn decode(code: []const u8) ?struct { Inst, u4 } {
        if (code.len == 0) return null;
        const tag = std.enums.fromInt(Tag, code[0]) orelse return null;
        const size = encodedSize(tag);
        if (code.len < size) return null;
        const data = decodeData(code[1..], tag);
        const inst: Inst = .{ .tag = tag, .data = data };
        return .{ inst, size };
    }

    /// Decode an instruction tag from the given bytecode slice.
    ///
    /// Assumes the slice is non-empty and the first byte is a valid tag.
    pub inline fn decodeTag(code: []const u8) Tag {
        return @enumFromInt(code[0]);
    }

    /// Decode instruction data from the given bytecode slice.
    ///
    /// Assumes the slice has enough bytes.
    pub inline fn decodeData(code: []const u8, tag: Tag) Data {
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
                    var offset: usize = 0;
                    inline for (type_info.@"struct".fields, 0..) |struct_field, i| {
                        result[i] = decodeField(struct_field.type, code[offset..]);
                        offset += @sizeOf(struct_field.type);
                    }
                    return @unionInit(Data, @tagName(field), result);
                }

                return @unionInit(Data, @tagName(field), decodeField(FieldType, code));
            },
        }
    }

    inline fn decodeField(comptime T: type, code: []const u8) T {
        return switch (T) {
            Reg => @enumFromInt(code[0]),
            StringIndex,
            BigIntIndex,
            FunctionIndex,
            ClassIndex,
            => @enumFromInt(std.mem.readInt(u32, code[0..4], .little)),
            u16 => std.mem.readInt(u16, code[0..2], .little),
            i32 => std.mem.readInt(i32, code[0..4], .little),
            u32 => std.mem.readInt(u32, code[0..4], .little),
            f64 => @bitCast(std.mem.readInt(u64, code[0..8], .little)),
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
            FunctionIndex,
            ClassIndex,
            => try writer.writeInt(u32, @intFromEnum(value), .little),
            u16 => try writer.writeInt(u16, value, .little),
            i32 => try writer.writeInt(i32, value, .little),
            u32 => try writer.writeInt(u32, value, .little),
            f64 => try writer.writeInt(u64, @bitCast(value), .little),
            void => {},
            else => comptime unreachable,
        }
    }

    pub fn encodedSize(tag: Tag) u4 {
        return encoded_sizes[@intFromEnum(tag)];
    }

    const encoded_sizes = blk: {
        var sizes: [@typeInfo(Tag).@"enum".fields.len]u4 = @splat(1);
        for (0..sizes.len) |i| {
            const FieldType = @TypeOf(@field(@as(Data, undefined), @tagName(data_tags[i])));
            const type_info = @typeInfo(FieldType);
            if (type_info == .@"struct") {
                for (type_info.@"struct".fields) |f| {
                    sizes[i] += @sizeOf(f.type);
                }
            } else {
                sizes[i] += @sizeOf(FieldType);
            }
        }
        break :blk sizes;
    };
};

pub fn deinit(bc: *const Bytecode, gpa: std.mem.Allocator) void {
    gpa.free(bc.name);
    gpa.free(bc.code);
    for (bc.strings) |string| gpa.free(string);
    gpa.free(bc.strings);
    for (bc.big_ints) |big_int| gpa.free(big_int.limbs);
    gpa.free(bc.big_ints);
    gpa.free(bc.functions);
    for (bc.classes) |class| gpa.free(class.element_names);
    gpa.free(bc.classes);
}

pub const Iterator = struct {
    code: []const u8,
    offset: u32,

    pub const Entry = struct {
        offset: u32,
        inst: Bytecode.Inst,
    };

    pub fn next(it: *Iterator) error{InvalidInstruction}!?Entry {
        if (it.offset >= it.code.len) return null;
        const inst, const size = Bytecode.Inst.decode(it.code[it.offset..]) orelse {
            return error.InvalidInstruction;
        };
        const offset = it.offset;
        it.offset += size;
        return .{ .offset = offset, .inst = inst };
    }
};

pub fn iterator(bc: *const Bytecode) Iterator {
    return .{
        .code = bc.code,
        .offset = 0,
    };
}

pub fn print(
    bc: *const Bytecode,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    try tty_config.setColor(writer, .bold);
    try writer.print("Bytecode ({s})\n", .{bc.name});
    try tty_config.setColor(writer, .reset);
    var it = bc.iterator();
    while (it.next() catch |err| switch (err) {
        error.InvalidInstruction => {
            try tty_config.setColor(writer, .red);
            try writer.print("Invalid instruction at offset {d}\n", .{it.offset});
            try tty_config.setColor(writer, .reset);
            return;
        },
    }) |entry| {
        const size = it.offset - entry.offset;

        try writer.print("{d: >4}: ", .{entry.offset});

        try tty_config.setColor(writer, .dim);
        for (bc.code[entry.offset..][0..size]) |byte| {
            try writer.print("{x:0>2} ", .{byte});
        }
        _ = try writer.splatByteAll(' ', (10 - size) * 3);
        try tty_config.setColor(writer, .reset);

        try writer.print("{f}\n", .{entry.inst.fmt(bc, tty_config)});
    }
}

fn printData(
    bc: *const Bytecode,
    tag: Inst.Tag,
    data: Inst.Data,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const data_tag = Inst.data_tags[@intFromEnum(tag)];
    if (data_tag == .none) return;
    if (tag == .@"return" and data.reg == .none) return;

    try writer.writeByte(' ');
    switch (data_tag) {
        .none => {},
        inline .i32,
        .reg,
        .string,
        => |dt| {
            const field_data = @field(data, @tagName(dt));
            try printField(bc, field_data, writer, tty_config);
        },
        inline else => |dt| {
            const field_data = @field(data, @tagName(dt));
            const field_type = @typeInfo(@TypeOf(field_data)).@"struct";
            inline for (field_type.fields, 0..) |struct_field, idx| {
                if (idx > 0) try writer.writeAll(", ");
                const value = @field(field_data, struct_field.name);
                try printField(bc, value, writer, tty_config);
            }
        },
    }
}

fn printField(
    bc: *const Bytecode,
    value: anytype,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const T = @TypeOf(value);
    switch (T) {
        u16,
        i32,
        u32,
        f64,
        => {
            try tty_config.setColor(writer, .magenta);
            try writer.print("{}", .{value});
            try tty_config.setColor(writer, .reset);
        },
        Inst.Reg => {
            try tty_config.setColor(writer, .blue);
            try writer.print("r{d}", .{@intFromEnum(value)});
            try tty_config.setColor(writer, .reset);
        },
        Inst.StringIndex => {
            const str = bc.strings[@intFromEnum(value)];
            try tty_config.setColor(writer, .yellow);
            try writer.print("@{d}", .{@intFromEnum(value)});
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(" (");
            try tty_config.setColor(writer, .green);
            try writer.print("\"{s}\"", .{str});
            try tty_config.setColor(writer, .reset);
            try writer.writeByte(')');
        },
        Inst.BigIntIndex => {
            const big_int = bc.big_ints[@intFromEnum(value)];
            try tty_config.setColor(writer, .yellow);
            try writer.print("@{d}", .{@intFromEnum(value)});
            try tty_config.setColor(writer, .reset);
            try writer.writeAll(" (");
            try tty_config.setColor(writer, .magenta);
            try big_int.formatNumber(writer, .{});
            try writer.writeByte('n');
            try tty_config.setColor(writer, .reset);
            try writer.writeByte(')');
        },
        Inst.FunctionIndex,
        Inst.ClassIndex,
        => {
            try tty_config.setColor(writer, .yellow);
            try writer.print("@{d}", .{@intFromEnum(value)});
            try tty_config.setColor(writer, .reset);
        },
        else => comptime unreachable,
    }
}
