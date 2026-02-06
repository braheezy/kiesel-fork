const std = @import("std");

pub const Ir = @This();

name: []const u8,
instructions: std.MultiArrayList(Inst).Slice,
liveness: std.DynamicBitSetUnmanaged,
live_ranges: []const LiveRange,
strings: []const []const u8,
big_ints: []const std.math.big.int.Const,
extras: []const u32,

pub const Builder = @import("Ir/Builder.zig");
pub const LiveRange = @import("Ir/live_ranges.zig").LiveRange;

pub const PrintError =
    std.Io.Writer.Error ||
    std.Io.tty.Config.SetColorError;

const CountingWriter = struct {
    out: *std.Io.Writer,
    count: usize,
    writer: std.Io.Writer,

    fn init(out: *std.Io.Writer, buffer: []u8) CountingWriter {
        return .{
            .out = out,
            .count = 0,
            .writer = .{
                .buffer = buffer,
                .vtable = &.{
                    .drain = drain,
                },
            },
        };
    }

    fn drain(w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        const cw: *CountingWriter = @alignCast(@fieldParentPtr("writer", w));

        const aux = w.buffered();
        const aux_n = try cw.out.writeSplatHeader(aux, data, splat);
        if (aux_n < w.end) {
            cw.count += aux_n;
            const remaining = w.buffer[aux_n..w.end];
            @memmove(w.buffer[0..remaining.len], remaining);
            w.end = remaining.len;
            return 0;
        }

        const total = w.end + blk: {
            var n: usize = 0;
            for (data[0 .. data.len - 1]) |slice| n += slice.len;
            n += data[data.len - 1].len * splat;
            break :blk n;
        };

        cw.count += total;
        w.end = 0;
        return aux_n - aux.len;
    }
};

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        undefined,
        null,
        true,
        false,
        zero,
        one,
        number,
        string,
        big_int,
        array,
        object,
        reg_exp,
        this,

        label,
        br,
        br_cond,

        to_number,
        to_string,
        negate,
        bitwise_not,
        logical_not,
        typeof,
        void,
        delete,
        spread,

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

        logical_and,
        logical_or,
        nullish_coalesce,

        push_scope,
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
        update_binding,
        update_binding_strict,
        update_property,
        update_property_strict,
        update_property_computed,
        update_property_computed_strict,
        update_property_indexed,
        update_property_indexed_strict,
        delete_binding,
        delete_property,
        delete_property_strict,
        delete_property_computed,
        delete_property_computed_strict,
        delete_property_indexed,
        delete_property_indexed_strict,
        copy_data_properties,

        call,
        new,

        get_iterator,
        get_for_in_iterator,
        iterator_step,
        iterator_step_value,
        iterator_is_done,
        iterator_collect,

        throw,

        end,
    };

    pub const Data = union {
        none: void,
        boolean: bool,
        number: f64,
        string: StringIndex,
        big_int: BigIntIndex,
        array: struct { extra_index: ExtraIndex, len: u32 },
        object: struct { extra_index: ExtraIndex, len: u32 },
        reg_exp: struct { pattern: StringIndex, flags: StringIndex },
        br: struct { target: Ref, value: Ref },
        br_cond: struct { condition: Ref, then_target: Ref, else_target: Ref },
        binary: struct { lhs: Ref, rhs: Ref },
        get_property: struct { base: Ref, name: StringIndex },
        get_property_computed: struct { base: Ref, property: Ref },
        get_property_indexed: struct { base: Ref, index: u32 },
        set_binding: struct { name: StringIndex, value: Ref },
        set_property: struct { base: Ref, name: StringIndex, value: Ref },
        set_property_computed: struct { base: Ref, property: Ref, value: Ref },
        set_property_indexed: struct { base: Ref, index: u32, value: Ref },
        update_binding: struct { name: StringIndex, update_op: UpdateOp, update_type: UpdateType },
        update_property: struct { base: Ref, name: StringIndex, update_op: UpdateOp, update_type: UpdateType },
        update_property_computed: struct { base: Ref, property: Ref, update_op: UpdateOp, update_type: UpdateType },
        update_property_indexed: struct { base: Ref, index: u32, update_op: UpdateOp, update_type: UpdateType },
        delete_property: struct { base: Ref, name: StringIndex },
        delete_property_computed: struct { base: Ref, property: Ref },
        delete_property_indexed: struct { base: Ref, index: u32 },
        copy_data_properties: struct { source: Ref, extra_index: ExtraIndex, len: u32 },
        call: struct { callee: Ref, this_value: Ref, extra_index: ExtraIndex, len: u32 },
        new: struct { constructor: Ref, extra_index: ExtraIndex, len: u32 },
        ref: Ref,
    };

    pub const data_tags = std.enums.directEnumArray(Tag, std.meta.FieldEnum(Data), 0, .{
        .undefined = .none,
        .null = .none,
        .true = .none,
        .false = .none,
        .zero = .none,
        .one = .none,
        .number = .number,
        .string = .string,
        .big_int = .big_int,
        .array = .array,
        .object = .object,
        .reg_exp = .reg_exp,
        .this = .none,
        .label = .none,
        .br = .br,
        .br_cond = .br_cond,
        .to_number = .ref,
        .to_string = .ref,
        .negate = .ref,
        .bitwise_not = .ref,
        .logical_not = .ref,
        .typeof = .ref,
        .void = .ref,
        .delete = .ref,
        .spread = .ref,
        .add = .binary,
        .sub = .binary,
        .mul = .binary,
        .div = .binary,
        .rem = .binary,
        .exp = .binary,
        .shift_left = .binary,
        .shift_right = .binary,
        .shift_right_unsigned = .binary,
        .bitwise_and = .binary,
        .bitwise_or = .binary,
        .bitwise_xor = .binary,
        .lt = .binary,
        .gt = .binary,
        .lt_eq = .binary,
        .gt_eq = .binary,
        .instanceof = .binary,
        .in = .binary,
        .eq = .binary,
        .not_eq = .binary,
        .eq_strict = .binary,
        .not_eq_strict = .binary,
        .logical_and = .binary,
        .logical_or = .binary,
        .nullish_coalesce = .binary,
        .push_scope = .none,
        .pop_scope = .none,
        .create_mutable_binding = .string,
        .create_immutable_binding = .string,
        .initialize_binding = .set_binding,
        .get_binding = .string,
        .get_property = .get_property,
        .get_property_computed = .get_property_computed,
        .get_property_indexed = .get_property_indexed,
        .set_binding = .set_binding,
        .set_binding_strict = .set_binding,
        .set_property = .set_property,
        .set_property_strict = .set_property,
        .set_property_computed = .set_property_computed,
        .set_property_computed_strict = .set_property_computed,
        .set_property_indexed = .set_property_indexed,
        .set_property_indexed_strict = .set_property_indexed,
        .update_binding = .update_binding,
        .update_binding_strict = .update_binding,
        .update_property = .update_property,
        .update_property_strict = .update_property,
        .update_property_computed = .update_property_computed,
        .update_property_computed_strict = .update_property_computed,
        .update_property_indexed = .update_property_indexed,
        .update_property_indexed_strict = .update_property_indexed,
        .delete_binding = .string,
        .delete_property = .delete_property,
        .delete_property_strict = .delete_property,
        .delete_property_computed = .delete_property_computed,
        .delete_property_computed_strict = .delete_property_computed,
        .delete_property_indexed = .delete_property_indexed,
        .delete_property_indexed_strict = .delete_property_indexed,
        .copy_data_properties = .copy_data_properties,
        .call = .call,
        .new = .new,
        .get_iterator = .ref,
        .get_for_in_iterator = .ref,
        .iterator_step = .ref,
        .iterator_step_value = .ref,
        .iterator_is_done = .ref,
        .iterator_collect = .ref,
        .throw = .ref,
        .end = .ref,
    });

    pub const UpdateOp = enum { increment, decrement };
    pub const UpdateType = enum { prefix, postfix };

    pub const StringIndex = enum(u32) { _ };
    pub const BigIntIndex = enum(u32) { _ };
    pub const ExtraIndex = enum(u32) { _ };

    pub const Ref = enum(u32) {
        none,
        _,

        const static_len = 1;

        pub fn toIndex(ref: Ref) ?Inst.Index {
            if (@intFromEnum(ref) < static_len) return null;
            return @enumFromInt(@intFromEnum(ref) - static_len);
        }
    };

    pub const Index = enum(u32) {
        start,
        _,

        pub fn toRef(index: Index) Inst.Ref {
            return @enumFromInt(Ref.static_len + @intFromEnum(index));
        }
    };

    pub fn collectRefs(
        gpa: std.mem.Allocator,
        tag: Tag,
        data: Data,
        extras: []const u32,
        uses: *std.ArrayList(Ref),
    ) std.mem.Allocator.Error!void {
        const data_tag = data_tags[@intFromEnum(tag)];
        switch (data_tag) {
            .none,
            .boolean,
            .number,
            .string,
            .big_int,
            => {},
            .array => {
                const extra_index = @intFromEnum(data.array.extra_index);
                const elements = @as([*]const Ref, @ptrCast(extras.ptr + extra_index))[0..data.array.len];
                for (elements) |elem| {
                    if (elem != .none) try uses.append(gpa, elem);
                }
            },
            .object => {
                const extra_index = @intFromEnum(data.object.extra_index);
                const pairs = @as([*]const Ref, @ptrCast(extras.ptr + extra_index))[0 .. data.object.len * 2];
                for (pairs) |ref| {
                    if (ref != .none) try uses.append(gpa, ref);
                }
            },
            .call => {
                try uses.append(gpa, data.call.callee);
                if (data.call.this_value != .none) try uses.append(gpa, data.call.this_value);
                const extra_index = @intFromEnum(data.call.extra_index);
                const args = @as([*]const Ref, @ptrCast(extras.ptr + extra_index))[0..data.call.len];
                for (args) |arg| try uses.append(gpa, arg);
            },
            .new => {
                try uses.append(gpa, data.new.constructor);
                const extra_index = @intFromEnum(data.new.extra_index);
                const args = @as([*]const Ref, @ptrCast(extras.ptr + extra_index))[0..data.new.len];
                for (args) |arg| try uses.append(gpa, arg);
            },
            .copy_data_properties => {
                try uses.append(gpa, data.copy_data_properties.source);
                const extra_index = @intFromEnum(data.copy_data_properties.extra_index);
                const excluded = @as([*]const Ref, @ptrCast(extras.ptr + extra_index))[0..data.copy_data_properties.len];
                for (excluded) |prop| {
                    if (prop != .none) try uses.append(gpa, prop);
                }
            },
            .ref => try uses.append(gpa, data.ref),
            inline else => |dt| {
                const field_data = @field(data, @tagName(dt));
                const field_type = @typeInfo(@TypeOf(field_data)).@"struct";
                inline for (field_type.fields) |struct_field| {
                    if (struct_field.type == Ref) {
                        try uses.append(gpa, @field(field_data, struct_field.name));
                    }
                }
            },
        }
    }
};

pub fn deinit(ir: *Ir, gpa: std.mem.Allocator) void {
    gpa.free(ir.name);
    ir.instructions.deinit(gpa);
    ir.liveness.deinit(gpa);
    gpa.free(ir.live_ranges);
    for (ir.strings) |string| gpa.free(string);
    gpa.free(ir.strings);
    for (ir.big_ints) |big_int| gpa.free(big_int.limbs);
    gpa.free(ir.big_ints);
    gpa.free(ir.extras);
}

pub fn print(
    ir: *const Ir,
    writer: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    try tty_config.setColor(writer, .bold);
    try writer.print("IR ({s})\n", .{ir.name});
    try tty_config.setColor(writer, .reset);
    for (ir.instructions.items(.tag), ir.instructions.items(.data), 0..) |tag, data, i| {
        var buffer: [256]u8 = undefined;
        var counting_writer = CountingWriter.init(writer, &buffer);
        const cw = &counting_writer.writer;

        try cw.print("{d: >4}: ", .{i});

        try cw.flush();
        try tty_config.setColor(writer, .cyan);
        try cw.print("{t}", .{tag});
        try cw.flush();
        try tty_config.setColor(writer, .reset);

        try printData(ir, tag, data, cw, tty_config);

        try cw.flush();
        try writer.flush();
        const width = counting_writer.count;
        const min_width = 60;
        if (width < min_width) {
            _ = try writer.splatByte(' ', min_width - width);
        } else {
            try writer.writeByte(' ');
        }

        const live_range = ir.live_ranges[i];
        const is_live = ir.liveness.isSet(i);

        try tty_config.setColor(writer, .dim);
        try writer.print("[{d}..{d}]", .{ live_range.start, live_range.end });
        if (!is_live) try writer.writeAll(" dead");
        try tty_config.setColor(writer, .reset);

        try writer.print("\n", .{});
    }
}

fn printData(
    ir: *const Ir,
    tag: Inst.Tag,
    data: Inst.Data,
    cw: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const data_tag = Inst.data_tags[@intFromEnum(tag)];
    if (data_tag == .none) return;
    if (tag == .end and data.ref == .none) return;

    try cw.writeByte(' ');
    switch (data_tag) {
        .none => {},
        inline .boolean,
        .number,
        .string,
        .big_int,
        .ref,
        => |dt| {
            const field_data = @field(data, @tagName(dt));
            try printField(ir, field_data, cw, tty_config);
        },
        .array => {
            try cw.writeByte('[');
            const extra_index = @intFromEnum(data.array.extra_index);
            const elements = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0..data.array.len];
            for (elements, 0..) |element, j| {
                if (j > 0) try cw.writeAll(", ");
                try printField(ir, element, cw, tty_config);
            }
            try cw.writeByte(']');
        },
        .object => {
            try cw.writeByte('{');
            const extra_index = @intFromEnum(data.object.extra_index);
            const pairs = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0 .. data.object.len * 2];
            var pair_index: usize = 0;
            while (pair_index < pairs.len) : (pair_index += 2) {
                if (pair_index > 0) try cw.writeAll(", ");
                try printField(ir, pairs[pair_index], cw, tty_config);
                try cw.writeAll(": ");
                try printField(ir, pairs[pair_index + 1], cw, tty_config);
            }
            try cw.writeByte('}');
        },
        .call => {
            try printField(ir, data.call.callee, cw, tty_config);
            try cw.writeAll(", ");
            try printField(ir, data.call.this_value, cw, tty_config);
            try cw.writeAll(", [");
            const extra_index = @intFromEnum(data.call.extra_index);
            const args = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0..data.call.len];
            for (args, 0..) |arg, j| {
                if (j > 0) try cw.writeAll(", ");
                try printField(ir, arg, cw, tty_config);
            }
            try cw.writeByte(']');
        },
        .new => {
            try printField(ir, data.new.constructor, cw, tty_config);
            try cw.writeAll(", [");
            const extra_index = @intFromEnum(data.new.extra_index);
            const args = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0..data.new.len];
            for (args, 0..) |arg, j| {
                if (j > 0) try cw.writeAll(", ");
                try printField(ir, arg, cw, tty_config);
            }
            try cw.writeByte(']');
        },
        .copy_data_properties => {
            try printField(ir, data.copy_data_properties.source, cw, tty_config);
            try cw.writeAll(", [");
            const extra_index = @intFromEnum(data.copy_data_properties.extra_index);
            const excluded = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0..data.copy_data_properties.len];
            for (excluded, 0..) |prop, j| {
                if (j > 0) try cw.writeAll(", ");
                try printField(ir, prop, cw, tty_config);
            }
            try cw.writeByte(']');
        },
        inline else => |field| {
            const field_data = @field(data, @tagName(field));
            const field_type = @typeInfo(@TypeOf(field_data)).@"struct";
            inline for (field_type.fields, 0..) |struct_field, i| {
                if (i > 0) try cw.writeAll(", ");
                const field_value = @field(field_data, struct_field.name);
                try printField(ir, field_value, cw, tty_config);
            }
        },
    }
}

fn printField(
    ir: *const Ir,
    value: anytype,
    cw: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const counting_writer: *CountingWriter = @alignCast(@fieldParentPtr("writer", cw));
    const writer = counting_writer.out;
    const T = @TypeOf(value);
    switch (T) {
        bool,
        u32,
        i32,
        f64,
        => {
            try cw.flush();
            try tty_config.setColor(writer, .yellow);
            try cw.print("{}", .{value});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        Inst.Ref => if (value.toIndex()) |index| {
            try cw.flush();
            try tty_config.setColor(writer, .blue);
            try cw.print("%{d}", .{@intFromEnum(index)});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        } else {
            try cw.flush();
            try tty_config.setColor(writer, .dim);
            try cw.print("none", .{});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        Inst.StringIndex => {
            const str = ir.strings[@intFromEnum(value)];
            try cw.flush();
            try tty_config.setColor(writer, .yellow);
            try cw.print("\"{s}\"", .{str});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        Inst.BigIntIndex => {
            const big_int = ir.big_ints[@intFromEnum(value)];
            try cw.flush();
            try tty_config.setColor(writer, .yellow);
            try big_int.formatNumber(cw, .{});
            try cw.writeByte('n');
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        Inst.ExtraIndex => {
            try cw.flush();
            try tty_config.setColor(writer, .yellow);
            try cw.print("@{d}", .{@intFromEnum(value)});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        Inst.UpdateOp,
        Inst.UpdateType,
        => {
            try cw.flush();
            try tty_config.setColor(writer, .magenta);
            try cw.print("{t}", .{value});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        else => comptime unreachable,
    }
}
