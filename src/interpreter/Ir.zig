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

        cw.count += w.end;
        for (data[0 .. data.len - 1]) |slice| {
            cw.count += slice.len;
        }
        cw.count += data[data.len - 1].len * splat;

        const aux = w.buffered();
        const aux_n = try cw.out.writeSplatHeader(aux, data, splat);
        if (aux_n < w.end) {
            const remaining = w.buffer[aux_n..w.end];
            @memmove(w.buffer[0..remaining.len], remaining);
            w.end = remaining.len;
            return 0;
        }
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

        @"if",
        @"while",
        @"for",
        loop,

        unary_plus,
        unary_minus,
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

        call,

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
        @"if": struct { @"test": Ref, then: Ref, @"else": Ref },
        @"while": struct { @"test": Ref, body: Ref },
        @"for": struct { @"test": Ref, update: Ref, body: Ref },
        loop: struct { body: Ref, update: Ref },
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
        call: struct { callee: Ref, this_value: Ref, extra_index: ExtraIndex, len: u32 },
        ref: Ref,
    };

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

        switch (tag) {
            .undefined,
            .null,
            .true,
            .false,
            .zero,
            .one,
            => {},
            .number => {
                try cw.writeByte(' ');
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("{d}", .{data.number});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .string,
            .get_binding,
            .delete_binding,
            => {
                const str = ir.strings[@intFromEnum(data.string)];
                try cw.writeByte(' ');
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .update_binding,
            .update_binding_strict,
            => {
                try cw.writeByte(' ');
                try cw.print("{t} {t} ", .{
                    data.update_binding.update_type,
                    data.update_binding.update_op,
                });
                const str = ir.strings[@intFromEnum(data.update_binding.name)];
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .get_property => {
                const str = ir.strings[@intFromEnum(data.get_property.name)];
                try cw.writeByte(' ');
                try printRef(data.get_property.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .get_property_computed => {
                try cw.writeByte(' ');
                try printRef(data.get_property_computed.base, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.get_property_computed.property, cw, tty_config);
            },
            .get_property_indexed => {
                try cw.writeByte(' ');
                try printRef(data.get_property_indexed.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("{d}", .{data.get_property_indexed.index});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .set_property,
            .set_property_strict,
            => {
                const str = ir.strings[@intFromEnum(data.set_property.name)];
                try cw.writeByte(' ');
                try printRef(data.set_property.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
                try cw.print(", ", .{});
                try printRef(data.set_property.value, cw, tty_config);
            },
            .set_property_computed,
            .set_property_computed_strict,
            => {
                try cw.writeByte(' ');
                try printRef(data.set_property_computed.base, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.set_property_computed.property, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.set_property_computed.value, cw, tty_config);
            },
            .set_property_indexed,
            .set_property_indexed_strict,
            => {
                try cw.writeByte(' ');
                try printRef(data.set_property_indexed.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("{d}", .{data.set_property_indexed.index});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
                try cw.print(", ", .{});
                try printRef(data.set_property_indexed.value, cw, tty_config);
            },
            .update_property,
            .update_property_strict,
            => {
                try cw.writeByte(' ');
                try cw.print("{t} {t} ", .{
                    data.update_property.update_type,
                    data.update_property.update_op,
                });
                try printRef(data.update_property.base, cw, tty_config);
                try cw.print(", ", .{});
                const str = ir.strings[@intFromEnum(data.update_property.name)];
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .update_property_computed,
            .update_property_computed_strict,
            => {
                try cw.writeByte(' ');
                try cw.print("{t} {t} ", .{
                    data.update_property_computed.update_type,
                    data.update_property_computed.update_op,
                });
                try printRef(data.update_property_computed.base, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.update_property_computed.property, cw, tty_config);
            },
            .update_property_indexed,
            .update_property_indexed_strict,
            => {
                try cw.writeByte(' ');
                try cw.print("{t} {t} ", .{
                    data.update_property_indexed.update_type,
                    data.update_property_indexed.update_op,
                });
                try printRef(data.update_property_indexed.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("{d}", .{data.update_property_indexed.index});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .delete_property,
            .delete_property_strict,
            => {
                const str = ir.strings[@intFromEnum(data.delete_property.name)];
                try cw.writeByte(' ');
                try printRef(data.delete_property.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .delete_property_computed,
            .delete_property_computed_strict,
            => {
                try cw.writeByte(' ');
                try printRef(data.delete_property_computed.base, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.delete_property_computed.property, cw, tty_config);
            },
            .delete_property_indexed,
            .delete_property_indexed_strict,
            => {
                try cw.writeByte(' ');
                try printRef(data.delete_property_indexed.base, cw, tty_config);
                try cw.print(", ", .{});
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("{d}", .{data.delete_property_indexed.index});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .call => {
                try cw.writeByte(' ');
                try printRef(data.call.callee, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.call.this_value, cw, tty_config);
                try cw.print(", ", .{});
                try cw.writeByte('[');
                const extra_index = @intFromEnum(data.call.extra_index);
                const args = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0..data.call.len];
                for (args, 0..) |arg, j| {
                    if (j > 0) try cw.writeAll(", ");
                    try printRef(arg, cw, tty_config);
                }
                try cw.writeByte(']');
            },
            .big_int => {
                const big_int = ir.big_ints[@intFromEnum(data.big_int)];
                try cw.writeByte(' ');
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try big_int.formatNumber(cw, .{});
                try cw.writeByte('n');
                try cw.flush();
                try tty_config.setColor(writer, .reset);
            },
            .array => {
                try cw.writeAll(" [");
                const extra_index = @intFromEnum(data.array.extra_index);
                const elements = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0..data.array.len];
                for (elements, 0..) |element, j| {
                    if (j > 0) try cw.writeAll(", ");
                    try printRef(element, cw, tty_config);
                }
                try cw.writeByte(']');
            },
            .object => {
                try cw.writeAll(" {");
                const extra_index = @intFromEnum(data.object.extra_index);
                const pairs = @as([*]const Inst.Ref, @ptrCast(ir.extras[extra_index..]))[0 .. data.object.len * 2];
                var pair_index: usize = 0;
                while (pair_index < pairs.len) : (pair_index += 2) {
                    if (pair_index > 0) try cw.writeAll(", ");
                    try printRef(pairs[pair_index], cw, tty_config);
                    try cw.writeAll(": ");
                    try printRef(pairs[pair_index + 1], cw, tty_config);
                }
                try cw.writeByte('}');
            },
            .@"if" => {
                try cw.writeByte(' ');
                try printRef(data.@"if".@"test", cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.@"if".then, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.@"if".@"else", cw, tty_config);
            },
            .@"while" => {
                try cw.writeByte(' ');
                try printRef(data.@"while".@"test", cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.@"while".body, cw, tty_config);
            },
            .@"for" => {
                try cw.writeByte(' ');
                try printRef(data.@"for".@"test", cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.@"for".update, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.@"for".body, cw, tty_config);
            },
            .loop => {
                try cw.writeByte(' ');
                try printRef(data.loop.body, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.loop.update, cw, tty_config);
            },
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
            .logical_and,
            .logical_or,
            .nullish_coalesce,
            => {
                try cw.writeByte(' ');
                try printRef(data.binary.lhs, cw, tty_config);
                try cw.print(", ", .{});
                try printRef(data.binary.rhs, cw, tty_config);
            },
            .unary_plus,
            .unary_minus,
            .bitwise_not,
            .logical_not,
            .typeof,
            .void,
            .delete,
            .spread,
            => {
                try cw.writeByte(' ');
                try printRef(data.ref, cw, tty_config);
            },
            .set_binding,
            .set_binding_strict,
            => {
                const str = ir.strings[@intFromEnum(data.set_binding.name)];
                try cw.writeByte(' ');
                try cw.flush();
                try tty_config.setColor(writer, .yellow);
                try cw.print("\"{s}\"", .{str});
                try cw.flush();
                try tty_config.setColor(writer, .reset);
                try cw.writeAll(", ");
                try printRef(data.set_binding.value, cw, tty_config);
            },
            .end => if (data.ref != .none) {
                try cw.writeByte(' ');
                try printRef(data.ref, cw, tty_config);
            },
        }

        try cw.flush();
        const width = counting_writer.count;
        const min_width = 30;
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

fn printRef(
    ref: Inst.Ref,
    cw: *std.Io.Writer,
    tty_config: std.Io.tty.Config,
) PrintError!void {
    const counting_writer: *CountingWriter = @alignCast(@fieldParentPtr("writer", cw));
    const writer = counting_writer.out;
    switch (ref) {
        .none => {
            try cw.flush();
            try tty_config.setColor(writer, .dim);
            try cw.print("none", .{});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
        else => {
            const index = ref.toIndex().?;
            try cw.flush();
            try tty_config.setColor(writer, .blue);
            try cw.print("%{d}", .{@intFromEnum(index)});
            try cw.flush();
            try tty_config.setColor(writer, .reset);
        },
    }
}
