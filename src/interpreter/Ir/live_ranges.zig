const std = @import("std");

const interpreter = @import("../../interpreter.zig");

const Ir = interpreter.Ir;

pub const LiveRange = struct {
    start: u32,
    end: u32,
};

pub fn computeLiveRanges(
    gpa: std.mem.Allocator,
    instructions: std.MultiArrayList(Ir.Inst).Slice,
    extras: []const u32,
) std.mem.Allocator.Error![]LiveRange {
    var live_ranges = try gpa.alloc(LiveRange, instructions.len);

    for (live_ranges, 0..) |*live_range, i| {
        live_range.* = .{ .start = @intCast(i), .end = @intCast(i) };
    }

    for (instructions.items(.tag), instructions.items(.data), 0..) |tag, data, inst_index| {
        var uses: std.ArrayList(Ir.Inst.Ref) = .empty;
        defer uses.deinit(gpa);

        switch (tag) {
            .undefined,
            .null,
            .true,
            .false,
            .zero,
            .one,
            .number,
            .string,
            .big_int,
            .get_binding,
            .update_binding,
            .update_binding_strict,
            .delete_binding,
            => {},
            .get_property => try uses.append(gpa, data.get_property.base),
            .get_property_computed => try uses.appendSlice(gpa, &.{
                data.get_property_computed.base,
                data.get_property_computed.property,
            }),
            .get_property_indexed => try uses.append(gpa, data.get_property_indexed.base),
            .set_binding,
            .set_binding_strict,
            => try uses.append(gpa, data.set_binding.value),
            .set_property,
            .set_property_strict,
            => try uses.appendSlice(gpa, &.{
                data.set_property.base,
                data.set_property.value,
            }),
            .set_property_computed,
            .set_property_computed_strict,
            => try uses.appendSlice(gpa, &.{
                data.set_property_computed.base,
                data.set_property_computed.property,
                data.set_property_computed.value,
            }),
            .set_property_indexed,
            .set_property_indexed_strict,
            => try uses.appendSlice(gpa, &.{
                data.set_property_indexed.base,
                data.set_property_indexed.value,
            }),
            .update_property,
            .update_property_strict,
            => try uses.append(gpa, data.update_property.base),
            .update_property_computed,
            .update_property_computed_strict,
            => try uses.appendSlice(gpa, &.{
                data.update_property_computed.base,
                data.update_property_computed.property,
            }),
            .update_property_indexed,
            .update_property_indexed_strict,
            => try uses.append(gpa, data.update_property_indexed.base),
            .delete_property,
            .delete_property_strict,
            => try uses.append(gpa, data.delete_property.base),
            .delete_property_computed,
            .delete_property_computed_strict,
            => try uses.appendSlice(gpa, &.{
                data.delete_property_computed.base,
                data.delete_property_computed.property,
            }),
            .delete_property_indexed,
            .delete_property_indexed_strict,
            => try uses.append(gpa, data.delete_property_indexed.base),
            .array => {
                const extra_index = @intFromEnum(data.array.extra_index);
                const elements = @as([*]const Ir.Inst.Ref, @ptrCast(extras[extra_index..]))[0..data.array.len];
                for (elements) |elem| {
                    if (elem != .none) try uses.append(gpa, elem);
                }
            },
            .object => {
                const extra_index = @intFromEnum(data.object.extra_index);
                const pairs = @as([*]const Ir.Inst.Ref, @ptrCast(extras[extra_index..]))[0 .. data.object.len * 2];
                for (pairs) |ref| {
                    if (ref != .none) try uses.append(gpa, ref);
                }
            },
            .@"if" => try uses.appendSlice(gpa, &.{
                data.@"if".@"test",
                data.@"if".then,
                data.@"if".@"else",
            }),
            .@"while" => try uses.appendSlice(gpa, &.{
                data.@"while".@"test",
                data.@"while".body,
            }),
            .@"for" => try uses.appendSlice(gpa, &.{
                data.@"for".@"test",
                data.@"for".update,
                data.@"for".body,
            }),
            .loop => try uses.appendSlice(gpa, &.{
                data.loop.body,
                data.loop.update,
            }),
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
            => try uses.appendSlice(gpa, &.{
                data.binary.lhs,
                data.binary.rhs,
            }),
            .unary_plus,
            .unary_minus,
            .bitwise_not,
            .logical_not,
            .typeof,
            .void,
            .delete,
            .end,
            => try uses.append(gpa, data.ref),
        }
        for (uses.items) |use| {
            if (use.toIndex()) |index| {
                const live_range = &live_ranges[@intFromEnum(index)];
                live_range.end = @max(live_range.end, @as(u32, @intCast(inst_index)));
            }
        }
    }

    return live_ranges;
}
