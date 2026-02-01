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
) std.mem.Allocator.Error![]LiveRange {
    var live_ranges = try gpa.alloc(LiveRange, instructions.len);

    for (live_ranges, 0..) |*live_range, i| {
        live_range.* = .{ .start = @intCast(i), .end = @intCast(i) };
    }

    for (instructions.items(.tag), instructions.items(.data), 0..) |tag, data, inst_index| {
        var buffer: [3]Ir.Inst.Ref = undefined;
        var uses: std.ArrayList(Ir.Inst.Ref) = .initBuffer(&buffer);
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
            => {},
            .@"if" => uses.appendSliceBounded(&.{
                data.@"if".@"test",
                data.@"if".then,
                data.@"if".@"else",
            }) catch unreachable,
            .@"while" => uses.appendSliceBounded(&.{
                data.@"while".@"test",
                data.@"while".body,
            }) catch unreachable,
            .@"for" => uses.appendSliceBounded(&.{
                data.@"for".@"test",
                data.@"for".update,
                data.@"for".body,
            }) catch unreachable,
            .loop => uses.appendSliceBounded(&.{
                data.loop.body,
                data.loop.update,
            }) catch unreachable,
            .add,
            .sub,
            .mul,
            .div,
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
            => uses.appendSliceBounded(&.{
                data.binary.lhs,
                data.binary.rhs,
            }) catch unreachable,
            .end => uses.appendBounded(data.ref) catch unreachable,
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
