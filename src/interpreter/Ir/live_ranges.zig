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
    extra: []const u32,
) std.mem.Allocator.Error![]LiveRange {
    var live_ranges = try gpa.alloc(LiveRange, instructions.len);

    for (live_ranges, 0..) |*live_range, i| {
        live_range.* = .{ .start = @intCast(i), .end = @intCast(i) };
    }

    const tags = instructions.items(.tag);
    const datas = instructions.items(.data);

    // Pass 1: Compute live ranges from direct uses
    for (tags, datas, 0..) |tag, data, inst_index| {
        var uses: std.ArrayList(Ir.Inst.Ref) = .empty;
        defer uses.deinit(gpa);
        try Ir.Inst.collectRefs(gpa, tag, data, extra, &uses);

        for (uses.items) |use| {
            if (use.toIndex()) |index| {
                const live_range = &live_ranges[@intFromEnum(index)];
                live_range.end = @max(live_range.end, @as(u32, @intCast(inst_index)));
            }
        }
    }

    // Pass 2: Extend live ranges across back-edges
    for (tags, datas, 0..) |tag, data, inst_index| {
        const index = switch (tag) {
            .br => blk: {
                if (data.br.target.toIndex()) |target_index| {
                    if (@intFromEnum(target_index) < inst_index) {
                        break :blk target_index;
                    }
                }
                continue;
            },
            .br_cond => {
                // br_cond currently only creates forward jumps
                if (data.br_cond.then_target.toIndex()) |target_index| {
                    std.debug.assert(@intFromEnum(target_index) >= inst_index);
                }
                if (data.br_cond.else_target.toIndex()) |target_index| {
                    std.debug.assert(@intFromEnum(target_index) >= inst_index);
                }
                continue;
            },
            else => continue,
        };
        for (live_ranges) |*live_range| {
            if (live_range.start <= @intFromEnum(index) and live_range.end >= @intFromEnum(index)) {
                live_range.end = @max(live_range.end, @as(u32, @intCast(inst_index)));
            }
        }
    }

    return live_ranges;
}
