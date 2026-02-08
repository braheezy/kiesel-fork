const std = @import("std");

const interpreter = @import("../../interpreter.zig");

const Ir = interpreter.Ir;

pub fn computeLiveness(
    gpa: std.mem.Allocator,
    instructions: std.MultiArrayList(Ir.Inst).Slice,
    extra: []const u32,
) std.mem.Allocator.Error!std.DynamicBitSetUnmanaged {
    var reachable: std.DynamicBitSetUnmanaged = try .initEmpty(gpa, instructions.len);
    defer reachable.deinit(gpa);
    var live: std.DynamicBitSetUnmanaged = try .initEmpty(gpa, instructions.len);
    errdefer live.deinit(gpa);

    const tags = instructions.items(.tag);

    // Pass 1: Reachability analysis (forward pass from start)
    try markReachable(gpa, instructions, &reachable, .start);

    // Pass 2: Liveness analysis (backward pass from side-effect instructions)
    var it = reachable.iterator(.{ .kind = .set });
    while (it.next()) |i| {
        const tag = tags[i];
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
            else => try markLive(gpa, instructions, extra, &live, @enumFromInt(i)),
        }
    }

    return live;
}

fn markReachable(
    gpa: std.mem.Allocator,
    instructions: std.MultiArrayList(Ir.Inst).Slice,
    reachable: *std.DynamicBitSetUnmanaged,
    start: Ir.Inst.Index,
) std.mem.Allocator.Error!void {
    const tags = instructions.items(.tag);
    const datas = instructions.items(.data);

    var worklist: std.ArrayList(Ir.Inst.Index) = .empty;
    defer worklist.deinit(gpa);
    try worklist.append(gpa, start);

    while (worklist.pop()) |index| {
        const i = @intFromEnum(index);
        if (reachable.isSet(i)) continue;
        reachable.set(i);

        const tag = tags[i];
        const data = datas[i];

        switch (tag) {
            .br => {
                const target_index = data.br.target.toIndex().?;
                if (!reachable.isSet(@intFromEnum(target_index))) {
                    try worklist.append(gpa, target_index);
                }
                continue; // Unconditional branch never reaches next instruction
            },
            .br_cond => {
                const then_index = data.br_cond.then_target.toIndex().?;
                const else_index = data.br_cond.else_target.toIndex().?;
                if (!reachable.isSet(@intFromEnum(then_index))) {
                    try worklist.append(gpa, then_index);
                }
                if (!reachable.isSet(@intFromEnum(else_index))) {
                    try worklist.append(gpa, else_index);
                }
                continue; // Conditional branch doesn't fall through
            },
            .throw,
            .throw_reference_error,
            .@"return",
            => continue,
            else => {},
        }
        if (i + 1 < instructions.len and !reachable.isSet(i + 1)) {
            try worklist.append(gpa, @enumFromInt(i + 1));
        }
    }
}

fn markLive(
    gpa: std.mem.Allocator,
    instructions: std.MultiArrayList(Ir.Inst).Slice,
    extra: []const u32,
    live: *std.DynamicBitSetUnmanaged,
    start: Ir.Inst.Index,
) std.mem.Allocator.Error!void {
    const tags = instructions.items(.tag);
    const datas = instructions.items(.data);

    var worklist: std.ArrayList(Ir.Inst.Index) = .empty;
    defer worklist.deinit(gpa);
    try worklist.append(gpa, start);

    while (worklist.pop()) |index| {
        const i = @intFromEnum(index);
        if (live.isSet(i)) continue;
        live.set(i);

        const tag = tags[i];
        const data = datas[i];

        var uses: std.ArrayList(Ir.Inst.Ref) = .empty;
        defer uses.deinit(gpa);
        try Ir.Inst.collectRefs(gpa, tag, data, extra, &uses);

        for (uses.items) |use| {
            if (use.toIndex()) |use_index| {
                try worklist.append(gpa, use_index);
            }
        }
    }
}
