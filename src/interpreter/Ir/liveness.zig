const std = @import("std");

const interpreter = @import("../../interpreter.zig");

const Ir = interpreter.Ir;

pub fn computeLiveness(
    gpa: std.mem.Allocator,
    instructions: std.MultiArrayList(Ir.Inst).Slice,
    extras: []const u32,
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
            else => try markLive(gpa, instructions, extras, &live, @enumFromInt(i)),
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
            .@"if" => {
                if (data.@"if".then.toIndex()) |then_index| {
                    if (!reachable.isSet(@intFromEnum(then_index))) {
                        try worklist.append(gpa, then_index);
                    }
                }
                if (data.@"if".@"else".toIndex()) |else_index| {
                    if (!reachable.isSet(@intFromEnum(else_index))) {
                        try worklist.append(gpa, else_index);
                    }
                }
            },
            .@"while" => {
                if (data.@"while".body.toIndex()) |body_idx| {
                    if (!reachable.isSet(@intFromEnum(body_idx))) {
                        try worklist.append(gpa, body_idx);
                    }
                }
            },
            .@"for" => {
                if (data.@"for".body.toIndex()) |body_index| {
                    if (!reachable.isSet(@intFromEnum(body_index))) {
                        try worklist.append(gpa, body_index);
                    }
                }
            },
            .loop => {
                if (data.loop.body.toIndex()) |body_index| {
                    if (!reachable.isSet(@intFromEnum(body_index))) {
                        try worklist.append(gpa, body_index);
                    }
                }
                continue; // Infinite loop never reaches next instruction
            },
            .end => continue,
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
    extras: []const u32,
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
        try Ir.Inst.collectRefs(gpa, tag, data, extras, &uses);

        for (uses.items) |use| {
            if (use.toIndex()) |use_index| {
                try worklist.append(gpa, use_index);
            }
        }
    }
}
