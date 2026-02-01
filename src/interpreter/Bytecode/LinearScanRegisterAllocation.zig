//! Linear Scan Register Allocation
//!
//! https://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

const std = @import("std");

const interpreter = @import("../../interpreter.zig");

const Bytecode = interpreter.Bytecode;
const Ir = interpreter.Ir;

const LinearScanRegisterAllocation = @This();

allocations: []Allocation,
num_spill_slots: u16,

pub const Allocation = union(enum) {
    register: Bytecode.Inst.Reg,
    spilled: u16,
    none,
};

const ActiveRange = struct {
    index: u32,
    end: u32,
};

pub fn init(
    gpa: std.mem.Allocator,
    live_ranges: []const Ir.LiveRange,
    num_regs: u8,
) std.mem.Allocator.Error!LinearScanRegisterAllocation {
    var allocations = try gpa.alloc(Allocation, live_ranges.len);
    errdefer gpa.free(allocations);
    @memset(allocations, .none);

    var active: std.ArrayListUnmanaged(ActiveRange) = .empty;
    defer active.deinit(gpa);

    var free_regs: std.bit_set.DynamicBitSetUnmanaged = try .initFull(gpa, num_regs);
    defer free_regs.deinit(gpa);

    var next_spill: u16 = 0;

    for (live_ranges, 0..) |live_range, i| {
        // Expire old ranges
        var j: usize = 0;
        while (j < active.items.len) {
            if (active.items[j].end < live_range.start) {
                switch (allocations[active.items[j].index]) {
                    .register => |reg| free_regs.set(@intFromEnum(reg)),
                    else => {},
                }
                _ = active.swapRemove(j);
            } else {
                j += 1;
            }
        }

        // Use register without marking active for dead values
        if (live_range.end == live_range.start) {
            if (free_regs.findFirstSet()) |reg| {
                allocations[i] = .{ .register = @enumFromInt(reg) };
            } else {
                allocations[i] = .{ .spilled = next_spill };
                next_spill += 1;
            }
            continue;
        }

        // Allocate register if available, spill otherwise
        if (free_regs.findFirstSet()) |reg| {
            allocations[i] = .{ .register = @enumFromInt(reg) };
            free_regs.unset(reg);
            try active.append(gpa, .{ .index = @intCast(i), .end = live_range.end });
        } else {
            var last_active_end = live_range.end;
            var last_active_index: ?usize = null;
            for (active.items, 0..) |active_range, k| {
                if (active_range.end > last_active_end) {
                    last_active_end = active_range.end;
                    last_active_index = k;
                }
            }
            if (last_active_index) |index| {
                const inst_to_spill = active.items[index].index;
                allocations[i] = .{ .register = allocations[inst_to_spill].register };
                allocations[inst_to_spill] = .{ .spilled = next_spill };
                next_spill += 1;
                active.items[index] = .{ .index = @intCast(i), .end = live_range.end };
            } else {
                allocations[i] = .{ .spilled = next_spill };
                next_spill += 1;
            }
        }
    }

    return .{
        .allocations = allocations,
        .num_spill_slots = next_spill,
    };
}

pub fn deinit(lsra: *LinearScanRegisterAllocation, gpa: std.mem.Allocator) void {
    gpa.free(lsra.allocations);
}
