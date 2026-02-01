const std = @import("std");

const interpreter = @import("../../interpreter.zig");

const Bytecode = interpreter.Bytecode;
const Ir = interpreter.Ir;
const Vm = interpreter.Vm;

const LinearScanRegisterAllocation = @import("LinearScanRegisterAllocation.zig");

const Builder = @This();

gpa: std.mem.Allocator,
ir: *const Ir,
blocks: std.ArrayListUnmanaged(*Block),
current: ?*Block,
lsra: LinearScanRegisterAllocation,

pub const Error = error{OutOfMemory};

pub fn init(gpa: std.mem.Allocator, ir: *const Ir) std.mem.Allocator.Error!Builder {
    const lsra: LinearScanRegisterAllocation = try .init(gpa, ir.live_ranges, Vm.num_regs);
    return .{
        .gpa = gpa,
        .ir = ir,
        .blocks = .empty,
        .current = null,
        .lsra = lsra,
    };
}

pub fn deinit(b: *Builder) void {
    for (b.blocks.items) |block| {
        block.deinit(b.gpa);
        b.gpa.destroy(block);
    }
    b.blocks.deinit(b.gpa);
    b.lsra.deinit(b.gpa);
}

fn computeRPO(
    gpa: std.mem.Allocator,
    block: *Block,
    visited: *std.AutoHashMapUnmanaged(*Block, void),
    out: *std.ArrayListUnmanaged(*Block),
) std.mem.Allocator.Error!void {
    const gop = try visited.getOrPut(gpa, block);
    if (gop.found_existing) return;

    switch (block.terminator) {
        .none, .noreturn => {},
        .jump => |target| try computeRPO(gpa, target, visited, out),
        .branch => |br| {
            try computeRPO(gpa, br.else_block, visited, out);
            try computeRPO(gpa, br.then_block, visited, out);
        },
    }

    try out.insert(gpa, 0, block);
}

pub fn build(b: *Builder) Error!Bytecode {
    const entry = try b.createBlock();
    b.switchToBlock(entry);

    // Lower alive instructions
    for (b.ir.instructions.items(.tag), b.ir.instructions.items(.data), 0..) |tag, data, i| {
        if (!b.ir.liveness.isSet(i)) continue;
        const index: Ir.Inst.Index = @enumFromInt(i);
        const dest = b.resolve(index.toRef());
        switch (tag) {
            .undefined => try b.lowerUndefined(dest),
            .null => try b.lowerNull(dest),
            .true => try b.lowerTrue(dest),
            .false => try b.lowerFalse(dest),
            .zero => try b.lowerZero(dest),
            .one => try b.lowerOne(dest),
            .number => try b.lowerNumber(data.number, dest),
            .string => try b.lowerString(data.string, dest),
            .big_int => try b.lowerBigInt(data.big_int, dest),
            .array => try b.lowerArray(data.array, dest),
            .object => try b.lowerObject(data.object, dest),
            .@"if" => try b.lowerIf(data.@"if", dest),
            .@"while" => try b.lowerWhile(data.@"while", dest),
            .@"for" => try b.lowerFor(data.@"for", dest),
            .loop => try b.lowerLoop(data.loop, dest),
            .unary_plus => try b.lowerUnaryPlus(data.ref, dest),
            .unary_minus => try b.lowerUnaryMinus(data.ref, dest),
            .bitwise_not => try b.lowerBitwiseNot(data.ref, dest),
            .logical_not => try b.lowerLogicalNot(data.ref, dest),
            .typeof => try b.lowerTypeof(data.ref, dest),
            .void => try b.lowerVoid(data.ref, dest),
            .add => try b.lowerAdd(data.binary, dest),
            .sub => try b.lowerSub(data.binary, dest),
            .mul => try b.lowerMul(data.binary, dest),
            .div => try b.lowerDiv(data.binary, dest),
            .rem => try b.lowerRem(data.binary, dest),
            .exp => try b.lowerExp(data.binary, dest),
            .shift_left => try b.lowerShiftLeft(data.binary, dest),
            .shift_right => try b.lowerShiftRight(data.binary, dest),
            .shift_right_unsigned => try b.lowerShiftRightUnsigned(data.binary, dest),
            .bitwise_and => try b.lowerBitwiseAnd(data.binary, dest),
            .bitwise_or => try b.lowerBitwiseOr(data.binary, dest),
            .bitwise_xor => try b.lowerBitwiseXor(data.binary, dest),
            .lt => try b.lowerLt(data.binary, dest),
            .gt => try b.lowerGt(data.binary, dest),
            .lt_eq => try b.lowerLtEq(data.binary, dest),
            .gt_eq => try b.lowerGtEq(data.binary, dest),
            .instanceof => try b.lowerInstanceof(data.binary, dest),
            .in => try b.lowerIn(data.binary, dest),
            .eq => try b.lowerEq(data.binary, dest),
            .not_eq => try b.lowerNotEq(data.binary, dest),
            .eq_strict => try b.lowerEqStrict(data.binary, dest),
            .not_eq_strict => try b.lowerNotEqStrict(data.binary, dest),
            .logical_and => try b.lowerLogicalAnd(data.binary, dest),
            .logical_or => try b.lowerLogicalOr(data.binary, dest),
            .nullish_coalesce => try b.lowerNullishCoalesce(data.binary, dest),
            .get_binding => try b.lowerGetBinding(data.string, dest),
            .set_binding => try b.lowerSetBinding(data.set_binding.name, data.set_binding.value, false, dest),
            .set_binding_strict => try b.lowerSetBinding(data.set_binding.name, data.set_binding.value, true, dest),
            .end => try b.lowerEnd(data.ref, dest),
        }
    }

    // Order blocks in reverse post-order
    var ordered: std.ArrayListUnmanaged(*Block) = .empty;
    defer ordered.deinit(b.gpa);
    var visited: std.AutoHashMapUnmanaged(*Block, void) = .empty;
    defer visited.deinit(b.gpa);
    try computeRPO(b.gpa, b.blocks.items[0], &visited, &ordered);

    // Assign offsets
    var offset: u32 = 0;
    for (ordered.items, 0..) |block, i| {
        block.offset = offset;
        offset += block.size();
        const next: ?*Block = if (i + 1 < ordered.items.len) ordered.items[i + 1] else null;
        offset += block.terminatorSize(next);
    }

    // Encode bytecode
    var aw: std.Io.Writer.Allocating = .init(b.gpa);
    errdefer aw.deinit();

    for (ordered.items, 0..) |block, i| {
        const next: ?*Block = if (i + 1 < ordered.items.len) ordered.items[i + 1] else null;
        block.encode(&aw.writer, next) catch |err| switch (err) {
            error.WriteFailed => return error.OutOfMemory,
        };
    }

    const name = try b.gpa.dupe(u8, b.ir.name);
    errdefer b.gpa.free(name);

    const code = try aw.toOwnedSlice();
    errdefer b.gpa.free(code);

    const strings = try b.gpa.alloc([]const u8, b.ir.strings.len);
    errdefer b.gpa.free(strings);
    // Ensure errdefer is valid mid-loop
    @memset(strings, &.{});
    for (b.ir.strings, 0..) |string, i| {
        strings[i] = try b.gpa.dupe(u8, string);
    }
    errdefer for (strings) |string| b.gpa.free(string);

    const big_ints = try b.gpa.alloc(std.math.big.int.Const, b.ir.big_ints.len);
    errdefer b.gpa.free(big_ints);
    // Ensure errdefer is valid mid-loop
    @memset(big_ints, .{ .limbs = &.{}, .positive = true });
    for (b.ir.big_ints, 0..) |big_int, i| {
        big_ints[i] = .{
            .limbs = try b.gpa.dupe(std.math.big.Limb, big_int.limbs),
            .positive = big_int.positive,
        };
    }
    errdefer for (big_ints) |big_int| b.gpa.free(big_int.limbs);

    return .{
        .name = name,
        .code = code,
        .strings = strings,
        .big_ints = big_ints,
    };
}

const Block = struct {
    instructions: std.ArrayListUnmanaged(Bytecode.Inst),
    terminator: Terminator,
    offset: u32,

    const empty: Block = .{
        .instructions = .empty,
        .terminator = .none,
        .offset = 0,
    };

    const Condition = enum {
        truthy,
        falsy,
        nullish,
    };

    const Terminator = union(enum) {
        none,
        jump: *Block,
        branch: struct {
            condition: Condition,
            condition_reg: Bytecode.Inst.Reg,
            then_block: *Block,
            else_block: *Block,
        },
        noreturn,
    };

    fn size(block: *const Block) u32 {
        var total: u32 = 0;
        for (block.instructions.items) |inst| total += inst.encodedSize();
        return total;
    }

    fn terminatorSize(block: *const Block, next: ?*const Block) u32 {
        return switch (block.terminator) {
            .none => unreachable,
            .noreturn => 0,
            .jump => |target| if (target == next) 0 else Bytecode.Inst.encodedSize(.{
                .tag = .jump,
                .data = .{ .i32 = 0 },
            }),
            .branch => |br| blk: {
                const jump_cond_tag: Bytecode.Inst.Tag = switch (br.condition) {
                    .truthy => .jump_if_true,
                    .falsy => .jump_if_false,
                    .nullish => .jump_if_nullish,
                };
                const jump_cond_size: u32 = Bytecode.Inst.encodedSize(.{
                    .tag = jump_cond_tag,
                    .data = .{ .reg_i32 = .{ br.condition_reg, 0 } },
                });
                const jump_size: u32 = if (br.else_block == next) 0 else Bytecode.Inst.encodedSize(.{
                    .tag = .jump,
                    .data = .{ .i32 = 0 },
                });
                break :blk jump_cond_size + jump_size;
            },
        };
    }

    fn encode(block: *const Block, writer: *std.Io.Writer, next: ?*const Block) std.Io.Writer.Error!void {
        for (block.instructions.items) |inst| {
            try inst.encode(writer);
        }

        const jump_size = comptime Bytecode.Inst.encodedSize(.{
            .tag = .jump,
            .data = .{ .i32 = 0 },
        });
        const jump_cond_size = comptime Bytecode.Inst.encodedSize(.{
            .tag = .jump_if_true,
            .data = .{ .reg_i32 = .{ .none, 0 } },
        });

        switch (block.terminator) {
            .none => unreachable,
            .noreturn => {},
            .jump => |target| {
                if (target != next) {
                    const current_offset = block.offset + block.size();
                    const target_relative: i32 = @as(i32, @intCast(target.offset)) - @as(i32, @intCast(current_offset + jump_size));
                    try (Bytecode.Inst{ .tag = .jump, .data = .{ .i32 = target_relative } }).encode(writer);
                }
            },
            .branch => |br| {
                const jump_cond_tag: Bytecode.Inst.Tag = switch (br.condition) {
                    .truthy => .jump_if_true,
                    .falsy => .jump_if_false,
                    .nullish => .jump_if_nullish,
                };
                const after_jump_cond = block.offset + block.size() + jump_cond_size;
                const then_relative: i32 = @as(i32, @intCast(br.then_block.offset)) - @as(i32, @intCast(after_jump_cond));
                try (Bytecode.Inst{
                    .tag = jump_cond_tag,
                    .data = .{ .reg_i32 = .{
                        br.condition_reg,
                        then_relative,
                    } },
                }).encode(writer);
                if (br.else_block != next) {
                    const current_offset = after_jump_cond;
                    const else_relative: i32 = @as(i32, @intCast(br.else_block.offset)) - @as(i32, @intCast(current_offset + jump_size));
                    try (Bytecode.Inst{
                        .tag = .jump,
                        .data = .{ .i32 = else_relative },
                    }).encode(writer);
                }
            },
        }
    }

    fn deinit(block: *Block, gpa: std.mem.Allocator) void {
        block.instructions.deinit(gpa);
    }
};

fn createBlock(b: *Builder) Error!*Block {
    const block = try b.gpa.create(Block);
    errdefer b.gpa.destroy(block);
    block.* = .empty;
    try b.blocks.append(b.gpa, block);
    return block;
}

fn switchToBlock(b: *Builder, block: *Block) void {
    if (b.current != null) std.debug.assert(b.terminated());
    b.current = block;
}

fn terminated(b: *const Builder) bool {
    return b.current.?.terminator != .none;
}

fn jump(b: *Builder, target: *Block) void {
    std.debug.assert(!b.terminated());
    b.current.?.terminator = .{ .jump = target };
}

fn branch(
    b: *Builder,
    condition: Block.Condition,
    condition_reg: Bytecode.Inst.Reg,
    then_block: *Block,
    else_block: *Block,
) void {
    std.debug.assert(!b.terminated());
    b.current.?.terminator = .{ .branch = .{
        .condition = condition,
        .condition_reg = condition_reg,
        .then_block = then_block,
        .else_block = else_block,
    } };
}

fn @"noreturn"(b: *Builder) void {
    std.debug.assert(!b.terminated());
    b.current.?.terminator = .noreturn;
}

fn emit(b: *Builder, inst: Bytecode.Inst) Error!void {
    try b.current.?.instructions.append(b.gpa, inst);
}

fn emitMoveIfNeeded(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src = switch (ref) {
        .none => return,
        else => b.resolve(ref),
    };
    if (src != dest) {
        try b.emit(.{ .tag = .move, .data = .{ .reg_reg = .{ dest, src } } });
    }
}

fn resolve(b: *Builder, ref: Ir.Inst.Ref) Bytecode.Inst.Reg {
    const index = ref.toIndex().?;
    switch (b.lsra.allocations[@intFromEnum(index)]) {
        .register => |reg| return reg,
        .spilled => unreachable, // TODO: Handle spill slots
        .none => unreachable, // Live instructions must have allocations
    }
}

fn lowerUndefined(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_undefined, .data = .{ .reg = dest } });
}

fn lowerNull(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_null, .data = .{ .reg = dest } });
}

fn lowerTrue(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_true, .data = .{ .reg = dest } });
}

fn lowerFalse(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_false, .data = .{ .reg = dest } });
}

fn lowerZero(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_number_i32, .data = .{ .reg_i32 = .{ dest, 0 } } });
}

fn lowerOne(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_number_i32, .data = .{ .reg_i32 = .{ dest, 1 } } });
}

fn lowerNumber(b: *Builder, n: f64, dest: Bytecode.Inst.Reg) Error!void {
    if (n == @floor(n) and n >= std.math.minInt(i32) and n <= std.math.maxInt(i32) and !std.math.isNegativeZero(n)) {
        try b.emit(.{ .tag = .load_number_i32, .data = .{ .reg_i32 = .{ dest, @intFromFloat(n) } } });
    } else {
        try b.emit(.{ .tag = .load_number_f64, .data = .{ .reg_f64 = .{ dest, n } } });
    }
}

fn lowerString(b: *Builder, string: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    const string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string));
    try b.emit(.{ .tag = .load_string, .data = .{
        .reg_string = .{ dest, string_index },
    } });
}

fn lowerBigInt(b: *Builder, big_int: Ir.Inst.BigIntIndex, dest: Bytecode.Inst.Reg) Error!void {
    const big_int_index: Bytecode.Inst.BigIntIndex = @enumFromInt(@intFromEnum(big_int));
    try b.emit(.{ .tag = .load_big_int, .data = .{
        .reg_big_int = .{ dest, big_int_index },
    } });
}

fn lowerArray(b: *Builder, array_data: @FieldType(Ir.Inst.Data, "array"), dest: Bytecode.Inst.Reg) Error!void {
    const extra_index = @intFromEnum(array_data.extra_index);
    const elements = @as([*]const Ir.Inst.Ref, @ptrCast(b.ir.extras[extra_index..]))[0..array_data.len];

    try b.emit(.{ .tag = .array_create, .data = .{
        .reg_u32 = .{ dest, array_data.len },
    } });

    for (elements, 0..) |elem, i| {
        if (elem == .none) continue; // Skip elisions
        const elem_reg = b.resolve(elem);
        try b.emit(.{ .tag = .array_set, .data = .{
            .reg_reg_u32 = .{ dest, elem_reg, @intCast(i) },
        } });
    }
}

fn lowerObject(b: *Builder, object_data: @FieldType(Ir.Inst.Data, "object"), dest: Bytecode.Inst.Reg) Error!void {
    const extra_index = @intFromEnum(object_data.extra_index);
    const pairs = @as([*]const Ir.Inst.Ref, @ptrCast(b.ir.extras[extra_index..]))[0 .. object_data.len * 2];

    try b.emit(.{
        .tag = .object_create,
        .data = .{ .reg = dest },
    });

    var i: usize = 0;
    while (i < pairs.len) : (i += 2) {
        const key_ref = pairs[i];
        const value_ref = pairs[i + 1];

        const value_reg = b.resolve(value_ref);

        const key_index = key_ref.toIndex().?;
        const key_tag = b.ir.instructions.items(.tag)[@intFromEnum(key_index)];
        const key_data = b.ir.instructions.items(.data)[@intFromEnum(key_index)];

        if (key_tag == .string) {
            const string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(key_data.string));
            try b.emit(.{
                .tag = .object_set,
                .data = .{ .reg_string_reg = .{
                    dest,
                    string_index,
                    value_reg,
                } },
            });
        } else {
            const key_reg = b.resolve(key_ref);
            try b.emit(.{
                .tag = .object_set_computed,
                .data = .{ .reg_reg_reg = .{
                    dest,
                    key_reg,
                    value_reg,
                } },
            });
        }
    }
}

fn lowerIf(b: *Builder, data: @FieldType(Ir.Inst.Data, "if"), dest: Bytecode.Inst.Reg) Error!void {
    const test_ref = data.@"test";
    const then_ref = data.then;
    const else_ref = data.@"else";

    const cond_reg = b.resolve(test_ref);
    const then_block = try b.createBlock();
    const else_block = try b.createBlock();
    const merge_block = try b.createBlock();

    b.branch(.truthy, cond_reg, then_block, else_block);

    b.switchToBlock(then_block);
    try b.emitMoveIfNeeded(then_ref, dest);
    b.jump(merge_block);

    b.switchToBlock(else_block);
    try b.emitMoveIfNeeded(else_ref, dest);
    b.jump(merge_block);

    b.switchToBlock(merge_block);
}

fn lowerWhile(b: *Builder, data: @FieldType(Ir.Inst.Data, "while"), dest: Bytecode.Inst.Reg) Error!void {
    const test_ref = data.@"test";
    const body_ref = data.body;

    const test_block = try b.createBlock();
    const body_block = try b.createBlock();
    const exit_block = try b.createBlock();

    b.jump(test_block);

    b.switchToBlock(test_block);
    const cond_reg = b.resolve(test_ref);
    b.branch(.truthy, cond_reg, body_block, exit_block);

    b.switchToBlock(body_block);
    try b.emitMoveIfNeeded(body_ref, dest);
    b.jump(test_block);

    b.switchToBlock(exit_block);
}

fn lowerFor(b: *Builder, data: @FieldType(Ir.Inst.Data, "for"), dest: Bytecode.Inst.Reg) Error!void {
    const test_ref = data.@"test";
    const update_ref = data.update;
    const body_ref = data.body;

    const test_block = try b.createBlock();
    const body_block = try b.createBlock();
    const update_block = try b.createBlock();
    const exit_block = try b.createBlock();

    b.jump(test_block);

    b.switchToBlock(test_block);
    const cond_reg = b.resolve(test_ref);
    b.branch(.truthy, cond_reg, body_block, exit_block);

    b.switchToBlock(body_block);
    try b.emitMoveIfNeeded(body_ref, dest);
    b.jump(update_block);

    b.switchToBlock(update_block);
    if (update_ref != .none) {
        try b.emitMoveIfNeeded(update_ref, dest);
    }
    b.jump(test_block);

    b.switchToBlock(exit_block);
}

fn lowerLoop(b: *Builder, data: @FieldType(Ir.Inst.Data, "loop"), dest: Bytecode.Inst.Reg) Error!void {
    const body_ref = data.body;
    const update_ref = data.update;

    if (update_ref == .none) {
        const body_block = try b.createBlock();
        const exit_block = try b.createBlock();

        b.jump(body_block);

        b.switchToBlock(body_block);
        try b.emitMoveIfNeeded(body_ref, dest);
        b.jump(body_block);

        b.switchToBlock(exit_block);
    } else {
        const body_block = try b.createBlock();
        const update_block = try b.createBlock();
        const exit_block = try b.createBlock();

        b.jump(body_block);

        b.switchToBlock(body_block);
        try b.emitMoveIfNeeded(body_ref, dest);
        b.jump(update_block);

        b.switchToBlock(update_block);
        try b.emitMoveIfNeeded(update_ref, dest);
        b.jump(body_block);

        b.switchToBlock(exit_block);
    }
}

fn lowerAdd(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .add, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerSub(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .sub, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerMul(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .mul, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerDiv(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .div, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerRem(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .rem, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerExp(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .exp, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerShiftLeft(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .shift_left, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerShiftRight(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .shift_right, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerShiftRightUnsigned(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .shift_right_unsigned, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerBitwiseAnd(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .bitwise_and, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerBitwiseOr(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .bitwise_or, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerBitwiseXor(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .bitwise_xor, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerLt(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .lt, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerGt(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .gt, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerLtEq(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .lt_eq, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerGtEq(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .gt_eq, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerInstanceof(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .instanceof, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerIn(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .in, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerEq(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .eq, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerNotEq(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .not_eq, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerEqStrict(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .eq_strict, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerNotEqStrict(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);
    const rhs_reg = b.resolve(data.rhs);
    try b.emit(.{ .tag = .not_eq_strict, .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } } });
}

fn lowerLogicalAnd(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);

    const rhs_block = try b.createBlock();
    const merge_block = try b.createBlock();

    try b.emitMoveIfNeeded(data.lhs, dest);
    b.branch(.falsy, lhs_reg, merge_block, rhs_block);

    b.switchToBlock(rhs_block);
    try b.emitMoveIfNeeded(data.rhs, dest);
    b.jump(merge_block);

    b.switchToBlock(merge_block);
}

fn lowerLogicalOr(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);

    const rhs_block = try b.createBlock();
    const merge_block = try b.createBlock();

    try b.emitMoveIfNeeded(data.lhs, dest);
    b.branch(.truthy, lhs_reg, merge_block, rhs_block);

    b.switchToBlock(rhs_block);
    try b.emitMoveIfNeeded(data.rhs, dest);
    b.jump(merge_block);

    b.switchToBlock(merge_block);
}

fn lowerNullishCoalesce(b: *Builder, data: @FieldType(Ir.Inst.Data, "binary"), dest: Bytecode.Inst.Reg) Error!void {
    const lhs_reg = b.resolve(data.lhs);

    const rhs_block = try b.createBlock();
    const merge_block = try b.createBlock();

    try b.emitMoveIfNeeded(data.lhs, dest);
    b.branch(.nullish, lhs_reg, rhs_block, merge_block);

    b.switchToBlock(rhs_block);
    try b.emitMoveIfNeeded(data.rhs, dest);
    b.jump(merge_block);

    b.switchToBlock(merge_block);
}

fn lowerUnaryPlus(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src_reg = b.resolve(ref);
    try b.emit(.{ .tag = .to_number, .data = .{ .reg_reg = .{ dest, src_reg } } });
}

fn lowerUnaryMinus(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src_reg = b.resolve(ref);
    try b.emit(.{ .tag = .unary_minus, .data = .{ .reg_reg = .{ dest, src_reg } } });
}

fn lowerBitwiseNot(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src_reg = b.resolve(ref);
    try b.emit(.{ .tag = .bitwise_not, .data = .{ .reg_reg = .{ dest, src_reg } } });
}

fn lowerLogicalNot(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src_reg = b.resolve(ref);
    try b.emit(.{ .tag = .logical_not, .data = .{ .reg_reg = .{ dest, src_reg } } });
}

fn lowerTypeof(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src_reg = b.resolve(ref);
    try b.emit(.{ .tag = .typeof, .data = .{ .reg_reg = .{ dest, src_reg } } });
}

fn lowerVoid(b: *Builder, _: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{ .tag = .load_undefined, .data = .{ .reg = dest } });
}

fn lowerGetBinding(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    try b.emit(.{
        .tag = .get_binding,
        .data = .{ .reg_string = .{
            dest,
            bytecode_string_index,
        } },
    });
}

fn lowerSetBinding(b: *Builder, string_index: Ir.Inst.StringIndex, value: Ir.Inst.Ref, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    const value_reg = b.resolve(value);
    const tag: Bytecode.Inst.Tag = if (strict) .set_binding_strict else .set_binding;
    try b.emit(.{
        .tag = tag,
        .data = .{ .string_reg = .{
            bytecode_string_index,
            value_reg,
        } },
    });
    try b.emitMoveIfNeeded(value, dest);
}

fn lowerEnd(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    _ = dest;
    const ret_reg = if (ref == .none) Bytecode.Inst.Reg.none else b.resolve(ref);
    try b.emit(.{ .tag = .end, .data = .{ .reg = ret_reg } });
    b.noreturn();
}
