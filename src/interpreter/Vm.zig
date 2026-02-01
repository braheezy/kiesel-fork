const std = @import("std");

const execution = @import("../execution.zig");
const interpreter = @import("../interpreter.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const BigInt = types.BigInt;
const Bytecode = interpreter.Bytecode;
const String = types.String;
const Value = types.Value;

const applyStringOrNumericBinaryOperator = language.runtime.applyStringOrNumericBinaryOperator;
const isLooselyEqual = types.isLooselyEqual;
const isStrictlyEqual = types.isStrictlyEqual;
const stringValueImpl = language.ast.stringValueImpl;

const Vm = @This();

agent: *Agent,
bytecode: *const Bytecode,
strings: []const *const String,
big_ints: []const *const BigInt,
regs: [num_regs]Value,

pub const num_regs = 32;

pub fn init(
    gpa: std.mem.Allocator,
    agent: *Agent,
    bytecode: *const Bytecode,
) std.mem.Allocator.Error!Vm {
    const strings = try gpa.alloc(*const String, bytecode.strings.len);
    errdefer gpa.free(strings);
    for (bytecode.strings, 0..) |utf8, i| {
        strings[i] = try stringValueImpl(agent.gc_allocator, utf8);
    }

    const big_ints = try gpa.alloc(*const BigInt, bytecode.big_ints.len);
    errdefer gpa.free(big_ints);
    for (bytecode.big_ints, 0..) |@"const", i| {
        const managed = try @"const".toManaged(agent.gc_allocator);
        big_ints[i] = try BigInt.fromManaged(agent, managed);
    }

    return .{
        .agent = agent,
        .bytecode = bytecode,
        .strings = strings,
        .big_ints = big_ints,
        // Not initialized to catch invalid stores more easily
        .regs = undefined,
    };
}

pub fn deinit(vm: *Vm, gpa: std.mem.Allocator) void {
    // Values are GC-allocated, only free the arrays
    gpa.free(vm.strings);
    gpa.free(vm.big_ints);
}

pub fn run(vm: *Vm) Agent.Error!?Value {
    var reader: std.Io.Reader = .fixed(vm.bytecode.code);
    const pc = &reader.seek;

    loop: switch (Bytecode.Inst.decodeTag(&reader) catch unreachable) {
        inline else => |tag| {
            const data = Bytecode.Inst.decodeData(&reader, tag) catch unreachable;
            const maybe_error = switch (tag) {
                .jump => vm.executeJump(data.i32, pc),
                .jump_if_true => vm.executeJumpIfTrue(data.reg_i32[0], data.reg_i32[1], pc),
                .jump_if_false => vm.executeJumpIfFalse(data.reg_i32[0], data.reg_i32[1], pc),
                .jump_if_nullish => vm.executeJumpIfNullish(data.reg_i32[0], data.reg_i32[1], pc),
                .load_undefined => vm.executeLoadUndefined(data.reg),
                .load_null => vm.executeLoadNull(data.reg),
                .load_true => vm.executeLoadTrue(data.reg),
                .load_false => vm.executeLoadFalse(data.reg),
                .load_number_i32 => vm.executeLoadNumberI32(data.reg_i32[0], data.reg_i32[1]),
                .load_number_f64 => vm.executeLoadNumberF64(data.reg_f64[0], data.reg_f64[1]),
                .load_string => vm.executeLoadString(data.reg_string[0], data.reg_string[1]),
                .load_big_int => vm.executeLoadBigInt(data.reg_big_int[0], data.reg_big_int[1]),
                .move => vm.executeMove(data.reg_reg[0], data.reg_reg[1]),
                .add => vm.executeAdd(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .sub => vm.executeSub(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .mul => vm.executeMul(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .div => vm.executeDiv(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .eq => vm.executeEq(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .not_eq => vm.executeNotEq(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .eq_strict => vm.executeEqStrict(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .not_eq_strict => vm.executeNotEqStrict(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .end => return if (data.reg != .none) vm.store(data.reg) else null,
            };
            switch (@typeInfo(@TypeOf(maybe_error))) {
                .void => {},
                .error_union => |u| {
                    comptime std.debug.assert(u.payload == void);
                    // TODO: Exception handling
                    try maybe_error;
                },
                else => comptime unreachable,
            }
            continue :loop Bytecode.Inst.decodeTag(&reader) catch unreachable;
        },
    }
}

fn store(vm: *Vm, reg: Bytecode.Inst.Reg) Value {
    std.debug.assert(reg != .none);
    return vm.regs[@intFromEnum(reg)];
}

fn load(vm: *Vm, reg: Bytecode.Inst.Reg, value: Value) void {
    std.debug.assert(reg != .none);
    vm.regs[@intFromEnum(reg)] = value;
}

fn jump(_: *Vm, offset: i32, pc: *usize) void {
    if (offset >= 0) {
        pc.* += @intCast(offset);
    } else {
        pc.* -= @intCast(-offset);
    }
}

fn executeJump(vm: *Vm, offset: i32, pc: *usize) void {
    vm.jump(offset, pc);
}

fn executeJumpIfTrue(vm: *Vm, reg: Bytecode.Inst.Reg, offset: i32, pc: *usize) void {
    if (vm.store(reg).toBoolean()) {
        vm.jump(offset, pc);
    }
}

fn executeJumpIfFalse(vm: *Vm, reg: Bytecode.Inst.Reg, offset: i32, pc: *usize) void {
    if (!vm.store(reg).toBoolean()) {
        vm.jump(offset, pc);
    }
}

fn executeJumpIfNullish(vm: *Vm, reg: Bytecode.Inst.Reg, offset: i32, pc: *usize) void {
    const value = vm.store(reg);
    if (value.isUndefined() or value.isNull()) {
        vm.jump(offset, pc);
    }
}

fn executeLoadUndefined(vm: *Vm, reg: Bytecode.Inst.Reg) void {
    vm.load(reg, .undefined);
}

fn executeLoadNull(vm: *Vm, reg: Bytecode.Inst.Reg) void {
    vm.load(reg, .null);
}

fn executeLoadTrue(vm: *Vm, reg: Bytecode.Inst.Reg) void {
    vm.load(reg, .true);
}

fn executeLoadFalse(vm: *Vm, reg: Bytecode.Inst.Reg) void {
    vm.load(reg, .false);
}

fn executeLoadNumberI32(vm: *Vm, reg: Bytecode.Inst.Reg, value: i32) void {
    vm.load(reg, Value.from(value));
}

fn executeLoadNumberF64(vm: *Vm, reg: Bytecode.Inst.Reg, value: f64) void {
    vm.load(reg, Value.from(value));
}

fn executeLoadString(vm: *Vm, reg: Bytecode.Inst.Reg, index: Bytecode.Inst.StringIndex) void {
    vm.load(reg, Value.from(vm.strings[@intFromEnum(index)]));
}

fn executeLoadBigInt(vm: *Vm, reg: Bytecode.Inst.Reg, index: Bytecode.Inst.BigIntIndex) void {
    vm.load(reg, Value.from(vm.big_ints[@intFromEnum(index)]));
}

fn executeMove(vm: *Vm, dest: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) void {
    vm.load(dest, vm.store(src));
}

fn executeAdd(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            if (std.math.add(i32, lhs_value.__asI32(), rhs_value.__asI32())) |result| {
                vm.load(dst, Value.from(result));
                return;
            } else |_| {}
        }
        vm.load(dst, Value.from(lhs_value.__toF64() + rhs_value.__toF64()));
        return;
    }

    // OPTIMIZATION: Fast path for string values
    if (lhs_value.isString() and rhs_value.isString()) {
        vm.load(dst, Value.from(
            try String.concat(vm.agent, &.{ lhs_value.asString(), rhs_value.asString() }),
        ));
        return;
    }

    vm.load(dst, try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"+", rhs_value));
}

fn executeSub(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            if (std.math.sub(i32, lhs_value.__asI32(), rhs_value.__asI32())) |result| {
                vm.load(dst, Value.from(result));
                return;
            } else |_| {}
        }
        vm.load(dst, Value.from(lhs_value.__toF64() - rhs_value.__toF64()));
        return;
    }

    vm.load(dst, try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"-", rhs_value));
}

fn executeMul(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            if (std.math.mul(i32, lhs_value.__asI32(), rhs_value.__asI32())) |result| {
                vm.load(dst, Value.from(result));
                return;
            } else |_| {}
        }
        vm.load(dst, Value.from(lhs_value.__toF64() * rhs_value.__toF64()));
        return;
    }

    vm.load(dst, try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"*", rhs_value));
}

fn executeDiv(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        vm.load(dst, Value.from(lhs_value.__toF64() / rhs_value.__toF64()));
        return;
    }

    vm.load(dst, try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"/", rhs_value));
}

fn executeEq(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() == rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() == rhs_value.__toF64()));
        }
        return;
    }

    vm.load(dst, Value.from(try isLooselyEqual(vm.agent, rhs_value, lhs_value)));
}

fn executeNotEq(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() != rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() != rhs_value.__toF64()));
        }
        return;
    }

    vm.load(dst, Value.from(!try isLooselyEqual(vm.agent, rhs_value, lhs_value)));
}

fn executeEqStrict(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() == rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() == rhs_value.__toF64()));
        }
        return;
    }

    vm.load(dst, Value.from(isStrictlyEqual(lhs_value, rhs_value)));
}

fn executeNotEqStrict(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() != rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() != rhs_value.__toF64()));
        }
        return;
    }

    vm.load(dst, Value.from(!isStrictlyEqual(lhs_value, rhs_value)));
}
