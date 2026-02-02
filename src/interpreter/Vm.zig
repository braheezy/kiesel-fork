const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const interpreter = @import("../interpreter.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const BigInt = types.BigInt;
const Bytecode = interpreter.Bytecode;
const Number = types.Number;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;

const applyStringOrNumericBinaryOperator = language.runtime.applyStringOrNumericBinaryOperator;
const arrayCreateFast = builtins.arrayCreateFast;
const isLessThan = types.isLessThan;
const isLooselyEqual = types.isLooselyEqual;
const isStrictlyEqual = types.isStrictlyEqual;
const ordinaryObjectCreateFast = builtins.ordinaryObjectCreateFast;
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
                .array_create => vm.executeCreateArray(data.reg_u32[0], data.reg_u32[1]),
                .array_push => vm.executeArrayPush(data.reg_reg[0], data.reg_reg[1]),
                .array_set => vm.executeArraySet(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2]),
                .object_create => vm.executeObjectCreate(data.reg),
                .object_set => vm.executeObjectSet(data.reg_string_reg[0], data.reg_string_reg[1], data.reg_string_reg[2]),
                .object_set_computed => vm.executeObjectSetComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .to_number => vm.executeToNumber(data.reg_reg[0], data.reg_reg[1]),
                .unary_minus => vm.executeUnaryMinus(data.reg_reg[0], data.reg_reg[1]),
                .bitwise_not => vm.executeBitwiseNot(data.reg_reg[0], data.reg_reg[1]),
                .logical_not => vm.executeLogicalNot(data.reg_reg[0], data.reg_reg[1]),
                .typeof => vm.executeTypeof(data.reg_reg[0], data.reg_reg[1]),
                .add => vm.executeAdd(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .sub => vm.executeSub(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .mul => vm.executeMul(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .div => vm.executeDiv(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .rem => vm.executeRem(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .exp => vm.executeExp(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .shift_left => vm.executeShiftLeft(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .shift_right => vm.executeShiftRight(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .shift_right_unsigned => vm.executeShiftRightUnsigned(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .bitwise_and => vm.executeBitwiseAnd(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .bitwise_or => vm.executeBitwiseOr(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .bitwise_xor => vm.executeBitwiseXor(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .lt => vm.executeLt(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .gt => vm.executeGt(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .lt_eq => vm.executeLtEq(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .gt_eq => vm.executeGtEq(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .instanceof => vm.executeInstanceOf(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .in => vm.executeIn(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .eq => vm.executeEq(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .not_eq => vm.executeNotEq(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .eq_strict => vm.executeEqStrict(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .not_eq_strict => vm.executeNotEqStrict(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .get_binding => vm.executeGetBinding(data.reg_string[0], data.reg_string[1]),
                .set_binding => vm.executeSetBinding(data.string_reg[0], data.string_reg[1], false),
                .set_binding_strict => vm.executeSetBinding(data.string_reg[0], data.string_reg[1], true),
                .delete_binding => vm.executeDeleteBinding(data.reg_string[0], data.reg_string[1]),
                .increment_binding_prefix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .prefix, false),
                .increment_binding_prefix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .prefix, true),
                .increment_binding_postfix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .postfix, false),
                .increment_binding_postfix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .postfix, true),
                .decrement_binding_prefix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .prefix, false),
                .decrement_binding_prefix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .prefix, true),
                .decrement_binding_postfix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .postfix, false),
                .decrement_binding_postfix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .postfix, true),
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

fn getString(vm: *Vm, index: Bytecode.Inst.StringIndex) *const String {
    return vm.strings[@intFromEnum(index)];
}

fn getBigInt(vm: *Vm, index: Bytecode.Inst.BigIntIndex) *const BigInt {
    return vm.big_ints[@intFromEnum(index)];
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
    const string = vm.getString(index);
    vm.load(reg, Value.from(string));
}

fn executeLoadBigInt(vm: *Vm, reg: Bytecode.Inst.Reg, index: Bytecode.Inst.BigIntIndex) void {
    const big_int = vm.getBigInt(index);
    vm.load(reg, Value.from(big_int));
}

fn executeMove(vm: *Vm, dest: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) void {
    vm.load(dest, vm.store(src));
}

fn executeCreateArray(vm: *Vm, dst: Bytecode.Inst.Reg, capacity: u32) Agent.Error!void {
    const array = try arrayCreateFast(vm.agent, capacity);
    vm.load(dst, Value.from(&array.object));
}

fn executeArrayPush(vm: *Vm, array_reg: Bytecode.Inst.Reg, elem_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const array_value = vm.store(array_reg);
    const elem_value = vm.store(elem_reg);
    const array = array_value.asObject().as(builtins.Array);
    const index = array.fields.length;
    try array.object.property_storage.indexed_properties.set(vm.agent.gc_allocator, index, .{
        .value_or_accessor = .{ .value = elem_value },
        .attributes = .all,
    });
}

fn executeArraySet(vm: *Vm, array_reg: Bytecode.Inst.Reg, elem_reg: Bytecode.Inst.Reg, index: u32) Agent.Error!void {
    const array_value = vm.store(array_reg);
    const elem_value = vm.store(elem_reg);
    const array = array_value.asObject().as(builtins.Array);
    try array.object.property_storage.indexed_properties.set(vm.agent.gc_allocator, index, .{
        .value_or_accessor = .{ .value = elem_value },
        .attributes = .all,
    });
}

fn executeObjectCreate(vm: *Vm, dst: Bytecode.Inst.Reg) Agent.Error!void {
    const object = try ordinaryObjectCreateFast(vm.agent);
    vm.load(dst, Value.from(object));
}

fn executeObjectSet(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_index: Bytecode.Inst.StringIndex, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object_value = vm.store(object_reg);
    const property_value = vm.store(value_reg);
    const object = object_value.asObject();
    const property_key = PropertyKey.from(vm.strings[@intFromEnum(key_index)]);
    try object.createDataPropertyDirect(vm.agent, property_key, property_value);
}

fn executeObjectSetComputed(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_reg: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object_value = vm.store(object_reg);
    const key_value = vm.store(key_reg);
    const property_value = vm.store(value_reg);
    const object = object_value.asObject();
    const property_key = try key_value.toPropertyKey(vm.agent);
    try object.createDataPropertyDirect(vm.agent, property_key, property_value);
}

fn executeToNumber(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);

    // OPTIMIZATION: Fast path for number values
    if (value.isNumber()) {
        @branchHint(.likely);
        vm.load(dst, value);
        return;
    }

    const number = try value.toNumber(vm.agent);
    vm.load(dst, Value.from(number));
}

fn executeUnaryMinus(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);

    // OPTIMIZATION: Fast path for number values
    if (value.isNumber()) {
        @branchHint(.likely);
        if (value.__isI32()) {
            const i = value.__asI32();
            if (i != 0 and i != std.math.minInt(i32)) {
                vm.load(dst, Value.from(-i));
                return;
            }
        }
        vm.load(dst, Value.from(-value.__toF64()));
        return;
    }

    const numeric = try value.toNumeric(vm.agent);
    vm.load(dst, switch (numeric) {
        .number => |n| Value.from(n.unaryMinus()),
        .big_int => |b| Value.from(try b.unaryMinus(vm.agent)),
    });
}

fn executeBitwiseNot(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);
    const i = try value.toInt32(vm.agent);
    vm.load(dst, Value.from(~i));
}

fn executeLogicalNot(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) void {
    const value = vm.store(src);
    vm.load(dst, Value.from(!value.toBoolean()));
}

fn executeTypeof(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) void {
    const value = vm.store(src);
    vm.load(dst, Value.from(value.typeof()));
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

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"+", rhs_value);
    vm.load(dst, result);
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

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"-", rhs_value);
    vm.load(dst, result);
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

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"*", rhs_value);
    vm.load(dst, result);
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

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"/", rhs_value);
    vm.load(dst, result);
}

fn executeRem(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            if (std.math.rem(i32, lhs_value.__asI32(), rhs_value.__asI32())) |result| {
                vm.load(dst, if (result == 0 and lhs_value.__asI32() < 0)
                    Value.from(-0.0)
                else
                    Value.from(result));
                return;
            } else |_| {}
        }
        vm.load(dst, Value.from(lhs_value.asNumber().remainder(rhs_value.asNumber())));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"%", rhs_value);
    vm.load(dst, result);
}

fn executeExp(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            if (std.math.powi(i32, lhs_value.__asI32(), rhs_value.__asI32())) |result| {
                vm.load(dst, Value.from(result));
                return;
            } else |_| {}
        }
        vm.load(dst, Value.from(lhs_value.asNumber().exponentiate(rhs_value.asNumber())));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"**", rhs_value);
    vm.load(dst, result);
}

fn executeShiftLeft(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for i32 values
    if (lhs_value.__isI32() and rhs_value.__isI32()) {
        @branchHint(.likely);
        const shift_count: u5 = @intCast(@mod(@as(u32, @bitCast(rhs_value.__asI32())), 32));
        vm.load(dst, Value.from(lhs_value.__asI32() << shift_count));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"<<", rhs_value);
    vm.load(dst, result);
}

fn executeShiftRight(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for i32 values
    if (lhs_value.__isI32() and rhs_value.__isI32()) {
        @branchHint(.likely);
        const shift_count: u5 = @intCast(@mod(@as(u32, @bitCast(rhs_value.__asI32())), 32));
        vm.load(dst, Value.from(lhs_value.__asI32() >> shift_count));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@">>", rhs_value);
    vm.load(dst, result);
}

fn executeShiftRightUnsigned(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for i32 values
    if (lhs_value.__isI32() and rhs_value.__isI32()) {
        @branchHint(.likely);
        const shift_count: u5 = @intCast(@mod(@as(u32, @bitCast(rhs_value.__asI32())), 32));
        vm.load(dst, Value.from(@as(u32, @bitCast(lhs_value.__asI32())) >> shift_count));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@">>>", rhs_value);
    vm.load(dst, result);
}

fn executeBitwiseAnd(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for i32 values
    if (lhs_value.__isI32() and rhs_value.__isI32()) {
        @branchHint(.likely);
        vm.load(dst, Value.from(lhs_value.__asI32() & rhs_value.__asI32()));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"&", rhs_value);
    vm.load(dst, result);
}

fn executeBitwiseOr(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for i32 values
    if (lhs_value.__isI32() and rhs_value.__isI32()) {
        @branchHint(.likely);
        vm.load(dst, Value.from(lhs_value.__asI32() | rhs_value.__asI32()));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"|", rhs_value);
    vm.load(dst, result);
}

fn executeBitwiseXor(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for i32 values
    if (lhs_value.__isI32() and rhs_value.__isI32()) {
        @branchHint(.likely);
        vm.load(dst, Value.from(lhs_value.__asI32() ^ rhs_value.__asI32()));
        return;
    }

    const result = try applyStringOrNumericBinaryOperator(vm.agent, lhs_value, .@"^", rhs_value);
    vm.load(dst, result);
}

fn executeLt(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() < rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() < rhs_value.__toF64()));
        }
        return;
    }

    const result = try isLessThan(vm.agent, lhs_value, rhs_value, .left_first);
    vm.load(dst, Value.from(result orelse false));
}

fn executeGt(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() > rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() > rhs_value.__toF64()));
        }
        return;
    }

    const result = try isLessThan(vm.agent, rhs_value, lhs_value, .right_first);
    vm.load(dst, Value.from(result orelse false));
}

fn executeLtEq(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() <= rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() <= rhs_value.__toF64()));
        }
        return;
    }

    const result = try isLessThan(vm.agent, rhs_value, lhs_value, .right_first);
    vm.load(dst, Value.from(!(result orelse true)));
}

fn executeGtEq(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    // OPTIMIZATION: Fast path for number values
    if (lhs_value.isNumber() and rhs_value.isNumber()) {
        @branchHint(.likely);
        if (lhs_value.__isI32() and rhs_value.__isI32()) {
            vm.load(dst, Value.from(lhs_value.__asI32() >= rhs_value.__asI32()));
        } else {
            vm.load(dst, Value.from(lhs_value.__toF64() >= rhs_value.__toF64()));
        }
        return;
    }

    const result = try isLessThan(vm.agent, lhs_value, rhs_value, .left_first);
    vm.load(dst, Value.from(!(result orelse true)));
}

fn executeInstanceOf(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    const result = try lhs_value.instanceofOperator(vm.agent, rhs_value);
    vm.load(dst, Value.from(result));
}

fn executeIn(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) Agent.Error!void {
    const lhs_value = vm.store(lhs);
    const rhs_value = vm.store(rhs);

    if (!rhs_value.isObject()) {
        @branchHint(.unlikely);
        return vm.agent.throwException(
            .type_error,
            "Right-hand side of 'in' operator must be an object",
            .{},
        );
    }

    const result = try rhs_value.asObject().hasProperty(
        vm.agent,
        try lhs_value.toPropertyKey(vm.agent),
    );
    vm.load(dst, Value.from(result));
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

    const result = try isLooselyEqual(vm.agent, rhs_value, lhs_value);
    vm.load(dst, Value.from(result));
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

    const result = !try isLooselyEqual(vm.agent, rhs_value, lhs_value);
    vm.load(dst, Value.from(result));
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

    const result = isStrictlyEqual(lhs_value, rhs_value);
    vm.load(dst, Value.from(result));
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

    const result = !isStrictlyEqual(lhs_value, rhs_value);
    vm.load(dst, Value.from(result));
}

fn executeGetBinding(vm: *Vm, dst: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);

    var env = vm.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    while (!try env.hasBinding(vm.agent, name)) {
        env = env.outerEnv() orelse {
            @branchHint(.unlikely);
            return vm.agent.throwException(
                .reference_error,
                "'{f}' is not defined",
                .{name.fmtRaw()},
            );
        };
    }

    const result = try env.getBindingValue(vm.agent, name, true);
    vm.load(dst, result);
}

fn executeSetBinding(
    vm: *Vm,
    name_index: Bytecode.Inst.StringIndex,
    value_reg: Bytecode.Inst.Reg,
    comptime strict: bool,
) Agent.Error!void {
    const name = vm.getString(name_index);
    const value = vm.store(value_reg);

    var env = vm.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    while (!try env.hasBinding(vm.agent, name)) {
        env = env.outerEnv() orelse {
            @branchHint(.unlikely);
            if (strict) {
                return vm.agent.throwException(
                    .reference_error,
                    "'{f}' is not defined",
                    .{name.fmtRaw()},
                );
            }
            const global_obj = vm.agent.getGlobalObject();
            try global_obj.set(vm.agent, PropertyKey.from(name), value, .ignore);
            return;
        };
    }

    try env.setMutableBinding(vm.agent, name, value, strict);
}

fn executeDeleteBinding(vm: *Vm, dst: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);

    var env = vm.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    while (!try env.hasBinding(vm.agent, name)) {
        env = env.outerEnv() orelse {
            @branchHint(.unlikely);
            vm.load(dst, .true);
            return;
        };
    }

    const result = try env.deleteBinding(vm.agent, name);
    vm.load(dst, Value.from(result));
}

const UpdateOp = enum { increment, decrement };
const UpdateType = enum { prefix, postfix };

fn executeUpdateBinding(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    name_index: Bytecode.Inst.StringIndex,
    comptime op: UpdateOp,
    comptime update_type: UpdateType,
    comptime strict: bool,
) Agent.Error!void {
    const name = vm.getString(name_index);

    var env = vm.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    while (!try env.hasBinding(vm.agent, name)) {
        env = env.outerEnv() orelse {
            @branchHint(.unlikely);
            return vm.agent.throwException(
                .reference_error,
                "'{f}' is not defined",
                .{name.fmtRaw()},
            );
        };
    }

    const old_value = try env.getBindingValue(vm.agent, name, strict);
    const old_numeric = try old_value.toNumeric(vm.agent);

    const new_value = switch (old_numeric) {
        .number => |n| switch (op) {
            .increment => Value.from(Number.add(n, Number.from(1))),
            .decrement => Value.from(Number.subtract(n, Number.from(1))),
        },
        .big_int => |b| switch (op) {
            .increment => Value.from(try BigInt.add(b, vm.agent, .one)),
            .decrement => Value.from(try BigInt.subtract(b, vm.agent, .one)),
        },
    };

    try env.setMutableBinding(vm.agent, name, new_value, strict);

    const result = switch (update_type) {
        .prefix => new_value,
        .postfix => switch (old_numeric) {
            .number => |n| Value.from(n),
            .big_int => |b| Value.from(b),
        },
    };
    vm.load(dest, result);
}
