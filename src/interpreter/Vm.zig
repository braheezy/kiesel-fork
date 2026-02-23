const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const interpreter = @import("../interpreter.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const BigInt = types.BigInt;
const Bytecode = interpreter.Bytecode;
const Iterator = types.Iterator;
const Number = types.Number;
const PrivateName = types.PrivateName;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;

const applyStringOrNumericBinaryOperator = language.runtime.applyStringOrNumericBinaryOperator;
const arrayCreateFast = builtins.arrayCreateFast;
const await = builtins.await;
const classDefinitionEvaluation = language.runtime.classDefinitionEvaluation;
const createArrayFromList = types.createArrayFromList;
const createForInIterator = builtins.createForInIterator;
const createMappedArgumentsObject = builtins.createMappedArgumentsObject;
const createUnmappedArgumentsObject = builtins.createUnmappedArgumentsObject;
const directEval = language.runtime.directEval;
const evaluateCall = language.runtime.evaluateCall;
const evaluateImportCall = language.runtime.evaluateImportCall;
const evaluateImportMeta = language.runtime.evaluateImportMeta;
const evaluateNew = language.runtime.evaluateNew;
const evaluateSuperCall = language.runtime.evaluateSuperCall;
const getIterator = types.getIterator;
const getIteratorDirect = types.getIteratorDirect;
const instantiateArrowFunctionExpression = language.runtime.instantiateArrowFunctionExpression;
const instantiateAsyncArrowFunctionExpression = language.runtime.instantiateAsyncArrowFunctionExpression;
const instantiateAsyncFunctionExpression = language.runtime.instantiateAsyncFunctionExpression;
const instantiateAsyncGeneratorFunctionExpression = language.runtime.instantiateAsyncGeneratorFunctionExpression;
const instantiateGeneratorFunctionExpression = language.runtime.instantiateGeneratorFunctionExpression;
const instantiateOrdinaryFunctionExpression = language.runtime.instantiateOrdinaryFunctionExpression;
const isLessThan = types.isLessThan;
const isLooselyEqual = types.isLooselyEqual;
const isStrictlyEqual = types.isStrictlyEqual;
const makeMethod = builtins.makeMethod;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const newObjectEnvironment = execution.newObjectEnvironment;
const newPrivateEnvironment = execution.newPrivateEnvironment;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const ordinaryObjectCreateFast = builtins.ordinaryObjectCreateFast;
const stringValueImpl = ast.stringValueImpl;
const yield = builtins.yield;

const Vm = @This();

agent: *Agent,
stack: std.ArrayList(Value),
call_stack: std.ArrayList(CallFrame),
constants_cache: std.AutoHashMapUnmanaged(*const Bytecode, Constants),

pub const Pc = enum(u32) {
    start = 0,
    _,

    pub fn offsetBy(pc: Pc, offset: i32) Pc {
        if (offset >= 0) {
            return @enumFromInt(@intFromEnum(pc) + @as(u32, @intCast(offset)));
        } else {
            return @enumFromInt(@intFromEnum(pc) - @as(u32, @intCast(-offset)));
        }
    }
};

pub const CallFrame = struct {
    bytecode: *const Bytecode,
    constants: Constants,
    stack_base: u32,
    regs_len: u16,
    arguments_len: u16,
    cached_this_value: ?Value,

    pub fn stackLen(frame: *const CallFrame) usize {
        return frame.regs_len + frame.arguments_len;
    }
};

pub const GeneratorSuspension = struct {
    stack: []Value,
    regs_len: u16,
    arguments_len: u16,
    cached_this_value: ?Value,
    saved_pc: Pc,
    yield_reg: Bytecode.Inst.Reg,
};

pub const RunResult = union(enum) {
    @"return": ?Value,
    yield: GeneratorSuspension,
};

pub const RunOptions = struct {
    start_pc: Pc = .start,
};

const constants_align = @max(@alignOf(String), @alignOf(BigInt));
const Constants = []const *align(constants_align) const anyopaque;

pub fn init(
    agent: *Agent,
    bytecode: *const Bytecode,
) Agent.Error!Vm {
    var vm: Vm = .{
        .agent = agent,
        .stack = .empty,
        .call_stack = .empty,
        .constants_cache = .empty,
    };
    try vm.pushCallFrame(bytecode, &.{});
    return vm;
}

pub fn deinit(vm: *Vm) void {
    const frame = vm.currentCallFrame();
    std.debug.assert(frame.stack_base == 0);
    std.debug.assert(frame.arguments_len == 0);
    std.debug.assert(vm.stack.items.len == frame.stackLen());
    std.debug.assert(vm.call_stack.items.len == 1);
    vm.stack.deinit(vm.agent.gc_allocator);
    vm.call_stack.deinit(vm.agent.gc_allocator);

    var it = vm.constants_cache.iterator();
    while (it.next()) |entry| {
        // Values might outlive the VM and need to be GC'd, but we can free the array
        vm.agent.gc_allocator.free(entry.value_ptr.*);
    }
    vm.constants_cache.deinit(vm.agent.gc_allocator);
}

pub fn run(vm: *Vm, options: RunOptions) Agent.Error!RunResult {
    const previous_vm = vm.agent.active_vm;
    vm.agent.active_vm = vm;
    defer vm.agent.active_vm = previous_vm;

    // Caching the bytecode slice below depends on the call stack depth remaining the same within
    // one `run()` call, so we maintain the invariant that nested calls need their own `run()` call
    // and must not leave call frames behind. This is asserted after each instruction.
    const initial_call_stack_depth = vm.call_stack.items.len;

    const frame = vm.currentCallFrame();
    var code = frame.bytecode.code;
    var pc = options.start_pc;

    loop: switch (Bytecode.Inst.decodeTag(code[@intFromEnum(pc)..])) {
        inline else => |tag| {
            @setEvalBranchQuota(3_000);
            const data = Bytecode.Inst.decodeData(code[@intFromEnum(pc) + 1 ..], tag);
            const inst_size = comptime Bytecode.Inst.encodedSize(tag);
            pc = pc.offsetBy(inst_size);
            const maybe_error = switch (tag) {
                .jump => vm.executeJump(data.i32, &pc),
                .jump_if_true => vm.executeJumpIfTrue(data.reg_i32[0], data.reg_i32[1], &pc),
                .jump_if_false => vm.executeJumpIfFalse(data.reg_i32[0], data.reg_i32[1], &pc),
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
                .array_push_hole => vm.executeArrayPushHole(data.reg),
                .array_set => vm.executeArraySet(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2]),
                .array_spread => vm.executeArraySpread(data.reg_reg[0], data.reg_reg[1]),
                .object_create => vm.executeObjectCreate(data.reg),
                .object_set => vm.executeObjectSet(data.reg_string_reg[0], data.reg_string_reg[1], data.reg_string_reg[2]),
                .object_set_computed => vm.executeObjectSetComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .object_set_getter => vm.executeObjectSetGetter(data.reg_string_reg[0], data.reg_string_reg[1], data.reg_string_reg[2]),
                .object_set_getter_computed => vm.executeObjectSetGetterComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .object_set_setter => vm.executeObjectSetSetter(data.reg_string_reg[0], data.reg_string_reg[1], data.reg_string_reg[2]),
                .object_set_setter_computed => vm.executeObjectSetSetterComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .object_set_prototype => vm.executeObjectSetPrototype(data.reg_reg[0], data.reg_reg[1]),
                .object_spread => vm.executeObjectSpread(data.reg_reg[0], data.reg_reg[1]),
                .reg_exp_create => vm.executeRegExpCreate(data.reg_string_string[0], data.reg_string_string[1], data.reg_string_string[2]),
                .resolve_this_binding => vm.executeResolveThisBinding(data.reg),
                .to_number => vm.executeToNumber(data.reg_reg[0], data.reg_reg[1]),
                .to_numeric => vm.executeToNumeric(data.reg_reg[0], data.reg_reg[1]),
                .to_string => vm.executeToString(data.reg_reg[0], data.reg_reg[1]),
                .to_object => vm.executeToObject(data.reg_reg[0], data.reg_reg[1]),
                .negate => vm.executeNegate(data.reg_reg[0], data.reg_reg[1]),
                .bitwise_not => vm.executeBitwiseNot(data.reg_reg[0], data.reg_reg[1]),
                .logical_not => vm.executeLogicalNot(data.reg_reg[0], data.reg_reg[1]),
                .typeof => vm.executeTypeof(data.reg_reg[0], data.reg_reg[1]),
                .typeof_binding => vm.executeTypeofBinding(data.reg_string[0], data.reg_string[1]),
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
                .push_scope => vm.executePushScope(),
                .push_var_scope => vm.executePushVarScope(),
                .push_with_scope => vm.executePushWithScope(data.reg),
                .pop_scope => vm.executePopScope(),
                .create_mutable_binding => vm.executeCreateMutableBinding(data.string),
                .create_immutable_binding => vm.executeCreateImmutableBinding(data.string),
                .initialize_binding => vm.executeInitializeBinding(data.string_reg[0], data.string_reg[1]),
                .get_binding => vm.executeGetBinding(data.reg_string[0], data.reg_string[1]),
                .get_property => vm.executeGetProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2]),
                .get_property_computed => vm.executeGetPropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .get_property_indexed => vm.executeGetPropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2]),
                .set_binding => vm.executeSetBinding(data.string_reg[0], data.string_reg[1], false),
                .set_binding_strict => vm.executeSetBinding(data.string_reg[0], data.string_reg[1], true),
                .set_property => vm.executeSetProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], false),
                .set_property_strict => vm.executeSetProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], true),
                .set_property_computed => vm.executeSetPropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], false),
                .set_property_computed_strict => vm.executeSetPropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], true),
                .set_property_indexed => vm.executeSetPropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], false),
                .set_property_indexed_strict => vm.executeSetPropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], true),
                .increment_binding_prefix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .prefix, false),
                .increment_binding_prefix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .prefix, true),
                .increment_binding_postfix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .postfix, false),
                .increment_binding_postfix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .increment, .postfix, true),
                .increment_property_prefix => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .increment, .prefix, false),
                .increment_property_prefix_strict => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .increment, .prefix, true),
                .increment_property_postfix => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .increment, .postfix, false),
                .increment_property_postfix_strict => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .increment, .postfix, true),
                .increment_property_computed_prefix => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .increment, .prefix, false),
                .increment_property_computed_prefix_strict => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .increment, .prefix, true),
                .increment_property_computed_postfix => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .increment, .postfix, false),
                .increment_property_computed_postfix_strict => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .increment, .postfix, true),
                .increment_property_indexed_prefix => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .increment, .prefix, false),
                .increment_property_indexed_prefix_strict => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .increment, .prefix, true),
                .increment_property_indexed_postfix => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .increment, .postfix, false),
                .increment_property_indexed_postfix_strict => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .increment, .postfix, true),
                .decrement_binding_prefix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .prefix, false),
                .decrement_binding_prefix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .prefix, true),
                .decrement_binding_postfix => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .postfix, false),
                .decrement_binding_postfix_strict => vm.executeUpdateBinding(data.reg_string[0], data.reg_string[1], .decrement, .postfix, true),
                .decrement_property_prefix => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .decrement, .prefix, false),
                .decrement_property_prefix_strict => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .decrement, .prefix, true),
                .decrement_property_postfix => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .decrement, .postfix, false),
                .decrement_property_postfix_strict => vm.executeUpdateProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], .decrement, .postfix, true),
                .decrement_property_computed_prefix => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .decrement, .prefix, false),
                .decrement_property_computed_prefix_strict => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .decrement, .prefix, true),
                .decrement_property_computed_postfix => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .decrement, .postfix, false),
                .decrement_property_computed_postfix_strict => vm.executeUpdatePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .decrement, .postfix, true),
                .decrement_property_indexed_prefix => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .decrement, .prefix, false),
                .decrement_property_indexed_prefix_strict => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .decrement, .prefix, true),
                .decrement_property_indexed_postfix => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .decrement, .postfix, false),
                .decrement_property_indexed_postfix_strict => vm.executeUpdatePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], .decrement, .postfix, true),
                .delete_binding => vm.executeDeleteBinding(data.reg_string[0], data.reg_string[1]),
                .delete_property => vm.executeDeleteProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], false),
                .delete_property_strict => vm.executeDeleteProperty(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2], true),
                .delete_property_computed => vm.executeDeletePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], false),
                .delete_property_computed_strict => vm.executeDeletePropertyComputed(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], true),
                .delete_property_indexed => vm.executeDeletePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], false),
                .delete_property_indexed_strict => vm.executeDeletePropertyIndexed(data.reg_reg_u32[0], data.reg_reg_u32[1], data.reg_reg_u32[2], true),
                .copy_data_properties => vm.executeCopyDataProperties(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .call => vm.executeCall(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .call0 => vm.executeCallN(0, data.reg_reg[0], data.reg_reg[1], .{}),
                .call1 => vm.executeCallN(1, data.reg_reg_reg[0], data.reg_reg_reg[1], .{data.reg_reg_reg[2]}),
                .call2 => vm.executeCallN(2, data.reg_reg_reg_reg[0], data.reg_reg_reg_reg[1], .{ data.reg_reg_reg_reg[2], data.reg_reg_reg_reg[3] }),
                .call_property => vm.executeCallProperty(data.reg_reg_reg_reg[0], data.reg_reg_reg_reg[1], data.reg_reg_reg_reg[2], data.reg_reg_reg_reg[3]),
                .call_property0 => vm.executeCallPropertyN(0, data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], .{}),
                .call_property1 => vm.executeCallPropertyN(1, data.reg_reg_reg_reg[0], data.reg_reg_reg_reg[1], data.reg_reg_reg_reg[2], .{data.reg_reg_reg_reg[3]}),
                .call_property2 => vm.executeCallPropertyN(2, data.reg_reg_reg_reg_reg[0], data.reg_reg_reg_reg_reg[1], data.reg_reg_reg_reg_reg[2], .{ data.reg_reg_reg_reg_reg[3], data.reg_reg_reg_reg_reg[4] }),
                .call_direct_eval => vm.executeCallDirectEval(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], false),
                .call_direct_eval_strict => vm.executeCallDirectEval(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2], true),
                .construct => vm.executeConstruct(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .construct0 => vm.executeConstructN(0, data.reg_reg[0], data.reg_reg[1], .{}),
                .construct1 => vm.executeConstructN(1, data.reg_reg_reg[0], data.reg_reg_reg[1], .{data.reg_reg_reg[2]}),
                .construct2 => vm.executeConstructN(2, data.reg_reg_reg_reg[0], data.reg_reg_reg_reg[1], .{ data.reg_reg_reg_reg[2], data.reg_reg_reg_reg[3] }),
                .get_template_object => vm.executeGetTemplateObject(data.reg_reg_reg_u16[0], data.reg_reg_reg_u16[1], data.reg_reg_reg_u16[2], data.reg_reg_reg_u16[3]),
                .get_iterator => vm.executeGetIterator(data.reg_reg[0], data.reg_reg[1]),
                .get_async_iterator => vm.executeGetAsyncIterator(data.reg_reg[0], data.reg_reg[1]),
                .get_for_in_iterator => vm.executeGetForInIterator(data.reg_reg[0], data.reg_reg[1]),
                .iterator_step => vm.executeIteratorStep(data.reg_reg[0], data.reg_reg[1]),
                .iterator_step_value => vm.executeIteratorStepValue(data.reg_reg[0], data.reg_reg[1]),
                .iterator_step_value_async => vm.executeIteratorStepValueAsync(data.reg_reg[0], data.reg_reg[1]),
                .iterator_close => vm.executeIteratorClose(data.reg),
                .iterator_is_done => vm.executeIteratorIsDone(data.reg_reg[0], data.reg_reg[1]),
                .iterator_collect => vm.executeIteratorCollect(data.reg_reg[0], data.reg_reg[1]),
                .throw => vm.executeThrow(data.reg),
                .throw_reference_error => vm.executeThrowReferenceError(),
                .@"return" => return vm.executeReturn(data.reg),
                .await => vm.executeAwait(data.reg),
                .yield => return vm.executeYield(data.reg, pc),
                .create_function => vm.executeCreateFunction(data.reg_function[0], data.reg_function[1]),
                .create_class => vm.executeCreateClass(data.reg_class[0], data.reg_class[1]),
                .set_home_object => vm.executeSetHomeObject(data.reg_reg[0], data.reg_reg[1]),
                .create_unmapped_arguments_object => vm.executeCreateUnmappedArgumentsObject(data.reg),
                .create_mapped_arguments_object => vm.executeCreateMappedArgumentsObject(data.reg),
                .get_argument => vm.executeGetArgument(data.reg_u16[0], data.reg_u16[1]),
                .get_rest_arguments => vm.executeGetRestArguments(data.reg_u16[0], data.reg_u16[1]),
                .get_new_target => vm.executeGetNewTarget(data.reg),
                .super_call => vm.executeSuperCall(data.reg_reg[0], data.reg_reg[1]),
                .get_super_property => vm.executeGetSuperProperty(data.reg_string[0], data.reg_string[1]),
                .get_super_property_computed => vm.executeGetSuperPropertyComputed(data.reg_reg[0], data.reg_reg[1]),
                .set_super_property => vm.executeSetSuperProperty(data.reg_string[0], data.reg_string[1], false),
                .set_super_property_strict => vm.executeSetSuperProperty(data.reg_string[0], data.reg_string[1], true),
                .set_super_property_computed => vm.executeSetSuperPropertyComputed(data.reg_reg[0], data.reg_reg[1], false),
                .set_super_property_computed_strict => vm.executeSetSuperPropertyComputed(data.reg_reg[0], data.reg_reg[1], true),
                .create_private_element => vm.executeCreatePrivateElement(data.reg_string[0], data.reg_string[1]),
                .resolve_private_element => vm.executeResolvePrivateElement(data.reg_string[0], data.reg_string[1]),
                .push_private_scope => vm.executePushPrivateScope(),
                .pop_private_scope => vm.executePopPrivateScope(),
                .get_private_element => vm.executeGetPrivateElement(data.reg_reg_string[0], data.reg_reg_string[1], data.reg_reg_string[2]),
                .set_private_element => vm.executeSetPrivateElement(data.reg_string_reg[0], data.reg_string_reg[1], data.reg_string_reg[2]),
                .has_private_element => vm.executeHasPrivateElement(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .import_call => vm.executeImportCall(data.reg_reg_reg[0], data.reg_reg_reg[1], data.reg_reg_reg[2]),
                .get_import_meta => vm.executeGetImportMeta(data.reg),
            };
            std.debug.assert(vm.call_stack.items.len == initial_call_stack_depth);
            switch (@typeInfo(@TypeOf(maybe_error))) {
                .void => {},
                .error_union => |u| {
                    comptime std.debug.assert(u.payload == void);
                    maybe_error catch |err| {
                        const inst_pc = pc.offsetBy(-@as(i32, inst_size));
                        try @call(.never_inline, handleError, .{ vm, err, inst_pc, &pc });
                    };
                },
                else => comptime unreachable,
            }
            continue :loop Bytecode.Inst.decodeTag(code[@intFromEnum(pc)..]);
        },
    }
}

fn handleError(vm: *Vm, err: Agent.Error, inst_pc: Pc, pc: *Pc) Agent.Error!void {
    switch (err) {
        error.OutOfMemory => return err,
        error.ExceptionThrown => {
            const frame = vm.currentCallFrame();
            const handler = frame.bytecode.findExceptionHandler(@intFromEnum(inst_pc)) orelse return err;
            const exception = vm.agent.clearException();
            vm.load(handler.exception_reg, exception.value);
            pc.* = @enumFromInt(handler.target);
        },
    }
}

pub fn @"resume"(
    vm: *Vm,
    callee_bytecode: *const Bytecode,
    suspension: GeneratorSuspension,
) Agent.Error!RunResult {
    const constants = try vm.ensureConstants(callee_bytecode);

    const regs_len = suspension.regs_len;
    const arguments_len = suspension.arguments_len;
    std.debug.assert(suspension.stack.len == regs_len + arguments_len);

    const stack_base: u32 = @intCast(vm.stack.items.len);
    try vm.stack.appendSlice(vm.agent.gc_allocator, suspension.stack);

    try vm.call_stack.append(vm.agent.gc_allocator, .{
        .bytecode = callee_bytecode,
        .constants = constants,
        .stack_base = stack_base,
        .regs_len = regs_len,
        .arguments_len = arguments_len,
        .cached_this_value = suspension.cached_this_value,
    });
    errdefer vm.popCallFrame();
    return vm.run(.{ .start_pc = suspension.saved_pc });
}

fn ensureConstants(vm: *Vm, bytecode: *const Bytecode) std.mem.Allocator.Error!Constants {
    const constants_gop = try vm.constants_cache.getOrPut(vm.agent.gc_allocator, bytecode);
    if (!constants_gop.found_existing) {
        const total_len = bytecode.strings.len + bytecode.big_ints.len;
        const Ptr = @typeInfo(Constants).pointer.child;
        const constants = try vm.agent.gc_allocator.alloc(Ptr, total_len);
        errdefer vm.agent.gc_allocator.free(constants);

        for (bytecode.strings, constants[0..bytecode.strings.len]) |utf8, *slot| {
            slot.* = @ptrCast(try stringValueImpl(vm.agent.gc_allocator, utf8));
        }
        for (bytecode.big_ints, constants[bytecode.strings.len..]) |@"const", *slot| {
            const managed = try @"const".toManaged(vm.agent.gc_allocator);
            slot.* = @ptrCast(@alignCast(try BigInt.fromManaged(vm.agent, managed)));
        }

        constants_gop.value_ptr.* = constants;
    }
    return constants_gop.value_ptr.*;
}

pub fn pushCallFrame(
    vm: *Vm,
    callee_bytecode: *const Bytecode,
    args: []const Value,
) std.mem.Allocator.Error!void {
    const constants = try vm.ensureConstants(callee_bytecode);
    const regs_len = callee_bytecode.num_regs;
    const arguments_len: u16 = @intCast(args.len);

    const stack_base: u32 = @intCast(vm.stack.items.len);
    try vm.stack.ensureUnusedCapacity(vm.agent.gc_allocator, regs_len + arguments_len);
    vm.stack.appendNTimesAssumeCapacity(undefined, regs_len);
    vm.stack.appendSliceAssumeCapacity(args);

    try vm.call_stack.append(vm.agent.gc_allocator, .{
        .bytecode = callee_bytecode,
        .constants = constants,
        .stack_base = stack_base,
        .regs_len = regs_len,
        .arguments_len = arguments_len,
        .cached_this_value = null,
    });
}

pub fn popCallFrame(vm: *Vm) void {
    std.debug.assert(vm.call_stack.items.len > 1);

    const frame = vm.call_stack.pop().?;
    const stack_len = frame.stackLen();
    vm.stack.shrinkRetainingCapacity(vm.stack.items.len - stack_len);
}

fn currentCallFrame(vm: *Vm) *CallFrame {
    return &vm.call_stack.items[vm.call_stack.items.len - 1];
}

fn regs(vm: *Vm) []Value {
    const frame = vm.currentCallFrame();
    const regs_start = frame.stack_base;
    return vm.stack.items[regs_start..][0..frame.regs_len];
}

fn arguments(vm: *Vm) []const Value {
    const frame = vm.currentCallFrame();
    const args_start = frame.stack_base + frame.regs_len;
    return vm.stack.items[args_start..][0..frame.arguments_len];
}

fn store(vm: *Vm, reg: Bytecode.Inst.Reg) Value {
    std.debug.assert(reg != .none);
    return vm.regs()[@intFromEnum(reg)];
}

fn load(vm: *Vm, reg: Bytecode.Inst.Reg, value: Value) void {
    std.debug.assert(reg != .none);
    vm.regs()[@intFromEnum(reg)] = value;
}

fn getString(vm: *Vm, index: Bytecode.Inst.StringIndex) *const String {
    const frame = vm.currentCallFrame();
    return @ptrCast(frame.constants[@intFromEnum(index)]);
}

fn getBigInt(vm: *Vm, index: Bytecode.Inst.BigIntIndex) *const BigInt {
    const frame = vm.currentCallFrame();
    return @ptrCast(frame.constants[frame.bytecode.strings.len + @intFromEnum(index)]);
}

fn getFunction(vm: *Vm, index: Bytecode.Inst.FunctionIndex) Bytecode.Function {
    const frame = vm.currentCallFrame();
    return frame.bytecode.functions[@intFromEnum(index)];
}

fn getClass(vm: *Vm, index: Bytecode.Inst.ClassIndex) Bytecode.Class {
    const frame = vm.currentCallFrame();
    return frame.bytecode.classes[@intFromEnum(index)];
}

fn executeJump(_: *Vm, offset: i32, pc: *Pc) void {
    pc.* = pc.offsetBy(offset);
}

fn executeJumpIfTrue(vm: *Vm, reg: Bytecode.Inst.Reg, offset: i32, pc: *Pc) void {
    if (vm.store(reg).toBoolean()) {
        pc.* = pc.offsetBy(offset);
    }
}

fn executeJumpIfFalse(vm: *Vm, reg: Bytecode.Inst.Reg, offset: i32, pc: *Pc) void {
    if (!vm.store(reg).toBoolean()) {
        pc.* = pc.offsetBy(offset);
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

fn executeCreateArray(vm: *Vm, dst: Bytecode.Inst.Reg, length: u32) std.mem.Allocator.Error!void {
    const array = try arrayCreateFast(vm.agent, length);
    vm.load(dst, Value.from(&array.object));
}

fn executeArrayPush(vm: *Vm, array_reg: Bytecode.Inst.Reg, elem_reg: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const array_value = vm.store(array_reg);
    const elem_value = vm.store(elem_reg);
    const array = array_value.asObject().as(builtins.Array);
    const index = array.fields.length;
    try array.object.createDataPropertyDirect(
        vm.agent,
        PropertyKey.from(@as(PropertyKey.IntegerIndex, index)),
        elem_value,
    );
}

fn executeArrayPushHole(vm: *Vm, array_reg: Bytecode.Inst.Reg) void {
    const array_value = vm.store(array_reg);
    const array = array_value.asObject().as(builtins.Array);
    array.fields.length += 1;
}

fn executeArraySet(vm: *Vm, array_reg: Bytecode.Inst.Reg, elem_reg: Bytecode.Inst.Reg, index: u32) std.mem.Allocator.Error!void {
    const array_value = vm.store(array_reg);
    const elem_value = vm.store(elem_reg);
    const array = array_value.asObject().as(builtins.Array);
    try array.object.property_storage.indexed_properties.set(vm.agent.gc_allocator, index, .{
        .value_or_accessor = .{ .value = elem_value },
        .attributes = .all,
    });
}

fn executeArraySpread(vm: *Vm, array_reg: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const array_value = vm.store(array_reg);
    const spread_value = vm.store(value_reg);
    const array = array_value.asObject();
    var iterator = try getIterator(vm.agent, spread_value, .sync);
    var next_index: u53 = array.as(builtins.Array).fields.length;
    while (try iterator.stepValue(vm.agent)) |next| : (next_index += 1) {
        try array.createDataPropertyDirect(vm.agent, PropertyKey.from(next_index), next);
    }
}

fn executeObjectCreate(vm: *Vm, dst: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const object = try ordinaryObjectCreateFast(vm.agent);
    vm.load(dst, Value.from(object));
}

fn executeObjectSet(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_index: Bytecode.Inst.StringIndex, value_reg: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const object = vm.store(object_reg).asObject();
    const property_key = PropertyKey.from(vm.getString(key_index));
    const property_value = vm.store(value_reg);
    try object.createDataPropertyDirect(vm.agent, property_key, property_value);
}

fn executeObjectSetComputed(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_reg: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object = vm.store(object_reg).asObject();
    const property_key = try vm.store(key_reg).toPropertyKey(vm.agent);
    const property_value = vm.store(value_reg);
    try object.createDataPropertyDirect(vm.agent, property_key, property_value);
}

fn executeObjectSetGetter(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_index: Bytecode.Inst.StringIndex, func_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object = vm.store(object_reg).asObject();
    const property_key = PropertyKey.from(vm.getString(key_index));
    const function = vm.store(func_reg).asObject();
    try object.definePropertyOrThrow(vm.agent, property_key, .{ .get = function, .enumerable = true, .configurable = true });
}

fn executeObjectSetGetterComputed(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_reg: Bytecode.Inst.Reg, func_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object = vm.store(object_reg).asObject();
    const property_key = try vm.store(key_reg).toPropertyKey(vm.agent);
    const function = vm.store(func_reg).asObject();
    try object.definePropertyOrThrow(vm.agent, property_key, .{ .get = function, .enumerable = true, .configurable = true });
}

fn executeObjectSetSetter(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_index: Bytecode.Inst.StringIndex, func_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object = vm.store(object_reg).asObject();
    const property_key = PropertyKey.from(vm.getString(key_index));
    const function = vm.store(func_reg).asObject();
    try object.definePropertyOrThrow(vm.agent, property_key, .{ .set = function, .enumerable = true, .configurable = true });
}

fn executeObjectSetSetterComputed(vm: *Vm, object_reg: Bytecode.Inst.Reg, key_reg: Bytecode.Inst.Reg, func_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object = vm.store(object_reg).asObject();
    const property_key = try vm.store(key_reg).toPropertyKey(vm.agent);
    const function = vm.store(func_reg).asObject();
    try object.definePropertyOrThrow(vm.agent, property_key, .{ .set = function, .enumerable = true, .configurable = true });
}

fn executeObjectSetPrototype(vm: *Vm, object_reg: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const object = vm.store(object_reg).asObject();
    const prototype_value = vm.store(value_reg);

    if (prototype_value.isObject() or prototype_value.isNull()) {
        const prototype = if (prototype_value.isObject()) prototype_value.asObject() else null;
        _ = object.internal_methods.setPrototypeOf(
            vm.agent,
            object,
            prototype,
        ) catch |err| try noexcept(err);
    }
}

fn executeObjectSpread(vm: *Vm, object_reg: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const object = vm.store(object_reg).asObject();
    const spread_value = vm.store(value_reg);
    const excluded_items: []const PropertyKey = &.{};
    try object.copyDataProperties(vm.agent, spread_value, excluded_items);
}

fn executeRegExpCreate(vm: *Vm, dst: Bytecode.Inst.Reg, pattern_index: Bytecode.Inst.StringIndex, flags_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const pattern = vm.getString(pattern_index);
    const flags = vm.getString(flags_index);
    const reg_exp = try builtins.regExpCreateFast(vm.agent, pattern, flags);
    vm.load(dst, Value.from(&reg_exp.object));
}

fn executeResolveThisBinding(vm: *Vm, reg: Bytecode.Inst.Reg) Agent.Error!void {
    const frame = vm.currentCallFrame();
    const this_value = frame.cached_this_value orelse blk: {
        const this_value = try vm.agent.resolveThisBinding();
        frame.cached_this_value = this_value;
        break :blk this_value;
    };
    vm.load(reg, this_value);
}

fn executeToNumber(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);
    const number = try value.toNumber(vm.agent);
    vm.load(dst, Value.from(number));
}

fn executeToNumeric(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);
    const numeric = try value.toNumeric(vm.agent);
    vm.load(dst, switch (numeric) {
        .number => |number| Value.from(number),
        .big_int => |big_int| Value.from(big_int),
    });
}

fn executeToString(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);
    const string = try value.toString(vm.agent);
    vm.load(dst, Value.from(string));
}

fn executeToObject(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(src);
    const object = try value.toObject(vm.agent);
    vm.load(dst, Value.from(object));
}

fn executeNegate(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) Agent.Error!void {
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

    // OPTIMIZATION: Fast path for i32 values
    if (value.__isI32()) {
        @branchHint(.likely);
        vm.load(dst, Value.from(~value.__asI32()));
        return;
    }

    const numeric = try value.toNumeric(vm.agent);
    vm.load(dst, switch (numeric) {
        .number => |n| Value.from(n.bitwiseNOT()),
        .big_int => |b| Value.from(try b.bitwiseNOT(vm.agent)),
    });
}

fn executeLogicalNot(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) void {
    const value = vm.store(src);
    vm.load(dst, Value.from(!value.toBoolean()));
}

fn executeTypeof(vm: *Vm, dst: Bytecode.Inst.Reg, src: Bytecode.Inst.Reg) void {
    const value = vm.store(src);
    vm.load(dst, Value.from(value.typeof()));
}

fn executeTypeofBinding(vm: *Vm, dst: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);

    const execution_context = vm.agent.runningExecutionContext();
    var env = execution_context.ecmascript_code.lexical_environment;
    while (!try env.hasBinding(vm.agent, name)) {
        env = env.outerEnv() orelse {
            @branchHint(.unlikely);
            vm.load(dst, Value.from("undefined"));
            return;
        };
    }

    const value = try env.getBindingValue(vm.agent, name, true);
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

fn executeEqStrict(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) void {
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

fn executeNotEqStrict(vm: *Vm, dst: Bytecode.Inst.Reg, lhs: Bytecode.Inst.Reg, rhs: Bytecode.Inst.Reg) void {
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

fn executePushScope(vm: *Vm) std.mem.Allocator.Error!void {
    const execution_context = vm.agent.runningExecutionContext();
    const old_env = execution_context.ecmascript_code.lexical_environment;
    const env = try newDeclarativeEnvironment(vm.agent.gc_allocator, old_env);
    execution_context.ecmascript_code.lexical_environment = .{ .declarative_environment = env };
}

fn executePushVarScope(vm: *Vm) std.mem.Allocator.Error!void {
    const execution_context = vm.agent.runningExecutionContext();
    const old_env = execution_context.ecmascript_code.lexical_environment;
    const env = try newDeclarativeEnvironment(vm.agent.gc_allocator, old_env);
    execution_context.ecmascript_code.lexical_environment = .{ .declarative_environment = env };
    execution_context.ecmascript_code.variable_environment = .{ .declarative_environment = env };
}

fn executePushWithScope(vm: *Vm, object_reg: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const object = vm.store(object_reg).asObject();
    const execution_context = vm.agent.runningExecutionContext();
    const old_env = execution_context.ecmascript_code.lexical_environment;
    const env = try newObjectEnvironment(vm.agent.gc_allocator, object, true, old_env);
    execution_context.ecmascript_code.lexical_environment = .{ .object_environment = env };
}

fn executePopScope(vm: *Vm) void {
    const execution_context = vm.agent.runningExecutionContext();
    execution_context.ecmascript_code.lexical_environment = execution_context.ecmascript_code.lexical_environment.outerEnv().?;
}

fn executeCreateMutableBinding(vm: *Vm, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);
    const execution_context = vm.agent.runningExecutionContext();
    const env = execution_context.ecmascript_code.lexical_environment;
    try env.createMutableBinding(vm.agent, name, false);
}

fn executeCreateImmutableBinding(vm: *Vm, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);
    const execution_context = vm.agent.runningExecutionContext();
    const env = execution_context.ecmascript_code.lexical_environment;
    try env.createImmutableBinding(vm.agent, name, true);
}

fn executeInitializeBinding(
    vm: *Vm,
    name_index: Bytecode.Inst.StringIndex,
    value_reg: Bytecode.Inst.Reg,
) Agent.Error!void {
    const name = vm.getString(name_index);
    const value = vm.store(value_reg);

    const execution_context = vm.agent.runningExecutionContext();
    const env = execution_context.ecmascript_code.lexical_environment;
    try env.initializeBinding(vm.agent, name, value);
}

fn executeGetBinding(vm: *Vm, dst: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);

    const execution_context = vm.agent.runningExecutionContext();
    var env = execution_context.ecmascript_code.lexical_environment;
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

fn executeGetProperty(
    vm: *Vm,
    dst: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    name_index: Bytecode.Inst.StringIndex,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(vm.getString(name_index));
    const result = try base_object.internal_methods.get(
        vm.agent,
        base_object,
        property_key,
        base_value,
    );
    vm.load(dst, result);
}

fn executeGetPropertyComputed(
    vm: *Vm,
    dst: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    property_reg: Bytecode.Inst.Reg,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const property_value = vm.store(property_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = try property_value.toPropertyKey(vm.agent);
    const result = try base_object.internal_methods.get(
        vm.agent,
        base_object,
        property_key,
        base_value,
    );
    vm.load(dst, result);
}

fn executeGetPropertyIndexed(
    vm: *Vm,
    dst: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    index: u32,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(@as(u53, index));
    const result = try base_object.internal_methods.get(
        vm.agent,
        base_object,
        property_key,
        base_value,
    );
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

    const execution_context = vm.agent.runningExecutionContext();
    var env = execution_context.ecmascript_code.lexical_environment;
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

fn executeSetProperty(
    vm: *Vm,
    base_reg: Bytecode.Inst.Reg,
    value_reg: Bytecode.Inst.Reg,
    name_index: Bytecode.Inst.StringIndex,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const value = vm.store(value_reg);

    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(vm.getString(name_index));
    const success = try base_object.internal_methods.set(
        vm.agent,
        base_object,
        property_key,
        value,
        base_value,
    );
    if (!success and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set property", .{});
    }
}

fn executeSetPropertyComputed(
    vm: *Vm,
    base_reg: Bytecode.Inst.Reg,
    property_reg: Bytecode.Inst.Reg,
    value_reg: Bytecode.Inst.Reg,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const property_value = vm.store(property_reg);
    const value = vm.store(value_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = try property_value.toPropertyKey(vm.agent);
    const success = try base_object.internal_methods.set(
        vm.agent,
        base_object,
        property_key,
        value,
        base_value,
    );
    if (!success and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set property", .{});
    }
}

fn executeSetPropertyIndexed(
    vm: *Vm,
    base_reg: Bytecode.Inst.Reg,
    value_reg: Bytecode.Inst.Reg,
    index: u32,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const value = vm.store(value_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(@as(u53, index));
    const success = try base_object.internal_methods.set(
        vm.agent,
        base_object,
        property_key,
        value,
        base_value,
    );
    if (!success and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set property", .{});
    }
}

const UpdateOp = enum { increment, decrement };
const UpdateType = enum { prefix, postfix };
const UpdateValues = struct {
    old_value_numeric: Value,
    new_value: Value,
};

inline fn computeUpdateValues(agent: *Agent, old_value: Value, comptime update_op: UpdateOp) Agent.Error!UpdateValues {
    if (old_value.__isI32()) {
        @branchHint(.likely);
        const func = switch (update_op) {
            .increment => std.math.add,
            .decrement => std.math.sub,
        };
        if (func(i32, old_value.__asI32(), 1)) |result| {
            return .{ .old_value_numeric = old_value, .new_value = Value.from(result) };
        } else |_| {}
    }
    const numeric = try old_value.toNumeric(agent);
    return switch (numeric) {
        .number => |n| .{
            .old_value_numeric = Value.from(n),
            .new_value = Value.from(switch (update_op) {
                .increment => Number.add(n, Number.from(1)),
                .decrement => Number.subtract(n, Number.from(1)),
            }),
        },
        .big_int => |b| .{
            .old_value_numeric = Value.from(b),
            .new_value = Value.from(switch (update_op) {
                .increment => try BigInt.add(b, agent, .one),
                .decrement => try BigInt.subtract(b, agent, .one),
            }),
        },
    };
}

fn executeUpdateBinding(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    name_index: Bytecode.Inst.StringIndex,
    comptime update_op: UpdateOp,
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
    const update = try computeUpdateValues(vm.agent, old_value, update_op);

    try env.setMutableBinding(vm.agent, name, update.new_value, strict);

    const result = switch (update_type) {
        .prefix => update.new_value,
        .postfix => update.old_value_numeric,
    };
    vm.load(dest, result);
}

fn executeUpdateProperty(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    name_index: Bytecode.Inst.StringIndex,
    comptime update_op: UpdateOp,
    comptime update_type: UpdateType,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(vm.getString(name_index));
    const old_value = try base_object.internal_methods.get(
        vm.agent,
        base_object,
        property_key,
        base_value,
    );
    const update = try computeUpdateValues(vm.agent, old_value, update_op);

    const success = try base_object.internal_methods.set(
        vm.agent,
        base_object,
        property_key,
        update.new_value,
        base_value,
    );
    if (!success and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set property", .{});
    }

    const result = switch (update_type) {
        .prefix => update.new_value,
        .postfix => update.old_value_numeric,
    };
    vm.load(dest, result);
}

fn executeUpdatePropertyComputed(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    property_reg: Bytecode.Inst.Reg,
    comptime update_op: UpdateOp,
    comptime update_type: UpdateType,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const property_value = vm.store(property_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = try property_value.toPropertyKey(vm.agent);
    const old_value = try base_object.internal_methods.get(
        vm.agent,
        base_object,
        property_key,
        base_value,
    );
    const update = try computeUpdateValues(vm.agent, old_value, update_op);

    const success = try base_object.internal_methods.set(
        vm.agent,
        base_object,
        property_key,
        update.new_value,
        base_value,
    );
    if (!success and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set property", .{});
    }

    const result = switch (update_type) {
        .prefix => update.new_value,
        .postfix => update.old_value_numeric,
    };
    vm.load(dest, result);
}

fn executeUpdatePropertyIndexed(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    index: u32,
    comptime update_op: UpdateOp,
    comptime update_type: UpdateType,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(@as(u53, index));
    const old_value = try base_object.internal_methods.get(
        vm.agent,
        base_object,
        property_key,
        base_value,
    );
    const update = try computeUpdateValues(vm.agent, old_value, update_op);

    const success = try base_object.internal_methods.set(
        vm.agent,
        base_object,
        property_key,
        update.new_value,
        base_value,
    );
    if (!success and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set property", .{});
    }

    const result = switch (update_type) {
        .prefix => update.new_value,
        .postfix => update.old_value_numeric,
    };
    vm.load(dest, result);
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

fn executeDeleteProperty(
    vm: *Vm,
    dst: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    name_index: Bytecode.Inst.StringIndex,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(vm.getString(name_index));
    const delete_status = try base_object.internal_methods.delete(
        vm.agent,
        base_object,
        property_key,
    );
    if (!delete_status and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not delete property", .{});
    }
    vm.load(dst, Value.from(delete_status));
}

fn executeDeletePropertyComputed(
    vm: *Vm,
    dst: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    property_reg: Bytecode.Inst.Reg,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const property_value = vm.store(property_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = try property_value.toPropertyKey(vm.agent);
    const delete_status = try base_object.internal_methods.delete(
        vm.agent,
        base_object,
        property_key,
    );
    if (!delete_status and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not delete property", .{});
    }
    vm.load(dst, Value.from(delete_status));
}

fn executeDeletePropertyIndexed(
    vm: *Vm,
    dst: Bytecode.Inst.Reg,
    base_reg: Bytecode.Inst.Reg,
    index: u32,
    comptime strict: bool,
) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(@as(u53, index));
    const delete_status = try base_object.internal_methods.delete(
        vm.agent,
        base_object,
        property_key,
    );
    if (!delete_status and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not delete property", .{});
    }
    vm.load(dst, Value.from(delete_status));
}

fn executeCopyDataProperties(vm: *Vm, dest: Bytecode.Inst.Reg, source_reg: Bytecode.Inst.Reg, excluded_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const source_value = vm.store(source_reg);

    const target = try ordinaryObjectCreateFast(vm.agent);
    if (excluded_reg == .none) {
        const excluded_items: []const PropertyKey = &.{};
        try target.copyDataProperties(vm.agent, source_value, excluded_items);
    } else {
        const excluded_object = vm.store(excluded_reg).asObject();
        const excluded_len = excluded_object.as(builtins.Array).fields.length;

        var excluded_items: std.ArrayList(PropertyKey) = try .initCapacity(vm.agent.gc_allocator, excluded_len);
        defer excluded_items.deinit(vm.agent.gc_allocator);
        for (0..excluded_len) |i| {
            const descriptor = excluded_object.property_storage.indexed_properties.get(@intCast(i)).?;
            const prop_key = PropertyKey.from(descriptor.value_or_accessor.value.asString());
            excluded_items.appendAssumeCapacity(prop_key);
        }

        try target.copyDataProperties(vm.agent, source_value, excluded_items.items);
    }

    vm.load(dest, Value.from(target));
}

fn executeCall(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    callee_reg: Bytecode.Inst.Reg,
    args_reg: Bytecode.Inst.Reg,
) Agent.Error!void {
    const callee_value = vm.store(callee_reg);
    const args_value = vm.store(args_reg);
    const args_object = args_value.asObject();
    const args_len = args_object.as(builtins.Array).fields.length;

    var args_list: std.ArrayList(Value) = try .initCapacity(vm.agent.gc_allocator, args_len);
    defer args_list.deinit(vm.agent.gc_allocator);
    for (0..args_len) |i| {
        const descriptor = args_object.property_storage.indexed_properties.get(@intCast(i)).?;
        const arg = descriptor.value_or_accessor.value;
        args_list.appendAssumeCapacity(arg);
    }

    const result = try evaluateCall(vm.agent, callee_value, .undefined, args_list.items);
    vm.load(dest, result);
}

fn executeCallN(
    vm: *Vm,
    comptime N: comptime_int,
    dest: Bytecode.Inst.Reg,
    callee_reg: Bytecode.Inst.Reg,
    arg_regs: [N]Bytecode.Inst.Reg,
) Agent.Error!void {
    const callee_value = vm.store(callee_reg);

    var args: [N]Value = undefined;
    inline for (0..N) |i| args[i] = vm.store(arg_regs[i]);

    const result = try evaluateCall(vm.agent, callee_value, .undefined, &args);
    vm.load(dest, result);
}

fn executeCallProperty(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    callee_reg: Bytecode.Inst.Reg,
    this_reg: Bytecode.Inst.Reg,
    args_reg: Bytecode.Inst.Reg,
) Agent.Error!void {
    const callee_value = vm.store(callee_reg);
    const this_value = vm.store(this_reg);
    const args_value = vm.store(args_reg);
    const args_object = args_value.asObject();
    const args_len = args_object.as(builtins.Array).fields.length;

    var args_list: std.ArrayList(Value) = try .initCapacity(vm.agent.gc_allocator, args_len);
    defer args_list.deinit(vm.agent.gc_allocator);
    for (0..args_len) |i| {
        const descriptor = args_object.property_storage.indexed_properties.get(@intCast(i)).?;
        const arg = descriptor.value_or_accessor.value;
        args_list.appendAssumeCapacity(arg);
    }

    const result = try evaluateCall(vm.agent, callee_value, this_value, args_list.items);
    vm.load(dest, result);
}

fn executeCallPropertyN(
    vm: *Vm,
    comptime N: comptime_int,
    dest: Bytecode.Inst.Reg,
    callee_reg: Bytecode.Inst.Reg,
    this_reg: Bytecode.Inst.Reg,
    arg_regs: [N]Bytecode.Inst.Reg,
) Agent.Error!void {
    const callee_value = vm.store(callee_reg);
    const this_value = vm.store(this_reg);

    var args: [N]Value = undefined;
    inline for (0..N) |i| args[i] = vm.store(arg_regs[i]);

    const result = try evaluateCall(vm.agent, callee_value, this_value, &args);
    vm.load(dest, result);
}

fn executeCallDirectEval(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    callee_reg: Bytecode.Inst.Reg,
    args_reg: Bytecode.Inst.Reg,
    strict: bool,
) Agent.Error!void {
    const callee_value = vm.store(callee_reg);
    const args_value = vm.store(args_reg);
    const args_object = args_value.asObject();
    const args_len = args_object.as(builtins.Array).fields.length;

    var args_list: std.ArrayList(Value) = try .initCapacity(vm.agent.gc_allocator, args_len);
    defer args_list.deinit(vm.agent.gc_allocator);
    for (0..args_len) |i| {
        const descriptor = args_object.property_storage.indexed_properties.get(@intCast(i)).?;
        const arg = descriptor.value_or_accessor.value;
        args_list.appendAssumeCapacity(arg);
    }

    const realm = vm.agent.currentRealm();
    const eval = try realm.intrinsics.@"%eval%"();

    const result = if (callee_value.sameValue(Value.from(eval)))
        try directEval(vm.agent, args_list.items, strict)
    else
        try evaluateCall(vm.agent, callee_value, .undefined, args_list.items);
    vm.load(dest, result);
}

fn executeConstruct(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    constructor_reg: Bytecode.Inst.Reg,
    args_reg: Bytecode.Inst.Reg,
) Agent.Error!void {
    const constructor = vm.store(constructor_reg);
    const args_value = vm.store(args_reg);
    const args_object = args_value.asObject();
    const args_len = args_object.as(builtins.Array).fields.length;

    var args_list: std.ArrayList(Value) = try .initCapacity(vm.agent.gc_allocator, args_len);
    defer args_list.deinit(vm.agent.gc_allocator);
    for (0..args_len) |i| {
        const descriptor = args_object.property_storage.indexed_properties.get(@intCast(i)).?;
        const arg = descriptor.value_or_accessor.value;
        args_list.appendAssumeCapacity(arg);
    }

    const result = try evaluateNew(vm.agent, constructor, args_list.items);
    vm.load(dest, result);
}

fn executeConstructN(
    vm: *Vm,
    comptime N: comptime_int,
    dest: Bytecode.Inst.Reg,
    constructor_reg: Bytecode.Inst.Reg,
    arg_regs: [N]Bytecode.Inst.Reg,
) Agent.Error!void {
    const constructor = vm.store(constructor_reg);

    var args: [N]Value = undefined;
    inline for (0..N) |i| args[i] = vm.store(arg_regs[i]);

    const result = try evaluateNew(vm.agent, constructor, &args);
    vm.load(dest, result);
}

fn executeGetTemplateObject(vm: *Vm, dest: Bytecode.Inst.Reg, cooked_reg: Bytecode.Inst.Reg, raw_reg: Bytecode.Inst.Reg, template_id: u16) Agent.Error!void {
    const realm = vm.agent.currentRealm();
    const frame = vm.currentCallFrame();
    const cache_key = std.hash.Wyhash.hash(template_id, std.mem.asBytes(&frame.bytecode));

    const gop = try realm.template_map.getOrPut(vm.agent.gc_allocator, cache_key);
    if (gop.found_existing) {
        vm.load(dest, Value.from(&gop.value_ptr.*.object));
        return;
    }

    const cooked = vm.store(cooked_reg).asObject();
    const raw = vm.store(raw_reg).asObject();

    _ = raw.setIntegrityLevel(vm.agent, .frozen) catch |err| try noexcept(err);

    try cooked.definePropertyDirect(vm.agent, PropertyKey.from("raw"), .{
        .value_or_accessor = .{ .value = Value.from(raw) },
        .attributes = .none,
    });
    _ = cooked.setIntegrityLevel(vm.agent, .frozen) catch |err| try noexcept(err);

    gop.value_ptr.* = cooked.as(builtins.Array);

    vm.load(dest, Value.from(cooked));
}

fn executeGetIterator(vm: *Vm, dest: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(value_reg);
    const iterator = try getIterator(vm.agent, value, .sync);

    const iterator_obj = try ordinaryObjectCreate(vm.agent, null);
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("iterator"), Value.from(iterator.iterator));
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("nextMethod"), iterator.next_method);
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("done"), Value.from(iterator.done));

    vm.load(dest, Value.from(iterator_obj));
}

fn executeGetAsyncIterator(vm: *Vm, dest: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(value_reg);
    const iterator = try getIterator(vm.agent, value, .async);

    const iterator_obj = try ordinaryObjectCreate(vm.agent, null);
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("iterator"), Value.from(iterator.iterator));
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("nextMethod"), iterator.next_method);
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("done"), Value.from(iterator.done));

    vm.load(dest, Value.from(iterator_obj));
}

fn executeGetForInIterator(vm: *Vm, dest: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(value_reg);
    const object = value.toObject(vm.agent) catch |err| try noexcept(err);
    const for_in_iterator = try createForInIterator(vm.agent, object);
    const iterator = try getIteratorDirect(vm.agent, &for_in_iterator.object);

    const iterator_obj = try ordinaryObjectCreate(vm.agent, null);
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("iterator"), Value.from(iterator.iterator));
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("nextMethod"), iterator.next_method);
    try iterator_obj.createDataPropertyDirect(vm.agent, PropertyKey.from("done"), Value.from(iterator.done));

    vm.load(dest, Value.from(iterator_obj));
}

fn executeIteratorStep(vm: *Vm, dest: Bytecode.Inst.Reg, iterator_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const iterator_obj = vm.store(iterator_reg).asObject();

    var iterator: Iterator = .{
        .iterator = iterator_obj.getPropertyValueDirect(PropertyKey.from("iterator")).asObject(),
        .next_method = iterator_obj.getPropertyValueDirect(PropertyKey.from("nextMethod")),
        .done = iterator_obj.getPropertyValueDirect(PropertyKey.from("done")).toBoolean(),
    };

    if (try iterator.step(vm.agent)) |next| {
        vm.load(dest, Value.from(next));
    } else {
        vm.load(dest, .undefined);
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
    }
}

fn executeIteratorStepValue(vm: *Vm, dest: Bytecode.Inst.Reg, iterator_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const iterator_obj = vm.store(iterator_reg).asObject();

    var iterator: Iterator = .{
        .iterator = iterator_obj.getPropertyValueDirect(PropertyKey.from("iterator")).asObject(),
        .next_method = iterator_obj.getPropertyValueDirect(PropertyKey.from("nextMethod")),
        .done = iterator_obj.getPropertyValueDirect(PropertyKey.from("done")).toBoolean(),
    };

    if (try iterator.stepValue(vm.agent)) |next| {
        vm.load(dest, next);
    } else {
        vm.load(dest, .undefined);
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
    }
}

fn executeIteratorStepValueAsync(vm: *Vm, dest: Bytecode.Inst.Reg, iterator_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const iterator_obj = vm.store(iterator_reg).asObject();

    const iterator_inner = iterator_obj.getPropertyValueDirect(PropertyKey.from("iterator")).asObject();
    const next_method = iterator_obj.getPropertyValueDirect(PropertyKey.from("nextMethod"));

    // Implements steps 6.a-f. of ForIn/OfBodyEvaluation for async iterators.
    // https://tc39.es/ecma262/#sec-runtime-semantics-forin-div-ofbodyevaluation-lhs-stmt-iterator-lhskind-labelset

    // a. Let nextResult be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
    const next_result = next_method.call(vm.agent, Value.from(iterator_inner), &.{}) catch |err| {
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
        return err;
    };

    // b. If iteratorKind is async, set nextResult to ? Await(nextResult).
    const awaited_result = await(vm.agent, next_result) catch |err| {
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
        return err;
    };

    // c. If nextResult is not an Object, throw a TypeError exception.
    if (!awaited_result.isObject()) {
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
        return vm.agent.throwException(.type_error, "{f} is not an Object", .{awaited_result});
    }

    // d. Let done be ? IteratorComplete(nextResult).
    const done = try Iterator.complete(vm.agent, awaited_result.asObject());

    // e. If done is true, return V.
    if (done) {
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
        vm.load(dest, .undefined);
        return;
    }

    // f. Let nextValue be ? IteratorValue(nextResult).
    const value = Iterator.value(vm.agent, awaited_result.asObject()) catch |err| {
        iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);
        return err;
    };

    vm.load(dest, value);
}

fn executeIteratorClose(vm: *Vm, iterator_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const iterator_obj = vm.store(iterator_reg).asObject();
    const done = iterator_obj.getPropertyValueDirect(PropertyKey.from("done")).toBoolean();
    if (done) return;

    var iterator: Iterator = .{
        .iterator = iterator_obj.getPropertyValueDirect(PropertyKey.from("iterator")).asObject(),
        .next_method = iterator_obj.getPropertyValueDirect(PropertyKey.from("nextMethod")),
        .done = false,
    };
    try iterator.close(vm.agent, @as(Agent.Error!void, {}));
}

fn executeIteratorIsDone(vm: *Vm, dest: Bytecode.Inst.Reg, iterator_reg: Bytecode.Inst.Reg) void {
    const iterator_obj = vm.store(iterator_reg).asObject();
    const done = iterator_obj.getPropertyValueDirect(PropertyKey.from("done"));
    vm.load(dest, done);
}

fn executeIteratorCollect(vm: *Vm, dest: Bytecode.Inst.Reg, iterator_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const iterator_obj = vm.store(iterator_reg).asObject();

    var iterator: Iterator = .{
        .iterator = iterator_obj.getPropertyValueDirect(PropertyKey.from("iterator")).asObject(),
        .next_method = iterator_obj.getPropertyValueDirect(PropertyKey.from("nextMethod")),
        .done = iterator_obj.getPropertyValueDirect(PropertyKey.from("done")).toBoolean(),
    };

    var values: std.ArrayList(Value) = .empty;
    defer values.deinit(vm.agent.gc_allocator);
    while (try iterator.stepValue(vm.agent)) |next| {
        try values.append(vm.agent.gc_allocator, next);
    }
    iterator_obj.setValueAtPropertyIndex(@enumFromInt(2), .true);

    const array = try createArrayFromList(vm.agent, values.items);
    vm.load(dest, Value.from(&array.object));
}

fn executeThrow(vm: *Vm, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(value_reg);
    vm.agent.exception = .{
        .value = value,
        .stack_trace = try vm.agent.captureStackTrace(),
    };
    return error.ExceptionThrown;
}

fn executeThrowReferenceError(vm: *Vm) Agent.Error!void {
    // Only emitted for web-compat assignment
    return vm.agent.throwException(.reference_error, "Invalid assignment to function call", .{});
}

fn executeReturn(vm: *Vm, reg: Bytecode.Inst.Reg) RunResult {
    const return_value: ?Value = if (reg != .none) vm.store(reg) else null;
    if (vm.call_stack.items.len > 1) vm.popCallFrame();
    return .{ .@"return" = return_value };
}

fn executeAwait(vm: *Vm, reg: Bytecode.Inst.Reg) Agent.Error!void {
    const value = vm.store(reg);
    const result = try await(vm.agent, value);
    vm.load(reg, result);
}

fn executeYield(vm: *Vm, reg: Bytecode.Inst.Reg, pc: Pc) Agent.Error!RunResult {
    // The initial `yield` instruction inserted after FDI doesn't have a register.
    if (reg != .none) {
        const value = vm.store(reg);
        _ = try yield(vm.agent, value);
    }

    std.debug.assert(vm.call_stack.items.len > 1);
    const frame = vm.currentCallFrame();
    const stack_start = frame.stack_base;
    const stack_len = frame.stackLen();
    const stack = try vm.agent.gc_allocator.dupe(
        Value,
        vm.stack.items[stack_start..][0..stack_len],
    );

    vm.popCallFrame();

    return .{ .yield = .{
        .stack = stack,
        .regs_len = frame.regs_len,
        .arguments_len = frame.arguments_len,
        .cached_this_value = frame.cached_this_value,
        .saved_pc = pc,
        .yield_reg = reg,
    } };
}

fn executeCreateFunction(vm: *Vm, dest: Bytecode.Inst.Reg, function_index: Bytecode.Inst.FunctionIndex) Agent.Error!void {
    const function = vm.getFunction(function_index);
    const source_text = try vm.getString(function.source_text).toUtf8(vm.agent.gc_allocator);
    const identifier: ?[]const u8 = switch (function.name) {
        .identifier => |name_index| try vm.getString(name_index).toUtf8(vm.agent.gc_allocator),
        .none, .default => null,
    };
    const default_name: ?[]const u8 = switch (function.name) {
        .default => |name_index| try vm.getString(name_index).toUtf8(vm.agent.gc_allocator),
        .none, .identifier => null,
    };
    const function_obj = switch (function.kind) {
        .normal => try instantiateOrdinaryFunctionExpression(vm.agent, .{
            .identifier = identifier,
            .formal_parameters = function.parameters,
            .function_body = function.body,
            .source_text = source_text,
        }, default_name),
        .arrow => try instantiateArrowFunctionExpression(vm.agent, .{
            .formal_parameters = function.parameters,
            .function_body = function.body,
            .source_text = source_text,
        }, default_name),
        .generator => try instantiateGeneratorFunctionExpression(vm.agent, .{
            .identifier = identifier,
            .formal_parameters = function.parameters,
            .function_body = function.body,
            .source_text = source_text,
        }, default_name),
        .async => try instantiateAsyncFunctionExpression(vm.agent, .{
            .identifier = identifier,
            .formal_parameters = function.parameters,
            .function_body = function.body,
            .source_text = source_text,
        }, default_name),
        .async_arrow => try instantiateAsyncArrowFunctionExpression(vm.agent, .{
            .formal_parameters = function.parameters,
            .function_body = function.body,
            .source_text = source_text,
        }, default_name),
        .async_generator => try instantiateAsyncGeneratorFunctionExpression(vm.agent, .{
            .identifier = identifier,
            .formal_parameters = function.parameters,
            .function_body = function.body,
            .source_text = source_text,
        }, default_name),
    };
    vm.load(dest, Value.from(&function_obj.object));
}

fn executeCreateClass(vm: *Vm, dest: Bytecode.Inst.Reg, class_index: Bytecode.Inst.ClassIndex) Agent.Error!void {
    const class = vm.getClass(class_index);
    const source_text = try vm.getString(class.source_text).toUtf8(vm.agent.gc_allocator);
    const class_binding: ?*const String = switch (class.name) {
        .identifier => |name_index| vm.getString(name_index),
        .none, .default => null,
    };
    const class_name: *const String = switch (class.name) {
        .identifier => |name_index| vm.getString(name_index),
        .default => |name_index| vm.getString(name_index),
        .none => .empty,
    };
    const heritage: ?Value = switch (class.heritage) {
        .none => null,
        else => vm.store(class.heritage),
    };
    const element_names = try vm.agent.gc_allocator.alloc(Value, class.element_names.len);
    defer vm.agent.gc_allocator.free(element_names);
    for (class.element_names, element_names) |name_reg, *value| {
        value.* = switch (name_reg) {
            .none => .undefined,
            else => vm.store(name_reg),
        };
    }
    const class_obj = try classDefinitionEvaluation(
        vm.agent,
        class.class_tail,
        class_binding,
        class_name,
        source_text,
        heritage,
        element_names,
    );
    vm.load(dest, Value.from(class_obj));
}

fn executeSetHomeObject(vm: *Vm, function_reg: Bytecode.Inst.Reg, home_object_reg: Bytecode.Inst.Reg) void {
    const function_value = vm.store(function_reg);
    const home_object_value = vm.store(home_object_reg);
    const function = function_value.asObject().as(builtins.ECMAScriptFunction);
    makeMethod(function, home_object_value.asObject());
}

fn executeCreateUnmappedArgumentsObject(vm: *Vm, dest: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const arguments_object = try createUnmappedArgumentsObject(vm.agent, vm.arguments());
    vm.load(dest, Value.from(&arguments_object.object));
}

fn executeCreateMappedArgumentsObject(vm: *Vm, dest: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const execution_context = vm.agent.runningExecutionContext();
    const function = execution_context.origin.function.as(builtins.ECMAScriptFunction);
    const arguments_object = try createMappedArgumentsObject(
        vm.agent,
        &function.object,
        function.fields.formal_parameters,
        vm.arguments(),
        execution_context.ecmascript_code.lexical_environment,
    );
    vm.load(dest, Value.from(&arguments_object.object));
}

fn executeGetArgument(vm: *Vm, dest: Bytecode.Inst.Reg, arg_index: u16) void {
    const args = vm.arguments();
    const value: Value = if (arg_index < args.len) args[arg_index] else .undefined;
    vm.load(dest, value);
}

fn executeGetRestArguments(vm: *Vm, dest: Bytecode.Inst.Reg, start_index: u16) std.mem.Allocator.Error!void {
    const args = vm.arguments();
    const rest_args = args[@min(start_index, args.len)..];
    const array = try createArrayFromList(vm.agent, rest_args);
    vm.load(dest, Value.from(&array.object));
}

fn executeGetNewTarget(vm: *Vm, reg: Bytecode.Inst.Reg) void {
    const value: Value = if (vm.agent.getNewTarget()) |new_target|
        Value.from(new_target)
    else
        .undefined;
    vm.load(reg, value);
}

fn executeSuperCall(
    vm: *Vm,
    dest: Bytecode.Inst.Reg,
    args_reg: Bytecode.Inst.Reg,
) Agent.Error!void {
    const args_value = vm.store(args_reg);
    const args_object = args_value.asObject();
    const args_len = args_object.as(builtins.Array).fields.length;

    var args_list: std.ArrayList(Value) = try .initCapacity(vm.agent.gc_allocator, args_len);
    defer args_list.deinit(vm.agent.gc_allocator);
    for (0..args_len) |i| {
        const descriptor = args_object.property_storage.indexed_properties.get(@intCast(i)).?;
        const arg = descriptor.value_or_accessor.value;
        args_list.appendAssumeCapacity(arg);
    }

    const result = try evaluateSuperCall(vm.agent, args_list.items);
    vm.load(dest, result);
}

fn executeGetSuperProperty(vm: *Vm, dest: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const env = vm.agent.getThisEnvironment();
    const actual_this = try env.getThisBinding(vm.agent);
    const base = try env.function_environment.getSuperBase(vm.agent);
    const base_value: Value = switch (base) {
        .undefined => .undefined,
        .object => |maybe_object| if (maybe_object) |o| Value.from(o) else .null,
    };
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(vm.getString(name_index));
    const result = try base_object.internal_methods.get(vm.agent, base_object, property_key, actual_this);
    vm.load(dest, result);
}

fn executeGetSuperPropertyComputed(vm: *Vm, dest: Bytecode.Inst.Reg, property_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const property_value = vm.store(property_reg);
    const env = vm.agent.getThisEnvironment();
    const actual_this = try env.getThisBinding(vm.agent);
    const base = try env.function_environment.getSuperBase(vm.agent);
    const base_value: Value = switch (base) {
        .undefined => .undefined,
        .object => |maybe_object| if (maybe_object) |o| Value.from(o) else .null,
    };
    const base_object = try base_value.toObject(vm.agent);
    const property_key = try property_value.toPropertyKey(vm.agent);
    const result = try base_object.internal_methods.get(vm.agent, base_object, property_key, actual_this);
    vm.load(dest, result);
}

fn executeSetSuperProperty(vm: *Vm, value_reg: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex, comptime strict: bool) Agent.Error!void {
    const value = vm.store(value_reg);
    const env = vm.agent.getThisEnvironment();
    const actual_this = try env.getThisBinding(vm.agent);
    const base = try env.function_environment.getSuperBase(vm.agent);
    const base_value: Value = switch (base) {
        .undefined => .undefined,
        .object => |maybe_object| if (maybe_object) |o| Value.from(o) else .null,
    };
    const base_object = try base_value.toObject(vm.agent);
    const property_key = PropertyKey.from(vm.getString(name_index));
    const succeeded = try base_object.internal_methods.set(vm.agent, base_object, property_key, value, actual_this);
    if (!succeeded and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set super property", .{});
    }
}

fn executeSetSuperPropertyComputed(vm: *Vm, property_reg: Bytecode.Inst.Reg, value_reg: Bytecode.Inst.Reg, comptime strict: bool) Agent.Error!void {
    const property_value = vm.store(property_reg);
    const value = vm.store(value_reg);
    const env = vm.agent.getThisEnvironment();
    const actual_this = try env.getThisBinding(vm.agent);
    const base = try env.function_environment.getSuperBase(vm.agent);
    const base_value: Value = switch (base) {
        .undefined => .undefined,
        .object => |maybe_object| if (maybe_object) |o| Value.from(o) else .null,
    };
    const base_object = try base_value.toObject(vm.agent);
    const property_key = try property_value.toPropertyKey(vm.agent);
    const succeeded = try base_object.internal_methods.set(vm.agent, base_object, property_key, value, actual_this);
    if (!succeeded and strict) {
        @branchHint(.unlikely);
        return vm.agent.throwException(.type_error, "Could not set super property", .{});
    }
}

fn executeCreatePrivateElement(vm: *Vm, dest: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);
    const name_utf8 = try name.toUtf8(vm.agent.gc_allocator);

    const execution_context = vm.agent.runningExecutionContext();
    const private_env = execution_context.ecmascript_code.private_environment.?;
    const private_name = try PrivateName.init(vm.agent.gc_allocator, name);

    try private_env.names.putNoClobber(vm.agent.gc_allocator, name_utf8, private_name);
    vm.load(dest, Value.from(private_name.symbol));
}

fn executeResolvePrivateElement(vm: *Vm, dest: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const name = vm.getString(name_index);
    const name_utf8 = try name.toUtf8(vm.agent.gc_allocator);
    defer vm.agent.gc_allocator.free(name_utf8);

    const private_environment = vm.agent.runningExecutionContext().ecmascript_code.private_environment.?;
    const private_name = private_environment.resolvePrivateIdentifier(name_utf8);
    vm.load(dest, Value.from(private_name.symbol));
}

fn executePushPrivateScope(vm: *Vm) std.mem.Allocator.Error!void {
    const execution_context = vm.agent.runningExecutionContext();
    const private_env = try newPrivateEnvironment(
        vm.agent.gc_allocator,
        execution_context.ecmascript_code.private_environment,
    );
    execution_context.ecmascript_code.private_environment = private_env;
}

fn executePopPrivateScope(vm: *Vm) void {
    const execution_context = vm.agent.runningExecutionContext();
    execution_context.ecmascript_code.private_environment = execution_context.ecmascript_code.private_environment.?.outer_private_environment;
}

fn executeGetPrivateElement(vm: *Vm, dest: Bytecode.Inst.Reg, base_reg: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const name = vm.getString(name_index);
    const name_utf8 = try name.toUtf8(vm.agent.gc_allocator);
    defer vm.agent.gc_allocator.free(name_utf8);

    const execution_context = vm.agent.runningExecutionContext();
    const private_env = execution_context.ecmascript_code.private_environment.?;
    const private_name = private_env.resolvePrivateIdentifier(name_utf8);

    const base_object = try base_value.toObject(vm.agent);
    const result = try base_object.privateGet(vm.agent, private_name);
    vm.load(dest, result);
}

fn executeSetPrivateElement(vm: *Vm, base_reg: Bytecode.Inst.Reg, name_index: Bytecode.Inst.StringIndex, value_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const base_value = vm.store(base_reg);
    const value = vm.store(value_reg);
    const name = vm.getString(name_index);
    const identifier = try name.toUtf8(vm.agent.gc_allocator);
    defer vm.agent.gc_allocator.free(identifier);

    const private_environment = vm.agent.runningExecutionContext().ecmascript_code.private_environment.?;
    const private_name = private_environment.resolvePrivateIdentifier(identifier);

    const base_object = try base_value.toObject(vm.agent);
    try base_object.privateSet(vm.agent, private_name, value);
}

fn executeHasPrivateElement(vm: *Vm, dest: Bytecode.Inst.Reg, symbol_reg: Bytecode.Inst.Reg, object_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const symbol_value = vm.store(symbol_reg);
    const object_value = vm.store(object_reg);

    if (!object_value.isObject()) {
        return vm.agent.throwException(
            .type_error,
            "Right-hand side of 'in' operator must be an object",
            .{},
        );
    }

    const private_name = symbol_value.toPrivateName().?;
    const result = object_value.asObject().privateElementFind(private_name) != null;
    vm.load(dest, Value.from(result));
}

fn executeImportCall(vm: *Vm, dest: Bytecode.Inst.Reg, specifier_reg: Bytecode.Inst.Reg, options_reg: Bytecode.Inst.Reg) Agent.Error!void {
    const specifier = vm.store(specifier_reg);
    const options = vm.store(options_reg);
    const result = try evaluateImportCall(vm.agent, specifier, options);
    vm.load(dest, result);
}

fn executeGetImportMeta(vm: *Vm, dest: Bytecode.Inst.Reg) std.mem.Allocator.Error!void {
    const result = try evaluateImportMeta(vm.agent);
    vm.load(dest, result);
}
