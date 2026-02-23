const std = @import("std");

const interpreter = @import("../../interpreter.zig");

const Bytecode = interpreter.Bytecode;
const Ir = interpreter.Ir;

const LinearScanRegisterAllocation = @import("LinearScanRegisterAllocation.zig");

const Builder = @This();

gpa: std.mem.Allocator,
ir: *const Ir,
blocks: std.ArrayList(*Block),
current: ?*Block,
lsra: LinearScanRegisterAllocation,
label_blocks: std.AutoHashMapUnmanaged(Ir.Inst.Ref, *Block),
exception_handlers: std.ArrayList(ExceptionHandler),

const ExceptionHandler = struct {
    start: *Block,
    end: *Block,
    target: *Block,
    exception_reg: Bytecode.Inst.Reg,
};

pub const Error = error{OutOfMemory};

pub fn init(gpa: std.mem.Allocator, ir: *const Ir) std.mem.Allocator.Error!Builder {
    const lsra: LinearScanRegisterAllocation = try .init(gpa, ir.live_ranges);
    return .{
        .gpa = gpa,
        .ir = ir,
        .blocks = .empty,
        .current = null,
        .lsra = lsra,
        .label_blocks = .empty,
        .exception_handlers = .empty,
    };
}

pub fn deinit(b: *Builder) void {
    for (b.blocks.items) |block| {
        block.deinit(b.gpa);
        b.gpa.destroy(block);
    }
    b.blocks.deinit(b.gpa);
    b.lsra.deinit(b.gpa);
    b.label_blocks.deinit(b.gpa);
    b.exception_handlers.deinit(b.gpa);
}

fn computeRPO(
    gpa: std.mem.Allocator,
    block: *Block,
    visited: *std.AutoHashMapUnmanaged(*Block, void),
    out: *std.ArrayList(*Block),
) std.mem.Allocator.Error!void {
    const start = out.items.len;
    try computeRPOInner(gpa, block, visited, out);
    std.mem.reverse(*Block, out.items[start..]);
}

fn computeRPOInner(
    gpa: std.mem.Allocator,
    block: *Block,
    visited: *std.AutoHashMapUnmanaged(*Block, void),
    out: *std.ArrayList(*Block),
) std.mem.Allocator.Error!void {
    const gop = try visited.getOrPut(gpa, block);
    if (gop.found_existing) return;

    switch (block.terminator) {
        .none, .noreturn => {},
        .jump => |target| try computeRPOInner(gpa, target, visited, out),
        .branch => |br| {
            try computeRPOInner(gpa, br.else_block, visited, out);
            try computeRPOInner(gpa, br.then_block, visited, out);
        },
    }

    try out.append(gpa, block);
}

pub fn build(b: *Builder) Error!Bytecode {
    const entry = try b.createBlock();
    b.switchToBlock(entry);

    // Pre-create blocks for all labels
    for (b.ir.instructions.items(.tag), 0..) |tag, i| {
        if (!b.ir.liveness.isSet(i)) continue;
        if (tag != .label) continue;
        const index: Ir.Inst.Index = @enumFromInt(i);
        const block = try b.createBlock();
        try b.label_blocks.put(b.gpa, index.toRef(), block);
    }

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
            .reg_exp => try b.lowerRegExp(data.reg_exp, dest),
            .this => try b.lowerThis(dest),
            .label => try b.lowerLabel(dest, index.toRef()),
            .br => try b.lowerBr(data.br, dest),
            .br_cond => try b.lowerBrCond(data.br_cond, dest),
            .exception_handler => try b.lowerExceptionHandler(data.exception_handler, index.toRef()),
            .to_number => try b.lowerToNumber(data.ref, dest),
            .to_numeric => try b.lowerToNumeric(data.ref, dest),
            .to_string => try b.lowerToString(data.ref, dest),
            .to_object => try b.lowerToObject(data.ref, dest),
            .negate => try b.lowerNegate(data.ref, dest),
            .bitwise_not => try b.lowerBitwiseNot(data.ref, dest),
            .logical_not => try b.lowerLogicalNot(data.ref, dest),
            .typeof => try b.lowerTypeof(data.ref, dest),
            .typeof_binding => try b.lowerTypeofBinding(data.string, dest),
            .void => try b.lowerVoid(data.ref, dest),
            .delete => try b.lowerDelete(data.ref, dest),
            .spread => try b.lowerSpread(data.ref, dest),
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
            .push_scope => try b.lowerPushScope(),
            .push_var_scope => try b.lowerPushVarScope(),
            .push_with_scope => try b.lowerPushWithScope(data.ref),
            .pop_scope => try b.lowerPopScope(),
            .create_mutable_binding => try b.lowerCreateMutableBinding(data.string),
            .create_immutable_binding => try b.lowerCreateImmutableBinding(data.string),
            .initialize_binding => try b.lowerInitializeBinding(data.set_binding.name, data.set_binding.value, dest),
            .get_binding => try b.lowerGetBinding(data.string, dest),
            .get_property => try b.lowerGetProperty(data.get_property, dest),
            .get_property_computed => try b.lowerGetPropertyComputed(data.get_property_computed, dest),
            .get_property_indexed => try b.lowerGetPropertyIndexed(data.get_property_indexed, dest),
            .set_binding => try b.lowerSetBinding(data.set_binding.name, data.set_binding.value, false, dest),
            .set_binding_strict => try b.lowerSetBinding(data.set_binding.name, data.set_binding.value, true, dest),
            .set_property => try b.lowerSetProperty(data.set_property, false, dest),
            .set_property_strict => try b.lowerSetProperty(data.set_property, true, dest),
            .set_property_computed => try b.lowerSetPropertyComputed(data.set_property_computed, false, dest),
            .set_property_computed_strict => try b.lowerSetPropertyComputed(data.set_property_computed, true, dest),
            .set_property_indexed => try b.lowerSetPropertyIndexed(data.set_property_indexed, false, dest),
            .set_property_indexed_strict => try b.lowerSetPropertyIndexed(data.set_property_indexed, true, dest),
            .update_binding => try b.lowerUpdateBinding(data.update_binding.name, dest, data.update_binding.update_op, false),
            .update_binding_strict => try b.lowerUpdateBinding(data.update_binding.name, dest, data.update_binding.update_op, true),
            .update_property => try b.lowerUpdateProperty(data.update_property, dest, false),
            .update_property_strict => try b.lowerUpdateProperty(data.update_property, dest, true),
            .update_property_computed => try b.lowerUpdatePropertyComputed(data.update_property_computed, dest, false),
            .update_property_computed_strict => try b.lowerUpdatePropertyComputed(data.update_property_computed, dest, true),
            .update_property_indexed => try b.lowerUpdatePropertyIndexed(data.update_property_indexed, dest, false),
            .update_property_indexed_strict => try b.lowerUpdatePropertyIndexed(data.update_property_indexed, dest, true),
            .delete_binding => try b.lowerDeleteBinding(data.string, dest),
            .delete_property => try b.lowerDeleteProperty(data.delete_property, false, dest),
            .delete_property_strict => try b.lowerDeleteProperty(data.delete_property, true, dest),
            .delete_property_computed => try b.lowerDeletePropertyComputed(data.delete_property_computed, false, dest),
            .delete_property_computed_strict => try b.lowerDeletePropertyComputed(data.delete_property_computed, true, dest),
            .delete_property_indexed => try b.lowerDeletePropertyIndexed(data.delete_property_indexed, false, dest),
            .delete_property_indexed_strict => try b.lowerDeletePropertyIndexed(data.delete_property_indexed, true, dest),
            .copy_data_properties => try b.lowerCopyDataProperties(data.copy_data_properties, dest),
            .call => try b.lowerCall(data.call, dest),
            .call_direct_eval => try b.lowerCallDirectEval(data.call, dest, false),
            .call_direct_eval_strict => try b.lowerCallDirectEval(data.call, dest, true),
            .construct => try b.lowerConstruct(data.construct, dest),
            .get_template_object => try b.lowerGetTemplateObject(data.get_template_object, dest),
            .get_iterator => try b.lowerGetIterator(data.ref, dest),
            .get_async_iterator => try b.lowerGetAsyncIterator(data.ref, dest),
            .get_for_in_iterator => try b.lowerGetForInIterator(data.ref, dest),
            .iterator_step => try b.lowerIteratorStep(data.ref, dest),
            .iterator_step_value => try b.lowerIteratorStepValue(data.ref, dest),
            .iterator_step_value_async => try b.lowerIteratorStepValueAsync(data.ref, dest),
            .iterator_close => try b.lowerIteratorClose(data.ref),
            .iterator_is_done => try b.lowerIteratorIsDone(data.ref, dest),
            .iterator_collect => try b.lowerIteratorCollect(data.ref, dest),
            .throw => try b.lowerThrow(data.ref, dest),
            .throw_reference_error => try b.lowerThrowReferenceError(dest),
            .@"return" => try b.lowerReturn(data.ref, dest),
            .await => try b.lowerAwait(data.ref, dest),
            .yield => try b.lowerYield(data.ref, dest),
            .create_function => try b.lowerCreateFunction(data.create_function, dest),
            .create_class => try b.lowerCreateClass(data.create_class, dest),
            .create_unmapped_arguments_object => try b.lowerCreateUnmappedArgumentsObject(dest),
            .create_mapped_arguments_object => try b.lowerCreateMappedArgumentsObject(dest),
            .get_argument => try b.lowerGetArgument(data.argument, dest),
            .get_rest_arguments => try b.lowerGetRestArguments(data.argument, dest),
            .get_new_target => try b.lowerGetNewTarget(dest),
            .getter => try b.lowerGetter(data.ref, dest),
            .setter => try b.lowerSetter(data.ref, dest),
            .super_call => try b.lowerSuperCall(data.super_call, dest),
            .get_super_property => try b.lowerGetSuperProperty(data.string, dest),
            .get_super_property_computed => try b.lowerGetSuperPropertyComputed(data.ref, dest),
            .set_super_property => try b.lowerSetSuperProperty(data.set_property, false, dest),
            .set_super_property_strict => try b.lowerSetSuperProperty(data.set_property, true, dest),
            .set_super_property_computed => try b.lowerSetSuperPropertyComputed(data.set_property_computed, false, dest),
            .set_super_property_computed_strict => try b.lowerSetSuperPropertyComputed(data.set_property_computed, true, dest),
            .create_private_element => try b.lowerCreatePrivateElement(data.string, dest),
            .resolve_private_element => try b.lowerResolvePrivateElement(data.string, dest),
            .push_private_scope => try b.lowerPushPrivateScope(),
            .pop_private_scope => try b.lowerPopPrivateScope(),
            .get_private_element => try b.lowerGetPrivateElement(data.get_property, dest),
            .set_private_element => try b.lowerSetPrivateElement(data.set_property, dest),
            .has_private_element => try b.lowerHasPrivateElement(data.binary, dest),
            .import_call => try b.lowerImportCall(data.binary, dest),
            .get_import_meta => try b.lowerGetImportMeta(dest),
        }
    }

    std.debug.assert(b.terminated());

    // Order blocks in reverse post-order
    var ordered: std.ArrayList(*Block) = .empty;
    defer ordered.deinit(b.gpa);
    var visited: std.AutoHashMapUnmanaged(*Block, void) = .empty;
    defer visited.deinit(b.gpa);
    try computeRPO(b.gpa, b.blocks.items[0], &visited, &ordered);
    for (b.blocks.items) |block| {
        if (!visited.contains(block)) {
            try computeRPO(b.gpa, block, &visited, &ordered);
        }
    }

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

    var strings_list: std.ArrayList([]const u8) = try .initCapacity(b.gpa, b.ir.strings.len);
    defer strings_list.deinit(b.gpa);
    errdefer for (strings_list.items) |string| b.gpa.free(string);
    for (b.ir.strings) |string| {
        const cloned = try b.gpa.dupe(u8, string);
        strings_list.appendAssumeCapacity(cloned);
    }
    const strings = try strings_list.toOwnedSlice(b.gpa);
    errdefer {
        for (strings) |string| b.gpa.free(string);
        b.gpa.free(strings);
    }

    var big_ints_list: std.ArrayList(std.math.big.int.Const) = try .initCapacity(b.gpa, b.ir.big_ints.len);
    defer big_ints_list.deinit(b.gpa);
    errdefer for (big_ints_list.items) |big_int| b.gpa.free(big_int.limbs);
    for (b.ir.big_ints) |big_int| {
        const cloned: std.math.big.int.Const = .{
            .limbs = try b.gpa.dupe(std.math.big.Limb, big_int.limbs),
            .positive = big_int.positive,
        };
        big_ints_list.appendAssumeCapacity(cloned);
    }
    const big_ints = try big_ints_list.toOwnedSlice(b.gpa);
    errdefer {
        for (big_ints) |big_int| b.gpa.free(big_int.limbs);
        b.gpa.free(big_ints);
    }

    var functions_list: std.ArrayList(Bytecode.Function) = try .initCapacity(b.gpa, b.ir.functions.len);
    defer functions_list.deinit(b.gpa);
    for (b.ir.functions) |function| {
        functions_list.appendAssumeCapacity(.{
            .source_text = @enumFromInt(@intFromEnum(function.source_text)),
            .name = switch (function.name) {
                .none => .none,
                .identifier => |s| .{ .identifier = @enumFromInt(@intFromEnum(s)) },
                .default => |s| .{ .default = @enumFromInt(@intFromEnum(s)) },
            },
            .parameters = function.parameters,
            .body = function.body,
            .kind = @enumFromInt(@intFromEnum(function.kind)),
        });
    }
    const functions = try functions_list.toOwnedSlice(b.gpa);
    errdefer b.gpa.free(functions);

    var classes_list: std.ArrayList(Bytecode.Class) = try .initCapacity(b.gpa, b.ir.classes.len);
    defer classes_list.deinit(b.gpa);
    for (b.ir.classes) |class| {
        const element_name_regs = try b.gpa.alloc(Bytecode.Inst.Reg, class.element_names.len);
        for (class.element_names, element_name_regs) |name_ref, *reg| {
            reg.* = switch (name_ref) {
                .none => .none,
                else => b.resolve(name_ref),
            };
        }
        classes_list.appendAssumeCapacity(.{
            .source_text = @enumFromInt(@intFromEnum(class.source_text)),
            .name = switch (class.name) {
                .none => .none,
                .identifier => |s| .{ .identifier = @enumFromInt(@intFromEnum(s)) },
                .default => |s| .{ .default = @enumFromInt(@intFromEnum(s)) },
            },
            .class_tail = class.class_tail,
            .heritage = switch (class.heritage) {
                .none => .none,
                else => b.resolve(class.heritage),
            },
            .element_names = element_name_regs,
        });
    }
    const classes = try classes_list.toOwnedSlice(b.gpa);
    errdefer {
        for (classes) |class| b.gpa.free(class.element_names);
        b.gpa.free(classes);
    }

    var exception_handlers_list: std.ArrayList(Bytecode.ExceptionHandler) = try .initCapacity(b.gpa, b.exception_handlers.items.len);
    defer exception_handlers_list.deinit(b.gpa);
    for (b.exception_handlers.items) |handler| {
        exception_handlers_list.appendAssumeCapacity(.{
            .start = handler.start.offset,
            .end = handler.end.offset,
            .target = handler.target.offset,
            .exception_reg = handler.exception_reg,
        });
    }
    const exception_handlers = try exception_handlers_list.toOwnedSlice(b.gpa);
    errdefer b.gpa.free(exception_handlers);

    return .{
        .name = name,
        .code = code,
        .num_regs = b.lsra.numRegs(),
        .strings = strings,
        .big_ints = big_ints,
        .functions = functions,
        .classes = classes,
        .exception_handlers = exception_handlers,
    };
}

const Block = struct {
    instructions: std.ArrayList(Bytecode.Inst),
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
        for (block.instructions.items) |inst| {
            total += Bytecode.Inst.encodedSize(inst.tag);
        }
        return total;
    }

    fn terminatorSize(block: *const Block, next: ?*const Block) u32 {
        return switch (block.terminator) {
            .none => unreachable,
            .noreturn => 0,
            .jump => |target| if (target == next) 0 else Bytecode.Inst.encodedSize(.jump),
            .branch => |br| blk: {
                const jump_cond_tag: Bytecode.Inst.Tag = switch (br.condition) {
                    .truthy => .jump_if_true,
                    .falsy => .jump_if_false,
                };
                const jump_cond_size: u32 = Bytecode.Inst.encodedSize(jump_cond_tag);
                const jump_size: u32 = if (br.else_block == next) 0 else Bytecode.Inst.encodedSize(.jump);
                break :blk jump_cond_size + jump_size;
            },
        };
    }

    fn encode(block: *const Block, writer: *std.Io.Writer, next: ?*const Block) std.Io.Writer.Error!void {
        for (block.instructions.items) |inst| {
            try inst.encode(writer);
        }

        const jump_size = comptime Bytecode.Inst.encodedSize(.jump);
        const jump_cond_size = comptime Bytecode.Inst.encodedSize(.jump_if_true);

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

fn emitUnaryOp(
    b: *Builder,
    tag: Bytecode.Inst.Tag,
    operand: Ir.Inst.Ref,
    dest: Bytecode.Inst.Reg,
) Error!void {
    const operand_reg = b.resolve(operand);
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg = .{ dest, operand_reg } },
    });
}

fn emitBinaryOp(
    b: *Builder,
    tag: Bytecode.Inst.Tag,
    lhs: Ir.Inst.Ref,
    rhs: Ir.Inst.Ref,
    dest: Bytecode.Inst.Reg,
) Error!void {
    const lhs_reg = b.resolve(lhs);
    const rhs_reg = b.resolve(rhs);
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_reg = .{ dest, lhs_reg, rhs_reg } },
    });
}

fn emitMoveIfNeeded(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const src = switch (ref) {
        .none => return,
        else => b.resolve(ref),
    };
    if (src != dest) {
        try b.emit(.{
            .tag = .move,
            .data = .{ .reg_reg = .{ dest, src } },
        });
    }
}

fn emitArgumentsArray(b: *Builder, args: []const Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .array_create,
        .data = .{ .reg_u32 = .{ dest, 0 } },
    });
    for (args) |arg| {
        const tag: Bytecode.Inst.Tag = blk: {
            if (arg.toIndex()) |arg_index| {
                if (b.ir.instructions.items(.tag)[@intFromEnum(arg_index)] == .spread) {
                    break :blk .array_spread;
                }
            }
            break :blk .array_push;
        };
        // `lowerSpread()` will move the ref into dest, so we use the same register here.
        const arg_reg = b.resolve(arg);
        try b.emit(.{
            .tag = tag,
            .data = .{ .reg_reg = .{
                dest,
                arg_reg,
            } },
        });
    }
}

fn resolve(b: *Builder, ref: Ir.Inst.Ref) Bytecode.Inst.Reg {
    const index = ref.toIndex().?;
    const reg = b.lsra.allocations[@intFromEnum(index)];
    std.debug.assert(reg != .none); // Live instructions must have allocations
    return reg;
}

fn lowerUndefined(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_undefined,
        .data = .{ .reg = dest },
    });
}

fn lowerNull(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_null,
        .data = .{ .reg = dest },
    });
}

fn lowerTrue(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_true,
        .data = .{ .reg = dest },
    });
}

fn lowerFalse(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_false,
        .data = .{ .reg = dest },
    });
}

fn lowerZero(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_number_i32,
        .data = .{ .reg_i32 = .{ dest, 0 } },
    });
}

fn lowerOne(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_number_i32,
        .data = .{ .reg_i32 = .{ dest, 1 } },
    });
}

fn lowerNumber(b: *Builder, n: f64, dest: Bytecode.Inst.Reg) Error!void {
    if (n == @floor(n) and n >= std.math.minInt(i32) and n <= std.math.maxInt(i32) and !std.math.isNegativeZero(n)) {
        try b.emit(.{
            .tag = .load_number_i32,
            .data = .{ .reg_i32 = .{ dest, @intFromFloat(n) } },
        });
    } else {
        try b.emit(.{
            .tag = .load_number_f64,
            .data = .{ .reg_f64 = .{ dest, n } },
        });
    }
}

fn lowerString(b: *Builder, string: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    const string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string));
    try b.emit(.{
        .tag = .load_string,
        .data = .{
            .reg_string = .{ dest, string_index },
        },
    });
}

fn lowerBigInt(b: *Builder, big_int: Ir.Inst.BigIntIndex, dest: Bytecode.Inst.Reg) Error!void {
    const big_int_index: Bytecode.Inst.BigIntIndex = @enumFromInt(@intFromEnum(big_int));
    try b.emit(.{
        .tag = .load_big_int,
        .data = .{
            .reg_big_int = .{ dest, big_int_index },
        },
    });
}

fn lowerArray(b: *Builder, data: Ir.Inst.Array, dest: Bytecode.Inst.Reg) Error!void {
    const elements = b.ir.refSlice(data.extra_index, data.len);

    const has_spread = for (elements) |elem| {
        if (elem.toIndex()) |elem_index| {
            if (b.ir.instructions.items(.tag)[@intFromEnum(elem_index)] == .spread) {
                break true;
            }
        }
    } else false;

    if (has_spread) {
        try b.emit(.{
            .tag = .array_create,
            .data = .{
                .reg_u32 = .{ dest, 0 },
            },
        });

        for (elements) |elem| {
            if (elem == .none) {
                try b.emit(.{
                    .tag = .array_push_hole,
                    .data = .{ .reg = dest },
                });
                continue;
            }
            const elem_index = elem.toIndex().?;
            const elem_tag = b.ir.instructions.items(.tag)[@intFromEnum(elem_index)];
            const tag: Bytecode.Inst.Tag = switch (elem_tag) {
                .spread => .array_spread,
                else => .array_push,
            };
            const elem_reg = b.resolve(elem);
            try b.emit(.{
                .tag = tag,
                .data = .{ .reg_reg = .{
                    dest,
                    elem_reg,
                } },
            });
        }
    } else {
        try b.emit(.{
            .tag = .array_create,
            .data = .{ .reg_u32 = .{
                dest,
                data.len,
            } },
        });

        for (elements, 0..) |elem, i| {
            if (elem == .none) continue; // Skip elisions
            const elem_reg = b.resolve(elem);
            try b.emit(.{
                .tag = .array_set,
                .data = .{ .reg_reg_u32 = .{
                    dest,
                    elem_reg,
                    @intCast(i),
                } },
            });
        }
    }
}

fn lowerObject(b: *Builder, data: Ir.Inst.Object, dest: Bytecode.Inst.Reg) Error!void {
    const pairs = b.ir.refSlice(data.extra_index, data.len * 2);

    try b.emit(.{
        .tag = .object_create,
        .data = .{ .reg = dest },
    });

    var i: usize = 0;
    while (i < pairs.len) : (i += 2) {
        const key_ref = pairs[i];
        const value_ref = pairs[i + 1];
        const value_index = value_ref.toIndex().?;
        const value_inst = b.ir.instructions.get(@intFromEnum(value_index));

        const value_reg = b.resolve(value_ref);

        if (key_ref == .none) {
            const tag: Bytecode.Inst.Tag = if (value_inst.tag == .spread)
                .object_spread
            else
                .object_set_prototype;
            try b.emit(.{
                .tag = tag,
                .data = .{ .reg_reg = .{
                    dest,
                    value_reg,
                } },
            });
            continue;
        }

        const key_index = key_ref.toIndex().?;
        const key_inst = b.ir.instructions.get(@intFromEnum(key_index));

        switch (value_inst.tag) {
            .getter, .setter => {
                try b.emit(.{
                    .tag = .set_home_object,
                    .data = .{ .reg_reg = .{
                        value_reg,
                        dest,
                    } },
                });

                const set_tag: Bytecode.Inst.Tag = if (key_inst.tag == .string)
                    (if (value_inst.tag == .getter) .object_set_getter else .object_set_setter)
                else
                    (if (value_inst.tag == .getter) .object_set_getter_computed else .object_set_setter_computed);

                if (key_inst.tag == .string) {
                    const string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(key_inst.data.string));
                    try b.emit(.{
                        .tag = set_tag,
                        .data = .{ .reg_string_reg = .{
                            dest,
                            string_index,
                            value_reg,
                        } },
                    });
                } else {
                    const key_reg = b.resolve(key_ref);
                    try b.emit(.{
                        .tag = set_tag,
                        .data = .{ .reg_reg_reg = .{
                            dest,
                            key_reg,
                            value_reg,
                        } },
                    });
                }
                continue;
            },
            .create_function => {
                try b.emit(.{
                    .tag = .set_home_object,
                    .data = .{ .reg_reg = .{
                        value_reg,
                        dest,
                    } },
                });
            },
            else => {},
        }

        if (key_inst.tag == .string) {
            const string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(key_inst.data.string));
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

fn lowerRegExp(b: *Builder, data: Ir.Inst.RegExp, dest: Bytecode.Inst.Reg) Error!void {
    const pattern_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(data.pattern));
    const flags_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(data.flags));
    try b.emit(.{
        .tag = .reg_exp_create,
        .data = .{ .reg_string_string = .{
            dest,
            pattern_index,
            flags_index,
        } },
    });
}

fn lowerThis(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .resolve_this_binding,
        .data = .{ .reg = dest },
    });
}

fn lowerLabel(b: *Builder, dest: Bytecode.Inst.Reg, label_ref: Ir.Inst.Ref) Error!void {
    _ = dest;
    const label_block = b.label_blocks.get(label_ref).?;
    if (!b.terminated()) {
        b.jump(label_block);
    }
    b.switchToBlock(label_block);
}

fn lowerBr(b: *Builder, data: Ir.Inst.Br, dest: Bytecode.Inst.Reg) Error!void {
    _ = dest;
    const target_reg = b.resolve(data.target);
    try b.emitMoveIfNeeded(data.value, target_reg);
    const target_block = b.label_blocks.get(data.target).?;
    b.jump(target_block);
}

fn lowerBrCond(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    _ = dest;
    const extra = b.ir.extraData(Ir.Inst.BrCond, data);
    const then_block = b.label_blocks.get(extra.data.then_target).?;
    const else_block = b.label_blocks.get(extra.data.else_target).?;
    const cond_reg = b.resolve(extra.data.condition);
    b.branch(.truthy, cond_reg, then_block, else_block);
}

fn lowerExceptionHandler(b: *Builder, exception_handler: Ir.Inst.ExtraIndex, exception_ref: Ir.Inst.Ref) Error!void {
    const extra = b.ir.extraData(Ir.Inst.ExceptionHandler, exception_handler).data;
    const start_block = b.label_blocks.get(extra.start).?;
    const end_block = b.label_blocks.get(extra.end).?;
    const target_block = b.label_blocks.get(extra.target).?;
    try b.exception_handlers.append(b.gpa, .{
        .start = start_block,
        .end = end_block,
        .target = target_block,
        .exception_reg = b.resolve(exception_ref),
    });
}

fn lowerToNumber(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.to_number, ref, dest);
}

fn lowerToNumeric(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.to_numeric, ref, dest);
}

fn lowerToString(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.to_string, ref, dest);
}

fn lowerToObject(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.to_object, ref, dest);
}

fn lowerNegate(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.negate, ref, dest);
}

fn lowerBitwiseNot(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.bitwise_not, ref, dest);
}

fn lowerLogicalNot(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.logical_not, ref, dest);
}

fn lowerTypeof(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitUnaryOp(.typeof, ref, dest);
}

fn lowerTypeofBinding(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    try b.emit(.{
        .tag = .typeof_binding,
        .data = .{ .reg_string = .{
            dest,
            bytecode_string_index,
        } },
    });
}

fn lowerVoid(b: *Builder, _: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_undefined,
        .data = .{ .reg = dest },
    });
}

fn lowerDelete(b: *Builder, _: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .load_true,
        .data = .{ .reg = dest },
    });
}

fn lowerSpread(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitMoveIfNeeded(ref, dest);
}

fn lowerAdd(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.add, data.lhs, data.rhs, dest);
}

fn lowerSub(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.sub, data.lhs, data.rhs, dest);
}

fn lowerMul(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.mul, data.lhs, data.rhs, dest);
}

fn lowerDiv(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.div, data.lhs, data.rhs, dest);
}

fn lowerRem(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.rem, data.lhs, data.rhs, dest);
}

fn lowerExp(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.exp, data.lhs, data.rhs, dest);
}

fn lowerShiftLeft(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.shift_left, data.lhs, data.rhs, dest);
}

fn lowerShiftRight(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.shift_right, data.lhs, data.rhs, dest);
}

fn lowerShiftRightUnsigned(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.shift_right_unsigned, data.lhs, data.rhs, dest);
}

fn lowerBitwiseAnd(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.bitwise_and, data.lhs, data.rhs, dest);
}

fn lowerBitwiseOr(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.bitwise_or, data.lhs, data.rhs, dest);
}

fn lowerBitwiseXor(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.bitwise_xor, data.lhs, data.rhs, dest);
}

fn lowerLt(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.lt, data.lhs, data.rhs, dest);
}

fn lowerGt(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.gt, data.lhs, data.rhs, dest);
}

fn lowerLtEq(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.lt_eq, data.lhs, data.rhs, dest);
}

fn lowerGtEq(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.gt_eq, data.lhs, data.rhs, dest);
}

fn lowerInstanceof(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.instanceof, data.lhs, data.rhs, dest);
}

fn lowerIn(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.in, data.lhs, data.rhs, dest);
}

fn lowerEq(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.eq, data.lhs, data.rhs, dest);
}

fn lowerNotEq(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.not_eq, data.lhs, data.rhs, dest);
}

fn lowerEqStrict(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.eq_strict, data.lhs, data.rhs, dest);
}

fn lowerNotEqStrict(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.not_eq_strict, data.lhs, data.rhs, dest);
}

fn lowerPushScope(b: *Builder) Error!void {
    try b.emit(.{
        .tag = .push_scope,
        .data = .{ .none = {} },
    });
}

fn lowerPushVarScope(b: *Builder) Error!void {
    try b.emit(.{
        .tag = .push_var_scope,
        .data = .{ .none = {} },
    });
}

fn lowerPushWithScope(b: *Builder, ref: Ir.Inst.Ref) Error!void {
    const object_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .push_with_scope,
        .data = .{ .reg = object_reg },
    });
}

fn lowerPopScope(b: *Builder) Error!void {
    try b.emit(.{
        .tag = .pop_scope,
        .data = .{ .none = {} },
    });
}

fn lowerCreateMutableBinding(b: *Builder, string_index: Ir.Inst.StringIndex) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    try b.emit(.{
        .tag = .create_mutable_binding,
        .data = .{ .string = bytecode_string_index },
    });
}

fn lowerCreateImmutableBinding(b: *Builder, string_index: Ir.Inst.StringIndex) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    try b.emit(.{
        .tag = .create_immutable_binding,
        .data = .{ .string = bytecode_string_index },
    });
}

fn lowerInitializeBinding(b: *Builder, string_index: Ir.Inst.StringIndex, value: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    const value_reg = b.resolve(value);
    try b.emit(.{
        .tag = .initialize_binding,
        .data = .{ .string_reg = .{
            bytecode_string_index,
            value_reg,
        } },
    });
    try b.emitMoveIfNeeded(value, dest);
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

fn lowerGetProperty(b: *Builder, data: Ir.Inst.GetProperty, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(data.name));
    try b.emit(.{
        .tag = .get_property,
        .data = .{ .reg_reg_string = .{
            dest,
            base_reg,
            bytecode_string_index,
        } },
    });
}

fn lowerGetPropertyComputed(b: *Builder, data: Ir.Inst.GetPropertyComputed, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    const property_reg = b.resolve(data.property);
    try b.emit(.{
        .tag = .get_property_computed,
        .data = .{ .reg_reg_reg = .{
            dest,
            base_reg,
            property_reg,
        } },
    });
}

fn lowerGetPropertyIndexed(b: *Builder, data: Ir.Inst.GetPropertyIndexed, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    try b.emit(.{
        .tag = .get_property_indexed,
        .data = .{ .reg_reg_u32 = .{
            dest,
            base_reg,
            data.index,
        } },
    });
}

fn lowerSetBinding(b: *Builder, string_index: Ir.Inst.StringIndex, value: Ir.Inst.Ref, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    const value_reg = b.resolve(value);
    const tag: Bytecode.Inst.Tag = if (strict)
        .set_binding_strict
    else
        .set_binding;
    try b.emit(.{
        .tag = tag,
        .data = .{ .string_reg = .{
            bytecode_string_index,
            value_reg,
        } },
    });
    try b.emitMoveIfNeeded(value, dest);
}

fn lowerSetProperty(b: *Builder, data: Ir.Inst.ExtraIndex, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SetProperty, data);
    const base_reg = b.resolve(extra.data.base);
    const value_reg = b.resolve(extra.data.value);
    const tag: Bytecode.Inst.Tag = if (strict)
        .set_property_strict
    else
        .set_property;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_string = .{
            base_reg,
            value_reg,
            @enumFromInt(@intFromEnum(extra.data.name)),
        } },
    });
    try b.emitMoveIfNeeded(extra.data.value, dest);
}

fn lowerSetPropertyComputed(b: *Builder, data: Ir.Inst.ExtraIndex, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SetPropertyComputed, data);
    const base_reg = b.resolve(extra.data.base);
    const property_reg = b.resolve(extra.data.property);
    const value_reg = b.resolve(extra.data.value);
    const tag: Bytecode.Inst.Tag = if (strict)
        .set_property_computed_strict
    else
        .set_property_computed;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_reg = .{
            base_reg,
            property_reg,
            value_reg,
        } },
    });
    try b.emitMoveIfNeeded(extra.data.value, dest);
}

fn lowerSetPropertyIndexed(b: *Builder, data: Ir.Inst.ExtraIndex, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SetPropertyIndexed, data);
    const base_reg = b.resolve(extra.data.base);
    const value_reg = b.resolve(extra.data.value);
    const tag: Bytecode.Inst.Tag = if (strict)
        .set_property_indexed_strict
    else
        .set_property_indexed;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_u32 = .{
            base_reg,
            value_reg,
            extra.data.index,
        } },
    });
    try b.emitMoveIfNeeded(extra.data.value, dest);
}

fn lowerUpdateBinding(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg, update_op: Ir.Inst.UpdateOp, strict: bool) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    const tag: Bytecode.Inst.Tag = switch (update_op) {
        .increment_prefix => if (strict) .increment_binding_prefix_strict else .increment_binding_prefix,
        .increment_postfix => if (strict) .increment_binding_postfix_strict else .increment_binding_postfix,
        .decrement_prefix => if (strict) .decrement_binding_prefix_strict else .decrement_binding_prefix,
        .decrement_postfix => if (strict) .decrement_binding_postfix_strict else .decrement_binding_postfix,
    };
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_string = .{
            dest,
            bytecode_string_index,
        } },
    });
}

fn lowerUpdateProperty(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg, strict: bool) Error!void {
    const extra = b.ir.extraData(Ir.Inst.UpdateProperty, data);
    const base_reg = b.resolve(extra.data.base);
    const tag: Bytecode.Inst.Tag = switch (extra.data.update_op) {
        .increment_prefix => if (strict) .increment_property_prefix_strict else .increment_property_prefix,
        .increment_postfix => if (strict) .increment_property_postfix_strict else .increment_property_postfix,
        .decrement_prefix => if (strict) .decrement_property_prefix_strict else .decrement_property_prefix,
        .decrement_postfix => if (strict) .decrement_property_postfix_strict else .decrement_property_postfix,
    };
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_string = .{
            dest,
            base_reg,
            @enumFromInt(@intFromEnum(extra.data.name)),
        } },
    });
}

fn lowerUpdatePropertyComputed(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg, strict: bool) Error!void {
    const extra = b.ir.extraData(Ir.Inst.UpdatePropertyComputed, data);
    const base_reg = b.resolve(extra.data.base);
    const property_reg = b.resolve(extra.data.property);
    const tag: Bytecode.Inst.Tag = switch (extra.data.update_op) {
        .increment_prefix => if (strict) .increment_property_computed_prefix_strict else .increment_property_computed_prefix,
        .increment_postfix => if (strict) .increment_property_computed_postfix_strict else .increment_property_computed_postfix,
        .decrement_prefix => if (strict) .decrement_property_computed_prefix_strict else .decrement_property_computed_prefix,
        .decrement_postfix => if (strict) .decrement_property_computed_postfix_strict else .decrement_property_computed_postfix,
    };
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_reg = .{
            dest,
            base_reg,
            property_reg,
        } },
    });
}

fn lowerUpdatePropertyIndexed(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg, strict: bool) Error!void {
    const extra = b.ir.extraData(Ir.Inst.UpdatePropertyIndexed, data);
    const base_reg = b.resolve(extra.data.base);
    const tag: Bytecode.Inst.Tag = switch (extra.data.update_op) {
        .increment_prefix => if (strict) .increment_property_indexed_prefix_strict else .increment_property_indexed_prefix,
        .increment_postfix => if (strict) .increment_property_indexed_postfix_strict else .increment_property_indexed_postfix,
        .decrement_prefix => if (strict) .decrement_property_indexed_prefix_strict else .decrement_property_indexed_prefix,
        .decrement_postfix => if (strict) .decrement_property_indexed_postfix_strict else .decrement_property_indexed_postfix,
    };
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_u32 = .{
            dest,
            base_reg,
            extra.data.index,
        } },
    });
}

fn lowerDeleteBinding(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_string_index: Bytecode.Inst.StringIndex = @enumFromInt(@intFromEnum(string_index));
    try b.emit(.{
        .tag = .delete_binding,
        .data = .{ .reg_string = .{
            dest,
            bytecode_string_index,
        } },
    });
}

fn lowerDeleteProperty(b: *Builder, data: Ir.Inst.DeleteProperty, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    const tag: Bytecode.Inst.Tag = if (strict)
        .delete_property_strict
    else
        .delete_property;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_string = .{
            dest,
            base_reg,
            @enumFromInt(@intFromEnum(data.name)),
        } },
    });
}

fn lowerDeletePropertyComputed(b: *Builder, data: Ir.Inst.DeletePropertyComputed, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    const property_reg = b.resolve(data.property);
    const tag: Bytecode.Inst.Tag = if (strict)
        .delete_property_computed_strict
    else
        .delete_property_computed;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_reg = .{
            dest,
            base_reg,
            property_reg,
        } },
    });
}

fn lowerDeletePropertyIndexed(b: *Builder, data: Ir.Inst.DeletePropertyIndexed, strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    const tag: Bytecode.Inst.Tag = if (strict)
        .delete_property_indexed_strict
    else
        .delete_property_indexed;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg_u32 = .{
            dest,
            base_reg,
            data.index,
        } },
    });
}

fn lowerCopyDataProperties(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.CopyDataProperties, data);
    const source_reg = b.resolve(extra.data.source);
    const excluded = b.ir.refSlice(extra.end, extra.data.excluded_len);

    if (excluded.len == 0) {
        try b.emit(.{
            .tag = .copy_data_properties,
            .data = .{ .reg_reg_reg = .{
                dest,
                source_reg,
                .none,
            } },
        });
        return;
    }

    const excluded_reg = try b.lsra.allocateTemp(b.gpa);
    defer b.lsra.freeTemp(excluded_reg);
    try b.emit(.{
        .tag = .array_create,
        .data = .{ .reg_u32 = .{
            excluded_reg,
            0,
        } },
    });
    for (excluded) |prop| {
        const prop_reg = b.resolve(prop);
        try b.emit(.{
            .tag = .array_push,
            .data = .{ .reg_reg = .{
                excluded_reg,
                prop_reg,
            } },
        });
    }

    try b.emit(.{
        .tag = .copy_data_properties,
        .data = .{ .reg_reg_reg = .{
            dest,
            source_reg,
            excluded_reg,
        } },
    });
}

fn lowerCall(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.Call, data);
    const callee_reg = b.resolve(extra.data.callee);
    const this_reg = switch (extra.data.this_value) {
        .none => Bytecode.Inst.Reg.none,
        else => |ref| b.resolve(ref),
    };
    const args = b.ir.refSlice(extra.end, extra.data.args_len);

    const has_spread = for (args) |arg| {
        if (arg.toIndex()) |arg_index| {
            if (b.ir.instructions.items(.tag)[@intFromEnum(arg_index)] == .spread) {
                break true;
            }
        }
    } else false;

    if (args.len <= 2 and !has_spread) {
        try b.emit(switch (this_reg) {
            .none => switch (args.len) {
                0 => .{ .tag = .call0, .data = .{ .reg_reg = .{ dest, callee_reg } } },
                1 => .{ .tag = .call1, .data = .{ .reg_reg_reg = .{ dest, callee_reg, b.resolve(args[0]) } } },
                2 => .{ .tag = .call2, .data = .{ .reg_reg_reg_reg = .{ dest, callee_reg, b.resolve(args[0]), b.resolve(args[1]) } } },
                else => unreachable,
            },
            else => switch (args.len) {
                0 => .{ .tag = .call_property0, .data = .{ .reg_reg_reg = .{ dest, callee_reg, this_reg } } },
                1 => .{ .tag = .call_property1, .data = .{ .reg_reg_reg_reg = .{ dest, callee_reg, this_reg, b.resolve(args[0]) } } },
                2 => .{ .tag = .call_property2, .data = .{ .reg_reg_reg_reg_reg = .{ dest, callee_reg, this_reg, b.resolve(args[0]), b.resolve(args[1]) } } },
                else => unreachable,
            },
        });
        return;
    }

    const args_reg = try b.lsra.allocateTemp(b.gpa);
    defer b.lsra.freeTemp(args_reg);
    try b.emitArgumentsArray(args, args_reg);

    switch (extra.data.this_value) {
        .none => try b.emit(.{
            .tag = .call,
            .data = .{ .reg_reg_reg = .{
                dest,
                callee_reg,
                args_reg,
            } },
        }),
        else => try b.emit(.{
            .tag = .call_property,
            .data = .{ .reg_reg_reg_reg = .{
                dest,
                callee_reg,
                this_reg,
                args_reg,
            } },
        }),
    }
}

fn lowerCallDirectEval(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg, strict: bool) Error!void {
    const extra = b.ir.extraData(Ir.Inst.Call, data);
    const callee_reg = b.resolve(extra.data.callee);
    const args = b.ir.refSlice(extra.end, extra.data.args_len);

    const args_reg = try b.lsra.allocateTemp(b.gpa);
    defer b.lsra.freeTemp(args_reg);
    try b.emitArgumentsArray(args, args_reg);

    try b.emit(.{
        .tag = if (strict) .call_direct_eval_strict else .call_direct_eval,
        .data = .{ .reg_reg_reg = .{
            dest,
            callee_reg,
            args_reg,
        } },
    });
}

fn lowerConstruct(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.Construct, data);
    const constructor_reg = b.resolve(extra.data.constructor);
    const args = b.ir.refSlice(extra.end, extra.data.args_len);

    const has_spread = for (args) |arg| {
        if (arg.toIndex()) |arg_index| {
            if (b.ir.instructions.items(.tag)[@intFromEnum(arg_index)] == .spread) {
                break true;
            }
        }
    } else false;

    if (args.len <= 2 and !has_spread) {
        try b.emit(switch (args.len) {
            0 => .{ .tag = .construct0, .data = .{ .reg_reg = .{ dest, constructor_reg } } },
            1 => .{ .tag = .construct1, .data = .{ .reg_reg_reg = .{ dest, constructor_reg, b.resolve(args[0]) } } },
            2 => .{ .tag = .construct2, .data = .{ .reg_reg_reg_reg = .{ dest, constructor_reg, b.resolve(args[0]), b.resolve(args[1]) } } },
            else => unreachable,
        });
        return;
    }

    const args_reg = try b.lsra.allocateTemp(b.gpa);
    defer b.lsra.freeTemp(args_reg);
    try b.emitArgumentsArray(args, args_reg);

    try b.emit(.{
        .tag = .construct,
        .data = .{ .reg_reg_reg = .{
            dest,
            constructor_reg,
            args_reg,
        } },
    });
}

fn lowerGetTemplateObject(b: *Builder, extra_index: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.GetTemplateObject, extra_index);
    const cooked_reg = b.resolve(extra.data.cooked);
    const raw_reg = b.resolve(extra.data.raw);

    try b.emit(.{
        .tag = .get_template_object,
        .data = .{ .reg_reg_reg_u16 = .{
            dest,
            cooked_reg,
            raw_reg,
            @intCast(extra.data.id),
        } },
    });
}

fn lowerGetIterator(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const value_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .get_iterator,
        .data = .{ .reg_reg = .{
            dest,
            value_reg,
        } },
    });
}

fn lowerGetAsyncIterator(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const value_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .get_async_iterator,
        .data = .{ .reg_reg = .{
            dest,
            value_reg,
        } },
    });
}

fn lowerGetForInIterator(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const value_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .get_for_in_iterator,
        .data = .{ .reg_reg = .{
            dest,
            value_reg,
        } },
    });
}

fn lowerIteratorStep(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const iterator_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .iterator_step,
        .data = .{ .reg_reg = .{
            dest,
            iterator_reg,
        } },
    });
}

fn lowerIteratorStepValue(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const iterator_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .iterator_step_value,
        .data = .{ .reg_reg = .{
            dest,
            iterator_reg,
        } },
    });
}

fn lowerIteratorStepValueAsync(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const iterator_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .iterator_step_value_async,
        .data = .{ .reg_reg = .{
            dest,
            iterator_reg,
        } },
    });
}

fn lowerIteratorClose(b: *Builder, ref: Ir.Inst.Ref) Error!void {
    const iterator_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .iterator_close,
        .data = .{ .reg = iterator_reg },
    });
}

fn lowerIteratorIsDone(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const iterator_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .iterator_is_done,
        .data = .{ .reg_reg = .{
            dest,
            iterator_reg,
        } },
    });
}

fn lowerIteratorCollect(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const iterator_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .iterator_collect,
        .data = .{ .reg_reg = .{
            dest,
            iterator_reg,
        } },
    });
}

fn lowerThrow(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    _ = dest;
    const value_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .throw,
        .data = .{ .reg = value_reg },
    });
    b.noreturn();
}

fn lowerThrowReferenceError(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    _ = dest;
    try b.emit(.{
        .tag = .throw_reference_error,
        .data = .{ .none = {} },
    });
    b.noreturn();
}

fn lowerReturn(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    _ = dest;
    const ret_reg = if (ref == .none) Bytecode.Inst.Reg.none else b.resolve(ref);
    try b.emit(.{
        .tag = .@"return",
        .data = .{ .reg = ret_reg },
    });
    b.noreturn();
}

fn lowerAwait(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitMoveIfNeeded(ref, dest);
    try b.emit(.{
        .tag = .await,
        .data = .{ .reg = dest },
    });
}

fn lowerYield(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    if (ref == .none) {
        try b.emit(.{
            .tag = .yield,
            .data = .{ .reg = .none },
        });
    } else {
        try b.emitMoveIfNeeded(ref, dest);
        try b.emit(.{
            .tag = .yield,
            .data = .{ .reg = dest },
        });
    }
}

fn lowerCreateFunction(b: *Builder, function_index: Ir.Inst.FunctionIndex, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_function_index: Bytecode.Inst.FunctionIndex = @enumFromInt(@intFromEnum(function_index));
    try b.emit(.{
        .tag = .create_function,
        .data = .{ .reg_function = .{
            dest,
            bytecode_function_index,
        } },
    });
}

fn lowerCreateClass(b: *Builder, class_index: Ir.Inst.ClassIndex, dest: Bytecode.Inst.Reg) Error!void {
    const bytecode_class_index: Bytecode.Inst.ClassIndex = @enumFromInt(@intFromEnum(class_index));
    try b.emit(.{
        .tag = .create_class,
        .data = .{ .reg_class = .{
            dest,
            bytecode_class_index,
        } },
    });
}

fn lowerCreateUnmappedArgumentsObject(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .create_unmapped_arguments_object,
        .data = .{ .reg = dest },
    });
}

fn lowerCreateMappedArgumentsObject(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .create_mapped_arguments_object,
        .data = .{ .reg = dest },
    });
}

fn lowerGetArgument(b: *Builder, arg_index: u16, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .get_argument,
        .data = .{ .reg_u16 = .{ dest, arg_index } },
    });
}

fn lowerGetRestArguments(b: *Builder, start_index: u16, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .get_rest_arguments,
        .data = .{ .reg_u16 = .{ dest, start_index } },
    });
}

fn lowerGetNewTarget(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .get_new_target,
        .data = .{ .reg = dest },
    });
}

fn lowerGetter(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitMoveIfNeeded(ref, dest);
}

fn lowerSetter(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitMoveIfNeeded(ref, dest);
}

fn lowerSuperCall(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SuperCall, data);
    const args = b.ir.refSlice(extra.end, extra.data.args_len);

    const args_reg = try b.lsra.allocateTemp(b.gpa);
    defer b.lsra.freeTemp(args_reg);
    try b.emitArgumentsArray(args, args_reg);

    try b.emit(.{
        .tag = .super_call,
        .data = .{ .reg_reg = .{
            dest,
            args_reg,
        } },
    });
}

fn lowerGetSuperProperty(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .get_super_property,
        .data = .{ .reg_string = .{ dest, @as(Bytecode.Inst.StringIndex, @enumFromInt(@intFromEnum(string_index))) } },
    });
}

fn lowerGetSuperPropertyComputed(b: *Builder, ref: Ir.Inst.Ref, dest: Bytecode.Inst.Reg) Error!void {
    const property_reg = b.resolve(ref);
    try b.emit(.{
        .tag = .get_super_property_computed,
        .data = .{ .reg_reg = .{ dest, property_reg } },
    });
}

fn lowerSetSuperProperty(b: *Builder, data: Ir.Inst.ExtraIndex, comptime strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SetProperty, data);
    const value_reg = b.resolve(extra.data.value);
    const tag: Bytecode.Inst.Tag = if (strict)
        .set_super_property_strict
    else
        .set_super_property;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_string = .{
            value_reg,
            @enumFromInt(@intFromEnum(extra.data.name)),
        } },
    });
    try b.emitMoveIfNeeded(extra.data.value, dest);
}

fn lowerSetSuperPropertyComputed(b: *Builder, data: Ir.Inst.ExtraIndex, comptime strict: bool, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SetPropertyComputed, data);
    const property_reg = b.resolve(extra.data.property);
    const value_reg = b.resolve(extra.data.value);
    const tag: Bytecode.Inst.Tag = if (strict)
        .set_super_property_computed_strict
    else
        .set_super_property_computed;
    try b.emit(.{
        .tag = tag,
        .data = .{ .reg_reg = .{
            property_reg,
            value_reg,
        } },
    });
    try b.emitMoveIfNeeded(extra.data.value, dest);
}

fn lowerPushPrivateScope(b: *Builder) Error!void {
    try b.emit(.{
        .tag = .push_private_scope,
        .data = .{ .none = {} },
    });
}

fn lowerPopPrivateScope(b: *Builder) Error!void {
    try b.emit(.{
        .tag = .pop_private_scope,
        .data = .{ .none = {} },
    });
}

fn lowerCreatePrivateElement(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .create_private_element,
        .data = .{ .reg_string = .{
            dest,
            @enumFromInt(@intFromEnum(string_index)),
        } },
    });
}

fn lowerResolvePrivateElement(b: *Builder, string_index: Ir.Inst.StringIndex, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .resolve_private_element,
        .data = .{ .reg_string = .{
            dest,
            @enumFromInt(@intFromEnum(string_index)),
        } },
    });
}

fn lowerGetPrivateElement(b: *Builder, data: Ir.Inst.GetProperty, dest: Bytecode.Inst.Reg) Error!void {
    const base_reg = b.resolve(data.base);
    try b.emit(.{
        .tag = .get_private_element,
        .data = .{ .reg_reg_string = .{
            dest,
            base_reg,
            @enumFromInt(@intFromEnum(data.name)),
        } },
    });
}

fn lowerSetPrivateElement(b: *Builder, data: Ir.Inst.ExtraIndex, dest: Bytecode.Inst.Reg) Error!void {
    const extra = b.ir.extraData(Ir.Inst.SetProperty, data);
    const base_reg = b.resolve(extra.data.base);
    const value_reg = b.resolve(extra.data.value);
    try b.emit(.{
        .tag = .set_private_element,
        .data = .{ .reg_string_reg = .{
            base_reg,
            @enumFromInt(@intFromEnum(extra.data.name)),
            value_reg,
        } },
    });
    try b.emitMoveIfNeeded(extra.data.value, dest);
}

fn lowerHasPrivateElement(b: *Builder, data: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    try b.emitBinaryOp(.has_private_element, data.lhs, data.rhs, dest);
}

fn lowerImportCall(b: *Builder, binary: Ir.Inst.Binary, dest: Bytecode.Inst.Reg) Error!void {
    const specifier = b.resolve(binary.lhs);
    const options = b.resolve(binary.rhs);
    try b.emit(.{
        .tag = .import_call,
        .data = .{ .reg_reg_reg = .{
            dest,
            specifier,
            options,
        } },
    });
}

fn lowerGetImportMeta(b: *Builder, dest: Bytecode.Inst.Reg) Error!void {
    try b.emit(.{
        .tag = .get_import_meta,
        .data = .{ .reg = dest },
    });
}
