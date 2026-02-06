const builtin = @import("builtin");
const std = @import("std");

const ast = @import("../../language/ast.zig");
const interpreter = @import("../../interpreter.zig");

const Constant = @import("constant_folding.zig").Constant;
const Ir = interpreter.Ir;

const computeLiveness = @import("liveness.zig").computeLiveness;
const computeLiveRanges = @import("live_ranges.zig").computeLiveRanges;
const constantFold = @import("constant_folding.zig").constantFold;

pub const Builder = @This();

gpa: std.mem.Allocator,
name: []const u8,
root_node: Ast,
in_strict_mode: bool,
instructions: std.MultiArrayList(Ir.Inst),
strings: std.StringArrayHashMapUnmanaged(void),
big_ints: BigIntArrayHashMapUnmanaged(void),
extras: std.ArrayListUnmanaged(u32),
breakable_stack: std.ArrayListUnmanaged(*BreakableContext) = .empty,
scope_depth: u32 = 0,

const Ast = union(enum) {
    script: *const ast.Script,
    module: *const ast.Module,
};

const BigIntContext = struct {
    pub fn hash(_: @This(), key: std.math.big.int.Const) u32 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&key.positive));
        for (key.limbs) |limb| {
            hasher.update(std.mem.asBytes(&limb));
        }
        return @truncate(hasher.final());
    }
    pub fn eql(_: @This(), a: std.math.big.int.Const, b: std.math.big.int.Const, _: usize) bool {
        return a.eql(b);
    }
};

fn BigIntArrayHashMapUnmanaged(comptime V: type) type {
    return std.ArrayHashMapUnmanaged(std.math.big.int.Const, V, BigIntContext, true);
}

const BreakableContext = struct {
    label: ?[]const u8,
    continue_target: JumpTarget,
    break_target: JumpTarget,
    result_ref: Ir.Inst.Ref,
    scope_depth: u32 = 0,

    const JumpTarget = union(enum) {
        known: Ir.Inst.Ref,
        deferred: std.ArrayListUnmanaged(DeferredJump),
    };

    const DeferredJump = struct {
        inst: Deferred,
        value: Ir.Inst.Ref,
    };

    fn deinit(ctx: *BreakableContext, gpa: std.mem.Allocator) void {
        switch (ctx.continue_target) {
            .known => {},
            .deferred => |*list| list.deinit(gpa),
        }
        switch (ctx.break_target) {
            .known => {},
            .deferred => |*list| list.deinit(gpa),
        }
    }

    fn setDeferredBreaks(ctx: *BreakableContext, target: Ir.Inst.Ref) void {
        for (ctx.break_target.deferred.items) |jump| {
            jump.inst.set(.{ .br = .{
                .target = target,
                .value = jump.value,
            } });
        }
    }

    fn setDeferredContinues(ctx: *BreakableContext, target: Ir.Inst.Ref) void {
        for (ctx.continue_target.deferred.items) |jump| {
            jump.inst.set(.{ .br = .{
                .target = target,
                .value = jump.value,
            } });
        }
    }
};

pub const Error = error{ OutOfMemory, NotImplemented };

pub fn init(gpa: std.mem.Allocator, name: []const u8, root_node: Ast) Builder {
    const in_strict_mode = switch (root_node) {
        .script => |script| script.scriptIsStrict(),
        .module => true,
    };
    return .{
        .gpa = gpa,
        .name = name,
        .root_node = root_node,
        .in_strict_mode = in_strict_mode,
        .instructions = .empty,
        .strings = .empty,
        .big_ints = .empty,
        .extras = .empty,
    };
}

pub fn deinit(b: *Builder) void {
    b.instructions.deinit(b.gpa);
    for (b.strings.keys()) |string| b.gpa.free(string);
    b.strings.deinit(b.gpa);
    for (b.big_ints.keys()) |big_int| b.gpa.free(big_int.limbs);
    b.big_ints.deinit(b.gpa);
    b.extras.deinit(b.gpa);
    for (b.breakable_stack.items) |ctx| {
        ctx.deinit(b.gpa);
        b.gpa.destroy(ctx);
    }
    b.breakable_stack.deinit(b.gpa);
}

pub fn build(b: *Builder) Error!Ir {
    const result = switch (b.root_node) {
        .script => |script| try b.lowerScript(script),
        .module => try b.todo("module"),
    };
    _ = try b.addInst(.{
        .tag = .end,
        .data = .{ .ref = result },
    });

    const name = try b.gpa.dupe(u8, b.name);
    errdefer b.gpa.free(name);

    var instructions = b.instructions.toOwnedSlice();
    errdefer instructions.deinit(b.gpa);

    const extras = try b.extras.toOwnedSlice(b.gpa);
    errdefer b.gpa.free(extras);

    var liveness = try computeLiveness(b.gpa, instructions, extras);
    errdefer liveness.deinit(b.gpa);

    const live_ranges = try computeLiveRanges(b.gpa, instructions, extras);
    errdefer b.gpa.free(live_ranges);

    const strings = try b.gpa.dupe([]const u8, b.strings.keys());
    errdefer b.gpa.free(strings);
    b.strings.clearRetainingCapacity(); // Transfer ownership
    errdefer for (strings) |string| b.gpa.free(string);

    const big_ints = try b.gpa.dupe(std.math.big.int.Const, b.big_ints.keys());
    errdefer b.gpa.free(big_ints);
    b.big_ints.clearRetainingCapacity(); // Transfer ownership
    errdefer for (big_ints) |big_int| b.gpa.free(big_int.limbs);

    return .{
        .name = name,
        .instructions = instructions,
        .liveness = liveness,
        .live_ranges = live_ranges,
        .strings = strings,
        .big_ints = big_ints,
        .extras = extras,
    };
}

fn todo(_: *Builder, msg: []const u8) Error!noreturn {
    switch (builtin.target.os.tag) {
        .uefi => {},
        else => std.debug.print("TODO: {s}\n", .{msg}),
    }
    return error.NotImplemented;
}

fn addInst(b: *Builder, inst: Ir.Inst) std.mem.Allocator.Error!Ir.Inst.Ref {
    const index: Ir.Inst.Index = @enumFromInt(b.instructions.len);
    try b.instructions.append(b.gpa, inst);
    return index.toRef();
}

const Deferred = struct {
    b: *Builder,
    ref: Ir.Inst.Ref,

    fn set(d: Deferred, data: Ir.Inst.Data) void {
        const index = d.ref.toIndex().?;
        d.b.instructions.slice().items(.data)[@intFromEnum(index)] = data;
    }
};

fn addInstDeferred(b: *Builder, tag: Ir.Inst.Tag) std.mem.Allocator.Error!Deferred {
    const ref = try b.addInst(.{
        .tag = tag,
        .data = undefined,
    });
    return .{ .b = b, .ref = ref };
}

fn addLabel(b: *Builder) std.mem.Allocator.Error!Ir.Inst.Ref {
    return b.addInst(.{
        .tag = .label,
        .data = .{ .none = {} },
    });
}

fn pushBreakableContext(b: *Builder, ctx: BreakableContext) std.mem.Allocator.Error!*BreakableContext {
    const heap_ctx = try b.gpa.create(BreakableContext);
    heap_ctx.* = ctx;
    heap_ctx.scope_depth = b.scope_depth;
    try b.breakable_stack.append(b.gpa, heap_ctx);
    return heap_ctx;
}

fn popBreakableContext(b: *Builder) void {
    const ctx = b.breakable_stack.pop().?;
    ctx.deinit(b.gpa);
    b.gpa.destroy(ctx);
}

fn findBreakableContext(b: *Builder, label: ?[]const u8) *BreakableContext {
    if (label) |l| {
        var it = std.mem.reverseIterator(b.breakable_stack.items);
        while (it.next()) |ctx| {
            if (ctx.label) |ctx_label| {
                if (std.mem.eql(u8, ctx_label, l)) {
                    return ctx;
                }
            }
        }
        unreachable;
    }
    return b.breakable_stack.items[b.breakable_stack.items.len - 1];
}

fn internString(b: *Builder, string: []const u8) std.mem.Allocator.Error!Ir.Inst.StringIndex {
    const gop = try b.strings.getOrPut(b.gpa, string);
    if (!gop.found_existing) {
        gop.key_ptr.* = try b.gpa.dupe(u8, string);
    }
    return @enumFromInt(gop.index);
}

fn internBigInt(b: *Builder, big_int: std.math.big.int.Const) std.mem.Allocator.Error!Ir.Inst.BigIntIndex {
    const gop = try b.big_ints.getOrPut(b.gpa, big_int);
    if (!gop.found_existing) {
        gop.key_ptr.limbs = try b.gpa.dupe(std.math.big.Limb, big_int.limbs);
    }
    return @enumFromInt(gop.index);
}

fn lowerConstant(b: *Builder, constant: Constant) Error!Ir.Inst.Ref {
    return switch (constant) {
        .undefined => b.addInst(.{
            .tag = .undefined,
            .data = .{ .none = {} },
        }),
        .null => b.addInst(.{
            .tag = .null,
            .data = .{ .none = {} },
        }),
        .boolean => |boolean| b.addInst(.{
            .tag = if (boolean) .true else .false,
            .data = .{ .none = {} },
        }),
        .number => |number| {
            if (number == 0 and !std.math.isNegativeZero(number)) {
                return b.addInst(.{
                    .tag = .zero,
                    .data = .{ .none = {} },
                });
            } else if (number == 1) {
                return b.addInst(.{
                    .tag = .one,
                    .data = .{ .none = {} },
                });
            } else {
                return b.addInst(.{
                    .tag = .number,
                    .data = .{ .number = number },
                });
            }
        },
        .big_int => |big_int| {
            const big_int_index = try b.internBigInt(big_int);
            return b.addInst(.{
                .tag = .big_int,
                .data = .{ .big_int = big_int_index },
            });
        },
        .string => |string| {
            const string_index = try b.internString(string);
            return b.addInst(.{
                .tag = .string,
                .data = .{ .string = string_index },
            });
        },
    };
}

fn lowerScript(b: *Builder, script: *const ast.Script) Error!Ir.Inst.Ref {
    return b.lowerStatementList(&script.statement_list);
}

fn lowerStatementList(b: *Builder, stmt_list: *const ast.StatementList) Error!Ir.Inst.Ref {
    var last: Ir.Inst.Ref = .none;
    for (stmt_list.items) |item| {
        const result = switch (item) {
            .statement => |stmt| try b.lowerStatement(stmt),
            .declaration => |decl| try b.lowerDeclaration(decl),
        };
        if (result != .none) {
            last = result;
        }
    }
    return last;
}

fn lowerStatement(b: *Builder, stmt: *const ast.Statement) Error!Ir.Inst.Ref {
    return switch (stmt.*) {
        .block_statement => |*block_stmt| try b.lowerBlockStatement(block_stmt, null),
        .variable_statement => |*var_stmt| try b.lowerVariableStatement(var_stmt),
        .empty_statement => .none,
        .expression_statement => |expr_stmt| try b.lowerExpression(&expr_stmt.expression),
        .if_statement => |*if_stmt| try b.lowerIfStatement(if_stmt),
        .breakable_statement => |*brk_stmt| try b.lowerBreakableStatement(brk_stmt, null),
        .continue_statement => |*cont_stmt| try b.lowerContinueStatement(cont_stmt),
        .break_statement => |*brk_stmt| try b.lowerBreakStatement(brk_stmt),
        .return_statement => try b.todo("return statement"),
        .with_statement => try b.todo("with statement"),
        .labelled_statement => |*lbl_stmt| try b.lowerLabelledStatement(lbl_stmt),
        .throw_statement => |*throw_stmt| try b.lowerThrowStatement(throw_stmt),
        .try_statement => try b.todo("try statement"),
        .debugger_statement => .none,
    };
}

fn lowerDeclaration(b: *Builder, decl: *const ast.Declaration) Error!Ir.Inst.Ref {
    return switch (decl.*) {
        .hoistable_declaration => try b.todo("hoistable declaration"),
        .class_declaration => try b.todo("class declaration"),
        .lexical_declaration => |*lex_decl| try b.lowerLexicalDeclaration(lex_decl),
    };
}

fn lowerBlockStatement(b: *Builder, block_stmt: *const ast.BlockStatement, breakable_ctx: ?*BreakableContext) Error!Ir.Inst.Ref {
    const stmt_list = &block_stmt.block.statement_list;
    const has_scope = stmt_list.hasLexicallyScopedDeclarations();

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .push_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;

        for (stmt_list.items) |item| {
            const lex_decl = switch (item) {
                .declaration => |decl| switch (decl.*) {
                    .lexical_declaration => |*lex_decl| lex_decl,
                    else => continue,
                },
                .statement => continue,
            };
            const tag: Ir.Inst.Tag = if (lex_decl.isConstantDeclaration())
                .create_immutable_binding
            else
                .create_mutable_binding;
            for (lex_decl.binding_list.items) |lex_binding| {
                switch (lex_binding) {
                    .binding_identifier => |binding| {
                        const string_index = try b.internString(binding.binding_identifier);
                        _ = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .string = string_index },
                        });
                    },
                    .binding_pattern => |pattern| {
                        var bound_names: std.ArrayList(ast.Identifier) = .empty;
                        defer bound_names.deinit(b.gpa);
                        try pattern.binding_pattern.collectBoundNames(b.gpa, &bound_names);
                        for (bound_names.items) |name| {
                            const string_index = try b.internString(name);
                            _ = try b.addInst(.{
                                .tag = tag,
                                .data = .{ .string = string_index },
                            });
                        }
                    },
                }
            }
        }
    }

    const result = if (breakable_ctx) |ctx| blk: {
        try b.lowerBreakableStatementList(stmt_list, ctx);
        break :blk ctx.result_ref;
    } else try b.lowerStatementList(stmt_list);

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth -= 1;
    }

    return result;
}

fn lowerVariableStatement(b: *Builder, var_stmt: *const ast.VariableStatement) Error!Ir.Inst.Ref {
    var last: Ir.Inst.Ref = .none;
    for (var_stmt.variable_declaration_list.items) |var_decl| {
        const result = try b.lowerVariableDeclaration(var_decl);
        if (result != .none) {
            last = result;
        }
    }
    return last;
}

fn lowerVariableDeclaration(b: *Builder, var_decl: ast.VariableDeclaration) Error!Ir.Inst.Ref {
    // GlobalDeclarationInstantiation is responsible for creating the bindings and initializing them to undefined.
    return switch (var_decl) {
        .binding_identifier => |binding| {
            if (binding.initializer) |*init_expr| {
                const value = try b.lowerExpression(init_expr);
                const string_index = try b.internString(binding.binding_identifier);
                return b.addInst(.{
                    .tag = .set_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = value,
                    } },
                });
            }
            return .none;
        },
        .binding_pattern => |pattern| {
            const value = try b.lowerExpression(&pattern.initializer);
            return b.lowerDestructuringAssignment(pattern.binding_pattern, value, .set);
        },
    };
}

fn lowerIfStatement(b: *Builder, if_stmt: *const ast.IfStatement) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, &if_stmt.test_expression)) |constant| {
        defer constant.deinit(b.gpa);
        return if (constant.isTruthy())
            try b.lowerStatement(if_stmt.consequent_statement)
        else if (if_stmt.alternate_statement) |stmt|
            try b.lowerStatement(stmt)
        else
            try b.addInst(.{
                .tag = .undefined,
                .data = .{ .none = {} },
            });
    }

    const test_result = try b.lowerExpression(&if_stmt.test_expression);
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const then_label = try b.addLabel();
    const then_result = try b.lowerStatement(if_stmt.consequent_statement);
    const then_br = try b.addInstDeferred(.br);

    const else_label = try b.addLabel();
    const else_result = if (if_stmt.alternate_statement) |stmt|
        try b.lowerStatement(stmt)
    else
        .none;
    const else_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    test_br_cond.set(.{ .br_cond = .{
        .condition = test_result,
        .then_target = then_label,
        .else_target = else_label,
    } });
    then_br.set(.{ .br = .{
        .target = end_label,
        .value = then_result,
    } });
    else_br.set(.{ .br = .{
        .target = end_label,
        .value = else_result,
    } });

    return end_label;
}

fn lowerBreakableStatement(b: *Builder, brk_stmt: *const ast.BreakableStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    return switch (brk_stmt.*) {
        .iteration_statement => |iter_stmt| switch (iter_stmt) {
            .do_while_statement => |*do_while_stmt| try b.lowerDoWhileStatement(do_while_stmt, label),
            .while_statement => |*while_stmt| try b.lowerWhileStatement(while_stmt, label),
            .for_statement => |*for_stmt| try b.lowerForStatement(for_stmt, label),
            .for_in_of_statement => |*for_in_of_stmt| try b.lowerForInOfStatement(for_in_of_stmt, label),
        },
        .switch_statement => |*switch_stmt| try b.lowerSwitchStatement(switch_stmt, label),
    };
}

fn lowerBreakableStatementList(b: *Builder, stmt_list: *const ast.StatementList, ctx: *BreakableContext) Error!void {
    for (stmt_list.items) |item| {
        const result = switch (item) {
            .statement => |stmt| try b.lowerStatement(stmt),
            .declaration => |decl| try b.lowerDeclaration(decl),
        };
        if (result != .none) ctx.result_ref = result;
    }
}

fn lowerDoWhileStatement(b: *Builder, do_while_stmt: *const ast.DoWhileStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    const body_label = try b.addLabel();

    const undefined_ref = try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });

    const breakable_ctx = try b.pushBreakableContext(.{
        .label = label,
        .continue_target = .{ .deferred = .empty },
        .break_target = .{ .deferred = .empty },
        .result_ref = undefined_ref,
    });
    defer b.popBreakableContext();

    const body_block_stmt: *const ast.BlockStatement = switch (do_while_stmt.consequent_statement.*) {
        .block_statement => |*block_stmt| block_stmt,
        else => &.{ .block = .{ .statement_list = .{ .items = &.{
            .{ .statement = do_while_stmt.consequent_statement },
        } } } },
    };
    _ = try b.lowerBlockStatement(body_block_stmt, breakable_ctx);
    const body_result = breakable_ctx.result_ref;
    const body_br = try b.addInstDeferred(.br);

    const test_label = try b.addLabel();
    const test_result = try b.lowerExpression(&do_while_stmt.test_expression);
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const continue_label = try b.addLabel();
    const continue_br = try b.addInstDeferred(.br);

    const exit_label = try b.addLabel();
    const exit_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    breakable_ctx.setDeferredContinues(test_label);
    breakable_ctx.setDeferredBreaks(end_label);

    body_br.set(.{ .br = .{
        .target = test_label,
        .value = body_result,
    } });
    test_br_cond.set(.{ .br_cond = .{
        .condition = test_result,
        .then_target = continue_label,
        .else_target = exit_label,
    } });
    continue_br.set(.{ .br = .{
        .target = body_label,
        .value = test_label,
    } });
    exit_br.set(.{ .br = .{
        .target = end_label,
        .value = test_label,
    } });

    return end_label;
}

fn lowerWhileStatement(b: *Builder, while_stmt: *const ast.WhileStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, &while_stmt.test_expression)) |constant| {
        defer constant.deinit(b.gpa);
        if (!constant.isTruthy()) {
            return try b.addInst(.{
                .tag = .undefined,
                .data = .{ .none = {} },
            });
        }
    }

    const undefined_ref = try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });
    const entry_br = try b.addInstDeferred(.br);

    const test_label = try b.addLabel();
    const test_result = try b.lowerExpression(&while_stmt.test_expression);
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const body_label = try b.addLabel();

    const breakable_ctx = try b.pushBreakableContext(.{
        .label = label,
        .continue_target = .{ .known = test_label },
        .break_target = .{ .deferred = .empty },
        .result_ref = undefined_ref,
    });
    defer b.popBreakableContext();

    const body_block_stmt: *const ast.BlockStatement = switch (while_stmt.consequent_statement.*) {
        .block_statement => |*block_stmt| block_stmt,
        else => &.{ .block = .{ .statement_list = .{ .items = &.{
            .{ .statement = while_stmt.consequent_statement },
        } } } },
    };
    _ = try b.lowerBlockStatement(body_block_stmt, breakable_ctx);
    const body_result = breakable_ctx.result_ref;
    const body_br = try b.addInstDeferred(.br);

    const exit_label = try b.addLabel();
    const exit_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    breakable_ctx.setDeferredBreaks(end_label);

    entry_br.set(.{ .br = .{
        .target = test_label,
        .value = undefined_ref,
    } });
    test_br_cond.set(.{ .br_cond = .{
        .condition = test_result,
        .then_target = body_label,
        .else_target = exit_label,
    } });
    body_br.set(.{ .br = .{
        .target = test_label,
        .value = body_result,
    } });
    exit_br.set(.{ .br = .{
        .target = end_label,
        .value = test_label,
    } });

    return end_label;
}

fn lowerForStatement(b: *Builder, for_stmt: *const ast.ForStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    const has_scope = if (for_stmt.initializer) |initializer|
        initializer == .lexical_declaration
    else
        false;

    if (has_scope) {
        const lex_decl = &for_stmt.initializer.?.lexical_declaration;
        _ = try b.addInst(.{
            .tag = .push_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;

        const binding_tag: Ir.Inst.Tag = if (lex_decl.isConstantDeclaration())
            .create_immutable_binding
        else
            .create_mutable_binding;
        for (lex_decl.binding_list.items) |lex_binding| {
            switch (lex_binding) {
                .binding_identifier => |binding| {
                    const string_index = try b.internString(binding.binding_identifier);
                    _ = try b.addInst(.{
                        .tag = binding_tag,
                        .data = .{ .string = string_index },
                    });
                },
                .binding_pattern => |pattern| {
                    var bound_names: std.ArrayList(ast.Identifier) = .empty;
                    defer bound_names.deinit(b.gpa);
                    try pattern.binding_pattern.collectBoundNames(b.gpa, &bound_names);
                    for (bound_names.items) |name| {
                        const string_index = try b.internString(name);
                        _ = try b.addInst(.{
                            .tag = binding_tag,
                            .data = .{ .string = string_index },
                        });
                    }
                },
            }
        }
    }

    if (for_stmt.initializer) |initializer| {
        _ = switch (initializer) {
            .expression => |*expr| try b.lowerExpression(expr),
            .variable_statement => |*var_stmt| try b.lowerVariableStatement(var_stmt),
            .lexical_declaration => |*lex_decl| try b.lowerLexicalDeclaration(lex_decl),
        };
    }

    if (for_stmt.test_expression) |*test_expr| {
        if (try constantFold(b.gpa, test_expr)) |constant| {
            defer constant.deinit(b.gpa);
            if (!constant.isTruthy()) {
                return try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });
            }
        }
    }

    const undefined_ref = try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });
    const entry_br = try b.addInstDeferred(.br);

    const test_label = try b.addLabel();
    const test_result = if (for_stmt.test_expression) |*test_expr|
        try b.lowerExpression(test_expr)
    else
        try b.addInst(.{
            .tag = .true,
            .data = .{ .none = {} },
        });
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const body_label = try b.addLabel();

    const breakable_ctx = try b.pushBreakableContext(.{
        .label = label,
        .continue_target = .{ .deferred = .empty },
        .break_target = .{ .deferred = .empty },
        .result_ref = undefined_ref,
    });
    defer b.popBreakableContext();

    const body_block_stmt: *const ast.BlockStatement = switch (for_stmt.consequent_statement.*) {
        .block_statement => |*block_stmt| block_stmt,
        else => &.{ .block = .{ .statement_list = .{ .items = &.{
            .{ .statement = for_stmt.consequent_statement },
        } } } },
    };
    _ = try b.lowerBlockStatement(body_block_stmt, breakable_ctx);
    const body_result = breakable_ctx.result_ref;

    const body_br = try b.addInstDeferred(.br);

    const continue_label = try b.addLabel();
    if (for_stmt.increment_expression) |*update_expr| {
        _ = try b.lowerExpression(update_expr);
    }
    const continue_br = try b.addInstDeferred(.br);

    const exit_label = try b.addLabel();

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth -= 1;
    }

    const exit_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    breakable_ctx.setDeferredContinues(continue_label);
    breakable_ctx.setDeferredBreaks(end_label);

    entry_br.set(.{ .br = .{
        .target = test_label,
        .value = undefined_ref,
    } });
    test_br_cond.set(.{ .br_cond = .{
        .condition = test_result,
        .then_target = body_label,
        .else_target = exit_label,
    } });
    body_br.set(.{ .br = .{
        .target = continue_label,
        .value = body_result,
    } });
    continue_br.set(.{ .br = .{
        .target = test_label,
        .value = body_result,
    } });
    exit_br.set(.{ .br = .{
        .target = end_label,
        .value = test_label,
    } });

    return end_label;
}

fn lowerForInOfStatement(b: *Builder, for_in_of_stmt: *const ast.ForInOfStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    const expr_value = try b.lowerExpression(&for_in_of_stmt.expression);

    const undefined_ref = try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });

    var skip_br_cond: Deferred = undefined;
    var skip_condition: Ir.Inst.Ref = .none;
    var setup_label: Ir.Inst.Ref = undefined;

    if (for_in_of_stmt.type == .in) {
        const null_ref = try b.addInst(.{
            .tag = .null,
            .data = .{ .none = {} },
        });
        skip_condition = try b.addInst(.{
            .tag = .eq,
            .data = .{ .binary = .{
                .lhs = expr_value,
                .rhs = null_ref,
            } },
        });
        skip_br_cond = try b.addInstDeferred(.br_cond);
        setup_label = try b.addLabel();
    }

    const iterator = switch (for_in_of_stmt.type) {
        .in => try b.addInst(.{
            .tag = .get_for_in_iterator,
            .data = .{ .ref = expr_value },
        }),
        .of => try b.addInst(.{
            .tag = .get_iterator,
            .data = .{ .ref = expr_value },
        }),
        .async_of => try b.todo("async for-of"),
    };

    const entry_br = try b.addInstDeferred(.br);

    const test_label = try b.addLabel();

    const next_value = try b.addInst(.{
        .tag = .iterator_step_value,
        .data = .{ .ref = iterator },
    });
    const is_done = try b.addInst(.{
        .tag = .iterator_is_done,
        .data = .{ .ref = iterator },
    });
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const body_label = try b.addLabel();

    const breakable_ctx = try b.pushBreakableContext(.{
        .label = label,
        .continue_target = .{ .deferred = .empty },
        .break_target = .{ .deferred = .empty },
        .result_ref = undefined_ref,
    });
    defer b.popBreakableContext();

    switch (for_in_of_stmt.initializer) {
        .expression => |*expr| switch (expr.*) {
            .primary_expression => |prim_expr| switch (prim_expr) {
                .identifier_reference => |identifier| {
                    const string_index = try b.internString(identifier);
                    _ = try b.addInst(.{
                        .tag = if (b.in_strict_mode) .set_binding_strict else .set_binding,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = next_value,
                        } },
                    });
                },
                else => unreachable,
            },
            .member_expression => |*member_expr| {
                const base = try b.lowerExpression(member_expr.expression);
                switch (member_expr.property) {
                    .identifier => |identifier| {
                        const string_index = try b.internString(identifier);
                        const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                            .set_property_strict
                        else
                            .set_property;
                        _ = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .set_property = .{
                                .base = base,
                                .name = string_index,
                                .value = next_value,
                            } },
                        });
                    },
                    .expression => |prop_expr| blk: {
                        if (try constantFold(b.gpa, prop_expr)) |constant| {
                            defer constant.deinit(b.gpa);
                            if (constant.toIndex()) |index| {
                                const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                                    .set_property_indexed_strict
                                else
                                    .set_property_indexed;
                                _ = try b.addInst(.{
                                    .tag = tag,
                                    .data = .{ .set_property_indexed = .{
                                        .base = base,
                                        .index = index,
                                        .value = next_value,
                                    } },
                                });
                                break :blk;
                            }
                        }
                        const property = try b.lowerExpression(prop_expr);
                        const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                            .set_property_computed_strict
                        else
                            .set_property_computed;
                        _ = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .set_property_computed = .{
                                .base = base,
                                .property = property,
                                .value = next_value,
                            } },
                        });
                    },
                    .private_identifier => try b.todo("private identifier in for-in/of LHS"),
                }
            },
            .super_property => try b.todo("super property in for-in/of LHS"),
            else => unreachable,
        },
        .for_binding => |for_binding| switch (for_binding) {
            .binding_identifier => |identifier| {
                const string_index = try b.internString(identifier);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .set_binding_strict
                else
                    .set_binding;
                _ = try b.addInst(.{
                    .tag = tag,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = next_value,
                    } },
                });
            },
            .binding_pattern => |pattern| {
                _ = try b.lowerDestructuringAssignment(pattern, next_value, .set);
            },
        },
        .for_declaration => |for_decl| {
            _ = try b.addInst(.{
                .tag = .push_scope,
                .data = .{ .none = {} },
            });
            b.scope_depth += 1;

            const tag: Ir.Inst.Tag = switch (for_decl.type) {
                .let => .create_mutable_binding,
                .@"const" => .create_immutable_binding,
            };
            switch (for_decl.for_binding) {
                .binding_identifier => |identifier| {
                    const string_index = try b.internString(identifier);
                    _ = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .string = string_index },
                    });
                    _ = try b.addInst(.{
                        .tag = .initialize_binding,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = next_value,
                        } },
                    });
                },
                .binding_pattern => |pattern| {
                    var bound_names: std.ArrayList(ast.Identifier) = .empty;
                    defer bound_names.deinit(b.gpa);
                    try pattern.collectBoundNames(b.gpa, &bound_names);
                    for (bound_names.items) |name| {
                        const string_index = try b.internString(name);
                        _ = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .string = string_index },
                        });
                    }
                    _ = try b.lowerDestructuringAssignment(pattern, next_value, .initialize);
                },
            }
        },
    }

    const body_block_stmt: *const ast.BlockStatement = switch (for_in_of_stmt.consequent_statement.*) {
        .block_statement => |*block_stmt| block_stmt,
        else => &.{ .block = .{ .statement_list = .{ .items = &.{
            .{ .statement = for_in_of_stmt.consequent_statement },
        } } } },
    };
    _ = try b.lowerBlockStatement(body_block_stmt, breakable_ctx);

    if (for_in_of_stmt.initializer == .for_declaration) {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth -= 1;
    }

    const body_result = breakable_ctx.result_ref;
    const body_br = try b.addInstDeferred(.br);

    const exit_label = try b.addLabel();
    const exit_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    breakable_ctx.setDeferredContinues(test_label);
    breakable_ctx.setDeferredBreaks(end_label);

    if (for_in_of_stmt.type == .in) {
        skip_br_cond.set(.{ .br_cond = .{
            .condition = skip_condition,
            .then_target = end_label,
            .else_target = setup_label,
        } });
    }
    entry_br.set(.{ .br = .{
        .target = test_label,
        .value = undefined_ref,
    } });
    test_br_cond.set(.{ .br_cond = .{
        .condition = is_done,
        .then_target = exit_label,
        .else_target = body_label,
    } });
    body_br.set(.{ .br = .{
        .target = test_label,
        .value = body_result,
    } });
    exit_br.set(.{ .br = .{
        .target = end_label,
        .value = test_label,
    } });

    return end_label;
}

fn lowerSwitchStatement(b: *Builder, switch_stmt: *const ast.SwitchStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    const discriminant = try b.lowerExpression(&switch_stmt.expression);

    const undefined_ref = try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });
    const entry_br = try b.addInstDeferred(.br);

    const breakable_ctx = try b.pushBreakableContext(.{
        .label = label,
        .continue_target = .{ .deferred = .empty },
        .break_target = .{ .deferred = .empty },
        .result_ref = undefined_ref,
    });
    defer b.popBreakableContext();

    const items = switch_stmt.case_block.items;

    var case_branches: std.ArrayListUnmanaged(struct { br_cond: Deferred, index: u32 }) = .empty;
    defer case_branches.deinit(b.gpa);

    var default_index: ?u32 = null;

    const check_start = try b.addLabel();
    for (items, 0..) |item, i| {
        switch (item) {
            .case_clause => |case_clause| {
                const case_value = try b.lowerExpression(&case_clause.expression);
                const condition = try b.addInst(.{
                    .tag = .eq_strict,
                    .data = .{ .binary = .{
                        .lhs = discriminant,
                        .rhs = case_value,
                    } },
                });
                const case_br_cond = try b.addInstDeferred(.br_cond);
                try case_branches.append(b.gpa, .{
                    .br_cond = case_br_cond,
                    .index = @intCast(i),
                });

                const next_check = try b.addLabel();
                case_br_cond.set(.{ .br_cond = .{
                    .condition = condition,
                    .then_target = .none,
                    .else_target = next_check,
                } });
            },
            .default_clause => default_index = @intCast(i),
        }
    }
    const default_br = try b.addInstDeferred(.br);

    const body_labels = try b.gpa.alloc(Ir.Inst.Ref, items.len);
    defer b.gpa.free(body_labels);
    for (items, 0..) |item, i| {
        body_labels[i] = try b.addLabel();
        switch (item) {
            .case_clause => |case_clause| try b.lowerBreakableStatementList(&case_clause.statement_list, breakable_ctx),
            .default_clause => |default_clause| try b.lowerBreakableStatementList(&default_clause.statement_list, breakable_ctx),
        }
    }

    const exit_br = try b.addInstDeferred(.br);
    const end_label = try b.addLabel();

    breakable_ctx.setDeferredBreaks(end_label);

    // Forward continues to parent context
    if (b.breakable_stack.items.len >= 2) {
        const parent_ctx = b.breakable_stack.items[b.breakable_stack.items.len - 2];
        for (breakable_ctx.continue_target.deferred.items) |jump| {
            switch (parent_ctx.continue_target) {
                .known => |target| jump.inst.set(.{ .br = .{
                    .target = target,
                    .value = jump.value,
                } }),
                .deferred => |*list| try list.append(b.gpa, jump),
            }
        }
    }

    entry_br.set(.{ .br = .{
        .target = check_start,
        .value = undefined_ref,
    } });
    for (case_branches.items) |item| {
        const inst_data = b.instructions.slice().items(.data)[@intFromEnum(item.br_cond.ref.toIndex().?)];
        item.br_cond.set(.{ .br_cond = .{
            .condition = inst_data.br_cond.condition,
            .then_target = body_labels[item.index],
            .else_target = inst_data.br_cond.else_target,
        } });
    }
    default_br.set(.{ .br = .{
        .target = if (default_index) |index| body_labels[index] else end_label,
        .value = undefined_ref,
    } });
    exit_br.set(.{ .br = .{
        .target = end_label,
        .value = breakable_ctx.result_ref,
    } });

    return end_label;
}

fn lowerContinueStatement(b: *Builder, cont_stmt: *const ast.ContinueStatement) Error!Ir.Inst.Ref {
    const ctx = b.findBreakableContext(cont_stmt.label);
    const value = ctx.result_ref;

    const scope_pops = b.scope_depth - ctx.scope_depth;
    for (0..scope_pops) |_| {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
    }

    switch (ctx.continue_target) {
        .known => |target| {
            _ = try b.addInst(.{
                .tag = .br,
                .data = .{ .br = .{
                    .target = target,
                    .value = value,
                } },
            });
        },
        .deferred => |*list| {
            const deferred = try b.addInstDeferred(.br);
            try list.append(b.gpa, .{
                .inst = deferred,
                .value = value,
            });
        },
    }
    return .none;
}

fn lowerBreakStatement(b: *Builder, brk_stmt: *const ast.BreakStatement) Error!Ir.Inst.Ref {
    const ctx = b.findBreakableContext(brk_stmt.label);
    const value = ctx.result_ref;

    const scope_pops = b.scope_depth - ctx.scope_depth;
    for (0..scope_pops) |_| {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
    }

    switch (ctx.break_target) {
        .known => |target| {
            _ = try b.addInst(.{
                .tag = .br,
                .data = .{ .br = .{
                    .target = target,
                    .value = value,
                } },
            });
        },
        .deferred => |*list| {
            const deferred = try b.addInstDeferred(.br);
            try list.append(b.gpa, .{
                .inst = deferred,
                .value = value,
            });
        },
    }
    return .none;
}

fn lowerLabelledStatement(b: *Builder, lbl_stmt: *const ast.LabelledStatement) Error!Ir.Inst.Ref {
    const label = lbl_stmt.label_identifier;
    return switch (lbl_stmt.labelled_item) {
        .statement => |stmt| switch (stmt.*) {
            .breakable_statement => |*brk_stmt| try b.lowerBreakableStatement(brk_stmt, label),
            else => {
                const undefined_ref = try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });

                const breakable_ctx = try b.pushBreakableContext(.{
                    .label = label,
                    .continue_target = .{ .deferred = .empty },
                    .break_target = .{ .deferred = .empty },
                    .result_ref = undefined_ref,
                });
                defer b.popBreakableContext();

                const result = try b.lowerStatement(stmt);
                if (result != .none) breakable_ctx.result_ref = result;

                const end_label = try b.addLabel();
                breakable_ctx.setDeferredBreaks(end_label);

                return end_label;
            },
        },
        .function_declaration => try b.todo("labelled function declaration"),
    };
}

fn lowerThrowStatement(b: *Builder, throw_stmt: *const ast.ThrowStatement) Error!Ir.Inst.Ref {
    const value = try b.lowerExpression(&throw_stmt.expression);
    return try b.addInst(.{
        .tag = .throw,
        .data = .{ .ref = value },
    });
}

fn lowerLexicalDeclaration(b: *Builder, lex_decl: *const ast.LexicalDeclaration) Error!Ir.Inst.Ref {
    for (lex_decl.binding_list.items) |lex_binding| {
        _ = try b.lowerLexicalBinding(lex_binding);
    }
    return .none;
}

fn lowerLexicalBinding(b: *Builder, lex_binding: ast.LexicalBinding) Error!Ir.Inst.Ref {
    // GlobalDeclarationInstantiation is responsible for creating the bindings.
    return switch (lex_binding) {
        .binding_identifier => |binding| {
            const value = if (binding.initializer) |*init_expr|
                try b.lowerExpression(init_expr)
            else
                try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });
            const string_index = try b.internString(binding.binding_identifier);
            return b.addInst(.{
                .tag = .initialize_binding,
                .data = .{ .set_binding = .{
                    .name = string_index,
                    .value = value,
                } },
            });
        },
        .binding_pattern => |pattern| {
            const value = try b.lowerExpression(&pattern.initializer);
            return try b.lowerDestructuringAssignment(pattern.binding_pattern, value, .initialize);
        },
    };
}

fn lowerPropertyName(b: *Builder, property_name: ast.PropertyName) Error!Ir.Inst.Ref {
    return switch (property_name) {
        .literal_property_name => |literal| switch (literal) {
            .identifier => |identifier| blk: {
                const string_index = try b.internString(identifier);
                break :blk try b.addInst(.{
                    .tag = .string,
                    .data = .{ .string = string_index },
                });
            },
            .string_literal => |str_lit| blk: {
                const expr: ast.Expression = .{
                    .primary_expression = .{
                        .literal = .{ .string = str_lit },
                    },
                };
                break :blk try b.lowerExpression(&expr);
            },
            .numeric_literal => |num_lit| blk: {
                const expr: ast.Expression = .{
                    .primary_expression = .{
                        .literal = .{ .numeric = num_lit },
                    },
                };
                break :blk try b.lowerExpression(&expr);
            },
        },
        .computed_property_name => |*expr| try b.lowerExpression(expr),
    };
}

fn lowerDefaultExpression(b: *Builder, value: Ir.Inst.Ref, default_expr: ?*const ast.Expression) Error!Ir.Inst.Ref {
    if (default_expr) |expr| {
        const is_undefined = try b.addInst(.{
            .tag = .eq_strict,
            .data = .{ .binary = .{
                .lhs = value,
                .rhs = try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                }),
            } },
        });
        const br_cond = try b.addInstDeferred(.br_cond);

        const then_label = try b.addInst(.{
            .tag = .label,
            .data = .{ .none = {} },
        });
        const default_value = try b.lowerExpression(expr);
        const then_br = try b.addInstDeferred(.br);

        const else_label = try b.addInst(.{
            .tag = .label,
            .data = .{ .none = {} },
        });
        const else_br = try b.addInstDeferred(.br);

        const end_label = try b.addInst(.{
            .tag = .label,
            .data = .{ .none = {} },
        });

        br_cond.set(.{ .br_cond = .{
            .condition = is_undefined,
            .then_target = then_label,
            .else_target = else_label,
        } });
        then_br.set(.{ .br = .{
            .target = end_label,
            .value = default_value,
        } });
        else_br.set(.{ .br = .{
            .target = end_label,
            .value = value,
        } });

        return end_label;
    }
    return value;
}

const BindingOp = enum { set, initialize };

fn lowerDestructuringAssignment(b: *Builder, pattern: ast.BindingPattern, value: Ir.Inst.Ref, binding_op: BindingOp) Error!Ir.Inst.Ref {
    return switch (pattern) {
        .array_binding_pattern => |array_pattern| try b.lowerArrayDestructuring(array_pattern, value, binding_op),
        .object_binding_pattern => |object_pattern| try b.lowerObjectDestructuring(object_pattern, value, binding_op),
    };
}

fn lowerArrayDestructuring(b: *Builder, pattern: ast.ArrayBindingPattern, array: Ir.Inst.Ref, binding_op: BindingOp) Error!Ir.Inst.Ref {
    var last_ref: Ir.Inst.Ref = .none;

    const iterator_ref = try b.addInst(.{
        .tag = .get_iterator,
        .data = .{ .ref = array },
    });

    for (pattern.elements) |element| switch (element) {
        .elision => {
            _ = try b.addInst(.{
                .tag = .iterator_step,
                .data = .{ .ref = iterator_ref },
            });
        },
        .binding_element => |binding_element| {
            const next_value = try b.addInst(.{
                .tag = .iterator_step_value,
                .data = .{ .ref = iterator_ref },
            });
            switch (binding_element) {
                .single_name_binding => |binding| {
                    const string_index = try b.internString(binding.binding_identifier);
                    const default_expr = if (binding.initializer) |*expr| expr else null;
                    const value = try b.lowerDefaultExpression(next_value, default_expr);
                    const tag: Ir.Inst.Tag = switch (binding_op) {
                        .initialize => .initialize_binding,
                        .set => .set_binding,
                    };
                    last_ref = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = value,
                        } },
                    });
                },
                .binding_pattern_and_expression => |bpe| {
                    const default_expr = if (bpe.initializer) |*expr| expr else null;
                    const value = try b.lowerDefaultExpression(next_value, default_expr);
                    last_ref = try b.lowerDestructuringAssignment(bpe.binding_pattern, value, binding_op);
                },
            }
        },
        .binding_rest_element => |rest| {
            const rest_array = try b.addInst(.{
                .tag = .iterator_collect,
                .data = .{ .ref = iterator_ref },
            });
            switch (rest) {
                .binding_identifier => |identifier| {
                    const string_index = try b.internString(identifier);
                    const tag: Ir.Inst.Tag = switch (binding_op) {
                        .initialize => .initialize_binding,
                        .set => .set_binding,
                    };
                    last_ref = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = rest_array,
                        } },
                    });
                },
                .binding_pattern => |bp| {
                    last_ref = try b.lowerDestructuringAssignment(bp, rest_array, binding_op);
                },
            }
        },
    };

    return last_ref;
}

fn lowerObjectDestructuring(b: *Builder, pattern: ast.ObjectBindingPattern, object: Ir.Inst.Ref, binding_op: BindingOp) Error!Ir.Inst.Ref {
    var last_ref: Ir.Inst.Ref = .none;

    for (pattern.properties) |property| switch (property) {
        .binding_property => |binding_property| switch (binding_property) {
            .single_name_binding => |binding| {
                const string_index = try b.internString(binding.binding_identifier);
                const prop_value = try b.addInst(.{
                    .tag = .get_property,
                    .data = .{ .get_property = .{
                        .base = object,
                        .name = string_index,
                    } },
                });
                const default_expr = if (binding.initializer) |*expr| expr else null;
                const value = try b.lowerDefaultExpression(prop_value, default_expr);
                const tag: Ir.Inst.Tag = switch (binding_op) {
                    .initialize => .initialize_binding,
                    .set => .set_binding,
                };
                last_ref = try b.addInst(.{
                    .tag = tag,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = value,
                    } },
                });
            },
            .property_name_and_binding_element => |pnbe| {
                const key_ref = try b.lowerPropertyName(pnbe.property_name);
                const prop_value = try b.addInst(.{
                    .tag = .get_property_computed,
                    .data = .{ .get_property_computed = .{
                        .base = object,
                        .property = key_ref,
                    } },
                });
                switch (pnbe.binding_element) {
                    .single_name_binding => |binding| {
                        const string_index = try b.internString(binding.binding_identifier);
                        const default_expr = if (binding.initializer) |*expr| expr else null;
                        const value = try b.lowerDefaultExpression(prop_value, default_expr);
                        const tag: Ir.Inst.Tag = switch (binding_op) {
                            .initialize => .initialize_binding,
                            .set => .set_binding,
                        };
                        last_ref = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .set_binding = .{
                                .name = string_index,
                                .value = value,
                            } },
                        });
                    },
                    .binding_pattern_and_expression => |bpe| {
                        const default_expr = if (bpe.initializer) |*expr| expr else null;
                        const value = try b.lowerDefaultExpression(prop_value, default_expr);
                        last_ref = try b.lowerDestructuringAssignment(bpe.binding_pattern, value, binding_op);
                    },
                }
            },
        },
        .binding_rest_property => |rest_property| {
            var excluded_names: std.ArrayList(Ir.Inst.Ref) = .empty;
            defer excluded_names.deinit(b.gpa);

            for (pattern.properties) |p| switch (p) {
                .binding_property => |binding_property| switch (binding_property) {
                    .single_name_binding => |binding| {
                        const string_index = try b.internString(binding.binding_identifier);
                        const name_ref = try b.addInst(.{
                            .tag = .string,
                            .data = .{ .string = string_index },
                        });
                        try excluded_names.append(b.gpa, name_ref);
                    },
                    .property_name_and_binding_element => |pnbe| {
                        const key_ref = try b.lowerPropertyName(pnbe.property_name);
                        try excluded_names.append(b.gpa, key_ref);
                    },
                },
                .binding_rest_property => {},
            };

            const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extras.items.len);
            const len: u32 = @intCast(excluded_names.items.len);
            try b.extras.appendSlice(b.gpa, @ptrCast(excluded_names.items));

            const rest_obj = try b.addInst(.{
                .tag = .copy_data_properties,
                .data = .{ .copy_data_properties = .{
                    .source = object,
                    .extra_index = extra_index,
                    .len = len,
                } },
            });

            const string_index = try b.internString(rest_property.binding_identifier);
            const tag: Ir.Inst.Tag = switch (binding_op) {
                .initialize => .initialize_binding,
                .set => .set_binding,
            };
            last_ref = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_binding = .{
                    .name = string_index,
                    .value = rest_obj,
                } },
            });
        },
    };

    return last_ref;
}

fn lowerExpression(b: *Builder, expr: *const ast.Expression) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, expr)) |constant| {
        defer constant.deinit(b.gpa);
        return b.lowerConstant(constant);
    }
    return switch (expr.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .this => try b.lowerThis(),
            .identifier_reference => |identifier| try b.lowerIdentifierReference(identifier),
            .literal => unreachable, // Guaranteed to constant-fold
            .array_literal => |*array_lit| try b.lowerArrayLiteral(array_lit),
            .object_literal => |*object_lit| try b.lowerObjectLiteral(object_lit),
            .function_expression => try b.todo("function expression"),
            .class_expression => try b.todo("class expression"),
            .generator_expression => try b.todo("generator expression"),
            .async_function_expression => try b.todo("async function expression"),
            .async_generator_expression => try b.todo("async generator expression"),
            .regular_expression_literal => |*regexp_lit| try b.lowerRegularExpressionLiteral(regexp_lit),
            .template_literal => |*template_lit| try b.lowerTemplateLiteral(template_lit),
            .arrow_function => try b.todo("arrow function"),
            .async_arrow_function => try b.todo("async arrow function"),
        },
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, null),
        .super_property => try b.todo("super property"),
        .meta_property => try b.todo("meta property"),
        .new_expression => |*new_expr| try b.lowerNewExpression(new_expr),
        .call_expression => |*call_expr| try b.lowerCallExpression(call_expr),
        .super_call => try b.todo("super call"),
        .import_call => try b.todo("import call"),
        .optional_expression => try b.todo("optional expression"),
        .update_expression => |*update_expr| try b.lowerUpdateExpression(update_expr),
        .unary_expression => |*unary_expr| try b.lowerUnaryExpression(unary_expr),
        .binary_expression => |*bin_expr| try b.lowerBinaryExpression(bin_expr),
        .relational_expression => |*rel_expr| try b.lowerRelationalExpression(rel_expr),
        .equality_expression => |*eq_expr| try b.lowerEqualityExpression(eq_expr),
        .logical_expression => |*log_expr| try b.lowerLogicalExpression(log_expr),
        .conditional_expression => |*cond_expr| try b.lowerConditionalExpression(cond_expr),
        .assignment_expression => |*assign_expr| try b.lowerAssignmentExpression(assign_expr),
        .sequence_expression => |*seq_expr| try b.lowerSequenceExpression(seq_expr),
        .await_expression => try b.todo("await expression"),
        .yield_expression => try b.todo("yield expression"),
        .tagged_template => try b.todo("tagged template"),
        .binding_pattern_for_assignment_expression => try b.todo("binding pattern for assignment expression"),
    };
}

fn lowerThis(b: *Builder) Error!Ir.Inst.Ref {
    return b.addInst(.{
        .tag = .this,
        .data = .{ .none = {} },
    });
}

fn lowerIdentifierReference(b: *Builder, identifier: []const u8) Error!Ir.Inst.Ref {
    const string_index = try b.internString(identifier);
    return b.addInst(.{
        .tag = .get_binding,
        .data = .{ .string = string_index },
    });
}

fn lowerArrayLiteral(b: *Builder, array_lit: *const ast.ArrayLiteral) Error!Ir.Inst.Ref {
    var elements: std.ArrayListUnmanaged(Ir.Inst.Ref) = .empty;
    defer elements.deinit(b.gpa);

    for (array_lit.element_list) |elem| {
        const elem_ref: Ir.Inst.Ref = switch (elem) {
            .elision => .none,
            .expression => |*expr| try b.lowerExpression(expr),
            .spread => |*expr| blk: {
                const value = try b.lowerExpression(expr);
                break :blk try b.addInst(.{
                    .tag = .spread,
                    .data = .{ .ref = value },
                });
            },
        };
        try elements.append(b.gpa, elem_ref);
    }

    const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extras.items.len);
    const len: u32 = @intCast(elements.items.len);
    try b.extras.appendSlice(b.gpa, @ptrCast(elements.items));

    return b.addInst(.{
        .tag = .array,
        .data = .{ .array = .{
            .extra_index = extra_index,
            .len = len,
        } },
    });
}

fn lowerObjectLiteral(b: *Builder, object_lit: *const ast.ObjectLiteral) Error!Ir.Inst.Ref {
    var pairs: std.ArrayListUnmanaged(Ir.Inst.Ref) = .empty;
    defer pairs.deinit(b.gpa);

    for (object_lit.property_definition_list.items) |prop_def| {
        switch (prop_def) {
            .identifier_reference => |identifier| {
                const string_index = try b.internString(identifier);
                const key_ref = try b.addInst(.{
                    .tag = .string,
                    .data = .{ .string = string_index },
                });
                const value_ref = try b.addInst(.{
                    .tag = .get_binding,
                    .data = .{ .string = string_index },
                });
                try pairs.append(b.gpa, key_ref);
                try pairs.append(b.gpa, value_ref);
            },
            .spread => |*expr| {
                const value_ref = try b.lowerExpression(expr);
                const spread_ref = try b.addInst(.{
                    .tag = .spread,
                    .data = .{ .ref = value_ref },
                });
                try pairs.append(b.gpa, .none);
                try pairs.append(b.gpa, spread_ref);
            },
            .method_definition => try b.todo("method definition in object literal"),
            .property_name_and_expression => |*prop| {
                const key_ref = try b.lowerPropertyName(prop.property_name);
                const value_ref = try b.lowerExpression(&prop.expression);
                try pairs.append(b.gpa, key_ref);
                try pairs.append(b.gpa, value_ref);
            },
        }
    }

    const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extras.items.len);
    const len: u32 = @intCast(pairs.items.len / 2);
    try b.extras.appendSlice(b.gpa, @ptrCast(pairs.items));

    return b.addInst(.{
        .tag = .object,
        .data = .{ .object = .{
            .extra_index = extra_index,
            .len = len,
        } },
    });
}

fn lowerRegularExpressionLiteral(b: *Builder, regexp_lit: *const ast.RegularExpressionLiteral) Error!Ir.Inst.Ref {
    const pattern_index = try b.internString(regexp_lit.pattern);
    const flags_index = try b.internString(regexp_lit.flags);
    return b.addInst(.{
        .tag = .reg_exp,
        .data = .{ .reg_exp = .{
            .pattern = pattern_index,
            .flags = flags_index,
        } },
    });
}

fn lowerTemplateLiteral(b: *Builder, template_lit: *const ast.TemplateLiteral) Error!Ir.Inst.Ref {
    var result: Ir.Inst.Ref = .none;
    for (template_lit.spans, 0..) |span, i| {
        std.debug.assert(if (i % 2 == 0) span == .text else span == .expression);
        const span_ref: Ir.Inst.Ref = switch (span) {
            .expression => |expr| blk: {
                const expr_ref = try b.lowerExpression(&expr);
                break :blk try b.addInst(.{
                    .tag = .to_string,
                    .data = .{ .ref = expr_ref },
                });
            },
            .text => blk: {
                const chars = span.templateCharacters();
                if (chars.len == 0) continue;
                const normalized = try std.mem.replaceOwned(
                    u8,
                    b.gpa,
                    chars,
                    "\r\n",
                    "\n",
                );
                defer b.gpa.free(normalized);
                _ = std.mem.replaceScalar(u8, normalized, '\r', '\n');
                const string_index = try b.internString(normalized);
                break :blk try b.addInst(.{
                    .tag = .string,
                    .data = .{ .string = string_index },
                });
            },
        };

        if (result == .none) {
            result = span_ref;
        } else {
            result = try b.addInst(.{
                .tag = .add,
                .data = .{ .binary = .{
                    .lhs = result,
                    .rhs = span_ref,
                } },
            });
        }
    }
    return result;
}

fn lowerMemberExpression(b: *Builder, member_expr: *const ast.MemberExpression, base_out: ?*Ir.Inst.Ref) Error!Ir.Inst.Ref {
    const base = try b.lowerExpression(member_expr.expression);
    if (base_out) |ptr| ptr.* = base;
    switch (member_expr.property) {
        .expression => |expr| {
            if (try constantFold(b.gpa, expr)) |constant| {
                defer constant.deinit(b.gpa);
                if (constant.toIndex()) |index| {
                    return b.addInst(.{
                        .tag = .get_property_indexed,
                        .data = .{ .get_property_indexed = .{
                            .base = base,
                            .index = index,
                        } },
                    });
                }
            }
            const property = try b.lowerExpression(expr);
            return b.addInst(.{
                .tag = .get_property_computed,
                .data = .{ .get_property_computed = .{
                    .base = base,
                    .property = property,
                } },
            });
        },
        .identifier => |identifier| {
            const string_index = try b.internString(identifier);
            return b.addInst(.{
                .tag = .get_property,
                .data = .{ .get_property = .{
                    .base = base,
                    .name = string_index,
                } },
            });
        },
        .private_identifier => try b.todo("private identifier in member expression"),
    }
}

fn lowerArguments(b: *Builder, arguments: ast.Arguments) Error!std.ArrayList(Ir.Inst.Ref) {
    var args: std.ArrayList(Ir.Inst.Ref) = try .initCapacity(b.gpa, arguments.len);
    errdefer args.deinit(b.gpa);

    for (arguments) |arg| {
        const arg_ref: Ir.Inst.Ref = switch (arg) {
            .expression => |*expr| try b.lowerExpression(expr),
            .spread => |*expr| blk: {
                const value = try b.lowerExpression(expr);
                break :blk try b.addInst(.{
                    .tag = .spread,
                    .data = .{ .ref = value },
                });
            },
        };
        args.appendAssumeCapacity(arg_ref);
    }

    return args;
}

fn lowerNewExpression(b: *Builder, new_expr: *const ast.NewExpression) Error!Ir.Inst.Ref {
    const constructor = try b.lowerExpression(new_expr.expression);

    var args = try b.lowerArguments(new_expr.arguments);
    defer args.deinit(b.gpa);

    const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extras.items.len);
    const len: u32 = @intCast(args.items.len);
    try b.extras.appendSlice(b.gpa, @ptrCast(args.items));

    return b.addInst(.{
        .tag = .construct,
        .data = .{ .construct = .{
            .constructor = constructor,
            .extra_index = extra_index,
            .len = len,
        } },
    });
}

fn lowerCallExpression(b: *Builder, call_expr: *const ast.CallExpression) Error!Ir.Inst.Ref {
    var this_value: Ir.Inst.Ref = .none;
    const callee = switch (call_expr.expression.*) {
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, &this_value),
        else => try b.lowerExpression(call_expr.expression),
    };

    var args = try b.lowerArguments(call_expr.arguments);
    defer args.deinit(b.gpa);

    const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extras.items.len);
    const len: u32 = @intCast(args.items.len);
    try b.extras.appendSlice(b.gpa, @ptrCast(args.items));

    return b.addInst(.{
        .tag = .call,
        .data = .{ .call = .{
            .callee = callee,
            .this_value = this_value,
            .extra_index = extra_index,
            .len = len,
        } },
    });
}

fn lowerUpdateExpression(b: *Builder, update_expr: *const ast.UpdateExpression) Error!Ir.Inst.Ref {
    const update_type: Ir.Inst.UpdateType = switch (update_expr.type) {
        .prefix => .prefix,
        .postfix => .postfix,
    };
    const update_op: Ir.Inst.UpdateOp = switch (update_expr.operator) {
        .@"++" => .increment,
        .@"--" => .decrement,
    };
    switch (update_expr.expression.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .identifier_reference => |identifier| {
                const string_index = try b.internString(identifier);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode) .update_binding_strict else .update_binding;
                return b.addInst(.{
                    .tag = tag,
                    .data = .{ .update_binding = .{
                        .name = string_index,
                        .update_op = update_op,
                        .update_type = update_type,
                    } },
                });
            },
            else => try b.todo("non-identifier update expression"),
        },
        .member_expression => |*member_expr| {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .update_property_strict
                    else
                        .update_property;
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .update_property = .{
                            .base = base,
                            .name = string_index,
                            .update_op = update_op,
                            .update_type = update_type,
                        } },
                    });
                },
                .expression => |expr| {
                    if (try constantFold(b.gpa, expr)) |constant| {
                        defer constant.deinit(b.gpa);
                        if (constant.toIndex()) |index| {
                            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                                .update_property_indexed_strict
                            else
                                .update_property_indexed;
                            return b.addInst(.{
                                .tag = tag,
                                .data = .{ .update_property_indexed = .{
                                    .base = base,
                                    .index = index,
                                    .update_op = update_op,
                                    .update_type = update_type,
                                } },
                            });
                        }
                    }
                    const property = try b.lowerExpression(expr);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .update_property_computed_strict
                    else
                        .update_property_computed;
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .update_property_computed = .{
                            .base = base,
                            .property = property,
                            .update_op = update_op,
                            .update_type = update_type,
                        } },
                    });
                },
                .private_identifier => try b.todo("private identifier update expression"),
            }
        },
        else => try b.todo("non-identifier update expression"),
    }
}

fn lowerUnaryExpression(b: *Builder, unary_expr: *const ast.UnaryExpression) Error!Ir.Inst.Ref {
    if (unary_expr.operator == .delete and
        unary_expr.expression.* == .primary_expression and
        unary_expr.expression.primary_expression == .identifier_reference)
    {
        const identifier = unary_expr.expression.primary_expression.identifier_reference;
        const string_index = try b.internString(identifier);
        return b.addInst(.{
            .tag = .delete_binding,
            .data = .{ .string = string_index },
        });
    }
    if (unary_expr.operator == .delete and unary_expr.expression.* == .member_expression) {
        const member_expr = unary_expr.expression.member_expression;
        const base = try b.lowerExpression(member_expr.expression);
        switch (member_expr.property) {
            .expression => |expr| {
                if (try constantFold(b.gpa, expr)) |constant| {
                    defer constant.deinit(b.gpa);
                    if (constant.toIndex()) |index| {
                        const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                            .delete_property_indexed_strict
                        else
                            .delete_property_indexed;
                        return b.addInst(.{
                            .tag = tag,
                            .data = .{ .delete_property_indexed = .{
                                .base = base,
                                .index = index,
                            } },
                        });
                    }
                }
                const property = try b.lowerExpression(expr);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .delete_property_computed_strict
                else
                    .delete_property_computed;
                return b.addInst(.{
                    .tag = tag,
                    .data = .{ .delete_property_computed = .{
                        .base = base,
                        .property = property,
                    } },
                });
            },
            .identifier => |identifier| {
                const string_index = try b.internString(identifier);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .delete_property_strict
                else
                    .delete_property;
                return b.addInst(.{
                    .tag = tag,
                    .data = .{ .delete_property = .{
                        .base = base,
                        .name = string_index,
                    } },
                });
            },
            .private_identifier => try b.todo("private identifier in delete expression"),
        }
    }
    const operand = try b.lowerExpression(unary_expr.expression);
    const tag: Ir.Inst.Tag = switch (unary_expr.operator) {
        .@"+" => .to_number,
        .@"-" => .negate,
        .@"~" => .bitwise_not,
        .@"!" => .logical_not,
        .typeof => .typeof,
        .void => .void,
        .delete => .delete,
    };
    return b.addInst(.{
        .tag = tag,
        .data = .{ .ref = operand },
    });
}

fn lowerBinaryExpression(b: *Builder, bin_expr: *const ast.BinaryExpression) Error!Ir.Inst.Ref {
    const lhs = try b.lowerExpression(bin_expr.lhs_expression);
    const rhs = try b.lowerExpression(bin_expr.rhs_expression);
    const tag: Ir.Inst.Tag = switch (bin_expr.operator) {
        .@"+" => .add,
        .@"-" => .sub,
        .@"*" => .mul,
        .@"/" => .div,
        .@"%" => .rem,
        .@"**" => .exp,
        .@"<<" => .shift_left,
        .@">>" => .shift_right,
        .@">>>" => .shift_right_unsigned,
        .@"&" => .bitwise_and,
        .@"^" => .bitwise_xor,
        .@"|" => .bitwise_or,
    };
    return b.addInst(.{
        .tag = tag,
        .data = .{ .binary = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
    });
}

fn lowerRelationalExpression(b: *Builder, rel_expr: *const ast.RelationalExpression) Error!Ir.Inst.Ref {
    const lhs = switch (rel_expr.lhs) {
        .expression => |expr| try b.lowerExpression(expr),
        .private_identifier => try b.todo("private identifier"),
    };
    const rhs = try b.lowerExpression(rel_expr.rhs_expression);
    const tag: Ir.Inst.Tag = switch (rel_expr.operator) {
        .@"<" => .lt,
        .@">" => .gt,
        .@"<=" => .lt_eq,
        .@">=" => .gt_eq,
        .instanceof => .instanceof,
        .in => .in,
    };
    return b.addInst(.{
        .tag = tag,
        .data = .{ .binary = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
    });
}

fn lowerEqualityExpression(b: *Builder, eq_expr: *const ast.EqualityExpression) Error!Ir.Inst.Ref {
    const lhs = try b.lowerExpression(eq_expr.lhs_expression);
    const rhs = try b.lowerExpression(eq_expr.rhs_expression);
    const tag: Ir.Inst.Tag = switch (eq_expr.operator) {
        .@"==" => .eq,
        .@"!=" => .not_eq,
        .@"===" => .eq_strict,
        .@"!==" => .not_eq_strict,
    };
    return b.addInst(.{
        .tag = tag,
        .data = .{ .binary = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
    });
}

fn lowerLogicalExpression(b: *Builder, log_expr: *const ast.LogicalExpression) Error!Ir.Inst.Ref {
    const lhs = try b.lowerExpression(log_expr.lhs_expression);
    const rhs = try b.lowerExpression(log_expr.rhs_expression);
    const tag: Ir.Inst.Tag = switch (log_expr.operator) {
        .@"&&" => .logical_and,
        .@"||" => .logical_or,
        .@"??" => .nullish_coalesce,
    };
    return b.addInst(.{
        .tag = tag,
        .data = .{ .binary = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
    });
}

fn lowerConditionalExpression(b: *Builder, cond_expr: *const ast.ConditionalExpression) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, cond_expr.test_expression)) |constant| {
        defer constant.deinit(b.gpa);
        return if (constant.isTruthy())
            try b.lowerExpression(cond_expr.consequent_expression)
        else
            try b.lowerExpression(cond_expr.alternate_expression);
    }

    const test_result = try b.lowerExpression(cond_expr.test_expression);
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const then_label = try b.addLabel();
    const then_value = try b.lowerExpression(cond_expr.consequent_expression);
    const then_br = try b.addInstDeferred(.br);

    const else_label = try b.addLabel();
    const else_value = try b.lowerExpression(cond_expr.alternate_expression);
    const else_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    test_br_cond.set(.{ .br_cond = .{
        .condition = test_result,
        .then_target = then_label,
        .else_target = else_label,
    } });
    then_br.set(.{ .br = .{
        .target = end_label,
        .value = then_value,
    } });
    else_br.set(.{ .br = .{
        .target = end_label,
        .value = else_value,
    } });

    return end_label;
}

fn lowerAssignmentExpression(b: *Builder, assign_expr: *const ast.AssignmentExpression) Error!Ir.Inst.Ref {
    return switch (assign_expr.operator) {
        .@"=" => try b.lowerSimpleAssignmentExpression(assign_expr),
        .@"&&=", .@"||=", .@"??=" => try b.lowerLogicalCompoundAssignmentExpression(assign_expr),
        else => try b.lowerBinaryCompoundAssignmentExpression(assign_expr),
    };
}

fn lowerSimpleAssignmentExpression(b: *Builder, assign_expr: *const ast.AssignmentExpression) Error!Ir.Inst.Ref {
    switch (assign_expr.lhs_expression.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .identifier_reference => |identifier| {
                const value = try b.lowerExpression(assign_expr.rhs_expression);
                const string_index = try b.internString(identifier);
                return b.addInst(.{
                    .tag = if (b.in_strict_mode)
                        .set_binding_strict
                    else
                        .set_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = value,
                    } },
                });
            },
            else => try b.todo("non-identifier lhs"),
        },
        .member_expression => |*member_expr| {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const value = try b.lowerExpression(assign_expr.rhs_expression);
                    const string_index = try b.internString(identifier);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_strict
                    else
                        .set_property;
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_property = .{
                            .base = base,
                            .name = string_index,
                            .value = value,
                        } },
                    });
                },
                .expression => |expr| {
                    if (try constantFold(b.gpa, expr)) |constant| {
                        defer constant.deinit(b.gpa);
                        if (constant.toIndex()) |index| {
                            const value = try b.lowerExpression(assign_expr.rhs_expression);
                            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                                .set_property_indexed_strict
                            else
                                .set_property_indexed;
                            return b.addInst(.{
                                .tag = tag,
                                .data = .{ .set_property_indexed = .{
                                    .base = base,
                                    .index = index,
                                    .value = value,
                                } },
                            });
                        }
                    }
                    const property = try b.lowerExpression(expr);
                    const value = try b.lowerExpression(assign_expr.rhs_expression);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_computed_strict
                    else
                        .set_property_computed;
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_property_computed = .{
                            .base = base,
                            .property = property,
                            .value = value,
                        } },
                    });
                },
                .private_identifier => try b.todo("private identifier in member assignment"),
            }
        },
        else => try b.todo("non-identifier lhs"),
    }
}

fn lowerBinaryCompoundAssignmentExpression(b: *Builder, assign_expr: *const ast.AssignmentExpression) Error!Ir.Inst.Ref {
    const binary_tag: Ir.Inst.Tag = switch (assign_expr.operator) {
        .@"+=" => .add,
        .@"-=" => .sub,
        .@"*=" => .mul,
        .@"/=" => .div,
        .@"%=" => .rem,
        .@"**=" => .exp,
        .@"<<=" => .shift_left,
        .@">>=" => .shift_right,
        .@">>>=" => .shift_right_unsigned,
        .@"&=" => .bitwise_and,
        .@"^=" => .bitwise_xor,
        .@"|=" => .bitwise_or,
        else => unreachable,
    };
    switch (assign_expr.lhs_expression.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .identifier_reference => |identifier| {
                const string_index = try b.internString(identifier);
                const current_value = try b.addInst(.{
                    .tag = .get_binding,
                    .data = .{ .string = string_index },
                });
                const rhs = try b.lowerExpression(assign_expr.rhs_expression);
                const result = try b.addInst(.{
                    .tag = binary_tag,
                    .data = .{ .binary = .{
                        .lhs = current_value,
                        .rhs = rhs,
                    } },
                });
                return b.addInst(.{
                    .tag = if (b.in_strict_mode) .set_binding_strict else .set_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = result,
                    } },
                });
            },
            else => try b.todo("non-identifier compound assignment lhs"),
        },
        .member_expression => |*member_expr| {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier);
                    const current_value = try b.addInst(.{
                        .tag = .get_property,
                        .data = .{ .get_property = .{
                            .base = base,
                            .name = string_index,
                        } },
                    });
                    const rhs = try b.lowerExpression(assign_expr.rhs_expression);
                    const result = try b.addInst(.{
                        .tag = binary_tag,
                        .data = .{ .binary = .{
                            .lhs = current_value,
                            .rhs = rhs,
                        } },
                    });
                    const set_tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_strict
                    else
                        .set_property;
                    return b.addInst(.{
                        .tag = set_tag,
                        .data = .{ .set_property = .{
                            .base = base,
                            .name = string_index,
                            .value = result,
                        } },
                    });
                },
                .expression => |expr| {
                    if (try constantFold(b.gpa, expr)) |constant| {
                        defer constant.deinit(b.gpa);
                        if (constant.toIndex()) |index| {
                            const current_value = try b.addInst(.{
                                .tag = .get_property_indexed,
                                .data = .{ .get_property_indexed = .{
                                    .base = base,
                                    .index = index,
                                } },
                            });
                            const rhs = try b.lowerExpression(assign_expr.rhs_expression);
                            const result = try b.addInst(.{
                                .tag = binary_tag,
                                .data = .{ .binary = .{
                                    .lhs = current_value,
                                    .rhs = rhs,
                                } },
                            });
                            const set_tag: Ir.Inst.Tag = if (b.in_strict_mode)
                                .set_property_indexed_strict
                            else
                                .set_property_indexed;
                            return b.addInst(.{
                                .tag = set_tag,
                                .data = .{ .set_property_indexed = .{
                                    .base = base,
                                    .index = index,
                                    .value = result,
                                } },
                            });
                        }
                    }
                    const property = try b.lowerExpression(expr);
                    const current_value = try b.addInst(.{
                        .tag = .get_property_computed,
                        .data = .{ .get_property_computed = .{
                            .base = base,
                            .property = property,
                        } },
                    });
                    const rhs = try b.lowerExpression(assign_expr.rhs_expression);
                    const result = try b.addInst(.{
                        .tag = binary_tag,
                        .data = .{ .binary = .{
                            .lhs = current_value,
                            .rhs = rhs,
                        } },
                    });
                    const set_tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_computed_strict
                    else
                        .set_property_computed;
                    return b.addInst(.{
                        .tag = set_tag,
                        .data = .{ .set_property_computed = .{
                            .base = base,
                            .property = property,
                            .value = result,
                        } },
                    });
                },
                .private_identifier => try b.todo("private identifier in binary compound assignment"),
            }
        },
        else => try b.todo("non-identifier lhs in binary compound assignment"),
    }
}

fn lowerLogicalCompoundAssignmentExpression(b: *Builder, assign_expr: *const ast.AssignmentExpression) Error!Ir.Inst.Ref {
    const Lhs = union(enum) {
        binding: Ir.Inst.StringIndex,
        property: struct { base: Ir.Inst.Ref, name: Ir.Inst.StringIndex },
        property_indexed: struct { base: Ir.Inst.Ref, index: u32 },
        property_computed: struct { base: Ir.Inst.Ref, property: Ir.Inst.Ref },
    };

    var lhs: Lhs = undefined;
    const current_value: Ir.Inst.Ref = switch (assign_expr.lhs_expression.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .identifier_reference => |identifier| blk: {
                const string_index = try b.internString(identifier);
                lhs = .{ .binding = string_index };
                break :blk try b.addInst(.{
                    .tag = .get_binding,
                    .data = .{ .string = string_index },
                });
            },
            else => return try b.todo("non-identifier logical compound assignment lhs"),
        },
        .member_expression => |*member_expr| blk: {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier);
                    lhs = .{ .property = .{
                        .base = base,
                        .name = string_index,
                    } };
                    break :blk try b.addInst(.{
                        .tag = .get_property,
                        .data = .{ .get_property = .{
                            .base = base,
                            .name = string_index,
                        } },
                    });
                },
                .expression => |expr| {
                    if (try constantFold(b.gpa, expr)) |constant| {
                        defer constant.deinit(b.gpa);
                        if (constant.toIndex()) |index| {
                            lhs = .{ .property_indexed = .{
                                .base = base,
                                .index = index,
                            } };
                            break :blk try b.addInst(.{
                                .tag = .get_property_indexed,
                                .data = .{ .get_property_indexed = .{
                                    .base = base,
                                    .index = index,
                                } },
                            });
                        }
                    }
                    const property = try b.lowerExpression(expr);
                    lhs = .{ .property_computed = .{
                        .base = base,
                        .property = property,
                    } };
                    break :blk try b.addInst(.{
                        .tag = .get_property_computed,
                        .data = .{ .get_property_computed = .{
                            .base = base,
                            .property = property,
                        } },
                    });
                },
                .private_identifier => return try b.todo("private identifier in logical compound assignment"),
            }
        },
        else => return try b.todo("non-identifier lhs in logical compound assignment"),
    };

    var condition: Ir.Inst.Ref = undefined;
    var assign_on_true: bool = undefined;
    switch (assign_expr.operator) {
        .@"&&=" => {
            condition = current_value;
            assign_on_true = true;
        },
        .@"||=" => {
            condition = current_value;
            assign_on_true = false;
        },
        .@"??=" => {
            const null_ref = try b.addInst(.{
                .tag = .null,
                .data = .{ .none = {} },
            });
            condition = try b.addInst(.{
                .tag = .eq,
                .data = .{ .binary = .{
                    .lhs = current_value,
                    .rhs = null_ref,
                } },
            });
            assign_on_true = true;
        },
        else => unreachable,
    }
    const br_cond_inst = try b.addInstDeferred(.br_cond);

    const assign_label = try b.addLabel();

    const rhs = try b.lowerExpression(assign_expr.rhs_expression);
    switch (lhs) {
        .binding => |name| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_binding_strict
            else
                .set_binding;
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_binding = .{
                    .name = name,
                    .value = rhs,
                } },
            });
        },
        .property => |p| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_property_strict
            else
                .set_property;
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property = .{
                    .base = p.base,
                    .name = p.name,
                    .value = rhs,
                } },
            });
        },
        .property_indexed => |p| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_property_indexed_strict
            else
                .set_property_indexed;
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property_indexed = .{
                    .base = p.base,
                    .index = p.index,
                    .value = rhs,
                } },
            });
        },
        .property_computed => |p| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_property_computed_strict
            else
                .set_property_computed;
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property_computed = .{
                    .base = p.base,
                    .property = p.property,
                    .value = rhs,
                } },
            });
        },
    }

    const assign_br = try b.addInstDeferred(.br);

    const skip_label = try b.addLabel();
    const skip_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    br_cond_inst.set(.{ .br_cond = .{
        .condition = condition,
        .then_target = if (assign_on_true) assign_label else skip_label,
        .else_target = if (assign_on_true) skip_label else assign_label,
    } });
    assign_br.set(.{ .br = .{
        .target = end_label,
        .value = rhs,
    } });
    skip_br.set(.{ .br = .{
        .target = end_label,
        .value = current_value,
    } });

    return end_label;
}

fn lowerSequenceExpression(b: *Builder, seq_expr: *const ast.SequenceExpression) Error!Ir.Inst.Ref {
    var last: Ir.Inst.Ref = undefined;
    for (seq_expr.expressions) |*expr| {
        last = try b.lowerExpression(expr);
    }
    return last;
}
