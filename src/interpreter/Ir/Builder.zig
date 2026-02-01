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
instructions: std.MultiArrayList(Ir.Inst),
strings: std.StringArrayHashMapUnmanaged(void),
big_ints: BigIntArrayHashMapUnmanaged(void),

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

pub const Error = error{ OutOfMemory, NotImplemented };

pub fn init(gpa: std.mem.Allocator, name: []const u8, root_node: Ast) Builder {
    return .{
        .gpa = gpa,
        .name = name,
        .root_node = root_node,
        .instructions = .empty,
        .strings = .empty,
        .big_ints = .empty,
    };
}

pub fn deinit(b: *Builder) void {
    b.instructions.deinit(b.gpa);
    for (b.strings.keys()) |string| b.gpa.free(string);
    b.strings.deinit(b.gpa);
    for (b.big_ints.keys()) |big_int| b.gpa.free(big_int.limbs);
    b.big_ints.deinit(b.gpa);
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

    var liveness = try computeLiveness(b.gpa, instructions);
    errdefer liveness.deinit(b.gpa);

    const live_ranges = try computeLiveRanges(b.gpa, instructions);
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
            const gop = try b.big_ints.getOrPut(b.gpa, big_int);
            if (!gop.found_existing) {
                gop.key_ptr.limbs = try b.gpa.dupe(std.math.big.Limb, big_int.limbs);
            }
            return b.addInst(.{
                .tag = .big_int,
                .data = .{ .big_int = @enumFromInt(gop.index) },
            });
        },
        .string => |string| {
            const gop = try b.strings.getOrPut(b.gpa, string);
            if (!gop.found_existing) {
                gop.key_ptr.* = try b.gpa.dupe(u8, string);
            }
            return b.addInst(.{
                .tag = .string,
                .data = .{ .string = @enumFromInt(gop.index) },
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
        last = switch (item) {
            .statement => |stmt| try b.lowerStatement(stmt),
            .declaration => try b.todo("declaration"),
        };
    }
    return last;
}

fn lowerStatement(b: *Builder, stmt: *const ast.Statement) Error!Ir.Inst.Ref {
    return switch (stmt.*) {
        .block_statement => |*block_stmt| try b.lowerBlockStatement(block_stmt),
        .variable_statement => try b.todo("variable statement"),
        .empty_statement => .none,
        .expression_statement => |expr_stmt| try b.lowerExpression(&expr_stmt.expression),
        .if_statement => |*if_stmt| try b.lowerIfStatement(if_stmt),
        .breakable_statement => |brk_stmt| switch (brk_stmt) {
            .iteration_statement => |iter_stmt| switch (iter_stmt) {
                .do_while_statement => |*do_while_stmt| try b.lowerDoWhileStatement(do_while_stmt),
                .while_statement => |*while_stmt| try b.lowerWhileStatement(while_stmt),
                .for_statement => |*for_stmt| try b.lowerForStatement(for_stmt),
                .for_in_of_statement => try b.todo("for in/of statement"),
            },
            .switch_statement => try b.todo("switch statement"),
        },
        .continue_statement => try b.todo("continue statement"),
        .break_statement => try b.todo("break statement"),
        .return_statement => try b.todo("return statement"),
        .with_statement => try b.todo("with statement"),
        .labelled_statement => try b.todo("labelled statement"),
        .throw_statement => try b.todo("throw statement"),
        .try_statement => try b.todo("try statement"),
        .debugger_statement => .none,
    };
}

fn lowerBlockStatement(b: *Builder, block_stmt: *const ast.BlockStatement) Error!Ir.Inst.Ref {
    return b.lowerStatementList(&block_stmt.block.statement_list);
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

    const @"test" = try b.lowerExpression(&if_stmt.test_expression);
    const then = try b.lowerStatement(if_stmt.consequent_statement);
    const @"else" = if (if_stmt.alternate_statement) |stmt|
        try b.lowerStatement(stmt)
    else
        try b.addInst(.{
            .tag = .undefined,
            .data = .{ .none = {} },
        });
    return b.addInst(.{
        .tag = .@"if",
        .data = .{ .@"if" = .{
            .@"test" = @"test",
            .then = then,
            .@"else" = @"else",
        } },
    });
}

fn lowerDoWhileStatement(b: *Builder, do_while_stmt: *const ast.DoWhileStatement) Error!Ir.Inst.Ref {
    const body = try b.lowerStatement(do_while_stmt.consequent_statement);

    if (try constantFold(b.gpa, &do_while_stmt.test_expression)) |constant| {
        defer constant.deinit(b.gpa);
        if (!constant.isTruthy()) {
            return body;
        }
        return b.addInst(.{
            .tag = .loop,
            .data = .{ .loop = .{
                .body = body,
                .update = .none,
            } },
        });
    }

    const @"test" = try b.lowerExpression(&do_while_stmt.test_expression);
    return b.addInst(.{
        .tag = .@"while",
        .data = .{ .@"while" = .{
            .@"test" = @"test",
            .body = body,
        } },
    });
}

fn lowerWhileStatement(b: *Builder, while_stmt: *const ast.WhileStatement) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, &while_stmt.test_expression)) |constant| {
        defer constant.deinit(b.gpa);
        if (!constant.isTruthy()) {
            return try b.addInst(.{
                .tag = .undefined,
                .data = .{ .none = {} },
            });
        }
        const body = try b.lowerStatement(while_stmt.consequent_statement);
        return b.addInst(.{
            .tag = .loop,
            .data = .{ .loop = .{
                .body = body,
                .update = .none,
            } },
        });
    }

    const @"test" = try b.lowerExpression(&while_stmt.test_expression);
    const body = try b.lowerStatement(while_stmt.consequent_statement);
    return b.addInst(.{
        .tag = .@"while",
        .data = .{ .@"while" = .{
            .@"test" = @"test",
            .body = body,
        } },
    });
}

fn lowerForStatement(b: *Builder, for_stmt: *const ast.ForStatement) Error!Ir.Inst.Ref {
    if (for_stmt.initializer) |initializer| {
        _ = switch (initializer) {
            .expression => |*expr| try b.lowerExpression(expr),
            .variable_statement,
            .lexical_declaration,
            => try b.todo("variable declarations in for loop"),
        };
    }

    const loop = if (for_stmt.test_expression) |*test_expr| blk: {
        if (try constantFold(b.gpa, test_expr)) |constant| {
            defer constant.deinit(b.gpa);
            if (!constant.isTruthy()) {
                return try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });
            }
            break :blk true;
        }
        break :blk false;
    } else true;

    if (loop) {
        const update: Ir.Inst.Ref = if (for_stmt.increment_expression) |*update_expr|
            try b.lowerExpression(update_expr)
        else
            .none;
        const body = try b.lowerStatement(for_stmt.consequent_statement);
        return b.addInst(.{
            .tag = .loop,
            .data = .{ .loop = .{
                .body = body,
                .update = update,
            } },
        });
    }

    const @"test" = try b.lowerExpression(&for_stmt.test_expression.?);
    const update: Ir.Inst.Ref = if (for_stmt.increment_expression) |*update_expr|
        try b.lowerExpression(update_expr)
    else
        .none;
    const body = try b.lowerStatement(for_stmt.consequent_statement);
    return b.addInst(.{
        .tag = .@"for",
        .data = .{ .@"for" = .{
            .@"test" = @"test",
            .update = update,
            .body = body,
        } },
    });
}

fn lowerExpression(b: *Builder, expr: *const ast.Expression) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, expr)) |constant| {
        defer constant.deinit(b.gpa);
        return b.lowerConstant(constant);
    }
    return switch (expr.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .this => try b.todo("this"),
            .identifier_reference => try b.todo("identifier reference"),
            .literal => unreachable, // Guaranteed to constant-fold
            .array_literal => try b.todo("array literal"),
            .object_literal => try b.todo("object literal"),
            .function_expression => try b.todo("function expression"),
            .class_expression => try b.todo("class expression"),
            .generator_expression => try b.todo("generator expression"),
            .async_function_expression => try b.todo("async function expression"),
            .async_generator_expression => try b.todo("async generator expression"),
            .regular_expression_literal => try b.todo("regular expression literal"),
            .template_literal => try b.todo("template literal"),
            .arrow_function => try b.todo("arrow function"),
            .async_arrow_function => try b.todo("async arrow function"),
        },
        .member_expression => try b.todo("member expression"),
        .super_property => try b.todo("super property"),
        .meta_property => try b.todo("meta property"),
        .new_expression => try b.todo("new expression"),
        .call_expression => try b.todo("call expression"),
        .super_call => try b.todo("super call"),
        .import_call => try b.todo("import call"),
        .optional_expression => try b.todo("optional expression"),
        .update_expression => try b.todo("update expression"),
        .unary_expression => try b.todo("unary expression"),
        .binary_expression => |*bin_expr| try b.lowerBinaryExpression(bin_expr),
        .relational_expression => try b.todo("relational expression"),
        .equality_expression => |*eq_expr| try b.lowerEqualityExpression(eq_expr),
        .logical_expression => try b.todo("logical expression"),
        .conditional_expression => try b.todo("conditional expression"),
        .assignment_expression => try b.todo("assignment expression"),
        .sequence_expression => |*seq_expr| try b.lowerSequenceExpression(seq_expr),
        .await_expression => try b.todo("await expression"),
        .yield_expression => try b.todo("yield expression"),
        .tagged_template => try b.todo("tagged template"),
        .binding_pattern_for_assignment_expression => try b.todo("binding pattern for assignment expression"),
    };
}

fn lowerBinaryExpression(b: *Builder, bin_expr: *const ast.BinaryExpression) Error!Ir.Inst.Ref {
    const lhs = try b.lowerExpression(bin_expr.lhs_expression);
    const rhs = try b.lowerExpression(bin_expr.rhs_expression);
    const tag: Ir.Inst.Tag = switch (bin_expr.operator) {
        .@"+" => .add,
        .@"-" => .sub,
        .@"*" => .mul,
        .@"/" => .div,
        else => try b.todo("binary operator"),
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

fn lowerSequenceExpression(b: *Builder, seq_expr: *const ast.SequenceExpression) Error!Ir.Inst.Ref {
    var last: Ir.Inst.Ref = undefined;
    for (seq_expr.expressions) |*expr| {
        last = try b.lowerExpression(expr);
    }
    return last;
}
