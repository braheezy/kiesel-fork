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
        .block_statement => |*block_stmt| try b.lowerBlockStatement(block_stmt),
        .variable_statement => |*var_stmt| try b.lowerVariableStatement(var_stmt),
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

fn lowerDeclaration(b: *Builder, decl: *const ast.Declaration) Error!Ir.Inst.Ref {
    return switch (decl.*) {
        .hoistable_declaration => try b.todo("hoistable declaration"),
        .class_declaration => try b.todo("class declaration"),
        .lexical_declaration => |*lex_decl| try b.lowerLexicalDeclaration(lex_decl),
    };
}

fn lowerBlockStatement(b: *Builder, block_stmt: *const ast.BlockStatement) Error!Ir.Inst.Ref {
    return b.lowerStatementList(&block_stmt.block.statement_list);
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
            if (binding.initializer) |*init_expr| {
                const value = try b.lowerExpression(init_expr);
                const string_index = try b.internString(binding.binding_identifier);
                return b.addInst(.{
                    .tag = .initialize_binding,
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
        const default_value = try b.lowerExpression(expr);
        return b.addInst(.{
            .tag = .@"if",
            .data = .{ .@"if" = .{
                .@"test" = is_undefined,
                .then = default_value,
                .@"else" = value,
            } },
        });
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
            .this => try b.todo("this"),
            .identifier_reference => |identifier| try b.lowerIdentifierReference(identifier),
            .literal => unreachable, // Guaranteed to constant-fold
            .array_literal => |*array_lit| try b.lowerArrayLiteral(array_lit),
            .object_literal => |*object_lit| try b.lowerObjectLiteral(object_lit),
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
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, null),
        .super_property => try b.todo("super property"),
        .meta_property => try b.todo("meta property"),
        .new_expression => try b.todo("new expression"),
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
            .identifier_reference => try b.todo("identifier reference in object literal"),
            .spread => try b.todo("spread in object literal"),
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

fn lowerCallExpression(b: *Builder, call_expr: *const ast.CallExpression) Error!Ir.Inst.Ref {
    var this_value: Ir.Inst.Ref = .none;
    const callee = switch (call_expr.expression.*) {
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, &this_value),
        else => try b.lowerExpression(call_expr.expression),
    };

    var args: std.ArrayListUnmanaged(Ir.Inst.Ref) = try .initCapacity(b.gpa, call_expr.arguments.len);
    defer args.deinit(b.gpa);

    for (call_expr.arguments) |arg| {
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
        .@"+" => .unary_plus,
        .@"-" => .unary_minus,
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

    const @"test" = try b.lowerExpression(cond_expr.test_expression);
    const then = try b.lowerExpression(cond_expr.consequent_expression);
    const @"else" = try b.lowerExpression(cond_expr.alternate_expression);
    return b.addInst(.{
        .tag = .@"if",
        .data = .{ .@"if" = .{
            .@"test" = @"test",
            .then = then,
            .@"else" = @"else",
        } },
    });
}

fn lowerAssignmentExpression(b: *Builder, assign_expr: *const ast.AssignmentExpression) Error!Ir.Inst.Ref {
    if (assign_expr.operator != .@"=") {
        try b.todo("compound assignment");
    }

    return switch (assign_expr.lhs_expression.*) {
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
        .member_expression => |*member_expr| blk: {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const value = try b.lowerExpression(assign_expr.rhs_expression);
                    const string_index = try b.internString(identifier);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_strict
                    else
                        .set_property;
                    break :blk b.addInst(.{
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
                    break :blk b.addInst(.{
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
    };
}

fn lowerSequenceExpression(b: *Builder, seq_expr: *const ast.SequenceExpression) Error!Ir.Inst.Ref {
    var last: Ir.Inst.Ref = undefined;
    for (seq_expr.expressions) |*expr| {
        last = try b.lowerExpression(expr);
    }
    return last;
}
