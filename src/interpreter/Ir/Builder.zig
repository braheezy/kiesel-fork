const std = @import("std");

const ast = @import("../../language/ast.zig");
const interpreter = @import("../../interpreter.zig");
const utils = @import("../../utils.zig");

const Constant = @import("constant_folding.zig").Constant;
const Ir = interpreter.Ir;

const computeLiveness = @import("liveness.zig").computeLiveness;
const computeLiveRanges = @import("live_ranges.zig").computeLiveRanges;
const constantFold = @import("constant_folding.zig").constantFold;
const containsSlice = utils.containsSlice;

pub const Builder = @This();

gpa: std.mem.Allocator,
name: []const u8,
root_node: Ast,
in_strict_mode: bool,
instructions: std.MultiArrayList(Ir.Inst),
extra: std.ArrayList(u32),
strings: StringArrayHashMapUnmanaged(void),
big_ints: BigIntArrayHashMapUnmanaged(void),
functions: std.ArrayList(Ir.Function),
classes: std.ArrayList(Ir.Class),
breakable_stack: std.ArrayList(*BreakableContext),
scope_depth: u16,
template_object_count: u16,

pub const Ast = union(enum) {
    script: *const ast.Script,
    module: *const ast.Module,
    function: struct {
        parameters: *const ast.FormalParameters,
        body: *const ast.FunctionBody,
    },
    eval: struct {
        script: *const ast.Script,
        strict: bool,
    },
};

const StringKey = struct {
    string: []const u8,
    kind: Ir.StringKind,
};

const StringKind = Ir.StringKind;

const StringContext = struct {
    pub fn hash(_: @This(), key: StringKey) u32 {
        var hasher: std.hash.Wyhash = .init(0);
        hasher.update(@ptrCast(&key.kind));
        hasher.update(key.string);
        return @truncate(hasher.final());
    }

    pub fn eql(_: @This(), a: StringKey, b: StringKey, _: usize) bool {
        return a.kind == b.kind and std.mem.eql(u8, a.string, b.string);
    }
};

fn StringArrayHashMapUnmanaged(comptime V: type) type {
    return std.ArrayHashMapUnmanaged(StringKey, V, StringContext, true);
}

const BigIntContext = struct {
    pub fn hash(_: @This(), key: std.math.big.int.Const) u32 {
        var hasher: std.hash.Wyhash = .init(0);
        hasher.update(@ptrCast(&key.positive));
        for (key.limbs) |limb| {
            hasher.update(@ptrCast(&limb));
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
    scope_depth: u16 = 0,
    iterator_ref: Ir.Inst.Ref = .none,

    const JumpTarget = union(enum) {
        known: Ir.Inst.Ref,
        deferred: std.ArrayList(DeferredJump),
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

pub const Error = error{OutOfMemory};

pub fn init(gpa: std.mem.Allocator, name: []const u8, root_node: Ast) Builder {
    const in_strict_mode = switch (root_node) {
        .script => |script| script.scriptIsStrict(),
        .module => true,
        .function => |function| function.body.strict,
        .eval => |eval| eval.strict or eval.script.scriptIsStrict(),
    };
    return .{
        .gpa = gpa,
        .name = name,
        .root_node = root_node,
        .in_strict_mode = in_strict_mode,
        .instructions = .empty,
        .extra = .empty,
        .strings = .empty,
        .big_ints = .empty,
        .functions = .empty,
        .classes = .empty,
        .breakable_stack = .empty,
        .scope_depth = 0,
        .template_object_count = 0,
    };
}

pub fn deinit(b: *Builder) void {
    b.instructions.deinit(b.gpa);
    b.extra.deinit(b.gpa);
    for (b.strings.keys()) |key| b.gpa.free(key.string);
    b.strings.deinit(b.gpa);
    for (b.big_ints.keys()) |big_int| b.gpa.free(big_int.limbs);
    b.big_ints.deinit(b.gpa);
    b.functions.deinit(b.gpa);
    for (b.classes.items) |class| b.gpa.free(class.element_names);
    b.classes.deinit(b.gpa);
    for (b.breakable_stack.items) |ctx| {
        ctx.deinit(b.gpa);
        b.gpa.destroy(ctx);
    }
    b.breakable_stack.deinit(b.gpa);
}

pub fn build(b: *Builder) Error!Ir {
    const result = switch (b.root_node) {
        .script => |script| try b.lowerScript(script),
        .module => |module| try b.lowerModule(module),
        .function => |function| try b.lowerFunction(function.parameters, function.body),
        .eval => |eval| try b.lowerScript(eval.script),
    };
    _ = try b.addInst(.{
        .tag = .@"return",
        .data = .{ .ref = result },
    });

    const name = try b.gpa.dupe(u8, b.name);
    errdefer b.gpa.free(name);

    var instructions = b.instructions.toOwnedSlice();
    errdefer instructions.deinit(b.gpa);

    const extra = try b.extra.toOwnedSlice(b.gpa);
    errdefer b.gpa.free(extra);

    const strings = try b.gpa.alloc([]const u8, b.strings.count());
    errdefer b.gpa.free(strings);
    const string_kinds = try b.gpa.alloc(Ir.StringKind, b.strings.count());
    errdefer b.gpa.free(string_kinds);
    for (b.strings.keys(), strings, string_kinds) |key, *string, *kind| {
        string.* = key.string;
        kind.* = key.kind;
    }
    b.strings.clearRetainingCapacity(); // Transfer ownership
    errdefer for (strings) |string| b.gpa.free(string);

    const big_ints = try b.gpa.dupe(std.math.big.int.Const, b.big_ints.keys());
    errdefer b.gpa.free(big_ints);
    b.big_ints.clearRetainingCapacity(); // Transfer ownership
    errdefer for (big_ints) |big_int| b.gpa.free(big_int.limbs);

    const functions = try b.functions.toOwnedSlice(b.gpa);
    errdefer b.gpa.free(functions);

    const classes = try b.classes.toOwnedSlice(b.gpa);
    errdefer {
        for (classes) |class| b.gpa.free(class.element_names);
        b.gpa.free(classes);
    }

    var ir: Ir = .{
        .name = name,
        .instructions = instructions,
        .extra = extra,
        .strings = strings,
        .string_kinds = string_kinds,
        .big_ints = big_ints,
        .functions = functions,
        .classes = classes,
        .liveness = undefined,
        .live_ranges = undefined,
    };

    ir.liveness = try computeLiveness(b.gpa, &ir);
    errdefer ir.liveness.deinit(b.gpa);

    ir.live_ranges = try computeLiveRanges(b.gpa, &ir);
    errdefer b.gpa.free(ir.live_ranges);

    return ir;
}

fn addInst(b: *Builder, inst: Ir.Inst) std.mem.Allocator.Error!Ir.Inst.Ref {
    const index: Ir.Inst.Index = @enumFromInt(b.instructions.len);
    try b.instructions.append(b.gpa, inst);
    return index.toRef();
}

const Deferred = struct {
    b: *Builder,
    index: Ir.Inst.Index,

    const Data = union {
        br: Ir.Inst.Br,
        br_cond: Ir.Inst.BrCond,
        exception_handler: Ir.Inst.ExceptionHandler,
    };

    fn set(d: Deferred, data: Data) void {
        const slice = d.b.instructions.slice();
        const i = @intFromEnum(d.index);
        switch (slice.items(.tag)[i]) {
            .br => slice.items(.data)[i] = .{ .br = data.br },
            .br_cond => {
                const ei = @intFromEnum(slice.items(.data)[i].br_cond);
                d.b.extra.items[ei] = @intFromEnum(data.br_cond.condition);
                d.b.extra.items[ei + 1] = @intFromEnum(data.br_cond.then_target);
                d.b.extra.items[ei + 2] = @intFromEnum(data.br_cond.else_target);
            },
            .exception_handler => {
                const ei = @intFromEnum(slice.items(.data)[i].exception_handler);
                d.b.extra.items[ei] = @intFromEnum(data.exception_handler.start);
                d.b.extra.items[ei + 1] = @intFromEnum(data.exception_handler.end);
                d.b.extra.items[ei + 2] = @intFromEnum(data.exception_handler.target);
                d.b.extra.items[ei + 3] = data.exception_handler.scope_depth;
            },
            else => unreachable,
        }
    }
};

fn addInstDeferred(b: *Builder, tag: Ir.Inst.Tag) std.mem.Allocator.Error!Deferred {
    const data: Ir.Inst.Data = switch (tag) {
        .br => .{ .br = undefined },
        .br_cond => blk: {
            const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extra.items.len);
            try b.extra.appendNTimes(b.gpa, undefined, 3);
            break :blk .{ .br_cond = extra_index };
        },
        .exception_handler => blk: {
            const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extra.items.len);
            try b.extra.appendNTimes(b.gpa, undefined, 4);
            break :blk .{ .exception_handler = extra_index };
        },
        else => unreachable,
    };
    const ref = try b.addInst(.{
        .tag = tag,
        .data = data,
    });
    return .{ .b = b, .index = ref.toIndex().? };
}

fn addLabel(b: *Builder) std.mem.Allocator.Error!Ir.Inst.Ref {
    return b.addInst(.{
        .tag = .label,
        .data = .{ .none = {} },
    });
}

fn addExtra(b: *Builder, comptime T: type, extra: T) std.mem.Allocator.Error!Ir.Inst.ExtraIndex {
    const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extra.items.len);
    const fields = @typeInfo(T).@"struct".fields;
    try b.extra.ensureUnusedCapacity(b.gpa, fields.len);
    inline for (fields) |field| {
        const value = @field(extra, field.name);
        b.extra.appendAssumeCapacity(switch (field.type) {
            u32 => value,
            Ir.Inst.Ref,
            Ir.Inst.StringIndex,
            Ir.Inst.UpdateOp,
            => @intFromEnum(value),
            else => unreachable,
        });
    }
    return extra_index;
}

fn addFunction(b: *Builder, function: Ir.Function) std.mem.Allocator.Error!Ir.Inst.FunctionIndex {
    const index: Ir.Inst.FunctionIndex = @enumFromInt(b.functions.items.len);
    try b.functions.append(b.gpa, function);
    return index;
}

fn addClass(b: *Builder, class: Ir.Class) std.mem.Allocator.Error!Ir.Inst.ClassIndex {
    const index: Ir.Inst.ClassIndex = @enumFromInt(b.classes.items.len);
    try b.classes.append(b.gpa, class);
    return index;
}

fn setAnonymousFunctionName(b: *Builder, ref: Ir.Inst.Ref, string_index: Ir.Inst.StringIndex) void {
    const index = ref.toIndex().?;
    const inst = b.instructions.get(@intFromEnum(index));
    switch (inst.tag) {
        .create_function => {
            const function_index = inst.data.create_function;
            const function = &b.functions.items[@intFromEnum(function_index)];
            if (function.name == .none) {
                function.name = .{ .default = string_index };
            }
        },
        .create_class => {
            const class_index = inst.data.create_class;
            const class = &b.classes.items[@intFromEnum(class_index)];
            if (class.name == .none) {
                class.name = .{ .default = string_index };
            }
        },
        else => unreachable,
    }
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

fn internString(b: *Builder, string: []const u8, kind: StringKind) std.mem.Allocator.Error!Ir.Inst.StringIndex {
    const canonical_kind: Ir.StringKind = switch (kind) {
        .literal => .literal,
        .escaped => if (std.mem.findScalar(u8, string, '\\') == null)
            .literal
        else
            .escaped,
    };
    const gop = try b.strings.getOrPut(b.gpa, .{ .string = string, .kind = canonical_kind });
    if (!gop.found_existing) {
        gop.key_ptr.string = try b.gpa.dupe(u8, string);
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
            const string_index = try b.internString(string, .escaped);
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

fn lowerModule(b: *Builder, module: *const ast.Module) Error!Ir.Inst.Ref {
    for (module.module_item_list.items) |module_item| {
        switch (module_item) {
            // InitializeEnvironment is responsible for creating the bindings.
            .import_declaration => {},
            .export_declaration => |*export_decl| _ = try b.lowerExportDeclaration(export_decl),
            .statement_list_item => |stmt_list_item| switch (stmt_list_item) {
                .statement => |stmt| _ = try b.lowerStatement(stmt),
                .declaration => |decl| _ = try b.lowerDeclaration(decl),
            },
        }
    }
    return .none;
}

fn lowerExportDeclaration(b: *Builder, export_decl: *const ast.ExportDeclaration) Error!Ir.Inst.Ref {
    switch (export_decl.*) {
        .export_from,
        .named_exports,
        => return .none,
        .variable_statement => |*var_stmt| return b.lowerVariableStatement(var_stmt),
        .declaration => |decl| return b.lowerDeclaration(decl),
        .default_hoistable_declaration => return .none, // Handled by InitializeEnvironment
        .default_class_declaration => |*class_decl| {
            const value = try b.lowerClassDeclaration(class_decl);
            if (class_decl.identifier == null) {
                const string_index = try b.internString("*default*", .literal);
                _ = try b.addInst(.{
                    .tag = .initialize_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = value,
                    } },
                });
            }
            return .none;
        },
        .default_expression => |*expr| {
            const value = try b.lowerExpression(expr);
            const string_index = try b.internString("*default*", .literal);
            if (expr.isAnonymousFunctionDefinition()) {
                b.setAnonymousFunctionName(value, string_index);
            }
            _ = try b.addInst(.{
                .tag = .initialize_binding,
                .data = .{ .set_binding = .{
                    .name = string_index,
                    .value = value,
                } },
            });
            return .none;
        },
    }
}

fn lowerFunction(b: *Builder, formal_parameters: *const ast.FormalParameters, function_body: *const ast.FunctionBody) Error!Ir.Inst.Ref {
    try b.lowerFunctionDeclarationInstantiation(formal_parameters, function_body);

    switch (function_body.type) {
        .generator, .async_generator => {
            // Emit a synthetic yield after FDI so the rest of the bytecode resumes later.
            _ = try b.addInst(.{
                .tag = .yield,
                .data = .{ .ref = .none },
            });
        },
        else => {},
    }

    _ = try b.lowerStatementList(&function_body.statement_list);
    // Implicit return is added in `build()`
    return .none;
}

/// 10.2.11 FunctionDeclarationInstantiation ( func, argumentsList )
/// https://tc39.es/ecma262/#sec-functiondeclarationinstantiation
fn lowerFunctionDeclarationInstantiation(b: *Builder, formal_parameters: *const ast.FormalParameters, function_body: *const ast.FunctionBody) Error!void {
    // 1. Let calleeContext be the running execution context.
    // NOTE: The function's execution context is set up by `prepareForOrdinaryCall()` at runtime.

    // 2. Let code be func.[[ECMAScriptCode]].
    // NOTE: This is `function_body`.

    // 3. Let strict be func.[[Strict]].
    const strict = b.in_strict_mode;

    // 4. Let formals be func.[[FormalParameters]].
    // NOTE: This is `formal_parameters`.

    // 5. Let parameterNames be the BoundNames of formals.
    var parameter_names: std.ArrayList(ast.Identifier) = .empty;
    defer parameter_names.deinit(b.gpa);
    try formal_parameters.collectBoundNames(b.gpa, &parameter_names);

    // 6. If parameterNames has any duplicate entries, let hasDuplicates be true; else let
    //    hasDuplicates be false.
    const has_duplicates = blk: {
        var seen: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer seen.deinit(b.gpa);
        for (parameter_names.items) |name| {
            const gop = try seen.getOrPut(b.gpa, name);
            if (gop.found_existing) break :blk true;
        }
        break :blk false;
    };

    // 7. Let simpleParameterList be IsSimpleParameterList of formals.
    const simple_parameter_list = formal_parameters.isSimpleParameterList();

    // 8. Let hasParameterExpressions be ContainsExpression of formals.
    const has_parameter_expressions = formal_parameters.containsExpression();

    // 9. Let varNames be the VarDeclaredNames of code.
    var var_names: std.ArrayList(ast.Identifier) = .empty;
    defer var_names.deinit(b.gpa);
    try function_body.collectVarDeclaredNames(b.gpa, &var_names);

    // 10. Let varDeclarations be the VarScopedDeclarations of code.
    var var_declarations: std.ArrayList(ast.VarScopedDeclaration) = .empty;
    defer var_declarations.deinit(b.gpa);
    try function_body.collectVarScopedDeclarations(b.gpa, &var_declarations);

    // 11. Let lexicalNames be the LexicallyDeclaredNames of code.
    var lexical_names: std.ArrayList(ast.Identifier) = .empty;
    defer lexical_names.deinit(b.gpa);
    try function_body.collectLexicallyDeclaredNames(b.gpa, &lexical_names);

    // 12. Let functionNames be a new empty List.
    var function_names: std.StringArrayHashMapUnmanaged(void) = .empty;
    defer function_names.deinit(b.gpa);

    // 13. Let functionsToInitialize be a new empty List.
    var functions_to_initialize: std.ArrayList(ast.HoistableDeclaration) = .empty;
    defer functions_to_initialize.deinit(b.gpa);

    // 14. For each element d of varDeclarations, in reverse List order, do
    var it = std.mem.reverseIterator(var_declarations.items);
    while (it.next()) |var_declaration| {
        // a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
        if (var_declaration == .hoistable_declaration) {
            // i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an
            //    AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
            const hoistable_declaration = var_declaration.hoistable_declaration;

            // ii. Let fn be the sole element of the BoundNames of d.
            const function_name = switch (hoistable_declaration) {
                inline else => |function_declaration| function_declaration.identifier.?,
            };

            // iii. If functionNames does not contain fn, then
            const gop = try function_names.getOrPut(b.gpa, function_name);
            if (!gop.found_existing) {
                // 1. Insert fn as the first element of functionNames.
                // 2. NOTE: If there are multiple function declarations for the same name, the last
                //    declaration is used.
                // 3. Insert d as the first element of functionsToInitialize.
                // NOTE: AFAICT the order isn't observable, so we can append.
                try functions_to_initialize.append(b.gpa, hoistable_declaration);
            }
        }
    }

    // 15. Let argumentsObjectNeeded be true.
    // OPTIMIZATION: If nothing accesses the arguments object we don't need to create one.
    //               This is determined during parsing, with a deopt when using eval.
    var arguments_object_needed = formal_parameters.arguments_object_needed or function_body.arguments_object_needed;

    // 16. If func.[[ThisMode]] is lexical, then
    //     a. NOTE: Arrow functions never have an arguments object.
    //     b. Set argumentsObjectNeeded to false.
    // NOTE: This is done by setting arguments_object_needed to false during parsing.

    // 17. Else if parameterNames contains "arguments", then
    if (containsSlice(parameter_names.items, "arguments")) {
        // a. Set argumentsObjectNeeded to false.
        arguments_object_needed = false;
    }

    // 18. Else if hasParameterExpressions is false, then
    else if (!has_parameter_expressions) {
        // a. If functionNames contains "arguments" or lexicalNames contains "arguments", then
        if (function_names.contains("arguments") or containsSlice(lexical_names.items, "arguments")) {
            // i. Set argumentsObjectNeeded to false.
            arguments_object_needed = false;
        }
    }

    // 19. If strict is true or hasParameterExpressions is false, then
    if (strict or !has_parameter_expressions) {
        // a. NOTE: Only a single Environment Record is needed for the parameters, since calls
        //    to eval in strict mode code cannot create new bindings which are visible outside
        //    of the eval.

        // b. Let env be the LexicalEnvironment of calleeContext.
        // NOTE: The function environment created by `prepareForOrdinaryCall()` is used directly.
    } else {
        // 20. Else,

        // a. NOTE: A separate Environment Record is needed to ensure that bindings created by
        //    direct eval calls in the formal parameter list are outside the environment where
        //    parameters are declared.

        // b. Let calleeEnv be the LexicalEnvironment of calleeContext.
        // c. Let env be NewDeclarativeEnvironment(calleeEnv).
        _ = try b.addInst(.{
            .tag = .push_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;

        // d. Assert: The VariableEnvironment of calleeContext and calleeEnv are the same
        //    Environment Record.

        // e. Set the LexicalEnvironment of calleeContext to env.
        // NOTE: This is handled by `push_scope`.
    }

    var already_declared: std.StringArrayHashMapUnmanaged(void) = .empty;
    defer already_declared.deinit(b.gpa);

    // 21. For each String paramName of parameterNames, do
    for (parameter_names.items) |param_name| {
        // a. Let alreadyDeclared be ! env.HasBinding(paramName).
        const gop = try already_declared.getOrPut(b.gpa, param_name);

        // b. NOTE: Early errors ensure that duplicate parameter names can only occur in
        //    non-strict functions that do not have parameter default values or rest parameters.

        // c. If alreadyDeclared is false, then
        if (!gop.found_existing) {
            // i. Perform ! env.CreateMutableBinding(paramName, false).
            const string_index = try b.internString(param_name, .literal);
            _ = try b.addInst(.{
                .tag = .create_mutable_binding,
                .data = .{ .string = string_index },
            });

            // ii. If hasDuplicates is true, then
            if (has_duplicates) {
                // 1. Perform ! env.InitializeBinding(paramName, undefined).
                const undefined_ref = try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });
                _ = try b.addInst(.{
                    .tag = .initialize_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = undefined_ref,
                    } },
                });
            }
        }
    }

    // 22. If argumentsObjectNeeded is true, then
    const parameter_bindings_has_arguments = if (arguments_object_needed) blk: {
        // a. If strict is true or simpleParameterList is false, then
        const ao = if (strict or !simple_parameter_list) ao_blk: {
            // i. Let ao be CreateUnmappedArgumentsObject(argumentsList).
            break :ao_blk try b.addInst(.{
                .tag = .create_unmapped_arguments_object,
                .data = .{ .none = {} },
            });
        } else ao_blk: {
            // b. Else,
            // i. NOTE: A mapped argument object is only provided for non-strict functions that
            //    don't have a rest parameter, any parameter default value initializers, or any
            //    destructured parameters.

            // ii. Let ao be CreateMappedArgumentsObject(func, formals, argumentsList, env).
            break :ao_blk try b.addInst(.{
                .tag = .create_mapped_arguments_object,
                .data = .{ .none = {} },
            });
        };

        // c. If strict is true, then
        const arguments_string = try b.internString("arguments", .literal);
        if (strict) {
            // i. Perform ! env.CreateImmutableBinding("arguments", false).
            _ = try b.addInst(.{
                .tag = .create_immutable_binding,
                .data = .{ .string = arguments_string },
            });

            // ii. NOTE: In strict mode code early errors prevent attempting to assign to this
            //     binding, so its mutability is not observable.
        } else {
            // d. Else,
            // i. Perform ! env.CreateMutableBinding("arguments", false).
            _ = try b.addInst(.{
                .tag = .create_mutable_binding,
                .data = .{ .string = arguments_string },
            });
        }

        // e. Perform ! env.InitializeBinding("arguments", ao).
        _ = try b.addInst(.{
            .tag = .initialize_binding,
            .data = .{ .set_binding = .{
                .name = arguments_string,
                .value = ao,
            } },
        });

        // f. Let parameterBindings be the list-concatenation of parameterNames and « "arguments" ».
        break :blk true;
    } else blk: {
        // 23. Else,
        // a. Let parameterBindings be parameterNames.
        break :blk false;
    };

    // 24. Let iteratorRecord be CreateListIteratorRecord(argumentsList).
    // NOTE: This is done with a manual loop below.

    // 25. If hasDuplicates is true, then
    //     a. Let usedEnv be undefined.
    // 26. Else,
    //     a. Let usedEnv be env.
    // NOTE: The binding tag (`set_binding` vs `initialize_binding`) controls whether we use
    //       the environment or not.
    const binding_tag: Ir.Inst.Tag = if (has_duplicates) .set_binding else .initialize_binding;
    const binding_op: BindingOp = if (has_duplicates) .set else .initialize;

    // 27. NOTE: The following step cannot return a ReturnCompletion because the only way such a
    //     completion can arise in expression position is by use of YieldExpression, which is
    //     forbidden in parameter lists by Early Error rules in 15.5.1 and 15.6.1.

    // 28. Perform ? IteratorBindingInitialization of formals with arguments iteratorRecord and usedEnv.
    for (formal_parameters.items, 0..) |item, i| switch (item) {
        .formal_parameter => |param| switch (param.binding_element) {
            .single_name_binding => |binding| {
                const string_index = try b.internString(binding.binding_identifier, .literal);
                const arg_value = try b.addInst(.{
                    .tag = .get_argument,
                    .data = .{ .argument = @intCast(i) },
                });
                const default_expr, const anonymous_function_name = if (binding.initializer) |*expr|
                    .{ expr, if (expr.isAnonymousFunctionDefinition()) string_index else null }
                else
                    .{ null, null };
                const value = try b.lowerDefaultExpression(arg_value, default_expr, anonymous_function_name);
                _ = try b.addInst(.{
                    .tag = binding_tag,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = value,
                    } },
                });
            },
            .binding_pattern_and_expression => |bpe| {
                const arg_value = try b.addInst(.{
                    .tag = .get_argument,
                    .data = .{ .argument = @intCast(i) },
                });
                const value = try b.lowerDefaultExpression(arg_value, if (bpe.initializer) |*expr| expr else null, null);
                _ = try b.lowerDestructuringAssignment(&bpe.binding_pattern, value, binding_op);
            },
        },
        .function_rest_parameter => |rest_param| switch (rest_param.binding_rest_element) {
            .binding_identifier => |identifier| {
                const string_index = try b.internString(identifier, .literal);
                const rest_value = try b.addInst(.{
                    .tag = .get_rest_arguments,
                    .data = .{ .argument = @intCast(i) },
                });
                _ = try b.addInst(.{
                    .tag = binding_tag,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = rest_value,
                    } },
                });
            },
            .binding_pattern => |*pattern| {
                const rest_value = try b.addInst(.{
                    .tag = .get_rest_arguments,
                    .data = .{ .argument = @intCast(i) },
                });
                _ = try b.lowerDestructuringAssignment(pattern, rest_value, binding_op);
            },
        },
    };

    // 29. If hasParameterExpressions is false, then
    if (!has_parameter_expressions) {
        // a. NOTE: Only a single Environment Record is needed for the parameters and top-level vars.

        // b. Let instantiatedVarNames be a copy of the List parameterBindings.
        var instantiated_var_names: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer instantiated_var_names.deinit(b.gpa);
        for (parameter_names.items) |name| {
            try instantiated_var_names.put(b.gpa, name, {});
        }
        if (parameter_bindings_has_arguments) {
            try instantiated_var_names.put(b.gpa, "arguments", {});
        }

        // c. For each element n of varNames, do
        for (var_names.items) |var_name| {
            // i. If instantiatedVarNames does not contain n, then
            const gop = try instantiated_var_names.getOrPut(b.gpa, var_name);
            if (!gop.found_existing) {
                // 1. Append n to instantiatedVarNames.

                // 2. Perform ! env.CreateMutableBinding(n, false).
                const string_index = try b.internString(var_name, .literal);
                _ = try b.addInst(.{
                    .tag = .create_mutable_binding,
                    .data = .{ .string = string_index },
                });

                // 3. Perform ! env.InitializeBinding(n, undefined).
                const undefined_ref = try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });
                _ = try b.addInst(.{
                    .tag = .initialize_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = undefined_ref,
                    } },
                });
            }
        }

        // d. Let varEnv be env.
        // NOTE: No separate environment needed.
    } else {
        // 30. Else,
        // a. NOTE: A separate Environment Record is needed to ensure that closures created by
        //    expressions in the formal parameter list do not have visibility of declarations in
        //    the function body.

        // b. Let varEnv be NewDeclarativeEnvironment(env).
        // c. Set the VariableEnvironment of calleeContext to varEnv.
        _ = try b.addInst(.{
            .tag = .push_var_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;

        // d. Let instantiatedVarNames be a new empty List.
        var instantiated_var_names: std.StringArrayHashMapUnmanaged(void) = .empty;
        defer instantiated_var_names.deinit(b.gpa);

        // e. For each element n of varNames, do
        for (var_names.items) |var_name| {
            // i. If instantiatedVarNames does not contain n, then
            const gop = try instantiated_var_names.getOrPut(b.gpa, var_name);
            if (!gop.found_existing) {
                // 1. Append n to instantiatedVarNames.

                // 3. If parameterBindings does not contain n, or if functionNames contains n, then
                const is_in_parameter_bindings = containsSlice(parameter_names.items, var_name) or
                    (parameter_bindings_has_arguments and std.mem.eql(u8, var_name, "arguments"));
                if (!is_in_parameter_bindings or function_names.contains(var_name)) {
                    // a. Let initialValue be undefined.
                    // 2. Perform ! varEnv.CreateMutableBinding(n, false).
                    const string_index = try b.internString(var_name, .literal);
                    _ = try b.addInst(.{
                        .tag = .create_mutable_binding,
                        .data = .{ .string = string_index },
                    });

                    // 5. Perform ! varEnv.InitializeBinding(n, initialValue).
                    const undefined_ref = try b.addInst(.{
                        .tag = .undefined,
                        .data = .{ .none = {} },
                    });
                    _ = try b.addInst(.{
                        .tag = .initialize_binding,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = undefined_ref,
                        } },
                    });
                } else {
                    // 4. Else,
                    //     a. Let initialValue be ! env.GetBindingValue(n, false).
                    // NOTE: We must read the value *before* creating the binding in varEnv, because
                    //       get_binding walks the environment chain and would find the uninitialized
                    //       binding in varEnv instead of the initialized one in env. This reorder is
                    //       equivalent because env.GetBindingValue has no side effects on varEnv.
                    const string_index = try b.internString(var_name, .literal);
                    const initial_value = try b.addInst(.{
                        .tag = .get_binding,
                        .data = .{ .string = string_index },
                    });

                    // 2. Perform ! varEnv.CreateMutableBinding(n, false).
                    _ = try b.addInst(.{
                        .tag = .create_mutable_binding,
                        .data = .{ .string = string_index },
                    });

                    // 5. Perform ! varEnv.InitializeBinding(n, initialValue).
                    _ = try b.addInst(.{
                        .tag = .initialize_binding,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = initial_value,
                        } },
                    });
                }

                // 6. NOTE: A var with the same name as a formal parameter initially has the same
                //    value as the corresponding initialized parameter.
            }
        }
    }

    // 31. If strict is true, then
    if (strict) {
        // a. Let lexEnv be varEnv.
        // NOTE: No separate environment needed.
    } else {
        // 32. Else,
        // a. If the host is a web browser or otherwise supports Block-Level Function Declarations
        //    Web Legacy Compatibility Semantics, then
        //    [...]

        // b. Let lexEnv be NewDeclarativeEnvironment(varEnv).
        _ = try b.addInst(.{
            .tag = .push_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;

        // c. NOTE: Non-strict functions use a separate Environment Record for top-level lexical
        //    declarations so that a direct eval can determine whether any var scoped declarations
        //    introduced by the eval code conflict with pre-existing top-level lexically scoped
        //    declarations. This is not needed for strict functions because a strict direct eval
        //    always places all declarations into a new Environment Record.
    }

    // 33. Set the LexicalEnvironment of calleeContext to lexEnv.
    // NOTE: This is handled by `push_scope` above.

    // 34. Let lexDeclarations be the LexicallyScopedDeclarations of code.
    var lex_declarations: std.ArrayList(ast.LexicallyScopedDeclaration) = .empty;
    defer lex_declarations.deinit(b.gpa);
    try function_body.collectLexicallyScopedDeclarations(b.gpa, &lex_declarations);

    var bound_names: std.ArrayList(ast.Identifier) = .empty;
    defer bound_names.deinit(b.gpa);

    // 35. For each element d of lexDeclarations, do
    for (lex_declarations.items) |declaration| {
        // a. NOTE: A lexically declared name cannot be the same as a function/generator
        //    declaration, formal parameter, or a var name. Lexically declared names are only
        //    instantiated here but not initialized.

        // b. For each element dn of the BoundNames of d, do
        bound_names.clearRetainingCapacity();
        try declaration.collectBoundNames(b.gpa, &bound_names);

        for (bound_names.items) |name| {
            const string_index = try b.internString(name, .literal);

            // i. If IsConstantDeclaration of d is true, then
            if (declaration.isConstantDeclaration()) {
                // 1. Perform ! lexEnv.CreateImmutableBinding(dn, true).
                _ = try b.addInst(.{
                    .tag = .create_immutable_binding,
                    .data = .{ .string = string_index },
                });
            } else {
                // ii. Else,
                // 1. Perform ! lexEnv.CreateMutableBinding(dn, false).
                _ = try b.addInst(.{
                    .tag = .create_mutable_binding,
                    .data = .{ .string = string_index },
                });
            }
        }
    }

    // 36. Let privateEnv be the PrivateEnvironment of calleeContext.
    // NOTE: When `create_function` executes at runtime, the VM captures the current execution
    //       context's private environment in the new function object.

    // 37. For each Parse Node f of functionsToInitialize, do
    for (functions_to_initialize.items) |hoistable_declaration| {
        // a. Let fn be the sole element of the BoundNames of f.
        // b. Let fo be InstantiateFunctionObject of f with arguments lexEnv and privateEnv.
        const string_index, const func_ref = switch (hoistable_declaration) {
            inline else => |function_declaration, tag| blk: {
                const string_index = try b.internString(function_declaration.identifier.?, .literal);
                const function_index = try b.addFunction(.{
                    .source_range = function_declaration.source_range,
                    .name = .{ .identifier = string_index },
                    .parameters = function_declaration.formal_parameters,
                    .body = function_declaration.function_body,
                    .kind = switch (tag) {
                        .function_declaration => .normal,
                        .generator_declaration => .generator,
                        .async_function_declaration => .async,
                        .async_generator_declaration => .async_generator,
                    },
                });
                const func_ref = try b.addInst(.{
                    .tag = .create_function,
                    .data = .{ .create_function = function_index },
                });
                break :blk .{ string_index, func_ref };
            },
        };

        // c. Perform ! varEnv.SetMutableBinding(fn, fo, false).
        _ = try b.addInst(.{
            .tag = .set_binding,
            .data = .{ .set_binding = .{
                .name = string_index,
                .value = func_ref,
            } },
        });
    }

    // 38. Return unused.
}

/// 14.2.3 BlockDeclarationInstantiation ( code, env )
/// https://tc39.es/ecma262/#sec-blockdeclarationinstantiation
fn lowerBlockDeclarationInstantiation(
    b: *Builder,
    ast_node: union(enum) {
        block: *const ast.Block,
        case_block: *const ast.CaseBlock,
    },
) Error!void {
    // 1. Let declarations be the LexicallyScopedDeclarations of code.
    var declarations: std.ArrayList(ast.LexicallyScopedDeclaration) = .empty;
    defer declarations.deinit(b.gpa);
    switch (ast_node) {
        .block => |block| try block.statement_list.collectLexicallyScopedDeclarations(b.gpa, &declarations),
        .case_block => |case_block| try case_block.collectLexicallyScopedDeclarations(b.gpa, &declarations),
    }

    // 2. Let privateEnv be the running execution context's PrivateEnvironment.

    var bound_names: std.ArrayList(ast.Identifier) = .empty;
    defer bound_names.deinit(b.gpa);

    // 3. For each element d of declarations, do
    for (declarations.items) |declaration| {
        // a. For each element dn of the BoundNames of d, do
        bound_names.clearRetainingCapacity();
        try declaration.collectBoundNames(b.gpa, &bound_names);
        for (bound_names.items) |name| {
            // i. If IsConstantDeclaration of d is true, then
            //     1. Perform ! env.CreateImmutableBinding(dn, true).
            // ii. Else,
            //     1. If the host is a web browser or otherwise supports Block-Level Function
            //        Declarations Web Legacy Compatibility Semantics, then
            //        [...]
            //     2. Else,
            //         a. Perform ! env.CreateMutableBinding(dn, false).
            const tag: Ir.Inst.Tag = if (declaration.isConstantDeclaration())
                .create_immutable_binding
            else
                .create_mutable_binding;
            const string_index = try b.internString(name, .literal);
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .string = string_index },
            });
        }

        // b. If d is either a FunctionDeclaration, a GeneratorDeclaration, an
        //    AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration, then
        if (declaration == .hoistable_declaration) {
            const hoistable_declaration = declaration.hoistable_declaration;

            // i. Let fn be the sole element of the BoundNames of d.
            // ii. Let fo be InstantiateFunctionObject of d with arguments env and privateEnv.
            // iii. If the host is a web browser or otherwise supports Block-Level Function
            //      Declarations Web Legacy Compatibility Semantics, then
            //      [...]
            // iv. Else,
            //     1. Perform ! env.InitializeBinding(fn, fo).
            const string_index, const function_ref = switch (hoistable_declaration) {
                inline else => |function_declaration, tag| blk: {
                    const string_index = try b.internString(function_declaration.identifier.?, .literal);
                    const function_index = try b.addFunction(.{
                        .source_range = function_declaration.source_range,
                        .name = .{ .identifier = string_index },
                        .parameters = function_declaration.formal_parameters,
                        .body = function_declaration.function_body,
                        .kind = switch (tag) {
                            .function_declaration => .normal,
                            .generator_declaration => .generator,
                            .async_function_declaration => .async,
                            .async_generator_declaration => .async_generator,
                        },
                    });
                    const function_ref = try b.addInst(.{
                        .tag = .create_function,
                        .data = .{ .create_function = function_index },
                    });
                    break :blk .{ string_index, function_ref };
                },
            };
            _ = try b.addInst(.{
                .tag = .initialize_binding,
                .data = .{ .set_binding = .{
                    .name = string_index,
                    .value = function_ref,
                } },
            });
        }
    }

    // 4. Return unused.
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
        .return_statement => |*ret_stmt| try b.lowerReturnStatement(ret_stmt),
        .with_statement => |*with_stmt| try b.lowerWithStatement(with_stmt),
        .labelled_statement => |*lbl_stmt| try b.lowerLabelledStatement(lbl_stmt),
        .throw_statement => |*throw_stmt| try b.lowerThrowStatement(throw_stmt),
        .try_statement => |*try_stmt| try b.lowerTryStatement(try_stmt),
        .debugger_statement => .none,
    };
}

fn lowerDeclaration(b: *Builder, decl: *const ast.Declaration) Error!Ir.Inst.Ref {
    switch (decl.*) {
        .hoistable_declaration => {}, // Handled by GDI/FDI before execution
        .class_declaration => |*class_decl| _ = try b.lowerClassDeclaration(class_decl),
        .lexical_declaration => |*lex_decl| _ = try b.lowerLexicalDeclaration(lex_decl),
    }
    return .none;
}

fn lowerBlockStatement(b: *Builder, block_stmt: *const ast.BlockStatement, breakable_ctx: ?*BreakableContext) Error!Ir.Inst.Ref {
    return b.lowerBlock(&block_stmt.block, breakable_ctx);
}

fn lowerBlock(b: *Builder, block: *const ast.Block, breakable_ctx: ?*BreakableContext) Error!Ir.Inst.Ref {
    const stmt_list = &block.statement_list;
    const has_scope = stmt_list.hasLexicallyScopedDeclarations();

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .push_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;
        try b.lowerBlockDeclarationInstantiation(.{ .block = block });
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
    for (var_stmt.variable_declaration_list.items) |*var_decl| {
        _ = try b.lowerVariableDeclaration(var_decl);
    }
    return .none;
}

fn lowerVariableDeclaration(b: *Builder, var_decl: *const ast.VariableDeclaration) Error!Ir.Inst.Ref {
    // GlobalDeclarationInstantiation is responsible for creating the bindings and initializing them to undefined.
    switch (var_decl.*) {
        .binding_identifier => |binding| {
            if (binding.initializer) |*init_expr| {
                const value = try b.lowerExpression(init_expr);
                const string_index = try b.internString(binding.binding_identifier, .literal);
                if (init_expr.isAnonymousFunctionDefinition()) {
                    b.setAnonymousFunctionName(value, string_index);
                }
                _ = try b.addInst(.{
                    .tag = .set_binding,
                    .data = .{ .set_binding = .{
                        .name = string_index,
                        .value = value,
                    } },
                });
            }
        },
        .binding_pattern => |pattern| {
            const value = try b.lowerExpression(&pattern.initializer);
            _ = try b.lowerDestructuringAssignment(&pattern.binding_pattern, value, .set);
        },
    }
    return .none;
}

fn lowerIfStatement(b: *Builder, if_stmt: *const ast.IfStatement) Error!Ir.Inst.Ref {
    if (try constantFold(b.gpa, &if_stmt.test_expression)) |constant| {
        defer constant.deinit(b.gpa);
        const result = if (constant.isTruthy())
            try b.lowerStatement(if_stmt.consequent_statement)
        else if (if_stmt.alternate_statement) |stmt|
            try b.lowerStatement(stmt)
        else
            Ir.Inst.Ref.none;
        return if (result != .none) result else try b.addInst(.{
            .tag = .undefined,
            .data = .{ .none = {} },
        });
    }

    const test_result = try b.lowerExpression(&if_stmt.test_expression);
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const then_label = try b.addLabel();
    const then_value = try b.lowerStatement(if_stmt.consequent_statement);
    const then_result = if (then_value != .none) then_value else try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });
    const then_br = try b.addInstDeferred(.br);

    const else_label = try b.addLabel();
    const else_value = if (if_stmt.alternate_statement) |stmt|
        try b.lowerStatement(stmt)
    else
        Ir.Inst.Ref.none;
    const else_result = if (else_value != .none) else_value else try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });
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
                    const string_index = try b.internString(binding.binding_identifier, .literal);
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
                        const string_index = try b.internString(name, .literal);
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
    const exit_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth -= 1;
    }

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
    var skip_label: Ir.Inst.Ref = .none;
    var skip_br: Deferred = undefined;

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

    const iterator = try b.addInst(.{
        .tag = switch (for_in_of_stmt.type) {
            .in => .get_for_in_iterator,
            .of => .get_iterator,
            .async_of => .get_async_iterator,
        },
        .data = .{ .ref = expr_value },
    });

    const entry_br = try b.addInstDeferred(.br);

    const test_label = try b.addLabel();

    const next_value = try b.addInst(.{
        .tag = if (for_in_of_stmt.type == .async_of) .iterator_step_value_async else .iterator_step_value,
        .data = .{ .ref = iterator },
    });
    const is_done = try b.addInst(.{
        .tag = .iterator_is_done,
        .data = .{ .ref = iterator },
    });
    // NOTE: We invert the condition to ensure the body block (then target, else target by default)
    //       is placed within the surrounding try/catch handler ranges during block linearization.
    //       (Yes, this is a hack.)
    const should_continue = try b.addInst(.{
        .tag = .logical_not,
        .data = .{ .ref = is_done },
    });
    const test_br_cond = try b.addInstDeferred(.br_cond);

    const body_label = try b.addLabel();

    const breakable_ctx = try b.pushBreakableContext(.{
        .label = label,
        .continue_target = .{ .deferred = .empty },
        .break_target = .{ .deferred = .empty },
        .result_ref = undefined_ref,
        .iterator_ref = if (for_in_of_stmt.type != .in) iterator else .none,
    });
    defer b.popBreakableContext();

    switch (for_in_of_stmt.initializer) {
        .expression => |*expr| switch (expr.*) {
            .primary_expression => |prim_expr| switch (prim_expr) {
                .identifier_reference => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
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
                        const string_index = try b.internString(identifier, .literal);
                        const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                            .set_property_strict
                        else
                            .set_property;
                        const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                            .base = base,
                            .name = string_index,
                            .value = next_value,
                        });
                        _ = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .set_property = extra_index },
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
                                const extra_index = try b.addExtra(Ir.Inst.SetPropertyIndexed, .{
                                    .base = base,
                                    .index = index,
                                    .value = next_value,
                                });
                                _ = try b.addInst(.{
                                    .tag = tag,
                                    .data = .{ .set_property_indexed = extra_index },
                                });
                                break :blk;
                            }
                        }
                        const property = try b.lowerExpression(prop_expr);
                        const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                            .set_property_computed_strict
                        else
                            .set_property_computed;
                        const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                            .base = base,
                            .property = property,
                            .value = next_value,
                        });
                        _ = try b.addInst(.{
                            .tag = tag,
                            .data = .{ .set_property_computed = extra_index },
                        });
                    },
                    .private_identifier => |private_identifier| {
                        const string_index = try b.internString(private_identifier, .literal);
                        const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                            .base = base,
                            .name = string_index,
                            .value = next_value,
                        });
                        _ = try b.addInst(.{
                            .tag = .set_private_element,
                            .data = .{ .set_property = extra_index },
                        });
                    },
                }
            },
            .super_property => |super_prop| switch (super_prop) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_super_property_strict
                    else
                        .set_super_property;
                    const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                        .base = .none,
                        .name = string_index,
                        .value = next_value,
                    });
                    _ = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_property = extra_index },
                    });
                },
                .expression => |prop_expr| {
                    const property = try b.lowerExpression(prop_expr);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_super_property_computed_strict
                    else
                        .set_super_property_computed;
                    const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                        .base = .none,
                        .property = property,
                        .value = next_value,
                    });
                    _ = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_property_computed = extra_index },
                    });
                },
            },
            .binding_pattern_for_assignment_expression => |*pattern| {
                _ = try b.lowerDestructuringAssignment(pattern, next_value, .set);
            },
            .call_expression => |*call_expr| {
                _ = try b.lowerCallExpression(call_expr);
                _ = try b.addInst(.{
                    .tag = .throw_reference_error,
                    .data = .{ .none = {} },
                });
            },
            else => unreachable,
        },
        .for_binding => |for_binding| switch (for_binding) {
            .binding_identifier => |identifier| {
                const string_index = try b.internString(identifier, .literal);
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
            .binding_pattern => |*pattern| {
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
                    const string_index = try b.internString(identifier, .literal);
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
                .binding_pattern => |*pattern| {
                    var bound_names: std.ArrayList(ast.Identifier) = .empty;
                    defer bound_names.deinit(b.gpa);
                    try pattern.collectBoundNames(b.gpa, &bound_names);
                    for (bound_names.items) |name| {
                        const string_index = try b.internString(name, .literal);
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

    if (for_in_of_stmt.type == .in) {
        skip_label = try b.addLabel();
        skip_br = try b.addInstDeferred(.br);
    }

    const end_label = try b.addLabel();

    breakable_ctx.setDeferredContinues(test_label);
    breakable_ctx.setDeferredBreaks(end_label);

    if (for_in_of_stmt.type == .in) {
        skip_br_cond.set(.{ .br_cond = .{
            .condition = skip_condition,
            .then_target = skip_label,
            .else_target = setup_label,
        } });
        skip_br.set(.{ .br = .{
            .target = end_label,
            .value = undefined_ref,
        } });
    }
    entry_br.set(.{ .br = .{
        .target = test_label,
        .value = undefined_ref,
    } });
    test_br_cond.set(.{ .br_cond = .{
        .condition = should_continue,
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

fn lowerSwitchStatement(b: *Builder, switch_stmt: *const ast.SwitchStatement, label: ?[]const u8) Error!Ir.Inst.Ref {
    const discriminant = try b.lowerExpression(&switch_stmt.expression);

    var lex_declarations: std.ArrayList(ast.LexicallyScopedDeclaration) = .empty;
    defer lex_declarations.deinit(b.gpa);
    try switch_stmt.case_block.collectLexicallyScopedDeclarations(b.gpa, &lex_declarations);

    const has_scope = lex_declarations.items.len > 0;

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .push_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth += 1;
        try b.lowerBlockDeclarationInstantiation(.{ .case_block = &switch_stmt.case_block });
    }

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

    var case_branches: std.ArrayList(struct { br_cond: Deferred, index: u32 }) = .empty;
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

    if (has_scope) {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
        b.scope_depth -= 1;
    }

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
        const inst = b.instructions.get(@intFromEnum(item.br_cond.index));
        const extra_index = @intFromEnum(inst.data.br_cond);
        // Set then_target which was initialized as .none
        b.extra.items[extra_index + 1] = @intFromEnum(body_labels[item.index]);
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
    const current_ctx = b.breakable_stack.items[b.breakable_stack.items.len - 1];
    const target_ctx = b.findBreakableContext(cont_stmt.label);
    const value = current_ctx.result_ref;

    var it = std.mem.reverseIterator(b.breakable_stack.items);
    while (it.next()) |ctx| {
        if (ctx == target_ctx) break;
        if (ctx.iterator_ref != .none) {
            _ = try b.addInst(.{
                .tag = .iterator_close,
                .data = .{ .ref = ctx.iterator_ref },
            });
        }
    }

    const scope_pops = b.scope_depth - target_ctx.scope_depth;
    for (0..scope_pops) |_| {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
    }

    switch (target_ctx.continue_target) {
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
    const current_ctx = b.breakable_stack.items[b.breakable_stack.items.len - 1];
    const target_ctx = b.findBreakableContext(brk_stmt.label);
    const value = current_ctx.result_ref;

    var it = std.mem.reverseIterator(b.breakable_stack.items);
    while (it.next()) |ctx| {
        if (ctx.iterator_ref != .none) {
            _ = try b.addInst(.{
                .tag = .iterator_close,
                .data = .{ .ref = ctx.iterator_ref },
            });
        }
        if (ctx == target_ctx) break;
    }

    const scope_pops = b.scope_depth - target_ctx.scope_depth;
    for (0..scope_pops) |_| {
        _ = try b.addInst(.{
            .tag = .pop_scope,
            .data = .{ .none = {} },
        });
    }

    switch (target_ctx.break_target) {
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

fn lowerReturnStatement(b: *Builder, ret_stmt: *const ast.ReturnStatement) Error!Ir.Inst.Ref {
    const value = if (ret_stmt.expression) |*expr|
        try b.lowerExpression(expr)
    else
        Ir.Inst.Ref.none;

    var it = std.mem.reverseIterator(b.breakable_stack.items);
    while (it.next()) |ctx| {
        if (ctx.iterator_ref != .none) {
            _ = try b.addInst(.{
                .tag = .iterator_close,
                .data = .{ .ref = ctx.iterator_ref },
            });
        }
    }

    _ = try b.addInst(.{
        .tag = .@"return",
        .data = .{ .ref = value },
    });
    return .none;
}

fn lowerWithStatement(b: *Builder, with_stmt: *const ast.WithStatement) Error!Ir.Inst.Ref {
    const expr_value = try b.lowerExpression(&with_stmt.expression);
    const object = try b.addInst(.{
        .tag = .to_object,
        .data = .{ .ref = expr_value },
    });

    _ = try b.addInst(.{
        .tag = .push_with_scope,
        .data = .{ .ref = object },
    });
    b.scope_depth += 1;

    const value = try b.lowerStatement(with_stmt.statement);
    const result = if (value != .none) value else try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });

    _ = try b.addInst(.{
        .tag = .pop_scope,
        .data = .{ .none = {} },
    });
    b.scope_depth -= 1;

    return result;
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

                const result = switch (stmt.*) {
                    .block_statement => |*block_stmt| try b.lowerBlockStatement(block_stmt, breakable_ctx),
                    else => try b.lowerStatement(stmt),
                };
                if (result != .none) breakable_ctx.result_ref = result;

                const result_br = try b.addInstDeferred(.br);

                const end_label = try b.addLabel();
                breakable_ctx.setDeferredBreaks(end_label);

                result_br.set(.{ .br = .{
                    .target = end_label,
                    .value = breakable_ctx.result_ref,
                } });

                return end_label;
            },
        },
        .function_declaration => .none, // Handled by GDI/FDI before execution
    };
}

fn lowerThrowStatement(b: *Builder, throw_stmt: *const ast.ThrowStatement) Error!Ir.Inst.Ref {
    const value = try b.lowerExpression(&throw_stmt.expression);
    _ = try b.addInst(.{
        .tag = .throw,
        .data = .{ .ref = value },
    });
    return .none;
}

fn lowerTryStatement(b: *Builder, try_stmt: *const ast.TryStatement) Error!Ir.Inst.Ref {
    const scope_depth = b.scope_depth;

    const try_label = try b.addLabel();
    const try_value = try b.lowerBlock(&try_stmt.try_block, null);
    const try_result = if (try_value != .none) try_value else try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });

    const try_end_label = try b.addLabel();
    const try_end_br = try b.addInstDeferred(.br);

    var catch_label: Ir.Inst.Ref = undefined;
    var catch_handler: Deferred = undefined;
    var catch_result: Ir.Inst.Ref = undefined;
    var catch_br: Deferred = undefined;

    if (try_stmt.catch_block) |*catch_block| {
        catch_label = try b.addLabel();
        catch_handler = try b.addInstDeferred(.exception_handler);
        const exception_value = catch_handler.index.toRef();

        if (try_stmt.catch_parameter) |catch_parameter| {
            _ = try b.addInst(.{
                .tag = .push_scope,
                .data = .{ .none = {} },
            });
            b.scope_depth += 1;

            var bound_names: std.ArrayList(ast.Identifier) = .empty;
            defer bound_names.deinit(b.gpa);
            try catch_parameter.collectBoundNames(b.gpa, &bound_names);
            for (bound_names.items) |name| {
                const string_index = try b.internString(name, .literal);
                _ = try b.addInst(.{
                    .tag = .create_mutable_binding,
                    .data = .{ .string = string_index },
                });
            }

            switch (catch_parameter) {
                .binding_identifier => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
                    _ = try b.addInst(.{
                        .tag = .initialize_binding,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = exception_value,
                        } },
                    });
                },
                .binding_pattern => |*binding_pattern| {
                    _ = try b.lowerDestructuringAssignment(binding_pattern, exception_value, .initialize);
                },
            }
        }

        const catch_value = try b.lowerBlock(catch_block, null);
        catch_result = if (catch_value != .none) catch_value else try b.addInst(.{
            .tag = .undefined,
            .data = .{ .none = {} },
        });

        if (try_stmt.catch_parameter != null) {
            _ = try b.addInst(.{
                .tag = .pop_scope,
                .data = .{ .none = {} },
            });
            b.scope_depth -= 1;
        }

        catch_br = try b.addInstDeferred(.br);
    }

    var finally_throw_label: Ir.Inst.Ref = undefined;
    var finally_handler: Deferred = undefined;
    var finally_label: Ir.Inst.Ref = undefined;
    var finally_br: Deferred = undefined;

    if (try_stmt.finally_block) |*finally_block| {
        finally_throw_label = try b.addLabel();
        finally_handler = try b.addInstDeferred(.exception_handler);
        const exception_value = finally_handler.index.toRef();
        _ = try b.lowerBlock(finally_block, null);
        _ = try b.addInst(.{
            .tag = .throw,
            .data = .{ .ref = exception_value },
        });

        finally_label = try b.addLabel();
        _ = try b.lowerBlock(finally_block, null);
        finally_br = try b.addInstDeferred(.br);
    }

    const end_label = try b.addLabel();

    const normal_target = if (try_stmt.finally_block != null) finally_label else end_label;

    try_end_br.set(.{ .br = .{
        .target = normal_target,
        .value = try_result,
    } });

    if (try_stmt.catch_block != null) {
        catch_handler.set(.{ .exception_handler = .{
            .start = try_label,
            .end = try_end_label,
            .target = catch_label,
            .scope_depth = scope_depth,
        } });
        catch_br.set(.{ .br = .{
            .target = normal_target,
            .value = catch_result,
        } });
    }

    if (try_stmt.finally_block != null) {
        finally_handler.set(.{ .exception_handler = .{
            .start = if (try_stmt.catch_block != null) catch_label else try_label,
            .end = finally_throw_label,
            .target = finally_throw_label,
            .scope_depth = scope_depth,
        } });
        finally_br.set(.{ .br = .{
            .target = end_label,
            .value = finally_label,
        } });
    }

    return end_label;
}

fn lowerClassDeclaration(b: *Builder, class_decl: *const ast.ClassDeclaration) Error!Ir.Inst.Ref {
    const name: Ir.Class.Name = if (class_decl.identifier) |identifier|
        .{ .identifier = try b.internString(identifier, .literal) }
    else
        .{ .default = try b.internString("default", .literal) };

    const heritage = try b.lowerClassHeritage(class_decl.class_tail.class_heritage);

    _ = try b.addInst(.{
        .tag = .push_private_scope,
        .data = .{ .none = {} },
    });
    const element_names = try b.lowerClassElementNames(&class_decl.class_tail.class_body);

    const class_index = try b.addClass(.{
        .source_range = class_decl.source_range,
        .name = name,
        .class_tail = class_decl.class_tail,
        .heritage = heritage,
        .element_names = element_names,
    });
    const value = try b.addInst(.{
        .tag = .create_class,
        .data = .{ .create_class = class_index },
    });
    _ = try b.addInst(.{
        .tag = .pop_private_scope,
        .data = .{ .none = {} },
    });
    if (class_decl.identifier) |identifier| {
        const string_index = try b.internString(identifier, .literal);
        _ = try b.addInst(.{
            .tag = .initialize_binding,
            .data = .{ .set_binding = .{
                .name = string_index,
                .value = value,
            } },
        });
    }
    return value;
}

fn lowerLexicalDeclaration(b: *Builder, lex_decl: *const ast.LexicalDeclaration) Error!Ir.Inst.Ref {
    for (lex_decl.binding_list.items) |*lex_binding| {
        _ = try b.lowerLexicalBinding(lex_binding);
    }
    return .none;
}

fn lowerLexicalBinding(b: *Builder, lex_binding: *const ast.LexicalBinding) Error!Ir.Inst.Ref {
    // GlobalDeclarationInstantiation is responsible for creating the bindings.
    return switch (lex_binding.*) {
        .binding_identifier => |binding| {
            const value = if (binding.initializer) |*init_expr|
                try b.lowerExpression(init_expr)
            else
                try b.addInst(.{
                    .tag = .undefined,
                    .data = .{ .none = {} },
                });
            const string_index = try b.internString(binding.binding_identifier, .literal);
            if (binding.initializer != null and binding.initializer.?.isAnonymousFunctionDefinition()) {
                b.setAnonymousFunctionName(value, string_index);
            }
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
            return try b.lowerDestructuringAssignment(&pattern.binding_pattern, value, .initialize);
        },
    };
}

fn lowerPropertyName(b: *Builder, property_name: *const ast.PropertyName) Error!Ir.Inst.Ref {
    return switch (property_name.*) {
        .literal_property_name => |literal| switch (literal) {
            .identifier => |identifier| blk: {
                const string_index = try b.internString(identifier, .literal);
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

fn lowerDefaultExpression(b: *Builder, value: Ir.Inst.Ref, default_expr: ?*const ast.Expression, anonymous_function_name: ?Ir.Inst.StringIndex) Error!Ir.Inst.Ref {
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
        if (anonymous_function_name) |string_index| {
            b.setAnonymousFunctionName(default_value, string_index);
        }
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

fn lowerDestructuringAssignment(b: *Builder, pattern: *const ast.BindingPattern, value: Ir.Inst.Ref, binding_op: BindingOp) Error!Ir.Inst.Ref {
    return switch (pattern.*) {
        .array_binding_pattern => |*array_pattern| try b.lowerArrayDestructuring(array_pattern, value, binding_op),
        .object_binding_pattern => |*object_pattern| try b.lowerObjectDestructuring(object_pattern, value, binding_op),
    };
}

fn lowerArrayDestructuring(b: *Builder, pattern: *const ast.ArrayBindingPattern, array_value: Ir.Inst.Ref, binding_op: BindingOp) Error!Ir.Inst.Ref {
    var last_ref: Ir.Inst.Ref = .none;

    const iterator_ref = try b.addInst(.{
        .tag = .get_iterator,
        .data = .{ .ref = array_value },
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
                    const string_index = try b.internString(binding.binding_identifier, .literal);
                    const default_expr, const anonymous_function_name = if (binding.initializer) |*expr|
                        .{ expr, if (expr.isAnonymousFunctionDefinition()) string_index else null }
                    else
                        .{ null, null };
                    const value = try b.lowerDefaultExpression(next_value, default_expr, anonymous_function_name);
                    const tag: Ir.Inst.Tag = switch (binding_op) {
                        .initialize => .initialize_binding,
                        .set => if (b.in_strict_mode) .set_binding_strict else .set_binding,
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
                    const value = try b.lowerDefaultExpression(next_value, default_expr, null);
                    last_ref = try b.lowerDestructuringAssignment(&bpe.binding_pattern, value, binding_op);
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
                    const string_index = try b.internString(identifier, .literal);
                    const tag: Ir.Inst.Tag = switch (binding_op) {
                        .initialize => .initialize_binding,
                        .set => if (b.in_strict_mode) .set_binding_strict else .set_binding,
                    };
                    last_ref = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_binding = .{
                            .name = string_index,
                            .value = rest_array,
                        } },
                    });
                },
                .binding_pattern => |*binding_pattern| {
                    last_ref = try b.lowerDestructuringAssignment(binding_pattern, rest_array, binding_op);
                },
            }
        },
    };

    const iterator_done = try b.addInst(.{
        .tag = .iterator_is_done,
        .data = .{ .ref = iterator_ref },
    });
    const close_cond = try b.addInstDeferred(.br_cond);

    const close_label = try b.addLabel();
    _ = try b.addInst(.{
        .tag = .iterator_close,
        .data = .{ .ref = iterator_ref },
    });

    const done_label = try b.addLabel();

    close_cond.set(.{ .br_cond = .{
        .condition = iterator_done,
        .then_target = done_label,
        .else_target = close_label,
    } });

    return last_ref;
}

fn lowerObjectDestructuring(b: *Builder, pattern: *const ast.ObjectBindingPattern, object_value: Ir.Inst.Ref, binding_op: BindingOp) Error!Ir.Inst.Ref {
    const object = try b.addInst(.{
        .tag = .to_object,
        .data = .{ .ref = object_value },
    });

    var last_ref = object;

    for (pattern.properties) |property| switch (property) {
        .binding_property => |binding_property| switch (binding_property) {
            .single_name_binding => |binding| {
                const string_index = try b.internString(binding.binding_identifier, .literal);
                const prop_value = try b.addInst(.{
                    .tag = .get_property,
                    .data = .{ .get_property = .{
                        .base = object,
                        .name = string_index,
                    } },
                });
                const default_expr, const anonymous_function_name = if (binding.initializer) |*expr|
                    .{ expr, if (expr.isAnonymousFunctionDefinition()) string_index else null }
                else
                    .{ null, null };
                const value = try b.lowerDefaultExpression(prop_value, default_expr, anonymous_function_name);
                const tag: Ir.Inst.Tag = switch (binding_op) {
                    .initialize => .initialize_binding,
                    .set => if (b.in_strict_mode) .set_binding_strict else .set_binding,
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
                const key_ref = try b.lowerPropertyName(&pnbe.property_name);
                const prop_value = try b.addInst(.{
                    .tag = .get_property_computed,
                    .data = .{ .get_property_computed = .{
                        .base = object,
                        .property = key_ref,
                    } },
                });
                switch (pnbe.binding_element) {
                    .single_name_binding => |binding| {
                        const string_index = try b.internString(binding.binding_identifier, .literal);
                        const default_expr, const anonymous_function_name = if (binding.initializer) |*expr|
                            .{ expr, if (expr.isAnonymousFunctionDefinition()) string_index else null }
                        else
                            .{ null, null };
                        const value = try b.lowerDefaultExpression(prop_value, default_expr, anonymous_function_name);
                        const tag: Ir.Inst.Tag = switch (binding_op) {
                            .initialize => .initialize_binding,
                            .set => if (b.in_strict_mode) .set_binding_strict else .set_binding,
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
                        const value = try b.lowerDefaultExpression(prop_value, default_expr, null);
                        last_ref = try b.lowerDestructuringAssignment(&bpe.binding_pattern, value, binding_op);
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
                        const string_index = try b.internString(binding.binding_identifier, .literal);
                        const name_ref = try b.addInst(.{
                            .tag = .string,
                            .data = .{ .string = string_index },
                        });
                        try excluded_names.append(b.gpa, name_ref);
                    },
                    .property_name_and_binding_element => |pnbe| {
                        const key_ref = try b.lowerPropertyName(&pnbe.property_name);
                        try excluded_names.append(b.gpa, key_ref);
                    },
                },
                .binding_rest_property => {},
            };

            const extra_index: Ir.Inst.ExtraIndex = @enumFromInt(b.extra.items.len);
            const excluded_len: u32 = @intCast(excluded_names.items.len);
            try b.extra.append(b.gpa, @intFromEnum(object));
            try b.extra.append(b.gpa, excluded_len);
            try b.extra.appendSlice(b.gpa, @ptrCast(excluded_names.items));

            const rest_obj = try b.addInst(.{
                .tag = .copy_data_properties,
                .data = .{ .copy_data_properties = extra_index },
            });

            const string_index = try b.internString(rest_property.binding_identifier, .literal);
            const tag: Ir.Inst.Tag = switch (binding_op) {
                .initialize => .initialize_binding,
                .set => if (b.in_strict_mode) .set_binding_strict else .set_binding,
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
            .function_expression => |*func_expr| try b.lowerFunctionExpression(func_expr),
            .class_expression => |*class_expr| try b.lowerClassExpression(class_expr),
            .generator_expression => |*gen_expr| try b.lowerFunctionExpression(gen_expr),
            .async_function_expression => |*async_expr| try b.lowerFunctionExpression(async_expr),
            .async_generator_expression => |*async_gen_expr| try b.lowerFunctionExpression(async_gen_expr),
            .regular_expression_literal => |*regexp_lit| try b.lowerRegularExpressionLiteral(regexp_lit),
            .template_literal => |*template_lit| try b.lowerTemplateLiteral(template_lit),
            .arrow_function => |*arrow_func| try b.lowerArrowFunction(arrow_func),
            .async_arrow_function => |*async_arrow_func| try b.lowerArrowFunction(async_arrow_func),
        },
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, null),
        .super_property => |*super_prop| try b.lowerSuperProperty(super_prop, null),
        .meta_property => |meta_prop| switch (meta_prop) {
            .new_target => try b.lowerNewTarget(),
            .import_meta => try b.lowerImportMeta(),
        },
        .new_expression => |*new_expr| try b.lowerNewExpression(new_expr),
        .call_expression => |*call_expr| try b.lowerCallExpression(call_expr),
        .super_call => |*super_call| try b.lowerSuperCall(super_call),
        .import_call => |*import_call| try b.lowerImportCall(import_call),
        .optional_expression => |*opt_expr| try b.lowerOptionalExpression(opt_expr),
        .update_expression => |*update_expr| try b.lowerUpdateExpression(update_expr),
        .unary_expression => |*unary_expr| try b.lowerUnaryExpression(unary_expr),
        .binary_expression => |*bin_expr| try b.lowerBinaryExpression(bin_expr),
        .relational_expression => |*rel_expr| try b.lowerRelationalExpression(rel_expr),
        .equality_expression => |*eq_expr| try b.lowerEqualityExpression(eq_expr),
        .logical_expression => |*log_expr| try b.lowerLogicalExpression(log_expr),
        .conditional_expression => |*cond_expr| try b.lowerConditionalExpression(cond_expr),
        .assignment_expression => |*assign_expr| try b.lowerAssignmentExpression(assign_expr),
        .sequence_expression => |*seq_expr| try b.lowerSequenceExpression(seq_expr),
        .await_expression => |*await_expr| try b.lowerAwaitExpression(await_expr),
        .yield_expression => |*yield_expr| try b.lowerYieldExpression(yield_expr),
        .tagged_template => |*tagged_template| try b.lowerTaggedTemplate(tagged_template),
        .binding_pattern_for_assignment_expression => unreachable, // Only valid as assignment LHS
    };
}

fn lowerThis(b: *Builder) Error!Ir.Inst.Ref {
    return b.addInst(.{
        .tag = .this,
        .data = .{ .none = {} },
    });
}

fn lowerIdentifierReference(b: *Builder, identifier: []const u8) Error!Ir.Inst.Ref {
    const string_index = try b.internString(identifier, .literal);
    return b.addInst(.{
        .tag = .get_binding,
        .data = .{ .string = string_index },
    });
}

fn lowerArrayLiteral(b: *Builder, array_lit: *const ast.ArrayLiteral) Error!Ir.Inst.Ref {
    const has_spread = for (array_lit.element_list) |elem| {
        if (elem == .spread) break true;
    } else false;

    const array_ref = try b.addInst(.{
        .tag = .array_create,
        .data = .{ .array = .{
            .len = @intCast(array_lit.element_list.len),
            .has_spread = has_spread,
        } },
    });

    for (array_lit.element_list) |elem| {
        switch (elem) {
            .elision => {
                _ = try b.addInst(.{
                    .tag = .array_push,
                    .data = .{ .binary = .{
                        .lhs = array_ref,
                        .rhs = .none,
                    } },
                });
            },
            .expression => |*expr| {
                const value = try b.lowerExpression(expr);
                _ = try b.addInst(.{
                    .tag = .array_push,
                    .data = .{ .binary = .{
                        .lhs = array_ref,
                        .rhs = value,
                    } },
                });
            },
            .spread => |*expr| {
                const value = try b.lowerExpression(expr);
                _ = try b.addInst(.{
                    .tag = .array_spread,
                    .data = .{ .binary = .{
                        .lhs = array_ref,
                        .rhs = value,
                    } },
                });
            },
        }
    }

    return array_ref;
}

fn lowerObjectLiteral(b: *Builder, object_lit: *const ast.ObjectLiteral) Error!Ir.Inst.Ref {
    const Key = union(enum) {
        string: Ir.Inst.StringIndex,
        computed: Ir.Inst.Ref,
    };

    const object_ref = try b.addInst(.{
        .tag = .object_create,
        .data = .{ .none = {} },
    });

    for (object_lit.property_definition_list.items) |prop_def| {
        switch (prop_def) {
            .identifier_reference => |identifier| {
                const string_index = try b.internString(identifier, .literal);
                const value_ref = try b.addInst(.{
                    .tag = .get_binding,
                    .data = .{ .string = string_index },
                });
                const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                    .base = object_ref,
                    .name = string_index,
                    .value = value_ref,
                });
                _ = try b.addInst(.{
                    .tag = .object_set,
                    .data = .{ .set_property = extra_index },
                });
            },
            .spread => |*expr| {
                const value_ref = try b.lowerExpression(expr);
                _ = try b.addInst(.{
                    .tag = .object_spread,
                    .data = .{ .binary = .{
                        .lhs = object_ref,
                        .rhs = value_ref,
                    } },
                });
            },
            .method_definition => |method_def| {
                const property_name = &method_def.class_element_name.property_name;
                const key: Key = if (property_name.* == .literal_property_name and
                    property_name.literal_property_name == .identifier)
                    .{ .string = try b.internString(
                        property_name.literal_property_name.identifier,
                        .literal,
                    ) }
                else blk: {
                    const key_ref = try b.lowerPropertyName(property_name);
                    const key_index = key_ref.toIndex().?;
                    const key_inst = b.instructions.get(@intFromEnum(key_index));
                    break :blk if (key_inst.tag == .string)
                        .{ .string = key_inst.data.string }
                    else
                        .{ .computed = key_ref };
                };
                const method_ref = switch (method_def.method) {
                    .get, .set => |func_expr| blk: {
                        const name: Ir.Function.Name = if (property_name.* == .literal_property_name and
                            property_name.literal_property_name == .identifier)
                        name: {
                            const prefix: []const u8 = if (method_def.method == .get) "get " else "set ";
                            const identifier = property_name.literal_property_name.identifier;
                            const prefixed = try std.fmt.allocPrint(b.gpa, "{s}{s}", .{ prefix, identifier });
                            defer b.gpa.free(prefixed);
                            break :name .{ .default = try b.internString(prefixed, .literal) };
                        } else .none;
                        const function_index = try b.addFunction(.{
                            .source_range = func_expr.source_range,
                            .name = name,
                            .parameters = func_expr.formal_parameters,
                            .body = func_expr.function_body,
                            .kind = .normal,
                        });
                        const func_ref = try b.addInst(.{
                            .tag = .create_function,
                            .data = .{ .create_function = function_index },
                        });
                        break :blk try b.addInst(.{
                            .tag = if (method_def.method == .get) .getter else .setter,
                            .data = .{ .ref = func_ref },
                        });
                    },
                    inline else => |func_expr, tag| blk: {
                        const name: Ir.Function.Name = if (property_name.* == .literal_property_name and
                            property_name.literal_property_name == .identifier)
                            .{ .default = try b.internString(
                                property_name.literal_property_name.identifier,
                                .literal,
                            ) }
                        else
                            .none;
                        const function_index = try b.addFunction(.{
                            .source_range = func_expr.source_range,
                            .name = name,
                            .parameters = func_expr.formal_parameters,
                            .body = func_expr.function_body,
                            .kind = switch (tag) {
                                .method => .normal,
                                .generator => .generator,
                                .async => .async,
                                .async_generator => .async_generator,
                                .get, .set => unreachable,
                            },
                        });
                        break :blk try b.addInst(.{
                            .tag = .create_function,
                            .data = .{ .create_function = function_index },
                        });
                    },
                };
                switch (key) {
                    .string => |string_index| {
                        const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                            .base = object_ref,
                            .name = string_index,
                            .value = method_ref,
                        });
                        _ = try b.addInst(.{
                            .tag = .object_set,
                            .data = .{ .set_property = extra_index },
                        });
                    },
                    .computed => |key_ref| {
                        const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                            .base = object_ref,
                            .property = key_ref,
                            .value = method_ref,
                        });
                        _ = try b.addInst(.{
                            .tag = .object_set_computed,
                            .data = .{ .set_property_computed = extra_index },
                        });
                    },
                }
            },
            .property_name_and_expression => |*prop| if (try prop.property_name.isProtoSetter(b.gpa)) {
                const value_ref = try b.lowerExpression(&prop.expression);
                _ = try b.addInst(.{
                    .tag = .object_set_prototype,
                    .data = .{ .binary = .{
                        .lhs = object_ref,
                        .rhs = value_ref,
                    } },
                });
            } else {
                const key: Key = if (prop.property_name == .literal_property_name and
                    prop.property_name.literal_property_name == .identifier)
                    .{ .string = try b.internString(
                        prop.property_name.literal_property_name.identifier,
                        .literal,
                    ) }
                else blk: {
                    const key_ref = try b.lowerPropertyName(&prop.property_name);
                    const key_index = key_ref.toIndex().?;
                    const key_inst = b.instructions.get(@intFromEnum(key_index));
                    break :blk if (key_inst.tag == .string)
                        .{ .string = key_inst.data.string }
                    else
                        .{ .computed = key_ref };
                };
                const value_ref = try b.lowerExpression(&prop.expression);
                switch (key) {
                    .string => |string_index| {
                        if (prop.expression.isAnonymousFunctionDefinition()) {
                            b.setAnonymousFunctionName(value_ref, string_index);
                        }
                        const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                            .base = object_ref,
                            .name = string_index,
                            .value = value_ref,
                        });
                        _ = try b.addInst(.{
                            .tag = .object_set,
                            .data = .{ .set_property = extra_index },
                        });
                    },
                    .computed => |key_ref| {
                        const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                            .base = object_ref,
                            .property = key_ref,
                            .value = value_ref,
                        });
                        _ = try b.addInst(.{
                            .tag = .object_set_computed,
                            .data = .{ .set_property_computed = extra_index },
                        });
                    },
                }
            },
        }
    }

    return object_ref;
}

fn lowerClassExpression(b: *Builder, class_expr: *const ast.ClassExpression) Error!Ir.Inst.Ref {
    const name: Ir.Class.Name = if (class_expr.identifier) |identifier|
        .{ .identifier = try b.internString(identifier, .literal) }
    else
        .none;

    const heritage = try b.lowerClassHeritage(class_expr.class_tail.class_heritage);

    _ = try b.addInst(.{
        .tag = .push_private_scope,
        .data = .{ .none = {} },
    });
    const element_names = try b.lowerClassElementNames(&class_expr.class_tail.class_body);

    const class_index = try b.addClass(.{
        .source_range = class_expr.source_range,
        .name = name,
        .class_tail = class_expr.class_tail,
        .heritage = heritage,
        .element_names = element_names,
    });
    const result = try b.addInst(.{
        .tag = .create_class,
        .data = .{ .create_class = class_index },
    });
    _ = try b.addInst(.{
        .tag = .pop_private_scope,
        .data = .{ .none = {} },
    });
    return result;
}

fn lowerClassHeritage(b: *Builder, class_heritage: ?*const ast.Expression) Error!Ir.Inst.Ref {
    if (class_heritage) |heritage| {
        return b.lowerExpression(heritage);
    }
    return .none;
}

fn lowerClassElementNames(b: *Builder, class_body: *const ast.ClassBody) Error![]const Ir.Inst.Ref {
    var names: std.ArrayList(Ir.Inst.Ref) = .empty;
    var private_names: std.StringHashMapUnmanaged(Ir.Inst.Ref) = .empty;
    defer private_names.deinit(b.gpa);
    for (class_body.class_element_list.items) |class_element| {
        switch (class_element.classElementKind()) {
            .constructor_method, .empty => continue,
            .non_constructor_method => {},
        }
        const name_ref: Ir.Inst.Ref = switch (class_element) {
            .method_definition, .static_method_definition => |method_def| try b.lowerClassElementName(&method_def.class_element_name, &private_names),
            .field_definition, .static_field_definition => |field_def| try b.lowerClassElementName(&field_def.class_element_name, &private_names),
            .class_static_block, .empty_statement => .none,
        };
        try names.append(b.gpa, name_ref);
    }
    return names.toOwnedSlice(b.gpa);
}

fn lowerClassElementName(b: *Builder, class_element_name: *const ast.ClassElementName, private_names: *std.StringHashMapUnmanaged(Ir.Inst.Ref)) Error!Ir.Inst.Ref {
    return switch (class_element_name.*) {
        .property_name => |*property_name| try b.lowerPropertyName(property_name),
        .private_identifier => |private_identifier| {
            const gop = try private_names.getOrPut(b.gpa, private_identifier);
            if (gop.found_existing) {
                return gop.value_ptr.*;
            }
            const string_index = try b.internString(private_identifier, .literal);
            const ref = try b.addInst(.{
                .tag = .create_private_element,
                .data = .{ .string = string_index },
            });
            gop.value_ptr.* = ref;
            return ref;
        },
    };
}

fn lowerFunctionExpression(b: *Builder, func_expr: anytype) Error!Ir.Inst.Ref {
    const name: Ir.Function.Name = if (func_expr.identifier) |identifier|
        .{ .identifier = try b.internString(identifier, .literal) }
    else
        .none;

    const function_index = try b.addFunction(.{
        .source_range = func_expr.source_range,
        .name = name,
        .parameters = func_expr.formal_parameters,
        .body = func_expr.function_body,
        .kind = switch (func_expr.function_body.type) {
            .normal => .normal,
            .generator => .generator,
            .async => .async,
            .async_generator => .async_generator,
        },
    });

    return b.addInst(.{
        .tag = .create_function,
        .data = .{ .create_function = function_index },
    });
}

fn lowerRegularExpressionLiteral(b: *Builder, regexp_lit: *const ast.RegularExpressionLiteral) Error!Ir.Inst.Ref {
    const pattern_index = try b.internString(regexp_lit.pattern, .literal);
    const flags_index = try b.internString(regexp_lit.flags, .literal);
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
                const string_index = try b.internString(normalized, .escaped);
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

fn lowerArrowFunction(b: *Builder, arrow_func: anytype) Error!Ir.Inst.Ref {
    const function_index = try b.addFunction(.{
        .source_range = arrow_func.source_range,
        .name = .none,
        .parameters = arrow_func.formal_parameters,
        .body = arrow_func.function_body,
        .kind = switch (arrow_func.function_body.type) {
            .normal => .arrow,
            .async => .async_arrow,
            .generator, .async_generator => unreachable,
        },
    });

    return b.addInst(.{
        .tag = .create_function,
        .data = .{ .create_function = function_index },
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
                if (constant == .string) {
                    const string_index = try b.internString(constant.string, .escaped);
                    return b.addInst(.{
                        .tag = .get_property,
                        .data = .{ .get_property = .{
                            .base = base,
                            .name = string_index,
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
            const string_index = try b.internString(identifier, .literal);
            return b.addInst(.{
                .tag = .get_property,
                .data = .{ .get_property = .{
                    .base = base,
                    .name = string_index,
                } },
            });
        },
        .private_identifier => |private_identifier| {
            const string_index = try b.internString(private_identifier, .literal);
            return b.addInst(.{
                .tag = .get_private_element,
                .data = .{ .get_property = .{
                    .base = base,
                    .name = string_index,
                } },
            });
        },
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

fn lowerSuperProperty(b: *Builder, super_prop: *const ast.SuperProperty, base_out: ?*Ir.Inst.Ref) Error!Ir.Inst.Ref {
    if (base_out) |ptr| ptr.* = try b.lowerThis();
    return switch (super_prop.*) {
        .identifier => |identifier| blk: {
            const string_index = try b.internString(identifier, .literal);
            break :blk try b.addInst(.{
                .tag = .get_super_property,
                .data = .{ .string = string_index },
            });
        },
        .expression => |expr| blk: {
            const property = try b.lowerExpression(expr);
            break :blk try b.addInst(.{
                .tag = .get_super_property_computed,
                .data = .{ .ref = property },
            });
        },
    };
}

fn lowerNewTarget(b: *Builder) Error!Ir.Inst.Ref {
    return b.addInst(.{
        .tag = .get_new_target,
        .data = .{ .none = {} },
    });
}

fn lowerImportMeta(b: *Builder) Error!Ir.Inst.Ref {
    return b.addInst(.{
        .tag = .get_import_meta,
        .data = .{ .none = {} },
    });
}

fn lowerNewExpression(b: *Builder, new_expr: *const ast.NewExpression) Error!Ir.Inst.Ref {
    const constructor = try b.lowerExpression(new_expr.expression);

    var args = try b.lowerArguments(new_expr.arguments);
    defer args.deinit(b.gpa);

    const extra_index = try b.addExtra(Ir.Inst.Construct, .{
        .constructor = constructor,
        .args_len = @intCast(args.items.len),
    });
    try b.extra.appendSlice(b.gpa, @ptrCast(args.items));

    return b.addInst(.{
        .tag = .construct,
        .data = .{ .construct = extra_index },
    });
}

fn lowerCallExpression(b: *Builder, call_expr: *const ast.CallExpression) Error!Ir.Inst.Ref {
    var this_value: Ir.Inst.Ref = .none;
    const callee = switch (call_expr.expression.*) {
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, &this_value),
        .super_property => |*super_prop| try b.lowerSuperProperty(super_prop, &this_value),
        else => try b.lowerExpression(call_expr.expression),
    };

    var args = try b.lowerArguments(call_expr.arguments);
    defer args.deinit(b.gpa);

    const extra_index = try b.addExtra(Ir.Inst.Call, .{
        .callee = callee,
        .this_value = this_value,
        .args_len = @intCast(args.items.len),
    });
    try b.extra.appendSlice(b.gpa, @ptrCast(args.items));

    const is_direct_eval = call_expr.expression.* == .primary_expression and
        call_expr.expression.primary_expression == .identifier_reference and
        std.mem.eql(u8, call_expr.expression.primary_expression.identifier_reference, "eval");

    const tag: Ir.Inst.Tag = if (is_direct_eval)
        (if (b.in_strict_mode) .call_direct_eval_strict else .call_direct_eval)
    else
        .call;

    return b.addInst(.{
        .tag = tag,
        .data = .{ .call = extra_index },
    });
}

fn lowerSuperCall(b: *Builder, super_call: *const ast.SuperCall) Error!Ir.Inst.Ref {
    var args = try b.lowerArguments(super_call.arguments);
    defer args.deinit(b.gpa);

    const extra_index = try b.addExtra(Ir.Inst.SuperCall, .{
        .args_len = @intCast(args.items.len),
    });
    try b.extra.appendSlice(b.gpa, @ptrCast(args.items));

    return b.addInst(.{
        .tag = .super_call,
        .data = .{ .super_call = extra_index },
    });
}

fn lowerImportCall(b: *Builder, import_call: *const ast.ImportCall) Error!Ir.Inst.Ref {
    const specifier = try b.lowerExpression(import_call.specifier_expression);
    const options = if (import_call.options_expression) |expr|
        try b.lowerExpression(expr)
    else
        try b.addInst(.{
            .tag = .undefined,
            .data = .{ .none = {} },
        });
    return b.addInst(.{
        .tag = .import_call,
        .data = .{ .binary = .{ .lhs = specifier, .rhs = options } },
    });
}

fn lowerOptionalExpression(b: *Builder, opt_expr: *const ast.OptionalExpression) Error!Ir.Inst.Ref {
    const first_property = opt_expr.properties[0];

    var this_value: Ir.Inst.Ref = .none;
    const base = switch (opt_expr.expression.*) {
        .member_expression => |*member_expr| blk: {
            if (first_property == .arguments) {
                break :blk try b.lowerMemberExpression(member_expr, &this_value);
            } else {
                break :blk try b.lowerMemberExpression(member_expr, null);
            }
        },
        else => try b.lowerExpression(opt_expr.expression),
    };

    const null_ref = try b.addInst(.{
        .tag = .null,
        .data = .{ .none = {} },
    });
    const is_nullish = try b.addInst(.{
        .tag = .eq,
        .data = .{ .binary = .{
            .lhs = base,
            .rhs = null_ref,
        } },
    });
    const nullish_br = try b.addInstDeferred(.br_cond);

    const properties_label = try b.addLabel();

    var current = base;
    for (opt_expr.properties, 0..) |property, i| {
        const next_is_call = i + 1 < opt_expr.properties.len and opt_expr.properties[i + 1] == .arguments;

        current = switch (property) {
            .arguments => |arguments| blk: {
                var args = try b.lowerArguments(arguments);
                defer args.deinit(b.gpa);

                const extra_index = try b.addExtra(Ir.Inst.Call, .{
                    .callee = current,
                    .this_value = this_value,
                    .args_len = @intCast(args.items.len),
                });
                try b.extra.appendSlice(b.gpa, @ptrCast(args.items));

                this_value = .none;
                break :blk try b.addInst(.{
                    .tag = .call,
                    .data = .{ .call = extra_index },
                });
            },
            .expression => |expr| blk: {
                if (next_is_call) this_value = current;

                if (try constantFold(b.gpa, expr)) |constant| {
                    defer constant.deinit(b.gpa);
                    if (constant.toIndex()) |index| {
                        break :blk try b.addInst(.{
                            .tag = .get_property_indexed,
                            .data = .{ .get_property_indexed = .{
                                .base = current,
                                .index = index,
                            } },
                        });
                    }
                    if (constant == .string) {
                        const string_index = try b.internString(constant.string, .escaped);
                        break :blk try b.addInst(.{
                            .tag = .get_property,
                            .data = .{ .get_property = .{
                                .base = current,
                                .name = string_index,
                            } },
                        });
                    }
                }
                const prop = try b.lowerExpression(expr);
                break :blk try b.addInst(.{
                    .tag = .get_property_computed,
                    .data = .{ .get_property_computed = .{
                        .base = current,
                        .property = prop,
                    } },
                });
            },
            .identifier => |identifier| blk: {
                if (next_is_call) this_value = current;

                const string_index = try b.internString(identifier, .literal);
                break :blk try b.addInst(.{
                    .tag = .get_property,
                    .data = .{ .get_property = .{
                        .base = current,
                        .name = string_index,
                    } },
                });
            },
            .private_identifier => |private_identifier| blk: {
                const string_index = try b.internString(private_identifier, .literal);
                break :blk try b.addInst(.{
                    .tag = .get_private_element,
                    .data = .{ .get_property = .{
                        .base = current,
                        .name = string_index,
                    } },
                });
            },
        };
    }
    const properties_br = try b.addInstDeferred(.br);

    const short_circuit_label = try b.addLabel();
    const undefined_ref = try b.addInst(.{
        .tag = .undefined,
        .data = .{ .none = {} },
    });
    const short_circuit_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    nullish_br.set(.{ .br_cond = .{
        .condition = is_nullish,
        .then_target = short_circuit_label,
        .else_target = properties_label,
    } });
    properties_br.set(.{ .br = .{
        .target = end_label,
        .value = current,
    } });
    short_circuit_br.set(.{ .br = .{
        .target = end_label,
        .value = undefined_ref,
    } });

    return end_label;
}

fn lowerUpdateExpression(b: *Builder, update_expr: *const ast.UpdateExpression) Error!Ir.Inst.Ref {
    const update_op: Ir.Inst.UpdateOp = switch (update_expr.operator) {
        .@"++" => switch (update_expr.type) {
            .prefix => .increment_prefix,
            .postfix => .increment_postfix,
        },
        .@"--" => switch (update_expr.type) {
            .prefix => .decrement_prefix,
            .postfix => .decrement_postfix,
        },
    };
    switch (update_expr.expression.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .identifier_reference => |identifier| {
                const string_index = try b.internString(identifier, .literal);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode) .update_binding_strict else .update_binding;
                return b.addInst(.{
                    .tag = tag,
                    .data = .{ .update_binding = .{
                        .name = string_index,
                        .update_op = update_op,
                    } },
                });
            },
            else => unreachable,
        },
        .member_expression => |*member_expr| {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .update_property_strict
                    else
                        .update_property;
                    const extra_index = try b.addExtra(Ir.Inst.UpdateProperty, .{
                        .base = base,
                        .name = string_index,
                        .update_op = update_op,
                    });
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .update_property = extra_index },
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
                            const extra_index = try b.addExtra(Ir.Inst.UpdatePropertyIndexed, .{
                                .base = base,
                                .index = index,
                                .update_op = update_op,
                            });
                            return b.addInst(.{
                                .tag = tag,
                                .data = .{ .update_property_indexed = extra_index },
                            });
                        }
                    }
                    const property = try b.lowerExpression(expr);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .update_property_computed_strict
                    else
                        .update_property_computed;
                    const extra_index = try b.addExtra(Ir.Inst.UpdatePropertyComputed, .{
                        .base = base,
                        .property = property,
                        .update_op = update_op,
                    });
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .update_property_computed = extra_index },
                    });
                },
                .private_identifier => |private_identifier| {
                    const string_index = try b.internString(private_identifier, .literal);
                    const current_value = try b.addInst(.{
                        .tag = .get_private_element,
                        .data = .{ .get_property = .{
                            .base = base,
                            .name = string_index,
                        } },
                    });
                    const to_numeric = try b.addInst(.{
                        .tag = .to_numeric,
                        .data = .{ .ref = current_value },
                    });
                    const one = try b.addInst(.{
                        .tag = .one,
                        .data = .{ .none = {} },
                    });
                    const tag: Ir.Inst.Tag = switch (update_expr.operator) {
                        .@"++" => .add,
                        .@"--" => .sub,
                    };
                    const new_value = try b.addInst(.{
                        .tag = tag,
                        .data = .{ .binary = .{
                            .lhs = to_numeric,
                            .rhs = one,
                        } },
                    });
                    const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                        .base = base,
                        .name = string_index,
                        .value = new_value,
                    });
                    _ = try b.addInst(.{
                        .tag = .set_private_element,
                        .data = .{ .set_property = extra_index },
                    });
                    return if (update_expr.type == .prefix) new_value else to_numeric;
                },
            }
        },
        .super_property => |super_prop| switch (super_prop) {
            .identifier => |identifier| {
                const string_index = try b.internString(identifier, .literal);
                const current_value = try b.addInst(.{
                    .tag = .get_super_property,
                    .data = .{ .string = string_index },
                });
                const to_numeric = try b.addInst(.{
                    .tag = .to_numeric,
                    .data = .{ .ref = current_value },
                });
                const one = try b.addInst(.{
                    .tag = .one,
                    .data = .{ .none = {} },
                });
                const tag: Ir.Inst.Tag = switch (update_expr.operator) {
                    .@"++" => .add,
                    .@"--" => .sub,
                };
                const new_value = try b.addInst(.{
                    .tag = tag,
                    .data = .{ .binary = .{
                        .lhs = to_numeric,
                        .rhs = one,
                    } },
                });
                const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                    .base = .none,
                    .name = string_index,
                    .value = new_value,
                });
                const set_tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .set_super_property_strict
                else
                    .set_super_property;
                _ = try b.addInst(.{
                    .tag = set_tag,
                    .data = .{ .set_property = extra_index },
                });
                return if (update_expr.type == .prefix) new_value else to_numeric;
            },
            .expression => |expr| {
                const property = try b.lowerExpression(expr);
                const current_value = try b.addInst(.{
                    .tag = .get_super_property_computed,
                    .data = .{ .ref = property },
                });
                const to_numeric = try b.addInst(.{
                    .tag = .to_numeric,
                    .data = .{ .ref = current_value },
                });
                const one = try b.addInst(.{
                    .tag = .one,
                    .data = .{ .none = {} },
                });
                const tag: Ir.Inst.Tag = switch (update_expr.operator) {
                    .@"++" => .add,
                    .@"--" => .sub,
                };
                const new_value = try b.addInst(.{
                    .tag = tag,
                    .data = .{ .binary = .{
                        .lhs = to_numeric,
                        .rhs = one,
                    } },
                });
                const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                    .base = .none,
                    .property = property,
                    .value = new_value,
                });
                const set_tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .set_super_property_computed_strict
                else
                    .set_super_property_computed;
                _ = try b.addInst(.{
                    .tag = set_tag,
                    .data = .{ .set_property_computed = extra_index },
                });
                return if (update_expr.type == .prefix) new_value else to_numeric;
            },
        },
        .call_expression => |*call_expr| {
            _ = try b.lowerCallExpression(call_expr);
            return b.addInst(.{
                .tag = .throw_reference_error,
                .data = .{ .none = {} },
            });
        },
        else => unreachable,
    }
}

fn lowerUnaryExpression(b: *Builder, unary_expr: *const ast.UnaryExpression) Error!Ir.Inst.Ref {
    if (unary_expr.operator == .typeof and
        unary_expr.expression.* == .primary_expression and
        unary_expr.expression.primary_expression == .identifier_reference)
    {
        const identifier = unary_expr.expression.primary_expression.identifier_reference;
        const string_index = try b.internString(identifier, .literal);
        return b.addInst(.{
            .tag = .typeof_binding,
            .data = .{ .string = string_index },
        });
    }
    if (unary_expr.operator == .delete and
        unary_expr.expression.* == .primary_expression and
        unary_expr.expression.primary_expression == .identifier_reference)
    {
        const identifier = unary_expr.expression.primary_expression.identifier_reference;
        const string_index = try b.internString(identifier, .literal);
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
                const string_index = try b.internString(identifier, .literal);
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
            .private_identifier => unreachable,
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
    switch (rel_expr.lhs) {
        .expression => |expr| {
            const lhs = try b.lowerExpression(expr);
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
        },
        .private_identifier => |private_identifier| {
            const string_index = try b.internString(private_identifier, .literal);
            const name = try b.addInst(.{
                .tag = .resolve_private_element,
                .data = .{ .string = string_index },
            });
            const rhs = try b.lowerExpression(rel_expr.rhs_expression);
            return b.addInst(.{
                .tag = .has_private_element,
                .data = .{ .binary = .{
                    .lhs = name,
                    .rhs = rhs,
                } },
            });
        },
    }
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

    const condition, const then_target_is_rhs = switch (log_expr.operator) {
        .@"&&" => .{ lhs, true },
        .@"||" => .{ lhs, false },
        .@"??" => blk: {
            const null_ref = try b.addInst(.{
                .tag = .null,
                .data = .{ .none = {} },
            });
            const is_nullish = try b.addInst(.{
                .tag = .eq,
                .data = .{ .binary = .{
                    .lhs = lhs,
                    .rhs = null_ref,
                } },
            });
            break :blk .{ is_nullish, true };
        },
    };
    const br_cond = try b.addInstDeferred(.br_cond);

    const lhs_label = try b.addLabel();
    const lhs_br = try b.addInstDeferred(.br);

    const rhs_label = try b.addLabel();
    const rhs = try b.lowerExpression(log_expr.rhs_expression);
    const rhs_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    br_cond.set(.{ .br_cond = .{
        .condition = condition,
        .then_target = if (then_target_is_rhs) rhs_label else lhs_label,
        .else_target = if (then_target_is_rhs) lhs_label else rhs_label,
    } });
    lhs_br.set(.{ .br = .{
        .target = end_label,
        .value = lhs,
    } });
    rhs_br.set(.{ .br = .{
        .target = end_label,
        .value = rhs,
    } });

    return end_label;
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
                const string_index = try b.internString(identifier, .literal);
                if (assign_expr.rhs_expression.isAnonymousFunctionDefinition()) {
                    b.setAnonymousFunctionName(value, string_index);
                }
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
            else => unreachable,
        },
        .member_expression => |*member_expr| {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const value = try b.lowerExpression(assign_expr.rhs_expression);
                    const string_index = try b.internString(identifier, .literal);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_strict
                    else
                        .set_property;
                    const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                        .base = base,
                        .name = string_index,
                        .value = value,
                    });
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_property = extra_index },
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
                            const extra_index = try b.addExtra(Ir.Inst.SetPropertyIndexed, .{
                                .base = base,
                                .index = index,
                                .value = value,
                            });
                            return b.addInst(.{
                                .tag = tag,
                                .data = .{ .set_property_indexed = extra_index },
                            });
                        }
                    }
                    const property = try b.lowerExpression(expr);
                    const value = try b.lowerExpression(assign_expr.rhs_expression);
                    const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                        .set_property_computed_strict
                    else
                        .set_property_computed;
                    const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                        .base = base,
                        .property = property,
                        .value = value,
                    });
                    return b.addInst(.{
                        .tag = tag,
                        .data = .{ .set_property_computed = extra_index },
                    });
                },
                .private_identifier => |private_identifier| {
                    const string_index = try b.internString(private_identifier, .literal);
                    const value = try b.lowerExpression(assign_expr.rhs_expression);
                    const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                        .base = base,
                        .name = string_index,
                        .value = value,
                    });
                    return b.addInst(.{
                        .tag = .set_private_element,
                        .data = .{ .set_property = extra_index },
                    });
                },
            }
        },
        .super_property => |super_prop| switch (super_prop) {
            .identifier => |identifier| {
                const value = try b.lowerExpression(assign_expr.rhs_expression);
                const string_index = try b.internString(identifier, .literal);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .set_super_property_strict
                else
                    .set_super_property;
                const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                    .base = .none,
                    .name = string_index,
                    .value = value,
                });
                return b.addInst(.{
                    .tag = tag,
                    .data = .{ .set_property = extra_index },
                });
            },
            .expression => |prop_expr| {
                const property = try b.lowerExpression(prop_expr);
                const value = try b.lowerExpression(assign_expr.rhs_expression);
                const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .set_super_property_computed_strict
                else
                    .set_super_property_computed;
                const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                    .base = .none,
                    .property = property,
                    .value = value,
                });
                return b.addInst(.{
                    .tag = tag,
                    .data = .{ .set_property_computed = extra_index },
                });
            },
        },
        .binding_pattern_for_assignment_expression => |*pattern| {
            const value = try b.lowerExpression(assign_expr.rhs_expression);
            _ = try b.lowerDestructuringAssignment(pattern, value, .set);
            return value;
        },
        .call_expression => |*call_expr| {
            _ = try b.lowerCallExpression(call_expr);
            return b.addInst(.{
                .tag = .throw_reference_error,
                .data = .{ .none = {} },
            });
        },
        else => unreachable,
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
                const string_index = try b.internString(identifier, .literal);
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
            else => unreachable,
        },
        .member_expression => |*member_expr| {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
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
                    const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                        .base = base,
                        .name = string_index,
                        .value = result,
                    });
                    return b.addInst(.{
                        .tag = set_tag,
                        .data = .{ .set_property = extra_index },
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
                            const extra_index = try b.addExtra(Ir.Inst.SetPropertyIndexed, .{
                                .base = base,
                                .index = index,
                                .value = result,
                            });
                            return b.addInst(.{
                                .tag = set_tag,
                                .data = .{ .set_property_indexed = extra_index },
                            });
                        }
                        if (constant == .string) {
                            const string_index = try b.internString(constant.string, .escaped);
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
                            const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                                .base = base,
                                .name = string_index,
                                .value = result,
                            });
                            return b.addInst(.{
                                .tag = set_tag,
                                .data = .{ .set_property = extra_index },
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
                    const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                        .base = base,
                        .property = property,
                        .value = result,
                    });
                    return b.addInst(.{
                        .tag = set_tag,
                        .data = .{ .set_property_computed = extra_index },
                    });
                },
                .private_identifier => |private_identifier| {
                    const string_index = try b.internString(private_identifier, .literal);
                    const current_value = try b.addInst(.{
                        .tag = .get_private_element,
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
                    const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                        .base = base,
                        .name = string_index,
                        .value = result,
                    });
                    return b.addInst(.{
                        .tag = .set_private_element,
                        .data = .{ .set_property = extra_index },
                    });
                },
            }
        },
        .super_property => |super_prop| switch (super_prop) {
            .identifier => |identifier| {
                const string_index = try b.internString(identifier, .literal);
                const current_value = try b.addInst(.{
                    .tag = .get_super_property,
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
                const set_tag: Ir.Inst.Tag = if (b.in_strict_mode)
                    .set_super_property_strict
                else
                    .set_super_property;
                const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                    .base = .none,
                    .name = string_index,
                    .value = result,
                });
                return b.addInst(.{
                    .tag = set_tag,
                    .data = .{ .set_property = extra_index },
                });
            },
            .expression => |expr| {
                const property = try b.lowerExpression(expr);
                const current_value = try b.addInst(.{
                    .tag = .get_super_property_computed,
                    .data = .{ .ref = property },
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
                    .set_super_property_computed_strict
                else
                    .set_super_property_computed;
                const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                    .base = .none,
                    .property = property,
                    .value = result,
                });
                return b.addInst(.{
                    .tag = set_tag,
                    .data = .{ .set_property_computed = extra_index },
                });
            },
        },
        .call_expression => |*call_expr| {
            _ = try b.lowerCallExpression(call_expr);
            return b.addInst(.{
                .tag = .throw_reference_error,
                .data = .{ .none = {} },
            });
        },
        else => unreachable,
    }
}

fn lowerLogicalCompoundAssignmentExpression(b: *Builder, assign_expr: *const ast.AssignmentExpression) Error!Ir.Inst.Ref {
    const Lhs = union(enum) {
        binding: Ir.Inst.StringIndex,
        property: struct { base: Ir.Inst.Ref, name: Ir.Inst.StringIndex },
        property_indexed: struct { base: Ir.Inst.Ref, index: u32 },
        property_computed: struct { base: Ir.Inst.Ref, property: Ir.Inst.Ref },
        super_property: Ir.Inst.StringIndex,
        super_property_computed: Ir.Inst.Ref,
        private_element: struct { base: Ir.Inst.Ref, name: Ir.Inst.StringIndex },
    };

    var lhs: Lhs = undefined;
    const current_value: Ir.Inst.Ref = switch (assign_expr.lhs_expression.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .identifier_reference => |identifier| blk: {
                const string_index = try b.internString(identifier, .literal);
                lhs = .{ .binding = string_index };
                break :blk try b.addInst(.{
                    .tag = .get_binding,
                    .data = .{ .string = string_index },
                });
            },
            else => unreachable,
        },
        .member_expression => |*member_expr| blk: {
            const base = try b.lowerExpression(member_expr.expression);
            switch (member_expr.property) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
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
                        if (constant == .string) {
                            const string_index = try b.internString(constant.string, .escaped);
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
                .private_identifier => |private_identifier| {
                    const string_index = try b.internString(private_identifier, .literal);
                    lhs = .{ .private_element = .{
                        .base = base,
                        .name = string_index,
                    } };
                    break :blk try b.addInst(.{
                        .tag = .get_private_element,
                        .data = .{ .get_property = .{
                            .base = base,
                            .name = string_index,
                        } },
                    });
                },
            }
        },
        .super_property => |super_prop| blk: {
            switch (super_prop) {
                .identifier => |identifier| {
                    const string_index = try b.internString(identifier, .literal);
                    lhs = .{ .super_property = string_index };
                    break :blk try b.addInst(.{
                        .tag = .get_super_property,
                        .data = .{ .string = string_index },
                    });
                },
                .expression => |expr| {
                    const property = try b.lowerExpression(expr);
                    lhs = .{ .super_property_computed = property };
                    break :blk try b.addInst(.{
                        .tag = .get_super_property_computed,
                        .data = .{ .ref = property },
                    });
                },
            }
        },
        else => unreachable,
    };

    const condition, const then_target_is_assign = switch (assign_expr.operator) {
        .@"&&=" => .{ current_value, true },
        .@"||=" => .{ current_value, false },
        .@"??=" => blk: {
            const null_ref = try b.addInst(.{
                .tag = .null,
                .data = .{ .none = {} },
            });
            const is_nullish = try b.addInst(.{
                .tag = .eq,
                .data = .{ .binary = .{
                    .lhs = current_value,
                    .rhs = null_ref,
                } },
            });
            break :blk .{ is_nullish, true };
        },
        else => unreachable,
    };
    const br_cond = try b.addInstDeferred(.br_cond);

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
            const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                .base = p.base,
                .name = p.name,
                .value = rhs,
            });
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property = extra_index },
            });
        },
        .property_indexed => |p| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_property_indexed_strict
            else
                .set_property_indexed;
            const extra_index = try b.addExtra(Ir.Inst.SetPropertyIndexed, .{
                .base = p.base,
                .index = p.index,
                .value = rhs,
            });
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property_indexed = extra_index },
            });
        },
        .property_computed => |p| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_property_computed_strict
            else
                .set_property_computed;
            const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                .base = p.base,
                .property = p.property,
                .value = rhs,
            });
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property_computed = extra_index },
            });
        },
        .super_property => |string_index| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_super_property_strict
            else
                .set_super_property;
            const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                .base = .none,
                .name = string_index,
                .value = rhs,
            });
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property = extra_index },
            });
        },
        .super_property_computed => |property| {
            const tag: Ir.Inst.Tag = if (b.in_strict_mode)
                .set_super_property_computed_strict
            else
                .set_super_property_computed;
            const extra_index = try b.addExtra(Ir.Inst.SetPropertyComputed, .{
                .base = .none,
                .property = property,
                .value = rhs,
            });
            _ = try b.addInst(.{
                .tag = tag,
                .data = .{ .set_property_computed = extra_index },
            });
        },
        .private_element => |p| {
            const extra_index = try b.addExtra(Ir.Inst.SetProperty, .{
                .base = p.base,
                .name = p.name,
                .value = rhs,
            });
            _ = try b.addInst(.{
                .tag = .set_private_element,
                .data = .{ .set_property = extra_index },
            });
        },
    }

    const assign_br = try b.addInstDeferred(.br);

    const skip_label = try b.addLabel();
    const skip_br = try b.addInstDeferred(.br);

    const end_label = try b.addLabel();

    br_cond.set(.{ .br_cond = .{
        .condition = condition,
        .then_target = if (then_target_is_assign) assign_label else skip_label,
        .else_target = if (then_target_is_assign) skip_label else assign_label,
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

fn lowerAwaitExpression(b: *Builder, await_expr: *const ast.AwaitExpression) Error!Ir.Inst.Ref {
    const value = try b.lowerExpression(await_expr.expression);
    return b.addInst(.{
        .tag = .await,
        .data = .{ .ref = value },
    });
}

fn lowerYieldExpression(b: *Builder, yield_expr: *const ast.YieldExpression) Error!Ir.Inst.Ref {
    switch (yield_expr.*) {
        .none => {
            const value = try b.addInst(.{
                .tag = .undefined,
                .data = .{ .none = {} },
            });
            return b.addInst(.{
                .tag = .yield,
                .data = .{ .ref = value },
            });
        },
        .expression => |expr| {
            const value = try b.lowerExpression(expr);
            return b.addInst(.{
                .tag = .yield,
                .data = .{ .ref = value },
            });
        },
        .delegate => |expr| {
            const value = try b.lowerExpression(expr);
            const iterator = try b.addInst(.{
                .tag = switch (b.root_node.function.body.type) {
                    .generator => .get_iterator,
                    .async_generator => .get_async_iterator,
                    else => unreachable,
                },
                .data = .{ .ref = value },
            });
            return b.addInst(.{
                .tag = .yield_star,
                .data = .{ .ref = iterator },
            });
        },
    }
}

fn lowerTaggedTemplate(b: *Builder, tagged_template: *const ast.TaggedTemplate) Error!Ir.Inst.Ref {
    var this_value: Ir.Inst.Ref = .none;
    const callee = switch (tagged_template.expression.*) {
        .member_expression => |*member_expr| try b.lowerMemberExpression(member_expr, &this_value),
        else => try b.lowerExpression(tagged_template.expression),
    };

    const template_literal = &tagged_template.template_literal;

    var cooked_indices: std.ArrayList(Ir.Inst.StringIndex) = .empty;
    defer cooked_indices.deinit(b.gpa);
    var raw_indices: std.ArrayList(Ir.Inst.StringIndex) = .empty;
    defer raw_indices.deinit(b.gpa);

    for (template_literal.spans, 0..) |span, i| {
        if (i % 2 != 0) {
            std.debug.assert(span == .expression);
            continue;
        }

        const text = span.templateCharacters();
        const normalized = try std.mem.replaceOwned(u8, b.gpa, text, "\r\n", "\n");
        defer b.gpa.free(normalized);
        _ = std.mem.replaceScalar(u8, normalized, '\r', '\n');

        try cooked_indices.append(b.gpa, try b.internString(normalized, .escaped));
        try raw_indices.append(b.gpa, try b.internString(normalized, .literal));
    }

    const cooked_array = blk: {
        const array_ref = try b.addInst(.{
            .tag = .array_create,
            .data = .{ .array = .{
                .len = @intCast(cooked_indices.items.len),
                .has_spread = false,
            } },
        });
        for (cooked_indices.items) |string_index| {
            const ref = try b.addInst(.{ .tag = .string, .data = .{
                .string = string_index,
            } });
            _ = try b.addInst(.{
                .tag = .array_push,
                .data = .{ .binary = .{
                    .lhs = array_ref,
                    .rhs = ref,
                } },
            });
        }
        break :blk array_ref;
    };

    const raw_array = blk: {
        const array_ref = try b.addInst(.{
            .tag = .array_create,
            .data = .{ .array = .{
                .len = @intCast(raw_indices.items.len),
                .has_spread = false,
            } },
        });
        for (raw_indices.items) |string_index| {
            const ref = try b.addInst(.{ .tag = .string, .data = .{
                .string = string_index,
            } });
            _ = try b.addInst(.{
                .tag = .array_push,
                .data = .{ .binary = .{
                    .lhs = array_ref,
                    .rhs = ref,
                } },
            });
        }
        break :blk array_ref;
    };

    const template_id = b.template_object_count;
    b.template_object_count += 1;
    const get_template_object_extra_index = try b.addExtra(Ir.Inst.GetTemplateObject, .{
        .cooked = cooked_array,
        .raw = raw_array,
        .id = template_id,
    });

    const template_object = try b.addInst(.{
        .tag = .get_template_object,
        .data = .{ .get_template_object = get_template_object_extra_index },
    });

    const substitution_count = template_literal.spans.len / 2;
    var args: std.ArrayList(Ir.Inst.Ref) = try .initCapacity(b.gpa, 1 + substitution_count);
    defer args.deinit(b.gpa);
    args.appendAssumeCapacity(template_object);

    for (template_literal.spans, 0..) |span, i| {
        if (i % 2 == 0) {
            std.debug.assert(span == .text);
            continue;
        }
        const expr_ref = try b.lowerExpression(&span.expression);
        args.appendAssumeCapacity(expr_ref);
    }

    const call_extra_index = try b.addExtra(Ir.Inst.Call, .{
        .callee = callee,
        .this_value = this_value,
        .args_len = @intCast(args.items.len),
    });
    try b.extra.appendSlice(b.gpa, @ptrCast(args.items));

    return b.addInst(.{
        .tag = .call,
        .data = .{ .call = call_extra_index },
    });
}
