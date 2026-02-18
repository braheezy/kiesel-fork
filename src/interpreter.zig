const std = @import("std");

const ast = @import("language/ast.zig");
const builtins = @import("builtins.zig");
const execution = @import("execution.zig");
const language = @import("language.zig");
const types = @import("types.zig");

const Agent = execution.Agent;
const Parser = language.Parser;
const Realm = execution.Realm;
const Script = language.Script;
const Value = types.Value;

pub const Bytecode = @import("interpreter/Bytecode.zig");
pub const Ir = @import("interpreter/Ir.zig");
pub const Vm = @import("interpreter/Vm.zig");

pub fn compile(
    agent: *Agent,
    name: []const u8,
    ast_node: Ir.Builder.Ast,
) Agent.Error!Bytecode {
    // TODO: Don't use the GC allocator for IR generation
    const gpa = agent.gc_allocator;

    var ir = ir: {
        var builder: Ir.Builder = .init(gpa, name, ast_node);
        defer builder.deinit();
        break :ir builder.build() catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.NotImplemented => return agent.throwException(.internal_error, "IR generation failed", .{}),
        };
    };
    defer ir.deinit(gpa);

    if (agent.options.debug.print_ir) {
        const stdout = agent.platform.stdout;
        const tty_config = agent.platform.tty_config;
        ir.print(stdout, tty_config) catch {};
        stdout.writeByte('\n') catch {};
        stdout.flush() catch {};
    }

    var bc = bc: {
        var builder: Bytecode.Builder = try .init(gpa, &ir);
        defer builder.deinit();
        break :bc try builder.build();
    };
    errdefer bc.deinit(gpa);

    if (agent.options.debug.print_bytecode) {
        const stdout = agent.platform.stdout;
        const tty_config = agent.platform.tty_config;
        bc.print(stdout, tty_config) catch {};
        stdout.writeByte('\n') catch {};
        stdout.flush() catch {};
    }

    return bc;
}

pub fn compileAndRun(
    agent: *Agent,
    ast_node: union(enum) {
        script: *const ast.Script,
        module: *const ast.Module,
        eval: struct {
            script: *const ast.Script,
            strict: bool,
        },
    },
    name: []const u8,
) Agent.Error!?Value {
    var bc = try compile(agent, name, switch (ast_node) {
        .script => |s| .{ .script = s },
        .module => |m| .{ .module = m },
        .eval => |e| .{ .eval = .{
            .script = e.script,
            .strict = e.strict,
        } },
    });
    defer bc.deinit(agent.gc_allocator);

    var vm: Vm = try .init(agent, &bc);
    defer vm.deinit();
    const result = try vm.run(.{});
    return switch (result) {
        .@"return" => |value| value,
        .yield => unreachable,
    };
}

const ExpectedResult = union(enum) {
    value: ?Value,
    promise: struct { fulfilled: Value },
    exception,
    ignore,
};

fn testInterpreter(
    gpa: std.mem.Allocator,
    source: []const u8,
    expected_result: ExpectedResult,
    expected_ir: ?[]const u8,
    expected_bc: ?[]const u8,
) !void {
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const script = try Parser.parse(language.ast.Script, arena, source, .{});

    var ir = ir: {
        var builder: Ir.Builder = .init(gpa, "test", .{ .script = &script });
        defer builder.deinit();
        break :ir try builder.build();
    };
    defer ir.deinit(gpa);

    var bc = bc: {
        var builder: Bytecode.Builder = try .init(gpa, &ir);
        defer builder.deinit();
        break :bc try builder.build();
    };
    defer bc.deinit(gpa);

    const platform: Agent.Platform = .default();
    defer platform.deinit();
    var agent: Agent = try .init(&platform, .{
        // Ensure generator code paths use new interpreter
        .new_interpreter = true,
    });
    defer agent.deinit();

    try Realm.initializeHostDefinedRealm(&agent, .{});

    const realm = agent.currentRealm();

    const script_record = try agent.gc_allocator.create(Script);
    script_record.* = .{
        .realm = realm,
        .ecmascript_code = script,
        .loaded_modules = .empty,
        .host_defined = .null_pointer,
    };

    const test_context = try agent.gc_allocator.create(execution.ExecutionContext);
    test_context.* = .{
        .origin = .script,
        .realm = realm,
        .script_or_module = .{ .script = script_record },
        .ecmascript_code = .{
            .variable_environment = .{ .global_environment = realm.global_env },
            .lexical_environment = .{ .global_environment = realm.global_env },
            .private_environment = null,
        },
    };
    try agent.execution_context_stack.append(agent.gc_allocator, test_context);
    defer _ = agent.execution_context_stack.pop().?;

    try Script.globalDeclarationInstantiation(&agent, script, realm.global_env);

    var vm: Vm = try .init(&agent, &bc);
    defer vm.deinit();

    // Agent.Error!RunResult -> Agent.Error!?Value
    const unwrapped = if (vm.run(.{})) |result| switch (result) {
        .@"return" => |value| value,
        .yield => unreachable,
    } else |err| err;

    if (unwrapped) |result| switch (expected_result) {
        .value => |expected| if (expected != null) {
            if (result == null) return error.TestExpectedEqual;
            if (!expected.?.isStrictlyEqual(result.?)) return error.TestExpectedEqual;
        } else if (result != null) return error.TestExpectedEqual,
        .promise => |expected| {
            agent.drainJobQueue();
            const promise = (result orelse return error.TestExpectedEqual).asObject().as(builtins.Promise);
            try std.testing.expectEqual(.fulfilled, promise.fields.promise_state);
            if (!expected.fulfilled.isStrictlyEqual(promise.fields.promise_result)) return error.TestExpectedEqual;
        },
        .exception => return error.TestExpectedException,
        .ignore => {},
    } else |err| switch (expected_result) {
        .exception => try std.testing.expectEqual(error.ExceptionThrown, err),
        else => return err,
    }

    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    if (expected_ir) |expected| {
        try ir.print(&aw.writer, .no_color);
        try std.testing.expectEqualStrings(expected, aw.written());
        aw.clearRetainingCapacity();
    }

    if (expected_bc) |expected| {
        try bc.print(&aw.writer, .no_color);
        try std.testing.expectEqualStrings(expected, aw.written());
        aw.clearRetainingCapacity();
    }
}

test {
    // Empty script
    try testInterpreter(
        std.testing.allocator,
        \\
    ,
        .{ .value = null },
        \\IR (test)
        \\  %0: [  0..0  ]                    return none
        \\
    ,
        \\Bytecode (test)
        \\   0: 90 ff                         return
        \\
        ,
    );

    // Simple expression statement
    try testInterpreter(std.testing.allocator,
        \\42;
        \\
    , .{ .value = Value.from(42) },
        \\IR (test)
        \\  %0: [  0..1  ]                    number 42
        \\  %1: [  1..1  ]                    return %0
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 2a 00 00 00             load_number_i32 r0, 42
        \\   6: 90 00                         return r0
        \\
    );

    // Dead code elimination
    try testInterpreter(std.testing.allocator,
        \\1;
        \\while (true) {
        \\  2;
        \\  break;
        \\  3;
        \\}
        \\4;
        \\
    , .{ .value = Value.from(4) },
        \\IR (test)
        \\  %0: [  0..0  ] dead               one
        \\  %1: [  1..2  ]                    undefined
        \\  %2: [  2..2  ]                    br %3, %1
        \\  %3: [  3..12 ]                    label
        \\  %4: [  4..5  ]                    true
        \\  %5: [  5..5  ]                    br_cond %4, %6, %11
        \\  %6: [  6..6  ]                    label
        \\  %7: [  7..8  ]                    number 2
        \\  %8: [  8..8  ]                    br %13, %7
        \\  %9: [  9..10 ] dead               number 3
        \\ %10: [ 10..10 ] dead               br %3, %9
        \\ %11: [ 11..11 ]                    label
        \\ %12: [ 12..12 ]                    br %13, %3
        \\ %13: [ 13..13 ]                    label
        \\ %14: [ 14..15 ]                    number 4
        \\ %15: [ 15..15 ]                    return %14
        \\
    , null);

    // Constant folding and propagation
    try testInterpreter(std.testing.allocator,
        \\if (null ?? "foo") {
        \\  do {
        \\    1 + 2 + 3;
        \\  } while (0);
        \\}
        \\
    , .{ .value = Value.from(6) },
        \\IR (test)
        \\  %0: [  0..8  ]                    label
        \\  %1: [  1..1  ] dead               undefined
        \\  %2: [  2..3  ]                    number 6
        \\  %3: [  3..3  ]                    br %4, %2
        \\  %4: [  4..10 ]                    label
        \\  %5: [  5..6  ]                    zero
        \\  %6: [  6..6  ]                    br_cond %5, %7, %9
        \\  %7: [  7..7  ]                    label
        \\  %8: [  8..8  ]                    br %0, %4
        \\  %9: [  9..9  ]                    label
        \\ %10: [ 10..10 ]                    br %11, %4
        \\ %11: [ 11..12 ]                    label
        \\ %12: [ 12..12 ]                    return %11
        \\
    , null);

    // Array literal
    try testInterpreter(std.testing.allocator,
        \\[1, , [2, 3], {x: 4}];
        \\
    , .ignore,
        \\IR (test)
        \\  %0: [  0..7  ]                    one
        \\  %1: [  1..3  ]                    number 2
        \\  %2: [  2..3  ]                    number 3
        \\  %3: [  3..7  ]                    array [%1, %2]
        \\  %4: [  4..6  ]                    string @0 ("x")
        \\  %5: [  5..6  ]                    number 4
        \\  %6: [  6..7  ]                    object {%4: %5}
        \\  %7: [  7..8  ]                    array [%0, none, %3, %6]
        \\  %8: [  8..8  ]                    return %7
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 01 00 00 00             load_number_i32 r0, 1
        \\   6: 08 01 02 00 00 00             load_number_i32 r1, 2
        \\  12: 08 02 03 00 00 00             load_number_i32 r2, 3
        \\  18: 0d 03 02 00 00 00             array_create r3, 2
        \\  24: 10 03 01 00 00 00 00          array_set r3, r1, 0
        \\  31: 10 03 02 01 00 00 00          array_set r3, r2, 1
        \\  38: 0a 01 00 00 00 00             load_string r1, @0 ("x")
        \\  44: 08 02 04 00 00 00             load_number_i32 r2, 4
        \\  50: 12 04                         object_create r4
        \\  52: 13 04 00 00 00 00 02          object_set r4, @0 ("x"), r2
        \\  59: 0d 01 04 00 00 00             array_create r1, 4
        \\  65: 10 01 00 00 00 00 00          array_set r1, r0, 0
        \\  72: 10 01 03 02 00 00 00          array_set r1, r3, 2
        \\  79: 10 01 04 03 00 00 00          array_set r1, r4, 3
        \\  86: 90 01                         return r1
        \\
    );

    // Object literal
    try testInterpreter(std.testing.allocator,
        \\var a = 1;
        \\var o = {c: 2};
        \\({a, [2]: "two", b: 3, ...o});
        \\
    , .ignore,
        \\IR (test)
        \\  %0: [  0..1  ]                    one
        \\  %1: [  1..1  ]                    set_binding @0 ("a"), %0
        \\  %2: [  2..4  ]                    string @1 ("c")
        \\  %3: [  3..4  ]                    number 2
        \\  %4: [  4..5  ]                    object {%2: %3}
        \\  %5: [  5..5  ]                    set_binding @2 ("o"), %4
        \\  %6: [  6..14 ]                    string @0 ("a")
        \\  %7: [  7..14 ]                    get_binding @0 ("a")
        \\  %8: [  8..14 ]                    number 2
        \\  %9: [  9..14 ]                    string @3 ("two")
        \\ %10: [ 10..14 ]                    string @4 ("b")
        \\ %11: [ 11..14 ]                    number 3
        \\ %12: [ 12..13 ]                    get_binding @2 ("o")
        \\ %13: [ 13..14 ]                    spread %12
        \\ %14: [ 14..15 ]                    object {%6: %7, %8: %9, %10: %11, none: %13}
        \\ %15: [ 15..15 ]                    return %14
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 01 00 00 00             load_number_i32 r0, 1
        \\   6: 46 00 00 00 00 00             set_binding @0 ("a"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 0a 00 01 00 00 00             load_string r0, @1 ("c")
        \\  21: 08 01 02 00 00 00             load_number_i32 r1, 2
        \\  27: 12 02                         object_create r2
        \\  29: 13 02 01 00 00 00 01          object_set r2, @1 ("c"), r1
        \\  36: 46 02 00 00 00 02             set_binding @2 ("o"), r2
        \\  42: 0c 00 02                      move r0, r2
        \\  45: 0a 00 00 00 00 00             load_string r0, @0 ("a")
        \\  51: 42 01 00 00 00 00             get_binding r1, @0 ("a")
        \\  57: 08 02 02 00 00 00             load_number_i32 r2, 2
        \\  63: 0a 03 03 00 00 00             load_string r3, @3 ("two")
        \\  69: 0a 04 04 00 00 00             load_string r4, @4 ("b")
        \\  75: 08 05 03 00 00 00             load_number_i32 r5, 3
        \\  81: 42 06 02 00 00 00             get_binding r6, @2 ("o")
        \\  87: 0c 07 06                      move r7, r6
        \\  90: 12 06                         object_create r6
        \\  92: 13 06 00 00 00 00 01          object_set r6, @0 ("a"), r1
        \\  99: 14 06 02 03                   object_set_computed r6, r2, r3
        \\ 103: 13 06 04 00 00 00 05          object_set r6, @4 ("b"), r5
        \\ 110: 19 06 07                      object_spread r6, r7
        \\ 113: 90 06                         return r6
        \\
    );

    // Regular expression literal
    try testInterpreter(std.testing.allocator,
        \\/abc/gi;
        \\
    , .ignore,
        \\IR (test)
        \\  %0: [  0..1  ]                    reg_exp @0 ("abc"), @1 ("gi")
        \\  %1: [  1..1  ]                    return %0
        \\
    ,
        \\Bytecode (test)
        \\   0: 1a 00 00 00 00 00 01 00 00 00 reg_exp_create r0, @0 ("abc"), @1 ("gi")
        \\  10: 90 00                         return r0
        \\
    );

    // Variable statements
    try testInterpreter(std.testing.allocator,
        \\var x = 10, y = 20;
        \\x + y;
        \\
    , .{ .value = Value.from(30) },
        \\IR (test)
        \\  %0: [  0..1  ]                    number 10
        \\  %1: [  1..1  ]                    set_binding @0 ("x"), %0
        \\  %2: [  2..3  ]                    number 20
        \\  %3: [  3..3  ]                    set_binding @1 ("y"), %2
        \\  %4: [  4..6  ]                    get_binding @0 ("x")
        \\  %5: [  5..6  ]                    get_binding @1 ("y")
        \\  %6: [  6..7  ]                    add %4, %5
        \\  %7: [  7..7  ]                    return %6
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 0a 00 00 00             load_number_i32 r0, 10
        \\   6: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 08 00 14 00 00 00             load_number_i32 r0, 20
        \\  21: 46 01 00 00 00 00             set_binding @1 ("y"), r0
        \\  27: 0c 01 00                      move r1, r0
        \\  30: 42 00 00 00 00 00             get_binding r0, @0 ("x")
        \\  36: 42 01 01 00 00 00             get_binding r1, @1 ("y")
        \\  42: 25 02 00 01                   add r2, r0, r1
        \\  46: 90 02                         return r2
        \\
    );

    // Lexical declarations
    try testInterpreter(std.testing.allocator,
        \\let a = 1;
        \\{
        \\  let a = 2;
        \\}
        \\a;
    , .{ .value = Value.from(1) },
        \\IR (test)
        \\  %0: [  0..1  ]                    one
        \\  %1: [  1..1  ]                    initialize_binding @0 ("a"), %0
        \\  %2: [  2..2  ]                    push_scope
        \\  %3: [  3..3  ]                    create_mutable_binding @0 ("a")
        \\  %4: [  4..5  ]                    number 2
        \\  %5: [  5..5  ]                    initialize_binding @0 ("a"), %4
        \\  %6: [  6..6  ]                    pop_scope
        \\  %7: [  7..8  ]                    get_binding @0 ("a")
        \\  %8: [  8..8  ]                    return %7
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 01 00 00 00             load_number_i32 r0, 1
        \\   6: 41 00 00 00 00 00             initialize_binding @0 ("a"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 3b                            push_scope
        \\  16: 3f 00 00 00 00                create_mutable_binding @0 ("a")
        \\  21: 08 00 02 00 00 00             load_number_i32 r0, 2
        \\  27: 41 00 00 00 00 00             initialize_binding @0 ("a"), r0
        \\  33: 0c 01 00                      move r1, r0
        \\  36: 3e                            pop_scope
        \\  37: 42 00 00 00 00 00             get_binding r0, @0 ("a")
        \\  43: 90 00                         return r0
        \\
    );

    // Increment/decrement operators
    try testInterpreter(std.testing.allocator,
        \\x = 5;
        \\++x;
        \\
    , .{ .value = Value.from(6) },
        \\IR (test)
        \\  %0: [  0..1  ]                    number 5
        \\  %1: [  1..1  ]                    set_binding @0 ("x"), %0
        \\  %2: [  2..3  ]                    update_binding @0 ("x"), increment_prefix
        \\  %3: [  3..3  ]                    return %2
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 05 00 00 00             load_number_i32 r0, 5
        \\   6: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 4e 00 00 00 00 00             increment_binding_prefix r0, @0 ("x")
        \\  21: 90 00                         return r0
        \\
    );
    try testInterpreter(std.testing.allocator, "x = 5; x++;", .{ .value = Value.from(5) }, null, null);
    try testInterpreter(std.testing.allocator, "x = 10; --x;", .{ .value = Value.from(9) }, null, null);
    try testInterpreter(std.testing.allocator, "x = 10; x--;", .{ .value = Value.from(10) }, null, null);

    // Call expressions
    try testInterpreter(
        std.testing.allocator,
        \\Math.random();
        \\Number("1");
        \\JSON.stringify({ foo: "bar" }, null, 2)
        \\Math.max(1, 5, ...[3, 9], 2);
        \\
    ,
        .{ .value = Value.from(9) },
        \\IR (test)
        \\  %0: [  0..2  ]                    get_binding @0 ("Math")
        \\  %1: [  1..2  ]                    get_property %0, @1 ("random")
        \\  %2: [  2..2  ]                    call %1, %0, []
        \\  %3: [  3..5  ]                    get_binding @2 ("Number")
        \\  %4: [  4..5  ]                    string @3 ("1")
        \\  %5: [  5..5  ]                    call %3, none, [%4]
        \\  %6: [  6..13 ]                    get_binding @4 ("JSON")
        \\  %7: [  7..13 ]                    get_property %6, @5 ("stringify")
        \\  %8: [  8..10 ]                    string @6 ("foo")
        \\  %9: [  9..10 ]                    string @7 ("bar")
        \\ %10: [ 10..13 ]                    object {%8: %9}
        \\ %11: [ 11..13 ]                    null
        \\ %12: [ 12..13 ]                    number 2
        \\ %13: [ 13..13 ]                    call %7, %6, [%10, %11, %12]
        \\ %14: [ 14..23 ]                    get_binding @0 ("Math")
        \\ %15: [ 15..23 ]                    get_property %14, @8 ("max")
        \\ %16: [ 16..23 ]                    one
        \\ %17: [ 17..23 ]                    number 5
        \\ %18: [ 18..20 ]                    number 3
        \\ %19: [ 19..20 ]                    number 9
        \\ %20: [ 20..21 ]                    array [%18, %19]
        \\ %21: [ 21..23 ]                    spread %20
        \\ %22: [ 22..23 ]                    number 2
        \\ %23: [ 23..24 ]                    call %15, %14, [%16, %17, %21, %22]
        \\ %24: [ 24..24 ]                    return %23
        \\
    ,
        \\Bytecode (test)
        \\   0: 42 00 00 00 00 00             get_binding r0, @0 ("Math")
        \\   6: 43 01 00 01 00 00 00          get_property r1, r0, @1 ("random")
        \\  13: 7b 02 01 00                   call_property0 r2, r1, r0
        \\  17: 42 00 02 00 00 00             get_binding r0, @2 ("Number")
        \\  23: 0a 01 03 00 00 00             load_string r1, @3 ("1")
        \\  29: 78 02 00 01                   call1 r2, r0, r1
        \\  33: 42 00 04 00 00 00             get_binding r0, @4 ("JSON")
        \\  39: 43 01 00 05 00 00 00          get_property r1, r0, @5 ("stringify")
        \\  46: 0a 02 06 00 00 00             load_string r2, @6 ("foo")
        \\  52: 0a 03 07 00 00 00             load_string r3, @7 ("bar")
        \\  58: 12 04                         object_create r4
        \\  60: 13 04 06 00 00 00 03          object_set r4, @6 ("foo"), r3
        \\  67: 05 02                         load_null r2
        \\  69: 08 03 02 00 00 00             load_number_i32 r3, 2
        \\  75: 0d 1f 00 00 00 00             array_create r31, 0
        \\  81: 0e 1f 04                      array_push r31, r4
        \\  84: 0e 1f 02                      array_push r31, r2
        \\  87: 0e 1f 03                      array_push r31, r3
        \\  90: 7a 05 01 00 1f                call_property r5, r1, r0, r31
        \\  95: 42 00 00 00 00 00             get_binding r0, @0 ("Math")
        \\ 101: 43 01 00 08 00 00 00          get_property r1, r0, @8 ("max")
        \\ 108: 08 02 01 00 00 00             load_number_i32 r2, 1
        \\ 114: 08 03 05 00 00 00             load_number_i32 r3, 5
        \\ 120: 08 04 03 00 00 00             load_number_i32 r4, 3
        \\ 126: 08 05 09 00 00 00             load_number_i32 r5, 9
        \\ 132: 0d 06 02 00 00 00             array_create r6, 2
        \\ 138: 10 06 04 00 00 00 00          array_set r6, r4, 0
        \\ 145: 10 06 05 01 00 00 00          array_set r6, r5, 1
        \\ 152: 0c 04 06                      move r4, r6
        \\ 155: 08 05 02 00 00 00             load_number_i32 r5, 2
        \\ 161: 0d 1f 00 00 00 00             array_create r31, 0
        \\ 167: 0e 1f 02                      array_push r31, r2
        \\ 170: 0e 1f 03                      array_push r31, r3
        \\ 173: 11 1f 06                      array_spread r31, r6
        \\ 176: 0e 1f 05                      array_push r31, r5
        \\ 179: 7a 06 01 00 1f                call_property r6, r1, r0, r31
        \\ 184: 90 06                         return r6
        \\
        ,
    );

    // Destructuring assignment
    try testInterpreter(std.testing.allocator,
        \\const {a, ...rest} = {a: 1, b: 2, c: 3};
        \\const [x, y] = [10, 20];
        \\const {d: {e}} = {d: {e: 5}};
        \\const {f = 99} = {f: undefined};
        \\rest.b + x + e + f;
        \\
    , .{ .value = Value.from(116) },
        \\IR (test)
        \\  %0: [  0..6  ]                    string @0 ("a")
        \\  %1: [  1..6  ]                    one
        \\  %2: [  2..6  ]                    string @1 ("b")
        \\  %3: [  3..6  ]                    number 2
        \\  %4: [  4..6  ]                    string @2 ("c")
        \\  %5: [  5..6  ]                    number 3
        \\  %6: [  6..10 ]                    object {%0: %1, %2: %3, %4: %5}
        \\  %7: [  7..8  ]                    get_property %6, @0 ("a")
        \\  %8: [  8..8  ]                    initialize_binding @0 ("a"), %7
        \\  %9: [  9..10 ]                    string @0 ("a")
        \\ %10: [ 10..11 ]                    copy_data_properties %6, [%9]
        \\ %11: [ 11..11 ]                    initialize_binding @3 ("rest"), %10
        \\ %12: [ 12..14 ]                    number 10
        \\ %13: [ 13..14 ]                    number 20
        \\ %14: [ 14..15 ]                    array [%12, %13]
        \\ %15: [ 15..18 ]                    get_iterator %14
        \\ %16: [ 16..17 ]                    iterator_step_value %15
        \\ %17: [ 17..17 ]                    initialize_binding @4 ("x"), %16
        \\ %18: [ 18..19 ]                    iterator_step_value %15
        \\ %19: [ 19..19 ]                    initialize_binding @5 ("y"), %18
        \\ %20: [ 20..24 ]                    string @6 ("d")
        \\ %21: [ 21..23 ]                    string @7 ("e")
        \\ %22: [ 22..23 ]                    number 5
        \\ %23: [ 23..24 ]                    object {%21: %22}
        \\ %24: [ 24..26 ]                    object {%20: %23}
        \\ %25: [ 25..26 ]                    string @6 ("d")
        \\ %26: [ 26..27 ]                    get_property_computed %24, %25
        \\ %27: [ 27..28 ]                    get_property %26, @7 ("e")
        \\ %28: [ 28..28 ]                    initialize_binding @7 ("e"), %27
        \\ %29: [ 29..31 ]                    string @8 ("f")
        \\ %30: [ 30..31 ]                    get_binding @9 ("undefined")
        \\ %31: [ 31..32 ]                    object {%29: %30}
        \\ %32: [ 32..40 ]                    get_property %31, @8 ("f")
        \\ %33: [ 33..34 ]                    undefined
        \\ %34: [ 34..35 ]                    eq_strict %32, %33
        \\ %35: [ 35..35 ]                    br_cond %34, %36, %39
        \\ %36: [ 36..36 ]                    label
        \\ %37: [ 37..38 ]                    number 99
        \\ %38: [ 38..38 ]                    br %41, %37
        \\ %39: [ 39..39 ]                    label
        \\ %40: [ 40..40 ]                    br %41, %32
        \\ %41: [ 41..42 ]                    label
        \\ %42: [ 42..42 ]                    initialize_binding @8 ("f"), %41
        \\ %43: [ 43..44 ]                    get_binding @3 ("rest")
        \\ %44: [ 44..46 ]                    get_property %43, @1 ("b")
        \\ %45: [ 45..46 ]                    get_binding @4 ("x")
        \\ %46: [ 46..48 ]                    add %44, %45
        \\ %47: [ 47..48 ]                    get_binding @7 ("e")
        \\ %48: [ 48..50 ]                    add %46, %47
        \\ %49: [ 49..50 ]                    get_binding @8 ("f")
        \\ %50: [ 50..51 ]                    add %48, %49
        \\ %51: [ 51..51 ]                    return %50
        \\
    ,
        \\Bytecode (test)
        \\   0: 0a 00 00 00 00 00             load_string r0, @0 ("a")
        \\   6: 08 01 01 00 00 00             load_number_i32 r1, 1
        \\  12: 0a 02 01 00 00 00             load_string r2, @1 ("b")
        \\  18: 08 03 02 00 00 00             load_number_i32 r3, 2
        \\  24: 0a 04 02 00 00 00             load_string r4, @2 ("c")
        \\  30: 08 05 03 00 00 00             load_number_i32 r5, 3
        \\  36: 12 06                         object_create r6
        \\  38: 13 06 00 00 00 00 01          object_set r6, @0 ("a"), r1
        \\  45: 13 06 01 00 00 00 03          object_set r6, @1 ("b"), r3
        \\  52: 13 06 02 00 00 00 05          object_set r6, @2 ("c"), r5
        \\  59: 43 00 06 00 00 00 00          get_property r0, r6, @0 ("a")
        \\  66: 41 00 00 00 00 00             initialize_binding @0 ("a"), r0
        \\  72: 0c 01 00                      move r1, r0
        \\  75: 0a 00 00 00 00 00             load_string r0, @0 ("a")
        \\  81: 0d 1f 00 00 00 00             array_create r31, 0
        \\  87: 0e 1f 00                      array_push r31, r0
        \\  90: 75 01 06 1f                   copy_data_properties r1, r6, r31
        \\  94: 41 03 00 00 00 01             initialize_binding @3 ("rest"), r1
        \\ 100: 0c 00 01                      move r0, r1
        \\ 103: 08 00 0a 00 00 00             load_number_i32 r0, 10
        \\ 109: 08 01 14 00 00 00             load_number_i32 r1, 20
        \\ 115: 0d 02 02 00 00 00             array_create r2, 2
        \\ 121: 10 02 00 00 00 00 00          array_set r2, r0, 0
        \\ 128: 10 02 01 01 00 00 00          array_set r2, r1, 1
        \\ 135: 85 00 02                      get_iterator r0, r2
        \\ 138: 89 01 00                      iterator_step_value r1, r0
        \\ 141: 41 04 00 00 00 01             initialize_binding @4 ("x"), r1
        \\ 147: 0c 02 01                      move r2, r1
        \\ 150: 89 01 00                      iterator_step_value r1, r0
        \\ 153: 41 05 00 00 00 01             initialize_binding @5 ("y"), r1
        \\ 159: 0c 00 01                      move r0, r1
        \\ 162: 0a 00 06 00 00 00             load_string r0, @6 ("d")
        \\ 168: 0a 01 07 00 00 00             load_string r1, @7 ("e")
        \\ 174: 08 02 05 00 00 00             load_number_i32 r2, 5
        \\ 180: 12 03                         object_create r3
        \\ 182: 13 03 07 00 00 00 02          object_set r3, @7 ("e"), r2
        \\ 189: 12 01                         object_create r1
        \\ 191: 13 01 06 00 00 00 03          object_set r1, @6 ("d"), r3
        \\ 198: 0a 00 06 00 00 00             load_string r0, @6 ("d")
        \\ 204: 44 02 01 00                   get_property_computed r2, r1, r0
        \\ 208: 43 00 02 07 00 00 00          get_property r0, r2, @7 ("e")
        \\ 215: 41 07 00 00 00 00             initialize_binding @7 ("e"), r0
        \\ 221: 0c 01 00                      move r1, r0
        \\ 224: 0a 00 08 00 00 00             load_string r0, @8 ("f")
        \\ 230: 42 01 09 00 00 00             get_binding r1, @9 ("undefined")
        \\ 236: 12 02                         object_create r2
        \\ 238: 13 02 08 00 00 00 01          object_set r2, @8 ("f"), r1
        \\ 245: 43 00 02 08 00 00 00          get_property r0, r2, @8 ("f")
        \\ 252: 04 01                         load_undefined r1
        \\ 254: 39 02 00 01                   eq_strict r2, r0, r1
        \\ 258: 01 02 05 00 00 00             jump_if_true r2, 5
        \\ 264: 00 0e 00 00 00                jump 14
        \\ 269: 08 01 63 00 00 00             load_number_i32 r1, 99
        \\ 275: 0c 00 01                      move r0, r1
        \\ 278: 00 00 00 00 00                jump 0
        \\ 283: 41 08 00 00 00 00             initialize_binding @8 ("f"), r0
        \\ 289: 0c 01 00                      move r1, r0
        \\ 292: 42 00 03 00 00 00             get_binding r0, @3 ("rest")
        \\ 298: 43 01 00 01 00 00 00          get_property r1, r0, @1 ("b")
        \\ 305: 42 00 04 00 00 00             get_binding r0, @4 ("x")
        \\ 311: 25 02 01 00                   add r2, r1, r0
        \\ 315: 42 00 07 00 00 00             get_binding r0, @7 ("e")
        \\ 321: 25 01 02 00                   add r1, r2, r0
        \\ 325: 42 00 08 00 00 00             get_binding r0, @8 ("f")
        \\ 331: 25 02 01 00                   add r2, r1, r0
        \\ 335: 90 02                         return r2
        \\
    );

    // Loops
    try testInterpreter(std.testing.allocator,
        \\var x = 0;
        \\while(x < 3) { x = x + 1; }
        \\do { x = x + 1; } while(x < 5);
        \\for(var i = 0; i < 3; i++) { x = x + i; }
        \\x;
        \\
    , .{ .value = Value.from(8) },
        \\IR (test)
        \\  %0: [  0..1  ]                    zero
        \\  %1: [  1..1  ]                    set_binding @0 ("x"), %0
        \\  %2: [  2..3  ]                    undefined
        \\  %3: [  3..3  ]                    br %4, %2
        \\  %4: [  4..16 ]                    label
        \\  %5: [  5..7  ]                    get_binding @0 ("x")
        \\  %6: [  6..7  ]                    number 3
        \\  %7: [  7..8  ]                    lt %5, %6
        \\  %8: [  8..8  ]                    br_cond %7, %9, %15
        \\  %9: [  9..9  ]                    label
        \\ %10: [ 10..12 ]                    get_binding @0 ("x")
        \\ %11: [ 11..12 ]                    one
        \\ %12: [ 12..13 ]                    add %10, %11
        \\ %13: [ 13..14 ]                    set_binding @0 ("x"), %12
        \\ %14: [ 14..14 ]                    br %4, %13
        \\ %15: [ 15..15 ]                    label
        \\ %16: [ 16..16 ]                    br %17, %4
        \\ %17: [ 17..17 ]                    label
        \\ %18: [ 18..31 ]                    label
        \\ %19: [ 19..19 ] dead               undefined
        \\ %20: [ 20..22 ]                    get_binding @0 ("x")
        \\ %21: [ 21..22 ]                    one
        \\ %22: [ 22..23 ]                    add %20, %21
        \\ %23: [ 23..24 ]                    set_binding @0 ("x"), %22
        \\ %24: [ 24..24 ]                    br %25, %23
        \\ %25: [ 25..33 ]                    label
        \\ %26: [ 26..28 ]                    get_binding @0 ("x")
        \\ %27: [ 27..28 ]                    number 5
        \\ %28: [ 28..29 ]                    lt %26, %27
        \\ %29: [ 29..29 ]                    br_cond %28, %30, %32
        \\ %30: [ 30..30 ]                    label
        \\ %31: [ 31..31 ]                    br %18, %25
        \\ %32: [ 32..32 ]                    label
        \\ %33: [ 33..33 ]                    br %34, %25
        \\ %34: [ 34..34 ]                    label
        \\ %35: [ 35..36 ]                    zero
        \\ %36: [ 36..36 ]                    set_binding @1 ("i"), %35
        \\ %37: [ 37..38 ]                    undefined
        \\ %38: [ 38..38 ]                    br %39, %37
        \\ %39: [ 39..54 ]                    label
        \\ %40: [ 40..42 ]                    get_binding @1 ("i")
        \\ %41: [ 41..42 ]                    number 3
        \\ %42: [ 42..43 ]                    lt %40, %41
        \\ %43: [ 43..43 ]                    br_cond %42, %44, %53
        \\ %44: [ 44..44 ]                    label
        \\ %45: [ 45..47 ]                    get_binding @0 ("x")
        \\ %46: [ 46..47 ]                    get_binding @1 ("i")
        \\ %47: [ 47..48 ]                    add %45, %46
        \\ %48: [ 48..52 ]                    set_binding @0 ("x"), %47
        \\ %49: [ 49..49 ]                    br %50, %48
        \\ %50: [ 50..50 ]                    label
        \\ %51: [ 51..51 ]                    update_binding @1 ("i"), increment_postfix
        \\ %52: [ 52..52 ]                    br %39, %48
        \\ %53: [ 53..53 ]                    label
        \\ %54: [ 54..54 ]                    br %55, %39
        \\ %55: [ 55..55 ]                    label
        \\ %56: [ 56..57 ]                    get_binding @0 ("x")
        \\ %57: [ 57..57 ]                    return %56
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 00 00 00 00             load_number_i32 r0, 0
        \\   6: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 04 00                         load_undefined r0
        \\  17: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\  23: 08 02 03 00 00 00             load_number_i32 r2, 3
        \\  29: 31 03 01 02                   lt r3, r1, r2
        \\  33: 01 03 05 00 00 00             jump_if_true r3, 5
        \\  39: 00 21 00 00 00                jump 33
        \\  44: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\  50: 08 02 01 00 00 00             load_number_i32 r2, 1
        \\  56: 25 03 01 02                   add r3, r1, r2
        \\  60: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\  66: 0c 01 03                      move r1, r3
        \\  69: 0c 00 01                      move r0, r1
        \\  72: 00 c4 ff ff ff                jump -60
        \\  77: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\  83: 08 02 01 00 00 00             load_number_i32 r2, 1
        \\  89: 25 03 01 02                   add r3, r1, r2
        \\  93: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\  99: 0c 01 03                      move r1, r3
        \\ 102: 42 02 00 00 00 00             get_binding r2, @0 ("x")
        \\ 108: 08 03 05 00 00 00             load_number_i32 r3, 5
        \\ 114: 31 04 02 03                   lt r4, r2, r3
        \\ 118: 01 04 05 00 00 00             jump_if_true r4, 5
        \\ 124: 00 08 00 00 00                jump 8
        \\ 129: 0c 00 01                      move r0, r1
        \\ 132: 00 c4 ff ff ff                jump -60
        \\ 137: 0c 00 01                      move r0, r1
        \\ 140: 08 00 00 00 00 00             load_number_i32 r0, 0
        \\ 146: 46 01 00 00 00 00             set_binding @1 ("i"), r0
        \\ 152: 0c 01 00                      move r1, r0
        \\ 155: 04 00                         load_undefined r0
        \\ 157: 42 01 01 00 00 00             get_binding r1, @1 ("i")
        \\ 163: 08 02 03 00 00 00             load_number_i32 r2, 3
        \\ 169: 31 03 01 02                   lt r3, r1, r2
        \\ 173: 01 03 05 00 00 00             jump_if_true r3, 5
        \\ 179: 00 2a 00 00 00                jump 42
        \\ 184: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\ 190: 42 02 01 00 00 00             get_binding r2, @1 ("i")
        \\ 196: 25 03 01 02                   add r3, r1, r2
        \\ 200: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\ 206: 0c 01 03                      move r1, r3
        \\ 209: 0c 02 01                      move r2, r1
        \\ 212: 50 02 01 00 00 00             increment_binding_postfix r2, @1 ("i")
        \\ 218: 0c 00 01                      move r0, r1
        \\ 221: 00 bb ff ff ff                jump -69
        \\ 226: 42 00 00 00 00 00             get_binding r0, @0 ("x")
        \\ 232: 90 00                         return r0
        \\
    );

    // Break and continue
    try testInterpreter(std.testing.allocator,
        \\var x = 0;
        \\while(true) { x = x + 1; if (x >= 5) break; }
        \\for(;;) { x = x + 1; if (x >= 10) break; }
        \\while(x < 20) { x = x + 1; if (x % 2 === 0) continue; x = x + 10; }
        \\foo: { x = x + 100; break foo; x = x + 200; }
        \\x;
        \\
    , .{ .value = Value.from(121) },
        \\IR (test)
        \\  %0: [  0..1  ]                    zero
        \\  %1: [  1..1  ]                    set_binding @0 ("x"), %0
        \\  %2: [  2..3  ]                    undefined
        \\  %3: [  3..3  ]                    br %4, %2
        \\  %4: [  4..24 ]                    label
        \\  %5: [  5..6  ]                    true
        \\  %6: [  6..6  ]                    br_cond %5, %7, %23
        \\  %7: [  7..7  ]                    label
        \\  %8: [  8..10 ]                    get_binding @0 ("x")
        \\  %9: [  9..10 ]                    one
        \\ %10: [ 10..11 ]                    add %8, %9
        \\ %11: [ 11..17 ]                    set_binding @0 ("x"), %10
        \\ %12: [ 12..14 ]                    get_binding @0 ("x")
        \\ %13: [ 13..14 ]                    number 5
        \\ %14: [ 14..15 ]                    gt_eq %12, %13
        \\ %15: [ 15..15 ]                    br_cond %14, %16, %19
        \\ %16: [ 16..16 ]                    label
        \\ %17: [ 17..17 ]                    br %25, %11
        \\ %18: [ 18..18 ] dead               br %21, none
        \\ %19: [ 19..19 ]                    label
        \\ %20: [ 20..20 ]                    br %21, none
        \\ %21: [ 21..22 ]                    label
        \\ %22: [ 22..22 ]                    br %4, %21
        \\ %23: [ 23..23 ]                    label
        \\ %24: [ 24..24 ]                    br %25, %4
        \\ %25: [ 25..25 ]                    label
        \\ %26: [ 26..27 ]                    undefined
        \\ %27: [ 27..27 ]                    br %28, %26
        \\ %28: [ 28..50 ]                    label
        \\ %29: [ 29..30 ]                    true
        \\ %30: [ 30..30 ]                    br_cond %29, %31, %49
        \\ %31: [ 31..31 ]                    label
        \\ %32: [ 32..34 ]                    get_binding @0 ("x")
        \\ %33: [ 33..34 ]                    one
        \\ %34: [ 34..35 ]                    add %32, %33
        \\ %35: [ 35..41 ]                    set_binding @0 ("x"), %34
        \\ %36: [ 36..38 ]                    get_binding @0 ("x")
        \\ %37: [ 37..38 ]                    number 10
        \\ %38: [ 38..39 ]                    gt_eq %36, %37
        \\ %39: [ 39..39 ]                    br_cond %38, %40, %43
        \\ %40: [ 40..40 ]                    label
        \\ %41: [ 41..41 ]                    br %51, %35
        \\ %42: [ 42..42 ] dead               br %45, none
        \\ %43: [ 43..43 ]                    label
        \\ %44: [ 44..44 ]                    br %45, none
        \\ %45: [ 45..48 ]                    label
        \\ %46: [ 46..46 ]                    br %47, %45
        \\ %47: [ 47..47 ]                    label
        \\ %48: [ 48..48 ]                    br %28, %45
        \\ %49: [ 49..49 ]                    label
        \\ %50: [ 50..50 ]                    br %51, %28
        \\ %51: [ 51..51 ]                    label
        \\ %52: [ 52..53 ]                    undefined
        \\ %53: [ 53..53 ]                    br %54, %52
        \\ %54: [ 54..82 ]                    label
        \\ %55: [ 55..57 ]                    get_binding @0 ("x")
        \\ %56: [ 56..57 ]                    number 20
        \\ %57: [ 57..58 ]                    lt %55, %56
        \\ %58: [ 58..58 ]                    br_cond %57, %59, %81
        \\ %59: [ 59..59 ]                    label
        \\ %60: [ 60..62 ]                    get_binding @0 ("x")
        \\ %61: [ 61..62 ]                    one
        \\ %62: [ 62..63 ]                    add %60, %61
        \\ %63: [ 63..71 ]                    set_binding @0 ("x"), %62
        \\ %64: [ 64..66 ]                    get_binding @0 ("x")
        \\ %65: [ 65..66 ]                    number 2
        \\ %66: [ 66..68 ]                    rem %64, %65
        \\ %67: [ 67..68 ]                    zero
        \\ %68: [ 68..69 ]                    eq_strict %66, %67
        \\ %69: [ 69..69 ]                    br_cond %68, %70, %73
        \\ %70: [ 70..70 ]                    label
        \\ %71: [ 71..71 ]                    br %54, %63
        \\ %72: [ 72..72 ] dead               br %75, none
        \\ %73: [ 73..73 ]                    label
        \\ %74: [ 74..74 ]                    br %75, none
        \\ %75: [ 75..75 ]                    label
        \\ %76: [ 76..78 ]                    get_binding @0 ("x")
        \\ %77: [ 77..78 ]                    number 10
        \\ %78: [ 78..79 ]                    add %76, %77
        \\ %79: [ 79..80 ]                    set_binding @0 ("x"), %78
        \\ %80: [ 80..80 ]                    br %54, %79
        \\ %81: [ 81..81 ]                    label
        \\ %82: [ 82..82 ]                    br %83, %54
        \\ %83: [ 83..83 ]                    label
        \\ %84: [ 84..84 ] dead               undefined
        \\ %85: [ 85..87 ]                    get_binding @0 ("x")
        \\ %86: [ 86..87 ]                    number 100
        \\ %87: [ 87..88 ]                    add %85, %86
        \\ %88: [ 88..89 ]                    set_binding @0 ("x"), %87
        \\ %89: [ 89..89 ]                    br %95, %88
        \\ %90: [ 90..92 ] dead               get_binding @0 ("x")
        \\ %91: [ 91..92 ] dead               number 200
        \\ %92: [ 92..93 ] dead               add %90, %91
        \\ %93: [ 93..94 ] dead               set_binding @0 ("x"), %92
        \\ %94: [ 94..94 ] dead               br %95, %93
        \\ %95: [ 95..95 ]                    label
        \\ %96: [ 96..97 ]                    get_binding @0 ("x")
        \\ %97: [ 97..97 ]                    return %96
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 00 00 00 00             load_number_i32 r0, 0
        \\   6: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 04 00                         load_undefined r0
        \\  17: 06 01                         load_true r1
        \\  19: 01 01 05 00 00 00             jump_if_true r1, 5
        \\  25: 00 44 00 00 00                jump 68
        \\  30: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\  36: 08 02 01 00 00 00             load_number_i32 r2, 1
        \\  42: 25 03 01 02                   add r3, r1, r2
        \\  46: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\  52: 0c 01 03                      move r1, r3
        \\  55: 42 02 00 00 00 00             get_binding r2, @0 ("x")
        \\  61: 08 03 05 00 00 00             load_number_i32 r3, 5
        \\  67: 34 04 02 03                   gt_eq r4, r2, r3
        \\  71: 01 04 05 00 00 00             jump_if_true r4, 5
        \\  77: 00 08 00 00 00                jump 8
        \\  82: 0c 00 01                      move r0, r1
        \\  85: 00 08 00 00 00                jump 8
        \\  90: 0c 00 01                      move r0, r1
        \\  93: 00 af ff ff ff                jump -81
        \\  98: 04 00                         load_undefined r0
        \\ 100: 06 01                         load_true r1
        \\ 102: 01 01 05 00 00 00             jump_if_true r1, 5
        \\ 108: 00 47 00 00 00                jump 71
        \\ 113: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\ 119: 08 02 01 00 00 00             load_number_i32 r2, 1
        \\ 125: 25 03 01 02                   add r3, r1, r2
        \\ 129: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\ 135: 0c 01 03                      move r1, r3
        \\ 138: 42 02 00 00 00 00             get_binding r2, @0 ("x")
        \\ 144: 08 03 0a 00 00 00             load_number_i32 r3, 10
        \\ 150: 34 04 02 03                   gt_eq r4, r2, r3
        \\ 154: 01 04 05 00 00 00             jump_if_true r4, 5
        \\ 160: 00 08 00 00 00                jump 8
        \\ 165: 0c 00 01                      move r0, r1
        \\ 168: 00 0b 00 00 00                jump 11
        \\ 173: 0c 02 01                      move r2, r1
        \\ 176: 0c 00 01                      move r0, r1
        \\ 179: 00 ac ff ff ff                jump -84
        \\ 184: 04 00                         load_undefined r0
        \\ 186: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\ 192: 08 02 14 00 00 00             load_number_i32 r2, 20
        \\ 198: 31 03 01 02                   lt r3, r1, r2
        \\ 202: 01 03 05 00 00 00             jump_if_true r3, 5
        \\ 208: 00 67 00 00 00                jump 103
        \\ 213: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\ 219: 08 02 01 00 00 00             load_number_i32 r2, 1
        \\ 225: 25 03 01 02                   add r3, r1, r2
        \\ 229: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\ 235: 0c 01 03                      move r1, r3
        \\ 238: 42 02 00 00 00 00             get_binding r2, @0 ("x")
        \\ 244: 08 03 02 00 00 00             load_number_i32 r3, 2
        \\ 250: 29 04 02 03                   rem r4, r2, r3
        \\ 254: 08 02 00 00 00 00             load_number_i32 r2, 0
        \\ 260: 39 03 04 02                   eq_strict r3, r4, r2
        \\ 264: 01 03 05 00 00 00             jump_if_true r3, 5
        \\ 270: 00 08 00 00 00                jump 8
        \\ 275: 0c 00 01                      move r0, r1
        \\ 278: 00 9f ff ff ff                jump -97
        \\ 283: 42 01 00 00 00 00             get_binding r1, @0 ("x")
        \\ 289: 08 02 0a 00 00 00             load_number_i32 r2, 10
        \\ 295: 25 03 01 02                   add r3, r1, r2
        \\ 299: 46 00 00 00 00 03             set_binding @0 ("x"), r3
        \\ 305: 0c 01 03                      move r1, r3
        \\ 308: 0c 00 01                      move r0, r1
        \\ 311: 00 7e ff ff ff                jump -130
        \\ 316: 42 00 00 00 00 00             get_binding r0, @0 ("x")
        \\ 322: 08 01 64 00 00 00             load_number_i32 r1, 100
        \\ 328: 25 02 00 01                   add r2, r0, r1
        \\ 332: 46 00 00 00 00 02             set_binding @0 ("x"), r2
        \\ 338: 0c 00 02                      move r0, r2
        \\ 341: 42 00 00 00 00 00             get_binding r0, @0 ("x")
        \\ 347: 90 00                         return r0
        \\
    );

    // Switch statement
    try testInterpreter(std.testing.allocator,
        \\var x = 0;
        \\switch("b") {
        \\    case "a": x = 1; break;
        \\    case "b": x = 2;
        \\    case "c": x = x + 3; break;
        \\    default: x = 100;
        \\}
        \\x;
        \\
    , .{ .value = Value.from(5) },
        \\IR (test)
        \\  %0: [  0..1  ]                    zero
        \\  %1: [  1..1  ]                    set_binding @0 ("x"), %0
        \\  %2: [  2..15 ]                    string @1 ("b")
        \\  %3: [  3..18 ]                    undefined
        \\  %4: [  4..4  ]                    br %5, %3
        \\  %5: [  5..5  ]                    label
        \\  %6: [  6..7  ]                    string @2 ("a")
        \\  %7: [  7..8  ]                    eq_strict %2, %6
        \\  %8: [  8..8  ]                    br_cond %7, %19, %9
        \\  %9: [  9..9  ]                    label
        \\ %10: [ 10..11 ]                    string @1 ("b")
        \\ %11: [ 11..12 ]                    eq_strict %2, %10
        \\ %12: [ 12..12 ]                    br_cond %11, %23, %13
        \\ %13: [ 13..13 ]                    label
        \\ %14: [ 14..15 ]                    string @3 ("c")
        \\ %15: [ 15..16 ]                    eq_strict %2, %14
        \\ %16: [ 16..16 ]                    br_cond %15, %26, %17
        \\ %17: [ 17..17 ]                    label
        \\ %18: [ 18..18 ]                    br %32, %3
        \\ %19: [ 19..19 ]                    label
        \\ %20: [ 20..21 ]                    one
        \\ %21: [ 21..22 ]                    set_binding @0 ("x"), %20
        \\ %22: [ 22..22 ]                    br %36, %21
        \\ %23: [ 23..23 ]                    label
        \\ %24: [ 24..25 ]                    number 2
        \\ %25: [ 25..25 ]                    set_binding @0 ("x"), %24
        \\ %26: [ 26..26 ]                    label
        \\ %27: [ 27..29 ]                    get_binding @0 ("x")
        \\ %28: [ 28..29 ]                    number 3
        \\ %29: [ 29..30 ]                    add %27, %28
        \\ %30: [ 30..31 ]                    set_binding @0 ("x"), %29
        \\ %31: [ 31..31 ]                    br %36, %30
        \\ %32: [ 32..32 ]                    label
        \\ %33: [ 33..34 ]                    number 100
        \\ %34: [ 34..35 ]                    set_binding @0 ("x"), %33
        \\ %35: [ 35..35 ]                    br %36, %34
        \\ %36: [ 36..36 ]                    label
        \\ %37: [ 37..38 ]                    get_binding @0 ("x")
        \\ %38: [ 38..38 ]                    return %37
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 00 00 00 00             load_number_i32 r0, 0
        \\   6: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 0a 00 01 00 00 00             load_string r0, @1 ("b")
        \\  21: 04 01                         load_undefined r1
        \\  23: 0c 02 01                      move r2, r1
        \\  26: 0a 02 02 00 00 00             load_string r2, @2 ("a")
        \\  32: 39 03 00 02                   eq_strict r3, r0, r2
        \\  36: 01 03 05 00 00 00             jump_if_true r3, 5
        \\  42: 00 17 00 00 00                jump 23
        \\  47: 08 00 01 00 00 00             load_number_i32 r0, 1
        \\  53: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\  59: 0c 01 00                      move r1, r0
        \\  62: 0c 00 01                      move r0, r1
        \\  65: 00 71 00 00 00                jump 113
        \\  70: 0a 02 01 00 00 00             load_string r2, @1 ("b")
        \\  76: 39 03 00 02                   eq_strict r3, r0, r2
        \\  80: 01 03 05 00 00 00             jump_if_true r3, 5
        \\  86: 00 14 00 00 00                jump 20
        \\  91: 08 00 02 00 00 00             load_number_i32 r0, 2
        \\  97: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\ 103: 0c 01 00                      move r1, r0
        \\ 106: 00 15 00 00 00                jump 21
        \\ 111: 0a 02 03 00 00 00             load_string r2, @3 ("c")
        \\ 117: 39 03 00 02                   eq_strict r3, r0, r2
        \\ 121: 01 03 05 00 00 00             jump_if_true r3, 5
        \\ 127: 00 1e 00 00 00                jump 30
        \\ 132: 42 00 00 00 00 00             get_binding r0, @0 ("x")
        \\ 138: 08 01 03 00 00 00             load_number_i32 r1, 3
        \\ 144: 25 02 00 01                   add r2, r0, r1
        \\ 148: 46 00 00 00 00 02             set_binding @0 ("x"), r2
        \\ 154: 0c 00 02                      move r0, r2
        \\ 157: 00 15 00 00 00                jump 21
        \\ 162: 0c 00 01                      move r0, r1
        \\ 165: 08 00 64 00 00 00             load_number_i32 r0, 100
        \\ 171: 46 00 00 00 00 00             set_binding @0 ("x"), r0
        \\ 177: 0c 01 00                      move r1, r0
        \\ 180: 0c 00 01                      move r0, r1
        \\ 183: 42 00 00 00 00 00             get_binding r0, @0 ("x")
        \\ 189: 90 00                         return r0
        \\
    );

    // Throw statement
    try testInterpreter(std.testing.allocator,
        \\throw new Error("test");
    , .exception,
        \\IR (test)
        \\  %0: [  0..2  ]                    get_binding @0 ("Error")
        \\  %1: [  1..2  ]                    string @1 ("test")
        \\  %2: [  2..3  ]                    construct %0, [%1]
        \\  %3: [  3..3  ]                    throw %2
        \\  %4: [  4..4  ] dead               return none
        \\
    ,
        \\Bytecode (test)
        \\   0: 42 00 00 00 00 00             get_binding r0, @0 ("Error")
        \\   6: 0a 01 01 00 00 00             load_string r1, @1 ("test")
        \\  12: 82 02 00 01                   construct1 r2, r0, r1
        \\  16: 8e 02                         throw r2
        \\
    );

    // For-in/of statement
    try testInterpreter(std.testing.allocator,
        \\var sum = 0;
        \\for (var x of [1, 2, 3]) {
        \\  sum = sum + x;
        \\}
        \\sum;
    , .{ .value = Value.from(6) },
        \\IR (test)
        \\  %0: [  0..1  ]                    zero
        \\  %1: [  1..1  ]                    set_binding @0 ("sum"), %0
        \\  %2: [  2..5  ]                    one
        \\  %3: [  3..5  ]                    number 2
        \\  %4: [  4..5  ]                    number 3
        \\  %5: [  5..7  ]                    array [%2, %3, %4]
        \\  %6: [  6..8  ]                    undefined
        \\  %7: [  7..19 ]                    get_iterator %5
        \\  %8: [  8..8  ]                    br %9, %6
        \\  %9: [  9..21 ]                    label
        \\ %10: [ 10..14 ]                    iterator_step_value %7
        \\ %11: [ 11..12 ]                    iterator_is_done %7
        \\ %12: [ 12..12 ]                    br_cond %11, %20, %13
        \\ %13: [ 13..13 ]                    label
        \\ %14: [ 14..14 ]                    set_binding @1 ("x"), %10
        \\ %15: [ 15..17 ]                    get_binding @0 ("sum")
        \\ %16: [ 16..17 ]                    get_binding @1 ("x")
        \\ %17: [ 17..18 ]                    add %15, %16
        \\ %18: [ 18..19 ]                    set_binding @0 ("sum"), %17
        \\ %19: [ 19..19 ]                    br %9, %18
        \\ %20: [ 20..20 ]                    label
        \\ %21: [ 21..21 ]                    br %22, %9
        \\ %22: [ 22..22 ]                    label
        \\ %23: [ 23..24 ]                    get_binding @0 ("sum")
        \\ %24: [ 24..24 ]                    return %23
        \\
    ,
        \\Bytecode (test)
        \\   0: 08 00 00 00 00 00             load_number_i32 r0, 0
        \\   6: 46 00 00 00 00 00             set_binding @0 ("sum"), r0
        \\  12: 0c 01 00                      move r1, r0
        \\  15: 08 00 01 00 00 00             load_number_i32 r0, 1
        \\  21: 08 01 02 00 00 00             load_number_i32 r1, 2
        \\  27: 08 02 03 00 00 00             load_number_i32 r2, 3
        \\  33: 0d 03 03 00 00 00             array_create r3, 3
        \\  39: 10 03 00 00 00 00 00          array_set r3, r0, 0
        \\  46: 10 03 01 01 00 00 00          array_set r3, r1, 1
        \\  53: 10 03 02 02 00 00 00          array_set r3, r2, 2
        \\  60: 04 00                         load_undefined r0
        \\  62: 85 01 03                      get_iterator r1, r3
        \\  65: 89 02 01                      iterator_step_value r2, r1
        \\  68: 8c 03 01                      iterator_is_done r3, r1
        \\  71: 01 03 05 00 00 00             jump_if_true r3, 5
        \\  77: 00 08 00 00 00                jump 8
        \\  82: 42 00 00 00 00 00             get_binding r0, @0 ("sum")
        \\  88: 90 00                         return r0
        \\  90: 46 01 00 00 00 02             set_binding @1 ("x"), r2
        \\  96: 0c 03 02                      move r3, r2
        \\  99: 42 02 00 00 00 00             get_binding r2, @0 ("sum")
        \\ 105: 42 03 01 00 00 00             get_binding r3, @1 ("x")
        \\ 111: 25 04 02 03                   add r4, r2, r3
        \\ 115: 46 00 00 00 00 04             set_binding @0 ("sum"), r4
        \\ 121: 0c 02 04                      move r2, r4
        \\ 124: 0c 00 02                      move r0, r2
        \\ 127: 00 bd ff ff ff                jump -67
        \\
    );

    // For-in/of with member expression LHS
    try testInterpreter(
        std.testing.allocator,
        \\var x = {};
        \\for (x.last in {a: 1, b: 2, c: 3});
        \\x.last;
    ,
        .{ .value = Value.from("c") },
        null,
        null,
    );

    // Optional expression
    try testInterpreter(std.testing.allocator,
        \\var o = {a: {b: 123}};
        \\o.x?.y ?? o.a?.["b"];
    , .{ .value = Value.from(123) },
        \\IR (test)
        \\  %0: [  0..4  ]                    string @0 ("a")
        \\  %1: [  1..3  ]                    string @1 ("b")
        \\  %2: [  2..3  ]                    number 123
        \\  %3: [  3..4  ]                    object {%1: %2}
        \\  %4: [  4..5  ]                    object {%0: %3}
        \\  %5: [  5..5  ]                    set_binding @2 ("o"), %4
        \\  %6: [  6..7  ]                    get_binding @2 ("o")
        \\  %7: [  7..12 ]                    get_property %6, @3 ("x")
        \\  %8: [  8..9  ]                    null
        \\  %9: [  9..10 ]                    eq %7, %8
        \\ %10: [ 10..10 ]                    br_cond %9, %14, %11
        \\ %11: [ 11..11 ]                    label
        \\ %12: [ 12..13 ]                    get_property %7, @4 ("y")
        \\ %13: [ 13..13 ]                    br %17, %12
        \\ %14: [ 14..14 ]                    label
        \\ %15: [ 15..16 ]                    undefined
        \\ %16: [ 16..16 ]                    br %17, %15
        \\ %17: [ 17..30 ]                    label
        \\ %18: [ 18..19 ]                    get_binding @2 ("o")
        \\ %19: [ 19..24 ]                    get_property %18, @0 ("a")
        \\ %20: [ 20..21 ]                    null
        \\ %21: [ 21..22 ]                    eq %19, %20
        \\ %22: [ 22..22 ]                    br_cond %21, %26, %23
        \\ %23: [ 23..23 ]                    label
        \\ %24: [ 24..25 ]                    get_property %19, @1 ("b")
        \\ %25: [ 25..25 ]                    br %29, %24
        \\ %26: [ 26..26 ]                    label
        \\ %27: [ 27..28 ]                    undefined
        \\ %28: [ 28..28 ]                    br %29, %27
        \\ %29: [ 29..30 ]                    label
        \\ %30: [ 30..31 ]                    nullish_coalesce %17, %29
        \\ %31: [ 31..31 ]                    return %30
        \\
    ,
        \\Bytecode (test)
        \\   0: 0a 00 00 00 00 00             load_string r0, @0 ("a")
        \\   6: 0a 01 01 00 00 00             load_string r1, @1 ("b")
        \\  12: 08 02 7b 00 00 00             load_number_i32 r2, 123
        \\  18: 12 03                         object_create r3
        \\  20: 13 03 01 00 00 00 02          object_set r3, @1 ("b"), r2
        \\  27: 12 01                         object_create r1
        \\  29: 13 01 00 00 00 00 03          object_set r1, @0 ("a"), r3
        \\  36: 46 02 00 00 00 01             set_binding @2 ("o"), r1
        \\  42: 0c 00 01                      move r0, r1
        \\  45: 42 00 02 00 00 00             get_binding r0, @2 ("o")
        \\  51: 43 01 00 03 00 00 00          get_property r1, r0, @3 ("x")
        \\  58: 05 00                         load_null r0
        \\  60: 37 02 01 00                   eq r2, r1, r0
        \\  64: 01 02 05 00 00 00             jump_if_true r2, 5
        \\  70: 00 07 00 00 00                jump 7
        \\  75: 04 00                         load_undefined r0
        \\  77: 00 07 00 00 00                jump 7
        \\  82: 43 00 01 04 00 00 00          get_property r0, r1, @4 ("y")
        \\  89: 42 01 02 00 00 00             get_binding r1, @2 ("o")
        \\  95: 43 02 01 00 00 00 00          get_property r2, r1, @0 ("a")
        \\ 102: 05 01                         load_null r1
        \\ 104: 37 03 02 01                   eq r3, r2, r1
        \\ 108: 01 03 05 00 00 00             jump_if_true r3, 5
        \\ 114: 00 07 00 00 00                jump 7
        \\ 119: 04 01                         load_undefined r1
        \\ 121: 00 07 00 00 00                jump 7
        \\ 126: 43 01 02 01 00 00 00          get_property r1, r2, @1 ("b")
        \\ 133: 0c 02 00                      move r2, r0
        \\ 136: 03 00 05 00 00 00             jump_if_nullish r0, 5
        \\ 142: 00 03 00 00 00                jump 3
        \\ 147: 0c 02 01                      move r2, r1
        \\ 150: 90 02                         return r2
        \\
    );

    // Tagged template
    try testInterpreter(std.testing.allocator,
        \\String.raw`a${"b"}c`;
    , .{ .value = Value.from("abc") },
        \\IR (test)
        \\  %0: [  0..10 ]                    get_binding @0 ("String")
        \\  %1: [  1..10 ]                    get_property %0, @1 ("raw")
        \\  %2: [  2..4  ]                    string @2 ("a")
        \\  %3: [  3..4  ]                    string @3 ("c")
        \\  %4: [  4..8  ]                    array [%2, %3]
        \\  %5: [  5..7  ]                    string @2 ("a")
        \\  %6: [  6..7  ]                    string @3 ("c")
        \\  %7: [  7..8  ]                    array [%5, %6]
        \\  %8: [  8..10 ]                    get_template_object %4, %7, 0
        \\  %9: [  9..10 ]                    string @4 ("b")
        \\ %10: [ 10..11 ]                    call %1, %0, [%8, %9]
        \\ %11: [ 11..11 ]                    return %10
        \\
    ,
        \\Bytecode (test)
        \\   0: 42 00 00 00 00 00             get_binding r0, @0 ("String")
        \\   6: 43 01 00 01 00 00 00          get_property r1, r0, @1 ("raw")
        \\  13: 0a 02 02 00 00 00             load_string r2, @2 ("a")
        \\  19: 0a 03 03 00 00 00             load_string r3, @3 ("c")
        \\  25: 0d 04 02 00 00 00             array_create r4, 2
        \\  31: 10 04 02 00 00 00 00          array_set r4, r2, 0
        \\  38: 10 04 03 01 00 00 00          array_set r4, r3, 1
        \\  45: 0a 02 02 00 00 00             load_string r2, @2 ("a")
        \\  51: 0a 03 03 00 00 00             load_string r3, @3 ("c")
        \\  57: 0d 05 02 00 00 00             array_create r5, 2
        \\  63: 10 05 02 00 00 00 00          array_set r5, r2, 0
        \\  70: 10 05 03 01 00 00 00          array_set r5, r3, 1
        \\  77: 84 02 04 05 00 00             get_template_object r2, r4, r5, 0
        \\  83: 0a 03 04 00 00 00             load_string r3, @4 ("b")
        \\  89: 7d 04 01 00 02 03             call_property2 r4, r1, r0, r2, r3
        \\  95: 90 04                         return r4
        \\
    );

    // Functions
    try testInterpreter(std.testing.allocator,
        \\var f = ({a}, b = 0, ...rest) => a + b + rest.length;
        \\f({a: 1}, 2, 3, 4);
        \\
    , .{ .value = Value.from(5) }, null, null);

    // Arguments object
    try testInterpreter(std.testing.allocator,
        \\function mapped(a) {
        \\  a = 99;
        \\  return arguments[0];
        \\}
        \\function unmapped(a) {
        \\  "use strict";
        \\  a = 99;
        \\  return arguments[0];
        \\}
        \\mapped(1) - unmapped(1);
        \\
    , .{ .value = Value.from(98) }, null, null);

    // Getter/setter
    try testInterpreter(std.testing.allocator,
        \\var o = {
        \\  get x() { return this._x; },
        \\  set x(v) { this._x = v; },
        \\};
        \\o.x = 42;
        \\o.x;
        \\
    , .{ .value = Value.from(42) }, null, null);

    // Generator
    try testInterpreter(std.testing.allocator,
        \\function* g() {
        \\  yield 1;
        \\  yield 2;
        \\  yield 3;
        \\}
        \\function f() {
        \\  var gen = g();
        \\  var a = gen.next();
        \\  var b = gen.next();
        \\  var c = gen.next();
        \\  return a.value + b.value + c.value;
        \\}
        \\f();
        \\
    , .{ .value = Value.from(6) }, null, null);

    // Async Generator
    try testInterpreter(std.testing.allocator,
        \\async function* g() {
        \\  yield 1;
        \\  yield 2;
        \\  yield 3;
        \\}
        \\async function f() {
        \\  var gen = g();
        \\  var a = await gen.next();
        \\  var b = await gen.next();
        \\  var c = await gen.next();
        \\  return a.value + b.value + c.value;
        \\}
        \\f();
        \\
    , .{ .promise = .{ .fulfilled = Value.from(6) } }, null, null);

    // Classes
    try testInterpreter(std.testing.allocator,
        \\class Foo {
        \\  #x;
        \\  constructor(x) { this.#x = x; }
        \\  getX() { return this.#x; }
        \\}
        \\class Bar extends Foo {
        \\  constructor(x) { super(x); }
        \\  getX() { return super.getX() + 1; }
        \\}
        \\var bar = new Bar(42);
        \\bar.getX();
        \\
    , .{ .value = Value.from(43) }, null, null);
}
