const std = @import("std");

const execution = @import("execution.zig");
const types = @import("types.zig");

const Agent = execution.Agent;
const Realm = execution.Realm;
const Value = types.Value;

pub const Bytecode = @import("interpreter/Bytecode.zig");
pub const Ir = @import("interpreter/Ir.zig");
pub const Vm = @import("interpreter/Vm.zig");

const ExpectedResult = union(enum) {
    value: ?Value,
    ignore,
};

fn testInterpreter(
    gpa: std.mem.Allocator,
    source: []const u8,
    expected_result: ExpectedResult,
    expected_ir: ?[]const u8,
    expected_bc: ?[]const u8,
) !void {
    const ast = @import("language/ast.zig");
    const Parser = @import("language/Parser.zig");

    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const script = try Parser.parse(ast.Script, arena, source, .{});

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
    var agent: Agent = try .init(&platform, .{});
    defer agent.deinit();

    try Realm.initializeHostDefinedRealm(&agent, .{});

    const realm = agent.currentRealm();
    const test_context = try agent.gc_allocator.create(execution.ExecutionContext);
    test_context.* = .{
        .origin = .script,
        .realm = realm,
        .script_or_module = null,
        .ecmascript_code = .{
            .variable_environment = .{ .global_environment = realm.global_env },
            .lexical_environment = .{ .global_environment = realm.global_env },
            .private_environment = null,
        },
    };
    try agent.execution_context_stack.append(agent.gc_allocator, test_context);
    defer _ = agent.execution_context_stack.pop().?;

    var vm: Vm = try .init(&agent, &bc);
    defer vm.deinit();
    const result = try vm.run();
    switch (expected_result) {
        .value => |expected| try std.testing.expectEqual(expected, result),
        .ignore => {},
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
        \\   0: end                     [0..0]
        \\
    ,
        \\Bytecode (test)
        \\   0: end
        \\
        ,
    );

    // Simple expression statement
    try testInterpreter(std.testing.allocator,
        \\42;
        \\
    , .{ .value = Value.from(42) },
        \\IR (test)
        \\   0: number 42               [0..1]
        \\   1: end %0                  [1..1]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 42
        \\   6: end r0
        \\
    );

    // Dead code elimination
    try testInterpreter(std.testing.allocator,
        \\40;
        \\41;
        \\42;
        \\
    , .{ .value = Value.from(42) },
        \\IR (test)
        \\   0: number 40               [0..0] dead
        \\   1: number 41               [1..1] dead
        \\   2: number 42               [2..3]
        \\   3: end %2                  [3..3]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 42
        \\   6: end r0
        \\
    );

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
        \\   0: number 6                [0..1]
        \\   1: end %0                  [1..1]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 6
        \\   6: end r0
        \\
    );

    // Array literal
    try testInterpreter(std.testing.allocator,
        \\[1, , [2, 3], {x: 4}];
        \\
    , .ignore,
        \\IR (test)
        \\   0: one                     [0..7]
        \\   1: number 2                [1..3]
        \\   2: number 3                [2..3]
        \\   3: array [%1, %2]          [3..7]
        \\   4: string "x"              [4..6]
        \\   5: number 4                [5..6]
        \\   6: object {%4: %5}         [6..7]
        \\   7: array [%0, none, %3, %6] [7..8]
        \\   8: end %7                  [8..8]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 1
        \\   6: load_number_i32 r1, 2
        \\  12: load_number_i32 r2, 3
        \\  18: array_create r3, 2
        \\  24: array_set r3, r1, 0
        \\  31: array_set r3, r2, 1
        \\  38: load_string r1, @0
        \\  44: load_number_i32 r2, 4
        \\  50: object_create r4
        \\  52: object_set r4, @0, r2
        \\  59: array_create r1, 4
        \\  65: array_set r1, r0, 0
        \\  72: array_set r1, r3, 2
        \\  79: array_set r1, r4, 3
        \\  86: end r1
        \\
    );

    // Object literal
    try testInterpreter(std.testing.allocator,
        \\({a: 1, [2]: "two", b: 3});
        \\
    , .ignore,
        \\IR (test)
        \\   0: string "a"              [0..6]
        \\   1: one                     [1..6]
        \\   2: number 2                [2..6]
        \\   3: string "two"            [3..6]
        \\   4: string "b"              [4..6]
        \\   5: number 3                [5..6]
        \\   6: object {%0: %1, %2: %3, %4: %5} [6..7]
        \\   7: end %6                  [7..7]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_string r0, @0
        \\   6: load_number_i32 r1, 1
        \\  12: load_number_i32 r2, 2
        \\  18: load_string r3, @1
        \\  24: load_string r4, @2
        \\  30: load_number_i32 r5, 3
        \\  36: object_create r6
        \\  38: object_set r6, @0, r1
        \\  45: object_set_computed r6, r2, r3
        \\  49: object_set r6, @2, r5
        \\  56: end r6
        \\
    );

    // Variable assignment
    try testInterpreter(std.testing.allocator,
        \\x = 42;
        \\x;
        \\
    , .{ .value = Value.from(42) },
        \\IR (test)
        \\   0: number 42               [0..1]
        \\   1: set_binding "x", %0     [1..1]
        \\   2: get_binding "x"         [2..3]
        \\   3: end %2                  [3..3]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 42
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: get_binding r0, @0
        \\  21: end r0
        \\
    );

    // Increment/decrement operators
    try testInterpreter(std.testing.allocator,
        \\x = 5;
        \\++x;
        \\
    , .{ .value = Value.from(6) },
        \\IR (test)
        \\   0: number 5                [0..1]
        \\   1: set_binding "x", %0     [1..1]
        \\   2: update_binding prefix increment "x" [2..3]
        \\   3: end %2                  [3..3]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 5
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: increment_binding_prefix r0, @0
        \\  21: end r0
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
        \\   0: get_binding "Math"      [0..2]
        \\   1: get_property %0, "random" [1..2]
        \\   2: call %1, %0, []         [2..2]
        \\   3: get_binding "Number"    [3..5]
        \\   4: string "1"              [4..5]
        \\   5: call %3, none, [%4]     [5..5]
        \\   6: get_binding "JSON"      [6..13]
        \\   7: get_property %6, "stringify" [7..13]
        \\   8: string "foo"            [8..10]
        \\   9: string "bar"            [9..10]
        \\  10: object {%8: %9}         [10..13]
        \\  11: null                    [11..13]
        \\  12: number 2                [12..13]
        \\  13: call %7, %6, [%10, %11, %12] [13..13]
        \\  14: get_binding "Math"      [14..23]
        \\  15: get_property %14, "max" [15..23]
        \\  16: one                     [16..23]
        \\  17: number 5                [17..23]
        \\  18: number 3                [18..20]
        \\  19: number 9                [19..20]
        \\  20: array [%18, %19]        [20..21]
        \\  21: spread %20              [21..23]
        \\  22: number 2                [22..23]
        \\  23: call %15, %14, [%16, %17, %21, %22] [23..24]
        \\  24: end %23                 [24..24]
        \\
    ,
        \\Bytecode (test)
        \\   0: get_binding r0, @0
        \\   6: get_property r1, r0, @1
        \\  13: call_property0 r2, r1, r0
        \\  17: get_binding r0, @2
        \\  23: load_string r1, @3
        \\  29: call1 r2, r0, r1
        \\  33: get_binding r0, @4
        \\  39: get_property r1, r0, @5
        \\  46: load_string r2, @6
        \\  52: load_string r3, @7
        \\  58: object_create r4
        \\  60: object_set r4, @6, r3
        \\  67: load_null r2
        \\  69: load_number_i32 r3, 2
        \\  75: array_create r31, 0
        \\  81: array_push r31, r4
        \\  84: array_push r31, r2
        \\  87: array_push r31, r3
        \\  90: call_property r5, r1, r0, r31
        \\  95: get_binding r0, @0
        \\ 101: get_property r1, r0, @8
        \\ 108: load_number_i32 r2, 1
        \\ 114: load_number_i32 r3, 5
        \\ 120: load_number_i32 r4, 3
        \\ 126: load_number_i32 r5, 9
        \\ 132: array_create r6, 2
        \\ 138: array_set r6, r4, 0
        \\ 145: array_set r6, r5, 1
        \\ 152: move r4, r6
        \\ 155: load_number_i32 r5, 2
        \\ 161: array_create r31, 0
        \\ 167: array_push r31, r2
        \\ 170: array_push r31, r3
        \\ 173: array_spread r31, r6
        \\ 176: array_push r31, r5
        \\ 179: call_property r6, r1, r0, r31
        \\ 184: end r6
        \\
        ,
    );
}
