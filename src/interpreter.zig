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
    expected_ir: []const u8,
    expected_bc: []const u8,
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

    var vm: Vm = try .init(gpa, &agent, &bc);
    defer vm.deinit(gpa);
    const result = try vm.run();
    switch (expected_result) {
        .value => |expected| try std.testing.expectEqual(expected, result),
        .ignore => {},
    }

    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    try ir.print(&aw.writer, .no_color);
    try std.testing.expectEqualStrings(expected_ir, aw.written());
    aw.clearRetainingCapacity();

    try bc.print(&aw.writer, .no_color);
    try std.testing.expectEqualStrings(expected_bc, aw.written());
    aw.clearRetainingCapacity();
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
}
