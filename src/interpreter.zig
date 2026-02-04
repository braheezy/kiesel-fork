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
    const Script = @import("language/Script.zig");

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

    try Script.globalDeclarationInstantiation(&agent, script, realm.global_env);

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
        \\   0: end                                                   [0..0]
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
        \\   0: number 42                                             [0..1]
        \\   1: end %0                                                [1..1]
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
        \\   0: number 40                                             [0..0] dead
        \\   1: number 41                                             [1..1] dead
        \\   2: number 42                                             [2..3]
        \\   3: end %2                                                [3..3]
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
        \\   0: number 6                                              [0..1]
        \\   1: end %0                                                [1..1]
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
        \\   0: one                                                   [0..7]
        \\   1: number 2                                              [1..3]
        \\   2: number 3                                              [2..3]
        \\   3: array [%1, %2]                                        [3..7]
        \\   4: string "x"                                            [4..6]
        \\   5: number 4                                              [5..6]
        \\   6: object {%4: %5}                                       [6..7]
        \\   7: array [%0, none, %3, %6]                              [7..8]
        \\   8: end %7                                                [8..8]
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
        \\   0: string "a"                                            [0..6]
        \\   1: one                                                   [1..6]
        \\   2: number 2                                              [2..6]
        \\   3: string "two"                                          [3..6]
        \\   4: string "b"                                            [4..6]
        \\   5: number 3                                              [5..6]
        \\   6: object {%0: %1, %2: %3, %4: %5}                       [6..7]
        \\   7: end %6                                                [7..7]
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

    // Variable statements
    try testInterpreter(std.testing.allocator,
        \\var x = 10, y = 20;
        \\x + y;
        \\
    , .{ .value = Value.from(30) },
        \\IR (test)
        \\   0: number 10                                             [0..1]
        \\   1: set_binding "x", %0                                   [1..1]
        \\   2: number 20                                             [2..3]
        \\   3: set_binding "y", %2                                   [3..3]
        \\   4: get_binding "x"                                       [4..6]
        \\   5: get_binding "y"                                       [5..6]
        \\   6: add %4, %5                                            [6..7]
        \\   7: end %6                                                [7..7]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 10
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: load_number_i32 r0, 20
        \\  21: set_binding @1, r0
        \\  27: move r1, r0
        \\  30: get_binding r0, @0
        \\  36: get_binding r1, @1
        \\  42: add r2, r0, r1
        \\  46: end r2
        \\
    );

    // Increment/decrement operators
    try testInterpreter(std.testing.allocator,
        \\x = 5;
        \\++x;
        \\
    , .{ .value = Value.from(6) },
        \\IR (test)
        \\   0: number 5                                              [0..1]
        \\   1: set_binding "x", %0                                   [1..1]
        \\   2: update_binding "x", increment, prefix                 [2..3]
        \\   3: end %2                                                [3..3]
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
        \\   0: get_binding "Math"                                    [0..2]
        \\   1: get_property %0, "random"                             [1..2]
        \\   2: call %1, %0, []                                       [2..2]
        \\   3: get_binding "Number"                                  [3..5]
        \\   4: string "1"                                            [4..5]
        \\   5: call %3, none, [%4]                                   [5..5]
        \\   6: get_binding "JSON"                                    [6..13]
        \\   7: get_property %6, "stringify"                          [7..13]
        \\   8: string "foo"                                          [8..10]
        \\   9: string "bar"                                          [9..10]
        \\  10: object {%8: %9}                                       [10..13]
        \\  11: null                                                  [11..13]
        \\  12: number 2                                              [12..13]
        \\  13: call %7, %6, [%10, %11, %12]                          [13..13]
        \\  14: get_binding "Math"                                    [14..23]
        \\  15: get_property %14, "max"                               [15..23]
        \\  16: one                                                   [16..23]
        \\  17: number 5                                              [17..23]
        \\  18: number 3                                              [18..20]
        \\  19: number 9                                              [19..20]
        \\  20: array [%18, %19]                                      [20..21]
        \\  21: spread %20                                            [21..23]
        \\  22: number 2                                              [22..23]
        \\  23: call %15, %14, [%16, %17, %21, %22]                   [23..24]
        \\  24: end %23                                               [24..24]
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
        \\   0: string "a"                                            [0..6]
        \\   1: one                                                   [1..6]
        \\   2: string "b"                                            [2..6]
        \\   3: number 2                                              [3..6]
        \\   4: string "c"                                            [4..6]
        \\   5: number 3                                              [5..6]
        \\   6: object {%0: %1, %2: %3, %4: %5}                       [6..10]
        \\   7: get_property %6, "a"                                  [7..8]
        \\   8: initialize_binding "a", %7                            [8..8]
        \\   9: string "a"                                            [9..10]
        \\  10: copy_data_properties %6, [%9]                         [10..11]
        \\  11: initialize_binding "rest", %10                        [11..11]
        \\  12: number 10                                             [12..14]
        \\  13: number 20                                             [13..14]
        \\  14: array [%12, %13]                                      [14..15]
        \\  15: get_iterator %14                                      [15..18]
        \\  16: iterator_step_value %15                               [16..17]
        \\  17: initialize_binding "x", %16                           [17..17]
        \\  18: iterator_step_value %15                               [18..19]
        \\  19: initialize_binding "y", %18                           [19..19]
        \\  20: string "d"                                            [20..24]
        \\  21: string "e"                                            [21..23]
        \\  22: number 5                                              [22..23]
        \\  23: object {%21: %22}                                     [23..24]
        \\  24: object {%20: %23}                                     [24..26]
        \\  25: string "d"                                            [25..26]
        \\  26: get_property_computed %24, %25                        [26..27]
        \\  27: get_property %26, "e"                                 [27..28]
        \\  28: initialize_binding "e", %27                           [28..28]
        \\  29: string "f"                                            [29..31]
        \\  30: get_binding "undefined"                               [30..31]
        \\  31: object {%29: %30}                                     [31..32]
        \\  32: get_property %31, "f"                                 [32..40]
        \\  33: undefined                                             [33..34]
        \\  34: eq_strict %32, %33                                    [34..35]
        \\  35: br_cond %34, %36, %39                                 [35..35]
        \\  36: label                                                 [36..36]
        \\  37: number 99                                             [37..38]
        \\  38: br %41, %37                                           [38..38]
        \\  39: label                                                 [39..39]
        \\  40: br %41, %32                                           [40..40]
        \\  41: label                                                 [41..42]
        \\  42: initialize_binding "f", %41                           [42..42]
        \\  43: get_binding "rest"                                    [43..44]
        \\  44: get_property %43, "b"                                 [44..46]
        \\  45: get_binding "x"                                       [45..46]
        \\  46: add %44, %45                                          [46..48]
        \\  47: get_binding "e"                                       [47..48]
        \\  48: add %46, %47                                          [48..50]
        \\  49: get_binding "f"                                       [49..50]
        \\  50: add %48, %49                                          [50..51]
        \\  51: end %50                                               [51..51]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_string r0, @0
        \\   6: load_number_i32 r1, 1
        \\  12: load_string r2, @1
        \\  18: load_number_i32 r3, 2
        \\  24: load_string r4, @2
        \\  30: load_number_i32 r5, 3
        \\  36: object_create r6
        \\  38: object_set r6, @0, r1
        \\  45: object_set r6, @1, r3
        \\  52: object_set r6, @2, r5
        \\  59: get_property r0, r6, @0
        \\  66: initialize_binding @0, r0
        \\  72: move r1, r0
        \\  75: load_string r0, @0
        \\  81: array_create r31, 0
        \\  87: array_push r31, r0
        \\  90: copy_data_properties r1, r6, r31
        \\  94: initialize_binding @3, r1
        \\ 100: move r0, r1
        \\ 103: load_number_i32 r0, 10
        \\ 109: load_number_i32 r1, 20
        \\ 115: array_create r2, 2
        \\ 121: array_set r2, r0, 0
        \\ 128: array_set r2, r1, 1
        \\ 135: get_iterator r0, r2
        \\ 138: iterator_step_value r1, r0
        \\ 141: initialize_binding @4, r1
        \\ 147: move r2, r1
        \\ 150: iterator_step_value r1, r0
        \\ 153: initialize_binding @5, r1
        \\ 159: move r0, r1
        \\ 162: load_string r0, @6
        \\ 168: load_string r1, @7
        \\ 174: load_number_i32 r2, 5
        \\ 180: object_create r3
        \\ 182: object_set r3, @7, r2
        \\ 189: object_create r1
        \\ 191: object_set r1, @6, r3
        \\ 198: load_string r0, @6
        \\ 204: get_property_computed r2, r1, r0
        \\ 208: get_property r0, r2, @7
        \\ 215: initialize_binding @7, r0
        \\ 221: move r1, r0
        \\ 224: load_string r0, @8
        \\ 230: get_binding r1, @9
        \\ 236: object_create r2
        \\ 238: object_set r2, @8, r1
        \\ 245: get_property r0, r2, @8
        \\ 252: load_undefined r1
        \\ 254: eq_strict r2, r0, r1
        \\ 258: jump_if_true r2, 5
        \\ 264: jump 14
        \\ 269: load_number_i32 r1, 99
        \\ 275: move r0, r1
        \\ 278: jump 0
        \\ 283: initialize_binding @8, r0
        \\ 289: move r1, r0
        \\ 292: get_binding r0, @3
        \\ 298: get_property r1, r0, @1
        \\ 305: get_binding r0, @4
        \\ 311: add r2, r1, r0
        \\ 315: get_binding r0, @7
        \\ 321: add r1, r2, r0
        \\ 325: get_binding r0, @8
        \\ 331: add r2, r1, r0
        \\ 335: end r2
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
        \\   0: zero                                                  [0..1]
        \\   1: set_binding "x", %0                                   [1..1]
        \\   2: undefined                                             [2..3]
        \\   3: br %4, %2                                             [3..3]
        \\   4: label                                                 [4..16]
        \\   5: get_binding "x"                                       [5..7]
        \\   6: number 3                                              [6..7]
        \\   7: lt %5, %6                                             [7..8]
        \\   8: br_cond %7, %9, %15                                   [8..8]
        \\   9: label                                                 [9..9]
        \\  10: get_binding "x"                                       [10..12]
        \\  11: one                                                   [11..12]
        \\  12: add %10, %11                                          [12..13]
        \\  13: set_binding "x", %12                                  [13..14]
        \\  14: br %4, %13                                            [14..14]
        \\  15: label                                                 [15..15]
        \\  16: br %17, %4                                            [16..16]
        \\  17: label                                                 [17..17]
        \\  18: label                                                 [18..30]
        \\  19: get_binding "x"                                       [19..21]
        \\  20: one                                                   [20..21]
        \\  21: add %19, %20                                          [21..22]
        \\  22: set_binding "x", %21                                  [22..23]
        \\  23: br %24, %22                                           [23..23]
        \\  24: label                                                 [24..32]
        \\  25: get_binding "x"                                       [25..27]
        \\  26: number 5                                              [26..27]
        \\  27: lt %25, %26                                           [27..28]
        \\  28: br_cond %27, %29, %31                                 [28..28]
        \\  29: label                                                 [29..29]
        \\  30: br %18, %24                                           [30..30]
        \\  31: label                                                 [31..31]
        \\  32: br %33, %24                                           [32..32]
        \\  33: label                                                 [33..33]
        \\  34: zero                                                  [34..35]
        \\  35: set_binding "i", %34                                  [35..35]
        \\  36: undefined                                             [36..37]
        \\  37: br %38, %36                                           [37..37]
        \\  38: label                                                 [38..51]
        \\  39: get_binding "i"                                       [39..41]
        \\  40: number 3                                              [40..41]
        \\  41: lt %39, %40                                           [41..42]
        \\  42: br_cond %41, %43, %50                                 [42..42]
        \\  43: label                                                 [43..43]
        \\  44: get_binding "x"                                       [44..46]
        \\  45: get_binding "i"                                       [45..46]
        \\  46: add %44, %45                                          [46..47]
        \\  47: set_binding "x", %46                                  [47..49]
        \\  48: update_binding "i", increment, postfix                [48..48]
        \\  49: br %38, %47                                           [49..49]
        \\  50: label                                                 [50..50]
        \\  51: br %52, %38                                           [51..51]
        \\  52: label                                                 [52..52]
        \\  53: get_binding "x"                                       [53..54]
        \\  54: end %53                                               [54..54]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 0
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: load_undefined r0
        \\  17: get_binding r1, @0
        \\  23: load_number_i32 r2, 3
        \\  29: lt r3, r1, r2
        \\  33: jump_if_true r3, 5
        \\  39: jump 33
        \\  44: get_binding r1, @0
        \\  50: load_number_i32 r2, 1
        \\  56: add r3, r1, r2
        \\  60: set_binding @0, r3
        \\  66: move r1, r3
        \\  69: move r0, r1
        \\  72: jump -60
        \\  77: get_binding r1, @0
        \\  83: load_number_i32 r2, 1
        \\  89: add r3, r1, r2
        \\  93: set_binding @0, r3
        \\  99: move r1, r3
        \\ 102: get_binding r2, @0
        \\ 108: load_number_i32 r3, 5
        \\ 114: lt r4, r2, r3
        \\ 118: jump_if_true r4, 5
        \\ 124: jump 8
        \\ 129: move r0, r1
        \\ 132: jump -60
        \\ 137: move r0, r1
        \\ 140: load_number_i32 r0, 0
        \\ 146: set_binding @1, r0
        \\ 152: move r1, r0
        \\ 155: load_undefined r0
        \\ 157: get_binding r1, @1
        \\ 163: load_number_i32 r2, 3
        \\ 169: lt r3, r1, r2
        \\ 173: jump_if_true r3, 5
        \\ 179: jump 39
        \\ 184: get_binding r1, @0
        \\ 190: get_binding r2, @1
        \\ 196: add r3, r1, r2
        \\ 200: set_binding @0, r3
        \\ 206: move r1, r3
        \\ 209: increment_binding_postfix r2, @1
        \\ 215: move r0, r1
        \\ 218: jump -66
        \\ 223: get_binding r0, @0
        \\ 229: end r0
        \\
    );
}
