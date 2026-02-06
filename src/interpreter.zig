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
    if (vm.run()) |result| switch (expected_result) {
        .value => |expected| if (expected != null) {
            if (result == null) return error.TestExpectedEqual;
            if (!expected.?.isStrictlyEqual(result.?)) return error.TestExpectedEqual;
        } else if (result != null) return error.TestExpectedEqual,
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
        \\   0: one                                                   [0..0] dead
        \\   1: undefined                                             [1..2]
        \\   2: br %3, %1                                             [2..2]
        \\   3: label                                                 [3..12]
        \\   4: true                                                  [4..5]
        \\   5: br_cond %4, %6, %11                                   [5..5]
        \\   6: label                                                 [6..6]
        \\   7: number 2                                              [7..8]
        \\   8: br %13, %7                                            [8..8]
        \\   9: number 3                                              [9..10] dead
        \\  10: br %3, %9                                             [10..10] dead
        \\  11: label                                                 [11..11]
        \\  12: br %13, %3                                            [12..12]
        \\  13: label                                                 [13..13]
        \\  14: number 4                                              [14..15]
        \\  15: end %14                                               [15..15]
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
        \\   0: label                                                 [0..8]
        \\   1: undefined                                             [1..1] dead
        \\   2: number 6                                              [2..3]
        \\   3: br %4, %2                                             [3..3]
        \\   4: label                                                 [4..10]
        \\   5: zero                                                  [5..6]
        \\   6: br_cond %5, %7, %9                                    [6..6]
        \\   7: label                                                 [7..7]
        \\   8: br %0, %4                                             [8..8]
        \\   9: label                                                 [9..9]
        \\  10: br %11, %4                                            [10..10]
        \\  11: label                                                 [11..12]
        \\  12: end %11                                               [12..12]
        \\
    , null);

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
        \\var a = 1;
        \\var o = {c: 2};
        \\({a, [2]: "two", b: 3, ...o});
        \\
    , .ignore,
        \\IR (test)
        \\   0: one                                                   [0..1]
        \\   1: set_binding "a", %0                                   [1..1]
        \\   2: string "c"                                            [2..4]
        \\   3: number 2                                              [3..4]
        \\   4: object {%2: %3}                                       [4..5]
        \\   5: set_binding "o", %4                                   [5..5]
        \\   6: string "a"                                            [6..14]
        \\   7: get_binding "a"                                       [7..14]
        \\   8: number 2                                              [8..14]
        \\   9: string "two"                                          [9..14]
        \\  10: string "b"                                            [10..14]
        \\  11: number 3                                              [11..14]
        \\  12: get_binding "o"                                       [12..13]
        \\  13: spread %12                                            [13..14]
        \\  14: object {%6: %7, %8: %9, %10: %11, none: %13}          [14..15]
        \\  15: end %14                                               [15..15]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 1
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: load_string r0, @1
        \\  21: load_number_i32 r1, 2
        \\  27: object_create r2
        \\  29: object_set r2, @1, r1
        \\  36: set_binding @2, r2
        \\  42: move r0, r2
        \\  45: load_string r0, @0
        \\  51: get_binding r1, @0
        \\  57: load_number_i32 r2, 2
        \\  63: load_string r3, @3
        \\  69: load_string r4, @4
        \\  75: load_number_i32 r5, 3
        \\  81: get_binding r6, @2
        \\  87: move r7, r6
        \\  90: object_create r6
        \\  92: object_set r6, @0, r1
        \\  99: object_set_computed r6, r2, r3
        \\ 103: object_set r6, @4, r5
        \\ 110: object_spread r6, r7
        \\ 113: end r6
        \\
    );

    // Regular expression literal
    try testInterpreter(std.testing.allocator,
        \\/abc/gi;
        \\
    , .ignore,
        \\IR (test)
        \\   0: reg_exp "abc", "gi"                                   [0..1]
        \\   1: end %0                                                [1..1]
        \\
    ,
        \\Bytecode (test)
        \\   0: reg_exp_create r0, @0, @1
        \\  10: end r0
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

    // Lexical declarations
    try testInterpreter(std.testing.allocator,
        \\let a = 1;
        \\{
        \\  let a = 2;
        \\}
        \\a;
    , .{ .value = Value.from(1) },
        \\IR (test)
        \\   0: one                                                   [0..1]
        \\   1: initialize_binding "a", %0                            [1..1]
        \\   2: push_scope                                            [2..2]
        \\   3: create_mutable_binding "a"                            [3..3]
        \\   4: number 2                                              [4..5]
        \\   5: initialize_binding "a", %4                            [5..5]
        \\   6: pop_scope                                             [6..6]
        \\   7: get_binding "a"                                       [7..8]
        \\   8: end %7                                                [8..8]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 1
        \\   6: initialize_binding @0, r0
        \\  12: move r1, r0
        \\  15: push_scope
        \\  16: create_mutable_binding @0
        \\  21: load_number_i32 r0, 2
        \\  27: initialize_binding @0, r0
        \\  33: move r1, r0
        \\  36: pop_scope
        \\  37: get_binding r0, @0
        \\  43: end r0
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
        \\  18: label                                                 [18..31]
        \\  19: undefined                                             [19..19] dead
        \\  20: get_binding "x"                                       [20..22]
        \\  21: one                                                   [21..22]
        \\  22: add %20, %21                                          [22..23]
        \\  23: set_binding "x", %22                                  [23..24]
        \\  24: br %25, %23                                           [24..24]
        \\  25: label                                                 [25..33]
        \\  26: get_binding "x"                                       [26..28]
        \\  27: number 5                                              [27..28]
        \\  28: lt %26, %27                                           [28..29]
        \\  29: br_cond %28, %30, %32                                 [29..29]
        \\  30: label                                                 [30..30]
        \\  31: br %18, %25                                           [31..31]
        \\  32: label                                                 [32..32]
        \\  33: br %34, %25                                           [33..33]
        \\  34: label                                                 [34..34]
        \\  35: zero                                                  [35..36]
        \\  36: set_binding "i", %35                                  [36..36]
        \\  37: undefined                                             [37..38]
        \\  38: br %39, %37                                           [38..38]
        \\  39: label                                                 [39..54]
        \\  40: get_binding "i"                                       [40..42]
        \\  41: number 3                                              [41..42]
        \\  42: lt %40, %41                                           [42..43]
        \\  43: br_cond %42, %44, %53                                 [43..43]
        \\  44: label                                                 [44..44]
        \\  45: get_binding "x"                                       [45..47]
        \\  46: get_binding "i"                                       [46..47]
        \\  47: add %45, %46                                          [47..48]
        \\  48: set_binding "x", %47                                  [48..52]
        \\  49: br %50, %48                                           [49..49]
        \\  50: label                                                 [50..50]
        \\  51: update_binding "i", increment, postfix                [51..51]
        \\  52: br %39, %48                                           [52..52]
        \\  53: label                                                 [53..53]
        \\  54: br %55, %39                                           [54..54]
        \\  55: label                                                 [55..55]
        \\  56: get_binding "x"                                       [56..57]
        \\  57: end %56                                               [57..57]
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
        \\ 179: jump 42
        \\ 184: get_binding r1, @0
        \\ 190: get_binding r2, @1
        \\ 196: add r3, r1, r2
        \\ 200: set_binding @0, r3
        \\ 206: move r1, r3
        \\ 209: move r2, r1
        \\ 212: increment_binding_postfix r2, @1
        \\ 218: move r0, r1
        \\ 221: jump -69
        \\ 226: get_binding r0, @0
        \\ 232: end r0
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
        \\   0: zero                                                  [0..1]
        \\   1: set_binding "x", %0                                   [1..1]
        \\   2: undefined                                             [2..3]
        \\   3: br %4, %2                                             [3..3]
        \\   4: label                                                 [4..24]
        \\   5: true                                                  [5..6]
        \\   6: br_cond %5, %7, %23                                   [6..6]
        \\   7: label                                                 [7..7]
        \\   8: get_binding "x"                                       [8..10]
        \\   9: one                                                   [9..10]
        \\  10: add %8, %9                                            [10..11]
        \\  11: set_binding "x", %10                                  [11..17]
        \\  12: get_binding "x"                                       [12..14]
        \\  13: number 5                                              [13..14]
        \\  14: gt_eq %12, %13                                        [14..15]
        \\  15: br_cond %14, %16, %19                                 [15..15]
        \\  16: label                                                 [16..16]
        \\  17: br %25, %11                                           [17..17]
        \\  18: br %21, none                                          [18..18] dead
        \\  19: label                                                 [19..19]
        \\  20: br %21, none                                          [20..20]
        \\  21: label                                                 [21..22]
        \\  22: br %4, %21                                            [22..22]
        \\  23: label                                                 [23..23]
        \\  24: br %25, %4                                            [24..24]
        \\  25: label                                                 [25..25]
        \\  26: undefined                                             [26..27]
        \\  27: br %28, %26                                           [27..27]
        \\  28: label                                                 [28..50]
        \\  29: true                                                  [29..30]
        \\  30: br_cond %29, %31, %49                                 [30..30]
        \\  31: label                                                 [31..31]
        \\  32: get_binding "x"                                       [32..34]
        \\  33: one                                                   [33..34]
        \\  34: add %32, %33                                          [34..35]
        \\  35: set_binding "x", %34                                  [35..41]
        \\  36: get_binding "x"                                       [36..38]
        \\  37: number 10                                             [37..38]
        \\  38: gt_eq %36, %37                                        [38..39]
        \\  39: br_cond %38, %40, %43                                 [39..39]
        \\  40: label                                                 [40..40]
        \\  41: br %51, %35                                           [41..41]
        \\  42: br %45, none                                          [42..42] dead
        \\  43: label                                                 [43..43]
        \\  44: br %45, none                                          [44..44]
        \\  45: label                                                 [45..48]
        \\  46: br %47, %45                                           [46..46]
        \\  47: label                                                 [47..47]
        \\  48: br %28, %45                                           [48..48]
        \\  49: label                                                 [49..49]
        \\  50: br %51, %28                                           [50..50]
        \\  51: label                                                 [51..51]
        \\  52: undefined                                             [52..53]
        \\  53: br %54, %52                                           [53..53]
        \\  54: label                                                 [54..82]
        \\  55: get_binding "x"                                       [55..57]
        \\  56: number 20                                             [56..57]
        \\  57: lt %55, %56                                           [57..58]
        \\  58: br_cond %57, %59, %81                                 [58..58]
        \\  59: label                                                 [59..59]
        \\  60: get_binding "x"                                       [60..62]
        \\  61: one                                                   [61..62]
        \\  62: add %60, %61                                          [62..63]
        \\  63: set_binding "x", %62                                  [63..71]
        \\  64: get_binding "x"                                       [64..66]
        \\  65: number 2                                              [65..66]
        \\  66: rem %64, %65                                          [66..68]
        \\  67: zero                                                  [67..68]
        \\  68: eq_strict %66, %67                                    [68..69]
        \\  69: br_cond %68, %70, %73                                 [69..69]
        \\  70: label                                                 [70..70]
        \\  71: br %54, %63                                           [71..71]
        \\  72: br %75, none                                          [72..72] dead
        \\  73: label                                                 [73..73]
        \\  74: br %75, none                                          [74..74]
        \\  75: label                                                 [75..75]
        \\  76: get_binding "x"                                       [76..78]
        \\  77: number 10                                             [77..78]
        \\  78: add %76, %77                                          [78..79]
        \\  79: set_binding "x", %78                                  [79..80]
        \\  80: br %54, %79                                           [80..80]
        \\  81: label                                                 [81..81]
        \\  82: br %83, %54                                           [82..82]
        \\  83: label                                                 [83..83]
        \\  84: undefined                                             [84..89]
        \\  85: get_binding "x"                                       [85..87]
        \\  86: number 100                                            [86..87]
        \\  87: add %85, %86                                          [87..88]
        \\  88: set_binding "x", %87                                  [88..88]
        \\  89: br %94, %84                                           [89..89]
        \\  90: get_binding "x"                                       [90..92] dead
        \\  91: number 200                                            [91..92] dead
        \\  92: add %90, %91                                          [92..93] dead
        \\  93: set_binding "x", %92                                  [93..93] dead
        \\  94: label                                                 [94..94]
        \\  95: get_binding "x"                                       [95..96]
        \\  96: end %95                                               [96..96]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 0
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: load_undefined r0
        \\  17: load_true r1
        \\  19: jump_if_true r1, 5
        \\  25: jump 68
        \\  30: get_binding r1, @0
        \\  36: load_number_i32 r2, 1
        \\  42: add r3, r1, r2
        \\  46: set_binding @0, r3
        \\  52: move r1, r3
        \\  55: get_binding r2, @0
        \\  61: load_number_i32 r3, 5
        \\  67: gt_eq r4, r2, r3
        \\  71: jump_if_true r4, 5
        \\  77: jump 8
        \\  82: move r0, r1
        \\  85: jump 8
        \\  90: move r0, r1
        \\  93: jump -81
        \\  98: load_undefined r0
        \\ 100: load_true r1
        \\ 102: jump_if_true r1, 5
        \\ 108: jump 71
        \\ 113: get_binding r1, @0
        \\ 119: load_number_i32 r2, 1
        \\ 125: add r3, r1, r2
        \\ 129: set_binding @0, r3
        \\ 135: move r1, r3
        \\ 138: get_binding r2, @0
        \\ 144: load_number_i32 r3, 10
        \\ 150: gt_eq r4, r2, r3
        \\ 154: jump_if_true r4, 5
        \\ 160: jump 8
        \\ 165: move r0, r1
        \\ 168: jump 11
        \\ 173: move r2, r1
        \\ 176: move r0, r1
        \\ 179: jump -84
        \\ 184: load_undefined r0
        \\ 186: get_binding r1, @0
        \\ 192: load_number_i32 r2, 20
        \\ 198: lt r3, r1, r2
        \\ 202: jump_if_true r3, 5
        \\ 208: jump 103
        \\ 213: get_binding r1, @0
        \\ 219: load_number_i32 r2, 1
        \\ 225: add r3, r1, r2
        \\ 229: set_binding @0, r3
        \\ 235: move r1, r3
        \\ 238: get_binding r2, @0
        \\ 244: load_number_i32 r3, 2
        \\ 250: rem r4, r2, r3
        \\ 254: load_number_i32 r2, 0
        \\ 260: eq_strict r3, r4, r2
        \\ 264: jump_if_true r3, 5
        \\ 270: jump 8
        \\ 275: move r0, r1
        \\ 278: jump -97
        \\ 283: get_binding r1, @0
        \\ 289: load_number_i32 r2, 10
        \\ 295: add r3, r1, r2
        \\ 299: set_binding @0, r3
        \\ 305: move r1, r3
        \\ 308: move r0, r1
        \\ 311: jump -130
        \\ 316: load_undefined r0
        \\ 318: get_binding r1, @0
        \\ 324: load_number_i32 r2, 100
        \\ 330: add r3, r1, r2
        \\ 334: set_binding @0, r3
        \\ 340: move r1, r3
        \\ 343: get_binding r0, @0
        \\ 349: end r0
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
        \\   0: zero                                                  [0..1]
        \\   1: set_binding "x", %0                                   [1..1]
        \\   2: string "b"                                            [2..15]
        \\   3: undefined                                             [3..18]
        \\   4: br %5, %3                                             [4..4]
        \\   5: label                                                 [5..5]
        \\   6: string "a"                                            [6..7]
        \\   7: eq_strict %2, %6                                      [7..8]
        \\   8: br_cond %7, %19, %9                                   [8..8]
        \\   9: label                                                 [9..9]
        \\  10: string "b"                                            [10..11]
        \\  11: eq_strict %2, %10                                     [11..12]
        \\  12: br_cond %11, %23, %13                                 [12..12]
        \\  13: label                                                 [13..13]
        \\  14: string "c"                                            [14..15]
        \\  15: eq_strict %2, %14                                     [15..16]
        \\  16: br_cond %15, %26, %17                                 [16..16]
        \\  17: label                                                 [17..17]
        \\  18: br %32, %3                                            [18..18]
        \\  19: label                                                 [19..19]
        \\  20: one                                                   [20..21]
        \\  21: set_binding "x", %20                                  [21..22]
        \\  22: br %36, %21                                           [22..22]
        \\  23: label                                                 [23..23]
        \\  24: number 2                                              [24..25]
        \\  25: set_binding "x", %24                                  [25..25]
        \\  26: label                                                 [26..26]
        \\  27: get_binding "x"                                       [27..29]
        \\  28: number 3                                              [28..29]
        \\  29: add %27, %28                                          [29..30]
        \\  30: set_binding "x", %29                                  [30..31]
        \\  31: br %36, %30                                           [31..31]
        \\  32: label                                                 [32..32]
        \\  33: number 100                                            [33..34]
        \\  34: set_binding "x", %33                                  [34..35]
        \\  35: br %36, %34                                           [35..35]
        \\  36: label                                                 [36..36]
        \\  37: get_binding "x"                                       [37..38]
        \\  38: end %37                                               [38..38]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 0
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: load_string r0, @1
        \\  21: load_undefined r1
        \\  23: move r2, r1
        \\  26: load_string r2, @2
        \\  32: eq_strict r3, r0, r2
        \\  36: jump_if_true r3, 5
        \\  42: jump 23
        \\  47: load_number_i32 r0, 1
        \\  53: set_binding @0, r0
        \\  59: move r1, r0
        \\  62: move r0, r1
        \\  65: jump 113
        \\  70: load_string r2, @1
        \\  76: eq_strict r3, r0, r2
        \\  80: jump_if_true r3, 5
        \\  86: jump 20
        \\  91: load_number_i32 r0, 2
        \\  97: set_binding @0, r0
        \\ 103: move r1, r0
        \\ 106: jump 21
        \\ 111: load_string r2, @3
        \\ 117: eq_strict r3, r0, r2
        \\ 121: jump_if_true r3, 5
        \\ 127: jump 30
        \\ 132: get_binding r0, @0
        \\ 138: load_number_i32 r1, 3
        \\ 144: add r2, r0, r1
        \\ 148: set_binding @0, r2
        \\ 154: move r0, r2
        \\ 157: jump 21
        \\ 162: move r0, r1
        \\ 165: load_number_i32 r0, 100
        \\ 171: set_binding @0, r0
        \\ 177: move r1, r0
        \\ 180: move r0, r1
        \\ 183: get_binding r0, @0
        \\ 189: end r0
        \\
    );

    // Throw statement
    try testInterpreter(std.testing.allocator,
        \\throw new Error("test");
    , .exception,
        \\IR (test)
        \\   0: get_binding "Error"                                   [0..2]
        \\   1: string "test"                                         [1..2]
        \\   2: construct %0, [%1]                                    [2..3]
        \\   3: throw %2                                              [3..4]
        \\   4: end %3                                                [4..4]
        \\
    ,
        \\Bytecode (test)
        \\   0: get_binding r0, @0
        \\   6: load_string r1, @1
        \\  12: construct1 r2, r0, r1
        \\  16: throw r2
        \\  18: end r0
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
        \\   0: zero                                                  [0..1]
        \\   1: set_binding "sum", %0                                 [1..1]
        \\   2: one                                                   [2..5]
        \\   3: number 2                                              [3..5]
        \\   4: number 3                                              [4..5]
        \\   5: array [%2, %3, %4]                                    [5..7]
        \\   6: undefined                                             [6..8]
        \\   7: get_iterator %5                                       [7..19]
        \\   8: br %9, %6                                             [8..8]
        \\   9: label                                                 [9..21]
        \\  10: iterator_step_value %7                                [10..14]
        \\  11: iterator_is_done %7                                   [11..12]
        \\  12: br_cond %11, %20, %13                                 [12..12]
        \\  13: label                                                 [13..13]
        \\  14: set_binding "x", %10                                  [14..14]
        \\  15: get_binding "sum"                                     [15..17]
        \\  16: get_binding "x"                                       [16..17]
        \\  17: add %15, %16                                          [17..18]
        \\  18: set_binding "sum", %17                                [18..19]
        \\  19: br %9, %18                                            [19..19]
        \\  20: label                                                 [20..20]
        \\  21: br %22, %9                                            [21..21]
        \\  22: label                                                 [22..22]
        \\  23: get_binding "sum"                                     [23..24]
        \\  24: end %23                                               [24..24]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 0
        \\   6: set_binding @0, r0
        \\  12: move r1, r0
        \\  15: load_number_i32 r0, 1
        \\  21: load_number_i32 r1, 2
        \\  27: load_number_i32 r2, 3
        \\  33: array_create r3, 3
        \\  39: array_set r3, r0, 0
        \\  46: array_set r3, r1, 1
        \\  53: array_set r3, r2, 2
        \\  60: load_undefined r0
        \\  62: get_iterator r1, r3
        \\  65: iterator_step_value r2, r1
        \\  68: iterator_is_done r3, r1
        \\  71: jump_if_true r3, 5
        \\  77: jump 8
        \\  82: get_binding r0, @0
        \\  88: end r0
        \\  90: set_binding @1, r2
        \\  96: move r3, r2
        \\  99: get_binding r2, @0
        \\ 105: get_binding r3, @1
        \\ 111: add r4, r2, r3
        \\ 115: set_binding @0, r4
        \\ 121: move r2, r4
        \\ 124: move r0, r2
        \\ 127: jump -67
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
}
