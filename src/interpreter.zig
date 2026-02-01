const std = @import("std");

const execution = @import("execution.zig");
const types = @import("types.zig");

const Agent = execution.Agent;
const Value = types.Value;

pub const Bytecode = @import("interpreter/Bytecode.zig");
pub const Ir = @import("interpreter/Ir.zig");
pub const Vm = @import("interpreter/Vm.zig");

fn testInterpreter(
    gpa: std.mem.Allocator,
    source: []const u8,
    expected_result: ?Value,
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

    var vm: Vm = try .init(gpa, &agent, &bc);
    defer vm.deinit(gpa);
    const result = try vm.run();
    try std.testing.expectEqual(expected_result, result);

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
    try testInterpreter(
        std.testing.allocator,
        \\
    ,
        null,
        \\IR (test)
        \\   0: end                     [0..0]
        \\
    ,
        \\Bytecode (test)
        \\   0: end
        \\
        ,
    );
    try testInterpreter(std.testing.allocator,
        \\42
        \\
    , Value.from(42),
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
    try testInterpreter(std.testing.allocator,
        \\42; // Eliminated by liveness analysis
        \\if (true) {
        \\  1 + 2 + 3;
        \\} else {
        \\  4 + 5; // Eliminated by constant folding
        \\}
        \\
    , Value.from(6),
        \\IR (test)
        \\   0: number 42               [0..0] dead
        \\   1: one                     [1..3]
        \\   2: number 2                [2..3]
        \\   3: add %1, %2              [3..5]
        \\   4: number 3                [4..5]
        \\   5: add %3, %4              [5..6]
        \\   6: end %5                  [6..6]
        \\
    ,
        \\Bytecode (test)
        \\   0: load_number_i32 r0, 1
        \\   6: load_number_i32 r1, 2
        \\  12: add r2, r0, r1
        \\  16: load_number_i32 r0, 3
        \\  22: add r1, r2, r0
        \\  26: end r1
        \\
    );
}
