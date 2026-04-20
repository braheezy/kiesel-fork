const std = @import("std");

const ast = @import("language/ast.zig");
const execution = @import("execution.zig");
const types = @import("types.zig");

const Agent = execution.Agent;
const Value = types.Value;

pub const Bytecode = @import("interpreter/Bytecode.zig");
pub const Ir = @import("interpreter/Ir.zig");
pub const Vm = @import("interpreter/Vm.zig");

pub fn compile(
    agent: *Agent,
    name: []const u8,
    ast_node: Ir.Builder.Ast,
) std.mem.Allocator.Error!Bytecode {
    const gpa = agent.gpa;

    const stdout = agent.platform.stdout;
    const terminal: std.Io.Terminal = .{
        .writer = stdout,
        .mode = agent.platform.terminal_mode,
    };

    var ir = ir: {
        var builder: Ir.Builder = .init(gpa, name, ast_node);
        defer builder.deinit();
        break :ir try builder.build();
    };
    defer ir.deinit(gpa);

    if (agent.options.debug.print_ir) {
        ir.print(terminal) catch {};
        // Print an extra newline for separation
        stdout.writeByte('\n') catch {};
        stdout.flush() catch {};
    }

    // Unlike IR, bytecode is GC-allocated and not explicitly freed.
    // This allows us to use UTF-8 strings from the string table directly without incurring a copy
    // via `Vm.getString()` and `String.toUtf8()`.
    var bc = bc: {
        var builder: Bytecode.Builder = try .init(agent.gc_allocator, &ir);
        defer builder.deinit();
        break :bc try builder.build();
    };
    errdefer bc.deinit(agent.gc_allocator);

    if (agent.options.debug.print_bytecode) {
        bc.print(terminal) catch {};
        // Print an extra newline for separation
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
    errdefer bc.deinit(agent.gc_allocator);

    var vm: Vm = try .init(agent, &bc);
    defer vm.deinit();
    const result = try vm.run(.{});
    return switch (result) {
        .@"return" => |value| value,
        .yield => unreachable,
    };
}

test {
    _ = @import("interpreter/test.zig");
}
