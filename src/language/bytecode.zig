const std = @import("std");

const ast = @import("../language/ast.zig");
const codegen = @import("bytecode/codegen.zig");
const execution = @import("../execution.zig");
const instructions = @import("bytecode/instructions.zig");
const interpreter = @import("../interpreter.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Completion = types.Completion;

pub const Executable = @import("bytecode/Executable.zig");
pub const Instruction = instructions.Instruction;
pub const InstructionIterator = instructions.InstructionIterator;
pub const Vm = @import("bytecode/Vm.zig");

test {
    _ = instructions;

    _ = Executable;
    _ = Vm;
}

pub const Options = struct {
    contained_in_strict_mode_code: bool = false,
    name: []const u8 = "unknown",
};

pub fn generateBytecode(
    allocator: std.mem.Allocator,
    ast_node: anytype,
    options: Options,
) Executable.Error!Executable {
    // Functions always use the old interpreter for now, so only `generateAndRunBytecode` cjecks the flag.
    var executable = Executable.init(allocator);

    var ctx = codegen.Context.init();
    defer ctx.deinit(executable.allocator);
    ctx.contained_in_strict_mode_code = options.contained_in_strict_mode_code;

    const ast_node_name = comptime blk: {
        var it = std.mem.splitBackwardsScalar(u8, @typeName(@TypeOf(ast_node)), '.');
        break :blk it.first();
    };
    const codegenFn = @field(codegen, "codegen" ++ ast_node_name);
    try codegenFn(ast_node, &executable, &ctx);

    // The VM requires this to be the last instruction.
    try executable.addInstruction(.end, {});

    // Already incremented by one by the last caller
    try executable.environment_lookup_cache.resize(executable.allocator, @intFromEnum(ctx.environment_lookup_cache_index));
    @memset(executable.environment_lookup_cache.items, null);
    try executable.property_lookup_cache.resize(executable.allocator, @intFromEnum(ctx.property_lookup_cache_index));
    @memset(executable.property_lookup_cache.items, null);

    return executable;
}

pub fn generateAndRunBytecode(
    agent: *Agent,
    ast_node: anytype,
    options: Options,
) Agent.Error!Completion {
    if (agent.options.new_interpreter) {
        if (@TypeOf(ast_node) != ast.Script) {
            return agent.throwException(.internal_error, "New interpreter only supports Script for now", .{});
        }
        const result = try interpreter.compileAndRun(agent, .{ .script = &ast_node }, options.name);
        return .normal(result);
    }

    var executable = generateBytecode(
        agent.gc_allocator,
        ast_node,
        options,
    ) catch |err| switch (err) {
        error.IndexOutOfRange => return agent.throwException(
            .internal_error,
            "Bytecode generation failed",
            .{},
        ),
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer executable.deinit();

    if (agent.options.debug.print_bytecode) {
        const stdout = agent.platform.stdout;
        const tty_config = agent.platform.tty_config;
        executable.print(stdout, tty_config) catch {};
        stdout.flush() catch {};
    }

    var vm = try Vm.init(agent, &executable);
    defer vm.deinit();
    return vm.run();
}
