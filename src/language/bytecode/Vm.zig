const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Executable = @import("Executable.zig");
const Value = types.Value;

const Self = @This();

agent: *Agent,
ip: usize,
stack: std.ArrayList(Value),
result: ?Value,

pub fn init(agent: *Agent) !Self {
    var stack = std.ArrayList(Value).init(agent.gc_allocator);
    try stack.ensureTotalCapacity(32);
    return .{
        .agent = agent,
        .ip = 0,
        .stack = stack,
        .result = null,
    };
}

pub fn deinit(self: Self) void {
    self.stack.deinit();
}

fn fetchInstruction(self: *Self, executable: Executable) ?Executable.Instruction {
    const instructions = executable.instructions.items;
    if (self.ip >= instructions.len)
        return null;
    defer self.ip += 1;
    return instructions[self.ip];
}

fn fetchConstant(self: *Self, executable: Executable) Value {
    const constants = executable.constants.items;
    const index = @enumToInt(self.fetchInstruction(executable).?);
    return constants[index];
}

pub fn run(self: *Self, executable: Executable) !?Value {
    while (self.fetchInstruction(executable)) |instruction| switch (instruction) {
        .load => try self.stack.append(self.result.?),
        .load_constant => {
            const value = self.fetchConstant(executable);
            try self.stack.append(value);
        },
        .resolve_binding => {
            const name = self.fetchConstant(executable).string;
            const reference = try self.agent.resolveBinding(name, null);
            self.result = try reference.getValue(self.agent);
        },
        .resolve_this_binding => self.result = try self.agent.resolveThisBinding(),
        .store => self.result = self.stack.pop(),
        .store_constant => {
            const value = self.fetchConstant(executable);
            self.result = value;
        },
        _ => unreachable,
    };
    return self.result;
}
