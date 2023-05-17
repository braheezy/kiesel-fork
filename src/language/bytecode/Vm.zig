const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Executable = @import("Executable.zig");
const Value = types.Value;

const Self = @This();

agent: *Agent,
stack: std.ArrayList(Value),
result: ?Value,

pub fn init(agent: *Agent) !Self {
    var stack = std.ArrayList(Value).init(agent.gc_allocator);
    try stack.ensureTotalCapacity(32);
    return .{
        .agent = agent,
        .stack = stack,
        .result = null,
    };
}

pub fn deinit(self: Self) void {
    self.stack.deinit();
}

fn fetchInstruction(executable: Executable, ip: *usize) ?Executable.Instruction {
    const instructions = executable.instructions.items;
    if (ip.* >= instructions.len)
        return null;
    defer ip.* += 1;
    return instructions[ip.*];
}

fn fetchConstant(executable: Executable, ip: *usize) Value {
    const constants = executable.constants.items;
    const index = @enumToInt(fetchInstruction(executable, ip).?);
    return constants[index];
}

pub fn run(self: *Self, executable: Executable) !?Value {
    var ip: usize = 0;
    while (fetchInstruction(executable, &ip)) |instruction| switch (instruction) {
        .load => try self.stack.append(self.result.?),
        .load_constant => {
            const value = fetchConstant(executable, &ip);
            try self.stack.append(value);
        },
        .resolve_binding => {
            const name = fetchConstant(executable, &ip).string;
            const reference = try self.agent.resolveBinding(name, null);
            self.result = try reference.getValue(self.agent);
        },
        .resolve_this_binding => self.result = try self.agent.resolveThisBinding(),
        .store => self.result = self.stack.pop(),
        .store_constant => {
            const value = fetchConstant(executable, &ip);
            self.result = value;
        },
        _ => unreachable,
    };
    return self.result;
}
