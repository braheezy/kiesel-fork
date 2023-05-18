const std = @import("std");

const execution = @import("../../execution.zig");
const instructions_ = @import("instructions.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Executable = @import("Executable.zig");
const Instruction = instructions_.Instruction;
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

fn fetchInstruction(self: *Self, executable: Executable) ?Instruction {
    const instructions = executable.instructions.items;
    if (self.ip >= instructions.len)
        return null;
    defer self.ip += 1;
    return instructions[self.ip];
}

fn fetchConstant(self: *Self, executable: Executable) Value {
    const constants = executable.constants.items;
    const index = self.fetchIndex(executable);
    return constants[index];
}

fn fetchIndex(self: *Self, executable: Executable) Executable.IndexType {
    const b1 = @enumToInt(self.fetchInstruction(executable).?);
    const b2 = @enumToInt(self.fetchInstruction(executable).?);
    return std.mem.bytesToValue(Executable.IndexType, &[_]u8{ b1, b2 });
}

pub fn run(self: *Self, executable: Executable) !?Value {
    while (self.fetchInstruction(executable)) |instruction| switch (instruction) {
        .jump => self.ip = self.fetchIndex(executable),
        .jump_conditional => {
            const ip_consequent = self.fetchIndex(executable);
            const ip_alternate = self.fetchIndex(executable);
            const value = self.stack.pop();
            self.ip = if (value.toBoolean()) ip_consequent else ip_alternate;
        },
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
