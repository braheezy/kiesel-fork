const std = @import("std");

const Executable = @import("Executable.zig");
const IndexType = Executable.IndexType;

pub const Instruction = enum(u8) {
    const Self = @This();

    /// Jump to another instruction by setting the instruction pointer.
    jump,
    /// Jump to one of two other instructions depending on whether the last value on the stack is
    /// truthy or not.
    jump_conditional,
    /// Load the result value and add it to the stack.
    load,
    /// Load a constant and add it to the stack.
    load_constant,
    /// Store ResolveBinding() as the result value.
    resolve_binding,
    /// Store ResolveThisBinding() as the result value.
    resolve_this_binding,
    /// Store the last value from the stack as the result value.
    store,
    /// Store a constant as the result value.
    store_constant,
    /// Throw the last value from the stack as an exception.
    throw,
    /// Non-exhaustive enum to allow arbitrary values as constant indices.
    _,

    pub fn argumentCount(self: Self) u2 {
        return switch (self) {
            .jump_conditional => 2,
            .jump, .load_constant, .resolve_binding, .store_constant => 1,
            else => 0,
        };
    }

    pub fn hasConstantIndex(self: Self) bool {
        return switch (self) {
            .load_constant, .resolve_binding, .store_constant => true,
            else => false,
        };
    }
};

pub const InstructionIterator = struct {
    const Self = @This();

    instructions: []const Instruction,
    index: usize = 0,
    instruction_index: usize = 0,
    instruction_args: [2]?IndexType = [_]?IndexType{ null, null },

    pub fn next(self: *Self) ?Instruction {
        if (self.index >= self.instructions.len)
            return null;
        const instruction = self.instructions[self.index];
        self.instruction_index = self.index;
        self.index += 1;
        self.instruction_args = [_]?IndexType{ null, null };
        for (0..instruction.argumentCount()) |i| {
            const b1 = @enumToInt(self.instructions[self.index]);
            self.index += 1;
            const b2 = @enumToInt(self.instructions[self.index]);
            self.index += 1;
            self.instruction_args[i] = std.mem.bytesToValue(IndexType, &[_]u8{ b1, b2 });
        }
        return instruction;
    }
};
