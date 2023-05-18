const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("../ast.zig");
const instructions_ = @import("instructions.zig");
const types = @import("../../types.zig");

const Instruction = instructions_.Instruction;
const InstructionIterator = instructions_.InstructionIterator;
const Value = types.Value;
const sameValue = types.sameValue;

const Self = @This();

allocator: Allocator,
instructions: std.ArrayList(Instruction),
constants: std.ArrayList(Value),

pub const IndexType = u16;

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .instructions = std.ArrayList(Instruction).init(allocator),
        .constants = std.ArrayList(Value).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.instructions.deinit();
    self.constants.deinit();
}

pub fn addInstruction(self: *Self, instruction: Instruction) !void {
    try self.instructions.append(instruction);
}

pub fn addConstant(self: *Self, constant: Value) !void {
    try self.constants.append(constant);
}

pub fn addInstructionWithConstant(
    self: *Self,
    instruction: Instruction,
    constant: Value,
) !void {
    std.debug.assert(instruction.hasConstantIndex());
    try self.addInstruction(instruction);
    try self.addConstant(constant);
    try self.addIndex(self.constants.items.len - 1);
}

const JumpIndex = struct {
    executable: *Self,
    index: usize,

    pub fn setTarget(self: JumpIndex, index: usize) !void {
        const instructions = self.executable.instructions.items;
        if (index >= std.math.maxInt(IndexType))
            return error.BytecodeGenerationFailed;
        const bytes = std.mem.toBytes(@intCast(IndexType, index));
        instructions[self.index] = @intToEnum(Instruction, bytes[0]);
        instructions[self.index + 1] = @intToEnum(Instruction, bytes[1]);
    }

    pub fn setTargetHere(self: JumpIndex) !void {
        const instructions = self.executable.instructions.items;
        try self.setTarget(instructions.len);
    }
};

pub fn addJumpIndex(self: *Self) !JumpIndex {
    try self.addIndex(0);
    return .{
        .executable = self,
        .index = self.instructions.items.len - @sizeOf(IndexType),
    };
}

pub fn addIndex(self: *Self, index: usize) !void {
    if (index >= std.math.maxInt(IndexType))
        return error.BytecodeGenerationFailed;
    const bytes = std.mem.toBytes(@intCast(IndexType, index));
    try self.instructions.append(@intToEnum(Instruction, bytes[0]));
    try self.instructions.append(@intToEnum(Instruction, bytes[1]));
}

pub fn print(self: Self, writer: anytype) !void {
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |instruction| {
        try writer.print("{}: ", .{iterator.instruction_index});
        switch (instruction) {
            .jump => try writer.print("{s} {}\n", .{
                @tagName(instruction),
                iterator.instruction_args[0].?,
            }),
            .jump_conditional => try writer.print(
                "{s} {} {}\n",
                .{
                    @tagName(instruction),
                    iterator.instruction_args[0].?,
                    iterator.instruction_args[1].?,
                },
            ),
            .load_constant, .resolve_binding, .store_constant => {
                const constant_index = iterator.instruction_args[0].?;
                const value = self.constants.items[constant_index];
                try writer.print(
                    "{s} {} [{pretty}]\n",
                    .{ @tagName(instruction), constant_index, value },
                );
            },
            else => try writer.print("{s}\n", .{@tagName(instruction)}),
        }
    }
    try writer.print("{}: <end>\n", .{self.instructions.items.len});
}

pub fn optimize(self: *Self) !void {
    try self.deduplicateConstants();
}

fn deduplicateConstants(self: *Self) !void {
    var deduplicated_constants = std.ArrayList(Value).init(self.allocator);
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |instruction| if (instruction.hasConstantIndex()) {
        const value = self.constants.items[iterator.instruction_args[0].?];
        const constant_index = for (deduplicated_constants.items, 0..) |other_value, index| {
            if (sameValue(value, other_value))
                break index;
        } else blk: {
            try deduplicated_constants.append(value);
            break :blk deduplicated_constants.items.len - 1;
        };
        const bytes = std.mem.toBytes(@intCast(IndexType, constant_index));
        self.instructions.items[iterator.instruction_index + 1] = @intToEnum(Instruction, bytes[0]);
        self.instructions.items[iterator.instruction_index + 2] = @intToEnum(Instruction, bytes[1]);
    };
    self.constants.deinit();
    self.constants = deduplicated_constants;
}
