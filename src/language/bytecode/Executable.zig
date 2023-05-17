const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("../ast.zig");
const types = @import("../../types.zig");

const Value = types.Value;
const sameValue = types.sameValue;

const Self = @This();

allocator: Allocator,
instructions: std.ArrayList(Instruction),
constants: std.ArrayList(Value),

pub const Instruction = enum(u8) {
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
    /// Non-exhaustive enum to allow arbitrary values as constant indices.
    _,

    pub fn hasConstantIndex(self: @This()) bool {
        return switch (self) {
            .load_constant, .resolve_binding, .store_constant => true,
            else => false,
        };
    }
};

pub const InstructionIterator = struct {
    instructions: []const Instruction,
    index: usize = 0,
    constant_index: ?usize = null,

    pub fn next(self: *InstructionIterator) ?Instruction {
        if (self.index >= self.instructions.len)
            return null;
        const instruction = self.instructions[self.index];
        self.index += 1;
        if (instruction.hasConstantIndex()) {
            self.constant_index = @enumToInt(self.instructions[self.index]);
            self.index += 1;
        } else {
            self.constant_index = null;
        }
        return instruction;
    }
};

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

pub fn addInstructionWithConstant(
    self: *Self,
    instruction: Instruction,
    constant: Value,
) !void {
    std.debug.assert(instruction.hasConstantIndex());
    try self.instructions.append(instruction);
    // TODO: Support more than 256 constants
    if (self.constants.items.len >= std.math.maxInt(@typeInfo(Instruction).Enum.tag_type))
        return error.BytecodeGenerationFailed;
    try self.constants.append(constant);
    try self.instructions.append(@intToEnum(
        Instruction,
        self.constants.items.len - 1,
    ));
}

pub fn print(self: Self, writer: anytype) !void {
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |instruction| {
        if (iterator.constant_index) |constant_index| {
            const value = self.constants.items[constant_index];
            try writer.print("{s} {} [{pretty}]\n", .{ @tagName(instruction), constant_index, value });
        } else {
            try writer.print("{s}\n", .{@tagName(instruction)});
        }
    }
}

pub fn optimize(self: *Self) !void {
    try self.deduplicateConstants();
}

fn deduplicateConstants(self: *Self) !void {
    var deduplicated_constants = std.ArrayList(Value).init(self.allocator);
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |_| if (iterator.constant_index) |constant_index| {
        const value = self.constants.items[constant_index];
        const deduplicated_index = for (deduplicated_constants.items, 0..) |other_value, index| {
            if (sameValue(value, other_value))
                break index;
            if (value == .number and other_value == .number and
                value.number.isNan() and other_value.number.isNan())
                break index;
        } else null;
        if (deduplicated_index) |index| {
            self.instructions.items[iterator.index - 1] = @intToEnum(Instruction, index);
        } else {
            try deduplicated_constants.append(value);
        }
    };
    self.constants.deinit();
    self.constants = deduplicated_constants;
}
