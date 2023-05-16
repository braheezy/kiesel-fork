const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("../ast.zig");
const types = @import("../../types.zig");

const Value = types.Value;

const Self = @This();

instructions: std.ArrayList(Instruction),
constants: std.ArrayList(Value),

pub const Instruction = enum(u8) {
    /// Load the result value and add it to the stack.
    load,
    /// Load a constant and add it to the stack.
    load_constant,
    /// Store ResolveThisBinding() as the result value.
    resolve_this_binding,
    /// Store the last value from the stack as the result value.
    store,
    /// Store a constant as the result value.
    store_constant,
    /// Non-exhaustive enum to allow arbitrary values as constant indices.
    _,

    pub fn hasConstantIndex(self: @This()) bool {
        return self == .load_constant or self == .store_constant;
    }
};

pub fn init(allocator: Allocator) Self {
    return .{
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
    const instructions = self.instructions.items;
    const constants = self.constants.items;
    var index: usize = 0;
    while (index < instructions.len) {
        const instruction = instructions[index];
        index += 1;
        if (instruction.hasConstantIndex()) {
            const constant_index = @enumToInt(instructions[index]);
            index += 1;
            const value = constants[constant_index];
            try writer.print("{s} {} [{}]\n", .{ @tagName(instruction), constant_index, value });
            continue;
        }
        try writer.print("{s}\n", .{@tagName(instruction)});
    }
}
