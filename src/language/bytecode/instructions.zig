pub const Instruction = enum(u8) {
    const Self = @This();

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
    constant_index: ?usize = null,

    pub fn next(self: *Self) ?Instruction {
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
