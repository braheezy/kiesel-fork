const instructions = @import("bytecode/instructions.zig");

pub const Executable = @import("bytecode/Executable.zig");
pub const Instruction = instructions.Instruction;
pub const InstructionIterator = instructions.InstructionIterator;
pub const Vm = @import("bytecode/Vm.zig");

test {
    _ = instructions;

    _ = Executable;
    _ = Vm;
}
