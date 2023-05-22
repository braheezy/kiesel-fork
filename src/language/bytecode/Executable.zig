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
identifiers: std.ArrayList(ast.Identifier),

pub const IndexType = u16;

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .instructions = std.ArrayList(Instruction).init(allocator),
        .constants = std.ArrayList(Value).init(allocator),
        .identifiers = std.ArrayList(ast.Identifier).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.instructions.deinit();
    self.constants.deinit();
    self.identifiers.deinit();
}

pub fn addInstruction(self: *Self, instruction: Instruction) !void {
    try self.instructions.append(instruction);
}

pub fn addConstant(self: *Self, constant: Value) !void {
    try self.constants.append(constant);
}

pub fn addIdentifier(self: *Self, identifier: ast.Identifier) !void {
    try self.identifiers.append(identifier);
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

pub fn addInstructionWithIdentifier(
    self: *Self,
    instruction: Instruction,
    identifier: ast.Identifier,
) !void {
    std.debug.assert(instruction.hasIdentifierIndex());
    try self.addInstruction(instruction);
    try self.addIdentifier(identifier);
    try self.addIndex(self.identifiers.items.len - 1);
}

const JumpIndex = struct {
    executable: *Self,
    index: usize,

    pub fn setTarget(self: JumpIndex, index: usize) !void {
        const instructions = self.executable.instructions.items;
        if (index >= std.math.maxInt(IndexType)) return error.IndexOutOfRange;
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
    if (index >= std.math.maxInt(IndexType)) return error.IndexOutOfRange;
    const bytes = std.mem.toBytes(@intCast(IndexType, index));
    try self.instructions.append(@intToEnum(Instruction, bytes[0]));
    try self.instructions.append(@intToEnum(Instruction, bytes[1]));
}

pub fn print(self: Self, writer: anytype) !void {
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |instruction| {
        try writer.print("{}: ", .{iterator.instruction_index});
        switch (instruction) {
            .evaluate_call, .jump, .prepare_call => try writer.print("{s} {}\n", .{
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
            .load_constant, .store_constant => {
                const constant_index = iterator.instruction_args[0].?;
                const value = self.constants.items[constant_index];
                try writer.print(
                    "{s} {} [{pretty}]\n",
                    .{ @tagName(instruction), constant_index, value },
                );
            },
            .resolve_binding => {
                const identifier_index = iterator.instruction_args[0].?;
                const identifier = self.identifiers.items[identifier_index];
                try writer.print(
                    "{s} {} [{s}]\n",
                    .{ @tagName(instruction), identifier_index, identifier },
                );
            },
            else => try writer.print("{s}\n", .{@tagName(instruction)}),
        }
    }
    try writer.print("{}: <end>\n", .{self.instructions.items.len});
}

pub fn optimize(self: *Self) !void {
    try self.deduplicateConstants();
    try self.deduplicateIdentifiers();
}

fn deduplicate(
    self: *Self,
    comptime T: type,
    comptime has_index_getter: []const u8,
    items: []const T,
    eql: fn (T, T) bool,
) !std.ArrayList(T) {
    var deduplicated_list = std.ArrayList(T).init(self.allocator);
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |instruction| if (@field(instruction, has_index_getter)()) {
        const item = items[iterator.instruction_args[0].?];
        const index = for (deduplicated_list.items, 0..) |other_item, index| {
            if (eql(item, other_item)) break index;
        } else blk: {
            try deduplicated_list.append(item);
            break :blk deduplicated_list.items.len - 1;
        };
        const bytes = std.mem.toBytes(@intCast(IndexType, index));
        self.instructions.items[iterator.instruction_index + 1] = @intToEnum(Instruction, bytes[0]);
        self.instructions.items[iterator.instruction_index + 2] = @intToEnum(Instruction, bytes[1]);
    };
    return deduplicated_list;
}

fn deduplicateConstants(self: *Self) !void {
    const deduplicated_constants = try self.deduplicate(
        Value,
        "hasConstantIndex",
        self.constants.items,
        struct {
            fn eql(a: Value, b: Value) bool {
                return sameValue(a, b);
            }
        }.eql,
    );
    self.constants.deinit();
    self.constants = deduplicated_constants;
}

fn deduplicateIdentifiers(self: *Self) !void {
    const deduplicated_identifiers = try self.deduplicate(
        ast.Identifier,
        "hasIdentifierIndex",
        self.identifiers.items,
        struct {
            fn eql(a: ast.Identifier, b: ast.Identifier) bool {
                return std.mem.eql(u8, a, b);
            }
        }.eql,
    );
    self.identifiers.deinit();
    self.identifiers = deduplicated_identifiers;
}
