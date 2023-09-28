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
function_expressions: std.ArrayList(ArrowOrOrdinaryFunctionExpression),

pub const ArrowOrOrdinaryFunctionExpression = union(enum) {
    arrow_function: ast.ArrowFunction,
    function_expression: ast.FunctionExpression,
};

pub const IndexType = u16;

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .instructions = std.ArrayList(Instruction).init(allocator),
        .constants = std.ArrayList(Value).init(allocator),
        .identifiers = std.ArrayList(ast.Identifier).init(allocator),
        .function_expressions = std.ArrayList(ArrowOrOrdinaryFunctionExpression).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.instructions.deinit();
    self.constants.deinit();
    self.identifiers.deinit();
    self.function_expressions.deinit();
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

pub fn addFunctionExpression(
    self: *Self,
    function_expression: ArrowOrOrdinaryFunctionExpression,
) !void {
    try self.function_expressions.append(function_expression);
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

pub fn addInstructionWithFunctionExpression(
    self: *Self,
    instruction: Instruction,
    function_expression: ArrowOrOrdinaryFunctionExpression,
) !void {
    std.debug.assert(instruction.hasFunctionExpressionIndex());
    try self.addInstruction(instruction);
    try self.addFunctionExpression(function_expression);
    try self.addIndex(self.function_expressions.items.len - 1);
}

pub const JumpIndex = struct {
    executable: *Self,
    index: usize,

    pub fn setTarget(self: JumpIndex, index: usize) !void {
        const instructions = self.executable.instructions.items;
        if (index >= std.math.maxInt(IndexType)) return error.IndexOutOfRange;
        const bytes = std.mem.toBytes(@as(IndexType, @intCast(index)));
        instructions[self.index] = @enumFromInt(bytes[0]);
        instructions[self.index + 1] = @enumFromInt(bytes[1]);
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
    const bytes = std.mem.toBytes(@as(IndexType, @intCast(index)));
    try self.instructions.append(@enumFromInt(bytes[0]));
    try self.instructions.append(@enumFromInt(bytes[1]));
}

pub fn print(self: Self, writer: anytype) !void {
    const file = if (@TypeOf(writer.context) == std.fs.File)
        writer.context
    else
        std.io.getStdOut();
    const tty_config = std.io.tty.detectConfig(file);
    var iterator = InstructionIterator{ .instructions = self.instructions.items };
    while (iterator.next()) |instruction| {
        try writer.print("{:>[1]}: ", .{
            iterator.instruction_index,
            @as(usize, @intCast(std.fmt.count("{d}", .{self.instructions.items.len}))),
        });
        try tty_config.setColor(writer, .bold);
        try writer.writeAll(@tagName(instruction));
        try tty_config.setColor(writer, .reset);
        if (instruction.argumentCount() != 0) try writer.writeAll(" ");
        switch (instruction) {
            .apply_string_or_numeric_binary_operator => {
                const operator_type = iterator.instruction_args[0].?;
                const operator: ast.BinaryExpression.Operator = @enumFromInt(operator_type);
                try writer.print("(operator: {s})", .{@tagName(operator)});
            },
            .create_catch_binding => {
                const identifier_index = iterator.instruction_args[0].?;
                const identifier = self.identifiers.items[identifier_index];
                try writer.print("{s} [{}]", .{ identifier, identifier_index });
            },
            .evaluate_call => {
                const argument_count = iterator.instruction_args[0].?;
                const strict = iterator.instruction_args[1].? == 1;
                try writer.print("(argument_count: {}, strict: {})", .{ argument_count, strict });
            },
            .evaluate_new => {
                const argument_count = iterator.instruction_args[0].?;
                try writer.print("(argument_count: {})", .{argument_count});
            },
            .evaluate_property_access_with_expression_key => {
                const strict = iterator.instruction_args[0].? == 1;
                try writer.print("(strict: {})", .{strict});
            },
            .evaluate_property_access_with_identifier_key => {
                const identifier_index = iterator.instruction_args[0].?;
                const strict = iterator.instruction_args[1].? == 1;
                const identifier = self.identifiers.items[identifier_index];
                try writer.print(
                    "{s} [{}] (strict: {})",
                    .{ identifier, identifier_index, strict },
                );
            },
            .array_set_length,
            .array_set_value,
            .instantiate_arrow_function_expression,
            .instantiate_ordinary_function_expression,
            .jump,
            .push_exception_jump_target,
            => {
                const index = iterator.instruction_args[0].?;
                try writer.print("{}", .{index});
            },
            .jump_conditional => {
                const consequent_index = iterator.instruction_args[0].?;
                const alternate_index = iterator.instruction_args[1].?;
                try writer.print("{} {}", .{ consequent_index, alternate_index });
            },
            .load_constant, .store_constant => {
                const constant_index = iterator.instruction_args[0].?;
                const constant = self.constants.items[constant_index];
                try writer.print("{pretty} [{}]", .{ constant, constant_index });
            },
            .object_define_method => {
                const function_expression_index = iterator.instruction_args[0].?;
                const method_type_raw = iterator.instruction_args[1].?;
                const method_type: ast.MethodDefinition.Type = @enumFromInt(method_type_raw);
                try writer.print("[{}] (type: {s})", .{ function_expression_index, @tagName(method_type) });
            },
            .resolve_binding => {
                const identifier_index = iterator.instruction_args[0].?;
                const strict = iterator.instruction_args[1].? == 1;
                const identifier = self.identifiers.items[identifier_index];
                try writer.print("{s} [{}] (strict: {})", .{ identifier, identifier_index, strict });
            },
            else => {},
        }
        try writer.writeAll("\n");
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
    while (iterator.next()) |instruction| if (@field(Instruction, has_index_getter)(instruction)) {
        const item = items[iterator.instruction_args[0].?];
        const index = for (deduplicated_list.items, 0..) |other_item, index| {
            if (eql(item, other_item)) break index;
        } else blk: {
            try deduplicated_list.append(item);
            break :blk deduplicated_list.items.len - 1;
        };
        const bytes = std.mem.toBytes(@as(IndexType, @intCast(index)));
        self.instructions.items[iterator.instruction_index + 1] = @enumFromInt(bytes[0]);
        self.instructions.items[iterator.instruction_index + 2] = @enumFromInt(bytes[1]);
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
