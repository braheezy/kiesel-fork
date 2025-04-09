const std = @import("std");

const ast = @import("../ast.zig");
const execution = @import("../../execution.zig");
const instructions_ = @import("instructions.zig");
const types = @import("../../types.zig");

const Environment = execution.Environment;
const Instruction = instructions_.Instruction;
const InstructionIterator = instructions_.InstructionIterator;
const Object = types.Object;
const String = types.String;
const Value = types.Value;
const sameValue = types.sameValue;

const Executable = @This();

allocator: std.mem.Allocator,
instructions: std.ArrayListUnmanaged(u8),
constants: Value.ArrayHashMapUnmanaged(void, sameValue),
identifiers: String.ArrayHashMapUnmanaged(void),
ast_nodes: std.ArrayListUnmanaged(AstNode),
environment_lookup_cache: std.ArrayListUnmanaged(?Environment.LookupCacheEntry),
property_lookup_cache: std.ArrayListUnmanaged(?Object.Shape.PropertyLookupCacheEntry),

// NOTE: In most cases instructions know which AST node they refer to, so this can be an untagged
// enum. If there can be more than one type we store that information separately.
pub const AstNode = union {
    arrow_function: ast.ArrowFunction,
    async_arrow_function: ast.AsyncArrowFunction,
    async_function_expression: ast.AsyncFunctionExpression,
    async_generator_expression: ast.AsyncGeneratorExpression,
    class_expression: ast.ClassExpression,
    class_declaration: ast.ClassDeclaration,
    function_expression: ast.FunctionExpression,
    generator_expression: ast.GeneratorExpression,
    statement_list: ast.StatementList,
    case_block: ast.CaseBlock,
    template_literal: ast.TemplateLiteral,
    lexical_declaration: ast.LexicalDeclaration,
    catch_parameter: ast.CatchParameter,
};

pub const CastIndexError = error{IndexOutOfRange};
pub const Error = CastIndexError || std.mem.Allocator.Error;

pub const IndexType = u16;

// The name is required to avoid very long names when dumping the bytecode.
fn Index(comptime name: []const u8, comptime BackingType: type) type {
    return enum(BackingType) {
        _,

        pub fn init(index: usize) CastIndexError!@This() {
            return @enumFromInt(std.math.cast(BackingType, index) orelse {
                @branchHint(.cold);
                return error.IndexOutOfRange;
            });
        }

        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print(name ++ "({d})", .{@intFromEnum(self)});
        }
    };
}

pub const AstNodeIndex = Index("AstNodeIndex", u16);
pub const ConstantIndex = Index("ConstantIndex", u16);
pub const EnvironmentLookupCacheIndex = Index("EnvironmentLookupCacheIndex", u16);
pub const IdentifierIndex = Index("IdentifierIndex", u16);
pub const InstructionIndex = Index("InstructionIndex", u16);
pub const PropertyLookupCacheIndex = Index("PropertyLookupCacheIndex", u16);

pub fn init(allocator: std.mem.Allocator) Executable {
    return .{
        .allocator = allocator,
        .instructions = .empty,
        .constants = .empty,
        .identifiers = .empty,
        .ast_nodes = .empty,
        .environment_lookup_cache = .empty,
        .property_lookup_cache = .empty,
    };
}

pub fn deinit(self: *Executable) void {
    self.instructions.deinit(self.allocator);
    self.constants.deinit(self.allocator);
    self.identifiers.deinit(self.allocator);
    self.ast_nodes.deinit(self.allocator);
    self.environment_lookup_cache.deinit(self.allocator);
    self.property_lookup_cache.deinit(self.allocator);
}

pub fn nextInstructionIndex(self: *Executable) CastIndexError!InstructionIndex {
    return InstructionIndex.init(self.instructions.items.len);
}

pub fn addInstruction(
    self: *Executable,
    comptime tag: Instruction.Tag,
    payload: Instruction.Payload(tag),
) std.mem.Allocator.Error!void {
    const Payload = @TypeOf(payload);
    const bytes = try self.instructions.addManyAsSlice(self.allocator, 1 + @sizeOf(Payload));
    bytes[0] = @intFromEnum(tag);
    const payload_ptr: *align(1) Payload = @ptrCast(bytes[1..][0..@sizeOf(Payload)]);
    payload_ptr.* = payload;
}

pub fn addConstant(self: *Executable, constant: Value) Error!ConstantIndex {
    const result = try self.constants.getOrPut(self.allocator, constant);
    return ConstantIndex.init(result.index);
}

pub fn getConstant(self: Executable, index: ConstantIndex) Value {
    return self.constants.keys()[@intFromEnum(index)];
}

pub fn addIdentifier(self: *Executable, identifier: ast.Identifier) Error!IdentifierIndex {
    const string = try String.fromUtf8Alloc(self.allocator, identifier);
    const result = try self.identifiers.getOrPut(self.allocator, string);
    return IdentifierIndex.init(result.index);
}

pub fn getIdentifier(self: Executable, index: IdentifierIndex) *const String {
    return self.identifiers.keys()[@intFromEnum(index)];
}

pub fn addAstNode(self: *Executable, ast_node: AstNode) Error!AstNodeIndex {
    const index = self.ast_nodes.items.len;
    try self.ast_nodes.append(self.allocator, ast_node);
    return AstNodeIndex.init(index);
}

pub fn getAstNode(self: Executable, index: AstNodeIndex) *AstNode {
    return &self.ast_nodes.items[@intFromEnum(index)];
}

pub fn addInstructionWithConstant(
    self: *Executable,
    comptime instruction: Instruction.Tag,
    constant: Value,
) Error!void {
    try self.addInstruction(instruction, try self.addConstant(constant));
}

pub fn addInstructionWithIdentifier(
    self: *Executable,
    comptime tag: Instruction.Tag,
    identifier: ast.Identifier,
) Error!void {
    try self.addInstruction(tag, try self.addIdentifier(identifier));
}

pub fn addInstructionWithAstNode(
    self: *Executable,
    comptime tag: Instruction.Tag,
    ast_node: AstNode,
) Error!void {
    try self.addInstruction(tag, try self.addAstNode(ast_node));
}

pub fn getEnvironmentLookupCacheEntry(
    self: Executable,
    index: EnvironmentLookupCacheIndex,
) *?Environment.LookupCacheEntry {
    return &self.environment_lookup_cache.items[@intFromEnum(index)];
}

pub fn getPropertyLookupCacheEntry(
    self: Executable,
    index: PropertyLookupCacheIndex,
) *?Object.Shape.PropertyLookupCacheEntry {
    return &self.property_lookup_cache.items[@intFromEnum(index)];
}

pub fn DeferredPayload(comptime T: type) type {
    return struct {
        executable: *Executable,
        index: usize,

        pub fn getPtr(self: @This()) *align(1) T {
            return @ptrCast(self.executable.instructions.items[self.index..][0..@sizeOf(T)]);
        }

        pub fn getFieldDeferred(
            self: @This(),
            comptime field_name: []const u8,
        ) DeferredPayload(@FieldType(T, field_name)) {
            const index = self.index + @offsetOf(T, field_name);
            return .{
                .executable = self.executable,
                .index = index,
            };
        }
    };
}

pub fn addInstructionDeferred(
    self: *Executable,
    comptime tag: Instruction.Tag,
) std.mem.Allocator.Error!DeferredPayload(Instruction.Payload(tag)) {
    const Payload = Instruction.Payload(tag);
    const bytes = try self.instructions.addManyAsSlice(self.allocator, 1 + @sizeOf(Payload));
    bytes[0] = @intFromEnum(tag);
    return .{
        .executable = self,
        .index = self.instructions.items.len - bytes[1..].len,
    };
}

pub fn print(self: Executable, writer: anytype, tty_config: std.io.tty.Config) @TypeOf(writer).Error!void {
    const instruction_count = blk: {
        var n: usize = 0;
        var iterator = InstructionIterator.init(self.instructions.items);
        while (iterator.next()) |_| n += 1;
        break :blk n;
    };
    // TODO: Record function/script/... that this executable is for any output it
    try writer.print("Bytecode Executable (instructions: {}, constants: {}, identifiers: {}, AST nodes: {})\n", .{
        instruction_count,
        self.constants.count(),
        self.identifiers.count(),
        self.ast_nodes.items.len,
    });
    var iterator = InstructionIterator.init(self.instructions.items);
    while (iterator.next()) |instruction| {
        try writer.print("{:>[1]}: ", .{
            iterator.instruction_index,
            @as(usize, @intCast(std.fmt.count("{d}", .{self.instructions.items.len}))),
        });
        try tty_config.setColor(writer, .bold);
        try writer.writeAll(@tagName(instruction));
        try tty_config.setColor(writer, .reset);
        switch (instruction) {
            inline else => |payload, comptime_instruction| {
                switch (comptime_instruction) {
                    .evaluate_property_access_with_identifier_key => {
                        try writer.print(" \"{s}\"", .{self.getIdentifier(payload.identifier)});
                    },
                    .load_constant, .store_constant => {
                        try writer.print(" {pretty}", .{self.getConstant(payload)});
                    },
                    .resolve_binding, .resolve_binding_direct => {
                        try writer.print(" \"{s}\"", .{self.getIdentifier(payload.identifier)});
                    },
                    .typeof_identifier => {
                        try writer.print(" \"{s}\"", .{self.getIdentifier(payload.identifier)});
                    },
                    .has_private_element,
                    .initialize_bound_name,
                    .make_private_reference,
                    .make_private_reference_direct,
                    .resolve_private_identifier,
                    => {
                        try writer.print(" \"{s}\"", .{self.getIdentifier(payload)});
                    },
                    else => {},
                }
                switch (@typeInfo(@TypeOf(payload))) {
                    .void => {},
                    .@"enum" => |info| {
                        // Only indices are allowed as standalone payloads
                        comptime std.debug.assert(!info.is_exhaustive);
                        try writer.print(" (index: {})", .{payload});
                    },
                    .@"struct" => |info| {
                        comptime std.debug.assert(!info.is_tuple);
                        try writer.writeAll(" (");
                        inline for (info.fields, 0..) |field, i| {
                            if (i != 0) try writer.writeAll(", ");
                            try writer.print("{s}: ", .{field.name});
                            const value = @field(payload, field.name);
                            if (@typeInfo(field.type) == .@"enum" and @typeInfo(field.type).@"enum".is_exhaustive) {
                                // Omit type name of exhaustive enums
                                try writer.print("{s}", .{@tagName(value)});
                            } else {
                                try writer.print("{}", .{value});
                            }
                        }
                        try writer.writeAll(")");
                    },
                    else => comptime unreachable,
                }
            },
        }
        try writer.writeAll("\n");
    }
}
