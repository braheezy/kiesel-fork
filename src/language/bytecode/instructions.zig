const std = @import("std");

const Executable = @import("Executable.zig");
const IndexType = Executable.IndexType;

pub const Instruction = enum(u8) {
    const Self = @This();

    /// Store ApplyStringOrNumericBinaryOperator() as the result value.
    apply_string_or_numeric_binary_operator,
    /// Store ArrayCreate(0) as the result value.
    array_create,
    /// Set an array's value at the given index.
    array_set_value,
    /// Set the length property of an array to the given index.
    array_set_length,
    /// Apply bitwise NOT to the last value on the stack and store it as the result value.
    bitwise_not,
    /// Store EvaluateCall() as the result value.
    /// This instruction has the number of argument values that need to be popped from the stack
    /// (last to first) as an argument, the values on the stack afterwards are the this value and
    /// lastly the function to call.
    evaluate_call,
    /// Store EvaluateNew() as the result value.
    /// This instruction has the number of argument values that need to be popped from the stack
    /// (last to first) as an argument, the value on the stack afterwards is the constructor to
    /// call.
    evaluate_new,
    /// Store EvaluatePropertyAccessWithExpressionKey() as the result value.
    evaluate_property_access_with_expression_key,
    /// Store EvaluatePropertyAccessWithIdentifierKey() as the result value.
    evaluate_property_access_with_identifier_key,
    /// Store GetValue() as the result value.
    get_value,
    /// Compare the last two values on the stack using the '>' operator rules.
    greater_than,
    /// Compare the last two values on the stack using the '>=' operator rules.
    greater_than_equals,
    /// Store HasProperty() as the result value.
    has_property,
    /// Store InstanceofOperator() as the result value.
    instanceof_operator,
    /// Store InstantiateOrdinaryFunctionExpression() as the result value.
    instantiate_ordinary_function_expression,
    /// Store IsLooselyEqual() as the result value.
    is_loosely_equal,
    /// Store IsStrictlyEqual() as the result value.
    is_strictly_equal,
    /// Jump to another instruction by setting the instruction pointer.
    jump,
    /// Jump to one of two other instructions depending on whether the last value on the stack is
    /// truthy or not.
    jump_conditional,
    /// Compare the last two values on the stack using the '<' operator rules.
    less_than,
    /// Compare the last two values on the stack using the '<=' operator rules.
    less_than_equals,
    /// Load the result value and add it to the stack.
    load,
    /// Load a constant and add it to the stack.
    load_constant,
    /// Determine the this value for an upcoming evaluate_call instruction and add it to the stack.
    load_this_value,
    /// Apply logical NOT to the last value on the stack and store it as the result value.
    logical_not,
    /// Store OrdinaryObjectCreate(%Object.prototype%) as the result value.
    object_create,
    /// Set an object's property to the key/value pair from the last two values on the stack.
    object_set_property,
    /// Pop a jump target for uncaught exceptions
    pop_exception_jump_target,
    /// Pop the last stored reference.
    pop_reference,
    /// Push a jump target for uncaught exceptions
    push_exception_jump_target,
    /// Push the last evaluated reference, if any.
    push_reference,
    /// Store ResolveBinding() as the result value.
    resolve_binding,
    /// Store ResolveThisBinding() as the result value.
    resolve_this_binding,
    /// Rethrow the stored exception, if any.
    rethrow_exception_if_any,
    /// Stop bytecode execution, indicating a return from the current function.
    @"return",
    /// Store the last value from the stack as the result value.
    store,
    /// Store a constant as the result value.
    store_constant,
    /// Throw the last value from the stack as an exception.
    throw,
    /// Store ToNumber() as the result value.
    to_number,
    /// Store ToNumeric() as the result value.
    to_numeric,
    /// Apply the typeof operation to the evaluated expression and set it as the result value.
    typeof,
    /// Store Number::unaryMinus() / BigInt::unaryMinus() as the result value.
    unary_minus,
    /// Non-exhaustive enum to allow arbitrary values as constant indices.
    _,

    pub fn argumentCount(self: Self) u2 {
        return switch (self) {
            .evaluate_call,
            .evaluate_property_access_with_identifier_key,
            .jump_conditional,
            .resolve_binding,
            => 2,
            .apply_string_or_numeric_binary_operator,
            .array_set_length,
            .array_set_value,
            .evaluate_new,
            .evaluate_property_access_with_expression_key,
            .instantiate_ordinary_function_expression,
            .jump,
            .load_constant,
            .push_exception_jump_target,
            .store_constant,
            => 1,
            else => 0,
        };
    }

    pub fn hasConstantIndex(self: Self) bool {
        return switch (self) {
            .load_constant, .store_constant => true,
            else => false,
        };
    }

    pub fn hasIdentifierIndex(self: Self) bool {
        return switch (self) {
            .evaluate_property_access_with_identifier_key, .resolve_binding => true,
            else => false,
        };
    }

    pub fn hasFunctionExpressionIndex(self: Self) bool {
        return switch (self) {
            .instantiate_ordinary_function_expression => true,
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
        if (self.index >= self.instructions.len) return null;
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
