const std = @import("std");

const Executable = @import("Executable.zig");
const IndexType = Executable.IndexType;

pub const Instruction = enum(u8) {
    const Self = @This();

    /// Store ApplyStringOrNumericBinaryOperator() as the result value.
    apply_string_or_numeric_binary_operator,
    /// Store ArrayCreate(0) as the result value.
    array_create,
    /// Append value on the stack to an array.
    array_push_value,
    /// Set the length property of an array to the given index.
    array_set_length,
    /// Spread value into an array.
    array_spread_value,
    /// Store Await() as the result value.
    @"await",
    /// Store BindingClassDeclarationEvaluation() as the result value.
    binding_class_declaration_evaluation,
    /// Apply bitwise NOT to the last value on the stack and store it as the result value.
    bitwise_not,
    /// Perform BlockDeclarationInstantiation().
    block_declaration_instantiation,
    /// Store ClassDefinitionEvaluation() as the result value.
    class_definition_evaluation,
    /// Create a catch binding for the given name and populate it with the stored exception.
    create_catch_binding,
    /// Create an object property iterator for for-in loops.
    create_object_property_iterator,
    /// Create a declarative environment for a with statement.
    create_with_environment,
    /// Decrement the numeric result value by one.
    decrement,
    /// Apply the delete operation to the evaluated expression and set it as the result value.
    delete,
    /// Store EvaluateCall() as the result value.
    /// This instruction has the number of argument values that need to be popped from the stack
    /// (last to first) as an argument, the values on the stack afterwards are the this value and
    /// lastly the function to call.
    evaluate_call,
    /// Store evaluation of a import() call as the result value.
    evaluate_import_call,
    /// Store EvaluateNew() as the result value.
    /// This instruction has the number of argument values that need to be popped from the stack
    /// (last to first) as an argument, the value on the stack afterwards is the constructor to
    /// call.
    evaluate_new,
    /// Store EvaluatePropertyAccessWithExpressionKey() as the result value.
    evaluate_property_access_with_expression_key,
    /// Store EvaluatePropertyAccessWithIdentifierKey() as the result value.
    evaluate_property_access_with_identifier_key,
    /// Store evaluation of a super() call as the result value.
    evaluate_super_call,
    /// Perform ForDeclarationBindingInstantiation with the given identifier and constant-ness.
    for_declaration_binding_instantiation,
    /// Store GetIterator() as the result value.
    get_iterator,
    /// Store GetNewTarget() as the result value.
    get_new_target,
    /// Store the import.meta object as the result value.
    get_or_create_import_meta,
    /// Store GetTemplateObject() as the result value.
    get_template_object,
    /// Store GetValue() as the result value.
    get_value,
    /// Compare the last two values on the stack using the '>' operator rules.
    greater_than,
    /// Compare the last two values on the stack using the '>=' operator rules.
    greater_than_equals,
    /// Store '#property in object' as the result value
    has_private_element,
    /// Store HasProperty() as the result value.
    has_property,
    /// Increment the numeric result value by one.
    increment,
    /// Call InitializeBoundName() with "*default*" and the result value.
    initialize_default_export,
    /// Call InitializeReferencedBinding() with the last reference on the reference stack and the result value.
    initialize_referenced_binding,
    /// Store InstanceofOperator() as the result value.
    instanceof_operator,
    /// Store InstantiateArrowFunctionExpression() as the result value.
    instantiate_arrow_function_expression,
    /// Store InstantiateAsyncArrowFunctionExpression() as the result value.
    instantiate_async_arrow_function_expression,
    /// Store InstantiateAsyncFunctionExpression() as the result value.
    instantiate_async_function_expression,
    /// Store InstantiateAsyncGeneratorFunctionExpression() as the result value.
    instantiate_async_generator_function_expression,
    /// Store InstantiateGeneratorFunctionExpression() as the result value.
    instantiate_generator_function_expression,
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
    /// Load the next method and iterator object from the top-most iterator record.
    load_iterator_next_args,
    /// Determine the this value for an upcoming evaluate_call instruction and add it to the stack.
    load_this_value_for_evaluate_call,
    /// Determine the this value for an upcoming make_super_property_reference instruction and add it to the stack.
    load_this_value_for_make_super_property_reference,
    /// Apply logical NOT to the last value on the stack and store it as the result value.
    logical_not,
    /// Store MakePrivateReference() as the result value.
    make_private_reference,
    /// Store MakeSuperPropertyReference() as the result value.
    make_super_property_reference,
    /// Store OrdinaryObjectCreate(%Object.prototype%) as the result value.
    object_create,
    /// Set an object's property to the given function expression.
    object_define_method,
    /// Set an object's property to the key/value pair from the last two values on the stack.
    object_set_property,
    /// Spread value into an object.
    object_spread_value,
    /// Pop a jump target for uncaught exceptions
    pop_exception_jump_target,
    /// Pop the last stored iterator.
    pop_iterator,
    /// Pop the last stored lexical environment.
    pop_lexical_environment,
    /// Pop the last stored reference.
    pop_reference,
    /// Push the current lexical environment.
    push_lexical_environment,
    /// Push a jump target for uncaught exceptions
    push_exception_jump_target,
    /// Push the last evaluated iterator, if any.
    push_iterator,
    /// Push the last evaluated reference, if any.
    push_reference,
    /// Call PutValue() with the last reference on the reference stack and the result value.
    put_value,
    /// Store RegExpCreate() as the result value.
    reg_exp_create,
    /// Store ResolveBinding() as the result value.
    resolve_binding,
    /// Resolve a private identifier (#foo) to a private name in the current private environment
    /// and store the underlying symbol as the result value.
    resolve_private_identifier,
    /// Store ResolveThisBinding() as the result value.
    resolve_this_binding,
    /// Restore the last stored lexical environment.
    restore_lexical_environment,
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
    /// Store ToObject() as the result value.
    to_object,
    /// Store ToString() as the result value.
    to_string,
    /// Apply the typeof operation to the evaluated expression and set it as the result value.
    typeof,
    /// Store Number::unaryMinus() / BigInt::unaryMinus() as the result value.
    unary_minus,
    /// Non-exhaustive enum to allow arbitrary values as constant indices.
    _,

    pub fn argumentCount(self: Self) u2 {
        return switch (self) {
            .resolve_binding => 3,
            .evaluate_call,
            .evaluate_property_access_with_identifier_key,
            .jump_conditional,
            .object_define_method,
            => 2,
            .apply_string_or_numeric_binary_operator,
            .array_set_length,
            .binding_class_declaration_evaluation,
            .block_declaration_instantiation,
            .class_definition_evaluation,
            .create_catch_binding,
            .evaluate_new,
            .evaluate_property_access_with_expression_key,
            .evaluate_super_call,
            .for_declaration_binding_instantiation,
            .get_iterator,
            .get_template_object,
            .has_private_element,
            .instantiate_arrow_function_expression,
            .instantiate_async_arrow_function_expression,
            .instantiate_async_function_expression,
            .instantiate_async_generator_function_expression,
            .instantiate_generator_function_expression,
            .instantiate_ordinary_function_expression,
            .jump,
            .load_constant,
            .make_private_reference,
            .make_super_property_reference,
            .push_exception_jump_target,
            .resolve_private_identifier,
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
            .create_catch_binding,
            .evaluate_property_access_with_identifier_key,
            .has_private_element,
            .make_private_reference,
            .resolve_binding,
            .resolve_private_identifier,
            => true,
            else => false,
        };
    }

    pub fn hasAstNodeIndex(self: Self) bool {
        return switch (self) {
            .binding_class_declaration_evaluation,
            .block_declaration_instantiation,
            .class_definition_evaluation,
            .for_declaration_binding_instantiation,
            .get_template_object,
            .instantiate_arrow_function_expression,
            .instantiate_async_arrow_function_expression,
            .instantiate_async_function_expression,
            .instantiate_async_generator_function_expression,
            .instantiate_generator_function_expression,
            .instantiate_ordinary_function_expression,
            .object_define_method,
            => true,
            else => false,
        };
    }
};

pub const InstructionIterator = struct {
    const Self = @This();

    instructions: []const Instruction,
    index: usize = 0,
    instruction_index: usize = 0,
    instruction_args: [3]?IndexType = .{ null, null, null },

    pub fn next(self: *Self) ?Instruction {
        if (self.index >= self.instructions.len) return null;
        const instruction = self.instructions[self.index];
        self.instruction_index = self.index;
        self.index += 1;
        self.instruction_args = .{ null, null, null };
        for (0..instruction.argumentCount()) |i| {
            const b1 = @intFromEnum(self.instructions[self.index]);
            self.index += 1;
            const b2 = @intFromEnum(self.instructions[self.index]);
            self.index += 1;
            self.instruction_args[i] = std.mem.bytesToValue(IndexType, &[_]u8{ b1, b2 });
        }
        return instruction;
    }
};
