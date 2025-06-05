const std = @import("std");

const BlockDeclarationInstantiationType = @import("../runtime.zig").BlockDeclarationInstantiationType;
const Executable = @import("Executable.zig");
const IteratorKind = @import("../../types.zig").IteratorKind;
const MethodDefinition = @import("../../language/ast.zig").MethodDefinition;

const AstNodeIndex = Executable.AstNodeIndex;
const ConstantIndex = Executable.ConstantIndex;
const EnvironmentLookupCacheIndex = Executable.EnvironmentLookupCacheIndex;
const IdentifierIndex = Executable.IdentifierIndex;
const InstructionIndex = Executable.InstructionIndex;
const PropertyLookupCacheIndex = Executable.PropertyLookupCacheIndex;

pub const Instruction = union(enum(u8)) {
    /// Store ArrayCreate(0) as the result value.
    array_create: struct {
        length: u32,
    },
    /// Append value on the stack to an array.
    array_push_value,
    /// Set the length property of an array to the given index.
    array_set_length: struct {
        length: u32,
    },
    /// Set an array value directly.
    array_set_value_direct: struct {
        index: u32,
    },
    /// Spread value into an array.
    array_spread_value,
    /// Store Await() as the result value.
    @"await",
    // Store ApplyStringOrNumericBinaryOperator() as the result value.
    // These are split up to save on decoding time and avoid a switch in the fast path.
    binary_operator_add,
    binary_operator_sub,
    binary_operator_mul,
    binary_operator_div,
    binary_operator_mod,
    binary_operator_exp,
    binary_operator_left_shift,
    binary_operator_right_shift,
    binary_operator_unsigned_right_shift,
    binary_operator_bitwise_and,
    binary_operator_bitwise_or,
    binary_operator_bitwise_xor,
    /// Store BindingClassDeclarationEvaluation() as the result value.
    binding_class_declaration_evaluation: AstNodeIndex,
    /// Apply bitwise NOT to the last value on the stack and store it as the result value.
    bitwise_not,
    /// Perform BlockDeclarationInstantiation().
    block_declaration_instantiation: struct {
        ast_node: AstNodeIndex,
        type: BlockDeclarationInstantiationType,
    },
    /// Store ClassDefinitionEvaluation() as the result value.
    class_definition_evaluation: AstNodeIndex,
    /// Create bindings for the given catch parameter.
    create_catch_bindings: AstNodeIndex,
    /// Create an object property iterator for for-in loops.
    create_object_property_iterator,
    /// Create a declarative environment for a with statement.
    create_with_environment,
    /// Decrement the numeric result value by one.
    decrement,
    /// Apply the delete operation to the evaluated expression and set it as the result value.
    delete,
    /// Duplicate the last iterator on the stack.
    dup_iterator,
    /// Duplicate the last reference on the stack.
    dup_reference,
    /// Store EvaluateCall() as the result value.
    /// This instruction has the number of argument values that need to be popped from the stack
    /// (last to first) as an argument, the values on the stack afterwards are the this value and
    /// lastly the function to call.
    evaluate_call: struct {
        arguments: Arguments,
    },
    /// Store EvaluateCall() as the result value, possibly invoking direct eval.
    evaluate_call_direct_eval: struct {
        arguments: Arguments,
        strict: bool,
    },
    /// Store evaluation of a import() call as the result value.
    evaluate_import_call,
    /// Store EvaluateNew() as the result value.
    /// This instruction has the number of argument values that need to be popped from the stack
    /// (last to first) as an argument, the value on the stack afterwards is the constructor to
    /// call.
    evaluate_new: struct {
        arguments: Arguments,
    },
    /// Store EvaluatePropertyAccessWithExpressionKey() as the result value.
    evaluate_property_access_with_expression_key: struct {
        strict: bool,
    },
    /// Store EvaluatePropertyAccessWithExpressionKey() + GetValue() as the result value.
    evaluate_property_access_with_expression_key_direct,
    /// Store EvaluatePropertyAccessWithIdentifierKey() as the result value.
    evaluate_property_access_with_identifier_key: struct {
        strict: bool,
        identifier: IdentifierIndex,
        property_lookup_cache_index: PropertyLookupCacheIndex,
    },
    /// Store EvaluatePropertyAccessWithIdentifierKey() + GetValue() as the result value.
    evaluate_property_access_with_identifier_key_direct: struct {
        identifier: IdentifierIndex,
        property_lookup_cache_index: PropertyLookupCacheIndex,
    },
    /// Store evaluation of a super() call as the result value.
    evaluate_super_call: struct {
        arguments: Arguments,
    },
    /// Perform ForDeclarationBindingInstantiation with the given identifier and constant-ness.
    for_declaration_binding_instantiation: AstNodeIndex,
    /// Store GetIterator() as the result value.
    get_iterator: struct {
        kind: IteratorKind,
    },
    /// Store GetNewTarget() as the result value.
    get_new_target,
    /// Store the import.meta object as the result value.
    get_or_create_import_meta,
    /// Store GetTemplateObject() as the result value.
    get_template_object: AstNodeIndex,
    /// Store GetValue() as the result value.
    get_value,
    /// Compare the last two values on the stack using the '>' operator rules.
    greater_than,
    /// Compare the last two values on the stack using the '>=' operator rules.
    greater_than_equals,
    /// Store '#property in object' as the result value
    has_private_element: IdentifierIndex,
    /// Store HasProperty() as the result value.
    has_property,
    /// Increment the numeric result value by one.
    increment,
    /// Call InitializeBoundName() with the given identifier.
    initialize_bound_name: IdentifierIndex,
    /// Call InitializeReferencedBinding() with the last reference on the reference stack and the result value.
    initialize_referenced_binding,
    /// Store InstanceofOperator() as the result value.
    instanceof_operator,
    /// Store InstantiateArrowFunctionExpression() as the result value.
    instantiate_arrow_function_expression: AstNodeIndex,
    /// Store InstantiateAsyncArrowFunctionExpression() as the result value.
    instantiate_async_arrow_function_expression: AstNodeIndex,
    /// Store InstantiateAsyncFunctionExpression() as the result value.
    instantiate_async_function_expression: AstNodeIndex,
    /// Store InstantiateAsyncGeneratorFunctionExpression() as the result value.
    instantiate_async_generator_function_expression: AstNodeIndex,
    /// Store InstantiateGeneratorFunctionExpression() as the result value.
    instantiate_generator_function_expression: AstNodeIndex,
    /// Store InstantiateOrdinaryFunctionExpression() as the result value.
    instantiate_ordinary_function_expression: AstNodeIndex,
    /// Store IsLooselyEqual() as the result value.
    is_loosely_equal,
    /// Store IsStrictlyEqual() as the result value.
    is_strictly_equal,
    /// Jump to another instruction by setting the instruction pointer.
    jump: InstructionIndex,
    /// Jump to one of two other instructions depending on whether the last value on the stack is
    /// truthy or not.
    jump_conditional: struct {
        consequent: InstructionIndex,
        alternate: InstructionIndex,
    },
    /// Compare the last two values on the stack using the '<' operator rules.
    less_than,
    /// Compare the last two values on the stack using the '<=' operator rules.
    less_than_equals,
    /// Load the result value and add it to the stack.
    load,
    /// Load a constant and add it to the stack.
    load_constant: ConstantIndex,
    /// Load the exception value and add it to the stack.
    load_and_clear_exception,
    /// Load the next method and iterator object from the top-most iterator record.
    load_iterator_next_args,
    /// Determine the this value for an upcoming evaluate_call instruction and add it to the stack.
    load_this_value_for_evaluate_call,
    /// Determine the this value for an upcoming make_super_property_reference instruction and add it to the stack.
    load_this_value_for_make_super_property_reference,
    /// Apply logical NOT to the last value on the stack and store it as the result value.
    logical_not,
    /// Store MakePrivateReference() as the result value.
    make_private_reference: IdentifierIndex,
    /// Store MakePrivateReference() + GetValue() as the result value.
    make_private_reference_direct: IdentifierIndex,
    /// Store MakeSuperPropertyReference() as the result value.
    make_super_property_reference: struct {
        strict: bool,
    },
    /// Store OrdinaryObjectCreate(%Object.prototype%) as the result value.
    object_create,
    /// Set an object's property to the given function expression.
    object_define_method: struct {
        ast_node: AstNodeIndex,
        type: MethodDefinition.Type,
    },
    /// Set an object's property to the key/value pair from the last two values on the stack.
    object_set_property,
    /// Set an object's prototype.
    object_set_prototype,
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
    push_exception_jump_target: InstructionIndex,
    /// Call PutValue() with the last reference on the reference stack and the result value.
    put_value,
    /// Store RegExpCreate() as the result value.
    reg_exp_create,
    /// Store ResolveBinding() as the result value.
    resolve_binding: struct {
        identifier: IdentifierIndex,
        strict: bool,
        environment_lookup_cache_index: EnvironmentLookupCacheIndex,
    },
    /// Store ResolveBinding() + GetValue() as the result value.
    resolve_binding_direct: struct {
        identifier: IdentifierIndex,
        strict: bool,
        environment_lookup_cache_index: EnvironmentLookupCacheIndex,
    },
    /// Resolve a private identifier (#foo) to a private name in the current private environment
    /// and store the underlying symbol as the result value.
    resolve_private_identifier: IdentifierIndex,
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
    store_constant: ConstantIndex,
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
    /// Apply the typeof operation to an identifier and set it as the result value.
    typeof_identifier: struct {
        identifier: IdentifierIndex,
        strict: bool,
    },
    /// Store Number::unaryMinus() / BigInt::unaryMinus() as the result value.
    unary_minus,
    /// Store Yield() as the result value.
    yield,
    /// The last instruction of an executable. Required for using labeled switch loops.
    end,

    pub const Tag = std.meta.Tag(Instruction);

    pub fn Payload(comptime tag: Instruction.Tag) type {
        return @FieldType(Instruction, @tagName(tag));
    }

    pub const Arguments = struct {
        count: u16,
        has_spread: bool = false,
    };
};

pub const InstructionIterator = struct {
    instructions: []const u8,
    index: usize,
    instruction_index: usize,

    pub fn init(instructions: []const u8) InstructionIterator {
        return .{ .instructions = instructions, .index = 0, .instruction_index = 0 };
    }

    pub fn next(self: *InstructionIterator) ?Instruction {
        if (self.index >= self.instructions.len) return null;
        const tag: Instruction.Tag = @enumFromInt(self.instructions[self.index]);
        self.instruction_index = self.index;
        self.index += 1;
        switch (tag) {
            inline else => |comptime_tag| {
                const Payload = Instruction.Payload(comptime_tag);
                const payload_ptr: *align(1) const Payload = @ptrCast(self.instructions[self.index..][0..@sizeOf(Payload)]);
                self.index += @sizeOf(Payload);
                return @unionInit(Instruction, @tagName(comptime_tag), payload_ptr.*);
            },
        }
    }
};
