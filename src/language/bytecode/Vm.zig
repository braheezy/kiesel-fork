const std = @import("std");

const ast = @import("../ast.zig");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const instructions_ = @import("instructions.zig");
const language = @import("../../language.zig");
const runtime = @import("../runtime.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const ClassConstructorFields = builtins.builtin_function.ClassConstructorFields;
const Completion = types.Completion;
const Environment = execution.Environment;
const Executable = @import("Executable.zig");
const ImportedModuleReferrer = language.ImportedModuleReferrer;
const Instruction = instructions_.Instruction;
const Iterator = types.Iterator;
const IteratorKind = types.IteratorKind;
const PropertyKey = types.PropertyKey;
const Reference = types.Reference;
const String = types.String;
const Value = types.Value;
const applyStringOrNumericBinaryOperator = runtime.applyStringOrNumericBinaryOperator;
const arrayCreate = builtins.arrayCreate;
const @"await" = builtins.@"await";
const bindingClassDeclarationEvaluation = runtime.bindingClassDeclarationEvaluation;
const blockDeclarationInstantiation = runtime.blockDeclarationInstantiation;
const classDefinitionEvaluation = runtime.classDefinitionEvaluation;
const createForInIterator = builtins.createForInIterator;
const directEval = runtime.directEval;
const evaluateCall = runtime.evaluateCall;
const evaluateCallGetThisValue = runtime.evaluateCallGetThisValue;
const evaluateNew = runtime.evaluateNew;
const getArrayLength = builtins.array.getArrayLength;
const getIterator = types.getIterator;
const getSuperConstructor = runtime.getSuperConstructor;
const getTemplateObject = runtime.getTemplateObject;
const initializeBoundName = runtime.initializeBoundName;
const instantiateArrowFunctionExpression = runtime.instantiateArrowFunctionExpression;
const instantiateAsyncArrowFunctionExpression = runtime.instantiateAsyncArrowFunctionExpression;
const instantiateAsyncFunctionExpression = runtime.instantiateAsyncFunctionExpression;
const instantiateAsyncGeneratorFunctionExpression = runtime.instantiateAsyncGeneratorFunctionExpression;
const instantiateGeneratorFunctionExpression = runtime.instantiateGeneratorFunctionExpression;
const instantiateOrdinaryFunctionExpression = runtime.instantiateOrdinaryFunctionExpression;
const isLessThan = types.isLessThan;
const isLooselyEqual = types.isLooselyEqual;
const isStrictlyEqual = types.isStrictlyEqual;
const methodDefinitionEvaluation = runtime.methodDefinitionEvaluation;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const newObjectEnvironment = execution.newObjectEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const yield = builtins.yield;

const Vm = @This();

agent: *Agent,
ip: usize,
stack: std.ArrayListUnmanaged(Value),
iterator_stack: std.ArrayListUnmanaged(Iterator),
lexical_environment_stack: std.ArrayListUnmanaged(Environment),
reference_stack: std.ArrayListUnmanaged(Reference),
exception_jump_target_stack: std.ArrayListUnmanaged(usize),
function_arguments: std.ArrayListUnmanaged(Value),
result: ?Value = null,
exception: ?Value = null,
cached_this_value: ?Value = null,

pub fn init(agent: *Agent) std.mem.Allocator.Error!Vm {
    const stack = try std.ArrayListUnmanaged(Value).initCapacity(agent.gc_allocator, 32);
    const function_arguments = try std.ArrayListUnmanaged(Value).initCapacity(agent.gc_allocator, 8);
    return .{
        .agent = agent,
        .ip = 0,
        .stack = stack,
        .iterator_stack = .empty,
        .lexical_environment_stack = .empty,
        .reference_stack = .empty,
        .exception_jump_target_stack = .empty,
        .function_arguments = function_arguments,
        .result = null,
        .exception = null,
        .cached_this_value = null,
    };
}

pub fn deinit(self: *Vm) void {
    self.stack.deinit(self.agent.gc_allocator);
    self.iterator_stack.deinit(self.agent.gc_allocator);
    self.lexical_environment_stack.deinit(self.agent.gc_allocator);
    self.reference_stack.deinit(self.agent.gc_allocator);
    self.exception_jump_target_stack.deinit(self.agent.gc_allocator);
    self.function_arguments.deinit(self.agent.gc_allocator);
}

fn fetchInstructionTag(self: *Vm, executable: Executable) Instruction.Tag {
    defer self.ip += 1;
    return @enumFromInt(executable.instructions.items[self.ip]);
}

fn getArgumentSpreadIndices(self: *Vm) std.mem.Allocator.Error![]const usize {
    const value = self.stack.pop();
    if (value.isUndefined()) return &.{};
    const array = value.asObject();
    const len = getArrayLength(array);
    var argument_spread_indices = try std.ArrayListUnmanaged(usize).initCapacity(self.agent.gc_allocator, len);
    for (0..len) |i| {
        const argument_spread_index = array.getPropertyValueDirect(
            PropertyKey.from(@as(u53, @intCast(i))),
        ).asNumber().i32;
        argument_spread_indices.appendAssumeCapacity(@intCast(argument_spread_index));
    }
    return argument_spread_indices.toOwnedSlice(self.agent.gc_allocator);
}

fn getArguments(self: *Vm, argument_count: usize) Agent.Error![]const Value {
    self.function_arguments.clearRetainingCapacity();
    try self.function_arguments.ensureTotalCapacity(self.agent.gc_allocator, argument_count); // May still resize when spreading
    const argument_spread_indices = try self.getArgumentSpreadIndices();
    defer self.agent.gc_allocator.free(argument_spread_indices);
    for (0..argument_count) |i| {
        const argument = self.stack.pop();
        if (std.mem.indexOfScalar(usize, argument_spread_indices, argument_count - i - 1) == null) {
            try self.function_arguments.insert(self.agent.gc_allocator, 0, argument);
        } else {
            var iterator = try getIterator(self.agent, argument, .sync);
            var n: usize = 0;
            while (try iterator.stepValue()) |value| : (n += 1) {
                try self.function_arguments.insert(self.agent.gc_allocator, n, value);
            }
        }
    }
    return self.function_arguments.items;
}

fn executeArrayCreate(self: *Vm, length: u16, _: Executable) Agent.Error!void {
    const array = try arrayCreate(self.agent, length, null);
    self.result = Value.from(array);
}

fn executeArrayPushValue(self: *Vm, _: Executable) Agent.Error!void {
    const init_value = self.stack.pop();
    const array = self.stack.pop().asObject();
    const index = getArrayLength(array);
    // From ArrayAccumulation:
    // 4. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), initValue).
    array.createDataPropertyOrThrow(
        PropertyKey.from(@as(PropertyKey.IntegerIndex, index)),
        init_value,
    ) catch |err| try noexcept(err);
    self.result = Value.from(array);
}

fn executeArraySetLength(self: *Vm, length: u16, _: Executable) Agent.Error!void {
    const array = self.result.?.asObject();
    // From ArrayAccumulation:
    // 2. Perform ? Set(array, "length", 𝔽(len), true).
    try array.set(PropertyKey.from("length"), Value.from(length), .throw);
}

fn executeArraySetValueDirect(self: *Vm, index: u16, _: Executable) Agent.Error!void {
    const value = self.stack.pop();
    const array = self.stack.pop().asObject();
    try array.property_storage.indexed_properties.set(self.agent.gc_allocator, index, .{
        .value_or_accessor = .{
            .value = value,
        },
        .attributes = .all,
    });
    self.result = Value.from(array);
}

fn executeArraySpreadValue(self: *Vm, _: Executable) Agent.Error!void {
    const array = self.stack.pop().asObject();
    var next_index: u53 = @intCast(getArrayLength(array));

    // From ArrayAccumulation:
    // 3. Let iteratorRecord be ? GetIterator(spreadObj, sync).
    var iterator = self.iterator_stack.pop();

    // 4. Repeat,
    //     a. Let next be ? IteratorStepValue(iteratorRecord).
    //     b. If next is done, return nextIndex.
    while (try iterator.stepValue()) |next| : (next_index += 1) {
        // c. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), next).
        array.createDataPropertyOrThrow(
            PropertyKey.from(next_index),
            next,
        ) catch |err| try noexcept(err);

        // d. Set nextIndex to nextIndex + 1.
    }
    self.result = Value.from(array);
}

fn executeAwait(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = try @"await"(self.agent, value);
}

fn executeBinaryOperatorAdd(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            if (std.math.add(i32, l_val.__asI32(), r_val.__asI32())) |result| {
                self.result = Value.from(result);
                return;
            } else |_| {}
        }
        self.result = Value.from(l_val.__toF64() + r_val.__toF64());
        return;
    }

    // OPTIMIZATION: Fast path for string values
    else if (l_val.isString() and r_val.isString()) {
        self.result = Value.from(
            try String.concat(self.agent.gc_allocator, &.{ l_val.asString(), r_val.asString() }),
        );
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"+", r_val);
}

fn executeBinaryOperatorSub(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            if (std.math.sub(i32, l_val.__asI32(), r_val.__asI32())) |result| {
                self.result = Value.from(result);
                return;
            } else |_| {}
        }
        self.result = Value.from(l_val.__toF64() - r_val.__toF64());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"-", r_val);
}

fn executeBinaryOperatorMul(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            if (std.math.mul(i32, l_val.__asI32(), r_val.__asI32())) |result| {
                self.result = Value.from(result);
                return;
            } else |_| {}
        }
        self.result = Value.from(l_val.__toF64() * r_val.__toF64());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"*", r_val);
}

fn executeBinaryOperatorDiv(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        self.result = Value.from(l_val.__toF64() / r_val.__toF64());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"/", r_val);
}

fn executeBinaryOperatorMod(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"%", r_val);
}

fn executeBinaryOperatorExp(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"**", r_val);
}

fn executeBinaryOperatorLeftShift(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"<<", r_val);
}

fn executeBinaryOperatorRightShift(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@">>", r_val);
}

fn executeBinaryOperatorUnsignedRightShift(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@">>>", r_val);
}

fn executeBinaryOperatorBitwiseAnd(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        self.result = Value.from(l_val.__asI32() & r_val.__asI32());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"&", r_val);
}

fn executeBinaryOperatorBitwiseOr(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        self.result = Value.from(l_val.__asI32() | r_val.__asI32());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"|", r_val);
}

fn executeBinaryOperatorBitwiseXor(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        self.result = Value.from(l_val.__asI32() ^ r_val.__asI32());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"^", r_val);
}

fn executeBindingClassDeclarationEvaluation(
    self: *Vm,
    ast_node_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const class_declaration = executable.getAstNode(ast_node_index).class_declaration;
    self.result = Value.from(try bindingClassDeclarationEvaluation(self.agent, class_declaration));
}

fn executeBitwiseNot(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: Fast path for i32 values
    if (value.__isI32()) {
        self.result = Value.from(~value.__asI32());
        return;
    }

    self.result = switch (value.type()) {
        .number => Value.from(value.asNumber().bitwiseNOT()),
        .big_int => Value.from(try value.asBigInt().bitwiseNOT(self.agent)),
        else => unreachable,
    };
}

fn executeBlockDeclarationInstantiation(
    self: *Vm,
    block_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const block = executable.getAstNode(block_index);
    const old_env = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;
    const block_env = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);
    try blockDeclarationInstantiation(
        self.agent,
        switch (block.*) {
            .statement_list => |statement_list| .{ .statement_list = statement_list },
            .case_block => |case_block| .{ .case_block = case_block },
            else => unreachable,
        },
        .{ .declarative_environment = block_env },
    );
    self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{
        .declarative_environment = block_env,
    };
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
fn executeClassDefinitionEvaluation(
    self: *Vm,
    class_expression_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const class_expression = executable.getAstNode(class_expression_index).class_expression;

    // ClassExpression : class BindingIdentifier ClassTail
    if (class_expression.identifier) |identifier| {
        // 1. Let className be the StringValue of BindingIdentifier.
        const class_name = try String.fromUtf8(self.agent.gc_allocator, identifier);

        // 2. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments className and className.
        const value = try classDefinitionEvaluation(
            self.agent,
            class_expression.class_tail,
            class_name,
            class_name,
        );

        // 3. Set value.[[SourceText]] to the source text matched by ClassExpression.
        if (value.is(builtins.ECMAScriptFunction)) {
            value.as(builtins.ECMAScriptFunction).fields.source_text = class_expression.source_text;
        } else if (value.is(builtins.BuiltinFunction)) {
            const class_constructor_fields = value.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
            class_constructor_fields.source_text = class_expression.source_text;
        } else unreachable;

        // 4. Return value.
        self.result = Value.from(value);
    }
    // ClassExpression : class ClassTail
    else {
        // 1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and "".
        const value = try classDefinitionEvaluation(
            self.agent,
            class_expression.class_tail,
            null,
            .empty,
        );

        // 2. Set value.[[SourceText]] to the source text matched by ClassExpression.
        if (value.is(builtins.ECMAScriptFunction)) {
            value.as(builtins.ECMAScriptFunction).fields.source_text = class_expression.source_text;
        } else if (value.is(builtins.BuiltinFunction)) {
            const class_constructor_fields = value.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
            class_constructor_fields.source_text = class_expression.source_text;
        } else unreachable;

        // 3. Return value.
        self.result = Value.from(value);
    }
}

fn executeCreateCatchBindings(
    self: *Vm,
    catch_parameter_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const catch_parameter = executable.getAstNode(catch_parameter_index).catch_parameter;

    // From CatchClauseEvaluation:
    // 1. Let oldEnv be the running execution context's LexicalEnvironment.
    const old_env = self.lexical_environment_stack.getLast();

    // 2. Let catchEnv be NewDeclarativeEnvironment(oldEnv).
    const catch_env = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);

    var bound_names: std.ArrayListUnmanaged(ast.Identifier) = .empty;
    defer bound_names.deinit(self.agent.gc_allocator);
    try catch_parameter.collectBoundNames(self.agent.gc_allocator, &bound_names);

    // 3. For each element argName of the BoundNames of CatchParameter, do
    for (bound_names.items) |arg_name_utf8| {
        const arg_name = try String.fromUtf8(self.agent.gc_allocator, arg_name_utf8);

        // a. Perform ! catchEnv.CreateMutableBinding(argName, false).
        catch_env.createMutableBinding(self.agent, arg_name, false) catch |err| try noexcept(err);
    }

    // 4. Set the running execution context's LexicalEnvironment to catchEnv.
    self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{
        .declarative_environment = catch_env,
    };
}

fn executeCreateObjectPropertyIterator(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // From ForIn/OfHeadEvaluation:
    // b. Let obj be ! ToObject(exprValue).
    const object = value.toObject(self.agent) catch |err| try noexcept(err);

    // c. Let iterator be EnumerateObjectProperties(obj).
    const iterator = try createForInIterator(self.agent, object);

    // d. Let nextMethod be ! GetV(iterator, "next").
    const next_method = iterator.get(PropertyKey.from("next")) catch |err| try noexcept(err);

    // e. Return the Iterator Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false }.
    const iterator_: Iterator = .{
        .iterator = iterator,
        .next_method = next_method,
        .done = false,
    };
    try self.iterator_stack.append(self.agent.gc_allocator, iterator_);
}

fn executeCreateWithEnvironment(self: *Vm, _: Executable) Agent.Error!void {
    const object = self.result.?.asObject();
    const old_env = self.lexical_environment_stack.getLast();
    const new_env: Environment = .{
        .object_environment = try newObjectEnvironment(
            self.agent.gc_allocator,
            object,
            true,
            old_env,
        ),
    };
    self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = new_env;
}

fn executeDecrement(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: Fast path for number values
    if (value.isNumber()) {
        if (value.__isI32()) {
            if (std.math.sub(i32, value.__asI32(), 1)) |result| {
                self.result = Value.from(result);
                return;
            } else |_| {}
        }
        self.result = Value.from(value.__toF64() - 1);
        return;
    }

    self.result = Value.from(try value.asBigInt().subtract(self.agent, self.agent.pre_allocated.one));
}

/// 13.5.1.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-delete-operator-runtime-semantics-evaluation
fn executeDelete(self: *Vm, _: Executable) Agent.Error!void {
    // NOTE: 1-2. are part of the generated bytecode.
    const reference = self.reference_stack.pop();

    // 3. If IsUnresolvableReference(ref) is true, then
    if (reference.isUnresolvableReference()) {
        // a. Assert: ref.[[Strict]] is false.
        std.debug.assert(!reference.strict);

        // b. Return true.
        self.result = Value.from(true);
        return;
    }

    // 4. If IsPropertyReference(ref) is true, then
    if (reference.isPropertyReference()) {
        // a. Assert: IsPrivateReference(ref) is false.
        std.debug.assert(!reference.isPrivateReference());

        // b. If IsSuperReference(ref) is true, throw a ReferenceError exception.
        if (reference.isSuperReference()) {
            return self.agent.throwException(
                .reference_error,
                "Cannot delete super reference",
                .{},
            );
        }

        // c. Let baseObj be ? ToObject(ref.[[Base]]).
        const base_obj = try reference.base.value.toObject(self.agent);

        // d. If ref.[[ReferencedName]] is not a property key, then
        const property_key = switch (reference.referenced_name.value.type()) {
            .string => PropertyKey.from(reference.referenced_name.value.asString()),
            .symbol => PropertyKey.from(reference.referenced_name.value.asSymbol()),
            // i. Set ref.[[ReferencedName]] to ? ToPropertyKey(ref.[[ReferencedName]]).
            else => try reference.referenced_name.value.toPropertyKey(self.agent),
        };

        // e. Let deleteStatus be ? baseObj.[[Delete]](ref.[[ReferencedName]]).
        const delete_status = try base_obj.internal_methods.delete(base_obj, property_key);

        // f. If deleteStatus is false and ref.[[Strict]] is true, throw a TypeError exception.
        if (!delete_status and reference.strict) {
            return self.agent.throwException(.type_error, "Could not delete property", .{});
        }

        // g. Return deleteStatus.
        self.result = Value.from(delete_status);
    }
    // 5. Else,
    else {
        // a. Let base be ref.[[Base]].
        // b. Assert: base is an Environment Record.
        const base = reference.base.environment;

        // c. Return ? base.DeleteBinding(ref.[[ReferencedName]]).
        self.result = Value.from(try base.deleteBinding(reference.referenced_name.value.asString()));
    }
}

fn executeDupIterator(self: *Vm, _: Executable) Agent.Error!void {
    const iterator = self.iterator_stack.getLast();
    try self.iterator_stack.append(self.agent.gc_allocator, iterator);
}

fn executeDupReference(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.getLast();
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

fn executeEvaluateCall(self: *Vm, argument_count: u16, _: Executable) Agent.Error!void {
    const arguments = try self.getArguments(argument_count);
    const this_value = self.stack.pop();
    const function = self.stack.pop();

    self.result = try evaluateCall(
        self.agent,
        function,
        this_value,
        arguments,
    );
}

fn executeEvaluateCallDirectEval(
    self: *Vm,
    argument_count: u16,
    strict: bool,
    _: Executable,
) Agent.Error!void {
    const arguments = try self.getArguments(argument_count);
    const this_value = self.stack.pop();
    const function = self.stack.pop();

    const realm = self.agent.currentRealm();
    const eval = try realm.intrinsics.@"%eval%"();

    // a. If SameValue(func, %eval%) is true, then
    if (function.sameValue(Value.from(eval))) {
        self.result = try directEval(self.agent, arguments, strict);
        return;
    }

    self.result = try evaluateCall(
        self.agent,
        function,
        this_value,
        arguments,
    );
}

/// 13.3.10.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-import-call-runtime-semantics-evaluation
fn executeEvaluateImportCall(self: *Vm, _: Executable) Agent.Error!void {
    const realm = self.agent.currentRealm();

    // 1. Let referrer be GetActiveScriptOrModule().
    // 2. If referrer is null, set referrer to the current Realm Record.
    const referrer: ImportedModuleReferrer = if (self.agent.getActiveScriptOrModule()) |script_or_module|
        switch (script_or_module) {
            .script => |script| .{ .script = script },
            .module => |module| .{ .module = module },
        }
    else
        .{ .realm = realm };

    // 3. Let argRef be ? Evaluation of AssignmentExpression.
    // 4. Let specifier be ? GetValue(argRef).
    const specifier = self.stack.pop();

    // 5. Let promiseCapability be ! NewPromiseCapability(%Promise%).
    const promise_capability = newPromiseCapability(
        self.agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 6. Let specifierString be Completion(ToString(specifier)).
    const specifier_string = specifier.toString(self.agent) catch |err| {
        // 7. IfAbruptRejectPromise(specifierString, promiseCapability).
        self.result = Value.from(try promise_capability.rejectPromise(self.agent, err));
        return;
    };

    // 8. Perform HostLoadImportedModule(referrer, specifierString, empty, promiseCapability).
    try self.agent.host_hooks.hostLoadImportedModule(
        self.agent,
        referrer,
        specifier_string,
        .null_pointer,
        .{ .promise_capability = promise_capability },
    );

    // 9. Return promiseCapability.[[Promise]].
    self.result = Value.from(promise_capability.promise);
}

fn executeEvaluateNew(self: *Vm, argument_count: u16, _: Executable) Agent.Error!void {
    const arguments = try self.getArguments(argument_count);
    const constructor = self.stack.pop();
    self.result = try evaluateNew(self.agent, constructor, arguments);
}

/// 13.3.3 EvaluatePropertyAccessWithExpressionKey ( baseValue, expression, strict )
/// https://tc39.es/ecma262/#sec-evaluate-property-access-with-expression-key
fn executeEvaluatePropertyAccessWithExpressionKey(
    self: *Vm,
    strict: bool,
    _: Executable,
) Agent.Error!void {
    // 1. Let propertyNameReference be ? Evaluation of expression.
    // 2. Let propertyNameValue be ? GetValue(propertyNameReference).
    const property_name_value = self.stack.pop();

    const base_value = self.stack.pop();

    // 3. NOTE: In most cases, ToPropertyKey will be performed on propertyNameValue
    //    immediately after this step. However, in the case of a[b] = c, it will not be
    //    performed until after evaluation of c.

    // 4. Return the Reference Record {
    //      [[Base]]: baseValue,
    //      [[ReferencedName]]: propertyNameValue,
    //      [[Strict]]: strict,
    //      [[ThisValue]]: empty
    //    }.
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{ .value = property_name_value },
        .strict = strict,
        .this_value = null,
    };
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

/// 13.3.4 EvaluatePropertyAccessWithIdentifierKey ( baseValue, identifierName, strict )
/// https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key
fn executeEvaluatePropertyAccessWithIdentifierKey(
    self: *Vm,
    strict: bool,
    identifier_name_index: Executable.IdentifierIndex,
    property_name_lookup_cache_index: Executable.PropertyLookupCacheIndex,
    executable: Executable,
) Agent.Error!void {
    // 1. Let propertyNameString be the StringValue of identifierName.
    const property_name_string = executable.getIdentifier(identifier_name_index);

    const lookup_cache_entry = executable.getPropertyLookupCacheEntry(property_name_lookup_cache_index);
    const base_value = self.stack.pop();

    // 2. Return the Reference Record {
    //      [[Base]]: baseValue,
    //      [[ReferencedName]]: propertyNameString,
    //      [[Strict]]: strict,
    //      [[ThisValue]]: empty
    //    }.
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{
            .value = Value.from(property_name_string),
        },
        .strict = strict,
        .this_value = null,
        .maybe_lookup_cache_entry = lookup_cache_entry,
    };
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

/// 13.3.7.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-super-keyword-runtime-semantics-evaluation
fn executeEvaluateSuperCall(self: *Vm, argument_count: u16, _: Executable) Agent.Error!void {
    // 1. Let newTarget be GetNewTarget().
    const new_target = self.agent.getNewTarget();

    // 2. Assert: newTarget is an Object.
    std.debug.assert(new_target != null);

    // 3. Let func be GetSuperConstructor().
    const function = try getSuperConstructor(self.agent);

    // 4. Let argList be ? ArgumentListEvaluation of Arguments.
    const arguments = try self.getArguments(argument_count);

    // 5. If IsConstructor(func) is false, throw a TypeError exception.
    if (!function.isConstructor()) {
        return self.agent.throwException(
            .type_error,
            "{} is not a constructor",
            .{function},
        );
    }

    // 6. Let result be ? Construct(func, argList, newTarget).
    var result = try function.asObject().construct(arguments, new_target);

    // 7. Let thisER be GetThisEnvironment().
    const this_environment = self.agent.getThisEnvironment();

    // 8. Perform ? thisER.BindThisValue(result).
    _ = try this_environment.bindThisValue(Value.from(result));

    // 9. Let F be thisER.[[FunctionObject]].
    // 10. Assert: F is an ECMAScript function object.
    const constructor = &this_environment.function_environment.function_object.object;

    // 11. Perform ? InitializeInstanceElements(result, F).
    try result.initializeInstanceElements(constructor);

    // 12. Return result.
    self.result = Value.from(result);
}

fn executeForDeclarationBindingInstantiation(
    self: *Vm,
    lexical_declaration_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const lexical_declaration = executable.getAstNode(lexical_declaration_index).lexical_declaration;

    // From ForIn/OfBodyEvaluation:
    // iii. Let iterationEnv be NewDeclarativeEnvironment(oldEnv).
    const old_env = self.lexical_environment_stack.getLast();
    const environment = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);

    // iv. Perform ForDeclarationBindingInstantiation of lhs with argument iterationEnv.

    // 14.7.5.4 Runtime Semantics: ForDeclarationBindingInstantiation
    // https://tc39.es/ecma262/#sec-runtime-semantics-fordeclarationbindinginstantiation
    var bound_names: std.ArrayListUnmanaged(ast.Identifier) = .empty;
    defer bound_names.deinit(self.agent.gc_allocator);
    try lexical_declaration.collectBoundNames(self.agent.gc_allocator, &bound_names);

    // 1. For each element name of the BoundNames of ForBinding, do
    for (bound_names.items) |name_utf8| {
        const name = try String.fromUtf8(self.agent.gc_allocator, name_utf8);

        // a. If IsConstantDeclaration of LetOrConst is true, then
        if (lexical_declaration.isConstantDeclaration()) {
            // i. Perform ! environment.CreateImmutableBinding(name, true).
            environment.createImmutableBinding(
                self.agent,
                name,
                true,
            ) catch |err| try noexcept(err);
        }
        // b. Else,
        else {
            // i. Perform ! environment.CreateMutableBinding(name, false).
            environment.createMutableBinding(
                self.agent,
                name,
                false,
            ) catch |err| try noexcept(err);
        }

        // 2. Return unused.
    }

    // v. Set the running execution context's LexicalEnvironment to iterationEnv.
    self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{
        .declarative_environment = environment,
    };
}

fn executeGetIterator(self: *Vm, iterator_kind: IteratorKind, _: Executable) Agent.Error!void {
    const iterator = try getIterator(self.agent, self.result.?, iterator_kind);
    try self.iterator_stack.append(self.agent.gc_allocator, iterator);
}

fn executeGetNewTarget(self: *Vm, _: Executable) Agent.Error!void {
    self.result = if (self.agent.getNewTarget()) |new_target|
        Value.from(new_target)
    else
        .undefined;
}

/// 13.3.12.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-meta-properties-runtime-semantics-evaluation
fn executeGetOrCreateImportMeta(self: *Vm, _: Executable) Agent.Error!void {
    // 1. Let module be GetActiveScriptOrModule().
    // 2. Assert: module is a Source Text Module Record.
    var module = self.agent.getActiveScriptOrModule().?.module;

    // 3. Let importMeta be module.[[ImportMeta]].
    // 4. If importMeta is empty, then
    if (module.import_meta == null) {
        // a. Set importMeta to OrdinaryObjectCreate(null).
        const import_meta = try ordinaryObjectCreate(self.agent, null);

        // b. Let importMetaValues be HostGetImportMetaProperties(module).
        var import_meta_values = try self.agent.host_hooks.hostGetImportMetaProperties(
            self.agent,
            module,
        );
        defer import_meta_values.deinit(self.agent.gc_allocator);

        // c. For each Record { [[Key]], [[Value]] } p of importMetaValues, do
        var it = import_meta_values.iterator();
        while (it.next()) |entry| {
            // i. Perform ! CreateDataPropertyOrThrow(importMeta, p.[[Key]], p.[[Value]]).
            import_meta.createDataPropertyOrThrow(
                entry.key_ptr.*,
                entry.value_ptr.*,
            ) catch |err| try noexcept(err);
        }

        // d. Perform HostFinalizeImportMeta(importMeta, module).
        self.agent.host_hooks.hostFinalizeImportMeta(import_meta, module);

        // e. Set module.[[ImportMeta]] to importMeta.
        module.import_meta = import_meta;

        // f. Return importMeta.
        self.result = Value.from(import_meta);
    }
    // 5. Else,
    else {
        // a. Assert: importMeta is an Object.
        // b. Return importMeta.
        self.result = Value.from(module.import_meta.?);
    }
}

fn executeGetTemplateObject(
    self: *Vm,
    template_literal_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const template_literal = &executable.getAstNode(template_literal_index).template_literal;
    self.result = Value.from(try getTemplateObject(self.agent, template_literal));
}

fn executeGetValue(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.pop();
    self.result = try reference.getValue(self.agent);
}

fn executeGreaterThan(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            self.result = Value.from(l_val.__asI32() > r_val.__asI32());
        } else {
            self.result = Value.from(l_val.__toF64() > r_val.__toF64());
        }
        return;
    }

    // 5. Let r be ? IsLessThan(rVal, lVal, false).
    const result = try isLessThan(self.agent, r_val, l_val, .right_first);

    // 6. If r is undefined, return false. Otherwise, return r.
    self.result = Value.from(result orelse false);
}

fn executeGreaterThanEquals(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            self.result = Value.from(l_val.__asI32() >= r_val.__asI32());
        } else {
            self.result = Value.from(l_val.__toF64() >= r_val.__toF64());
        }
        return;
    }

    // 5. Let r be ? IsLessThan(lVal, rVal, true).
    const result = try isLessThan(self.agent, l_val, r_val, .left_first);

    // 6. If r is either true or undefined, return false. Otherwise, return true.
    self.result = Value.from(!(result orelse true));
}

fn executeHasPrivateElement(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
    executable: Executable,
) Agent.Error!void {
    const private_identifier = executable.getIdentifier(private_identifier_index);
    const r_val = self.stack.pop();

    // 4. If rVal is not an Object, throw a TypeError exception.
    if (!r_val.isObject()) {
        return self.agent.throwException(
            .type_error,
            "Right-hand side of 'in' operator must be an object",
            .{},
        );
    }

    // 5. Let privateEnv be the running execution context's PrivateEnvironment.
    // 6. Assert: privateEnv is not null.
    const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

    // 7. Let privateName be ResolvePrivateIdentifier(privateEnv, privateIdentifier).
    const private_name = private_environment.resolvePrivateIdentifier(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    );

    // 8. If PrivateElementFind(rVal, privateName) is not empty, return true.
    // 9. Return false.
    self.result = Value.from(r_val.asObject().privateElementFind(private_name) != null);
}

fn executeHasProperty(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. If rVal is not an Object, throw a TypeError exception.
    if (!r_val.isObject()) {
        return self.agent.throwException(
            .type_error,
            "Right-hand side of 'in' operator must be an object",
            .{},
        );
    }

    // 6. Return ? HasProperty(rVal, ? ToPropertyKey(lVal)).
    self.result = Value.from(
        try r_val.asObject().hasProperty(try l_val.toPropertyKey(self.agent)),
    );
}

fn executeIncrement(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: Fast path for number values
    if (value.isNumber()) {
        if (value.__isI32()) {
            if (std.math.add(i32, value.__asI32(), 1)) |result| {
                self.result = Value.from(result);
                return;
            } else |_| {}
        }
        self.result = Value.from(value.__toF64() + 1);
        return;
    }

    self.result = Value.from(try value.asBigInt().add(self.agent, self.agent.pre_allocated.one));
}

fn executeInitializeBoundName(
    self: *Vm,
    name_index: Executable.IdentifierIndex,
    executable: Executable,
) Agent.Error!void {
    const name = executable.getIdentifier(name_index);
    const value = self.stack.pop();
    const environment = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;
    try initializeBoundName(
        self.agent,
        name,
        value,
        .{ .environment = environment },
    );
}

fn executeInitializeReferencedBinding(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.pop();
    const value = self.result.?;
    try reference.initializeReferencedBinding(self.agent, value);
}

fn executeInstanceofOperator(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. Return ? InstanceofOperator(lVal, rVal).
    self.result = Value.from(try l_val.instanceofOperator(self.agent, r_val));
}

fn executeInstantiateArrowFunctionExpression(
    self: *Vm,
    arrow_function_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const arrow_function = executable.getAstNode(arrow_function_index).arrow_function;
    const closure = try instantiateArrowFunctionExpression(
        self.agent,
        arrow_function,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateAsyncArrowFunctionExpression(
    self: *Vm,
    async_arrow_function_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const async_arrow_function = executable.getAstNode(async_arrow_function_index).async_arrow_function;
    const closure = try instantiateAsyncArrowFunctionExpression(
        self.agent,
        async_arrow_function,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateAsyncFunctionExpression(
    self: *Vm,
    async_function_expression_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const async_function_expression = executable.getAstNode(async_function_expression_index).async_function_expression;
    const closure = try instantiateAsyncFunctionExpression(
        self.agent,
        async_function_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateAsyncGeneratorFunctionExpression(
    self: *Vm,
    async_generator_expression_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const async_generator_expression = executable.getAstNode(async_generator_expression_index).async_generator_expression;
    const closure = try instantiateAsyncGeneratorFunctionExpression(
        self.agent,
        async_generator_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateGeneratorFunctionExpression(
    self: *Vm,
    generator_expression_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const generator_expression = executable.getAstNode(generator_expression_index).generator_expression;
    const closure = try instantiateGeneratorFunctionExpression(
        self.agent,
        generator_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateOrdinaryFunctionExpression(
    self: *Vm,
    function_expression_index: Executable.AstNodeIndex,
    executable: Executable,
) Agent.Error!void {
    const function_expression = executable.getAstNode(function_expression_index).function_expression;
    const closure = try instantiateOrdinaryFunctionExpression(
        self.agent,
        function_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeIsLooselyEqual(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            self.result = Value.from(l_val.__asI32() == r_val.__asI32());
        } else {
            self.result = Value.from(l_val.__toF64() == r_val.__toF64());
        }
        return;
    }

    // 5. Return IsLooselyEqual(rVal, lVal).
    self.result = Value.from(try isLooselyEqual(self.agent, r_val, l_val));
}

fn executeIsStrictlyEqual(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            self.result = Value.from(l_val.__asI32() == r_val.__asI32());
        } else {
            self.result = Value.from(l_val.__toF64() == r_val.__toF64());
        }
        return;
    }

    // 5. Return IsStrictlyEqual(rVal, lVal).
    self.result = Value.from(isStrictlyEqual(r_val, l_val));
}

fn executeJump(self: *Vm, index: Executable.InstructionIndex, _: Executable) Agent.Error!void {
    self.ip = @intFromEnum(index);
}

fn executeJumpConditional(
    self: *Vm,
    ip_consequent: Executable.InstructionIndex,
    ip_alternate: Executable.InstructionIndex,
    _: Executable,
) Agent.Error!void {
    const value = self.result.?;
    self.ip = if (value.toBoolean()) @intFromEnum(ip_consequent) else @intFromEnum(ip_alternate);
}

fn executeLessThan(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            self.result = Value.from(l_val.__asI32() < r_val.__asI32());
        } else {
            self.result = Value.from(l_val.__toF64() < r_val.__toF64());
        }
        return;
    }

    // 5. Let r be ? IsLessThan(lVal, rVal, true).
    const result = try isLessThan(self.agent, l_val, r_val, .left_first);

    // 6. If r is undefined, return false. Otherwise, return r.
    self.result = Value.from(result orelse false);
}

fn executeLessThanEquals(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        if (l_val.__isI32() and r_val.__isI32()) {
            self.result = Value.from(l_val.__asI32() <= r_val.__asI32());
        } else {
            self.result = Value.from(l_val.__toF64() <= r_val.__toF64());
        }
        return;
    }

    // 5. Let r be ? IsLessThan(rVal, lVal, false).
    const result = try isLessThan(self.agent, r_val, l_val, .right_first);

    // 6. If r is either true or undefined, return false. Otherwise, return true.
    self.result = Value.from(!(result orelse true));
}

fn executeLoad(self: *Vm, _: Executable) Agent.Error!void {
    // Handle null value to allow load of 'empty' result at beginning of script
    if (self.result) |value| try self.stack.append(self.agent.gc_allocator, value);
}

fn executeLoadConstant(
    self: *Vm,
    value_index: Executable.ConstantIndex,
    executable: Executable,
) Agent.Error!void {
    const value = executable.getConstant(value_index);
    try self.stack.append(self.agent.gc_allocator, value);
}

fn executeLoadAndClearException(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.exception.?;
    self.exception = null;
    try self.stack.append(self.agent.gc_allocator, value);
}

fn executeLoadIteratorNextArgs(self: *Vm, _: Executable) Agent.Error!void {
    const iterator = self.iterator_stack.pop();
    try self.stack.append(self.agent.gc_allocator, iterator.next_method);
    try self.stack.append(self.agent.gc_allocator, Value.from(iterator.iterator));
}

fn executeLoadThisValueForEvaluateCall(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.pop();
    const this_value = evaluateCallGetThisValue(reference);
    try self.stack.append(self.agent.gc_allocator, this_value);
}

fn executeLoadThisValueForMakeSuperPropertyReference(self: *Vm, _: Executable) Agent.Error!void {
    // 1. Let env be GetThisEnvironment().
    const env = self.agent.getThisEnvironment();

    // 2. Let actualThis be ? env.GetThisBinding().
    const actual_this = try env.getThisBinding();

    try self.stack.append(self.agent.gc_allocator, actual_this);
}

fn executeLogicalNot(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(!value.toBoolean());
}

/// 6.2.5.9 MakePrivateReference ( baseValue, privateIdentifier )
/// https://tc39.es/ecma262/#sec-makeprivatereference
fn executeMakePrivateReference(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
    executable: Executable,
) Agent.Error!void {
    const private_identifier = executable.getIdentifier(private_identifier_index);
    const base_value = self.stack.pop();

    // 1. Let privateEnv be the running execution context's PrivateEnvironment.
    // 2. Assert: privateEnv is not null.
    const private_env = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

    // 3. Let privateName be ResolvePrivateIdentifier(privateEnv, privateIdentifier).
    const private_name = private_env.resolvePrivateIdentifier(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    );

    // 4. Return the Reference Record {
    //      [[Base]]: baseValue, [[ReferencedName]]: privateName, [[Strict]]: true, [[ThisValue]]: empty
    //    }.
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{ .private_name = private_name },
        .strict = true,
        .this_value = null,
    };
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

/// 13.3.7.3 MakeSuperPropertyReference ( actualThis, propertyKey, strict )
/// https://tc39.es/ecma262/#sec-makesuperpropertyreference
fn executeMakeSuperPropertyReference(self: *Vm, strict: bool, _: Executable) Agent.Error!void {
    const property_key = self.stack.pop();
    const actual_this = self.stack.pop();

    // 1. Let env be GetThisEnvironment().
    const env = self.agent.getThisEnvironment();

    // 2. Assert: env.HasSuperBinding() is true.
    std.debug.assert(env.hasSuperBinding());

    // 3. Let baseValue be env.GetSuperBase().
    const base_value = try env.getSuperBase();

    // 4. Return the Reference Record {
    //      [[Base]]: baseValue, [[ReferencedName]]: propertyKey, [[Strict]]: strict, [[ThisValue]]: actualThis
    //    }.
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{ .value = property_key },
        .strict = strict,
        .this_value = actual_this,
    };
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

fn executeObjectCreate(self: *Vm, _: Executable) Agent.Error!void {
    const object = try ordinaryObjectCreate(
        self.agent,
        try self.agent.currentRealm().intrinsics.@"%Object.prototype%"(),
    );
    self.result = Value.from(object);
}

fn executeObjectDefineMethod(
    self: *Vm,
    function_or_class_index: Executable.AstNodeIndex,
    method_type: ast.MethodDefinition.Type,
    executable: Executable,
) Agent.Error!void {
    const function_or_class = executable.getAstNode(function_or_class_index);
    const method = switch (method_type) {
        inline else => |@"type"| @unionInit(
            ast.MethodDefinition.Method,
            @tagName(@"type"),
            @field(function_or_class, switch (@"type") {
                .method, .get, .set => "function_expression",
                .generator => "generator_expression",
                .@"async" => "async_function_expression",
                .async_generator => "async_generator_expression",
            }),
        ),
    };
    const property_name = self.stack.pop();
    const object = self.stack.pop().asObject();
    _ = try methodDefinitionEvaluation(
        self.agent,
        .{ .property_name = property_name, .method = method },
        object,
        true,
    );
    self.result = Value.from(object);
}

fn executeObjectSetProperty(self: *Vm, _: Executable) Agent.Error!void {
    const property_value = self.stack.pop();
    const property_name = try self.stack.pop().toPropertyKey(self.agent);
    const object = self.stack.pop().asObject();
    // From PropertyDefinitionEvaluation:
    // 5. Perform ! CreateDataPropertyOrThrow(object, propName, propValue).
    object.createDataPropertyOrThrow(property_name, property_value) catch |err| try noexcept(err);
    self.result = Value.from(object);
}

fn executeObjectSetPrototype(self: *Vm, _: Executable) Agent.Error!void {
    const property_value = self.stack.pop();
    const object = self.stack.pop().asObject();
    // From PropertyDefinitionEvaluation:
    // a. If propValue is an Object or propValue is null, then
    if (property_value.isObject() or property_value.isNull()) {
        // i. Perform ! object.[[SetPrototypeOf]](propValue).
        const prototype = if (property_value.isObject()) property_value.asObject() else null;
        _ = object.internal_methods.setPrototypeOf(
            object,
            prototype,
        ) catch |err| try noexcept(err);
    }
    self.result = Value.from(object);
}

fn executeObjectSpreadValue(self: *Vm, _: Executable) Agent.Error!void {
    const from_value = self.stack.pop();
    var object = self.stack.pop().asObject();
    const excluded_names: []const PropertyKey = &.{};
    // From PropertyDefinitionEvaluation:
    // 4. Perform ? CopyDataProperties(object, fromValue, excludedNames).
    try object.copyDataProperties(from_value, excluded_names);
    self.result = Value.from(object);
}

fn executePopExceptionJumpTarget(self: *Vm, _: Executable) Agent.Error!void {
    _ = self.exception_jump_target_stack.pop();
}

fn executePopIterator(self: *Vm, _: Executable) Agent.Error!void {
    _ = self.iterator_stack.pop();
}

fn executePopLexicalEnvironment(self: *Vm, _: Executable) Agent.Error!void {
    _ = self.lexical_environment_stack.pop();
}

fn executePopReference(self: *Vm, _: Executable) Agent.Error!void {
    _ = self.reference_stack.pop();
}

fn executePushLexicalEnvironment(self: *Vm, _: Executable) Agent.Error!void {
    const lexical_environment = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;
    try self.lexical_environment_stack.append(self.agent.gc_allocator, lexical_environment);
}

fn executePushExceptionJumpTarget(
    self: *Vm,
    jump_target: Executable.InstructionIndex,
    _: Executable,
) Agent.Error!void {
    try self.exception_jump_target_stack.append(self.agent.gc_allocator, @intFromEnum(jump_target));
}

fn executePutValue(self: *Vm, _: Executable) Agent.Error!void {
    const l_ref = self.reference_stack.pop();
    const r_val = self.result.?;
    try l_ref.putValue(self.agent, r_val);
}

fn executeRegExpCreate(self: *Vm, _: Executable) Agent.Error!void {
    const flags = self.stack.pop();
    const pattern = self.stack.pop();
    self.result = Value.from(try builtins.regExpCreate(self.agent, pattern, flags));
}

fn executeResolveBinding(
    self: *Vm,
    name_index: Executable.IdentifierIndex,
    strict: bool,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
    executable: Executable,
) Agent.Error!void {
    const name = executable.getIdentifier(name_index);
    const lookup_cache_entry = executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
fn executeResolvePrivateIdentifier(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
    executable: Executable,
) Agent.Error!void {
    // 1. Let privateIdentifier be the StringValue of PrivateIdentifier.
    const private_identifier = executable.getIdentifier(private_identifier_index);

    // 2. Let privateEnvRec be the running execution context's PrivateEnvironment.
    const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

    // 3. Let names be privateEnvRec.[[Names]].
    // 4. Assert: Exactly one element of names is a Private Name whose [[Description]] is privateIdentifier.
    // 5. Let privateName be the Private Name in names whose [[Description]] is privateIdentifier.
    // 6. Return privateName.
    const private_name = private_environment.names.get(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    ).?;
    std.debug.assert(private_name.symbol.is_private);
    self.result = Value.from(private_name.symbol);
}

fn executeResolveThisBinding(self: *Vm, _: Executable) Agent.Error!void {
    // NOTE: Caching the this value currently relies on the fact that each function runs in its own
    //       Vm, it'll need to be cleared between calls if that changes.
    if (self.cached_this_value == null) {
        self.cached_this_value = try self.agent.resolveThisBinding();
    }
    self.result = self.cached_this_value.?;
}

fn executeRestoreLexicalEnvironment(self: *Vm, _: Executable) Agent.Error!void {
    const lexical_environment = self.lexical_environment_stack.getLast();
    self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = lexical_environment;
}

fn executeRethrowExceptionIfAny(self: *Vm, _: Executable) Agent.Error!void {
    if (self.exception) |value| {
        self.agent.exception = value;
        return error.ExceptionThrown;
    }
}

fn executeReturn(_: *Vm, _: Executable) Agent.Error!void {
    @compileError("Should not be used"); // Handled in run()
}

fn executeStore(self: *Vm, _: Executable) Agent.Error!void {
    // Handle empty stack to allow restoring a null `.load`
    self.result = self.stack.popOrNull();
}

fn executeStoreConstant(
    self: *Vm,
    value_index: Executable.ConstantIndex,
    executable: Executable,
) Agent.Error!void {
    const value = executable.getConstant(value_index);
    self.result = value;
}

fn executeThrow(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.agent.exception = value;
    return error.ExceptionThrown;
}

fn executeToNumber(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: If we already have a number value return early
    if (value.isNumber()) return;

    self.result = Value.from(try value.toNumber(self.agent));
}

fn executeToNumeric(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: If we already have a numeric value return early
    if (value.isNumber() or value.isBigInt()) return;

    const numeric = try value.toNumeric(self.agent);
    self.result = switch (numeric) {
        .number => |number| Value.from(number),
        .big_int => |big_int| Value.from(big_int),
    };
}

fn executeToObject(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(try value.toObject(self.agent));
}

fn executeToString(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(try value.toString(self.agent));
}

/// 13.5.3.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-typeof-operator-runtime-semantics-evaluation
fn executeTypeof(self: *Vm, _: Executable) Agent.Error!void {
    // 1. Let val be ? Evaluation of UnaryExpression.
    // 2. If val is a Reference Record, then
    //    [...]
    // 3. Set val to ? GetValue(val).
    // NOTE: This is part of the generated bytecode.

    // 4-14.
    const value = self.result.?;
    self.result = Value.from(value.typeof());
}

/// 13.5.3.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-typeof-operator-runtime-semantics-evaluation
fn executeTypeofIdentifier(
    self: *Vm,
    name_index: Executable.IdentifierIndex,
    strict: bool,
    executable: Executable,
) Agent.Error!void {
    const name = executable.getIdentifier(name_index);

    // 1. Let val be ? Evaluation of UnaryExpression.
    const reference = try self.agent.resolveBinding(name, null, strict, null);

    // 2. If val is a Reference Record, then
    //     a. If IsUnresolvableReference(val) is true, return "undefined".
    if (reference.isUnresolvableReference()) {
        self.result = Value.from("undefined");
        return;
    }

    // 3. Set val to ? GetValue(val).
    const value = try reference.getValue(self.agent);

    // 4-14.
    self.result = Value.from(value.typeof());
}

fn executeUnaryMinus(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: Fast path for number values
    if (value.isNumber()) {
        if (value.__isI32() and value.__asI32() != 0) {
            self.result = Value.from(-value.__asI32());
        } else {
            self.result = Value.from(-value.__toF64());
        }
        return;
    }

    self.result = Value.from(try value.asBigInt().unaryMinus(self.agent));
}

fn executeYield(_: *Vm, _: Executable) Agent.Error!void {
    @compileError("Should not be used"); // Handled in run()
}

pub fn run(self: *Vm, executable: Executable) Agent.Error!Completion {
    std.debug.assert(@as(Instruction.Tag, @enumFromInt(executable.instructions.getLast())) == .end);
    main: switch (self.fetchInstructionTag(executable)) {
        .@"return" => return .{ .type = .@"return", .value = self.result, .target = null },
        .yield => return yield(self.agent, self.result.?),
        .end => return Completion.normal(self.result),
        inline else => |tag| {
            const Payload = Instruction.Payload(tag);
            const payload_ptr: *align(1) const Payload = @ptrCast(executable.instructions.items[self.ip..][0..@sizeOf(Payload)]);
            self.ip += @sizeOf(Payload);
            try self.executeInstruction(tag, payload_ptr.*, executable);
            continue :main self.fetchInstructionTag(executable);
        },
    }
}

fn executeInstruction(
    self: *Vm,
    comptime tag: Instruction.Tag,
    payload: Instruction.Payload(tag),
    executable: Executable,
) Agent.Error!void {
    (switch (tag) {
        .array_create => self.executeArrayCreate(payload.length, executable),
        .array_push_value => self.executeArrayPushValue(executable),
        .array_set_length => self.executeArraySetLength(payload.length, executable),
        .array_set_value_direct => self.executeArraySetValueDirect(payload.index, executable),
        .array_spread_value => self.executeArraySpreadValue(executable),
        .@"await" => self.executeAwait(executable),
        .binary_operator_add => self.executeBinaryOperatorAdd(executable),
        .binary_operator_sub => self.executeBinaryOperatorSub(executable),
        .binary_operator_mul => self.executeBinaryOperatorMul(executable),
        .binary_operator_div => self.executeBinaryOperatorDiv(executable),
        .binary_operator_mod => self.executeBinaryOperatorMod(executable),
        .binary_operator_exp => self.executeBinaryOperatorExp(executable),
        .binary_operator_left_shift => self.executeBinaryOperatorLeftShift(executable),
        .binary_operator_right_shift => self.executeBinaryOperatorRightShift(executable),
        .binary_operator_unsigned_right_shift => self.executeBinaryOperatorUnsignedRightShift(executable),
        .binary_operator_bitwise_and => self.executeBinaryOperatorBitwiseAnd(executable),
        .binary_operator_bitwise_or => self.executeBinaryOperatorBitwiseOr(executable),
        .binary_operator_bitwise_xor => self.executeBinaryOperatorBitwiseXor(executable),
        .binding_class_declaration_evaluation => self.executeBindingClassDeclarationEvaluation(payload, executable),
        .bitwise_not => self.executeBitwiseNot(executable),
        .block_declaration_instantiation => self.executeBlockDeclarationInstantiation(payload, executable),
        .class_definition_evaluation => self.executeClassDefinitionEvaluation(payload, executable),
        .create_catch_bindings => self.executeCreateCatchBindings(payload, executable),
        .create_object_property_iterator => self.executeCreateObjectPropertyIterator(executable),
        .create_with_environment => self.executeCreateWithEnvironment(executable),
        .decrement => self.executeDecrement(executable),
        .delete => self.executeDelete(executable),
        .dup_iterator => self.executeDupIterator(executable),
        .dup_reference => self.executeDupReference(executable),
        .evaluate_call => self.executeEvaluateCall(payload.argument_count, executable),
        .evaluate_call_direct_eval => self.executeEvaluateCallDirectEval(payload.argument_count, payload.strict, executable),
        .evaluate_import_call => self.executeEvaluateImportCall(executable),
        .evaluate_new => self.executeEvaluateNew(payload.argument_count, executable),
        .evaluate_property_access_with_expression_key => self.executeEvaluatePropertyAccessWithExpressionKey(payload.strict, executable),
        .evaluate_property_access_with_identifier_key => self.executeEvaluatePropertyAccessWithIdentifierKey(payload.strict, payload.identifier, payload.property_lookup_cache_index, executable),
        .evaluate_super_call => self.executeEvaluateSuperCall(payload.argument_count, executable),
        .for_declaration_binding_instantiation => self.executeForDeclarationBindingInstantiation(payload, executable),
        .get_iterator => self.executeGetIterator(payload, executable),
        .get_new_target => self.executeGetNewTarget(executable),
        .get_or_create_import_meta => self.executeGetOrCreateImportMeta(executable),
        .get_template_object => self.executeGetTemplateObject(payload, executable),
        .get_value => self.executeGetValue(executable),
        .greater_than => self.executeGreaterThan(executable),
        .greater_than_equals => self.executeGreaterThanEquals(executable),
        .has_private_element => self.executeHasPrivateElement(payload, executable),
        .has_property => self.executeHasProperty(executable),
        .increment => self.executeIncrement(executable),
        .initialize_bound_name => self.executeInitializeBoundName(payload, executable),
        .initialize_referenced_binding => self.executeInitializeReferencedBinding(executable),
        .instanceof_operator => self.executeInstanceofOperator(executable),
        .instantiate_arrow_function_expression => self.executeInstantiateArrowFunctionExpression(payload, executable),
        .instantiate_async_arrow_function_expression => self.executeInstantiateAsyncArrowFunctionExpression(payload, executable),
        .instantiate_async_function_expression => self.executeInstantiateAsyncFunctionExpression(payload, executable),
        .instantiate_async_generator_function_expression => self.executeInstantiateAsyncGeneratorFunctionExpression(payload, executable),
        .instantiate_generator_function_expression => self.executeInstantiateGeneratorFunctionExpression(payload, executable),
        .instantiate_ordinary_function_expression => self.executeInstantiateOrdinaryFunctionExpression(payload, executable),
        .is_loosely_equal => self.executeIsLooselyEqual(executable),
        .is_strictly_equal => self.executeIsStrictlyEqual(executable),
        .jump => self.executeJump(payload, executable),
        .jump_conditional => self.executeJumpConditional(payload.consequent, payload.alternate, executable),
        .less_than => self.executeLessThan(executable),
        .less_than_equals => self.executeLessThanEquals(executable),
        .load => self.executeLoad(executable),
        .load_constant => self.executeLoadConstant(payload, executable),
        .load_and_clear_exception => self.executeLoadAndClearException(executable),
        .load_iterator_next_args => self.executeLoadIteratorNextArgs(executable),
        .load_this_value_for_evaluate_call => self.executeLoadThisValueForEvaluateCall(executable),
        .load_this_value_for_make_super_property_reference => self.executeLoadThisValueForMakeSuperPropertyReference(executable),
        .logical_not => self.executeLogicalNot(executable),
        .make_private_reference => self.executeMakePrivateReference(payload, executable),
        .make_super_property_reference => self.executeMakeSuperPropertyReference(payload.strict, executable),
        .object_create => self.executeObjectCreate(executable),
        .object_define_method => self.executeObjectDefineMethod(payload.ast_node, payload.type, executable),
        .object_set_property => self.executeObjectSetProperty(executable),
        .object_set_prototype => self.executeObjectSetPrototype(executable),
        .object_spread_value => self.executeObjectSpreadValue(executable),
        .pop_exception_jump_target => self.executePopExceptionJumpTarget(executable),
        .pop_iterator => self.executePopIterator(executable),
        .pop_lexical_environment => self.executePopLexicalEnvironment(executable),
        .pop_reference => self.executePopReference(executable),
        .push_lexical_environment => self.executePushLexicalEnvironment(executable),
        .push_exception_jump_target => self.executePushExceptionJumpTarget(payload, executable),
        .put_value => self.executePutValue(executable),
        .reg_exp_create => self.executeRegExpCreate(executable),
        .resolve_binding => self.executeResolveBinding(payload.identifier, payload.strict, payload.environment_lookup_cache_index, executable),
        .resolve_private_identifier => self.executeResolvePrivateIdentifier(payload, executable),
        .resolve_this_binding => self.executeResolveThisBinding(executable),
        .restore_lexical_environment => self.executeRestoreLexicalEnvironment(executable),
        .rethrow_exception_if_any => self.executeRethrowExceptionIfAny(executable),
        .store => self.executeStore(executable),
        .store_constant => self.executeStoreConstant(payload, executable),
        .throw => self.executeThrow(executable),
        .to_number => self.executeToNumber(executable),
        .to_numeric => self.executeToNumeric(executable),
        .to_object => self.executeToObject(executable),
        .to_string => self.executeToString(executable),
        .typeof => self.executeTypeof(executable),
        .typeof_identifier => self.executeTypeofIdentifier(payload.identifier, payload.strict, executable),
        .unary_minus => self.executeUnaryMinus(executable),
        .@"return", .yield, .end => unreachable,
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ExceptionThrown => {
            if (self.exception_jump_target_stack.items.len != 0) {
                self.exception = self.agent.clearException();
                self.ip = self.exception_jump_target_stack.getLast();
            } else return err;
        },
    };
}
