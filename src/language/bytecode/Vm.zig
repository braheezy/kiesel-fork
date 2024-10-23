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
stack: std.ArrayList(Value),
iterator_stack: std.ArrayList(Iterator),
lexical_environment_stack: std.ArrayList(Environment),
reference_stack: std.ArrayList(Reference),
exception_jump_target_stack: std.ArrayList(usize),
function_arguments: std.ArrayList(Value),
environment_lookup_cache: std.ArrayList(?Environment.LookupCacheEntry),
result: ?Value = null,
exception: ?Value = null,
iterator: ?Iterator = null,

pub fn init(agent: *Agent) std.mem.Allocator.Error!Vm {
    const stack = try std.ArrayList(Value).initCapacity(agent.gc_allocator, 32);
    const iterator_stack = std.ArrayList(Iterator).init(agent.gc_allocator);
    const lexical_environment_stack = std.ArrayList(Environment).init(agent.gc_allocator);
    const reference_stack = std.ArrayList(Reference).init(agent.gc_allocator);
    const exception_jump_target_stack = std.ArrayList(usize).init(agent.gc_allocator);
    const function_arguments = try std.ArrayList(Value).initCapacity(agent.gc_allocator, 8);
    const environment_lookup_cache = std.ArrayList(?Environment.LookupCacheEntry).init(agent.gc_allocator);
    return .{
        .agent = agent,
        .ip = 0,
        .stack = stack,
        .iterator_stack = iterator_stack,
        .lexical_environment_stack = lexical_environment_stack,
        .reference_stack = reference_stack,
        .exception_jump_target_stack = exception_jump_target_stack,
        .function_arguments = function_arguments,
        .environment_lookup_cache = environment_lookup_cache,
    };
}

pub fn deinit(self: Vm) void {
    self.stack.deinit();
    self.iterator_stack.deinit();
    self.lexical_environment_stack.deinit();
    self.reference_stack.deinit();
    self.exception_jump_target_stack.deinit();
    self.function_arguments.deinit();
    self.environment_lookup_cache.deinit();
}

fn fetchInstruction(self: *Vm, executable: Executable) Instruction {
    defer self.ip += 1;
    return @enumFromInt(executable.instructions.items[self.ip]);
}

fn fetchConstant(self: *Vm, executable: Executable) Value {
    const index = self.fetchIndex(executable);
    return executable.constants.unmanaged.entries.get(index).key;
}

fn fetchIdentifier(self: *Vm, executable: Executable) String {
    const index = self.fetchIndex(executable);
    return executable.identifiers.unmanaged.entries.get(index).key;
}

fn fetchAstNode(self: *Vm, executable: Executable) *Executable.AstNode {
    const index = self.fetchIndex(executable);
    return &executable.ast_nodes.items[index];
}

fn fetchIndex(self: *Vm, executable: Executable) Executable.IndexType {
    defer self.ip += @sizeOf(Executable.IndexType);
    return std.mem.bytesToValue(Executable.IndexType, &executable.instructions.items[self.ip]);
}

fn getArgumentSpreadIndices(self: *Vm) std.mem.Allocator.Error![]const usize {
    const value = self.stack.pop();
    if (value.isUndefined()) return &.{};
    const array = value.asObject();
    const len = getArrayLength(array);
    var argument_spread_indices = try std.ArrayList(usize).initCapacity(self.agent.gc_allocator, len);
    for (0..len) |i| {
        const argument_spread_index = array.propertyStorage().get(
            PropertyKey.from(@as(u53, @intCast(i))),
        ).?.value.?.asNumber().i32;
        argument_spread_indices.appendAssumeCapacity(@intCast(argument_spread_index));
    }
    return argument_spread_indices.toOwnedSlice();
}

fn getArguments(self: *Vm, argument_count: usize) Agent.Error![]const Value {
    self.function_arguments.clearRetainingCapacity();
    try self.function_arguments.ensureTotalCapacity(argument_count); // May still resize when spreading
    const argument_spread_indices = try self.getArgumentSpreadIndices();
    defer self.agent.gc_allocator.free(argument_spread_indices);
    for (0..argument_count) |i| {
        const argument = self.stack.pop();
        if (std.mem.indexOfScalar(usize, argument_spread_indices, argument_count - i - 1) == null) {
            try self.function_arguments.insert(0, argument);
        } else {
            var iterator = try getIterator(self.agent, argument, .sync);
            var n: usize = 0;
            while (try iterator.stepValue()) |value| : (n += 1) {
                try self.function_arguments.insert(n, value);
            }
        }
    }
    return self.function_arguments.items;
}

fn executeApplyStringOrNumericBinaryOperator(self: *Vm, executable: Executable) Agent.Error!void {
    const operator_type = self.fetchIndex(executable);
    const operator: ast.BinaryExpression.Operator = @enumFromInt(operator_type);
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();
    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, operator, r_val);
}

fn executeArrayCreate(self: *Vm, _: Executable) Agent.Error!void {
    self.result = Value.from(try arrayCreate(self.agent, 0, null));
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

fn executeArraySetLength(self: *Vm, executable: Executable) Agent.Error!void {
    const length = self.fetchIndex(executable);
    const array = self.result.?.asObject();
    // From ArrayAccumulation:
    // 2. Perform ? Set(array, "length", 𝔽(len), true).
    try array.set(PropertyKey.from("length"), Value.from(length), .throw);
}

fn executeArraySpreadValue(self: *Vm, _: Executable) Agent.Error!void {
    const array = self.stack.pop().asObject();
    var next_index: u53 = @intCast(getArrayLength(array));

    // From ArrayAccumulation:
    // 3. Let iteratorRecord be ? GetIterator(spreadObj, sync).
    const iterator = &self.iterator_stack.items[self.iterator_stack.items.len - 1];

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

fn executeBindingClassDeclarationEvaluation(self: *Vm, executable: Executable) Agent.Error!void {
    const class_declaration = self.fetchAstNode(executable).class_declaration;
    self.result = Value.from(try bindingClassDeclarationEvaluation(self.agent, class_declaration));
}

fn executeBitwiseNot(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = switch (value.type()) {
        .number => Value.from(value.asNumber().bitwiseNOT()),
        .big_int => Value.from(try value.asBigInt().bitwiseNOT(self.agent)),
        else => unreachable,
    };
}

fn executeBlockDeclarationInstantiation(self: *Vm, executable: Executable) Agent.Error!void {
    const block = self.fetchAstNode(executable);
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
fn executeClassDefinitionEvaluation(self: *Vm, executable: Executable) Agent.Error!void {
    const class_expression = self.fetchAstNode(executable).class_expression;

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

fn executeCreateCatchBinding(self: *Vm, executable: Executable) Agent.Error!void {
    // TODO: This should create a new environment - for now we approximate this by creating
    //       a new binding and ignoring the error if one already exists.
    const name = self.fetchIdentifier(executable);
    const thrown_value = self.exception.?;
    self.exception = null;
    const running_context = self.agent.runningExecutionContext();
    const catch_env = running_context.ecmascript_code.?.lexical_environment;
    if (!try catch_env.hasBinding(name)) {
        try catch_env.createMutableBinding(self.agent, name, false);
        try catch_env.initializeBinding(self.agent, name, thrown_value);
    } else {
        catch_env.setMutableBinding(
            self.agent,
            name,
            thrown_value,
            false,
        ) catch unreachable;
    }
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
    self.iterator = .{ .iterator = iterator, .next_method = next_method, .done = false };
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
    self.result = switch (value.type()) {
        .number => Value.from(value.asNumber().subtract(.{ .i32 = 1 })),
        .big_int => Value.from(
            try value.asBigInt().subtract(self.agent, self.agent.pre_allocated.one),
        ),
        else => @panic("decrement instruction must only be used with numeric value"),
    };
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
        const delete_status = try base_obj.internalMethods().delete(base_obj, property_key);

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

fn executeDupReference(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.getLast();
    try self.reference_stack.append(reference);
}

fn executeEvaluateCall(self: *Vm, executable: Executable) Agent.Error!void {
    const argument_count = self.fetchIndex(executable);
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

fn executeEvaluateCallDirectEval(self: *Vm, executable: Executable) Agent.Error!void {
    const argument_count = self.fetchIndex(executable);
    const strict = self.fetchIndex(executable) == 1;
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

fn executeEvaluateNew(self: *Vm, executable: Executable) Agent.Error!void {
    const argument_count = self.fetchIndex(executable);
    const arguments = try self.getArguments(argument_count);
    const constructor = self.stack.pop();
    self.result = try evaluateNew(self.agent, constructor, arguments);
}

/// 13.3.3 EvaluatePropertyAccessWithExpressionKey ( baseValue, expression, strict )
/// https://tc39.es/ecma262/#sec-evaluate-property-access-with-expression-key
fn executeEvaluatePropertyAccessWithExpressionKey(self: *Vm, executable: Executable) Agent.Error!void {
    // 1. Let propertyNameReference be ? Evaluation of expression.
    // 2. Let propertyNameValue be ? GetValue(propertyNameReference).
    const property_name_value = self.stack.pop();

    const strict = self.fetchIndex(executable) == 1;
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
    try self.reference_stack.append(reference);
}

/// 13.3.4 EvaluatePropertyAccessWithIdentifierKey ( baseValue, identifierName, strict )
/// https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key
fn executeEvaluatePropertyAccessWithIdentifierKey(self: *Vm, executable: Executable) Agent.Error!void {
    // 1. Let propertyNameString be the StringValue of identifierName.
    const property_name_string = self.fetchIdentifier(executable);

    const strict = self.fetchIndex(executable) == 1;
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
    };
    try self.reference_stack.append(reference);
}

/// 13.3.7.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-super-keyword-runtime-semantics-evaluation
fn executeEvaluateSuperCall(self: *Vm, executable: Executable) Agent.Error!void {
    const argument_count = self.fetchIndex(executable);

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
    const constructor = this_environment.function_environment.function_object.object();

    // 11. Perform ? InitializeInstanceElements(result, F).
    try result.initializeInstanceElements(constructor);

    // 12. Return result.
    self.result = Value.from(result);
}

fn executeForDeclarationBindingInstantiation(self: *Vm, executable: Executable) Agent.Error!void {
    const lexical_declaration = self.fetchAstNode(executable).lexical_declaration;

    // From ForIn/OfBodyEvaluation:
    // iii. Let iterationEnv be NewDeclarativeEnvironment(oldEnv).
    const old_env = self.lexical_environment_stack.getLast();
    const environment = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);

    // iv. Perform ForDeclarationBindingInstantiation of lhs with argument iterationEnv.

    // 14.7.5.4 Runtime Semantics: ForDeclarationBindingInstantiation
    // https://tc39.es/ecma262/#sec-runtime-semantics-fordeclarationbindinginstantiation
    var bound_names = std.ArrayList(ast.Identifier).init(self.agent.gc_allocator);
    defer bound_names.deinit();
    try lexical_declaration.collectBoundNames(&bound_names);

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

fn executeGetIterator(self: *Vm, executable: Executable) Agent.Error!void {
    const iterator_kind: IteratorKind = @enumFromInt((self.fetchIndex(executable)));
    self.iterator = try getIterator(self.agent, self.result.?, iterator_kind);
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
        var import_meta_values = try self.agent.host_hooks.hostGetImportMetaProperties(module);
        defer import_meta_values.deinit();

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

fn executeGetTemplateObject(self: *Vm, executable: Executable) Agent.Error!void {
    const template_literal = &self.fetchAstNode(executable).template_literal;
    self.result = Value.from(try getTemplateObject(self.agent, template_literal));
}

fn executeGetValue(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.pop();
    self.result = try reference.getValue(self.agent);
}

fn executeGreaterThan(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. Let r be ? IsLessThan(rVal, lVal, false).
    const result = try isLessThan(self.agent, r_val, l_val, .right_first);

    // 6. If r is undefined, return false. Otherwise, return r.
    self.result = Value.from(result orelse false);
}

fn executeGreaterThanEquals(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. Let r be ? IsLessThan(lVal, rVal, true).
    const result = try isLessThan(self.agent, l_val, r_val, .left_first);

    // 6. If r is either true or undefined, return false. Otherwise, return true.
    self.result = Value.from(!(result orelse true));
}

fn executeHasPrivateElement(self: *Vm, executable: Executable) Agent.Error!void {
    const private_identifier = self.fetchIdentifier(executable);
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
    const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

    // 6. Let privateName be ResolvePrivateIdentifier(privateEnv, privateIdentifier).
    const private_name = private_environment.resolvePrivateIdentifier(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    );

    // 7. If PrivateElementFind(rVal, privateName) is not empty, return true.
    // 8. Return false.
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
    self.result = switch (value.type()) {
        .number => Value.from(value.asNumber().add(.{ .i32 = 1 })),
        .big_int => Value.from(
            try value.asBigInt().add(self.agent, self.agent.pre_allocated.one),
        ),
        else => unreachable,
    };
}

fn executeInitializeDefaultExport(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;

    // 3. Let env be the running execution context's LexicalEnvironment.
    const environment = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 4. Perform ? InitializeBoundName("*default*", value, env).
    try initializeBoundName(
        self.agent,
        String.fromLiteral("*default*"),
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

fn executeInstantiateArrowFunctionExpression(self: *Vm, executable: Executable) Agent.Error!void {
    const arrow_function = self.fetchAstNode(executable).arrow_function;
    const closure = try instantiateArrowFunctionExpression(
        self.agent,
        arrow_function,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateAsyncArrowFunctionExpression(self: *Vm, executable: Executable) Agent.Error!void {
    const async_arrow_function = self.fetchAstNode(executable).async_arrow_function;
    const closure = try instantiateAsyncArrowFunctionExpression(
        self.agent,
        async_arrow_function,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateAsyncFunctionExpression(self: *Vm, executable: Executable) Agent.Error!void {
    const async_function_expression = self.fetchAstNode(executable).async_function_expression;
    const closure = try instantiateAsyncFunctionExpression(
        self.agent,
        async_function_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateAsyncGeneratorFunctionExpression(self: *Vm, executable: Executable) Agent.Error!void {
    const async_generator_expression = self.fetchAstNode(executable).async_generator_expression;
    const closure = try instantiateAsyncGeneratorFunctionExpression(
        self.agent,
        async_generator_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateGeneratorFunctionExpression(self: *Vm, executable: Executable) Agent.Error!void {
    const generator_expression = self.fetchAstNode(executable).generator_expression;
    const closure = try instantiateGeneratorFunctionExpression(
        self.agent,
        generator_expression,
        null,
    );
    self.result = Value.from(closure);
}

fn executeInstantiateOrdinaryFunctionExpression(self: *Vm, executable: Executable) Agent.Error!void {
    const function_expression = self.fetchAstNode(executable).function_expression;
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

    // 5. Return IsLooselyEqual(rVal, lVal).
    self.result = Value.from(try isLooselyEqual(self.agent, r_val, l_val));
}

fn executeIsStrictlyEqual(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. Return IsStrictlyEqual(rVal, lVal).
    self.result = Value.from(isStrictlyEqual(r_val, l_val));
}

fn executeJump(self: *Vm, executable: Executable) Agent.Error!void {
    self.ip = self.fetchIndex(executable);
}

fn executeJumpConditional(self: *Vm, executable: Executable) Agent.Error!void {
    const ip_consequent = self.fetchIndex(executable);
    const ip_alternate = self.fetchIndex(executable);
    const value = self.result.?;
    self.ip = if (value.toBoolean()) ip_consequent else ip_alternate;
}

fn executeLessThan(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. Let r be ? IsLessThan(lVal, rVal, true).
    const result = try isLessThan(self.agent, l_val, r_val, .left_first);

    // 6. If r is undefined, return false. Otherwise, return r.
    self.result = Value.from(result orelse false);
}

fn executeLessThanEquals(self: *Vm, _: Executable) Agent.Error!void {
    const r_val = self.stack.pop();
    const l_val = self.stack.pop();

    // 5. Let r be ? IsLessThan(rVal, lVal, false).
    const result = try isLessThan(self.agent, r_val, l_val, .right_first);

    // 6. If r is either true or undefined, return false. Otherwise, return true.
    self.result = Value.from(!(result orelse true));
}

fn executeLoad(self: *Vm, _: Executable) Agent.Error!void {
    // Handle null value to allow load of 'empty' result at beginning of script
    if (self.result) |value| try self.stack.append(value);
}

fn executeLoadConstant(self: *Vm, executable: Executable) Agent.Error!void {
    const value = self.fetchConstant(executable);
    try self.stack.append(value);
}

fn executeLoadIteratorNextArgs(self: *Vm, _: Executable) Agent.Error!void {
    const iterator = self.iterator_stack.getLast();
    try self.stack.append(iterator.next_method);
    try self.stack.append(Value.from(iterator.iterator));
}

fn executeLoadThisValueForEvaluateCall(self: *Vm, _: Executable) Agent.Error!void {
    const reference = self.reference_stack.pop();
    const this_value = evaluateCallGetThisValue(reference);
    try self.stack.append(this_value);
}

fn executeLoadThisValueForMakeSuperPropertyReference(self: *Vm, _: Executable) Agent.Error!void {
    // 1. Let env be GetThisEnvironment().
    const env = self.agent.getThisEnvironment();

    // 2. Let actualThis be ? env.GetThisBinding().
    const actual_this = try env.getThisBinding();

    try self.stack.append(actual_this);
}

fn executeLogicalNot(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(!value.toBoolean());
}

/// 6.2.5.9 MakePrivateReference ( baseValue, privateIdentifier )
/// https://tc39.es/ecma262/#sec-makeprivatereference
fn executeMakePrivateReference(self: *Vm, executable: Executable) Agent.Error!void {
    const private_identifier = self.fetchIdentifier(executable);
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
    try self.reference_stack.append(reference);
}

/// 13.3.7.3 MakeSuperPropertyReference ( actualThis, propertyKey, strict )
/// https://tc39.es/ecma262/#sec-makesuperpropertyreference
fn executeMakeSuperPropertyReference(self: *Vm, executable: Executable) Agent.Error!void {
    const property_key = self.stack.pop();
    const strict = self.fetchIndex(executable) == 1;
    const actual_this = self.stack.pop();

    // 1. Let env be GetThisEnvironment().
    const env = self.agent.getThisEnvironment();

    // 2. Assert: env.HasSuperBinding() is true.
    std.debug.assert(env.hasSuperBinding());

    // 3. Let baseValue be ? env.GetSuperBase().
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
    try self.reference_stack.append(reference);
}

fn executeObjectCreate(self: *Vm, _: Executable) Agent.Error!void {
    const object = try ordinaryObjectCreate(
        self.agent,
        try self.agent.currentRealm().intrinsics.@"%Object.prototype%"(),
    );
    self.result = Value.from(object);
}

fn executeObjectDefineMethod(self: *Vm, executable: Executable) Agent.Error!void {
    const function_or_class = self.fetchAstNode(executable);
    const method_type: ast.MethodDefinition.Type = @enumFromInt((self.fetchIndex(executable)));
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
        _ = object.internalMethods().setPrototypeOf(
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
    try self.lexical_environment_stack.append(lexical_environment);
}

fn executePushExceptionJumpTarget(self: *Vm, executable: Executable) Agent.Error!void {
    const jump_target = self.fetchIndex(executable);
    try self.exception_jump_target_stack.append(jump_target);
}

fn executePushIterator(self: *Vm, _: Executable) Agent.Error!void {
    try self.iterator_stack.append(self.iterator.?);
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

fn executeResolveBinding(self: *Vm, executable: Executable) Agent.Error!void {
    const name = self.fetchIdentifier(executable);
    const strict = self.fetchIndex(executable) == 1;
    const environment_lookup_cache_index = self.fetchIndex(executable);
    const lookup_cache_entry = &self.environment_lookup_cache.items[
        environment_lookup_cache_index
    ];
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.reference_stack.append(reference);
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
fn executeResolvePrivateIdentifier(self: *Vm, executable: Executable) Agent.Error!void {
    // 1. Let privateIdentifier be the StringValue of PrivateIdentifier.
    const private_identifier = self.fetchIdentifier(executable);

    // 2. Let privateEnvRec be the running execution context's PrivateEnvironment.
    const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

    // 3. Let names be privateEnvRec.[[Names]].
    // 4. Assert: Exactly one element of names is a Private Name whose [[Description]] is privateIdentifier.
    // 5. Let privateName be the Private Name in names whose [[Description]] is privateIdentifier.
    // 6. Return privateName.
    const private_name = private_environment.names.get(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    ).?;
    std.debug.assert(private_name.symbol.data.is_private);
    self.result = Value.from(private_name.symbol);
}

fn executeResolveThisBinding(self: *Vm, _: Executable) Agent.Error!void {
    self.result = try self.agent.resolveThisBinding();
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

fn executeStoreConstant(self: *Vm, executable: Executable) Agent.Error!void {
    const value = self.fetchConstant(executable);
    self.result = value;
}

fn executeThrow(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.agent.exception = value;
    return error.ExceptionThrown;
}

fn executeToNumber(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(try value.toNumber(self.agent));
}

fn executeToNumeric(self: *Vm, _: Executable) Agent.Error!void {
    const value = self.result.?;
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
fn executeTypeofIdentifier(self: *Vm, executable: Executable) Agent.Error!void {
    const name = self.fetchIdentifier(executable);
    const strict = self.fetchIndex(executable) == 1;

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
    self.result = switch (value.type()) {
        .number => Value.from(value.asNumber().unaryMinus()),
        .big_int => Value.from(try value.asBigInt().unaryMinus(self.agent)),
        else => unreachable,
    };
}

fn executeYield(_: *Vm, _: Executable) Agent.Error!void {
    @compileError("Should not be used"); // Handled in run()
}

pub fn run(self: *Vm, executable: Executable) Agent.Error!Completion {
    std.debug.assert(@as(Instruction, @enumFromInt(executable.instructions.getLast())) == .end);
    try self.environment_lookup_cache.resize(executable.environment_lookup_cache_size);
    @memset(self.environment_lookup_cache.items, null);
    main: switch (self.fetchInstruction(executable)) {
        .@"return" => return .{ .type = .@"return", .value = self.result, .target = null },
        .yield => return yield(self.agent, self.result.?),
        .end => return Completion.normal(self.result),
        inline else => |instruction| {
            try self.executeInstruction(instruction, executable);
            continue :main self.fetchInstruction(executable);
        },
    }
}

fn executeInstruction(
    self: *Vm,
    comptime instruction: Instruction,
    executable: Executable,
) Agent.Error!void {
    (switch (instruction) {
        .apply_string_or_numeric_binary_operator => self.executeApplyStringOrNumericBinaryOperator(executable),
        .array_create => self.executeArrayCreate(executable),
        .array_push_value => self.executeArrayPushValue(executable),
        .array_set_length => self.executeArraySetLength(executable),
        .array_spread_value => self.executeArraySpreadValue(executable),
        .@"await" => self.executeAwait(executable),
        .binding_class_declaration_evaluation => self.executeBindingClassDeclarationEvaluation(executable),
        .bitwise_not => self.executeBitwiseNot(executable),
        .block_declaration_instantiation => self.executeBlockDeclarationInstantiation(executable),
        .class_definition_evaluation => self.executeClassDefinitionEvaluation(executable),
        .create_catch_binding => self.executeCreateCatchBinding(executable),
        .create_object_property_iterator => self.executeCreateObjectPropertyIterator(executable),
        .create_with_environment => self.executeCreateWithEnvironment(executable),
        .decrement => self.executeDecrement(executable),
        .delete => self.executeDelete(executable),
        .dup_reference => self.executeDupReference(executable),
        .evaluate_call => self.executeEvaluateCall(executable),
        .evaluate_call_direct_eval => self.executeEvaluateCallDirectEval(executable),
        .evaluate_import_call => self.executeEvaluateImportCall(executable),
        .evaluate_new => self.executeEvaluateNew(executable),
        .evaluate_property_access_with_expression_key => self.executeEvaluatePropertyAccessWithExpressionKey(executable),
        .evaluate_property_access_with_identifier_key => self.executeEvaluatePropertyAccessWithIdentifierKey(executable),
        .evaluate_super_call => self.executeEvaluateSuperCall(executable),
        .for_declaration_binding_instantiation => self.executeForDeclarationBindingInstantiation(executable),
        .get_iterator => self.executeGetIterator(executable),
        .get_new_target => self.executeGetNewTarget(executable),
        .get_or_create_import_meta => self.executeGetOrCreateImportMeta(executable),
        .get_template_object => self.executeGetTemplateObject(executable),
        .get_value => self.executeGetValue(executable),
        .greater_than => self.executeGreaterThan(executable),
        .greater_than_equals => self.executeGreaterThanEquals(executable),
        .has_private_element => self.executeHasPrivateElement(executable),
        .has_property => self.executeHasProperty(executable),
        .increment => self.executeIncrement(executable),
        .initialize_default_export => self.executeInitializeDefaultExport(executable),
        .initialize_referenced_binding => self.executeInitializeReferencedBinding(executable),
        .instanceof_operator => self.executeInstanceofOperator(executable),
        .instantiate_arrow_function_expression => self.executeInstantiateArrowFunctionExpression(executable),
        .instantiate_async_arrow_function_expression => self.executeInstantiateAsyncArrowFunctionExpression(executable),
        .instantiate_async_function_expression => self.executeInstantiateAsyncFunctionExpression(executable),
        .instantiate_async_generator_function_expression => self.executeInstantiateAsyncGeneratorFunctionExpression(executable),
        .instantiate_generator_function_expression => self.executeInstantiateGeneratorFunctionExpression(executable),
        .instantiate_ordinary_function_expression => self.executeInstantiateOrdinaryFunctionExpression(executable),
        .is_loosely_equal => self.executeIsLooselyEqual(executable),
        .is_strictly_equal => self.executeIsStrictlyEqual(executable),
        .jump => self.executeJump(executable),
        .jump_conditional => self.executeJumpConditional(executable),
        .less_than => self.executeLessThan(executable),
        .less_than_equals => self.executeLessThanEquals(executable),
        .load => self.executeLoad(executable),
        .load_constant => self.executeLoadConstant(executable),
        .load_iterator_next_args => self.executeLoadIteratorNextArgs(executable),
        .load_this_value_for_evaluate_call => self.executeLoadThisValueForEvaluateCall(executable),
        .load_this_value_for_make_super_property_reference => self.executeLoadThisValueForMakeSuperPropertyReference(executable),
        .logical_not => self.executeLogicalNot(executable),
        .make_private_reference => self.executeMakePrivateReference(executable),
        .make_super_property_reference => self.executeMakeSuperPropertyReference(executable),
        .object_create => self.executeObjectCreate(executable),
        .object_define_method => self.executeObjectDefineMethod(executable),
        .object_set_property => self.executeObjectSetProperty(executable),
        .object_set_prototype => self.executeObjectSetPrototype(executable),
        .object_spread_value => self.executeObjectSpreadValue(executable),
        .pop_exception_jump_target => self.executePopExceptionJumpTarget(executable),
        .pop_iterator => self.executePopIterator(executable),
        .pop_lexical_environment => self.executePopLexicalEnvironment(executable),
        .pop_reference => self.executePopReference(executable),
        .push_lexical_environment => self.executePushLexicalEnvironment(executable),
        .push_exception_jump_target => self.executePushExceptionJumpTarget(executable),
        .push_iterator => self.executePushIterator(executable),
        .put_value => self.executePutValue(executable),
        .reg_exp_create => self.executeRegExpCreate(executable),
        .resolve_binding => self.executeResolveBinding(executable),
        .resolve_private_identifier => self.executeResolvePrivateIdentifier(executable),
        .resolve_this_binding => self.executeResolveThisBinding(executable),
        .restore_lexical_environment => self.executeRestoreLexicalEnvironment(executable),
        .rethrow_exception_if_any => self.executeRethrowExceptionIfAny(executable),
        .store => self.executeStore(executable),
        .store_constant => self.executeStoreConstant(executable),
        .throw => self.executeThrow(executable),
        .to_number => self.executeToNumber(executable),
        .to_numeric => self.executeToNumeric(executable),
        .to_object => self.executeToObject(executable),
        .to_string => self.executeToString(executable),
        .typeof => self.executeTypeof(executable),
        .typeof_identifier => self.executeTypeofIdentifier(executable),
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
