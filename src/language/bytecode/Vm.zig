const std = @import("std");

const ast = @import("../ast.zig");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const instructions_ = @import("instructions.zig");
const runtime = @import("../runtime.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const BlockDeclarationInstantiationType = runtime.BlockDeclarationInstantiationType;
const Completion = types.Completion;
const Environment = execution.Environment;
const Executable = @import("Executable.zig");
const Instruction = instructions_.Instruction;
const Iterator = types.Iterator;
const IteratorKind = types.IteratorKind;
const PropertyKey = types.PropertyKey;
const Reference = types.Reference;
const String = types.String;
const Value = types.Value;
const applyStringOrNumericBinaryOperator = runtime.applyStringOrNumericBinaryOperator;
const arrayCreate = builtins.arrayCreate;
const await = builtins.await;
const bindingClassDeclarationEvaluation = runtime.bindingClassDeclarationEvaluation;
const blockDeclarationInstantiation = runtime.blockDeclarationInstantiation;
const classDefinitionEvaluation = runtime.classDefinitionEvaluation;
const createForInIterator = builtins.createForInIterator;
const directEval = runtime.directEval;
const evaluateCall = runtime.evaluateCall;
const evaluateCallGetThisValue = runtime.evaluateCallGetThisValue;
const evaluateImportCall = runtime.evaluateImportCall;
const evaluateNew = runtime.evaluateNew;
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
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const yield = builtins.yield;

const Vm = @This();

agent: *Agent,
executable: *const Executable,
ip: usize,
stack: std.ArrayList(Value),
iterator_stack: std.ArrayList(Iterator),
lexical_environment_stack: std.ArrayList(Environment),
reference_stack: std.ArrayList(Reference),
exception_jump_target_stack: std.ArrayList(usize),
function_arguments: std.ArrayList(Value),
result: ?Value = null,
exception: ?Agent.Exception = null,
cached_this_value: ?Value = null,

pub fn init(agent: *Agent, executable: *const Executable) std.mem.Allocator.Error!Vm {
    const stack = try std.ArrayList(Value).initCapacity(agent.gc_allocator, 32);
    const function_arguments = try std.ArrayList(Value).initCapacity(agent.gc_allocator, 8);
    return .{
        .agent = agent,
        .executable = executable,
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

fn fetchInstructionTag(self: *Vm) Instruction.Tag {
    defer self.ip += 1;
    return @enumFromInt(self.executable.instructions.items[self.ip]);
}

fn getArguments(self: *Vm, arguments: Instruction.Arguments) Agent.Error![]const Value {
    self.function_arguments.clearRetainingCapacity();
    if (!arguments.has_spread) {
        try self.function_arguments.appendSlice(
            self.agent.gc_allocator,
            self.stack.items[self.stack.items.len - arguments.count ..],
        );
        self.stack.items.len -= arguments.count;
    } else {
        const array = self.stack.pop().?.asObject();
        const argument_spread_indices = array.property_storage.indexed_properties.storage.dense_i32.items;
        // May still resize when spreading
        try self.function_arguments.ensureTotalCapacity(self.agent.gc_allocator, arguments.count);
        for (0..arguments.count) |i| {
            const argument = self.stack.pop().?;
            if (std.mem.indexOfScalar(i32, argument_spread_indices, @intCast(arguments.count - i - 1)) == null) {
                try self.function_arguments.insert(self.agent.gc_allocator, 0, argument);
            } else {
                var iterator = try getIterator(self.agent, argument, .sync);
                var n: usize = 0;
                while (try iterator.stepValue(self.agent)) |value| : (n += 1) {
                    try self.function_arguments.insert(self.agent.gc_allocator, n, value);
                }
            }
        }
    }
    return self.function_arguments.items;
}

fn executeArrayCreate(self: *Vm, length: u32) Agent.Error!void {
    const array = try arrayCreate(self.agent, length, null);
    self.result = Value.from(&array.object);
}

fn executeArrayPushValue(self: *Vm) Agent.Error!void {
    const init_value = self.stack.pop().?;
    const array = self.stack.pop().?.asObject();
    const index = array.as(builtins.Array).fields.length;
    // From ArrayAccumulation:
    // 4. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), initValue).
    try array.createDataPropertyDirect(
        self.agent,
        PropertyKey.from(@as(PropertyKey.IntegerIndex, index)),
        init_value,
    );
    self.result = Value.from(array);
}

fn executeArraySetLength(self: *Vm, length: u32) Agent.Error!void {
    const array = self.result.?.asObject();
    // From ArrayAccumulation:
    // 2. Perform ? Set(array, "length", 𝔽(len), true).
    try array.set(self.agent, PropertyKey.from("length"), Value.from(length), .throw);
}

fn executeArraySetValueDirect(self: *Vm, index: u32) Agent.Error!void {
    const value = self.stack.pop().?;
    const array = self.stack.pop().?.asObject();
    try array.property_storage.indexed_properties.set(self.agent.gc_allocator, index, .{
        .value_or_accessor = .{
            .value = value,
        },
        .attributes = .all,
    });
    self.result = Value.from(array);
}

fn executeArraySpreadValue(self: *Vm) Agent.Error!void {
    const array = self.stack.pop().?.asObject();
    var next_index: u53 = array.as(builtins.Array).fields.length;

    // From ArrayAccumulation:
    // 3. Let iteratorRecord be ? GetIterator(spreadObj, sync).
    var iterator = self.iterator_stack.pop().?;

    // 4. Repeat,
    //     a. Let next be ? IteratorStepValue(iteratorRecord).
    //     b. If next is done, return nextIndex.
    while (try iterator.stepValue(self.agent)) |next| : (next_index += 1) {
        // c. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), next).
        try array.createDataPropertyDirect(self.agent, PropertyKey.from(next_index), next);

        // d. Set nextIndex to nextIndex + 1.
    }
    self.result = Value.from(array);
}

fn executeAwait(self: *Vm) Agent.Error!void {
    const value = self.result.?;
    self.result = try await(self.agent, value);
}

fn executeBinaryOperatorAdd(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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
            try String.concat(self.agent, &.{ l_val.asString(), r_val.asString() }),
        );
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"+", r_val);
}

fn executeBinaryOperatorSub(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

fn executeBinaryOperatorMul(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

fn executeBinaryOperatorDiv(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // OPTIMIZATION: Fast path for number values
    if (l_val.isNumber() and r_val.isNumber()) {
        self.result = Value.from(l_val.__toF64() / r_val.__toF64());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"/", r_val);
}

fn executeBinaryOperatorMod(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"%", r_val);
}

fn executeBinaryOperatorExp(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"**", r_val);
}

fn executeBinaryOperatorLeftShift(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        const shift_count: u5 = @intCast(@mod(@as(u32, @bitCast(r_val.__asI32())), 32));
        self.result = Value.from(l_val.__asI32() << shift_count);
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"<<", r_val);
}

fn executeBinaryOperatorRightShift(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        const shift_count: u5 = @intCast(@mod(@as(u32, @bitCast(r_val.__asI32())), 32));
        self.result = Value.from(l_val.__asI32() >> shift_count);
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@">>", r_val);
}

fn executeBinaryOperatorUnsignedRightShift(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        const shift_count: u5 = @intCast(@mod(@as(u32, @bitCast(r_val.__asI32())), 32));
        self.result = Value.from(@as(u32, @bitCast(l_val.__asI32())) >> shift_count);
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@">>>", r_val);
}

fn executeBinaryOperatorBitwiseAnd(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        self.result = Value.from(l_val.__asI32() & r_val.__asI32());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"&", r_val);
}

fn executeBinaryOperatorBitwiseOr(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // OPTIMIZATION: Fast path for i32 values
    if (l_val.__isI32() and r_val.__isI32()) {
        self.result = Value.from(l_val.__asI32() | r_val.__asI32());
        return;
    }

    self.result = try applyStringOrNumericBinaryOperator(self.agent, l_val, .@"|", r_val);
}

fn executeBinaryOperatorBitwiseXor(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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
) Agent.Error!void {
    const class_declaration = self.executable.getAstNode(ast_node_index).class_declaration;
    self.result = Value.from(try bindingClassDeclarationEvaluation(self.agent, class_declaration));
}

fn executeBitwiseNot(self: *Vm) Agent.Error!void {
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
    block_type: BlockDeclarationInstantiationType,
) Agent.Error!void {
    const block = self.executable.getAstNode(block_index);
    const old_env = self.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    const block_env = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);
    try blockDeclarationInstantiation(
        self.agent,
        switch (block_type) {
            .statement_list => .{ .statement_list = block.statement_list },
            .case_block => .{ .case_block = block.case_block },
        },
        .{ .declarative_environment = block_env },
    );
    self.agent.runningExecutionContext().ecmascript_code.lexical_environment = .{
        .declarative_environment = block_env,
    };
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
fn executeClassDefinitionEvaluation(
    self: *Vm,
    class_expression_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const class_expression = self.executable.getAstNode(class_expression_index).class_expression;

    // ClassExpression : class BindingIdentifier ClassTail
    if (class_expression.identifier) |identifier| {
        // 1. Let className be the StringValue of BindingIdentifier.
        const class_name = try String.fromUtf8(self.agent, identifier);

        // 2. Let sourceText be the source text matched by ClassExpression.
        const source_text = class_expression.source_text;

        // 3. Return ? ClassDefinitionEvaluation of ClassTail with arguments className, className,
        //    and sourceText.
        const value = try classDefinitionEvaluation(
            self.agent,
            class_expression.class_tail,
            class_name,
            class_name,
            source_text,
        );
        self.result = Value.from(value);
    }
    // ClassExpression : class ClassTail
    else {
        // 1. Let sourceText be the source text matched by ClassExpression.
        const source_text = class_expression.source_text;

        // 2. Return ? ClassDefinitionEvaluation of ClassTail with arguments undefined, "", and sourceText.
        const value = try classDefinitionEvaluation(
            self.agent,
            class_expression.class_tail,
            null,
            .empty,
            source_text,
        );
        self.result = Value.from(value);
    }
}

fn executeCreateCatchBindings(
    self: *Vm,
    catch_parameter_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const catch_parameter = self.executable.getAstNode(catch_parameter_index).catch_parameter;

    // From CatchClauseEvaluation:
    // 1. Let oldEnv be the running execution context's LexicalEnvironment.
    const old_env = self.lexical_environment_stack.getLast();

    // 2. Let catchEnv be NewDeclarativeEnvironment(oldEnv).
    const catch_env = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);

    var bound_names: std.ArrayList(ast.Identifier) = .empty;
    defer bound_names.deinit(self.agent.gc_allocator);
    try catch_parameter.collectBoundNames(self.agent.gc_allocator, &bound_names);

    // 3. For each element argName of the BoundNames of CatchParameter, do
    for (bound_names.items) |arg_name_utf8| {
        const arg_name = try String.fromUtf8(self.agent, arg_name_utf8);

        // a. Perform ! catchEnv.CreateMutableBinding(argName, false).
        catch_env.createMutableBinding(self.agent, arg_name, false) catch |err| try noexcept(err);
    }

    // 4. Set the running execution context's LexicalEnvironment to catchEnv.
    self.agent.runningExecutionContext().ecmascript_code.lexical_environment = .{
        .declarative_environment = catch_env,
    };
}

fn executeCreateObjectPropertyIterator(self: *Vm) Agent.Error!void {
    const value = self.result.?;

    // From ForIn/OfHeadEvaluation:
    // b. Let obj be ! ToObject(exprValue).
    const object = value.toObject(self.agent) catch |err| try noexcept(err);

    // c. Let iterator be EnumerateObjectProperties(obj).
    const iterator = try createForInIterator(self.agent, object);

    // d. Let nextMethod be ! GetV(iterator, "next").
    const next_method = iterator.object.get(self.agent, PropertyKey.from("next")) catch |err| try noexcept(err);

    // e. Return the Iterator Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false }.
    const iterator_: Iterator = .{
        .iterator = &iterator.object,
        .next_method = next_method,
        .done = false,
    };
    try self.iterator_stack.append(self.agent.gc_allocator, iterator_);
}

fn executeCreateWithEnvironment(self: *Vm) Agent.Error!void {
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
    self.agent.runningExecutionContext().ecmascript_code.lexical_environment = new_env;
}

fn executeDecrement(self: *Vm) Agent.Error!void {
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

    self.result = Value.from(try value.asBigInt().subtract(self.agent, .one));
}

fn updateExpressionImpl(
    self: *Vm,
    reference: Reference,
    comptime op: enum { increment, decrement },
    comptime kind: enum { prefix, postfix },
) Agent.Error!void {
    const old_value = try reference.getValue(self.agent);
    const old_value_numeric, const new_value = switch (op) {
        .increment => blk: {
            // OPTIMIZATION: Fast path for number values
            if (old_value.isNumber()) {
                if (old_value.__isI32()) {
                    if (std.math.add(i32, old_value.__asI32(), 1)) |result| {
                        break :blk .{ old_value, Value.from(result) };
                    } else |_| {}
                }
                break :blk .{ old_value, Value.from(old_value.__toF64() + 1) };
            }
            const numeric = try old_value.toNumeric(self.agent);
            break :blk switch (numeric) {
                .number => |number| .{
                    Value.from(number),
                    Value.from(number.add(.from(1))),
                },
                .big_int => |big_int| .{
                    Value.from(big_int),
                    Value.from(try big_int.add(self.agent, .one)),
                },
            };
        },
        .decrement => blk: {
            // OPTIMIZATION: Fast path for number values
            if (old_value.isNumber()) {
                if (old_value.__isI32()) {
                    if (std.math.sub(i32, old_value.__asI32(), 1)) |result| {
                        break :blk .{ old_value, Value.from(result) };
                    } else |_| {}
                }
                break :blk .{ old_value, Value.from(old_value.__toF64() - 1) };
            }
            const numeric = try old_value.toNumeric(self.agent);
            break :blk switch (numeric) {
                .number => |number| .{
                    Value.from(number),
                    Value.from(number.subtract(.from(1))),
                },
                .big_int => |big_int| .{
                    Value.from(big_int),
                    Value.from(try big_int.subtract(self.agent, .one)),
                },
            };
        },
    };
    try reference.putValue(self.agent, new_value);
    self.result = switch (kind) {
        .prefix => new_value,
        .postfix => old_value_numeric,
    };
}

fn executeDecrementBindingPrefix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.updateExpressionImpl(reference, .decrement, .prefix);
}

fn executeDecrementBindingPostfix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.updateExpressionImpl(reference, .decrement, .postfix);
}

fn executeDecrementPropertyPrefix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    property_lookup_cache_index: Executable.PropertyLookupCacheIndex,
) Agent.Error!void {
    const property_name_string = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getPropertyLookupCacheEntry(property_lookup_cache_index);
    const base_value = self.stack.pop().?;
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{
            .value = Value.from(property_name_string),
        },
        .strict = strict,
        .this_value = null,
        .maybe_lookup_cache_entry = lookup_cache_entry,
    };
    try self.updateExpressionImpl(reference, .decrement, .prefix);
}

fn executeDecrementPropertyPostfix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    property_lookup_cache_index: Executable.PropertyLookupCacheIndex,
) Agent.Error!void {
    const property_name_string = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getPropertyLookupCacheEntry(property_lookup_cache_index);
    const base_value = self.stack.pop().?;
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{
            .value = Value.from(property_name_string),
        },
        .strict = strict,
        .this_value = null,
        .maybe_lookup_cache_entry = lookup_cache_entry,
    };
    try self.updateExpressionImpl(reference, .decrement, .postfix);
}

/// 13.5.1.2 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-delete-operator-runtime-semantics-evaluation
fn executeDelete(self: *Vm) Agent.Error!void {
    // NOTE: 1-2. are part of the generated bytecode.
    const reference = self.reference_stack.pop().?;

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
        const delete_status = try base_obj.internal_methods.delete(
            self.agent,
            base_obj,
            property_key,
        );

        // f. If deleteStatus is false and ref.[[Strict]] is true, throw a TypeError exception.
        if (!delete_status and reference.strict) {
            return self.agent.throwException(.type_error, "Could not delete property", .{});
        }

        // g. Return deleteStatus.
        self.result = Value.from(delete_status);
    } else {
        // 5. Else,
        // a. Let base be ref.[[Base]].
        // b. Assert: base is an Environment Record.
        const base = reference.base.environment;

        // c. Return ? base.DeleteBinding(ref.[[ReferencedName]]).
        self.result = Value.from(
            try base.deleteBinding(self.agent, reference.referenced_name.value.asString()),
        );
    }
}

fn executeDupIterator(self: *Vm) Agent.Error!void {
    const iterator = self.iterator_stack.getLast();
    try self.iterator_stack.append(self.agent.gc_allocator, iterator);
}

fn executeDupReference(self: *Vm) Agent.Error!void {
    const reference = self.reference_stack.getLast();
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

fn executeEvaluateCall(self: *Vm, args: Instruction.Arguments) Agent.Error!void {
    const arguments = try self.getArguments(args);
    const this_value = self.stack.pop().?;
    const function = self.stack.pop().?;

    self.result = try evaluateCall(
        self.agent,
        function,
        this_value,
        arguments,
    );
}

fn executeEvaluateCallDirectEval(
    self: *Vm,
    args: Instruction.Arguments,
    strict: bool,
) Agent.Error!void {
    const arguments = try self.getArguments(args);
    const this_value = self.stack.pop().?;
    const function = self.stack.pop().?;

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

fn executeEvaluateImportCall(self: *Vm) Agent.Error!void {
    const options = self.stack.pop().?;
    const specifier = self.stack.pop().?;
    self.result = try evaluateImportCall(self.agent, specifier, options);
}

fn executeEvaluateNew(self: *Vm, args: Instruction.Arguments) Agent.Error!void {
    const arguments = try self.getArguments(args);
    const constructor = self.stack.pop().?;
    self.result = try evaluateNew(self.agent, constructor, arguments);
}

/// 13.3.3 EvaluatePropertyAccessWithExpressionKey ( baseValue, expression, strict )
/// https://tc39.es/ecma262/#sec-evaluate-property-access-with-expression-key
fn executeEvaluatePropertyAccessWithExpressionKey(
    self: *Vm,
    strict: bool,
) Agent.Error!void {
    // 1. Let propertyNameReference be ? Evaluation of expression.
    // 2. Let propertyNameValue be ? GetValue(propertyNameReference).
    const property_name_value = self.stack.pop().?;

    const base_value = self.stack.pop().?;

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

fn executeEvaluatePropertyAccessWithExpressionKeyDirect(self: *Vm) Agent.Error!void {
    // Combines executeEvaluatePropertyAccessWithExpressionKey() and Reference.getValue(), entirely
    // bypassing the creation of a reference.
    const property_name_value = self.stack.pop().?;
    const base_value = self.stack.pop().?;
    const base_object = switch (base_value.type()) {
        .object => base_value.asObject(),
        .string => blk: {
            if (property_name_value.type() == .string and property_name_value.asString().eql(String.fromLiteral("length"))) {
                self.result = Value.from(base_value.asString().length);
                return;
            } else if (property_name_value.type() == .symbol) {
                break :blk (try base_value.synthesizePrototype(self.agent)).?;
            }
            // Might be a property handled by String's [[GetOwnProperty]], we need to make an object
            // TODO: This could have a fast path for numeric property lookups
            break :blk try base_value.toObject(self.agent);
        },
        // Guaranteed to throw
        .null, .undefined => try base_value.toObject(self.agent),
        else => (try base_value.synthesizePrototype(self.agent)).?,
    };
    const property_key = switch (property_name_value.type()) {
        .string => PropertyKey.from(property_name_value.asString()),
        .symbol => PropertyKey.from(property_name_value.asSymbol()),
        else => try property_name_value.toPropertyKey(self.agent),
    };
    self.result = try base_object.internal_methods.get(
        self.agent,
        base_object,
        property_key,
        base_value,
    );
}

/// 13.3.4 EvaluatePropertyAccessWithIdentifierKey ( baseValue, identifierName, strict )
/// https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key
fn executeEvaluatePropertyAccessWithIdentifierKey(
    self: *Vm,
    strict: bool,
    identifier_name_index: Executable.IdentifierIndex,
    property_name_lookup_cache_index: Executable.PropertyLookupCacheIndex,
) Agent.Error!void {
    // 1. Let propertyNameString be the StringValue of identifierName.
    const property_name_string = self.executable.getIdentifier(identifier_name_index);

    const lookup_cache_entry = self.executable.getPropertyLookupCacheEntry(property_name_lookup_cache_index);
    const base_value = self.stack.pop().?;

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

fn executeEvaluatePropertyAccessWithIdentifierKeyDirect(
    self: *Vm,
    identifier_name_index: Executable.IdentifierIndex,
    property_name_lookup_cache_index: Executable.PropertyLookupCacheIndex,
) Agent.Error!void {
    // Combines executeEvaluatePropertyAccessWithIdentifierKey() and Reference.getValue(), entirely
    // bypassing the creation of a reference.
    const property_name_string = self.executable.getIdentifier(identifier_name_index);
    const lookup_cache_entry = self.executable.getPropertyLookupCacheEntry(property_name_lookup_cache_index);
    const base_value = self.stack.pop().?;

    const base_object = switch (base_value.type()) {
        .object => base_value.asObject(),
        .string => blk: {
            if (property_name_string.eql(String.fromLiteral("length"))) {
                self.result = Value.from(base_value.asString().length);
                return;
            }
            // Might be a property handled by String's [[GetOwnProperty]], we need to make an object
            // TODO: This could have a fast path for numeric property lookups
            break :blk try base_value.toObject(self.agent);
        },
        // Guaranteed to throw
        .null, .undefined => try base_value.toObject(self.agent),
        else => (try base_value.synthesizePrototype(self.agent)).?,
    };

    if (lookup_cache_entry.*) |cache| {
        if (base_object.property_storage.shape == cache.shape) {
            switch (cache.index) {
                .value => |index| {
                    self.result = base_object.property_storage.values.items[@intFromEnum(index)];
                    return;
                },
                .accessor => |index| {
                    const accessor = base_object.property_storage.accessors.items[@intFromEnum(index)];
                    // Excerpt from ordinaryGet()
                    if (accessor.get) |getter| {
                        self.result = try Value.from(getter).callAssumeCallable(self.agent, base_value, &.{});
                    } else {
                        self.result = .undefined;
                    }
                    return;
                },
            }
        } else {
            lookup_cache_entry.* = null;
        }
    }

    const property_key = PropertyKey.from(property_name_string);
    self.result = try base_object.internal_methods.get(
        self.agent,
        base_object,
        property_key,
        base_value,
    );
    if (base_object.property_storage.shape.properties.get(property_key)) |property_metadata| {
        lookup_cache_entry.* = .{
            .shape = base_object.property_storage.shape,
            .index = property_metadata.index,
        };
    }
}

/// 13.3.7.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-super-keyword-runtime-semantics-evaluation
fn executeEvaluateSuperCall(self: *Vm, args: Instruction.Arguments) Agent.Error!void {
    // 1. Let newTarget be GetNewTarget().
    const new_target = self.agent.getNewTarget();

    // 2. Assert: newTarget is a constructor.
    std.debug.assert(new_target.?.internal_methods.construct != null);

    // 3. Let func be GetSuperConstructor().
    const function = try getSuperConstructor(self.agent);

    // 4. Let argList be ? ArgumentListEvaluation of Arguments.
    const arguments = try self.getArguments(args);

    // 5. If IsConstructor(func) is false, throw a TypeError exception.
    if (!function.isConstructor()) {
        return self.agent.throwException(.type_error, "{f} is not a constructor", .{function});
    }

    // 6. Let result be ? Construct(func, argList, newTarget).
    var result = try function.asObject().construct(self.agent, arguments, new_target);

    // 7. Let thisER be GetThisEnvironment().
    const this_environment = self.agent.getThisEnvironment();

    // 8. Assert: thisER is a Function Environment Record.
    // 9. Perform ? BindThisValue(thisER, result).
    try this_environment.function_environment.bindThisValue(self.agent, Value.from(result));

    // 10. Let F be thisER.[[FunctionObject]].
    // 11. Assert: F is an ECMAScript function object.
    const constructor = &this_environment.function_environment.function_object.object;

    // 12. Perform ? InitializeInstanceElements(result, F).
    try result.initializeInstanceElements(self.agent, constructor);

    // 13. Return result.
    self.result = Value.from(result);
}

fn executeForDeclarationBindingInstantiation(
    self: *Vm,
    lexical_declaration_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const lexical_declaration = self.executable.getAstNode(lexical_declaration_index).lexical_declaration;

    // From ForIn/OfBodyEvaluation:
    // iii. Let iterationEnv be NewDeclarativeEnvironment(oldEnv).
    const old_env = self.lexical_environment_stack.getLast();
    const environment = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);

    // iv. Perform ForDeclarationBindingInstantiation of lhs with argument iterationEnv.

    // 14.7.5.4 Runtime Semantics: ForDeclarationBindingInstantiation
    // https://tc39.es/ecma262/#sec-runtime-semantics-fordeclarationbindinginstantiation
    var bound_names: std.ArrayList(ast.Identifier) = .empty;
    defer bound_names.deinit(self.agent.gc_allocator);
    try lexical_declaration.collectBoundNames(self.agent.gc_allocator, &bound_names);

    // 1. For each element name of the BoundNames of ForBinding, do
    for (bound_names.items) |name_utf8| {
        const name = try String.fromUtf8(self.agent, name_utf8);

        // a. If IsConstantDeclaration of LetOrConst is true, then
        if (lexical_declaration.isConstantDeclaration()) {
            // i. Perform ! environment.CreateImmutableBinding(name, true).
            environment.createImmutableBinding(
                self.agent,
                name,
                true,
            ) catch |err| try noexcept(err);
        } else {
            // b. Else,
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
    self.agent.runningExecutionContext().ecmascript_code.lexical_environment = .{
        .declarative_environment = environment,
    };
}

fn executeGetIterator(self: *Vm, iterator_kind: IteratorKind) Agent.Error!void {
    const iterator = try getIterator(self.agent, self.result.?, iterator_kind);
    try self.iterator_stack.append(self.agent.gc_allocator, iterator);
}

fn executeGetNewTarget(self: *Vm) Agent.Error!void {
    self.result = if (self.agent.getNewTarget()) |new_target|
        Value.from(new_target)
    else
        .undefined;
}

/// 13.3.12.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-meta-properties-runtime-semantics-evaluation
fn executeGetOrCreateImportMeta(self: *Vm) Agent.Error!void {
    // 1. Let module be GetActiveScriptOrModule().
    // 2. Assert: module is a Source Text Module Record.
    var module = self.agent.getActiveScriptOrModule().?.module.source_text_module;

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
            try import_meta.createDataPropertyDirect(
                self.agent,
                entry.key_ptr.*,
                entry.value_ptr.*,
            );
        }

        // d. Perform HostFinalizeImportMeta(importMeta, module).
        self.agent.host_hooks.hostFinalizeImportMeta(import_meta, module);

        // e. Set module.[[ImportMeta]] to importMeta.
        module.import_meta = import_meta;

        // f. Return importMeta.
        self.result = Value.from(import_meta);
    } else {
        // 5. Else,
        // a. Assert: importMeta is an Object.
        // b. Return importMeta.
        self.result = Value.from(module.import_meta.?);
    }
}

fn executeGetTemplateObject(
    self: *Vm,
    template_literal_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const template_literal = &self.executable.getAstNode(template_literal_index).template_literal;
    const template_object = try getTemplateObject(self.agent, template_literal);
    self.result = Value.from(&template_object.object);
}

fn executeGetValue(self: *Vm) Agent.Error!void {
    const reference = self.reference_stack.pop().?;
    self.result = try reference.getValue(self.agent);
}

fn executeGreaterThan(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

    // 6. If r is undefined, return false; otherwise return r.
    self.result = Value.from(result orelse false);
}

fn executeGreaterThanEquals(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

    // 6. If r is either true or undefined, return false; otherwise return true.
    self.result = Value.from(!(result orelse true));
}

fn executeHasPrivateElement(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
) Agent.Error!void {
    const private_identifier = self.executable.getIdentifier(private_identifier_index);
    const r_val = self.stack.pop().?;

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
    const private_environment = self.agent.runningExecutionContext().ecmascript_code.private_environment.?;

    // 7. Let privateName be ResolvePrivateIdentifier(privateEnv, privateIdentifier).
    const private_name = private_environment.resolvePrivateIdentifier(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    );

    // 8. If PrivateElementFind(rVal, privateName) is not empty, return true.
    // 9. Return false.
    self.result = Value.from(r_val.asObject().privateElementFind(private_name) != null);
}

fn executeHasProperty(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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
        try r_val.asObject().hasProperty(self.agent, try l_val.toPropertyKey(self.agent)),
    );
}

fn executeIncrement(self: *Vm) Agent.Error!void {
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

    self.result = Value.from(try value.asBigInt().add(self.agent, .one));
}

fn executeIncrementBindingPrefix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.updateExpressionImpl(reference, .increment, .prefix);
}

fn executeIncrementBindingPostfix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.updateExpressionImpl(reference, .increment, .postfix);
}

fn executeIncrementPropertyPrefix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    property_lookup_cache_index: Executable.PropertyLookupCacheIndex,
) Agent.Error!void {
    const property_name_string = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getPropertyLookupCacheEntry(property_lookup_cache_index);
    const base_value = self.stack.pop().?;
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{
            .value = Value.from(property_name_string),
        },
        .strict = strict,
        .this_value = null,
        .maybe_lookup_cache_entry = lookup_cache_entry,
    };
    try self.updateExpressionImpl(reference, .increment, .prefix);
}

fn executeIncrementPropertyPostfix(
    self: *Vm,
    strict: bool,
    identifier_index: Executable.IdentifierIndex,
    property_lookup_cache_index: Executable.PropertyLookupCacheIndex,
) Agent.Error!void {
    const property_name_string = self.executable.getIdentifier(identifier_index);
    const lookup_cache_entry = self.executable.getPropertyLookupCacheEntry(property_lookup_cache_index);
    const base_value = self.stack.pop().?;
    const reference: Reference = .{
        .base = .{ .value = base_value },
        .referenced_name = .{
            .value = Value.from(property_name_string),
        },
        .strict = strict,
        .this_value = null,
        .maybe_lookup_cache_entry = lookup_cache_entry,
    };
    try self.updateExpressionImpl(reference, .increment, .postfix);
}

fn executeInitializeBoundName(
    self: *Vm,
    name_index: Executable.IdentifierIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(name_index);
    const value = self.stack.pop().?;
    const environment = self.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    try initializeBoundName(
        self.agent,
        name,
        value,
        .{ .environment = environment },
    );
}

fn executeInitializeReferencedBinding(self: *Vm) Agent.Error!void {
    const reference = self.reference_stack.pop().?;
    const value = self.result.?;
    try reference.initializeReferencedBinding(self.agent, value);
}

fn executeInstanceofOperator(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

    // 5. Return ? InstanceofOperator(lVal, rVal).
    self.result = Value.from(try l_val.instanceofOperator(self.agent, r_val));
}

fn executeInstantiateArrowFunctionExpression(
    self: *Vm,
    arrow_function_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const arrow_function = self.executable.getAstNode(arrow_function_index).arrow_function;
    const closure = try instantiateArrowFunctionExpression(
        self.agent,
        arrow_function,
        null,
    );
    self.result = Value.from(&closure.object);
}

fn executeInstantiateAsyncArrowFunctionExpression(
    self: *Vm,
    async_arrow_function_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const async_arrow_function = self.executable.getAstNode(async_arrow_function_index).async_arrow_function;
    const closure = try instantiateAsyncArrowFunctionExpression(
        self.agent,
        async_arrow_function,
        null,
    );
    self.result = Value.from(&closure.object);
}

fn executeInstantiateAsyncFunctionExpression(
    self: *Vm,
    async_function_expression_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const async_function_expression = self.executable.getAstNode(async_function_expression_index).async_function_expression;
    const closure = try instantiateAsyncFunctionExpression(
        self.agent,
        async_function_expression,
        null,
    );
    self.result = Value.from(&closure.object);
}

fn executeInstantiateAsyncGeneratorFunctionExpression(
    self: *Vm,
    async_generator_expression_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const async_generator_expression = self.executable.getAstNode(async_generator_expression_index).async_generator_expression;
    const closure = try instantiateAsyncGeneratorFunctionExpression(
        self.agent,
        async_generator_expression,
        null,
    );
    self.result = Value.from(&closure.object);
}

fn executeInstantiateGeneratorFunctionExpression(
    self: *Vm,
    generator_expression_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const generator_expression = self.executable.getAstNode(generator_expression_index).generator_expression;
    const closure = try instantiateGeneratorFunctionExpression(
        self.agent,
        generator_expression,
        null,
    );
    self.result = Value.from(&closure.object);
}

fn executeInstantiateOrdinaryFunctionExpression(
    self: *Vm,
    function_expression_index: Executable.AstNodeIndex,
) Agent.Error!void {
    const function_expression = self.executable.getAstNode(function_expression_index).function_expression;
    const closure = try instantiateOrdinaryFunctionExpression(
        self.agent,
        function_expression,
        null,
    );
    self.result = Value.from(&closure.object);
}

fn executeIsLooselyEqual(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

fn executeIsStrictlyEqual(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

fn executeJump(self: *Vm, index: Executable.InstructionIndex) Agent.Error!void {
    self.ip = @intFromEnum(index);
}

fn executeJumpConditional(
    self: *Vm,
    ip_consequent: Executable.InstructionIndex,
    ip_alternate: Executable.InstructionIndex,
) Agent.Error!void {
    const value = self.result.?;
    self.ip = if (value.toBoolean()) @intFromEnum(ip_consequent) else @intFromEnum(ip_alternate);
}

fn executeLessThan(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

    // 6. If r is undefined, return false; otherwise return r.
    self.result = Value.from(result orelse false);
}

fn executeLessThanEquals(self: *Vm) Agent.Error!void {
    const r_val = self.stack.pop().?;
    const l_val = self.stack.pop().?;

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

    // 6. If r is either true or undefined, return false; otherwise return true.
    self.result = Value.from(!(result orelse true));
}

fn executeLoad(self: *Vm) Agent.Error!void {
    // Handle null value to allow load of 'empty' result at beginning of script
    if (self.result) |value| try self.stack.append(self.agent.gc_allocator, value);
}

fn executeLoadConstant(
    self: *Vm,
    value_index: Executable.ConstantIndex,
) Agent.Error!void {
    const value = self.executable.getConstant(value_index);
    try self.stack.append(self.agent.gc_allocator, value);
}

fn executeLoadAndClearException(self: *Vm) Agent.Error!void {
    const value = self.exception.?.value;
    self.exception = null;
    try self.stack.append(self.agent.gc_allocator, value);
}

fn executeLoadIteratorNextArgs(self: *Vm) Agent.Error!void {
    const iterator = self.iterator_stack.pop().?;
    try self.stack.append(self.agent.gc_allocator, iterator.next_method);
    try self.stack.append(self.agent.gc_allocator, Value.from(iterator.iterator));
}

fn executeLoadThisValueForEvaluateCall(self: *Vm) Agent.Error!void {
    const reference = self.reference_stack.pop().?;
    const this_value = evaluateCallGetThisValue(reference);
    try self.stack.append(self.agent.gc_allocator, this_value);
}

fn executeLoadThisValueForMakeSuperPropertyReference(self: *Vm) Agent.Error!void {
    // 1. Let env be GetThisEnvironment().
    const env = self.agent.getThisEnvironment();

    // 2. Let actualThis be ? env.GetThisBinding().
    const actual_this = try env.getThisBinding(self.agent);

    try self.stack.append(self.agent.gc_allocator, actual_this);
}

fn executeLogicalNot(self: *Vm) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(!value.toBoolean());
}

/// 6.2.5.9 MakePrivateReference ( baseValue, privateIdentifier )
/// https://tc39.es/ecma262/#sec-makeprivatereference
fn executeMakePrivateReference(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
) Agent.Error!void {
    const private_identifier = self.executable.getIdentifier(private_identifier_index);
    const base_value = self.stack.pop().?;

    // 1. Let privateEnv be the running execution context's PrivateEnvironment.
    // 2. Assert: privateEnv is not null.
    const private_env = self.agent.runningExecutionContext().ecmascript_code.private_environment.?;

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

fn executeMakePrivateReferenceDirect(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
) Agent.Error!void {
    // Combines executeMakePrivateReference() and Reference.getValue(), entirely bypassing the
    // creation of a reference.
    const private_identifier = self.executable.getIdentifier(private_identifier_index);
    const base_value = self.stack.pop().?;
    const private_env = self.agent.runningExecutionContext().ecmascript_code.private_environment.?;
    const private_name = private_env.resolvePrivateIdentifier(
        try private_identifier.toUtf8(self.agent.gc_allocator),
    );
    const base_object = switch (base_value.type()) {
        .object => base_value.asObject(),
        // Guaranteed to throw
        .null, .undefined => try base_value.toObject(self.agent),
        else => (try base_value.synthesizePrototype(self.agent)).?,
    };
    self.result = try base_object.privateGet(self.agent, private_name);
}

/// 13.3.7.3 MakeSuperPropertyReference ( actualThis, propertyKey, strict )
/// https://tc39.es/ecma262/#sec-makesuperpropertyreference
fn executeMakeSuperPropertyReference(self: *Vm, strict: bool) Agent.Error!void {
    const property_key = self.stack.pop().?;
    const actual_this = self.stack.pop().?;

    // 1. Let env be GetThisEnvironment().
    const env = self.agent.getThisEnvironment();

    // 2. Assert: env.HasSuperBinding() is true.
    std.debug.assert(env.hasSuperBinding());

    // 3. Assert: env is a Function Environment Record.
    // 4. Let baseValue be GetSuperBase(env).
    const base_value = try env.function_environment.getSuperBase(self.agent);

    // 5. Return the Reference Record {
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

fn executeObjectCreate(self: *Vm) Agent.Error!void {
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
) Agent.Error!void {
    const function_or_class = self.executable.getAstNode(function_or_class_index);
    const method = switch (method_type) {
        inline else => |@"type"| @unionInit(
            ast.MethodDefinition.Method,
            @tagName(@"type"),
            @field(function_or_class, switch (@"type") {
                .method, .get, .set => "function_expression",
                .generator => "generator_expression",
                .async => "async_function_expression",
                .async_generator => "async_generator_expression",
            }),
        ),
    };
    const property_name = self.stack.pop().?;
    const object = self.stack.pop().?.asObject();
    _ = try methodDefinitionEvaluation(
        self.agent,
        .{ .property_name = property_name, .method = method },
        object,
        true,
    );
    self.result = Value.from(object);
}

fn executeObjectSetProperty(self: *Vm) Agent.Error!void {
    const property_value = self.stack.pop().?;
    const property_name = try self.stack.pop().?.toPropertyKey(self.agent);
    const object = self.stack.pop().?.asObject();
    // From PropertyDefinitionEvaluation:
    // 5. Perform ! CreateDataPropertyOrThrow(object, propName, propValue).
    try object.createDataPropertyDirect(self.agent, property_name, property_value);
    self.result = Value.from(object);
}

fn executeObjectSetPrototype(self: *Vm) Agent.Error!void {
    const property_value = self.stack.pop().?;
    const object = self.stack.pop().?.asObject();
    // From PropertyDefinitionEvaluation:
    // a. If propValue is an Object or propValue is null, then
    if (property_value.isObject() or property_value.isNull()) {
        // i. Perform ! object.[[SetPrototypeOf]](propValue).
        const prototype = if (property_value.isObject()) property_value.asObject() else null;
        _ = object.internal_methods.setPrototypeOf(
            self.agent,
            object,
            prototype,
        ) catch |err| try noexcept(err);
    }
    self.result = Value.from(object);
}

fn executeObjectSpreadValue(self: *Vm) Agent.Error!void {
    const from_value = self.stack.pop().?;
    var object = self.stack.pop().?.asObject();
    const excluded_names: []const PropertyKey = &.{};
    // From PropertyDefinitionEvaluation:
    // 4. Perform ? CopyDataProperties(object, fromValue, excludedNames).
    try object.copyDataProperties(self.agent, from_value, excluded_names);
    self.result = Value.from(object);
}

fn executePopExceptionJumpTarget(self: *Vm) Agent.Error!void {
    _ = self.exception_jump_target_stack.pop().?;
}

fn executePopIterator(self: *Vm) Agent.Error!void {
    _ = self.iterator_stack.pop().?;
}

fn executePopLexicalEnvironment(self: *Vm) Agent.Error!void {
    _ = self.lexical_environment_stack.pop().?;
}

fn executePopReference(self: *Vm) Agent.Error!void {
    _ = self.reference_stack.pop().?;
}

fn executePushLexicalEnvironment(self: *Vm) Agent.Error!void {
    const lexical_environment = self.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    try self.lexical_environment_stack.append(self.agent.gc_allocator, lexical_environment);
}

fn executePushExceptionJumpTarget(
    self: *Vm,
    jump_target: Executable.InstructionIndex,
) Agent.Error!void {
    try self.exception_jump_target_stack.append(self.agent.gc_allocator, @intFromEnum(jump_target));
}

fn executePutValue(self: *Vm) Agent.Error!void {
    const l_ref = self.reference_stack.pop().?;
    const r_val = self.result.?;
    try l_ref.putValue(self.agent, r_val);
}

fn executeRegExpCreate(self: *Vm) Agent.Error!void {
    const flags = self.stack.pop().?;
    const pattern = self.stack.pop().?;
    const reg_exp = try builtins.regExpCreate(self.agent, pattern, flags);
    self.result = Value.from(&reg_exp.object);
}

fn executeResolveBinding(
    self: *Vm,
    name_index: Executable.IdentifierIndex,
    strict: bool,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(name_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
    try self.reference_stack.append(self.agent.gc_allocator, reference);
}

fn executeResolveBindingDirect(
    self: *Vm,
    name_index: Executable.IdentifierIndex,
    strict: bool,
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    // Combines Agent.resolveBinding() and Reference.getValue(), entirely bypassing the creation of
    // a reference.
    const name = self.executable.getIdentifier(name_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);
    var env = self.agent.runningExecutionContext().ecmascript_code.lexical_environment;
    if (lookup_cache_entry.*) |cache| {
        for (0..cache.distance) |_| {
            env = env.outerEnv() orelse {
                @branchHint(.unlikely);
                return self.agent.throwException(.reference_error, "'{f}' is not defined", .{name.fmtRaw()});
            };
        }
    } else {
        var distance: usize = 0;
        defer lookup_cache_entry.* = .{ .distance = distance };
        while (!try env.hasBinding(self.agent, name)) : (distance += 1) {
            env = env.outerEnv() orelse {
                @branchHint(.unlikely);
                return self.agent.throwException(.reference_error, "'{f}' is not defined", .{name.fmtRaw()});
            };
        }
    }
    self.result = try env.getBindingValue(self.agent, name, strict);
}

/// 15.7.16 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-class-definitions-runtime-semantics-evaluation
fn executeResolvePrivateIdentifier(
    self: *Vm,
    private_identifier_index: Executable.IdentifierIndex,
) Agent.Error!void {
    // 1. Let privateIdentifier be the StringValue of PrivateIdentifier.
    const private_identifier = self.executable.getIdentifier(private_identifier_index);

    // 2. Let privateEnvRec be the running execution context's PrivateEnvironment.
    const private_environment = self.agent.runningExecutionContext().ecmascript_code.private_environment.?;

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

fn executeResolveThisBinding(self: *Vm) Agent.Error!void {
    // NOTE: Caching the this value currently relies on the fact that each function runs in its own
    //       Vm, it'll need to be cleared between calls if that changes.
    if (self.cached_this_value == null) {
        self.cached_this_value = try self.agent.resolveThisBinding();
    }
    self.result = self.cached_this_value.?;
}

fn executeRestoreLexicalEnvironment(self: *Vm) Agent.Error!void {
    const lexical_environment = self.lexical_environment_stack.getLast();
    self.agent.runningExecutionContext().ecmascript_code.lexical_environment = lexical_environment;
}

fn executeRethrowExceptionIfAny(self: *Vm) Agent.Error!void {
    if (self.exception) |exception| {
        self.agent.exception = exception;
        return error.ExceptionThrown;
    }
}

fn executeReturn(_: *Vm) Agent.Error!void {
    @compileError("Should not be used"); // Handled in run()
}

fn executeStore(self: *Vm) Agent.Error!void {
    // Handle empty stack to allow restoring a null `.load`
    self.result = self.stack.pop();
}

fn executeStoreConstant(
    self: *Vm,
    value_index: Executable.ConstantIndex,
) Agent.Error!void {
    const value = self.executable.getConstant(value_index);
    self.result = value;
}

fn executeThrow(self: *Vm) Agent.Error!void {
    const value = self.result.?;
    self.agent.exception = .{
        .value = value,
        .stack_trace = try self.agent.captureStackTrace(),
    };
    return error.ExceptionThrown;
}

fn executeThrowCallAssignmentReferenceError(self: *Vm) Agent.Error!void {
    return self.agent.throwException(.reference_error, "Invalid assignment to function call", .{});
}

fn executeToNumber(self: *Vm) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: If we already have a number value return early
    if (value.isNumber()) return;

    self.result = Value.from(try value.toNumber(self.agent));
}

fn executeToNumeric(self: *Vm) Agent.Error!void {
    const value = self.result.?;

    // OPTIMIZATION: If we already have a numeric value return early
    if (value.isNumber() or value.isBigInt()) return;

    const numeric = try value.toNumeric(self.agent);
    self.result = switch (numeric) {
        .number => |number| Value.from(number),
        .big_int => |big_int| Value.from(big_int),
    };
}

fn executeToObject(self: *Vm) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(try value.toObject(self.agent));
}

fn executeToString(self: *Vm) Agent.Error!void {
    const value = self.result.?;
    self.result = Value.from(try value.toString(self.agent));
}

/// 13.5.3.1 Runtime Semantics: Evaluation
/// https://tc39.es/ecma262/#sec-typeof-operator-runtime-semantics-evaluation
fn executeTypeof(self: *Vm) Agent.Error!void {
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
    environment_lookup_cache_index: Executable.EnvironmentLookupCacheIndex,
) Agent.Error!void {
    const name = self.executable.getIdentifier(name_index);
    const lookup_cache_entry = self.executable.getEnvironmentLookupCacheEntry(environment_lookup_cache_index);

    // 1. Let val be ? Evaluation of UnaryExpression.
    const reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);

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

fn executeUnaryMinus(self: *Vm) Agent.Error!void {
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

fn executeYield(_: *Vm) Agent.Error!void {
    @compileError("Should not be used"); // Handled in run()
}

pub fn run(self: *Vm) Agent.Error!Completion {
    std.debug.assert(@as(Instruction.Tag, @enumFromInt(self.executable.instructions.getLast())) == .end);
    main: switch (self.fetchInstructionTag()) {
        .@"return" => return Completion.@"return"(self.result.?),
        .yield => return yield(self.agent, self.result.?),
        .end => return Completion.normal(self.result),
        inline else => |tag| {
            const Payload = Instruction.Payload(tag);
            const payload_ptr: *align(1) const Payload = @ptrCast(self.executable.instructions.items[self.ip..][0..@sizeOf(Payload)]);
            self.ip += @sizeOf(Payload);
            try @call(
                .always_inline,
                Vm.executeInstruction,
                .{ self, tag, payload_ptr.* },
            );
            continue :main self.fetchInstructionTag();
        },
    }
}

fn executeInstruction(
    self: *Vm,
    comptime tag: Instruction.Tag,
    payload: Instruction.Payload(tag),
) Agent.Error!void {
    (switch (tag) {
        .array_create => self.executeArrayCreate(payload.length),
        .array_push_value => self.executeArrayPushValue(),
        .array_set_length => self.executeArraySetLength(payload.length),
        .array_set_value_direct => self.executeArraySetValueDirect(payload.index),
        .array_spread_value => self.executeArraySpreadValue(),
        .await => self.executeAwait(),
        .binary_operator_add => self.executeBinaryOperatorAdd(),
        .binary_operator_sub => self.executeBinaryOperatorSub(),
        .binary_operator_mul => self.executeBinaryOperatorMul(),
        .binary_operator_div => self.executeBinaryOperatorDiv(),
        .binary_operator_mod => self.executeBinaryOperatorMod(),
        .binary_operator_exp => self.executeBinaryOperatorExp(),
        .binary_operator_left_shift => self.executeBinaryOperatorLeftShift(),
        .binary_operator_right_shift => self.executeBinaryOperatorRightShift(),
        .binary_operator_unsigned_right_shift => self.executeBinaryOperatorUnsignedRightShift(),
        .binary_operator_bitwise_and => self.executeBinaryOperatorBitwiseAnd(),
        .binary_operator_bitwise_or => self.executeBinaryOperatorBitwiseOr(),
        .binary_operator_bitwise_xor => self.executeBinaryOperatorBitwiseXor(),
        .binding_class_declaration_evaluation => self.executeBindingClassDeclarationEvaluation(payload),
        .bitwise_not => self.executeBitwiseNot(),
        .block_declaration_instantiation => self.executeBlockDeclarationInstantiation(payload.ast_node, payload.type),
        .class_definition_evaluation => self.executeClassDefinitionEvaluation(payload),
        .create_catch_bindings => self.executeCreateCatchBindings(payload),
        .create_object_property_iterator => self.executeCreateObjectPropertyIterator(),
        .create_with_environment => self.executeCreateWithEnvironment(),
        .decrement => self.executeDecrement(),
        .decrement_binding_prefix => self.executeDecrementBindingPrefix(payload.strict, payload.identifier, payload.environment_lookup_cache_index),
        .decrement_binding_postfix => self.executeDecrementBindingPostfix(payload.strict, payload.identifier, payload.environment_lookup_cache_index),
        .decrement_property_prefix => self.executeDecrementPropertyPrefix(payload.strict, payload.identifier, payload.property_lookup_cache_index),
        .decrement_property_postfix => self.executeDecrementPropertyPostfix(payload.strict, payload.identifier, payload.property_lookup_cache_index),
        .delete => self.executeDelete(),
        .dup_iterator => self.executeDupIterator(),
        .dup_reference => self.executeDupReference(),
        .evaluate_call => self.executeEvaluateCall(payload.arguments),
        .evaluate_call_direct_eval => self.executeEvaluateCallDirectEval(payload.arguments, payload.strict),
        .evaluate_import_call => self.executeEvaluateImportCall(),
        .evaluate_new => self.executeEvaluateNew(payload.arguments),
        .evaluate_property_access_with_expression_key => self.executeEvaluatePropertyAccessWithExpressionKey(payload.strict),
        .evaluate_property_access_with_expression_key_direct => self.executeEvaluatePropertyAccessWithExpressionKeyDirect(),
        .evaluate_property_access_with_identifier_key => self.executeEvaluatePropertyAccessWithIdentifierKey(payload.strict, payload.identifier, payload.property_lookup_cache_index),
        .evaluate_property_access_with_identifier_key_direct => self.executeEvaluatePropertyAccessWithIdentifierKeyDirect(payload.identifier, payload.property_lookup_cache_index),
        .evaluate_super_call => self.executeEvaluateSuperCall(payload.arguments),
        .for_declaration_binding_instantiation => self.executeForDeclarationBindingInstantiation(payload),
        .get_iterator => self.executeGetIterator(payload.kind),
        .get_new_target => self.executeGetNewTarget(),
        .get_or_create_import_meta => self.executeGetOrCreateImportMeta(),
        .get_template_object => self.executeGetTemplateObject(payload),
        .get_value => self.executeGetValue(),
        .greater_than => self.executeGreaterThan(),
        .greater_than_equals => self.executeGreaterThanEquals(),
        .has_private_element => self.executeHasPrivateElement(payload),
        .has_property => self.executeHasProperty(),
        .increment => self.executeIncrement(),
        .increment_binding_prefix => self.executeIncrementBindingPrefix(payload.strict, payload.identifier, payload.environment_lookup_cache_index),
        .increment_binding_postfix => self.executeIncrementBindingPostfix(payload.strict, payload.identifier, payload.environment_lookup_cache_index),
        .increment_property_prefix => self.executeIncrementPropertyPrefix(payload.strict, payload.identifier, payload.property_lookup_cache_index),
        .increment_property_postfix => self.executeIncrementPropertyPostfix(payload.strict, payload.identifier, payload.property_lookup_cache_index),
        .initialize_bound_name => self.executeInitializeBoundName(payload),
        .initialize_referenced_binding => self.executeInitializeReferencedBinding(),
        .instanceof_operator => self.executeInstanceofOperator(),
        .instantiate_arrow_function_expression => self.executeInstantiateArrowFunctionExpression(payload),
        .instantiate_async_arrow_function_expression => self.executeInstantiateAsyncArrowFunctionExpression(payload),
        .instantiate_async_function_expression => self.executeInstantiateAsyncFunctionExpression(payload),
        .instantiate_async_generator_function_expression => self.executeInstantiateAsyncGeneratorFunctionExpression(payload),
        .instantiate_generator_function_expression => self.executeInstantiateGeneratorFunctionExpression(payload),
        .instantiate_ordinary_function_expression => self.executeInstantiateOrdinaryFunctionExpression(payload),
        .is_loosely_equal => self.executeIsLooselyEqual(),
        .is_strictly_equal => self.executeIsStrictlyEqual(),
        .jump => self.executeJump(payload),
        .jump_conditional => self.executeJumpConditional(payload.consequent, payload.alternate),
        .less_than => self.executeLessThan(),
        .less_than_equals => self.executeLessThanEquals(),
        .load => self.executeLoad(),
        .load_constant => self.executeLoadConstant(payload),
        .load_and_clear_exception => self.executeLoadAndClearException(),
        .load_iterator_next_args => self.executeLoadIteratorNextArgs(),
        .load_this_value_for_evaluate_call => self.executeLoadThisValueForEvaluateCall(),
        .load_this_value_for_make_super_property_reference => self.executeLoadThisValueForMakeSuperPropertyReference(),
        .logical_not => self.executeLogicalNot(),
        .make_private_reference => self.executeMakePrivateReference(payload),
        .make_private_reference_direct => self.executeMakePrivateReferenceDirect(payload),
        .make_super_property_reference => self.executeMakeSuperPropertyReference(payload.strict),
        .object_create => self.executeObjectCreate(),
        .object_define_method => self.executeObjectDefineMethod(payload.ast_node, payload.type),
        .object_set_property => self.executeObjectSetProperty(),
        .object_set_prototype => self.executeObjectSetPrototype(),
        .object_spread_value => self.executeObjectSpreadValue(),
        .pop_exception_jump_target => self.executePopExceptionJumpTarget(),
        .pop_iterator => self.executePopIterator(),
        .pop_lexical_environment => self.executePopLexicalEnvironment(),
        .pop_reference => self.executePopReference(),
        .push_lexical_environment => self.executePushLexicalEnvironment(),
        .push_exception_jump_target => self.executePushExceptionJumpTarget(payload),
        .put_value => self.executePutValue(),
        .reg_exp_create => self.executeRegExpCreate(),
        .resolve_binding => self.executeResolveBinding(payload.identifier, payload.strict, payload.environment_lookup_cache_index),
        .resolve_binding_direct => self.executeResolveBindingDirect(payload.identifier, payload.strict, payload.environment_lookup_cache_index),
        .resolve_private_identifier => self.executeResolvePrivateIdentifier(payload),
        .resolve_this_binding => self.executeResolveThisBinding(),
        .restore_lexical_environment => self.executeRestoreLexicalEnvironment(),
        .rethrow_exception_if_any => self.executeRethrowExceptionIfAny(),
        .store => self.executeStore(),
        .store_constant => self.executeStoreConstant(payload),
        .throw => self.executeThrow(),
        .throw_call_assignment_reference_error => self.executeThrowCallAssignmentReferenceError(),
        .to_number => self.executeToNumber(),
        .to_numeric => self.executeToNumeric(),
        .to_object => self.executeToObject(),
        .to_string => self.executeToString(),
        .typeof => self.executeTypeof(),
        .typeof_identifier => self.executeTypeofIdentifier(payload.identifier, payload.strict, payload.environment_lookup_cache_index),
        .unary_minus => self.executeUnaryMinus(),
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
