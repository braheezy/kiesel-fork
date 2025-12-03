//! 27.5 Generator Objects
//! https://tc39.es/ecma262/#sec-generator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const bytecode = @import("../language/bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Completion = types.Completion;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const Vm = bytecode.Vm;
const asyncGeneratorYield = builtins.asyncGeneratorYield;
const await = builtins.await;
const createIteratorResultObject = types.createIteratorResultObject;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 27.5.1 The %GeneratorPrototype% Object
/// https://tc39.es/ecma262/#sec-properties-of-generator-prototype
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Iterator.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "next", next, 1, realm);
        try object.defineBuiltinFunction(agent, "return", @"return", 1, realm);
        try object.defineBuiltinFunction(agent, "throw", throw, 1, realm);

        // 27.5.1.1 %GeneratorPrototype%.constructor
        // https://tc39.es/ecma262/#sec-generator.prototype.constructor
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%GeneratorFunction.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 27.5.1.5 %GeneratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-generator.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Generator"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 27.5.1.2 %GeneratorPrototype%.next ( value )
    /// https://tc39.es/ecma262/#sec-generator.prototype.next
    fn next(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Return ? GeneratorResume(this value, value, empty).
        return Value.from(try generatorResume(agent, this_value, value));
    }

    /// 27.5.1.3 %GeneratorPrototype%.return ( value )
    /// https://tc39.es/ecma262/#sec-generator.prototype.return
    fn @"return"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Let g be the this value.
        const generator = this_value;

        // 2. Let C be ReturnCompletion(value).
        const completion = Completion.@"return"(value);

        // 3. Return ? GeneratorResumeAbrupt(g, C, empty).
        return Value.from(try generatorResumeAbrupt(agent, generator, completion));
    }

    /// 27.5.1.4 %GeneratorPrototype%.throw ( exception )
    /// https://tc39.es/ecma262/#sec-generator.prototype.return
    fn throw(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const exception = arguments.get(0);

        // 1. Let g be the this value.
        const generator = this_value;

        // 2. Let C be ThrowCompletion(exception).
        const completion = Completion.throw(exception);

        // 3. Return ? GeneratorResumeAbrupt(g, C, empty).
        return Value.from(try generatorResumeAbrupt(agent, generator, completion));
    }
};

/// 27.5.2 Properties of Generator Instances
/// https://tc39.es/ecma262/#sec-properties-of-generator-instances
pub const Generator = MakeObject(.{
    .Fields = struct {
        pub const State = enum {
            suspended_start,
            suspended_yield,
            executing,
            completed,
        };

        /// [[GeneratorState]]
        generator_state: State,

        /// [[GeneratorContext]]
        generator_context: *ExecutionContext,

        // Non-standard
        evaluation_state: struct {
            vm: Vm,
            closure: *const fn (*Agent, *builtins.ECMAScriptFunction, Completion) Agent.Error!*Object,
            generator_function: *builtins.ECMAScriptFunction,
            suspension_result: ?Value = null,
        },
    },
    .tag = .generator,
    .display_name = "Generator",
});

/// 27.5.3.1 GeneratorStart ( generator, generatorBody )
/// https://tc39.es/ecma262/#sec-generatorstart
pub fn generatorStart(
    agent: *Agent,
    generator: *Generator,
    generator_function: *builtins.ECMAScriptFunction,
) std.mem.Allocator.Error!void {
    // 1. Assert: generator.[[GeneratorState]] is suspended-start.
    std.debug.assert(generator.fields.generator_state == .suspended_start);

    // 2. Let genContext be the running execution context.
    const generator_context = agent.runningExecutionContext();

    // 3. Set the Generator component of genContext to generator.
    generator_context.generator = .{ .generator = generator };

    // 4. Let closure be a new Abstract Closure with no parameters that captures generatorBody and
    //    performs the following steps when called:
    const closure = struct {
        fn func(
            agent_: *Agent,
            generator_function_: *builtins.ECMAScriptFunction,
            resume_completion: Completion,
        ) Agent.Error!*Object {
            // a. Let acGenContext be the running execution context.
            const closure_generator_context = agent_.runningExecutionContext();

            // b. Let acGenerator be the Generator component of acGenContext.
            const closure_generator = closure_generator_context.generator.generator;

            // c. If generatorBody is a Parse Node, then
            const result = if (true) blk: {
                // TODO: Integrate throw/return completions with exception handlers in the Vm
                switch (resume_completion.type) {
                    .normal => closure_generator.fields.evaluation_state.vm.result = resume_completion.value.?,
                    .@"return" => break :blk resume_completion,
                    .throw => {
                        agent_.exception = .{
                            .value = resume_completion.value.?,
                            .stack_trace = try agent_.captureStackTrace(),
                        };
                        break :blk error.ExceptionThrown;
                    },
                    else => unreachable,
                }

                // i. Let result be Completion(Evaluation of generatorBody).
                break :blk generator_function_.fields.evaluateBody(
                    agent_,
                    &closure_generator.fields.evaluation_state.vm,
                );
            } else {
                // TODO: d. Else,
                // i. Assert: generatorBody is an Abstract Closure with no parameters.
                // ii. Let result be Completion(generatorBody()).
            };

            // e. Assert: If we return here, the generator either threw an exception or performed
            //    either an implicit or explicit return.

            if (closure_generator.fields.evaluation_state.suspension_result) |suspension_result| {
                closure_generator.fields.evaluation_state.suspension_result = null;
                return suspension_result.asObject();
            }

            // f. Remove acGenContext from the execution context stack and restore the execution
            //    context that is at the top of the execution context stack as the running
            //    execution context.
            _ = agent_.execution_context_stack.pop().?;

            // g. Set acGenerator.[[GeneratorState]] to completed.
            closure_generator.fields.generator_state = .completed;
            closure_generator.fields.evaluation_state.vm.deinit();

            // h. NOTE: Once a generator enters the completed state it never leaves it and its
            //    associated execution context is never resumed. Any execution state associated
            //    with acGenerator can be discarded at this point.
            closure_generator.fields.evaluation_state = undefined;

            const result_value: Value = if (result) |completion| switch (completion.type) {
                // i. If result is a normal completion, then
                //     i. Let resultValue be undefined.
                .normal => .undefined,

                // j. Else if result is a return completion, then
                //     i. Let resultValue be result.[[Value]].
                .@"return" => completion.value.?,

                else => unreachable,
            } else |err| {
                // k. Else,
                // i. Assert: result is a throw completion.
                // ii. Return ? result.
                return err;
            };

            // l. Return NormalCompletion(CreateIteratorResultObject(resultValue, true)).
            return createIteratorResultObject(agent_, result_value, true);
        }
    }.func;

    // 5. Set the code evaluation state of genContext such that when evaluation is resumed for that
    //    execution context, closure will be called with no arguments.
    generator.fields.evaluation_state = .{
        .vm = try Vm.init(agent, undefined),
        .closure = closure,
        .generator_function = generator_function,
    };

    // 6. Set generator.[[GeneratorContext]] to genContext.
    generator.fields.generator_context = generator_context;

    // 7. Return unused.
}

/// 27.5.3.2 GeneratorValidate ( generator, generatorBrand )
/// https://tc39.es/ecma262/#sec-generatorvalidate
pub fn generatorValidate(agent: *Agent, generator_value: Value) Agent.Error!Generator.Fields.State {
    // 1. Perform ? RequireInternalSlot(generator, [[GeneratorState]]).
    // 2. Perform ? RequireInternalSlot(generator, [[GeneratorBrand]]).
    const generator = try generator_value.requireInternalSlot(agent, Generator);

    // 3. If generator.[[GeneratorBrand]] is not generatorBrand, throw a TypeError exception.
    // NOTE: All iterators using [[GeneratorBrand]] in the spec are implemented without generators
    //       so this is currently not needed.

    // 4. Assert: generator also has a [[GeneratorContext]] internal slot.
    // 5. Let state be generator.[[GeneratorState]].
    const state = generator.fields.generator_state;

    // 6. If state is executing, throw a TypeError exception.
    if (state == .executing) {
        return agent.throwException(.type_error, "Generator is currently executing", .{});
    }

    // 7. Return state.
    return state;
}

/// 27.5.3.3 GeneratorResume ( generator, value, generatorBrand )
/// https://tc39.es/ecma262/#sec-generatorresume
pub fn generatorResume(agent: *Agent, generator_value: Value, value: Value) Agent.Error!*Object {
    // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
    const state = try generatorValidate(agent, generator_value);

    // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
    if (state == .completed) return createIteratorResultObject(agent, .undefined, true);

    // 3. Assert: state is either suspended-start or suspended-yield.
    std.debug.assert(state == .suspended_start or state == .suspended_yield);

    const generator = generator_value.asObject().as(Generator);

    // 4. Let genContext be generator.[[GeneratorContext]].
    const generator_context = generator.fields.generator_context;

    // 5. Let methodContext be the running execution context.
    const method_context = agent.runningExecutionContext();

    // 6. Suspend methodContext.

    // 7. Set generator.[[GeneratorState]] to executing.
    generator.fields.generator_state = .executing;

    // 8. Push genContext onto the execution context stack; genContext is now the running execution
    //    context.
    try agent.execution_context_stack.append(agent.gc_allocator, generator_context);

    // 9. Resume the suspended evaluation of genContext using NormalCompletion(value) as the result
    //    of the operation that suspended it. Let result be the value returned by the resumed
    //    computation.
    const result = try generator.fields.evaluation_state.closure(
        agent,
        generator.fields.evaluation_state.generator_function,
        Completion.normal(value),
    );

    // 10. Assert: When we return here, genContext has already been removed from the execution
    //     context stack and methodContext is the currently running execution context.
    std.debug.assert(method_context == agent.runningExecutionContext());

    // 11. Return ? result.
    return result;
}

/// 27.5.3.4 GeneratorResumeAbrupt ( generator, abruptCompletion, generatorBrand )
/// https://tc39.es/ecma262/#sec-generatorresumeabrupt
pub fn generatorResumeAbrupt(
    agent: *Agent,
    generator_value: Value,
    abrupt_completion: Completion,
) Agent.Error!*Object {
    // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
    var state = try generatorValidate(agent, generator_value);

    const generator = generator_value.asObject().as(Generator);

    // 2. If state is suspended-start, then
    if (state == .suspended_start) {
        // a. Set generator.[[GeneratorState]] to completed.
        generator.fields.generator_state = .completed;
        generator.fields.evaluation_state.vm.deinit();

        // b. NOTE: Once a generator enters the completed state it never leaves it and its associated
        //    execution context is never resumed. Any execution state associated with generator can be
        //    discarded at this point.
        generator.fields.evaluation_state = undefined;

        // c. Set state to completed.
        state = .completed;
    }

    // 3. If state is completed, then
    if (state == .completed) {
        // a. If abruptCompletion is a return completion, then
        if (abrupt_completion.type == .@"return") {
            // i. Return CreateIteratorResultObject(abruptCompletion.[[Value]], true).
            return createIteratorResultObject(agent, abrupt_completion.value.?, true);
        }

        // b. Return ? abruptCompletion.
        std.debug.assert(abrupt_completion.type == .throw);
        agent.exception = .{
            .value = abrupt_completion.value.?,
            .stack_trace = try agent.captureStackTrace(),
        };
        return error.ExceptionThrown;
    }

    // 4. Assert: state is suspended-yield.
    std.debug.assert(state == .suspended_yield);

    // 5. Let genContext be generator.[[GeneratorContext]].
    const generator_context = generator.fields.generator_context;

    // 6. Let methodContext be the running execution context.
    const method_context = agent.runningExecutionContext();

    // 7. Suspend methodContext.

    // 8. Set generator.[[GeneratorState]] to executing.
    generator.fields.generator_state = .executing;

    // 9. Push genContext onto the execution context stack; genContext is now the running execution
    //    context.
    try agent.execution_context_stack.append(agent.gc_allocator, generator_context);

    // 10. Resume the suspended evaluation of genContext using abruptCompletion as the result of
    //     the operation that suspended it. Let result be the Completion Record returned by the
    //     resumed computation.
    const result = try generator.fields.evaluation_state.closure(
        agent,
        generator.fields.evaluation_state.generator_function,
        abrupt_completion,
    );

    // 11. Assert: When we return here, genContext has already been removed from the execution
    //     context stack and methodContext is the currently running execution context.
    std.debug.assert(method_context == agent.runningExecutionContext());

    // 12. Return ? result.
    return result;
}

pub const GeneratorKind = enum {
    non_generator,
    sync,
    async,
};

/// 27.5.3.5 GetGeneratorKind ( )
/// https://tc39.es/ecma262/#sec-getgeneratorkind
pub fn getGeneratorKind(agent: *Agent) GeneratorKind {
    // 1. Let genContext be the running execution context.
    const generator_context = agent.runningExecutionContext();

    // 2. If genContext does not have a Generator component, return non-generator.
    // 3. Let generator be the Generator component of genContext.
    return switch (generator_context.generator) {
        .unset => .non_generator,

        // 4. If generator has an [[AsyncGeneratorState]] internal slot, return async.
        .async_generator => .async,

        // 5. Else, return sync.
        .generator => .sync,
    };
}

/// 27.5.3.6 GeneratorYield ( iteratorResult )
/// https://tc39.es/ecma262/#sec-generatoryield
pub fn generatorYield(agent: *Agent, iterator_result: *Object) Agent.Error!Completion {
    // 1. Let genContext be the running execution context.
    const generator_context = agent.runningExecutionContext();

    // 2. Assert: genContext is the execution context of a generator.
    std.debug.assert(generator_context.generator != .unset);

    // 3. Let generator be the value of the Generator component of genContext.
    // 4. Assert: GetGeneratorKind() is sync.
    const generator = generator_context.generator.generator;

    // 5. Set generator.[[GeneratorState]] to suspended-yield.
    generator.fields.generator_state = .suspended_yield;

    // 6. Remove genContext from the execution context stack and restore the execution context that
    //    is at the top of the execution context stack as the running execution context.
    _ = agent.execution_context_stack.pop().?;

    // 7. Let callerContext be the running execution context.
    // 8. Resume callerContext passing NormalCompletion(iteratorResult). If genContext is ever
    //    resumed again, let resumptionValue be the Completion Record with which it is resumed.
    generator.fields.evaluation_state.suspension_result = Value.from(iterator_result);

    // TODO: 9. Assert: If control reaches here, then genContext is the running execution context again.
    // TODO: 10. Return resumptionValue.
    return Completion.normal(null);
}

/// 27.5.3.7 Yield ( value )
/// https://tc39.es/ecma262/#sec-yield
pub fn yield(agent: *Agent, value: Value) Agent.Error!Completion {
    // 1. Let generatorKind be GetGeneratorKind().
    const generator_kind = getGeneratorKind(agent);

    switch (generator_kind) {
        // 2. If generatorKind is async, return ? AsyncGeneratorYield(? Await(value)).
        .async => return asyncGeneratorYield(agent, try await(agent, value)),

        // 3. Otherwise, return ? GeneratorYield(CreateIteratorResultObject(value, false)).
        .sync => return generatorYield(agent, try createIteratorResultObject(agent, value, false)),

        .non_generator => unreachable,
    }
}
