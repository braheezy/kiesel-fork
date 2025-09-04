//! 27.6 AsyncGenerator Objects
//! https://tc39.es/ecma262/#sec-asyncgenerator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const bytecode = @import("../language/bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Completion = types.Completion;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PromiseCapability = builtins.promise.PromiseCapability;
const Realm = execution.Realm;
const Value = types.Value;
const Vm = bytecode.Vm;
const await = builtins.await;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createIteratorResultObject = types.createIteratorResultObject;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const performPromiseThen = builtins.performPromiseThen;
const promiseResolve = builtins.promiseResolve;

/// 27.6.1 The %AsyncGeneratorPrototype% Object
/// https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-prototype
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%AsyncIteratorPrototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "next", next, 1, realm);
        try object.defineBuiltinFunction(agent, "return", @"return", 1, realm);
        try object.defineBuiltinFunction(agent, "throw", throw, 1, realm);

        // 27.6.1.1 %AsyncGeneratorPrototype%.constructor
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-constructor
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 27.6.1.5 %AsyncGeneratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-tostringtag
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("AsyncGenerator"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 27.6.1.2 %AsyncGeneratorPrototype%.next ( value )
    /// https://tc39.es/ecma262/#sec-asyncgenerator-prototype-next
    fn next(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const value = arguments.get(0);

        // 1. Let generator be the this value.
        const generator_value = this_value;

        // 2. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // 3. Let result be Completion(AsyncGeneratorValidate(generator, empty)).
        asyncGeneratorValidate(agent, generator_value) catch |err| {
            // 4. IfAbruptRejectPromise(result, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        const generator = generator_value.asObject().as(AsyncGenerator);

        // 5. Let state be generator.[[AsyncGeneratorState]].
        const state = generator.fields.async_generator_state;

        // 6. If state is completed, then
        if (state == .completed) {
            // a. Let iteratorResult be CreateIteratorResultObject(undefined, true).
            const iterator_result = try createIteratorResultObject(agent, .undefined, true);

            // b. Perform ! Call(promiseCapability.[[Resolve]], undefined, « iteratorResult »).
            _ = Value.from(promise_capability.resolve).callAssumeCallable(
                agent,
                .undefined,
                &.{Value.from(iterator_result)},
            ) catch |err| try noexcept(err);

            // c. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 7. Let completion be NormalCompletion(value).
        const completion = Completion.normal(value);

        // 8. Perform AsyncGeneratorEnqueue(generator, completion, promiseCapability).
        try asyncGeneratorEnqueue(agent, generator, completion, promise_capability);

        // 9. If state is either suspended-start or suspended-yield, then
        if (state == .suspended_start or state == .suspended_yield) {
            // a. Perform AsyncGeneratorResume(generator, completion).
            try asyncGeneratorResume(agent, generator, completion);
        } else {
            // 10. Else,
            // a. Assert: state is either executing or draining-queue.
            std.debug.assert(state == .executing or state == .draining_queue);
        }

        // 11. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    /// 27.6.1.3 %AsyncGeneratorPrototype%.return ( value )
    /// https://tc39.es/ecma262/#sec-asyncgenerator-prototype-return
    fn @"return"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const value = arguments.get(0);

        // 1. Let generator be the this value.
        const generator_value = this_value;

        // 2. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // 3. Let result be Completion(AsyncGeneratorValidate(generator, empty)).
        asyncGeneratorValidate(agent, generator_value) catch |err| {
            // 4. IfAbruptRejectPromise(result, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        const generator = generator_value.asObject().as(AsyncGenerator);

        // 5. Let completion be ReturnCompletion(value).
        const completion = Completion.@"return"(value);

        // 6. Perform AsyncGeneratorEnqueue(generator, completion, promiseCapability).
        try asyncGeneratorEnqueue(agent, generator, completion, promise_capability);

        // 7. Let state be generator.[[AsyncGeneratorState]].
        const state = generator.fields.async_generator_state;

        // 8. If state is either suspended-start or completed, then
        if (state == .suspended_start or state == .completed) {
            // a. Set generator.[[AsyncGeneratorState]] to draining-queue.
            generator.fields.async_generator_state = .draining_queue;

            // b. Perform AsyncGeneratorAwaitReturn(generator).
            try asyncGeneratorAwaitReturn(agent, generator);
        }
        // 9. Else if state is suspended-yield, then
        else if (state == .suspended_yield) {
            // a. Perform AsyncGeneratorResume(generator, completion).
            try asyncGeneratorResume(agent, generator, completion);
        } else {
            // 10. Else,
            // a. Assert: state is either executing or draining-queue.
            std.debug.assert(state == .executing or state == .draining_queue);
        }

        // 11. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    /// 27.6.1.4 %AsyncGeneratorPrototype%.throw ( exception )
    /// https://tc39.es/ecma262/#sec-asyncgenerator-prototype-throw
    fn throw(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const exception = arguments.get(0);

        // 1. Let generator be the this value.
        const generator_value = this_value;

        // 2. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // 3. Let result be Completion(AsyncGeneratorValidate(generator, empty)).
        asyncGeneratorValidate(agent, generator_value) catch |err| {
            // 4. IfAbruptRejectPromise(result, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        const generator = generator_value.asObject().as(AsyncGenerator);

        // 5. Let state be generator.[[AsyncGeneratorState]].
        var state = generator.fields.async_generator_state;

        // 6. If state is suspended-start, then
        if (state == .suspended_start) {
            // a. Set generator.[[AsyncGeneratorState]] to completed.
            generator.fields.async_generator_state = .completed;

            // b. Set state to completed.
            state = .completed;
        }

        // 7. If state is completed, then
        if (state == .completed) {
            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « exception »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                agent,
                .undefined,
                &.{exception},
            ) catch |err| try noexcept(err);

            // b. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 8. Let completion be ThrowCompletion(exception).
        const completion = Completion.throw(exception);

        // 9. Perform AsyncGeneratorEnqueue(generator, completion, promiseCapability).
        try asyncGeneratorEnqueue(agent, generator, completion, promise_capability);

        // 10. If state is suspended-yield, then
        if (state == .suspended_yield) {
            // a. Perform AsyncGeneratorResume(generator, completion).
            try asyncGeneratorResume(agent, generator, completion);
        } else {
            // 11. Else,
            // a. Assert: state is either executing or draining-queue.
            std.debug.assert(state == .executing or state == .draining_queue);
        }

        // 12. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }
};

/// 27.6.2 Properties of AsyncGenerator Instances
/// https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-intances
pub const AsyncGenerator = MakeObject(.{
    .Fields = struct {
        pub const State = enum {
            suspended_start,
            suspended_yield,
            executing,
            draining_queue,
            completed,
        };

        /// [[AsyncGeneratorState]]
        async_generator_state: State,

        /// [[AsyncGeneratorContext]]
        async_generator_context: *ExecutionContext,

        /// [[AsyncGeneratorQueue]]
        async_generator_queue: std.ArrayList(AsyncGeneratorRequest),

        // Non-standard
        evaluation_state: struct {
            vm: Vm,
            closure: *const fn (*Agent, *builtins.ECMAScriptFunction, Completion) std.mem.Allocator.Error!void,
            generator_function: *builtins.ECMAScriptFunction,
            suspension_result: ?Value = null,
        },
    },
    .tag = .async_generator,
});

/// 27.6.3.1 AsyncGeneratorRequest Records
/// https://tc39.es/ecma262/#sec-asyncgeneratorrequest-records
pub const AsyncGeneratorRequest = struct {
    /// [[Completion]]
    completion: Completion,

    /// [[Capability]]
    capability: PromiseCapability,
};

/// 27.6.3.2 AsyncGeneratorStart ( generator, generatorBody )
/// https://tc39.es/ecma262/#sec-asyncgeneratorstart
pub fn asyncGeneratorStart(
    agent: *Agent,
    generator: *AsyncGenerator,
    generator_function: *builtins.ECMAScriptFunction,
) std.mem.Allocator.Error!void {
    // 1. Assert: generator.[[AsyncGeneratorState]] is suspended-start.
    std.debug.assert(generator.fields.async_generator_state == .suspended_start);

    // 2. Let genContext be the running execution context.
    const generator_context = agent.runningExecutionContext();

    // 3. Set the Generator component of genContext to generator.
    generator_context.generator = .{ .async_generator = generator };

    // 4. Let closure be a new Abstract Closure with no parameters that captures generatorBody and
    //    performs the following steps when called:
    const closure = struct {
        fn func(
            agent_: *Agent,
            generator_function_: *builtins.ECMAScriptFunction,
            resume_completion: Completion,
        ) std.mem.Allocator.Error!void {
            // a. Let acGenContext be the running execution context.
            const closure_generator_context = agent_.runningExecutionContext();

            // b. Let acGenerator be the Generator component of acGenContext.
            const closure_generator = closure_generator_context.generator.async_generator;

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
                // d. Else,
                // i. Assert: generatorBody is an Abstract Closure with no parameters.
                // ii. Let result be Completion(generatorBody()).
            };

            // e. Assert: If we return here, the async generator either threw an exception or
            //    performed either an implicit or explicit return.

            if (closure_generator.fields.evaluation_state.suspension_result) |_| {
                closure_generator.fields.evaluation_state.suspension_result = null;
                return;
            }

            // f. Remove acGenContext from the execution context stack and restore the execution
            //    context that is at the top of the execution context stack as the running
            //    execution context.
            _ = agent_.execution_context_stack.pop().?;

            // g. Set acGenerator.[[AsyncGeneratorState]] to draining-queue.
            closure_generator.fields.async_generator_state = .draining_queue;

            const result_completion: Completion = if (result) |completion| switch (completion.type) {
                // h. If result is a normal completion, set result to NormalCompletion(undefined).
                .normal => Completion.normal(.undefined),

                // i. If result is a return completion, set result to NormalCompletion(result.[[Value]]).
                .@"return" => Completion.normal(completion.value.?),

                else => unreachable,
            } else |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ExceptionThrown => blk: {
                    const exception = agent_.clearException();
                    break :blk Completion.throw(exception.value);
                },
            };

            // j. Perform AsyncGeneratorCompleteStep(acGenerator, result, true).
            try asyncGeneratorCompleteStep(agent_, closure_generator, result_completion, true, null);

            // k. Perform AsyncGeneratorDrainQueue(acGenerator).
            try asyncGeneratorDrainQueue(agent_, closure_generator);

            // l. Return NormalCompletion(undefined).
        }
    }.func;

    // 5. Set the code evaluation state of genContext such that when evaluation is resumed for that
    //    execution context, closure will be called with no arguments.
    generator.fields.evaluation_state = .{
        .vm = try Vm.init(agent, undefined),
        .closure = closure,
        .generator_function = generator_function,
    };

    // 6. Set generator.[[AsyncGeneratorContext]] to genContext.
    generator.fields.async_generator_context = generator_context;

    // 7. Set generator.[[AsyncGeneratorQueue]] to a new empty List.
    generator.fields.async_generator_queue = .empty;

    // 8. Return unused.
}

/// 27.6.3.3 AsyncGeneratorValidate ( generator, generatorBrand )
/// https://tc39.es/ecma262/#sec-asyncgeneratorvalidate
pub fn asyncGeneratorValidate(agent: *Agent, generator_value: Value) error{ExceptionThrown}!void {
    // 1. Perform ? RequireInternalSlot(generator, [[AsyncGeneratorContext]]).
    // 2. Perform ? RequireInternalSlot(generator, [[AsyncGeneratorState]]).
    // 3. Perform ? RequireInternalSlot(generator, [[AsyncGeneratorQueue]]).
    _ = try generator_value.requireInternalSlot(agent, AsyncGenerator);

    // 4. If generator.[[GeneratorBrand]] is not generatorBrand, throw a TypeError exception.
    // NOTE: All iterators using [[GeneratorBrand]] in the spec are implemented without generators
    //       so this is currently not needed.

    // 5. Return unused.
}

/// 27.6.3.4 AsyncGeneratorEnqueue ( generator, completion, promiseCapability )
/// https://tc39.es/ecma262/#sec-asyncgeneratorenqueue
pub fn asyncGeneratorEnqueue(
    agent: *Agent,
    generator: *AsyncGenerator,
    completion: Completion,
    promise_capability: PromiseCapability,
) std.mem.Allocator.Error!void {
    // 1. Let request be AsyncGeneratorRequest {
    //      [[Completion]]: completion, [[Capability]]: promiseCapability
    //    }.
    const request: AsyncGeneratorRequest = .{ .completion = completion, .capability = promise_capability };

    // 2. Append request to generator.[[AsyncGeneratorQueue]].
    try generator.fields.async_generator_queue.append(agent.gc_allocator, request);

    // 3. Return unused.
}

/// 27.6.3.5 AsyncGeneratorCompleteStep ( generator, completion, done [ , realm ] )
/// https://tc39.es/ecma262/#sec-asyncgeneratorcompletestep
pub fn asyncGeneratorCompleteStep(
    agent: *Agent,
    generator: *AsyncGenerator,
    completion: Completion,
    done: bool,
    realm: ?*Realm,
) std.mem.Allocator.Error!void {
    // 1. Assert: generator.[[AsyncGeneratorQueue]] is not empty.
    std.debug.assert(generator.fields.async_generator_queue.items.len != 0);

    // 2. Let next be the first element of generator.[[AsyncGeneratorQueue]].
    // 3. Remove the first element from generator.[[AsyncGeneratorQueue]].
    const next = generator.fields.async_generator_queue.orderedRemove(0);

    // 4. Let promiseCapability be next.[[Capability]].
    const promise_capability = next.capability;

    // 5. Let value be completion.[[Value]].
    const value = completion.value.?;

    // 6. If completion is a throw completion, then
    if (completion.type == .throw) {
        // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « value »).
        _ = Value.from(promise_capability.reject).callAssumeCallable(
            agent,
            .undefined,
            &.{value},
        ) catch |err| try noexcept(err);
    } else {
        // 7. Else,
        // a. Assert: completion is a normal completion.
        std.debug.assert(completion.type == .normal);

        // b. If realm is present, then
        const iterator_result = if (realm) |new_realm| blk: {
            // i. Let oldRealm be the running execution context's Realm.
            const old_realm = agent.runningExecutionContext().realm;

            // ii. Set the running execution context's Realm to realm.
            agent.runningExecutionContext().realm = new_realm;
            defer agent.runningExecutionContext().realm = old_realm;

            // iii. Let iteratorResult be CreateIteratorResultObject(value, done).
            break :blk try createIteratorResultObject(agent, value, done);

            // iv. Set the running execution context's Realm to oldRealm.
        } else blk: {
            // c. Else,
            // i. Let iteratorResult be CreateIteratorResultObject(value, done).
            break :blk try createIteratorResultObject(agent, value, done);
        };

        // d. Perform ! Call(promiseCapability.[[Resolve]], undefined, « iteratorResult »).
        _ = Value.from(promise_capability.resolve).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(iterator_result)},
        ) catch |err| try noexcept(err);
    }

    // 8. Return unused.
}

/// 27.6.3.6 AsyncGeneratorResume ( generator, completion )
/// https://tc39.es/ecma262/#sec-asyncgeneratorresume
pub fn asyncGeneratorResume(
    agent: *Agent,
    generator: *AsyncGenerator,
    completion: Completion,
) std.mem.Allocator.Error!void {
    // 1. Assert: generator.[[AsyncGeneratorState]] is either suspended-start or suspended-yield.
    std.debug.assert(generator.fields.async_generator_state == .suspended_start or
        generator.fields.async_generator_state == .suspended_yield);

    // 2. Let genContext be generator.[[AsyncGeneratorContext]].
    const generator_context = generator.fields.async_generator_context;

    // 3. Let callerContext be the running execution context.
    const caller_context = agent.runningExecutionContext();

    // 4. Suspend callerContext.

    // 5. Set generator.[[AsyncGeneratorState]] to executing.
    generator.fields.async_generator_state = .executing;

    // 6. Push genContext onto the execution context stack; genContext is now the running execution
    //    context.
    try agent.execution_context_stack.append(agent.gc_allocator, generator_context);

    // 7. Resume the suspended evaluation of genContext using completion as the result of the
    //    operation that suspended it. Let result be the Completion Record returned by the resumed
    //    computation.
    // 8. Assert: result is never an abrupt completion.
    try generator.fields.evaluation_state.closure(
        agent,
        generator.fields.evaluation_state.generator_function,
        completion,
    );

    // 9. Assert: When we return here, genContext has already been removed from the execution
    //    context stack and callerContext is the currently running execution context.
    std.debug.assert(caller_context == agent.runningExecutionContext());

    // 10. Return unused.
}

/// 27.6.3.7 AsyncGeneratorUnwrapYieldResumption ( resumptionValue )
/// https://tc39.es/ecma262/#sec-asyncgeneratorunwrapyieldresumption
pub fn asyncGeneratorUnwrapYieldResumption(agent: *Agent, resumption_value: Completion) Agent.Error!Completion {
    // 1. If resumptionValue is not a return completion, return ? resumptionValue.
    if (resumption_value.type != .@"return") return resumption_value;

    // 2. Let awaited be Completion(Await(resumptionValue.[[Value]])).
    const awaited = await(agent, resumption_value.value.?);

    // 3. If awaited is a throw completion, return ? awaited.
    const awaited_value = awaited catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ExceptionThrown => {
            const exception = agent.clearException();
            return Completion.throw(exception.value);
        },
    };

    // 4. Assert: awaited is a normal completion.
    // 5. Return ReturnCompletion(awaited.[[Value]]).
    return Completion.@"return"(awaited_value);
}

/// 27.6.3.8 AsyncGeneratorYield ( value )
/// https://tc39.es/ecma262/#sec-asyncgeneratoryield
pub fn asyncGeneratorYield(agent: *Agent, value: Value) Agent.Error!Completion {
    // 1. Let genContext be the running execution context.
    const generator_context = agent.runningExecutionContext();

    // 2. Assert: genContext is the execution context of a generator.
    // 3. Let generator be the value of the Generator component of genContext.
    // 4. Assert: GetGeneratorKind() is async.
    const generator = generator_context.generator.async_generator;

    // 5. Let completion be NormalCompletion(value).
    const completion = Completion.normal(value);

    // 6. Assert: The execution context stack has at least two elements.
    std.debug.assert(agent.execution_context_stack.items.len >= 2);

    // 7. Let previousContext be the second to top element of the execution context stack.
    const previous_context = agent.execution_context_stack.items[agent.execution_context_stack.items.len - 2];

    // 8. Let previousRealm be previousContext's Realm.
    const previous_realm = previous_context.realm;

    // 9. Perform AsyncGeneratorCompleteStep(generator, completion, false, previousRealm).
    try asyncGeneratorCompleteStep(agent, generator, completion, false, previous_realm);

    // 10. Let queue be generator.[[AsyncGeneratorQueue]].
    const queue = &generator.fields.async_generator_queue;

    // 11. If queue is not empty, then
    if (queue.items.len != 0) {
        // a. NOTE: Execution continues without suspending the generator.
        // b. Let toYield be the first element of queue.
        const to_yield = queue.items[0];

        // c. Let resumptionValue be Completion(toYield.[[Completion]]).
        const resumption_value = to_yield.completion;

        // d. Return ? AsyncGeneratorUnwrapYieldResumption(resumptionValue).
        return asyncGeneratorUnwrapYieldResumption(agent, resumption_value);
    } else {
        // 12. Else,
        // a. Set generator.[[AsyncGeneratorState]] to suspended-yield.
        generator.fields.async_generator_state = .suspended_yield;

        // b. Remove genContext from the execution context stack and restore the execution context
        //    that is at the top of the execution context stack as the running execution context.
        _ = agent.execution_context_stack.pop().?;

        // c. Let callerContext be the running execution context.
        // d. Resume callerContext passing undefined. If genContext is ever resumed again, let
        //    resumptionValue be the Completion Record with which it is resumed.
        generator.fields.evaluation_state.suspension_result = .undefined;

        // TODO: e. Assert: If control reaches here, then genContext is the running execution context again.
        // TODO: f. Return ? AsyncGeneratorUnwrapYieldResumption(resumptionValue).
        return Completion.normal(null);
    }
}

/// 27.6.3.10 AsyncGeneratorDrainQueue ( generator )
/// https://tc39.es/ecma262/#sec-asyncgeneratordrainqueue
pub fn asyncGeneratorDrainQueue(
    agent: *Agent,
    generator: *AsyncGenerator,
) std.mem.Allocator.Error!void {
    // 1. Assert: generator.[[AsyncGeneratorState]] is draining-queue.
    std.debug.assert(generator.fields.async_generator_state == .draining_queue);

    // 2. Let queue be generator.[[AsyncGeneratorQueue]].
    const queue = &generator.fields.async_generator_queue;

    // 3. Repeat, while queue is not empty,
    while (queue.items.len != 0) {
        // a. Let next be the first element of queue.
        const next = queue.items[0];

        // b. Let completion be Completion(next.[[Completion]]).
        var completion = next.completion;

        // c. If completion is a return completion, then
        if (completion.type == .@"return") {
            // i. Perform AsyncGeneratorAwaitReturn(generator).
            try asyncGeneratorAwaitReturn(agent, generator);

            // ii. Return unused.
            return;
        } else {
            // d. Else,
            // i. If completion is a normal completion, then
            if (completion.type == .normal) {
                // 1. Set completion to NormalCompletion(undefined).
                completion = Completion.normal(.undefined);
            }

            // ii. Perform AsyncGeneratorCompleteStep(generator, completion, true).
            try asyncGeneratorCompleteStep(agent, generator, completion, true, null);
        }
    }

    // 4. Set generator.[[AsyncGeneratorState]] to completed.
    generator.fields.async_generator_state = .completed;

    // 5. Return unused.
}

/// 27.6.3.9 AsyncGeneratorAwaitReturn ( generator )
/// https://tc39.es/ecma262/#sec-asyncgeneratorawaitreturn
pub fn asyncGeneratorAwaitReturn(
    agent: *Agent,
    generator: *AsyncGenerator,
) std.mem.Allocator.Error!void {
    const realm = agent.currentRealm();

    // 1. Assert: generator.[[AsyncGeneratorState]] is draining-queue.
    std.debug.assert(generator.fields.async_generator_state == .draining_queue);

    // 2. Let queue be generator.[[AsyncGeneratorQueue]].
    const queue = &generator.fields.async_generator_queue;

    // 3. Assert: queue is not empty.
    std.debug.assert(queue.items.len != 0);

    // 4. Let next be the first element of queue.
    const next = queue.items[0];

    // 5. Let completion be Completion(next.[[Completion]]).
    const completion = next.completion;

    // 6. Assert: completion is a return completion.
    std.debug.assert(completion.type == .@"return");

    // 7. Let promiseCompletion be Completion(PromiseResolve(%Promise%, completion.[[Value]])).
    // 9. Assert: promiseCompletion is a normal completion.
    // 10. Let promise be promiseCompletion.[[Value]].
    const promise = promiseResolve(
        agent,
        try realm.intrinsics.@"%Promise%"(),
        completion.value.?,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,

        // 8. If promiseCompletion is an abrupt completion, then
        error.ExceptionThrown => {
            const exception = agent.clearException();
            const promise_completion = Completion.throw(exception.value);

            // a. Set generator.[[AsyncGeneratorState]] to completed.
            generator.fields.async_generator_state = .completed;

            // b. Perform AsyncGeneratorCompleteStep(generator, promiseCompletion, true).
            try asyncGeneratorCompleteStep(agent, generator, promise_completion, true, null);

            // c. Perform AsyncGeneratorDrainQueue(generator).
            try asyncGeneratorDrainQueue(agent, generator);

            // d. Return unused.
            return;
        },
    };

    const Captures = struct {
        generator: *AsyncGenerator,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{
        .generator = generator,
    };

    // 11. Let fulfilledClosure be a new Abstract Closure with parameters (value) that captures
    //    generator and performs the following steps when called:
    const fulfilled_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const generator_ = captures_.generator;
            const value = arguments_.get(0);

            // a. Assert: generator.[[AsyncGeneratorState]] is draining-queue.
            std.debug.assert(generator_.fields.async_generator_state == .draining_queue);

            // b. Let result be NormalCompletion(value).
            const result = Completion.normal(value);

            // c. Perform AsyncGeneratorCompleteStep(generator, result, true).
            try asyncGeneratorCompleteStep(agent_, generator_, result, true, null);

            // d. Perform AsyncGeneratorDrainQueue(generator).
            try asyncGeneratorDrainQueue(agent_, generator_);

            // e. Return NormalCompletion(undefined).
            return .undefined;
        }
    }.func;

    // 12. Let onFulfilled be CreateBuiltinFunction(fulfilledClosure, 1, "", « »).
    const on_fulfilled = try createBuiltinFunction(
        agent,
        .{ .function = fulfilled_closure },
        1,
        "",
        .{ .additional_fields = .make(*Captures, captures) },
    );

    // 13. Let rejectedClosure be a new Abstract Closure with parameters (reason) that captures
    //    generator and performs the following steps when called:
    const rejected_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const generator_ = captures_.generator;
            const reason = arguments_.get(0);

            // a. Assert: generator.[[AsyncGeneratorState]] is draining-queue.
            std.debug.assert(generator_.fields.async_generator_state == .draining_queue);

            // b. Let result be ThrowCompletion(reason).
            const result = Completion.throw(reason);

            // c. Perform AsyncGeneratorCompleteStep(generator, result, true).
            try asyncGeneratorCompleteStep(agent_, generator_, result, true, null);

            // d. Perform AsyncGeneratorDrainQueue(generator).
            try asyncGeneratorDrainQueue(agent_, generator_);

            // e. Return NormalCompletion(undefined).
            return .undefined;
        }
    }.func;

    // 14. Let onRejected be CreateBuiltinFunction(rejectedClosure, 1, "", « »).
    const on_rejected = try createBuiltinFunction(
        agent,
        .{ .function = rejected_closure },
        1,
        "",
        .{ .additional_fields = .make(*Captures, captures) },
    );

    // 15. Perform PerformPromiseThen(promise, onFulfilled, onRejected).
    _ = try performPromiseThen(
        agent,
        promise.as(builtins.Promise),
        Value.from(&on_fulfilled.object),
        Value.from(&on_rejected.object),
        null,
    );

    // 16. Return unused.
}
