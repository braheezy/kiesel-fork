//! 27.6 AsyncGenerator Objects
//! https://tc39.es/ecma262/#sec-asyncgenerator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Completion = types.Completion;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const performPromiseThen = builtins.performPromiseThen;
const promiseResolve = builtins.promiseResolve;

/// 27.6.1 The %AsyncGeneratorPrototype% Object
/// https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-prototype
pub const AsyncGeneratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%AsyncIteratorPrototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 1, realm);
        try defineBuiltinFunction(object, "return", @"return", 1, realm);
        try defineBuiltinFunction(object, "throw", throw, 1, realm);

        // 27.6.1.1 %AsyncGeneratorPrototype%.constructor
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-constructor
        try defineBuiltinProperty(object, "constructor", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 27.6.1.5 %AsyncGeneratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-tostringtag
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("AsyncGenerator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
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
            // a. Let iteratorResult be CreateIterResultObject(undefined, true).
            const iterator_result = try createIterResultObject(agent, Value.undefined, true);

            // b. Perform ! Call(promiseCapability.[[Resolve]], undefined, « iteratorResult »).
            _ = Value.from(promise_capability.resolve).callAssumeCallable(
                Value.undefined,
                &.{Value.from(iterator_result)},
            ) catch |err| try noexcept(err);

            // c. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 7. Let completion be NormalCompletion(value).
        const completion = Completion.normal(value);

        // 8. Perform AsyncGeneratorEnqueue(generator, completion, promiseCapability).
        try asyncGeneratorEnqueue(generator, completion, promise_capability);

        // 9. If state is either suspended-start or suspended-yield, then
        if (state == .suspended_start or state == .suspended_yield) {
            // a. Perform AsyncGeneratorResume(generator, completion).
            try asyncGeneratorResume(agent, generator, completion);
        }
        // 10. Else,
        else {
            // a. Assert: state is either executing or awaiting-return.
            std.debug.assert(state == .executing or state == .awaiting_return);
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

        // 5. Let completion be Completion Record {
        //      [[Type]]: return, [[Value]]: value, [[Target]]: empty
        //    }.
        const completion: Completion = .{ .type = .@"return", .value = value, .target = null };

        // 6. Perform AsyncGeneratorEnqueue(generator, completion, promiseCapability).
        try asyncGeneratorEnqueue(generator, completion, promise_capability);

        // 7. Let state be generator.[[AsyncGeneratorState]].
        const state = generator.fields.async_generator_state;

        // 8. If state is either suspended-start or completed, then
        if (state == .suspended_start or state == .completed) {
            // a. Set generator.[[AsyncGeneratorState]] to awaiting-return.
            generator.fields.async_generator_state = .awaiting_return;

            // b. Perform AsyncGeneratorAwaitReturn(generator).
            try asyncGeneratorAwaitReturn(agent, generator);
        }
        // 9. Else if state is suspended-yield, then
        else if (state == .suspended_yield) {
            // a. Perform AsyncGeneratorResume(generator, completion).
            try asyncGeneratorResume(agent, generator, completion);
        }
        // 10. Else,
        else {
            // a. Assert: state is either executing or awaiting-return.
            std.debug.assert(state == .executing or state == .awaiting_return);
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
                Value.undefined,
                &.{exception},
            ) catch |err| try noexcept(err);

            // b. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 8. Let completion be ThrowCompletion(exception).
        const completion = Completion.throw(exception);

        // 9. Perform AsyncGeneratorEnqueue(generator, completion, promiseCapability).
        try asyncGeneratorEnqueue(generator, completion, promise_capability);

        // 10. If state is suspended-yield, then
        if (state == .suspended_yield) {
            // a. Perform AsyncGeneratorResume(generator, completion).
            try asyncGeneratorResume(agent, generator, completion);
        }
        // 11. Else,
        else {
            // a. Assert: state is either executing or awaiting-return.
            std.debug.assert(state == .executing or state == .awaiting_return);
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
            awaiting_return,
            completed,
        };

        /// [[AsyncGeneratorState]]
        async_generator_state: ?State,

        /// [[AsyncGeneratorContext]]
        async_generator_context: ExecutionContext,

        /// [[AsyncGeneratorQueue]]
        async_generator_queue: std.ArrayList(AsyncGeneratorRequest),

        // Non-standard
        evaluation_state: struct {
            closure: *const fn (*Agent, *builtins.ECMAScriptFunction) Allocator.Error!void,
            generator_function: *builtins.ECMAScriptFunction,
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
) void {
    // 1. Assert: generator.[[AsyncGeneratorState]] is undefined.
    std.debug.assert(generator.fields.async_generator_state == null);

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
        ) Allocator.Error!void {
            // a. Let acGenContext be the running execution context.
            const closure_generator_context = agent_.runningExecutionContext();

            // b. Let acGenerator be the Generator component of acGenContext.
            const closure_generator = closure_generator_context.generator.?.async_generator;

            // c. If generatorBody is a Parse Node, then
            const result = if (true) blk: {
                // i. Let result be Completion(Evaluation of generatorBody).
                break :blk generator_function_.fields.generateAndRunBytecode(agent_);
            }
            // d. Else,
            else {
                // i. Assert: generatorBody is an Abstract Closure with no parameters.
                // ii. Let result be Completion(generatorBody()).
            };

            // e. Assert: If we return here, the async generator either threw an exception or
            //    performed either an implicit or explicit return.

            // f. Remove acGenContext from the execution context stack and restore the execution
            //    context that is at the top of the execution context stack as the running
            //    execution context.
            _ = agent_.execution_context_stack.pop();

            // g. Set acGenerator.[[AsyncGeneratorState]] to completed.
            closure_generator.fields.async_generator_state = .completed;

            const result_completion = if (result) |completion| blk: {
                // h. If result is a normal completion, set result to NormalCompletion(undefined).
                // i. If result is a return completion, set result to NormalCompletion(result.[[Value]]).
                std.debug.assert(completion.type == .normal or completion.type == .@"return");
                break :blk Completion.normal(completion.value orelse Value.undefined);
            } else |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ExceptionThrown => blk: {
                    const exception = agent_.clearException();
                    break :blk Completion.throw(exception);
                },
            };

            // j. Perform AsyncGeneratorCompleteStep(acGenerator, result, true).
            try asyncGeneratorCompleteStep(agent_, closure_generator, result_completion, true, null);

            // k. Perform AsyncGeneratorDrainQueue(acGenerator).
            try asyncGeneratorDrainQueue(agent_, closure_generator);

            // l. Return undefined.
        }
    }.func;

    // 5. Set the code evaluation state of genContext such that when evaluation is resumed for that
    //    execution context, closure will be called with no arguments.
    generator.fields.evaluation_state = .{ .closure = closure, .generator_function = generator_function };

    // 6. Set generator.[[AsyncGeneratorContext]] to genContext.
    generator.fields.async_generator_context = generator_context.*;

    // 7. Set generator.[[AsyncGeneratorState]] to suspended-start.
    generator.fields.async_generator_state = .suspended_start;

    // 8. Set generator.[[AsyncGeneratorQueue]] to a new empty List.
    generator.fields.async_generator_queue = std.ArrayList(AsyncGeneratorRequest).init(agent.gc_allocator);

    // 9. Return unused.
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
    generator: *AsyncGenerator,
    completion: Completion,
    promise_capability: PromiseCapability,
) Allocator.Error!void {
    // 1. Let request be AsyncGeneratorRequest {
    //      [[Completion]]: completion, [[Capability]]: promiseCapability
    //    }.
    const request: AsyncGeneratorRequest = .{ .completion = completion, .capability = promise_capability };

    // 2. Append request to generator.[[AsyncGeneratorQueue]].
    try generator.fields.async_generator_queue.append(request);

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
) Allocator.Error!void {
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
            Value.undefined,
            &.{value},
        ) catch |err| try noexcept(err);
    }
    // 7. Else,
    else {
        // a. Assert: completion is a normal completion.
        std.debug.assert(completion.type == .normal);

        // b. If realm is present, then
        const iterator_result = if (realm) |new_realm| blk: {
            // i. Let oldRealm be the running execution context's Realm.
            const old_realm = agent.runningExecutionContext().realm;

            // ii. Set the running execution context's Realm to realm.
            agent.runningExecutionContext().realm = new_realm;
            defer agent.runningExecutionContext().realm = old_realm;

            // iii. Let iteratorResult be CreateIterResultObject(value, done).
            break :blk try createIterResultObject(agent, value, done);

            // iv. Set the running execution context's Realm to oldRealm.
        }
        // c. Else,
        else blk: {
            // i. Let iteratorResult be CreateIterResultObject(value, done).
            break :blk try createIterResultObject(agent, value, done);
        };

        // d. Perform ! Call(promiseCapability.[[Resolve]], undefined, « iteratorResult »).
        _ = Value.from(promise_capability.resolve).callAssumeCallable(
            Value.undefined,
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
) Allocator.Error!void {
    // 1. Assert: generator.[[AsyncGeneratorState]] is either suspended-start or suspended-yield.
    std.debug.assert(generator.fields.async_generator_state == .suspended_start or
        generator.fields.async_generator_state == .suspended_yield);

    // 2. Let genContext be generator.[[AsyncGeneratorContext]].
    const generator_context = generator.fields.async_generator_context;

    // 3. Let callerContext be the running execution context.
    const caller_context = agent.runningExecutionContext();

    // TODO: 4. Suspend callerContext.

    // 5. Set generator.[[AsyncGeneratorState]] to executing.
    generator.fields.async_generator_state = .executing;

    // 6. Push genContext onto the execution context stack; genContext is now the running execution
    //    context.
    try agent.execution_context_stack.append(generator_context);

    // 7. Resume the suspended evaluation of genContext using completion as the result of the
    //    operation that suspended it. Let result be the Completion Record returned by the resumed
    //    computation.
    // 8. Assert: result is never an abrupt completion.
    _ = completion;
    try generator.fields.evaluation_state.closure(
        agent,
        generator.fields.evaluation_state.generator_function,
    );

    // 9. Assert: When we return here, genContext has already been removed from the execution
    //    context stack and callerContext is the currently running execution context.
    // TODO: This may not be valid, the pointer can change if the execution context stack is resized.
    //       For this to work we need to either heap-allocate execution contexts so they have a
    //       stable pointer or otherwise give them a unique ID.
    // std.debug.assert(caller_context == agent.runningExecutionContext());
    _ = caller_context;

    // 10. Return unused.
}

/// 27.6.3.10 AsyncGeneratorDrainQueue ( generator )
/// https://tc39.es/ecma262/#sec-asyncgeneratordrainqueue
pub fn asyncGeneratorDrainQueue(
    agent: *Agent,
    generator: *AsyncGenerator,
) Allocator.Error!void {
    // 1. Assert: generator.[[AsyncGeneratorState]] is completed.
    std.debug.assert(generator.fields.async_generator_state == .completed);

    // 2. Let queue be generator.[[AsyncGeneratorQueue]].
    const queue = &generator.fields.async_generator_queue;

    // 3. If queue is empty, return unused.
    if (queue.items.len == 0) return;

    // 4. Let done be false.
    // 5. Repeat, while done is false,
    while (true) {
        // a. Let next be the first element of queue.
        const next = queue.items[0];

        // b. Let completion be Completion(next.[[Completion]]).
        var completion = next.completion;

        // c. If completion is a return completion, then
        if (completion.type == .@"return") {
            // i. Set generator.[[AsyncGeneratorState]] to awaiting-return.
            generator.fields.async_generator_state = .awaiting_return;

            // ii. Perform AsyncGeneratorAwaitReturn(generator).
            try asyncGeneratorAwaitReturn(agent, generator);

            // iii. Set done to true.
            break;
        }
        // d. Else,
        else {
            // i. If completion is a normal completion, then
            if (completion.type == .normal) {
                // 1. Set completion to NormalCompletion(undefined).
                completion = Completion.normal(Value.undefined);
            }

            // ii. Perform AsyncGeneratorCompleteStep(generator, completion, true).
            try asyncGeneratorCompleteStep(agent, generator, completion, true, null);

            // iii. If queue is empty, set done to true.
            if (queue.items.len == 0) break;
        }
    }

    // 6. Return unused.
}

/// 27.6.3.9 AsyncGeneratorAwaitReturn ( generator )
/// https://tc39.es/ecma262/#sec-asyncgeneratorawaitreturn
///
/// NOTE: This includes the changes from https://github.com/tc39/ecma262/pull/2683 to avoid
///       crashing on broken promises.
pub fn asyncGeneratorAwaitReturn(agent: *Agent, generator: *AsyncGenerator) Allocator.Error!void {
    const realm = agent.currentRealm();

    // 1. Let queue be generator.[[AsyncGeneratorQueue]].
    const queue = &generator.fields.async_generator_queue;

    // 2. Assert: queue is not empty.
    std.debug.assert(queue.items.len != 0);

    // 3. Let next be the first element of queue.
    const next = queue.items[0];

    // 4. Let completion be Completion(next.[[Completion]]).
    const completion = next.completion;

    // 5. Assert: completion is a return completion.
    std.debug.assert(completion.type == .@"return");

    // 6. Let promiseCompletion be Completion(PromiseResolve(%Promise%, completion.[[Value]])).
    // 8. Assert: promiseCompletion is a normal completion.
    // 9. Let promise be promiseCompletion.[[Value]].
    const promise = promiseResolve(
        agent,
        try realm.intrinsics.@"%Promise%"(),
        completion.value.?,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,

        // 7. If promiseCompletion is an abrupt completion, then
        error.ExceptionThrown => {
            const promise_completion = Completion.throw(agent.clearException());

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

    // 10. Let fulfilledClosure be a new Abstract Closure with parameters (value) that captures
    //    generator and performs the following steps when called:
    const fulfilled_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const generator_ = captures_.generator;
            const value = arguments_.get(0);

            // a. Set generator.[[AsyncGeneratorState]] to completed.
            generator_.fields.async_generator_state = .completed;

            // b. Let result be NormalCompletion(value).
            const result = Completion.normal(value);

            // c. Perform AsyncGeneratorCompleteStep(generator, result, true).
            try asyncGeneratorCompleteStep(agent_, generator_, result, true, null);

            // d. Perform AsyncGeneratorDrainQueue(generator).
            try asyncGeneratorDrainQueue(agent_, generator_);

            // e. Return undefined.
            return Value.undefined;
        }
    }.func;

    // 11. Let onFulfilled be CreateBuiltinFunction(fulfilledClosure, 1, "", « »).
    const on_fulfilled = Value.from(
        try createBuiltinFunction(agent, .{ .function = fulfilled_closure }, .{
            .length = 1,
            .name = "",
            .additional_fields = SafePointer.make(*Captures, captures),
        }),
    );

    // 12. Let rejectedClosure be a new Abstract Closure with parameters (reason) that captures
    //    generator and performs the following steps when called:
    const rejected_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const generator_ = captures_.generator;
            const reason = arguments_.get(0);

            // a. Set generator.[[AsyncGeneratorState]] to completed.
            generator_.fields.async_generator_state = .completed;

            // b. Let result be ThrowCompletion(reason).
            const result = Completion.throw(reason);

            // c. Perform AsyncGeneratorCompleteStep(generator, result, true).
            try asyncGeneratorCompleteStep(agent_, generator_, result, true, null);

            // d. Perform AsyncGeneratorDrainQueue(generator).
            try asyncGeneratorDrainQueue(agent_, generator_);

            // e. Return undefined.
            return Value.undefined;
        }
    }.func;

    // 13. Let onRejected be CreateBuiltinFunction(rejectedClosure, 1, "", « »).
    const on_rejected = Value.from(
        try createBuiltinFunction(agent, .{ .function = rejected_closure }, .{
            .length = 1,
            .name = "",
            .additional_fields = SafePointer.make(*Captures, captures),
        }),
    );

    // 14. Perform PerformPromiseThen(promise, onFulfilled, onRejected).
    _ = try performPromiseThen(
        agent,
        promise.as(builtins.Promise),
        on_fulfilled,
        on_rejected,
        null,
    );

    // 15. Return unused.
}
