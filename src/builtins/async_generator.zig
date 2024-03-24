//! 27.6 AsyncGenerator Objects
//! https://tc39.es/ecma262/#sec-asyncgenerator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Completion = types.Completion;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const performPromiseThen = builtins.performPromiseThen;
const promiseResolve = builtins.promiseResolve;

/// 27.6.1 The %AsyncGeneratorPrototype% Object
/// https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-prototype
pub const AsyncGeneratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%AsyncIteratorPrototype%"(),
        });

        // 27.6.1.1 %AsyncGeneratorPrototype%.constructor
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-constructor
        try defineBuiltinProperty(object, "constructor", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 27.6.1.5 %AsyncGeneratorPrototype% [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-asyncgenerator-prototype-tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("AsyncGenerator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
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
                break :blk Completion.normal(completion.value orelse .undefined);
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
            .undefined,
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
            .undefined,
            &.{Value.from(iterator_result)},
        ) catch |err| try noexcept(err);
    }

    // 8. Return unused.
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
                completion = Completion.normal(.undefined);
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
    // 8. Assert: promiseCompletion.[[Type]] is normal.
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
    const fulfilled_closure_captures = try agent.gc_allocator.create(Captures);
    fulfilled_closure_captures.* = .{
        .generator = generator,
    };

    // 10. Let fulfilledClosure be a new Abstract Closure with parameters (value) that captures
    //    generator and performs the following steps when called:
    const fulfilled_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: ArgumentsList) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const generator_ = captures.generator;
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
            return .undefined;
        }
    }.func;

    // 11. Let onFulfilled be CreateBuiltinFunction(fulfilledClosure, 1, "", « »).
    const on_fulfilled = Value.from(
        try createBuiltinFunction(agent, .{ .regular = fulfilled_closure }, .{
            .length = 1,
            .name = "",
        }),
    );

    // 12. Let rejectedClosure be a new Abstract Closure with parameters (reason) that captures
    //    generator and performs the following steps when called:
    const rejected_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: ArgumentsList) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const generator_ = captures.generator;
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
            return .undefined;
        }
    }.func;

    // 13. Let onRejected be CreateBuiltinFunction(rejectedClosure, 1, "", « »).
    const on_rejected = Value.from(
        try createBuiltinFunction(agent, .{ .regular = rejected_closure }, .{
            .length = 1,
            .name = "",
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
