//! 27.2 Promise Objects
//! https://tc39.es/ecma262/#sec-promise-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Completion = types.Completion;
const Iterator = types.Iterator;
const Job = execution.Job;
const JobCallback = execution.JobCallback;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getIterator = types.getIterator;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

/// 27.2.1.1 PromiseCapability Records
/// https://tc39.es/ecma262/#sec-promisecapability-records
pub const PromiseCapability = struct {
    /// [[Promise]]
    promise: *Object,

    /// [[Resolve]]
    resolve: *Object,

    /// [[Reject]]
    reject: *Object,

    /// 27.2.1.1.1 IfAbruptRejectPromise ( value, capability )
    /// https://tc39.es/ecma262/#sec-ifabruptrejectpromise
    pub fn rejectPromise(self: @This(), agent: *Agent, err: Agent.Error) Agent.Error!*Object {
        // 1. Assert: value is a Completion Record.
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,

            // 2. If value is an abrupt completion, then
            error.ExceptionThrown => {
                const exception = agent.clearException();

                // a. Perform ? Call(capability.[[Reject]], undefined, « value.[[Value]] »).
                _ = try Value.from(self.reject).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{exception.value},
                );

                // b. Return capability.[[Promise]].
                return self.promise;
            },
        }

        // 3. Else,
        //     a. Set value to ! value.
        // NOTE: This has to be handled at the call site.
    }
};

/// 27.2.1.2 PromiseReaction Records
/// https://tc39.es/ecma262/#sec-promisereaction-records
const PromiseReaction = struct {
    /// [[Capability]]
    capability: ?PromiseCapability,

    /// [[Type]]
    type: enum { fulfill, reject },

    /// [[Handler]]
    handler: ?JobCallback,
};

const ResolvingFunctions = struct {
    resolve: *Object,
    reject: *Object,
};

/// 27.2.1.3 CreateResolvingFunctions ( promise )
/// https://tc39.es/ecma262/#sec-createresolvingfunctions
pub fn createResolvingFunctions(
    agent: *Agent,
    promise: *Promise,
) std.mem.Allocator.Error!ResolvingFunctions {
    const AlreadyResolved = struct { value: bool };
    const AdditionalFields = struct {
        promise: *Promise,
        already_resolved: AlreadyResolved,
    };

    const additional_fields = try agent.gc_allocator.create(AdditionalFields);

    // 1. Let alreadyResolved be the Record { [[Value]]: false }.
    const already_resolved: AlreadyResolved = .{ .value = false };

    // 2. Let stepsResolve be the algorithm steps defined in Promise Resolve Functions.
    const steps_resolve = struct {
        /// 27.2.1.3.2 Promise Resolve Functions
        /// https://tc39.es/ecma262/#sec-promise-resolve-functions
        fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
            const resolution = arguments.get(0);

            // 1. Let F be the active function object.
            const function = agent_.activeFunctionObject();

            // 2. Assert: F has a [[Promise]] internal slot whose value is an Object.
            const additional_fields_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

            // 3. Let promise be F.[[Promise]].
            const promise_ = additional_fields_.promise;

            // 4. Let alreadyResolved be F.[[AlreadyResolved]].
            const already_resolved_ = &additional_fields_.already_resolved;

            // 5. If alreadyResolved.[[Value]] is true, return undefined.
            if (already_resolved_.value) return .undefined;

            // 6. Set alreadyResolved.[[Value]] to true.
            already_resolved_.value = true;

            // 7. If SameValue(resolution, promise) is true, then
            if (sameValue(resolution, Value.from(&promise_.object))) {
                // a. Let selfResolutionError be a newly created TypeError object.
                const self_resolution_error = try agent_.createErrorObject(
                    .type_error,
                    "Cannot resolve promise with itself",
                    .{},
                );

                // b. Perform RejectPromise(promise, selfResolutionError).
                try rejectPromise(agent_, promise_, Value.from(self_resolution_error));

                // c. Return undefined.
                return .undefined;
            }

            // 8. If resolution is not an Object, then
            if (!resolution.isObject()) {
                // a. Perform FulfillPromise(promise, resolution).
                try fulfillPromise(agent_, promise_, resolution);

                // b. Return undefined.
                return .undefined;
            }

            // 9. Let then be Completion(Get(resolution, "then")).
            // 11. Let thenAction be then.[[Value]].
            const then_action = resolution.asObject().get(
                agent_,
                PropertyKey.from("then"),
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                // 10. If then is an abrupt completion, then
                error.ExceptionThrown => {
                    const exception = agent_.clearException();

                    // a. Perform RejectPromise(promise, then.[[Value]]).
                    try rejectPromise(agent_, promise_, exception.value);

                    // b. Return undefined.
                    return .undefined;
                },
            };

            // 12. If IsCallable(thenAction) is false, then
            if (!then_action.isCallable()) {
                // a. Perform FulfillPromise(promise, resolution).
                try fulfillPromise(agent_, promise_, resolution);

                // b. Return undefined.
                return .undefined;
            }

            // 13. Let thenJobCallback be HostMakeJobCallback(thenAction).
            const then_job_callback = agent_.host_hooks.hostMakeJobCallback(then_action.asObject());

            // 14. Let job be NewPromiseResolveThenableJob(promise, resolution, thenJobCallback).
            const job = try newPromiseResolveThenableJob(
                agent_,
                promise_,
                resolution.asObject(),
                then_job_callback,
            );

            // 15. Perform HostEnqueuePromiseJob(job.[[Job]], job.[[Realm]]).
            try agent_.host_hooks.hostEnqueuePromiseJob(agent_, job.job, job.realm);

            // 16. Return undefined.
            return .undefined;
        }
    }.func;

    // 3. Let lengthResolve be the number of non-optional parameters of the function definition in
    //    Promise Resolve Functions.
    const length_resolve = 1;

    // 4. Let resolve be CreateBuiltinFunction(stepsResolve, lengthResolve, "", « [[Promise]],
    //    [[AlreadyResolved]] »).
    const resolve = try createBuiltinFunction(
        agent,
        .{ .function = steps_resolve },
        length_resolve,
        "",
        .{ .additional_fields = .make(*AdditionalFields, additional_fields) },
    );

    additional_fields.* = .{
        // 5. Set resolve.[[Promise]] to promise.
        .promise = promise,

        // 6. Set resolve.[[AlreadyResolved]] to alreadyResolved.
        .already_resolved = already_resolved,
    };

    // 7. Let stepsReject be the algorithm steps defined in Promise Reject Functions.
    const steps_reject = struct {
        /// 27.2.1.3.1 Promise Reject Functions
        /// https://tc39.es/ecma262/#sec-promise-reject-functions
        fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
            const reason = arguments.get(0);

            // 1. Let F be the active function object.
            const function = agent_.activeFunctionObject();

            // 2. Assert: F has a [[Promise]] internal slot whose value is an Object.
            const additional_fields_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

            // 3. Let promise be F.[[Promise]].
            const promise_ = additional_fields_.promise;

            // 4. Let alreadyResolved be F.[[AlreadyResolved]].
            const already_resolved_ = &additional_fields_.already_resolved;

            // 5. If alreadyResolved.[[Value]] is true, return undefined.
            if (already_resolved_.value) return .undefined;

            // 6. Set alreadyResolved.[[Value]] to true.
            already_resolved_.value = true;

            // 7. Perform RejectPromise(promise, reason).
            try rejectPromise(agent_, promise_, reason);

            // 8. Return undefined.
            return .undefined;
        }
    }.func;

    // 8. Let lengthReject be the number of non-optional parameters of the function definition in
    //    Promise Reject Functions.
    const length_reject = 1;

    // 9. Let reject be CreateBuiltinFunction(stepsReject, lengthReject, "", « [[Promise]],
    //    [[AlreadyResolved]] »).
    const reject = try createBuiltinFunction(
        agent,
        .{ .function = steps_reject },
        length_reject,
        "",
        .{ .additional_fields = .make(*AdditionalFields, additional_fields) },
    );

    // 10. Set reject.[[Promise]] to promise.
    // 11. Set reject.[[AlreadyResolved]] to alreadyResolved.
    // NOTE: This was already done for the resolve function, `additional_fields` is shared between both.

    // 12. Return the Record { [[Resolve]]: resolve, [[Reject]]: reject }.
    return .{ .resolve = resolve, .reject = reject };
}

/// 27.2.1.4 FulfillPromise ( promise, value )
/// https://tc39.es/ecma262/#sec-fulfillpromise
pub fn fulfillPromise(
    agent: *Agent,
    promise: *Promise,
    value: Value,
) std.mem.Allocator.Error!void {
    // 1. Assert: The value of promise.[[PromiseState]] is pending.
    std.debug.assert(promise.fields.promise_state == .pending);

    // 2. Let reactions be promise.[[PromiseFulfillReactions]].
    const reactions = &promise.fields.promise_fulfill_reactions;

    // 3. Set promise.[[PromiseResult]] to value.
    promise.fields.promise_result = value;

    // 4. Set promise.[[PromiseFulfillReactions]] to undefined.
    defer promise.fields.promise_fulfill_reactions.deinit(agent.gc_allocator);

    // 5. Set promise.[[PromiseRejectReactions]] to undefined.
    defer promise.fields.promise_reject_reactions.deinit(agent.gc_allocator);

    // 6. Set promise.[[PromiseState]] to fulfilled.
    promise.fields.promise_state = .fulfilled;

    // 7. Perform TriggerPromiseReactions(reactions, value).
    try triggerPromiseReactions(agent, reactions.items, value);

    // 8. Return unused.
}

/// 27.2.1.5 NewPromiseCapability ( C )
/// https://tc39.es/ecma262/#sec-newpromisecapability
pub fn newPromiseCapability(agent: *Agent, constructor_: Value) Agent.Error!PromiseCapability {
    // 1. If IsConstructor(C) is false, throw a TypeError exception.
    if (!constructor_.isConstructor()) {
        return agent.throwException(.type_error, "{} is not a constructor", .{constructor_});
    }

    // 2. NOTE: C is assumed to be a constructor function that supports the parameter conventions
    //    of the Promise constructor (see 27.2.3.1).

    // 3. Let resolvingFunctions be the Record { [[Resolve]]: undefined, [[Reject]]: undefined }.
    // NOTE: This is created later.

    const AdditionalFields = struct {
        resolving_functions: struct {
            resolve: Value,
            reject: Value,
        },
    };

    // 4. Let executorClosure be a new Abstract Closure with parameters (resolve, reject) that
    //    captures resolvingFunctions and performs the following steps when called:
    const executor_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
            const resolve = arguments.get(0);
            const reject = arguments.get(1);
            const function = agent_.activeFunctionObject();
            const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);
            const resolving_functions_ = &additional_fields.resolving_functions;

            // a. If resolvingFunctions.[[Resolve]] is not undefined, throw a TypeError exception.
            if (!resolving_functions_.resolve.isUndefined()) {
                return agent_.throwException(
                    .type_error,
                    "Resolve function has already been set",
                    .{},
                );
            }
            // b. If resolvingFunctions.[[Reject]] is not undefined, throw a TypeError exception.
            if (!resolving_functions_.reject.isUndefined()) {
                return agent_.throwException(
                    .type_error,
                    "Reject function has already been set",
                    .{},
                );
            }

            // c. Set resolvingFunctions.[[Resolve]] to resolve.
            resolving_functions_.resolve = resolve;

            // d. Set resolvingFunctions.[[Reject]] to reject.
            resolving_functions_.reject = reject;

            // e. Return undefined.
            return .undefined;
        }
    }.func;

    // 5. Let executor be CreateBuiltinFunction(executorClosure, 2, "", « »).
    const additional_fields = try agent.gc_allocator.create(AdditionalFields);
    const executor = try createBuiltinFunction(
        agent,
        .{ .function = executor_closure },
        2,
        "",
        .{ .additional_fields = .make(*AdditionalFields, additional_fields) },
    );

    // NOTE: This struct can outlive the function scope if anything holds on to the callback above.
    additional_fields.* = .{
        .resolving_functions = .{ .resolve = .undefined, .reject = .undefined },
    };
    const resolving_functions = &additional_fields.resolving_functions;

    // 6. Let promise be ? Construct(C, « executor »).
    const promise = try constructor_.asObject().construct(agent, &.{Value.from(executor)}, null);

    // 7. If IsCallable(resolvingFunctions.[[Resolve]]) is false, throw a TypeError exception.
    if (!resolving_functions.resolve.isCallable()) {
        return agent.throwException(.type_error, "{} is not callable", .{resolving_functions.resolve});
    }

    // 8. If IsCallable(resolvingFunctions.[[Reject]]) is false, throw a TypeError exception.
    if (!resolving_functions.reject.isCallable()) {
        return agent.throwException(
            .type_error,
            "{} is not callable",
            .{resolving_functions.reject},
        );
    }

    // 9. Return the PromiseCapability Record { [[Promise]]: promise, [[Resolve]]:
    //    resolvingFunctions.[[Resolve]], [[Reject]]: resolvingFunctions.[[Reject]] }.
    return .{
        .promise = promise,
        .resolve = resolving_functions.resolve.asObject(),
        .reject = resolving_functions.reject.asObject(),
    };
}

/// 27.2.1.7 RejectPromise ( promise, reason )
/// https://tc39.es/ecma262/#sec-rejectpromise
pub fn rejectPromise(
    agent: *Agent,
    promise: *Promise,
    reason: Value,
) std.mem.Allocator.Error!void {
    // 1. Assert: The value of promise.[[PromiseState]] is pending.
    std.debug.assert(promise.fields.promise_state == .pending);

    // 2. Let reactions be promise.[[PromiseRejectReactions]].
    const reactions = promise.fields.promise_reject_reactions;

    // 3. Set promise.[[PromiseResult]] to reason.
    promise.fields.promise_result = reason;

    // 4. Set promise.[[PromiseFulfillReactions]] to undefined.
    defer promise.fields.promise_fulfill_reactions.deinit(agent.gc_allocator);

    // 5. Set promise.[[PromiseRejectReactions]] to undefined.
    defer promise.fields.promise_reject_reactions.deinit(agent.gc_allocator);

    // 6. Set promise.[[PromiseState]] to rejected.
    promise.fields.promise_state = .rejected;

    // 7. If promise.[[PromiseIsHandled]] is false, perform HostPromiseRejectionTracker(promise, "reject").
    if (!promise.fields.promise_is_handled) {
        agent.host_hooks.hostPromiseRejectionTracker(agent, promise, .reject);
    }

    // 8. Perform TriggerPromiseReactions(reactions, reason).
    try triggerPromiseReactions(agent, reactions.items, reason);

    // 9. Return unused.
}

/// 27.2.1.8 TriggerPromiseReactions ( reactions, argument )
/// https://tc39.es/ecma262/#sec-triggerpromisereactions
pub fn triggerPromiseReactions(
    agent: *Agent,
    reactions: []const PromiseReaction,
    argument: Value,
) std.mem.Allocator.Error!void {
    // 1. For each element reaction of reactions, do
    for (reactions) |reaction| {
        // a. Let job be NewPromiseReactionJob(reaction, argument).
        const job = try newPromiseReactionJob(agent, reaction, argument);

        // b. Perform HostEnqueuePromiseJob(job.[[Job]], job.[[Realm]]).
        try agent.host_hooks.hostEnqueuePromiseJob(agent, job.job, job.realm);
    }

    // 2. Return unused.
}

/// 27.2.4.7.1 PromiseResolve ( C, x )
/// https://tc39.es/ecma262/#sec-promise-resolve
pub fn promiseResolve(agent: *Agent, constructor_: *Object, x: Value) Agent.Error!*Object {
    // 1. If IsPromise(x) is true, then
    if (x.isPromise()) {
        // a. Let xConstructor be ? Get(x, "constructor").
        const x_constructor = try x.asObject().get(agent, PropertyKey.from("constructor"));

        // b. If SameValue(xConstructor, C) is true, return x.
        if (sameValue(x_constructor, Value.from(constructor_))) return x.asObject();
    }

    // 2. Let promiseCapability be ? NewPromiseCapability(C).
    const promise_capability = try newPromiseCapability(agent, Value.from(constructor_));

    // 3. Perform ? Call(promiseCapability.[[Resolve]], undefined, « x »).
    _ = try Value.from(promise_capability.resolve).callAssumeCallable(agent, .undefined, &.{x});

    // 4. Return promiseCapability.[[Promise]].
    return promise_capability.promise;
}

/// 27.2.2.1 NewPromiseReactionJob ( reaction, argument )
/// https://tc39.es/ecma262/#sec-newpromisereactionjob
pub fn newPromiseReactionJob(
    agent: *Agent,
    reaction: PromiseReaction,
    argument: Value,
) std.mem.Allocator.Error!struct { job: Job, realm: ?*Realm } {
    const Captures = struct {
        agent: *Agent,
        reaction: PromiseReaction,
        argument: Value,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .agent = agent, .reaction = reaction, .argument = argument };

    // 1. Let job be a new Job Abstract Closure with no parameters that captures reaction and
    //    argument and performs the following steps when called:
    const func = struct {
        fn func(captures_: SafePointer) Agent.Error!Value {
            const agent_ = captures_.cast(*Captures).agent;
            const reaction_ = captures_.cast(*Captures).reaction;
            const argument_ = captures_.cast(*Captures).argument;

            // a. Let promiseCapability be reaction.[[Capability]].
            const promise_capability = reaction_.capability;

            // b. Let type be reaction.[[Type]].
            const @"type" = reaction_.type;

            // c. Let handler be reaction.[[Handler]].
            const handler = reaction_.handler;

            // d. If handler is empty, then
            const handler_result = if (handler == null) blk: {
                switch (@"type") {
                    // i. If type is fulfill, then
                    .fulfill => {
                        // 1. Let handlerResult be NormalCompletion(argument).
                        break :blk Completion.normal(argument_);
                    },
                    // ii. Else,
                    //    1. Assert: type is reject.
                    .reject => {
                        // 2. Let handlerResult be ThrowCompletion(argument).
                        break :blk Completion.throw(argument_);
                    },
                }
            } else blk: {
                // e. Else,
                // i. Let handlerResult be Completion(HostCallJobCallback(handler, undefined, « argument »)).
                if (agent_.host_hooks.hostCallJobCallback(
                    agent_,
                    handler.?,
                    .undefined,
                    &.{argument_},
                )) |value|
                    break :blk Completion.normal(value)
                else |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.ExceptionThrown => {
                        const exception = agent_.clearException();
                        break :blk Completion.throw(exception.value);
                    },
                }
            };

            // f. If promiseCapability is undefined, then
            if (promise_capability == null) {
                // i. Assert: handlerResult is not an abrupt completion.
                std.debug.assert(handler_result.type == .normal);

                // ii. Return empty.
                return .undefined;
            }

            // g. Assert: promiseCapability is a PromiseCapability Record.
            // h. If handlerResult is an abrupt completion, then
            if (handler_result.type != .normal) {
                // i. Return ? Call(promiseCapability.[[Reject]], undefined, « handlerResult.[[Value]] »).
                return Value.from(promise_capability.?.reject).callAssumeCallable(
                    agent_,
                    .undefined,
                    &.{handler_result.value.?},
                );
            } else {
                // i. Else,
                // i. Return ? Call(promiseCapability.[[Resolve]], undefined, « handlerResult.[[Value]] »).
                return Value.from(promise_capability.?.resolve).callAssumeCallable(
                    agent_,
                    .undefined,
                    &.{handler_result.value.?},
                );
            }
        }
    }.func;
    const job: Job = .{ .func = func, .captures = .make(*Captures, captures) };

    // 2. Let handlerRealm be null.
    var handler_realm: ?*Realm = null;

    // 3. If reaction.[[Handler]] is not empty, then
    if (reaction.handler) |handler| {
        // a. Let getHandlerRealmResult be Completion(GetFunctionRealm(reaction.[[Handler]].[[Callback]])).
        const get_handler_realm_result = handler.callback.getFunctionRealm(agent);

        // b. If getHandlerRealmResult is a normal completion, set handlerRealm to
        //    getHandlerRealmResult.[[Value]].
        if (get_handler_realm_result) |realm| {
            handler_realm = realm;
        }
        // c. Else, set handlerRealm to the current Realm Record.
        else |_| {
            handler_realm = agent.currentRealm();
        }

        // d. NOTE: handlerRealm is never null unless the handler is undefined. When the handler is
        //    a revoked Proxy and no ECMAScript code runs, handlerRealm is used to create error objects.
    }

    // 4. Return the Record { [[Job]]: job, [[Realm]]: handlerRealm }.
    return .{ .job = job, .realm = handler_realm };
}

/// 27.2.2.2 NewPromiseResolveThenableJob ( promiseToResolve, thenable, then )
/// https://tc39.es/ecma262/#sec-newpromiseresolvethenablejob
pub fn newPromiseResolveThenableJob(
    agent: *Agent,
    promise_to_resolve: *Promise,
    thenable: *Object,
    then: JobCallback,
) std.mem.Allocator.Error!struct { job: Job, realm: *Realm } {
    const Captures = struct {
        agent: *Agent,
        promise_to_resolve: *Promise,
        thenable: *Object,
        then: JobCallback,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{
        .agent = agent,
        .promise_to_resolve = promise_to_resolve,
        .thenable = thenable,
        .then = then,
    };

    // 1. Let job be a new Job Abstract Closure with no parameters that captures promiseToResolve,
    //    thenable, and then and performs the following steps when called:
    const func = struct {
        fn func(captures_: SafePointer) Agent.Error!Value {
            const agent_ = captures_.cast(*Captures).agent;
            const promise_to_resolve_ = captures_.cast(*Captures).promise_to_resolve;
            const thenable_ = captures_.cast(*Captures).thenable;
            const then_ = captures_.cast(*Captures).then;

            // a. Let resolvingFunctions be CreateResolvingFunctions(promiseToResolve).
            const resolving_functions = try createResolvingFunctions(agent_, promise_to_resolve_);

            // b. Let thenCallResult be Completion(HostCallJobCallback(then, thenable,
            //    « resolvingFunctions.[[Resolve]], resolvingFunctions.[[Reject]] »)).
            const then_call_result = agent_.host_hooks.hostCallJobCallback(
                agent_,
                then_,
                Value.from(thenable_),
                &.{
                    Value.from(resolving_functions.resolve),
                    Value.from(resolving_functions.reject),
                },
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                // c. If thenCallResult is an abrupt completion, then
                error.ExceptionThrown => {
                    const exception = agent_.clearException();

                    // i. Return ? Call(resolvingFunctions.[[Reject]], undefined,
                    //    « thenCallResult.[[Value]] »).
                    return Value.from(resolving_functions.reject).callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{exception.value},
                    );
                },
            };

            // d. Return ! thenCallResult.
            return then_call_result;
        }
    }.func;
    const job: Job = .{ .func = func, .captures = .make(*Captures, captures) };

    // 2. Let getThenRealmResult be Completion(GetFunctionRealm(then.[[Callback]])).
    const get_handler_realm_result = then.callback.getFunctionRealm(agent);

    // 3. If getThenRealmResult is a normal completion, let thenRealm be
    //    getThenRealmResult.[[Value]].
    const then_realm = if (get_handler_realm_result) |realm| blk: {
        break :blk realm;
    } else |_| blk: {
        // 4. Else, let thenRealm be the current Realm Record.
        break :blk agent.currentRealm();
    };

    // 5. NOTE: thenRealm is never null. When then.[[Callback]] is a revoked Proxy and no code
    //    runs, thenRealm is used to create error objects.

    // 6. Return the Record { [[Job]]: job, [[Realm]]: thenRealm }.
    return .{ .job = job, .realm = then_realm };
}

/// 27.2.4.1.1 GetPromiseResolve ( promiseConstructor )
fn getPromiseResolve(agent: *Agent, promise_constructor: *Object) Agent.Error!*Object {
    // 1. Let promiseResolve be ? Get(promiseConstructor, "resolve").
    const promise_resolve = try promise_constructor.get(agent, PropertyKey.from("resolve"));

    // 2. If IsCallable(promiseResolve) is false, throw a TypeError exception.
    if (!promise_resolve.isCallable()) {
        return agent.throwException(.type_error, "{} is not callable", .{promise_resolve});
    }

    // 3. Return promiseResolve.
    return promise_resolve.asObject();
}

const RemainingElements = struct {
    value: usize,
};

/// 27.2.4.1.2 PerformPromiseAll ( iteratorRecord, constructor, resultCapability, promiseResolve )
/// https://tc39.es/ecma262/#sec-performpromiseall
fn performPromiseAll(
    agent: *Agent,
    iterator: *Iterator,
    constructor_: *Object,
    result_capability: PromiseCapability,
    promise_resolve: *Object,
) Agent.Error!Value {
    // 1. Let values be a new empty List.
    var values = try agent.gc_allocator.create(std.ArrayListUnmanaged(Value));
    values.* = .empty;

    // 2. Let remainingElementsCount be the Record { [[Value]]: 1 }.
    var remaining_elements_count = try agent.gc_allocator.create(RemainingElements);
    remaining_elements_count.* = .{ .value = 1 };

    // 3. Let index be 0.
    var index: usize = 0;

    // 4. Repeat,
    while (true) {
        // a. Let next be ? IteratorStepValue(iteratorRecord).
        // b. If next is done, then
        const next = try iterator.stepValue(agent) orelse {
            // i. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
            remaining_elements_count.value -= 1;

            // ii. If remainingElementsCount.[[Value]] = 0, then
            if (remaining_elements_count.value == 0) {
                // 1. Let valuesArray be CreateArrayFromList(values).
                const values_array = try createArrayFromList(agent, values.items);

                // 2. Perform ? Call(resultCapability.[[Resolve]], undefined, « valuesArray »).
                _ = try Value.from(result_capability.resolve).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{Value.from(values_array)},
                );
            }

            // iii. Return resultCapability.[[Promise]].
            return Value.from(result_capability.promise);
        };

        // c. Append undefined to values.
        try values.append(agent.gc_allocator, .undefined);

        // d. Let nextPromise be ? Call(promiseResolve, constructor, « next »).
        const next_promise = try Value.from(promise_resolve).callAssumeCallable(
            agent,
            Value.from(constructor_),
            &.{next},
        );

        const AdditionalFields = struct {
            /// [[AlreadyCalled]]
            already_called: bool,

            /// [[Index]]
            index: usize,

            /// [[Values]]
            values: *std.ArrayListUnmanaged(Value),

            /// [[Capability]]
            capability: PromiseCapability,

            /// [[RemainingElements]]
            remaining_elements: *RemainingElements,
        };

        // e. Let steps be the algorithm steps defined in Promise.all Resolve Element Functions.
        const steps = struct {
            /// 27.2.4.1.3 Promise.all Resolve Element Functions
            /// https://tc39.es/ecma262/#sec-promise.all-resolve-element-functions
            fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
                const x = arguments.get(0);

                // 1. Let F be the active function object.
                const function = agent_.activeFunctionObject();

                const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

                // 2. If F.[[AlreadyCalled]] is true, return undefined.
                if (additional_fields.already_called) return .undefined;

                // 3. Set F.[[AlreadyCalled]] to true.
                additional_fields.already_called = true;

                // 4. Let index be F.[[Index]].
                const index_ = additional_fields.index;

                // 5. Let values be F.[[Values]].
                const values_ = additional_fields.values;

                // 6. Let promiseCapability be F.[[Capability]].
                const promise_capability = additional_fields.capability;

                // 7. Let remainingElementsCount be F.[[RemainingElements]].
                const remaining_elements_count_ = additional_fields.remaining_elements;

                // 8. Set values[index] to x.
                values_.items[index_] = x;

                // 9. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
                remaining_elements_count_.value -= 1;

                // 10. If remainingElementsCount.[[Value]] = 0, then
                if (remaining_elements_count_.value == 0) {
                    // a. Let valuesArray be CreateArrayFromList(values).
                    const values_array = try createArrayFromList(agent_, values_.items);

                    // b. Return ? Call(promiseCapability.[[Resolve]], undefined, « valuesArray »).
                    return Value.from(promise_capability.resolve).callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{Value.from(values_array)},
                    );
                }

                // 11. Return undefined.
                return .undefined;
            }
        }.func;

        // f. Let length be the number of non-optional parameters of the function definition in
        //    Promise.all Resolve Element Functions.
        // g. Let onFulfilled be CreateBuiltinFunction(steps, length, "", « [[AlreadyCalled]],
        //    [[Index]], [[Values]], [[Capability]], [[RemainingElements]] »).
        const additional_fields = try agent.gc_allocator.create(AdditionalFields);
        const on_fulfilled = try createBuiltinFunction(
            agent,
            .{ .function = steps },
            1,
            "",
            .{ .additional_fields = .make(*AdditionalFields, additional_fields) },
        );

        additional_fields.* = .{
            // h. Set onFulfilled.[[AlreadyCalled]] to false.
            .already_called = false,

            // i. Set onFulfilled.[[Index]] to index.
            .index = index,

            // j. Set onFulfilled.[[Values]] to values.
            .values = values,

            // k. Set onFulfilled.[[Capability]] to resultCapability.
            .capability = result_capability,

            // l. Set onFulfilled.[[RemainingElements]] to remainingElementsCount.
            .remaining_elements = remaining_elements_count,
        };

        // m. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] + 1.
        remaining_elements_count.value += 1;

        // n. Perform ? Invoke(nextPromise, "then", « onFulfilled, resultCapability.[[Reject]] »).
        _ = try next_promise.invoke(
            agent,
            PropertyKey.from("then"),
            &.{ Value.from(on_fulfilled), Value.from(result_capability.reject) },
        );

        // o. Set index to index + 1.
        index += 1;
    }
}

/// 27.2.4.2.1 PerformPromiseAllSettled ( iteratorRecord, constructor, resultCapability, promiseResolve )
/// https://tc39.es/ecma262/#sec-performpromiseallsettled
fn performPromiseAllSettled(
    agent: *Agent,
    iterator: *Iterator,
    constructor_: *Object,
    result_capability: PromiseCapability,
    promise_resolve: *Object,
) Agent.Error!Value {
    // 1. Let values be a new empty List.
    var values = try agent.gc_allocator.create(std.ArrayListUnmanaged(Value));
    values.* = .empty;

    // 2. Let remainingElementsCount be the Record { [[Value]]: 1 }.
    var remaining_elements_count = try agent.gc_allocator.create(RemainingElements);
    remaining_elements_count.* = .{ .value = 1 };

    // 3. Let index be 0.
    var index: usize = 0;

    // 4. Repeat,
    while (true) {
        // a. Let next be ? IteratorStepValue(iteratorRecord).
        // b. If next is done, then
        const next = try iterator.stepValue(agent) orelse {
            // i. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
            remaining_elements_count.value -= 1;

            // ii. If remainingElementsCount.[[Value]] = 0, then
            if (remaining_elements_count.value == 0) {
                // 1. Let valuesArray be CreateArrayFromList(values).
                const values_array = try createArrayFromList(agent, values.items);

                // 2. Perform ? Call(resultCapability.[[Resolve]], undefined, « valuesArray »).
                _ = try Value.from(result_capability.resolve).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{Value.from(values_array)},
                );
            }

            // iii. Return resultCapability.[[Promise]].
            return Value.from(result_capability.promise);
        };

        // c. Append undefined to values.
        try values.append(agent.gc_allocator, .undefined);

        // d. Let nextPromise be ? Call(promiseResolve, constructor, « next »).
        const next_promise = try Value.from(promise_resolve).callAssumeCallable(
            agent,
            Value.from(constructor_),
            &.{next},
        );

        const AlreadyCalled = struct { value: bool };
        const AdditionalFields = struct {
            /// [[AlreadyCalled]]
            already_called: *AlreadyCalled,

            /// [[Index]]
            index: usize,

            /// [[Values]]
            values: *std.ArrayListUnmanaged(Value),

            /// [[Capability]]
            capability: PromiseCapability,

            /// [[RemainingElements]]
            remaining_elements: *RemainingElements,
        };

        // e. Let stepsFulfilled be the algorithm steps defined in Promise.allSettled Resolve
        //    Element Functions.
        const steps_fulfilled = struct {
            /// 27.2.4.2.2 Promise.allSettled Resolve Element Functions
            /// https://tc39.es/ecma262/#sec-promise.allsettled-resolve-element-
            fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
                const realm = agent_.currentRealm();
                const x = arguments_.get(0);

                // 1. Let F be the active function object.
                const function = agent_.activeFunctionObject();

                const additional_fields_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

                // 2. Let alreadyCalled be F.[[AlreadyCalled]].
                const already_called_ = additional_fields_.already_called;

                // 3. If alreadyCalled.[[Value]] is true, return undefined.
                if (already_called_.value) return .undefined;

                // 4. Set alreadyCalled.[[Value]] to true.
                already_called_.value = true;

                // 5. Let index be F.[[Index]].
                const index_ = additional_fields_.index;

                // 6. Let values be F.[[Values]].
                const values_ = additional_fields_.values;

                // 7. Let promiseCapability be F.[[Capability]].
                const promise_capability = additional_fields_.capability;

                // 8. Let remainingElementsCount be F.[[RemainingElements]].
                const remaining_elements_count_ = additional_fields_.remaining_elements;

                // 9. Let obj be OrdinaryObjectCreate(%Object.prototype%).
                const object = try ordinaryObjectCreate(
                    agent_,
                    try realm.intrinsics.@"%Object.prototype%"(),
                );

                // 10. Perform ! CreateDataPropertyOrThrow(obj, "status", "fulfilled").
                try object.createDataPropertyDirect(
                    agent_,
                    PropertyKey.from("status"),
                    Value.from("fulfilled"),
                );

                // 11. Perform ! CreateDataPropertyOrThrow(obj, "value", x).
                try object.createDataPropertyDirect(agent_, PropertyKey.from("value"), x);

                // 12. Set values[index] to obj.
                values_.items[index_] = Value.from(object);

                // 13. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
                remaining_elements_count_.value -= 1;

                // 14. If remainingElementsCount.[[Value]] = 0, then
                if (remaining_elements_count_.value == 0) {
                    // a. Let valuesArray be CreateArrayFromList(values).
                    const values_array = try createArrayFromList(agent_, values_.items);

                    // b. Return ? Call(promiseCapability.[[Resolve]], undefined, « valuesArray »).
                    return Value.from(promise_capability.resolve).callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{Value.from(values_array)},
                    );
                }

                // 15. Return undefined.
                return .undefined;
            }
        }.func;

        // f. Let lengthFulfilled be the number of non-optional parameters of the function
        //    definition in Promise.allSettled Resolve Element Functions.
        // g. Let onFulfilled be CreateBuiltinFunction(stepsFulfilled, lengthFulfilled, "",
        //    « [[AlreadyCalled]], [[Index]], [[Values]], [[Capability]], [[RemainingElements]] »).
        const on_fulfilled_additional_fields = try agent.gc_allocator.create(AdditionalFields);
        const on_fulfilled = try createBuiltinFunction(
            agent,
            .{ .function = steps_fulfilled },
            1,
            "",
            .{ .additional_fields = .make(*AdditionalFields, on_fulfilled_additional_fields) },
        );

        // h. Let alreadyCalled be the Record { [[Value]]: false }.
        const already_called = try agent.gc_allocator.create(AlreadyCalled);
        already_called.* = .{ .value = false };

        on_fulfilled_additional_fields.* = .{
            // i. Set onFulfilled.[[AlreadyCalled]] to alreadyCalled.
            .already_called = already_called,

            // j. Set onFulfilled.[[Index]] to index.
            .index = index,

            // k. Set onFulfilled.[[Values]] to values.
            .values = values,

            // l. Set onFulfilled.[[Capability]] to resultCapability.
            .capability = result_capability,

            // m. Set onFulfilled.[[RemainingElements]] to remainingElementsCount.
            .remaining_elements = remaining_elements_count,
        };

        // n. Let stepsRejected be the algorithm steps defined in Promise.allSettled Reject Element
        //    Functions.
        const steps_rejected = struct {
            /// 27.2.4.2.3 Promise.allSettled Reject Element Functions
            /// https://tc39.es/ecma262/#sec-promise.allsettled-reject-element-functions
            fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
                const realm = agent_.currentRealm();
                const x = arguments_.get(0);

                // 1. Let F be the active function object.
                const function = agent_.activeFunctionObject();

                const additional_fields_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

                // 2. Let alreadyCalled be F.[[AlreadyCalled]].
                const already_called_ = additional_fields_.already_called;

                // 3. If alreadyCalled.[[Value]] is true, return undefined.
                if (already_called_.value) return .undefined;

                // 4. Set alreadyCalled.[[Value]] to true.
                already_called_.value = true;

                // 5. Let index be F.[[Index]].
                const index_ = additional_fields_.index;

                // 6. Let values be F.[[Values]].
                const values_ = additional_fields_.values;

                // 7. Let promiseCapability be F.[[Capability]].
                const promise_capability = additional_fields_.capability;

                // 8. Let remainingElementsCount be F.[[RemainingElements]].
                const remaining_elements_count_ = additional_fields_.remaining_elements;

                // 9. Let obj be OrdinaryObjectCreate(%Object.prototype%).
                const object = try ordinaryObjectCreate(
                    agent_,
                    try realm.intrinsics.@"%Object.prototype%"(),
                );

                // 10. Perform ! CreateDataPropertyOrThrow(obj, "status", "rejected").
                try object.createDataPropertyDirect(
                    agent_,
                    PropertyKey.from("status"),
                    Value.from("rejected"),
                );

                // 11. Perform ! CreateDataPropertyOrThrow(obj, "reason", x).
                try object.createDataPropertyDirect(agent_, PropertyKey.from("reason"), x);

                // 12. Set values[index] to obj.
                values_.items[index_] = Value.from(object);

                // 13. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
                remaining_elements_count_.value -= 1;

                // 14. If remainingElementsCount.[[Value]] = 0, then
                if (remaining_elements_count_.value == 0) {
                    // a. Let valuesArray be CreateArrayFromList(values).
                    const values_array = try createArrayFromList(agent_, values_.items);

                    // b. Return ? Call(promiseCapability.[[Resolve]], undefined, « valuesArray »).
                    return Value.from(promise_capability.resolve).callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{Value.from(values_array)},
                    );
                }

                // 15. Return undefined.
                return .undefined;
            }
        }.func;

        // o. Let lengthRejected be the number of non-optional parameters of the function
        //    definition in Promise.allSettled Reject Element Functions.
        // p. Let onRejected be CreateBuiltinFunction(stepsRejected, lengthRejected, "",
        //    « [[AlreadyCalled]], [[Index]], [[Values]], [[Capability]], [[RemainingElements]] »).
        const on_rejected_additional_fields = try agent.gc_allocator.create(AdditionalFields);
        const on_rejected = try createBuiltinFunction(
            agent,
            .{ .function = steps_rejected },
            1,
            "",
            .{ .additional_fields = .make(*AdditionalFields, on_rejected_additional_fields) },
        );

        on_rejected_additional_fields.* = .{
            // q. Set onRejected.[[AlreadyCalled]] to alreadyCalled.
            .already_called = already_called,

            // r. Set onRejected.[[Index]] to index.
            .index = index,

            // s. Set onRejected.[[Values]] to values.
            .values = values,

            // t. Set onRejected.[[Capability]] to resultCapability.
            .capability = result_capability,

            // u. Set onRejected.[[RemainingElements]] to remainingElementsCount.
            .remaining_elements = remaining_elements_count,
        };

        // v. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] + 1.
        remaining_elements_count.value += 1;

        // w. Perform ? Invoke(nextPromise, "then", « onFulfilled, onRejected »).
        _ = try next_promise.invoke(
            agent,
            PropertyKey.from("then"),
            &.{ Value.from(on_fulfilled), Value.from(on_rejected) },
        );

        // x. Set index to index + 1.
        index += 1;
    }
}

/// 27.2.4.3.1 PerformPromiseAny ( iteratorRecord, constructor, resultCapability, promiseResolve )
/// https://tc39.es/ecma262/#sec-performpromiseany
fn performPromiseAny(
    agent: *Agent,
    iterator: *Iterator,
    constructor_: *Object,
    result_capability: PromiseCapability,
    promise_resolve: *Object,
) Agent.Error!Value {
    // 1. Let errors be a new empty List.
    var errors = try agent.gc_allocator.create(std.ArrayListUnmanaged(Value));
    errors.* = .empty;

    // 2. Let remainingElementsCount be the Record { [[Value]]: 1 }.
    var remaining_elements_count = try agent.gc_allocator.create(RemainingElements);
    remaining_elements_count.* = .{ .value = 1 };

    // 3. Let index be 0.
    var index: usize = 0;

    // 4. Repeat,
    while (true) {
        // a. Let next be ? IteratorStepValue(iteratorRecord).
        // b. If next is done, then
        const next = try iterator.stepValue(agent) orelse {
            // i. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
            remaining_elements_count.value -= 1;

            // ii. If remainingElementsCount.[[Value]] = 0, then
            if (remaining_elements_count.value == 0) {
                // 1. Let error be a newly created AggregateError object.
                const error_ = try agent.createErrorObject(
                    .aggregate_error,
                    "All promises were rejected",
                    .{},
                );

                // 2. Perform ! DefinePropertyOrThrow(error, "errors", PropertyDescriptor {
                //      [[Configurable]]: true, [[Enumerable]]: false, [[Writable]]: true,
                //      [[Value]]: CreateArrayFromList(errors)
                //    }).
                try error_.definePropertyDirect(agent, PropertyKey.from("errors"), .{
                    .value_or_accessor = .{
                        .value = Value.from(try createArrayFromList(agent, errors.items)),
                    },
                    .attributes = .builtin_default,
                });

                // 3. Return ThrowCompletion(error).
                agent.exception = .{
                    .value = Value.from(error_),
                    .stack_trace = try agent.captureStackTrace(),
                };
                return error.ExceptionThrown;
            }

            // iii. Return resultCapability.[[Promise]].
            return Value.from(result_capability.promise);
        };

        // c. Append undefined to errors.
        try errors.append(agent.gc_allocator, .undefined);

        // d. Let nextPromise be ? Call(promiseResolve, constructor, « next »).
        const next_promise = try Value.from(promise_resolve).callAssumeCallable(
            agent,
            Value.from(constructor_),
            &.{next},
        );

        const AdditionalFields = struct {
            /// [[AlreadyCalled]]
            already_called: bool,

            /// [[Index]]
            index: usize,

            /// [[Errors]]
            errors: *std.ArrayListUnmanaged(Value),

            /// [[Capability]]
            capability: PromiseCapability,

            /// [[RemainingElements]]
            remaining_elements: *RemainingElements,
        };

        // e. Let stepsRejected be the algorithm steps defined in Promise.any Reject Element
        //    Functions.
        const steps_rejected = struct {
            /// 27.2.4.3.2 Promise.any Reject Element Functions
            /// https://tc39.es/ecma262/#sec-promise.any-reject-element-functions
            fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
                const x = arguments_.get(0);

                // 1. Let F be the active function object.
                const function = agent_.activeFunctionObject();

                const additional_fields_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

                // 2. If F.[[AlreadyCalled]] is true, return undefined.
                if (additional_fields_.already_called) return .undefined;

                // 3. Set F.[[AlreadyCalled]] to true.
                additional_fields_.already_called = true;

                // 4. Let index be F.[[Index]].
                const index_ = additional_fields_.index;

                // 5. Let errors be F.[[Errors]].
                const errors_ = additional_fields_.errors;

                // 6. Let promiseCapability be F.[[Capability]].
                const promise_capability = additional_fields_.capability;

                // 7. Let remainingElementsCount be F.[[RemainingElements]].
                const remaining_elements_count_ = additional_fields_.remaining_elements;

                // 8. Set errors[index] to x.
                errors_.items[index_] = x;

                // 9. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] - 1.
                remaining_elements_count_.value -= 1;

                // 10. If remainingElementsCount.[[Value]] = 0, then
                if (remaining_elements_count_.value == 0) {
                    // a. Let error be a newly created AggregateError object.
                    const error_ = try agent_.createErrorObject(
                        .aggregate_error,
                        "All promises were rejected",
                        .{},
                    );

                    // b. Perform ! DefinePropertyOrThrow(error, "errors", PropertyDescriptor {
                    //      [[Configurable]]: true, [[Enumerable]]: false, [[Writable]]: true,
                    //      [[Value]]: CreateArrayFromList(errors)
                    //    }).
                    try error_.definePropertyDirect(agent_, PropertyKey.from("errors"), .{
                        .value_or_accessor = .{
                            .value = Value.from(try createArrayFromList(agent_, errors_.items)),
                        },
                        .attributes = .builtin_default,
                    });

                    // c. Return ? Call(promiseCapability.[[Reject]], undefined, « error »).
                    return Value.from(promise_capability.reject).callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{Value.from(error_)},
                    );
                }

                // 11. Return undefined.
                return .undefined;
            }
        }.func;

        // f. Let lengthRejected be the number of non-optional parameters of the function
        //    definition in Promise.any Reject Element Functions.
        // g. Let onRejected be CreateBuiltinFunction(stepsRejected, lengthRejected, "",
        //    « [[AlreadyCalled]], [[Index]], [[Errors]], [[Capability]], [[RemainingElements]] »).
        const additional_fields = try agent.gc_allocator.create(AdditionalFields);
        const on_rejected = try createBuiltinFunction(
            agent,
            .{ .function = steps_rejected },
            1,
            "",
            .{ .additional_fields = .make(*AdditionalFields, additional_fields) },
        );

        additional_fields.* = .{
            // h. Set onRejected.[[AlreadyCalled]] to false.
            .already_called = false,

            // i. Set onRejected.[[Index]] to index.
            .index = index,

            // j. Set onRejected.[[Errors]] to errors.
            .errors = errors,

            // k. Set onRejected.[[Capability]] to resultCapability.
            .capability = result_capability,

            // l. Set onRejected.[[RemainingElements]] to remainingElementsCount.
            .remaining_elements = remaining_elements_count,
        };

        // m. Set remainingElementsCount.[[Value]] to remainingElementsCount.[[Value]] + 1.
        remaining_elements_count.value += 1;

        // n. Perform ? Invoke(nextPromise, "then", « resultCapability.[[Resolve]], onRejected »).
        _ = try next_promise.invoke(
            agent,
            PropertyKey.from("then"),
            &.{ Value.from(result_capability.resolve), Value.from(on_rejected) },
        );

        // o. Set index to index + 1.
        index += 1;
    }
}

/// 27.2.4.5.1 PerformPromiseRace ( iteratorRecord, constructor, resultCapability, promiseResolve )
/// https://tc39.es/ecma262/#sec-performpromiserace
fn performPromiseRace(
    agent: *Agent,
    iterator: *Iterator,
    constructor_: *Object,
    result_capability: PromiseCapability,
    promise_resolve: *Object,
) Agent.Error!Value {
    // 1. Repeat,
    while (true) {
        // a. Let next be ? IteratorStepValue(iteratorRecord).
        // b. If next is done, then
        const next = try iterator.stepValue(agent) orelse {
            // i. Return resultCapability.[[Promise]].
            return Value.from(result_capability.promise);
        };

        // c. Let nextPromise be ? Call(promiseResolve, constructor, « next »).
        const next_promise = try Value.from(promise_resolve).callAssumeCallable(
            agent,
            Value.from(constructor_),
            &.{next},
        );

        // d. Perform ? Invoke(nextPromise, "then", « resultCapability.[[Resolve]],
        //    resultCapability.[[Reject]] »).
        _ = try next_promise.invoke(
            agent,
            PropertyKey.from("then"),
            &.{ Value.from(result_capability.resolve), Value.from(result_capability.reject) },
        );
    }
}

/// 27.2.5.4.1 PerformPromiseThen ( promise, onFulfilled, onRejected [ , resultCapability ] )
/// https://tc39.es/ecma262/#sec-performpromisethen
pub fn performPromiseThen(
    agent: *Agent,
    promise: *Promise,
    on_fulfilled: Value,
    on_rejected: Value,
    result_capability: ?PromiseCapability,
) std.mem.Allocator.Error!?*Object {
    // 1. Assert: IsPromise(promise) is true.
    // 2. If resultCapability is not present, then
    //     a. Set resultCapability to undefined.
    // NOTE: These are enforced through the parameter types.

    // 3. If IsCallable(onFulfilled) is false, then
    const on_fulfilled_job_callback = if (!on_fulfilled.isCallable()) blk: {
        // a. Let onFulfilledJobCallback be empty.
        break :blk null;
    } else blk: {
        // 4. Else,
        // a. Let onFulfilledJobCallback be HostMakeJobCallback(onFulfilled).
        break :blk agent.host_hooks.hostMakeJobCallback(on_fulfilled.asObject());
    };

    // 5. If IsCallable(onRejected) is false, then
    const on_rejected_job_callback = if (!on_rejected.isCallable()) blk: {
        // a. Let onRejectedJobCallback be empty.
        break :blk null;
    } else blk: {
        // 6. Else,
        // a. Let onRejectedJobCallback be HostMakeJobCallback(onRejected).
        break :blk agent.host_hooks.hostMakeJobCallback(on_rejected.asObject());
    };

    // 7. Let fulfillReaction be the PromiseReaction Record {
    //      [[Capability]]: resultCapability, [[Type]]: fulfill, [[Handler]]: onFulfilledJobCallback
    //    }.
    const fulfill_reaction: PromiseReaction = .{
        .capability = result_capability,
        .type = .fulfill,
        .handler = on_fulfilled_job_callback,
    };

    // 8. Let rejectReaction be the PromiseReaction Record {
    //      [[Capability]]: resultCapability, [[Type]]: reject, [[Handler]]: onRejectedJobCallback
    //    }.
    const reject_reaction: PromiseReaction = .{
        .capability = result_capability,
        .type = .reject,
        .handler = on_rejected_job_callback,
    };

    switch (promise.fields.promise_state) {
        // 9. If promise.[[PromiseState]] is pending, then
        .pending => {
            // a. Append fulfillReaction to promise.[[PromiseFulfillReactions]].
            try promise.fields.promise_fulfill_reactions.append(
                agent.gc_allocator,
                fulfill_reaction,
            );

            // b. Append rejectReaction to promise.[[PromiseRejectReactions]].
            try promise.fields.promise_reject_reactions.append(
                agent.gc_allocator,
                reject_reaction,
            );
        },

        // 10. Else if promise.[[PromiseState]] is fulfilled, then
        .fulfilled => {
            // a. Let value be promise.[[PromiseResult]].
            const value = promise.fields.promise_result;

            // b. Let fulfillJob be NewPromiseReactionJob(fulfillReaction, value).
            const fulfill_job = try newPromiseReactionJob(agent, fulfill_reaction, value);

            // c. Perform HostEnqueuePromiseJob(fulfillJob.[[Job]], fulfillJob.[[Realm]]).
            try agent.host_hooks.hostEnqueuePromiseJob(agent, fulfill_job.job, fulfill_job.realm);
        },

        // 11. Else,
        //     a. Assert: The value of promise.[[PromiseState]] is rejected.
        .rejected => {
            // b. Let reason be promise.[[PromiseResult]].
            const reason = promise.fields.promise_result;

            // c. If promise.[[PromiseIsHandled]] is false, perform HostPromiseRejectionTracker(promise, "handle").
            if (!promise.fields.promise_is_handled) {
                agent.host_hooks.hostPromiseRejectionTracker(agent, promise, .handle);
            }

            // d. Let rejectJob be NewPromiseReactionJob(rejectReaction, reason).
            const reject_job = try newPromiseReactionJob(agent, reject_reaction, reason);

            // e. Perform HostEnqueuePromiseJob(rejectJob.[[Job]], rejectJob.[[Realm]]).
            try agent.host_hooks.hostEnqueuePromiseJob(agent, reject_job.job, reject_job.realm);
        },
    }

    // 12. Set promise.[[PromiseIsHandled]] to true.
    promise.fields.promise_is_handled = true;

    // 13. If resultCapability is undefined, then
    if (result_capability == null) {
        // a. Return undefined.
        return null;
    } else {
        // 14. Else,
        // a. Return resultCapability.[[Promise]].
        return result_capability.?.promise;
    }
}

/// 27.2.4 Properties of the Promise Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-promise-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Promise",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "all", all, 1, realm);
        try object.defineBuiltinFunction(agent, "allSettled", allSettled, 1, realm);
        try object.defineBuiltinFunction(agent, "any", any, 1, realm);
        try object.defineBuiltinFunction(agent, "race", race, 1, realm);
        try object.defineBuiltinFunction(agent, "reject", reject, 1, realm);
        try object.defineBuiltinFunction(agent, "resolve", resolve, 1, realm);
        try object.defineBuiltinFunction(agent, "try", @"try", 1, realm);
        try object.defineBuiltinFunction(agent, "withResolvers", withResolvers, 0, realm);
        try object.defineBuiltinAccessor(agent, "%Symbol.species%", @"%Symbol.species%", null, realm);

        // 27.2.4.4 Promise.prototype
        // https://tc39.es/ecma262/#sec-promise.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Promise.prototype%"()),
            .none,
        );
    }

    /// 27.2.3.1 Promise ( executor )
    /// https://tc39.es/ecma262/#sec-promise-executor
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const executor = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "Promise must be constructed with 'new'",
                .{},
            );
        }

        // 2. If IsCallable(executor) is false, throw a TypeError exception.
        if (!executor.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{executor});
        }

        // 3. Let promise be ? OrdinaryCreateFromConstructor(NewTarget, "%Promise.prototype%",
        //    « [[PromiseState]], [[PromiseResult]], [[PromiseFulfillReactions]],
        //    [[PromiseRejectReactions]], [[PromiseIsHandled]] »).
        const promise = try ordinaryCreateFromConstructor(
            Promise,
            agent,
            new_target.?,
            "%Promise.prototype%",
            .{
                // 4. Set promise.[[PromiseState]] to pending.
                .promise_state = .pending,

                // 5. Set promise.[[PromiseResult]] to empty.
                .promise_result = undefined,

                // 6. Set promise.[[PromiseFulfillReactions]] to a new empty List.
                .promise_fulfill_reactions = .empty,

                // 7. Set promise.[[PromiseRejectReactions]] to a new empty List.
                .promise_reject_reactions = .empty,

                // 8. Set promise.[[PromiseIsHandled]] to false.
                .promise_is_handled = false,
            },
        );

        // 9. Let resolvingFunctions be CreateResolvingFunctions(promise).
        const resolving_functions = try createResolvingFunctions(agent, promise.as(Promise));

        // 10. Let completion be Completion(Call(executor, undefined, « resolvingFunctions.[[Resolve]],
        //     resolvingFunctions.[[Reject]] »)).
        _ = executor.callAssumeCallable(
            agent,
            .undefined,
            &.{ Value.from(resolving_functions.resolve), Value.from(resolving_functions.reject) },
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,

            // 11. If completion is an abrupt completion, then
            error.ExceptionThrown => {
                const exception = agent.clearException();

                // a. Perform ? Call(resolvingFunctions.[[Reject]], undefined, « completion.[[Value]] »).
                _ = try Value.from(resolving_functions.reject).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{exception.value},
                );
            },
        };

        // 12. Return promise.
        return Value.from(promise);
    }

    /// 27.2.4.1 Promise.all ( iterable )
    /// https://tc39.es/ecma262/#sec-promise.all
    fn all(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const iterable = arguments.get(0);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 3. Let promiseResolve be Completion(GetPromiseResolve(C)).
        const promise_resolve = getPromiseResolve(agent, constructor_.asObject()) catch |err| {
            // 4. IfAbruptRejectPromise(promiseResolve, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 5. Let iteratorRecord be Completion(GetIterator(iterable, sync)).
        var iterator = getIterator(agent, iterable, .sync) catch |err| {
            // 6. IfAbruptRejectPromise(iteratorRecord, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 7. Let result be Completion(PerformPromiseAll(iteratorRecord, C, promiseCapability, promiseResolve)).
        var result = performPromiseAll(
            agent,
            &iterator,
            constructor_.asObject(),
            promise_capability,
            promise_resolve,
        );

        // 8. If result is an abrupt completion, then
        if (std.meta.isError(result)) {
            // a. If iteratorRecord.[[Done]] is false, set result to Completion(IteratorClose(iteratorRecord, result)).
            if (!iterator.done) result = iterator.close(agent, result);

            // b. IfAbruptRejectPromise(result, promiseCapability).
            if (result) |_| {} else |err| {
                return Value.from(try promise_capability.rejectPromise(agent, err));
            }
        }

        // 9. Return ! result.
        return result;
    }

    /// 27.2.4.2 Promise.allSettled ( iterable )
    /// https://tc39.es/ecma262/#sec-promise.allsettled
    fn allSettled(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const iterable = arguments.get(0);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 3. Let promiseResolve be Completion(GetPromiseResolve(C)).
        const promise_resolve = getPromiseResolve(agent, constructor_.asObject()) catch |err| {
            // 4. IfAbruptRejectPromise(promiseResolve, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 5. Let iteratorRecord be Completion(GetIterator(iterable, sync)).
        var iterator = getIterator(agent, iterable, .sync) catch |err| {
            // 6. IfAbruptRejectPromise(iteratorRecord, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 7. Let result be Completion(PerformPromiseAllSettled(iteratorRecord, C, promiseCapability, promiseResolve)).
        var result = performPromiseAllSettled(
            agent,
            &iterator,
            constructor_.asObject(),
            promise_capability,
            promise_resolve,
        );

        // 8. If result is an abrupt completion, then
        if (std.meta.isError(result)) {
            // a. If iteratorRecord.[[Done]] is false, set result to Completion(IteratorClose(iteratorRecord, result)).
            if (!iterator.done) result = iterator.close(agent, result);

            // b. IfAbruptRejectPromise(result, promiseCapability).
            if (result) |_| {} else |err| {
                return Value.from(try promise_capability.rejectPromise(agent, err));
            }
        }

        // 9. Return ! result.
        return result;
    }

    /// 27.2.4.3 Promise.any ( iterable )
    /// https://tc39.es/ecma262/#sec-promise.
    fn any(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const iterable = arguments.get(0);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 3. Let promiseResolve be Completion(GetPromiseResolve(C)).
        const promise_resolve = getPromiseResolve(agent, constructor_.asObject()) catch |err| {
            // 4. IfAbruptRejectPromise(promiseResolve, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 5. Let iteratorRecord be Completion(GetIterator(iterable, sync)).
        var iterator = getIterator(agent, iterable, .sync) catch |err| {
            // 6. IfAbruptRejectPromise(iteratorRecord, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 7. Let result be Completion(PerformPromiseAny(iteratorRecord, C, promiseCapability, promiseResolve)).
        var result = performPromiseAny(
            agent,
            &iterator,
            constructor_.asObject(),
            promise_capability,
            promise_resolve,
        );

        // 8. If result is an abrupt completion, then
        if (std.meta.isError(result)) {
            // a. If iteratorRecord.[[Done]] is false, set result to Completion(IteratorClose(iteratorRecord, result)).
            if (!iterator.done) result = iterator.close(agent, result);

            // b. IfAbruptRejectPromise(result, promiseCapability).
            if (result) |_| {} else |err| {
                return Value.from(try promise_capability.rejectPromise(agent, err));
            }
        }

        // 9. Return ! result.
        return result;
    }

    /// 27.2.4.5 Promise.race ( iterable )
    /// https://tc39.es/ecma262/#sec-promise.race
    fn race(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const iterable = arguments.get(0);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 3. Let promiseResolve be Completion(GetPromiseResolve(C)).
        const promise_resolve = getPromiseResolve(agent, constructor_.asObject()) catch |err| {
            // 4. IfAbruptRejectPromise(promiseResolve, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 5. Let iteratorRecord be Completion(GetIterator(iterable, sync)).
        var iterator = getIterator(agent, iterable, .sync) catch |err| {
            // 6. IfAbruptRejectPromise(iteratorRecord, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 7. Let result be Completion(PerformPromiseRace(iteratorRecord, C, promiseCapability, promiseResolve)).
        var result = performPromiseRace(
            agent,
            &iterator,
            constructor_.asObject(),
            promise_capability,
            promise_resolve,
        );

        // 8. If result is an abrupt completion, then
        if (std.meta.isError(result)) {
            // a. If iteratorRecord.[[Done]] is false, set result to Completion(IteratorClose(iteratorRecord, result)).
            if (!iterator.done) result = iterator.close(agent, result);

            // b. IfAbruptRejectPromise(result, promiseCapability).
            if (result) |_| {} else |err| {
                return Value.from(try promise_capability.rejectPromise(agent, err));
            }
        }

        // 9. Return ! result.
        return result;
    }

    /// 27.2.4.6 Promise.reject ( r )
    /// https://tc39.es/ecma262/#sec-promise.reject
    fn reject(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const reason = arguments.get(0);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 3. Perform ? Call(promiseCapability.[[Reject]], undefined, « r »).
        _ = try Value.from(promise_capability.reject).call(agent, .undefined, &.{reason});

        // 4. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    /// 27.2.4.7 Promise.resolve ( x )
    /// https://tc39.es/ecma262/#sec-promise.resolve
    fn resolve(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const resolution = arguments.get(0);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. If C is not an Object, throw a TypeError exception.
        if (!constructor_.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{constructor_});
        }

        // 3. Return ? PromiseResolve(C, x).
        return Value.from(try promiseResolve(agent, constructor_.asObject(), resolution));
    }

    /// 27.2.4.8 Promise.try ( callback, ...args )
    /// https://tc39.es/ecma262/#sec-promise.try
    fn @"try"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const args = if (arguments.count() <= 1) &[_]Value{} else arguments.values[1..];

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. If C is not an Object, throw a TypeError exception.
        if (!constructor_.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{constructor_});
        }

        // 3. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 4. Let status be Completion(Call(callback, undefined, args)).
        const status = callback.call(agent, .undefined, args);

        // 5. If status is an abrupt completion, then
        //     a. Perform ? Call(promiseCapability.[[Reject]], undefined, « status.[[Value]] »).
        // 6. Else,
        //     a. Perform ? Call(promiseCapability.[[Resolve]], undefined, « status.[[Value]] »).
        if (status) |value| {
            _ = try Value.from(promise_capability.resolve).callAssumeCallable(
                agent,
                .undefined,
                &.{value},
            );
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ExceptionThrown => {
                const exception = agent.clearException();
                _ = try Value.from(promise_capability.reject).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{exception.value},
                );
            },
        }

        // 7. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    /// 27.2.4.9 Promise.withResolvers ( )
    /// https://tc39.es/ecma262/#sec-promise.withResolvers
    fn withResolvers(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor_);

        // 3. Let obj be OrdinaryObjectCreate(%Object.prototype%).
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. Perform ! CreateDataPropertyOrThrow(obj, "promise", promiseCapability.[[Promise]]).
        try object.createDataPropertyDirect(
            agent,
            PropertyKey.from("promise"),
            Value.from(promise_capability.promise),
        );

        // 5. Perform ! CreateDataPropertyOrThrow(obj, "resolve", promiseCapability.[[Resolve]]).
        try object.createDataPropertyDirect(
            agent,
            PropertyKey.from("resolve"),
            Value.from(promise_capability.resolve),
        );

        // 6. Perform ! CreateDataPropertyOrThrow(obj, "reject", promiseCapability.[[Reject]]).
        try object.createDataPropertyDirect(
            agent,
            PropertyKey.from("reject"),
            Value.from(promise_capability.reject),
        );

        // 7. Return obj.
        return Value.from(object);
    }

    /// 27.2.4.10 get Promise [ %Symbol.species% ]
    /// https://tc39.es/ecma262/#sec-get-promise-%symbol.species%
    fn @"%Symbol.species%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 27.2.5 Properties of the Promise Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-promise-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "catch", @"catch", 1, realm);
        try object.defineBuiltinFunction(agent, "finally", finally, 1, realm);
        try object.defineBuiltinFunction(agent, "then", then, 2, realm);

        // 27.2.5.2 Promise.prototype.constructor
        // https://tc39.es/ecma262/#sec-promise.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Promise%"()),
        );

        // 27.2.5.5 Promise.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-promise.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Promise"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 27.2.5.1 Promise.prototype.catch ( onRejected )
    /// https://tc39.es/ecma262/#sec-promise.prototype.catch
    fn @"catch"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const on_rejected = arguments.get(0);

        // 1. Let promise be the this value.
        const promise = this_value;

        // 2. Return ? Invoke(promise, "then", « undefined, onRejected »).
        return promise.invoke(agent, PropertyKey.from("then"), &.{ .undefined, on_rejected });
    }

    /// 27.2.5.3 Promise.prototype.finally ( onFinally )
    /// https://tc39.es/ecma262/#sec-promise.prototype.finally
    fn finally(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const on_finally = arguments.get(0);

        // 1. Let promise be the this value.
        const promise = this_value;

        // 2. If promise is not an Object, throw a TypeError exception.
        if (!promise.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{promise});
        }

        // 3. Let C be ? SpeciesConstructor(promise, %Promise%).
        const constructor_ = try promise.asObject().speciesConstructor(
            agent,
            try realm.intrinsics.@"%Promise%"(),
        );

        // 4. Assert: IsConstructor(C) is true.
        std.debug.assert(Value.from(constructor_).isConstructor());

        var then_finally: Value = undefined;
        var catch_finally: Value = undefined;

        // 5. If IsCallable(onFinally) is false, then
        if (!on_finally.isCallable()) {
            // a. Let thenFinally be onFinally.
            then_finally = on_finally;

            // b. Let catchFinally be onFinally.
            catch_finally = on_finally;
        } else {
            // 6. Else,
            const Captures = struct {
                on_finally: Value,
                constructor: *Object,
            };
            const captures = try agent.gc_allocator.create(Captures);
            captures.* = .{ .on_finally = on_finally, .constructor = constructor_ };

            // a. Let thenFinallyClosure be a new Abstract Closure with parameters (value) that
            //    captures onFinally and C and performs the following steps when called:
            const then_finally_closure = struct {
                fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
                    const function = agent_.activeFunctionObject();
                    const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
                    const on_finally_ = captures_.on_finally;
                    const constructor__ = captures_.constructor;
                    const value = arguments_.get(0);

                    // i. Let result be ? Call(onFinally, undefined).
                    const result = try on_finally_.callAssumeCallable(agent_, .undefined, &.{});

                    // ii. Let p be ? PromiseResolve(C, result).
                    const new_promise = try promiseResolve(agent_, constructor__, result);

                    const value_capture = try agent_.gc_allocator.create(Value);
                    value_capture.* = value;

                    // iii. Let returnValue be a new Abstract Closure with no parameters that captures
                    //      value and performs the following steps when called:
                    const return_value = struct {
                        fn func(agent__: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                            const function_ = agent__.activeFunctionObject();
                            const value_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Value).*;

                            // 1. Return value.
                            return value_;
                        }
                    }.func;

                    // iv. Let valueThunk be CreateBuiltinFunction(returnValue, 0, "", « »).
                    const value_thunk = try createBuiltinFunction(
                        agent_,
                        .{ .function = return_value },
                        0,
                        "",
                        .{ .additional_fields = .make(*Value, value_capture) },
                    );

                    // v. Return ? Invoke(p, "then", « valueThunk »).
                    return Value.from(new_promise).invoke(
                        agent_,
                        PropertyKey.from("then"),
                        &.{Value.from(value_thunk)},
                    );
                }
            }.func;

            // b. Let thenFinally be CreateBuiltinFunction(thenFinallyClosure, 1, "", « »).
            then_finally = Value.from(
                try createBuiltinFunction(
                    agent,
                    .{ .function = then_finally_closure },
                    1,
                    "",
                    .{ .additional_fields = .make(*Captures, captures) },
                ),
            );

            // c. Let catchFinallyClosure be a new Abstract Closure with parameters (reason) that
            //    captures onFinally and C and performs the following steps when called:
            const catch_finally_closure = struct {
                fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
                    const function = agent_.activeFunctionObject();
                    const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
                    const on_finally_ = captures_.on_finally;
                    const constructor__ = captures_.constructor;
                    const reason = arguments_.get(0);

                    // i. Let result be ? Call(onFinally, undefined).
                    const result = try on_finally_.callAssumeCallable(agent_, .undefined, &.{});

                    // ii. Let p be ? PromiseResolve(C, result).
                    const new_promise = try promiseResolve(agent_, constructor__, result);

                    const reason_capture = try agent_.gc_allocator.create(Value);
                    reason_capture.* = reason;

                    // iii. Let throwReason be a new Abstract Closure with no parameters that captures
                    //      reason and performs the following steps when called:
                    const throw_reason = struct {
                        fn func(agent__: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                            const function_ = agent__.activeFunctionObject();
                            const reason_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Value).*;

                            // 1. Return ThrowCompletion(reason).
                            agent__.exception = .{
                                .value = reason_,
                                .stack_trace = try agent__.captureStackTrace(),
                            };
                            return error.ExceptionThrown;
                        }
                    }.func;

                    // iv. Let thrower be CreateBuiltinFunction(throwReason, 0, "", « »).
                    const thrower = try createBuiltinFunction(
                        agent_,
                        .{ .function = throw_reason },
                        0,
                        "",
                        .{ .additional_fields = .make(*Value, reason_capture) },
                    );

                    // v. Return ? Invoke(p, "then", « thrower »).
                    return Value.from(new_promise).invoke(
                        agent_,
                        PropertyKey.from("then"),
                        &.{Value.from(thrower)},
                    );
                }
            }.func;

            // d. Let catchFinally be CreateBuiltinFunction(catchFinallyClosure, 1, "", « »).
            catch_finally = Value.from(
                try createBuiltinFunction(
                    agent,
                    .{ .function = catch_finally_closure },
                    1,
                    "",
                    .{ .additional_fields = .make(*Captures, captures) },
                ),
            );
        }

        // 7. Return ? Invoke(promise, "then", « thenFinally, catchFinally »).
        return promise.invoke(agent, PropertyKey.from("then"), &.{ then_finally, catch_finally });
    }

    /// 27.2.5.4 Promise.prototype.then ( onFulfilled, onRejected )
    /// https://tc39.es/ecma262/#sec-promise.prototype.then
    fn then(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const on_fulfilled = arguments.get(0);
        const on_rejected = arguments.get(1);

        // 1. Let promise be the this value.
        const promise = this_value;

        // 2. If IsPromise(promise) is false, throw a TypeError exception.
        if (!promise.isPromise()) {
            return agent.throwException(.type_error, "{} is not a Promise", .{promise});
        }

        // 3. Let C be ? SpeciesConstructor(promise, %Promise%).
        const constructor_ = try promise.asObject().speciesConstructor(
            agent,
            try realm.intrinsics.@"%Promise%"(),
        );

        // 4. Let resultCapability be ? NewPromiseCapability(C).
        const result_capability = try newPromiseCapability(agent, Value.from(constructor_));

        // 5. Return PerformPromiseThen(promise, onFulfilled, onRejected, resultCapability).
        return Value.from(
            try performPromiseThen(
                agent,
                promise.asObject().as(Promise),
                on_fulfilled,
                on_rejected,
                result_capability,
            ) orelse return .undefined,
        );
    }
};

/// 27.2.6 Properties of Promise Instances
/// https://tc39.es/ecma262/#sec-properties-of-promise-instances
pub const Promise = MakeObject(.{
    .Fields = struct {
        /// [[PromiseState]]
        promise_state: enum {
            pending,
            fulfilled,
            rejected,
        },

        /// [[PromiseResult]]
        promise_result: Value,

        /// [[PromiseFulfillReactions]]
        promise_fulfill_reactions: std.ArrayListUnmanaged(PromiseReaction),

        /// [[PromiseRejectReactions]]
        promise_reject_reactions: std.ArrayListUnmanaged(PromiseReaction),

        /// [[PromiseIsHandled]]
        promise_is_handled: bool,
    },
    .tag = .promise,
});
