//! 27.2 Promise Objects
//! https://tc39.es/ecma262/#sec-promise-objects

const std = @import("std");

const SafePointer = @import("any-pointer").SafePointer;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

/// 27.2.1.1 PromiseCapability Records
/// https://tc39.es/ecma262/#sec-promisecapability-records
const PromiseCapability = struct {
    /// [[Promise]]
    promise: Object,

    /// [[Resolve]]
    resolve: Object,

    /// [[Reject]]
    reject: Object,
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

// TODO: Implement JobCallback records
const JobCallback = struct {};

const ResolvingFunctions = struct {
    resolve: Object,
    reject: Object,
};

/// 27.2.1.3 CreateResolvingFunctions ( promise )
/// https://tc39.es/ecma262/#sec-createresolvingfunctions
pub fn createResolvingFunctions(agent: *Agent, promise: *Promise) !ResolvingFunctions {
    const AlreadyResolved = struct { value: bool };
    const AdditionalFields = struct {
        promise: *Promise,
        already_resolved: AlreadyResolved,
    };

    // 1. Let alreadyResolved be the Record { [[Value]]: false }.
    const already_resolved = AlreadyResolved{ .value = false };

    // 2. Let stepsResolve be the algorithm steps defined in Promise Resolve Functions.
    const steps_resolve = struct {
        /// 27.2.1.3.2 Promise Resolve Functions
        /// https://tc39.es/ecma262/#sec-promise-resolve-functions
        fn func(agent_: *Agent, _: Value, arguments: ArgumentsList) !Value {
            const resolution = arguments.get(0);

            // 1. Let F be the active function object.
            const function = agent_.activeFunctionObject();

            // 2. Assert: F has a [[Promise]] internal slot whose value is an Object.
            const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

            // 3. Let promise be F.[[Promise]].
            const promise_ = additional_fields.promise;

            // 4. Let alreadyResolved be F.[[AlreadyResolved]].
            const already_resolved_ = &additional_fields.already_resolved;

            // 5. If alreadyResolved.[[Value]] is true, return undefined.
            if (already_resolved_.value) return .undefined;

            // 6. Set alreadyResolved.[[Value]] to true.
            already_resolved_.value = true;

            // 7. If SameValue(resolution, promise) is true, then
            if (sameValue(resolution, Value.from(promise_.object()))) {
                // a. Let selfResolutionError be a newly created TypeError object.
                // FIXME: This is awkward :)
                agent_.throwException(.type_error, "Cannot resolve promise with itself") catch {};
                const self_resolution_error = agent_.exception.?;
                agent_.exception = null;

                // b. Perform RejectPromise(promise, selfResolutionError).
                rejectPromise(promise_, self_resolution_error);

                // c. Return undefined.
                return .undefined;
            }

            // 8. If resolution is not an Object, then
            if (resolution != .object) {
                // a. Perform FulfillPromise(promise, resolution).
                fulfillPromise(promise_, resolution);

                // b. Return undefined.
                return .undefined;
            }

            // 9. Let then be Completion(Get(resolution, "then")).
            // 11. Let thenAction be then.[[Value]].
            const then_action = resolution.object.get(PropertyKey.from("then")) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                // 10. If then is an abrupt completion, then
                error.ExceptionThrown => {
                    // a. Perform RejectPromise(promise, then.[[Value]]).
                    rejectPromise(promise_, agent_.exception.?);
                    agent_.exception = null;

                    // b. Return undefined.
                    return .undefined;
                },
            };

            // 12. If IsCallable(thenAction) is false, then
            if (!then_action.isCallable()) {
                // a. Perform FulfillPromise(promise, resolution).
                fulfillPromise(promise_, resolution);

                // b. Return undefined.
                return .undefined;
            }

            // TODO: 13. Let thenJobCallback be HostMakeJobCallback(thenAction).
            // TODO: 14. Let job be NewPromiseResolveThenableJob(promise, resolution, thenJobCallback).
            // TODO: 15. Perform HostEnqueuePromiseJob(job.[[Job]], job.[[Realm]]).

            // 16. Return undefined.
            return .undefined;
        }
    }.func;

    // 3. Let lengthResolve be the number of non-optional parameters of the function definition in
    //    Promise Resolve Functions.
    const length_resolve = 1;

    // 4. Let resolve be CreateBuiltinFunction(stepsResolve, lengthResolve, "", « [[Promise]],
    //    [[AlreadyResolved]] »).
    const resolve_additional_fields = try agent.gc_allocator.create(AdditionalFields);
    const resolve = try createBuiltinFunction(agent, .{ .regular = steps_resolve }, .{
        .length = length_resolve,
        .name = "",
        .additional_fields = SafePointer.make(*AdditionalFields, resolve_additional_fields),
    });

    resolve_additional_fields.* = .{
        // 5. Set resolve.[[Promise]] to promise.
        .promise = promise,

        // 6. Set resolve.[[AlreadyResolved]] to alreadyResolved.
        .already_resolved = already_resolved,
    };

    // 7. Let stepsReject be the algorithm steps defined in Promise Reject Functions.
    const steps_reject = struct {
        /// 27.2.1.3.1 Promise Reject Functions
        /// https://tc39.es/ecma262/#sec-promise-reject-functions
        fn func(agent_: *Agent, _: Value, arguments: ArgumentsList) !Value {
            const reason = arguments.get(0);

            // 1. Let F be the active function object.
            const function = agent_.activeFunctionObject();

            // 2. Assert: F has a [[Promise]] internal slot whose value is an Object.
            const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);

            // 3. Let promise be F.[[Promise]].
            const promise_ = additional_fields.promise;

            // 4. Let alreadyResolved be F.[[AlreadyResolved]].
            const already_resolved_ = &additional_fields.already_resolved;

            // 5. If alreadyResolved.[[Value]] is true, return undefined.
            if (already_resolved_.value) return .undefined;

            // 6. Set alreadyResolved.[[Value]] to true.
            already_resolved_.value = true;

            // 7. Perform RejectPromise(promise, reason).
            rejectPromise(promise_, reason);

            // 8. Return undefined.
            return .undefined;
        }
    }.func;

    // 8. Let lengthReject be the number of non-optional parameters of the function definition in
    //    Promise Reject Functions.
    const length_reject = 1;

    // 9. Let reject be CreateBuiltinFunction(stepsReject, lengthReject, "", « [[Promise]],
    //    [[AlreadyResolved]] »).
    const reject_additional_fields = try agent.gc_allocator.create(AdditionalFields);
    const reject = try createBuiltinFunction(agent, .{ .regular = steps_reject }, .{
        .length = length_reject,
        .name = "",
        .additional_fields = SafePointer.make(*AdditionalFields, reject_additional_fields),
    });

    reject_additional_fields.* = .{
        // 10. Set reject.[[Promise]] to promise.
        .promise = promise,

        // 11. Set reject.[[AlreadyResolved]] to alreadyResolved.
        .already_resolved = already_resolved,
    };

    // 12. Return the Record { [[Resolve]]: resolve, [[Reject]]: reject }.
    return .{ .resolve = resolve, .reject = reject };
}

/// 27.2.1.4 FulfillPromise ( promise, value )
/// https://tc39.es/ecma262/#sec-fulfillpromise
pub fn fulfillPromise(promise: *Promise, value: Value) void {
    // 1. Assert: The value of promise.[[PromiseState]] is pending.
    std.debug.assert(promise.fields.promise_state == .pending);

    // 2. Let reactions be promise.[[PromiseFulfillReactions]].
    const reactions = promise.fields.promise_fulfill_reactions;

    // 3. Set promise.[[PromiseResult]] to value.
    promise.fields.promise_result = value;

    // 4. Set promise.[[PromiseFulfillReactions]] to undefined.
    defer promise.fields.promise_fulfill_reactions.deinit();

    // 5. Set promise.[[PromiseRejectReactions]] to undefined.
    defer promise.fields.promise_reject_reactions.deinit();

    // 6. Set promise.[[PromiseState]] to fulfilled.
    promise.fields.promise_state = .fulfilled;

    // TODO: 7. Perform TriggerPromiseReactions(reactions, value).
    _ = reactions;

    // 8. Return unused.
}

/// 27.2.1.5 NewPromiseCapability ( C )
/// https://tc39.es/ecma262/#sec-newpromisecapability
pub fn newPromiseCapability(agent: *Agent, constructor: Value) !PromiseCapability {
    // 1. If IsConstructor(C) is false, throw a TypeError exception.
    if (!constructor.isConstructor()) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not a constructor", .{constructor}),
        );
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
        fn func(agent_: *Agent, _: Value, arguments: ArgumentsList) !Value {
            const resolve = arguments.get(0);
            const reject = arguments.get(1);
            const function = agent_.activeFunctionObject();
            const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);
            const resolving_functions_ = &additional_fields.resolving_functions;

            // a. If resolvingFunctions.[[Resolve]] is not undefined, throw a TypeError exception.
            if (resolving_functions_.resolve != .undefined) {
                return agent_.throwException(.type_error, "Resolve function has already been set");
            }
            // b. If resolvingFunctions.[[Reject]] is not undefined, throw a TypeError exception.
            if (resolving_functions_.reject != .undefined) {
                return agent_.throwException(.type_error, "Reject function has already been set");
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
    const executor = try createBuiltinFunction(agent, .{ .regular = executor_closure }, .{
        .length = 2,
        .name = "",
        .additional_fields = SafePointer.make(*AdditionalFields, additional_fields),
    });

    // NOTE: This struct can outlive the function scope if anything holds on to the callback above.
    additional_fields.* = .{
        .resolving_functions = .{ .resolve = .undefined, .reject = .undefined },
    };
    const resolving_functions = &additional_fields.resolving_functions;

    // 6. Let promise be ? Construct(C, « executor »).
    const promise = try constructor.object.construct(.{Value.from(executor)}, null);

    // 7. If IsCallable(resolvingFunctions.[[Resolve]]) is false, throw a TypeError exception.
    if (!resolving_functions.resolve.isCallable()) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{resolving_functions.resolve}),
        );
    }

    // 8. If IsCallable(resolvingFunctions.[[Reject]]) is false, throw a TypeError exception.
    if (!resolving_functions.reject.isCallable()) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{resolving_functions.reject}),
        );
    }

    // 9. Return the PromiseCapability Record { [[Promise]]: promise, [[Resolve]]:
    //    resolvingFunctions.[[Resolve]], [[Reject]]: resolvingFunctions.[[Reject]] }.
    return .{
        .promise = promise,
        .resolve = resolving_functions.resolve.object,
        .reject = resolving_functions.reject.object,
    };
}

/// 27.2.1.7 RejectPromise ( promise, reason )
/// https://tc39.es/ecma262/#sec-rejectpromise
pub fn rejectPromise(promise: *Promise, reason: Value) void {
    // 1. Assert: The value of promise.[[PromiseState]] is pending.
    std.debug.assert(promise.fields.promise_state == .pending);

    // 2. Let reactions be promise.[[PromiseRejectReactions]].
    const reactions = promise.fields.promise_reject_reactions;

    // 3. Set promise.[[PromiseResult]] to reason.
    promise.fields.promise_result = reason;

    // 4. Set promise.[[PromiseFulfillReactions]] to undefined.
    defer promise.fields.promise_fulfill_reactions.deinit();

    // 5. Set promise.[[PromiseRejectReactions]] to undefined.
    defer promise.fields.promise_reject_reactions.deinit();

    // 6. Set promise.[[PromiseState]] to rejected.
    promise.fields.promise_state = .rejected;

    // TODO: 7. If promise.[[PromiseIsHandled]] is false, perform HostPromiseRejectionTracker(promise, "reject").
    // TODO: 8. Perform TriggerPromiseReactions(reactions, reason).
    _ = reactions;

    // 9. Return unused.
}

/// 27.2.4.7.1 PromiseResolve ( C, x )
/// https://tc39.es/ecma262/#sec-promise-resolve
pub fn promiseResolve(agent: *Agent, constructor: Object, x: Value) !Object {
    // 1. If IsPromise(x) is true, then
    if (x.isPromise()) {
        // a. Let xConstructor be ? Get(x, "constructor").
        const x_constructor = try x.object.get(PropertyKey.from("constructor"));

        // b. If SameValue(xConstructor, C) is true, return x.
        if (sameValue(x_constructor, Value.from(constructor))) return x.object;
    }

    // 2. Let promiseCapability be ? NewPromiseCapability(C).
    const promise_capability = try newPromiseCapability(agent, Value.from(constructor));

    // 3. Perform ? Call(promiseCapability.[[Resolve]], undefined, « x »).
    _ = try Value.from(promise_capability.resolve).callAssumeCallable(.undefined, .{x});

    // 4. Return promiseCapability.[[Promise]].
    return promise_capability.promise;
}

/// 27.2.5.4.1 PerformPromiseThen ( promise, onFulfilled, onRejected [ , resultCapability ] )
/// https://tc39.es/ecma262/#sec-performpromisethen
pub fn performPromiseThen(
    agent: *Agent,
    promise: *Promise,
    on_fulfilled: Value,
    on_rejected: Value,
    result_capability: ?PromiseCapability,
) !?Object {
    _ = agent;
    // 1. Assert: IsPromise(promise) is true.
    // 2. If resultCapability is not present, then
    //     a. Set resultCapability to undefined.
    // NOTE: These are enforced through the parameter types.

    // 3. If IsCallable(onFulfilled) is false, then
    const on_fulfilled_job_callback = if (!on_fulfilled.isCallable()) blk: {
        // a. Let onFulfilledJobCallback be empty.
        break :blk null;
    }
    // 4. Else,
    else blk: {
        // TODO: a. Let onFulfilledJobCallback be HostMakeJobCallback(onFulfilled).
        break :blk JobCallback{};
    };

    // 5. If IsCallable(onRejected) is false, then
    const on_rejected_job_callback = if (!on_rejected.isCallable()) blk: {
        // a. Let onRejectedJobCallback be empty.
        break :blk null;
    }
    // 6. Else,
    else blk: {
        // TODO: a. Let onRejectedJobCallback be HostMakeJobCallback(onRejected).
        break :blk JobCallback{};
    };

    // 7. Let fulfillReaction be the PromiseReaction {
    //      [[Capability]]: resultCapability, [[Type]]: fulfill, [[Handler]]: onFulfilledJobCallback
    //    }.
    const fulfill_reaction = PromiseReaction{
        .capability = result_capability,
        .type = .fulfill,
        .handler = on_fulfilled_job_callback,
    };

    // 8. Let rejectReaction be the PromiseReaction {
    //      [[Capability]]: resultCapability, [[Type]]: reject, [[Handler]]: onRejectedJobCallback
    //    }.
    const reject_reaction = PromiseReaction{
        .capability = result_capability,
        .type = .reject,
        .handler = on_rejected_job_callback,
    };

    switch (promise.fields.promise_state) {
        // 9. If promise.[[PromiseState]] is pending, then
        .pending => {
            // a. Append fulfillReaction to promise.[[PromiseFulfillReactions]].
            try promise.fields.promise_fulfill_reactions.append(fulfill_reaction);

            // b. Append rejectReaction to promise.[[PromiseRejectReactions]].
            try promise.fields.promise_reject_reactions.append(reject_reaction);
        },

        // 10. Else if promise.[[PromiseState]] is fulfilled, then
        .fulfilled => {
            // a. Let value be promise.[[PromiseResult]].
            const value = promise.fields.promise_result;

            // TODO: b. Let fulfillJob be NewPromiseReactionJob(fulfillReaction, value).
            _ = value;

            // TODO: c. Perform HostEnqueuePromiseJob(fulfillJob.[[Job]], fulfillJob.[[Realm]]).
        },

        // 11. Else,
        //     a. Assert: The value of promise.[[PromiseState]] is rejected.
        .rejected => {
            // b. Let reason be promise.[[PromiseResult]].
            const reason = promise.fields.promise_result;

            // TODO: c. If promise.[[PromiseIsHandled]] is false, perform HostPromiseRejectionTracker(promise, "handle").
            // TODO: d. Let rejectJob be NewPromiseReactionJob(rejectReaction, reason).
            _ = reason;

            // TODO: e. Perform HostEnqueuePromiseJob(rejectJob.[[Job]], rejectJob.[[Realm]]).
        },
    }

    // 12. Set promise.[[PromiseIsHandled]] to true.
    promise.fields.promise_is_handled = true;

    // 13. If resultCapability is undefined, then
    if (result_capability == null) {
        // a. Return undefined.
        return null;
    }
    // 14. Else,
    else {
        // a. Return resultCapability.[[Promise]].
        return result_capability.?.promise;
    }
}

/// 27.2.4 Properties of the Promise Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-promise-constructor
pub const PromiseConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Promise",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "reject", reject, 1, realm);
        try defineBuiltinFunction(object, "resolve", resolve, 1, realm);

        // 27.2.4.4 Promise.prototype
        // https://tc39.es/ecma262/#sec-promise.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Promise.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 27.2.4.8 get Promise [ @@species ]
        // https://tc39.es/ecma262/#sec-get-promise-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        // 27.2.5.2 Promise.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Promise.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 27.2.3.1 Promise ( executor )
    /// https://tc39.es/ecma262/#sec-promise-executor
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const executor = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Promise must be constructed with 'new'");
        }

        // 2. If IsCallable(executor) is false, throw a TypeError exception.
        if (!executor.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{executor}),
            );
        }

        // 3. Let promise be ? OrdinaryCreateFromConstructor(NewTarget, "%Promise.prototype%",
        //    « [[PromiseState]], [[PromiseResult]], [[PromiseFulfillReactions]],
        //    [[PromiseRejectReactions]], [[PromiseIsHandled]] »).
        const promise = try ordinaryCreateFromConstructor(Promise, agent, new_target.?, "%Promise.prototype%");

        promise.as(Promise).fields = .{
            .promise_result = undefined,

            // 4. Set promise.[[PromiseState]] to pending.
            .promise_state = .pending,

            // 5. Set promise.[[PromiseFulfillReactions]] to a new empty List.
            .promise_fulfill_reactions = std.ArrayList(PromiseReaction).init(agent.gc_allocator),

            // 6. Set promise.[[PromiseRejectReactions]] to a new empty List.
            .promise_reject_reactions = std.ArrayList(PromiseReaction).init(agent.gc_allocator),

            // 7. Set promise.[[PromiseIsHandled]] to false.
            .promise_is_handled = false,
        };

        // 8. Let resolvingFunctions be CreateResolvingFunctions(promise).
        const resolving_functions = try createResolvingFunctions(agent, promise.as(Promise));

        // 9. Let completion be Completion(Call(executor, undefined, « resolvingFunctions.[[Resolve]],
        //    resolvingFunctions.[[Reject]] »)).
        _ = executor.callAssumeCallable(
            .undefined,
            .{ Value.from(resolving_functions.resolve), Value.from(resolving_functions.reject) },
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,

            // 10. If completion is an abrupt completion, then
            error.ExceptionThrown => {
                const exception = agent.exception.?;
                agent.exception = null;

                // a. Perform ? Call(resolvingFunctions.[[Reject]], undefined, « completion.[[Value]] »).
                _ = try Value.from(resolving_functions.reject).callAssumeCallable(
                    .undefined,
                    .{exception},
                );
            },
        };

        // 11. Return promise.
        return Value.from(promise);
    }

    /// 27.2.4.6 Promise.reject ( r )
    /// https://tc39.es/ecma262/#sec-promise.reject
    fn reject(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const reason = arguments.get(0);

        // 1. Let C be the this value.
        const constructor = this_value;

        // 2. Let promiseCapability be ? NewPromiseCapability(C).
        const promise_capability = try newPromiseCapability(agent, constructor);

        // 3. Perform ? Call(promiseCapability.[[Reject]], undefined, « r »).
        _ = try Value.from(promise_capability.reject).call(agent, undefined, .{reason});

        // 4. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    /// 27.2.4.7 Promise.resolve ( x )
    /// https://tc39.es/ecma262/#sec-promise.resolve
    fn resolve(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const resolution = arguments.get(0);

        // 1. Let C be the this value.
        const constructor = this_value;

        // 2. If C is not an Object, throw a TypeError exception.
        if (constructor != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{constructor}),
            );
        }

        // 3. Return ? PromiseResolve(C, x).
        return Value.from(try promiseResolve(agent, constructor.object, resolution));
    }
};

/// 27.2.5 Properties of the Promise Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-promise-prototype-object
pub const PromisePrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "catch", @"catch", 1, realm);
        try defineBuiltinFunction(object, "then", then, 2, realm);

        // 27.2.5.5 Promise.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-promise.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Promise"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 27.2.5.1 Promise.prototype.catch ( onRejected )
    /// https://tc39.es/ecma262/#sec-promise.prototype.catch
    fn @"catch"(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const on_rejected = arguments.get(0);

        // 1. Let promise be the this value.
        const promise = this_value;

        // 2. Return ? Invoke(promise, "then", « undefined, onRejected »).
        return promise.invoke(agent, PropertyKey.from("then"), .{ .undefined, on_rejected });
    }

    /// 27.2.5.4 Promise.prototype.then ( onFulfilled, onRejected )
    /// https://tc39.es/ecma262/#sec-promise.prototype.then
    fn then(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const on_fulfilled = arguments.get(0);
        const on_rejected = arguments.get(1);

        // 1. Let promise be the this value.
        const promise = this_value;

        // 2. If IsPromise(promise) is false, throw a TypeError exception.
        if (!promise.isPromise()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not a Promise", .{promise}),
            );
        }

        // 3. Let C be ? SpeciesConstructor(promise, %Promise%).
        const constructor = try promise.object.speciesConstructor(
            try realm.intrinsics.@"%Promise%"(),
        );

        // 4. Let resultCapability be ? NewPromiseCapability(C).
        const result_capability = try newPromiseCapability(agent, Value.from(constructor));

        // 5. Return PerformPromiseThen(promise, onFulfilled, onRejected, resultCapability).
        return Value.from(
            try performPromiseThen(
                agent,
                promise.object.as(Promise),
                on_fulfilled,
                on_rejected,
                result_capability,
            ) orelse return .undefined,
        );
    }
};

/// 27.2.6 Properties of Promise Instances
/// https://tc39.es/ecma262/#sec-properties-of-promise-instances
pub const Promise = Object.Factory(.{
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
        promise_fulfill_reactions: std.ArrayList(PromiseReaction),

        /// [[PromiseRejectReactions]]
        promise_reject_reactions: std.ArrayList(PromiseReaction),

        /// [[PromiseIsHandled]]
        promise_is_handled: bool,
    },
    .tag = .promise,
});
