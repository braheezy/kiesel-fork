//! 27.1.4 Async-from-Sync Iterator Objects
//! https://tc39.es/ecma262/#sec-async-from-sync-iterator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Iterator = types.Iterator;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const performPromiseThen = builtins.performPromiseThen;
const promiseResolve = builtins.promiseResolve;

/// 27.1.4.1 CreateAsyncFromSyncIterator ( syncIteratorRecord )
/// https://tc39.es/ecma262/#sec-createasyncfromsynciterator
pub fn createAsyncFromSyncIterator(
    agent: *Agent,
    sync_iterator: Iterator,
) Allocator.Error!Iterator {
    const realm = agent.currentRealm();

    // 1. Let asyncIterator be OrdinaryObjectCreate(%AsyncFromSyncIteratorPrototype%,
    //    « [[SyncIteratorRecord]] »).
    // 2. Set asyncIterator.[[SyncIteratorRecord]] to syncIteratorRecord.
    const async_iterator = try AsyncFromSyncIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%AsyncFromSyncIteratorPrototype%"(),
        .fields = .{
            .sync_iterator = sync_iterator,
        },
    });

    // 3. Let nextMethod be ! Get(asyncIterator, "next").
    const next_method = async_iterator.get(PropertyKey.from("next")) catch |err| try noexcept(err);

    // 4. Let iteratorRecord be the Iterator Record {
    //      [[Iterator]]: asyncIterator, [[NextMethod]]: nextMethod, [[Done]]: false
    //    }.
    const iterator: Iterator = .{
        .iterator = async_iterator,
        .next_method = next_method,
        .done = false,
    };

    // 5. Return iteratorRecord.
    return iterator;
}

/// 27.1.4.2 The %AsyncFromSyncIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%-object
pub const AsyncFromSyncIteratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%AsyncIteratorPrototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);
        try defineBuiltinFunction(object, "return", @"return", 0, realm);
        try defineBuiltinFunction(object, "throw", throw, 0, realm);
    }

    /// 27.1.4.2.1 %AsyncFromSyncIteratorPrototype%.next ( [ value ] )
    /// https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%-object
    fn next(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const maybe_value = arguments.getOrNull(0);

        // 1. Let O be the this value.
        // 2. Assert: O is an Object that has a [[SyncIteratorRecord]] internal slot.
        const async_from_sync_iterator = this_value.object.as(AsyncFromSyncIterator);

        // 3. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // 4. Let syncIteratorRecord be O.[[SyncIteratorRecord]].
        const sync_iterator = &async_from_sync_iterator.fields.sync_iterator;

        // 5. If value is present, then
        const result = (if (maybe_value) |value| blk: {
            // a. Let result be Completion(IteratorNext(syncIteratorRecord, value)).
            break :blk sync_iterator.next(value);
        }
        // 6. Else,
        else blk: {
            // a. Let result be Completion(IteratorNext(syncIteratorRecord)).
            break :blk sync_iterator.next(null);
        }) catch |err| {
            // 7. IfAbruptRejectPromise(result, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 8. Return AsyncFromSyncIteratorContinuation(result, promiseCapability).
        return Value.from(
            try asyncFromSyncIteratorContinuation(agent, result, promise_capability),
        );
    }

    /// 27.1.4.2.2 %AsyncFromSyncIteratorPrototype%.return ( [ value ] )
    /// https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%.return
    fn @"return"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const maybe_value = arguments.getOrNull(0);

        // 1. Let O be the this value.
        // 2. Assert: O is an Object that has a [[SyncIteratorRecord]] internal slot.
        const async_from_sync_iterator = this_value.object.as(AsyncFromSyncIterator);

        // 3. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // 4. Let syncIterator be O.[[SyncIteratorRecord]].[[Iterator]].
        const sync_iterator = async_from_sync_iterator.fields.sync_iterator.iterator;

        // 5. Let return be Completion(GetMethod(syncIterator, "return")).
        const return_ = Value.from(sync_iterator).getMethod(agent, PropertyKey.from("return")) catch |err| {
            // 6. IfAbruptRejectPromise(return, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 7. If return is undefined, then
        if (return_ == null) {
            // a. Let iterResult be CreateIterResultObject(value, true).
            const iter_result = try createIterResultObject(
                agent,
                maybe_value orelse .undefined,
                true,
            );

            // b. Perform ! Call(promiseCapability.[[Resolve]], undefined, « iterResult »).
            _ = Value.from(promise_capability.resolve).callAssumeCallable(
                .undefined,
                &.{Value.from(iter_result)},
            ) catch |err| try noexcept(err);

            // c. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 8. If value is present, then
        const result = (if (maybe_value) |value| blk: {
            // a. Let result be Completion(Call(return, syncIterator, « value »)).
            break :blk Value.from(return_.?).callAssumeCallable(
                Value.from(sync_iterator),
                &.{value},
            );
        }
        // 9. Else,
        else blk: {
            // a. Let result be Completion(Call(return, syncIterator)).
            break :blk Value.from(return_.?).callAssumeCallableNoArgs(Value.from(sync_iterator));
        }) catch |err| {
            // 10. IfAbruptRejectPromise(result, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 11. If result is not an Object, then
        if (result != .object) {
            const type_error = try agent.createException(
                .type_error,
                "Return value of iterator 'return' function must be object",
                .{},
            );

            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « a newly created
            //    TypeError object »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                .undefined,
                &.{Value.from(type_error)},
            ) catch |err| try noexcept(err);

            // b. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 12. Return AsyncFromSyncIteratorContinuation(result, promiseCapability).
        return Value.from(
            try asyncFromSyncIteratorContinuation(agent, result.object, promise_capability),
        );
    }

    /// 27.1.4.2.3 %AsyncFromSyncIteratorPrototype%.throw ( [ value ] )
    /// https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%.throw
    fn throw(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const maybe_value = arguments.getOrNull(0);

        // 1. Let O be the this value.
        // 2. Assert: O is an Object that has a [[SyncIteratorRecord]] internal slot.
        const async_from_sync_iterator = this_value.object.as(AsyncFromSyncIterator);

        // 3. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // 4. Let syncIterator be O.[[SyncIteratorRecord]].[[Iterator]].
        const sync_iterator = async_from_sync_iterator.fields.sync_iterator.iterator;

        // 5. Let throw be Completion(GetMethod(syncIterator, "throw")).
        const throw_ = Value.from(sync_iterator).getMethod(agent, PropertyKey.from("throw")) catch |err| {
            // 6. IfAbruptRejectPromise(throw, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 7. If throw is undefined, then
        if (throw_ == null) {
            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « value »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                .undefined,
                &.{maybe_value orelse .undefined},
            ) catch |err| try noexcept(err);

            // b. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 8. If value is present, then
        const result = (if (maybe_value) |value| blk: {
            // a. Let result be Completion(Call(throw, syncIterator, « value »)).
            break :blk Value.from(throw_.?).callAssumeCallable(
                Value.from(sync_iterator),
                &.{value},
            );
        }
        // 9. Else,
        else blk: {
            // a. Let result be Completion(Call(throw, syncIterator)).
            break :blk Value.from(throw_.?).callAssumeCallableNoArgs(Value.from(sync_iterator));
        }) catch |err| {
            // 10. IfAbruptRejectPromise(result, promiseCapability).
            return Value.from(try promise_capability.rejectPromise(agent, err));
        };

        // 11. If result is not an Object, then
        if (result != .object) {
            const type_error = try agent.createException(
                .type_error,
                "Return value of iterator 'throw' function must be object",
                .{},
            );

            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « a newly created
            //    TypeError object »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                .undefined,
                &.{Value.from(type_error)},
            ) catch |err| try noexcept(err);

            // b. Return promiseCapability.[[Promise]].
            return Value.from(promise_capability.promise);
        }

        // 12. Return AsyncFromSyncIteratorContinuation(result, promiseCapability).
        return Value.from(
            try asyncFromSyncIteratorContinuation(agent, result.object, promise_capability),
        );
    }
};

/// 27.1.4.3 Properties of Async-from-Sync Iterator Instances
/// https://tc39.es/ecma262/#sec-properties-of-async-from-sync-iterator-instances
pub const AsyncFromSyncIterator = MakeObject(.{
    .Fields = struct {
        /// [[SyncIteratorRecord]]
        sync_iterator: Iterator,
    },
});

/// 27.1.4.4 AsyncFromSyncIteratorContinuation ( result, promiseCapability )
/// https://tc39.es/ecma262/#sec-asyncfromsynciteratorcontinuation
fn asyncFromSyncIteratorContinuation(agent: *Agent, result: Object, promise_capability: PromiseCapability) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. NOTE: Because promiseCapability is derived from the intrinsic %Promise%, the calls to
    //    promiseCapability.[[Reject]] entailed by the use IfAbruptRejectPromise below are
    //    guaranteed not to throw.

    // 2. Let done be Completion(IteratorComplete(result)).
    const done = Iterator.complete(result) catch |err| {
        // 3. IfAbruptRejectPromise(done, promiseCapability).
        return promise_capability.rejectPromise(agent, err) catch |err_| try noexcept(err_);
    };

    // 4. Let value be Completion(IteratorValue(result)).
    const value = Iterator.value(result) catch |err| {
        // 5. IfAbruptRejectPromise(value, promiseCapability).
        return promise_capability.rejectPromise(agent, err) catch |err_| try noexcept(err_);
    };

    // 6. Let valueWrapper be Completion(PromiseResolve(%Promise%, value)).
    const value_wrapper = promiseResolve(agent, try realm.intrinsics.@"%Promise%"(), value) catch |err| {
        // 7. IfAbruptRejectPromise(valueWrapper, promiseCapability).
        return promise_capability.rejectPromise(agent, err) catch |err_| try noexcept(err_);
    };

    const Captures = struct {
        done: bool,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .done = done };

    // 8. Let unwrap be a new Abstract Closure with parameters (v) that captures done and performs
    //    the following steps when called:
    const unwrap = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const done_ = captures_.done;
            const value_ = arguments_.get(0);

            // a. Return CreateIterResultObject(v, done).
            return Value.from(try createIterResultObject(agent_, value_, done_));
        }
    }.func;

    // 9. Let onFulfilled be CreateBuiltinFunction(unwrap, 1, "", « »).
    const on_fulfilled = Value.from(
        try createBuiltinFunction(agent, .{ .function = unwrap }, .{
            .length = 1,
            .name = "",
            .additional_fields = SafePointer.make(*Captures, captures),
        }),
    );

    // 10. NOTE: onFulfilled is used when processing the "value" property of an IteratorResult
    //     object in order to wait for its value if it is a promise and re-package the result in a
    //     new "unwrapped" IteratorResult object.

    // 11. Perform PerformPromiseThen(valueWrapper, onFulfilled, undefined, promiseCapability).
    _ = try performPromiseThen(
        agent,
        value_wrapper.as(builtins.Promise),
        on_fulfilled,
        .undefined,
        promise_capability,
    );

    // 12. Return promiseCapability.[[Promise]].
    return promise_capability.promise;
}
