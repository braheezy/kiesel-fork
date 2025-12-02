//! 7.4 Operations on Iterator Objects
//! https://tc39.es/ecma262/#sec-operations-on-iterator-objects

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const await = builtins.await;
const createAsyncFromSyncIterator = builtins.createAsyncFromSyncIterator;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 7.4.1 Iterator Records
/// https://tc39.es/ecma262/#sec-iterator-records
pub const Iterator = struct {
    /// [[Iterator]]
    iterator: *Object,

    /// [[NextMethod]]
    next_method: Value,

    /// [[Done]]
    done: bool,

    /// 7.4.6 IteratorNext ( iteratorRecord [ , value ] )
    /// https://tc39.es/ecma262/#sec-iteratornext
    pub fn next(self: *Iterator, agent: *Agent, value_: ?Value) Agent.Error!*Object {
        // 1. If value is not present, then
        const result_completion = if (value_ == null) blk: {
            // a. Let result be Completion(Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]])).
            break :blk self.next_method.call(agent, Value.from(self.iterator), &.{});
        } else blk: {
            // 2. Else,
            // a. Let result be Completion(Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]], « value »)).
            break :blk self.next_method.call(agent, Value.from(self.iterator), &.{value_.?});
        };

        // 3. If result is a throw completion, then
        // 4. Set result to ! result.
        const result = result_completion catch |err| {
            // a. Set iteratorRecord.[[Done]] to true.
            self.done = true;

            // b. Return ? result.
            return err;
        };

        // 5. If result is not an Object, then
        if (!result.isObject()) {
            // a. Set iteratorRecord.[[Done]] to true.
            self.done = true;

            // b. Throw a TypeError exception.
            return agent.throwException(.type_error, "{f} is not an Object", .{result});
        }

        // 6. Return result.
        return result.asObject();
    }

    /// 7.4.7 IteratorComplete ( iteratorResult )
    /// https://tc39.es/ecma262/#sec-iteratorcomplete
    pub fn complete(agent: *Agent, iterator_result: *Object) Agent.Error!bool {
        // 1. Return ToBoolean(? Get(iteratorResult, "done")).
        return (try iterator_result.get(agent, PropertyKey.from("done"))).toBoolean();
    }

    /// 7.4.8 IteratorValue ( iteratorResult )
    /// https://tc39.es/ecma262/#sec-iteratorvalue
    pub fn value(agent: *Agent, iterator_result: *Object) Agent.Error!Value {
        // Return ? Get(iteratorResult, "value").
        return iterator_result.get(agent, PropertyKey.from("value"));
    }

    /// 7.4.9 IteratorStep ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratorstep
    pub fn step(self: *Iterator, agent: *Agent) Agent.Error!?*Object {
        // 1. Let result be ? IteratorNext(iteratorRecord).
        const result = try self.next(agent, null);

        // 2. Let done be Completion(IteratorComplete(result)).
        // 3. If done is a throw completion, then
        // 4. Set done to ! done.
        const done = complete(agent, result) catch |err| {
            // a. Set iteratorRecord.[[Done]] to true.
            self.done = true;

            // b. Return ? done.
            return err;
        };

        // 5. If done is true, then
        if (done) {
            // a. Set iteratorRecord.[[Done]] to true.
            self.done = true;

            // b. Return done.
            return null;
        }

        // 6. Return result.
        return result;
    }

    /// 7.4.10 IteratorStepValue ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratorstepvalue
    pub fn stepValue(self: *Iterator, agent: *Agent) Agent.Error!?Value {
        // 1. Let result be ? IteratorStep(iteratorRecord).
        const result = try self.step(agent) orelse {
            // 2. If result is done, then
            //     a. Return done.
            return null;
        };

        // 3. Let value be Completion(IteratorValue(result)).
        // 4. If value is a throw completion, then
        const value_ = value(agent, result) catch |err| {
            // a. Set iteratorRecord.[[Done]] to true.
            self.done = true;

            return err;
        };

        // 5. Return ? value.
        return value_;
    }

    /// 7.4.11 IteratorClose ( iteratorRecord, completion )
    /// https://tc39.es/ecma262/#sec-iteratorclose
    pub fn close(self: Iterator, agent: *Agent, completion: anytype) @TypeOf(completion) {
        const completion_exception = agent.exception;

        // 1. Assert: iteratorRecord.[[Iterator]] is an Object.
        // 2. Let iterator be iteratorRecord.[[Iterator]].
        const iterator = self.iterator;

        // 3. Let innerResult be Completion(GetMethod(iterator, "return")).
        const inner_result_object = Value.from(iterator).getMethod(
            agent,
            PropertyKey.from("return"),
        );

        // 4. If innerResult is a normal completion, then
        const inner_result = if (inner_result_object) |@"return"| blk: {
            // a. Let return be innerResult.[[Value]].
            // b. If return is undefined, return ? completion.
            if (@"return" == null) return try completion;

            // c. Set innerResult to Completion(Call(return, iterator)).
            break :blk Value.from(@"return".?).callAssumeCallable(
                agent,
                Value.from(iterator),
                &.{},
            );
        } else |err| err;

        // 5. If completion is a throw completion, return ? completion.
        _ = completion catch |err| {
            agent.exception = completion_exception;
            return err;
        };

        // 6. If innerResult is a throw completion, return ? innerResult.
        const inner_result_value = inner_result catch |err| return err;

        // 7. If innerResult.[[Value]] is not an Object, throw a TypeError exception.
        if (!inner_result_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{inner_result_value});
        }

        // 8. Return ? completion.
        return completion;
    }

    /// 7.4.12 IteratorCloseAll ( iters, completion )
    /// https://tc39.es/ecma262/#sec-iteratorcloseall
    pub fn closeAll(agent: *Agent, iterators: []Iterator, completion: anytype) @TypeOf(completion) {
        var new_completion = completion;

        // 1. For each element iter of iters, in reverse List order, do
        var it = std.mem.reverseIterator(iterators);
        while (it.nextPtr()) |iterator| {
            // a. Set completion to Completion(IteratorClose(iter, completion)).
            new_completion = iterator.close(agent, new_completion);
        }

        // 2. Return ? completion.
        return new_completion;
    }

    /// 7.4.13 AsyncIteratorClose ( iteratorRecord, completion )
    /// https://tc39.es/ecma262/#sec-asynciteratorclose
    pub fn closeAsync(self: Iterator, agent: *Agent, completion: anytype) @TypeOf(completion) {
        const completion_exception = agent.exception;

        // 1. Assert: iteratorRecord.[[Iterator]] is an Object.
        // 2. Let iterator be iteratorRecord.[[Iterator]].
        const iterator = self.iterator;

        // 3. Let innerResult be Completion(GetMethod(iterator, "return")).
        const inner_result_object = Value.from(iterator).getMethod(
            agent,
            PropertyKey.from("return"),
        );

        // 4. If innerResult is a normal completion, then
        const inner_result = if (inner_result_object) |@"return"| blk: {
            // a. Let return be innerResult.[[Value]].
            // b. If return is undefined, return ? completion.
            if (@"return" == null) return try completion;

            // c. Set innerResult to Completion(Call(return, iterator)).
            const inner_result = Value.from(@"return".?).callAssumeCallable(
                agent,
                Value.from(iterator),
                &.{},
            );

            // d. If innerResult is a normal completion, set innerResult to
            //    Completion(Await(innerResult.[[Value]])).
            break :blk if (inner_result) |value_| await(agent, value_) else |err| err;
        } else |err| err;

        // 5. If completion is a throw completion, return ? completion.
        _ = completion catch |err| {
            agent.exception = completion_exception;
            return err;
        };

        // 6. If innerResult is a throw completion, return ? innerResult.
        const inner_result_value = inner_result catch |err| return err;

        // 7. If innerResult.[[Value]] is not an Object, throw a TypeError exception.
        if (!inner_result_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{inner_result_value});
        }

        // 8. Return ? completion.
        return completion;
    }

    /// 7.4.16 IteratorToList ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratortolist
    pub fn toList(self: *Iterator, agent: *Agent) Agent.Error![]const Value {
        // 1. Let values be a new empty List.
        var values: std.ArrayList(Value) = .empty;
        errdefer values.deinit(agent.gc_allocator);

        // 2. Repeat,
        //     a. Let next be ? IteratorStepValue(iteratorRecord).
        //     b. If next is done, then
        //         i. Return values.
        while (try self.stepValue(agent)) |next_| {
            // c. Append next to values.
            try values.append(agent.gc_allocator, next_);
        }

        return values.toOwnedSlice(agent.gc_allocator);
    }
};

/// 7.4.2 GetIteratorDirect ( obj )
/// https://tc39.es/ecma262/#sec-getiteratordirect
pub fn getIteratorDirect(agent: *Agent, object: *Object) Agent.Error!Iterator {
    // 1. Let nextMethod be ? Get(obj, "next").
    const next_method = try object.get(agent, PropertyKey.from("next"));

    // 2. Let iteratorRecord be the Iterator Record { [[Iterator]]: obj, [[NextMethod]]: nextMethod, [[Done]]: false }.
    const iterator: Iterator = .{
        .iterator = object,
        .next_method = next_method,
        .done = false,
    };

    // 3. Return iteratorRecord.
    return iterator;
}

/// 7.4.3 GetIteratorFromMethod ( obj, method )
/// https://tc39.es/ecma262/#sec-getiteratorfrommethod
pub fn getIteratorFromMethod(agent: *Agent, object: Value, method: *Object) Agent.Error!Iterator {
    // 1. Let iterator be ? Call(method, obj).
    const iterator = try Value.from(method).call(agent, object, &.{});

    // 2. If iterator is not an Object, throw a TypeError exception.
    if (!iterator.isObject()) {
        return agent.throwException(.type_error, "{f} is not an Object", .{iterator});
    }

    // 3. Return ? GetIteratorDirect(iterator).
    return getIteratorDirect(agent, iterator.asObject());
}

pub const IteratorKind = enum { sync, async };

/// 7.4.4 GetIterator ( obj, kind )
/// https://tc39.es/ecma262/#sec-getiterator
pub fn getIterator(
    agent: *Agent,
    object: Value,
    kind: IteratorKind,
) Agent.Error!Iterator {
    // 1. If kind is async, then
    const method = (if (kind == .async) blk: {
        // a. Let method be ? GetMethod(obj, %Symbol.asyncIterator%).
        const method = try object.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.asyncIterator%"),
        );

        // b. If method is undefined, then
        if (method == null) {
            // i. Let syncMethod be ? GetMethod(obj, %Symbol.iterator%).
            const sync_method = try object.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
            ) orelse {
                // ii. If syncMethod is undefined, throw a TypeError exception.
                return agent.throwException(
                    .type_error,
                    "Object has no Symbol.asyncIterator or Symbol.iterator method",
                    .{},
                );
            };

            // iii. Let syncIteratorRecord be ? GetIteratorFromMethod(obj, syncMethod).
            const sync_iterator = try getIteratorFromMethod(agent, object, sync_method);

            // iv. Return CreateAsyncFromSyncIterator(syncIteratorRecord).
            return createAsyncFromSyncIterator(agent, sync_iterator);
        }

        break :blk method;
    } else blk: {
        // 2. Else,
        // a. Let method be ? GetMethod(obj, %Symbol.iterator%).
        break :blk try object.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
        );
    }) orelse {
        // 3. If method is undefined, throw a TypeError exception.
        return agent.throwException(.type_error, "Object has no Symbol.iterator method", .{});
    };

    // 4. Return ? GetIteratorFromMethod(obj, method).
    return getIteratorFromMethod(agent, object, method);
}

/// 7.4.5 GetIteratorFlattenable ( obj, primitiveHandling )
/// https://tc39.es/ecma262/#sec-getiteratorflattenable
pub fn getIteratorFlattenable(
    agent: *Agent,
    object: Value,
    primitive_handling: enum {
        iterate_string_primitives,
        reject_primitives,
    },
) Agent.Error!Iterator {
    // 1. If obj is not an Object, then
    if (!object.isObject()) {
        switch (primitive_handling) {
            // a. If primitiveHandling is reject-primitives, throw a TypeError exception.
            .reject_primitives => {
                return agent.throwException(.type_error, "{f} is not an Object", .{object});
            },

            // b. Assert: primitiveHandling is iterate-string-primitives.
            .iterate_string_primitives => {
                // c. If obj is not a String, throw a TypeError exception.
                if (!object.isString()) {
                    return agent.throwException(.type_error, "{f} is not a string", .{object});
                }
            },
        }
    }

    // 2. Let method be ? GetMethod(obj, %Symbol.iterator%).
    const method = try object.getMethod(
        agent,
        PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
    );

    // 3. If method is undefined, then
    const iterator = if (method == null) blk: {
        // a. Let iterator be obj.
        break :blk object;
    } else blk: {
        // 4. Else,
        // a. Let iterator be ? Call(method, obj).
        break :blk try Value.from(method.?).callAssumeCallable(agent, object, &.{});
    };

    // 5. If iterator is not an Object, throw a TypeError exception.
    if (!iterator.isObject()) {
        return agent.throwException(.type_error, "{f} is not an Object", .{iterator});
    }

    // 6. Return ? GetIteratorDirect(iterator).
    return getIteratorDirect(agent, iterator.asObject());
}

/// 7.4.14 CreateIteratorResultObject ( value, done )
/// https://tc39.es/ecma262/#sec-createiterresultobject
pub fn createIteratorResultObject(
    agent: *Agent,
    value: Value,
    done: bool,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    const object = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

    // 2. Perform ! CreateDataPropertyOrThrow(obj, "value", value).
    try object.createDataPropertyDirect(agent, PropertyKey.from("value"), value);

    // 3. Perform ! CreateDataPropertyOrThrow(obj, "done", done).
    try object.createDataPropertyDirect(agent, PropertyKey.from("done"), Value.from(done));

    // 4. Return obj.
    return object;
}
