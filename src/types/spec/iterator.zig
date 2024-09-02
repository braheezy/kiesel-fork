//! 7.4 Operations on Iterator Objects
//! https://tc39.es/ecma262/#sec-operations-on-iterator-objects

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const createAsyncFromSyncIterator = builtins.createAsyncFromSyncIterator;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 7.4.1 Iterator Records
/// https://tc39.es/ecma262/#sec-iterator-records
pub const Iterator = struct {
    /// [[Iterator]]
    iterator: Object,

    /// [[NextMethod]]
    next_method: Value,

    /// [[Done]]
    done: bool,

    /// 7.4.4 IteratorNext ( iteratorRecord [ , value ] )
    /// https://tc39.es/ecma262/#sec-iteratornext
    pub fn next(self: *Iterator, value_: ?Value) Agent.Error!Object {
        const agent = self.iterator.agent();

        // 1. If value is not present, then
        const result_completion = if (value_ == null) blk: {
            // a. Let result be Completion(Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]])).
            break :blk self.next_method.callNoArgs(agent, Value.from(self.iterator));
        }
        // 2. Else,
        else blk: {
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
            return agent.throwException(.type_error, "{} is not an Object", .{result});
        }

        // 6. Return result.
        return result.asObject();
    }

    /// 7.4.5 IteratorComplete ( iteratorResult )
    /// https://tc39.es/ecma262/#sec-iteratorcomplete
    pub fn complete(iterator_result: Object) Agent.Error!bool {
        // 1. Return ToBoolean(? Get(iteratorResult, "done")).
        return (try iterator_result.get(PropertyKey.from("done"))).toBoolean();
    }

    /// 7.4.6 IteratorValue ( iteratorResult )
    /// https://tc39.es/ecma262/#sec-iteratorvalue
    pub fn value(iterator_result: Object) Agent.Error!Value {
        // Return ? Get(iteratorResult, "value").
        return iterator_result.get(PropertyKey.from("value"));
    }

    /// 7.4.7 IteratorStep ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratorstep
    pub fn step(self: *Iterator) Agent.Error!?Object {
        // 1. Let result be ? IteratorNext(iteratorRecord).
        const result = try next(self, null);

        // 2. Let done be Completion(IteratorComplete(result)).
        // 3. If done is a throw completion, then
        // 4. Set done to ! done.
        const done = complete(result) catch |err| {
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

    /// 7.4.8 IteratorStepValue ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratorstepvalue
    pub fn stepValue(self: *Iterator) Agent.Error!?Value {
        // 1. Let result be ? IteratorStep(iteratorRecord).
        const result = try self.step();

        // 2. If result is done, then
        if (result == null) {
            // a. Return done.
            return null;
        }

        // 3. Let value be Completion(IteratorValue(result)).
        // 4. If value is a throw completion, then
        const value_ = value(result.?) catch |err| {
            // a. Set iteratorRecord.[[Done]] to true.
            self.done = true;

            return err;
        };

        // 5. Return ? value.
        return value_;
    }

    /// 7.4.9 IteratorClose ( iteratorRecord, completion )
    /// https://tc39.es/ecma262/#sec-iteratorclose
    pub fn close(self: Iterator, completion: anytype) @TypeOf(completion) {
        const agent = self.iterator.agent();

        const completion_exception = agent.exception;

        // 1. Assert: iteratorRecord.[[Iterator]] is an Object.
        // 2. Let iterator be iteratorRecord.[[Iterator]].
        const iterator = self.iterator;

        // 3. Let innerResult be Completion(GetMethod(iterator, "return")).
        const inner_result_object = Value.from(iterator).getMethod(
            agent,
            PropertyKey.from("return"),
        );

        // 4. If innerResult a normal completion, then
        const inner_result = if (inner_result_object) |@"return"| blk: {
            // a. Let return be innerResult.[[Value]].
            // b. If return is undefined, return ? completion.
            if (@"return" == null) return try completion;

            // c. Set innerResult to Completion(Call(return, iterator)).
            break :blk Value.from(@"return".?).callAssumeCallableNoArgs(Value.from(iterator));
        } else |err| err;

        // 5. If completion a throw completion, return ? completion.
        _ = completion catch |err| {
            agent.exception = completion_exception;
            return err;
        };

        // 6. If innerResult a throw completion, return ? innerResult.
        const inner_result_value = inner_result catch |err| return err;

        // 7. If innerResult.[[Value]] is not an Object, throw a TypeError exception.
        if (!inner_result_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{inner_result_value});
        }

        // 8. Return ? completion.
        return completion;
    }

    /// 7.4.14 IteratorToList ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratortolist
    pub fn toList(self: *Iterator) Agent.Error![]const Value {
        const agent = self.iterator.agent();

        // 1. Let values be a new empty List.
        var values = std.ArrayList(Value).init(agent.gc_allocator);
        errdefer values.deinit();

        // 2. Repeat,
        //     a. Let next be ? IteratorStepValue(iteratorRecord).
        //     b. If next is done, then
        //         i. Return values.
        while (try self.stepValue()) |next_| {
            // c. Append next to values.
            try values.append(next_);
        }

        return values.toOwnedSlice();
    }
};

/// 7.4.2 GetIteratorFromMethod ( obj, method )
/// https://tc39.es/ecma262/#sec-normalcompletion
pub fn getIteratorFromMethod(agent: *Agent, object: Value, method: Object) Agent.Error!Iterator {
    // 1. Let iterator be ? Call(method, obj).
    const iterator = try Value.from(method).callNoArgs(agent, object);

    // 2. If iterator is not an Object, throw a TypeError exception.
    if (!iterator.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{iterator});
    }

    // 3. Let nextMethod be ? Get(iterator, "next").
    const next_method = try iterator.get(agent, PropertyKey.from("next"));

    // 4. Let iteratorRecord be the Iterator Record {
    //      [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false
    //    }.
    const iterator_record: Iterator = .{
        .iterator = iterator.asObject(),
        .next_method = next_method,
        .done = false,
    };

    // 5. Return iteratorRecord.
    return iterator_record;
}

pub const IteratorKind = enum { sync, @"async" };

/// 7.4.3 GetIterator ( obj, kind )
/// https://tc39.es/ecma262/#sec-getiterator
pub fn getIterator(
    agent: *Agent,
    object: Value,
    kind: IteratorKind,
) Agent.Error!Iterator {
    // 1. If kind is async, then
    const method = if (kind == .@"async") blk: {
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
            );

            // ii. If syncMethod is undefined, throw a TypeError exception.
            if (sync_method == null) {
                return agent.throwException(
                    .type_error,
                    "Object has no Symbol.asyncIterator or Symbol.iterator method",
                    .{},
                );
            }

            // iii. Let syncIteratorRecord be ? GetIteratorFromMethod(obj, syncMethod).
            const sync_iterator = try getIteratorFromMethod(agent, object, sync_method.?);

            // iv. Return CreateAsyncFromSyncIterator(syncIteratorRecord).
            return createAsyncFromSyncIterator(agent, sync_iterator);
        }

        break :blk method;
    }
    // 2. Else,
    else blk: {
        // a. Let method be ? GetMethod(obj, %Symbol.iterator%).
        break :blk try object.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
        );
    };

    // 3. If method is undefined, throw a TypeError exception.
    if (method == null) {
        return agent.throwException(.type_error, "Object has no Symbol.iterator method", .{});
    }

    // 4. Return ? GetIteratorFromMethod(obj, method).
    return getIteratorFromMethod(agent, object, method.?);
}

/// 7.4.12 CreateIteratorResultObject ( value, done )
/// https://tc39.es/ecma262/#sec-createiterresultobject
pub fn createIteratorResultObject(
    agent: *Agent,
    value: Value,
    done: bool,
) std.mem.Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    const object = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

    // 2. Perform ! CreateDataPropertyOrThrow(obj, "value", value).
    object.createDataPropertyOrThrow(
        PropertyKey.from("value"),
        value,
    ) catch |err| try noexcept(err);

    // 3. Perform ! CreateDataPropertyOrThrow(obj, "done", done).
    object.createDataPropertyOrThrow(
        PropertyKey.from("done"),
        Value.from(done),
    ) catch |err| try noexcept(err);

    // 4. Return obj.
    return object;
}
