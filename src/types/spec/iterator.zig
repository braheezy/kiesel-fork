//! 7.4 Operations on Iterator Objects
//! https://tc39.es/ecma262/#sec-operations-on-iterator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 7.4.1 Iterator Records
/// https://tc39.es/ecma262/#sec-iterator-records
pub const Iterator = struct {
    const Self = @This();

    /// [[Iterator]]
    iterator: Object,

    /// [[NextMethod]]
    next_method: Value,

    /// [[Done]]
    done: bool,

    /// 7.4.4 IteratorNext ( iteratorRecord [ , value ] )
    /// https://tc39.es/ecma262/#sec-iteratornext
    pub fn next(self: Self, value_: ?Value) Agent.Error!Object {
        const agent = self.iterator.agent();

        // 1. If value is not present, then
        const result = if (value_ == null) blk: {
            // a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
            break :blk try self.next_method.callNoArgs(agent, Value.from(self.iterator));
        }
        // 2. Else,
        else blk: {
            // a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]], « value »).
            break :blk try self.next_method.call(agent, Value.from(self.iterator), &.{value_.?});
        };

        // 3. If result is not an Object, throw a TypeError exception.
        if (result != .object) {
            return agent.throwException(.type_error, "{} is not an Object", .{result});
        }

        // 4. Return result.
        return result.object;
    }

    /// 7.4.5 IteratorComplete ( iterResult )
    /// https://tc39.es/ecma262/#sec-iteratorcomplete
    pub fn complete(iter_result: Object) Agent.Error!bool {
        // 1. Return ToBoolean(? Get(iterResult, "done")).
        return (try iter_result.get(PropertyKey.from("done"))).toBoolean();
    }

    /// 7.4.6 IteratorValue ( iterResult )
    /// https://tc39.es/ecma262/#sec-iteratorvalue
    pub fn value(iter_result: Object) Agent.Error!Value {
        // Return ? Get(iterResult, "value").
        return iter_result.get(PropertyKey.from("value"));
    }

    /// 7.4.7 IteratorStep ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratorstep
    pub fn step(self: Self) Agent.Error!?Object {
        // 1. Let result be ? IteratorNext(iteratorRecord).
        const result = try next(self, null);

        // 2. Let done be ? IteratorComplete(result).
        const done = try complete(result);

        // 3. If done is true, return false.
        if (done) return null;

        // 4. Return result.
        return result;
    }

    /// 7.4.8 IteratorClose ( iteratorRecord, completion )
    /// https://tc39.es/ecma262/#sec-iteratorclose
    pub fn close(self: Self, completion: anytype) @TypeOf(completion) {
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

        // 4. If innerResult.[[Type]] is normal, then
        const inner_result = if (inner_result_object) |@"return"| blk: {
            // a. Let return be innerResult.[[Value]].
            // b. If return is undefined, return ? completion.
            if (@"return" == null) return try completion;

            // c. Set innerResult to Completion(Call(return, iterator)).
            break :blk Value.from(@"return".?).callAssumeCallableNoArgs(Value.from(iterator));
        } else |err| err;

        // 5. If completion.[[Type]] is throw, return ? completion.
        _ = completion catch |err| {
            agent.exception = completion_exception;
            return err;
        };

        // 6. If innerResult.[[Type]] is throw, return ? innerResult.
        const inner_result_value = inner_result catch |err| return err;

        // 7. If innerResult.[[Value]] is not an Object, throw a TypeError exception.
        if (inner_result_value != .object) {
            return agent.throwException(.type_error, "{} is not an Object", .{inner_result_value});
        }

        // 8. Return ? completion.
        return completion;
    }

    /// 7.4.13 IteratorToList ( iteratorRecord )
    /// https://tc39.es/ecma262/#sec-iteratortolist
    pub fn toList(self: Self) Agent.Error![]const Value {
        const agent = self.iterator.agent();

        // 1. Let values be a new empty List.
        var values = std.ArrayList(Value).init(agent.gc_allocator);
        errdefer values.deinit();

        // 2. Let next be true.

        // 3. Repeat, while next is not false,
        //     a. Set next to ? IteratorStep(iteratorRecord).
        //     b. If next is not false, then
        while (try self.step()) |next_| {
            // i. Let nextValue be ? IteratorValue(next).
            const next_value = try value(next_);

            // ii. Append nextValue to values.
            try values.append(next_value);
        }

        // 4. Return values.
        return values.toOwnedSlice();
    }
};

/// 7.4.2 GetIteratorFromMethod ( obj, method )
/// https://tc39.es/ecma262/#sec-normalcompletion
pub fn getIteratorFromMethod(agent: *Agent, object: Value, method: Object) Agent.Error!Iterator {
    // 1. Let iterator be ? Call(method, obj).
    const iterator = try Value.from(method).callNoArgs(agent, object);

    // 2. If iterator is not an Object, throw a TypeError exception.
    if (iterator != .object) {
        return agent.throwException(.type_error, "{} is not an Object", .{iterator});
    }

    // 3. Let nextMethod be ? Get(iterator, "next").
    const next_method = try iterator.get(agent, PropertyKey.from("next"));

    // 4. Let iteratorRecord be the Iterator Record {
    //      [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false
    //    }.
    const iterator_record = Iterator{
        .iterator = iterator.object,
        .next_method = next_method,
        .done = false,
    };

    // 5. Return iteratorRecord.
    return iterator_record;
}

/// 7.4.3 GetIterator ( obj, kind )
/// https://tc39.es/ecma262/#sec-getiterator
pub fn getIterator(
    agent: *Agent,
    object: Value,
    kind: enum { sync, @"async" },
) Agent.Error!Iterator {
    // 1. If kind is async, then
    const method = if (kind == .@"async") blk: {
        // a. Let method be ? GetMethod(obj, @@asyncIterator).
        const method = try object.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@asyncIterator"),
        );

        // b. If method is undefined, then
        if (method == null) {
            // i. Let syncMethod be ? GetMethod(obj, @@iterator).
            const sync_method = try object.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@iterator"),
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
            const sync_iterator_record = try getIteratorFromMethod(agent, object, sync_method.?);

            // TODO: iv. Return CreateAsyncFromSyncIterator(syncIteratorRecord).
            _ = sync_iterator_record;
            @panic("Not implemented");
        }

        break :blk method;
    }
    // 2. Else,
    else blk: {
        // a. Let method be ? GetMethod(obj, @@iterator).
        break :blk try object.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@iterator"),
        );
    };

    // 3. If method is undefined, throw a TypeError exception.
    if (method == null) {
        return agent.throwException(.type_error, "Object has no Symbol.iterator method", .{});
    }

    // 4. Return ? GetIteratorFromMethod(obj, method).
    return getIteratorFromMethod(agent, object, method.?);
}

/// 7.4.11 CreateIterResultObject ( value, done )
/// https://tc39.es/ecma262/#sec-createiterresultobject
pub fn createIterResultObject(agent: *Agent, value: Value, done: bool) Allocator.Error!Object {
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
