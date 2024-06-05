//! 24.2 Set Objects
//! https://tc39.es/ecma262/#sec-set-objects

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
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const ValueArrayHashMap = types.ValueArrayHashMap;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createSetIterator = builtins.createSetIterator;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;
const getIteratorFromMethod = types.getIteratorFromMethod;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreateWithType = builtins.ordinaryObjectCreateWithType;
const sameValue = types.sameValue;

/// 24.2.1.1 Set Records
/// https://tc39.es/ecma262/#sec-set-records
const SetRecord = struct {
    /// [[Set]]
    set: Object,

    /// [[Size]]
    size: usize,

    /// [[Has]]
    has: Object,

    /// [[Keys]]
    keys: Object,
};

/// 24.2.1.2 GetSetRecord ( obj )
/// https://tc39.es/ecma262/#sec-getsetrecord
fn getSetRecord(agent: *Agent, object_value: Value) Agent.Error!SetRecord {
    // 1. If obj is not an Object, throw a TypeError exception.
    const object = if (object_value != .object) {
        return agent.throwException(
            .type_error,
            "{} is not an Object",
            .{object_value},
        );
    } else object_value.object;

    // 2. Let rawSize be ? Get(obj, "size").
    const raw_size = try object.get(PropertyKey.from("size"));

    // 3. Let numSize be ? ToNumber(rawSize).
    const num_size = try raw_size.toNumber(agent);

    // 4. NOTE: If rawSize is undefined, then numSize will be NaN.

    // 5. If numSize is NaN, throw a TypeError exception.
    if (num_size.isNan()) {
        return agent.throwException(
            .type_error,
            "Size of Set-like object must not be NaN",
            .{},
        );
    }

    // 6. Let intSize be ! ToIntegerOrInfinity(numSize).
    const int_size = Value.from(num_size).toIntegerOrInfinity(agent) catch |err| try noexcept(err);

    // 7. If intSize < 0, throw a RangeError exception.
    if (int_size < 0) {
        return agent.throwException(
            .range_error,
            "Size of Set-like object must be non-negative",
            .{},
        );
    }

    // 8. Let has be ? Get(obj, "has").
    const has = try object.get(PropertyKey.from("has"));

    // 9. If IsCallable(has) is false, throw a TypeError exception.
    if (!has.isCallable()) {
        return agent.throwException(
            .type_error,
            "'has' property of Set-like object must be callable",
            .{},
        );
    }

    // 10. Let keys be ? Get(obj, "keys").
    const keys = try object.get(PropertyKey.from("keys"));

    // 11. If IsCallable(keys) is false, throw a TypeError exception.
    if (!keys.isCallable()) {
        return agent.throwException(
            .type_error,
            "'keys' property of Set-like object must be callable",
            .{},
        );
    }

    // 12. Return a new Set Record { [[Set]]: obj, [[Size]]: intSize, [[Has]]: has, [[Keys]]: keys }.
    return .{
        .set = object,
        .size = @intFromFloat(int_size),
        .has = has.object,
        .keys = keys.object,
    };
}

/// 24.2.1.3 SetDataHas ( setData, value )
/// https://tc39.es/ecma262/#sec-setdatahas
fn setDataHas(set_data: SetData, value: Value) bool {
    // 1. If SetDataIndex(setData, value) is not-found, return false.
    // 2. Return true.
    return setDataIndex(set_data, value) != null;
}

/// 24.2.1.4 SetDataIndex ( setData, value )
/// https://tc39.es/ecma262/#sec-setdataindex
fn setDataIndex(set_data: SetData, value: Value) ?usize {
    // 1. Set value to CanonicalizeKeyedCollectionKey(value).
    // 2. Let size be the number of elements in setData.
    // 3. Let index be 0.
    // 4. Repeat, while index < size,
    //    a. Let e be setData[index].
    //    b. If e is not empty and e is value, then
    //       i. Return index.
    //    c. Set index to index + 1.
    // 5. Return not-found.
    return set_data.getIndex(value.canonicalizeKeyedCollectionKey());
}

/// 24.2.1.5 SetDataSize ( setData )
/// https://tc39.es/ecma262/#sec-setdatasize
fn setDataSize(set_data: SetData) usize {
    // 1. Let count be 0.
    // 2. For each element e of setData, do
    //     a. If e is not empty, set count to count + 1.
    // 3. Return count.
    return set_data.count();
}

/// 24.2.3 Properties of the Set Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-set-constructor
pub const SetConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 0,
            .name = "Set",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinAccessor(object, "@@species", @"@@species", null, realm);

        // 24.2.3.1 Set.prototype
        // https://tc39.es/ecma262/#sec-set.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Set.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 24.2.4.3 Set.prototype.constructor
        // https://tc39.es/ecma262/#sec-set.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Set.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 24.2.2.1 Set ( [ iterable ] )
    /// https://tc39.es/ecma262/#sec-set-iterable
    fn behaviour(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const iterable = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Set must be constructed with 'new'", .{});
        }

        // 2. Let set be ? OrdinaryCreateFromConstructor(NewTarget, "%Set.prototype%", « [[SetData]] »).
        const set = try ordinaryCreateFromConstructor(
            Set,
            agent,
            new_target.?,
            "%Set.prototype%",
            .{
                // 3. Set set.[[SetData]] to a new empty List.
                .set_data = SetData.init(agent.gc_allocator),
            },
        );

        // 4. If iterable is either undefined or null, return set.
        if (iterable == .undefined or iterable == .null) return Value.from(set);

        // 5. Let adder be ? Get(set, "add").
        const adder = try set.get(PropertyKey.from("add"));

        // 6. If IsCallable(adder) is false, throw a TypeError exception.
        if (!adder.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{adder});
        }

        // 7. Let iteratorRecord be ? GetIterator(iterable, sync).
        var iterator = try getIterator(agent, iterable, .sync);

        // 8. Repeat,
        //     a. Let next be ? IteratorStepValue(iteratorRecord).
        //     b. If next is done, return set.
        while (try iterator.stepValue()) |next| {
            // c. Let status be Completion(Call(adder, set, « next »)).
            _ = adder.callAssumeCallable(Value.from(set), &.{next}) catch |err| {
                // d. IfAbruptCloseIterator(status, iteratorRecord).
                return iterator.close(@as(Agent.Error!Value, err));
            };
        }

        return Value.from(set);
    }

    /// 24.2.3.2 get Set [ @@species ]
    /// https://tc39.es/ecma262/#sec-get-set-@@species
    fn @"@@species"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 24.2.4 Properties of the Set Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-set-prototype-object
pub const SetPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "add", add, 1, realm);
        try defineBuiltinFunction(object, "clear", clear, 0, realm);
        try defineBuiltinFunction(object, "delete", delete, 1, realm);
        try defineBuiltinFunction(object, "difference", difference, 1, realm);
        try defineBuiltinFunction(object, "entries", entries, 0, realm);
        try defineBuiltinFunction(object, "forEach", forEach, 1, realm);
        try defineBuiltinFunction(object, "has", has, 1, realm);
        try defineBuiltinFunction(object, "intersection", intersection, 1, realm);
        try defineBuiltinFunction(object, "isDisjointFrom", isDisjointFrom, 1, realm);
        try defineBuiltinFunction(object, "isSubsetOf", isSubsetOf, 1, realm);
        try defineBuiltinFunction(object, "isSupersetOf", isSupersetOf, 1, realm);
        try defineBuiltinAccessor(object, "size", size, null, realm);
        try defineBuiltinFunction(object, "symmetricDifference", symmetricDifference, 1, realm);
        try defineBuiltinFunction(object, "union", @"union", 1, realm);
        try defineBuiltinFunction(object, "values", values, 0, realm);

        // 24.2.4.13 Set.prototype.keys ( )
        // https://tc39.es/ecma262/#sec-set.prototype.keys
        const @"%Set.prototype.values%" = object.propertyStorage().get(PropertyKey.from("values")).?;
        try defineBuiltinProperty(object, "keys", @"%Set.prototype.values%");

        // 24.2.4.18 Set.prototype [ @@iterator ] ( )
        // https://tc39.es/ecma262/#sec-set.prototype-@@iterator
        try defineBuiltinProperty(object, "@@iterator", @"%Set.prototype.values%");

        // 24.2.4.19 Set.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-set.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Set"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 24.2.4.1 Set.prototype.add ( value )
    /// https://tc39.es/ecma262/#sec-set.prototype.add
    fn add(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var value = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. Set value to CanonicalizeKeyedCollectionKey(value).
        value = value.canonicalizeKeyedCollectionKey();

        // 4. For each element e of S.[[SetData]], do
        //     a. If e is not empty and e is value, then
        //         i. Return S.
        // 5. Append value to S.[[SetData]].
        const result = try set.fields.set_data.getOrPut(value);
        if (!result.found_existing) {
            result.value_ptr.* = {};
            if (set.fields.iterable_values) |*iterable_values| {
                try iterable_values.append(value);
            }
        }

        // 6. Return S.
        return Value.from(set.object());
    }

    /// 24.2.4.2 Set.prototype.clear ( )
    /// https://tc39.es/ecma262/#sec-set.prototype.clear
    fn clear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. For each element e of S.[[SetData]], do
        //     a. Replace the element of S.[[SetData]] whose value is e with an element whose value
        //        is empty.
        set.fields.set_data.clearAndFree();
        if (set.fields.iterable_values) |*iterable_values| {
            iterable_values.clearAndFree();
        }

        // 4. Return undefined.
        return .undefined;
    }

    /// 24.2.4.4 Set.prototype.delete ( value )
    /// https://tc39.es/ecma262/#sec-set.prototype.delete
    fn delete(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var value = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. Set value to CanonicalizeKeyedCollectionKey(value).
        value = value.canonicalizeKeyedCollectionKey();

        // 4. For each element e of S.[[SetData]], do
        //     a. If e is not empty and e is value, then
        //         i. Replace the element of S.[[SetData]] whose value is e with an element whose
        //            value is empty.
        //         ii. Return true.
        if (set.fields.set_data.getIndex(value)) |index| {
            set.fields.set_data.orderedRemoveAt(index);
            if (set.fields.iterable_values) |*iterable_values| {
                iterable_values.items[index] = null;
            }
            return Value.from(true);
        }

        // 5. Return false.
        return Value.from(false);
    }

    /// 24.2.4.5 Set.prototype.difference ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.difference
    pub fn difference(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const object = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. Let resultSetData be a copy of O.[[SetData]].
        var result_set_data = try object.fields.set_data.clone();

        // 5. If SetDataSize(O.[[SetData]]) ≤ otherRec.[[Size]], then
        if (setDataSize(object.fields.set_data) <= other_rec.size) {
            var indexes_to_remove = std.ArrayList(usize).init(agent.gc_allocator);
            defer indexes_to_remove.deinit();

            // a. Let thisSize be the number of elements in O.[[SetData]].
            const this_size = object.fields.set_data.count();

            // b. Let index be 0.
            var index: usize = 0;

            // c. Repeat, while index < thisSize,
            while (index < this_size) : (index += 1) {
                // i. Let e be resultSetData[index].
                const element = result_set_data.keys()[index];

                // ii. If e is not empty, then
                // 1. Let inOther be ToBoolean(? Call(otherRec.[[Has]], otherRec.[[Set]], « e »)).
                const in_other = (try Value.from(other_rec.has).callAssumeCallable(
                    Value.from(other_rec.set),
                    &.{element},
                )).toBoolean();

                // 2. If inOther is true, then
                if (in_other) {
                    // a. Set resultSetData[index] to empty.
                    try indexes_to_remove.append(index);
                }

                // iii. Set index to index + 1.
            }

            std.mem.reverse(usize, indexes_to_remove.items);
            for (indexes_to_remove.items) |i| result_set_data.orderedRemoveAt(i);
        }
        // 6. Else,
        else {
            // a. Let keysIter be ? GetIteratorFromMethod(otherRec.[[Set]], otherRec.[[Keys]]).
            var keys_iter = try getIteratorFromMethod(
                agent,
                Value.from(other_rec.set),
                other_rec.keys,
            );

            // b. Let next be not-started.
            // c. Repeat, while next is not done,
            //     i. Set next to ? IteratorStepValue(keysIter).
            //     ii. If next is not done, then
            while (try keys_iter.stepValue()) |next_| {
                // 1. Set next to CanonicalizeKeyedCollectionKey(next).
                const next = next_.canonicalizeKeyedCollectionKey();

                // 2. Let valueIndex be SetDataIndex(resultSetData, next).
                const maybe_value_index = setDataIndex(result_set_data, next);

                // 3. If valueIndex is not not-found, then
                if (maybe_value_index) |value_index| {
                    // a. Set resultSetData[valueIndex] to empty.
                    result_set_data.orderedRemoveAt(value_index);
                }
            }
        }

        // 7. Let result be OrdinaryObjectCreate(%Set.prototype%, « [[SetData]] »).
        const result = try ordinaryObjectCreateWithType(
            Set,
            agent,
            try agent.currentRealm().intrinsics.@"%Set.prototype%"(),
            .{
                // 8. Set result.[[SetData]] to resultSetData.
                .set_data = result_set_data,
            },
        );

        // 9. Return result.
        return Value.from(result);
    }

    /// 24.2.4.6 Set.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-set.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateSetIterator(S, key+value).
        return Value.from(try createSetIterator(agent, map, .@"key+value"));
    }

    /// 24.2.4.7 Set.prototype.forEach ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-set.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 4. Let entries be S.[[SetData]].
        const iterable_values = try set.fields.registerIterator();
        defer set.fields.unregisterIterator();

        // 5. Let numEntries be the number of elements in entries.
        var num_entries = iterable_values.items.len;

        // 6. Let index be 0.
        var index: usize = 0;

        // 7. Repeat, while index < numEntries,
        while (index < num_entries) : (index += 1) {
            // a. Let e be entries[index].
            // b. Set index to index + 1.
            // c. If e is not empty, then
            if (iterable_values.items[index]) |value| {
                // i. Perform ? Call(callbackfn, thisArg, « e, e, S »).
                _ = try callback_fn.callAssumeCallable(
                    this_arg,
                    &.{ value, value, Value.from(set.object()) },
                );

                // ii. NOTE: The number of elements in entries may have increased during execution
                //     of callbackfn.
                // iii. Set numEntries to the number of elements in entries.
                num_entries = iterable_values.items.len;
            }
        }

        // 8. Return undefined.
        return .undefined;
    }

    /// 24.2.4.8 Set.prototype.has ( value )
    /// https://tc39.es/ecma262/#sec-set.prototype.has
    fn has(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var value = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. Set value to CanonicalizeKeyedCollectionKey(value).
        value = value.canonicalizeKeyedCollectionKey();

        // 4. For each element e of S.[[SetData]], do
        //     a. If e is not empty and e is value, return true.
        // 5. Return false.
        return Value.from(set.fields.set_data.contains(value));
    }

    /// 24.2.4.9 Set.prototype.intersection ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.intersection
    fn intersection(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const object = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. Let resultSetData be a new empty List.
        var result_set_data = SetData.init(agent.gc_allocator);

        // 5. If SetDataSize(O.[[SetData]]) ≤ otherRec.[[Size]], then
        if (setDataSize(object.fields.set_data) <= other_rec.size) {
            // a. Let thisSize be the number of elements in O.[[SetData]].
            var this_size = object.fields.set_data.count();

            // b. Let index be 0.
            var index: usize = 0;

            // c. Repeat, while index < thisSize,
            while (index < this_size) : (index += 1) {
                // i. Let e be O.[[SetData]][index].
                const element = object.fields.set_data.keys()[index];

                // ii. Set index to index + 1.
                // iii. If e is not empty, then

                // 1. Let inOther be ToBoolean(? Call(otherRec.[[Has]], otherRec.[[Set]], « e »)).
                const in_other = (try Value.from(other_rec.has).callAssumeCallable(
                    Value.from(other_rec.set),
                    &.{element},
                )).toBoolean();

                // 2. If inOther is true, then
                if (in_other) {
                    // a. NOTE: It is possible for earlier calls to otherRec.[[Has]] to remove and
                    //    re-add an element of O.[[SetData]], which can cause the same element to
                    //    be visited twice during this iteration.
                    // b. Let alreadyInResult be SetDataHas(resultSetData, e).
                    // c. If alreadyInResult is false, then
                    //     i. Append e to resultSetData.
                    // NOTE: We do not need to check because put allows clobbers.
                    try result_set_data.put(element, {});
                }

                // 3. NOTE: The number of elements in O.[[SetData]] may have increased during
                //    execution of otherRec.[[Has]].
                // 4. Set thisSize to the number of elements in O.[[SetData]].
                this_size = object.fields.set_data.count();
            }
        }
        // 6. Else,
        else {
            // a. Let keysIter be ? GetIteratorFromMethod(otherRec.[[Set]], otherRec.[[Keys]]).
            var keys_iter = try getIteratorFromMethod(
                agent,
                Value.from(other_rec.set),
                other_rec.keys,
            );

            // b. Let next be not-started.
            // c. Repeat, while next is not done,
            //     i. Set next to ? IteratorStepValue(keysIter).
            //     ii. If next is not done, then
            while (try keys_iter.stepValue()) |next_| {
                // 1. Set next to CanonicalizeKeyedCollectionKey(next).
                const next = next_.canonicalizeKeyedCollectionKey();

                // 2. NOTE: Because other is an arbitrary object, it is possible for its "keys"
                //    iterator to produce the same value more than once.

                // 3. Let alreadyInResult be SetDataHas(resultSetData, next).
                const already_in_result = setDataHas(result_set_data, next);

                // 4. Let inThis be SetDataHas(O.[[SetData]], next).
                const in_this = object.fields.set_data.contains(next);

                // 5. If alreadyInResult is false and inThis is true, then
                if (!already_in_result and in_this) {
                    // a. Append next to resultSetData.
                    try result_set_data.put(next, {});
                }
            }
        }

        // 7. Let result be OrdinaryObjectCreate(%Set.prototype%, « [[SetData]] »).
        const result = try ordinaryObjectCreateWithType(
            Set,
            agent,
            try agent.currentRealm().intrinsics.@"%Set.prototype%"(),
            .{
                // 8. Set result.[[SetData]] to resultSetData.
                .set_data = result_set_data,
            },
        );

        // 9. Return result.
        return Value.from(result);
    }

    /// 24.2.4.10 Set.prototype.isDisjointFrom ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.isdisjointfrom
    pub fn isDisjointFrom(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const object = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. If SetDataSize(O.[[SetData]]) ≤ otherRec.[[Size]], then
        if (setDataSize(object.fields.set_data) <= other_rec.size) {
            // a. Let thisSize be the number of elements in O.[[SetData]].
            var this_size = object.fields.set_data.count();

            // b. Let index be 0.
            var index: usize = 0;

            // c. Repeat, while index < thisSize,
            while (index < this_size) : (index += 1) {
                // i. Let e be O.[[SetData]][index].
                const element = object.fields.set_data.keys()[index];

                // ii. Set index to index + 1.
                // iii. If e is not empty, then

                // 1. Let inOther be ToBoolean(? Call(otherRec.[[Has]], otherRec.[[Set]], « e »)).
                const in_other = (try Value.from(other_rec.has).callAssumeCallable(
                    Value.from(other_rec.set),
                    &.{element},
                )).toBoolean();

                // 2. If inOther is true, return false.
                if (in_other) {
                    return Value.from(false);
                }

                // 3. NOTE: The number of elements in O.[[SetData]] may have increased during
                //    execution of otherRec.[[Has]].
                // 4. Set thisSize to the number of elements in O.[[SetData]].
                this_size = object.fields.set_data.count();
            }
        }
        // 5. Else,
        else {
            // a. Let keysIter be ? GetIteratorFromMethod(otherRec.[[Set]], otherRec.[[Keys]]).
            var keys_iter = try getIteratorFromMethod(
                agent,
                Value.from(other_rec.set),
                other_rec.keys,
            );

            // b. Let next be not-started.
            // c. Repeat, while next is not done,
            //     i. Set next to ? IteratorStepValue(keysIter).
            //     ii. If next is not done, then
            while (try keys_iter.stepValue()) |next| {
                // 1. If SetDataHas(O.[[SetData]], next) is true, then
                if (setDataHas(object.fields.set_data, next)) {
                    // a. Perform ? IteratorClose(keysIter, NormalCompletion(unused)).
                    try keys_iter.close(@as(Agent.Error!void, {}));

                    // b. Return false.
                    return Value.from(false);
                }
            }
        }

        // 6. Return true.
        return Value.from(true);
    }

    /// 24.2.4.11 Set.prototype.isSubsetOf ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.issubsetof
    pub fn isSubsetOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const object = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. If SetDataSize(O.[[SetData]]) > otherRec.[[Size]], return false.
        if (setDataSize(object.fields.set_data) > other_rec.size) {
            return Value.from(false);
        }

        // 5. Let thisSize be the number of elements in O.[[SetData]].
        var this_size = object.fields.set_data.count();

        // 6. Let index be 0.
        var index: usize = 0;

        // 7. Repeat, while index < thisSize,
        while (index < this_size) : (index += 1) {
            // a. Let e be O.[[SetData]][index].
            const element = object.fields.set_data.keys()[index];

            // b. Set index to index + 1.
            // c. If e is not empty, then

            // i. Let inOther be ToBoolean(? Call(otherRec.[[Has]], otherRec.[[Set]], « e »)).
            const in_other = (try Value.from(other_rec.has).callAssumeCallable(
                Value.from(other_rec.set),
                &.{element},
            )).toBoolean();

            // ii. If inOther is false, return false.
            if (!in_other) {
                return Value.from(false);
            }

            // iii. NOTE: The number of elements in O.[[SetData]] may have increased during
            //      execution of otherRec.[[Has]].
            // iv. Set thisSize to the number of elements in O.[[SetData]].
            this_size = object.fields.set_data.count();
        }

        // 8. Return true.
        return Value.from(true);
    }

    /// 24.2.4.12 Set.prototype.isSupersetOf ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.issupersetof
    pub fn isSupersetOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const object = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. If SetDataSize(O.[[SetData]]) < otherRec.[[Size]], return false.
        if (setDataSize(object.fields.set_data) < other_rec.size) {
            return Value.from(false);
        }

        // 5. Let keysIter be ? GetIteratorFromMethod(otherRec.[[Set]], otherRec.[[Keys]]).
        var keys_iter = try getIteratorFromMethod(
            agent,
            Value.from(other_rec.set),
            other_rec.keys,
        );

        // 6. Let next be not-started.
        // 7. Repeat, while next is not done,
        //     a. Set next to ? IteratorStepValue(keysIter).
        //     b. If next is not done, then
        while (try keys_iter.stepValue()) |next| {
            // i. If SetDataHas(O.[[SetData]], next) is false, then
            if (!setDataHas(object.fields.set_data, next)) {
                // 1. Perform ? IteratorClose(keysIter, NormalCompletion(unused)).
                try keys_iter.close(@as(Agent.Error!void, {}));

                // 2. Return false.
                return Value.from(false);
            }
        }

        // 8. Return true.
        return Value.from(true);
    }

    /// 24.2.4.14 get Set.prototype.size
    /// https://tc39.es/ecma262/#sec-get-set.prototype.size
    fn size(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. Let size be SetDataSize(S.[[SetData]]).
        const size_ = setDataSize(set.fields.set_data);

        // 4. Return 𝔽(size).
        return Value.from(@as(u53, @intCast(size_)));
    }

    /// 24.2.4.15 Set.prototype.symmetricDifference ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.symmetricdifference
    pub fn symmetricDifference(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const object = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. Let keysIter be ? GetIteratorFromMethod(otherRec.[[Set]], otherRec.[[Keys]]).
        var keys_iter = try getIteratorFromMethod(
            agent,
            Value.from(other_rec.set),
            other_rec.keys,
        );

        // 5. Let resultSetData be a copy of O.[[SetData]].
        var result_set_data = try object.fields.set_data.clone();

        // 6. Let next be not-started.
        // 7. Repeat, while next is not done,
        //     a. Set next to ? IteratorStepValue(keysIter).
        //     b. If next is not done, then
        while (try keys_iter.stepValue()) |next_| {
            // i. Set next to CanonicalizeKeyedCollectionKey(next).
            const next = next_.canonicalizeKeyedCollectionKey();

            // ii. Let resultIndex be SetDataIndex(resultSetData, next).
            const maybe_result_index = setDataIndex(result_set_data, next);

            // iii. If SetDataHas(O.[[SetData]], next) is true, then
            if (setDataHas(object.fields.set_data, next)) {
                // 1. If resultIndex is not not-found, set resultSetData[resultIndex] to empty.
                if (maybe_result_index) |result_index| {
                    result_set_data.orderedRemoveAt(result_index);
                }
            }
            // v. Else,
            else {
                // 1. If resultIndex is not-found, append next to resultSetData.
                if (maybe_result_index == null) {
                    try result_set_data.put(next, {});
                }
            }
        }

        // 8. Let result be OrdinaryObjectCreate(%Set.prototype%, « [[SetData]] »).
        const result = try ordinaryObjectCreateWithType(
            Set,
            agent,
            try agent.currentRealm().intrinsics.@"%Set.prototype%"(),
            .{
                // 9. Set result.[[SetData]] to resultSetData.
                .set_data = result_set_data,
            },
        );

        // 10. Return result.
        return Value.from(result);
    }

    /// 24.2.4.16 Set.prototype.union ( other )
    /// https://tc39.es/ecma262/#sec-set.prototype.union
    fn @"union"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. Let otherRec be ? GetSetRecord(other).
        const other_rec = try getSetRecord(agent, other);

        // 4. Let keysIter be ? GetIteratorFromMethod(otherRec.[[Set]], otherRec.[[Keys]]).
        var keys_iter = try getIteratorFromMethod(
            agent,
            Value.from(other_rec.set),
            other_rec.keys,
        );

        // 5. Let resultSetData be a copy of O.[[SetData]].
        var result_set_data = try set.fields.set_data.clone();

        // 6. Let next be not-started.
        // 7. Repeat, while next is not done,
        //     a. Set next to ? IteratorStepValue(keysIter).
        //     b. If next is not done, then
        while (try keys_iter.stepValue()) |next_| {
            // i. Set next to CanonicalizeKeyedCollectionKey(next).
            const next = next_.canonicalizeKeyedCollectionKey();

            // ii. If SetDataHas(resultSetData, next) is false, then
            //     1. Append next to resultSetData.
            // NOTE: We do not need to check because put allows clobbers.
            try result_set_data.put(next, {});
        }

        // 8. Let result be OrdinaryObjectCreate(%Set.prototype%, « [[SetData]] »).
        const result = try ordinaryObjectCreateWithType(
            Set,
            agent,
            try agent.currentRealm().intrinsics.@"%Set.prototype%"(),
            .{
                // 9. Set result.[[SetData]] to resultSetData.
                .set_data = result_set_data,
            },
        );

        // 10. Return result.
        return Value.from(result);
    }

    /// 24.2.4.17 Set.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-set.prototype.values
    fn values(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateSetIterator(S, value).
        return Value.from(try createSetIterator(agent, map, .value));
    }
};

const SetData = ValueArrayHashMap(void, sameValue);
const IterableValues = std.ArrayList(?Value);

/// 24.2.5 Properties of Set Instances
/// https://tc39.es/ecma262/#sec-properties-of-set-instances
pub const Set = MakeObject(.{
    .Fields = struct {
        const Self = @This();

        /// [[SetData]]
        set_data: SetData,

        /// List of values and their deletion status for SetIterator and Set.prototype.forEach(),
        /// created and destroyed on demand.
        iterable_values: ?IterableValues = null,
        active_iterators: usize = 0,

        pub fn registerIterator(self: *Self) Allocator.Error!*IterableValues {
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_values == null);
                self.iterable_values = try IterableValues.initCapacity(
                    self.set_data.allocator,
                    self.set_data.count(),
                );
                for (self.set_data.keys()) |value| {
                    self.iterable_values.?.appendAssumeCapacity(value);
                }
            }
            self.active_iterators += 1;
            return &self.iterable_values.?;
        }

        pub fn unregisterIterator(self: *Self) void {
            self.active_iterators -= 1;
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_values != null);
                self.iterable_values.?.deinit();
                self.iterable_values = null;
            }
        }
    },
    .tag = .set,
});
