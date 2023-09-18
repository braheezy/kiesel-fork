//! 10.4.2 Array Exotic Objects
//! https://tc39.es/ecma262/#sec-array-exotic-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const ordinary = @import("ordinary.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Iterator = types.Iterator;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createArrayIterator = builtins.createArrayIterator;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIteratorFromMethod = types.getIteratorFromMethod;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const isStrictlyEqual = types.isStrictlyEqual;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = ordinary.ordinaryDefineOwnProperty;
const ordinaryGetOwnProperty = ordinary.ordinaryGetOwnProperty;
const ordinaryObjectCreate = ordinary.ordinaryObjectCreate;
const sameValueZero = types.sameValueZero;

// Non-standard helper to get the length property of an array
pub fn getArrayLength(array: Object) u32 {
    const property_descriptor = array.data.property_storage.get(PropertyKey.from("length")).?;
    const value = property_descriptor.value.?;
    return @intFromFloat(value.number.asFloat());
}

/// 10.4.2.1 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-array-exotic-objects-defineownproperty-p-desc
fn defineOwnProperty(
    array: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) !bool {
    const agent = array.agent();

    // 1. If P is "length", then
    if (property_key == .string and property_key.string.eql(String.from("length"))) {
        // a. Return ? ArraySetLength(A, Desc).
        return arraySetLength(agent, array, property_descriptor);
    }

    // 2. Else if P is an array index, then
    else if (property_key.isArrayIndex()) {
        // a. Let lengthDesc be OrdinaryGetOwnProperty(A, "length").
        var length_descriptor = ordinaryGetOwnProperty(array, PropertyKey.from("length")).?;

        // b. Assert: IsDataDescriptor(lengthDesc) is true.
        std.debug.assert(length_descriptor.isDataDescriptor());

        // c. Assert: lengthDesc.[[Configurable]] is false.
        std.debug.assert(length_descriptor.configurable == false);

        // d. Let length be lengthDesc.[[Value]].
        const length_value = length_descriptor.value.?;

        // e. Assert: length is a non-negative integral Number.
        const length = length_value.number.asFloat();
        std.debug.assert(std.math.isFinite(length) and std.math.trunc(length) == length);

        // f. Let index be ! ToUint32(P).
        const index: f64 = @floatFromInt(property_key.integer_index);

        // g. If index ≥ length and lengthDesc.[[Writable]] is false, return false.
        if (index >= length and length_descriptor.writable == false)
            return false;

        // h. Let succeeded be ! OrdinaryDefineOwnProperty(A, P, Desc).
        var succeeded = ordinaryDefineOwnProperty(
            array,
            property_key,
            property_descriptor,
        ) catch |err| try noexcept(err);

        // i. If succeeded is false, return false.
        if (!succeeded)
            return false;

        // j. If index ≥ length, then
        if (index >= length) {
            // i. Set lengthDesc.[[Value]] to index + 1𝔽.
            length_descriptor.value = Value.from(index + 1);

            // ii. Set succeeded to ! OrdinaryDefineOwnProperty(A, "length", lengthDesc).
            succeeded = ordinaryDefineOwnProperty(
                array,
                PropertyKey.from("length"),
                length_descriptor,
            ) catch |err| try noexcept(err);

            // iii. Assert: succeeded is true.
            std.debug.assert(succeeded);
        }

        // k. Return true.
        return true;
    }

    // 3. Return ? OrdinaryDefineOwnProperty(A, P, Desc).
    return ordinaryDefineOwnProperty(array, property_key, property_descriptor);
}

/// 10.4.2.2 ArrayCreate ( length [ , proto ] )
/// https://tc39.es/ecma262/#sec-arraycreate
pub fn arrayCreate(agent: *Agent, length: u64, maybe_prototype: ?Object) !Object {
    const realm = agent.currentRealm();

    // 1. If length > 2^32 - 1, throw a RangeError exception.
    if (length >= std.math.maxInt(u32)) return agent.throwException(
        .range_error,
        "Invalid array length",
    );

    // 2. If proto is not present, set proto to %Array.prototype%.
    const prototype = maybe_prototype orelse try realm.intrinsics.@"%Array.prototype%"();

    // 3. Let A be MakeBasicObject(« [[Prototype]], [[Extensible]] »).
    const array = try Array.create(agent, .{
        // 4. Set A.[[Prototype]] to proto.
        .prototype = prototype,

        // 5. Set A.[[DefineOwnProperty]] as specified in 10.4.2.1.
        .internal_methods = .{
            .defineOwnProperty = defineOwnProperty,
        },
    });

    // 6. Perform ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    _ = ordinaryDefineOwnProperty(array, PropertyKey.from("length"), .{
        .value = Value.from(length),
        .writable = true,
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 7. Return A.
    return array;
}

/// 10.4.2.3 ArraySpeciesCreate ( originalArray, length )
/// https://tc39.es/ecma262/#sec-arrayspeciescreate
pub fn arraySpeciesCreate(agent: *Agent, original_array: Object, length: u64) !Object {
    // 1. Let isArray be ? IsArray(originalArray).
    const is_array = try Value.from(original_array).isArray();

    // 2. If isArray is false, return ? ArrayCreate(length).
    if (!is_array) return arrayCreate(agent, length, null);

    // 3. Let C be ? Get(originalArray, "constructor").
    var constructor = try original_array.get(PropertyKey.from("constructor"));

    // 4. If IsConstructor(C) is true, then
    if (constructor.isConstructor()) {
        // a. Let thisRealm be the current Realm Record.
        const this_realm = agent.currentRealm();

        // b. Let realmC be ? GetFunctionRealm(C).
        const constructor_realm = try constructor.object.getFunctionRealm();

        // c. If thisRealm and realmC are not the same Realm Record, then
        if (this_realm != constructor_realm) {
            // i. If SameValue(C, realmC.[[Intrinsics]].[[%Array%]]) is true, set C to undefined.
            if (constructor.object.sameValue(try constructor_realm.intrinsics.@"%Array%"())) {
                constructor = .undefined;
            }
        }
    }

    // 5. If C is an Object, then
    if (constructor == .object) {
        // a. Set C to ? Get(C, @@species).
        constructor = try constructor.get(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@species"),
        );

        // b. If C is null, set C to undefined.
        if (constructor == .null) constructor = .undefined;
    }

    // 6. If C is undefined, return ? ArrayCreate(length).
    if (constructor == .undefined) return arrayCreate(agent, length, null);

    // 7. If IsConstructor(C) is false, throw a TypeError exception.
    if (!constructor.isConstructor()) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not a constructor", .{constructor}),
        );
    }

    // 8. Return ? Construct(C, « 𝔽(length) »).
    return constructor.object.construct(.{Value.from(length)}, null);
}

/// 10.4.2.4 ArraySetLength ( A, Desc )
/// https://tc39.es/ecma262/#sec-arraysetlength
pub fn arraySetLength(agent: *Agent, array: Object, property_descriptor: PropertyDescriptor) !bool {
    // 1. If Desc does not have a [[Value]] field, then
    if (property_descriptor.value == null) {
        // a. Return ! OrdinaryDefineOwnProperty(A, "length", Desc).
        return ordinaryDefineOwnProperty(
            array,
            PropertyKey.from("length"),
            property_descriptor,
        ) catch |err| try noexcept(err);
    }

    // 2. Let newLenDesc be a copy of Desc.
    var new_len_desc = property_descriptor;

    // 3. Let newLen be ? ToUint32(Desc.[[Value]]).
    const new_len = try property_descriptor.value.?.toUint32(agent);

    // 4. Let numberLen be ? ToNumber(Desc.[[Value]]).
    const number_len = try property_descriptor.value.?.toNumber(agent);

    // 5. If SameValueZero(newLen, numberLen) is false, throw a RangeError exception.
    if (@as(f64, @floatFromInt(new_len)) != number_len.asFloat()) return agent.throwException(
        .range_error,
        "Invalid array length",
    );

    // 6. Set newLenDesc.[[Value]] to newLen.
    new_len_desc.value = Value.from(new_len);

    // 7. Let oldLenDesc be OrdinaryGetOwnProperty(A, "length").
    const old_len_desc = ordinaryGetOwnProperty(array, PropertyKey.from("length")).?;

    // 8. Assert: IsDataDescriptor(oldLenDesc) is true.
    std.debug.assert(old_len_desc.isDataDescriptor());

    // 9. Assert: oldLenDesc.[[Configurable]] is false.
    std.debug.assert(old_len_desc.configurable == false);

    // 10. Let oldLen be oldLenDesc.[[Value]].
    const old_len: u32 = @intFromFloat(old_len_desc.value.?.number.asFloat());

    // 11. If newLen ≥ oldLen, then
    if (new_len >= old_len) {
        // a. Return ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
        return ordinaryDefineOwnProperty(
            array,
            PropertyKey.from("length"),
            new_len_desc,
        ) catch |err| try noexcept(err);
    }

    // 12. If oldLenDesc.[[Writable]] is false, return false.
    if (old_len_desc.writable == false) return false;

    var new_writable: bool = undefined;

    // 13. If newLenDesc does not have a [[Writable]] field or newLenDesc.[[Writable]] is true, then
    if (new_len_desc.writable == null or new_len_desc.writable == true) {
        // a. Let newWritable be true.
        new_writable = true;
    }
    // 14. Else,
    else {
        // a. NOTE: Setting the [[Writable]] attribute to false is deferred in case any elements
        //          cannot be deleted.
        // b. Let newWritable be false.
        new_writable = false;

        // c. Set newLenDesc.[[Writable]] to true.
        new_len_desc.writable = true;
    }

    // 15. Let succeeded be ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    var succeeded = ordinaryDefineOwnProperty(
        array,
        PropertyKey.from("length"),
        new_len_desc,
    ) catch |err| try noexcept(err);

    // 16. If succeeded is false, return false.
    if (!succeeded) return false;

    // 17. For each own property key P of A such that P is an array index and ! ToUint32(P) ≥ newLen,
    //     in descending numeric index order, do
    // NOTE: Deletion invalidates the ArrayHashMap.keys() array, so we have to make a copy
    var indices = std.ArrayList(u32).init(agent.gc_allocator);
    defer indices.deinit();
    for (array.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key.isArrayIndex() and property_key.integer_index >= new_len) {
            try indices.append(@as(u32, @intCast(property_key.integer_index)));
        }
    }
    std.sort.insertion(u32, indices.items, {}, std.sort.desc(u32));
    for (indices.items) |index| {
        // a. Let deleteSucceeded be ! A.[[Delete]](P).
        const delete_succeeded = array.internalMethods().delete(
            array,
            PropertyKey.from(@as(u53, index)),
        ) catch |err| try noexcept(err);

        // b. If deleteSucceeded is false, then
        if (!delete_succeeded) {
            // i. Set newLenDesc.[[Value]] to ! ToUint32(P) + 1𝔽.
            new_len_desc.value = Value.from(@as(f64, @floatFromInt(index)) + 1);

            // ii. If newWritable is false, set newLenDesc.[[Writable]] to false.
            if (!new_writable) new_len_desc.writable = false;

            // iii. Perform ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
            _ = ordinaryDefineOwnProperty(
                array,
                PropertyKey.from("length"),
                new_len_desc,
            ) catch |err| try noexcept(err);

            // iv. Return false.
            return false;
        }
    }

    // 18. If newWritable is false, then
    if (!new_writable) {
        // a. Set succeeded to ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor {
        //      [[Writable]]: false
        //    }).
        succeeded = ordinaryDefineOwnProperty(
            array,
            PropertyKey.from("length"),
            .{ .writable = false },
        ) catch |err| try noexcept(err);

        // b. Assert: succeeded is true.
        std.debug.assert(succeeded);
    }

    // 19. Return true.
    return true;
}

/// 23.1.2 Properties of the Array Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-array-constructor
pub const ArrayConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Array",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "from", from, 1, realm);
        try defineBuiltinFunction(object, "isArray", isArray, 1, realm);
        try defineBuiltinFunction(object, "of", of, 0, realm);

        // 23.1.2.4 Array.prototype
        // https://tc39.es/ecma262/#sec-array.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Array.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 23.1.2.5 get Array [ @@species ]
        // https://tc39.es/ecma262/#sec-get-array-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        // 23.1.3.3 Array.prototype.constructor
        // https://tc39.es/ecma262/#sec-array.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Array.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 23.1.1.1 Array ( ...values )
    /// https://tc39.es/ecma262/#sec-array
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, maybe_new_target: ?Object) !Value {

        // 1. If NewTarget is undefined, let newTarget be the active function object; else let newTarget be NewTarget.
        const new_target = maybe_new_target orelse agent.activeFunctionObject();

        // 2. Let proto be ? GetPrototypeFromConstructor(newTarget, "%Array.prototype%").
        const prototype = try getPrototypeFromConstructor(new_target, "%Array.prototype%");

        // 3. Let numberOfArgs be the number of elements in values.
        const number_of_args = arguments.count();

        // 4. If numberOfArgs = 0, then
        if (number_of_args == 0) {
            // a. Return ! ArrayCreate(0, proto).
            return Value.from(arrayCreate(agent, 0, prototype) catch |err| try noexcept(err));
        }
        // 5. Else if numberOfArgs = 1, then
        else if (number_of_args == 1) {
            // a. Let len be values[0].
            const len = arguments.get(0);

            // b. Let array be ! ArrayCreate(0, proto).
            const array = arrayCreate(agent, 0, prototype) catch |err| try noexcept(err);

            var int_len: u32 = undefined;

            // c. If len is not a Number, then
            if (len != .number) {
                // i. Perform ! CreateDataPropertyOrThrow(array, "0", len).
                array.createDataPropertyOrThrow(
                    PropertyKey.from(0),
                    len,
                ) catch |err| try noexcept(err);

                // ii. Let intLen be 1𝔽.
                int_len = 1;
            }
            // d. Else,
            else {
                // i. Let intLen be ! ToUint32(len).
                int_len = len.toUint32(agent) catch unreachable;

                // ii. If SameValueZero(intLen, len) is false, throw a RangeError exception.
                if (@as(f64, @floatFromInt(int_len)) != len.number.asFloat()) return agent.throwException(
                    .range_error,
                    "Invalid array length",
                );
            }

            // e. Perform ! Set(array, "length", intLen, true).
            _ = array.set(
                PropertyKey.from("length"),
                Value.from(int_len),
                .throw,
            ) catch |err| try noexcept(err);

            // f. Return array.
            return Value.from(array);
        }
        // 6. Else,
        else {
            // a. Assert: numberOfArgs ≥ 2.
            std.debug.assert(number_of_args >= 2);

            // b. Let array be ? ArrayCreate(numberOfArgs, proto).
            const array = try arrayCreate(agent, number_of_args, prototype);

            // c. Let k be 0.
            // d. Repeat, while k < numberOfArgs,
            for (arguments.values, 0..) |item_k, k| {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(k)));

                // ii. Let itemK be values[k].
                // iii. Perform ! CreateDataPropertyOrThrow(array, Pk, itemK).
                array.createDataPropertyOrThrow(property_key, item_k) catch |err| try noexcept(err);

                // iv. Set k to k + 1.
            }

            // e. Assert: The mathematical value of array's "length" property is numberOfArgs.
            std.debug.assert(getArrayLength(array) == @as(u32, @intCast(number_of_args)));

            // f. Return array.
            return Value.from(array);
        }
    }

    /// 23.1.2.1 Array.from ( items [ , mapfn [ , thisArg ] ] )
    /// https://tc39.es/ecma262/#sec-array.from
    fn from(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const items = arguments.get(0);
        const map_fn = arguments.get(1);
        const this_arg = arguments.get(2);

        // 1. Let C be the this value.
        const constructor = this_value;

        // 2. If mapfn is undefined, then
        const mapping = if (map_fn == .undefined) blk: {
            // a. Let mapping be false.
            break :blk false;
        }
        // 3. Else,
        else blk: {
            // a. If IsCallable(mapfn) is false, throw a TypeError exception.
            if (!map_fn.isCallable()) {
                return agent.throwException(
                    .type_error,
                    try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{map_fn}),
                );
            }

            // b. Let mapping be true.
            break :blk true;
        };

        // 4. Let usingIterator be ? GetMethod(items, @@iterator).
        const using_iterator = try items.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@iterator"),
        );

        // 5. If usingIterator is not undefined, then
        if (using_iterator != null) {
            // a. If IsConstructor(C) is true, then
            const array = if (constructor.isConstructor()) blk: {
                // i. Let A be ? Construct(C).
                break :blk try constructor.object.construct(.{}, null);
            }
            // b. Else,
            else blk: {
                // i. Let A be ! ArrayCreate(0).
                break :blk try arrayCreate(agent, 0, null);
            };

            // c. Let iteratorRecord be ? GetIteratorFromMethod(items, usingIterator).
            const iterator = try getIteratorFromMethod(agent, items, using_iterator.?);

            // d. Let k be 0.
            var k: u53 = 0;

            // e. Repeat,
            while (true) : (k += 1) {
                // i. If k ≥ 2^53 - 1, then
                if (k == std.math.maxInt(u53)) {
                    // 1. Let error be ThrowCompletion(a newly created TypeError object).
                    const @"error" = agent.throwException(
                        .type_error,
                        "Maximum array length exceeded",
                    );

                    // 2. Return ? IteratorClose(iteratorRecord, error).
                    return iterator.close(@as(Agent.Error!Value, @"error"));
                }

                // ii. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k);

                // iii. Let next be ? IteratorStep(iteratorRecord).
                const next = try iterator.step();

                // iv. If next is false, then
                if (next == null) {
                    // 1. Perform ? Set(A, "length", 𝔽(k), true).
                    try array.set(PropertyKey.from("length"), Value.from(k), .throw);

                    // 2. Return A.
                    return Value.from(array);
                }

                // v. Let nextValue be ? IteratorValue(next).
                const next_value = try Iterator.value(next.?);

                // vi. If mapping is true, then
                const mapped_value = if (mapping) blk: {
                    // 1. Let mappedValue be Completion(Call(mapfn, thisArg, « nextValue, 𝔽(k) »)).
                    break :blk map_fn.callAssumeCallable(
                        this_arg,
                        .{ next_value, Value.from(k) },
                    ) catch |err| {
                        // 2. IfAbruptCloseIterator(mappedValue, iteratorRecord).
                        return iterator.close(@as(Agent.Error!Value, err));
                    };
                }
                // vii. Else,
                else blk: {
                    // 1. Let mappedValue be nextValue.
                    break :blk next_value;
                };

                // viii. Let defineStatus be Completion(CreateDataPropertyOrThrow(A, Pk, mappedValue)).
                _ = array.createDataPropertyOrThrow(property_key, mapped_value) catch |err| {
                    // ix. IfAbruptCloseIterator(defineStatus, iteratorRecord).
                    return iterator.close(@as(Agent.Error!Value, err));
                };

                // x. Set k to k + 1.
            }
        }

        // 6. NOTE: items is not an Iterable so assume it is an array-like object.
        // 7. Let arrayLike be ! ToObject(items).
        const array_like = items.toObject(agent) catch |err| try noexcept(err);

        // 8. Let len be ? LengthOfArrayLike(arrayLike).
        const len = try array_like.lengthOfArrayLike();

        // 9. If IsConstructor(C) is true, then
        const array = if (constructor.isConstructor()) blk: {
            // a. Let A be ? Construct(C, « 𝔽(len) »).
            break :blk try constructor.object.construct(.{Value.from(len)}, null);
        }
        // 10. Else,
        else blk: {
            // a. Let A be ? ArrayCreate(len).
            break :blk try arrayCreate(agent, len, null);
        };

        // 11. Let k be 0.
        var k: u53 = 0;

        // 12. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ? Get(arrayLike, Pk).
            const k_value = try array_like.get(property_key);

            // c. If mapping is true, then
            const mapped_value = if (mapping) blk: {
                // i. Let mappedValue be ? Call(mapfn, thisArg, « kValue, 𝔽(k) »).
                break :blk try map_fn.callAssumeCallable(this_arg, .{ k_value, Value.from(k) });
            }
            // d. Else,
            else blk: {
                // i. Let mappedValue be kValue.
                break :blk k_value;
            };

            // e. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
            try array.createDataPropertyOrThrow(property_key, mapped_value);

            // f. Set k to k + 1.
        }

        // 13. Perform ? Set(A, "length", 𝔽(len), true).
        try array.set(PropertyKey.from("length"), Value.from(len), .throw);

        // 14. Return A.
        return Value.from(array);
    }

    /// 23.1.2.2 Array.isArray ( arg )
    /// https://tc39.es/ecma262/#sec-array.isarray
    fn isArray(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const arg = arguments.get(0);

        // 1. Return ? IsArray(arg).
        return Value.from(try arg.isArray());
    }

    /// 23.1.2.3 Array.of ( ...items )
    /// https://tc39.es/ecma262/#sec-array.of
    fn of(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        // 1. Let len be the number of elements in items.
        const len = arguments.count();

        // 2. Let lenNumber be 𝔽(len).
        const len_number = Value.from(len);

        // 3. Let C be the this value.
        const constructor = this_value;

        // 4. If IsConstructor(C) is true, then
        const array = blk: {
            if (constructor.isConstructor()) {
                // a. Let A be ? Construct(C, « lenNumber »).
                break :blk try constructor.object.construct(.{len_number}, null);
            }
            // 5. Else,
            else {
                // a. Let A be ? ArrayCreate(len).
                break :blk try arrayCreate(agent, len, null);
            }
        };

        // 6. Let k be 0.
        // 7. Repeat, while k < len,
        for (arguments.values, 0..) |k_value, k| {
            // a. Let kValue be items[k].

            // b. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(k)));

            // c. Perform ? CreateDataPropertyOrThrow(A, Pk, kValue).
            try array.createDataPropertyOrThrow(property_key, k_value);

            // d. Set k to k + 1.
        }

        // 8. Perform ? Set(A, "length", lenNumber, true).
        try array.set(PropertyKey.from("length"), len_number, .throw);

        // 9. Return A.
        return Value.from(array);
    }
};

/// 23.1.3 Properties of the Array Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-array-prototype-object
pub const ArrayPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = arrayCreate(
            realm.agent,
            0,
            try realm.intrinsics.@"%Object.prototype%"(),
        ) catch |err| try noexcept(err);

        try defineBuiltinFunction(object, "at", at, 1, realm);
        try defineBuiltinFunction(object, "concat", concat, 1, realm);
        try defineBuiltinFunction(object, "copyWithin", copyWithin, 2, realm);
        try defineBuiltinFunction(object, "entries", entries, 0, realm);
        try defineBuiltinFunction(object, "every", every, 1, realm);
        try defineBuiltinFunction(object, "fill", fill, 1, realm);
        try defineBuiltinFunction(object, "filter", filter, 1, realm);
        try defineBuiltinFunction(object, "find", find, 1, realm);
        try defineBuiltinFunction(object, "findIndex", findIndex, 1, realm);
        try defineBuiltinFunction(object, "findLast", findLast, 1, realm);
        try defineBuiltinFunction(object, "findLastIndex", findLastIndex, 1, realm);
        try defineBuiltinFunction(object, "forEach", forEach, 1, realm);
        try defineBuiltinFunction(object, "includes", includes, 1, realm);
        try defineBuiltinFunction(object, "indexOf", indexOf, 1, realm);
        try defineBuiltinFunction(object, "join", join, 1, realm);
        try defineBuiltinFunction(object, "keys", keys, 0, realm);
        try defineBuiltinFunction(object, "lastIndexOf", lastIndexOf, 1, realm);
        try defineBuiltinFunction(object, "map", map, 1, realm);
        try defineBuiltinFunction(object, "pop", pop, 0, realm);
        try defineBuiltinFunction(object, "push", push, 1, realm);
        try defineBuiltinFunction(object, "reduce", reduce, 1, realm);
        try defineBuiltinFunction(object, "reduceRight", reduceRight, 1, realm);
        try defineBuiltinFunction(object, "shift", shift, 0, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);
        try defineBuiltinFunction(object, "some", some, 1, realm);
        try defineBuiltinFunction(object, "toLocaleString", toLocaleString, 0, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "unshift", unshift, 1, realm);
        try defineBuiltinFunction(object, "values", values, 0, realm);
        try defineBuiltinFunction(object, "with", with, 2, realm);

        // 23.1.3.40 Array.prototype [ @@iterator ] ( )
        // https://tc39.es/ecma262/#sec-array.prototype-@@iterator
        const @"%Array.prototype.values%" = object.propertyStorage().get(PropertyKey.from("values")).?;
        try defineBuiltinProperty(object, "@@iterator", @"%Array.prototype.values%");

        // 23.1.3.41 Array.prototype [ @@unscopables ]
        // https://tc39.es/ecma262/#sec-array.prototype-@@unscopables
        try defineBuiltinProperty(object, "@@unscopables", PropertyDescriptor{
            .value = blk: {
                // 1. Let unscopableList be OrdinaryObjectCreate(null).
                const unscopable_list = try ordinaryObjectCreate(realm.agent, null);

                // 2. Perform ! CreateDataPropertyOrThrow(unscopableList, "at", true).
                // 3. Perform ! CreateDataPropertyOrThrow(unscopableList, "copyWithin", true).
                // 4. Perform ! CreateDataPropertyOrThrow(unscopableList, "entries", true).
                // 5. Perform ! CreateDataPropertyOrThrow(unscopableList, "fill", true).
                // 6. Perform ! CreateDataPropertyOrThrow(unscopableList, "find", true).
                // 7. Perform ! CreateDataPropertyOrThrow(unscopableList, "findIndex", true).
                // 8. Perform ! CreateDataPropertyOrThrow(unscopableList, "findLast", true).
                // 9. Perform ! CreateDataPropertyOrThrow(unscopableList, "findLastIndex", true).
                // 10. Perform ! CreateDataPropertyOrThrow(unscopableList, "flat", true).
                // 11. Perform ! CreateDataPropertyOrThrow(unscopableList, "flatMap", true).
                // 12. Perform ! CreateDataPropertyOrThrow(unscopableList, "includes", true).
                // 13. Perform ! CreateDataPropertyOrThrow(unscopableList, "keys", true).
                // 14. Perform ! CreateDataPropertyOrThrow(unscopableList, "toReversed", true).
                // 15. Perform ! CreateDataPropertyOrThrow(unscopableList, "toSorted", true).
                // 16. Perform ! CreateDataPropertyOrThrow(unscopableList, "toSpliced", true).
                // 17. Perform ! CreateDataPropertyOrThrow(unscopableList, "values", true).
                inline for (.{
                    "at",         "copyWithin",    "entries",   "fill",    "find",     "findIndex",
                    "findLast",   "findLastIndex", "flat",      "flatMap", "includes", "keys",
                    "toReversed", "toSorted",      "toSpliced", "values",
                }) |name| {
                    unscopable_list.createDataPropertyOrThrow(
                        PropertyKey.from(name),
                        Value.from(true),
                    ) catch |err| try noexcept(err);
                }

                // 18. Return unscopableList.
                break :blk Value.from(unscopable_list);
            },
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 23.1.3.1 Array.prototype.at ( index )
    /// https://tc39.es/ecma262/#sec-array.prototype.at
    fn at(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const index = arguments.get(0);
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let relativeIndex be ? ToIntegerOrInfinity(index).
        const relative_index = try index.toIntegerOrInfinity(agent);

        // 4. If relativeIndex ≥ 0, then
        //     a. Let k be relativeIndex.
        // 5. Else,
        //     a. Let k be len + relativeIndex.
        const k_f64 = if (relative_index >= 0)
            relative_index
        else
            @as(f64, @floatFromInt(len)) + relative_index;

        // 6. If k < 0 or k ≥ len, return undefined.
        if (k_f64 < 0 or k_f64 >= @as(f64, @floatFromInt(len))) return .undefined;
        const k: u53 = @intFromFloat(k_f64);

        // 7. Return ? Get(O, ! ToString(𝔽(k))).
        return object.get(PropertyKey.from(k));
    }

    /// 23.1.3.2 Array.prototype.concat ( ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.concat
    fn concat(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let A be ? ArraySpeciesCreate(O, 0).
        const array = try arraySpeciesCreate(agent, object, 0);

        // 3. Let n be 0.
        var n: u53 = 0;

        // 4. Prepend O to items.

        // 5. For each element E of items, do
        var index: u53 = 0;
        while (index <= arguments.count()) : (index += 1) {
            const element = if (index == 0)
                Value.from(object)
            else
                arguments.values[@as(usize, @intCast(index)) - 1];

            // a. Let spreadable be ? IsConcatSpreadable(E).
            const spreadable = try isConcatSpreadable(agent, element);

            // b. If spreadable is true, then
            if (spreadable) {
                // i. Let len be ? LengthOfArrayLike(E).
                const len = try element.object.lengthOfArrayLike();

                // ii. If n + len > 2^53 - 1, throw a TypeError exception.
                if (std.meta.isError(std.math.add(u53, n, len))) {
                    return agent.throwException(.type_error, "Maximum array length exceeded");
                }

                // iii. Let k be 0.
                var k: u53 = 0;

                // iv. Repeat, while k < len,
                while (k < len) : ({
                    n += 1;
                    k += 1;
                }) {
                    // 1. Let P be ! ToString(𝔽(k)).
                    const property_key = PropertyKey.from(k);

                    // 2. Let exists be ? HasProperty(E, P).
                    const exists = try element.object.hasProperty(property_key);

                    // 3. If exists is true, then
                    if (exists) {
                        // a. Let subElement be ? Get(E, P).
                        const sub_element = try element.object.get(property_key);

                        // b. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), subElement).
                        try array.createDataPropertyOrThrow(PropertyKey.from(n), sub_element);
                    }

                    // 4. Set n to n + 1.
                    // 5. Set k to k + 1.
                }
            }
            // c. Else,
            else {
                // i. NOTE: E is added as a single item rather than spread.

                // ii. If n ≥ 2^53 - 1, throw a TypeError exception.
                if (n == std.math.maxInt(u53)) {
                    return agent.throwException(.type_error, "Maximum array length exceeded");
                }

                // iii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), E).
                try array.createDataPropertyOrThrow(PropertyKey.from(n), element);

                // iv. Set n to n + 1.
                n += 1;
            }
        }

        // 6. Perform ? Set(A, "length", 𝔽(n), true).
        try array.set(PropertyKey.from("length"), Value.from(n), .throw);

        // 7. Return A.
        return Value.from(array);
    }

    /// 23.1.3.2.1 IsConcatSpreadable ( O )
    /// https://tc39.es/ecma262/#sec-isconcatspreadable
    fn isConcatSpreadable(agent: *Agent, value: Value) !bool {
        // 1. If O is not an Object, return false.
        if (value != .object) return false;

        // 2. Let spreadable be ? Get(O, @@isConcatSpreadable).
        const spreadable = try value.object.get(
            PropertyKey.from(agent.well_known_symbols.@"@@isConcatSpreadable"),
        );

        // 3. If spreadable is not undefined, return ToBoolean(spreadable).
        if (spreadable != .undefined) return spreadable.toBoolean();

        // 4. Return ? IsArray(O).
        return value.isArray();
    }

    /// 23.1.3.4 Array.prototype.copyWithin ( target, start [ , end ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.copywithin
    fn copyWithin(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const start = arguments.get(1);
        const end = arguments.get(2);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();
        const len_f64: f64 = @floatFromInt(len);

        // 3. Let relativeTarget be ? ToIntegerOrInfinity(target).
        const relative_starget = try target.toIntegerOrInfinity(agent);

        // 4. If relativeTarget = -∞, let to be 0.
        const to_f64 = if (relative_starget == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 5. Else if relativeTarget < 0, let to be max(len + relativeTarget, 0).
        else if (relative_starget < 0) blk: {
            break :blk @max(len_f64 + relative_starget, 0);
        }
        // 6. Else, let to be min(relativeTarget, len).
        else blk: {
            break :blk @min(relative_starget, len_f64);
        };
        var to: u53 = @intFromFloat(to_f64);

        // 7. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 8. If relativeStart = -∞, let from be 0.
        const from_f64 = if (relative_start == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 9. Else if relativeStart < 0, let from be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 10. Else, let from be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        var from: u53 = @intFromFloat(from_f64);

        // 11. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //     ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 12. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (relative_end == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 13. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 14. Else, let final be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };
        const final: u53 = @intFromFloat(final_f64);

        // 15. Let count be min(final - from, len - to).
        var count = @min(final -| from, len -| to);

        // 16. If from < to and to < from + count, then
        const direction: i2 = if (from < to and to < (from + count)) blk: {
            // b. Set from to from + count - 1.
            from = from + count - 1;

            // c. Set to to to + count - 1.
            to = to + count - 1;

            // a. Let direction be -1.
            break :blk -1;
        }
        // 17. Else,
        else blk: {
            // a. Let direction be 1.
            break :blk 1;
        };

        // 18. Repeat, while count > 0,
        while (count > 0) : ({
            if (direction == 1) from += 1 else from -|= 1;
            if (direction == 1) to += 1 else to -|= 1;
            count -= 1;
        }) {
            // a. Let fromKey be ! ToString(𝔽(from)).
            const from_key = PropertyKey.from(from);

            // b. Let toKey be ! ToString(𝔽(to)).
            const to_key = PropertyKey.from(to);

            // c. Let fromPresent be ? HasProperty(O, fromKey).
            const from_present = try object.hasProperty(from_key);

            // d. If fromPresent is true, then
            if (from_present) {
                // i. Let fromVal be ? Get(O, fromKey).
                const from_value = try object.get(from_key);

                // ii. Perform ? Set(O, toKey, fromVal, true).
                try object.set(to_key, from_value, .throw);
            }
            // e. Else,
            else {
                // i. Assert: fromPresent is false.
                std.debug.assert(!from_present);

                // ii. Perform ? DeletePropertyOrThrow(O, toKey).
                try object.deletePropertyOrThrow(to_key);
            }

            // f. Set from to from + direction.
            // g. Set to to to + direction.
            // h. Set count to count - 1.
        }

        // 19. Return O.
        return Value.from(object);
    }

    /// 23.1.3.5 Array.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return CreateArrayIterator(O, key+value).
        return Value.from(try createArrayIterator(agent, object, .@"key+value"));
    }

    /// 23.1.3.6 Array.prototype.every ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.every
    fn every(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. Let k be 0.
        var k: u53 = 0;

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
                const test_result = (try callback_fn.callAssumeCallable(
                    this_arg,
                    .{ k_value, Value.from(k), Value.from(object) },
                )).toBoolean();

                // iii. If testResult is false, return false.
                if (!test_result) return Value.from(false);
            }

            // d. Set k to k + 1.
        }

        // 6. Return true.
        return Value.from(true);
    }

    /// 23.1.3.7 Array.prototype.fill ( value [ , start [ , end ] ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.fill
    fn fill(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const value = arguments.get(0);
        const start = arguments.get(1);
        const end = arguments.get(2);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();
        const len_f64 = @as(f64, @floatFromInt(len));

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 4. If relativeStart = -∞, let k be 0.
        const k_f64 = if (relative_start == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 6. Else, let k be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        var k: u53 = @intFromFloat(k_f64);

        // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //    ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 8. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (relative_end == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 10. Else, let final be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };
        const final: u53 = @intFromFloat(final_f64);

        // 11. Repeat, while k < final,
        while (k < final) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Perform ? Set(O, Pk, value, true).
            try object.set(property_key, value, .throw);

            // c. Set k to k + 1.
        }

        // 12. Return O.
        return Value.from(object);
    }

    /// 23.1.3.8 Array.prototype.filter ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.filter
    fn filter(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. Let A be ? ArraySpeciesCreate(O, 0).
        const array = try arraySpeciesCreate(agent, object, 0);

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Let to be 0.
        var to: u53 = 0;

        // 7. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Let selected be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
                const selected = (try callback_fn.callAssumeCallable(
                    this_arg,
                    .{ k_value, Value.from(k), Value.from(object) },
                )).toBoolean();

                // iii. If selected is true, then
                if (selected) {
                    // 1. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(to)), kValue).
                    try array.createDataPropertyOrThrow(PropertyKey.from(to), k_value);

                    // 2. Set to to to + 1.
                    to += 1;
                }
            }

            // d. Set k to k + 1.
        }

        // 8. Return A.
        return Value.from(array);
    }

    /// 23.1.3.9 Array.prototype.find ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.find
    fn find(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .ascending, predicate, this_arg);

        // 4. Return findRec.[[Value]].
        return find_record.value;
    }

    /// 23.1.3.10 Array.prototype.findIndex ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.findindex
    fn findIndex(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .ascending, predicate, this_arg);

        // 4. Return findRec.[[Index]].
        return find_record.index;
    }

    /// 23.1.3.11 Array.prototype.findLast ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.findlast
    fn findLast(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .descending, predicate, this_arg);

        // 4. Return findRec.[[Value]].
        return find_record.value;
    }

    /// 23.1.3.12 Array.prototype.findLastIndex ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.findlastindex
    fn findLastIndex(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .descending, predicate, this_arg);

        // 4. Return findRec.[[Index]].
        return find_record.index;
    }

    /// 23.1.3.15 Array.prototype.forEach ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. Let k be 0.
        var k: u53 = 0;

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Perform ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
                _ = try callback_fn.callAssumeCallable(
                    this_arg,
                    .{ k_value, Value.from(k), Value.from(object) },
                );
            }

            // d. Set k to k + 1.
        }

        // 6. Return undefined.
        return .undefined;
    }

    /// 23.1.3.16 Array.prototype.includes ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.includes
    fn includes(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If len = 0, return false.
        if (len == 0) return Value.from(false);

        // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
        var n = try from_index.toIntegerOrInfinity(agent);

        // 5. Assert: If fromIndex is undefined, then n is 0.
        if (from_index == .undefined) std.debug.assert(n == 0);

        // 6. If n = +∞, return false.
        if (n == std.math.inf(f64)) return Value.from(false);

        // 7. Else if n = -∞, set n to 0.
        if (n == -std.math.inf(f64)) n = 0;

        // 8. If n ≥ 0, then
        //     a. Let k be n.
        // 9. Else,
        //     a. Let k be len + n.
        //     b. If k < 0, set k to 0.
        const k_f64 = if (n >= 0) n else @max(@as(f64, @floatFromInt(len)) + n, 0);
        if (k_f64 >= std.math.maxInt(u53)) return Value.from(false);
        var k: u53 = @intFromFloat(k_f64);

        // 10. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let elementK be ? Get(O, ! ToString(𝔽(k))).
            const element_k = try object.get(PropertyKey.from(k));

            // b. If SameValueZero(searchElement, elementK) is true, return true.
            if (sameValueZero(search_element, element_k)) return Value.from(true);

            // c. Set k to k + 1.
        }

        // 11. Return false.
        return Value.from(false);
    }

    /// 23.1.3.17 Array.prototype.indexOf ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.indexof
    fn indexOf(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If len = 0, return -1𝔽.
        if (len == 0) return Value.from(-1);

        // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
        var n = try from_index.toIntegerOrInfinity(agent);

        // 5. Assert: If fromIndex is undefined, then n is 0.
        if (from_index == .undefined) std.debug.assert(n == 0);

        // 6. If n = +∞, return -1𝔽.
        if (n == std.math.inf(f64)) return Value.from(-1);

        // 7. Else if n = -∞, set n to 0.
        if (n == -std.math.inf(f64)) n = 0;

        // 8. If n ≥ 0, then
        //     a. Let k be n.
        // 9. Else,
        //     a. Let k be len + n.
        //     b. If k < 0, set k to 0.
        const k_f64 = if (n >= 0) n else @max(@as(f64, @floatFromInt(len)) + n, 0);
        if (k_f64 >= std.math.maxInt(u53)) return Value.from(-1);
        var k: u53 = @intFromFloat(k_f64);

        // 10. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
            const k_present = try object.hasProperty(PropertyKey.from(k));

            // b. If kPresent is true, then
            if (k_present) {
                // i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
                const element_k = try object.get(PropertyKey.from(k));

                // ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
                if (isStrictlyEqual(search_element, element_k)) return Value.from(k);
            }

            // c. Set k to k + 1.
        }

        // 11. Return -1𝔽.
        return Value.from(-1);
    }

    /// 23.1.3.18 Array.prototype.join ( separator )
    /// https://tc39.es/ecma262/#sec-array.prototype.join
    fn join(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const separator = arguments.get(0);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If separator is undefined, let sep be ",".
        // 4. Else, let sep be ? ToString(separator).
        const sep = if (separator == .undefined) "," else (try separator.toString(agent)).utf8;

        // 5. Let R be the empty String.
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var elements = try std.ArrayList([]const u8).initCapacity(agent.gc_allocator, @intCast(len));
        defer elements.deinit();

        // 6. Let k be 0.
        var k: u53 = 0;

        // 7. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, set R to the string-concatenation of R and sep.

            // b. Let element be ? Get(O, ! ToString(𝔽(k))).
            const element = try object.get(PropertyKey.from(k));

            // c. If element is either undefined or null, let next be the empty String; otherwise,
            //    let next be ? ToString(element).
            const next = if (element == .undefined or element == .null)
                ""
            else
                (try element.toString(agent)).utf8;

            // d. Set R to the string-concatenation of R and next.
            try elements.append(next);

            // e. Set k to k + 1.
        }

        // 8. Return R.
        return Value.from(
            try std.mem.join(agent.gc_allocator, sep, elements.items),
        );
    }

    /// 23.1.3.19 Array.prototype.keys ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.keys
    fn keys(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return CreateArrayIterator(O, key).
        return Value.from(try createArrayIterator(agent, object, .key));
    }

    /// 23.1.3.20 Array.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.lastindexof
    fn lastIndexOf(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If len = 0, return -1𝔽.
        if (len == 0) return Value.from(-1);

        // 4. If fromIndex is present, let n be ? ToIntegerOrInfinity(fromIndex); else let n be len - 1.
        var n = if (arguments.count() > 1)
            try from_index.toIntegerOrInfinity(agent)
        else
            @as(f64, @floatFromInt(len)) - 1;

        // 5. If n = -∞, return -1𝔽.
        if (n == -std.math.inf(f64)) return Value.from(-1);

        // 6. If n ≥ 0, then
        //     a. Let k be min(n, len - 1).
        // 7. Else,
        //     a. Let k be len + n.
        const k_f64 = if (n >= 0)
            @min(n, @as(f64, @floatFromInt(len)) - 1)
        else
            @as(f64, @floatFromInt(len)) + n;
        if (k_f64 < 0) return Value.from(-1);
        var k: u53 = @intFromFloat(k_f64);

        // 8. Repeat, while k ≥ 0,
        while (k >= 0) : (k -|= 1) {
            // a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
            const k_present = try object.hasProperty(PropertyKey.from(k));

            // b. If kPresent is true, then
            if (k_present) {
                // i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
                const element_k = try object.get(PropertyKey.from(k));

                // ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
                if (isStrictlyEqual(search_element, element_k)) return Value.from(k);
            }

            // c. Set k to k - 1.
            if (k == 0) break;
        }

        // 9. Return -1𝔽.
        return Value.from(-1);
    }

    /// 23.1.3.21 Array.prototype.map ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.map
    fn map(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. Let A be ? ArraySpeciesCreate(O, len).
        const array = try arraySpeciesCreate(agent, object, len);

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Let mappedValue be ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
                const mapped_value = try callback_fn.callAssumeCallable(
                    this_arg,
                    .{ k_value, Value.from(k), Value.from(object) },
                );

                // iii. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
                try array.createDataPropertyOrThrow(property_key, mapped_value);
            }

            // d. Set k to k + 1.
        }

        // 7. Return A.
        return Value.from(array);
    }

    /// 23.1.3.22 Array.prototype.pop ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.pop
    fn pop(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // If len = 0, then
        if (len == 0) {
            // a. Perform ? Set(O, "length", +0𝔽, true).
            try object.set(PropertyKey.from("length"), Value.from(0), .throw);

            // b. Return undefined.
            return .undefined;
        }
        // 4. Else,
        else {
            // a. Assert: len > 0.
            std.debug.assert(len > 0);

            // b. Let newLen be 𝔽(len - 1).
            const new_len = len - 1;

            // c. Let index be ! ToString(newLen).
            const property_key = PropertyKey.from(new_len);

            // d. Let element be ? Get(O, index).
            const element = try object.get(property_key);

            // e. Perform ? DeletePropertyOrThrow(O, index).
            try object.deletePropertyOrThrow(property_key);

            // f. Perform ? Set(O, "length", newLen, true).
            try object.set(PropertyKey.from("length"), Value.from(new_len), .throw);

            // g. Return element.
            return element;
        }
    }

    /// 23.1.3.23 Array.prototype.push ( ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.push
    fn push(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        var len = try object.lengthOfArrayLike();

        // 3. Let argCount be the number of elements in items.
        const arg_count: u53 = @intCast(arguments.count());

        // 4. If len + argCount > 2^53 - 1, throw a TypeError exception.
        if (std.meta.isError(std.math.add(u53, len, arg_count))) {
            return agent.throwException(.type_error, "Maximum array length exceeded");
        }

        // 5. For each element E of items, do
        for (arguments.values) |element| {
            // a. Perform ? Set(O, ! ToString(𝔽(len)), E, true).
            try object.set(PropertyKey.from(len), element, .throw);

            // b. Set len to len + 1.
            len += 1;
        }

        // 6. Perform ? Set(O, "length", 𝔽(len), true).
        try object.set(PropertyKey.from("length"), Value.from(len), .throw);

        // 7. Return 𝔽(len).
        return Value.from(len);
    }

    /// 23.1.3.24 Array.prototype.reduce ( callbackfn [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.reduce
    fn reduce(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
        if (len == 0 and initial_value == null) {
            return agent.throwException(
                .type_error,
                "Cannot reduce empty array without initial value",
            );
        }

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Let accumulator be undefined.
        var accumulator: Value = undefined;

        // 7. If initialValue is present, then
        if (initial_value != null) {
            // a. Set accumulator to initialValue.
            accumulator = initial_value.?;
        }
        // 8. Else,
        else {
            // a. Let kPresent be false.
            var k_present = false;

            // b. Repeat, while kPresent is false and k < len,
            while (!k_present and k < len) : (k += 1) {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k);

                // ii. Set kPresent to ? HasProperty(O, Pk).
                k_present = try object.hasProperty(property_key);

                // iii. If kPresent is true, then
                if (k_present) {
                    // 1. Set accumulator to ? Get(O, Pk).
                    accumulator = try object.get(property_key);
                }

                // iv. Set k to k + 1.
            }

            // c. If kPresent is false, throw a TypeError exception.
            if (!k_present) {
                return agent.throwException(
                    .type_error,
                    "Cannot reduce empty array without initial value",
                );
            }
        }

        // 9. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
                accumulator = try callback_fn.callAssumeCallable(
                    .undefined,
                    .{ accumulator, k_value, Value.from(k), Value.from(object) },
                );
            }

            // d. Set k to k + 1.
        }

        // 10. Return accumulator.
        return accumulator;
    }

    /// 23.1.3.25 Array.prototype.reduceRight ( callbackfn [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.reduceright
    fn reduceRight(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
        if (len == 0 and initial_value == null) {
            return agent.throwException(
                .type_error,
                "Cannot reduce empty array without initial value",
            );
        }

        // 5. Let k be len - 1.
        var k: ?u53 = std.math.sub(u53, len, 1) catch null;

        // 6. Let accumulator be undefined.
        var accumulator: Value = undefined;

        // 7. If initialValue is present, then
        if (initial_value != null) {
            // a. Set accumulator to initialValue.
            accumulator = initial_value.?;
        }
        // 8. Else,
        else {
            // a. Let kPresent be false.
            var k_present = false;

            // b. Repeat, while kPresent is false and k ≥ 0,
            while (!k_present and k != null) : (k = (std.math.sub(u53, k.?, 1) catch null)) {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k.?);

                // ii. Set kPresent to ? HasProperty(O, Pk).
                k_present = try object.hasProperty(property_key);

                // iii. If kPresent is true, then
                if (k_present) {
                    // 1. Set accumulator to ? Get(O, Pk).
                    accumulator = try object.get(property_key);
                }

                // iv. Set k to k - 1.
            }

            // c. If kPresent is false, throw a TypeError exception.
            if (!k_present) {
                return agent.throwException(
                    .type_error,
                    "Cannot reduce empty array without initial value",
                );
            }
        }

        // 9. Repeat, while k ≥ 0,
        while (k != null) : (k = (std.math.sub(u53, k.?, 1) catch null)) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k.?);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
                accumulator = try callback_fn.callAssumeCallable(
                    .undefined,
                    .{ accumulator, k_value, Value.from(k.?), Value.from(object) },
                );
            }

            // d. Set k to k - 1.
        }

        // 10. Return accumulator.
        return accumulator;
    }

    /// 23.1.3.27 Array.prototype.shift ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.shift
    fn shift(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If len = 0, then
        if (len == 0) {
            // a. Perform ? Set(O, "length", +0𝔽, true).
            try object.set(PropertyKey.from("length"), Value.from(0), .throw);

            // b. Return undefined.
            return .undefined;
        }

        // 4. Let first be ? Get(O, "0").
        const first = object.get(PropertyKey.from(0));

        // 5. Let k be 1.
        var k: u53 = 1;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let from be ! ToString(𝔽(k)).
            const from = PropertyKey.from(k);

            // b. Let to be ! ToString(𝔽(k - 1)).
            const to = PropertyKey.from(k - 1);

            // c. Let fromPresent be ? HasProperty(O, from).
            const from_present = try object.hasProperty(from);

            // d. If fromPresent is true, then
            if (from_present) {
                // i. Let fromVal be ? Get(O, from).
                const from_value = try object.get(from);

                // ii. Perform ? Set(O, to, fromVal, true).
                try object.set(to, from_value, .throw);
            }
            // e. Else,
            else {
                // i. Assert: fromPresent is false.
                std.debug.assert(!from_present);

                // ii. Perform ? DeletePropertyOrThrow(O, to).
                try object.deletePropertyOrThrow(to);
            }

            // f. Set k to k + 1.
        }

        // 7. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(len - 1))).
        try object.deletePropertyOrThrow(PropertyKey.from(len - 1));

        // 8. Perform ? Set(O, "length", 𝔽(len - 1), true).
        try object.set(PropertyKey.from("length"), Value.from(len - 1), .throw);

        // 9. Return first.
        return first;
    }

    /// 23.1.3.28 Array.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-array.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();
        const len_f64 = @as(f64, @floatFromInt(len));

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 4. If relativeStart = -∞, let k be 0.
        const k_f64 = if (relative_start == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 6. Else, let k be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        var k: u53 = @intFromFloat(k_f64);

        // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //    ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 8. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (relative_end == -std.math.inf(f64)) blk: {
            break :blk 0;
        }
        // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 10. Else, let final be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };
        const final: u53 = @intFromFloat(final_f64);

        // 11. Let count be max(final - k, 0).
        const count: u53 = @intFromFloat(@max(final_f64 - k_f64, 0));

        // 12. Let A be ? ArraySpeciesCreate(O, count).
        const array = try arraySpeciesCreate(agent, object, count);

        // 13. Let n be 0.
        var n: u53 = 0;

        // 14. Repeat, while k < final,
        while (k < final) : ({
            k += 1;
            n += 1;
        }) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), kValue).
                try array.createDataPropertyOrThrow(PropertyKey.from(n), k_value);
            }

            // d. Set k to k + 1.
            // e. Set n to n + 1.
        }

        // 15. Perform ? Set(A, "length", 𝔽(n), true).
        try array.set(PropertyKey.from("length"), Value.from(n), .throw);

        // 16. Return A.
        return Value.from(array);
    }

    /// 23.1.3.29 Array.prototype.some ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.some
    fn some(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{callback_fn}),
            );
        }

        // 4. Let k be 0.
        var k: u53 = 0;

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(property_key);

                // ii. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
                const test_result = (try callback_fn.callAssumeCallable(
                    this_arg,
                    .{ k_value, Value.from(k), Value.from(object) },
                )).toBoolean();

                // iii. If testResult is true, return true.
                if (test_result) return Value.from(true);
            }

            // d. Set k to k + 1.
        }

        // 6. Return true.
        return Value.from(false);
    }

    /// 23.1.3.32 Array.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let array be ? ToObject(this value).
        const array = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(array).
        const len = try array.lengthOfArrayLike();

        // 3. Let separator be the implementation-defined list-separator String value appropriate
        //    for the host environment's current locale (such as ", ").
        const separator = ", ";

        // 4. Let R be the empty String.
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var elements = try std.ArrayList([]const u8).initCapacity(agent.gc_allocator, @intCast(len));
        defer elements.deinit();

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, then
            // i. Set R to the string-concatenation of R and separator.

            // b. Let nextElement be ? Get(array, ! ToString(𝔽(k))).
            const next_element = try array.get(PropertyKey.from(k));

            // c. If nextElement is neither undefined nor null, then
            if (next_element != .undefined and next_element != .null) {
                // i. Let S be ? ToString(? Invoke(nextElement, "toLocaleString")).
                const string = try (try next_element.invoke(
                    agent,
                    PropertyKey.from("toLocaleString"),
                    .{},
                )).toString(agent);

                // ii. Set R to the string-concatenation of R and S.
                try elements.append(string.utf8);
            } else {
                try elements.append("");
            }

            // d. Set k to k + 1.
        }

        // 7. Return R.
        return Value.from(
            try std.mem.join(agent.gc_allocator, separator, elements.items),
        );
    }

    /// 23.1.3.36 Array.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        const realm = agent.currentRealm();

        // 1. Let array be ? ToObject(this value).
        const array = try this_value.toObject(agent);

        // 2. Let func be ? Get(array, "join").
        var func = try array.get(PropertyKey.from("join"));

        // 3. If IsCallable(func) is false, set func to the intrinsic function %Object.prototype.toString%.
        if (!func.isCallable()) func = Value.from(try realm.intrinsics.@"%Object.prototype.toString%"());

        // 4. Return ? Call(func, array).
        return func.callAssumeCallableNoArgs(Value.from(array));
    }

    /// 23.1.3.37 Array.prototype.unshift ( ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.unshift
    fn unshift(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let argCount be the number of elements in items.
        const arg_count = arguments.count();

        // 4. If argCount > 0, then
        if (arg_count > 0) {
            // a. If len + argCount > 2^53 - 1, throw a TypeError exception.
            if (std.meta.isError(std.math.add(u53, len, @intCast(arg_count)))) {
                return agent.throwException(.type_error, "Maximum array length exceeded");
            }

            // b. Let k be len.
            var k = len;

            // c. Repeat, while k > 0,
            while (k > 0) : (k -= 1) {
                // i. Let from be ! ToString(𝔽(k - 1)).
                const from = PropertyKey.from(k - 1);

                // ii. Let to be ! ToString(𝔽(k + argCount - 1)).
                const to = PropertyKey.from(
                    k + @as(PropertyKey.IntegerIndex, @intCast(arg_count)) - 1,
                );

                // iii. Let fromPresent be ? HasProperty(O, from).
                const from_present = try object.hasProperty(from);

                // iv. If fromPresent is true, then
                if (from_present) {
                    // 1. Let fromValue be ? Get(O, from).
                    const from_value = try object.get(from);

                    // 2. Perform ? Set(O, to, fromValue, true).
                    try object.set(to, from_value, .throw);
                }
                // v. Else,
                else {
                    // 1. Assert: fromPresent is false.
                    std.debug.assert(!from_present);

                    // 2. Perform ? DeletePropertyOrThrow(O, to).
                    try object.deletePropertyOrThrow(to);
                }

                // vi. Set k to k - 1.
            }

            // d. Let j be +0𝔽.
            // e. For each element E of items, do
            for (arguments.values, 0..) |element, j| {
                // i. Perform ? Set(O, ! ToString(j), E, true).
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(j)));
                try object.set(property_key, element, .throw);

                // ii. Set j to j + 1𝔽.
            }
        }

        // 5. Perform ? Set(O, "length", 𝔽(len + argCount), true).
        try object.set(PropertyKey.from("length"), Value.from(len + arg_count), .throw);

        // 6. Return 𝔽(len + argCount).
        return Value.from(len + @as(u53, @intCast(arg_count)));
    }

    /// 23.1.3.38 Array.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.values
    fn values(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return CreateArrayIterator(O, value).
        return Value.from(try createArrayIterator(agent, object, .value));
    }

    /// 23.1.3.39 Array.prototype.with ( index, value )
    /// https://tc39.es/ecma262/#sec-array.prototype.with
    fn with(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const index = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike();

        // 3. Let relativeIndex be ? ToIntegerOrInfinity(index).
        const relative_index = try index.toIntegerOrInfinity(agent);

        // 4. If relativeIndex ≥ 0, let actualIndex be relativeIndex.
        // 5. Else, let actualIndex be len + relativeIndex.
        const actual_index_f64 = if (relative_index >= 0)
            relative_index
        else
            @as(f64, @floatFromInt(len)) + relative_index;

        // 6. If actualIndex ≥ len or actualIndex < 0, throw a RangeError exception.
        if (actual_index_f64 >= @as(f64, @floatFromInt(len)) or actual_index_f64 < 0) {
            return agent.throwException(.range_error, "Index is out of array bounds");
        }
        const actual_index: u53 = @intFromFloat(actual_index_f64);

        // 7. Let A be ? ArrayCreate(len).
        const array = try arrayCreate(agent, len, null);

        // 8. Let k be 0.
        var k: u53 = 0;

        // 9. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. If k is actualIndex, let fromValue be value.
            // c. Else, let fromValue be ? Get(O, Pk).
            const from_value = if (k == actual_index)
                value
            else
                try object.get(property_key);

            // d. Perform ! CreateDataPropertyOrThrow(A, Pk, fromValue).
            array.createDataPropertyOrThrow(property_key, from_value) catch |err| try noexcept(err);

            // e. Set k to k + 1.
        }

        // 10. Return A.
        return Value.from(array);
    }
};

/// 23.1.3.12.1 FindViaPredicate ( O, len, direction, predicate, thisArg )
/// https://tc39.es/ecma262/#sec-findviapredicate
pub fn findViaPredicate(
    object: Object,
    len: u53,
    comptime direction: enum { ascending, descending },
    predicate: Value,
    this_arg: Value,
) !struct { index: Value, value: Value } {
    const agent = object.agent();

    // 1. If IsCallable(predicate) is false, throw a TypeError exception.
    if (!predicate.isCallable()) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{predicate}),
        );
    }

    // 2. If direction is ascending, then
    //     a. Let indices be a List of the integers in the interval from 0 (inclusive) to len
    //        (exclusive), in ascending order.
    // 3. Else,
    //     a. Let indices be a List of the integers in the interval from 0 (inclusive) to len
    //        (exclusive), in descending order.
    // 4. For each integer k of indices, do
    var k: u53 = if (direction == .ascending) 0 else len;
    while (if (direction == .ascending) k < len else k > 0) : (k = if (direction == .ascending) k + 1 else k - 1) {
        // a. Let Pk be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k);

        // b. NOTE: If O is a TypedArray, the following invocation of Get will return a normal completion.
        // c. Let kValue be ? Get(O, Pk).
        const k_value = try object.get(property_key);

        // d. Let testResult be ? Call(predicate, thisArg, « kValue, 𝔽(k), O »).
        const test_result = try predicate.callAssumeCallable(
            this_arg,
            .{ k_value, Value.from(k), Value.from(object) },
        );

        // e. If ToBoolean(testResult) is true, return the Record { [[Index]]: 𝔽(k), [[Value]]: kValue }.
        if (test_result.toBoolean()) return .{ .index = Value.from(k), .value = k_value };
    }

    // 5. Return the Record { [[Index]]: -1𝔽, [[Value]]: undefined }.
    return .{ .index = Value.from(-1), .value = .undefined };
}

/// 23.1.4 Properties of Array Instances
/// https://tc39.es/ecma262/#sec-properties-of-array-instances
pub const Array = Object.Factory(.{
    .tag = .array,
});
