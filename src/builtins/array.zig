//! 10.4.2 Array Exotic Objects
//! https://tc39.es/ecma262/#sec-array-exotic-objects

const builtin = @import("builtin");
const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const ordinary = @import("ordinary.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Completion = types.Completion;
const Iterator = types.Iterator;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const asyncFunctionStart = builtins.asyncFunctionStart;
const await = builtins.await;
const createArrayIterator = builtins.createArrayIterator;
const createAsyncFromSyncIterator = builtins.createAsyncFromSyncIterator;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getIteratorFromMethod = types.getIteratorFromMethod;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const isLessThan = types.isLessThan;
const isStrictlyEqual = types.isStrictlyEqual;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = ordinary.ordinaryDefineOwnProperty;
const ordinaryObjectCreate = ordinary.ordinaryObjectCreate;
const sameValueZero = types.sameValueZero;

const array_fast_paths = @import("array_fast_paths.zig");

const runtime_safety = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

// Non-standard helper to get the length property of an array
pub fn getArrayLength(array: *const Object) u32 {
    // The "length" property is always at index 0 in the shape's properties, so we can access it
    // without a hashmap lookup. This warrants a sanity check in debug mode :^)
    if (runtime_safety) {
        const property_key = array.property_storage.shape.properties.keys()[0];
        std.debug.assert(property_key.eql(PropertyKey.from("length")));
        const property_metadata = array.property_storage.shape.properties.values()[0];
        std.debug.assert(@intFromEnum(property_metadata.index.value) == 0);
    }
    const length_value = array.property_storage.values.items[0];
    return @intFromFloat(length_value.asNumber().asFloat());
}

pub fn getArrayLengthPropertyMetadata(array: *const Object) Object.Shape.PropertyMetadata {
    // The "length" property is always at index 0 in the shape's properties, so we can access it
    // without a hashmap lookup. This warrants a sanity check in debug mode :^)
    if (runtime_safety) {
        const property_key = array.property_storage.shape.properties.keys()[0];
        std.debug.assert(property_key.eql(PropertyKey.from("length")));
    }
    return array.property_storage.shape.properties.values()[0];
}

/// 10.4.2.1 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-array-exotic-objects-defineownproperty-p-desc
fn defineOwnProperty(
    agent: *Agent,
    array: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    // 1. If P is "length", then
    if (property_key == .string and property_key.string.eql(String.fromLiteral("length"))) {
        // a. Return ? ArraySetLength(A, Desc).
        return arraySetLength(agent, array, property_descriptor);
    }
    // 2. Else if P is an array index, then
    else if (property_key.isArrayIndex()) {
        // a. Let lengthDesc be OrdinaryGetOwnProperty(A, "length").
        const length_property_metadata = getArrayLengthPropertyMetadata(array);

        // b. Assert: lengthDesc is not undefined.
        // c. Assert: IsDataDescriptor(lengthDesc) is true.
        // d. Assert: lengthDesc.[[Configurable]] is false.
        std.debug.assert(length_property_metadata.index == .value);
        std.debug.assert(!length_property_metadata.attributes.configurable);

        // e. Let length be lengthDesc.[[Value]].
        // f. Assert: length is a non-negative integral Number.
        const length = getArrayLength(array);

        // g. Let index be ! ToUint32(P).
        const index: u32 = @intCast(property_key.integer_index);

        // h. If index ≥ length and lengthDesc.[[Writable]] is false, return false.
        if (index >= length and length_property_metadata.attributes.writable == false)
            return false;

        // i. Let succeeded be ! OrdinaryDefineOwnProperty(A, P, Desc).
        const succeeded = ordinaryDefineOwnProperty(
            agent,
            array,
            property_key,
            property_descriptor,
        ) catch |err| try noexcept(err);

        // j. If succeeded is false, return false.
        if (!succeeded)
            return false;

        // k. If index ≥ length, then
        if (index >= length) {
            // i. Set lengthDesc.[[Value]] to index + 1𝔽.
            // ii. Set succeeded to ! OrdinaryDefineOwnProperty(A, "length", lengthDesc).
            // iii. Assert: succeeded is true.
            array.property_storage.values.items[0] = Value.from(index + 1);
        }

        // l. Return true.
        return true;
    }

    // 3. Return ? OrdinaryDefineOwnProperty(A, P, Desc).
    return ordinaryDefineOwnProperty(agent, array, property_key, property_descriptor);
}

/// 10.4.2.2 ArrayCreate ( length [ , proto ] )
/// https://tc39.es/ecma262/#sec-arraycreate
pub fn arrayCreate(agent: *Agent, length: u53, maybe_prototype: ?*Object) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If length > 2**32 - 1, throw a RangeError exception.
    if (length >= std.math.maxInt(u32)) {
        return agent.throwException(.range_error, "Invalid array length", .{});
    }

    // 2. If proto is not present, set proto to %Array.prototype%.
    const prototype_ = maybe_prototype orelse try realm.intrinsics.@"%Array.prototype%"();

    // 3. Let A be MakeBasicObject(« [[Prototype]], [[Extensible]] »).
    const array = try Array.create(agent, .{
        // 4. Set A.[[Prototype]] to proto.
        .prototype = prototype_,

        // 5. Set A.[[DefineOwnProperty]] as specified in 10.4.2.1.
        .internal_methods = .initComptime(.{
            .defineOwnProperty = defineOwnProperty,
        }),
    });

    // 6. Perform ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    _ = ordinaryDefineOwnProperty(agent, array, PropertyKey.from("length"), .{
        .value = Value.from(@as(u32, @intCast(length))),
        .writable = true,
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 7. Return A.
    return array;
}

/// 10.4.2.3 ArraySpeciesCreate ( originalArray, length )
/// https://tc39.es/ecma262/#sec-arrayspeciescreate
pub fn arraySpeciesCreate(agent: *Agent, original_array: *Object, length: u53) Agent.Error!*Object {
    // 1. Let isArray be ? IsArray(originalArray).
    const is_array = try Value.from(original_array).isArray(agent);

    // 2. If isArray is false, return ? ArrayCreate(length).
    if (!is_array) return arrayCreate(agent, length, null);

    // 3. Let C be ? Get(originalArray, "constructor").
    var constructor_ = try original_array.get(agent, PropertyKey.from("constructor"));

    // 4. If IsConstructor(C) is true, then
    if (constructor_.isConstructor()) {
        // a. Let thisRealm be the current Realm Record.
        const this_realm = agent.currentRealm();

        // b. Let realmC be ? GetFunctionRealm(C).
        const constructor_realm = try constructor_.asObject().getFunctionRealm(agent);

        // c. If thisRealm and realmC are not the same Realm Record, then
        if (this_realm != constructor_realm) {
            // i. If SameValue(C, realmC.[[Intrinsics]].[[%Array%]]) is true, set C to undefined.
            if (constructor_.asObject() == try constructor_realm.intrinsics.@"%Array%"()) {
                constructor_ = .undefined;
            }
        }
    }

    // 5. If C is an Object, then
    if (constructor_.isObject()) {
        // a. Set C to ? Get(C, %Symbol.species%).
        constructor_ = try constructor_.get(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.species%"),
        );

        // b. If C is null, set C to undefined.
        if (constructor_.isNull()) constructor_ = .undefined;
    }

    // 6. If C is undefined, return ? ArrayCreate(length).
    if (constructor_.isUndefined()) return arrayCreate(agent, length, null);

    // 7. If IsConstructor(C) is false, throw a TypeError exception.
    if (!constructor_.isConstructor()) {
        return agent.throwException(.type_error, "{f} is not a constructor", .{constructor_});
    }

    // 8. Return ? Construct(C, « 𝔽(length) »).
    return constructor_.asObject().construct(agent, &.{Value.from(length)}, null);
}

/// 10.4.2.4 ArraySetLength ( A, Desc )
/// https://tc39.es/ecma262/#sec-arraysetlength
pub fn arraySetLength(
    agent: *Agent,
    array: *Object,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    // 1. If Desc does not have a [[Value]] field, then
    if (property_descriptor.value == null) {
        // a. Return ! OrdinaryDefineOwnProperty(A, "length", Desc).
        return ordinaryDefineOwnProperty(
            agent,
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
    if (@as(f64, @floatFromInt(new_len)) != number_len.asFloat()) {
        return agent.throwException(.range_error, "Invalid array length", .{});
    }

    // 6. Set newLenDesc.[[Value]] to newLen.
    new_len_desc.value = Value.from(new_len);

    // 7. Let oldLenDesc be OrdinaryGetOwnProperty(A, "length").
    const length_property_metadata = getArrayLengthPropertyMetadata(array);

    // 8. Assert: oldLenDesc is not undefined.
    // 9. Assert: IsDataDescriptor(oldLenDesc) is true.
    // 10. Assert: oldLenDesc.[[Configurable]] is false.
    std.debug.assert(length_property_metadata.index == .value);
    std.debug.assert(!length_property_metadata.attributes.configurable);

    // 11. Let oldLen be oldLenDesc.[[Value]].
    const old_len = getArrayLength(array);

    // 12. If newLen ≥ oldLen, then
    if (new_len >= old_len) {
        // a. Return ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
        return ordinaryDefineOwnProperty(
            agent,
            array,
            PropertyKey.from("length"),
            new_len_desc,
        ) catch |err| try noexcept(err);
    }

    // 13. If oldLenDesc.[[Writable]] is false, return false.
    if (length_property_metadata.attributes.writable == false) return false;

    var new_writable: bool = undefined;

    // 14. If newLenDesc does not have a [[Writable]] field or newLenDesc.[[Writable]] is true, then
    if (new_len_desc.writable == null or new_len_desc.writable == true) {
        // a. Let newWritable be true.
        new_writable = true;
    } else {
        // 15. Else,
        // a. NOTE: Setting the [[Writable]] attribute to false is deferred in case any elements
        //          cannot be deleted.
        // b. Let newWritable be false.
        new_writable = false;

        // c. Set newLenDesc.[[Writable]] to true.
        new_len_desc.writable = true;
    }

    // 16. Let succeeded be ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    var succeeded = ordinaryDefineOwnProperty(
        agent,
        array,
        PropertyKey.from("length"),
        new_len_desc,
    ) catch |err| try noexcept(err);

    // 17. If succeeded is false, return false.
    if (!succeeded) return false;

    // 18. For each own property key P of A such that P is an array index and ! ToUint32(P) ≥ newLen,
    //     in descending numeric index order, do
    var sparse_indices = switch (array.property_storage.indexed_properties.storage) {
        .sparse => |sparse| blk: {
            var indices: std.ArrayList(u32) = .empty;
            try indices.ensureTotalCapacity(agent.gc_allocator, sparse.size);
            var it = sparse.keyIterator();
            while (it.next()) |index| indices.appendAssumeCapacity(index.*);
            std.sort.insertion(u32, indices.items, {}, std.sort.asc(u32));
            break :blk indices;
        },
        else => null,
    };
    defer if (sparse_indices) |*indices| indices.deinit(agent.gc_allocator);
    var index: ?u32 = switch (array.property_storage.indexed_properties.storage) {
        .none => null,
        .sparse => sparse_indices.?.pop(),
        else => @intCast(array.property_storage.indexed_properties.count() - 1),
    };
    while (index != null and index.? >= new_len) : ({
        index = if (sparse_indices) |*indices|
            indices.pop()
        else
            std.math.sub(u32, index.?, 1) catch null;
    }) {
        const property_key = PropertyKey.from(@as(u53, index.?));

        // a. Let deleteSucceeded be ! A.[[Delete]](P).
        const delete_succeeded = array.internal_methods.delete(
            agent,
            array,
            property_key,
        ) catch |err| try noexcept(err);

        // b. If deleteSucceeded is false, then
        if (!delete_succeeded) {
            // i. Set newLenDesc.[[Value]] to ! ToUint32(P) + 1𝔽.
            new_len_desc.value = Value.from(@as(f64, @floatFromInt(index.?)) + 1);

            // ii. If newWritable is false, set newLenDesc.[[Writable]] to false.
            if (!new_writable) new_len_desc.writable = false;

            // iii. Perform ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
            _ = ordinaryDefineOwnProperty(
                agent,
                array,
                PropertyKey.from("length"),
                new_len_desc,
            ) catch |err| try noexcept(err);

            // iv. Return false.
            return false;
        }
    }

    // 19. If newWritable is false, then
    if (!new_writable) {
        // a. Set succeeded to ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor {
        //      [[Writable]]: false
        //    }).
        succeeded = ordinaryDefineOwnProperty(
            agent,
            array,
            PropertyKey.from("length"),
            .{ .writable = false },
        ) catch |err| try noexcept(err);

        // b. Assert: succeeded is true.
        std.debug.assert(succeeded);
    }

    // 20. Return true.
    return true;
}

/// 23.1.2 Properties of the Array Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-array-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Array",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);
        try object.defineBuiltinFunction(agent, "fromAsync", fromAsync, 1, realm);
        try object.defineBuiltinFunction(agent, "isArray", isArray, 1, realm);
        try object.defineBuiltinFunction(agent, "of", of, 0, realm);
        try object.defineBuiltinAccessor(agent, "%Symbol.species%", @"%Symbol.species%", null, realm);

        // 23.1.2.4 Array.prototype
        // https://tc39.es/ecma262/#sec-array.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Array.prototype%"()),
            .none,
        );
    }

    /// 23.1.1.1 Array ( ...values )
    /// https://tc39.es/ecma262/#sec-array
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        // 1. If NewTarget is undefined, let newTarget be the active function object; else let newTarget be NewTarget.
        const new_target_ = new_target orelse agent.activeFunctionObject();

        // 2. Let proto be ? GetPrototypeFromConstructor(newTarget, "%Array.prototype%").
        const prototype_ = try getPrototypeFromConstructor(
            agent,
            new_target_,
            "%Array.prototype%",
        );

        // 3. Let numberOfArgs be the number of elements in values.
        const number_of_args = arguments.count();

        // 4. If numberOfArgs = 0, then
        if (number_of_args == 0) {
            // a. Return ! ArrayCreate(0, proto).
            return Value.from(arrayCreate(agent, 0, prototype_) catch |err| try noexcept(err));
        }
        // 5. Else if numberOfArgs = 1, then
        else if (number_of_args == 1) {
            // a. Let len be values[0].
            const len = arguments.get(0);

            // b. Let array be ! ArrayCreate(0, proto).
            const array = arrayCreate(agent, 0, prototype_) catch |err| try noexcept(err);

            var int_len: u32 = undefined;

            // c. If len is not a Number, then
            if (!len.isNumber()) {
                // i. Perform ! CreateDataPropertyOrThrow(array, "0", len).
                try array.createDataPropertyDirect(agent, PropertyKey.from(0), len);

                // ii. Let intLen be 1𝔽.
                int_len = 1;
            } else {
                // d. Else,
                // i. Let intLen be ! ToUint32(len).
                int_len = len.toUint32(agent) catch unreachable;

                // ii. If SameValueZero(intLen, len) is false, throw a RangeError exception.
                if (@as(f64, @floatFromInt(int_len)) != len.asNumber().asFloat()) {
                    return agent.throwException(.range_error, "Invalid array length", .{});
                }
            }

            // e. Perform ! Set(array, "length", intLen, true).
            _ = array.set(
                agent,
                PropertyKey.from("length"),
                Value.from(int_len),
                .throw,
            ) catch |err| try noexcept(err);

            // f. Return array.
            return Value.from(array);
        } else {
            // 6. Else,
            // a. Assert: numberOfArgs ≥ 2.
            std.debug.assert(number_of_args >= 2);

            // b. Let array be ? ArrayCreate(numberOfArgs, proto).
            const array = try arrayCreate(agent, @intCast(number_of_args), prototype_);

            // c. Let k be 0.
            // d. Repeat, while k < numberOfArgs,
            for (arguments.values, 0..) |item_k, k| {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(k)));

                // ii. Let itemK be values[k].
                // iii. Perform ! CreateDataPropertyOrThrow(array, Pk, itemK).
                try array.createDataPropertyDirect(agent, property_key, item_k);

                // iv. Set k to k + 1.
            }

            // e. Assert: The mathematical value of array's "length" property is numberOfArgs.
            std.debug.assert(getArrayLength(array) == @as(u32, @intCast(number_of_args)));

            // f. Return array.
            return Value.from(array);
        }
    }

    /// 23.1.2.1 Array.from ( items [ , mapper [ , thisArg ] ] )
    /// https://tc39.es/ecma262/#sec-array.from
    fn from(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const items = arguments.get(0);
        const mapper = arguments.get(1);
        const this_arg = arguments.get(2);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. If mapper is undefined, then
        const mapping = if (mapper.isUndefined()) blk: {
            // a. Let mapping be false.
            break :blk false;
        } else blk: {
            // 3. Else,
            // a. If IsCallable(mapper) is false, throw a TypeError exception.
            if (!mapper.isCallable()) {
                return agent.throwException(.type_error, "{f} is not callable", .{mapper});
            }

            // b. Let mapping be true.
            break :blk true;
        };

        // 4. Let usingIterator be ? GetMethod(items, %Symbol.iterator%).
        const using_iterator = try items.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
        );

        // 5. If usingIterator is not undefined, then
        if (using_iterator != null) {
            // a. If IsConstructor(C) is true, then
            const array = if (constructor_.isConstructor()) blk: {
                // i. Let A be ? Construct(C).
                break :blk try constructor_.asObject().construct(agent, &.{}, null);
            } else blk: {
                // b. Else,
                // i. Let A be ! ArrayCreate(0).
                break :blk try arrayCreate(agent, 0, null);
            };

            // c. Let iteratorRecord be ? GetIteratorFromMethod(items, usingIterator).
            var iterator = try getIteratorFromMethod(agent, items, using_iterator.?);

            // d. Let k be 0.
            var k: u53 = 0;

            // e. Repeat,
            while (true) : (k += 1) {
                // i. If k ≥ 2**53 - 1, then
                if (k == std.math.maxInt(u53)) {
                    // 1. Let error be ThrowCompletion(a newly created TypeError object).
                    const @"error" = agent.throwException(
                        .type_error,
                        "Maximum array length exceeded",
                        .{},
                    );

                    // 2. Return ? IteratorClose(iteratorRecord, error).
                    return iterator.close(agent, @as(Agent.Error!Value, @"error"));
                }

                // ii. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k);

                // iii. Let next be ? IteratorStepValue(iteratorRecord).
                // iv. If next is done, then
                const next = try iterator.stepValue(agent) orelse {
                    // 1. Perform ? Set(A, "length", 𝔽(k), true).
                    try array.set(agent, PropertyKey.from("length"), Value.from(k), .throw);

                    // 2. Return A.
                    return Value.from(array);
                };

                // v. If mapping is true, then
                const mapped_value = if (mapping) blk: {
                    // 1. Let mappedValue be Completion(Call(mapper, thisArg, « next, 𝔽(k) »)).
                    break :blk mapper.callAssumeCallable(
                        agent,
                        this_arg,
                        &.{ next, Value.from(k) },
                    ) catch |err| {
                        // 2. IfAbruptCloseIterator(mappedValue, iteratorRecord).
                        return iterator.close(agent, @as(Agent.Error!Value, err));
                    };
                } else blk: {
                    // vi. Else,
                    // 1. Let mappedValue be next.
                    break :blk next;
                };

                // vii. Let defineStatus be Completion(CreateDataPropertyOrThrow(A, Pk, mappedValue)).
                _ = array.createDataPropertyOrThrow(agent, property_key, mapped_value) catch |err| {
                    // viii. IfAbruptCloseIterator(defineStatus, iteratorRecord).
                    return iterator.close(agent, @as(Agent.Error!Value, err));
                };

                // ix. Set k to k + 1.
            }
        }

        // 6. NOTE: items is not iterable so assume it is an array-like object.
        // 7. Let arrayLike be ! ToObject(items).
        const array_like = items.toObject(agent) catch |err| try noexcept(err);

        // 8. Let len be ? LengthOfArrayLike(arrayLike).
        const len = try array_like.lengthOfArrayLike(agent);

        // 9. If IsConstructor(C) is true, then
        const array = if (constructor_.isConstructor()) blk: {
            // a. Let A be ? Construct(C, « 𝔽(len) »).
            break :blk try constructor_.asObject().construct(agent, &.{Value.from(len)}, null);
        } else blk: {
            // 10. Else,
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
            const k_value = try array_like.get(agent, property_key);

            // c. If mapping is true, then
            const mapped_value = if (mapping) blk: {
                // i. Let mappedValue be ? Call(mapper, thisArg, « kValue, 𝔽(k) »).
                break :blk try mapper.callAssumeCallable(
                    agent,
                    this_arg,
                    &.{ k_value, Value.from(k) },
                );
            } else blk: {
                // d. Else,
                // i. Let mappedValue be kValue.
                break :blk k_value;
            };

            // e. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
            try array.createDataPropertyOrThrow(agent, property_key, mapped_value);

            // f. Set k to k + 1.
        }

        // 13. Perform ? Set(A, "length", 𝔽(len), true).
        try array.set(agent, PropertyKey.from("length"), Value.from(len), .throw);

        // 14. Return A.
        return Value.from(array);
    }

    /// 2.1.1.1 Array.fromAsync ( asyncItems [ , mapper [ , thisArg ] ] )
    /// https://tc39.es/proposal-array-from-async/#sec-array.fromAsync
    fn fromAsync(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let C be the this value.

        // 2. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        const Captures = struct {
            constructor: Value,
            async_items: Value,
            mapper: Value,
            this_arg: Value,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .constructor = this_value,
            .async_items = arguments.get(0),
            .mapper = arguments.get(1),
            .this_arg = arguments.get(2),
        };

        // 3. Let fromAsyncClosure be a new Abstract Closure with no parameters that captures C,
        //    asyncItems, mapper, and thisArg and performs the following steps when called:
        const fromAsyncClosure = struct {
            fn func(agent_: *Agent, captures_: SafePointer) Agent.Error!Completion {
                const constructor_ = captures_.cast(*Captures).constructor;
                const async_items = captures_.cast(*Captures).async_items;
                const mapper = captures_.cast(*Captures).mapper;
                const this_arg = captures_.cast(*Captures).this_arg;

                // a. If mapper is undefined, let mapping be false.
                const mapping = if (mapper.isUndefined()) blk: {
                    // a. Let mapping be false.
                    break :blk false;
                } else blk: {
                    // b. Else,
                    // i. If IsCallable(mapper) is false, throw a TypeError exception.
                    if (!mapper.isCallable()) {
                        return agent_.throwException(.type_error, "{f} is not callable", .{mapper});
                    }

                    // ii. Let mapping be true.
                    break :blk true;
                };

                // c. Let usingAsyncIterator be ? GetMethod(asyncItems, @@asyncIterator).
                const using_async_iterator = try async_items.getMethod(
                    agent_,
                    PropertyKey.from(agent_.well_known_symbols.@"%Symbol.asyncIterator%"),
                );

                var using_sync_iterator: ?*Object = undefined;

                // d. If usingAsyncIterator is undefined, then
                if (using_async_iterator == null) {
                    // i. Let usingSyncIterator be ? GetMethod(asyncItems, @@iterator).
                    using_sync_iterator = try async_items.getMethod(
                        agent_,
                        PropertyKey.from(agent_.well_known_symbols.@"%Symbol.iterator%"),
                    );
                }

                // e. Let iteratorRecord be undefined.
                var maybe_iterator: ?Iterator = null;

                // f. If usingAsyncIterator is not undefined, then
                if (using_async_iterator) |async_iterator| {
                    // i. Set iteratorRecord to ? GetIteratorFromMethod(asyncItems, usingAsyncIterator).
                    maybe_iterator = try getIteratorFromMethod(agent_, async_items, async_iterator);
                } else if (using_sync_iterator) |sync_iterator| {
                    // g. Else if usingSyncIterator is not undefined, then
                    // i. Set iteratorRecord to ? CreateAsyncFromSyncIterator(
                    //    ? GetIteratorFromMethod(asyncItems, usingSyncIterator)).
                    maybe_iterator = try createAsyncFromSyncIterator(
                        agent_,
                        try getIteratorFromMethod(agent_, async_items, sync_iterator),
                    );
                }

                // h. If iteratorRecord is not undefined, then
                if (maybe_iterator) |iterator| {
                    // i. If IsConstructor(C) is true, then
                    const array = if (constructor_.isConstructor()) blk: {
                        // 1. Let A be ? Construct(C).
                        break :blk try constructor_.asObject().construct(agent_, &.{}, null);
                    } else blk: {
                        // ii. Else,
                        // 1. Let A be ! ArrayCreate(0).
                        break :blk arrayCreate(agent_, 0, null) catch |err| try noexcept(err);
                    };

                    // iii. Let k be 0.
                    var k: u53 = 0;

                    // iv. Repeat,
                    while (true) : (k += 1) {
                        // 1. If k ≥ 2**53 - 1, then
                        if (k == std.math.maxInt(u53)) {
                            // a. Let error be ThrowCompletion(a newly created TypeError object).
                            const @"error" = agent_.throwException(
                                .type_error,
                                "Maximum array length exceeded",
                                .{},
                            );

                            // b. Return ? AsyncIteratorClose(iteratorRecord, error).
                            return iterator.closeAsync(agent_, @as(Agent.Error!Completion, @"error"));
                        }

                        // 2. Let Pk be ! ToString(𝔽(k)).
                        const property_key = PropertyKey.from(k);

                        // 3. Let nextResult be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
                        var next_result_value = try iterator.next_method.callAssumeCallable(
                            agent_,
                            Value.from(iterator.iterator),
                            &.{},
                        );

                        // 4. Set nextResult to ? Await(nextResult).
                        next_result_value = try await(agent_, next_result_value);

                        // 5. If nextResult is not an Object, throw a TypeError exception.
                        if (!next_result_value.isObject()) {
                            return agent_.throwException(.type_error, "{f} is not an Object", .{next_result_value});
                        }
                        const next_result = next_result_value.asObject();

                        // 6. Let done be ? IteratorComplete(nextResult).
                        const done = try Iterator.complete(agent_, next_result);

                        // 7. If done is true,
                        if (done) {
                            // a. Perform ? Set(A, "length", 𝔽(k), true).
                            try array.set(agent_, PropertyKey.from("length"), Value.from(k), .throw);

                            // b. Return Completion Record { [[Type]]: return, [[Value]]: A, [[Target]]: empty }.
                            return .@"return"(Value.from(array));
                        }

                        // a. Perform ? Set(A, "length", 𝔽(k), true).
                        try array.set(agent_, PropertyKey.from("length"), Value.from(k), .throw);

                        // 8. Let nextValue be ? IteratorValue(nextResult).
                        const next_value = try Iterator.value(agent_, next_result);

                        // 9. If mapping is true, then
                        const mapped_value = if (mapping) blk: {
                            // a. Let mappedValue be Call(mapper, thisArg, « nextValue, 𝔽(k) »).
                            var mapped_value = mapper.callAssumeCallable(
                                agent_,
                                this_arg,
                                &.{ next_value, Value.from(k) },
                            ) catch |err| switch (err) {
                                error.OutOfMemory => return error.OutOfMemory,
                                error.ExceptionThrown => {
                                    // b. IfAbruptCloseAsyncIterator(mappedValue, iteratorRecord).
                                    return iterator.closeAsync(agent_, @as(Agent.Error!Completion, err));
                                },
                            };

                            // c. Set mappedValue to Await(mappedValue).
                            mapped_value = await(agent_, mapped_value) catch |err| switch (err) {
                                error.OutOfMemory => return error.OutOfMemory,
                                error.ExceptionThrown => {
                                    // d. IfAbruptCloseAsyncIterator(mappedValue, iteratorRecord).
                                    return iterator.closeAsync(agent_, @as(Agent.Error!Completion, err));
                                },
                            };

                            break :blk mapped_value;
                        } else blk: {
                            // 10. Else, let mappedValue be nextValue.
                            break :blk next_value;
                        };

                        // 11. Let defineStatus be CreateDataPropertyOrThrow(A, Pk, mappedValue).
                        array.createDataPropertyOrThrow(agent_, property_key, mapped_value) catch |err| switch (err) {
                            error.OutOfMemory => return error.OutOfMemory,
                            error.ExceptionThrown => {
                                // 12. If defineStatus is an abrupt completion, return ? AsyncIteratorClose(iteratorRecord, defineStatus).
                                return iterator.closeAsync(agent_, @as(Agent.Error!Completion, err));
                            },
                        };

                        // 13. Set k to k + 1.
                    }
                } else {
                    // i. Else,
                    // i. NOTE: asyncItems is neither an AsyncIterable nor an Iterable so assume it
                    //    is an array-like object.

                    // ii. Let arrayLike be ! ToObject(asyncItems).
                    const array_like = async_items.toObject(agent_) catch |err| try noexcept(err);

                    // iii. Let len be ? LengthOfArrayLike(arrayLike).
                    const len = try array_like.lengthOfArrayLike(agent_);

                    // iv. If IsConstructor(C) is true, then
                    const array = if (constructor_.isConstructor()) blk: {
                        // 1. Let A be ? Construct(C, « 𝔽(len) »).
                        break :blk try constructor_.asObject().construct(
                            agent_,
                            &.{Value.from(len)},
                            null,
                        );
                    } else blk: {
                        // v. Else,
                        // 1. Let A be ? ArrayCreate(len).
                        break :blk try arrayCreate(agent_, len, null);
                    };

                    // vi. Let k be 0.
                    var k: u53 = 0;

                    // vii. Repeat, while k < len,
                    while (k < len) : (k += 1) {
                        // 1. Let Pk be ! ToString(𝔽(k)).
                        const property_key = PropertyKey.from(k);

                        // 2. Let kValue be ? Get(arrayLike, Pk).
                        var k_value = try array_like.get(agent_, property_key);

                        // 3. Set kValue to ? Await(kValue).
                        k_value = try await(agent_, k_value);

                        // 4. If mapping is true, then
                        const mapped_value = if (mapping) blk: {
                            // a. Let mappedValue be ? Call(mapper, thisArg, « kValue, 𝔽(k) »).
                            var mapped_value = try mapper.callAssumeCallable(
                                agent_,
                                this_arg,
                                &.{ k_value, Value.from(k) },
                            );

                            // b. Set mappedValue to ? Await(mappedValue).
                            mapped_value = try await(agent_, mapped_value);

                            break :blk mapped_value;
                        } else blk: {
                            // 5. Else, let mappedValue be kValue.
                            break :blk k_value;
                        };

                        // 6. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
                        try array.createDataPropertyOrThrow(agent_, property_key, mapped_value);

                        // 7. Set k to k + 1.
                    }

                    // viii. Perform ? Set(A, "length", 𝔽(len), true).
                    try array.set(agent_, PropertyKey.from("length"), Value.from(len), .throw);

                    // ix. Return Completion Record { [[Type]]: return, [[Value]]: A, [[Target]]: empty }.
                    return .@"return"(Value.from(array));
                }
            }
        }.func;

        // 4. Perform AsyncFunctionStart(promiseCapability, fromAsyncClosure).
        try asyncFunctionStart(agent, promise_capability, .{
            .abstract_closure = .{
                .func = fromAsyncClosure,
                .captures = .make(*Captures, captures),
            },
        });

        // 5. Return promiseCapability.[[Promise]].
        return Value.from(promise_capability.promise);
    }

    /// 23.1.2.2 Array.isArray ( arg )
    /// https://tc39.es/ecma262/#sec-array.isarray
    fn isArray(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const arg = arguments.get(0);

        // 1. Return ? IsArray(arg).
        return Value.from(try arg.isArray(agent));
    }

    /// 23.1.2.3 Array.of ( ...items )
    /// https://tc39.es/ecma262/#sec-array.of
    fn of(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let len be the number of elements in items.
        const len: u53 = @intCast(arguments.count());

        // 2. Let lenNumber be 𝔽(len).
        const len_number = Value.from(len);

        // 3. Let C be the this value.
        const constructor_ = this_value;

        // 4. If IsConstructor(C) is true, then
        const array = blk: {
            if (constructor_.isConstructor()) {
                // a. Let A be ? Construct(C, « lenNumber »).
                break :blk try constructor_.asObject().construct(agent, &.{len_number}, null);
            } else {
                // 5. Else,
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
            try array.createDataPropertyOrThrow(agent, property_key, k_value);

            // d. Set k to k + 1.
        }

        // 8. Perform ? Set(A, "length", lenNumber, true).
        try array.set(agent, PropertyKey.from("length"), len_number, .throw);

        // 9. Return A.
        return Value.from(array);
    }

    /// 23.1.2.5 get Array [ %Symbol.species% ]
    /// https://tc39.es/ecma262/#sec-get-array-%symbol.species%
    fn @"%Symbol.species%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 23.1.3 Properties of the Array Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-array-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return arrayCreate(
            agent,
            0,
            try realm.intrinsics.@"%Object.prototype%"(),
        ) catch |err| try noexcept(err);
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "at", at, 1, realm);
        try object.defineBuiltinFunction(agent, "concat", concat, 1, realm);
        try object.defineBuiltinFunction(agent, "copyWithin", copyWithin, 2, realm);
        try object.defineBuiltinFunction(agent, "entries", entries, 0, realm);
        try object.defineBuiltinFunction(agent, "every", every, 1, realm);
        try object.defineBuiltinFunction(agent, "fill", fill, 1, realm);
        try object.defineBuiltinFunction(agent, "filter", filter, 1, realm);
        try object.defineBuiltinFunction(agent, "find", find, 1, realm);
        try object.defineBuiltinFunction(agent, "findIndex", findIndex, 1, realm);
        try object.defineBuiltinFunction(agent, "findLast", findLast, 1, realm);
        try object.defineBuiltinFunction(agent, "findLastIndex", findLastIndex, 1, realm);
        try object.defineBuiltinFunction(agent, "flat", flat, 0, realm);
        try object.defineBuiltinFunction(agent, "flatMap", flatMap, 1, realm);
        try object.defineBuiltinFunction(agent, "forEach", forEach, 1, realm);
        try object.defineBuiltinFunction(agent, "includes", includes, 1, realm);
        try object.defineBuiltinFunction(agent, "indexOf", indexOf, 1, realm);
        try object.defineBuiltinFunction(agent, "join", join, 1, realm);
        try object.defineBuiltinFunction(agent, "keys", keys, 0, realm);
        try object.defineBuiltinFunction(agent, "lastIndexOf", lastIndexOf, 1, realm);
        try object.defineBuiltinFunction(agent, "map", map, 1, realm);
        try object.defineBuiltinFunction(agent, "pop", pop, 0, realm);
        try object.defineBuiltinFunction(agent, "push", push, 1, realm);
        try object.defineBuiltinFunction(agent, "reduce", reduce, 1, realm);
        try object.defineBuiltinFunction(agent, "reduceRight", reduceRight, 1, realm);
        try object.defineBuiltinFunction(agent, "reverse", reverse, 0, realm);
        try object.defineBuiltinFunction(agent, "shift", shift, 0, realm);
        try object.defineBuiltinFunction(agent, "slice", slice, 2, realm);
        try object.defineBuiltinFunction(agent, "some", some, 1, realm);
        try object.defineBuiltinFunction(agent, "sort", sort, 1, realm);
        try object.defineBuiltinFunction(agent, "splice", splice, 2, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toReversed", toReversed, 0, realm);
        try object.defineBuiltinFunction(agent, "toSorted", toSorted, 1, realm);
        try object.defineBuiltinFunction(agent, "toSpliced", toSpliced, 2, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "unshift", unshift, 1, realm);
        try object.defineBuiltinFunction(agent, "values", values, 0, realm);
        try object.defineBuiltinFunction(agent, "with", with, 2, realm);

        // 23.1.3.3 Array.prototype.constructor
        // https://tc39.es/ecma262/#sec-array.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Array%"()),
        );

        // 23.1.3.40 Array.prototype [ %Symbol.iterator% ] ( )
        // https://tc39.es/ecma262/#sec-array.prototype-%symbol.iterator%
        // NOTE: We can't use the intrinsic getter for this while creating the underlying prototype
        //       object, as it hasn't been finalized yet.
        const @"%Array.prototype.values%" = object.getPropertyValueDirect(PropertyKey.from("values"));
        try object.defineBuiltinProperty(agent, "%Symbol.iterator%", @"%Array.prototype.values%");

        // 23.1.3.41 Array.prototype [ %Symbol.unscopables% ]
        // https://tc39.es/ecma262/#sec-array.prototype-%symbol.unscopables%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.unscopables%",
            blk: {
                // 1. Let unscopableList be OrdinaryObjectCreate(null).
                const unscopable_list = try ordinaryObjectCreate(agent, null);

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
                    try unscopable_list.createDataPropertyDirect(
                        agent,
                        PropertyKey.from(name),
                        Value.from(true),
                    );
                }

                // 18. Return unscopableList.
                break :blk Value.from(unscopable_list);
            },
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // Ensure function intrinsics are set right after the object is created
        _ = try realm.intrinsics.@"%Array.prototype.toString%"();
        _ = try realm.intrinsics.@"%Array.prototype.values%"();
    }

    /// 23.1.3.1 Array.prototype.at ( index )
    /// https://tc39.es/ecma262/#sec-array.prototype.at
    fn at(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const index = arguments.get(0);
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

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
        return object.get(agent, PropertyKey.from(k));
    }

    /// 23.1.3.2 Array.prototype.concat ( ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.concat
    fn concat(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
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
                const len = try element.asObject().lengthOfArrayLike(agent);

                // ii. If n + len > 2**53 - 1, throw a TypeError exception.
                _ = std.math.add(u53, n, len) catch {
                    return agent.throwException(.type_error, "Maximum array length exceeded", .{});
                };

                // iii. Let k be 0.
                var k: u53 = 0;

                // iv. Repeat, while k < len,
                while (k < len) : ({
                    n += 1;
                    k += 1;
                }) {
                    // 1. Let Pk be ! ToString(𝔽(k)).
                    const property_key = PropertyKey.from(k);

                    // 2. Let exists be ? HasProperty(E, Pk).
                    const exists = try element.asObject().hasProperty(agent, property_key);

                    // 3. If exists is true, then
                    if (exists) {
                        // a. Let subElement be ? Get(E, Pk).
                        const sub_element = try element.asObject().get(agent, property_key);

                        // b. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), subElement).
                        try array.createDataPropertyOrThrow(
                            agent,
                            PropertyKey.from(n),
                            sub_element,
                        );
                    }

                    // 4. Set n to n + 1.
                    // 5. Set k to k + 1.
                }
            } else {
                // c. Else,
                // i. NOTE: E is added as a single item rather than spread.

                // ii. If n ≥ 2**53 - 1, throw a TypeError exception.
                if (n == std.math.maxInt(u53)) {
                    return agent.throwException(.type_error, "Maximum array length exceeded", .{});
                }

                // iii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), E).
                try array.createDataPropertyOrThrow(agent, PropertyKey.from(n), element);

                // iv. Set n to n + 1.
                n += 1;
            }
        }

        // 6. Perform ? Set(A, "length", 𝔽(n), true).
        try array.set(agent, PropertyKey.from("length"), Value.from(n), .throw);

        // 7. Return A.
        return Value.from(array);
    }

    /// 23.1.3.2.1 IsConcatSpreadable ( O )
    /// https://tc39.es/ecma262/#sec-isconcatspreadable
    fn isConcatSpreadable(agent: *Agent, value: Value) Agent.Error!bool {
        // 1. If O is not an Object, return false.
        if (!value.isObject()) return false;

        // 2. Let spreadable be ? Get(O, %Symbol.isConcatSpreadable%).
        const spreadable = try value.asObject().get(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.isConcatSpreadable%"),
        );

        // 3. If spreadable is not undefined, return ToBoolean(spreadable).
        if (!spreadable.isUndefined()) return spreadable.toBoolean();

        // 4. Return ? IsArray(O).
        return value.isArray(agent);
    }

    /// 23.1.3.4 Array.prototype.copyWithin ( target, start [ , end ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.copywithin
    fn copyWithin(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const start = arguments.get(1);
        const end = arguments.get(2);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);
        const len_f64: f64 = @floatFromInt(len);

        // 3. Let relativeTarget be ? ToIntegerOrInfinity(target).
        const relative_target = try target.toIntegerOrInfinity(agent);

        // 4. If relativeTarget = -∞, let to be 0.
        const to_f64 = if (std.math.isNegativeInf(relative_target)) blk: {
            break :blk 0;
        } else if (relative_target < 0) blk: {
            // 5. Else if relativeTarget < 0, let to be max(len + relativeTarget, 0).
            break :blk @max(len_f64 + relative_target, 0);
        } else blk: {
            // 6. Else, let to be min(relativeTarget, len).
            break :blk @min(relative_target, len_f64);
        };
        var to: u53 = @intFromFloat(to_f64);

        // 7. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 8. If relativeStart = -∞, let from be 0.
        const from_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        } else if (relative_start < 0) blk: {
            // 9. Else if relativeStart < 0, let from be max(len + relativeStart, 0).
            break :blk @max(len_f64 + relative_start, 0);
        } else blk: {
            // 10. Else, let from be min(relativeStart, len).
            break :blk @min(relative_start, len_f64);
        };
        var from: u53 = @intFromFloat(from_f64);

        // 11. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //     ? ToIntegerOrInfinity(end).
        const relative_end = if (end.isUndefined())
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 12. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        } else if (relative_end < 0) blk: {
            // 13. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
            break :blk @max(len_f64 + relative_end, 0);
        } else blk: {
            // 14. Else, let final be min(relativeEnd, len).
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
        } else blk: {
            // 17. Else,
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
            const from_present = try object.hasProperty(agent, from_key);

            // d. If fromPresent is true, then
            if (from_present) {
                // i. Let fromValue be ? Get(O, fromKey).
                const from_value = try object.get(agent, from_key);

                // ii. Perform ? Set(O, toKey, fromValue, true).
                try object.set(agent, to_key, from_value, .throw);
            } else {
                // e. Else,
                // i. Assert: fromPresent is false.
                std.debug.assert(!from_present);

                // ii. Perform ? DeletePropertyOrThrow(O, toKey).
                try object.deletePropertyOrThrow(agent, to_key);
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
    fn entries(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return CreateArrayIterator(O, key+value).
        return Value.from(try createArrayIterator(agent, object, .@"key+value"));
    }

    /// 23.1.3.6 Array.prototype.every ( callback [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.every
    fn every(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
        }

        // 4. Let k be 0.
        var k: u53 = 0;

        // OPTIMIZATION: Use fast path if applicable
        if (try array_fast_paths.every(
            agent,
            object,
            len,
            callback,
            this_arg,
        )) |result| switch (result) {
            .done => |value| return Value.from(value),
            .continue_slow => |index| k = @intCast(index),
        };

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Let testResult be ToBoolean(? Call(callback, thisArg, « kValue, 𝔽(k), O »)).
                const test_result = (try callback.callAssumeCallable(
                    agent,
                    this_arg,
                    &.{ k_value, Value.from(k), Value.from(object) },
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
    fn fill(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);
        const start = arguments.get(1);
        const end = arguments.get(2);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);
        const len_f64: f64 = @floatFromInt(len);

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 4. If relativeStart = -∞, let k be 0.
        const k_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        } else if (relative_start < 0) blk: {
            // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
            break :blk @max(len_f64 + relative_start, 0);
        } else blk: {
            // 6. Else, let k be min(relativeStart, len).
            break :blk @min(relative_start, len_f64);
        };
        var k: u53 = @intFromFloat(k_f64);

        // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //    ? ToIntegerOrInfinity(end).
        const relative_end = if (end.isUndefined())
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 8. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        } else if (relative_end < 0) blk: {
            // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
            break :blk @max(len_f64 + relative_end, 0);
        } else blk: {
            // 10. Else, let final be min(relativeEnd, len).
            break :blk @min(relative_end, len_f64);
        };
        const final: u53 = @intFromFloat(final_f64);

        // OPTIMIZATION: Use fast path if applicable
        if (try array_fast_paths.fill(
            agent.gc_allocator,
            object,
            len,
            k,
            final,
            value,
        )) |_| {
            return Value.from(object);
        }

        // 11. Repeat, while k < final,
        while (k < final) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Perform ? Set(O, Pk, value, true).
            try object.set(agent, property_key, value, .throw);

            // c. Set k to k + 1.
        }

        // 12. Return O.
        return Value.from(object);
    }

    /// 23.1.3.8 Array.prototype.filter ( callback [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.filter
    fn filter(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
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
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Let selected be ToBoolean(? Call(callback, thisArg, « kValue, 𝔽(k), O »)).
                const selected = (try callback.callAssumeCallable(
                    agent,
                    this_arg,
                    &.{ k_value, Value.from(k), Value.from(object) },
                )).toBoolean();

                // iii. If selected is true, then
                if (selected) {
                    // 1. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(to)), kValue).
                    try array.createDataPropertyOrThrow(agent, PropertyKey.from(to), k_value);

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
    fn find(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
        const find_record = try findViaPredicate(
            agent,
            object,
            len,
            .ascending,
            predicate,
            this_arg,
        );

        // 4. Return findRec.[[Value]].
        return find_record.value;
    }

    /// 23.1.3.10 Array.prototype.findIndex ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.findindex
    fn findIndex(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
        const find_record = try findViaPredicate(
            agent,
            object,
            len,
            .ascending,
            predicate,
            this_arg,
        );

        // 4. Return findRec.[[Index]].
        return find_record.index;
    }

    /// 23.1.3.11 Array.prototype.findLast ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.findlast
    fn findLast(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
        const find_record = try findViaPredicate(
            agent,
            object,
            len,
            .descending,
            predicate,
            this_arg,
        );

        // 4. Return findRec.[[Value]].
        return find_record.value;
    }

    /// 23.1.3.12 Array.prototype.findLastIndex ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.findlastindex
    fn findLastIndex(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
        const find_record = try findViaPredicate(
            agent,
            object,
            len,
            .descending,
            predicate,
            this_arg,
        );

        // 4. Return findRec.[[Index]].
        return find_record.index;
    }
    /// 23.1.3.13 Array.prototype.flat ( [ depth ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.flat
    fn flat(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const depth = arguments.get(0);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let sourceLen be ? LengthOfArrayLike(O).
        const source_len = try object.lengthOfArrayLike(agent);

        // 3. Let depthNum be 1.
        var depth_num: f64 = 1;

        // 4. If depth is not undefined, then
        if (!depth.isUndefined()) {
            // a. Set depthNum to ? ToIntegerOrInfinity(depth).
            depth_num = try depth.toIntegerOrInfinity(agent);

            // b. If depthNum < 0, set depthNum to 0.
            if (depth_num < 0) depth_num = 0;
        }

        // 5. Let A be ? ArraySpeciesCreate(O, 0).
        const array = try arraySpeciesCreate(agent, object, 0);

        // 6. Perform ? FlattenIntoArray(A, O, sourceLen, 0, depthNum).
        _ = try flattenIntoArray(agent, array, object, source_len, 0, depth_num, null, null);

        // 7. Return A.
        return Value.from(array);
    }

    /// 23.1.3.13.1 FlattenIntoArray ( target, source, sourceLen, start, depth [ , mapperFunction [ , thisArg ] ] )
    /// https://tc39.es/ecma262/#sec-flattenintoarray
    fn flattenIntoArray(
        agent: *Agent,
        target: *Object,
        source: *Object,
        source_len: u53,
        start: f64,
        depth: f64,
        mapper_function: ?*Object,
        this_arg: ?Value,
    ) Agent.Error!f64 {
        // 1. Assert: If mapperFunction is present, then IsCallable(mapperFunction) is true,
        //    thisArg is present, and depth is 1.
        if (mapper_function != null) {
            std.debug.assert(Value.from(mapper_function.?).isCallable());
            std.debug.assert(this_arg != null);
            std.debug.assert(depth == 1);
        }

        // 2. Let targetIndex be start.
        var target_index = start;

        // 3. Let sourceIndex be +0𝔽.
        var source_index: u53 = 0;

        // 4. Repeat, while ℝ(sourceIndex) < sourceLen,
        while (source_index < source_len) : (source_index += 1) {
            // a. Let P be ! ToString(sourceIndex).
            const property_key = PropertyKey.from(source_index);

            // b. Let exists be ? HasProperty(source, P).
            const exists = try source.hasProperty(agent, property_key);

            // c. If exists is true, then
            if (exists) {
                // i. Let element be ? Get(source, P).
                var element = try source.get(agent, property_key);

                // ii. If mapperFunction is present, then
                if (mapper_function != null) {
                    // 1. Set element to ? Call(mapperFunction, thisArg, « element, sourceIndex, source »).
                    element = try Value.from(mapper_function.?).callAssumeCallable(
                        agent,
                        this_arg.?,
                        &.{ element, Value.from(source_index), Value.from(source) },
                    );
                }

                // iii. Let shouldFlatten be false.
                var should_flatten = false;

                // iv. If depth > 0, then
                if (depth > 0) {
                    // 1. Set shouldFlatten to ? IsArray(element).
                    should_flatten = try element.isArray(agent);
                }

                // v. If shouldFlatten is true, then
                if (should_flatten) {
                    // 1. If depth = +∞, let newDepth be +∞.
                    // 2. Else, let newDepth be depth - 1.
                    const new_depth = if (std.math.isPositiveInf(depth))
                        std.math.inf(f64)
                    else
                        depth - 1;

                    // 3. Let elementLen be ? LengthOfArrayLike(element).
                    const element_len = try element.asObject().lengthOfArrayLike(agent);

                    // NOTE: flattenIntoArray() is being called recursively here.
                    try agent.checkStackOverflow();

                    // 4. Set targetIndex to ? FlattenIntoArray(target, element, elementLen,
                    //    targetIndex, newDepth).
                    target_index = try flattenIntoArray(
                        agent,
                        target,
                        element.asObject(),
                        element_len,
                        target_index,
                        new_depth,
                        null,
                        null,
                    );
                } else {
                    // vi. Else,
                    // 1. If targetIndex ≥ 2**53 - 1, throw a TypeError exception.
                    if (target_index >= std.math.maxInt(u53)) {
                        return agent.throwException(.type_error, "Maximum array length exceeded", .{});
                    }

                    // 2. Perform ? CreateDataPropertyOrThrow(target, ! ToString(𝔽(targetIndex)), element).
                    try target.createDataPropertyOrThrow(
                        agent,
                        PropertyKey.from(@as(PropertyKey.IntegerIndex, @intFromFloat(target_index))),
                        element,
                    );

                    // 3. Set targetIndex to targetIndex + 1.
                    target_index += 1;
                }
            }

            // d. Set sourceIndex to sourceIndex + 1𝔽.
        }

        // 5. Return targetIndex.
        return target_index;
    }

    /// 23.1.3.14 Array.prototype.flatMap ( mapperFunction [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.flatmap
    fn flatMap(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const mapper_function = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let sourceLen be ? LengthOfArrayLike(O).
        const source_len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(mapperFunction) is false, throw a TypeError exception.
        if (!mapper_function.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{mapper_function});
        }

        // 4. Let A be ? ArraySpeciesCreate(O, 0).
        const array = try arraySpeciesCreate(agent, object, 0);

        // 5. Perform ? FlattenIntoArray(A, O, sourceLen, 0, 1, mapperFunction, thisArg).
        _ = try flattenIntoArray(
            agent,
            array,
            object,
            source_len,
            0,
            1,
            mapper_function.asObject(),
            this_arg,
        );

        // 6. Return A.
        return Value.from(array);
    }

    /// 23.1.3.15 Array.prototype.forEach ( callback [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
        }

        // 4. Let k be 0.
        var k: u53 = 0;

        // OPTIMIZATION: Use fast path if applicable
        if (try array_fast_paths.forEach(
            agent,
            object,
            len,
            callback,
            this_arg,
        )) |result| switch (result) {
            .done => return .undefined,
            .continue_slow => |index| k = @intCast(index),
        };

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Perform ? Call(callback, thisArg, « kValue, 𝔽(k), O »).
                _ = try callback.callAssumeCallable(
                    agent,
                    this_arg,
                    &.{ k_value, Value.from(k), Value.from(object) },
                );
            }

            // d. Set k to k + 1.
        }

        // 6. Return undefined.
        return .undefined;
    }

    /// 23.1.3.16 Array.prototype.includes ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.includes
    fn includes(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If len = 0, return false.
        if (len == 0) return Value.from(false);

        // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
        var n = try from_index.toIntegerOrInfinity(agent);

        // 5. Assert: If fromIndex is undefined, then n is 0.
        if (from_index.isUndefined()) std.debug.assert(n == 0);

        // 6. If n = +∞, return false.
        if (std.math.isPositiveInf(n)) return Value.from(false);

        // 7. Else if n = -∞, set n to 0.
        if (std.math.isNegativeInf(n)) n = 0;

        // 8. If n ≥ 0, then
        //     a. Let k be n.
        // 9. Else,
        //     a. Let k be len + n.
        //     b. If k < 0, set k to 0.
        const k_f64 = if (n >= 0) n else @max(@as(f64, @floatFromInt(len)) + n, 0);
        if (k_f64 >= std.math.maxInt(u53)) return Value.from(false);
        var k: u53 = @intFromFloat(k_f64);

        // OPTIMIZATION: Use fast path if applicable
        if (array_fast_paths.includes(object, len, k, search_element)) |result| {
            return Value.from(result);
        }

        // 10. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let elementK be ? Get(O, ! ToString(𝔽(k))).
            const element_k = try object.get(agent, PropertyKey.from(k));

            // b. If SameValueZero(searchElement, elementK) is true, return true.
            if (sameValueZero(search_element, element_k)) return Value.from(true);

            // c. Set k to k + 1.
        }

        // 11. Return false.
        return Value.from(false);
    }

    /// 23.1.3.17 Array.prototype.indexOf ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.indexof
    fn indexOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If len = 0, return -1𝔽.
        if (len == 0) return Value.from(-1);

        // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
        var n = try from_index.toIntegerOrInfinity(agent);

        // 5. Assert: If fromIndex is undefined, then n is 0.
        if (from_index.isUndefined()) std.debug.assert(n == 0);

        // 6. If n = +∞, return -1𝔽.
        if (std.math.isPositiveInf(n)) return Value.from(-1);

        // 7. Else if n = -∞, set n to 0.
        if (std.math.isNegativeInf(n)) n = 0;

        // 8. If n ≥ 0, then
        //     a. Let k be n.
        // 9. Else,
        //     a. Let k be len + n.
        //     b. If k < 0, set k to 0.
        const k_f64 = if (n >= 0) n else @max(@as(f64, @floatFromInt(len)) + n, 0);
        if (k_f64 >= std.math.maxInt(u53)) return Value.from(-1);
        var k: u53 = @intFromFloat(k_f64);

        // OPTIMIZATION: Use fast path if applicable
        if (array_fast_paths.indexOf(object, len, k, search_element)) |result| {
            return result;
        }

        // 10. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let elementK be ? Get(O, Pk).
                const element_k = try object.get(agent, property_key);

                // ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
                if (isStrictlyEqual(search_element, element_k)) return Value.from(k);
            }

            // d. Set k to k + 1.
        }

        // 11. Return -1𝔽.
        return Value.from(-1);
    }

    /// 23.1.3.18 Array.prototype.join ( separator )
    /// https://tc39.es/ecma262/#sec-array.prototype.join
    fn join(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const separator = arguments.get(0);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If separator is undefined, let sep be ",".
        // 4. Else, let sep be ? ToString(separator).
        const sep: String.Builder.Segment = if (separator.isUndefined())
            .{ .char = ',' }
        else
            .{ .string = try separator.toString(agent) };

        // OPTIMIZATION: If the array is empty the result will be an empty string
        if (len == 0) return Value.from(String.empty);

        // 5. Let R be the empty String.
        // NOTE: This allocates the maximum needed capacity upfront
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var result = try String.Builder.initCapacity(agent.gc_allocator, @intCast((len * 2) - 1));
        defer result.deinit(agent.gc_allocator);

        // 6. Let k be 0.
        var k: u53 = 0;

        // 7. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, set R to the string-concatenation of R and sep.
            if (k > 0) result.appendSegmentAssumeCapacity(sep);

            // b. Let element be ? Get(O, ! ToString(𝔽(k))).
            const element = try object.get(agent, PropertyKey.from(k));

            // c. If element is neither undefined nor null, then
            if (!element.isUndefined() and !element.isNull()) {
                // i. Let S be ? ToString(element).
                const string = try element.toString(agent);

                // ii. Set R to the string-concatenation of R and S.
                result.appendStringAssumeCapacity(string);
            }

            // d. Set k to k + 1.
        }

        // 8. Return R.
        return Value.from(try result.build(agent));
    }

    /// 23.1.3.19 Array.prototype.keys ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.keys
    fn keys(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return CreateArrayIterator(O, key).
        return Value.from(try createArrayIterator(agent, object, .key));
    }

    /// 23.1.3.20 Array.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.lastindexof
    fn lastIndexOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If len = 0, return -1𝔽.
        if (len == 0) return Value.from(-1);

        // 4. If fromIndex is present, let n be ? ToIntegerOrInfinity(fromIndex); else let n be len - 1.
        const n = if (arguments.count() > 1)
            try from_index.toIntegerOrInfinity(agent)
        else
            @as(f64, @floatFromInt(len)) - 1;

        // 5. If n = -∞, return -1𝔽.
        if (std.math.isNegativeInf(n)) return Value.from(-1);

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

        // OPTIMIZATION: Use fast path if applicable
        if (array_fast_paths.lastIndexOf(object, len, k, search_element)) |result| {
            return result;
        }

        // 8. Repeat, while k ≥ 0,
        while (k >= 0) : (k -|= 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let elementK be ? Get(O, Pk).
                const element_k = try object.get(agent, property_key);

                // ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
                if (isStrictlyEqual(search_element, element_k)) return Value.from(k);
            }

            // d. Set k to k - 1.
            if (k == 0) break;
        }

        // 9. Return -1𝔽.
        return Value.from(-1);
    }

    /// 23.1.3.21 Array.prototype.map ( callback [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.map
    fn map(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
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
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Let mappedValue be ? Call(callback, thisArg, « kValue, 𝔽(k), O »).
                const mapped_value = try callback.callAssumeCallable(
                    agent,
                    this_arg,
                    &.{ k_value, Value.from(k), Value.from(object) },
                );

                // iii. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
                try array.createDataPropertyOrThrow(agent, property_key, mapped_value);
            }

            // d. Set k to k + 1.
        }

        // 7. Return A.
        return Value.from(array);
    }

    /// 23.1.3.22 Array.prototype.pop ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.pop
    fn pop(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // If len = 0, then
        if (len == 0) {
            // a. Perform ? Set(O, "length", +0𝔽, true).
            try object.set(agent, PropertyKey.from("length"), Value.from(0), .throw);

            // b. Return undefined.
            return .undefined;
        } else {
            // 4. Else,
            // a. Assert: len > 0.
            std.debug.assert(len > 0);

            // b. Let newLen be 𝔽(len - 1).
            const new_len = len - 1;

            // c. Let index be ! ToString(newLen).
            const property_key = PropertyKey.from(new_len);

            // d. Let element be ? Get(O, index).
            const element = try object.get(agent, property_key);

            // e. Perform ? DeletePropertyOrThrow(O, index).
            try object.deletePropertyOrThrow(agent, property_key);

            // f. Perform ? Set(O, "length", newLen, true).
            try object.set(agent, PropertyKey.from("length"), Value.from(new_len), .throw);

            // g. Return element.
            return element;
        }
    }

    /// 23.1.3.23 Array.prototype.push ( ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.push
    fn push(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        var len = try object.lengthOfArrayLike(agent);

        // 3. Let argCount be the number of elements in items.
        const arg_count: u53 = @intCast(arguments.count());

        // 4. If len + argCount > 2**53 - 1, throw a TypeError exception.
        _ = std.math.add(u53, len, arg_count) catch {
            return agent.throwException(.type_error, "Maximum array length exceeded", .{});
        };

        // 5. For each element E of items, do
        for (arguments.values) |element| {
            // a. Perform ? Set(O, ! ToString(𝔽(len)), E, true).
            try object.set(agent, PropertyKey.from(len), element, .throw);

            // b. Set len to len + 1.
            len += 1;
        }

        // 6. Perform ? Set(O, "length", 𝔽(len), true).
        try object.set(agent, PropertyKey.from("length"), Value.from(len), .throw);

        // 7. Return 𝔽(len).
        return Value.from(len);
    }

    /// 23.1.3.24 Array.prototype.reduce ( callback [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.reduce
    fn reduce(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
        }

        // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
        if (len == 0 and initial_value == null) {
            return agent.throwException(
                .type_error,
                "Cannot reduce empty array without initial value",
                .{},
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
        } else {
            // 8. Else,
            // a. Let kPresent be false.
            var k_present = false;

            // b. Repeat, while kPresent is false and k < len,
            while (!k_present and k < len) : (k += 1) {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k);

                // ii. Set kPresent to ? HasProperty(O, Pk).
                k_present = try object.hasProperty(agent, property_key);

                // iii. If kPresent is true, then
                if (k_present) {
                    // 1. Set accumulator to ? Get(O, Pk).
                    accumulator = try object.get(agent, property_key);
                }

                // iv. Set k to k + 1.
            }

            // c. If kPresent is false, throw a TypeError exception.
            if (!k_present) {
                return agent.throwException(
                    .type_error,
                    "Cannot reduce empty array without initial value",
                    .{},
                );
            }
        }

        // 9. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Set accumulator to ? Call(callback, undefined, « accumulator, kValue, 𝔽(k), O »).
                accumulator = try callback.callAssumeCallable(
                    agent,
                    .undefined,
                    &.{ accumulator, k_value, Value.from(k), Value.from(object) },
                );
            }

            // d. Set k to k + 1.
        }

        // 10. Return accumulator.
        return accumulator;
    }

    /// 23.1.3.25 Array.prototype.reduceRight ( callback [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.reduceright
    fn reduceRight(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
        }

        // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
        if (len == 0 and initial_value == null) {
            return agent.throwException(
                .type_error,
                "Cannot reduce empty array without initial value",
                .{},
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
        } else {
            // 8. Else,
            // a. Let kPresent be false.
            var k_present = false;

            // b. Repeat, while kPresent is false and k ≥ 0,
            while (!k_present and k != null) : (k = (std.math.sub(u53, k.?, 1) catch null)) {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k.?);

                // ii. Set kPresent to ? HasProperty(O, Pk).
                k_present = try object.hasProperty(agent, property_key);

                // iii. If kPresent is true, then
                if (k_present) {
                    // 1. Set accumulator to ? Get(O, Pk).
                    accumulator = try object.get(agent, property_key);
                }

                // iv. Set k to k - 1.
            }

            // c. If kPresent is false, throw a TypeError exception.
            if (!k_present) {
                return agent.throwException(
                    .type_error,
                    "Cannot reduce empty array without initial value",
                    .{},
                );
            }
        }

        // 9. Repeat, while k ≥ 0,
        while (k != null) : (k = (std.math.sub(u53, k.?, 1) catch null)) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k.?);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Set accumulator to ? Call(callback, undefined, « accumulator, kValue, 𝔽(k), O »).
                accumulator = try callback.callAssumeCallable(
                    agent,
                    .undefined,
                    &.{ accumulator, k_value, Value.from(k.?), Value.from(object) },
                );
            }

            // d. Set k to k - 1.
        }

        // 10. Return accumulator.
        return accumulator;
    }

    /// 23.1.3.26 Array.prototype.reverse ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.reverse
    fn reverse(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // OPTIMIZATION: Use fast path if applicable
        if (array_fast_paths.reverse(object, len)) |_| {
            return Value.from(object);
        }

        // 3. Let middle be floor(len / 2).
        const middle = len / 2;

        // 4. Let lower be 0.
        var lower: u53 = 0;

        // 5. Repeat, while lower ≠ middle,
        while (lower != middle) : (lower += 1) {
            // a. Let upper be len - lower - 1.
            const upper = len - lower - 1;

            // b. Let upperP be ! ToString(𝔽(upper)).
            const upper_property_key = PropertyKey.from(upper);

            // c. Let lowerP be ! ToString(𝔽(lower)).
            const lower_property_key = PropertyKey.from(lower);

            // d. Let lowerExists be ? HasProperty(O, lowerP).
            const lower_exists = try object.hasProperty(agent, lower_property_key);

            // e. If lowerExists is true, then
            const lower_value = if (lower_exists) blk: {
                // i. Let lowerValue be ? Get(O, lowerP).
                break :blk try object.get(agent, lower_property_key);
            } else undefined;

            // f. Let upperExists be ? HasProperty(O, upperP).
            const upper_exists = try object.hasProperty(agent, upper_property_key);

            // g. If upperExists is true, then
            const upper_value = if (upper_exists) blk: {
                // i. Let upperValue be ? Get(O, upperP).
                break :blk try object.get(agent, upper_property_key);
            } else undefined;

            // h. If lowerExists is true and upperExists is true, then
            if (lower_exists and upper_exists) {
                // i. Perform ? Set(O, lowerP, upperValue, true).
                try object.set(agent, lower_property_key, upper_value, .throw);

                // ii. Perform ? Set(O, upperP, lowerValue, true)
                try object.set(agent, upper_property_key, lower_value, .throw);
            }
            // i. Else if lowerExists is false and upperExists is true, then
            else if (!lower_exists and upper_exists) {
                // i. Perform ? Set(O, lowerP, upperValue, true).
                try object.set(agent, lower_property_key, upper_value, .throw);

                // ii. Perform ? DeletePropertyOrThrow(O, upperP).
                try object.deletePropertyOrThrow(agent, upper_property_key);
            }
            // j. Else if lowerExists is true and upperExists is false, then
            else if (lower_exists and !upper_exists) {
                // i. Perform ? DeletePropertyOrThrow(O, lowerP).
                try object.deletePropertyOrThrow(agent, lower_property_key);

                // ii. Perform ? Set(O, upperP, lowerValue, true).
                try object.set(agent, upper_property_key, lower_value, .throw);
            } else {
                // k. Else,
                // i. Assert: lowerExists and upperExists are both false.
                std.debug.assert(!lower_exists and !upper_exists);

                // ii. NOTE: No action is required.
            }

            // l. Set lower to lower + 1.
        }

        // 6. Return O.
        return Value.from(object);
    }

    /// 23.1.3.27 Array.prototype.shift ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.shift
    fn shift(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If len = 0, then
        if (len == 0) {
            // a. Perform ? Set(O, "length", +0𝔽, true).
            try object.set(agent, PropertyKey.from("length"), Value.from(0), .throw);

            // b. Return undefined.
            return .undefined;
        }

        // 4. Let first be ? Get(O, "0").
        const first = try object.get(agent, PropertyKey.from(0));

        // 5. Let k be 1.
        var k: u53 = 1;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let from be ! ToString(𝔽(k)).
            const from = PropertyKey.from(k);

            // b. Let to be ! ToString(𝔽(k - 1)).
            const to = PropertyKey.from(k - 1);

            // c. Let fromPresent be ? HasProperty(O, from).
            const from_present = try object.hasProperty(agent, from);

            // d. If fromPresent is true, then
            if (from_present) {
                // i. Let fromValue be ? Get(O, from).
                const from_value = try object.get(agent, from);

                // ii. Perform ? Set(O, to, fromValue, true).
                try object.set(agent, to, from_value, .throw);
            } else {
                // e. Else,
                // i. Assert: fromPresent is false.
                std.debug.assert(!from_present);

                // ii. Perform ? DeletePropertyOrThrow(O, to).
                try object.deletePropertyOrThrow(agent, to);
            }

            // f. Set k to k + 1.
        }

        // 7. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(len - 1))).
        try object.deletePropertyOrThrow(agent, PropertyKey.from(len - 1));

        // 8. Perform ? Set(O, "length", 𝔽(len - 1), true).
        try object.set(agent, PropertyKey.from("length"), Value.from(len - 1), .throw);

        // 9. Return first.
        return first;
    }

    /// 23.1.3.28 Array.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-array.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);
        const len_f64: f64 = @floatFromInt(len);

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 4. If relativeStart = -∞, let k be 0.
        const k_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        } else if (relative_start < 0) blk: {
            // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
            break :blk @max(len_f64 + relative_start, 0);
        } else blk: {
            // 6. Else, let k be min(relativeStart, len).
            break :blk @min(relative_start, len_f64);
        };
        var k: u53 = @intFromFloat(k_f64);

        // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //    ? ToIntegerOrInfinity(end).
        const relative_end = if (end.isUndefined())
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 8. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        } else if (relative_end < 0) blk: {
            // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
            break :blk @max(len_f64 + relative_end, 0);
        } else blk: {
            // 10. Else, let final be min(relativeEnd, len).
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
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), kValue).
                try array.createDataPropertyOrThrow(agent, PropertyKey.from(n), k_value);
            }

            // d. Set k to k + 1.
            // e. Set n to n + 1.
        }

        // 15. Perform ? Set(A, "length", 𝔽(n), true).
        try array.set(agent, PropertyKey.from("length"), Value.from(n), .throw);

        // 16. Return A.
        return Value.from(array);
    }

    /// 23.1.3.29 Array.prototype.some ( callback [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.some
    fn some(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{callback});
        }

        // 4. Let k be 0.
        var k: u53 = 0;

        // OPTIMIZATION: Use fast path if applicable
        if (try array_fast_paths.some(
            agent,
            object,
            len,
            callback,
            this_arg,
        )) |result| switch (result) {
            .done => |value| return Value.from(value),
            .continue_slow => |index| k = @intCast(index),
        };

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kPresent be ? HasProperty(O, Pk).
            const k_present = try object.hasProperty(agent, property_key);

            // c. If kPresent is true, then
            if (k_present) {
                // i. Let kValue be ? Get(O, Pk).
                const k_value = try object.get(agent, property_key);

                // ii. Let testResult be ToBoolean(? Call(callback, thisArg, « kValue, 𝔽(k), O »)).
                const test_result = (try callback.callAssumeCallable(
                    agent,
                    this_arg,
                    &.{ k_value, Value.from(k), Value.from(object) },
                )).toBoolean();

                // iii. If testResult is true, return true.
                if (test_result) return Value.from(true);
            }

            // d. Set k to k + 1.
        }

        // 6. Return true.
        return Value.from(false);
    }

    /// 23.1.3.30 Array.prototype.sort ( comparator )
    /// https://tc39.es/ecma262/#sec-array.prototype.sort
    fn sort(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const comparator = arguments.get(0);

        // 1. If comparator is not undefined and IsCallable(comparator) is false, throw a TypeError
        //    exception.
        if (!comparator.isUndefined() and !comparator.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{comparator});
        }

        // 2. Let obj be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 3. Let len be ? LengthOfArrayLike(obj).
        const len = try object.lengthOfArrayLike(agent);

        // 4. Let SortCompare be a new Abstract Closure with parameters (x, y) that captures
        //    comparator and performs the following steps when called:
        const sortCompare = struct {
            fn func(agent_: *Agent, x: Value, y: Value, comparator_: ?*Object) Agent.Error!std.math.Order {
                // a. Return ? CompareArrayElements(x, y, comparator).
                return compareArrayElements(agent_, x, y, comparator_);
            }
        }.func;

        // 5. Let sortedList be ? SortIndexedProperties(obj, len, SortCompare, skip-holes).
        const sorted_list = try sortIndexedProperties(
            agent,
            object,
            len,
            .{
                .impl = sortCompare,
                .comparator = if (!comparator.isUndefined()) comparator.asObject() else null,
            },
            .skip_holes,
        );

        // 6. Let itemCount be the number of elements in sortedList.
        const item_count: u53 = @intCast(sorted_list.len);

        // 7. Let j be 0.
        var j: u53 = 0;

        // 8. Repeat, while j < itemCount,
        while (j < item_count) : (j += 1) {
            // a. Perform ? Set(obj, ! ToString(𝔽(j)), sortedList[j], true).
            try object.set(agent, PropertyKey.from(j), sorted_list[@intCast(j)], .throw);

            // b. Set j to j + 1.
        }

        // 9. NOTE: The call to SortIndexedProperties in step 5 uses skip-holes. The remaining
        //    indices are deleted to preserve the number of holes that were detected and excluded
        //    from the sort.

        // 10. Repeat, while j < len,
        while (j < len) : (j += 1) {
            // a. Perform ? DeletePropertyOrThrow(obj, ! ToString(𝔽(j))).
            try object.deletePropertyOrThrow(agent, PropertyKey.from(j));

            // b. Set j to j + 1.
        }

        // 11. Return obj.
        return Value.from(object);
    }

    /// 23.1.3.31 Array.prototype.splice ( start, deleteCount, ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.splice
    fn splice(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.getOrNull(0);
        const delete_count = arguments.getOrNull(1);
        const items = if (arguments.count() <= 2) &[_]Value{} else arguments.values[2..];

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);
        const len_f64: f64 = @floatFromInt(len);

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = if (start) |s| try s.toIntegerOrInfinity(agent) else 0;

        // 4. If relativeStart = -∞, let actualStart be 0.
        const actual_start_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        } else if (relative_start < 0) blk: {
            // 5. Else if relativeStart < 0, let actualStart be max(len + relativeStart, 0).
            break :blk @max(len_f64 + relative_start, 0);
        } else blk: {
            // 6. Else, let actualStart be min(relativeStart, len).
            break :blk @min(relative_start, len_f64);
        };
        const actual_start: u53 = @intFromFloat(actual_start_f64);

        // 7. Let itemCount be the number of elements in items.
        const item_count: u53 = @intCast(items.len);

        // 8. If start is not present, then
        const actual_delete_count = if (start == null) blk: {
            // a. Let actualDeleteCount be 0.
            break :blk 0;
        } else if (delete_count == null) blk: {
            // 9. Else if deleteCount is not present, then
            // a. Let actualDeleteCount be len - actualStart.
            break :blk len - actual_start;
        } else blk: {
            // 10. Else,
            // a. Let dc be ? ToIntegerOrInfinity(deleteCount).
            const delete_count_f64 = try delete_count.?.toIntegerOrInfinity(agent);

            // b. Let actualDeleteCount be the result of clamping dc between 0 and len - actualStart.
            break :blk @as(u53, @intFromFloat(
                std.math.clamp(delete_count_f64, 0, len_f64 - actual_start_f64),
            ));
        };

        // 11. If len + itemCount - actualDeleteCount > 2**53 - 1, throw a TypeError exception.
        _ = std.math.add(u53, len - actual_delete_count, item_count) catch {
            return agent.throwException(.type_error, "Maximum array length exceeded", .{});
        };

        // 12. Let A be ? ArraySpeciesCreate(O, actualDeleteCount).
        const array = try arraySpeciesCreate(agent, object, actual_delete_count);

        // 13. Let k be 0.
        var k: u53 = 0;

        // 14. Repeat, while k < actualDeleteCount,
        while (k < actual_delete_count) : (k += 1) {
            // a. Let from be ! ToString(𝔽(actualStart + k)).
            const from = PropertyKey.from(actual_start + k);

            // b. If ? HasProperty(O, from) is true, then
            if (try object.hasProperty(agent, from)) {
                // i. Let fromValue be ? Get(O, from).
                const from_value = try object.get(agent, from);

                // ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(k)), fromValue).
                try array.createDataPropertyOrThrow(agent, PropertyKey.from(k), from_value);
            }

            // c. Set k to k + 1.
        }

        // 15. Perform ? Set(A, "length", 𝔽(actualDeleteCount), true).
        try array.set(agent, PropertyKey.from("length"), Value.from(actual_delete_count), .throw);

        // 16. If itemCount < actualDeleteCount, then
        if (item_count < actual_delete_count) {
            // a. Set k to actualStart.
            k = actual_start;

            // b. Repeat, while k < (len - actualDeleteCount),
            while (k < (len - actual_delete_count)) : (k += 1) {
                // i. Let from be ! ToString(𝔽(k + actualDeleteCount)).
                const from = PropertyKey.from(k + actual_delete_count);

                // ii. Let to be ! ToString(𝔽(k + itemCount)).
                const to = PropertyKey.from(k + item_count);

                // iii. If ? HasProperty(O, from) is true, then
                if (try object.hasProperty(agent, from)) {
                    // 1. Let fromValue be ? Get(O, from).
                    const from_value = try object.get(agent, from);

                    // 2. Perform ? Set(O, to, fromValue, true).
                    try object.set(agent, to, from_value, .throw);
                } else {
                    // iv. Else,
                    // 1. Perform ? DeletePropertyOrThrow(O, to).
                    try object.deletePropertyOrThrow(agent, to);
                }

                // v. Set k to k + 1.
            }

            // c. Set k to len.
            k = len;

            // d. Repeat, while k > (len - actualDeleteCount + itemCount),
            while (k > (len - actual_delete_count + item_count)) : (k -= 1) {
                // i. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(k - 1))).
                try object.deletePropertyOrThrow(agent, PropertyKey.from(k - 1));

                // ii. Set k to k - 1.
            }
        }
        // 17. Else if itemCount > actualDeleteCount, then
        else if (item_count > actual_delete_count) {
            // a. Set k to (len - actualDeleteCount).
            k = len - actual_delete_count;

            // b. Repeat, while k > actualStart,
            while (k > actual_start) : (k -= 1) {
                // i. Let from be ! ToString(𝔽(k + actualDeleteCount - 1)).
                const from = PropertyKey.from(k + actual_delete_count - 1);

                // ii. Let to be ! ToString(𝔽(k + itemCount - 1)).
                const to = PropertyKey.from(k + item_count - 1);

                // iii. If ? HasProperty(O, from) is true, then
                if (try object.hasProperty(agent, from)) {
                    // 1. Let fromValue be ? Get(O, from).
                    const from_value = try object.get(agent, from);

                    // 2. Perform ? Set(O, to, fromValue, true).
                    try object.set(agent, to, from_value, .throw);
                } else {
                    // iv. Else,
                    // 1. Perform ? DeletePropertyOrThrow(O, to).
                    try object.deletePropertyOrThrow(agent, to);
                }

                // v. Set k to k - 1.
            }
        }

        // 18. Set k to actualStart.
        k = actual_start;

        // 19. For each element E of items, do
        for (items) |element| {
            // a. Perform ? Set(O, ! ToString(𝔽(k)), E, true).
            try object.set(agent, PropertyKey.from(k), element, .throw);

            // b. Set k to k + 1.
            k += 1;
        }

        // 20. Perform ? Set(O, "length", 𝔽(len - actualDeleteCount + itemCount), true).
        try object.set(
            agent,
            PropertyKey.from("length"),
            Value.from(len - actual_delete_count + item_count),
            .throw,
        );

        // 21. Return A.
        return Value.from(array);
    }

    /// 23.1.3.32 Array.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-array.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let array be ? ToObject(this value).
        const array = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(array).
        const len = try array.lengthOfArrayLike(agent);

        // OPTIMIZATION: If the array is empty the result will be an empty string
        if (len == 0) return Value.from(String.empty);

        // 3. Let separator be the implementation-defined list-separator String value appropriate
        //    for the host environment's current locale (such as ", ").
        const separator = String.fromLiteral(", ");

        // 4. Let R be the empty String.
        // NOTE: This allocates the maximum needed capacity upfront
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var result = try String.Builder.initCapacity(agent.gc_allocator, @intCast((len * 2) - 1));
        defer result.deinit(agent.gc_allocator);

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, set R to the string-concatenation of R and separator.
            if (k > 0) result.appendStringAssumeCapacity(separator);

            // b. Let element be ? Get(array, ! ToString(𝔽(k))).
            const element = try array.get(agent, PropertyKey.from(k));

            // c. If element is neither undefined nor null, then
            if (!element.isUndefined() and !element.isNull()) {
                // i. Let S be ? ToString(? Invoke(element, "toLocaleString")).
                const string = try (try element.invoke(
                    agent,
                    PropertyKey.from("toLocaleString"),
                    &.{},
                )).toString(agent);

                // ii. Set R to the string-concatenation of R and S.
                result.appendStringAssumeCapacity(string);
            }

            // d. Set k to k + 1.
        }

        // 7. Return R.
        return Value.from(try result.build(agent));
    }

    /// 23.1.3.33 Array.prototype.toReversed ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.toreversed
    fn toReversed(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. Let A be ? ArrayCreate(len).
        const array = try arrayCreate(agent, len, null);

        // 4. Let k be 0.
        var k: u53 = 0;

        // 5. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let from be ! ToString(𝔽(len - k - 1)).
            const from = PropertyKey.from(len - k - 1);

            // b. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // c. Let fromValue be ? Get(O, from).
            const from_value = try object.get(agent, from);

            // d. Perform ! CreateDataPropertyOrThrow(A, Pk, fromValue).
            try array.createDataPropertyDirect(agent, property_key, from_value);

            // e. Set k to k + 1.
        }

        // 6. Return A.
        return Value.from(array);
    }

    /// 23.1.3.34 Array.prototype.toSorted ( comparator )
    /// https://tc39.es/ecma262/#sec-array.prototype.tosorted
    fn toSorted(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const comparator = arguments.get(0);

        // 1. If comparator is not undefined and IsCallable(comparator) is false, throw a TypeError
        //    exception.
        if (!comparator.isUndefined() and !comparator.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{comparator});
        }

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 3. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 4. Let A be ? ArrayCreate(len).
        const array = try arrayCreate(agent, len, null);

        // 5. Let SortCompare be a new Abstract Closure with parameters (x, y) that captures
        //    comparator and performs the following steps when called:
        const sortCompare = struct {
            fn func(agent_: *Agent, x: Value, y: Value, comparator_: ?*Object) Agent.Error!std.math.Order {
                // a. Return ? CompareArrayElements(x, y, comparator).
                return compareArrayElements(agent_, x, y, comparator_);
            }
        }.func;

        // 6. Let sortedList be ? SortIndexedProperties(O, len, SortCompare, read-through-holes).
        const sorted_list = try sortIndexedProperties(
            agent,
            object,
            len,
            .{
                .impl = sortCompare,
                .comparator = if (!comparator.isUndefined()) comparator.asObject() else null,
            },
            .read_through_holes,
        );

        // 7. Let j be 0.
        var j: u53 = 0;

        // 8. Repeat, while j < len,
        while (j < len) : (j += 1) {
            // a. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(j)), sortedList[j]).
            try array.createDataPropertyDirect(agent, PropertyKey.from(j), sorted_list[@intCast(j)]);

            // b. Set j to j + 1.
        }

        // 9. Return A.
        return Value.from(array);
    }

    /// 23.1.3.35 Array.prototype.toSpliced ( start, skipCount, ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.tospliced
    fn toSpliced(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.getOrNull(0);
        const skip_count = arguments.getOrNull(1);
        const items = if (arguments.count() <= 2) &[_]Value{} else arguments.values[2..];

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);
        const len_f64: f64 = @floatFromInt(len);

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = if (start) |s| try s.toIntegerOrInfinity(agent) else 0;

        // 4. If relativeStart = -∞, let actualStart be 0.
        const actual_start_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        } else if (relative_start < 0) blk: {
            // 5. Else if relativeStart < 0, let actualStart be max(len + relativeStart, 0).
            break :blk @max(len_f64 + relative_start, 0);
        } else blk: {
            // 6. Else, let actualStart be min(relativeStart, len).
            break :blk @min(relative_start, len_f64);
        };
        const actual_start: u53 = @intFromFloat(actual_start_f64);

        // 7. Let insertCount be the number of elements in items.
        const insert_count: u53 = @intCast(items.len);

        // 8. If start is not present, then
        const actual_skip_count = if (start == null) blk: {
            // a. Let actualSkipCount be 0.
            break :blk 0;
        } else if (skip_count == null) blk: {
            // 9. Else if skipCount is not present, then
            // a. Let actualSkipCount be len - actualStart.
            break :blk len - actual_start;
        } else blk: {
            // 10. Else,
            // a. Let sc be ? ToIntegerOrInfinity(skipCount).
            const skip_count_f64 = try skip_count.?.toIntegerOrInfinity(agent);

            // b. Let actualSkipCount be the result of clamping sc between 0 and len - actualStart.
            break :blk @as(u53, @intFromFloat(
                std.math.clamp(skip_count_f64, 0, len_f64 - actual_start_f64),
            ));
        };

        // 11. Let newLen be len + insertCount - actualSkipCount.
        // 12. If newLen > 2**53 - 1, throw a TypeError exception.
        const new_len = std.math.add(u53, len - actual_skip_count, insert_count) catch {
            return agent.throwException(.type_error, "Maximum array length exceeded", .{});
        };

        // 13. Let A be ? ArrayCreate(newLen).
        const array = try arrayCreate(agent, new_len, null);

        // 14. Let i be 0.
        var i: u53 = 0;

        // 15. Let r be actualStart + actualSkipCount.
        var r = actual_start + actual_skip_count;

        // 16. Repeat, while i < actualStart,
        while (i < actual_start) : (i += 1) {
            // a. Let Pi be ! ToString(𝔽(i)).
            const property_key = PropertyKey.from(i);

            // b. Let iValue be ? Get(O, Pi).
            const i_value = try object.get(agent, property_key);

            // c. Perform ! CreateDataPropertyOrThrow(A, Pi, iValue).
            try array.createDataPropertyDirect(agent, property_key, i_value);

            // d. Set i to i + 1.
        }

        // 17. For each element E of items, do
        for (items) |element| {
            // a. Let Pi be ! ToString(𝔽(i)).
            const property_key = PropertyKey.from(i);

            // b. Perform ! CreateDataPropertyOrThrow(A, Pi, E).
            try array.createDataPropertyDirect(agent, property_key, element);

            // c. Set i to i + 1.
            i += 1;
        }

        // 18. Repeat, while i < newLen,
        while (i < new_len) : ({
            i += 1;
            r += 1;
        }) {
            // a. Let Pi be ! ToString(𝔽(i)).
            const property_key = PropertyKey.from(i);

            // b. Let from be ! ToString(𝔽(r)).
            const from = PropertyKey.from(r);

            // c. Let fromValue be ? Get(O, from).
            const from_value = try object.get(agent, from);

            // d. Perform ! CreateDataPropertyOrThrow(A, Pi, fromValue).
            try array.createDataPropertyDirect(agent, property_key, from_value);

            // e. Set i to i + 1.
            // f. Set r to r + 1.
        }

        // 19. Return A.
        return Value.from(array);
    }

    /// 23.1.3.36 Array.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let array be ? ToObject(this value).
        const array = try this_value.toObject(agent);

        // 2. Let func be ? Get(array, "join").
        var func = try array.get(agent, PropertyKey.from("join"));

        // 3. If IsCallable(func) is false, set func to the intrinsic function %Object.prototype.toString%.
        if (!func.isCallable()) func = Value.from(try realm.intrinsics.@"%Object.prototype.toString%"());

        // 4. Return ? Call(func, array).
        return func.callAssumeCallable(agent, Value.from(array), &.{});
    }

    /// 23.1.3.37 Array.prototype.unshift ( ...items )
    /// https://tc39.es/ecma262/#sec-array.prototype.unshift
    fn unshift(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

        // 3. Let argCount be the number of elements in items.
        const arg_count = arguments.count();

        // 4. If argCount > 0, then
        if (arg_count > 0) {
            // a. If len + argCount > 2**53 - 1, throw a TypeError exception.
            _ = std.math.add(u53, len, @intCast(arg_count)) catch {
                return agent.throwException(.type_error, "Maximum array length exceeded", .{});
            };

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
                const from_present = try object.hasProperty(agent, from);

                // iv. If fromPresent is true, then
                if (from_present) {
                    // 1. Let fromValue be ? Get(O, from).
                    const from_value = try object.get(agent, from);

                    // 2. Perform ? Set(O, to, fromValue, true).
                    try object.set(agent, to, from_value, .throw);
                } else {
                    // v. Else,
                    // 1. Assert: fromPresent is false.
                    std.debug.assert(!from_present);

                    // 2. Perform ? DeletePropertyOrThrow(O, to).
                    try object.deletePropertyOrThrow(agent, to);
                }

                // vi. Set k to k - 1.
            }

            // d. Let j be +0𝔽.
            // e. For each element E of items, do
            for (arguments.values, 0..) |element, j| {
                // i. Perform ? Set(O, ! ToString(j), E, true).
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(j)));
                try object.set(agent, property_key, element, .throw);

                // ii. Set j to j + 1𝔽.
            }
        }

        // 5. Perform ? Set(O, "length", 𝔽(len + argCount), true).
        try object.set(
            agent,
            PropertyKey.from("length"),
            Value.from(len + @as(u53, @intCast(arg_count))),
            .throw,
        );

        // 6. Return 𝔽(len + argCount).
        return Value.from(len + @as(u53, @intCast(arg_count)));
    }

    /// 23.1.3.38 Array.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-array.prototype.values
    fn values(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return CreateArrayIterator(O, value).
        return Value.from(try createArrayIterator(agent, object, .value));
    }

    /// 23.1.3.39 Array.prototype.with ( index, value )
    /// https://tc39.es/ecma262/#sec-array.prototype.with
    fn with(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const index = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let len be ? LengthOfArrayLike(O).
        const len = try object.lengthOfArrayLike(agent);

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
            return agent.throwException(.range_error, "Index is out of array bounds", .{});
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

            // b. If k = actualIndex, let fromValue be value.
            // c. Else, let fromValue be ? Get(O, Pk).
            const from_value = if (k == actual_index)
                value
            else
                try object.get(agent, property_key);

            // d. Perform ! CreateDataPropertyOrThrow(A, Pk, fromValue).
            try array.createDataPropertyDirect(agent, property_key, from_value);

            // e. Set k to k + 1.
        }

        // 10. Return A.
        return Value.from(array);
    }
};

pub const FindViaPredicateDirection = enum { ascending, descending };
pub const FindViaPredicateResult = struct { index: Value, value: Value };

/// 23.1.3.12.1 FindViaPredicate ( O, len, direction, predicate, thisArg )
/// https://tc39.es/ecma262/#sec-findviapredicate
pub fn findViaPredicate(
    agent: *Agent,
    object: *Object,
    len: u53,
    comptime direction: FindViaPredicateDirection,
    predicate: Value,
    this_arg: Value,
) Agent.Error!FindViaPredicateResult {
    // 1. If IsCallable(predicate) is false, throw a TypeError exception.
    if (!predicate.isCallable()) {
        return agent.throwException(.type_error, "{f} is not callable", .{predicate});
    }

    // 2. If direction is ascending, then
    //     a. Let indices be a List of the integers in the interval from 0 (inclusive) to len
    //        (exclusive), in ascending order.
    // 3. Else,
    //     a. Let indices be a List of the integers in the interval from 0 (inclusive) to len
    //        (exclusive), in descending order.
    // 4. For each integer k of indices, do
    var k: ?u53 = if (direction == .ascending) 0 else std.math.sub(u53, len, 1) catch null;

    // OPTIMIZATION: Use fast path if applicable
    if (try array_fast_paths.findViaPredicate(
        agent,
        object,
        len,
        direction,
        predicate,
        this_arg,
    )) |result| switch (result) {
        .done => |value| return value,
        .continue_slow => |index| k = if (index) |i| @intCast(i) else null,
    };

    // zig fmt: off
    while (
        if (direction == .ascending) k.? < len else k != null
    ) : (
        k = if (direction == .ascending) k.? + 1 else std.math.sub(u53, k.?, 1) catch null
    ) {
        // zig fmt: on
        // a. Let Pk be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k.?);

        // b. NOTE: If O is a TypedArray, the following invocation of Get will return a normal completion.
        // c. Let kValue be ? Get(O, Pk).
        const k_value = try object.get(agent, property_key);

        // d. Let testResult be ? Call(predicate, thisArg, « kValue, 𝔽(k), O »).
        const test_result = try predicate.callAssumeCallable(
            agent,
            this_arg,
            &.{ k_value, Value.from(k.?), Value.from(object) },
        );

        // e. If ToBoolean(testResult) is true, return the Record { [[Index]]: 𝔽(k), [[Value]]: kValue }.
        if (test_result.toBoolean()) return .{ .index = Value.from(k.?), .value = k_value };
    }

    // 5. Return the Record { [[Index]]: -1𝔽, [[Value]]: undefined }.
    return .{ .index = Value.from(-1), .value = .undefined };
}

const SortCompare = struct {
    impl: *const fn (
        agent: *Agent,
        x: Value,
        y: Value,
        comparator: ?*Object,
    ) Agent.Error!std.math.Order,
    comparator: ?*Object,
};

/// Custom insertion sort implementation, `std.mem` doesn't have fallible sorting functions
/// https://github.com/Koura/algorithms/blob/main/sorting/insertion_sort.zig
fn insertionSort(agent: *Agent, items: []Value, sort_compare: SortCompare) Agent.Error!void {
    const sortCompare = sort_compare.impl;
    const comparator = sort_compare.comparator;
    var i: usize = 1;
    while (i < items.len) : (i += 1) {
        const x = items[i];
        var j = i;
        while (j > 0) : (j -= 1) {
            const y = items[j - 1];
            if (try sortCompare(agent, x, y, comparator) != .lt) break;
            items[j] = y;
        }
        items[j] = x;
    }
}

/// 23.1.3.30.1 SortIndexedProperties ( obj, len, SortCompare, holes )
/// https://tc39.es/ecma262/#sec-sortindexedproperties
pub fn sortIndexedProperties(
    agent: *Agent,
    object: *Object,
    len: u53,
    sort_compare: SortCompare,
    comptime holes: enum { skip_holes, read_through_holes },
) Agent.Error![]const Value {
    // 1. Let items be a new empty List.
    var items: std.ArrayList(Value) = .empty;

    // 2. Let k be 0.
    var k: u53 = 0;

    // 3. Repeat, while k < len,
    while (k < len) : (k += 1) {
        // a. Let Pk be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k);

        const k_read = switch (holes) {
            // b. If holes is skip-holes, then
            .skip_holes => blk: {
                // i. Let kRead be ? HasProperty(obj, Pk).
                break :blk try object.hasProperty(agent, property_key);
            },
            // c. Else,
            .read_through_holes => blk: {
                // i. Assert: holes is read-through-holes.
                // ii. Let kRead be true.
                break :blk true;
            },
        };

        // d. If kRead is true, then
        if (k_read) {
            // i. Let kValue be ? Get(obj, Pk).
            const k_value = try object.get(agent, property_key);

            // ii. Append kValue to items.
            try items.append(agent.gc_allocator, k_value);
        }

        // e. Set k to k + 1.
    }

    // 4. Sort items using an implementation-defined sequence of calls to SortCompare. If any such
    //    call returns an abrupt completion, stop before performing any further calls to
    //    SortCompare and return that Completion Record.
    try insertionSort(agent, items.items, sort_compare);

    // 5. Return items.
    return items.toOwnedSlice(agent.gc_allocator);
}

/// 23.1.3.30.2 CompareArrayElements ( x, y, comparator )
/// https://tc39.es/ecma262/#sec-comparearrayelements
pub fn compareArrayElements(
    agent: *Agent,
    x: Value,
    y: Value,
    maybe_comparator: ?*Object,
) Agent.Error!std.math.Order {
    // 1. If x and y are both undefined, return +0𝔽.
    if (x.isUndefined() and y.isUndefined()) return .eq;

    // 2. If x is undefined, return 1𝔽.
    if (x.isUndefined()) return .gt;

    // 3. If y is undefined, return -1𝔽.
    if (y.isUndefined()) return .lt;

    // 4. If comparator is not undefined, then
    if (maybe_comparator) |comparator| {
        // a. Let v be ? ToNumber(? Call(comparator, undefined, « x, y »)).
        const value = try (try Value.from(comparator).callAssumeCallable(
            agent,
            .undefined,
            &.{ x, y },
        )).toNumber(agent);

        // b. If v is NaN, return +0𝔽.
        if (value.isNan()) return .eq;

        // c. Return v.
        return if (value.isZero()) .eq else if (value.asFloat() < 0) .lt else .gt;
    }

    // 5. Let xString be ? ToString(x).
    const x_string = try x.toString(agent);

    // 6. Let yString be ? ToString(y).
    const y_string = try y.toString(agent);

    // 7. Let xSmaller be ! IsLessThan(xString, yString, true).
    const x_smaller = isLessThan(
        agent,
        Value.from(x_string),
        Value.from(y_string),
        .left_first,
    ) catch unreachable;

    // 8. If xSmaller is true, return -1𝔽.
    if (x_smaller == true) return .lt;

    // 9. Let ySmaller be ! IsLessThan(yString, xString, true).
    const y_smaller = isLessThan(
        agent,
        Value.from(y_string),
        Value.from(x_string),
        .left_first,
    ) catch unreachable;

    // 10. If ySmaller is true, return 1𝔽.
    if (y_smaller == true) return .gt;

    // 11. Return +0𝔽.
    return .eq;
}

/// 23.1.4 Properties of Array Instances
/// https://tc39.es/ecma262/#sec-properties-of-array-instances
pub const Array = MakeObject(.{
    .tag = .array,
});
