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
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = ordinary.ordinaryDefineOwnProperty;
const ordinaryGetOwnProperty = ordinary.ordinaryGetOwnProperty;

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
    if (property_key == .string and std.mem.eql(u8, property_key.string, "length")) {
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
pub fn arrayCreate(agent: *Agent, length: usize, maybe_prototype: ?Object) !Object {
    const realm = agent.currentRealm();

    // 1. If length > 2^32 - 1, throw a RangeError exception.
    if (length >= @as(usize, std.math.maxInt(u32))) return agent.throwException(
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
                break :blk try constructor.object.construct(.{
                    .arguments_list = ArgumentsList.from(.{len_number}),
                });
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
        const object = try Array.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinProperty(object, "length", PropertyDescriptor{
            .value = Value.from(0),
            .writable = true,
            .enumerable = false,
            .configurable = false,
        });

        return object;
    }
};

/// 23.1.4 Properties of Array Instances
/// https://tc39.es/ecma262/#sec-properties-of-array-instances
pub const Array = Object.Factory(.{
    .tag = .array,
});
