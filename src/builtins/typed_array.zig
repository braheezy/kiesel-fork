//! 23.2 TypedArray Objects
//! https://tc39.es/ecma262/#sec-typedarray-objects

const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Order = @import("../builtins/array_buffer.zig").Order;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const allocateArrayBuffer = builtins.allocateArrayBuffer;
const arrayBufferByteLength = builtins.arrayBufferByteLength;
const cloneArrayBuffer = builtins.cloneArrayBuffer;
const createArrayIterator = builtins.createArrayIterator;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIteratorFromMethod = types.getIteratorFromMethod;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const getValueFromBuffer = builtins.getValueFromBuffer;
const isDetachedBuffer = builtins.isDetachedBuffer;
const isFixedLengthArrayBuffer = builtins.isFixedLengthArrayBuffer;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryDelete = builtins.ordinaryDelete;
const ordinaryGet = builtins.ordinaryGet;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const ordinaryHasProperty = builtins.ordinaryHasProperty;
const ordinarySet = builtins.ordinarySet;
const setValueInBuffer = builtins.setValueInBuffer;

/// Table 71: The TypedArray Constructors
/// https://tc39.es/ecma262/#table-the-typedarray-constructors
pub const Int8ArrayConstructor = MakeTypedArrayConstructor("Int8Array");
pub const Int8ArrayPrototype = MakeTypedArrayPrototype("Int8Array");

pub const Uint8ArrayConstructor = MakeTypedArrayConstructor("Uint8Array");
pub const Uint8ArrayPrototype = MakeTypedArrayPrototype("Uint8Array");

pub const Uint8ClampedArrayConstructor = MakeTypedArrayConstructor("Uint8ClampedArray");
pub const Uint8ClampedArrayPrototype = MakeTypedArrayPrototype("Uint8ClampedArray");

pub const Int16ArrayConstructor = MakeTypedArrayConstructor("Int16Array");
pub const Int16ArrayPrototype = MakeTypedArrayPrototype("Int16Array");

pub const Uint16ArrayConstructor = MakeTypedArrayConstructor("Uint16Array");
pub const Uint16ArrayPrototype = MakeTypedArrayPrototype("Uint16Array");

pub const Int32ArrayConstructor = MakeTypedArrayConstructor("Int32Array");
pub const Int32ArrayPrototype = MakeTypedArrayPrototype("Int32Array");

pub const Uint32ArrayConstructor = MakeTypedArrayConstructor("Uint32Array");
pub const Uint32ArrayPrototype = MakeTypedArrayPrototype("Uint32Array");

pub const BigInt64ArrayConstructor = MakeTypedArrayConstructor("BigInt64Array");
pub const BigInt64ArrayPrototype = MakeTypedArrayPrototype("BigInt64Array");

pub const BigUint64ArrayConstructor = MakeTypedArrayConstructor("BigUint64Array");
pub const BigUint64ArrayPrototype = MakeTypedArrayPrototype("BigUint64Array");

pub const Float32ArrayConstructor = MakeTypedArrayConstructor("Float32Array");
pub const Float32ArrayPrototype = MakeTypedArrayPrototype("Float32Array");

pub const Float64ArrayConstructor = MakeTypedArrayConstructor("Float64Array");
pub const Float64ArrayPrototype = MakeTypedArrayPrototype("Float64Array");

pub const typed_array_element_types = .{
    .{ "Int8Array", i8 },
    .{ "Uint8Array", u8 },
    .{ "Uint8ClampedArray", u8 },
    .{ "Int16Array", i16 },
    .{ "Uint16Array", u16 },
    .{ "Int32Array", i32 },
    .{ "Uint32Array", u32 },
    .{ "BigInt64Array", i64 },
    .{ "BigUint64Array", u64 },
    .{ "Float32Array", f32 },
    .{ "Float64Array", f64 },
};

/// 10.4.5.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-typedarray-getownproperty
fn getOwnProperty(object: Object, property_key: PropertyKey) Agent.Error!?PropertyDescriptor {
    const agent = object.agent();

    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, then
    if (property_key == .integer_index) {
        // i. Let value be TypedArrayGetElement(O, numericIndex).
        const value = typedArrayGetElement(
            agent,
            object.as(TypedArray),
            property_key.integer_index,
        );

        // ii. If value is undefined, return undefined.
        if (value == .undefined) return null;

        // iii. Return the PropertyDescriptor {
        //        [[Value]]: value, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true
        //      }.
        return .{ .value = value, .writable = true, .enumerable = true, .configurable = true };
    }

    // 2. Return OrdinaryGetOwnProperty(O, P).
    return ordinaryGetOwnProperty(object, property_key);
}

/// 10.4.5.2 [[HasProperty]] ( P )
/// https://tc39.es/ecma262/#sec-typedarray-hasproperty
fn hasProperty(object: Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, return IsValidIntegerIndex(O, numericIndex).
    if (property_key == .integer_index) {
        const numeric_index: f64 = @floatFromInt(property_key.integer_index);

        return isValidIntegerIndex(object.as(TypedArray), numeric_index);
    }

    // 2. Return ? OrdinaryHasProperty(O, P).
    return ordinaryHasProperty(object, property_key);
}

/// 10.4.5.3 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-typedarray-defineownproperty
fn defineOwnProperty(
    object: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    const agent = object.agent();

    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, then
    if (property_key == .integer_index) {
        const numeric_index: f64 = @floatFromInt(property_key.integer_index);

        // i. If IsValidIntegerIndex(O, numericIndex) is false, return false.
        if (!isValidIntegerIndex(object.as(TypedArray), numeric_index)) return false;

        // ii. If Desc has a [[Configurable]] field and Desc.[[Configurable]] is false, return false.
        if (property_descriptor.configurable == false) return false;

        // iii. If Desc has an [[Enumerable]] field and Desc.[[Enumerable]] is false, return false.
        if (property_descriptor.enumerable == false) return false;

        // iv. If IsAccessorDescriptor(Desc) is true, return false.
        if (property_descriptor.isAccessorDescriptor()) return false;

        // v. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, return false.
        if (property_descriptor.writable == false) return false;

        // vi. If Desc has a [[Value]] field, perform ? TypedArraySetElement(O, numericIndex,
        //     Desc.[[Value]]).
        if (property_descriptor.value) |value| {
            try typedArraySetElement(agent, object.as(TypedArray), numeric_index, value);
        }

        // vii. Return true.
        return true;
    }

    // 2. Return ! OrdinaryDefineOwnProperty(O, P, Desc).
    return ordinaryDefineOwnProperty(
        object,
        property_key,
        property_descriptor,
    ) catch |err| try noexcept(err);
}

/// 10.4.5.4 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-typedarray-get
fn get(object: Object, property_key: PropertyKey, receiver: Value) Agent.Error!Value {
    const agent = object.agent();

    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, then
    if (property_key == .integer_index) {
        // i. Return TypedArrayGetElement(O, numericIndex).
        return typedArrayGetElement(agent, object.as(TypedArray), property_key.integer_index);
    }

    // 2. Return ? OrdinaryGet(O, P, Receiver).
    return ordinaryGet(object, property_key, receiver);
}

/// 10.4.5.5 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-typedarray-set
fn set(object: Object, property_key: PropertyKey, value: Value, receiver: Value) Agent.Error!bool {
    const agent = object.agent();

    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, then
    if (property_key == .integer_index) {
        const numeric_index: f64 = @floatFromInt(property_key.integer_index);

        // i. If SameValue(O, Receiver) is true, then
        if (receiver == .object and object.sameValue(receiver.object)) {
            // 1. Perform ? TypedArraySetElement(O, numericIndex, V).
            try typedArraySetElement(agent, object.as(TypedArray), numeric_index, value);

            // 2. Return true.
            return true;
        }

        // ii. If IsValidIntegerIndex(O, numericIndex) is false, return true.
        if (!isValidIntegerIndex(object.as(TypedArray), numeric_index)) return true;
    }

    // 2. Return ? OrdinarySet(O, P, V, Receiver).
    return ordinarySet(object, property_key, value, receiver);
}

/// 10.4.5.6 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-typedarray-delete
fn delete(object: Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, then
    if (property_key == .integer_index) {
        const numeric_index: f64 = @floatFromInt(property_key.integer_index);

        // i. If IsValidIntegerIndex(O, numericIndex) is false, return true; else return false.
        return !isValidIntegerIndex(object.as(TypedArray), numeric_index);
    }

    // 2. Return ! OrdinaryDelete(O, P).
    return ordinaryDelete(object, property_key) catch |err| try noexcept(err);
}

/// 10.4.5.7 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-typedarray-ownpropertykeys
fn ownPropertyKeys(object: Object) Allocator.Error!std.ArrayList(PropertyKey) {
    const agent = object.agent();
    const property_storage_hash_map = object.propertyStorage().hash_map;

    // 1. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
    const ta = makeTypedArrayWithBufferWitnessRecord(object.as(TypedArray), .seq_cst);

    // 2. Let keys be a new empty List.
    var keys = try std.ArrayList(PropertyKey).initCapacity(
        agent.gc_allocator,
        property_storage_hash_map.count() + if (!isTypedArrayOutOfBounds(ta))
            @as(usize, @intCast(typedArrayLength(ta)))
        else
            0,
    );

    // 3. If IsTypedArrayOutOfBounds(taRecord) is false, then
    if (!isTypedArrayOutOfBounds(ta)) {
        // a. Let length be TypedArrayLength(taRecord).
        const length = typedArrayLength(ta);

        // b. For each integer i such that 0 ≤ i < length, in ascending order, do
        var i: u53 = 0;
        while (i < length) : (i += i) {
            // i. Append ! ToString(𝔽(i)) to keys.
            keys.appendAssumeCapacity(PropertyKey.from(i));
        }
    }

    // 4. For each own property key P of O such that P is a String and P is not an integer index,
    //    in ascending chronological order of property creation, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key == .string) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 5. For each own property key P of O such that P is a Symbol, in ascending chronological
    //    order of property creation, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key == .symbol) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 6. Return keys.
    return keys;
}

/// 10.4.5.8 TypedArray With Buffer Witness Records
/// https://tc39.es/ecma262/#sec-typedarray-with-buffer-witness-records
const TypedArrayWithBufferWitness = struct {
    pub const CachedBufferByteLength = union(enum) {
        detached,
        value: u53,
    };

    /// [[Object]]
    object: *const TypedArray,

    // [[CachedBufferByteLength]]
    cached_buffer_byte_length: CachedBufferByteLength,
};

/// 10.4.5.9 MakeTypedArrayWithBufferWitnessRecord ( obj, order )
/// https://tc39.es/ecma262/#sec-maketypedarraywithbufferwitnessrecord
pub fn makeTypedArrayWithBufferWitnessRecord(
    object: *const TypedArray,
    order: Order,
) TypedArrayWithBufferWitness {
    // 1. Let buffer be obj.[[ViewedArrayBuffer]].
    const buffer = object.fields.viewed_array_buffer;

    // 2. If IsDetachedBuffer(buffer) is true, then
    const byte_length: TypedArrayWithBufferWitness.CachedBufferByteLength = if (isDetachedBuffer(buffer)) blk: {
        // a. Let byteLength be detached.
        break :blk .detached;
    }
    // 3. Else,
    else blk: {
        // a. Let byteLength be ArrayBufferByteLength(buffer, order).
        break :blk .{ .value = arrayBufferByteLength(buffer, order) };
    };

    // 4. Return the TypedArray With Buffer Witness Record {
    //      [[Object]]: obj, [[CachedBufferByteLength]]: byteLength
    //    }.
    return .{ .object = object, .cached_buffer_byte_length = byte_length };
}

/// 10.4.5.11 TypedArrayByteLength ( taRecord )
/// https://tc39.es/ecma262/#sec-typedarraybytelength
pub fn typedArrayByteLength(ta: TypedArrayWithBufferWitness) u53 {

    // 1. If IsTypedArrayOutOfBounds(taRecord) is true, return 0.
    if (isTypedArrayOutOfBounds(ta)) return 0;

    // 2. Let length be TypedArrayLength(taRecord).
    const length = typedArrayLength(ta);

    // 3. If length = 0, return 0.
    if (length == 0) return 0;

    // 4. Let O be taRecord.[[Object]].
    const typed_array = ta.object;

    // 5. If O.[[ByteLength]] is not auto, return O.[[ByteLength]].
    if (typed_array.fields.byte_length != .auto) return typed_array.fields.byte_length.value;

    // 6. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(typed_array);

    // 7. Return length × elementSize.
    return length * element_size;
}

/// 10.4.5.12 TypedArrayLength ( taRecord )
/// https://tc39.es/ecma262/#sec-typedarraylength
pub fn typedArrayLength(ta: TypedArrayWithBufferWitness) u53 {
    // 1. Assert: IsTypedArrayOutOfBounds(taRecord) is false.
    std.debug.assert(!isTypedArrayOutOfBounds(ta));

    // 2. Let O be taRecord.[[Object]].
    const typed_array = ta.object;

    // 3. If O.[[ArrayLength]] is not auto, return O.[[ArrayLength]].
    if (typed_array.fields.array_length != .auto) return typed_array.fields.array_length.value;

    // 4. Assert: IsFixedLengthArrayBuffer(O.[[ViewedArrayBuffer]]) is false.
    std.debug.assert(!isFixedLengthArrayBuffer(typed_array.fields.viewed_array_buffer));

    // 5. Let byteOffset be O.[[ByteOffset]].
    const byte_offset = typed_array.fields.byte_offset;

    // 6. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(typed_array);

    // 7. Let byteLength be taRecord.[[CachedBufferByteLength]].
    const byte_length = ta.cached_buffer_byte_length;

    // 8. Assert: byteLength is not detached.
    std.debug.assert(byte_length != .detached);

    // 9. Return floor((byteLength - byteOffset) / elementSize).
    return @divFloor(byte_length.value - byte_offset, element_size);
}

/// 10.4.5.13 IsTypedArrayOutOfBounds ( taRecord )
/// https://tc39.es/ecma262/#sec-istypedarrayoutofbounds
pub fn isTypedArrayOutOfBounds(ta: TypedArrayWithBufferWitness) bool {
    // 1. Let O be taRecord.[[Object]].
    const typed_array = ta.object;

    // 2. Let bufferByteLength be taRecord.[[CachedBufferByteLength]].
    const buffer_byte_length = ta.cached_buffer_byte_length;

    // 3. Assert: IsDetachedBuffer(O.[[ViewedArrayBuffer]]) is true if and only if bufferByteLength
    //    is detached.
    std.debug.assert(
        isDetachedBuffer(typed_array.fields.viewed_array_buffer) == (buffer_byte_length == .detached),
    );

    // 4. If bufferByteLength is detached, return true.
    if (buffer_byte_length == .detached) return true;

    // 5. Let byteOffsetStart be O.[[ByteOffset]].
    const byte_offset_start = typed_array.fields.byte_offset;

    // 6. If O.[[ArrayLength]] is auto, then
    const byte_offset_end = if (typed_array.fields.array_length == .auto) blk: {
        // a. Let byteOffsetEnd be bufferByteLength.
        break :blk buffer_byte_length.value;
    }
    // 7. Else,
    else blk: {
        // a. Let elementSize be TypedArrayElementSize(O).
        const element_size = typedArrayElementSize(typed_array);

        // b. Let byteOffsetEnd be byteOffsetStart + O.[[ArrayLength]] × elementSize.
        break :blk std.math.add(
            u53,
            byte_offset_start,
            std.math.mul(
                u53,
                typed_array.fields.array_length.value,
                element_size,
            ) catch return true,
        ) catch return true;
    };

    // 8. If byteOffsetStart > bufferByteLength or byteOffsetEnd > bufferByteLength, return true.
    if (byte_offset_start > buffer_byte_length.value or
        byte_offset_end > buffer_byte_length.value) return true;

    // 9. NOTE: 0-length TypedArrays are not considered out-of-bounds.
    // 10. Return false.
    return false;
}

/// 10.4.5.14 IsValidIntegerIndex ( O, index )
/// https://tc39.es/ecma262/#sec-isvalidintegerindex
fn isValidIntegerIndex(typed_array: *const TypedArray, index: f64) bool {
    // 1. If IsDetachedBuffer(O.[[ViewedArrayBuffer]]) is true, return false.
    if (isDetachedBuffer(typed_array.fields.viewed_array_buffer)) return false;

    // 2. If IsIntegralNumber(index) is false, return false.
    if (@trunc(index) != index) return false;

    // 3. If index is -0𝔽, return false.
    if (std.math.isNegativeZero(index)) return false;

    // 4. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(O, unordered).
    // 5. NOTE: Bounds checking is not a synchronizing operation when O's backing buffer is a
    //    growable SharedArrayBuffer.
    const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .unordered);

    // 6. If IsTypedArrayOutOfBounds(taRecord) is true, return false.
    if (isTypedArrayOutOfBounds(ta)) return false;

    // 7. Let length be TypedArrayLength(taRecord).
    const length = typedArrayLength(ta);

    // 8. If ℝ(index) < 0 or ℝ(index) ≥ length, return false.
    if (index < 0 or index >= @as(f64, @floatFromInt(length))) return false;

    // 9. Return true.
    return true;
}

/// 10.4.5.15 TypedArrayGetElement ( O, index )
/// https://tc39.es/ecma262/#sec-typedarraygetelement
fn typedArrayGetElement(agent: *Agent, typed_array: *const TypedArray, index: u53) Value {
    // 1. If IsValidIntegerIndex(O, index) is false, return undefined.
    if (!isValidIntegerIndex(typed_array, @floatFromInt(index))) return .undefined;

    // 2. Let offset be O.[[ByteOffset]].
    const offset = typed_array.fields.byte_offset;

    // 3. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(typed_array);

    // 4. Let byteIndexInBuffer be (ℝ(index) × elementSize) + offset.
    const byte_index_in_buffer = (index * element_size) + offset;

    // 5. Let elementType be TypedArrayElementType(O).
    inline for (typed_array_element_types) |entry| {
        const name, const T = entry;
        if (std.mem.eql(u8, typed_array.fields.typed_array_name, name)) {
            // 6. Return GetValueFromBuffer(O.[[ViewedArrayBuffer]], byteIndexInBuffer,
            //    elementType, true, unordered).
            return Value.from(getValueFromBuffer(
                agent,
                typed_array.fields.viewed_array_buffer,
                byte_index_in_buffer,
                T,
                true,
                .unordered,
                null,
            ));
        }
    } else unreachable;
}

/// 10.4.5.16 TypedArraySetElement ( O, index, value )
/// https://tc39.es/ecma262/#sec-typedarraysetelement
fn typedArraySetElement(agent: *Agent, typed_array: *const TypedArray, index: f64, value: Value) Agent.Error!void {
    // 1. If O.[[ContentType]] is bigint, let numValue be ? ToBigInt(value).
    // 2. Otherwise, let numValue be ? ToNumber(value).
    const number_value = if (typed_array.fields.content_type == .bigint)
        Value.from(try value.toBigInt(agent))
    else
        Value.from(try value.toNumber(agent));

    // 3. If IsValidIntegerIndex(O, index) is true, then
    if (isValidIntegerIndex(typed_array, index)) {
        // a. Let offset be O.[[ByteOffset]].
        const offset = typed_array.fields.byte_offset;

        // b. Let elementSize be TypedArrayElementSize(O).
        const element_size = typedArrayElementSize(typed_array);

        // c. Let byteIndexInBuffer be (ℝ(index) × elementSize) + offset.
        const byte_index_in_buffer = @as(u53, @intFromFloat(index)) * element_size + offset;

        // d. Let elementType be TypedArrayElementType(O).
        inline for (typed_array_element_types) |entry| {
            const name, const T = entry;
            if (std.mem.eql(u8, typed_array.fields.typed_array_name, name)) {
                // e. Perform SetValueInBuffer(O.[[ViewedArrayBuffer]], byteIndexInBuffer,
                //    elementType, numValue, true, unordered).
                try setValueInBuffer(
                    agent,
                    typed_array.fields.viewed_array_buffer,
                    byte_index_in_buffer,
                    T,
                    number_value,
                    true,
                    .unordered,
                    null,
                );
                break;
            }
        } else unreachable;
    }

    // 4. Return unused.
}

/// 23.2.2 Properties of the %TypedArray% Intrinsic Object
/// https://tc39.es/ecma262/#sec-properties-of-the-%typedarray%-intrinsic-object
pub const TypedArrayConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 0,
            .name = "TypedArray",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinAccessor(object, "@@species", @"@@species", null, realm);

        // 23.2.2.3 %TypedArray%.prototype
        // https://tc39.es/ecma262/#sec-%typedarray%.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%TypedArray.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 23.2.3.5 %TypedArray%.prototype.constructor
        // https://tc39.es/ecma262/#sec-%typedarray%.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%TypedArray.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 23.2.1.1 %TypedArray% ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%
    fn behaviour(agent: *Agent, _: Value, _: ArgumentsList, _: ?Object) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "TypedArray abstract superclass cannot be constructed",
            .{},
        );
    }

    /// 23.2.2.4 get %TypedArray% [ @@species ]
    /// https://tc39.es/ecma262/#sec-get-%typedarray%-@@species
    fn @"@@species"(_: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 23.2.3 Properties of the %TypedArray% Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-%typedarrayprototype%-object
pub const TypedArrayPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "buffer", buffer, null, realm);
        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "byteOffset", byteOffset, null, realm);
        try defineBuiltinFunction(object, "entries", entries, 0, realm);
        try defineBuiltinFunction(object, "join", join, 1, realm);
        try defineBuiltinFunction(object, "keys", keys, 0, realm);
        try defineBuiltinAccessor(object, "length", length, null, realm);
        try defineBuiltinFunction(object, "values", values, 0, realm);
        try defineBuiltinAccessor(object, "@@toStringTag", @"@@toStringTag", null, realm);

        // 23.2.3.34 %TypedArray%.prototype.toString ( )
        // https://tc39.es/ecma262/#sec-%typedarray%.prototype.tostring
        try defineBuiltinProperty(object, "toString", Value.from(try realm.intrinsics.@"%Array.prototype.toString%"()));

        // 23.2.3.37 %TypedArray%.prototype [ @@iterator ] ( )
        // https://tc39.es/ecma262/#sec-%typedarray%.prototype-@@iterator
        const @"%TypedArray.prototype.values%" = object.propertyStorage().get(PropertyKey.from("values")).?;
        try defineBuiltinProperty(object, "@@iterator", @"%TypedArray.prototype.values%");

        return object;
    }

    /// 23.2.3.2 get %TypedArray%.prototype.buffer
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.buffer
    fn buffer(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[TypedArrayName]]).
        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        const typed_array = try this_value.requireInternalSlot(agent, TypedArray);

        // 4. Let buffer be O.[[ViewedArrayBuffer]].
        const buffer_ = typed_array.fields.viewed_array_buffer;

        // 5. Return buffer.
        return Value.from(buffer_.object());
    }

    /// 23.2.3.3 get %TypedArray%.prototype.byteLength
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.bytelength
    fn byteLength(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[TypedArrayName]]).
        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        const typed_array = try this_value.requireInternalSlot(agent, TypedArray);

        // 4. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
        const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);

        // 5. Let size be TypedArrayByteLength(taRecord).
        const size = typedArrayByteLength(ta);

        // 6. Return 𝔽(size).
        return Value.from(size);
    }

    /// 23.2.3.4 get %TypedArray%.prototype.byteOffset
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.byteoffset
    fn byteOffset(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[TypedArrayName]]).
        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        const typed_array = try this_value.requireInternalSlot(agent, TypedArray);

        // 4. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
        const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);

        // 5. If IsTypedArrayOutOfBounds(taRecord) is true, return +0𝔽.
        if (isTypedArrayOutOfBounds(ta)) return Value.from(0);

        // 6. Let offset be O.[[ByteOffset]].
        const offset = typed_array.fields.byte_offset;

        // 7. Return 𝔽(offset).
        return Value.from(offset);
    }

    /// 23.2.3.7 %TypedArray%.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? ValidateTypedArray(O, seq-cst).
        const object = @constCast(
            (try validateTypedArray(agent, this_value, .seq_cst)).object,
        ).object();

        // 3. Return CreateArrayIterator(O, key+value).
        return Value.from(try createArrayIterator(agent, object, .@"key+value"));
    }

    /// 23.2.3.18 %TypedArray%.prototype.join ( separator )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.join
    fn join(agent: *Agent, this_value: Value, arguments: ArgumentsList) Agent.Error!Value {
        const separator = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If separator is undefined, let sep be ",".
        // 5. Else, let sep be ? ToString(separator).
        const sep = if (separator == .undefined) "," else (try separator.toString(agent)).utf8;

        // 6. Let R be the empty String.
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var elements = try std.ArrayList([]const u8).initCapacity(agent.gc_allocator, @intCast(len));
        defer elements.deinit();

        // 7. Let k be 0.
        var k: u53 = 0;

        // 8. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, set R to the string-concatenation of R and sep.

            // b. Let element be ! Get(O, ! ToString(𝔽(k))).
            const element = object.get(PropertyKey.from(k)) catch |err| try noexcept(err);

            // c. If element is undefined, let next be the empty String; otherwise, let next be
            //    ! ToString(element).
            const next = if (element == .undefined) "" else (try element.toString(agent)).utf8;

            // d. Set R to the string-concatenation of R and next.
            try elements.append(next);

            // e. Set k to k + 1.
        }

        // 9. Return R.
        return Value.from(
            try std.mem.join(agent.gc_allocator, sep, elements.items),
        );
    }

    /// 23.2.3.19 %TypedArray%.prototype.keys ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.keys
    fn keys(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? ValidateTypedArray(O, seq-cst).
        const object = @constCast(
            (try validateTypedArray(agent, this_value, .seq_cst)).object,
        ).object();

        // 3. Return CreateArrayIterator(O, key).
        return Value.from(try createArrayIterator(agent, object, .key));
    }

    /// 23.2.3.21 get %TypedArray%.prototype.length
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.length
    fn length(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[TypedArrayName]]).
        // 3. Assert: O has [[ViewedArrayBuffer]] and [[ArrayLength]] internal slots.
        const typed_array = try this_value.requireInternalSlot(agent, TypedArray);

        // 4. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
        const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);

        // 5. If IsTypedArrayOutOfBounds(taRecord) is true, return +0𝔽.
        if (isTypedArrayOutOfBounds(ta)) return Value.from(0);

        // 6. Let length be TypedArrayLength(taRecord).
        const length_ = typedArrayLength(ta);

        // 7. Return 𝔽(length).
        return Value.from(length_);
    }

    /// 23.2.3.35 %TypedArray%.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.values
    fn values(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? ValidateTypedArray(O, seq-cst).
        const object = @constCast(
            (try validateTypedArray(agent, this_value, .seq_cst)).object,
        ).object();

        // 3. Return CreateArrayIterator(O, value).
        return Value.from(try createArrayIterator(agent, object, .value));
    }

    /// 23.2.3.38 get %TypedArray%.prototype [ @@toStringTag ]
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype-@@tostringtag
    fn @"@@toStringTag"(_: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, return undefined.
        const object_ = switch (this_value) {
            .object => |object_| object_,
            else => return .undefined,
        };

        // 3. If O does not have a [[TypedArrayName]] internal slot, return undefined.
        if (!object_.is(TypedArray)) return .undefined;

        // 4. Let name be O.[[TypedArrayName]].
        const name = object_.as(TypedArray).fields.typed_array_name;

        // 5. Assert: name is a String.
        // 6. Return name.
        return Value.from(name);
    }
};

/// 23.2.4.4 ValidateTypedArray ( O, order )
/// https://tc39.es/ecma262/#sec-validatetypedarray
fn validateTypedArray(agent: *Agent, object: Value, order: Order) Agent.Error!TypedArrayWithBufferWitness {
    // 1. Perform ? RequireInternalSlot(O, [[TypedArrayName]]).
    // 2. Assert: O has a [[ViewedArrayBuffer]] internal slot.
    const typed_array = try object.requireInternalSlot(agent, TypedArray);

    // 3. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(O, order).
    const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, order);

    // 4. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
    if (isTypedArrayOutOfBounds(ta)) {
        return agent.throwException(.type_error, "Typed array is out of bounds", .{});
    }

    // 5. Return taRecord.
    return ta;
}

/// 23.2.4.5 TypedArrayElementSize ( O )
/// https://tc39.es/ecma262/#sec-typedarrayelementsize
fn typedArrayElementSize(typed_array: *const TypedArray) u53 {
    // 1. Return the Element Size value specified in Table 70 for O.[[TypedArrayName]].
    // FIXME: Would be nice to completely do this at comptime based on the struct type, but once
    //        they are unique (for type comparison) we cannot cast to the generic TypedArray anymore.
    return @intCast(std.ComptimeStringMap(u4, .{
        .{ "Int8Array", 1 },
        .{ "Uint8Array", 1 },
        .{ "Uint8ClampedArray", 1 },
        .{ "Int16Array", 2 },
        .{ "Uint16Array", 2 },
        .{ "Int32Array", 4 },
        .{ "Uint32Array", 4 },
        .{ "BigInt64Array", 8 },
        .{ "BigUint64Array", 8 },
        .{ "Float32Array", 4 },
        .{ "Float64Array", 8 },
    }).get(typed_array.fields.typed_array_name).?);
}

/// 23.2.5.1.1 AllocateTypedArray ( constructorName, newTarget, defaultProto [ , length ] )
/// https://tc39.es/ecma262/#sec-allocatetypedarray
fn allocateTypedArray(
    agent: *Agent,
    comptime constructor_name: []const u8,
    new_target: Object,
    comptime default_prototype: []const u8,
    length: ?u53,
) !Object {
    // 1. Let proto be ? GetPrototypeFromConstructor(newTarget, defaultProto).
    const prototype = try getPrototypeFromConstructor(new_target, default_prototype);

    // 2. Let obj be TypedArrayCreate(proto).
    // 3. Assert: obj.[[ViewedArrayBuffer]] is undefined.
    const object = try TypedArray.create(agent, .{
        // 10.4.5.10 TypedArrayCreate ( prototype )
        // https://tc39.es/ecma262/#sec-typedarraycreate
        // 1. Let internalSlotsList be « [[Prototype]], [[Extensible]], [[ViewedArrayBuffer]],
        //    [[TypedArrayName]], [[ContentType]], [[ByteLength]], [[ByteOffset]], [[ArrayLength]] ».
        // 2. Let A be MakeBasicObject(internalSlotsList).
        .internal_methods = .{
            // 3. Set A.[[GetOwnProperty]] as specified in 10.4.5.1.
            .getOwnProperty = getOwnProperty,

            // 4. Set A.[[HasProperty]] as specified in 10.4.5.2.
            .hasProperty = hasProperty,

            // 5. Set A.[[DefineOwnProperty]] as specified in 10.4.5.3.
            .defineOwnProperty = defineOwnProperty,

            // 6. Set A.[[Get]] as specified in 10.4.5.4.
            .get = get,

            // 7. Set A.[[Set]] as specified in 10.4.5.5.
            .set = set,

            // 8. Set A.[[Delete]] as specified in 10.4.5.6.
            .delete = delete,

            // 9. Set A.[[OwnPropertyKeys]] as specified in 10.4.5.7.
            .ownPropertyKeys = ownPropertyKeys,
        },

        // 10. Set A.[[Prototype]] to prototype.
        .prototype = prototype,

        .fields = .{
            // NOTE: This is either set via allocateTypedArrayBuffer() below, or at the call site.
            .viewed_array_buffer = undefined,

            // 4. Set obj.[[TypedArrayName]] to constructorName.
            .typed_array_name = constructor_name,

            // 5. If constructorName is either "BigInt64Array" or "BigUint64Array", set
            //    obj.[[ContentType]] to bigint.
            // 6. Otherwise, set obj.[[ContentType]] to number.
            .content_type = comptime blk: {
                if (std.mem.eql(u8, constructor_name, "BigInt64Array")) break :blk .bigint;
                if (std.mem.eql(u8, constructor_name, "BigUint64Array")) break :blk .bigint;
                break :blk .number;
            },

            // 7. If length is not present, then
            // NOTE: We do this unconditionally here and skip the branch below instead.

            // a. Set obj.[[ByteLength]] to 0.
            .byte_length = .{ .value = 0 },

            // b. Set obj.[[ByteOffset]] to 0.
            .byte_offset = 0,

            // c. Set obj.[[ArrayLength]] to 0.
            .array_length = .{ .value = 0 },
        },
    });

    // 7. If length is not present, then
    // 8. Else,
    if (length != null) {
        // a. Perform ? AllocateTypedArrayBuffer(obj, length).
        try allocateTypedArrayBuffer(agent, object.as(TypedArray), length.?);
    }

    // 9. Return obj.
    return object;
}

/// 23.2.5.1.2 InitializeTypedArrayFromTypedArray ( O, srcArray )
/// https://tc39.es/ecma262/#sec-initializetypedarrayfromtypedarray
fn initializeTypedArrayFromTypedArray(
    agent: *Agent,
    typed_array: *builtins.TypedArray,
    src_array: *const builtins.TypedArray,
) Agent.Error!void {
    const realm = agent.currentRealm();

    // 1. Let srcData be srcArray.[[ViewedArrayBuffer]].
    const src_data = src_array.fields.viewed_array_buffer;

    // 3. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(typed_array);

    // 2. Let elementType be TypedArrayElementType(O).
    // NOTE: These are done with a string comparison for now.
    const element_type = typed_array.fields.typed_array_name;

    // 5. Let srcElementSize be TypedArrayElementSize(srcArray).
    const src_element_size = typedArrayElementSize(src_array);

    // 4. Let srcType be TypedArrayElementType(srcArray).
    const src_type = src_array.fields.typed_array_name;

    // 6. Let srcByteOffset be srcArray.[[ByteOffset]].
    const src_byte_offset = src_array.fields.byte_offset;

    // 7. Let srcRecord be MakeTypedArrayWithBufferWitnessRecord(srcArray, seq-cst).
    const src = makeTypedArrayWithBufferWitnessRecord(src_array, .seq_cst);

    // 8. If IsTypedArrayOutOfBounds(srcRecord) is true, throw a TypeError exception.
    if (isTypedArrayOutOfBounds(src)) {
        return agent.throwException(.type_error, "Typed array is out of bounds", .{});
    }

    // 9. Let elementLength be TypedArrayLength(srcRecord).
    const element_length = typedArrayLength(src);

    // 10. Let byteLength be elementSize × elementLength.
    const byte_length = std.math.mul(u53, element_size, element_length) catch {
        return agent.throwException(
            .range_error,
            "Invalid typed array length {}",
            .{element_length},
        );
    };

    // 11. If elementType is srcType, then
    const data = if (std.mem.eql(u8, element_type, src_type)) blk: {
        // a. Let data be ? CloneArrayBuffer(srcData, srcByteOffset, byteLength).
        break :blk (try cloneArrayBuffer(
            agent,
            src_data,
            src_byte_offset,
            byte_length,
        )).as(builtins.ArrayBuffer);
    }
    // 12. Else,
    else blk: {
        // a. Let data be ? AllocateArrayBuffer(%ArrayBuffer%, byteLength).
        const data = (try allocateArrayBuffer(
            agent,
            try realm.intrinsics.@"%ArrayBuffer%"(),
            byte_length,
            null,
        )).as(builtins.ArrayBuffer);

        // b. If srcArray.[[ContentType]] is not O.[[ContentType]], throw a TypeError exception.
        if (src_array.fields.content_type != typed_array.fields.content_type) {
            return agent.throwException(
                .type_error,
                "Cannot convert between BigInt and Number typed arrays",
                .{},
            );
        }

        // c. Let srcByteIndex be srcByteOffset.
        var src_byte_index = src_byte_offset;

        // d. Let targetByteIndex be 0.
        var target_byte_index: u53 = 0;

        // e. Let count be elementLength.
        var count = element_length;

        // f. Repeat, while count > 0,
        while (count > 0) : (count -= 1) {
            // FIXME: The repeated branching and string comparisons here are definitely not ideal,
            //        either convert this into two comptime arguments passing the actual type or at
            //        least some kind of jump table.
            const value = blk_value: inline for (typed_array_element_types) |entry| {
                const name, const T = entry;
                if (std.mem.eql(u8, src_type, name)) {
                    // i. Let value be GetValueFromBuffer(srcData, srcByteIndex, srcType, true, unordered).
                    break :blk_value Value.from(getValueFromBuffer(
                        agent,
                        src_data,
                        src_byte_index,
                        T,
                        true,
                        .unordered,
                        null,
                    ));
                }
            } else unreachable;

            inline for (typed_array_element_types) |entry| {
                const name, const T = entry;
                if (std.mem.eql(u8, element_type, name)) {
                    // ii. Perform SetValueInBuffer(data, targetByteIndex, elementType, value, true, unordered).
                    try setValueInBuffer(
                        agent,
                        data,
                        target_byte_index,
                        T,
                        value,
                        true,
                        .unordered,
                        null,
                    );
                    break;
                }
            } else unreachable;

            // iii. Set srcByteIndex to srcByteIndex + srcElementSize.
            src_byte_index += src_element_size;

            // iv. Set targetByteIndex to targetByteIndex + elementSize.
            target_byte_index += element_size;

            // v. Set count to count - 1.
        }

        break :blk data;
    };

    // 13. Set O.[[ViewedArrayBuffer]] to data.
    typed_array.fields.viewed_array_buffer = data;

    // 14. Set O.[[ByteLength]] to byteLength.
    typed_array.fields.byte_length = .{ .value = byte_length };

    // 15. Set O.[[ByteOffset]] to 0.
    typed_array.fields.byte_offset = 0;

    // 16. Set O.[[ArrayLength]] to elementLength.
    typed_array.fields.array_length = .{ .value = element_length };

    // 17. Return unused.
}

/// 23.2.5.1.3 InitializeTypedArrayFromArrayBuffer ( O, buffer, byteOffset, length )
/// https://tc39.es/ecma262/#sec-initializetypedarrayfromarraybuffer
fn initializeTypedArrayFromArrayBuffer(
    agent: *Agent,
    typed_array: *TypedArray,
    buffer: *builtins.ArrayBuffer,
    byte_offset: Value,
    length: Value,
) Agent.Error!void {
    // 1. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(typed_array);

    // 2. Let offset be ? ToIndex(byteOffset).
    const offset = try byte_offset.toIndex(agent);

    // 3. If offset modulo elementSize ≠ 0, throw a RangeError exception.
    if (@mod(offset, element_size) != 0) {
        return agent.throwException(
            .range_error,
            "Offset must be multiple of {}, got {}",
            .{ element_size, offset },
        );
    }

    // 4. Let bufferIsFixedLength be IsFixedLengthArrayBuffer(buffer).
    const buffer_is_fixed_length = isFixedLengthArrayBuffer(buffer);

    // 5. If length is not undefined, then
    //     a. Let newLength be ? ToIndex(length).
    const new_length: u53 = if (length != .undefined) try length.toIndex(agent) else undefined;

    // 6. If IsDetachedBuffer(buffer) is true, throw a TypeError exception.
    if (isDetachedBuffer(buffer)) {
        return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
    }

    // 7. Let bufferByteLength be ArrayBufferByteLength(buffer, seq-cst).
    const buffer_byte_length = arrayBufferByteLength(buffer, .seq_cst);

    // 8. If length is undefined and bufferIsFixedLength is false, then
    if (length == .undefined and !buffer_is_fixed_length) {
        // a. If offset > bufferByteLength, throw a RangeError exception.
        if (offset > buffer_byte_length) {
            return agent.throwException(
                .range_error,
                "Offset must not exceed buffer byte length {}, got {}",
                .{ buffer_byte_length, offset },
            );
        }

        // b. Set O.[[ByteLength]] to auto.
        typed_array.fields.byte_length = .auto;

        // c. Set O.[[ArrayLength]] to auto.
        typed_array.fields.array_length = .auto;
    }
    // 9. Else,
    else {
        // a. If length is undefined, then
        const new_byte_length = if (length == .undefined) blk: {
            // i. If bufferByteLength modulo elementSize ≠ 0, throw a RangeError exception.
            if (@mod(buffer_byte_length, element_size) != 0) {
                return agent.throwException(
                    .range_error,
                    "Buffer byte length must be multiple of {}, got {}",
                    .{ element_size, buffer_byte_length },
                );
            }

            // ii. Let newByteLength be bufferByteLength - offset.
            break :blk std.math.sub(u53, buffer_byte_length, offset) catch {
                // iii. If newByteLength < 0, throw a RangeError exception.
                return agent.throwException(
                    .range_error,
                    "Offset must not exceed buffer byte length {}, got {}",
                    .{ buffer_byte_length, offset },
                );
            };
        }
        // b. Else,
        else blk: {
            // i. Let newByteLength be newLength × elementSize.
            const new_byte_length = std.math.mul(u53, new_length, element_size) catch {
                return agent.throwException(
                    .range_error,
                    "Invalid typed array length {}",
                    .{new_length},
                );
            };

            // ii. If offset + newByteLength > bufferByteLength, throw a RangeError exception.
            if (if (std.math.add(u53, offset, new_byte_length)) |x|
                x > buffer_byte_length
            else |_|
                true)
            {
                return agent.throwException(
                    .range_error,
                    "Offset {} and byte length {} are out of range for buffer byte length {}",
                    .{ offset, new_byte_length, buffer_byte_length },
                );
            }

            break :blk new_byte_length;
        };

        // c. Set O.[[ByteLength]] to newByteLength.
        typed_array.fields.byte_length = .{ .value = new_byte_length };

        // d. Set O.[[ArrayLength]] to newByteLength / elementSize.
        typed_array.fields.array_length = .{ .value = @divExact(new_byte_length, element_size) };
    }

    // 10. Set O.[[ViewedArrayBuffer]] to buffer.
    typed_array.fields.viewed_array_buffer = buffer;

    // 11. Set O.[[ByteOffset]] to offset.
    typed_array.fields.byte_offset = offset;

    // 12. Return unused.
}

/// 23.2.5.1.4 InitializeTypedArrayFromList ( O, values )
/// https://tc39.es/ecma262/#sec-initializetypedarrayfromlist
fn initializeTypedArrayFromList(
    agent: *Agent,
    typed_array: *TypedArray,
    values: []const Value,
) Agent.Error!void {
    // 1. Let len be the number of elements in values.
    // NOTE: allocateTypedArrayBuffer() will throw a nice error if this is too large, so truncating is fine
    const len = std.math.lossyCast(u53, values.len);

    // 2. Perform ? AllocateTypedArrayBuffer(O, len).
    try allocateTypedArrayBuffer(agent, typed_array, len);

    // 3. Let k be 0.
    var k: u53 = 0;

    // 4. Repeat, while k < len,
    while (k < len) : (k += 1) {
        // a. Let Pk be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k);

        // b. Let kValue be the first element of values.
        const k_value = values[@intCast(k)];

        // c. Remove the first element from values.
        // NOTE: The caller retains ownership over `values`, so we're not doing this.

        // d. Perform ? Set(O, Pk, kValue, true).
        try typed_array.object().set(property_key, k_value, .throw);

        // e. Set k to k + 1.
    }

    // 5. Assert: values is now an empty List.
    // 6. Return unused.
}

/// 23.2.5.1.5 InitializeTypedArrayFromArrayLike ( O, arrayLike )
/// https://tc39.es/ecma262/#sec-initializetypedarrayfromarraylike
fn initializeTypedArrayFromArrayLike(
    agent: *Agent,
    typed_array: *TypedArray,
    array_like: Object,
) Agent.Error!void {
    // 1. Let len be ? LengthOfArrayLike(arrayLike).
    const len = try array_like.lengthOfArrayLike();

    // 2. Perform ? AllocateTypedArrayBuffer(O, len).
    try allocateTypedArrayBuffer(agent, typed_array, len);

    // 3. Let k be 0.
    var k: u53 = 0;

    // 4. Repeat, while k < len,
    while (k < len) : (k += 1) {
        // a. Let Pk be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k);

        // b. Let kValue be ? Get(arrayLike, Pk).
        const k_value = try array_like.get(property_key);

        // c. Perform ? Set(O, Pk, kValue, true).
        try typed_array.object().set(property_key, k_value, .throw);

        // d. Set k to k + 1.
    }

    // 5. Return unused.
}

/// 23.2.5.1.6 AllocateTypedArrayBuffer ( O, length )
/// https://tc39.es/ecma262/#sec-allocatetypedarraybuffer
fn allocateTypedArrayBuffer(agent: *Agent, typed_array: *TypedArray, length: u53) !void {
    const realm = agent.currentRealm();

    // 1. Assert: O.[[ViewedArrayBuffer]] is undefined.

    // 2. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(typed_array);

    // 3. Let byteLength be elementSize × length.
    const byte_length = std.math.mul(u53, element_size, length) catch {
        return agent.throwException(
            .range_error,
            "Invalid typed array length {}",
            .{length},
        );
    };

    // 4. Let data be ? AllocateArrayBuffer(%ArrayBuffer%, byteLength).
    const data = try allocateArrayBuffer(
        agent,
        try realm.intrinsics.@"%ArrayBuffer%"(),
        byte_length,
        null,
    );

    // 5. Set O.[[ViewedArrayBuffer]] to data.
    typed_array.fields.viewed_array_buffer = data.as(builtins.ArrayBuffer);

    // 6. Set O.[[ByteLength]] to byteLength.
    typed_array.fields.byte_length = .{ .value = byte_length };

    // 7. Set O.[[ByteOffset]] to 0.
    typed_array.fields.byte_offset = 0;

    // 8. Set O.[[ArrayLength]] to length.
    typed_array.fields.array_length = .{ .value = length };

    // 9. Return unused.
}

/// 23.2.6 Properties of the TypedArray Constructors
/// https://tc39.es/ecma262/#sec-properties-of-the-typedarray-constructors
fn MakeTypedArrayConstructor(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) !Object {
            const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
                .length = 3,
                .name = name,
                .realm = realm,
                .prototype = try realm.intrinsics.@"%TypedArray%"(),
            });

            const prototypeFn = @field(Realm.Intrinsics, "%" ++ name ++ ".prototype%");

            // 23.2.6.1 TypedArray.BYTES_PER_ELEMENT
            // https://tc39.es/ecma262/#sec-typedarray.bytes_per_element
            try defineBuiltinProperty(object, "BYTES_PER_ELEMENT", PropertyDescriptor{
                .value = Value.from(comptime inline for (typed_array_element_types) |entry| {
                    const name_, const T = entry;
                    if (std.mem.eql(u8, name, name_)) break @sizeOf(T);
                }),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            });

            // 23.2.6.2 TypedArray.prototype
            // https://tc39.es/ecma262/#sec-typedarray.prototype
            try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
                .value = Value.from(try prototypeFn(&realm.intrinsics)),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            });

            // 23.2.7.2 TypedArray.prototype.constructor
            // https://tc39.es/ecma262/#sec-typedarray.prototype.constructor
            try defineBuiltinProperty(
                prototypeFn(&realm.intrinsics) catch unreachable,
                "constructor",
                Value.from(object),
            );

            return object;
        }

        /// 23.2.5.1 TypedArray ( ...args )
        /// https://tc39.es/ecma262/#sec-typedarray
        fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) Agent.Error!Value {
            // 1. If NewTarget is undefined, throw a TypeError exception.
            if (new_target == null) {
                return agent.throwException(
                    .type_error,
                    name ++ " must be constructed with 'new'",
                    .{},
                );
            }

            // 2. Let constructorName be the String value of the Constructor Name value specified
            //    in Table 70 for this TypedArray constructor.
            const constructor_name = name;

            // 3. Let proto be "%TypedArray.prototype%".
            const prototype = "%" ++ name ++ ".prototype%";

            // 4. Let numberOfArgs be the number of elements in args.
            const number_of_args = arguments.count();

            // 5. If numberOfArgs = 0, then
            if (number_of_args == 0) {
                // a. Return ? AllocateTypedArray(constructorName, NewTarget, proto, 0).
                return Value.from(try allocateTypedArray(
                    agent,
                    constructor_name,
                    new_target.?,
                    prototype,
                    0,
                ));
            }
            // 6. Else,
            else {
                // a. Let firstArgument be args[0].
                const first_argument = arguments.get(0);

                // b. If firstArgument is an Object, then
                if (first_argument == .object) {
                    // i. Let O be ? AllocateTypedArray(constructorName, NewTarget, proto).
                    const object = try allocateTypedArray(
                        agent,
                        constructor_name,
                        new_target.?,
                        prototype,
                        null,
                    );

                    // ii. If firstArgument has a [[TypedArrayName]] internal slot, then
                    if (first_argument.object.is(TypedArray)) {
                        // 1. Perform ? InitializeTypedArrayFromTypedArray(O, firstArgument).
                        try initializeTypedArrayFromTypedArray(
                            agent,
                            object.as(TypedArray),
                            first_argument.object.as(TypedArray),
                        );
                    }
                    // iii. Else if firstArgument has an [[ArrayBufferData]] internal slot, then
                    else if (first_argument.object.is(builtins.ArrayBuffer)) {
                        // 1. If numberOfArgs > 1, let byteOffset be args[1]; else let byteOffset
                        //    be undefined.
                        const byte_offset = arguments.get(1);

                        // 2. If numberOfArgs > 2, let length be args[2]; else let length be
                        //    undefined.
                        const length = arguments.get(2);

                        // 3. Perform ? InitializeTypedArrayFromArrayBuffer(O, firstArgument,
                        //    byteOffset, length).
                        try initializeTypedArrayFromArrayBuffer(
                            agent,
                            object.as(TypedArray),
                            first_argument.object.as(builtins.ArrayBuffer),
                            byte_offset,
                            length,
                        );
                    }
                    // iv. Else,
                    else {
                        // 1. Assert: firstArgument is an Object and firstArgument does not have
                        //    either a [[TypedArrayName]] or an [[ArrayBufferData]] internal slot.
                        std.debug.assert(
                            first_argument == .object and
                                !first_argument.object.is(TypedArray) and
                                !first_argument.object.is(builtins.ArrayBuffer),
                        );

                        // 2. Let usingIterator be ? GetMethod(firstArgument, @@iterator).
                        const using_iterator = try first_argument.getMethod(
                            agent,
                            PropertyKey.from(agent.well_known_symbols.@"@@iterator"),
                        );

                        // 3. If usingIterator is not undefined, then
                        if (using_iterator != null) {
                            // a. Let values be ? IteratorToList(? GetIteratorFromMethod(
                            //    firstArgument, usingIterator)).
                            const values = try (try getIteratorFromMethod(
                                agent,
                                first_argument,
                                using_iterator.?,
                            )).toList();
                            defer agent.gc_allocator.free(values);

                            // b. Perform ? InitializeTypedArrayFromList(O, values).
                            try initializeTypedArrayFromList(agent, object.as(TypedArray), values);
                        }
                        // 4. Else,
                        else {
                            // a. NOTE: firstArgument is not an Iterable so assume it is already an
                            //    array-like object.
                            // b. Perform ? InitializeTypedArrayFromArrayLike(O, firstArgument).
                            try initializeTypedArrayFromArrayLike(
                                agent,
                                object.as(TypedArray),
                                first_argument.object,
                            );
                        }
                    }

                    // v. Return O.
                    return Value.from(object);
                }
                // c. Else,
                else {
                    // i. Assert: firstArgument is not an Object.
                    std.debug.assert(first_argument != .object);

                    // ii. Let elementLength be ? ToIndex(firstArgument).
                    const element_length = try first_argument.toIndex(agent);

                    // iii. Return ? AllocateTypedArray(constructorName, NewTarget, proto, elementLength).
                    return Value.from(try allocateTypedArray(
                        agent,
                        constructor_name,
                        new_target.?,
                        prototype,
                        element_length,
                    ));
                }
            }
        }
    };
}

/// 23.2.7 Properties of the TypedArray Prototype Objects
/// https://tc39.es/ecma262/#sec-properties-of-typedarray-prototype-objects
fn MakeTypedArrayPrototype(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) !Object {
            const object = try builtins.Object.create(realm.agent, .{
                .prototype = try realm.intrinsics.@"%TypedArray.prototype%"(),
            });

            // 23.2.7.1 TypedArray.prototype.BYTES_PER_ELEMENT
            // https://tc39.es/ecma262/#sec-typedarray.prototype.bytes_per_element
            try defineBuiltinProperty(object, "BYTES_PER_ELEMENT", PropertyDescriptor{
                .value = Value.from(comptime inline for (typed_array_element_types) |entry| {
                    const name_, const T = entry;
                    if (std.mem.eql(u8, name, name_)) break @sizeOf(T);
                }),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            });

            return object;
        }
    };
}

/// 23.2.8 Properties of TypedArray Instances
/// https://tc39.es/ecma262/#sec-properties-of-typedarray-instances
pub const TypedArray = MakeObject(.{
    .Fields = struct {
        pub const ByteLength = union(enum) {
            auto,
            value: u53,
        };

        /// [[TypedArrayName]]
        typed_array_name: []const u8,

        /// [[ContentType]]
        content_type: enum { bigint, number },

        /// [[ViewedArrayBuffer]]
        viewed_array_buffer: *builtins.ArrayBuffer,

        /// [[ByteLength]]
        byte_length: ByteLength,

        /// [[ByteOffset]]
        byte_offset: u53,

        /// [[ArrayLength]]
        array_length: ByteLength,
    },
    .tag = .typed_array,
});
