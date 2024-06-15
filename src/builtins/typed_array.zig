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
const Arguments = types.Arguments;
const ArrayBufferLike = @import("array_buffer.zig").ArrayBufferLike;
const BigInt = types.BigInt;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Order = @import("../builtins/array_buffer.zig").Order;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const allocateArrayBuffer = builtins.allocateArrayBuffer;
const arrayBufferByteLength = builtins.arrayBufferByteLength;
const cloneArrayBuffer = builtins.cloneArrayBuffer;
const createArrayIterator = builtins.createArrayIterator;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const findViaPredicate = builtins.findViaPredicate;
const getIteratorFromMethod = types.getIteratorFromMethod;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const getValueFromBuffer = builtins.getValueFromBuffer;
const isDetachedBuffer = builtins.isDetachedBuffer;
const isFixedLengthArrayBuffer = builtins.isFixedLengthArrayBuffer;
const isStrictlyEqual = types.isStrictlyEqual;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryDelete = builtins.ordinaryDelete;
const ordinaryGet = builtins.ordinaryGet;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const ordinaryHasProperty = builtins.ordinaryHasProperty;
const ordinarySet = builtins.ordinarySet;
const sameValueZero = types.sameValueZero;
const setValueInBuffer = builtins.setValueInBuffer;
const sortIndexedProperties = builtins.sortIndexedProperties;

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

pub const Float16ArrayConstructor = MakeTypedArrayConstructor("Float16Array");
pub const Float16ArrayPrototype = MakeTypedArrayPrototype("Float32Array");

pub const Float32ArrayConstructor = MakeTypedArrayConstructor("Float32Array");
pub const Float32ArrayPrototype = MakeTypedArrayPrototype("Float32Array");

pub const Float64ArrayConstructor = MakeTypedArrayConstructor("Float64Array");
pub const Float64ArrayPrototype = MakeTypedArrayPrototype("Float64Array");

pub const TypedArrayElementType = struct {
    const Self = @This();

    pub const Clamping = enum {
        clamped,
        unclamped,
    };

    T: type,
    clamping: Clamping = .unclamped,

    pub fn elementSize(comptime self: Self) comptime_int {
        return @sizeOf(self.T);
    }

    pub fn conversationOperation(comptime self: Self) fn (Value, *Agent) Agent.Error!self.T {
        const field_name = switch (self.T) {
            i8 => "toInt8",
            u8 => if (self.clamping == .unclamped) "toUint8" else "toUint8Clamp",
            i16 => "toInt16",
            u16 => "toUint16",
            i32 => "toInt32",
            u32 => "toUint32",
            i64 => "toBigInt64",
            u64 => "toBigUint64",
            else => unreachable,
        };
        return @field(Value, field_name);
    }

    /// 25.1.3.10 IsUnclampedIntegerElementType ( type )
    /// https://tc39.es/ecma262/#sec-isunclampedintegerelementtype
    pub inline fn isUnclampedIntegerElementType(comptime self: Self) bool {
        // 1. If type is one of int8, uint8, int16, uint16, int32, or uint32, return true.
        // 2. Return false.
        return self.clamping != .clamped and self.T != f16 and self.T != f32 and self.T != f64;
    }

    /// 25.1.3.11 IsBigIntElementType ( type )
    /// https://tc39.es/ecma262/#sec-isbigintelementtype
    pub inline fn isBigIntElementType(comptime self: Self) bool {
        // 1. If type is either biguint64 or bigint64, return true.
        // 2. Return false.
        return self.T == u64 or self.T == i64;
    }
};

pub const typed_array_element_types = [_]struct { []const u8, TypedArrayElementType }{
    .{ "Int8Array", .{ .T = i8 } },
    .{ "Uint8Array", .{ .T = u8 } },
    .{ "Uint8ClampedArray", .{ .T = u8, .clamping = .clamped } },
    .{ "Int16Array", .{ .T = i16 } },
    .{ "Uint16Array", .{ .T = u16 } },
    .{ "Int32Array", .{ .T = i32 } },
    .{ "Uint32Array", .{ .T = u32 } },
    .{ "BigInt64Array", .{ .T = i64 } },
    .{ "BigUint64Array", .{ .T = u64 } },
    .{ "Float16Array", .{ .T = f16 } },
    .{ "Float32Array", .{ .T = f32 } },
    .{ "Float64Array", .{ .T = f64 } },
};

/// 10.4.5.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-typedarray-getownproperty
fn getOwnProperty(object: Object, property_key: PropertyKey) Allocator.Error!?PropertyDescriptor {
    const agent = object.agent();

    // 1. If P is a String, then
    //     a. Let numericIndex be CanonicalNumericIndexString(P).
    //     b. If numericIndex is not undefined, then
    if (property_key == .integer_index) {
        // i. Let value be TypedArrayGetElement(O, numericIndex).
        const value = try typedArrayGetElement(
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
fn delete(object: Object, property_key: PropertyKey) Allocator.Error!bool {
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
        while (i < length) : (i += 1) {
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
pub const TypedArrayWithBufferWitness = struct {
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

    // 2. If index is not an integral Number, return false.
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
fn typedArrayGetElement(agent: *Agent, typed_array: *const TypedArray, index: u53) Allocator.Error!Value {
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
        const name, const @"type" = entry;
        if (std.mem.eql(u8, typed_array.fields.typed_array_name, name)) {
            // 6. Return GetValueFromBuffer(O.[[ViewedArrayBuffer]], byteIndexInBuffer,
            //    elementType, true, unordered).
            const value = getValueFromBuffer(
                agent,
                typed_array.fields.viewed_array_buffer,
                byte_index_in_buffer,
                @"type",
                true,
                .unordered,
                null,
            );
            return if (@"type".isBigIntElementType())
                Value.from(try BigInt.from(agent.gc_allocator, value))
            else
                Value.from(value);
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
            const name, const @"type" = entry;
            if (std.mem.eql(u8, typed_array.fields.typed_array_name, name)) {
                // e. Perform SetValueInBuffer(O.[[ViewedArrayBuffer]], byteIndexInBuffer,
                //    elementType, numValue, true, unordered).
                try setValueInBuffer(
                    agent,
                    typed_array.fields.viewed_array_buffer,
                    byte_index_in_buffer,
                    @"type",
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
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "TypedArray",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "from", from, 1, realm);
        try defineBuiltinFunction(object, "of", of, 0, realm);
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
    fn constructor(agent: *Agent, _: Arguments, _: ?Object) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "TypedArray abstract superclass cannot be constructed",
            .{},
        );
    }

    /// 23.2.2.1 %TypedArray%.from ( source [ , mapfn [ , thisArg ] ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.from
    fn from(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const source = arguments.get(0);
        const map_fn = arguments.get(1);
        const this_arg = arguments.get(2);

        // 1. Let C be the this value.
        const constructor_ = this_value;

        // 2. If IsConstructor(C) is false, throw a TypeError exception.
        if (!constructor_.isConstructor()) {
            return agent.throwException(.type_error, "{} is not a constructor", .{constructor_});
        }

        // 3. If mapfn is undefined, then
        const mapping = if (map_fn == .undefined) blk: {
            // a. Let mapping be false.
            break :blk false;
        }
        // 4. Else,
        else blk: {
            // a. If IsCallable(mapfn) is false, throw a TypeError exception.
            if (!map_fn.isCallable()) {
                return agent.throwException(.type_error, "{} is not callable", .{map_fn});
            }

            // b. Let mapping be true.
            break :blk true;
        };

        // 5. Let usingIterator be ? GetMethod(source, @@iterator).
        const using_iterator = try source.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@iterator"),
        );

        // 6. If usingIterator is not undefined, then
        if (using_iterator != null) {
            // a. Let values be ? IteratorToList(? GetIteratorFromMethod(source, usingIterator)).
            var iterator = try getIteratorFromMethod(agent, source, using_iterator.?);
            const values = try iterator.toList();
            defer agent.gc_allocator.free(values);

            // b. Let len be the number of elements in values.
            const len = values.len;

            // c. Let targetObj be ? TypedArrayCreateFromConstructor(C, « 𝔽(len) »).
            const target_object = try typedArrayCreateFromConstructor(
                agent,
                constructor_.object,
                &.{Value.from(@as(u53, @intCast(len)))},
            );

            // d. Let k be 0.
            var k: u53 = 0;

            // e. Repeat, while k < len,
            while (k < len) : (k += 1) {
                // i. Let Pk be ! ToString(𝔽(k)).
                const property_key = PropertyKey.from(k);

                // ii. Let kValue be the first element of values.
                const k_value = values[@intCast(k)];

                // iii. Remove the first element from values.
                // NOTE: `values` is a slice, so we're not doing this.

                // iv. If mapping is true, then
                const mapped_value = if (mapping) blk: {
                    // 1. Let mappedValue be ? Call(mapfn, thisArg, « kValue, 𝔽(k) »).
                    break :blk try map_fn.callAssumeCallable(this_arg, &.{ k_value, Value.from(k) });
                }
                // v. Else,
                else blk: {
                    // 1. Let mappedValue be kValue.
                    break :blk k_value;
                };

                // vi. Perform ? Set(targetObj, Pk, mappedValue, true).
                try target_object.set(property_key, mapped_value, .throw);

                // vii. Set k to k + 1.
            }

            // f. Assert: values is now an empty List.
            // g. Return targetObj.
            return Value.from(target_object);
        }

        // 7. NOTE: source is not an Iterable so assume it is already an array-like object.

        // 8. Let arrayLike be ! ToObject(source).
        const array_like = source.toObject(agent) catch |err| try noexcept(err);

        // 9. Let len be ? LengthOfArrayLike(arrayLike).
        const len = try array_like.lengthOfArrayLike();

        // 10. Let targetObj be ? TypedArrayCreateFromConstructor(C, « 𝔽(len) »).
        const target_object = try typedArrayCreateFromConstructor(
            agent,
            constructor_.object,
            &.{Value.from(len)},
        );

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
                break :blk try map_fn.callAssumeCallable(
                    this_arg,
                    &.{ k_value, Value.from(k) },
                );
            }
            // d. Else,
            else blk: {
                // i. Let mappedValue be kValue.
                break :blk k_value;
            };

            // e. Perform ? Set(targetObj, Pk, mappedValue, true).
            try target_object.set(property_key, mapped_value, .throw);

            // f. Set k to k + 1.
        }

        // 13. Return targetObj.
        return Value.from(target_object);
    }

    /// 23.2.2.2 %TypedArray%.of ( ...items )
    /// https://tc39.es/ecma262/#sec-%typedarray%.of
    fn of(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let len be the number of elements in items.
        const len = arguments.count();

        // 2. Let C be the this value.
        const constructor_ = this_value;

        // 3. If IsConstructor(C) is false, throw a TypeError exception.
        if (!constructor_.isConstructor()) {
            return agent.throwException(.type_error, "{} is not a constructor", .{constructor_});
        }

        // 4. Let newObj be ? TypedArrayCreateFromConstructor(C, « 𝔽(len) »).
        const new_object = try typedArrayCreateFromConstructor(
            agent,
            constructor_.object,
            &.{Value.from(@as(u53, @intCast(len)))},
        );

        // 5. Let k be 0.
        // 6. Repeat, while k < len,
        for (arguments.values, 0..) |k_value, k| {
            // a. Let kValue be items[k].

            // b. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(k)));

            // c. Perform ? Set(newObj, Pk, kValue, true).
            try new_object.set(property_key, k_value, .throw);

            // d. Set k to k + 1.
        }

        // 7. Return newObj.
        return Value.from(new_object);
    }

    /// 23.2.2.4 get %TypedArray% [ @@species ]
    /// https://tc39.es/ecma262/#sec-get-%typedarray%-@@species
    fn @"@@species"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 23.2.3 Properties of the %TypedArray% Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-%typedarrayprototype%-object
pub const TypedArrayPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "at", at, 1, realm);
        try defineBuiltinAccessor(object, "buffer", buffer, null, realm);
        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "byteOffset", byteOffset, null, realm);
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
        try defineBuiltinAccessor(object, "length", length, null, realm);
        try defineBuiltinFunction(object, "map", map, 1, realm);
        try defineBuiltinFunction(object, "reduce", reduce, 1, realm);
        try defineBuiltinFunction(object, "reduceRight", reduceRight, 1, realm);
        try defineBuiltinFunction(object, "reverse", reverse, 0, realm);
        try defineBuiltinFunction(object, "set", set_, 1, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);
        try defineBuiltinFunction(object, "some", some, 1, realm);
        try defineBuiltinFunction(object, "sort", sort, 1, realm);
        try defineBuiltinFunction(object, "subarray", subarray, 2, realm);
        try defineBuiltinFunction(object, "toLocaleString", toLocaleString, 0, realm);
        try defineBuiltinFunction(object, "toReversed", toReversed, 0, realm);
        try defineBuiltinFunction(object, "toSorted", toSorted, 1, realm);
        try defineBuiltinFunction(object, "values", values, 0, realm);
        try defineBuiltinFunction(object, "with", with, 2, realm);
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

    /// 23.2.3.1 %TypedArray%.prototype.at ( index )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.at
    fn at(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const index = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let relativeIndex be ? ToIntegerOrInfinity(index).
        const relative_index = try index.toIntegerOrInfinity(agent);

        // 5. If relativeIndex ≥ 0, then
        //     a. Let k be relativeIndex.
        // 6. Else,
        //     a. Let k be len + relativeIndex.
        const k_f64 = if (relative_index >= 0)
            relative_index
        else
            @as(f64, @floatFromInt(len)) + relative_index;

        // 7. If k < 0 or k ≥ len, return undefined.
        if (k_f64 < 0 or k_f64 >= @as(f64, @floatFromInt(len))) return .undefined;
        const k: u53 = @intFromFloat(k_f64);

        // 8. Return ! Get(O, ! ToString(𝔽(k))).
        return object.get(PropertyKey.from(k)) catch |err| try noexcept(err);
    }

    /// 23.2.3.2 get %TypedArray%.prototype.buffer
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.buffer
    fn buffer(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
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
    fn byteLength(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
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
    fn byteOffset(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
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

    /// 23.2.3.6 %TypedArray%.prototype.copyWithin ( target, start [ , end ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.copywithin
    fn copyWithin(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const start = arguments.get(1);
        const end = arguments.get(2);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        var ta = try validateTypedArray(agent, this_value, .seq_cst);
        const typed_array = ta.object;
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        var len = typedArrayLength(ta);
        const len_f64: f64 = @floatFromInt(len);

        // 4. Let relativeTarget be ? ToIntegerOrInfinity(target).
        const relative_target = try target.toIntegerOrInfinity(agent);

        // 5. If relativeTarget = -∞, let targetIndex be 0.
        const target_index_f64 = if (std.math.isNegativeInf(relative_target)) blk: {
            break :blk 0;
        }
        // 6. Else if relativeTarget < 0, let targetIndex be max(len + relativeTarget, 0).
        else if (relative_target < 0) blk: {
            break :blk @max(len_f64 + relative_target, 0);
        }
        // 7. Else, let targetIndex be min(relativeTarget, len).
        else blk: {
            break :blk @min(relative_target, len_f64);
        };
        const target_index: u53 = @intFromFloat(target_index_f64);

        // 8. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 9. If relativeStart = -∞, let startIndex be 0.
        const start_index_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        }
        // 10. Else if relativeStart < 0, let startIndex be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 11. Else, let startIndex be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        const start_index: u53 = @intFromFloat(start_index_f64);

        // 12. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //     ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 13. If relativeEnd = -∞, let endIndex be 0.
        const end_index_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        }
        // 14. Else if relativeEnd < 0, let endIndex be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 15. Else, let endIndex be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };

        // 16. Let count be min(endIndex - startIndex, len - targetIndex).
        const count_f64 = @min(end_index_f64 - start_index_f64, len_f64 - target_index_f64);

        // 17. If count > 0, then
        if (count_f64 > 0) {
            const count: u53 = @intFromFloat(count_f64);

            // a. NOTE: The copying must be performed in a manner that preserves the bit-level
            //    encoding of the source data.

            // b. Let buffer be O.[[ViewedArrayBuffer]].
            const buffer_ = typed_array.fields.viewed_array_buffer;

            // c. Set taRecord to MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
            ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);

            // d. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
            if (isTypedArrayOutOfBounds(ta)) {
                return agent.throwException(.type_error, "Typed array is out of bounds", .{});
            }

            // e. Set len to TypedArrayLength(taRecord).
            len = typedArrayLength(ta);

            // f. Let elementSize be TypedArrayElementSize(O).
            const element_size = typedArrayElementSize(typed_array);

            // g. Let byteOffset be O.[[ByteOffset]].
            const byte_offset = typed_array.fields.byte_offset;

            // h. Let bufferByteLimit be (len × elementSize) + byteOffset.
            const buffer_byte_limit = (len * element_size) + byte_offset;

            // i. Let toByteIndex be (targetIndex × elementSize) + byteOffset.
            var to_byte_index = (target_index * element_size) + byte_offset;

            // j. Let fromByteIndex be (startIndex × elementSize) + byteOffset.
            var from_byte_index = (start_index * element_size) + byte_offset;

            // k. Let countBytes be count × elementSize.
            var count_bytes = count * element_size;

            // l. If fromByteIndex < toByteIndex and toByteIndex < fromByteIndex + countBytes, then
            const direction: i2 = if (from_byte_index < to_byte_index and
                to_byte_index < (from_byte_index + count_bytes))
            blk: {
                // ii. Set fromByteIndex to fromByteIndex + countBytes - 1.
                from_byte_index += count_bytes - 1;

                // iii. Set toByteIndex to toByteIndex + countBytes - 1.
                to_byte_index += count_bytes - 1;

                // i. Let direction be -1.
                break :blk -1;
            }
            // m. Else,
            else blk: {
                // i. Let direction be 1.
                break :blk 1;
            };

            // n. Repeat, while countBytes > 0,
            while (count_bytes > 0) {
                // i. If fromByteIndex < bufferByteLimit and toByteIndex < bufferByteLimit, then
                if (from_byte_index < buffer_byte_limit and to_byte_index < buffer_byte_limit) {
                    // 1. Let value be GetValueFromBuffer(buffer, fromByteIndex, uint8, true, unordered).
                    const value = getValueFromBuffer(
                        agent,
                        buffer_,
                        from_byte_index,
                        .{ .T = u8 },
                        true,
                        .unordered,
                        null,
                    );

                    // 2. Perform SetValueInBuffer(buffer, toByteIndex, uint8, value, true, unordered).
                    try setValueInBuffer(
                        agent,
                        buffer_,
                        to_byte_index,
                        .{ .T = u8 },
                        Value.from(value),
                        true,
                        .unordered,
                        null,
                    );

                    // 3. Set fromByteIndex to fromByteIndex + direction.
                    if (direction == 1) from_byte_index += 1 else from_byte_index -|= 1;

                    // 4. Set toByteIndex to toByteIndex + direction.
                    if (direction == 1) to_byte_index += 1 else to_byte_index -|= 1;

                    // 5. Set countBytes to countBytes - 1.
                    count_bytes -= 1;
                }
                // ii. Else,
                else {
                    // 1. Set countBytes to 0.
                    count_bytes = 0;
                }
            }
        }

        // 18. Return O.
        return Value.from(object);
    }

    /// 23.2.3.7 %TypedArray%.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? ValidateTypedArray(O, seq-cst).
        const object = @constCast(
            (try validateTypedArray(agent, this_value, .seq_cst)).object,
        ).object();

        // 3. Return CreateArrayIterator(O, key+value).
        return Value.from(try createArrayIterator(agent, object, .@"key+value"));
    }

    /// 23.2.3.8 %TypedArray%.prototype.every ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.every
    fn every(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
            const test_result = (try callback_fn.callAssumeCallable(
                this_arg,
                &.{ k_value, Value.from(k), Value.from(object) },
            )).toBoolean();

            // d. If testResult is false, return false.
            if (!test_result) return Value.from(false);

            // e. Set k to k + 1.
        }

        // 7. Return true.
        return Value.from(true);
    }

    /// 23.2.3.9 %TypedArray%.prototype.fill ( value [ , start [ , end ] ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.fill
    fn fill(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var value = arguments.get(0);
        const start = arguments.get(1);
        const end = arguments.get(2);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        var ta = try validateTypedArray(agent, this_value, .seq_cst);
        const typed_array = ta.object;
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        var len = typedArrayLength(ta);
        const len_f64: f64 = @floatFromInt(len);

        // 4. If O.[[ContentType]] is bigint, set value to ? ToBigInt(value).
        // 5. Otherwise, set value to ? ToNumber(value).
        value = if (typed_array.fields.content_type == .bigint)
            Value.from(try value.toBigInt(agent))
        else
            Value.from(try value.toNumber(agent));

        // 6. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 7. If relativeStart = -∞, let startIndex be 0.
        const start_index_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        }
        // 8. Else if relativeStart < 0, let startIndex be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 9. Else, let startIndex be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        const start_index: u53 = @intFromFloat(start_index_f64);

        // 10. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //     ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 11. If relativeEnd = -∞, let endIndex be 0.
        const end_index_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        }
        // 12. Else if relativeEnd < 0, let endIndex be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 13. Else, let endIndex be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };
        var end_index: u53 = @intFromFloat(end_index_f64);

        // 14. Set taRecord to MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
        ta = makeTypedArrayWithBufferWitnessRecord(object.as(TypedArray), .seq_cst);

        // 15. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
        if (isTypedArrayOutOfBounds(ta)) {
            return agent.throwException(.type_error, "Typed array is out of bounds", .{});
        }

        // 16. Set len to TypedArrayLength(taRecord).
        len = typedArrayLength(ta);

        // 17. Set endIndex to min(endIndex, len).
        end_index = @min(end_index, len);

        // 18. Let k be startIndex.
        var k: u53 = start_index;

        // 19. Repeat, while k < endIndex,
        while (k < end_index) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Perform ! Set(O, Pk, value, true).
            object.set(property_key, value, .throw) catch |err| try noexcept(err);

            // c. Set k to k + 1.
        }

        // 20. Return O.
        return Value.from(object);
    }

    /// 23.2.3.10 %TypedArray%.prototype.filter ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.filter
    fn filter(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. Let kept be a new empty List.
        var kept = std.ArrayList(Value).init(agent.gc_allocator);
        defer kept.deinit();

        // 6. Let captured be 0.
        var captured: u53 = 0;

        // 7. Let k be 0.
        var k: u53 = 0;

        // 7. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Let selected be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
            const selected = (try callback_fn.callAssumeCallable(
                this_arg,
                &.{ k_value, Value.from(k), Value.from(object) },
            )).toBoolean();

            // d. If selected is true, then
            if (selected) {
                // i. Append kValue to kept.
                try kept.append(k_value);

                // ii. Set captured to captured + 1.
                captured += 1;
            }

            // e. Set k to k + 1.
        }

        // 9. Let A be ? TypedArraySpeciesCreate(O, « 𝔽(captured) »).
        const typed_array = try typedArraySpeciesCreate(
            agent,
            object.as(TypedArray),
            &.{Value.from(captured)},
        );

        // 10. Let n be 0.
        // 11. For each element e of kept, do
        for (kept.items, 0..) |element, n| {
            // a. Perform ! Set(A, ! ToString(𝔽(n)), e, true).
            typed_array.set(
                PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(n))),
                element,
                .throw,
            ) catch |err| try noexcept(err);

            // b. Set n to n + 1.
        }

        // 12. Return A.
        return Value.from(typed_array);
    }

    /// 23.2.3.11 %TypedArray%.prototype.find ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.find
    fn find(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .ascending, predicate, this_arg);

        // 5. Return findRec.[[Value]].
        return find_record.value;
    }

    /// 23.2.3.12 %TypedArray%.prototype.findIndex ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.findindex
    fn findIndex(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .ascending, predicate, this_arg);

        // 5. Return findRec.[[Index]].
        return find_record.index;
    }

    /// 23.2.3.13 %TypedArray%.prototype.findLast ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.findlast
    fn findLast(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .descending, predicate, this_arg);

        // 5. Return findRec.[[Value]].
        return find_record.value;
    }

    /// 23.2.3.14 %TypedArray%.prototype.findLastIndex ( predicate [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.findlastindex
    fn findLastIndex(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
        const find_record = try findViaPredicate(object, len, .descending, predicate, this_arg);

        // 5. Return findRec.[[Index]].
        return find_record.index;
    }

    /// 23.2.3.15 %TypedArray%.prototype.forEach ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Perform ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
            _ = try callback_fn.callAssumeCallable(
                this_arg,
                &.{ k_value, Value.from(k), Value.from(object) },
            );

            // d. Set k to k + 1.
        }

        // 7. Return undefined.
        return .undefined;
    }

    /// 23.2.3.16 %TypedArray%.prototype.includes ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.includes
    fn includes(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If len = 0, return false.
        if (len == 0) return Value.from(false);

        // 5. Let n be ? ToIntegerOrInfinity(fromIndex).
        var n = try from_index.toIntegerOrInfinity(agent);

        // 6. Assert: If fromIndex is undefined, then n is 0.
        if (from_index == .undefined) std.debug.assert(n == 0);

        // 7. If n = +∞, return false.
        if (std.math.isPositiveInf(n)) return Value.from(false);

        // 8. Else if n = -∞, set n to 0.
        if (std.math.isNegativeInf(n)) n = 0;

        // 9. If n ≥ 0, then
        //     a. Let k be n.
        // 10. Else,
        //     a. Let k be len + n.
        //     b. If k < 0, set k to 0.
        const k_f64 = if (n >= 0) n else @max(@as(f64, @floatFromInt(len)) + n, 0);
        if (k_f64 >= std.math.maxInt(u53)) return Value.from(-1);
        var k: u53 = @intFromFloat(k_f64);

        // 11. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let elementK be ! Get(O, ! ToString(𝔽(k))).
            const element_k = object.get(PropertyKey.from(k)) catch |err| try noexcept(err);

            // b. If SameValueZero(searchElement, elementK) is true, return true.
            if (sameValueZero(search_element, element_k)) return Value.from(true);

            // c. Set k to k + 1.
        }

        // 12. Return false.
        return Value.from(false);
    }

    /// 23.2.3.17 %TypedArray%.prototype.indexOf ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.indexof
    fn indexOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If len = 0, return -1𝔽.
        if (len == 0) return Value.from(-1);

        // 5. Let n be ? ToIntegerOrInfinity(fromIndex).
        var n = try from_index.toIntegerOrInfinity(agent);

        // 6. Assert: If fromIndex is undefined, then n is 0.
        if (from_index == .undefined) std.debug.assert(n == 0);

        // 7. If n = +∞, return -1𝔽.
        if (std.math.isPositiveInf(n)) return Value.from(-1);

        // 8. Else if n = -∞, set n to 0.
        if (std.math.isNegativeInf(n)) n = 0;

        // 9. If n ≥ 0, then
        //     a. Let k be n.
        // 10. Else,
        //     a. Let k be len + n.
        //     b. If k < 0, set k to 0.
        const k_f64 = if (n >= 0) n else @max(@as(f64, @floatFromInt(len)) + n, 0);
        if (k_f64 >= std.math.maxInt(u53)) return Value.from(-1);
        var k: u53 = @intFromFloat(k_f64);

        // 11. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let kPresent be ! HasProperty(O, ! ToString(𝔽(k))).
            const k_present = object.hasProperty(PropertyKey.from(k)) catch |err| try noexcept(err);

            // b. If kPresent is true, then
            if (k_present) {
                // i. Let elementK be ! Get(O, ! ToString(𝔽(k))).
                const element_k = object.get(PropertyKey.from(k)) catch |err| try noexcept(err);

                // ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
                if (isStrictlyEqual(search_element, element_k)) return Value.from(k);
            }

            // c. Set k to k + 1.
        }

        // 12. Return -1𝔽.
        return Value.from(-1);
    }

    /// 23.2.3.18 %TypedArray%.prototype.join ( separator )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.join
    fn join(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const separator = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If separator is undefined, let sep be ",".
        // 5. Else, let sep be ? ToString(separator).
        const sep: String.Builder.Segment = if (separator == .undefined)
            .{ .char = ',' }
        else
            .{ .string = try separator.toString(agent) };

        // 6. Let R be the empty String.
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var result = String.Builder.init(agent.gc_allocator);
        defer result.deinit();
        try result.segments.ensureTotalCapacity(@intCast(len));

        // 7. Let k be 0.
        var k: u53 = 0;

        // 8. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, set R to the string-concatenation of R and sep.
            if (k > 0) try result.appendSegment(sep);

            // b. Let element be ! Get(O, ! ToString(𝔽(k))).
            const element = object.get(PropertyKey.from(k)) catch |err| try noexcept(err);

            // c. If element is not undefined, then
            if (element != .undefined) {
                // i. Let S be ! ToString(element).
                const string = element.toString(agent) catch |err| try noexcept(err);

                // ii. Set R to the string-concatenation of R and S.
                try result.appendString(string);
            }

            // d. Set k to k + 1.
        }

        // 9. Return R.
        return Value.from(try result.build());
    }

    /// 23.2.3.19 %TypedArray%.prototype.keys ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.keys
    fn keys(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? ValidateTypedArray(O, seq-cst).
        const object = @constCast(
            (try validateTypedArray(agent, this_value, .seq_cst)).object,
        ).object();

        // 3. Return CreateArrayIterator(O, key).
        return Value.from(try createArrayIterator(agent, object, .key));
    }

    /// 23.2.3.20 %TypedArray%.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.lastindexof
    fn lastIndexOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_element = arguments.get(0);
        const from_index = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If len = 0, return -1𝔽.
        if (len == 0) return Value.from(-1);

        // 5. If fromIndex is present, let n be ? ToIntegerOrInfinity(fromIndex); else let n be len - 1.
        const n = if (arguments.count() > 1)
            try from_index.toIntegerOrInfinity(agent)
        else
            @as(f64, @floatFromInt(len)) - 1;

        // 6. If n = -∞, return -1𝔽.
        if (std.math.isNegativeInf(n)) return Value.from(-1);

        // 7. If n ≥ 0, then
        //     a. Let k be min(n, len - 1).
        // 10. Else,
        //     a. Let k be len + n.
        const k_f64 = if (n >= 0)
            @min(n, @as(f64, @floatFromInt(len)) - 1)
        else
            @as(f64, @floatFromInt(len)) + n;
        if (k_f64 < 0) return Value.from(-1);
        var k: u53 = @intFromFloat(k_f64);

        // 9. Repeat, while k ≥ 0,
        while (k >= 0) : (k -|= 1) {
            // a. Let kPresent be ! HasProperty(O, ! ToString(𝔽(k))).
            const k_present = object.hasProperty(PropertyKey.from(k)) catch |err| try noexcept(err);

            // b. If kPresent is true, then
            if (k_present) {
                // i. Let elementK be ! Get(O, ! ToString(𝔽(k))).
                const element_k = object.get(PropertyKey.from(k)) catch |err| try noexcept(err);

                // ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
                if (isStrictlyEqual(search_element, element_k)) return Value.from(k);
            }

            // c. Set k to k - 1.
            if (k == 0) break;
        }

        // 10. Return -1𝔽.
        return Value.from(-1);
    }

    /// 23.2.3.21 get %TypedArray%.prototype.length
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.length
    fn length(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
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

    /// 23.2.3.22 %TypedArray%.prototype.map ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.map
    fn map(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. Let A be ? TypedArraySpeciesCreate(O, « 𝔽(len) »).
        const array = try typedArraySpeciesCreate(
            agent,
            object.as(TypedArray),
            &.{Value.from(len)},
        );

        // 6. Let k be 0.
        var k: u53 = 0;

        // 7. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Let mappedValue be ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
            const mapped_value = try callback_fn.callAssumeCallable(
                this_arg,
                &.{ k_value, Value.from(k), Value.from(object) },
            );

            // d. Perform ? Set(A, Pk, mappedValue, true).
            try array.set(property_key, mapped_value, .throw);

            // e. Set k to k + 1.
        }

        // 8. Return A.
        return Value.from(array);
    }

    /// 23.2.3.23 %TypedArray%.prototype.reduce ( callbackfn [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.reduce
    fn reduce(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. If len = 0 and initialValue is not present, throw a TypeError exception.
        if (len == 0 and initial_value == null) {
            return agent.throwException(
                .type_error,
                "Cannot reduce empty typed array without initial value",
                .{},
            );
        }

        // 6. Let k be 0.
        var k: u53 = 0;

        // 7. Let accumulator be undefined.
        var accumulator: Value = undefined;

        // 8. If initialValue is present, then
        if (initial_value != null) {
            // a. Set accumulator to initialValue.
            accumulator = initial_value.?;
        }
        // 9. Else,
        else {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Set accumulator to ! Get(O, Pk).
            accumulator = object.get(property_key) catch |err| try noexcept(err);

            // c. Set k to k + 1.
            k += 1;
        }

        // 10. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
            accumulator = try callback_fn.callAssumeCallable(
                .undefined,
                &.{ accumulator, k_value, Value.from(k), Value.from(object) },
            );

            // d. Set k to k + 1.
        }

        // 11. Return accumulator.
        return accumulator;
    }

    /// 23.2.3.24 %TypedArray%.prototype.reduceRight ( callbackfn [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.reduceright
    fn reduceRight(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. If len = 0 and initialValue is not present, throw a TypeError exception.
        if (len == 0 and initial_value == null) {
            return agent.throwException(
                .type_error,
                "Cannot reduce empty typed array without initial value",
                .{},
            );
        }

        // 6. Let k be len - 1.
        var k: ?u53 = std.math.sub(u53, len, 1) catch null;

        // 7. Let accumulator be undefined.
        var accumulator: Value = undefined;

        // 8. If initialValue is present, then
        if (initial_value != null) {
            // a. Set accumulator to initialValue.
            accumulator = initial_value.?;
        }
        // 9. Else,
        else {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k.?);

            // b. Set accumulator to ! Get(O, Pk).
            accumulator = object.get(property_key) catch |err| try noexcept(err);

            // c. Set k to k - 1.
            if (k != null) k = std.math.sub(u53, k.?, 1) catch null;
        }

        // 10. Repeat, while k ≥ 0,
        while (k != null) : (k = (std.math.sub(u53, k.?, 1) catch null)) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k.?);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
            accumulator = try callback_fn.callAssumeCallable(
                .undefined,
                &.{ accumulator, k_value, Value.from(k.?), Value.from(object) },
            );

            // d. Set k to k - 1.
        }

        // 11. Return accumulator.
        return accumulator;
    }

    /// 23.2.3.25 %TypedArray%.prototype.reverse ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.reverse
    fn reverse(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let middle be floor(len / 2).
        const middle = @divFloor(len, 2);

        // 5. Let lower be 0.
        var lower: u53 = 0;

        // 6. Repeat, while lower ≠ middle,
        while (lower != middle) {
            // a. Let upper be len - lower - 1.
            const upper = len - lower - 1;

            // b. Let upperP be ! ToString(𝔽(upper)).
            const upper_property_key = PropertyKey.from(upper);

            // c. Let lowerP be ! ToString(𝔽(lower)).
            const lower_property_key = PropertyKey.from(lower);

            // d. Let lowerValue be ! Get(O, lowerP).
            const lower_value = object.get(lower_property_key) catch |err| try noexcept(err);

            // e. Let upperValue be ! Get(O, upperP).
            const upper_value = object.get(upper_property_key) catch |err| try noexcept(err);

            // f. Perform ! Set(O, lowerP, upperValue, true).
            object.set(lower_property_key, upper_value, .throw) catch |err| try noexcept(err);

            // g. Perform ! Set(O, upperP, lowerValue, true).
            object.set(upper_property_key, lower_value, .throw) catch |err| try noexcept(err);

            // h. Set lower to lower + 1.
            lower += 1;
        }

        // 7. Return O.
        return Value.from(object);
    }

    /// 23.2.3.26 %TypedArray%.prototype.set ( source [ , offset ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.set
    fn set_(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const source = arguments.get(0);
        const offset = arguments.get(1);

        // 1. Let target be the this value.
        // 2. Perform ? RequireInternalSlot(target, [[TypedArrayName]]).
        // 3. Assert: target has a [[ViewedArrayBuffer]] internal slot.
        const target = try this_value.requireInternalSlot(agent, TypedArray);

        // 4. Let targetOffset be ? ToIntegerOrInfinity(offset).
        const target_offset = try offset.toIntegerOrInfinity(agent);

        // 5. If targetOffset < 0, throw a RangeError exception.
        if (target_offset < 0) {
            return agent.throwException(.range_error, "Offset must not be negative", .{});
        }

        // 6. If source is an Object that has a [[TypedArrayName]] internal slot, then
        if (source == .object and source.object.is(TypedArray)) {
            // a. Perform ? SetTypedArrayFromTypedArray(target, targetOffset, source).
            try setTypedArrayFromTypedArray(agent, target, target_offset, source.object.as(TypedArray));
        }
        // 7. Else,
        else {
            // a. Perform ? SetTypedArrayFromArrayLike(target, targetOffset, source).
            try setTypedArrayFromArrayLike(agent, target, target_offset, source);
        }

        // 8. Return undefined.
        return .undefined;
    }

    /// 23.2.3.26.1 SetTypedArrayFromTypedArray ( target, targetOffset, source )
    /// https://tc39.es/ecma262/#sec-settypedarrayfromtypedarray
    fn setTypedArrayFromTypedArray(
        agent: *Agent,
        target: *TypedArray,
        target_offset: f64,
        source: *TypedArray,
    ) Agent.Error!void {
        std.debug.assert(target_offset >= 0);

        // 1. Let targetBuffer be target.[[ViewedArrayBuffer]].
        const target_buffer = target.fields.viewed_array_buffer;

        // 2. Let targetRecord be MakeTypedArrayWithBufferWitnessRecord(target, seq-cst).
        const target_ta = makeTypedArrayWithBufferWitnessRecord(target, .seq_cst);

        // 3. If IsTypedArrayOutOfBounds(targetRecord) is true, throw a TypeError exception.
        if (isTypedArrayOutOfBounds(target_ta)) {
            return agent.throwException(.type_error, "Typed array is out of bounds", .{});
        }

        // 4. Let targetLength be TypedArrayLength(targetRecord).
        const target_length = typedArrayLength(target_ta);

        // 5. Let srcBuffer be source.[[ViewedArrayBuffer]].
        var src_buffer = source.fields.viewed_array_buffer;

        // 6. Let srcRecord be MakeTypedArrayWithBufferWitnessRecord(source, seq-cst).
        const src_ta = makeTypedArrayWithBufferWitnessRecord(source, .seq_cst);

        // 7. If IsTypedArrayOutOfBounds(srcRecord) is true, throw a TypeError exception.
        if (isTypedArrayOutOfBounds(src_ta)) {
            return agent.throwException(.type_error, "Typed array is out of bounds", .{});
        }

        // 8. Let srcLength be TypedArrayLength(srcRecord).
        const src_length = typedArrayLength(src_ta);

        // 9. Let targetType be TypedArrayElementType(target).
        const target_type = target.fields.typed_array_name;

        // 10. Let targetElementSize be TypedArrayElementSize(target).
        const target_element_size = typedArrayElementSize(target);

        // 11. Let targetByteOffset be target.[[ByteOffset]].
        const target_byte_offset = target.fields.byte_offset;

        // 12. Let srcType be TypedArrayElementType(source).
        const src_type = source.fields.typed_array_name;

        // 13. Let srcElementSize be TypedArrayElementSize(source).
        const src_element_size = typedArrayElementSize(source);

        // 14. Let srcByteOffset be source.[[ByteOffset]].
        const src_byte_offset = source.fields.byte_offset;

        // 15. If targetOffset = +∞, throw a RangeError exception.
        if (target_offset == std.math.inf(f64)) {
            return agent.throwException(.range_error, "Offset must not be infinite", .{});
        }

        // 16. If srcLength + targetOffset > targetLength, throw a RangeError exception.
        if (if (std.math.add(u53, src_length, std.math.lossyCast(u53, target_offset))) |x|
            x > target_length
        else |_|
            true)
        {
            return agent.throwException(
                .range_error,
                "Offset {} and source length {} are out of range for target length {}",
                .{ target_offset, src_length, target_length },
            );
        }

        // 17. If target.[[ContentType]] is not source.[[ContentType]], throw a TypeError exception.
        if (target.fields.content_type != source.fields.content_type) {
            return agent.throwException(
                .type_error,
                "Cannot convert between BigInt and Number typed arrays",
                .{},
            );
        }

        // 18. If IsSharedArrayBuffer(srcBuffer) is true, IsSharedArrayBuffer(targetBuffer) is true,
        //     and srcBuffer.[[ArrayBufferData]] is targetBuffer.[[ArrayBufferData]], let
        //     sameSharedArrayBuffer be true; otherwise, let sameSharedArrayBuffer be false.
        const same_shared_array_buffer = src_buffer == .shared_array_buffer and
            target_buffer == .shared_array_buffer and
            src_buffer.shared_array_buffer.fields.array_buffer_data.items.ptr ==
            target_buffer.shared_array_buffer.fields.array_buffer_data.items.ptr;

        // 19. If SameValue(srcBuffer, targetBuffer) is true or sameSharedArrayBuffer is true, then
        var src_byte_index = if (src_buffer.object().sameValue(target_buffer.object()) or same_shared_array_buffer) blk: {
            // a. Let srcByteLength be TypedArrayByteLength(srcRecord).
            const src_byte_length = typedArrayByteLength(src_ta);

            // b. Set srcBuffer to ? CloneArrayBuffer(srcBuffer, srcByteOffset, srcByteLength).
            src_buffer = .{
                .array_buffer = (try cloneArrayBuffer(
                    agent,
                    src_buffer,
                    src_byte_offset,
                    src_byte_length,
                )).as(builtins.ArrayBuffer),
            };

            // c. Let srcByteIndex be 0.
            break :blk 0;
        }
        // 20. Else,
        else blk: {
            // a. Let srcByteIndex be srcByteOffset.
            break :blk src_byte_offset;
        };

        // 21. Let targetByteIndex be (targetOffset × targetElementSize) + targetByteOffset.
        var target_byte_index = (@as(u53, @intFromFloat(target_offset)) * target_element_size) + target_byte_offset;

        // 22. Let limit be targetByteIndex + (targetElementSize × srcLength).
        const limit = target_byte_index + (target_element_size * src_length);

        // 23. If srcType is targetType, then
        if (std.mem.eql(u8, src_type, target_type)) {
            // a. NOTE: The transfer must be performed in a manner that preserves the bit-level
            //    encoding of the source data.
            // b. Repeat, while targetByteIndex < limit,
            while (target_byte_index < limit) : ({
                src_byte_index += 1;
                target_byte_index += 1;
            }) {
                // i. Let value be GetValueFromBuffer(srcBuffer, srcByteIndex, uint8, true, unordered).
                const value = getValueFromBuffer(
                    agent,
                    src_buffer,
                    src_byte_index,
                    .{ .T = u8 },
                    true,
                    .unordered,
                    null,
                );

                // ii. Perform SetValueInBuffer(targetBuffer, targetByteIndex, uint8, value, true, unordered).
                try setValueInBuffer(
                    agent,
                    target_buffer,
                    target_byte_index,
                    .{ .T = u8 },
                    Value.from(value),
                    true,
                    .unordered,
                    null,
                );

                // iii. Set srcByteIndex to srcByteIndex + 1.
                // iv. Set targetByteIndex to targetByteIndex + 1.
            }
        }
        // 24. Else,
        else {

            // a. Repeat, while targetByteIndex < limit,
            while (target_byte_index < limit) : ({
                src_byte_index += src_element_size;
                target_byte_index += target_element_size;
            }) {
                const value = inline for (typed_array_element_types) |entry| {
                    const name, const @"type" = entry;
                    if (std.mem.eql(u8, src_type, name)) {
                        // i. Let value be GetValueFromBuffer(srcBuffer, srcByteIndex, srcType, true, unordered).
                        const value = getValueFromBuffer(
                            agent,
                            src_buffer,
                            src_byte_index,
                            @"type",
                            true,
                            .unordered,
                            null,
                        );
                        break if (@"type".isBigIntElementType())
                            Value.from(try BigInt.from(agent.gc_allocator, value))
                        else
                            Value.from(value);
                    }
                } else unreachable;

                inline for (typed_array_element_types) |entry| {
                    const name, const @"type" = entry;
                    if (std.mem.eql(u8, target_type, name)) {
                        // ii. Perform SetValueInBuffer(targetBuffer, targetByteIndex, targetType, value, true, unordered).
                        try setValueInBuffer(
                            agent,
                            target_buffer,
                            target_byte_index,
                            @"type",
                            value,
                            true,
                            .unordered,
                            null,
                        );
                        break;
                    }
                } else unreachable;

                // iii. Set srcByteIndex to srcByteIndex + srcElementSize.
                // iv. Set targetByteIndex to targetByteIndex + targetElementSize.
            }
        }

        // 25. Return unused.
    }

    /// 23.2.3.26.2 SetTypedArrayFromArrayLike ( target, targetOffset, source )
    /// https://tc39.es/ecma262/#sec-settypedarrayfromarraylike
    fn setTypedArrayFromArrayLike(
        agent: *Agent,
        target: *const TypedArray,
        target_offset: f64,
        source: Value,
    ) Agent.Error!void {
        std.debug.assert(target_offset >= 0);

        // 1. Let targetRecord be MakeTypedArrayWithBufferWitnessRecord(target, seq-cst).
        const target_ta = makeTypedArrayWithBufferWitnessRecord(target, .seq_cst);

        // 2. If IsTypedArrayOutOfBounds(targetRecord) is true, throw a TypeError exception.
        if (isTypedArrayOutOfBounds(target_ta)) {
            return agent.throwException(.type_error, "Typed array is out of bounds", .{});
        }

        // 3. Let targetLength be TypedArrayLength(targetRecord).
        const target_length = typedArrayLength(target_ta);

        // 4. Let src be ? ToObject(source).
        const src = try source.toObject(agent);

        // 5. Let srcLength be ? LengthOfArrayLike(src).
        const src_length = try src.lengthOfArrayLike();

        // 6. If targetOffset = +∞, throw a RangeError exception.
        if (target_offset == std.math.inf(f64)) {
            return agent.throwException(.range_error, "Offset must not be infinite", .{});
        }

        // 7. If srcLength + targetOffset > targetLength, throw a RangeError exception.
        if (if (std.math.add(u53, src_length, std.math.lossyCast(u53, target_offset))) |x|
            x > target_length
        else |_|
            true)
        {
            return agent.throwException(
                .range_error,
                "Offset {} and source length {} are out of range for target length {}",
                .{ target_offset, src_length, target_length },
            );
        }

        // 8. Let k be 0.
        var k: u53 = 0;

        // 9. Repeat, while k < srcLength,
        while (k < src_length) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let value be ? Get(src, Pk).
            const value = try src.get(property_key);

            // c. Let targetIndex be 𝔽(targetOffset + k).
            const target_index = target_offset + @as(f64, @floatFromInt(k));

            // d. Perform ? TypedArraySetElement(target, targetIndex, value).
            try typedArraySetElement(agent, target, target_index, value);

            // e. Set k to k + 1.
        }

        // 10. Return unused.
    }

    /// 23.2.3.27 %TypedArray%.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        var ta = try validateTypedArray(agent, this_value, .seq_cst);
        const typed_array = ta.object;
        const object = this_value.object;

        // 3. Let srcArrayLength be TypedArrayLength(taRecord).
        const src_array_length: f64 = @floatFromInt(typedArrayLength(ta));

        // 4. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 5. If relativeStart = -∞, let startIndex be 0.
        const start_index_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        }
        // 6. Else if relativeStart < 0, let startIndex be max(srcArrayLength + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(src_array_length + relative_start, 0);
        }
        // 7. Else, let startIndex be min(relativeStart, srcArrayLength).
        else blk: {
            break :blk @min(relative_start, src_array_length);
        };
        const start_index: u53 = @intFromFloat(start_index_f64);

        // 8. If end is undefined, let relativeEnd be srcArrayLength; else let relativeEnd be
        //    ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            src_array_length
        else
            try end.toIntegerOrInfinity(agent);

        // 9. If relativeEnd = -∞, let endIndex be 0.
        var end_index_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        }
        // 10. Else if relativeEnd < 0, let endIndex be max(srcArrayLength + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(src_array_length + relative_end, 0);
        }
        // 11. Else, let endIndex be min(relativeEnd, srcArrayLength).
        else blk: {
            break :blk @min(relative_end, src_array_length);
        };
        var end_index: u53 = @intFromFloat(end_index_f64);

        // 12. Let countBytes be max(endIndex - startIndex, 0).
        var count_bytes_f64 = @max(end_index_f64 - start_index_f64, 0);
        var count_bytes: u53 = @intFromFloat(count_bytes_f64);

        // 13. Let A be ? TypedArraySpeciesCreate(O, « 𝔽(countBytes) »).
        const new_typed_array = try typedArraySpeciesCreate(
            agent,
            object.as(TypedArray),
            &.{Value.from(count_bytes)},
        );

        // 14. If countBytes > 0, then
        if (count_bytes > 0) {
            // a. Set taRecord to MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
            ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);

            // b. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
            if (isTypedArrayOutOfBounds(ta)) {
                return agent.throwException(.type_error, "Typed array is out of bounds", .{});
            }

            // c. Set endIndex to min(endIndex, TypedArrayLength(taRecord)).
            end_index = @min(end_index, typedArrayLength(ta));
            end_index_f64 = @floatFromInt(end_index);

            // d. Set countBytes to max(endIndex - startIndex, 0).
            count_bytes_f64 = @max(end_index_f64 - start_index_f64, 0);
            count_bytes = @intFromFloat(count_bytes_f64);

            // e. Let srcType be TypedArrayElementType(O).
            const src_type = typed_array.fields.typed_array_name;

            // f. Let targetType be TypedArrayElementType(A).
            const target_type = new_typed_array.as(TypedArray).fields.typed_array_name;

            // g. If srcType is targetType, then
            if (std.mem.eql(u8, src_type, target_type)) {
                // i. NOTE: The transfer must be performed in a manner that preserves the bit-level
                //    encoding of the source data.

                // ii. Let srcBuffer be O.[[ViewedArrayBuffer]].
                const src_buffer = typed_array.fields.viewed_array_buffer;

                // iii. Let targetBuffer be A.[[ViewedArrayBuffer]].
                const target_buffer = new_typed_array.as(TypedArray).fields.viewed_array_buffer;

                // iv. Let elementSize be TypedArrayElementSize(O).
                const element_size = typedArrayElementSize(typed_array);

                // v. Let srcByteOffset be O.[[ByteOffset]].
                const src_byte_offset = typed_array.fields.byte_offset;

                // vi. Let srcByteIndex be (startIndex × elementSize) + srcByteOffset.
                var src_byte_index = (start_index * element_size) + src_byte_offset;

                // vii. Let targetByteIndex be A.[[ByteOffset]].
                var target_byte_index = new_typed_array.as(TypedArray).fields.byte_offset;

                // viii. Let endByteIndex be targetByteIndex + (countBytes × elementSize).
                const end_byte_index = target_byte_index + (count_bytes * element_size);

                // ix. Repeat, while targetByteIndex < endByteIndex,
                while (target_byte_index < end_byte_index) : ({
                    src_byte_index += 1;
                    target_byte_index += 1;
                }) {
                    // 1. Let value be GetValueFromBuffer(srcBuffer, srcByteIndex, uint8, true,
                    //    unordered).
                    const value = getValueFromBuffer(
                        agent,
                        src_buffer,
                        src_byte_index,
                        .{ .T = u8 },
                        true,
                        .unordered,
                        null,
                    );

                    // 2. Perform SetValueInBuffer(targetBuffer, targetByteIndex, uint8, value,
                    //    true, unordered).
                    try setValueInBuffer(
                        agent,
                        target_buffer,
                        target_byte_index,
                        .{ .T = u8 },
                        Value.from(value),
                        true,
                        .unordered,
                        null,
                    );

                    // 3. Set srcByteIndex to srcByteIndex + 1.
                    // 4. Set targetByteIndex to targetByteIndex + 1.
                }
            }
            // h. Else,
            else {
                // i. Let n be 0.
                var n: u53 = 0;

                // ii. Let k be startIndex.
                var k: u53 = start_index;

                // iii. Repeat, while k < endIndex,
                while (k < end_index) : ({
                    k += 1;
                    n += 1;
                }) {
                    // 1. Let Pk be ! ToString(𝔽(k)).
                    const property_key = PropertyKey.from(k);

                    // 2. Let kValue be ! Get(O, Pk).
                    const k_value = object.get(property_key) catch |err| try noexcept(err);

                    // 3. Perform ! Set(A, ! ToString(𝔽(n)), kValue, true).
                    new_typed_array.set(
                        PropertyKey.from(n),
                        k_value,
                        .throw,
                    ) catch |err| try noexcept(err);

                    // 4. Set k to k + 1.
                    // 5. Set n to n + 1.
                }
            }
        }

        // 15. Return A.
        return Value.from(new_typed_array);
    }

    /// 23.2.3.28 %TypedArray%.prototype.some ( callbackfn [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.some
    fn some(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback_fn = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. Let kValue be ! Get(O, Pk).
            const k_value = object.get(property_key) catch |err| try noexcept(err);

            // c. Let testResult be ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
            const test_result = (try callback_fn.callAssumeCallable(
                this_arg,
                &.{ k_value, Value.from(k), Value.from(object) },
            )).toBoolean();

            // d. If testResult is true, return true.
            if (test_result) return Value.from(true);

            // e. Set k to k + 1.
        }

        // 7. Return false.
        return Value.from(false);
    }

    /// 23.2.3.29 %TypedArray%.prototype.sort ( comparefn )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.sort
    fn sort(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const compare_fn = arguments.get(0);

        // 1. If comparefn is not undefined and IsCallable(comparefn) is false, throw a TypeError
        //    exception.
        if (compare_fn != .undefined and !compare_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{compare_fn});
        }

        // 2. Let obj be the this value.
        // 3. Let taRecord be ? ValidateTypedArray(obj, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const object = this_value.object;

        // 4. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 5. NOTE: The following closure performs a numeric comparison rather than the string
        //    comparison used in 23.1.3.30.
        // 6. Let SortCompare be a new Abstract Closure with parameters (x, y) that captures
        //    comparefn and performs the following steps when called:
        const sortCompare = struct {
            fn func(agent_: *Agent, x: Value, y: Value, compare_fn_: ?Object) Agent.Error!std.math.Order {
                // a. Return ? CompareTypedArrayElements(x, y, comparefn).
                return compareTypedArrayElements(agent_, x, y, compare_fn_);
            }
        }.func;

        // 7. Let sortedList be ? SortIndexedProperties(obj, len, SortCompare, read-through-holes).
        const sorted_list = try sortIndexedProperties(
            agent,
            object,
            len,
            .{
                .impl = sortCompare,
                .compare_fn = if (compare_fn != .undefined) compare_fn.object else null,
            },
            .read_through_holes,
        );

        // 8. Let j be 0.
        var j: u53 = 0;

        // 9. Repeat, while j < len,
        while (j < len) : (j += 1) {
            // a. Perform ! Set(obj, ! ToString(𝔽(j)), sortedList[j], true).
            object.set(
                PropertyKey.from(j),
                sorted_list[@intCast(j)],
                .throw,
            ) catch |err| try noexcept(err);

            // b. Set j to j + 1.
        }

        // 10. Return obj.
        return Value.from(object);
    }

    /// 23.2.3.30 %TypedArray%.prototype.subarray ( start, end )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.subarray
    fn subarray(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[TypedArrayName]]).
        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        const typed_array = try this_value.requireInternalSlot(agent, TypedArray);

        // 4. Let buffer be O.[[ViewedArrayBuffer]].
        const buffer_ = typed_array.fields.viewed_array_buffer;

        // 5. Let srcRecord be MakeTypedArrayWithBufferWitnessRecord(O, seq-cst).
        const src = makeTypedArrayWithBufferWitnessRecord(typed_array, .seq_cst);

        // 6. If IsTypedArrayOutOfBounds(srcRecord) is true, then
        const src_length: f64 = if (isTypedArrayOutOfBounds(src)) blk: {
            // a. Let srcLength be 0.
            break :blk 0;
        }
        // 7. Else,
        else blk: {
            // a. Let srcLength be TypedArrayLength(srcRecord).
            break :blk @floatFromInt(typedArrayLength(src));
        };

        // 8. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 9. If relativeStart = -∞, let startIndex be 0.
        const start_index_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        }
        // 10. Else if relativeStart < 0, let startIndex be max(srcLength + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(src_length + relative_start, 0);
        }
        // 11. Else, let startIndex be min(relativeStart, srcLength).
        else blk: {
            break :blk @min(relative_start, src_length);
        };
        const start_index: u53 = @intFromFloat(start_index_f64);

        // 12. Let elementSize be TypedArrayElementSize(O).
        const element_size = typedArrayElementSize(typed_array);

        // 13. Let srcByteOffset be O.[[ByteOffset]].
        const src_byte_offset = typed_array.fields.byte_offset;

        // 14. Let beginByteOffset be srcByteOffset + (startIndex × elementSize).
        const begin_byte_offset = src_byte_offset + (start_index * element_size);

        // 15. If O.[[ArrayLength]] is auto and end is undefined, then
        const arguments_list = if (typed_array.fields.array_length == .auto and end == .undefined) blk_args: {
            // a. Let argumentsList be « buffer, 𝔽(beginByteOffset) ».
            break :blk_args &.{ Value.from(buffer_.object()), Value.from(begin_byte_offset) };
        }
        // 16. Else,
        else blk_args: {
            // a. If end is undefined, let relativeEnd be srcLength; else let relativeEnd be
            //    ? ToIntegerOrInfinity(end).
            const relative_end = if (end == .undefined)
                src_length
            else
                try end.toIntegerOrInfinity(agent);

            // b. If relativeEnd = -∞, let endIndex be 0.
            const end_index = if (std.math.isNegativeInf(relative_end)) blk: {
                break :blk 0;
            }
            // c. Else if relativeEnd < 0, let endIndex be max(srcLength + relativeEnd, 0).
            else if (relative_end < 0) blk: {
                break :blk @max(src_length + relative_end, 0);
            }
            // d. Else, let endIndex be min(relativeEnd, srcLength).
            else blk: {
                break :blk @min(relative_end, src_length);
            };

            // e. Let newLength be max(endIndex - startIndex, 0).
            const new_length = @max(end_index - start_index_f64, 0);

            // f. Let argumentsList be « buffer, 𝔽(beginByteOffset), 𝔽(newLength) ».
            break :blk_args &.{
                Value.from(buffer_.object()),
                Value.from(begin_byte_offset),
                Value.from(new_length),
            };
        };

        // 17. Return ? TypedArraySpeciesCreate(O, argumentsList).
        return Value.from(try typedArraySpeciesCreate(agent, typed_array, arguments_list));
    }

    /// 23.2.3.31 %TypedArray%.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let array be ? ToObject(this value).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const array = this_value.object;

        // 2. Let len be ? LengthOfArrayLike(array).
        const len = typedArrayLength(ta);

        // 3. Let separator be the implementation-defined list-separator String value appropriate
        //    for the host environment's current locale (such as ", ").
        const separator = String.fromLiteral(", ");

        // 4. Let R be the empty String.
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var result = String.Builder.init(agent.gc_allocator);
        defer result.deinit();
        try result.segments.ensureTotalCapacity(@intCast(len));

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. If k > 0, set R to the string-concatenation of R and separator.
            if (k > 0) try result.appendString(separator);

            // b. Let element be ? Get(array, ! ToString(𝔽(k))).
            const element = array.get(PropertyKey.from(k)) catch |err| try noexcept(err);

            // c. If element is neither undefined nor null, then
            if (element != .undefined and element != .null) {
                // i. Let S be ? ToString(? Invoke(element, "toLocaleString")).
                const string = try (try element.invokeNoArgs(
                    agent,
                    PropertyKey.from("toLocaleString"),
                )).toString(agent);

                // ii. Set R to the string-concatenation of R and S.
                try result.appendString(string);
            }

            // d. Set k to k + 1.
        }

        // 7. Return R.
        return Value.from(try result.build());
    }

    /// 23.2.3.32 %TypedArray%.prototype.toReversed ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.toreversed
    fn toReversed(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const typed_array = ta.object;

        // 3. Let length be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let A be ? TypedArrayCreateSameType(O, « 𝔽(length) »).
        const new_typed_array = try typedArrayCreateSameType(
            agent,
            typed_array,
            &.{Value.from(len)},
        );

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat, while k < length,
        while (k < len) : (k += 1) {
            // a. Let from be ! ToString(𝔽(length - k - 1)).
            const from = PropertyKey.from(len - k - 1);

            // b. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // c. Let fromValue be ! Get(O, from).
            const from_value = @constCast(typed_array).object().get(from) catch |err| try noexcept(err);

            // d. Perform ! Set(A, Pk, fromValue, true).
            new_typed_array.set(property_key, from_value, .throw) catch |err| try noexcept(err);

            // e. Set k to k + 1.
        }

        // 7. Return A.
        return Value.from(new_typed_array);
    }

    /// 23.2.3.33 %TypedArray%.prototype.toSorted ( comparefn )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.tosorted
    fn toSorted(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const compare_fn = arguments.get(0);

        // 1. If comparefn is not undefined and IsCallable(comparefn) is false, throw a TypeError
        //    exception.
        if (compare_fn != .undefined and !compare_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{compare_fn});
        }

        // 2. Let O be the this value.
        // 3. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const typed_array = ta.object;
        const object = this_value.object;

        // 4. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 5. Let A be ? TypedArrayCreateSameType(O, « 𝔽(len) »).
        const new_typed_array = try typedArrayCreateSameType(
            agent,
            typed_array,
            &.{Value.from(len)},
        );

        // 6. NOTE: The following closure performs a numeric comparison rather than the string
        //    comparison used in 23.1.3.34.
        // 7. Let SortCompare be a new Abstract Closure with parameters (x, y) that captures
        //    comparefn and performs the following steps when called:
        const sortCompare = struct {
            fn func(agent_: *Agent, x: Value, y: Value, compare_fn_: ?Object) Agent.Error!std.math.Order {
                // a. Return ? CompareTypedArrayElements(x, y, comparefn).
                return compareTypedArrayElements(agent_, x, y, compare_fn_);
            }
        }.func;

        // 8. Let sortedList be ? SortIndexedProperties(O, len, SortCompare, read-through-holes).
        const sorted_list = try sortIndexedProperties(
            agent,
            object,
            len,
            .{
                .impl = sortCompare,
                .compare_fn = if (compare_fn != .undefined) compare_fn.object else null,
            },
            .read_through_holes,
        );

        // 9. Let j be 0.
        var j: u53 = 0;

        // 10. Repeat, while j < len,
        while (j < len) : (j += 1) {
            // a. Perform ! Set(A, ! ToString(𝔽(j)), sortedList[j], true).
            new_typed_array.set(
                PropertyKey.from(j),
                sorted_list[@intCast(j)],
                .throw,
            ) catch |err| try noexcept(err);

            // b. Set j to j + 1.
        }

        // 11. Return A.
        return Value.from(new_typed_array);
    }

    /// 23.2.3.35 %TypedArray%.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.values
    fn values(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? ValidateTypedArray(O, seq-cst).
        const object = @constCast(
            (try validateTypedArray(agent, this_value, .seq_cst)).object,
        ).object();

        // 3. Return CreateArrayIterator(O, value).
        return Value.from(try createArrayIterator(agent, object, .value));
    }

    /// 23.2.3.36 %TypedArray%.prototype.with ( index, value )
    /// https://tc39.es/ecma262/#sec-%typedarray%.prototype.with
    fn with(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const index = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Let taRecord be ? ValidateTypedArray(O, seq-cst).
        const ta = try validateTypedArray(agent, this_value, .seq_cst);
        const typed_array = ta.object;

        // 3. Let len be TypedArrayLength(taRecord).
        const len = typedArrayLength(ta);

        // 4. Let relativeIndex be ? ToIntegerOrInfinity(index).
        const relative_index = try index.toIntegerOrInfinity(agent);

        // 5. If relativeIndex ≥ 0, let actualIndex be relativeIndex.
        // 6. Else, let actualIndex be len + relativeIndex.
        const actual_index_f64 = if (relative_index >= 0)
            relative_index
        else
            @as(f64, @floatFromInt(len)) + relative_index;

        // 7. If O.[[ContentType]] is bigint, let numericValue be ? ToBigInt(value).
        // 8. Else, let numericValue be ? ToNumber(value).
        const numeric_value = if (typed_array.fields.content_type == .bigint)
            Value.from(try value.toBigInt(agent))
        else
            Value.from(try value.toNumber(agent));

        // 9. If IsValidIntegerIndex(O, 𝔽(actualIndex)) is false, throw a RangeError exception.
        if (!isValidIntegerIndex(typed_array, actual_index_f64)) {
            return agent.throwException(
                .range_error,
                "Invalid index {d} for typed array of length {}",
                .{ actual_index_f64, len },
            );
        }
        const actual_index: u53 = @intFromFloat(actual_index_f64);

        // 10. Let A be ? TypedArrayCreateSameType(O, « 𝔽(len) »).
        const new_typed_array = try typedArrayCreateSameType(
            agent,
            typed_array,
            &.{Value.from(len)},
        );

        // 11. Let k be 0.
        var k: u53 = 0;

        // 12. Repeat, while k < len,
        while (k < len) : (k += 1) {
            // a. Let Pk be ! ToString(𝔽(k)).
            const property_key = PropertyKey.from(k);

            // b. If k = actualIndex, let fromValue be numericValue.
            // c. Else, let fromValue be ! Get(O, Pk).
            const from_value = if (k == actual_index)
                numeric_value
            else
                @constCast(typed_array).object().get(property_key) catch |err| try noexcept(err);

            // d. Perform ! Set(A, Pk, fromValue, true).
            new_typed_array.set(property_key, from_value, .throw) catch |err| try noexcept(err);

            // e. Set k to k + 1.
        }

        // 13. Return A.
        return Value.from(new_typed_array);
    }

    /// 23.2.3.38 get %TypedArray%.prototype [ @@toStringTag ]
    /// https://tc39.es/ecma262/#sec-get-%typedarray%.prototype-@@tostringtag
    fn @"@@toStringTag"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
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
        return Value.from(String.fromAscii(name));
    }
};

/// 23.2.4.1 TypedArraySpeciesCreate ( exemplar, argumentList )
/// https://tc39.es/ecma262/#typedarray-species-create
fn typedArraySpeciesCreate(
    agent: *Agent,
    exemplar: *const TypedArray,
    argument_list: []const Value,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let defaultConstructor be the intrinsic object associated with the constructor name
    //    exemplar.[[TypedArrayName]] in Table 71.
    const default_constructor = inline for (typed_array_element_types) |entry| {
        const name, _ = entry;
        if (std.mem.eql(u8, exemplar.fields.typed_array_name, name)) {
            break try @field(Realm.Intrinsics, "%" ++ name ++ "%")(&realm.intrinsics);
        }
    } else unreachable;

    // 2. Let constructor be ? SpeciesConstructor(exemplar, defaultConstructor).
    const constructor = try @constCast(exemplar).object().speciesConstructor(default_constructor);

    // 3. Let result be ? TypedArrayCreateFromConstructor(constructor, argumentList).
    const result = try typedArrayCreateFromConstructor(agent, constructor, argument_list);

    // 4. Assert: result has [[TypedArrayName]] and [[ContentType]] internal slots.
    std.debug.assert(result.is(TypedArray));

    // 5. If result.[[ContentType]] is not exemplar.[[ContentType]], throw a TypeError exception.
    if (result.as(TypedArray).fields.content_type != exemplar.fields.content_type) {
        return agent.throwException(
            .type_error,
            "Cannot convert between BigInt and Number typed arrays",
            .{},
        );
    }

    // 6. Return result.
    return result;
}

/// 23.2.4.2 TypedArrayCreateFromConstructor ( constructor, argumentList )
/// https://tc39.es/ecma262/#sec-typedarraycreatefromconstructor
fn typedArrayCreateFromConstructor(
    agent: *Agent,
    constructor: Object,
    argument_list: []const Value,
) Agent.Error!Object {
    // 1. Let newTypedArray be ? Construct(constructor, argumentList).
    const new_typed_array = try constructor.construct(argument_list, null);

    // 2. Let taRecord be ? ValidateTypedArray(newTypedArray, seq-cst).
    const ta = try validateTypedArray(agent, Value.from(new_typed_array), .seq_cst);

    // 3. If the number of elements in argumentList is 1 and argumentList[0] is a Number, then
    if (argument_list.len == 1 and argument_list[0] == .number) {
        // a. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
        if (isTypedArrayOutOfBounds(ta)) {
            return agent.throwException(.type_error, "Typed array is out of bounds", .{});
        }

        // b. Let length be TypedArrayLength(taRecord).
        const length = typedArrayLength(ta);

        // c. If length < ℝ(argumentList[0]), throw a TypeError exception.
        if (@as(f64, @floatFromInt(length)) < argument_list[0].number.asFloat()) {
            return agent.throwException(
                .type_error,
                "Typed array must have at least length {}, got {}",
                .{ length, argument_list[0] },
            );
        }
    }

    // 4. Return newTypedArray.
    return new_typed_array;
}

/// 23.2.4.3 TypedArrayCreateSameType ( exemplar, argumentList )
/// https://tc39.es/ecma262/#sec-typedarray-create-same-type
fn typedArrayCreateSameType(
    agent: *Agent,
    exemplar: *const TypedArray,
    argument_list: []const Value,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let constructor be the intrinsic object associated with the constructor name
    //    exemplar.[[TypedArrayName]] in Table 71.
    const constructor = inline for (typed_array_element_types) |entry| {
        const name, _ = entry;
        if (std.mem.eql(u8, exemplar.fields.typed_array_name, name)) {
            break try @field(Realm.Intrinsics, "%" ++ name ++ "%")(&realm.intrinsics);
        }
    } else unreachable;

    // 2. Let result be ? TypedArrayCreateFromConstructor(constructor, argumentList).
    const result = try typedArrayCreateFromConstructor(agent, constructor, argument_list);

    // 3. Assert: result has [[TypedArrayName]] and [[ContentType]] internal slots.
    std.debug.assert(result.is(TypedArray));

    // 4. Assert: result.[[ContentType]] is exemplar.[[ContentType]].
    std.debug.assert(result.as(TypedArray).fields.content_type == exemplar.fields.content_type);

    // 5. Return result.
    return result;
}

/// 23.2.4.4 ValidateTypedArray ( O, order )
/// https://tc39.es/ecma262/#sec-validatetypedarray
pub fn validateTypedArray(
    agent: *Agent,
    object: Value,
    order: Order,
) error{ExceptionThrown}!TypedArrayWithBufferWitness {
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
pub fn typedArrayElementSize(typed_array: *const TypedArray) u53 {
    // 1. Return the Element Size value specified in Table 70 for O.[[TypedArrayName]].
    // FIXME: Would be nice to completely do this at comptime based on the struct type, but once
    //        they are unique (for type comparison) we cannot cast to the generic TypedArray anymore.
    return @intCast(std.StaticStringMap(u4).initComptime(.{
        .{ "Int8Array", 1 },
        .{ "Uint8Array", 1 },
        .{ "Uint8ClampedArray", 1 },
        .{ "Int16Array", 2 },
        .{ "Uint16Array", 2 },
        .{ "Int32Array", 4 },
        .{ "Uint32Array", 4 },
        .{ "BigInt64Array", 8 },
        .{ "BigUint64Array", 8 },
        .{ "Float16Array", 2 },
        .{ "Float32Array", 4 },
        .{ "Float64Array", 8 },
    }).get(typed_array.fields.typed_array_name).?);
}

/// 23.2.4.7 CompareTypedArrayElements ( x, y, comparefn )
/// https://tc39.es/ecma262/#sec-comparetypedarrayelements
pub fn compareTypedArrayElements(
    agent: *Agent,
    x: Value,
    y: Value,
    maybe_compare_fn: ?Object,
) Agent.Error!std.math.Order {
    // 1. Assert: x is a Number and y is a Number, or x is a BigInt and y is a BigInt.
    std.debug.assert((x == .number and y == .number) or (x == .big_int and y == .big_int));

    // 2. If comparefn is not undefined, then
    if (maybe_compare_fn) |compare_fn| {
        // a. Let v be ? ToNumber(? Call(comparefn, undefined, « x, y »)).
        const value = try (try Value.from(compare_fn).callAssumeCallable(
            .undefined,
            &.{ x, y },
        )).toNumber(agent);

        // b. If v is NaN, return +0𝔽.
        if (value.isNan()) return .eq;

        // c. Return v.
        return if (value.isZero()) .eq else if (value.asFloat() < 0) .lt else .gt;
    }

    if (x == .number and y == .number) {
        // 3. If x and y are both NaN, return +0𝔽.
        if (x.number.isNan() and y.number.isNan()) return .eq;

        // 4. If x is NaN, return 1𝔽.
        if (x.number.isNan()) return .gt;

        // 5. If y is NaN, return -1𝔽.
        if (y.number.isNan()) return .lt;

        // 6. If x < y, return -1𝔽.
        if (x.number.lessThan(y.number).?) return .lt;

        // 7. If x > y, return 1𝔽.
        if (y.number.lessThan(x.number).?) return .gt;

        // 8. If x is -0𝔽 and y is +0𝔽, return -1𝔽.
        if (x.number.isNegativeZero() and y.number.isPositiveZero()) return .lt;

        // 9. If x is +0𝔽 and y is -0𝔽, return 1𝔽.
        if (x.number.isPositiveZero() and y.number.isNegativeZero()) return .gt;
    } else {
        // 6-7.
        if (x.big_int.lessThan(y.big_int)) return .lt;
        if (y.big_int.lessThan(x.big_int)) return .gt;
    }

    // 10. Return +0𝔽.
    return .eq;
}

/// 23.2.5.1.1 AllocateTypedArray ( constructorName, newTarget, defaultProto [ , length ] )
/// https://tc39.es/ecma262/#sec-allocatetypedarray
pub fn allocateTypedArray(
    agent: *Agent,
    comptime constructor_name: []const u8,
    new_target: Object,
    comptime default_prototype: []const u8,
    length: ?u53,
) Agent.Error!Object {
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
        break :blk try cloneArrayBuffer(
            agent,
            src_data,
            src_byte_offset,
            byte_length,
        );
    }
    // 12. Else,
    else blk: {
        // a. Let data be ? AllocateArrayBuffer(%ArrayBuffer%, byteLength).
        const data = try allocateArrayBuffer(
            agent,
            try realm.intrinsics.@"%ArrayBuffer%"(),
            byte_length,
            null,
        );

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
            const value = inline for (typed_array_element_types) |entry| {
                const name, const @"type" = entry;
                if (std.mem.eql(u8, src_type, name)) {
                    // i. Let value be GetValueFromBuffer(srcData, srcByteIndex, srcType, true, unordered).
                    const value = getValueFromBuffer(
                        agent,
                        src_data,
                        src_byte_index,
                        @"type",
                        true,
                        .unordered,
                        null,
                    );
                    break if (@"type".isBigIntElementType())
                        Value.from(try BigInt.from(agent.gc_allocator, value))
                    else
                        Value.from(value);
                }
            } else unreachable;

            inline for (typed_array_element_types) |entry| {
                const name, const @"type" = entry;
                if (std.mem.eql(u8, element_type, name)) {
                    // ii. Perform SetValueInBuffer(data, targetByteIndex, elementType, value, true, unordered).
                    try setValueInBuffer(
                        agent,
                        .{ .array_buffer = data.as(builtins.ArrayBuffer) },
                        target_byte_index,
                        @"type",
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
    typed_array.fields.viewed_array_buffer = .{ .array_buffer = data.as(builtins.ArrayBuffer) };

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
    buffer: ArrayBufferLike,
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
fn allocateTypedArrayBuffer(
    agent: *Agent,
    typed_array: *TypedArray,
    length: u53,
) Agent.Error!void {
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
    typed_array.fields.viewed_array_buffer = .{ .array_buffer = data.as(builtins.ArrayBuffer) };

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
        pub fn create(realm: *Realm) Allocator.Error!Object {
            const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
                .length = 3,
                .name = name,
                .realm = realm,
                .prototype = try realm.intrinsics.@"%TypedArray%"(),
            });

            const prototypeFn = @field(Realm.Intrinsics, "%" ++ name ++ ".prototype%");

            // 23.2.6.1 TypedArray.BYTES_PER_ELEMENT
            // https://tc39.es/ecma262/#sec-typedarray.bytes_per_element
            try defineBuiltinProperty(object, "BYTES_PER_ELEMENT", PropertyDescriptor{
                .value = Value.from(comptime for (typed_array_element_types) |entry| {
                    const name_, const @"type" = entry;
                    if (std.mem.eql(u8, name, name_)) break @"type".elementSize();
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
        fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
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
                    else if (first_argument.object.is(builtins.ArrayBuffer) or
                        first_argument.object.is(builtins.SharedArrayBuffer))
                    {
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
                            if (first_argument.object.is(builtins.ArrayBuffer))
                                .{ .array_buffer = first_argument.object.as(builtins.ArrayBuffer) }
                            else
                                .{ .shared_array_buffer = first_argument.object.as(builtins.SharedArrayBuffer) },
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
                                !first_argument.object.is(builtins.ArrayBuffer) and
                                !first_argument.object.is(builtins.SharedArrayBuffer),
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
                            var iterator = try getIteratorFromMethod(
                                agent,
                                first_argument,
                                using_iterator.?,
                            );
                            const values = try iterator.toList();
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
        pub fn create(realm: *Realm) Allocator.Error!Object {
            const object = try builtins.Object.create(realm.agent, .{
                .prototype = try realm.intrinsics.@"%TypedArray.prototype%"(),
            });

            // 23.2.7.1 TypedArray.prototype.BYTES_PER_ELEMENT
            // https://tc39.es/ecma262/#sec-typedarray.prototype.bytes_per_element
            try defineBuiltinProperty(object, "BYTES_PER_ELEMENT", PropertyDescriptor{
                .value = Value.from(comptime for (typed_array_element_types) |entry| {
                    const name_, const @"type" = entry;
                    if (std.mem.eql(u8, name, name_)) break @"type".elementSize();
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
        viewed_array_buffer: ArrayBufferLike,

        /// [[ByteLength]]
        byte_length: ByteLength,

        /// [[ByteOffset]]
        byte_offset: u53,

        /// [[ArrayLength]]
        array_length: ByteLength,
    },
    .tag = .typed_array,
});
