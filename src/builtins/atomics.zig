//! 25.4 The Atomics Object
//! https://tc39.es/ecma262/#sec-atomics-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const BigInt = types.BigInt;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const TypedArrayWithBufferWitness = builtins.TypedArrayWithBufferWitness;
const Value = types.Value;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getValueFromBuffer = builtins.getValueFromBuffer;
const isBigIntElementType = builtins.isBigIntElementType;
const isTypedArrayOutOfBounds = builtins.isTypedArrayOutOfBounds;
const makeTypedArrayWithBufferWitnessRecord = builtins.makeTypedArrayWithBufferWitnessRecord;
const setValueInBuffer = builtins.setValueInBuffer;
const typedArrayElementSize = builtins.typedArrayElementSize;
const typedArrayLength = builtins.typedArrayLength;
const validateTypedArray = builtins.validateTypedArray;

/// 25.4.3.1 ValidateIntegerTypedArray ( typedArray, waitable )
/// https://tc39.es/ecma262/#sec-validateintegertypedarray
fn validateIntegerTypedArray(
    agent: *Agent,
    typed_array_value: Value,
    waitable: bool,
) Agent.Error!TypedArrayWithBufferWitness {
    // 1. Let taRecord be ? ValidateTypedArray(typedArray, unordered).
    // 2. NOTE: Bounds checking is not a synchronizing operation when typedArray's backing buffer
    //    is a growable SharedArrayBuffer.
    const ta = try validateTypedArray(agent, typed_array_value, .unordered);
    const typed_array = ta.object;
    const name = typed_array.fields.typed_array_name;

    // 3. If waitable is true, then
    if (waitable) {
        // a. If typedArray.[[TypedArrayName]] is neither "Int32Array" nor "BigInt64Array", throw a
        //    TypeError exception.
        if (!std.mem.eql(u8, name, "Int32Array") and !std.mem.eql(u8, name, "BigInt64Array")) {
            return agent.throwException(
                .type_error,
                "Only Int32Array and BigInt64Array can be waited on, got {s}",
                .{name},
            );
        }
    }
    // 4. Else,
    else {
        // a. Let type be TypedArrayElementType(typedArray).
        // b. If IsUnclampedIntegerElementType(type) is false and IsBigIntElementType(type) is
        //    false, throw a TypeError exception.
        if (std.mem.eql(u8, name, "Uint8ClampedArray") or
            std.mem.eql(u8, name, "Float32Array") or
            std.mem.eql(u8, name, "Float64Array"))
        {
            return agent.throwException(
                .type_error,
                "Atomic operations are only supported on integer typed arrays",
                .{},
            );
        }
    }

    // 5. Return taRecord.
    return ta;
}

/// 25.4.3.2 ValidateAtomicAccess ( taRecord, requestIndex )
/// https://tc39.es/ecma262/#sec-validateatomicaccess
fn validateAtomicAccess(
    agent: *Agent,
    ta: TypedArrayWithBufferWitness,
    request_index: Value,
) Agent.Error!u53 {
    // 1. Let length be TypedArrayLength(taRecord).
    const length = typedArrayLength(ta);

    // 2. Let accessIndex be ? ToIndex(requestIndex).
    const access_index = try request_index.toIndex(agent);

    // 3. Assert: accessIndex ≥ 0.
    // 4. If accessIndex ≥ length, throw a RangeError exception.
    if (access_index >= length) {
        return agent.throwException(
            .range_error,
            "Invalid index {} for typed array with length {}",
            .{ access_index, length },
        );
    }

    // 5. Let typedArray be taRecord.[[Object]].
    const typed_array = ta.object;

    // 6. Let elementSize be TypedArrayElementSize(typedArray).
    const element_size = typedArrayElementSize(typed_array);

    // 7. Let offset be typedArray.[[ByteOffset]].
    const offset = typed_array.fields.byte_offset;

    // 8. Return (accessIndex × elementSize) + offset.
    return (access_index * element_size) + offset;
}

/// 25.4.3.3 ValidateAtomicAccessOnIntegerTypedArray ( typedArray, requestIndex [ , waitable ] )
/// https://tc39.es/ecma262/#sec-validateatomicaccessonintegertypedarray
fn validateAtomicAccessOnIntegerTypedArray(
    agent: *Agent,
    typed_array: Value,
    request_index: Value,
    waitable: ?bool,
) Agent.Error!u53 {
    // 1. If waitable is not present, set waitable to false.
    // 2. Let taRecord be ? ValidateIntegerTypedArray(typedArray, waitable).
    const ta = try validateIntegerTypedArray(agent, typed_array, waitable orelse false);

    // 3. Return ? ValidateAtomicAccess(taRecord, requestIndex).
    return validateAtomicAccess(agent, ta, request_index);
}

/// 25.4.3.4 RevalidateAtomicAccess ( typedArray, byteIndexInBuffer )
/// https://tc39.es/ecma262/#sec-revalidateatomicaccess
fn revalidateAtomicAccess(
    agent: *Agent,
    typed_array: *const builtins.TypedArray,
    byte_index_in_buffer: u53,
) Agent.Error!void {
    // 1. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(typedArray, unordered).
    // 2. NOTE: Bounds checking is not a synchronizing operation when typedArray's backing buffer
    //    is a growable SharedArrayBuffer.
    const ta = makeTypedArrayWithBufferWitnessRecord(typed_array, .unordered);

    // 3. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
    if (isTypedArrayOutOfBounds(ta)) {
        return agent.throwException(.type_error, "Typed array is out of bounds", .{});
    }

    // 4. Assert: byteIndexInBuffer ≥ typedArray.[[ByteOffset]].
    std.debug.assert(byte_index_in_buffer >= typed_array.fields.byte_offset);

    // 5. If byteIndexInBuffer ≥ taRecord.[[CachedBufferByteLength]], throw a RangeError exception.
    if (byte_index_in_buffer >= ta.cached_buffer_byte_length.value) {
        return agent.throwException(
            .range_error,
            "Invalid index {} for buffer with byte length {}",
            .{ byte_index_in_buffer, ta.cached_buffer_byte_length.value },
        );
    }

    // 6. Return unused.
}

pub const Atomics = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "load", load, 2, realm);
        try defineBuiltinFunction(object, "store", store, 3, realm);

        // 25.4.17 Atomics [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-atomics-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Atomics"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 25.4.9 Atomics.load ( typedArray, index )
    /// https://tc39.es/ecma262/#sec-atomics.load
    fn load(agent: *Agent, _: Value, arguments: ArgumentsList) Agent.Error!Value {
        const typed_array_value = arguments.get(0);
        const index = arguments.get(1);

        // 1. Let byteIndexInBuffer be ? ValidateAtomicAccessOnIntegerTypedArray(typedArray, index).
        const byte_index_in_buffer = try validateAtomicAccessOnIntegerTypedArray(
            agent,
            typed_array_value,
            index,
            null,
        );
        const typed_array = typed_array_value.object.as(builtins.TypedArray);

        // 2. Perform ? RevalidateAtomicAccess(typedArray, byteIndexInBuffer).
        try revalidateAtomicAccess(agent, typed_array, byte_index_in_buffer);

        // 3. Let buffer be typedArray.[[ViewedArrayBuffer]].
        const buffer = typed_array.fields.viewed_array_buffer;

        // 4. Let elementType be TypedArrayElementType(typedArray).
        inline for (builtins.typed_array_element_types) |entry| {
            const name, const T = entry;
            if (std.mem.eql(u8, typed_array.fields.typed_array_name, name)) {
                // 5. Return GetValueFromBuffer(buffer, byteIndexInBuffer, elementType, true, seq-cst).
                const value = getValueFromBuffer(
                    agent,
                    buffer,
                    byte_index_in_buffer,
                    T,
                    true,
                    .seq_cst,
                    null,
                );
                return if (isBigIntElementType(T))
                    Value.from(try BigInt.from(agent.gc_allocator, value))
                else
                    Value.from(value);
            }
        } else unreachable;
    }

    /// 25.4.11 Atomics.store ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.store
    fn store(agent: *Agent, _: Value, arguments: ArgumentsList) Agent.Error!Value {
        const typed_array_value = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let byteIndexInBuffer be ? ValidateAtomicAccessOnIntegerTypedArray(typedArray, index).
        const byte_index_in_buffer = try validateAtomicAccessOnIntegerTypedArray(
            agent,
            typed_array_value,
            index,
            null,
        );
        const typed_array = typed_array_value.object.as(builtins.TypedArray);

        // 2. If typedArray.[[ContentType]] is bigint, let v be ? ToBigInt(value).
        // 3. Otherwise, let v be 𝔽(? ToIntegerOrInfinity(value)).
        const numeric_value = if (typed_array.fields.content_type == .bigint)
            Value.from(try value.toBigInt(agent))
        else
            Value.from(try value.toIntegerOrInfinity(agent));

        // 4. Perform ? RevalidateAtomicAccess(typedArray, byteIndexInBuffer).
        try revalidateAtomicAccess(agent, typed_array, byte_index_in_buffer);

        // 5. Let buffer be typedArray.[[ViewedArrayBuffer]].
        const buffer = typed_array.fields.viewed_array_buffer;

        // 6. Let elementType be TypedArrayElementType(typedArray).
        inline for (builtins.typed_array_element_types) |entry| {
            const name, const T = entry;
            if (std.mem.eql(u8, typed_array.fields.typed_array_name, name)) {
                // 7. Perform SetValueInBuffer(buffer, byteIndexInBuffer, elementType, v, true, seq-cst).
                try setValueInBuffer(
                    agent,
                    buffer,
                    byte_index_in_buffer,
                    T,
                    numeric_value,
                    true,
                    .seq_cst,
                    null,
                );
                break;
            }
        } else unreachable;

        // 8. Return v.
        return numeric_value;
    }
};
