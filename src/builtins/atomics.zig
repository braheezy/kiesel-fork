//! 25.4 The Atomics Object
//! https://tc39.es/ecma262/#sec-atomics-object

const builtin = @import("builtin");
const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BigInt = types.BigInt;
const Object = types.Object;
const PromiseCapability = builtins.promise.PromiseCapability;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const TypedArrayWithBufferWitness = builtins.typed_array.TypedArrayWithBufferWitness;
const Value = types.Value;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getModifySetValueInBuffer = builtins.getModifySetValueInBuffer;
const getValueFromBuffer = builtins.getValueFromBuffer;
const isTypedArrayOutOfBounds = builtins.isTypedArrayOutOfBounds;
const makeTypedArrayWithBufferWitnessRecord = builtins.makeTypedArrayWithBufferWitnessRecord;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const numericToRawBytes = builtins.numericToRawBytes;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const rawBytesToNumeric = builtins.rawBytesToNumeric;
const sameValue = types.sameValue;
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
    const @"type" = typed_array.fields.element_type;

    // 3. If waitable is true, then
    if (waitable) {
        // a. If typedArray.[[TypedArrayName]] is neither "Int32Array" nor "BigInt64Array", throw a
        //    TypeError exception.
        if (@"type" != .int32 and @"type" != .bigint64) {
            return agent.throwException(
                .type_error,
                "Only Int32Array and BigInt64Array can be waited on, got {s}",
                .{@"type".typedArrayName()},
            );
        }
    } else {
        // 4. Else,
        // a. Let type be TypedArrayElementType(typedArray).
        // b. If IsUnclampedIntegerElementType(type) is false and IsBigIntElementType(type) is
        //    false, throw a TypeError exception.
        if (!@"type".isUnclampedIntegerElementType() and !@"type".isBigIntElementType()) {
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

/// 25.4.3.3 ValidateAtomicAccessOnIntegerTypedArray ( typedArray, requestIndex )
/// https://tc39.es/ecma262/#sec-validateatomicaccessonintegertypedarray
fn validateAtomicAccessOnIntegerTypedArray(
    agent: *Agent,
    typed_array: Value,
    request_index: Value,
) Agent.Error!u53 {
    // 1. Let taRecord be ? ValidateIntegerTypedArray(typedArray, false).
    const ta = try validateIntegerTypedArray(agent, typed_array, false);

    // 2. Return ? ValidateAtomicAccess(taRecord, requestIndex).
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
    if (byte_index_in_buffer >= @intFromEnum(ta.cached_buffer_byte_length)) {
        return agent.throwException(
            .range_error,
            "Invalid index {} for buffer with byte length {}",
            .{ byte_index_in_buffer, @intFromEnum(ta.cached_buffer_byte_length) },
        );
    }

    // 6. Return unused.
}

/// 25.4.3.14 DoWait ( mode, typedArray, index, value, timeout )
/// https://tc39.es/ecma262/#sec-dowait
fn doWait(
    agent: *Agent,
    mode: enum { sync, @"async" },
    typed_array_value: Value,
    index: Value,
    value: Value,
    timeout: Value,
) Agent.Error!Value {
    const realm = agent.currentRealm();

    // 1. Let taRecord be ? ValidateIntegerTypedArray(typedArray, true).
    const ta = try validateIntegerTypedArray(agent, typed_array_value, true);

    // 2. Let buffer be taRecord.[[Object]].[[ViewedArrayBuffer]].
    const buffer = ta.object.fields.viewed_array_buffer;

    // 3. If IsSharedArrayBuffer(buffer) is false, throw a TypeError exception.
    if (buffer != .shared_array_buffer) {
        return agent.throwException(
            .type_error,
            "TypedArray must be backed by a SharedArrayBuffer",
            .{},
        );
    }

    // 4. Let i be ? ValidateAtomicAccess(taRecord, index).
    const i = try validateAtomicAccess(agent, ta, index);

    const typed_array = typed_array_value.asObject().as(builtins.TypedArray);

    // 5. Let arrayTypeName be typedArray.[[TypedArrayName]].
    // 6. If arrayTypeName is "BigInt64Array", let v be ? ToBigInt64(value).
    // 7. Else, let v be ? ToInt32(value).
    const v = if (typed_array.fields.element_type == .bigint64)
        Value.from(try BigInt.from(agent.gc_allocator, try value.toBigInt64(agent)))
    else
        Value.from(try value.toInt32(agent));

    // 8. Let q be ? ToNumber(timeout).
    const q = try timeout.toNumber(agent);

    // 9. If q is either NaN or +∞𝔽, let t be +∞; else if q is -∞𝔽, let t be 0; else let t be max(ℝ(q), 0).
    const t = if (q.isNan() or q.isPositiveInf())
        std.math.inf(f64)
    else if (q.isNegativeInf())
        0
    else
        @max(q.asFloat(), 0);

    // 10. If mode is sync and AgentCanSuspend() is false, throw a TypeError exception.

    // 11. Let block be buffer.[[ArrayBufferData]].
    const block = &buffer.shared_array_buffer.fields.array_buffer_data;

    // 12. Let offset be typedArray.[[ByteOffset]].
    const offset = typed_array.fields.byte_offset;

    // 13. Let byteIndexInBuffer be (i × 4) + offset.
    const byte_index_in_buffer = (i * 4) + offset;

    // TODO: 14. Let WL be GetWaiterList(block, byteIndexInBuffer).
    _ = block;

    var promise_capability: PromiseCapability = undefined;
    var result_object: *Object = undefined;

    // 15. If mode is sync, then
    if (mode == .sync) {
        // a. Let promiseCapability be blocking.
        // b. Let resultObject be undefined.
    } else {
        // 16. Else,
        // a. Let promiseCapability be ! NewPromiseCapability(%Promise%).
        promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);

        // b. Let resultObject be OrdinaryObjectCreate(%Object.prototype%).
        result_object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );
    }

    // TODO: 17. Perform EnterCriticalSection(WL).

    // 18. Let elementType be TypedArrayElementType(typedArray).
    const w = switch (typed_array.fields.element_type) {
        .uint8_clamped, .float16, .float32, .float64 => unreachable,
        inline else => |@"type"| blk: {
            // 19. Let w be GetValueFromBuffer(buffer, byteIndexInBuffer, elementType, true, seq-cst).
            const w = getValueFromBuffer(
                agent,
                buffer,
                byte_index_in_buffer,
                @"type",
                true,
                .seq_cst,
                null,
            );
            break :blk if (@"type".isBigIntElementType())
                Value.from(try BigInt.from(agent.gc_allocator, w))
            else
                Value.from(w);
        },
    };

    // 20. If v ≠ w, then
    if (!sameValue(v, w)) {
        // TODO: a. Perform LeaveCriticalSection(WL).

        // b. If mode is sync, return "not-equal".
        if (mode == .sync) return Value.from("not-equal");

        // c. Perform ! CreateDataPropertyOrThrow(resultObject, "async", false).
        try result_object.createDataPropertyDirect(
            PropertyKey.from("async"),
            Value.from(false),
        );

        // d. Perform ! CreateDataPropertyOrThrow(resultObject, "value", "not-equal").
        try result_object.createDataPropertyDirect(
            PropertyKey.from("value"),
            Value.from("not-equal"),
        );

        // e. Return resultObject.
        return Value.from(result_object);
    }

    // 21. If t = 0 and mode is async, then
    if (t == 0 and mode == .@"async") {
        // a. NOTE: There is no special handling of synchronous immediate timeouts. Asynchronous
        //    immediate timeouts have special handling in order to fail fast and avoid unnecessary
        //    Promise jobs.

        // TODO: b. Perform LeaveCriticalSection(WL).

        // c. Perform ! CreateDataPropertyOrThrow(resultObject, "async", false).
        try result_object.createDataPropertyDirect(
            PropertyKey.from("async"),
            Value.from(false),
        );

        // d. Perform ! CreateDataPropertyOrThrow(resultObject, "value", "timed-out").
        try result_object.createDataPropertyDirect(
            PropertyKey.from("value"),
            Value.from("timed-out"),
        );

        // e. Return resultObject.
        return Value.from(result_object);
    }

    // TODO: 22-31.
    const waiter = .{ .result = "timed-out" };

    // 32. If mode is sync, return waiterRecord.[[Result]].
    if (mode == .sync) return Value.from(waiter.result);

    // 33. Perform ! CreateDataPropertyOrThrow(resultObject, "async", true).
    try result_object.createDataPropertyDirect(
        PropertyKey.from("async"),
        Value.from(true),
    );

    // 34. Perform ! CreateDataPropertyOrThrow(resultObject, "value", promiseCapability.[[Promise]]).
    try result_object.createDataPropertyDirect(
        PropertyKey.from("value"),
        Value.from(promise_capability.promise),
    );

    // 35. Return resultObject.
    return Value.from(result_object);
}

/// 25.4.3.17 AtomicReadModifyWrite ( typedArray, index, value, op )
/// https://tc39.es/ecma262/#sec-atomicreadmodifywrite
fn atomicReadModifyWrite(
    agent: *Agent,
    typed_array_value: Value,
    index: Value,
    value: Value,
    comptime op: std.builtin.AtomicRmwOp,
) Agent.Error!Value {
    // 1. Let byteIndexInBuffer be ? ValidateAtomicAccessOnIntegerTypedArray(typedArray, index).
    const byte_index_in_buffer = try validateAtomicAccessOnIntegerTypedArray(
        agent,
        typed_array_value,
        index,
    );
    const typed_array = typed_array_value.asObject().as(builtins.TypedArray);

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
    switch (typed_array.fields.element_type) {
        .uint8_clamped, .float16, .float32, .float64 => unreachable,
        inline else => |@"type"| {
            // Bypass 'expected 32-bit integer type or smaller; found 64-bit integer type' for @atomicRmw()
            if (comptime @bitSizeOf(@"type".type()) > builtin.target.ptrBitWidth()) {
                return agent.throwException(
                    .internal_error,
                    "Atomic operation on {s} not supported on this platform",
                    .{@"type".typedArrayName()},
                );
            }
            // 7. Return GetModifySetValueInBuffer(buffer, byteIndexInBuffer, elementType, v, op).
            const modified_value = try getModifySetValueInBuffer(
                agent,
                buffer,
                byte_index_in_buffer,
                @"type",
                numeric_value,
                op,
            );
            return if (@"type".isBigIntElementType())
                Value.from(try BigInt.from(agent.gc_allocator, modified_value))
            else
                Value.from(modified_value);
        },
    }
}

pub const namespace = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "add", add, 3, realm);
        try defineBuiltinFunction(object, "and", @"and", 3, realm);
        try defineBuiltinFunction(object, "compareExchange", compareExchange, 4, realm);
        try defineBuiltinFunction(object, "exchange", exchange, 3, realm);
        try defineBuiltinFunction(object, "isLockFree", isLockFree, 1, realm);
        try defineBuiltinFunction(object, "load", load, 2, realm);
        try defineBuiltinFunction(object, "notify", notify, 3, realm);
        try defineBuiltinFunction(object, "or", @"or", 3, realm);
        try defineBuiltinFunction(object, "pause", pause, 0, realm);
        try defineBuiltinFunction(object, "store", store, 3, realm);
        try defineBuiltinFunction(object, "sub", sub, 3, realm);
        try defineBuiltinFunction(object, "wait", wait, 4, realm);
        try defineBuiltinFunction(object, "waitAsync", waitAsync, 4, realm);
        try defineBuiltinFunction(object, "xor", xor, 3, realm);

        // 25.4.17 Atomics [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-atomics-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Atomics"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 25.4.4 Atomics.add ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.add
    fn add(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let add be a new read-modify-write modification function with parameters (xBytes,
        //    yBytes) that captures typedArray and performs the following steps atomically when
        //    called:
        //     a-i.
        // 2. Return ? AtomicReadModifyWrite(typedArray, index, value, add).
        return atomicReadModifyWrite(agent, typed_array, index, value, .Add);
    }

    /// 25.4.5 Atomics.and ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.and
    fn @"and"(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let and be a new read-modify-write modification function with parameters (xBytes,
        //    yBytes) that captures nothing and performs the following steps atomically when called:
        //     a. Return ByteListBitwiseOp(&, xBytes, yBytes).
        // 2. Return ? AtomicReadModifyWrite(typedArray, index, value, and).
        return atomicReadModifyWrite(agent, typed_array, index, value, .And);
    }

    /// 25.4.6 Atomics.compareExchange ( typedArray, index, expectedValue, replacementValue )
    /// https://tc39.es/ecma262/#sec-atomics.compareexchange
    fn compareExchange(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array_value = arguments.get(0);
        const index = arguments.get(1);
        const expected_value = arguments.get(2);
        const replacement_value = arguments.get(3);

        // 1. Let byteIndexInBuffer be ? ValidateAtomicAccessOnIntegerTypedArray(typedArray, index).
        const byte_index_in_buffer = try validateAtomicAccessOnIntegerTypedArray(
            agent,
            typed_array_value,
            index,
        );
        const typed_array = typed_array_value.asObject().as(builtins.TypedArray);

        // 2. Let buffer be typedArray.[[ViewedArrayBuffer]].
        const buffer = typed_array.fields.viewed_array_buffer;

        // 3. Let block be buffer.[[ArrayBufferData]].
        const block = buffer.arrayBufferData().?;

        // 4. If typedArray.[[ContentType]] is bigint, then
        const expected, const replacement = if (typed_array.fields.content_type == .bigint) .{
            // a. Let expected be ? ToBigInt(expectedValue).
            Value.from(try expected_value.toBigInt(agent)),

            // b. Let replacement be ? ToBigInt(replacementValue).
            Value.from(try replacement_value.toBigInt(agent)),
        } else .{
            // 5. Else,
            // a. Let expected be 𝔽(? ToIntegerOrInfinity(expectedValue)).
            Value.from(try expected_value.toIntegerOrInfinity(agent)),

            // b. Let replacement be 𝔽(? ToIntegerOrInfinity(replacementValue)).
            Value.from(try replacement_value.toIntegerOrInfinity(agent)),
        };

        // 6. Perform ? RevalidateAtomicAccess(typedArray, byteIndexInBuffer).
        try revalidateAtomicAccess(agent, typed_array, byte_index_in_buffer);

        // 7. Let elementType be TypedArrayElementType(typedArray).
        // 8. Let elementSize be TypedArrayElementSize(typedArray).
        // 9. Let isLittleEndian be the value of the [[LittleEndian]] field of the surrounding agent's Agent Record.
        const is_little_endian = agent.little_endian;
        switch (typed_array.fields.element_type) {
            .uint8_clamped, .float16, .float32, .float64 => unreachable,
            inline else => |@"type"| {
                // Bypass 'expected 32-bit integer type or smaller; found 64-bit integer type' for @cmpxchgStrong()
                if (comptime @bitSizeOf(@"type".type()) > builtin.target.ptrBitWidth()) {
                    return agent.throwException(
                        .internal_error,
                        "Atomic operation on {s} not supported on this platform",
                        .{@"type".typedArrayName()},
                    );
                }
                // 10. Let expectedBytes be NumericToRawBytes(elementType, expected, isLittleEndian).
                const expected_bytes = try numericToRawBytes(
                    agent,
                    @"type",
                    expected,
                    is_little_endian,
                );

                // 11. Let replacementBytes be NumericToRawBytes(elementType, replacement, isLittleEndian).
                const replacement_bytes = try numericToRawBytes(
                    agent,
                    @"type",
                    replacement,
                    is_little_endian,
                );

                const raw_bytes_read = block.items[@intCast(byte_index_in_buffer)..@intCast(byte_index_in_buffer + @"type".elementSize())];
                var previous = std.mem.bytesToValue(@"type".type(), raw_bytes_read);

                // 12. If IsSharedArrayBuffer(buffer) is true, then
                if (buffer == .shared_array_buffer) {
                    // a. Let rawBytesRead be AtomicCompareExchangeInSharedBlock(block,
                    //    byteIndexInBuffer, elementSize, expectedBytes, replacementBytes).
                    const ptr = std.mem.bytesAsValue(@"type".type(), raw_bytes_read);
                    _ = @cmpxchgStrong(
                        @"type".type(),
                        @as(*@"type".type(), @alignCast(ptr)),
                        std.mem.bytesToValue(@"type".type(), &expected_bytes),
                        std.mem.bytesToValue(@"type".type(), &replacement_bytes),
                        .seq_cst,
                        .seq_cst,
                    );
                } else {
                    // 13. Else,
                    // a. Let rawBytesRead be a List of length elementSize whose elements are the
                    //    sequence of elementSize bytes starting with block[byteIndexInBuffer].
                    // b. If ByteListEqual(rawBytesRead, expectedBytes) is true, then
                    if (std.mem.eql(u8, raw_bytes_read, &expected_bytes)) {
                        // i. Store the individual bytes of replacementBytes into block, starting
                        //    at block[byteIndexInBuffer].
                        @memcpy(raw_bytes_read, &replacement_bytes);
                    }
                }

                // 14. Return RawBytesToNumeric(elementType, rawBytesRead, isLittleEndian).
                const value = rawBytesToNumeric(
                    @"type",
                    std.mem.asBytes(&previous),
                    is_little_endian,
                );
                return if (@"type".isBigIntElementType())
                    Value.from(try BigInt.from(agent.gc_allocator, value))
                else
                    Value.from(value);
            },
        }
    }

    /// 25.4.7 Atomics.exchange ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.exchange
    fn exchange(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let second be a new read-modify-write modification function with parameters
        //    (oldBytes, newBytes) that captures nothing and performs the following steps
        //    atomically when called:
        //     a. Return newBytes.
        // 2. Return ? AtomicReadModifyWrite(typedArray, index, value, second).
        return atomicReadModifyWrite(agent, typed_array, index, value, .Xchg);
    }

    /// 25.4.8 Atomics.isLockFree ( size )
    /// https://tc39.es/ecma262/#sec-atomics.islockfree
    fn isLockFree(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const size = arguments.get(0);

        // 1. Let n be ? ToIntegerOrInfinity(size).
        const n = try size.toIntegerOrInfinity(agent);

        // NOTE: Everyone but LibJS hardcodes these, so we might as well :^)
        // 2. Let AR be the Agent Record of the surrounding agent.
        // 3. If n = 1, return AR.[[IsLockFree1]].
        // 4. If n = 2, return AR.[[IsLockFree2]].
        // 5. If n = 4, return true.
        // 6. If n = 8, return AR.[[IsLockFree8]].
        if (n == 1 or n == 2 or n == 4 or n == 8) return Value.from(true);

        // 7. Return false.
        return Value.from(false);
    }

    /// 25.4.9 Atomics.load ( typedArray, index )
    /// https://tc39.es/ecma262/#sec-atomics.load
    fn load(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array_value = arguments.get(0);
        const index = arguments.get(1);

        // 1. Let byteIndexInBuffer be ? ValidateAtomicAccessOnIntegerTypedArray(typedArray, index).
        const byte_index_in_buffer = try validateAtomicAccessOnIntegerTypedArray(
            agent,
            typed_array_value,
            index,
        );
        const typed_array = typed_array_value.asObject().as(builtins.TypedArray);

        // 2. Perform ? RevalidateAtomicAccess(typedArray, byteIndexInBuffer).
        try revalidateAtomicAccess(agent, typed_array, byte_index_in_buffer);

        // 3. Let buffer be typedArray.[[ViewedArrayBuffer]].
        const buffer = typed_array.fields.viewed_array_buffer;

        // 4. Let elementType be TypedArrayElementType(typedArray).
        switch (typed_array.fields.element_type) {
            .uint8_clamped, .float16, .float32, .float64 => unreachable,
            inline else => |@"type"| {
                // 5. Return GetValueFromBuffer(buffer, byteIndexInBuffer, elementType, true, seq-cst).
                const value = getValueFromBuffer(
                    agent,
                    buffer,
                    byte_index_in_buffer,
                    @"type",
                    true,
                    .seq_cst,
                    null,
                );
                return if (@"type".isBigIntElementType())
                    Value.from(try BigInt.from(agent.gc_allocator, value))
                else
                    Value.from(value);
            },
        }
    }

    /// 25.4.15 Atomics.notify ( typedArray, index, count )
    /// https://tc39.es/ecma262/#sec-atomics.notify
    fn notify(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array_value = arguments.get(0);
        const index = arguments.get(1);
        const count_value = arguments.get(2);

        // 1. Let taRecord be ? ValidateIntegerTypedArray(typedArray, true).
        const ta = try validateIntegerTypedArray(agent, typed_array_value, true);

        // 2. Let byteIndexInBuffer be ? ValidateAtomicAccess(taRecord, index).
        const byte_index_in_buffer = try validateAtomicAccess(agent, ta, index);
        const typed_array = typed_array_value.asObject().as(builtins.TypedArray);

        // 3. If count is undefined, then
        const count = if (count_value.isUndefined()) blk: {
            // a. Let c be +∞.
            break :blk std.math.inf(f64);
        } else blk: {
            // 4. Else,
            // a. Let intCount be ? ToIntegerOrInfinity(count).
            const int_count = try count_value.toIntegerOrInfinity(agent);

            // b. Let c be max(intCount, 0).
            break :blk @max(int_count, 0);
        };

        // 5. Let buffer be typedArray.[[ViewedArrayBuffer]].
        const buffer = typed_array.fields.viewed_array_buffer;

        // 6. Let block be buffer.[[ArrayBufferData]].
        // 7. If IsSharedArrayBuffer(buffer) is false, return +0𝔽.
        const block = switch (buffer) {
            .array_buffer => return Value.from(0),
            .shared_array_buffer => |shared_array_buffer| &shared_array_buffer.fields.array_buffer_data,
        };

        // TODO: 8. Let WL be GetWaiterList(block, byteIndexInBuffer).
        _ = block;
        _ = byte_index_in_buffer;

        // TODO: 9-13.
        _ = count;
        const n = 0;

        // 14. Return 𝔽(n).
        return Value.from(n);
    }

    /// 25.4.10 Atomics.or ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.or
    fn @"or"(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let or be a new read-modify-write modification function with parameters (xBytes,
        //    yBytes) that captures nothing and performs the following steps atomically when called:
        //     a. Return ByteListBitwiseOp(|, xBytes, yBytes).
        // 2. Return ? AtomicReadModifyWrite(typedArray, index, value, or).
        return atomicReadModifyWrite(agent, typed_array, index, value, .Or);
    }

    /// 1 Atomics.pause ( [ N ] )
    /// https://tc39.es/proposal-atomics-microwait/#Atomics.pause
    fn pause(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const n = arguments.get(0);

        // 1. If N is neither undefined nor an integral Number, throw a TypeError exception.
        if (!n.isUndefined() and !(n.isNumber() and n.asNumber().isIntegral())) {
            return agent.throwException(.type_error, "{} is not an integral number", .{n});
        }

        // 2. If the execution environment of the ECMAScript implementation supports signaling to
        //    the operating system or CPU that the current executing code is in a spin-wait loop,
        //    such as executing a pause CPU instruction, send that signal. When N is not undefined,
        //    it determines the number of times that signal is sent. The number of times the signal
        //    is sent for an integral Number N is less than or equal to the number times it is sent
        //    for N + 1 if both N and N + 1 have the same sign.
        const iterations = if (!n.isUndefined() and n.asNumber().asFloat() >= 1)
            // Use u16 here to avoid freezing for large numbers (like MAX_SAFE_INTEGER).
            std.math.lossyCast(u16, n.asNumber().asFloat())
        else
            1;
        for (0..iterations) |_| {
            std.atomic.spinLoopHint();
        }

        // 3. Return undefined.
        return .undefined;
    }

    /// 25.4.11 Atomics.store ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.store
    fn store(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array_value = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let byteIndexInBuffer be ? ValidateAtomicAccessOnIntegerTypedArray(typedArray, index).
        const byte_index_in_buffer = try validateAtomicAccessOnIntegerTypedArray(
            agent,
            typed_array_value,
            index,
        );
        const typed_array = typed_array_value.asObject().as(builtins.TypedArray);

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
        switch (typed_array.fields.element_type) {
            .uint8_clamped, .float16, .float32, .float64 => unreachable,
            inline else => |@"type"| {
                // 7. Perform SetValueInBuffer(buffer, byteIndexInBuffer, elementType, v, true, seq-cst).
                try setValueInBuffer(
                    agent,
                    buffer,
                    byte_index_in_buffer,
                    @"type",
                    numeric_value,
                    true,
                    .seq_cst,
                    null,
                );
            },
        }

        // 8. Return v.
        return numeric_value;
    }

    /// 25.4.12 Atomics.sub ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.sub
    fn sub(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let subtract be a new read-modify-write modification function with parameters
        //    (xBytes, yBytes) that captures typedArray and performs the following steps atomically
        //    when called:
        //     a-i.
        // 2. Return ? AtomicReadModifyWrite(typedArray, index, value, subtract).
        return atomicReadModifyWrite(agent, typed_array, index, value, .Sub);
    }

    /// 25.4.13 Atomics.wait ( typedArray, index, value, timeout )
    /// https://tc39.es/ecma262/#sec-atomics.wait
    fn wait(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);
        const timeout = arguments.get(3);

        // 1. Return ? DoWait(sync, typedArray, index, value, timeout).
        return doWait(agent, .sync, typed_array, index, value, timeout);
    }

    /// 25.4.14 Atomics.waitAsync ( typedArray, index, value, timeout )
    /// https://tc39.es/ecma262/#sec-atomics.waitasync
    fn waitAsync(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);
        const timeout = arguments.get(3);

        // 1. Return ? DoWait(async, typedArray, index, value, timeout).
        return doWait(agent, .@"async", typed_array, index, value, timeout);
    }

    /// 25.4.16 Atomics.xor ( typedArray, index, value )
    /// https://tc39.es/ecma262/#sec-atomics.xor
    fn xor(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const typed_array = arguments.get(0);
        const index = arguments.get(1);
        const value = arguments.get(2);

        // 1. Let xor be a new read-modify-write modification function with parameters (xBytes,
        //    yBytes) that captures nothing and performs the following steps atomically when called:
        //     a. Return ByteListBitwiseOp(^, xBytes, yBytes).
        // 2. Return ? AtomicReadModifyWrite(typedArray, index, value, xor).
        return atomicReadModifyWrite(agent, typed_array, index, value, .Xor);
    }
};
