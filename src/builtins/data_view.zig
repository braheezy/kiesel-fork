//! 25.3 DataView Objects
//! https://tc39.es/ecma262/#sec-dataview-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ArrayBufferLike = builtins.array_buffer.ArrayBufferLike;
const BigInt = types.BigInt;
const ElementType = builtins.typed_array.ElementType;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Order = builtins.array_buffer.Order;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const arrayBufferByteLength = builtins.arrayBufferByteLength;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getValueFromBuffer = builtins.getValueFromBuffer;
const isDetachedBuffer = builtins.isDetachedBuffer;
const isFixedLengthArrayBuffer = builtins.isFixedLengthArrayBuffer;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const setValueInBuffer = builtins.setValueInBuffer;

/// 25.3.1.1 DataView With Buffer Witness Records
/// https://tc39.es/ecma262/#sec-dataview-with-buffer-witness-records
const DataViewWithBufferWitness = struct {
    pub const CachedBufferByteLength = union(enum) {
        detached,
        value: u53,
    };

    /// [[Object]]
    object: *const DataView,

    // [[CachedBufferByteLength]]
    cached_buffer_byte_length: CachedBufferByteLength,
};

/// 25.3.1.2 MakeDataViewWithBufferWitnessRecord ( obj, order )
/// https://tc39.es/ecma262/#sec-makedataviewwithbufferwitnessrecord
fn makeDataViewWithBufferWitnessRecord(
    object: *const DataView,
    order: Order,
) DataViewWithBufferWitness {
    // 1. Let buffer be obj.[[ViewedArrayBuffer]].
    const buffer = object.fields.viewed_array_buffer;

    // 2. If IsDetachedBuffer(buffer) is true, then
    const byte_length: DataViewWithBufferWitness.CachedBufferByteLength = if (isDetachedBuffer(buffer)) blk: {
        // a. Let byteLength be detached.
        break :blk .detached;
    }
    // 3. Else,
    else blk: {
        // a. Let byteLength be ArrayBufferByteLength(buffer, order).
        break :blk .{ .value = arrayBufferByteLength(buffer, order) };
    };

    // 4. Return the DataView With Buffer Witness Record {
    //      [[Object]]: obj, [[CachedBufferByteLength]]: byteLength
    //    }.
    return .{ .object = object, .cached_buffer_byte_length = byte_length };
}

/// 25.3.1.3 GetViewByteLength ( viewRecord )
/// https://tc39.es/ecma262/#sec-getviewbytelength
fn getViewByteLength(view: DataViewWithBufferWitness) u53 {
    // 1. Assert: IsViewOutOfBounds(viewRecord) is false.
    std.debug.assert(!isViewOutOfBounds(view));

    // 2. Let view be viewRecord.[[Object]].
    const data_view = view.object;

    // 3. If view.[[ByteLength]] is not auto, return view.[[ByteLength]].
    if (data_view.fields.byte_length != .auto) return data_view.fields.byte_length.value;

    // 4. Assert: IsFixedLengthArrayBuffer(view.[[ViewedArrayBuffer]]) is false.
    std.debug.assert(!isFixedLengthArrayBuffer(data_view.fields.viewed_array_buffer));

    // 5. Let byteOffset be view.[[ByteOffset]].
    const byte_offset = data_view.fields.byte_offset;

    // 6. Let byteLength be viewRecord.[[CachedBufferByteLength]].
    const byte_length = view.cached_buffer_byte_length;

    // 7. Assert: byteLength is not detached.
    std.debug.assert(byte_length != .detached);

    // 8. Return byteLength - byteOffset.
    return byte_length.value - byte_offset;
}

/// 25.3.1.4 IsViewOutOfBounds ( viewRecord )
/// https://tc39.es/ecma262/#sec-isviewoutofbounds
fn isViewOutOfBounds(view: DataViewWithBufferWitness) bool {
    // 1. Let view be viewRecord.[[Object]].
    const data_view = view.object;

    // 2. Let bufferByteLength be viewRecord.[[CachedBufferByteLength]].
    const buffer_byte_length = view.cached_buffer_byte_length;

    // 3. Assert: IsDetachedBuffer(view.[[ViewedArrayBuffer]]) is true if and only if
    //    bufferByteLength is detached.
    std.debug.assert(
        isDetachedBuffer(data_view.fields.viewed_array_buffer) == (buffer_byte_length == .detached),
    );

    // 4. If bufferByteLength is detached, return true.
    if (buffer_byte_length == .detached) return true;

    // 5. Let byteOffsetStart be view.[[ByteOffset]].
    const byte_offset_start = data_view.fields.byte_offset;

    // 6. If view.[[ByteLength]] is auto, then
    const byte_offset_end = if (data_view.fields.byte_length == .auto) blk: {
        // a. Let byteOffsetEnd be bufferByteLength.
        break :blk buffer_byte_length.value;
    }
    // 7. Else,
    else blk: {
        // a. Let byteOffsetEnd be byteOffsetStart + view.[[ByteLength]].
        break :blk std.math.add(
            u53,
            byte_offset_start,
            data_view.fields.byte_length.value,
        ) catch return true;
    };

    // 8. If byteOffsetStart > bufferByteLength or byteOffsetEnd > bufferByteLength, return true.
    if (byte_offset_start > buffer_byte_length.value or
        byte_offset_end > buffer_byte_length.value) return true;

    // 9. NOTE: 0-length DataViews are not considered out-of-bounds.
    // 10. Return false.
    return false;
}

/// 25.3.1.5 GetViewValue ( view, requestIndex, isLittleEndian, type )
/// https://tc39.es/ecma262/#sec-getviewvalue
fn getViewValue(
    agent: *Agent,
    view_value: Value,
    request_index: Value,
    is_little_endian_value: Value,
    comptime @"type": ElementType,
) Agent.Error!Value {
    // 1. Perform ? RequireInternalSlot(view, [[DataView]]).
    // 2. Assert: view has a [[ViewedArrayBuffer]] internal slot.
    const data_view = try view_value.requireInternalSlot(agent, DataView);

    // 3. Let getIndex be ? ToIndex(requestIndex).
    const get_index = try request_index.toIndex(agent);

    // 4. Set isLittleEndian to ToBoolean(isLittleEndian).
    const is_little_endian = is_little_endian_value.toBoolean();

    // 5. Let viewOffset be view.[[ByteOffset]].
    const view_offset = data_view.fields.byte_offset;

    // 6. Let viewRecord be MakeDataViewWithBufferWitnessRecord(view, unordered).
    // 7. NOTE: Bounds checking is not a synchronizing operation when view's backing buffer is a
    //    growable SharedArrayBuffer.
    const view = makeDataViewWithBufferWitnessRecord(data_view, .unordered);

    // 8. If IsViewOutOfBounds(viewRecord) is true, throw a TypeError exception.
    if (isViewOutOfBounds(view)) {
        return agent.throwException(.type_error, "DataView is out of bounds", .{});
    }

    // 9. Let viewSize be GetViewByteLength(viewRecord).
    const view_size = getViewByteLength(view);

    // 10. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    // 11. If getIndex + elementSize > viewSize, throw a RangeError exception.
    if (if (std.math.add(u53, get_index, element_size)) |x| x > view_size else |_| true) {
        return agent.throwException(
            .range_error,
            "Cannot get element of size {} at index {}",
            .{ element_size, get_index },
        );
    }

    // 12. Let bufferIndex be getIndex + viewOffset.
    const buffer_index = get_index + view_offset;

    // 13. Return GetValueFromBuffer(view.[[ViewedArrayBuffer]], bufferIndex, type, false,
    //     unordered, isLittleEndian).
    const value = getValueFromBuffer(
        agent,
        data_view.fields.viewed_array_buffer,
        buffer_index,
        @"type",
        false,
        .unordered,
        is_little_endian,
    );
    return if (@"type".isBigIntElementType())
        Value.from(try BigInt.from(agent.gc_allocator, value))
    else
        Value.from(value);
}

/// 25.3.1.6 SetViewValue ( view, requestIndex, isLittleEndian, type, value )
/// https://tc39.es/ecma262/#sec-setviewvalue
fn setViewValue(
    agent: *Agent,
    view_value: Value,
    request_index: Value,
    is_little_endian_value: Value,
    comptime @"type": ElementType,
    value: Value,
) Agent.Error!Value {
    // 1. Perform ? RequireInternalSlot(view, [[DataView]]).
    // 2. Assert: view has a [[ViewedArrayBuffer]] internal slot.
    const data_view = try view_value.requireInternalSlot(agent, DataView);

    // 3. Let getIndex be ? ToIndex(requestIndex).
    const get_index = try request_index.toIndex(agent);

    // 4. If IsBigIntElementType(type) is true, let numberValue be ? ToBigInt(value).
    // 5. Otherwise, let numberValue be ? ToNumber(value).
    const number_value = if (@"type".isBigIntElementType())
        Value.from(try value.toBigInt(agent))
    else
        Value.from(try value.toNumber(agent));

    // 6. Set isLittleEndian to ToBoolean(isLittleEndian).
    const is_little_endian = is_little_endian_value.toBoolean();

    // 7. Let viewOffset be view.[[ByteOffset]].
    const view_offset = data_view.fields.byte_offset;

    // 8. Let viewRecord be MakeDataViewWithBufferWitnessRecord(view, unordered).
    // 9. NOTE: Bounds checking is not a synchronizing operation when view's backing buffer is a
    //    growable SharedArrayBuffer.
    const view = makeDataViewWithBufferWitnessRecord(data_view, .unordered);

    // 10. If IsViewOutOfBounds(viewRecord) is true, throw a TypeError exception.
    if (isViewOutOfBounds(view)) {
        return agent.throwException(.type_error, "DataView is out of bounds", .{});
    }

    // 11. Let viewSize be GetViewByteLength(viewRecord).
    const view_size = getViewByteLength(view);

    // 12. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    // 13. If getIndex + elementSize > viewSize, throw a RangeError exception.
    if (if (std.math.add(u53, get_index, element_size)) |x| x > view_size else |_| true) {
        return agent.throwException(
            .range_error,
            "Cannot set element of size {} at index {}",
            .{ element_size, get_index },
        );
    }

    // 14. Let bufferIndex be getIndex + viewOffset.
    const buffer_index = get_index + view_offset;

    // 15. Perform SetValueInBuffer(view.[[ViewedArrayBuffer]], bufferIndex, type, numberValue,
    //     false, unordered, isLittleEndian).
    try setValueInBuffer(
        agent,
        data_view.fields.viewed_array_buffer,
        buffer_index,
        @"type",
        number_value,
        false,
        .unordered,
        is_little_endian,
    );

    // 16. Return undefined.
    return .undefined;
}

/// 25.3.3 Properties of the DataView Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-dataview-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 1,
            .name = "DataView",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 25.3.3.1 DataView.prototype
        // https://tc39.es/ecma262/#sec-dataview.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%DataView.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] )
    /// https://tc39.es/ecma262/#sec-dataview-buffer-byteoffset-bytelength
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const buffer_value = arguments.get(0);
        const byte_offset = arguments.get(1);
        const byte_length = arguments.get(2);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "ArrayBuffer must be constructed with 'new'",
                .{},
            );
        }

        // 2. Perform ? RequireInternalSlot(buffer, [[ArrayBufferData]]).
        if (!buffer_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{buffer_value});
        }
        if (!buffer_value.asObject().is(builtins.ArrayBuffer) and !buffer_value.asObject().is(builtins.SharedArrayBuffer)) {
            return agent.throwException(.type_error, "{} is not an ArrayBuffer or SharedArrayBuffer object", .{buffer_value});
        }
        const buffer: ArrayBufferLike = if (buffer_value.asObject().is(builtins.ArrayBuffer))
            .{ .array_buffer = buffer_value.asObject().as(builtins.ArrayBuffer) }
        else
            .{ .shared_array_buffer = buffer_value.asObject().as(builtins.SharedArrayBuffer) };

        // 3. Let offset be ? ToIndex(byteOffset).
        const offset = try byte_offset.toIndex(agent);

        // 4. If IsDetachedBuffer(buffer) is true, throw a TypeError exception.
        if (isDetachedBuffer(buffer)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
        }

        // 5. Let bufferByteLength be ArrayBufferByteLength(buffer, seq-cst).
        var buffer_byte_length = arrayBufferByteLength(buffer, .seq_cst);

        // 6. If offset > bufferByteLength, throw a RangeError exception.
        if (offset > buffer_byte_length) {
            return agent.throwException(
                .range_error,
                "DataView start cannot exceed buffer byte length",
                .{},
            );
        }

        // 7. Let bufferIsFixedLength be IsFixedLengthArrayBuffer(buffer).
        const buffer_is_fixed_length = isFixedLengthArrayBuffer(buffer);

        // 8. If byteLength is undefined, then
        const view_byte_length: DataView.Fields.ByteLength = if (byte_length.isUndefined()) blk: {
            // a. If bufferIsFixedLength is true, then
            if (buffer_is_fixed_length) {
                // i. Let viewByteLength be bufferByteLength - offset.
                break :blk .{ .value = buffer_byte_length - offset };
            }
            // b. Else,
            else {
                // i. Let viewByteLength be auto.
                break :blk .auto;
            }
        }
        // 9. Else,
        else blk: {
            // a. Let viewByteLength be ? ToIndex(byteLength).
            const view_byte_length = try byte_length.toIndex(agent);

            // b. If offset + viewByteLength > bufferByteLength, throw a RangeError exception.
            if (if (std.math.add(u53, offset, view_byte_length)) |x|
                x > buffer_byte_length
            else |_|
                true)
            {
                return agent.throwException(
                    .range_error,
                    "DataView end cannot exceed buffer byte length",
                    .{},
                );
            }

            break :blk .{ .value = view_byte_length };
        };

        // 10. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%DataView.prototype%",
        //     « [[DataView]], [[ViewedArrayBuffer]], [[ByteLength]], [[ByteOffset]] »).
        const object = try ordinaryCreateFromConstructor(
            DataView,
            agent,
            new_target.?,
            "%DataView.prototype%",
            .{
                .viewed_array_buffer = undefined,
                .byte_length = undefined,
                .byte_offset = undefined,
            },
        );

        // 11. If IsDetachedBuffer(buffer) is true, throw a TypeError exception.
        if (isDetachedBuffer(buffer)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
        }

        // 12. Set bufferByteLength to ArrayBufferByteLength(buffer, seq-cst).
        buffer_byte_length = arrayBufferByteLength(buffer, .seq_cst);

        // 13. If offset > bufferByteLength, throw a RangeError exception.
        if (offset > buffer_byte_length) {
            return agent.throwException(
                .range_error,
                "DataView start cannot exceed buffer byte length",
                .{},
            );
        }

        // 14. If byteLength is not undefined, then
        if (!byte_length.isUndefined()) {
            // a. If offset + viewByteLength > bufferByteLength, throw a RangeError exception.
            if (if (std.math.add(u53, offset, view_byte_length.value)) |x|
                x > buffer_byte_length
            else |_|
                true)
            {
                return agent.throwException(
                    .range_error,
                    "DataView end cannot exceed buffer byte length",
                    .{},
                );
            }
        }

        // 15. Set O.[[ViewedArrayBuffer]] to buffer.
        object.as(DataView).fields.viewed_array_buffer = buffer;

        // 16. Set O.[[ByteLength]] to viewByteLength.
        object.as(DataView).fields.byte_length = view_byte_length;

        // 17. Set O.[[ByteOffset]] to offset.
        object.as(DataView).fields.byte_offset = offset;

        // 18. Return O.
        return Value.from(object);
    }
};

/// 25.3.4 Properties of the DataView Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-dataview-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinAccessor(object, "buffer", buffer, null, realm);
        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "byteOffset", byteOffset, null, realm);
        try defineBuiltinFunction(object, "getBigInt64", getBigInt64, 1, realm);
        try defineBuiltinFunction(object, "getBigUint64", getBigUint64, 1, realm);
        try defineBuiltinFunction(object, "getFloat16", getFloat16, 1, realm);
        try defineBuiltinFunction(object, "getFloat32", getFloat32, 1, realm);
        try defineBuiltinFunction(object, "getFloat64", getFloat64, 1, realm);
        try defineBuiltinFunction(object, "getInt8", getInt8, 1, realm);
        try defineBuiltinFunction(object, "getInt16", getInt16, 1, realm);
        try defineBuiltinFunction(object, "getInt32", getInt32, 1, realm);
        try defineBuiltinFunction(object, "getUint8", getUint8, 1, realm);
        try defineBuiltinFunction(object, "getUint16", getUint16, 1, realm);
        try defineBuiltinFunction(object, "getUint32", getUint32, 1, realm);
        try defineBuiltinFunction(object, "setBigInt64", setBigInt64, 2, realm);
        try defineBuiltinFunction(object, "setBigUint64", setBigUint64, 2, realm);
        try defineBuiltinFunction(object, "setFloat16", setFloat16, 2, realm);
        try defineBuiltinFunction(object, "setFloat32", setFloat32, 2, realm);
        try defineBuiltinFunction(object, "setFloat64", setFloat64, 2, realm);
        try defineBuiltinFunction(object, "setInt8", setInt8, 2, realm);
        try defineBuiltinFunction(object, "setInt16", setInt16, 2, realm);
        try defineBuiltinFunction(object, "setInt32", setInt32, 2, realm);
        try defineBuiltinFunction(object, "setUint8", setUint8, 2, realm);
        try defineBuiltinFunction(object, "setUint16", setUint16, 2, realm);
        try defineBuiltinFunction(object, "setUint32", setUint32, 2, realm);

        // 25.3.4.4 DataView.prototype.constructor
        // https://tc39.es/ecma262/#sec-dataview.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%DataView%"()),
        );

        // 25.3.4.25 DataView.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-dataview.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("DataView"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 25.3.4.1 get DataView.prototype.buffer
    /// https://tc39.es/ecma262/#sec-get-dataview.prototype.buffer
    fn buffer(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[DataView]]).
        const object = try this_value.requireInternalSlot(agent, DataView);

        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        // 4. Let buffer be O.[[ViewedArrayBuffer]].
        const buffer_ = object.fields.viewed_array_buffer;

        // 5. Return buffer.
        return Value.from(buffer_.object());
    }

    /// 25.3.4.2 get DataView.prototype.byteLength
    /// https://tc39.es/ecma262/#sec-get-dataview.prototype.bytelength
    fn byteLength(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[DataView]]).
        const object = try this_value.requireInternalSlot(agent, DataView);

        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        // 4. Let viewRecord be MakeDataViewWithBufferWitnessRecord(O, seq-cst).
        const view = makeDataViewWithBufferWitnessRecord(object, .seq_cst);

        // 5. If IsViewOutOfBounds(viewRecord) is true, throw a TypeError exception.
        if (isViewOutOfBounds(view)) {
            return agent.throwException(.type_error, "DataView is out of bounds", .{});
        }

        // 6. Let size be GetViewByteLength(viewRecord).
        const size = getViewByteLength(view);

        // 7. Return 𝔽(size).
        return Value.from(size);
    }

    /// 25.3.4.3 get DataView.prototype.byteOffset
    /// https://tc39.es/ecma262/#sec-get-dataview.prototype.byteoffset
    fn byteOffset(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[DataView]]).
        const object = try this_value.requireInternalSlot(agent, DataView);

        // 3. Assert: O has a [[ViewedArrayBuffer]] internal slot.
        // 4. Let viewRecord be MakeDataViewWithBufferWitnessRecord(O, seq-cst).
        const view = makeDataViewWithBufferWitnessRecord(object, .seq_cst);

        // 5. If IsViewOutOfBounds(viewRecord) is true, throw a TypeError exception.
        if (isViewOutOfBounds(view)) {
            return agent.throwException(.type_error, "DataView is out of bounds", .{});
        }

        // 6. Let offset be O.[[ByteOffset]].
        const offset = object.fields.byte_offset;

        // 7. Return 𝔽(offset).
        return Value.from(offset);
    }

    /// 25.3.4.5 DataView.prototype.getBigInt64 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getbigint64
    fn getBigInt64(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.get(1);

        // 1. Let v be the this value.
        // 2. Return ? GetViewValue(v, byteOffset, littleEndian, bigint64).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = i64 });
    }

    /// 25.3.4.6 DataView.prototype.getBigUint64 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getbiguint64
    fn getBigUint64(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.get(1);

        // 1. Let v be the this value.
        // 2. Return ? GetViewValue(v, byteOffset, littleEndian, biguint64).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = u64 });
    }

    /// 7.1 DataView.prototype.getFloat16 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/proposal-float16array/#sec-dataview.prototype.getfloat16
    fn getFloat16(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.get(1);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, Float16).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = f16 });
    }

    /// 25.3.4.7 DataView.prototype.getFloat32 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getfloat32
    fn getFloat32(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.getOrNull(1) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, float32).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = f32 });
    }

    /// 25.3.4.8 DataView.prototype.getFloat64 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getfloat64
    fn getFloat64(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.getOrNull(1) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, float64).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = f64 });
    }

    /// 25.3.4.9 DataView.prototype.getInt8 ( byteOffset )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getint8
    fn getInt8(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);

        // 1. Let v be the this value.
        // 2. Return ? GetViewValue(v, byteOffset, true, int8).
        return getViewValue(agent, this_value, byte_offset, Value.from(true), .{ .T = i8 });
    }

    /// 25.3.4.10 DataView.prototype.getInt16 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getint16
    fn getInt16(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.getOrNull(1) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, int16).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = i16 });
    }

    /// 25.3.4.11 DataView.prototype.getInt32 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getint32
    fn getInt32(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.getOrNull(1) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, int32).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = i32 });
    }

    /// 25.3.4.12 DataView.prototype.getUint8 ( byteOffset )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getuint8
    fn getUint8(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);

        // 1. Let v be the this value.
        // 2. Return ? GetViewValue(v, byteOffset, true, uint8).
        return getViewValue(agent, this_value, byte_offset, Value.from(true), .{ .T = u8 });
    }

    /// 25.3.4.13 DataView.prototype.getUint16 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getuint16
    fn getUint16(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.getOrNull(1) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, uint16).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = u16 });
    }

    /// 25.3.4.14 DataView.prototype.getUint32 ( byteOffset [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.getuint32
    fn getUint32(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const little_endian = arguments.getOrNull(1) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? GetViewValue(v, byteOffset, littleEndian, uint32).
        return getViewValue(agent, this_value, byte_offset, little_endian, .{ .T = u32 });
    }

    /// 25.3.4.15 DataView.prototype.setBigInt64 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setbigint64
    fn setBigInt64(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.get(2);

        // 1. Let v be the this value.
        // 2. Return ? SetViewValue(v, byteOffset, littleEndian, bigint64, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = i64 }, value);
    }

    /// 25.3.4.16 DataView.prototype.setBigUint64 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setbiguint64
    fn setBigUint64(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.get(2);

        // 1. Let v be the this value.
        // 2. Return ? SetViewValue(v, byteOffset, littleEndian, biguint64, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = u64 }, value);
    }

    /// 7.2 DataView.prototype.setFloat16 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/proposal-float16array/#sec-dataview.prototype.setfloat16
    fn setFloat16(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, Float16, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = f16 }, value);
    }

    /// 25.3.4.17 DataView.prototype.setFloat32 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setfloat32
    fn setFloat32(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, float32, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = f32 }, value);
    }

    /// 25.3.4.18 DataView.prototype.setFloat64 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setfloat64
    fn setFloat64(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, float64, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = f64 }, value);
    }

    /// 25.3.4.19 DataView.prototype.setInt8 ( byteOffset, value )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setint8
    fn setInt8(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let v be the this value.
        // 2. Return ? SetViewValue(v, byteOffset, true, int8, value).
        return setViewValue(agent, this_value, byte_offset, Value.from(true), .{ .T = i8 }, value);
    }

    /// 25.3.4.20 DataView.prototype.setInt16 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setint16
    fn setInt16(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, int16, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = i16 }, value);
    }

    /// 25.3.4.21 DataView.prototype.setInt32 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setint32
    fn setInt32(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, int32, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = i32 }, value);
    }

    /// 25.3.4.22 DataView.prototype.setUint8 ( byteOffset, value )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setuint8
    fn setUint8(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let v be the this value.
        // 2. Return ? SetViewValue(v, byteOffset, true, uint8, value).
        return setViewValue(agent, this_value, byte_offset, Value.from(true), .{ .T = u8 }, value);
    }

    /// 25.3.4.23 DataView.prototype.setUint16 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setuint16
    fn setUint16(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, uint16, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = u16 }, value);
    }

    /// 25.3.4.24 DataView.prototype.setUint32 ( byteOffset, value [ , littleEndian ] )
    /// https://tc39.es/ecma262/#sec-dataview.prototype.setuint32
    fn setUint32(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const byte_offset = arguments.get(0);
        const value = arguments.get(1);
        const little_endian = arguments.getOrNull(2) orelse Value.from(false);

        // 1. Let v be the this value.
        // 2. If littleEndian is not present, set littleEndian to false.
        // 3. Return ? SetViewValue(v, byteOffset, littleEndian, uint32, value).
        return setViewValue(agent, this_value, byte_offset, little_endian, .{ .T = u32 }, value);
    }
};

/// 25.3.5 Properties of DataView Instances
/// https://tc39.es/ecma262/#sec-properties-of-dataview-instances
pub const DataView = MakeObject(.{
    .Fields = struct {
        pub const ByteLength = union(enum) {
            auto,
            value: u53,
        };

        /// [[ViewedArrayBuffer]]
        viewed_array_buffer: ArrayBufferLike,

        /// [[ByteLength]]
        byte_length: ByteLength,

        /// [[ByteOffset]]
        byte_offset: u53,
    },
    .tag = .data_view,
});
