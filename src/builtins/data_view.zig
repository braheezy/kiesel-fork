//! 25.3 DataView Objects
//! https://tc39.es/ecma262/#sec-dataview-objects

const std = @import("std");

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
const Realm = execution.Realm;
const Value = types.Value;
const arrayBufferByteLength = builtins.arrayBufferByteLength;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const isDetachedBuffer = builtins.isDetachedBuffer;
const isFixedLengthArrayBuffer = builtins.isFixedLengthArrayBuffer;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

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

/// 25.3.3 Properties of the DataView Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-dataview-constructor
pub const DataViewConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "DataView",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 25.3.3.1 DataView.prototype
        // https://tc39.es/ecma262/#sec-dataview.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%DataView.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 25.3.4.4 DataView.prototype.constructor
        // https://tc39.es/ecma262/#sec-dataview.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%DataView.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 25.3.2.1 DataView ( buffer [ , byteOffset [ , byteLength ] ] )
    /// https://tc39.es/ecma262/#sec-dataview-buffer-byteoffset-bytelength
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
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
        const buffer = try buffer_value.requireInternalSlot(agent, builtins.ArrayBuffer);

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
        const view_byte_length: DataView.Fields.ByteLength = if (byte_length == .undefined) blk: {
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
        if (byte_length != .undefined) {
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
pub const DataViewPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "buffer", buffer, null, realm);
        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "byteOffset", byteOffset, null, realm);

        // 25.3.4.25 DataView.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-dataview.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("DataView"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 25.3.4.1 get DataView.prototype.buffer
    /// https://tc39.es/ecma262/#sec-get-dataview.prototype.buffer
    fn buffer(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
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
    fn byteLength(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
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
    fn byteOffset(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
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
        viewed_array_buffer: *builtins.ArrayBuffer,

        /// [[ByteLength]]
        byte_length: ByteLength,

        /// [[ByteOffset]]
        byte_offset: u53,
    },
    .tag = .data_view,
});
