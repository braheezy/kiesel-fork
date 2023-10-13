//! 25.1 ArrayBuffer Objects
//! https://tc39.es/ecma262/#sec-arraybuffer-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const copyDataBlockBytes = types.copyDataBlockBytes;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createByteDataBlock = types.createByteDataBlock;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

/// 25.1.3.1 AllocateArrayBuffer ( constructor, byteLength )
/// https://tc39.es/ecma262/#sec-allocatearraybuffer
pub fn allocateArrayBuffer(agent: *Agent, constructor: Object, byte_length: u64, max_byte_length: ?u53) !Object {
    // 1. Let slots be « [[ArrayBufferData]], [[ArrayBufferByteLength]], [[ArrayBufferDetachKey]] ».

    // 2. If maxByteLength is present and maxByteLength is not empty, let allocatingResizableBuffer
    //    be true; otherwise let allocatingResizableBuffer be false.
    const allocating_resizable_bufffer = max_byte_length != null;

    // 3. If allocatingResizableBuffer is true, then
    if (allocating_resizable_bufffer) {
        // a. If byteLength > maxByteLength, throw a RangeError exception.
        if (byte_length > max_byte_length.?) {
            return agent.throwException(.range_error, "Maximum buffer size exceeded");
        }

        // b. Append [[ArrayBufferMaxByteLength]] to slots.
    }

    // 4. Let obj be ? OrdinaryCreateFromConstructor(constructor, "%ArrayBuffer.prototype%", slots).
    const object = try ordinaryCreateFromConstructor(
        ArrayBuffer,
        agent,
        constructor,
        "%ArrayBuffer.prototype%",
    );

    // 5. Let block be ? CreateByteDataBlock(byteLength).
    const block = try createByteDataBlock(agent, byte_length);

    object.as(ArrayBuffer).fields = .{
        // 6. Set obj.[[ArrayBufferData]] to block.
        // 7. Set obj.[[ArrayBufferByteLength]] to byteLength.
        .array_buffer_data = block,
    };

    // 8. If allocatingResizableBuffer is true, then
    if (allocating_resizable_bufffer) {
        // a. If it is not possible to create a Data Block block consisting of maxByteLength bytes,
        //    throw a RangeError exception.
        // b. NOTE: Resizable ArrayBuffers are designed to be implementable with in-place growth.
        //    Implementations may throw if, for example, virtual memory cannot be reserved up front.

        // c. Set obj.[[ArrayBufferMaxByteLength]] to maxByteLength.
        object.as(ArrayBuffer).fields.array_buffer_max_byte_length = @intCast(max_byte_length.?);
    }

    // 5. Return obj.
    return object;
}

/// 25.1.3.4 IsDetachedBuffer ( arrayBuffer )
/// https://tc39.es/ecma262/#sec-isdetachedbuffer
pub fn isDetachedBuffer(array_buffer: *const ArrayBuffer) bool {
    // 1. If arrayBuffer.[[ArrayBufferData]] is null, return true.
    // 2. Return false.
    return array_buffer.fields.array_buffer_data == null;
}

/// 25.1.3.3 DetachArrayBuffer ( arrayBuffer [ , key ] )
/// https://tc39.es/ecma262/#sec-detacharraybuffer
pub fn detachArrayBuffer(agent: *Agent, array_buffer: *ArrayBuffer, maybe_key: ?Value) !void {
    // TODO: 1. Assert: IsSharedArrayBuffer(arrayBuffer) is false.

    // 2. If key is not present, set key to undefined.
    const key = maybe_key orelse .undefined;

    // 3. If arrayBuffer.[[ArrayBufferDetachKey]] is not key, throw a TypeError exception.
    if (!sameValue(array_buffer.fields.array_buffer_detach_key, key)) {
        return agent.throwException(.type_error, "ArrayBuffer detach key does not match");
    }

    // 4. Set arrayBuffer.[[ArrayBufferData]] to null.
    // 5. Set arrayBuffer.[[ArrayBufferByteLength]] to 0.
    if (array_buffer.fields.array_buffer_data) |*data| {
        data.deinit();
        array_buffer.fields.array_buffer_data = null;
    }

    // 6. Return unused.
}

/// 25.1.3.6 GetArrayBufferMaxByteLengthOption ( options )
/// https://tc39.es/ecma262/#sec-getarraybuffermaxbytelengthoption
pub fn getArrayBufferMaxByteLengthOption(agent: *Agent, options: Value) !?u53 {
    // 1. If options is not an Object, return empty.
    if (options != .object) return null;

    // 2. Let maxByteLength be ? Get(options, "maxByteLength").
    const max_byte_length = try options.object.get(PropertyKey.from("maxByteLength"));

    // 3. If maxByteLength is undefined, return empty.
    if (max_byte_length == .undefined) return null;

    // 4. Return ? ToIndex(maxByteLength).
    return try max_byte_length.toIndex(agent);
}

/// 25.1.3.8 IsFixedLengthArrayBuffer ( arrayBuffer )
/// https://tc39.es/ecma262/#sec-isfixedlengtharraybuffer
pub fn isFixedLengthArrayBuffer(array_buffer: *const ArrayBuffer) bool {
    // 1. If arrayBuffer has an [[ArrayBufferMaxByteLength]] internal slot, return false.
    // 2. Return true.
    return array_buffer.fields.array_buffer_max_byte_length == null;
}

/// 25.1.5 Properties of the ArrayBuffer Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-constructor
pub const ArrayBufferConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "ArrayBuffer",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "isView", isView, 1, realm);

        // 25.1.5.2 ArrayBuffer.prototype
        // https://tc39.es/ecma262/#sec-arraybuffer.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%ArrayBuffer.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 25.1.6.2 ArrayBuffer.prototype.constructor
        // https://tc39.es/ecma262/#sec-arraybuffer.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%ArrayBuffer.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        // 25.1.5.3 get ArrayBuffer [ @@species ]
        // https://tc39.es/ecma262/#sec-get-arraybuffer-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        return object;
    }

    /// 25.1.4.1 ArrayBuffer ( length [ , options ] )
    /// https://tc39.es/ecma262/#sec-arraybuffer-length
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const length = arguments.get(0);
        const options = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "ArrayBuffer must be constructed with 'new'");
        }

        // 2. Let byteLength be ? ToIndex(length).
        const byte_length = try length.toIndex(agent);

        // 3. Let requestedMaxByteLength be ? GetArrayBufferMaxByteLengthOption(options).
        const requested_max_byte_length = try getArrayBufferMaxByteLengthOption(agent, options);

        // 4. Return ? AllocateArrayBuffer(NewTarget, byteLength, requestedMaxByteLength).
        return Value.from(
            try allocateArrayBuffer(
                agent,
                new_target.?,
                @intCast(byte_length),
                requested_max_byte_length,
            ),
        );
    }

    /// 25.1.5.1 ArrayBuffer.isView ( arg )
    /// https://tc39.es/ecma262/#sec-arraybuffer.isview
    fn isView(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const arg = arguments.get(0);

        // 1. If arg is not an Object, return false.
        if (arg != .object) return Value.from(false);

        // TODO: 2. If arg has a [[ViewedArrayBuffer]] internal slot, return true.

        // 3. Return false.
        return Value.from(false);
    }
};

/// 25.1.6 Properties of the ArrayBuffer Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-prototype-object
pub const ArrayBufferPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "maxByteLength", maxByteLength, null, realm);
        try defineBuiltinAccessor(object, "resizable", resizable, null, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);

        // 25.1.6.7 ArrayBuffer.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-arraybuffer.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("ArrayBuffer"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 25.1.6.1 get ArrayBuffer.prototype.byteLength
    /// https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.bytelength
    fn byteLength(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // TODO: 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.

        // 4. If IsDetachedBuffer(O) is true, return +0𝔽.
        if (isDetachedBuffer(object)) return Value.from(0);

        // 5. Let length be O.[[ArrayBufferByteLength]].
        const length = object.fields.array_buffer_data.?.items.len;

        // 6. Return 𝔽(length).
        return Value.from(length);
    }

    /// 25.1.6.3 get ArrayBuffer.prototype.maxByteLength
    /// https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.maxbytelength
    fn maxByteLength(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // TODO: 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.

        // 4. If IsDetachedBuffer(O) is true, return +0𝔽.
        if (isDetachedBuffer(object)) return Value.from(0);

        // 5. If IsFixedLengthArrayBuffer(O) is true, then
        const length = if (isFixedLengthArrayBuffer(object)) blk: {
            // a. Let length be O.[[ArrayBufferByteLength]].
            break :blk object.fields.array_buffer_data.?.items.len;
        }

        // 6. Else,
        else blk: {
            // a. Let length be O.[[ArrayBufferMaxByteLength]].
            break :blk object.fields.array_buffer_max_byte_length.?;
        };

        // 7. Return 𝔽(length).
        return Value.from(length);
    }

    /// 25.1.6.4 get ArrayBuffer.prototype.resizable
    /// https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.resizable
    fn resizable(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // TODO: 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.

        // 4. If IsFixedLengthArrayBuffer(O) is false, return true; otherwise return false.
        return Value.from(!isFixedLengthArrayBuffer(object));
    }

    /// 25.1.6.6 ArrayBuffer.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-arraybuffer.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // TODO: 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.

        // 4. If IsDetachedBuffer(O) is true, throw a TypeError exception.
        if (isDetachedBuffer(object)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached");
        }

        // 5. Let len be O.[[ArrayBufferByteLength]].
        const len = object.fields.array_buffer_data.?.items.len;
        const len_f64: f64 = @floatFromInt(len);

        // 6. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 7. If relativeStart = -∞, let first be 0.
        const first_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        }
        // 8. Else if relativeStart < 0, let first be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 9. Else, let first be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        const first: u53 = @intFromFloat(first_f64);

        // 10. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //     ? ToIntegerOrInfinity(end).
        const relative_end = if (end == .undefined)
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 11. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        }
        // 12. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 13. Else, let final be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };

        // 14. Let newLen be max(final - first, 0).
        const new_len: u53 = @intFromFloat(@max(final_f64 - first_f64, 0));

        // 15. Let ctor be ? SpeciesConstructor(O, %ArrayBuffer%).
        const constructor = try object.object().speciesConstructor(try realm.intrinsics.@"%ArrayBuffer%"());

        // 16. Let new be ? Construct(ctor, « 𝔽(newLen) »).
        const new_object = try constructor.construct(.{Value.from(new_len)}, null);

        // 17. Perform ? RequireInternalSlot(new, [[ArrayBufferData]]).
        const new = try Value.from(new_object).requireInternalSlot(agent, ArrayBuffer);

        // TODO: 18. If IsSharedArrayBuffer(new) is true, throw a TypeError exception.

        // 19. If IsDetachedBuffer(new) is true, throw a TypeError exception.
        if (isDetachedBuffer(new)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached");
        }

        // 20. If SameValue(new, O) is true, throw a TypeError exception.
        if (new.object().sameValue(object.object())) {
            return agent.throwException(.type_error, "Species constructor must return a new object");
        }

        // 21. If new.[[ArrayBufferByteLength]] < newLen, throw a TypeError exception.
        if (new.fields.array_buffer_data.?.items.len < new_len) {
            return agent.throwException(.type_error, "ArrayBuffer is too small");
        }

        // 22. NOTE: Side-effects of the above steps may have detached or resized O.
        // 23. If IsDetachedBuffer(O) is true, throw a TypeError exception.
        if (isDetachedBuffer(object)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached");
        }

        // 24. Let fromBuf be O.[[ArrayBufferData]].
        var from_buf = &object.fields.array_buffer_data.?;

        // 25. Let toBuf be new.[[ArrayBufferData]].
        var to_buf = &new.fields.array_buffer_data.?;

        // 26. Let currentLen be O.[[ArrayBufferByteLength]].
        const current_len: u53 = @intCast(object.fields.array_buffer_data.?.items.len);

        // 27. If first < currentLen, then
        if (first < current_len) {
            // a. Let count be min(newLen, currentLen - first).
            const count = @min(new_len, current_len - first);

            // b. Perform CopyDataBlockBytes(toBuf, 0, fromBuf, first, count).
            copyDataBlockBytes(to_buf, 0, from_buf, first, count);
        }

        // 28. Return new.
        return Value.from(new.object());
    }
};

/// 25.1.7 Properties of ArrayBuffer Instances
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-instances
pub const ArrayBuffer = MakeObject(.{
    .Fields = struct {
        /// [[ArrayBufferData]]
        /// [[ArrayBufferByteLength]]
        array_buffer_data: ?std.ArrayList(u8),

        /// [[ArrayBufferDetachKey]]
        array_buffer_detach_key: Value = .undefined,

        /// [[ArrayBufferMaxByteLength]]
        array_buffer_max_byte_length: ?u53 = null,
    },
    .tag = .array_buffer,
});
