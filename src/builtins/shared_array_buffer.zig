//! 25.2 SharedArrayBuffer Objects
//! https://tc39.es/ecma262/#sec-sharedarraybuffer-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ArrayBufferLike = @import("array_buffer.zig").ArrayBufferLike;
const DataBlock = types.DataBlock;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const arrayBufferByteLength = builtins.arrayBufferByteLength;
const copyDataBlockBytes = types.copyDataBlockBytes;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createSharedByteDataBlock = types.createSharedByteDataBlock;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getArrayBufferMaxByteLengthOption = builtins.getArrayBufferMaxByteLengthOption;
const isFixedLengthArrayBuffer = builtins.isFixedLengthArrayBuffer;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const data_block_max_byte_length = types.data_block_max_byte_length;

/// 25.2.2.1 AllocateSharedArrayBuffer ( constructor, byteLength [ , maxByteLength ] )
/// https://tc39.es/ecma262/#sec-allocatesharedarraybuffer
pub fn allocateSharedArrayBuffer(
    agent: *Agent,
    constructor: Object,
    byte_length: u64,
    max_byte_length: ?u53,
) Agent.Error!Object {
    // 1. Let slots be « [[ArrayBufferData]] ».

    // 2. If maxByteLength is present and maxByteLength is not empty, let allocatingGrowableBuffer
    //    be true; otherwise let allocatingGrowableBuffer be false.
    const allocating_growable_buffer = max_byte_length != null;

    // 3. If allocatingGrowableBuffer is true, then
    if (allocating_growable_buffer) {
        // a. If byteLength > maxByteLength, throw a RangeError exception.
        if (byte_length > max_byte_length.?) {
            return agent.throwException(.range_error, "Maximum buffer size exceeded", .{});
        }

        // NOTE: Checking for a reasonable size below the theoretical limit is non-standard but also
        //       done in other engines (and tested by test262)
        if (max_byte_length.? > data_block_max_byte_length) {
            return agent.throwException(.range_error, "Maximum buffer size exceeded", .{});
        }

        // b. Append [[ArrayBufferByteLengthData]] and [[ArrayBufferMaxByteLength]] to slots.
    }
    // 4. Else,
    //     a. Append [[ArrayBufferByteLength]] to slots.

    // 5. Let obj be ? OrdinaryCreateFromConstructor(constructor, "%SharedArrayBuffer.prototype%", slots).
    const object = try ordinaryCreateFromConstructor(
        SharedArrayBuffer,
        agent,
        constructor,
        "%SharedArrayBuffer.prototype%",
        .{ .array_buffer_data = undefined },
    );

    // 6. If allocatingGrowableBuffer is true, let allocLength be maxByteLength; otherwise let
    //    allocLength be byteLength.
    const alloc_length: u64 = if (allocating_growable_buffer) max_byte_length.? else byte_length;

    // 7. Let block be ? CreateSharedByteDataBlock(allocLength).
    const block = try createSharedByteDataBlock(agent, alloc_length);

    object.as(SharedArrayBuffer).fields = .{
        // 8. Set obj.[[ArrayBufferData]] to block.
        .array_buffer_data = block,
    };

    // 9. If allocatingGrowableBuffer is true, then
    if (allocating_growable_buffer) {
        // a. Assert: byteLength ≤ maxByteLength.
        std.debug.assert(byte_length <= max_byte_length.?);

        // b. Let byteLengthBlock be ? CreateSharedByteDataBlock(8).
        // c. Perform SetValueInBuffer(byteLengthBlock, 0, biguint64, ℤ(byteLength), true, seq-cst).
        // d. Set obj.[[ArrayBufferByteLengthData]] to byteLengthBlock.
        // TODO: This should be an atomic usize on the DataBlock struct, for now we use the
        //       ArrayBuffer's slice length (NOT thread safe!)

        // e. Set obj.[[ArrayBufferMaxByteLength]] to maxByteLength.
        object.as(SharedArrayBuffer).fields.array_buffer_max_byte_length = max_byte_length.?;
    }
    // 10. Else,
    else {
        // a. Set obj.[[ArrayBufferByteLength]] to byteLength.
    }

    // 11. Return obj.
    return object;
}

/// 25.2.4 Properties of the SharedArrayBuffer Constructor
/// https://tc39.es/ecma262/#sec-sharedarraybuffer-constructor
pub const SharedArrayBufferConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 1,
            .name = "SharedArrayBuffer",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinAccessor(object, "%Symbol.species%", @"%Symbol.species%", null, realm);

        // 25.2.4.1 SharedArrayBuffer.prototype
        // https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%SharedArrayBuffer.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 25.2.3.1 SharedArrayBuffer ( length [ , options ] )
    /// https://tc39.es/ecma262/#sec-sharedarraybuffer-length
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const length = arguments.get(0);
        const options = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "SharedArrayBuffer must be constructed with 'new'",
                .{},
            );
        }

        // 2. Let byteLength be ? ToIndex(length).
        const byte_length = try length.toIndex(agent);

        // 3. Let requestedMaxByteLength be ? GetArrayBufferMaxByteLengthOption(options).
        const requested_max_byte_length = try getArrayBufferMaxByteLengthOption(agent, options);

        // 4. Return ? AllocateSharedArrayBuffer(NewTarget, byteLength, requestedMaxByteLength).
        return Value.from(
            try allocateSharedArrayBuffer(
                agent,
                new_target.?,
                @intCast(byte_length),
                requested_max_byte_length,
            ),
        );
    }

    /// 25.2.4.2 get SharedArrayBuffer [ %Symbol.species% ]
    /// https://tc39.es/ecma262/#sec-sharedarraybuffer-%symbol.species%
    fn @"%Symbol.species%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 25.2.5 Properties of the SharedArrayBuffer Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-prototype-object
pub const SharedArrayBufferPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinFunction(object, "grow", grow, 1, realm);
        try defineBuiltinAccessor(object, "growable", growable, null, realm);
        try defineBuiltinAccessor(object, "maxByteLength", maxByteLength, null, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);

        // 25.2.5.2 SharedArrayBuffer.prototype.constructor
        // https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%SharedArrayBuffer%"()),
        );

        // 25.2.5.7 SharedArrayBuffer.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("SharedArrayBuffer"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 25.2.5.1 get SharedArrayBuffer.prototype.byteLength
    /// https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.bytelength
    fn byteLength(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is false, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, SharedArrayBuffer);

        // 4. Let length be ArrayBufferByteLength(O, seq-cst).
        const length = arrayBufferByteLength(
            ArrayBufferLike{ .shared_array_buffer = object },
            .seq_cst,
        );

        // 5. Return 𝔽(length).
        return Value.from(length);
    }

    /// 25.2.5.3 SharedArrayBuffer.prototype.grow ( newLength )
    /// https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype.grow
    fn grow(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const new_length = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferMaxByteLength]]).
        // 3. If IsSharedArrayBuffer(O) is false, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, SharedArrayBuffer);
        if (object.fields.array_buffer_max_byte_length == null) {
            return agent.throwException(.type_error, "SharedArrayBuffer is not growable", .{});
        }

        // 4. Let newByteLength be ? ToIndex(newLength).
        const new_byte_length = try new_length.toIndex(agent);

        // 5. Let hostHandled be ? HostGrowSharedArrayBuffer(O, newByteLength).
        const host_handled = try agent.host_hooks.hostGrowSharedArrayBuffer(object, new_byte_length);

        // 6. If hostHandled is handled, return undefined.
        if (host_handled == .handled) return Value.undefined;

        // 7. Let isLittleEndian be the value of the [[LittleEndian]] field of the surrounding
        //    agent's Agent Record.
        // 8. Let byteLengthBlock be O.[[ArrayBufferByteLengthData]].
        // 9. Let currentByteLengthRawBytes be GetRawBytesFromSharedBlock(byteLengthBlock, 0,
        //    biguint64, true, seq-cst).
        // 10. Let newByteLengthRawBytes be NumericToRawBytes(biguint64, ℤ(newByteLength),
        //     isLittleEndian).
        // 11. Repeat,
        // a. NOTE: This is a compare-and-exchange loop to ensure that parallel, racing grows of
        //    the same buffer are totally ordered, are not lost, and do not silently do nothing.
        //    The loop exits if it was able to attempt to grow uncontended.
        // b. Let currentByteLength be ℝ(RawBytesToNumeric(biguint64, currentByteLengthRawBytes,
        //    isLittleEndian)).
        const current_byte_length = object.fields.array_buffer_data.items.len;

        // c. If newByteLength = currentByteLength, return undefined.
        if (new_byte_length == current_byte_length) return Value.undefined;

        // d. If newByteLength < currentByteLength or newByteLength > O.[[ArrayBufferMaxByteLength]],
        //    throw a RangeError exception.
        if (new_byte_length < current_byte_length) {
            return agent.throwException(.range_error, "Cannot shrink buffer", .{});
        }
        if (new_byte_length > object.fields.array_buffer_max_byte_length.?) {
            return agent.throwException(.range_error, "Maximum buffer size exceeded", .{});
        }

        // e. Let byteLengthDelta be newByteLength - currentByteLength.
        // f. If it is impossible to create a new Shared Data Block value consisting of
        //    byteLengthDelta bytes, throw a RangeError exception.
        // g. NOTE: No new Shared Data Block is constructed and used here. The observable behaviour
        //    of growable SharedArrayBuffers is specified by allocating a max-sized Shared Data
        //    Block at construction time, and this step captures the requirement that
        //    implementations that run out of memory must throw a RangeError.
        // h. Let readByteLengthRawBytes be AtomicCompareExchangeInSharedBlock(byteLengthBlock, 0,
        //    8, currentByteLengthRawBytes, newByteLengthRawBytes).
        // i. If ByteListEqual(readByteLengthRawBytes, currentByteLengthRawBytes) is true, return
        //    undefined.
        // j. Set currentByteLengthRawBytes to readByteLengthRawBytes.
        const result = if (std.math.cast(usize, new_byte_length)) |new_byte_length_casted|
            object.fields.array_buffer_data.resize(new_byte_length_casted)
        else
            error.Overflow;
        result catch return agent.throwException(
            .range_error,
            "Cannot resize buffer to size {}",
            .{new_byte_length},
        );
        @memset(object.fields.array_buffer_data.items[current_byte_length..], 0);
        return Value.undefined;
    }

    /// 25.2.5.4 get SharedArrayBuffer.prototype.growable
    /// https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.growable
    fn growable(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is false, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, SharedArrayBuffer);

        // 4. If IsFixedLengthArrayBuffer(O) is false, return true; otherwise return false.
        return Value.from(!isFixedLengthArrayBuffer(ArrayBufferLike{ .shared_array_buffer = object }));
    }

    /// 25.2.5.5 get SharedArrayBuffer.prototype.maxByteLength
    /// https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.maxbytelength
    fn maxByteLength(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is false, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, SharedArrayBuffer);

        // 4. If IsFixedLengthArrayBuffer(O) is true, then
        const length =
            if (isFixedLengthArrayBuffer(ArrayBufferLike{ .shared_array_buffer = object }))
        blk: {
            // a. Let length be O.[[ArrayBufferByteLength]].
            break :blk object.fields.array_buffer_data.items.len;
        }
        // 5. Else,
        else blk: {
            // a. Let length be O.[[ArrayBufferMaxByteLength]].
            break :blk object.fields.array_buffer_max_byte_length.?;
        };

        // 6. Return 𝔽(length).
        return Value.from(@as(u53, @intCast(length)));
    }

    /// 25.2.5.6 SharedArrayBuffer.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is false, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, SharedArrayBuffer);

        // 4. Let len be ArrayBufferByteLength(O, seq-cst).
        const len = arrayBufferByteLength(
            ArrayBufferLike{ .shared_array_buffer = object },
            .seq_cst,
        );
        const len_f64: f64 = @floatFromInt(len);

        // 5. Let relativeStart be ? ToIntegerOrInfinity(start).
        const relative_start = try start.toIntegerOrInfinity(agent);

        // 6. If relativeStart = -∞, let first be 0.
        const first_f64 = if (std.math.isNegativeInf(relative_start)) blk: {
            break :blk 0;
        }
        // 7. Else if relativeStart < 0, let first be max(len + relativeStart, 0).
        else if (relative_start < 0) blk: {
            break :blk @max(len_f64 + relative_start, 0);
        }
        // 8. Else, let first be min(relativeStart, len).
        else blk: {
            break :blk @min(relative_start, len_f64);
        };
        const first: u53 = @intFromFloat(first_f64);

        // 9. If end is undefined, let relativeEnd be len; else let relativeEnd be
        //    ? ToIntegerOrInfinity(end).
        const relative_end = if (end.isUndefined())
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 10. If relativeEnd = -∞, let final be 0.
        const final_f64 = if (std.math.isNegativeInf(relative_end)) blk: {
            break :blk 0;
        }
        // 11. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        else if (relative_end < 0) blk: {
            break :blk @max(len_f64 + relative_end, 0);
        }
        // 12. Else, let final be min(relativeEnd, len).
        else blk: {
            break :blk @min(relative_end, len_f64);
        };

        // 13. Let newLen be max(final - first, 0).
        const new_len: u53 = @intFromFloat(@max(final_f64 - first_f64, 0));

        // 14. Let ctor be ? SpeciesConstructor(O, %SharedArrayBuffer%).
        const constructor = try object.object().speciesConstructor(
            try realm.intrinsics.@"%SharedArrayBuffer%"(),
        );

        // 15. Let new be ? Construct(ctor, « 𝔽(newLen) »).
        const new_object = try constructor.construct(&.{Value.from(new_len)}, null);

        // 16. Perform ? RequireInternalSlot(new, [[ArrayBufferData]]).
        // 17. If IsSharedArrayBuffer(new) is false, throw a TypeError exception.
        const new = try Value.from(new_object).requireInternalSlot(agent, SharedArrayBuffer);

        // 18. If new.[[ArrayBufferData]] is O.[[ArrayBufferData]], throw a TypeError exception.
        if (new.fields.array_buffer_data.items.ptr == object.fields.array_buffer_data.items.ptr) {
            return agent.throwException(
                .type_error,
                "Species constructor must return a new buffer",
                .{},
            );
        }

        // 19. If ArrayBufferByteLength(new, seq-cst) < newLen, throw a TypeError exception.
        if (arrayBufferByteLength(
            ArrayBufferLike{ .shared_array_buffer = new },
            .seq_cst,
        ) < new_len) {
            return agent.throwException(.type_error, "SharedArrayBuffer is too small", .{});
        }

        // 20. Let fromBuf be O.[[ArrayBufferData]].
        const from_buf = &object.fields.array_buffer_data;

        // 21. Let toBuf be new.[[ArrayBufferData]].
        const to_buf = &new.fields.array_buffer_data;

        // 22. Perform CopyDataBlockBytes(toBuf, 0, fromBuf, first, newLen).
        copyDataBlockBytes(to_buf, 0, from_buf, first, new_len);

        // 23. Return new.
        return Value.from(new.object());
    }
};

/// 25.2.6 Properties of SharedArrayBuffer Instances
/// https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-instances
pub const SharedArrayBuffer = MakeObject(.{
    .Fields = struct {
        /// [[ArrayBufferData]]
        /// [[ArrayBufferByteLength]]
        array_buffer_data: DataBlock,

        // TODO: [[ArrayBufferByteLengthData]]

        /// [[ArrayBufferMaxByteLength]]
        array_buffer_max_byte_length: ?u53 = null,
    },
    .tag = .shared_array_buffer,
});
