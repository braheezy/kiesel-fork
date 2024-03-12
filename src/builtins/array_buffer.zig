//! 25.1 ArrayBuffer Objects
//! https://tc39.es/ecma262/#sec-arraybuffer-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const typed_array = @import("../builtins/typed_array.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const DataBlock = types.DataBlock;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const TypedArrayElementType = typed_array.TypedArrayElementType;
const Value = types.Value;
const copyDataBlockBytes = types.copyDataBlockBytes;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createByteDataBlock = types.createByteDataBlock;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

pub const ArrayBufferLike = union(enum) {
    const Self = @This();

    array_buffer: *const ArrayBuffer,
    shared_array_buffer: *const builtins.SharedArrayBuffer,

    pub inline fn object(self: Self) Object {
        return switch (self) {
            .array_buffer => |array_buffer| @constCast(array_buffer).object(),
            .shared_array_buffer => |shared_array_buffer| @constCast(shared_array_buffer).object(),
        };
    }

    pub inline fn arrayBufferData(self: Self) ?*const DataBlock {
        return switch (self) {
            .array_buffer => |array_buffer| if (array_buffer.fields.array_buffer_data) |array_buffer_data|
                &array_buffer_data
            else
                null,
            .shared_array_buffer => |shared_array_buffer| &shared_array_buffer.fields.array_buffer_data,
        };
    }
};

pub const Order = enum {
    seq_cst,
    unordered,
};

/// 25.1.3.1 AllocateArrayBuffer ( constructor, byteLength )
/// https://tc39.es/ecma262/#sec-allocatearraybuffer
pub fn allocateArrayBuffer(
    agent: *Agent,
    constructor: Object,
    byte_length: u64,
    max_byte_length: ?u53,
) Agent.Error!Object {
    // 1. Let slots be « [[ArrayBufferData]], [[ArrayBufferByteLength]], [[ArrayBufferDetachKey]] ».

    // 2. If maxByteLength is present and maxByteLength is not empty, let allocatingResizableBuffer
    //    be true; otherwise let allocatingResizableBuffer be false.
    const allocating_resizable_buffer = max_byte_length != null;

    // 3. If allocatingResizableBuffer is true, then
    if (allocating_resizable_buffer) {
        // a. If byteLength > maxByteLength, throw a RangeError exception.
        if (byte_length > max_byte_length.?) {
            return agent.throwException(.range_error, "Maximum buffer size exceeded", .{});
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
    if (allocating_resizable_buffer) {
        // a. If it is not possible to create a Data Block block consisting of maxByteLength bytes,
        //    throw a RangeError exception.
        // b. NOTE: Resizable ArrayBuffers are designed to be implementable with in-place growth.
        //    Implementations may throw if, for example, virtual memory cannot be reserved up front.

        // c. Set obj.[[ArrayBufferMaxByteLength]] to maxByteLength.
        object.as(ArrayBuffer).fields.array_buffer_max_byte_length = max_byte_length.?;
    }

    // 5. Return obj.
    return object;
}

/// 25.1.3.2 ArrayBufferByteLength ( arrayBuffer, order )
/// https://tc39.es/ecma262/#sec-arraybufferbytelength
pub fn arrayBufferByteLength(array_buffer: anytype, _: Order) u53 {
    // TODO: 1. If IsSharedArrayBuffer(arrayBuffer) is true and arrayBuffer has an
    //          [[ArrayBufferByteLengthData]] internal slot, then
    // [...]

    // 2. Assert: IsDetachedBuffer(arrayBuffer) is false.
    std.debug.assert(!isDetachedBuffer(array_buffer));

    // 3. Return arrayBuffer.[[ArrayBufferByteLength]].
    if (@TypeOf(array_buffer) == ArrayBufferLike) {
        return @intCast(array_buffer.arrayBufferData().?.items.len);
    } else {
        comptime std.debug.assert(@typeInfo(@TypeOf(array_buffer)).Pointer.child == ArrayBuffer);
        return @intCast(array_buffer.fields.array_buffer_data.?.items.len);
    }
}

/// 25.1.3.3 ArrayBufferCopyAndDetach ( arrayBuffer, newLength, preserveResizability )
pub fn arrayBufferCopyAndDetach(
    agent: *Agent,
    array_buffer_value: Value,
    new_length: Value,
    preserve_resizability: enum { preserve_resizability, fixed_length },
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Perform ? RequireInternalSlot(arrayBuffer, [[ArrayBufferData]]).
    // 2. If IsSharedArrayBuffer(arrayBuffer) is true, throw a TypeError exception.
    const array_buffer = try array_buffer_value.requireInternalSlot(agent, ArrayBuffer);

    const array_buffer_byte_length = if (array_buffer.fields.array_buffer_data) |array_buffer_data|
        array_buffer_data.items.len
    else
        0;

    // 3. If newLength is undefined, then
    const new_byte_length = if (new_length == .undefined) blk: {
        // a. Let newByteLength be arrayBuffer.[[ArrayBufferByteLength]].
        break :blk array_buffer_byte_length;
    }
    // 4. Else,
    else blk: {
        // a. Let newByteLength be ? ToIndex(newLength).
        break :blk try new_length.toIndex(agent);
    };

    // 5. If IsDetachedBuffer(arrayBuffer) is true, throw a TypeError exception.
    if (isDetachedBuffer(array_buffer)) {
        return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
    }

    // 6. If preserveResizability is preserve-resizability and IsFixedLengthArrayBuffer(arrayBuffer)
    //    is false, then
    const new_max_byte_length = if (preserve_resizability == .preserve_resizability and
        !isFixedLengthArrayBuffer(array_buffer))
    blk: {
        // a. Let newMaxByteLength be arrayBuffer.[[ArrayBufferMaxByteLength]].
        break :blk array_buffer.fields.array_buffer_max_byte_length;
    }
    // 7. Else,
    else blk: {
        // a. Let newMaxByteLength be empty.
        break :blk null;
    };

    // 8. If arrayBuffer.[[ArrayBufferDetachKey]] is not undefined, throw a TypeError exception.
    if (array_buffer.fields.array_buffer_detach_key != .undefined) {
        return agent.throwException(.type_error, "ArrayBuffer detach key does not match", .{});
    }

    // 9. Let newBuffer be ? AllocateArrayBuffer(%ArrayBuffer%, newByteLength, newMaxByteLength).
    const new_buffer = try allocateArrayBuffer(
        agent,
        try realm.intrinsics.@"%ArrayBuffer%"(),
        new_byte_length,
        new_max_byte_length,
    );

    // 10. Let copyLength be min(newByteLength, arrayBuffer.[[ArrayBufferByteLength]]).
    const copy_length: u53 = @intCast(@min(new_byte_length, array_buffer_byte_length));

    // 11. Let fromBlock be arrayBuffer.[[ArrayBufferData]].
    const from_block = &array_buffer.fields.array_buffer_data.?;

    // 12. Let toBlock be newBuffer.[[ArrayBufferData]].
    const to_block = &new_buffer.as(ArrayBuffer).fields.array_buffer_data.?;

    // 13. Perform CopyDataBlockBytes(toBlock, 0, fromBlock, 0, copyLength).
    copyDataBlockBytes(to_block, 0, from_block, 0, copy_length);

    // 14. NOTE: Neither creation of the new Data Block nor copying from the old Data Block are
    //     observable. Implementations may implement this method as a zero-copy move or a realloc.
    // TODO: Implement this as mentioned :^)

    // 15. Perform ! DetachArrayBuffer(arrayBuffer).
    detachArrayBuffer(agent, array_buffer, null) catch unreachable;

    // 16. Return newBuffer.
    return new_buffer;
}

/// 25.1.3.4 IsDetachedBuffer ( arrayBuffer )
/// https://tc39.es/ecma262/#sec-isdetachedbuffer
pub fn isDetachedBuffer(array_buffer: anytype) bool {
    // 1. If arrayBuffer.[[ArrayBufferData]] is null, return true.
    // 2. Return false.
    if (@TypeOf(array_buffer) == ArrayBufferLike) {
        return switch (array_buffer) {
            .array_buffer => |object| object.fields.array_buffer_data == null,
            .shared_array_buffer => false,
        };
    } else {
        comptime std.debug.assert(@typeInfo(@TypeOf(array_buffer)).Pointer.child == ArrayBuffer);
        return array_buffer.fields.array_buffer_data == null;
    }
}

/// 25.1.3.5 DetachArrayBuffer ( arrayBuffer [ , key ] )
/// https://tc39.es/ecma262/#sec-detacharraybuffer
pub fn detachArrayBuffer(
    agent: *Agent,
    array_buffer: *ArrayBuffer,
    maybe_key: ?Value,
) error{ExceptionThrown}!void {
    // 1. Assert: IsSharedArrayBuffer(arrayBuffer) is false.

    // 2. If key is not present, set key to undefined.
    const key = maybe_key orelse .undefined;

    // 3. If arrayBuffer.[[ArrayBufferDetachKey]] is not key, throw a TypeError exception.
    if (!sameValue(array_buffer.fields.array_buffer_detach_key, key)) {
        return agent.throwException(.type_error, "ArrayBuffer detach key does not match", .{});
    }

    // 4. Set arrayBuffer.[[ArrayBufferData]] to null.
    // 5. Set arrayBuffer.[[ArrayBufferByteLength]] to 0.
    if (array_buffer.fields.array_buffer_data) |*data| {
        data.deinit();
        array_buffer.fields.array_buffer_data = null;
    }

    // 6. Return unused.
}

/// 25.1.3.6 CloneArrayBuffer ( srcBuffer, srcByteOffset, srcLength )
/// https://tc39.es/ecma262/#sec-clonearraybuffer
pub fn cloneArrayBuffer(
    agent: *Agent,
    src_buffer: ArrayBufferLike,
    src_byte_offset: u53,
    src_length: u53,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Assert: IsDetachedBuffer(srcBuffer) is false.
    std.debug.assert(!isDetachedBuffer(src_buffer));

    // 2. Let targetBuffer be ? AllocateArrayBuffer(%ArrayBuffer%, srcLength).
    const target_buffer = try allocateArrayBuffer(
        agent,
        try realm.intrinsics.@"%ArrayBuffer%"(),
        src_length,
        null,
    );

    // 3. Let srcBlock be srcBuffer.[[ArrayBufferData]].
    const src_block = src_buffer.arrayBufferData().?;

    // 4. Let targetBlock be targetBuffer.[[ArrayBufferData]].
    const target_block = &target_buffer.as(ArrayBuffer).fields.array_buffer_data.?;

    // 5. Perform CopyDataBlockBytes(targetBlock, 0, srcBlock, srcByteOffset, srcLength).
    copyDataBlockBytes(target_block, 0, src_block, src_byte_offset, src_length);

    // 6. Return targetBuffer.
    return target_buffer;
}

/// 25.1.3.7 GetArrayBufferMaxByteLengthOption ( options )
/// https://tc39.es/ecma262/#sec-getarraybuffermaxbytelengthoption
pub fn getArrayBufferMaxByteLengthOption(agent: *Agent, options: Value) Agent.Error!?u53 {
    // 1. If options is not an Object, return empty.
    if (options != .object) return null;

    // 2. Let maxByteLength be ? Get(options, "maxByteLength").
    const max_byte_length = try options.object.get(PropertyKey.from("maxByteLength"));

    // 3. If maxByteLength is undefined, return empty.
    if (max_byte_length == .undefined) return null;

    // 4. Return ? ToIndex(maxByteLength).
    return try max_byte_length.toIndex(agent);
}

/// 25.1.3.9 IsFixedLengthArrayBuffer ( arrayBuffer )
/// https://tc39.es/ecma262/#sec-isfixedlengtharraybuffer
pub fn isFixedLengthArrayBuffer(array_buffer: anytype) bool {
    // 1. If arrayBuffer has an [[ArrayBufferMaxByteLength]] internal slot, return false.
    // 2. Return true.
    if (@TypeOf(array_buffer) == ArrayBufferLike) {
        return switch (array_buffer) {
            .array_buffer => |object| object.fields.array_buffer_max_byte_length == null,
            .shared_array_buffer => |object| object.fields.array_buffer_max_byte_length == null,
        };
    } else {
        comptime std.debug.assert(@typeInfo(@TypeOf(array_buffer)).Pointer.child == ArrayBuffer);
        return array_buffer.fields.array_buffer_max_byte_length == null;
    }
}

/// 25.1.3.14 RawBytesToNumeric ( type, rawBytes, isLittleEndian )
/// https://tc39.es/ecma262/#sec-rawbytestonumeric
pub fn rawBytesToNumeric(
    comptime @"type": TypedArrayElementType,
    raw_bytes: []const u8,
    is_little_endian: bool,
) @"type".T {
    // 1. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    var bytes: [element_size]u8 = undefined;
    @memcpy(&bytes, raw_bytes);

    // 2. If isLittleEndian is false, reverse the order of the elements of rawBytes.
    if (!is_little_endian) std.mem.reverse(u8, &bytes);

    // 3-8.
    return std.mem.bytesToValue(@"type".T, &bytes);
}

/// 25.1.3.15 GetRawBytesFromSharedBlock ( block, byteIndex, type, isTypedArray, order )
/// https://tc39.es/ecma262/#sec-getrawbytesfromsharedblock
pub fn getRawBytesFromSharedBlock(
    block: *const DataBlock,
    byte_index: u53,
    comptime @"type": TypedArrayElementType,
    is_typed_array: bool,
    order: Order,
) [@sizeOf(@"type".T)]u8 {
    // 1. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    // TODO: 2-10.
    _ = is_typed_array;
    _ = order;
    return block.items[@intCast(byte_index)..][0..element_size].*;
}

/// 25.1.3.16 GetValueFromBuffer ( arrayBuffer, byteIndex, type, isTypedArray, order [ , isLittleEndian ] )
/// https://tc39.es/ecma262/#sec-getvaluefrombuffer
pub fn getValueFromBuffer(
    agent: *Agent,
    array_buffer: ArrayBufferLike,
    byte_index: u53,
    comptime @"type": TypedArrayElementType,
    is_typed_array: bool,
    order: Order,
    maybe_is_little_endian: ?bool,
) @"type".T {
    // 1. Assert: IsDetachedBuffer(arrayBuffer) is false.
    std.debug.assert(!isDetachedBuffer(array_buffer));

    // 2. Assert: There are sufficient bytes in arrayBuffer starting at byteIndex to represent a
    //    value of type.
    std.debug.assert(array_buffer.arrayBufferData().?.items.len >= byte_index + @"type".elementSize());

    // 3. Let block be arrayBuffer.[[ArrayBufferData]].
    const block = array_buffer.arrayBufferData().?;

    // 4. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    // 5. If IsSharedArrayBuffer(arrayBuffer) is true, then
    const raw_value = if (array_buffer == .shared_array_buffer) blk: {
        // a. Assert: block is a Shared Data Block.

        // b. Let rawValue be GetRawBytesFromSharedBlock(block, byteIndex, type, isTypedArray,
        //    order).
        break :blk &getRawBytesFromSharedBlock(block, byte_index, @"type", is_typed_array, order);
    }
    // 6. Else,
    else blk: {
        // a. Let rawValue be a List whose elements are bytes from block at indices in the interval
        //    from byteIndex (inclusive) to byteIndex + elementSize (exclusive).
        break :blk block.items[@intCast(byte_index)..@intCast(byte_index + element_size)];
    };

    // 7. Assert: The number of elements in rawValue is elementSize.
    std.debug.assert(raw_value.len == element_size);

    // 8. If isLittleEndian is not present, set isLittleEndian to the value of the [[LittleEndian]]
    //    field of the surrounding agent's Agent Record.
    const is_little_endian = maybe_is_little_endian orelse agent.little_endian;

    // 9. Return RawBytesToNumeric(type, rawValue, isLittleEndian).
    return rawBytesToNumeric(@"type", raw_value, is_little_endian);
}

/// 25.1.3.17 NumericToRawBytes ( type, value, isLittleEndian )
/// https://tc39.es/ecma262/#sec-numerictorawbytes
pub fn numericToRawBytes(
    agent: *Agent,
    comptime @"type": TypedArrayElementType,
    value: Value,
    is_little_endian: bool,
) Allocator.Error![@sizeOf(@"type".T)]u8 {
    // 1. If type is float32, then
    var raw_bytes = if (@"type".T == f32) blk: {
        // a. Let rawBytes be a List whose elements are the 4 bytes that are the result of
        //    converting value to IEEE 754-2019 binary32 format using roundTiesToEven mode. The
        //    bytes are arranged in little endian order. If value is NaN, rawBytes may be set to
        //    any implementation chosen IEEE 754-2019 binary32 format Not-a-Number encoding. An
        //    implementation must always choose the same encoding for each implementation
        //    distinguishable NaN value.
        break :blk std.mem.toBytes(
            if (value.number.isNan())
                std.math.nan(f32)
            else
                @as(f32, @floatCast(value.number.asFloat())),
        );
    }
    // 2. Else if type is float64, then
    else if (@"type".T == f64) blk: {
        // a. Let rawBytes be a List whose elements are the 8 bytes that are the IEEE 754-2019
        //    binary64 format encoding of value. The bytes are arranged in little endian order. If
        //    value is NaN, rawBytes may be set to any implementation chosen IEEE 754-2019 binary64
        //    format Not-a-Number encoding. An implementation must always choose the same encoding
        //    for each implementation distinguishable NaN value.
        break :blk std.mem.toBytes(
            if (value.number.isNan())
                std.math.nan(f64)
            else
                value.number.asFloat(),
        );
    }
    // 3. Else,
    else blk: {
        // a. Let n be the Element Size value specified in Table 71 for Element Type type.

        // b. Let convOp be the abstract operation named in the Conversion Operation column in
        //    Table 71 for Element Type type.
        const convOp = @"type".conversationOperation();

        // c. Let intValue be ℝ(convOp(value)).
        const int_value = convOp(value, agent) catch |err| try noexcept(err);

        // d. If intValue ≥ 0, then
        //     i. Let rawBytes be a List whose elements are the n-byte binary encoding of intValue.
        //        The bytes are ordered in little endian order.
        // e. Else,
        //     i. Let rawBytes be a List whose elements are the n-byte binary two's complement
        //        encoding of intValue. The bytes are ordered in little endian order.
        break :blk std.mem.toBytes(int_value);
    };

    // 4. If isLittleEndian is false, reverse the order of the elements of rawBytes.
    if (!is_little_endian) std.mem.reverse(u8, &raw_bytes);

    // 5. Return rawBytes.
    return raw_bytes;
}

/// 25.1.3.18 SetValueInBuffer ( arrayBuffer, byteIndex, type, value, isTypedArray, order [ , isLittleEndian ] )
/// https://tc39.es/ecma262/#sec-setvalueinbuffer
pub fn setValueInBuffer(
    agent: *Agent,
    array_buffer: ArrayBufferLike,
    byte_index: u53,
    comptime @"type": TypedArrayElementType,
    value: Value,
    is_typed_array: bool,
    order: Order,
    maybe_is_little_endian: ?bool,
) Allocator.Error!void {
    // 1. Assert: IsDetachedBuffer(arrayBuffer) is false.
    std.debug.assert(!isDetachedBuffer(array_buffer));

    // 2. Assert: There are sufficient bytes in arrayBuffer starting at byteIndex to represent a
    //    value of type.
    std.debug.assert(array_buffer.arrayBufferData().?.items.len >= byte_index + @"type".elementSize());

    // 3. Assert: value is a BigInt if IsBigIntElementType(type) is true; otherwise, value is a
    //    Number.
    std.debug.assert(value == if (@"type".isBigIntElementType()) .big_int else .number);

    // 4. Let block be arrayBuffer.[[ArrayBufferData]].
    const block = array_buffer.arrayBufferData().?;

    // 5. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    // 6. If isLittleEndian is not present, set isLittleEndian to the value of the [[LittleEndian]]
    //    field of the surrounding agent's Agent Record.
    const is_little_endian = maybe_is_little_endian orelse agent.little_endian;

    // 7. Let rawBytes be NumericToRawBytes(type, value, isLittleEndian).
    const raw_bytes = try numericToRawBytes(agent, @"type", value, is_little_endian);

    // TODO: 8. If IsSharedArrayBuffer(arrayBuffer) is true, then
    if (false) {
        // [...]
        _ = order;
        _ = is_typed_array;
    }
    // 9. Else,
    else {
        // a. Store the individual bytes of rawBytes into block, starting at block[byteIndex].
        @memcpy(block.items[@intCast(byte_index)..@intCast(byte_index + element_size)], &raw_bytes);
    }

    // 10. Return unused.
}

/// 25.1.3.19 GetModifySetValueInBuffer ( arrayBuffer, byteIndex, type, value, op )
/// https://tc39.es/ecma262/#sec-getmodifysetvalueinbuffer
pub fn getModifySetValueInBuffer(
    agent: *Agent,
    array_buffer: ArrayBufferLike,
    byte_index: u53,
    comptime @"type": TypedArrayElementType,
    value: Value,
    comptime op: std.builtin.AtomicRmwOp,
) Allocator.Error!@"type".T {
    // 1. Assert: IsDetachedBuffer(arrayBuffer) is false.
    std.debug.assert(!isDetachedBuffer(array_buffer));

    // 2. Assert: There are sufficient bytes in arrayBuffer starting at byteIndex to represent a
    //    value of type.
    std.debug.assert(array_buffer.arrayBufferData().?.items.len >= byte_index + @"type".elementSize());

    // 3. Assert: value is a BigInt if IsBigIntElementType(type) is true; otherwise, value is a
    //    Number.
    std.debug.assert(value == if (@"type".isBigIntElementType()) .big_int else .number);

    // 4. Let block be arrayBuffer.[[ArrayBufferData]].
    const block = array_buffer.arrayBufferData().?;

    // 5. Let elementSize be the Element Size value specified in Table 71 for Element Type type.
    const element_size = @"type".elementSize();

    // 6. Let isLittleEndian be the value of the [[LittleEndian]] field of the surrounding agent's
    //    Agent Record.
    const is_little_endian = agent.little_endian;

    // 7. Let rawBytes be NumericToRawBytes(type, value, isLittleEndian).
    const raw_bytes = try numericToRawBytes(agent, @"type", value, is_little_endian);

    var previous: @"type".T = undefined;

    // TODO: 8. If IsSharedArrayBuffer(arrayBuffer) is true, then
    if (false) {
        // a-g.
    }
    // 9. Else,
    else {
        // a. Let rawBytesRead be a List of length elementSize whose elements are the sequence of
        //    elementSize bytes starting with block[byteIndex].
        // b. Let rawBytesModified be op(rawBytesRead, rawBytes).
        // c. Store the individual bytes of rawBytesModified into block, starting at block[byteIndex].
        const ptr = std.mem.bytesAsValue(
            @"type".T,
            block.items[@intCast(byte_index)..@intCast(byte_index + element_size)],
        );
        previous = @atomicRmw(
            @"type".T,
            @as(*@"type".T, @alignCast(ptr)),
            op,
            std.mem.bytesToValue(@"type".T, &raw_bytes),
            .seq_cst,
        );
    }

    // 10. Return RawBytesToNumeric(type, rawBytesRead, isLittleEndian).
    return rawBytesToNumeric(@"type", std.mem.asBytes(&previous), is_little_endian);
}

/// 25.1.5 Properties of the ArrayBuffer Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-constructor
pub const ArrayBufferConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "ArrayBuffer",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "isView", isView, 1, realm);
        try defineBuiltinAccessor(object, "@@species", @"@@species", null, realm);

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

        return object;
    }

    /// 25.1.4.1 ArrayBuffer ( length [ , options ] )
    /// https://tc39.es/ecma262/#sec-arraybuffer-length
    fn behaviour(agent: *Agent, arguments: ArgumentsList, new_target: ?Object) Agent.Error!Value {
        const length = arguments.get(0);
        const options = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "ArrayBuffer must be constructed with 'new'",
                .{},
            );
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
    fn isView(_: *Agent, _: Value, arguments: ArgumentsList) Agent.Error!Value {
        const arg = arguments.get(0);

        // 1. If arg is not an Object, return false.
        if (arg != .object) return Value.from(false);

        // 2. If arg has a [[ViewedArrayBuffer]] internal slot, return true.
        // 3. Return false.
        return Value.from(arg.object.is(builtins.DataView) or arg.object.is(builtins.TypedArray));
    }

    /// 25.1.5.3 get ArrayBuffer [ @@species ]
    /// https://tc39.es/ecma262/#sec-get-arraybuffer-@@species
    fn @"@@species"(_: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 25.1.6 Properties of the ArrayBuffer Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-prototype-object
pub const ArrayBufferPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "detached", detached, null, realm);
        try defineBuiltinAccessor(object, "maxByteLength", maxByteLength, null, realm);
        try defineBuiltinAccessor(object, "resizable", resizable, null, realm);
        try defineBuiltinFunction(object, "resize", resize, 1, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);
        try defineBuiltinFunction(object, "transfer", transfer, 0, realm);
        try defineBuiltinFunction(object, "transferToFixedLength", transferToFixedLength, 0, realm);

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
    fn byteLength(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // 4. If IsDetachedBuffer(O) is true, return +0𝔽.
        if (isDetachedBuffer(object)) return Value.from(0);

        // 5. Let length be O.[[ArrayBufferByteLength]].
        const length = object.fields.array_buffer_data.?.items.len;

        // 6. Return 𝔽(length).
        return Value.from(@as(u53, @intCast(length)));
    }

    /// 25.1.6.3 get ArrayBuffer.prototype.detached
    /// https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.detached
    fn detached(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // 4. Return IsDetachedBuffer(O).
        return Value.from(isDetachedBuffer(object));
    }

    /// 25.1.6.4 get ArrayBuffer.prototype.maxByteLength
    /// https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.maxbytelength
    fn maxByteLength(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

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
        return Value.from(@as(u53, @intCast(length)));
    }

    /// 25.1.6.5 get ArrayBuffer.prototype.resizable
    /// https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.resizable
    fn resizable(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // 4. If IsFixedLengthArrayBuffer(O) is false, return true; otherwise return false.
        return Value.from(!isFixedLengthArrayBuffer(object));
    }

    /// 25.1.6.6 ArrayBuffer.prototype.resize ( newLength )
    /// https://tc39.es/ecma262/#sec-arraybuffer.prototype.resize
    fn resize(agent: *Agent, this_value: Value, arguments: ArgumentsList) Agent.Error!Value {
        const new_length = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferMaxByteLength]]).
        // 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);
        if (object.fields.array_buffer_max_byte_length == null) {
            return agent.throwException(.type_error, "ArrayBuffer is not resizable", .{});
        }

        // 4. Let newByteLength be ? ToIndex(newLength).
        const new_byte_length = try new_length.toIndex(agent);

        // 5. If IsDetachedBuffer(O) is true, throw a TypeError exception.
        if (isDetachedBuffer(object)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
        }

        // 6. If newByteLength > O.[[ArrayBufferMaxByteLength]], throw a RangeError exception.
        if (new_byte_length > object.fields.array_buffer_max_byte_length.?) {
            return agent.throwException(.range_error, "Maximum buffer size exceeded", .{});
        }

        // 7. Let hostHandled be ? HostResizeArrayBuffer(O, newByteLength).
        const host_handled = try agent.host_hooks.hostResizeArrayBuffer(object, new_byte_length);

        // 8. If hostHandled is handled, return undefined.
        if (host_handled == .handled) return .undefined;

        // 9. Let oldBlock be O.[[ArrayBufferData]].
        // 10. Let newBlock be ? CreateByteDataBlock(newByteLength).
        // 11. Let copyLength be min(newByteLength, O.[[ArrayBufferByteLength]]).
        // 12. Perform CopyDataBlockBytes(newBlock, 0, oldBlock, 0, copyLength).
        // 13. NOTE: Neither creation of the new Data Block nor copying from the old Data Block are
        //    observable. Implementations may implement this method as in-place growth or shrinkage.
        // 14. Set O.[[ArrayBufferData]] to newBlock.
        // 15. Set O.[[ArrayBufferByteLength]] to newByteLength.
        const old_byte_length = object.fields.array_buffer_data.?.items.len;
        const result = if (std.math.cast(usize, new_byte_length)) |new_byte_length_casted|
            object.fields.array_buffer_data.?.resize(new_byte_length_casted)
        else
            error.Overflow;
        result catch {
            return agent.throwException(
                .range_error,
                "Cannot resize buffer to size {}",
                .{new_byte_length},
            );
        };
        if (new_byte_length > old_byte_length) {
            @memset(object.fields.array_buffer_data.?.items[old_byte_length..], 0);
        }

        // 16. Return undefined.
        return .undefined;
    }

    /// 25.1.6.7 ArrayBuffer.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-arraybuffer.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: ArgumentsList) Agent.Error!Value {
        const realm = agent.currentRealm();
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is true, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, ArrayBuffer);

        // 4. If IsDetachedBuffer(O) is true, throw a TypeError exception.
        if (isDetachedBuffer(object)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
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
        const new_object = try constructor.construct(&.{Value.from(new_len)}, null);

        // 17. Perform ? RequireInternalSlot(new, [[ArrayBufferData]]).
        // 18. If IsSharedArrayBuffer(new) is true, throw a TypeError exception.
        const new = try Value.from(new_object).requireInternalSlot(agent, ArrayBuffer);

        // 19. If IsDetachedBuffer(new) is true, throw a TypeError exception.
        if (isDetachedBuffer(new)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
        }

        // 20. If SameValue(new, O) is true, throw a TypeError exception.
        if (new.object().sameValue(object.object())) {
            return agent.throwException(
                .type_error,
                "Species constructor must return a new object",
                .{},
            );
        }

        // 21. If new.[[ArrayBufferByteLength]] < newLen, throw a TypeError exception.
        if (new.fields.array_buffer_data.?.items.len < new_len) {
            return agent.throwException(.type_error, "ArrayBuffer is too small", .{});
        }

        // 22. NOTE: Side-effects of the above steps may have detached or resized O.
        // 23. If IsDetachedBuffer(O) is true, throw a TypeError exception.
        if (isDetachedBuffer(object)) {
            return agent.throwException(.type_error, "ArrayBuffer is detached", .{});
        }

        // 24. Let fromBuf be O.[[ArrayBufferData]].
        const from_buf = &object.fields.array_buffer_data.?;

        // 25. Let toBuf be new.[[ArrayBufferData]].
        const to_buf = &new.fields.array_buffer_data.?;

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

    /// 25.1.6.8 ArrayBuffer.prototype.transfer ( [ newLength ] )
    /// https://tc39.es/ecma262/#sec-arraybuffer.prototype.transfer
    fn transfer(agent: *Agent, this_value: Value, arguments: ArgumentsList) Agent.Error!Value {
        const new_length = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Return ? ArrayBufferCopyAndDetach(O, newLength, preserve-resizability).
        return Value.from(
            try arrayBufferCopyAndDetach(agent, this_value, new_length, .preserve_resizability),
        );
    }

    /// 25.1.6.9 ArrayBuffer.prototype.transferToFixedLength ( [ newLength ] )
    /// https://tc39.es/ecma262/#sec-arraybuffer.prototype.transfertofixedlength
    fn transferToFixedLength(agent: *Agent, this_value: Value, arguments: ArgumentsList) Agent.Error!Value {
        const new_length = arguments.get(0);

        // 1. Let O be the this value.
        // 2. Return ? ArrayBufferCopyAndDetach(O, newLength, fixed-length).
        return Value.from(
            try arrayBufferCopyAndDetach(agent, this_value, new_length, .fixed_length),
        );
    }
};

/// 25.1.7 Properties of ArrayBuffer Instances
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-instances
pub const ArrayBuffer = MakeObject(.{
    .Fields = struct {
        /// [[ArrayBufferData]]
        /// [[ArrayBufferByteLength]]
        array_buffer_data: ?DataBlock,

        /// [[ArrayBufferDetachKey]]
        array_buffer_detach_key: Value = .undefined,

        /// [[ArrayBufferMaxByteLength]]
        array_buffer_max_byte_length: ?u53 = null,
    },
    .tag = .array_buffer,
});
