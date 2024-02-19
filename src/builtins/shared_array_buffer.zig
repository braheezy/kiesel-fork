//! 25.2 SharedArrayBuffer Objects
//! https://tc39.es/ecma262/#sec-sharedarraybuffer-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const ArrayBufferLike = @import("array_buffer.zig").ArrayBufferLike;
const DataBlock = types.DataBlock;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const arrayBufferByteLength = builtins.arrayBufferByteLength;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createSharedByteDataBlock = types.createSharedByteDataBlock;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getArrayBufferMaxByteLengthOption = builtins.getArrayBufferMaxByteLengthOption;
const isFixedLengthArrayBuffer = builtins.isFixedLengthArrayBuffer;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 25.2.2.1 AllocateSharedArrayBuffer ( constructor, byteLength [ , maxByteLength ] )
/// https://tc39.es/ecma262/#sec-allocatesharedarraybuffer
pub fn allocateSharedArrayBuffer(
    agent: *Agent,
    constructor: Object,
    byte_length: u64,
    max_byte_length: ?u53,
) Agent.Error!Object {
    // Non-standard but present in other engines (and tested by test262)
    if (byte_length > SharedArrayBuffer.Fields.max_byte_length or
        (max_byte_length orelse 0) > SharedArrayBuffer.Fields.max_byte_length)
    {
        return agent.throwException(.range_error, "Maximum buffer size exceeded", .{});
    }

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
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "SharedArrayBuffer",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinAccessor(object, "@@species", @"@@species", null, realm);

        // 25.2.4.1 SharedArrayBuffer.prototype
        // https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%SharedArrayBuffer.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 25.2.5.2 SharedArrayBuffer.prototype.constructor
        // https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%SharedArrayBuffer.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 25.2.3.1 SharedArrayBuffer ( length [ , options ] )
    /// https://tc39.es/ecma262/#sec-sharedarraybuffer-length
    fn behaviour(agent: *Agent, arguments: ArgumentsList, new_target: ?Object) Agent.Error!Value {
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

    /// 25.2.4.2 get SharedArrayBuffer [ @@species ]
    /// https://tc39.es/ecma262/#sec-sharedarraybuffer-@@species
    fn @"@@species"(_: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 25.2.5 Properties of the SharedArrayBuffer Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-prototype-object
pub const SharedArrayBufferPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);
        try defineBuiltinAccessor(object, "growable", growable, null, realm);

        // 25.2.5.7 SharedArrayBuffer.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("SharedArrayBuffer"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 25.2.5.1 get SharedArrayBuffer.prototype.byteLength
    /// https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.bytelength
    fn byteLength(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
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

    /// 25.2.5.4 get SharedArrayBuffer.prototype.growable
    /// https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.growable
    fn growable(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[ArrayBufferData]]).
        // 3. If IsSharedArrayBuffer(O) is false, throw a TypeError exception.
        const object = try this_value.requireInternalSlot(agent, SharedArrayBuffer);

        // 4. If IsFixedLengthArrayBuffer(O) is false, return true; otherwise return false.
        return Value.from(!isFixedLengthArrayBuffer(ArrayBufferLike{ .shared_array_buffer = object }));
    }
};

/// 25.2.6 Properties of SharedArrayBuffer Instances
/// https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-instances
pub const SharedArrayBuffer = MakeObject(.{
    .Fields = struct {
        /// Arbitrary size limit (32 GiB)
        const max_byte_length = 1024 * 1024 * 1024 * 32;

        /// [[ArrayBufferData]]
        /// [[ArrayBufferByteLength]]
        array_buffer_data: DataBlock,

        // TODO: [[ArrayBufferByteLengthData]]

        /// [[ArrayBufferMaxByteLength]]
        array_buffer_max_byte_length: ?u53 = null,
    },
    .tag = .shared_array_buffer,
});
