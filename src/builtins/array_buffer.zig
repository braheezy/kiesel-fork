//! 25.1 ArrayBuffer Objects
//! https://tc39.es/ecma262/#sec-arraybuffer-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createByteDataBlock = types.createByteDataBlock;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

/// 25.1.2.1 AllocateArrayBuffer ( constructor, byteLength )
/// https://tc39.es/ecma262/#sec-allocatearraybuffer
pub fn allocateArrayBuffer(agent: *Agent, constructor: Object, byte_length: u64) !Object {
    // 1. Let obj be ? OrdinaryCreateFromConstructor(constructor, "%ArrayBuffer.prototype%",
    //    « [[ArrayBufferData]], [[ArrayBufferByteLength]], [[ArrayBufferDetachKey]] »).
    const object = try ordinaryCreateFromConstructor(
        ArrayBuffer,
        agent,
        constructor,
        "%ArrayBuffer.prototype%",
    );

    // 2. Let block be ? CreateByteDataBlock(byteLength).
    const block = try createByteDataBlock(agent, byte_length);

    object.as(ArrayBuffer).fields = .{
        // 3. Set obj.[[ArrayBufferData]] to block.
        // 4. Set obj.[[ArrayBufferByteLength]] to byteLength.
        .array_buffer_data = block,
    };

    // 5. Return obj.
    return object;
}

/// 25.1.2.2 IsDetachedBuffer ( arrayBuffer )
/// https://tc39.es/ecma262/#sec-isdetachedbuffer
pub fn isDetachedBuffer(array_buffer: *const ArrayBuffer) bool {
    // 1. If arrayBuffer.[[ArrayBufferData]] is null, return true.
    // 2. Return false.
    return array_buffer.fields.array_buffer_data == null;
}

/// 25.1.2.3 DetachArrayBuffer ( arrayBuffer [ , key ] )
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

/// 25.1.4 Properties of the ArrayBuffer Constructor
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

        // 25.1.4.2 ArrayBuffer.prototype
        // https://tc39.es/ecma262/#sec-arraybuffer.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%ArrayBuffer.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 25.1.5.2 ArrayBuffer.prototype.constructor
        // https://tc39.es/ecma262/#sec-arraybuffer.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%ArrayBuffer.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        // 25.1.4.3 get ArrayBuffer [ @@species ]
        // https://tc39.es/ecma262/#sec-get-arraybuffer-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        return object;
    }

    /// 25.1.3.1 ArrayBuffer ( length )
    /// https://tc39.es/ecma262/#sec-arraybuffer-length
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const length = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "ArrayBuffer must be constructed with 'new'");
        }

        // 2. Let byteLength be ? ToIndex(length).
        const byte_length = try length.toIndex(agent);

        // 3. Return ? AllocateArrayBuffer(NewTarget, byteLength).
        return Value.from(try allocateArrayBuffer(agent, new_target.?, @intCast(byte_length)));
    }

    /// 25.1.4.1 ArrayBuffer.isView ( arg )
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

/// 25.1.5 Properties of the ArrayBuffer Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-prototype-object
pub const ArrayBufferPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "byteLength", byteLength, null, realm);

        return object;
    }

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
};

/// 25.1.6 Properties of ArrayBuffer Instances
/// https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-instances
pub const ArrayBuffer = Object.Factory(.{
    .Fields = struct {
        /// [[ArrayBufferData]]
        /// [[ArrayBufferByteLength]]
        array_buffer_data: ?std.ArrayList(u8),

        /// [[ArrayBufferDetachKey]]
        array_buffer_detach_key: Value = .undefined,
    },
    .tag = .array_buffer,
});
