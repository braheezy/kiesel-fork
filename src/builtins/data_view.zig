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
