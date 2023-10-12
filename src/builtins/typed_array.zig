//! 23.2 TypedArray Objects
//! https://tc39.es/ecma262/#sec-typedarray-objects

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
const allocateArrayBuffer = builtins.allocateArrayBuffer;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;

const Self = @This();

/// Table 70: The TypedArray Constructors
/// https://tc39.es/ecma262/#table-the-typedarray-constructors
pub const Int8Array = MakeTypedArray("Int8Array");
pub const Int8ArrayConstructor = MakeTypedArrayConstructor("Int8Array");
pub const Int8ArrayPrototype = MakeTypedArrayPrototype("Int8Array");

pub const Uint8Array = MakeTypedArray("Uint8Array");
pub const Uint8ArrayConstructor = MakeTypedArrayConstructor("Uint8Array");
pub const Uint8ArrayPrototype = MakeTypedArrayPrototype("Uint8Array");

pub const Uint8ClampedArray = MakeTypedArray("Uint8ClampedArray");
pub const Uint8ClampedArrayConstructor = MakeTypedArrayConstructor("Uint8ClampedArray");
pub const Uint8ClampedArrayPrototype = MakeTypedArrayPrototype("Uint8ClampedArray");

pub const Int16Array = MakeTypedArray("Int16Array");
pub const Int16ArrayConstructor = MakeTypedArrayConstructor("Int16Array");
pub const Int16ArrayPrototype = MakeTypedArrayPrototype("Int16Array");

pub const Uint16Array = MakeTypedArray("Uint16Array");
pub const Uint16ArrayConstructor = MakeTypedArrayConstructor("Uint16Array");
pub const Uint16ArrayPrototype = MakeTypedArrayPrototype("Uint16Array");

pub const Int32Array = MakeTypedArray("Int32Array");
pub const Int32ArrayConstructor = MakeTypedArrayConstructor("Int32Array");
pub const Int32ArrayPrototype = MakeTypedArrayPrototype("Int32Array");

pub const Uint32Array = MakeTypedArray("Uint32Array");
pub const Uint32ArrayConstructor = MakeTypedArrayConstructor("Uint32Array");
pub const Uint32ArrayPrototype = MakeTypedArrayPrototype("Uint32Array");

pub const BigInt64Array = MakeTypedArray("BigInt64Array");
pub const BigInt64ArrayConstructor = MakeTypedArrayConstructor("BigInt64Array");
pub const BigInt64ArrayPrototype = MakeTypedArrayPrototype("BigInt64Array");

pub const BigUint64Array = MakeTypedArray("BigUint64Array");
pub const BigUint64ArrayConstructor = MakeTypedArrayConstructor("BigUint64Array");
pub const BigUint64ArrayPrototype = MakeTypedArrayPrototype("BigUint64Array");

pub const Float32Array = MakeTypedArray("Float32Array");
pub const Float32ArrayConstructor = MakeTypedArrayConstructor("Float32Array");
pub const Float32ArrayPrototype = MakeTypedArrayPrototype("Float32Array");

pub const Float64Array = MakeTypedArray("Float64Array");
pub const Float64ArrayConstructor = MakeTypedArrayConstructor("Float64Array");
pub const Float64ArrayPrototype = MakeTypedArrayPrototype("Float64Array");

/// 23.2.2 Properties of the %TypedArray% Intrinsic Object
/// https://tc39.es/ecma262/#sec-properties-of-the-%typedarray%-intrinsic-object
pub const TypedArrayConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 0,
            .name = "TypedArray",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 23.2.2.3 %TypedArray%.prototype
        // https://tc39.es/ecma262/#sec-%typedarray%.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%TypedArray.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 23.2.3.5 %TypedArray%.prototype.constructor
        // https://tc39.es/ecma262/#sec-%typedarray%.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%TypedArray.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 23.2.1.1 %TypedArray% ( )
    /// https://tc39.es/ecma262/#sec-%typedarray%
    fn behaviour(agent: *Agent, _: Value, _: ArgumentsList, _: ?Object) !Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "TypedArray abstract superclass cannot be constructed",
        );
    }
};

/// 23.2.3 Properties of the %TypedArray% Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-%typedarrayprototype%-object
pub const TypedArrayPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        return object;
    }
};

/// 23.2.4.5 TypedArrayElementSize ( O )
/// https://tc39.es/ecma262/#sec-typedarrayelementsize
pub fn typedArrayElementSize(object: anytype) usize {
    // 1. Return the Element Size value specified in Table 70 for O.[[TypedArrayName]].
    return @sizeOf(@TypeOf(object.fields).ElementType);
}

/// 23.2.5.1.1 AllocateTypedArray ( constructorName, newTarget, defaultProto [ , length ] )
/// https://tc39.es/ecma262/#sec-allocatetypedarray
pub fn allocateTypedArray(
    agent: *Agent,
    comptime constructor_name: []const u8,
    new_target: Object,
    comptime default_prototype: []const u8,
    length: ?u53,
) !Object {
    const T = @field(Self, constructor_name);

    // 1. Let proto be ? GetPrototypeFromConstructor(newTarget, defaultProto).
    const prototype = try getPrototypeFromConstructor(new_target, default_prototype);

    // 2. Let obj be IntegerIndexedObjectCreate(proto).
    // 3. Assert: obj.[[ViewedArrayBuffer]] is undefined.
    const object = try T.create(agent, .{
        .prototype = prototype,
        .internal_methods = .{
            // TODO: Implement these :)
        },
        .fields = .{
            // NOTE: This is either set via allocateTypedArrayBuffer() below, or at the call site.
            .viewed_array_buffer = undefined,

            // 4. Set obj.[[TypedArrayName]] to constructorName.
            .typed_array_name = constructor_name,

            // 5. If constructorName is either "BigInt64Array" or "BigUint64Array", set
            //    obj.[[ContentType]] to bigint.
            // 6. Otherwise, set obj.[[ContentType]] to number.
            .content_type = comptime blk: {
                if (std.mem.eql(u8, constructor_name, "BigInt64Array")) break :blk .bigint;
                if (std.mem.eql(u8, constructor_name, "BigUint64Array")) break :blk .bigint;
                break :blk .number;
            },

            // 7. If length is not present, then
            // NOTE: We do this unconditionally here and skip the branch below instead.

            // a. Set obj.[[ByteLength]] to 0.
            .byte_length = 0,

            // b. Set obj.[[ByteOffset]] to 0.
            .byte_offset = 0,

            // c. Set obj.[[ArrayLength]] to 0.
            .array_length = 0,
        },
    });

    // 7. If length is not present, then
    // 8. Else,
    if (length != null) {
        // a. Perform ? AllocateTypedArrayBuffer(obj, length).
        try allocateTypedArrayBuffer(agent, object.as(T), length.?);
    }

    // 9. Return obj.
    return object;
}

/// 23.2.5.1.6 AllocateTypedArrayBuffer ( O, length )
/// https://tc39.es/ecma262/#sec-allocatetypedarraybuffer
pub fn allocateTypedArrayBuffer(agent: *Agent, object: anytype, length: u53) !void {
    const realm = agent.currentRealm();

    // 1. Assert: O.[[ViewedArrayBuffer]] is undefined.

    // 2. Let elementSize be TypedArrayElementSize(O).
    const element_size = typedArrayElementSize(object);

    // 3. Let byteLength be elementSize × length.
    // NOTE: allocateArrayBuffer() ensures this is in the u53 range, by using saturating
    //       multiplication we easily can get a nice error message for out-of-range values.
    const byte_length: u64 = element_size *| length;

    // 4. Let data be ? AllocateArrayBuffer(%ArrayBuffer%, byteLength).
    const data = try allocateArrayBuffer(
        agent,
        try realm.intrinsics.@"%ArrayBuffer%"(),
        byte_length,
    );

    // 5. Set O.[[ViewedArrayBuffer]] to data.
    object.fields.viewed_array_buffer = data.as(builtins.ArrayBuffer);

    // 6. Set O.[[ByteLength]] to byteLength.
    object.fields.byte_length = @intCast(byte_length);

    // 7. Set O.[[ByteOffset]] to 0.
    object.fields.byte_offset = 0;

    // 8. Set O.[[ArrayLength]] to length.
    object.fields.array_length = length;

    // 9. Return unused.
}

/// 23.2.6 Properties of the TypedArray Constructors
/// https://tc39.es/ecma262/#sec-properties-of-the-typedarray-constructors
fn MakeTypedArrayConstructor(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) !Object {
            const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
                .length = 3,
                .name = name,
                .realm = realm,
                .prototype = try realm.intrinsics.@"%TypedArray%"(),
            });

            const prototypeFn = @field(Realm.Intrinsics, "%" ++ name ++ ".prototype%");

            // 23.2.6.2 TypedArray.prototype
            // https://tc39.es/ecma262/#sec-typedarray.prototype
            try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
                .value = Value.from(try prototypeFn(&realm.intrinsics)),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            });

            // 23.2.7.2 TypedArray.prototype.constructor
            // https://tc39.es/ecma262/#sec-typedarray.prototype.constructor
            try defineBuiltinProperty(
                prototypeFn(&realm.intrinsics) catch unreachable,
                "constructor",
                Value.from(object),
            );

            return object;
        }

        /// 23.2.5.1 TypedArray ( ...args )
        /// https://tc39.es/ecma262/#sec-typedarray
        fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
            // 1. If NewTarget is undefined, throw a TypeError exception.
            if (new_target == null) {
                return agent.throwException(.type_error, name ++ " must be constructed with 'new'");
            }

            // 2. Let constructorName be the String value of the Constructor Name value specified
            //    in Table 70 for this TypedArray constructor.
            const constructor_name = name;

            // 3. Let proto be "%TypedArray.prototype%".
            const prototype = "%" ++ name ++ ".prototype%";

            // 4. Let numberOfArgs be the number of elements in args.
            const number_of_args = arguments.count();

            // 5. If numberOfArgs = 0, then
            if (number_of_args == 0) {
                // a. Return ? AllocateTypedArray(constructorName, NewTarget, proto, 0).
                return Value.from(try allocateTypedArray(
                    agent,
                    constructor_name,
                    new_target.?,
                    prototype,
                    0,
                ));
            }
            // 6. Else,
            else {
                // a. Let firstArgument be args[0].
                const first_argument = arguments.get(0);

                // b. If firstArgument is an Object, then
                if (first_argument == .object) {
                    return agent.throwException(
                        .internal_error,
                        "TypedArray(...args) is not implemented",
                    );
                }
                // c. Else,
                else {
                    // i. Assert: firstArgument is not an Object.
                    std.debug.assert(first_argument != .object);

                    // ii. Let elementLength be ? ToIndex(firstArgument).
                    const element_length = try first_argument.toIndex(agent);

                    // iii. Return ? AllocateTypedArray(constructorName, NewTarget, proto, elementLength).
                    return Value.from(try allocateTypedArray(
                        agent,
                        constructor_name,
                        new_target.?,
                        prototype,
                        element_length,
                    ));
                }
            }
        }
    };
}

/// 23.2.7 Properties of the TypedArray Prototype Objects
/// https://tc39.es/ecma262/#sec-properties-of-typedarray-prototype-objects
fn MakeTypedArrayPrototype(comptime name: []const u8) type {
    _ = name;
    return struct {
        pub fn create(realm: *Realm) !Object {
            const object = try builtins.Object.create(realm.agent, .{
                .prototype = try realm.intrinsics.@"%TypedArray.prototype%"(),
            });

            return object;
        }
    };
}

/// 23.2.8 Properties of TypedArray Instances
/// https://tc39.es/ecma262/#sec-properties-of-typedarray-instances
fn MakeTypedArray(comptime name: []const u8) type {
    return MakeObject(.{
        .Fields = struct {
            const ElementType: type = inline for (.{
                .{ "Int8Array", i8 },
                .{ "Uint8Array", u8 },
                .{ "Uint8ClampedArray", u8 },
                .{ "Int16Array", i16 },
                .{ "Uint16Array", u16 },
                .{ "Int32Array", i32 },
                .{ "Uint32Array", u32 },
                .{ "BigInt64Array", i64 },
                .{ "BigUint64Array", u64 },
                .{ "Float32Array", f32 },
                .{ "Float64Array", f64 },
            }) |entry| {
                if (std.mem.eql(u8, entry[0], name)) break entry[1];
            };

            /// [[TypedArrayName]]
            typed_array_name: []const u8 = name,

            /// [[ContentType]]
            content_type: enum { bigint, number },

            /// [[ViewedArrayBuffer]]
            viewed_array_buffer: *builtins.ArrayBuffer,

            /// [[ByteLength]]
            byte_length: u53,

            /// [[ByteOffset]]
            byte_offset: u53,

            /// [[ArrayLength]]
            array_length: u53,
        },
        .tag = .typed_array,
    });
}
