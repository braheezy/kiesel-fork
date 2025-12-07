//! 10. Crypto interface
//! https://w3c.github.io/webcrypto/#crypto-interface

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const MakeObject = kiesel.types.MakeObject;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

const Tag = @import("tag.zig").Tag;

pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Crypto {
    return Crypto.create(agent, .{
        .prototype = try prototype.create(agent, realm),
    });
}

pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        try object.defineBuiltinFunctionWithAttributes(agent, "getRandomValues", getRandomValues, 1, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "randomUUID", randomUUID, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// 10.1.1 The getRandomValues method
    /// https://w3c.github.io/webcrypto/#Crypto-method-getRandomValues
    fn getRandomValues(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const array = arguments.get(0);

        // 1. If array is not an Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array,
        //    Int32Array, Uint32Array, BigInt64Array, or BigUint64Array, then throw a
        //    TypeMismatchError and terminate the algorithm.
        // TODO: Wrong error type
        const typed_array = try array.requireInternalSlot(agent, kiesel.builtins.TypedArray);

        // 2. If the byteLength of array is greater than 65536, throw a QuotaExceededError and
        //    terminate the algorithm.
        switch (typed_array.fields.byte_length) {
            .auto => {},
            else => |value| if (@intFromEnum(value) > 65536) {
                // TODO: Wrong error type
                return agent.throwException(.range_error, "Byte length must not exceed 65536", .{});
            },
        }

        // 3. Overwrite all elements of array with cryptographically strong random values of the
        //    appropriate type.
        if (typed_array.fields.viewed_array_buffer.fields.data_block) |data_block| {
            std.crypto.random.bytes(data_block.bytes);
        }

        // 4. Return array.
        return array;
    }

    /// 10.1.2 The randomUUID method
    /// https://w3c.github.io/webcrypto/#Crypto-method-randomUUID
    fn randomUUID(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Let bytes be a byte sequence of length 16.
        var bytes: [16]u8 = undefined;

        // 2. Fill bytes with cryptographically secure random bytes.
        std.crypto.random.bytes(&bytes);

        // 3. Set the 4 most significant bits of bytes[6], which represent the UUID version, to 0100.
        bytes[6] = (bytes[6] & 0b00001111) | 0b01000000;

        // 4. Set the 2 most significant bits of bytes[8], which represent the UUID variant, to 10.
        bytes[8] = (bytes[8] & 0b00111111) | 0b10000000;

        // 5. Return the string concatenation of «
        //      hexadecimal representation of bytes[0], hexadecimal representation of bytes[1],
        //      hexadecimal representation of bytes[2], hexadecimal representation of bytes[3],
        //      "-",
        //      hexadecimal representation of bytes[4], hexadecimal representation of bytes[5],
        //      "-",
        //      hexadecimal representation of bytes[6], hexadecimal representation of bytes[7],
        //      "-",
        //      hexadecimal representation of bytes[8], hexadecimal representation of bytes[9],
        //      "-",
        //      hexadecimal representation of bytes[10], hexadecimal representation of bytes[11],
        //      hexadecimal representation of bytes[12], hexadecimal representation of bytes[13],
        //      hexadecimal representation of bytes[14], hexadecimal representation of bytes[15]
        //    ».
        const uuid = try std.fmt.allocPrint(
            agent.gc_allocator,
            "{s}-{s}-{s}-{s}-{s}",
            .{
                std.fmt.bytesToHex(bytes[0..4], .lower),
                std.fmt.bytesToHex(bytes[4..6], .lower),
                std.fmt.bytesToHex(bytes[6..8], .lower),
                std.fmt.bytesToHex(bytes[8..10], .lower),
                std.fmt.bytesToHex(bytes[10..15], .lower),
            },
        );
        return Value.from(try String.fromAscii(agent, uuid));
    }
};

pub const Crypto = MakeObject(.{
    .tag = @enumFromInt(@intFromEnum(Tag.crypto)),
    .display_name = "Crypto",
});
