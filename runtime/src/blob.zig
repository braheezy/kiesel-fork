//! 3. The Blob Interface and Binary Data
//! https://w3c.github.io/FileAPI/#blob-section

const builtin = @import("builtin");
const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const MakeObject = kiesel.types.MakeObject;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const allocateArrayBuffer = kiesel.builtins.allocateArrayBuffer;
const allocateTypedArray = kiesel.builtins.allocateTypedArray;
const createBuiltinFunction = kiesel.builtins.createBuiltinFunction;
const newPromiseCapability = kiesel.builtins.newPromiseCapability;
const noexcept = kiesel.utils.noexcept;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

const convertValueToSequence = @import("webidl/sequence.zig").convertValueToSequence;

const Tag = @import("tag.zig").Tag;

/// https://w3c.github.io/FileAPI/#enumdef-endingtype
const EndingType = enum {
    transparent,
    native,
};

/// https://w3c.github.io/FileAPI/#process-blob-parts
fn processBlobParts(
    allocator: std.mem.Allocator,
    parts: []const Value,
    options: struct { endings: EndingType },
) std.mem.Allocator.Error![]const u8 {
    // 1. Let bytes be an empty sequence of bytes.
    var bytes: std.ArrayList(u8) = .empty;

    // 2. For each element in parts:
    for (parts) |element| {
        // 1. If element is a USVString, run the following substeps:
        if (element.isString()) {
            // 1. Let s be element.
            var string = element.asString();

            // 2. If the endings member of options is "native", set s to the result of converting
            //    line endings to native of element.
            if (options.endings == .native) {
                string = try convertLineEndingsToNative(allocator, string);
            }

            // 3. Append the result of UTF-8 encoding s to bytes.
            // Note: The algorithm from WebIDL [WebIDL] replaces unmatched surrogates in an invalid
            //       utf-16 string with U+FFFD replacement characters. Scenarios exist when the
            //       Blob constructor may result in some data loss due to lost or scrambled
            //       character sequences.
            try bytes.appendSlice(allocator, try string.toUtf8(allocator));
        }

        // TODO: 2. If element is a BufferSource, get a copy of the bytes held by the buffer
        //          source, and append those bytes to bytes.

        // 3. If element is a Blob, append the bytes it represents to bytes.
        // Note: The type of the Blob array element is ignored and will not affect type of returned
        //       Blob object.
        if (element.castObject(Blob)) |blob| {
            try bytes.appendSlice(allocator, blob.fields.bytes);
        }
    }

    // 3. Return bytes.
    return bytes.toOwnedSlice(allocator);
}

/// https://w3c.github.io/FileAPI/#convert-line-endings-to-native
fn convertLineEndingsToNative(
    allocator: std.mem.Allocator,
    string: *const String,
) std.mem.Allocator.Error!*const String {
    // 1. Let native line ending be be the code point U+000A LF.
    // 2. If the underlying platform’s conventions are to represent newlines as a carriage return
    //    and line feed sequence, set native line ending to the code point U+000D CR followed by
    //    the code point U+000A LF.
    const native_line_ending = switch (builtin.os.tag) {
        .windows => String.fromLiteral("\r\n"),
        else => String.fromLiteral("\n"),
    };

    // 3. Set result to the empty string.
    var result: String.Builder = .empty;
    defer result.deinit(allocator);

    // 4. Let position be a position variable for s, initially pointing at the start of s.
    // 5. Let token be the result of collecting a sequence of code points that are not equal to
    //    U+000A LF or U+000D CR from s given position.
    // 6. Append token to result.

    // 7. While position is not past the end of s:
    var it = string.codeUnitIterator();
    while (it.next()) |code_unit| switch (code_unit) {
        // 1. If the code point at position within s equals U+000D CR:
        '\r' => {
            // 1. Append native line ending to result.
            try result.appendString(allocator, native_line_ending);

            // 2. Advance position by 1.
            // 3. If position is not past the end of s and the code point at position within s
            //    equals U+000A LF advance position by 1.
            if (string.codeUnitAt(it.index) == '\n') it.index += 1;
        },

        // 2. Otherwise if the code point at position within s equals U+000A LF, advance position
        //    by 1 and append native line ending to result.
        '\n' => try result.appendString(allocator, native_line_ending),

        // 3. Let token be the result of collecting a sequence of code points that are not equal to
        //    U+000A LF or U+000D CR from s given position.
        // 4. Append token to result.
        else => try result.appendCodeUnit(allocator, code_unit),
    };

    // 8. Return result.
    return result.buildAlloc(allocator);
}

var blob_prototype: *Object = undefined;

pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Blob",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );

        blob_prototype = try prototype.create(agent, realm);
        try builtin_function.object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(blob_prototype),
            .none,
        );
        try blob_prototype.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(&builtin_function.object),
        );

        return &builtin_function.object;
    }

    /// https://w3c.github.io/FileAPI/#constructorBlob
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const blob_parts = try convertValueToSequence(agent, arguments.get(0));
        const options_value = arguments.get(1);

        if (new_target == null) {
            return agent.throwException(.type_error, "Blob must be constructed with 'new'", .{});
        }

        var type_ = String.empty;
        var endings_option = String.fromLiteral("transparent");
        if (!options_value.isUndefined()) {
            const options = try options_value.toObject(agent);
            type_ = try options.getOption(agent, "type", .string, null, String.empty);
            endings_option = try options.getOption(
                agent,
                "endings",
                .string,
                &.{ String.fromLiteral("transparent"), String.fromLiteral("native") },
                String.fromLiteral("transparent"),
            );
        }
        const endings_map = std.StaticStringMap(EndingType).initComptime(&.{
            .{ "transparent", .transparent },
            .{ "native", .native },
        });
        const endings = endings_map.get(endings_option.asAscii()).?;

        // 1. If invoked with zero parameters, return a new Blob object consisting of 0 bytes, with
        //    size set to 0, and with type set to the empty string.
        if (arguments.count() == 0) {
            const blob = try Blob.create(agent, .{
                .prototype = blob_prototype,
                .fields = .{
                    .bytes = &.{},
                    .type = .empty,
                },
            });
            return Value.from(&blob.object);
        }

        // 2. Let bytes be the result of processing blob parts given blobParts and options.
        const bytes = try processBlobParts(
            agent.gc_allocator,
            blob_parts,
            .{ .endings = endings },
        );

        // 3. If the type member of the options argument is not the empty string, run the following sub-steps:
        if (!type_.isEmpty()) {
            // 1. Let t be the type dictionary member. If t contains any characters outside the
            //    range U+0020 to U+007E, then set t to the empty string and return from these substeps.
            var it = type_.codeUnitIterator();
            type_ = while (it.next()) |c| {
                if (c < 0x20 or c > 0x7e) break String.empty;
            } else blk: {
                // 2. Convert every character in t to ASCII lowercase.
                break :blk try type_.toLowerCaseAscii(agent);
            };
        }

        // 4. Return a Blob object referring to bytes as its associated byte sequence, with its
        //    size set to the length of bytes, and its type set to the value of t from the substeps
        //    above.
        const blob = try Blob.create(agent, .{
            .prototype = blob_prototype,
            .fields = .{
                .bytes = bytes,
                .type = type_,
            },
        });
        return Value.from(&blob.object);
    }
};

pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        try object.defineBuiltinAccessorWithAttributes(agent, "size", size, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinAccessorWithAttributes(agent, "type", @"type", null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "text", text, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "arrayBuffer", arrayBuffer, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "bytes", bytes, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// https://w3c.github.io/FileAPI/#dfn-size
    fn size(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // Returns the size of the byte sequence in number of bytes. On getting, conforming user
        // agents must return the total number of bytes that can be read by a FileReader or
        // FileReaderSync object, or 0 if the Blob has no bytes to be read.
        const blob = try this_value.requireInternalSlot(agent, Blob);
        return Value.from(@as(u53, @intCast(blob.fields.bytes.len)));
    }

    /// https://w3c.github.io/FileAPI/#dfn-type
    fn @"type"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The ASCII-encoded string in lower case representing the media type of the Blob. On
        // getting, user agents must return the type of a Blob as an ASCII-encoded string in lower
        // case, such that when it is converted to a byte sequence, it is a parsable MIME type, or
        // the empty string – 0 bytes – if the type cannot be determined.
        const blob = try this_value.requireInternalSlot(agent, Blob);
        return Value.from(blob.fields.type);
    }

    /// https://w3c.github.io/FileAPI/#dom-blob-text
    fn text(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let stream be the result of calling get stream on this.
        // 2. Let reader be the result of getting a reader from stream. If that threw an exception,
        //    return a new promise rejected with that exception.
        // 3. Let promise be the result of reading all bytes from stream with reader.
        // 4. Return the result of transforming promise by a fulfillment handler that returns the
        //    result of running UTF-8 decode on its first argument.
        const blob = try this_value.requireInternalSlot(agent, Blob);
        const utf8 = try std.fmt.allocPrint(
            agent.gc_allocator,
            "{f}",
            .{std.unicode.fmtUtf8(blob.fields.bytes)},
        );
        const string = try String.fromUtf8(agent, utf8);
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);
        _ = try Value.from(promise_capability.resolve).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(string)},
        );
        return Value.from(promise_capability.promise);
    }

    /// https://w3c.github.io/FileAPI/#dom-blob-arraybuffer
    fn arrayBuffer(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let stream be the result of calling get stream on this.
        // 2. Let reader be the result of getting a reader from stream. If that threw an exception,
        //    return a new promise rejected with that exception.
        // 3. Let promise be the result of reading all bytes from stream with reader.
        // 4. Return the result of transforming promise by a fulfillment handler that returns a new
        //    ArrayBuffer whose contents are its first argument.
        const blob = try this_value.requireInternalSlot(agent, Blob);
        const array_buffer = try allocateArrayBuffer(
            agent,
            try realm.intrinsics.@"%ArrayBuffer%"(),
            @enumFromInt(@as(u53, @intCast(blob.fields.bytes.len))),
            .none,
        );
        @memcpy(
            array_buffer.fields.data_block.?.bytes,
            blob.fields.bytes,
        );
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);
        _ = try Value.from(promise_capability.resolve).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(&array_buffer.object)},
        );
        return Value.from(promise_capability.promise);
    }

    /// https://w3c.github.io/FileAPI/#dom-blob-bytes
    fn bytes(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let stream be the result of calling get stream on this.
        // 2. Let reader be the result of getting a reader from stream. If that threw an exception,
        //    return a new promise rejected with that exception.
        // 3. Let promise be the result of reading all bytes from stream with reader.
        // 4. Return the result of transforming promise by a fulfillment handler that returns a new
        //    Uint8Array wrapping an ArrayBuffer containing its first argument.
        const blob = try this_value.requireInternalSlot(agent, Blob);
        const typed_array = try allocateTypedArray(
            agent,
            .uint8,
            try realm.intrinsics.@"%Uint8Array%"(),
            "%Uint8Array.prototype%",
            @enumFromInt(blob.fields.bytes.len),
        );
        @memcpy(
            typed_array.fields.viewed_array_buffer.fields.data_block.?.bytes,
            blob.fields.bytes,
        );
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);
        _ = try Value.from(promise_capability.resolve).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(&typed_array.object)},
        );
        return Value.from(promise_capability.promise);
    }
};

pub const Blob = MakeObject(.{
    .Fields = struct {
        bytes: []const u8,
        type: *const String,
    },
    .tag = @enumFromInt(@intFromEnum(Tag.blob)),
    .display_name = "Blob",
});
