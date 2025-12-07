//! 7.4. Interface TextEncoder
//! https://encoding.spec.whatwg.org/#interface-textencoder

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const MakeObject = kiesel.types.MakeObject;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const Value = kiesel.types.Value;
const allocateTypedArray = kiesel.builtins.allocateTypedArray;
const createBuiltinFunction = kiesel.builtins.createBuiltinFunction;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

const Tag = @import("tag.zig").Tag;

var text_encoder_prototype: *Object = undefined;

pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "TextEncoder",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );

        text_encoder_prototype = try prototype.create(agent, realm);
        try builtin_function.object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(text_encoder_prototype),
            .none,
        );
        try text_encoder_prototype.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(&builtin_function.object),
        );

        return &builtin_function.object;
    }

    /// https://encoding.spec.whatwg.org/#dom-textencoder
    fn impl(agent: *Agent, _: Arguments, new_target: ?*Object) Agent.Error!Value {
        // The new TextEncoder() constructor steps are to do nothing.
        if (new_target == null) {
            return agent.throwException(.type_error, "TextEncoder must be constructed with 'new'", .{});
        }

        const text_encoder = try TextEncoder.create(agent, .{
            .prototype = text_encoder_prototype,
        });
        return Value.from(&text_encoder.object);
    }
};

pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        try object.defineBuiltinAccessorWithAttributes(agent, "encoding", encoding, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "encode", encode, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// https://encoding.spec.whatwg.org/#dom-textencoder-encoding
    fn encoding(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The encoding getter steps are to return "utf-8".
        _ = try this_value.requireInternalSlot(agent, TextEncoder);
        return Value.from("utf-8");
    }

    /// https://encoding.spec.whatwg.org/#dom-textencoder-encode
    fn encode(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const input_value = arguments.get(0);
        _ = try this_value.requireInternalSlot(agent, TextEncoder);
        const input = try input_value.toString(agent);
        const typed_array = try allocateTypedArray(
            agent,
            .uint8,
            try realm.intrinsics.@"%Uint8Array%"(),
            "%Uint8Array.prototype%",
            @enumFromInt(std.fmt.count("{f}", .{input.fmtRaw()})),
        );
        const array_buffer = typed_array.fields.viewed_array_buffer;
        _ = std.fmt.bufPrint(
            array_buffer.fields.data_block.?.bytes,
            "{f}",
            .{input.fmtRaw()},
        ) catch unreachable;
        return Value.from(&typed_array.object);
    }
};

pub const TextEncoder = MakeObject(.{
    .tag = @enumFromInt(@intFromEnum(Tag.text_encoder)),
    .display_name = "TextEncoder",
});
