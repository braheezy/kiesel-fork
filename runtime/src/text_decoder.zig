//! 7.2. Interface TextDecoder
//! https://encoding.spec.whatwg.org/#interface-textdecoder

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const MakeObject = kiesel.types.MakeObject;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const createBuiltinFunction = kiesel.builtins.createBuiltinFunction;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

const Tag = @import("tag.zig").Tag;

const bom = "\xEF\xBB\xBF";

/// https://encoding.spec.whatwg.org/#encoding
const Encoding = enum {
    @"utf-8",
    replacement,
};

/// https://encoding.spec.whatwg.org/#error-mode
const ErrorMode = enum {
    replacement,
    fatal,
};

/// https://encoding.spec.whatwg.org/#concept-encoding-get
fn getEncoding(label: *const String) ?Encoding {
    if (label.eql(String.fromLiteral("unicode-1-1-utf-8")) or
        label.eql(String.fromLiteral("unicode11utf8")) or
        label.eql(String.fromLiteral("unicode20utf8")) or
        label.eql(String.fromLiteral("utf-8")) or
        label.eql(String.fromLiteral("utf8")) or
        label.eql(String.fromLiteral("x-unicode20utf8")))
        return .@"utf-8";
    if (label.eql(String.fromLiteral("csiso2022kr")) or
        label.eql(String.fromLiteral("hz-gb-2312")) or
        label.eql(String.fromLiteral("iso-2022-cn")) or
        label.eql(String.fromLiteral("iso-2022-cn-ext")) or
        label.eql(String.fromLiteral("iso-2022-kr")) or
        label.eql(String.fromLiteral("replacement")))
        return .replacement;
    // TODO: Handle anything other than UTF-8 and replacement
    return null;
}

var text_decoder_prototype: *Object = undefined;

pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "TextDecoder",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );

        text_decoder_prototype = try prototype.create(agent, realm);
        try builtin_function.object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(text_decoder_prototype),
            .none,
        );
        try text_decoder_prototype.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(&builtin_function.object),
        );

        return &builtin_function.object;
    }

    /// https://encoding.spec.whatwg.org/#dom-textdecoder
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const label_value = arguments.get(0);
        const options_value = arguments.get(1);

        if (new_target == null) {
            return agent.throwException(.type_error, "TextDecoder must be constructed with 'new'", .{});
        }

        const label = if (label_value.isUndefined())
            String.fromLiteral("utf-8")
        else
            try label_value.toString(agent);

        var fatal = false;
        var ignore_bom = false;
        if (!options_value.isUndefined()) {
            const options = try options_value.toObject(agent);
            fatal = try options.getOption(agent, "fatal", .boolean, null, false);
            ignore_bom = try options.getOption(agent, "ignoreBOM", .boolean, null, false);
        }

        // 1. Let encoding be the result of getting an encoding from label.
        const encoding = getEncoding(label);

        // 2. If encoding is failure or replacement, then throw a RangeError.
        if (encoding == null or encoding.? == .replacement) {
            return agent.throwException(.range_error, "Invalid encoding {f}", .{label});
        }

        const text_decoder = try TextDecoder.create(agent, .{
            .prototype = text_decoder_prototype,
            .fields = .{
                // 3. Set this’s encoding to encoding.
                .encoding = encoding.?,

                // 4. If options["fatal"] is true, then set this’s error mode to "fatal".
                .error_mode = if (fatal) .fatal else .replacement,

                // 5. Set this’s ignore BOM to options["ignoreBOM"].
                .ignore_bom = ignore_bom,
            },
        });
        return Value.from(&text_decoder.object);
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
        try object.defineBuiltinAccessorWithAttributes(agent, "fatal", fatal, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinAccessorWithAttributes(agent, "ignoreBOM", ignoreBOM, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "decode", decode, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// https://encoding.spec.whatwg.org/#dom-textdecoder-encoding
    fn encoding(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The encoding getter steps are to return this’s encoding’s name, ASCII lowercased.
        const text_decoder = try this_value.requireInternalSlot(agent, TextDecoder);
        return Value.from(switch (text_decoder.fields.encoding) {
            .@"utf-8" => String.fromLiteral("utf-8"),
            .replacement => String.fromLiteral("replacement"),
        });
    }

    /// https://encoding.spec.whatwg.org/#dom-textdecoder-decode
    fn fatal(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The fatal getter steps are to return true if this’s error mode is "fatal", otherwise false.
        const text_decoder = try this_value.requireInternalSlot(agent, TextDecoder);
        return Value.from(text_decoder.fields.error_mode == .fatal);
    }

    /// https://encoding.spec.whatwg.org/#dom-textdecoder-decode
    fn ignoreBOM(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The ignoreBOM getter steps are to return this’s ignore BOM.
        const text_decoder = try this_value.requireInternalSlot(agent, TextDecoder);
        return Value.from(text_decoder.fields.ignore_bom);
    }

    /// https://encoding.spec.whatwg.org/#dom-textdecoder-decode
    fn decode(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const input = arguments.get(0);
        const text_decoder = try this_value.requireInternalSlot(agent, TextDecoder);
        const maybe_data_block = if (input.castObject(kiesel.builtins.ArrayBuffer)) |array_buffer|
            array_buffer.fields.data_block
        else if (input.castObject(kiesel.builtins.TypedArray)) |typed_array|
            // TODO: Handle byte offset/length
            typed_array.fields.viewed_array_buffer.fields.data_block
        else
            return agent.throwException(.type_error, "Invalid input {f}", .{input});
        var bytes = if (maybe_data_block) |data_block| data_block.bytes else &.{};
        std.debug.assert(text_decoder.fields.encoding == .@"utf-8");
        if (!text_decoder.fields.ignore_bom and std.mem.startsWith(u8, bytes, bom)) {
            bytes = bytes[bom.len..];
        }
        const utf8 = if (std.unicode.utf8ValidateSlice(bytes))
            try String.fromUtf8(agent, bytes)
        else switch (text_decoder.fields.error_mode) {
            .replacement => try String.fromUtf8(
                agent,
                try std.fmt.allocPrint(agent.gc_allocator, "{f}", .{std.unicode.fmtUtf8(bytes)}),
            ),
            .fatal => return agent.throwException(.type_error, "Invalid UTF-8 data", .{}),
        };

        return Value.from(utf8);
    }
};

pub const TextDecoder = MakeObject(.{
    .Fields = struct {
        encoding: Encoding,
        ignore_bom: bool,
        error_mode: ErrorMode,
    },
    .tag = @enumFromInt(@intFromEnum(Tag.text_decoder)),
    .display_name = "TextDecoder",
});
