//! 25.5 The JSON Object
//! https://tc39.es/ecma262/#sec-json-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const PropertyKeyArrayHashMap = Object.PropertyStorage.PropertyKeyArrayHashMap;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const arrayCreate = builtins.arrayCreate;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// Recursively convert a `std.json.Value` to a JS `Value`.
fn convertJsonValue(agent: *Agent, value: std.json.Value) Allocator.Error!Value {
    return switch (value) {
        .null => .null,
        .bool => |x| Value.from(x),
        .float => |x| Value.from(x),
        .integer => |x| Value.from(@as(f64, @floatFromInt(x))),
        .string => |x| Value.from(
            try String.fromUtf8(agent.gc_allocator, try agent.gc_allocator.dupe(u8, x)),
        ),
        .number_string => |x| Value.from(std.fmt.parseFloat(f64, x) catch unreachable),
        .array => |x| blk: {
            const array = arrayCreate(agent, 0, null) catch |err| try noexcept(err);
            for (x.items, 0..) |value_i, i| {
                array.createDataPropertyOrThrow(
                    PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))),
                    try convertJsonValue(agent, value_i),
                ) catch |err| try noexcept(err);
            }
            break :blk Value.from(array);
        },
        .object => |x| blk: {
            const realm = agent.currentRealm();
            const object = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%Object.prototype%"(),
            );
            var it = x.iterator();
            while (it.next()) |entry| {
                object.createDataPropertyOrThrow(
                    PropertyKey.from(
                        try String.fromUtf8(
                            agent.gc_allocator,
                            try agent.gc_allocator.dupe(u8, entry.key_ptr.*),
                        ),
                    ),
                    try convertJsonValue(agent, entry.value_ptr.*),
                ) catch |err| try noexcept(err);
            }
            break :blk Value.from(object);
        },
    };
}

/// 25.5.1.1 InternalizeJSONProperty ( holder, name, reviver )
/// https://tc39.es/ecma262/#sec-internalizejsonproperty
fn internalizeJSONProperty(
    agent: *Agent,
    holder: Object,
    name: PropertyKey,
    reviver: Object,
) Agent.Error!Value {
    // 1. Let val be ? Get(holder, name).
    const value = try holder.get(name);

    // 2. If val is an Object, then
    if (value == .object) {
        // a. Let isArray be ? IsArray(val).
        const is_array = try value.isArray();

        // b. If isArray is true, then
        if (is_array) {
            // i. Let len be ? LengthOfArrayLike(val).
            const len = try value.object.lengthOfArrayLike();

            // ii. Let I be 0.
            var i: u53 = 0;

            // iii. Repeat, while I < len,
            while (i < len) : (i += 1) {
                // 1. Let prop be ! ToString(𝔽(I)).
                const property_key = PropertyKey.from(i);

                // 2. Let newElement be ? InternalizeJSONProperty(val, prop, reviver).
                const new_element = try internalizeJSONProperty(
                    agent,
                    value.object,
                    property_key,
                    reviver,
                );

                // 3. If newElement is undefined, then
                if (new_element == .undefined) {
                    // a. Perform ? val.[[Delete]](prop).
                    _ = try value.object.internalMethods().delete(value.object, property_key);
                }
                // 4. Else,
                else {
                    // a. Perform ? CreateDataProperty(val, prop, newElement).
                    _ = try value.object.createDataProperty(property_key, new_element);
                }

                // 5. Set I to I + 1.
            }
        }
        // c. Else,
        else {
            // i. Let keys be ? EnumerableOwnProperties(val, key).
            const keys = try value.object.enumerableOwnProperties(.key);
            defer keys.deinit();

            // ii. For each String P of keys, do
            for (keys.items) |key| {
                const property_key = try key.toPropertyKey(agent);

                // 1. Let newElement be ? InternalizeJSONProperty(val, P, reviver).
                const new_element = try internalizeJSONProperty(
                    agent,
                    value.object,
                    property_key,
                    reviver,
                );

                // 2. If newElement is undefined, then
                if (new_element == .undefined) {
                    // a. Perform ? val.[[Delete]](P).
                    _ = try value.object.internalMethods().delete(value.object, property_key);
                }
                // 3. Else,
                else {
                    // a. Perform ? CreateDataProperty(val, P, newElement).
                    _ = try value.object.createDataProperty(property_key, new_element);
                }
            }
        }
    }

    // 3. Return ? Call(reviver, holder, « name, val »).
    return Value.from(reviver).callAssumeCallable(
        Value.from(holder),
        &.{ name.toValue(agent) catch unreachable, value },
    );
}

/// 25.5.2.1 JSON Serialization Record
/// https://tc39.es/ecma262/#sec-json-serialization-record
const JSONSerialization = struct {
    pub const Stack = std.AutoHashMap(Object, void);

    /// [[ReplacerFunction]]
    replacer_function: ?Object,

    /// [[PropertyList]]
    property_list: ?PropertyKeyArrayHashMap(void),

    /// [[Gap]]
    gap: String,

    /// [[Stack]]
    stack: Stack,

    /// [[Indent]]
    indent: String,
};

/// 25.5.2.2 SerializeJSONProperty ( state, key, holder )
/// https://tc39.es/ecma262/#sec-serializejsonproperty
fn serializeJSONProperty(
    agent: *Agent,
    state: *JSONSerialization,
    key: PropertyKey,
    holder: Object,
) Agent.Error!?String {
    // 1. Let value be ? Get(holder, key).
    var value = try holder.get(key);

    // 2. If value is an Object or value is a BigInt, then
    if (value == .object or value == .big_int) {
        // a. Let toJSON be ? GetV(value, "toJSON").
        const to_json = try value.get(agent, PropertyKey.from("toJSON"));

        // b. If IsCallable(toJSON) is true, then
        if (to_json.isCallable()) {
            // i. Set value to ? Call(toJSON, value, « key »).
            value = try to_json.callAssumeCallable(value, &.{try key.toValue(agent)});
        }
    }

    // 3. If state.[[ReplacerFunction]] is not undefined, then
    if (state.replacer_function) |replacer_function| {
        // a. Set value to ? Call(state.[[ReplacerFunction]], holder, « key, value »).
        value = try Value.from(replacer_function).callAssumeCallable(
            Value.from(holder),
            &.{ try key.toValue(agent), value },
        );
    }

    // 4. If value is an Object, then
    if (value == .object) {
        // a. If value has a [[NumberData]] internal slot, then
        if (value.object.is(builtins.Number)) {
            // i. Set value to ? ToNumber(value).
            value = Value.from(try value.toNumber(agent));
        }
        // b. Else if value has a [[StringData]] internal slot, then
        else if (value.object.is(builtins.String)) {
            // i. Set value to ? ToString(value).
            value = Value.from(try value.toString(agent));
        }
        // c. Else if value has a [[BooleanData]] internal slot, then
        else if (value.object.is(builtins.Boolean)) {
            // i. Set value to value.[[BooleanData]].
            value = Value.from(value.object.as(builtins.Boolean).fields.boolean_data);
        }
        // d. Else if value has a [[BigIntData]] internal slot, then
        else if (value.object.is(builtins.BigInt)) {
            // i. Set value to value.[[BigIntData]].
            value = Value.from(value.object.as(builtins.BigInt).fields.big_int_data);
        }
    }

    switch (value) {
        // 5. If value is null, return "null".
        .null => return String.fromLiteral("null"),

        // 6. If value is true, return "true".
        // 7. If value is false, return "false".
        .boolean => |boolean| return if (boolean) String.fromLiteral("true") else String.fromLiteral("false"),

        // 8. If value is a String, return QuoteJSONString(value).
        .string => |string| return try quoteJSONString(agent, string),

        // 9. If value is a Number, then
        .number => |number| {
            // a. If value is finite, return ! ToString(value).
            if (number.isFinite()) return try number.toString(agent.gc_allocator, 10);

            // b. Return "null".
            return String.fromLiteral("null");
        },

        else => {},
    }

    // 10. If value is a BigInt, throw a TypeError exception.
    if (value == .big_int) {
        return agent.throwException(.type_error, "Cannot serialize BigInt to JSON", .{});
    }

    // 11. If value is an Object and IsCallable(value) is false, then
    if (value == .object and !value.isCallable()) {
        // a. Let isArray be ? IsArray(value).
        const is_array = try value.isArray();

        // b. If isArray is true, return ? SerializeJSONArray(state, value).
        if (is_array) return try serializeJSONArray(agent, state, value.object);

        // c. Return ? SerializeJSONObject(state, value).
        return try serializeJSONObject(agent, state, value.object);
    }

    // 12. Return undefined.
    return null;
}

/// 25.5.2.3 QuoteJSONString ( value )
/// https://tc39.es/ecma262/#sec-quotejsonstring
fn quoteJSONString(agent: *Agent, value: String) Allocator.Error!String {
    // 1. Let product be the String value consisting solely of the code unit 0x0022 (QUOTATION MARK).
    var product = std.ArrayList(u8).init(agent.gc_allocator);
    try product.append('"');

    // 2. For each code point C of StringToCodePoints(value), do
    var it = value.codeUnitIterator();
    while (it.next()) |c| {
        // a. If C is listed in the “Code Point” column of Table 76, then
        if (c == 0x08 or c == 0x09 or c == 0x0A or c == 0x0C or c == 0x0D or c == 0x22 or c == 0x5C) {
            // i. Set product to the string-concatenation of product and the escape sequence for C
            //    as specified in the “Escape Sequence” column of the corresponding row.
            switch (c) {
                0x08 => try product.appendSlice("\\b"),
                0x09 => try product.appendSlice("\\t"),
                0x0A => try product.appendSlice("\\n"),
                0x0C => try product.appendSlice("\\f"),
                0x0D => try product.appendSlice("\\r"),
                0x22 => try product.appendSlice("\\\""),
                0x5C => try product.appendSlice("\\\\"),
                else => unreachable,
            }
        }
        // b. Else if C has a numeric value less than 0x0020 (SPACE) or C has the same numeric
        //    value as a leading surrogate or trailing surrogate, then
        else if (c < 0x20 or std.unicode.utf16IsLowSurrogate(c) or std.unicode.utf16IsHighSurrogate(c)) {
            // i. Let unit be the code unit whose numeric value is the numeric value of C.
            // ii. Set product to the string-concatenation of product and UnicodeEscape(unit).
            try product.appendSlice(try unicodeEscape(agent, c));
        }
        // c. Else,
        else {
            // i. Set product to the string-concatenation of product and UTF16EncodeCodePoint(C).
            try product.appendSlice(
                std.unicode.utf16LeToUtf8Alloc(agent.gc_allocator, &.{c}) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.DanglingSurrogateHalf,
                    error.ExpectedSecondSurrogateHalf,
                    error.UnexpectedSecondSurrogateHalf,
                    => unreachable,
                },
            );
        }
    }

    // 3. Set product to the string-concatenation of product and the code unit 0x0022 (QUOTATION MARK).
    try product.append('"');

    // 4. Return product.
    return String.fromUtf8(agent.gc_allocator, try product.toOwnedSlice());
}

/// 25.5.2.4 UnicodeEscape ( C )
/// https://tc39.es/ecma262/#sec-unicodeescape
fn unicodeEscape(agent: *Agent, c: u16) Allocator.Error![]const u8 {
    // 1. Let n be the numeric value of C.
    // 2. Assert: n ≤ 0xFFFF.
    // 3. Let hex be the String representation of n, formatted as a lowercase hexadecimal number.
    // 4. Return the string-concatenation of the code unit 0x005C (REVERSE SOLIDUS), "u", and
    //    StringPad(hex, 4, "0", start).
    return std.fmt.allocPrint(agent.gc_allocator, "\\u{x:0>4}", .{c});
}

/// 25.5.2.5 SerializeJSONObject ( state, value )
/// https://tc39.es/ecma262/#sec-serializejsonobject
fn serializeJSONObject(
    agent: *Agent,
    state: *JSONSerialization,
    value: Object,
) error{ OutOfMemory, ExceptionThrown }!String {
    // 1. If state.[[Stack]] contains value, throw a TypeError exception because the structure is
    //    cyclical.
    if (state.stack.contains(value)) {
        return agent.throwException(.type_error, "Cannot serialize cyclic object to JSON", .{});
    }

    // 2. Append value to state.[[Stack]].
    try state.stack.put(value, {});

    // 3. Let stepback be state.[[Indent]].
    const stepback = state.indent;

    // 4. Set state.[[Indent]] to the string-concatenation of state.[[Indent]] and state.[[Gap]].
    state.indent = try String.concat(agent.gc_allocator, &.{ state.indent, state.gap });

    // 5. If state.[[PropertyList]] is not undefined, then
    //     a. Let K be state.[[PropertyList]].
    // 6. Else,
    //     a. Let K be ? EnumerableOwnProperties(value, key).
    var keys = state.property_list orelse blk: {
        const keys = try value.enumerableOwnProperties(.key);
        defer keys.deinit();
        var converted = PropertyKeyArrayHashMap(void).init(agent.gc_allocator);
        try converted.ensureUnusedCapacity(keys.items.len);
        for (keys.items) |key| {
            converted.putAssumeCapacityNoClobber(key.toPropertyKey(agent) catch |err| try noexcept(err), {});
        }
        break :blk converted;
    };
    defer if (state.property_list == null) keys.deinit();

    // 7. Let partial be a new empty List.
    var partial = try std.ArrayList([]const u8).initCapacity(agent.gc_allocator, keys.count());
    defer partial.deinit();

    // 8. For each element P of K, do
    for (keys.keys()) |property_key| {
        // a. Let strP be ? SerializeJSONProperty(state, P, value).
        const str_property = try serializeJSONProperty(
            agent,
            state,
            property_key,
            value,
        );

        // b. If strP is not undefined, then
        if (str_property != null) {
            // i. Let member be QuoteJSONString(P).
            // ii. Set member to the string-concatenation of member and ":".
            // iii. If state.[[Gap]] is not the empty String, then
            //     1. Set member to the string-concatenation of member and the code unit 0x0020 (SPACE).
            // iv. Set member to the string-concatenation of member and strP.
            const member = try std.fmt.allocPrint(
                agent.gc_allocator,
                "{}:{s}{}",
                .{
                    try quoteJSONString(
                        agent,
                        (try property_key.toStringOrSymbol(agent)).string,
                    ),
                    if (!state.gap.isEmpty()) " " else "",
                    str_property.?,
                },
            );

            // v. Append member to partial.
            try partial.append(member);
        }
    }

    // 9. If partial is empty, then
    const final = if (partial.items.len == 0) blk: {
        // a. Let final be "{}".
        break :blk String.fromLiteral("{}");
    }
    // 10. Else,
    else blk: {
        // a. If state.[[Gap]] is the empty String, then
        if (state.gap.isEmpty()) {
            // i. Let properties be the String value formed by concatenating all the element
            //    Strings of partial with each adjacent pair of Strings separated with the code
            //    unit 0x002C (COMMA). A comma is not inserted either before the first String or
            //    after the last String.
            const properties = try std.mem.join(agent.gc_allocator, ",", partial.items);

            // ii. Let final be the string-concatenation of "{", properties, and "}".
            break :blk String.fromUtf8(
                agent.gc_allocator,
                try std.fmt.allocPrint(agent.gc_allocator, "{{{s}}}", .{properties}),
            );
        }
        // b. Else,
        else {
            // i. Let separator be the string-concatenation of the code unit 0x002C (COMMA), the
            //    code unit 0x000A (LINE FEED), and state.[[Indent]].
            const separator = try std.fmt.allocPrint(agent.gc_allocator, ",\n{}", .{state.indent});

            // ii. Let properties be the String value formed by concatenating all the element
            //     Strings of partial with each adjacent pair of Strings separated with separator.
            //     The separator String is not inserted either before the first String or after the
            //     last String.
            const properties = try std.mem.join(agent.gc_allocator, separator, partial.items);

            // iii. Let final be the string-concatenation of "{", the code unit 0x000A (LINE FEED),
            //      state.[[Indent]], properties, the code unit 0x000A (LINE FEED), stepback, and "}".
            break :blk String.fromUtf8(
                agent.gc_allocator,
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "{{\n{}{s}\n{s}}}",
                    .{ state.indent, properties, stepback },
                ),
            );
        }
    };

    // 11. Remove the last element of state.[[Stack]].
    _ = state.stack.remove(value);

    // 12. Set state.[[Indent]] to stepback.
    state.indent = stepback;

    // 13. Return final.
    return final;
}

/// 25.5.2.6 SerializeJSONArray ( state, value )
/// https://tc39.es/ecma262/#sec-serializejsonarray
fn serializeJSONArray(
    agent: *Agent,
    state: *JSONSerialization,
    value: Object,
) error{ OutOfMemory, ExceptionThrown }!String {
    // 1. If state.[[Stack]] contains value, throw a TypeError exception because the structure is
    //    cyclical.
    if (state.stack.contains(value)) {
        return agent.throwException(.type_error, "Cannot serialize cyclic array to JSON", .{});
    }

    // 2. Append value to state.[[Stack]].
    try state.stack.put(value, {});

    // 3. Let stepback be state.[[Indent]].
    const stepback = state.indent;

    // 4. Set state.[[Indent]] to the string-concatenation of state.[[Indent]] and state.[[Gap]].
    state.indent = try String.concat(agent.gc_allocator, &.{ state.indent, state.gap });

    // 6. Let len be ? LengthOfArrayLike(value).
    const len = try value.lengthOfArrayLike();

    // 5. Let partial be a new empty List.
    var partial = try std.ArrayList([]const u8).initCapacity(agent.gc_allocator, @intCast(len));
    defer partial.deinit();

    // 7. Let index be 0.
    var index: u53 = 0;

    // 8. Repeat, while index < len,
    while (index < len) : (index += 1) {
        // a. Let strP be ? SerializeJSONProperty(state, ! ToString(𝔽(index)), value).
        const str_property = try serializeJSONProperty(
            agent,
            state,
            PropertyKey.from(index),
            value,
        );

        // b. If strP is undefined, then
        if (str_property == null) {
            // i. Append "null" to partial.
            partial.appendAssumeCapacity("null");
        }
        // c. Else,
        else {
            // i. Append strP to partial.
            partial.appendAssumeCapacity(try str_property.?.toUtf8(agent.gc_allocator));
        }

        // d. Set index to index + 1.
    }

    // 9. If partial is empty, then
    const final = if (partial.items.len == 0) blk: {
        // a. Let final be "[]".
        break :blk String.fromLiteral("[]");
    }
    // 10. Else,
    else blk: {
        // a. If state.[[Gap]] is the empty String, then
        if (state.gap.isEmpty()) {
            // i. Let properties be the String value formed by concatenating all the element
            //    Strings of partial with each adjacent pair of Strings separated with the code
            //    unit 0x002C (COMMA). A comma is not inserted either before the first String or
            //    after the last String.
            const properties = try std.mem.join(agent.gc_allocator, ",", partial.items);

            // ii. Let final be the string-concatenation of "[", properties, and "]".
            break :blk try String.fromUtf8(
                agent.gc_allocator,
                try std.fmt.allocPrint(agent.gc_allocator, "[{s}]", .{properties}),
            );
        }
        // b. Else,
        else {
            // i. Let separator be the string-concatenation of the code unit 0x002C (COMMA), the
            //    code unit 0x000A (LINE FEED), and state.[[Indent]].
            const separator = try std.fmt.allocPrint(
                agent.gc_allocator,
                ",\n{s}",
                .{state.indent},
            );

            // ii. Let properties be the String value formed by concatenating all the element
            //     Strings of partial with each adjacent pair of Strings separated with separator.
            //     The separator String is not inserted either before the first String or after the
            //     last String.
            const properties = try std.mem.join(agent.gc_allocator, separator, partial.items);

            // iii. Let final be the string-concatenation of "[", the code unit 0x000A (LINE FEED),
            //      state.[[Indent]], properties, the code unit 0x000A (LINE FEED), stepback, and "]".
            break :blk try String.fromUtf8(
                agent.gc_allocator,
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "[\n{s}{s}\n{s}]",
                    .{ state.indent, properties, stepback },
                ),
            );
        }
    };

    // 11. Remove the last element of state.[[Stack]].
    _ = state.stack.remove(value);

    // 12. Set state.[[Indent]] to stepback.
    state.indent = stepback;

    // 13. Return final.
    return final;
}

pub const JSON = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        // 25.5.3 JSON [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-json-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("JSON"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        try defineBuiltinFunction(object, "parse", parse, 2, realm);
        try defineBuiltinFunction(object, "stringify", stringify, 3, realm);
    }

    /// 25.5.1 JSON.parse ( text [ , reviver ] )
    /// https://tc39.es/ecma262/#sec-json.parse
    fn parse(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const text = arguments.get(0);
        const reviver = arguments.get(1);

        // 1. Let jsonString be ? ToString(text).
        const json_string = try text.toString(agent);

        // 2. Parse StringToCodePoints(jsonString) as a JSON text as specified in ECMA-404. Throw
        //    a SyntaxError exception if it is not a valid JSON text as defined in that specification.
        // 3. Let scriptString be the string-concatenation of "(", jsonString, and ");".
        // 4. Let script be ParseText(scriptString, Script).
        // 5. NOTE: The early error rules defined in 13.2.5.1 have special handling for the above
        //    invocation of ParseText.
        // 6. Assert: script is a Parse Node.
        // 7. Let completion be Completion(Evaluation of script).
        // 8. NOTE: The PropertyDefinitionEvaluation semantics defined in 13.2.5.5 have special
        //    handling for the above evaluation.
        const completion = std.json.parseFromSlice(
            std.json.Value,
            agent.gc_allocator,
            try json_string.toUtf8(agent.gc_allocator),
            .{},
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return agent.throwException(.syntax_error, "Invalid JSON document", .{}),
        };
        defer completion.deinit();

        // 9. Let unfiltered be completion.[[Value]].
        const unfiltered = completion.value;

        // 10. Assert: unfiltered is either a String, a Number, a Boolean, an Object that is
        //     defined by either an ArrayLiteral or an ObjectLiteral, or null.

        // 11. If IsCallable(reviver) is true, then
        if (reviver.isCallable()) {
            // a. Let root be OrdinaryObjectCreate(%Object.prototype%).
            const root = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%Object.prototype%"(),
            );

            // b. Let rootName be the empty String.
            const root_name = PropertyKey.from("");

            // c. Perform ! CreateDataPropertyOrThrow(root, rootName, unfiltered).
            root.createDataPropertyOrThrow(
                root_name,
                try convertJsonValue(agent, unfiltered),
            ) catch |err| try noexcept(err);

            // d. Return ? InternalizeJSONProperty(root, rootName, reviver).
            return internalizeJSONProperty(agent, root, root_name, reviver.object);
        }
        // 12. Else,
        else {
            // a. Return unfiltered.
            return convertJsonValue(agent, unfiltered);
        }
    }

    /// 25.5.2 JSON.stringify ( value [ , replacer [ , space ] ] )
    /// https://tc39.es/ecma262/#sec-json.stringify
    fn stringify(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const value = arguments.get(0);
        const replacer = arguments.get(1);
        var space = arguments.get(2);

        // 1. Let stack be a new empty List.
        var stack = JSONSerialization.Stack.init(agent.gc_allocator);
        defer stack.deinit();

        // 2. Let indent be the empty String.
        const indent = String.empty;

        // 3. Let PropertyList be undefined.
        var property_list: ?PropertyKeyArrayHashMap(void) = null;
        defer if (property_list) |*p| p.deinit();

        // 4. Let ReplacerFunction be undefined.
        var replacer_function: ?Object = null;

        // 5. If replacer is an Object, then
        if (replacer == .object) {
            // a. If IsCallable(replacer) is true, then
            if (replacer.isCallable()) {
                // i. Set ReplacerFunction to replacer.
                replacer_function = replacer.object;
            }
            // b. Else,
            else {
                // i. Let isArray be ? IsArray(replacer).
                const is_array = try replacer.isArray();

                // ii. If isArray is true, then
                if (is_array) {
                    // 1. Set PropertyList to a new empty List.
                    property_list = PropertyKeyArrayHashMap(void).init(agent.gc_allocator);

                    // 2. Let len be ? LengthOfArrayLike(replacer).
                    const len = try replacer.object.lengthOfArrayLike();

                    // 3. Let k be 0.
                    var k: u53 = 0;

                    // 4. Repeat, while k < len,
                    while (k < len) : (k += 1) {
                        // a. Let prop be ! ToString(𝔽(k)).
                        const property_key = PropertyKey.from(k);

                        // b. Let v be ? Get(replacer, prop).
                        const k_value = try replacer.object.get(property_key);

                        // c. Let item be undefined.
                        var item: ?PropertyKey = null;

                        switch (k_value) {
                            // d. If v is a String, then
                            .string => |string| {
                                // i. Set item to v.
                                item = PropertyKey.from(string);
                            },

                            // e. Else if v is a Number, then
                            .number => |number| {
                                // i. Set item to ! ToString(v).
                                item = PropertyKey.from(
                                    try number.toString(agent.gc_allocator, 10),
                                );
                            },

                            // f. Else if v is an Object, then
                            .object => |object| {
                                // i. If v has a [[StringData]] or [[NumberData]] internal slot,
                                //    set item to ? ToString(v).
                                if (object.is(builtins.String) or object.is(builtins.Number)) {
                                    item = PropertyKey.from(try k_value.toString(agent));
                                }
                            },

                            else => {},
                        }

                        // g. If item is not undefined and PropertyList does not contain item, then
                        if (item != null and !property_list.?.contains(item.?)) {
                            // i. Append item to PropertyList.
                            try property_list.?.putNoClobber(item.?, {});
                        }

                        // h. Set k to k + 1.
                    }
                }
            }
        }

        // 6. If space is an Object, then
        if (space == .object) {
            // a. If space has a [[NumberData]] internal slot, then
            if (space.object.is(builtins.Number)) {
                // i. Set space to ? ToNumber(space).
                space = Value.from(try space.toNumber(agent));
            }
            // b. Else if space has a [[StringData]] internal slot, then
            else if (space.object.is(builtins.String)) {
                // i. Set space to ? ToString(space).
                space = Value.from(try space.toString(agent));
            }
        }

        const gap = switch (space) {
            // 7. If space is a Number, then
            .number => blk: {
                // a. Let spaceMV be ! ToIntegerOrInfinity(space).
                // b. Set spaceMV to min(10, spaceMV).
                const space_mv = @min(10, space.toIntegerOrInfinity(agent) catch unreachable);

                // c. If spaceMV < 1, let gap be the empty String; otherwise let gap be the String
                //    value containing spaceMV occurrences of the code unit 0x0020 (SPACE).
                if (space_mv < 1)
                    break :blk String.empty
                else {
                    const s = try agent.gc_allocator.alloc(u8, @intFromFloat(space_mv));
                    @memset(s, ' ');
                    break :blk String.fromAscii(s);
                }
            },

            // 8. Else if space is a String, then
            .string => |string| blk: {
                // a. If the length of space ≤ 10, let gap be space; otherwise let gap be the
                //    substring of space from 0 to 10.
                break :blk if (string.length() <= 10)
                    string
                else
                    try string.substring(agent.gc_allocator, 0, 10);
            },

            // 9. Else,
            else => blk: {
                // a. Let gap be the empty String.
                break :blk String.empty;
            },
        };

        // 10. Let wrapper be OrdinaryObjectCreate(%Object.prototype%).
        const wrapper = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 11. Perform ! CreateDataPropertyOrThrow(wrapper, the empty String, value).
        wrapper.createDataPropertyOrThrow(
            PropertyKey.from(""),
            value,
        ) catch |err| try noexcept(err);

        // 12. Let state be the JSON Serialization Record {
        //       [[ReplacerFunction]]: ReplacerFunction, [[Stack]]: stack, [[Indent]]: indent,
        //       [[Gap]]: gap, [[PropertyList]]: PropertyList
        //     }.
        var state: JSONSerialization = .{
            .replacer_function = replacer_function,
            .stack = stack,
            .indent = indent,
            .gap = gap,
            .property_list = property_list,
        };

        // 13. Return ? SerializeJSONProperty(state, the empty String, wrapper).
        return if (try serializeJSONProperty(agent, &state, PropertyKey.from(""), wrapper)) |string|
            Value.from(string)
        else
            .undefined;
    }
};
