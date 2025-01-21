//! 22.1 String Objects
//! https://tc39.es/ecma262/#sec-string-objects

const std = @import("std");

const build_options = @import("build-options");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const StringIterator = builtins.StringIterator;
const Value = types.Value;
const PropertyKey = types.PropertyKey;
const createArrayFromList = types.createArrayFromList;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const isCompatiblePropertyDescriptor = builtins.isCompatiblePropertyDescriptor;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const regExpCreate = builtins.regExpCreate;

pub const StringPadPlacement = enum { start, end };

/// 22.1.3.17.2 StringPad ( S, maxLength, fillString, placement )
/// https://tc39.es/ecma262/#sec-stringpad
pub fn stringPad(
    agent: *Agent,
    string: *const types.String,
    max_length: usize,
    fill_string: *const types.String,
    placement: StringPadPlacement,
) Agent.Error!*const types.String {
    // 1. Let stringLength be the length of S.
    const string_length = string.length();

    // 2. If maxLength ≤ stringLength, return S.
    if (max_length <= string_length) return string;

    // 3. If fillString is the empty String, return S.
    if (fill_string.isEmpty()) return string;

    // 4. Let fillLen be maxLength - stringLength.
    const fill_len = max_length - string_length;

    // 5. Let truncatedStringFiller be the String value consisting of repeated concatenations of
    //    fillString truncated to length fillLen.
    const truncated_string_filler = blk: {
        const fill_string_code_units = try fill_string.toUtf16(agent.gc_allocator);
        defer agent.gc_allocator.free(fill_string_code_units);

        const repeated_code_units = try agent.gc_allocator.alloc(u16, fill_len);

        var i: usize = 0;
        while (i < fill_len) : (i += fill_string_code_units.len) {
            const dest = repeated_code_units[i..@min(i + fill_string_code_units.len, fill_len)];
            @memcpy(dest, fill_string_code_units[0..dest.len]);
        }

        break :blk try types.String.fromUtf16(agent.gc_allocator, repeated_code_units);
    };

    switch (placement) {
        // 6. If placement is start, return the string-concatenation of truncatedStringFiller and S.
        .start => return types.String.concat(agent.gc_allocator, &.{
            truncated_string_filler,
            string,
        }),
        // 7. Else, return the string-concatenation of S and truncatedStringFiller.
        .end => return types.String.concat(agent.gc_allocator, &.{
            string,
            truncated_string_filler,
        }),
    }
}

/// 22.1.3.19.1 GetSubstitution ( matched, str, position, captures, namedCaptures, replacementTemplate )
/// https://tc39.es/ecma262/#sec-getsubstitution
pub fn getSubstitution(
    agent: *Agent,
    matched: *const types.String,
    str: *const types.String,
    position: usize,
    captures: []const ?*const types.String,
    named_captures: ?*Object,
    replacement_template: *const types.String,
) Agent.Error!*const types.String {
    // 1. Let stringLength be the length of str.
    const string_length = str.length();

    // 2. Assert: position ≤ stringLength.
    std.debug.assert(position <= string_length);

    // 3. Let result be the empty String.
    var result: types.String.Builder = .empty;
    defer result.deinit(agent.gc_allocator);

    // 4. Let templateRemainder be replacementTemplate.
    var template_reminder = replacement_template;

    // 5. Repeat, while templateRemainder is not the empty String,
    while (!template_reminder.isEmpty()) {
        // a. NOTE: The following steps isolate ref (a prefix of templateRemainder), determine
        //    refReplacement (its replacement), and then append that replacement to result.
        // b. If templateRemainder starts with "$$", then
        const ref, const ref_replacement = if (template_reminder.startsWith(types.String.fromLiteral("$$"))) blk: {
            // i. Let ref be "$$".
            const ref = types.String.fromLiteral("$$");

            // ii. Let refReplacement be "$".
            const ref_replacement = types.String.fromLiteral("$");

            break :blk .{ ref, ref_replacement };
        }
        // c. Else if templateRemainder starts with "$`", then
        else if (template_reminder.startsWith(types.String.fromLiteral("$`"))) blk: {
            // i. Let ref be "$`".
            const ref = types.String.fromLiteral("$`");

            // ii. Let refReplacement be the substring of str from 0 to position.
            const ref_replacement = try str.substring(agent.gc_allocator, 0, position);

            break :blk .{ ref, ref_replacement };
        }
        // d. Else if templateRemainder starts with "$&", then
        else if (template_reminder.startsWith(types.String.fromLiteral("$&"))) blk: {
            // i. Let ref be "$&".
            const ref = types.String.fromLiteral("$&");

            // ii. Let refReplacement be matched.
            const ref_replacement = matched;

            break :blk .{ ref, ref_replacement };
        }
        // e. Else if templateRemainder starts with "$'" (0x0024 (DOLLAR SIGN) followed by 0x0027 (APOSTROPHE)), then
        else if (template_reminder.startsWith(types.String.fromLiteral("$'"))) blk: {
            // i. Let ref be "$'".
            const ref = types.String.fromLiteral("$'");

            // ii. Let matchLength be the length of matched.
            const match_length = matched.length();

            // iii. Let tailPos be position + matchLength.
            const tail_pos = position +| match_length;

            // iv. Let refReplacement be the substring of str from min(tailPos, stringLength).
            // v. NOTE: tailPos can exceed stringLength only if this abstract operation was invoked
            //    by a call to the intrinsic %Symbol.replace% method of %RegExp.prototype% on an
            //    object whose "exec" property is not the intrinsic %RegExp.prototype.exec%.
            const ref_replacement = try str.substring(
                agent.gc_allocator,
                @min(tail_pos, string_length),
                null,
            );

            break :blk .{ ref, ref_replacement };
        }
        // f. Else if templateRemainder starts with "$" followed by 1 or more decimal digits, then
        else if (template_reminder.length() >= 2 and
            template_reminder.codeUnitAt(0) == '$' and
            std.ascii.isDigit(@truncate(template_reminder.codeUnitAt(1))))
        blk: {
            // i. If templateRemainder starts with "$" followed by 2 or more decimal digits, let
            //    digitCount be 2. Otherwise, let digitCount be 1.
            var digit_count: usize = if (template_reminder.length() >= 3 and
                std.ascii.isDigit(@truncate(template_reminder.codeUnitAt(1))) and
                std.ascii.isDigit(@truncate(template_reminder.codeUnitAt(2)))) 2 else 1;

            // ii. Let digits be the substring of templateRemainder from 1 to 1 + digitCount.
            var digits = (try template_reminder.substring(
                agent.gc_allocator,
                1,
                1 + digit_count,
            )).slice.ascii;

            // iii. Let index be ℝ(StringToNumber(digits)).
            var index = std.fmt.parseInt(usize, digits, 10) catch unreachable;

            // iv. Assert: 0 ≤ index ≤ 99.
            std.debug.assert(index <= 99);

            // v. Let captureLen be the number of elements in captures.
            const capture_len = captures.len;

            // vi. If index > captureLen and digitCount = 2, then
            if (index > capture_len and digit_count == 2) {
                // 1. NOTE: When a two-digit replacement pattern specifies an index exceeding the count
                //    of capturing groups, it is treated as a one-digit replacement pattern followed by
                //    a literal digit.

                // 2. Set digitCount to 1.
                digit_count = 1;

                // 3. Set digits to the substring of digits from 0 to 1.
                digits = digits[0..1];

                // 4. Set index to ℝ(StringToNumber(digits)).
                index = std.fmt.parseInt(usize, digits, 10) catch unreachable;
            }

            // vii. Let ref be the substring of templateRemainder from 0 to 1 + digitCount.
            const ref = try template_reminder.substring(agent.gc_allocator, 0, 1 + digit_count);

            // viii. If 1 ≤ index ≤ captureLen, then
            const ref_replacement: *const types.String = if (index >= 1 and index <= capture_len) blk_ref_replacement: {
                // 1. Let capture be captures[index - 1].
                const capture = captures[index - 1];

                // 2. If capture is undefined, then
                if (capture == null) {
                    // a. Let refReplacement be the empty String.
                    break :blk_ref_replacement .empty;
                }
                // 3. Else,
                else {
                    // a. Let refReplacement be capture.
                    break :blk_ref_replacement capture.?;
                }
            }
            // ix. Else,
            else blk_ref_replacement: {
                // 1. Let refReplacement be ref.
                break :blk_ref_replacement ref;
            };

            break :blk .{ ref, ref_replacement };
        }
        // g. Else if templateRemainder starts with "$<", then
        else if (template_reminder.startsWith(types.String.fromLiteral("$<"))) blk: {
            // i. Let gtPos be StringIndexOf(templateRemainder, ">", 0).
            const gt_pos = template_reminder.indexOf(types.String.fromLiteral(">"), 0);

            // ii. If gtPos is not-found or namedCaptures is undefined, then
            if (gt_pos == null or named_captures == null) {
                // 1. Let ref be "$<".
                const ref = types.String.fromLiteral("$<");

                // 2. Let refReplacement be ref.
                const ref_replacement = ref;

                break :blk .{ ref, ref_replacement };
            }
            // iii. Else,
            else {
                // 1. Let ref be the substring of templateRemainder from 0 to gtPos + 1.
                const ref = try template_reminder.substring(agent.gc_allocator, 0, gt_pos.? + 1);

                // 2. Let groupName be the substring of templateRemainder from 2 to gtPos.
                const group_name = try template_reminder.substring(
                    agent.gc_allocator,
                    2,
                    gt_pos.?,
                );

                // 3. Assert: namedCaptures is an Object.
                std.debug.assert(named_captures != null);

                // 4. Let capture be ? Get(namedCaptures, groupName).
                const capture = try named_captures.?.get(PropertyKey.from(group_name));

                // 5. If capture is undefined, then
                //     a. Let refReplacement be the empty String.
                // 6. Else,
                //     a. Let refReplacement be ? ToString(capture).
                const ref_replacement: *const types.String = if (capture.isUndefined())
                    .empty
                else
                    try capture.toString(agent);

                break :blk .{ ref, ref_replacement };
            }
        }
        // h. Else,
        else blk: {
            // i. Let ref be the substring of templateRemainder from 0 to 1.
            const ref = try template_reminder.substring(agent.gc_allocator, 0, 1);

            // ii. Let refReplacement be ref.
            const ref_replacement = ref;

            break :blk .{ ref, ref_replacement };
        };

        // i. Let refLength be the length of ref.
        const ref_length = ref.length();

        // j. Set templateRemainder to the substring of templateRemainder from refLength.
        template_reminder = try template_reminder.substring(
            agent.gc_allocator,
            ref_length,
            null,
        );

        // k. Set result to the string-concatenation of result and refReplacement.
        try result.appendString(agent.gc_allocator, ref_replacement);
    }

    // 6. Return result.
    return result.build(agent.gc_allocator);
}

/// 10.4.3.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-string-exotic-objects-getownproperty-p
fn getOwnProperty(
    object: *Object,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    // 1. Let desc be OrdinaryGetOwnProperty(S, P).
    const property_descriptor = ordinaryGetOwnProperty(object, property_key) catch unreachable;

    // 2. If desc is not undefined, return desc.
    if (property_descriptor != null) return property_descriptor;

    // 3. Return StringGetOwnProperty(S, P).
    return stringGetOwnProperty(object.as(String), property_key);
}

/// 10.4.3.2 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-string-exotic-objects-defineownproperty-p-desc
fn defineOwnProperty(
    object: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) std.mem.Allocator.Error!bool {
    const string = object.as(String);

    // 1. Let stringDesc be StringGetOwnProperty(S, P).
    const maybe_string_property_descriptor = try stringGetOwnProperty(string, property_key);

    // 2. If stringDesc is not undefined, then
    if (maybe_string_property_descriptor) |string_property_descriptor| {
        // a. Let extensible be S.[[Extensible]].
        const extensible = string.object.extensible();

        // b. Return IsCompatiblePropertyDescriptor(extensible, Desc, stringDesc).
        return isCompatiblePropertyDescriptor(
            extensible,
            property_descriptor,
            string_property_descriptor,
        );
    }

    // 3. Return ! OrdinaryDefineOwnProperty(S, P, Desc).
    return ordinaryDefineOwnProperty(
        object,
        property_key,
        property_descriptor,
    ) catch |err| try noexcept(err);
}

/// 10.4.3.3 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-string-exotic-objects-ownpropertykeys
fn ownPropertyKeys(object: *Object) std.mem.Allocator.Error!std.ArrayListUnmanaged(PropertyKey) {
    const agent = object.agent;

    // 2. Let str be O.[[StringData]].
    // 3. Assert: str is a String.
    const str = object.as(String).fields.string_data;

    // 4. Let len be the length of str.
    const len = str.length();

    // 1. Let keys be a new empty List.
    var keys = try std.ArrayListUnmanaged(PropertyKey).initCapacity(
        agent.gc_allocator,
        object.property_storage.count() + len,
    );

    // 5. For each integer i such that 0 ≤ i < len, in ascending order,
    for (0..len) |i| {
        // a. Append ! ToString(𝔽(i)) to keys.
        keys.appendAssumeCapacity(PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))));
    }

    // 6. For each own property key P of O such that P is an array index and
    //    ! ToIntegerOrInfinity(P) ≥ len, in ascending numeric index order, do
    //     a. Append P to keys.
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .sparse => |sparse| {
            var it = sparse.keyIterator();
            while (it.next()) |index| {
                if (index.* < len) continue;
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index.*)));
                keys.appendAssumeCapacity(property_key);
            }
            std.mem.sortUnstable(PropertyKey, keys.items, {}, struct {
                fn lessThanFn(_: void, a: PropertyKey, b: PropertyKey) bool {
                    return a.integer_index < b.integer_index;
                }
            }.lessThanFn);
        },
        else => {
            for (len..object.property_storage.indexed_properties.count()) |index| {
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index)));
                keys.appendAssumeCapacity(property_key);
            }
        },
    }

    // 7. For each own property key P of O such that P is a String and P is not an array index, in
    //    ascending chronological order of property creation, do
    for (object.property_storage.shape.properties.keys()) |property_key| {
        if (property_key == .string or property_key == .integer_index) {
            std.debug.assert(!property_key.isArrayIndex());

            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 8. For each own property key P of O such that P is a Symbol, in ascending chronological
    //    order of property creation, do
    for (object.property_storage.shape.properties.keys()) |property_key| {
        if (property_key == .symbol) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 9. Return keys.
    return keys;
}

/// 10.4.3.4 StringCreate ( value, prototype )
/// https://tc39.es/ecma262/#sec-stringcreate
pub fn stringCreate(
    agent: *Agent,
    value: *const types.String,
    prototype_: *Object,
) std.mem.Allocator.Error!*Object {
    // 1. Let S be MakeBasicObject(« [[Prototype]], [[Extensible]], [[StringData]] »).
    const string = try String.create(agent, .{
        // 2. Set S.[[Prototype]] to prototype.
        .prototype = prototype_,

        .fields = .{
            // 3. Set S.[[StringData]] to value.
            .string_data = value,
        },

        .internal_methods = &.{
            // 4. Set S.[[GetOwnProperty]] as specified in 10.4.3.1.
            .getOwnProperty = getOwnProperty,

            // 5. Set S.[[DefineOwnProperty]] as specified in 10.4.3.2.
            .defineOwnProperty = defineOwnProperty,

            // 6. Set S.[[OwnPropertyKeys]] as specified in 10.4.3.3.
            .ownPropertyKeys = ownPropertyKeys,
        },
    });

    // 7. Let length be the length of value.
    const length = value.length();

    // 8. Perform ! DefinePropertyOrThrow(S, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    string.definePropertyOrThrow(PropertyKey.from("length"), .{
        .value = Value.from(@as(u53, @intCast(length))),
        .writable = false,
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 9. Return S.
    return string;
}

/// 10.4.3.5 StringGetOwnProperty ( S, P )
/// https://tc39.es/ecma262/#sec-stringgetownproperty
fn stringGetOwnProperty(
    string: *const String,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    const agent = string.object.agent;

    // 1. If P is not a String, return undefined.
    // 2. Let index be CanonicalNumericIndexString(P).
    // 3. If index is not an integral Number, return undefined.
    // 4. If index is -0𝔽 or index < -0𝔽, return undefined.
    if (property_key != .integer_index) return null;
    if (property_key.integer_index > std.math.maxInt(usize) - 1) return null;
    const index: usize = @intCast(property_key.integer_index);

    // 5. Let str be S.[[StringData]].
    // 6. Assert: str is a String.
    const str = string.fields.string_data;

    // 7. Let len be the length of str.
    const len = str.length();

    // 8. If ℝ(index) ≥ len, return undefined.
    if (index >= len) return null;

    // 9. Let resultStr be the substring of str from ℝ(index) to ℝ(index) + 1.
    const result_str = try str.substring(agent.gc_allocator, index, index + 1);

    // 10. Return the PropertyDescriptor {
    //       [[Value]]: resultStr, [[Writable]]: false, [[Enumerable]]: true, [[Configurable]]: false
    //     }.
    return .{ .value = Value.from(result_str), .writable = false, .enumerable = true, .configurable = false };
}

/// 22.1.2 Properties of the String Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-string-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 1,
            .name = "String",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "fromCharCode", fromCharCode, 1, realm);
        try defineBuiltinFunction(object, "fromCodePoint", fromCodePoint, 1, realm);
        try defineBuiltinFunction(object, "raw", raw, 1, realm);

        // 22.1.2.3 String.prototype
        // https://tc39.es/ecma262/#sec-string.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%String.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 22.1.1.1 String ( value )
    /// https://tc39.es/ecma262/#sec-string-constructor-string-value
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const value = arguments.get(0);

        const s: *const types.String = blk: {
            // 1. If value is not present, then
            if (arguments.count() == 0) {
                // a. Let s be the empty String.
                break :blk .empty;
            }
            // 2. Else,
            else {
                // a. If NewTarget is undefined and value is a Symbol, return SymbolDescriptiveString(value).
                if (new_target == null and value.isSymbol()) {
                    return Value.from(try value.asSymbol().descriptiveString(agent));
                }

                // b. Let s be ? ToString(value).
                break :blk try value.toString(agent);
            }
        };

        // 3. If NewTarget is undefined, return s.
        if (new_target == null) return Value.from(s);

        // 4. Return StringCreate(s, ? GetPrototypeFromConstructor(NewTarget, "%String.prototype%")).
        return Value.from(try stringCreate(
            agent,
            s,
            try getPrototypeFromConstructor(new_target.?, "%String.prototype%"),
        ));
    }

    /// 22.1.2.1 String.fromCharCode ( ...codeUnits )
    /// https://tc39.es/ecma262/#sec-string.fromcharcode
    fn fromCharCode(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let result be the empty String.
        // NOTE: This allocates the exact needed capacity upfront
        var result = try types.String.Builder.initCapacity(agent.gc_allocator, arguments.count());
        defer result.deinit(agent.gc_allocator);

        // 2. For each element next of codeUnits, do
        for (arguments.values) |next| {
            // a. Let nextCU be the code unit whose numeric value is ℝ(? ToUint16(next)).
            const next_code_unit = try next.toUint16(agent);

            // b. Set result to the string-concatenation of result and nextCU.
            result.appendCodeUnitAssumeCapacity(next_code_unit);
        }

        // 3. Return result.
        return Value.from(try result.build(agent.gc_allocator));
    }

    /// 22.1.2.2 String.fromCodePoint ( ...codePoints )
    /// https://tc39.es/ecma262/#sec-string.fromcharcode
    fn fromCodePoint(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let result be the empty String.
        // NOTE: This allocates the exact needed capacity upfront
        var result = try types.String.Builder.initCapacity(agent.gc_allocator, arguments.count());
        defer result.deinit(agent.gc_allocator);

        // 2. For each element next of codePoints, do
        for (arguments.values) |next| {
            // a. Let nextCP be ? ToNumber(next).
            const next_code_point = try next.toNumber(agent);

            // b. If nextCP is not an integral Number, throw a RangeError exception.
            if (!next_code_point.isIntegral()) {
                return agent.throwException(
                    .range_error,
                    "Cannot convert non-integral number to code point",
                    .{},
                );
            }

            // c. If ℝ(nextCP) < 0 or ℝ(nextCP) > 0x10FFFF, throw a RangeError exception.
            if (next_code_point.asFloat() < 0 or next_code_point.asFloat() > 0x10FFFF) {
                return agent.throwException(
                    .range_error,
                    "Invalid code point {}",
                    .{next_code_point},
                );
            }

            // d. Set result to the string-concatenation of result and UTF16EncodeCodePoint(ℝ(nextCP)).
            result.appendCodePointAssumeCapacity(@intFromFloat(next_code_point.asFloat()));
        }

        // 3. Assert: If codePoints is empty, then result is the empty String.
        // 4. Return result.
        return Value.from(try result.build(agent.gc_allocator));
    }

    /// 22.1.2.4 String.raw ( template, ...substitutions )
    /// https://tc39.es/ecma262/#sec-string.raw
    fn raw(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const template = arguments.get(0);
        const substitutions = if (arguments.count() <= 1) &[_]Value{} else arguments.values[1..];

        // 1. Let substitutionCount be the number of elements in substitutions.
        const substitution_count = substitutions.len;

        // 2. Let cooked be ? ToObject(template).
        const cooked = try template.toObject(agent);

        // 3. Let literals be ? ToObject(? Get(cooked, "raw")).
        const literals = try (try cooked.get(PropertyKey.from("raw"))).toObject(agent);

        // 4. Let literalCount be ? LengthOfArrayLike(literals).
        const literal_count = try literals.lengthOfArrayLike();

        // 5. If literalCount ≤ 0, return the empty String.
        if (literal_count == 0) return Value.from("");

        // 6. Let R be the empty String.
        var result: types.String.Builder = .empty;
        defer result.deinit(agent.gc_allocator);

        // 7. Let nextIndex be 0.
        var next_index: u53 = 0;

        // 8. Repeat,
        while (true) : (next_index += 1) {
            // a. Let nextLiteralVal be ? Get(literals, ! ToString(𝔽(nextIndex))).
            const next_literal_value = try literals.get(PropertyKey.from(next_index));

            // b. Let nextLiteral be ? ToString(nextLiteralVal).
            const next_literal = try next_literal_value.toString(agent);

            // c. Set R to the string-concatenation of R and nextLiteral.
            try result.appendString(agent.gc_allocator, next_literal);

            // d. If nextIndex + 1 = literalCount, return R.
            if (next_index + 1 == literal_count) return Value.from(try result.build(agent.gc_allocator));

            // e. If nextIndex < substitutionCount, then
            if (next_index < substitution_count) {
                // i. Let nextSubVal be substitutions[nextIndex].
                const next_substitution_value = substitutions[@intCast(next_index)];

                // ii. Let nextSub be ? ToString(nextSubVal).
                const next_substitution = try next_substitution_value.toString(agent);

                // iii. Set R to the string-concatenation of R and nextSub.
                try result.appendString(agent.gc_allocator, next_substitution);
            }

            // f. Set nextIndex to nextIndex + 1.
        }
    }
};

/// 22.1.3 Properties of the String Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return stringCreate(realm.agent, .empty, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "at", at, 1, realm);
        try defineBuiltinFunction(object, "charAt", charAt, 1, realm);
        try defineBuiltinFunction(object, "charCodeAt", charCodeAt, 1, realm);
        try defineBuiltinFunction(object, "codePointAt", codePointAt, 1, realm);
        try defineBuiltinFunction(object, "concat", concat, 1, realm);
        try defineBuiltinFunction(object, "endsWith", endsWith, 1, realm);
        try defineBuiltinFunction(object, "includes", includes, 1, realm);
        try defineBuiltinFunction(object, "indexOf", indexOf, 1, realm);
        try defineBuiltinFunction(object, "isWellFormed", isWellFormed, 0, realm);
        try defineBuiltinFunction(object, "lastIndexOf", lastIndexOf, 1, realm);
        try defineBuiltinFunction(object, "localeCompare", localeCompare, 1, realm);
        try defineBuiltinFunction(object, "match", match, 1, realm);
        try defineBuiltinFunction(object, "matchAll", matchAll, 1, realm);
        try defineBuiltinFunction(object, "padEnd", padEnd, 1, realm);
        try defineBuiltinFunction(object, "padStart", padStart, 1, realm);
        try defineBuiltinFunction(object, "repeat", repeat, 1, realm);
        try defineBuiltinFunction(object, "replace", replace, 2, realm);
        try defineBuiltinFunction(object, "replaceAll", replaceAll, 2, realm);
        try defineBuiltinFunction(object, "search", search, 1, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);
        try defineBuiltinFunction(object, "split", split, 2, realm);
        try defineBuiltinFunction(object, "startsWith", startsWith, 1, realm);
        try defineBuiltinFunction(object, "substring", substring, 2, realm);
        try defineBuiltinFunction(object, "toLocaleLowerCase", toLocaleLowerCase, 0, realm);
        try defineBuiltinFunction(object, "toLocaleUpperCase", toLocaleUpperCase, 0, realm);
        try defineBuiltinFunction(object, "toLowerCase", toLowerCase, 0, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "toUpperCase", toUpperCase, 0, realm);
        try defineBuiltinFunction(object, "toWellFormed", toWellFormed, 0, realm);
        try defineBuiltinFunction(object, "trim", trim, 0, realm);
        try defineBuiltinFunction(object, "trimEnd", trimEnd, 0, realm);
        try defineBuiltinFunction(object, "trimStart", trimStart, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);
        try defineBuiltinFunction(object, "%Symbol.iterator%", @"%Symbol.iterator%", 0, realm);

        // 22.1.3.6 String.prototype.constructor
        // https://tc39.es/ecma262/#sec-string.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%String%"()),
        );

        if (build_options.enable_annex_b) {
            try defineBuiltinFunction(object, "substr", substr, 2, realm);
            try defineBuiltinFunction(object, "anchor", anchor, 1, realm);
            try defineBuiltinFunction(object, "big", big, 0, realm);
            try defineBuiltinFunction(object, "blink", blink, 0, realm);
            try defineBuiltinFunction(object, "bold", bold, 0, realm);
            try defineBuiltinFunction(object, "fixed", fixed, 0, realm);
            try defineBuiltinFunction(object, "fontcolor", fontcolor, 1, realm);
            try defineBuiltinFunction(object, "fontsize", fontsize, 1, realm);
            try defineBuiltinFunction(object, "italics", italics, 0, realm);
            try defineBuiltinFunction(object, "link", link, 1, realm);
            try defineBuiltinFunction(object, "small", small, 0, realm);
            try defineBuiltinFunction(object, "strike", strike, 0, realm);
            try defineBuiltinFunction(object, "sub", sub, 0, realm);
            try defineBuiltinFunction(object, "sup", sup, 0, realm);

            // B.2.2.15 String.prototype.trimLeft ( )
            // https://tc39.es/ecma262/#String.prototype.trimleft
            const @"%String.prototype.trimStart%" = object.getPropertyValueDirect(PropertyKey.from("trimStart"));
            try defineBuiltinProperty(object, "trimLeft", @"%String.prototype.trimStart%");

            // B.2.2.16 String.prototype.trimRight ( )
            // https://tc39.es/ecma262/#String.prototype.trimright
            const @"%String.prototype.trimEnd%" = object.getPropertyValueDirect(PropertyKey.from("trimEnd"));
            try defineBuiltinProperty(object, "trimRight", @"%String.prototype.trimEnd%");
        }
    }

    /// 22.1.3.35.1 ThisStringValue ( value )
    /// https://tc39.es/ecma262/#sec-thisstringvalue
    fn thisStringValue(agent: *Agent, value: Value) error{ExceptionThrown}!*const types.String {
        // 1. If value is a String, return value.
        if (value.isString()) return value.asString();

        // 2. If value is an Object and value has a [[StringData]] internal slot, then
        if (value.isObject() and value.asObject().is(String)) {
            // a. Let s be value.[[StringData]].
            // b. Assert: s is a String.
            const s = value.asObject().as(String).fields.string_data;

            // c. Return s.
            return s;
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a string or String object",
            .{},
        );
    }

    /// 22.1.3.1 String.prototype.at ( index )
    /// https://tc39.es/ecma262/#sec-string.prototype.at
    fn at(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const index = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let len be the length of S.
        const len = string.length();

        // 4. Let relativeIndex be ? ToIntegerOrInfinity(index).
        const relative_index = try index.toIntegerOrInfinity(agent);

        // 5. If relativeIndex ≥ 0, then
        //     a. Let k be relativeIndex.
        // 6. Else,
        //     a. Let k be len + relativeIndex.
        const k_f64 = if (relative_index >= 0)
            relative_index
        else
            @as(f64, @floatFromInt(len)) + relative_index;

        // 7. If k < 0 or k ≥ len, return undefined.
        if (k_f64 < 0 or k_f64 >= @as(f64, @floatFromInt(len))) return .undefined;
        const k: usize = @intFromFloat(k_f64);

        // 8. Return the substring of S from k to k + 1.
        return Value.from(try string.substring(agent.gc_allocator, k, k + 1));
    }

    /// 22.1.3.2 String.prototype.charAt ( pos )
    /// https://tc39.es/ecma262/#sec-string.prototype.charat
    fn charAt(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const pos = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let position be ? ToIntegerOrInfinity(pos).
        const position_f64 = try pos.toIntegerOrInfinity(agent);

        // 4. Let size be the length of S.
        const size = string.length();

        // 5. If position < 0 or position ≥ size, return the empty String.
        if (position_f64 < 0 or position_f64 >= @as(f64, @floatFromInt(size))) return Value.from("");
        const position: usize = @intFromFloat(position_f64);

        // 6. Return the substring of S from position to position + 1.
        return Value.from(try string.substring(agent.gc_allocator, position, position + 1));
    }

    /// 22.1.3.3 String.prototype.charCodeAt ( pos )
    /// https://tc39.es/ecma262/#sec-string.prototype.charcodeat
    fn charCodeAt(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const pos = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let position be ? ToIntegerOrInfinity(pos).
        const position_f64 = try pos.toIntegerOrInfinity(agent);

        // 4. Let size be the length of S.
        const size = string.length();

        // 5. If position < 0 or position ≥ size, return NaN.
        if (position_f64 < 0 or position_f64 >= @as(f64, @floatFromInt(size))) return .nan;
        const position: usize = @intFromFloat(position_f64);

        // 6. Return the Number value for the numeric value of the code unit at index position
        //    within the String S.
        return Value.from(string.codeUnitAt(position));
    }

    /// 22.1.3.4 String.prototype.codePointAt ( pos )
    /// https://tc39.es/ecma262/#sec-string.prototype.codepointat
    fn codePointAt(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const pos = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let position be ? ToIntegerOrInfinity(pos).
        const position_f64 = try pos.toIntegerOrInfinity(agent);

        // 4. Let size be the length of S.
        const size = string.length();

        // 5. If position < 0 or position ≥ size, return undefined.
        if (position_f64 < 0 or position_f64 >= @as(f64, @floatFromInt(size))) return .undefined;
        const position: usize = @intFromFloat(position_f64);

        // 6. Let cp be CodePointAt(S, position).
        const code_point = string.codePointAt(position);

        // 7. Return 𝔽(cp.[[CodePoint]]).
        return Value.from(code_point.code_point);
    }

    /// 22.1.3.5 String.prototype.concat ( ...args )
    /// https://tc39.es/ecma262/#sec-string.prototype.concat
    fn concat(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let R be S.
        // NOTE: This allocates the exact needed capacity upfront
        var result = try types.String.Builder.initCapacity(agent.gc_allocator, arguments.count() + 1);
        defer result.deinit(agent.gc_allocator);
        result.appendStringAssumeCapacity(string);

        // 4. For each element next of args, do
        for (arguments.values) |next| {
            // a. Let nextString be ? ToString(next).
            const next_string = try next.toString(agent);

            // b. Set R to the string-concatenation of R and nextString.
            result.appendStringAssumeCapacity(next_string);
        }

        // 5. Return R.
        return Value.from(try result.build(agent.gc_allocator));
    }

    /// 22.1.3.7 String.prototype.endsWith ( searchString [ , endPosition ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.endswith
    fn endsWith(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_string = arguments.get(0);
        const end_position = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let isRegExp be ? IsRegExp(searchString).
        const is_regexp = try search_string.isRegExp();

        // 4. If isRegExp is true, throw a TypeError exception.
        if (is_regexp) {
            return agent.throwException(
                .type_error,
                "String.prototype.endsWith() argument must not be a regular expression",
                .{},
            );
        }

        // 5. Let searchStr be ? ToString(searchString).
        const search_str = try search_string.toString(agent);

        // 6. Let len be the length of S.
        const len = string.length();

        // 7. If endPosition is undefined, let pos be len; else let pos be ? ToIntegerOrInfinity(endPosition).
        const pos = if (end_position.isUndefined())
            @as(f64, @floatFromInt(len))
        else
            try end_position.toIntegerOrInfinity(agent);

        // 8. Let end be the result of clamping pos between 0 and len.
        const end = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 9. Let searchLength be the length of searchStr.
        const search_length = search_str.length();

        // 10. If searchLength = 0, return true.
        if (search_length == 0) return Value.from(true);

        // 11. Let start be end - searchLength.
        // 12. If start < 0, return false.
        const start = std.math.sub(usize, end, search_length) catch return Value.from(false);

        // 13. Let substring be the substring of S from start to end.
        const substring_ = try string.substring(agent.gc_allocator, start, end);

        // 14. If substring is searchStr, return true.
        if (substring_.eql(search_str)) return Value.from(true);

        // 15. Return false.
        return Value.from(false);
    }

    /// 22.1.3.8 String.prototype.includes ( searchString [ , position ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.includes
    fn includes(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_string = arguments.get(0);
        const position = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let isRegExp be ? IsRegExp(searchString).
        const is_regexp = try search_string.isRegExp();

        // 4. If isRegExp is true, throw a TypeError exception.
        if (is_regexp) {
            return agent.throwException(
                .type_error,
                "String.prototype.includes() argument must not be a regular expression",
                .{},
            );
        }

        // 5. Let searchStr be ? ToString(searchString).
        const search_str = try search_string.toString(agent);

        // 6. Let pos be ? ToIntegerOrInfinity(position).
        // 7. Assert: If position is undefined, then pos is 0.
        const pos = try position.toIntegerOrInfinity(agent);

        // 8. Let len be the length of S.
        const len = string.length();

        // 9. Let start be the result of clamping pos between 0 and len.
        const start = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 10. Let index be StringIndexOf(S, searchStr, start).
        const index = string.indexOf(search_str, start);

        // 11. If index is not-found, return false.
        // 12. Return true.
        return Value.from(index != null);
    }

    /// 22.1.3.9 String.prototype.indexOf ( searchString [ , position ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.indexof
    fn indexOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_string = arguments.get(0);
        const position = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let searchStr be ? ToString(searchString).
        const search_str = try search_string.toString(agent);

        // 4. Let pos be ? ToIntegerOrInfinity(position).
        // 5. Assert: If position is undefined, then pos is 0.
        const pos = try position.toIntegerOrInfinity(agent);

        // 6. Let len be the length of S.
        const len = string.length();

        // 7. Let start be the result of clamping pos between 0 and len.
        const start = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 8. Let result be StringIndexOf(S, searchStr, start).
        // 9. If result is not-found, return -1𝔽.
        // 10. Return 𝔽(result).
        return if (string.indexOf(search_str, start)) |result|
            Value.from(@as(u53, @intCast(result)))
        else
            Value.from(-1);
    }

    /// 22.1.3.10 String.prototype.isWellFormed ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.iswellformed
    fn isWellFormed(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Return IsStringWellFormedUnicode(S).
        return Value.from(string.isWellFormedUnicode());
    }

    /// 22.1.3.11 String.prototype.lastIndexOf ( searchString [ , position ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.lastindexof
    fn lastIndexOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_string = arguments.get(0);
        const position = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let searchStr be ? ToString(searchString).
        const search_str = try search_string.toString(agent);

        // 4. Let numPos be ? ToNumber(position).
        const num_pos = try position.toNumber(agent);

        // 5. Assert: If position is undefined, then numPos is NaN.
        // 6. If numPos is NaN, let pos be +∞; otherwise, let pos be ! ToIntegerOrInfinity(numPos).
        const pos = if (num_pos.isNan())
            std.math.inf(f64)
        else
            Value.from(num_pos).toIntegerOrInfinity(agent) catch unreachable;

        // 7. Let len be the length of S.
        const len = string.length();

        // 8. Let searchLen be the length of searchStr.
        const search_len = search_str.length();

        // 9. Let start be the result of clamping pos between 0 and len - searchLen.
        const start = std.math.clamp(
            std.math.lossyCast(usize, pos),
            0,
            std.math.sub(usize, len, search_len) catch return Value.from(-1),
        );

        // 10. Let result be StringLastIndexOf(S, searchStr, start).
        // 11. If result is not-found, return -1𝔽.
        // 12. Return 𝔽(result).
        return if (string.lastIndexOf(search_str, start)) |result|
            Value.from(@as(u53, @intCast(result)))
        else
            Value.from(-1);
    }

    /// 22.1.3.12 String.prototype.localeCompare ( that [ , reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.localecompare
    fn localeCompare(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const that = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let thatValue be ? ToString(that).
        const that_value = try that.toString(agent);

        const order = if (string.slice == .ascii and that_value.slice == .ascii) blk: {
            break :blk std.mem.order(u8, string.slice.ascii, that_value.slice.ascii);
        } else if (string.slice == .utf16 and that_value.slice == .utf16) blk: {
            break :blk std.mem.order(u16, string.slice.utf16, that_value.slice.utf16);
        } else if (string.slice == .ascii and that_value.slice == .utf16) blk: {
            const string_utf16 = try string.toUtf16(agent.gc_allocator);
            defer agent.gc_allocator.free(string_utf16);
            break :blk std.mem.order(u16, string_utf16, that_value.slice.utf16);
        } else if (string.slice == .utf16 and that_value.slice == .ascii) blk: {
            const that_value_utf16 = try that_value.toUtf16(agent.gc_allocator);
            defer agent.gc_allocator.free(that_value_utf16);
            break :blk std.mem.order(u16, string.slice.utf16, that_value_utf16);
        } else unreachable;
        return switch (order) {
            .lt => Value.from(-1),
            .gt => Value.from(1),
            .eq => Value.from(0),
        };
    }

    /// 22.1.3.13 String.prototype.match ( regexp )
    /// https://tc39.es/ecma262/#sec-string.prototype.match
    fn match(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const regexp = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If regexp is neither undefined nor null, then
        if (!regexp.isUndefined() and !regexp.isNull()) {
            // a. Let matcher be ? GetMethod(regexp, %Symbol.match%).
            const matcher = try regexp.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.match%"),
            );

            // b. If matcher is not undefined, then
            if (matcher != null) {
                // i. Return ? Call(matcher, regexp, « O »).
                return Value.from(matcher.?).callAssumeCallable(regexp, &.{object});
            }
        }

        // 3. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 4. Let rx be ? RegExpCreate(regexp, undefined).
        const rx = try regExpCreate(agent, regexp, .undefined);

        // 5. Return ? Invoke(rx, %Symbol.match%, « S »).
        return Value.from(rx).invoke(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.match%"),
            &.{Value.from(string)},
        );
    }

    /// 22.1.3.14 String.prototype.matchAll ( regexp )
    /// https://tc39.es/ecma262/#sec-string.prototype.matchall
    fn matchAll(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const regexp = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If regexp is neither undefined nor null, then
        if (!regexp.isUndefined() and !regexp.isNull()) {
            // a. Let isRegExp be ? IsRegExp(regexp).
            const is_reg_exp = try regexp.isRegExp();

            // b. If isRegExp is true, then
            if (is_reg_exp) {
                // i. Let flags be ? Get(regexp, "flags").
                const flags = try regexp.asObject().get(PropertyKey.from("flags"));

                // ii. Perform ? RequireObjectCoercible(flags).
                _ = try flags.requireObjectCoercible(agent);

                // iii. If ? ToString(flags) does not contain "g", throw a TypeError exception.
                if ((try flags.toString(agent)).indexOf(types.String.fromLiteral("g"), 0) == null) {
                    return agent.throwException(
                        .type_error,
                        "RegExp object must have the 'g' flag set",
                        .{},
                    );
                }
            }

            // c. Let matcher be ? GetMethod(regexp, %Symbol.matchAll%).
            const matcher = try regexp.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.matchAll%"),
            );

            // d. If matcher is not undefined, then
            if (matcher != null) {
                // i. Return ? Call(matcher, regexp, « O »).
                return Value.from(matcher.?).callAssumeCallable(regexp, &.{object});
            }
        }

        // 3. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 4. Let rx be ? RegExpCreate(regexp, "g").
        const rx = try regExpCreate(agent, regexp, Value.from("g"));

        // 5. Return ? Invoke(rx, %Symbol.matchAll%, « S »).
        return Value.from(rx).invoke(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.matchAll%"),
            &.{Value.from(string)},
        );
    }

    /// 22.1.3.16 String.prototype.padEnd ( maxLength [ , fillString ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.padend
    fn padEnd(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const max_length = arguments.get(0);
        const fill_string = arguments.get(1);

        // 1. let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Return ? StringPaddingBuiltinsImpl(O, maxLength, fillString, end).
        return Value.from(
            try stringPaddingBuiltinsImpl(agent, object, max_length, fill_string, .end),
        );
    }

    /// 22.1.3.17 String.prototype.padStart ( maxLength [ , fillString ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.padstart
    fn padStart(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const max_length = arguments.get(0);
        const fill_string = arguments.get(1);

        // 1. let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Return ? StringPaddingBuiltinsImpl(O, maxLength, fillString, start).
        return Value.from(
            try stringPaddingBuiltinsImpl(agent, object, max_length, fill_string, .start),
        );
    }

    /// 22.1.3.17.1 StringPaddingBuiltinsImpl ( O, maxLength, fillString, placement )
    /// https://tc39.es/ecma262/#sec-stringpaddingbuiltinsimpl
    fn stringPaddingBuiltinsImpl(
        agent: *Agent,
        object: Value,
        max_length: Value,
        fill_string_value: Value,
        placement: StringPadPlacement,
    ) Agent.Error!*const types.String {
        // 1. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 2. Let intMaxLength be ℝ(? ToLength(maxLength)).
        const int_max_length = try max_length.toLength(agent);

        // 3. Let stringLength be the length of S.
        const string_length = string.length();

        // 4. If intMaxLength ≤ stringLength, return S.
        if (int_max_length <= string_length) return string;

        // 5. If fillString is undefined, set fillString to the String value consisting solely of
        //    the code unit 0x0020 (SPACE).
        // 6. Else, set fillString to ? ToString(fillString).
        const fill_string = if (fill_string_value.isUndefined())
            types.String.fromLiteral(" ")
        else
            try fill_string_value.toString(agent);

        // 7. Return StringPad(S, intMaxLength, fillString, placement).
        return stringPad(
            agent,
            string,
            @intCast(int_max_length),
            fill_string,
            placement,
        );
    }

    /// 22.1.3.18 String.prototype.repeat ( count )
    /// https://tc39.es/ecma262/#sec-string.prototype.repeat
    fn repeat(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const count = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let n be ? ToIntegerOrInfinity(count).
        const n = try count.toIntegerOrInfinity(agent);

        // 4. If n < 0 or n = +∞, throw a RangeError exception.
        if (n < 0 or std.math.isPositiveInf(n)) {
            return agent.throwException(
                .range_error,
                "Repeat count must be a positive finite number",
                .{},
            );
        }

        // 5. If n = 0, return the empty String.
        if (n == 0) return Value.from("");

        if (string.isEmpty()) return Value.from("");

        // 6. Return the String value that is made from n copies of S appended together.
        const n_usize = std.math.lossyCast(usize, n);
        return Value.from(try string.repeat(agent.gc_allocator, n_usize));
    }

    /// 22.1.3.19 String.prototype.replace ( searchValue, replaceValue )
    /// https://tc39.es/ecma262/#sec-string.prototype.replace
    fn replace(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_value = arguments.get(0);
        var replace_value = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If searchValue is neither undefined nor null, then
        if (!search_value.isUndefined() and !search_value.isNull()) {
            // a. Let replacer be ? GetMethod(searchValue, %Symbol.replace%).
            const replacer = try search_value.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.replace%"),
            );

            // b. If replacer is not undefined, then
            if (replacer != null) {
                // i. Return ? Call(replacer, searchValue, « O, replaceValue »).
                return Value.from(replacer.?).callAssumeCallable(
                    search_value,
                    &.{ object, replace_value },
                );
            }
        }

        // 3. Let string be ? ToString(O).
        const string = try object.toString(agent);

        // 4. Let searchString be ? ToString(searchValue).
        const search_string = try search_value.toString(agent);

        // 5. Let functionalReplace be IsCallable(replaceValue).
        const functional_replace = replace_value.isCallable();

        // 6. If functionalReplace is false, then
        if (!functional_replace and !replace_value.isString()) {
            // a. Set replaceValue to ? ToString(replaceValue).
            replace_value = Value.from(try replace_value.toString(agent));
        }

        // 7. Let searchLength be the length of searchString.
        const search_length = search_string.length();

        // 8. Let position be StringIndexOf(string, searchString, 0).
        const position = string.indexOf(search_string, 0);

        // 9. If position is not-found, return string.
        if (position == null) return Value.from(string);

        // 10. Let preceding be the substring of string from 0 to position.
        const preceding = try string.substring(agent.gc_allocator, 0, position.?);

        // 11. Let following be the substring of string from position + searchLength.
        const following = try string.substring(
            agent.gc_allocator,
            position.? + search_length,
            null,
        );

        // 12. If functionalReplace is true, then
        const replacement = if (functional_replace) blk: {
            // a. Let replacement be ? ToString(? Call(replaceValue, undefined, « searchString,
            //    𝔽(position), string »)).
            break :blk try (try replace_value.call(
                agent,
                .undefined,
                &.{
                    Value.from(search_string),
                    Value.from(@as(f64, @floatFromInt(position.?))),
                    Value.from(string),
                },
            )).toString(agent);
        }
        // 13. Else,
        else blk: {
            // a. Assert: replaceValue is a String.
            std.debug.assert(replace_value.isString());

            // b. Let captures be a new empty List.
            // c. Let replacement be ! GetSubstitution(searchString, string, position, captures,
            //    undefined, replaceValue).
            break :blk getSubstitution(
                agent,
                search_string,
                string,
                position.?,
                &.{},
                null,
                replace_value.asString(),
            ) catch |err| try noexcept(err);
        };

        // 14. Return the string-concatenation of preceding, replacement, and following.
        return Value.from(
            try types.String.concat(agent.gc_allocator, &.{ preceding, replacement, following }),
        );
    }

    /// 22.1.3.20 String.prototype.replaceAll ( searchValue, replaceValue )
    /// https://tc39.es/ecma262/#sec-string.prototype.replaceall
    fn replaceAll(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_value = arguments.get(0);
        var replace_value = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If searchValue is neither undefined nor null, then
        if (!search_value.isUndefined() and !search_value.isNull()) {
            // a. Let isRegExp be ? IsRegExp(searchValue).
            const is_reg_exp = try search_value.isRegExp();

            // b. If isRegExp is true, then
            if (is_reg_exp) {
                // i. Let flags be ? Get(searchValue, "flags").
                const flags = try search_value.get(agent, PropertyKey.from("flags"));

                // ii. Perform ? RequireObjectCoercible(flags).
                _ = try flags.requireObjectCoercible(agent);

                // iii. If ? ToString(flags) does not contain "g", throw a TypeError exception.
                if ((try flags.toString(agent)).indexOf(types.String.fromLiteral("g"), 0) == null) {
                    return agent.throwException(
                        .type_error,
                        "RegExp object must have the 'g' flag set",
                        .{},
                    );
                }
            }

            // c. Let replacer be ? GetMethod(searchValue, %Symbol.replace%).
            const replacer = try search_value.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.replace%"),
            );

            // d. If replacer is not undefined, then
            if (replacer != null) {
                // i. Return ? Call(replacer, searchValue, « O, replaceValue »).
                return Value.from(replacer.?).callAssumeCallable(
                    search_value,
                    &.{ object, replace_value },
                );
            }
        }

        // 3. Let string be ? ToString(O).
        const string = try object.toString(agent);

        // 4. Let searchString be ? ToString(searchValue).
        const search_string = try search_value.toString(agent);

        // 5. Let functionalReplace be IsCallable(replaceValue).
        const functional_replace = replace_value.isCallable();

        // 6. If functionalReplace is false, then
        if (!functional_replace and !replace_value.isString()) {
            // a. Set replaceValue to ? ToString(replaceValue).
            replace_value = Value.from(try replace_value.toString(agent));
        }

        // 7. Let searchLength be the length of searchString.
        const search_length = search_string.length();

        // 8. Let advanceBy be max(1, searchLength).
        const advance_by = @max(1, search_length);

        // 9. Let matchPositions be a new empty List.
        var match_positions: std.ArrayListUnmanaged(usize) = .empty;
        defer match_positions.deinit(agent.gc_allocator);

        // 10. Let position be StringIndexOf(string, searchString, 0).
        var maybe_position = string.indexOf(search_string, 0);

        // 11. Repeat, while position is not not-found,
        while (maybe_position) |position| {
            // a. Append position to matchPositions.
            try match_positions.append(agent.gc_allocator, position);

            // b. Set position to StringIndexOf(string, searchString, position + advanceBy).
            maybe_position = string.indexOf(search_string, position + advance_by);
        }

        // 12. Let endOfLastMatch be 0.
        var end_of_last_match: usize = 0;

        // 13. Let result be the empty String.
        var result: *const types.String = .empty;

        // 14. For each element p of matchPositions, do
        for (match_positions.items) |position| {
            // a. Let preserved be the substring of string from endOfLastMatch to p.
            const preserved = try string.substring(agent.gc_allocator, end_of_last_match, position);

            // b. If functionalReplace is true, then
            const replacement = if (functional_replace) blk: {
                // i. Let replacement be ? ToString(? Call(replaceValue, undefined, « searchString,
                //    𝔽(p), string »)).
                break :blk try (try replace_value.callAssumeCallable(
                    .undefined,
                    &.{
                        Value.from(search_string),
                        Value.from(@as(f64, @floatFromInt(position))),
                        Value.from(string),
                    },
                )).toString(agent);
            }
            // c. Else,
            else blk: {
                // i. Assert: replaceValue is a String.
                std.debug.assert(replace_value.isString());

                // ii. Let captures be a new empty List.
                // iii. Let replacement be ! GetSubstitution(searchString, string, p, captures,
                //      undefined, replaceValue).
                break :blk getSubstitution(
                    agent,
                    search_string,
                    string,
                    position,
                    &.{},
                    null,
                    replace_value.asString(),
                ) catch |err| try noexcept(err);
            };

            // d. Set result to the string-concatenation of result, preserved, and replacement.
            result = try types.String.concat(agent.gc_allocator, &.{ result, preserved, replacement });

            // e. Set endOfLastMatch to p + searchLength.
            end_of_last_match = position + search_length;
        }

        // 15. If endOfLastMatch < the length of string, then
        if (end_of_last_match < string.length()) {
            // a. Set result to the string-concatenation of result and the substring of string from
            //    endOfLastMatch.
            result = try types.String.concat(
                agent.gc_allocator,
                &.{
                    result,
                    try string.substring(
                        agent.gc_allocator,
                        end_of_last_match,
                        null,
                    ),
                },
            );
        }

        // 16. Return result.
        return Value.from(result);
    }

    /// 22.1.3.21 String.prototype.search ( regexp )
    /// https://tc39.es/ecma262/#sec-string.prototype.search
    fn search(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const regexp = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If regexp is neither undefined nor null, then
        if (!regexp.isUndefined() and !regexp.isNull()) {
            // a. Let searcher be ? GetMethod(regexp, %Symbol.search%).
            const searcher = try regexp.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.search%"),
            );

            // b. If searcher is not undefined, then
            if (searcher != null) {
                // i. Return ? Call(searcher, regexp, « O »).
                return Value.from(searcher.?).callAssumeCallable(regexp, &.{object});
            }
        }

        // 3. Let string be ? ToString(O).
        const string = try object.toString(agent);

        // 4. Let rx be ? RegExpCreate(regexp, undefined).
        const rx = try regExpCreate(agent, regexp, .undefined);

        // 5. Return ? Invoke(rx, %Symbol.search%, « string »).
        return Value.from(rx).invoke(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.search%"),
            &.{Value.from(string)},
        );
    }

    /// 22.1.3.22 String.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-string.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let len be the length of S.
        const len = string.length();
        const len_f64: f64 = @floatFromInt(len);

        // 4. Let intStart be ? ToIntegerOrInfinity(start).
        const int_start = try start.toIntegerOrInfinity(agent);

        // 5. If intStart = -∞, let from be 0.
        const from_f64 = if (std.math.isNegativeInf(int_start)) blk: {
            break :blk 0;
        }
        // 6. Else if intStart < 0, let from be max(len + intStart, 0).
        else if (int_start < 0) blk: {
            break :blk @max(len_f64 + int_start, 0);
        }
        // 7. Else, let from be min(intStart, len).
        else blk: {
            break :blk @min(int_start, len_f64);
        };
        const from: u53 = @intFromFloat(from_f64);

        // 8. If end is undefined, let intEnd be len; else let intEnd be ? ToIntegerOrInfinity(end).
        const int_end = if (end.isUndefined())
            len_f64
        else
            try end.toIntegerOrInfinity(agent);

        // 9. If intEnd = -∞, let to be 0.
        const to_f64 = if (std.math.isNegativeInf(int_end)) blk: {
            break :blk 0;
        }
        // 10. Else if intEnd < 0, let to be max(len + intEnd, 0).
        else if (int_end < 0) blk: {
            break :blk @max(len_f64 + int_end, 0);
        }
        // 11. Else, let to be min(intEnd, len).
        else blk: {
            break :blk @min(int_end, len_f64);
        };
        const to: u53 = @intFromFloat(to_f64);

        // 12. If from ≥ to, return the empty String.
        if (from >= to) return Value.from("");

        // 13. Return the substring of S from from to to.
        return Value.from(
            try string.substring(
                agent.gc_allocator,
                std.math.lossyCast(usize, from),
                std.math.lossyCast(usize, to),
            ),
        );
    }

    /// 22.1.3.23 String.prototype.split ( separator, limit )
    /// https://tc39.es/ecma262/#sec-string.prototype.split
    fn split(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const separator_value = arguments.get(0);
        const limit_value = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If separator is neither undefined nor null, then
        if (!separator_value.isUndefined() and !separator_value.isNull()) {
            // a. Let splitter be ? GetMethod(separator, %Symbol.split%).
            const splitter = try separator_value.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.split%"),
            );

            // b. If splitter is not undefined, then
            if (splitter != null) {
                // i. Return ? Call(splitter, separator, « O, limit »).
                return Value.from(splitter.?).callAssumeCallable(
                    separator_value,
                    &.{ object, limit_value },
                );
            }
        }

        // 3. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 4. If limit is undefined, let lim be 2**32 - 1; else let lim be ℝ(? ToUint32(limit)).
        const limit = if (limit_value.isUndefined())
            std.math.maxInt(u32)
        else
            try limit_value.toUint32(agent);

        // 5. Let R be ? ToString(separator).
        const separator = try separator_value.toString(agent);

        // 6. If lim = 0, then
        if (limit == 0) {
            // a. Return CreateArrayFromList(« »).
            return Value.from(try createArrayFromList(agent, &.{}));
        }

        // 7. If separator is undefined, then
        if (separator_value.isUndefined()) {
            // a. Return CreateArrayFromList(« S »).
            return Value.from(try createArrayFromList(agent, &.{Value.from(string)}));
        }

        // 8. Let separatorLength be the length of R.
        const separator_length = separator.length();

        // 9. If separatorLength = 0, then
        if (separator_length == 0) {
            // a. Let strLen be the length of S.
            const str_len = string.length();

            // b. Let outLen be the result of clamping lim between 0 and strLen.
            const out_len = std.math.clamp(limit, 0, str_len);

            // c. Let head be the substring of S from 0 to outLen.
            const head = try string.substring(agent.gc_allocator, 0, out_len);

            // d. Let codeUnits be a List consisting of the sequence of code units that are the elements of head.
            const code_units = try head.toUtf16(agent.gc_allocator);

            // e. Return CreateArrayFromList(codeUnits).
            return Value.from(
                try createArrayFromListMapToValue(agent, u16, code_units, struct {
                    fn mapFn(agent_: *Agent, code_unit: u16) std.mem.Allocator.Error!Value {
                        const code_unit_string = if (code_unit > 0x7F) blk: {
                            var utf16 = try agent_.gc_allocator.alloc(u16, 1);
                            utf16[0] = code_unit;
                            break :blk try types.String.fromUtf16(agent_.gc_allocator, utf16);
                        } else blk: {
                            var ascii = try agent_.gc_allocator.alloc(u8, 1);
                            ascii[0] = @intCast(code_unit);
                            break :blk try types.String.fromAscii(agent_.gc_allocator, ascii);
                        };
                        return Value.from(code_unit_string);
                    }
                }.mapFn),
            );
        }

        // 10. If S is the empty String, return CreateArrayFromList(« S »).
        if (separator.isEmpty()) {
            return Value.from(try createArrayFromList(agent, &.{Value.from(string)}));
        }

        // 11. Let substrings be a new empty List.
        var substrings: std.ArrayListUnmanaged(*const types.String) = .empty;
        defer substrings.deinit(agent.gc_allocator);

        // 12. Let i be 0.
        var i: usize = 0;

        // 13. Let j be StringIndexOf(S, R, 0).
        var j = string.indexOf(separator, 0);

        // 14. Repeat, while j is not not-found,
        while (j != null) {
            // a. Let T be the substring of S from i to j.
            const tail = try string.substring(agent.gc_allocator, i, j.?);

            // b. Append T to substrings.
            try substrings.append(agent.gc_allocator, tail);

            // c. If the number of elements in substrings is lim, return CreateArrayFromList(substrings).
            if (substrings.items.len == limit) {
                return Value.from(
                    try createArrayFromListMapToValue(agent, *const types.String, substrings.items, struct {
                        fn mapFn(_: *Agent, string_: *const types.String) std.mem.Allocator.Error!Value {
                            return Value.from(string_);
                        }
                    }.mapFn),
                );
            }

            // d. Set i to j + separatorLength.
            i = j.? + separator_length;

            // e. Set j to StringIndexOf(S, R, i).
            j = string.indexOf(separator, i);
        }

        // 15. Let T be the substring of S from i.
        const tail = try string.substring(agent.gc_allocator, i, null);

        // 16. Append T to substrings.
        try substrings.append(agent.gc_allocator, tail);

        // 17. Return CreateArrayFromList(substrings).
        return Value.from(
            try createArrayFromListMapToValue(agent, *const types.String, substrings.items, struct {
                fn mapFn(_: *Agent, string_: *const types.String) std.mem.Allocator.Error!Value {
                    return Value.from(string_);
                }
            }.mapFn),
        );
    }

    /// 22.1.3.24 String.prototype.startsWith ( searchString [ , position ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.startswith
    fn startsWith(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_string = arguments.get(0);
        const position = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let isRegExp be ? IsRegExp(searchString).
        const is_regexp = try search_string.isRegExp();

        // 4. If isRegExp is true, throw a TypeError exception.
        if (is_regexp) {
            return agent.throwException(
                .type_error,
                "String.prototype.startsWith() argument must not be a regular expression",
                .{},
            );
        }

        // 5. Let searchStr be ? ToString(searchString).
        const search_str = try search_string.toString(agent);

        // 6. Let len be the length of S.
        const len = string.length();

        // 7. If position is undefined, let pos be 0; else let pos be ? ToIntegerOrInfinity(position).
        const pos = if (position.isUndefined()) 0 else try position.toIntegerOrInfinity(agent);

        // 8. Let start be the result of clamping pos between 0 and len.
        const start = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 9. Let searchLength be the length of searchStr.
        const search_length = search_str.length();

        // 10. If searchLength = 0, return true.
        if (search_length == 0) return Value.from(true);

        // 11. Let end be start + searchLength.
        const end = start +| search_length;

        // 12. If end > len, return false.
        if (end > len) return Value.from(false);

        // 13. Let substring be the substring of S from start to end.
        const substring_ = try string.substring(agent.gc_allocator, start, end);

        // 14. If substring is searchStr, return true.
        if (substring_.eql(search_str)) return Value.from(true);

        // 15. Return false.
        return Value.from(false);
    }

    /// 22.1.3.25 String.prototype.substring ( start, end )
    /// https://tc39.es/ecma262/#sec-string.prototype.substring
    fn substring(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.get(0);
        const end = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let len be the length of S.
        const len = string.length();

        // 4. Let intStart be ? ToIntegerOrInfinity(start).
        const int_start = try start.toIntegerOrInfinity(agent);

        // 5. If end is undefined, let intEnd be len; else let intEnd be ? ToIntegerOrInfinity(end).
        const int_end = if (end.isUndefined())
            @as(f64, @floatFromInt(len))
        else
            try end.toIntegerOrInfinity(agent);

        // 6. Let finalStart be the result of clamping intStart between 0 and len.
        const final_start = std.math.clamp(std.math.lossyCast(usize, int_start), 0, len);

        // 7. Let finalEnd be the result of clamping intEnd between 0 and len.
        const final_end = std.math.clamp(std.math.lossyCast(usize, int_end), 0, len);

        // 8. Let from be min(finalStart, finalEnd).
        const from = @min(final_start, final_end);

        // 9. Let to be max(finalStart, finalEnd).
        const to = @max(final_start, final_end);

        // 10. Return the substring of S from from to to.
        return Value.from(try string.substring(agent.gc_allocator, from, to));
    }

    /// 22.1.3.26 String.prototype.toLocaleLowerCase ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.tolocalelowercase
    fn toLocaleLowerCase(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const object = try this_value.requireObjectCoercible(agent);
        const string = try object.toString(agent);
        const lower = try string.toLowerCase(agent.gc_allocator);
        return Value.from(lower);
    }

    /// 22.1.3.27 String.prototype.toLocaleUpperCase ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.tolocaleuppercase
    fn toLocaleUpperCase(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const object = try this_value.requireObjectCoercible(agent);
        const string = try object.toString(agent);
        const lower = try string.toUpperCase(agent.gc_allocator);
        return Value.from(lower);
    }

    /// 22.1.3.28 String.prototype.toLowerCase ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.tolowercase
    fn toLowerCase(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let sText be StringToCodePoints(S).
        // 4. Let lowerText be toLowercase(sText), according to the Unicode Default
        //    Case Conversion algorithm.
        // 5. Let L be CodePointsToString(lowerText).
        const lower = try string.toLowerCase(agent.gc_allocator);

        // 6. Return L.
        return Value.from(lower);
    }

    /// 22.1.3.29 String.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisStringValue(this value).
        return Value.from(try thisStringValue(agent, this_value));
    }

    /// 22.1.3.30 String.prototype.toUpperCase ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.touppercase
    fn toUpperCase(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // NOTE: The spec simply references toLowerCase() for this, so the steps below are inferred.

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let sText be StringToCodePoints(S).
        // 4. Let upperText be toUppercase(sText), according to the Unicode Default
        //    Case Conversion algorithm.
        // 5. Let U be CodePointsToString(upperText).
        const upper = try string.toUpperCase(agent.gc_allocator);

        // 6. Return U.
        return Value.from(upper);
    }

    /// 22.1.3.31 String.prototype.toWellFormed ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.towellformed
    fn toWellFormed(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let strLen be the length of S.
        const str_len = string.length();

        // OPTIMIZATION: If the array is empty the result will be an empty string
        if (str_len == 0) return Value.from(types.String.empty);

        // 4. Let k be 0.
        var k: usize = 0;

        // 5. Let result be the empty String.
        // NOTE: This allocates the exact needed capacity upfront
        var result = try types.String.Builder.initCapacity(agent.gc_allocator, str_len);
        defer result.deinit(agent.gc_allocator);

        // 6. Repeat, while k < strLen,
        while (k < str_len) {
            // a. Let cp be CodePointAt(S, k).
            const code_point = string.codePointAt(k);

            // b. If cp.[[IsUnpairedSurrogate]] is true, then
            if (code_point.is_unpaired_surrogate) {
                // i. Set result to the string-concatenation of result and 0xFFFD
                //    (REPLACEMENT CHARACTER).
                result.appendCodePointAssumeCapacity(std.unicode.replacement_character);
            }
            // c. Else,
            else {
                // i. Set result to the string-concatenation of result and
                //    UTF16EncodeCodePoint(cp.[[CodePoint]]).
                result.appendCodePointAssumeCapacity(code_point.code_point);
            }

            // d. Set k to k + cp.[[CodeUnitCount]].
            k += code_point.code_unit_count;
        }

        // 7. Return result.
        return Value.from(try result.build(agent.gc_allocator));
    }

    /// 22.1.3.32 String.prototype.trim ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.trim
    fn trim(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? TrimString(S, start+end).
        return Value.from(try trimString(agent, this_value, .@"start+end"));
    }

    /// 22.1.3.32.1 TrimString ( string, where )
    /// https://tc39.es/ecma262/#sec-trimstring
    fn trimString(
        agent: *Agent,
        string_value: Value,
        where: enum { start, end, @"start+end" },
    ) Agent.Error!*const types.String {
        // 1. Let str be ? RequireObjectCoercible(string).
        const str = try string_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(str).
        const string = try str.toString(agent);

        const trimmed = switch (where) {
            // 3. If where is start, then
            .start => blk: {
                // a. Let T be the String value that is a copy of S with leading white space
                //    removed.
                break :blk try string.trim(agent.gc_allocator, .start);
            },

            // 4. Else if where is end, then
            .end => blk: {
                // a. Let T be the String value that is a copy of S with trailing white space
                //    removed.
                break :blk try string.trim(agent.gc_allocator, .end);
            },

            // 5. Else,
            //     a. Assert: where is start+end.
            .@"start+end" => blk: {
                // b. Let T be the String value that is a copy of S with both leading and trailing
                //    white space removed.
                break :blk try string.trim(agent.gc_allocator, .@"start+end");
            },
        };

        // 6. Return T.
        return trimmed;
    }

    /// 22.1.3.33 String.prototype.trimEnd ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.trimend
    fn trimEnd(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? TrimString(S, end).
        return Value.from(try trimString(agent, this_value, .end));
    }

    /// 22.1.3.34 String.prototype.trimStart ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.trimend
    fn trimStart(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? TrimString(S, start).
        return Value.from(try trimString(agent, this_value, .start));
    }

    /// 22.1.3.35 String.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisStringValue(this value).
        return Value.from(try thisStringValue(agent, this_value));
    }

    /// 22.1.3.36 String.prototype [ %Symbol.iterator% ] ( )
    /// https://tc39.es/ecma262/#sec-string.prototype-%symbol.iterator%
    fn @"%Symbol.iterator%"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let s be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let closure be a new Abstract Closure with no parameters that captures s and performs
        //    the following steps when called:
        //    [...]
        // 4. Return CreateIteratorFromClosure(closure, "%StringIteratorPrototype%", %StringIteratorPrototype%).
        return Value.from(try StringIterator.create(agent, .{
            .prototype = try realm.intrinsics.@"%StringIteratorPrototype%"(),
            .fields = .{
                .state = .{ .string = string, .position = 0 },
            },
        }));
    }

    /// B.2.2.1 String.prototype.substr ( start, length )
    /// https://tc39.es/ecma262/#sec-string.prototype.substr
    fn substr(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start = arguments.get(0);
        const length = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let size be the length of S.
        const size: f64 = @floatFromInt(string.length());

        // 4. Let intStart be ? ToIntegerOrInfinity(start).
        var int_start = try start.toIntegerOrInfinity(agent);

        // 5. If intStart = -∞, set intStart to 0.
        if (std.math.isNegativeInf(int_start)) {
            int_start = 0;
        }
        // 6. Else if intStart < 0, set intStart to max(size + intStart, 0).
        else if (int_start < 0) {
            int_start = @max(size + int_start, 0);
        }
        // 7. Else, set intStart to min(intStart, size).
        else {
            int_start = @min(int_start, size);
        }

        // 8. If length is undefined, let intLength be size; otherwise let intLength be ? ToIntegerOrInfinity(length).
        var int_length = if (length.isUndefined())
            size
        else
            try length.toIntegerOrInfinity(agent);

        // 9. Set intLength to the result of clamping intLength between 0 and size.
        int_length = std.math.clamp(int_length, 0, size);

        // 10. Let intEnd be min(intStart + intLength, size).
        const int_end = @min(int_start + int_length, size);

        // 11. Return the substring of S from intStart to intEnd.
        return Value.from(
            try string.substring(
                agent.gc_allocator,
                @intFromFloat(int_start),
                @intFromFloat(int_end),
            ),
        );
    }

    /// B.2.2.2.1 CreateHTML ( string, tag, attribute, value )
    /// https://tc39.es/ecma262/#sec-createhtml
    fn createHTML(
        agent: *Agent,
        string_value: Value,
        tag: []const u8,
        attribute: ?struct { name: []const u8, value: Value },
    ) Agent.Error!*const types.String {
        // 1. Let str be ? RequireObjectCoercible(string).
        _ = try string_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(str).
        const string = try string_value.toString(agent);

        // 3. Let p1 be the string-concatenation of "<" and tag.
        // 5. Let p2 be the string-concatenation of p1 and ">".
        // 6. Let p3 be the string-concatenation of p2 and S.
        // 7. Let p4 be the string-concatenation of p3, "</", tag, and ">".
        // 8. Return p4.

        // 4. If attribute is not the empty String, then
        if (attribute) |attr| {
            // a. Let V be ? ToString(value).
            const value_string = try attr.value.toString(agent);

            // b. Let escapedV be the String value that is the same as V except that each
            //    occurrence of the code unit 0x0022 (QUOTATION MARK) in V has been replaced with
            //    the six code unit sequence "&quot;".
            const value_string_escaped = try value_string.replace(
                agent.gc_allocator,
                "\"",
                "&quot;",
            );

            // c. Set p1 to the string-concatenation of:
            // - p1
            // - the code unit 0x0020 (SPACE)
            // - attribute
            // - the code unit 0x003D (EQUALS SIGN)
            // - the code unit 0x0022 (QUOTATION MARK)
            // - escapedV
            // - the code unit 0x0022 (QUOTATION MARK)
            return types.String.fromUtf8(
                agent.gc_allocator,
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "<{[tag]s} {[attribute]s}=\"{[value]s}\">{[string]}</{[tag]s}>",
                    .{
                        .string = string,
                        .tag = tag,
                        .attribute = attr.name,
                        .value = value_string_escaped,
                    },
                ),
            );
        }

        return types.String.fromUtf8(
            agent.gc_allocator,
            try std.fmt.allocPrint(
                agent.gc_allocator,
                "<{[tag]s}>{[string]}</{[tag]s}>",
                .{ .string = string, .tag = tag },
            ),
        );
    }

    /// B.2.2.2 String.prototype.anchor ( name )
    /// https://tc39.es/ecma262/#sec-string.prototype.anchor
    fn anchor(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const name = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "a", "name", name).
        return Value.from(
            try createHTML(agent, this_value, "a", .{ .name = "name", .value = name }),
        );
    }

    /// B.2.2.3 String.prototype.big ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.big
    fn big(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "big", "", "").
        return Value.from(try createHTML(agent, this_value, "big", null));
    }

    /// B.2.2.4 String.prototype.blink ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.blink
    fn blink(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "blink", "", "").
        return Value.from(try createHTML(agent, this_value, "blink", null));
    }

    /// B.2.2.5 String.prototype.bold ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.bold
    fn bold(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "b", "", "").
        return Value.from(try createHTML(agent, this_value, "b", null));
    }

    /// B.2.2.6 String.prototype.fixed ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.fixed
    fn fixed(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "tt", "", "").
        return Value.from(try createHTML(agent, this_value, "tt", null));
    }

    /// B.2.2.7 String.prototype.fontcolor ( colour )
    /// https://tc39.es/ecma262/#sec-string.prototype.fontcolor
    fn fontcolor(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const colour = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "font", "color", colour).
        return Value.from(
            try createHTML(agent, this_value, "font", .{ .name = "color", .value = colour }),
        );
    }

    /// B.2.2.8 String.prototype.fontsize ( size )
    /// https://tc39.es/ecma262/#sec-string.prototype.fontsize
    fn fontsize(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const size = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "font", "size", size).
        return Value.from(
            try createHTML(agent, this_value, "font", .{ .name = "size", .value = size }),
        );
    }

    /// B.2.2.9 String.prototype.italics ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.italics
    fn italics(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "i", "", "").
        return Value.from(try createHTML(agent, this_value, "i", null));
    }

    /// B.2.2.10 String.prototype.link ( url )
    /// https://tc39.es/ecma262/#sec-string.prototype.link
    fn link(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const url = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "a", "href", url).
        return Value.from(
            try createHTML(agent, this_value, "a", .{ .name = "href", .value = url }),
        );
    }

    /// B.2.2.11 String.prototype.small ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.small
    fn small(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "small", "", "").
        return Value.from(try createHTML(agent, this_value, "small", null));
    }

    /// B.2.2.12 String.prototype.strike ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.strike
    fn strike(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "strike", "", "").
        return Value.from(try createHTML(agent, this_value, "strike", null));
    }

    /// B.2.2.13 String.prototype.sub ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.sub
    fn sub(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "sub", "", "").
        return Value.from(try createHTML(agent, this_value, "sub", null));
    }

    /// B.2.2.14 String.prototype.sup ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.sup
    fn sup(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "sup", "", "").
        return Value.from(try createHTML(agent, this_value, "sup", null));
    }
};

/// 22.1.4 Properties of String Instances
/// https://tc39.es/ecma262/#sec-properties-of-string-instances
pub const String = MakeObject(.{
    .Fields = struct {
        /// [[StringData]]
        string_data: *const types.String,
    },
    .tag = .string,
});
