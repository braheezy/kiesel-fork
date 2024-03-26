//! 22.1 String Objects
//! https://tc39.es/ecma262/#sec-string-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

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
    string: types.String,
    max_length: usize,
    fill_string: types.String,
    placement: StringPadPlacement,
) Agent.Error!types.String {
    // 1. Let stringLength be the length of S.
    const string_length = string.utf16Length();

    // 2. If maxLength ≤ stringLength, return S.
    if (max_length <= string_length) return string;

    // 3. If fillString is the empty String, return S.
    if (fill_string.isEmpty()) return string;

    // 4. Let fillLen be maxLength - stringLength.
    const fill_len = max_length - string_length;

    // 5. Let truncatedStringFiller be the String value consisting of repeated concatenations of
    //    fillString truncated to length fillLen.
    const truncated_string_filler_utf8 = blk: {
        const fill_string_code_units = try fill_string.utf16CodeUnits(agent.gc_allocator);
        defer agent.gc_allocator.free(fill_string_code_units);

        const truncated_string_filler = try agent.gc_allocator.alloc(u16, fill_len);
        defer agent.gc_allocator.free(truncated_string_filler);

        var i: usize = 0;
        while (i < fill_len) : (i += fill_string_code_units.len) {
            const dest = truncated_string_filler[i..@min(i + fill_string_code_units.len, fill_len)];
            @memcpy(dest, fill_string_code_units[0..dest.len]);
        }

        break :blk std.unicode.utf16leToUtf8Alloc(
            agent.gc_allocator,
            truncated_string_filler,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DanglingSurrogateHalf,
            error.ExpectedSecondSurrogateHalf,
            error.UnexpectedSecondSurrogateHalf,
            => return agent.throwException(
                .internal_error,
                "UTF-16 strings are not implemented yet",
                .{},
            ),
        };
    };
    defer agent.gc_allocator.free(truncated_string_filler_utf8);

    // 6. If placement is start, return the string-concatenation of truncatedStringFiller and S.
    // 7. Else, return the string-concatenation of S and truncatedStringFiller.
    return types.String.from(
        try std.mem.concat(agent.gc_allocator, u8, switch (placement) {
            .start => &.{ truncated_string_filler_utf8, string.utf8 },
            .end => &.{ string.utf8, truncated_string_filler_utf8 },
        }),
    );
}

/// 10.4.3.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-string-exotic-objects-getownproperty-p
fn getOwnProperty(object: Object, property_key: PropertyKey) Allocator.Error!?PropertyDescriptor {
    // 1. Let desc be OrdinaryGetOwnProperty(S, P).
    const property_descriptor = ordinaryGetOwnProperty(object, property_key);

    // 2. If desc is not undefined, return desc.
    if (property_descriptor != null) return property_descriptor;

    // 3. Return StringGetOwnProperty(S, P).
    return stringGetOwnProperty(object.as(String), property_key);
}

/// 10.4.3.2 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-string-exotic-objects-defineownproperty-p-desc
fn defineOwnProperty(
    object: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Allocator.Error!bool {
    const string = object.as(String);

    // 1. Let stringDesc be StringGetOwnProperty(S, P).
    const maybe_string_property_descriptor = try stringGetOwnProperty(string, property_key);

    // 2. If stringDesc is not undefined, then
    if (maybe_string_property_descriptor) |string_property_descriptor| {
        // a. Let extensible be S.[[Extensible]].
        const extensible = string.data.extensible;

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
fn ownPropertyKeys(object: Object) Allocator.Error!std.ArrayList(PropertyKey) {
    const agent = object.agent();
    const property_storage_hash_map = object.propertyStorage().hash_map;

    // 2. Let str be O.[[StringData]].
    // 3. Assert: str is a String.
    const str = object.as(String).fields.string_data;

    // 4. Let len be the length of str.
    const len = str.utf16Length();

    // 1. Let keys be a new empty List.
    var keys = try std.ArrayList(PropertyKey).initCapacity(
        agent.gc_allocator,
        property_storage_hash_map.count() + len,
    );

    // 5. For each integer i such that 0 ≤ i < len, in ascending order,
    for (0..len) |i| {
        // a. Append ! ToString(𝔽(i)) to keys.
        keys.appendAssumeCapacity(PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))));
    }

    // 6. For each own property key P of O such that P is an array index and
    //    ! ToIntegerOrInfinity(P) ≥ len, in ascending numeric index order, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key.isArrayIndex() and property_key.integer_index >= len) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }
    std.mem.sortUnstable(PropertyKey, keys.items[len..], {}, struct {
        fn lessThanFn(_: void, a: PropertyKey, b: PropertyKey) bool {
            return a.integer_index < b.integer_index;
        }
    }.lessThanFn);

    // 7. For each own property key P of O such that P is a String and P is not an array index, in
    //    ascending chronological order of property creation, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key == .string or (property_key == .integer_index and !property_key.isArrayIndex())) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 8. For each own property key P of O such that P is a Symbol, in ascending chronological
    //    order of property creation, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
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
pub fn stringCreate(agent: *Agent, value: types.String, prototype: Object) Allocator.Error!Object {
    // 1. Let S be MakeBasicObject(« [[Prototype]], [[Extensible]], [[StringData]] »).
    const string = try String.create(agent, .{
        // 2. Set S.[[Prototype]] to prototype.
        .prototype = prototype,

        .fields = .{
            // 3. Set S.[[StringData]] to value.
            .string_data = value,
        },

        .internal_methods = .{
            // 4. Set S.[[GetOwnProperty]] as specified in 10.4.3.1.
            .getOwnProperty = getOwnProperty,

            // 5. Set S.[[DefineOwnProperty]] as specified in 10.4.3.2.
            .defineOwnProperty = defineOwnProperty,

            // 6. Set S.[[OwnPropertyKeys]] as specified in 10.4.3.3.
            .ownPropertyKeys = ownPropertyKeys,
        },
    });

    // 7. Let length be the length of value.
    const length = value.utf16Length();

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
) Allocator.Error!?PropertyDescriptor {
    const agent = string.data.agent;

    // 1. If P is not a String, return undefined.
    // 2. Let index be CanonicalNumericIndexString(P).
    // 3. If index is undefined, return undefined.
    // 4. If IsIntegralNumber(index) is false, return undefined.
    // 5. If index is -0𝔽, return undefined.
    if (property_key != .integer_index) return null;
    if (property_key.integer_index > std.math.maxInt(usize) - 1) return null;
    const index: usize = @intCast(property_key.integer_index);

    // 6. Let str be S.[[StringData]].
    // 7. Assert: str is a String.
    const str = string.fields.string_data;

    // 8. Let len be the length of str.
    const len = str.utf16Length();

    // 9. If ℝ(index) < 0 or len ≤ ℝ(index), return undefined.
    if (len <= index) return null;

    // 10. Let resultStr be the substring of str from ℝ(index) to ℝ(index) + 1.
    const result_str = try str.substring(agent.gc_allocator, index, index + 1);

    // 11. Return the PropertyDescriptor {
    //       [[Value]]: resultStr, [[Writable]]: false, [[Enumerable]]: true, [[Configurable]]: false
    //     }.
    return .{ .value = Value.from(result_str), .writable = false, .enumerable = true, .configurable = false };
}

/// 22.1.2 Properties of the String Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-string-constructor
pub const StringConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "String",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "fromCharCode", fromCharCode, 1, realm);
        try defineBuiltinFunction(object, "fromCodePoint", fromCodePoint, 1, realm);

        // 22.1.2.3 String.prototype
        // https://tc39.es/ecma262/#sec-string.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%String.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 22.1.3.6 String.prototype.constructor
        // https://tc39.es/ecma262/#sec-string.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%String.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 22.1.1.1 String ( value )
    /// https://tc39.es/ecma262/#sec-string-constructor-string-value
    fn behaviour(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const value = arguments.get(0);

        const s = blk: {
            // 1. If value is not present, then
            if (arguments.count() == 0) {
                // a. Let s be the empty String.
                break :blk types.String.from("");
            }
            // 2. Else,
            else {
                // a. If NewTarget is undefined and value is a Symbol, return SymbolDescriptiveString(value).
                if (new_target == null and value == .symbol) {
                    return Value.from(try value.symbol.descriptiveString(agent));
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
        var result = try std.ArrayList(u16).initCapacity(agent.gc_allocator, arguments.count());
        defer result.deinit();

        // 2. For each element next of codeUnits, do
        for (arguments.values) |next| {
            // a. Let nextCU be the code unit whose numeric value is ℝ(? ToUint16(next)).
            const next_code_unit = try next.toUint16(agent);

            // b. Set result to the string-concatenation of result and nextCU.
            result.appendAssumeCapacity(next_code_unit);
        }

        // 3. Return result.
        return Value.from(std.unicode.utf16leToUtf8Alloc(
            agent.gc_allocator,
            result.items,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.DanglingSurrogateHalf,
            error.ExpectedSecondSurrogateHalf,
            error.UnexpectedSecondSurrogateHalf,
            => return agent.throwException(
                .internal_error,
                "UTF-16 strings are not implemented yet",
                .{},
            ),
        });
    }

    /// 22.1.2.2 String.fromCodePoint ( ...codePoints )
    /// https://tc39.es/ecma262/#sec-string.fromcharcode
    fn fromCodePoint(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let result be the empty String.
        var result = std.ArrayList(u8).init(agent.gc_allocator);

        // 2. For each element next of codePoints, do
        for (arguments.values) |next| {
            // a. Let nextCP be ? ToNumber(next).
            const next_code_point = try next.toNumber(agent);

            // b. If IsIntegralNumber(nextCP) is false, throw a RangeError exception.
            if (!Value.from(next_code_point).isIntegralNumber()) {
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
            var out: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(
                @intFromFloat(next_code_point.asFloat()),
                &out,
            ) catch |err| switch (err) {
                error.CodepointTooLarge => unreachable,
                error.Utf8CannotEncodeSurrogateHalf => return agent.throwException(
                    .internal_error,
                    "UTF-16 strings are not implemented yet",
                    .{},
                ),
            };
            try result.appendSlice(out[0..len]);
        }

        // 3. Assert: If codePoints is empty, then result is the empty String.
        // 4. Return result.
        return Value.from(try result.toOwnedSlice());
    }
};

/// 22.1.3 Properties of the String Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object
pub const StringPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try String.create(realm.agent, .{
            .fields = .{
                .string_data = types.String.from(""),
            },
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

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
        try defineBuiltinFunction(object, "@@iterator", @"@@iterator", 0, realm);

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
            const @"%String.prototype.trimStart%" = object.propertyStorage().get(PropertyKey.from("trimStart")).?;
            try defineBuiltinProperty(object, "trimLeft", @"%String.prototype.trimStart%");

            // B.2.2.16 String.prototype.trimRight ( )
            // https://tc39.es/ecma262/#String.prototype.trimright
            const @"%String.prototype.trimEnd%" = object.propertyStorage().get(PropertyKey.from("trimEnd")).?;
            try defineBuiltinProperty(object, "trimRight", @"%String.prototype.trimEnd%");
        }

        return object;
    }

    /// 22.1.3.35.1 ThisStringValue ( value )
    /// https://tc39.es/ecma262/#sec-thisstringvalue
    fn thisStringValue(agent: *Agent, value: Value) error{ExceptionThrown}!types.String {
        switch (value) {
            // 1. If value is a String, return value.
            .string => |string| return string,

            // 2. If value is an Object and value has a [[StringData]] internal slot, then
            .object => |object| if (object.is(String)) {
                // a. Let s be value.[[StringData]].
                // b. Assert: s is a String.
                const s = object.as(String).fields.string_data;

                // c. Return s.
                return s;
            },

            else => {},
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
        const len = string.utf16Length();

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
        const size = string.utf16Length();

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
        const size = string.utf16Length();

        // 5. If position < 0 or position ≥ size, return NaN.
        if (position_f64 < 0 or position_f64 >= @as(f64, @floatFromInt(size))) return Value.nan();
        const position: usize = @intFromFloat(position_f64);

        // 6. Return the Number value for the numeric value of the code unit at index position
        //    within the String S.
        return Value.from(string.utf16CodeUnitAt(position));
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
        const size = string.utf16Length();

        // 5. If position < 0 or position ≥ size, return undefined.
        if (position_f64 < 0 or position_f64 >= @as(f64, @floatFromInt(size))) return .undefined;
        const position: usize = @intFromFloat(position_f64);

        // 6. Let cp be CodePointAt(S, position).
        // 7. Return 𝔽(cp.[[CodePoint]]).
        return Value.from(string.codePointAt(position));
    }

    /// 22.1.3.5 String.prototype.concat ( ...args )
    /// https://tc39.es/ecma262/#sec-string.prototype.concat
    fn concat(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let R be S.
        var new_string = std.ArrayList(u8).init(agent.gc_allocator);
        try new_string.appendSlice(string.utf8);

        // 4. For each element next of args, do
        for (arguments.values) |next| {
            // a. Let nextString be ? ToString(next).
            const next_string = try next.toString(agent);

            // b. Set R to the string-concatenation of R and nextString.
            try new_string.appendSlice(next_string.utf8);
        }

        // 5. Return R.
        return Value.from(try new_string.toOwnedSlice());
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
        const len = string.utf16Length();

        // 7. If endPosition is undefined, let pos be len; else let pos be ? ToIntegerOrInfinity(endPosition).
        const pos = if (end_position == .undefined)
            @as(f64, @floatFromInt(len))
        else
            try end_position.toIntegerOrInfinity(agent);

        // 8. Let end be the result of clamping pos between 0 and len.
        const end = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 9. Let searchLength be the length of searchStr.
        const search_length = search_str.utf16Length();

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
        const len = string.utf16Length();

        // 9. Let start be the result of clamping pos between 0 and len.
        const start = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 10. Let index be StringIndexOf(S, searchStr, start).
        const index = string.indexOf(search_str, start);

        // 11. If index ≠ -1, return true.
        // 12. Return false.
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
        const len = string.utf16Length();

        // 7. Let start be the result of clamping pos between 0 and len.
        const start = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 8. Return 𝔽(StringIndexOf(S, searchStr, start)).
        const index = string.indexOf(search_str, start) orelse return Value.from(-1);
        return Value.from(@as(u53, @intCast(index)));
    }

    /// 22.1.3.10 String.prototype.isWellFormed ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.iswellformed
    fn isWellFormed(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // TODO: 3. Return IsStringWellFormedUnicode(S).
        // NOTE: We only store valid UTF-8 strings so this always returns true for now.
        _ = string;
        return Value.from(true);
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
        const len = string.utf16Length();

        // 8. Let searchLen be the length of searchStr.
        const search_len = search_str.utf16Length();

        // 9. Let start be the result of clamping pos between 0 and len - searchLen.
        const start = std.math.clamp(
            std.math.lossyCast(usize, pos),
            0,
            std.math.sub(usize, len, search_len) catch return Value.from(-1),
        );

        // 10. Return 𝔽(StringLastIndexOf(S, searchStr, start)).
        const index = string.lastIndexOf(search_str, start) orelse return Value.from(-1);
        return Value.from(@as(u53, @intCast(index)));
    }

    /// 22.1.3.13 String.prototype.match ( regexp )
    /// https://tc39.es/ecma262/#sec-string.prototype.match
    fn match(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const regexp = arguments.get(0);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If regexp is neither undefined nor null, then
        if (regexp != .undefined and regexp != .null) {
            // a. Let matcher be ? GetMethod(regexp, @@match).
            const matcher = try regexp.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@match"),
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

        // 5. Return ? Invoke(rx, @@match, « S »).
        return Value.from(rx).invoke(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@match"),
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
        if (regexp != .undefined and regexp != .null) {
            // a. Let isRegExp be ? IsRegExp(regexp).
            const is_reg_exp = try regexp.isRegExp();

            // b. If isRegExp is true, then
            if (is_reg_exp) {
                // i. Let flags be ? Get(regexp, "flags").
                const flags = try regexp.object.get(PropertyKey.from("flags"));

                // ii. Perform ? RequireObjectCoercible(flags).
                _ = try flags.requireObjectCoercible(agent);

                // iii. If ? ToString(flags) does not contain "g", throw a TypeError exception.
                if (std.mem.indexOfScalar(u8, (try flags.toString(agent)).utf8, 'g') == null) {
                    return agent.throwException(
                        .type_error,
                        "RegExp object must have the 'g' flag set",
                        .{},
                    );
                }
            }

            // c. Let matcher be ? GetMethod(regexp, @@matchAll).
            const matcher = try regexp.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@matchAll"),
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

        // 5. Return ? Invoke(rx, @@matchAll, « S »).
        return Value.from(rx).invoke(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@matchAll"),
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
    ) Agent.Error!types.String {
        // 1. Let S be ? ToString(O).
        const string = try object.toString(agent);

        // 2. Let intMaxLength be ℝ(? ToLength(maxLength)).
        const int_max_length = try max_length.toLength(agent);

        // 3. Let stringLength be the length of S.
        const string_length = string.utf16Length();

        // 4. If intMaxLength ≤ stringLength, return S.
        if (int_max_length <= string_length) return string;

        // 5. If fillString is undefined, set fillString to the String value consisting solely of
        //    the code unit 0x0020 (SPACE).
        // 6. Else, set fillString to ? ToString(fillString).
        const fill_string = if (fill_string_value == .undefined)
            types.String.from(" ")
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

        const n_usize = std.math.lossyCast(usize, n);
        const new_len = std.math.mul(
            usize,
            string.utf16Length(),
            n_usize,
        ) catch return error.OutOfMemory;

        // 6. Return the String value that is made from n copies of S appended together.
        var new_string = try std.ArrayList(u8).initCapacity(agent.gc_allocator, new_len);
        for (0..n_usize) |_| {
            new_string.appendSliceAssumeCapacity(string.utf8);
        }
        return Value.from(try new_string.toOwnedSlice());
    }

    /// 22.1.3.19 String.prototype.replace ( searchValue, replaceValue )
    /// https://tc39.es/ecma262/#sec-string.prototype.replace
    fn replace(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const search_value = arguments.get(0);
        var replace_value = arguments.get(1);

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. If searchValue is neither undefined nor null, then
        if (search_value != .undefined and search_value != .null) {
            // a. Let replacer be ? GetMethod(searchValue, @@replace).
            const replacer = try search_value.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@replace"),
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
        if (!functional_replace and replace_value != .string) {
            // a. Set replaceValue to ? ToString(replaceValue).
            replace_value = Value.from(try replace_value.toString(agent));
        }

        // 7. Let searchLength be the length of searchString.
        const search_length = search_string.utf16Length();

        // 8. Let position be StringIndexOf(string, searchString, 0).
        const position = string.indexOf(search_string, 0);

        // 9. If position = -1, return string.
        if (position == null) return Value.from(string);

        // 10. Let preceding be the substring of string from 0 to position.
        const preceding = try string.substring(agent.gc_allocator, 0, position.?);

        // 11. Let following be the substring of string from position + searchLength.
        const following = try string.substring(
            agent.gc_allocator,
            position.? + search_length,
            string.utf16Length(),
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
            std.debug.assert(replace_value == .string);

            // TODO: b. Let captures be a new empty List.
            // TODO: c. Let replacement be ! GetSubstitution(searchString, string, position, captures,
            //          undefined, replaceValue).
            break :blk replace_value.string;
        };

        // 14. Return the string-concatenation of preceding, replacement, and following.
        return Value.from(
            try std.mem.join(
                agent.gc_allocator,
                "",
                &.{ preceding.utf8, replacement.utf8, following.utf8 },
            ),
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
        if (search_value != .undefined and search_value != .null) {
            // a. Let isRegExp be ? IsRegExp(searchValue).
            const is_reg_exp = try search_value.isRegExp();

            // b. If isRegExp is true, then
            if (is_reg_exp) {
                // i. Let flags be ? Get(searchValue, "flags").
                const flags = try search_value.get(agent, PropertyKey.from("flags"));

                // ii. Perform ? RequireObjectCoercible(flags).
                _ = try flags.requireObjectCoercible(agent);

                // iii. If ? ToString(flags) does not contain "g", throw a TypeError exception.
                if (std.mem.indexOfScalar(u8, (try flags.toString(agent)).utf8, 'g') == null) {
                    return agent.throwException(
                        .type_error,
                        "RegExp object must have the 'g' flag set",
                        .{},
                    );
                }
            }

            // c. Let replacer be ? GetMethod(searchValue, @@replace).
            const replacer = try search_value.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@replace"),
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
        if (!functional_replace and replace_value != .string) {
            // a. Set replaceValue to ? ToString(replaceValue).
            replace_value = Value.from(try replace_value.toString(agent));
        }

        // 7. Let searchLength be the length of searchString.
        const search_length = search_string.utf16Length();

        // 8. Let advanceBy be max(1, searchLength).
        const advance_by = @max(1, search_length);

        // 9. Let matchPositions be a new empty List.
        var match_positions = std.ArrayList(usize).init(agent.gc_allocator);
        defer match_positions.deinit();

        // 10. Let position be StringIndexOf(string, searchString, 0).
        var maybe_position = string.indexOf(search_string, 0);

        // 11. Repeat, while position ≠ -1,
        while (maybe_position) |position| {
            // a. Append position to matchPositions.
            try match_positions.append(position);

            // b. Set position to StringIndexOf(string, searchString, position + advanceBy).
            maybe_position = string.indexOf(search_string, position + advance_by);
        }

        // 12. Let endOfLastMatch be 0.
        var end_of_last_match: usize = 0;

        // 13. Let result be the empty String.
        var result = types.String.from("");

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
                std.debug.assert(replace_value == .string);

                // TODO: ii. Let captures be a new empty List.
                // TODO: iii. Let replacement be ! GetSubstitution(searchString, string, p, captures,
                //            undefined, replaceValue).
                break :blk replace_value.string;
            };

            // d. Set result to the string-concatenation of result, preserved, and replacement.
            result = types.String.from(
                try std.mem.concat(agent.gc_allocator, u8, &.{
                    result.utf8, preserved.utf8, replacement.utf8,
                }),
            );

            // e. Set endOfLastMatch to p + searchLength.
            end_of_last_match = position + search_length;
        }

        // 15. If endOfLastMatch < the length of string, then
        if (end_of_last_match < string.utf16Length()) {
            // a. Set result to the string-concatenation of result and the substring of string from
            //    endOfLastMatch.
            result = types.String.from(
                try std.mem.concat(
                    agent.gc_allocator,
                    u8,
                    &.{
                        result.utf8,
                        (try string.substring(
                            agent.gc_allocator,
                            end_of_last_match,
                            string.utf16Length(),
                        )).utf8,
                    },
                ),
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
        if (regexp != .undefined and regexp != .null) {
            // a. Let searcher be ? GetMethod(regexp, @@search).
            const searcher = try regexp.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@search"),
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

        // 5. Return ? Invoke(rx, @@search, « string »).
        return Value.from(rx).invoke(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"@@search"),
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
        const len = string.utf16Length();
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
        const int_end = if (end == .undefined)
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
        if (separator_value != .undefined and separator_value != .null) {
            // a. Let splitter be ? GetMethod(separator, @@split).
            const splitter = try separator_value.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"@@split"),
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
        const limit = if (limit_value == .undefined)
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
        if (separator_value == .undefined) {
            // a. Return CreateArrayFromList(« S »).
            return Value.from(try createArrayFromList(agent, &.{Value.from(string)}));
        }

        // 8. Let separatorLength be the length of R.
        const separator_length = separator.utf16Length();

        // 9. If separatorLength = 0, then
        if (separator_length == 0) {
            // FIXME: This is missing a bounds check. See: https://github.com/tc39/ecma262/issues/3261
            // a. Let head be the substring of S from 0 to lim.
            const head = try string.substring(
                agent.gc_allocator,
                0,
                @min(limit, string.utf16Length()),
            );

            // b. Let codeUnits be a List consisting of the sequence of code units that are the elements of head.
            const code_units = try head.utf16CodeUnits(agent.gc_allocator);

            // c. Return CreateArrayFromList(codeUnits).
            return Value.from(
                try createArrayFromListMapToValue(agent, u16, code_units, struct {
                    fn mapFn(agent_: *Agent, code_unit: u16) Allocator.Error!Value {
                        return Value.from(std.unicode.utf16leToUtf8Alloc(
                            agent_.gc_allocator,
                            &.{code_unit},
                        ) catch |err| switch (err) {
                            error.OutOfMemory => return error.OutOfMemory,
                            error.DanglingSurrogateHalf,
                            error.ExpectedSecondSurrogateHalf,
                            error.UnexpectedSecondSurrogateHalf,
                            // TODO: Implement UTF-16 strings
                            => return Value.from("\u{fffd}"),
                        });
                    }
                }.mapFn),
            );
        }

        // 10. If S is the empty String, return CreateArrayFromList(« S »).
        if (separator.isEmpty()) {
            return Value.from(try createArrayFromList(agent, &.{Value.from(string)}));
        }

        // 11. Let substrings be a new empty List.
        var substrings = std.ArrayList(types.String).init(agent.gc_allocator);
        defer substrings.deinit();

        // 12. Let i be 0.
        var i: usize = 0;

        // 13. Let j be StringIndexOf(S, R, 0).
        var j = string.indexOf(separator, 0);

        // 14. Repeat, while j ≠ -1,
        while (j != null) {
            // a. Let T be the substring of S from i to j.
            const tail = try string.substring(agent.gc_allocator, i, j.?);

            // b. Append T to substrings.
            try substrings.append(tail);

            // c. If the number of elements in substrings is lim, return CreateArrayFromList(substrings).
            if (substrings.items.len == limit) {
                return Value.from(
                    try createArrayFromListMapToValue(agent, types.String, substrings.items, struct {
                        fn mapFn(_: *Agent, string_: types.String) Allocator.Error!Value {
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
        const tail = try string.substring(agent.gc_allocator, i, string.utf16Length());

        // 16. Append T to substrings.
        try substrings.append(tail);

        // 17. Return CreateArrayFromList(substrings).
        return Value.from(
            try createArrayFromListMapToValue(agent, types.String, substrings.items, struct {
                fn mapFn(_: *Agent, string_: types.String) Allocator.Error!Value {
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
        const len = string.utf16Length();

        // 7. If position is undefined, let pos be 0; else let pos be ? ToIntegerOrInfinity(position).
        const pos = if (position == .undefined) 0 else try position.toIntegerOrInfinity(agent);

        // 8. Let start be the result of clamping pos between 0 and len.
        const start = std.math.clamp(std.math.lossyCast(usize, pos), 0, len);

        // 9. Let searchLength be the length of searchStr.
        const search_length = search_str.utf16Length();

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
        const len = string.utf16Length();

        // 4. Let intStart be ? ToIntegerOrInfinity(start).
        const int_start = try start.toIntegerOrInfinity(agent);

        // 5. If end is undefined, let intEnd be len; else let intEnd be ? ToIntegerOrInfinity(end).
        const int_end = if (end == .undefined)
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
        const lower = try std.ascii.allocLowerString(agent.gc_allocator, string.utf8);
        return Value.from(lower);
    }

    /// 22.1.3.27 String.prototype.toLocaleUpperCase ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-string.prototype.tolocaleuppercase
    fn toLocaleUpperCase(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const object = try this_value.requireObjectCoercible(agent);
        const string = try object.toString(agent);
        const lower = try std.ascii.allocUpperString(agent.gc_allocator, string.utf8);
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
        // 4. Let lowerText be the result of toLowercase(sText), according to the Unicode Default
        //    Case Conversion algorithm.
        // 5. Let L be CodePointsToString(lowerText).
        const lower = try std.ascii.allocLowerString(agent.gc_allocator, string.utf8);

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
        // 4. Let upperText be the result of toUppercase(sText), according to the Unicode Default
        //    Case Conversion algorithm.
        // 5. Let U be CodePointsToString(upperText).
        const upper = try std.ascii.allocUpperString(agent.gc_allocator, string.utf8);

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

        // TODO: 3-7.
        // NOTE: We only store valid UTF-8 strings so this always returns the original string for now.
        return Value.from(string);
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
    ) Agent.Error![]const u8 {
        // 1. Let str be ? RequireObjectCoercible(string).
        const str = try string_value.requireObjectCoercible(agent);

        // 2. Let S be ? ToString(str).
        const string = try str.toString(agent);

        const trimmed = switch (where) {
            // 3. If where is start, then
            .start => blk: {
                // a. Let T be the String value that is a copy of S with leading white space
                //    removed.
                break :blk utils.trimLeft(string.utf8, &types.String.whitespace);
            },

            // 4. Else if where is end, then
            .end => blk: {
                // a. Let T be the String value that is a copy of S with trailing white space
                //    removed.
                break :blk utils.trimRight(string.utf8, &types.String.whitespace);
            },

            // 5. Else,
            //     a. Assert: where is start+end.
            .@"start+end" => blk: {
                // b. Let T be the String value that is a copy of S with both leading and trailing
                //    white space removed.
                break :blk utils.trim(string.utf8, &types.String.whitespace);
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

    /// 22.1.3.36 String.prototype [ @@iterator ] ( )
    /// https://tc39.es/ecma262/#sec-string.prototype-@@iterator
    fn @"@@iterator"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let O be ? RequireObjectCoercible(this value).
        const object = try this_value.requireObjectCoercible(agent);

        // 2. Let s be ? ToString(O).
        const string = try object.toString(agent);

        // 3. Let closure be a new Abstract Closure with no parameters that captures s and performs
        //    the following steps when called:
        //    [...]
        // 4. Return CreateIteratorFromClosure(closure, "%StringIteratorPrototype%", %StringIteratorPrototype%).
        const it = std.unicode.Utf16LeIterator.init(try string.utf16CodeUnits(agent.gc_allocator));
        return Value.from(try StringIterator.create(agent, .{
            .prototype = try realm.intrinsics.@"%StringIteratorPrototype%"(),
            .fields = .{
                .state = .{ .it = it },
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
        const size: f64 = @floatFromInt(string.utf16Length());

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
        var int_length = if (length == .undefined)
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
    ) Agent.Error!types.String {
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
            const value_string_escaped = try std.mem.replaceOwned(
                u8,
                agent.gc_allocator,
                value_string.utf8,
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
            return types.String.from(
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "<{[tag]s} {[attribute]s}=\"{[value]s}\">{[string]s}</{[tag]s}>",
                    .{
                        .string = string.utf8,
                        .tag = tag,
                        .attribute = attr.name,
                        .value = value_string_escaped,
                    },
                ),
            );
        }

        return types.String.from(
            try std.fmt.allocPrint(
                agent.gc_allocator,
                "<{[tag]s}>{[string]s}</{[tag]s}>",
                .{ .string = string.utf8, .tag = tag },
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

    /// B.2.2.7 String.prototype.fontcolor ( color )
    /// https://tc39.es/ecma262/#sec-string.prototype.fontcolor
    fn fontcolor(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const color = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Return ? CreateHTML(S, "font", "color", color).
        return Value.from(
            try createHTML(agent, this_value, "font", .{ .name = "color", .value = color }),
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
        string_data: types.String,
    },
    .tag = .string,
});
