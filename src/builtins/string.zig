//! 22.1 String Objects
//! https://tc39.es/ecma262/#sec-string-objects

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
const StringIterator = builtins.StringIterator;
const Value = types.Value;
const PropertyKey = types.PropertyKey;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const isCompatiblePropertyDescriptor = builtins.isCompatiblePropertyDescriptor;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const regExpCreate = builtins.regExpCreate;

/// 10.4.3.1 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-string-exotic-objects-getownproperty-p
fn getOwnProperty(object: Object, property_key: PropertyKey) !?PropertyDescriptor {
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
) !bool {
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
    return ordinaryDefineOwnProperty(object, property_key, property_descriptor);
}

/// 10.4.3.4 StringCreate ( value, prototype )
/// https://tc39.es/ecma262/#sec-stringcreate
pub fn stringCreate(agent: *Agent, value: types.String, prototype: Object) !Object {
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

            // TODO: 6. Set S.[[OwnPropertyKeys]] as specified in 10.4.3.3.
        },
    });

    // 7. Let length be the length of value.
    const length = value.utf16Length();

    // 8. Perform ! DefinePropertyOrThrow(S, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    string.definePropertyOrThrow(PropertyKey.from("length"), .{
        .value = Value.from(length),
        .writable = false,
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 9. Return S.
    return string;
}

/// 10.4.3.5 StringGetOwnProperty ( S, P )
/// https://tc39.es/ecma262/#sec-stringgetownproperty
fn stringGetOwnProperty(string: *const String, property_key: PropertyKey) !?PropertyDescriptor {
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
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "String",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

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
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
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
};

/// 22.1.3 Properties of the String Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object
pub const StringPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try String.create(realm.agent, .{
            .fields = .{
                .string_data = types.String.from(""),
            },
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "at", at, 1, realm);
        try defineBuiltinFunction(object, "charAt", charAt, 1, realm);
        try defineBuiltinFunction(object, "charCodeAt", charCodeAt, 1, realm);
        try defineBuiltinFunction(object, "concat", concat, 1, realm);
        try defineBuiltinFunction(object, "matchAll", matchAll, 1, realm);
        try defineBuiltinFunction(object, "repeat", repeat, 1, realm);
        try defineBuiltinFunction(object, "search", search, 1, realm);
        try defineBuiltinFunction(object, "slice", slice, 2, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);
        try defineBuiltinFunction(object, "@@iterator", @"@@iterator", 0, realm);

        return object;
    }

    /// 22.1.3.35.1 ThisStringValue ( value )
    /// https://tc39.es/ecma262/#sec-thisstringvalue
    fn thisStringValue(agent: *Agent, value: Value) !types.String {
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
    fn at(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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
    fn charAt(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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
    fn charCodeAt(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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
        const code_units = try string.utf16CodeUnits(agent.gc_allocator);
        defer agent.gc_allocator.free(code_units);
        return Value.from(code_units[position]);
    }

    /// 22.1.3.5 String.prototype.concat ( ...args )
    /// https://tc39.es/ecma262/#sec-string.prototype.concat
    fn concat(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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

    /// 22.1.3.14 String.prototype.matchAll ( regexp )
    /// https://tc39.es/ecma262/#sec-string.prototype.matchall
    fn matchAll(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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
                return Value.from(matcher.?).callAssumeCallable(regexp, .{object});
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
            .{Value.from(string)},
        );
    }

    /// 22.1.3.18 String.prototype.repeat ( count )
    /// https://tc39.es/ecma262/#sec-string.prototype.repeat
    fn repeat(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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

        if (string.utf16Length() == 0) return Value.from("");

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

    /// 22.1.3.21 String.prototype.search ( regexp )
    /// https://tc39.es/ecma262/#sec-string.prototype.search
    fn search(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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
                return Value.from(searcher.?).callAssumeCallable(regexp, .{object});
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
            .{Value.from(string)},
        );
    }

    /// 22.1.3.22 String.prototype.slice ( start, end )
    /// https://tc39.es/ecma262/#sec-string.prototype.slice
    fn slice(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
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
        var int_start = try start.toIntegerOrInfinity(agent);

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

    /// 22.1.3.29 String.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? ThisStringValue(this value).
        return Value.from(try thisStringValue(agent, this_value));
    }

    /// 22.1.3.35 String.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? ThisStringValue(this value).
        return Value.from(try thisStringValue(agent, this_value));
    }

    /// 22.1.3.36 String.prototype [ @@iterator ] ( )
    /// https://tc39.es/ecma262/#sec-string.prototype-@@iterator
    fn @"@@iterator"(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
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
