//! 19 The Global Object
//! https://tc39.es/ecma262/#sec-global-object

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const tokenizer = @import("../language/tokenizer.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const performEval = @import("eval.zig").performEval;
const trimLeft = utils.trimLeft;
const line_terminators = tokenizer.line_terminators;
const whitespace = tokenizer.whitespace;

const Self = @This();

const NameAndPropertyDescriptor = struct {
    []const u8,
    PropertyDescriptor,
};

pub fn globalObjectProperties(realm: *Realm) ![36]NameAndPropertyDescriptor {
    // NOTE: For the sake of compactness we're breaking the line length recommendations here.
    return [_]NameAndPropertyDescriptor{
        // 19.1.1 globalThis
        // https://tc39.es/ecma262/#sec-globalthis
        .{ "globalThis", .{ .value = Value.from(realm.global_env.global_this_value), .writable = true, .enumerable = false, .configurable = true } },

        // 19.1.2 Infinity
        // https://tc39.es/ecma262/#sec-value-properties-of-the-global-object-infinity
        .{ "Infinity", .{ .value = Value.infinity(), .writable = false, .enumerable = false, .configurable = false } },

        // 19.1.3 NaN
        // https://tc39.es/ecma262/#sec-value-properties-of-the-global-object-nan
        .{ "NaN", .{ .value = Value.nan(), .writable = false, .enumerable = false, .configurable = false } },

        // 19.1.4 undefined
        // https://tc39.es/ecma262/#sec-undefined
        .{ "undefined", .{ .value = .undefined, .writable = false, .enumerable = false, .configurable = false } },

        // 19.2.1 eval ( x )
        // https://tc39.es/ecma262/#sec-eval-x
        .{ "eval", .{ .value = Value.from(try realm.intrinsics.@"%eval%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.2.2 isFinite ( number )
        // https://tc39.es/ecma262/#sec-isfinite-number
        .{ "isFinite", .{ .value = Value.from(try realm.intrinsics.@"%isFinite%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.2.3 isNaN ( number )
        // https://tc39.es/ecma262/#sec-isnan-number
        .{ "isNaN", .{ .value = Value.from(try realm.intrinsics.@"%isNaN%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.2.4 parseFloat ( string )
        // https://tc39.es/ecma262/#sec-parsefloat-string
        .{ "parseFloat", .{ .value = Value.from(try realm.intrinsics.@"%parseFloat%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.2.5 parseInt ( string, radix )
        // https://tc39.es/ecma262/#sec-parseint-string-radix
        .{ "parseInt", .{ .value = Value.from(try realm.intrinsics.@"%parseInt%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.1 AggregateError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-aggregate-error
        .{ "AggregateError", .{ .value = Value.from(try realm.intrinsics.@"%AggregateError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.2 Array ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-array
        .{ "Array", .{ .value = Value.from(try realm.intrinsics.@"%Array%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.3 ArrayBuffer ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-arraybuffer
        .{ "ArrayBuffer", .{ .value = Value.from(try realm.intrinsics.@"%ArrayBuffer%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.4 BigInt ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-bigint
        .{ "BigInt", .{ .value = Value.from(try realm.intrinsics.@"%BigInt%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.7 Boolean ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-boolean
        .{ "Boolean", .{ .value = Value.from(try realm.intrinsics.@"%Boolean%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.8 DataView ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-dataview
        .{ "DataView", .{ .value = Value.from(try realm.intrinsics.@"%DataView%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.9 Date ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-date
        .{ "Date", .{ .value = Value.from(try realm.intrinsics.@"%Date%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.10 Error ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-error
        .{ "Error", .{ .value = Value.from(try realm.intrinsics.@"%Error%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.11 EvalError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-evalerror
        .{ "EvalError", .{ .value = Value.from(try realm.intrinsics.@"%EvalError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.15 Function ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-function
        .{ "Function", .{ .value = Value.from(try realm.intrinsics.@"%Function%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.19 Map ( . . . )
        // https://tc39.es/ecma262/#sec-map
        .{ "Map", .{ .value = Value.from(try realm.intrinsics.@"%Map%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.20 Number ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-number
        .{ "Number", .{ .value = Value.from(try realm.intrinsics.@"%Number%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.21 Object ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-object
        .{ "Object", .{ .value = Value.from(try realm.intrinsics.@"%Object%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.22 Promise ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-promise
        .{ "Promise", .{ .value = Value.from(try realm.intrinsics.@"%Promise%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.23 Proxy ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-proxy
        .{ "Proxy", .{ .value = Value.from(try realm.intrinsics.@"%Proxy%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.24 RangeError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-rangeerror
        .{ "RangeError", .{ .value = Value.from(try realm.intrinsics.@"%RangeError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.25 ReferenceError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-referenceerror
        .{ "ReferenceError", .{ .value = Value.from(try realm.intrinsics.@"%ReferenceError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.26 RegExp ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-regexp
        .{ "RegExp", .{ .value = Value.from(try realm.intrinsics.@"%RegExp%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.27 Set ( . . . )
        // https://tc39.es/ecma262/#sec-set
        .{ "Set", .{ .value = Value.from(try realm.intrinsics.@"%Set%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.29 String ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-string
        .{ "String", .{ .value = Value.from(try realm.intrinsics.@"%String%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.30 Symbol ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-symbol
        .{ "Symbol", .{ .value = Value.from(try realm.intrinsics.@"%Symbol%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.31 SyntaxError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-syntaxerror
        .{ "SyntaxError", .{ .value = Value.from(try realm.intrinsics.@"%SyntaxError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.32 TypeError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-typeerror
        .{ "TypeError", .{ .value = Value.from(try realm.intrinsics.@"%TypeError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.37 URIError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-urierror
        .{ "URIError", .{ .value = Value.from(try realm.intrinsics.@"%URIError%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.4.2 JSON
        // https://tc39.es/ecma262/#sec-json
        .{ "JSON", .{ .value = Value.from(try realm.intrinsics.@"%JSON%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.4.3 Math
        // https://tc39.es/ecma262/#sec-math
        .{ "Math", .{ .value = Value.from(try realm.intrinsics.@"%Math%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.4.4 Reflect
        // https://tc39.es/ecma262/#sec-reflect
        .{ "Reflect", .{ .value = Value.from(try realm.intrinsics.@"%Reflect%"()), .writable = true, .enumerable = false, .configurable = true } },
    };
}

fn GlobalFunction(comptime options: struct { name: []const u8, length: u32 }) type {
    return struct {
        pub fn create(realm: *Realm) !Object {
            return createBuiltinFunction(realm.agent, .{ .regular = @field(Self, options.name) }, .{
                .length = options.length,
                .name = options.name,
                .realm = realm,
            });
        }
    };
}

pub const global_functions = struct {
    pub const Eval = GlobalFunction(.{ .name = "eval", .length = 1 });
    pub const IsFinite = GlobalFunction(.{ .name = "isFinite", .length = 1 });
    pub const IsNaN = GlobalFunction(.{ .name = "isNaN", .length = 1 });
    pub const ParseFloat = GlobalFunction(.{ .name = "parseFloat", .length = 1 });
    pub const ParseInt = GlobalFunction(.{ .name = "parseInt", .length = 2 });
};

/// 19.2.1 eval ( x )
/// https://tc39.es/ecma262/#sec-eval-x
fn eval(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
    const x = arguments.get(0);

    // 1. Return ? PerformEval(x, false, false).
    return performEval(agent, x, false, false);
}

/// 19.2.2 isFinite ( number )
/// https://tc39.es/ecma262/#sec-isfinite-number
fn isFinite(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
    const number = arguments.get(0);

    // 1. Let num be ? ToNumber(number).
    const num = try number.toNumber(agent);

    // 2. If num is not finite, return false.
    // 3. Otherwise, return true.
    return Value.from(num.isFinite());
}

/// 19.2.3 isNaN ( number )
/// https://tc39.es/ecma262/#sec-isnan-number
fn isNaN(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
    const number = arguments.get(0);

    // 1. Let num be ? ToNumber(number).
    const num = try number.toNumber(agent);

    // 2. If num is NaN, return true.
    // 3. Otherwise, return false.
    return Value.from(num.isNan());
}

/// 19.2.4 parseFloat ( string )
/// https://tc39.es/ecma262/#sec-parsefloat-string
fn parseFloat(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
    const string_value = arguments.get(0);

    // 1. Let inputString be ? ToString(string).
    const input_string = try string_value.toString(agent);

    // 2. Let trimmedString be ! TrimString(inputString, start).
    const trimmed_string = trimLeft(input_string.utf8, &(whitespace ++ line_terminators));

    // 3. Let trimmed be StringToCodePoints(trimmedString).
    // 4. Let trimmedPrefix be the longest prefix of trimmed that satisfies the syntax of a
    //    StrDecimalLiteral, which might be trimmed itself. If there is no such prefix, return NaN.
    // 5. Let parsedNumber be ParseText(trimmedPrefix, StrDecimalLiteral).
    // 6. Assert: parsedNumber is a Parse Node.
    // 7. Return StringNumericValue of parsedNumber.
    if (std.mem.startsWith(u8, trimmed_string, "-Infinity")) return Value.negativeInfinity();
    if (std.mem.startsWith(u8, trimmed_string, "+Infinity")) return Value.infinity();
    if (std.mem.startsWith(u8, trimmed_string, "Infinity")) return Value.infinity();
    // FIXME: This is very much not correct :)
    // Slice at a few chars that parseFloat() would accept but which are not part of StrDecimalLiteral
    const string = trimmed_string[0 .. std.mem.indexOfAny(u8, trimmed_string, "_x") orelse trimmed_string.len];
    const parsed = std.fmt.parseFloat(f64, string) catch return Value.nan();
    // Ignore valid result of "inf"/"nan"
    if (!std.math.isFinite(parsed)) return Value.nan();
    return Value.from(parsed);
}

/// 19.2.5 parseInt ( string, radix )
/// https://tc39.es/ecma262/#sec-parseint-string-radix
fn parseInt(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
    const string_value = arguments.get(0);
    const radix_value = arguments.get(1);

    // 1. Let inputString be ? ToString(string).
    const input_string = try string_value.toString(agent);

    // 2. Let S be ! TrimString(inputString, start).
    var string = trimLeft(input_string.utf8, &(whitespace ++ line_terminators));

    // 3. Let sign be 1.
    var sign: f64 = 1;

    // 4. If S is not empty and the first code unit of S is the code unit 0x002D (HYPHEN-MINUS),
    //    set sign to -1.
    if (std.mem.startsWith(u8, string, "-")) sign = -1;

    // 5. If S is not empty and the first code unit of S is either the code unit 0x002B (PLUS SIGN)
    //    or the code unit 0x002D (HYPHEN-MINUS), set S to the substring of S from index 1.
    if (std.mem.startsWith(u8, string, "+") or std.mem.startsWith(u8, string, "-")) {
        string = string[1..];
    }

    // 6. Let R be ℝ(? ToInt32(radix)).
    var radix = try radix_value.toInt32(agent);

    // 7. Let stripPrefix be true.
    var strip_prefix = true;

    // 8. If R ≠ 0, then
    if (radix != 0) {
        // a. If R < 2 or R > 36, return NaN.
        if (radix < 2 or radix > 36) return Value.nan();

        // b. If R ≠ 16, set stripPrefix to false.
        if (radix != 16) strip_prefix = false;
    }
    // 9. Else,
    else {
        // a. Set R to 10.
        radix = 10;
    }

    // 10. If stripPrefix is true, then
    if (strip_prefix) {
        // a. If the length of S is at least 2 and the first two code units of S are either "0x" or
        //    "0X", then
        if (std.mem.startsWith(u8, string, "0x") or std.mem.startsWith(u8, string, "0X")) {
            // i. Set S to the substring of S from index 2.
            string = string[2..];

            // ii. Set R to 16.
            radix = 16;
        }
    }

    // 11. If S contains a code unit that is not a radix-R digit, let end be the index within S of
    //     the first such code unit; otherwise, let end be the length of S.
    // 12. Let Z be the substring of S from 0 to end.
    // 13. If Z is empty, return NaN.
    // 14. Let mathInt be the integer value that is represented by Z in radix-R notation, using the
    //     letters A through Z and a through z for digits with values 10 through 35. (However, if
    //     R = 10 and Z contains more than 20 significant digits, every significant digit after the
    //     20th may be replaced by a 0 digit, at the option of the implementation; and if R is not
    //     one of 2, 4, 8, 10, 16, or 32, then mathInt may be an implementation-approximated integer
    //     representing the integer value denoted by Z in radix-R notation.)
    var math_int: ?f64 = null;
    for (string) |c| {
        const digit = std.fmt.charToDigit(c, @intCast(radix)) catch break;
        if (math_int == null) math_int = 0;
        math_int.? *= @floatFromInt(radix);
        math_int.? += @floatFromInt(digit);
    }

    if (math_int == null) return Value.nan();

    // 15. If mathInt = 0, then
    //     a. If sign = -1, return -0𝔽.
    //     b. Return +0𝔽.
    // 16. Return 𝔽(sign × mathInt).
    return Value.from(sign * math_int.?);
}
