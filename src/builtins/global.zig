//! 19 The Global Object
//! https://tc39.es/ecma262/#sec-global-object

const std = @import("std");

const build_options = @import("build-options");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const performEval = builtins.performEval;

const module = @This();

const GlobalObjectProperty = struct {
    []const u8,
    union(enum) {
        property_descriptor: PropertyDescriptor,
        lazy_intrinsic: *const fn (*Realm.Intrinsics) std.mem.Allocator.Error!Object,
    },
};

const num_properties = 59 +
    (if (build_options.enable_annex_b) 2 else 0) +
    (if (build_options.enable_intl) 1 else 0);

pub fn globalObjectProperties(realm: *Realm) [num_properties]GlobalObjectProperty {
    // NOTE: For the sake of compactness we're breaking the line length recommendations here.
    return [_]GlobalObjectProperty{
        // 19.1.1 globalThis
        // https://tc39.es/ecma262/#sec-globalthis
        .{ "globalThis", .{ .property_descriptor = .{ .value = Value.from(realm.global_env.global_this_value), .writable = true, .enumerable = false, .configurable = true } } },

        // 19.1.2 Infinity
        // https://tc39.es/ecma262/#sec-value-properties-of-the-global-object-infinity
        .{ "Infinity", .{ .property_descriptor = .{ .value = .infinity, .writable = false, .enumerable = false, .configurable = false } } },

        // 19.1.3 NaN
        // https://tc39.es/ecma262/#sec-value-properties-of-the-global-object-nan
        .{ "NaN", .{ .property_descriptor = .{ .value = .nan, .writable = false, .enumerable = false, .configurable = false } } },

        // 19.1.4 undefined
        // https://tc39.es/ecma262/#sec-undefined
        .{ "undefined", .{ .property_descriptor = .{ .value = .undefined, .writable = false, .enumerable = false, .configurable = false } } },

        // 19.2.1 eval ( x )
        // https://tc39.es/ecma262/#sec-eval-x
        .{ "eval", .{ .lazy_intrinsic = Realm.Intrinsics.@"%eval%" } },

        // 19.2.2 isFinite ( number )
        // https://tc39.es/ecma262/#sec-isfinite-number
        .{ "isFinite", .{ .lazy_intrinsic = Realm.Intrinsics.@"%isFinite%" } },

        // 19.2.3 isNaN ( number )
        // https://tc39.es/ecma262/#sec-isnan-number
        .{ "isNaN", .{ .lazy_intrinsic = Realm.Intrinsics.@"%isNaN%" } },

        // 19.2.4 parseFloat ( string )
        // https://tc39.es/ecma262/#sec-parsefloat-string
        .{ "parseFloat", .{ .lazy_intrinsic = Realm.Intrinsics.@"%parseFloat%" } },

        // 19.2.5 parseInt ( string, radix )
        // https://tc39.es/ecma262/#sec-parseint-string-radix
        .{ "parseInt", .{ .lazy_intrinsic = Realm.Intrinsics.@"%parseInt%" } },

        // 19.2.6.1 decodeURI ( encodedURI )
        // https://tc39.es/ecma262/#sec-decodeuri-encodeduri
        .{ "decodeURI", .{ .lazy_intrinsic = Realm.Intrinsics.@"%decodeURI%" } },

        // 19.2.6.2 decodeURIComponent ( encodedURIComponent )
        // https://tc39.es/ecma262/#sec-decodeuricomponent-encodeduricomponent
        .{ "decodeURIComponent", .{ .lazy_intrinsic = Realm.Intrinsics.@"%decodeURIComponent%" } },

        // 19.2.6.3 encodeURI ( uri )
        // https://tc39.es/ecma262/#sec-encodeuri-uri
        .{ "encodeURI", .{ .lazy_intrinsic = Realm.Intrinsics.@"%encodeURI%" } },

        // 19.2.6.4 encodeURIComponent ( uriComponent )
        // https://tc39.es/ecma262/#sec-encodeuricomponent-uricomponent
        .{ "encodeURIComponent", .{ .lazy_intrinsic = Realm.Intrinsics.@"%encodeURIComponent%" } },

        // 19.3.1 AggregateError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-aggregate-error
        .{ "AggregateError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%AggregateError%" } },

        // 19.3.2 Array ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-array
        .{ "Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Array%" } },

        // 19.3.3 ArrayBuffer ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-arraybuffer
        .{ "ArrayBuffer", .{ .lazy_intrinsic = Realm.Intrinsics.@"%ArrayBuffer%" } },

        // 19.3.4 BigInt ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-bigint
        .{ "BigInt", .{ .lazy_intrinsic = Realm.Intrinsics.@"%BigInt%" } },

        // 19.3.5 BigInt64Array ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-bigint64array
        .{ "BigInt64Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%BigInt64Array%" } },

        // 19.3.6 BigUint64Array ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-biguint64array
        .{ "BigUint64Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%BigUint64Array%" } },

        // 19.3.7 Boolean ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-boolean
        .{ "Boolean", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Boolean%" } },

        // 19.3.8 DataView ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-dataview
        .{ "DataView", .{ .lazy_intrinsic = Realm.Intrinsics.@"%DataView%" } },

        // 19.3.9 Date ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-date
        .{ "Date", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Date%" } },

        // 19.3.10 Error ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-error
        .{ "Error", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Error%" } },

        // 19.3.11 EvalError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-evalerror
        .{ "EvalError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%EvalError%" } },

        // 19.3.12 FinalizationRegistry ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-finnalization-registry
        .{ "FinalizationRegistry", .{ .lazy_intrinsic = Realm.Intrinsics.@"%FinalizationRegistry%" } },

        // 2.1 Float16Array ( . . . )
        // https://tc39.es/proposal-float16array/#sec-float16array
        .{ "Float16Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Float16Array%" } },

        // 19.3.13 Float32Array ( . . . )
        // https://tc39.es/ecma262/#sec-float32array
        .{ "Float32Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Float32Array%" } },

        // 19.3.14 Float64Array ( . . . )
        // https://tc39.es/ecma262/#sec-float64array
        .{ "Float64Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Float64Array%" } },

        // 19.3.15 Function ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-function
        .{ "Function", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Function%" } },

        // 19.3.16 Int8Array ( . . . )
        // https://tc39.es/ecma262/#sec-int8array
        .{ "Int8Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Int8Array%" } },

        // 19.3.17 Int16Array ( . . . )
        // https://tc39.es/ecma262/#sec-int16array
        .{ "Int16Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Int16Array%" } },

        // 19.3.18 Int32Array ( . . . )
        // https://tc39.es/ecma262/#sec-int32array
        .{ "Int32Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Int32Array%" } },

        // 19.3.19 Iterator ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-iterator
        .{ "Iterator", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Iterator%" } },

        // 19.3.20 Map ( . . . )
        // https://tc39.es/ecma262/#sec-map
        .{ "Map", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Map%" } },

        // 19.3.21 Number ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-number
        .{ "Number", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Number%" } },

        // 19.3.22 Object ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-object
        .{ "Object", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Object%" } },

        // 19.3.23 Promise ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-promise
        .{ "Promise", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Promise%" } },

        // 19.3.24 Proxy ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-proxy
        .{ "Proxy", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Proxy%" } },

        // 19.3.25 RangeError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-rangeerror
        .{ "RangeError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%RangeError%" } },

        // 19.3.26 ReferenceError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-referenceerror
        .{ "ReferenceError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%ReferenceError%" } },

        // 19.3.27 RegExp ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-regexp
        .{ "RegExp", .{ .lazy_intrinsic = Realm.Intrinsics.@"%RegExp%" } },

        // 19.3.28 Set ( . . . )
        // https://tc39.es/ecma262/#sec-set
        .{ "Set", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Set%" } },

        // 19.3.29 SharedArrayBuffer ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-sharedarraybuffer
        .{ "SharedArrayBuffer", .{ .lazy_intrinsic = Realm.Intrinsics.@"%SharedArrayBuffer%" } },

        // 19.3.30 String ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-string
        .{ "String", .{ .lazy_intrinsic = Realm.Intrinsics.@"%String%" } },

        // 19.3.31 Symbol ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-symbol
        .{ "Symbol", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Symbol%" } },

        // 19.3.32 SyntaxError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-syntaxerror
        .{ "SyntaxError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%SyntaxError%" } },

        // 19.3.33 TypeError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-typeerror
        .{ "TypeError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%TypeError%" } },

        // 19.3.34 Uint8Array ( . . . )
        // https://tc39.es/ecma262/#sec-uint8array
        .{ "Uint8Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Uint8Array%" } },

        // 19.3.35 Uint8ClampedArray ( . . . )
        // https://tc39.es/ecma262/#sec-uint8clampedarray
        .{ "Uint8ClampedArray", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Uint8ClampedArray%" } },

        // 19.3.36 Uint16Array ( . . . )
        // https://tc39.es/ecma262/#sec-uint16array
        .{ "Uint16Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Uint16Array%" } },

        // 19.3.37 Uint32Array ( . . . )
        // https://tc39.es/ecma262/#sec-uint32array
        .{ "Uint32Array", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Uint32Array%" } },

        // 19.3.38 URIError ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-urierror
        .{ "URIError", .{ .lazy_intrinsic = Realm.Intrinsics.@"%URIError%" } },

        // 19.3.39 WeakMap ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-weakmap
        .{ "WeakMap", .{ .lazy_intrinsic = Realm.Intrinsics.@"%WeakMap%" } },

        // 19.3.40 WeakRef ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-weakref
        .{ "WeakRef", .{ .lazy_intrinsic = Realm.Intrinsics.@"%WeakRef%" } },

        // 19.3.41 WeakSet ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-weakset
        .{ "WeakSet", .{ .lazy_intrinsic = Realm.Intrinsics.@"%WeakSet%" } },

        // 19.4.1 Atomics
        // https://tc39.es/ecma262/#sec-atomics
        .{ "Atomics", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Atomics%" } },

        // 19.4.2 JSON
        // https://tc39.es/ecma262/#sec-json
        .{ "JSON", .{ .lazy_intrinsic = Realm.Intrinsics.@"%JSON%" } },

        // 19.4.3 Math
        // https://tc39.es/ecma262/#sec-math
        .{ "Math", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Math%" } },

        // 19.4.4 Reflect
        // https://tc39.es/ecma262/#sec-reflect
        .{ "Reflect", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Reflect%" } },
    } ++ (if (build_options.enable_annex_b) [_]GlobalObjectProperty{
        .{ "escape", .{ .lazy_intrinsic = Realm.Intrinsics.@"%escape%" } },
        .{ "unescape", .{ .lazy_intrinsic = Realm.Intrinsics.@"%unescape%" } },
    } else .{}) ++ (if (build_options.enable_intl) [_]GlobalObjectProperty{
        .{ "Intl", .{ .lazy_intrinsic = Realm.Intrinsics.@"%Intl%" } },
    } else .{});
}

fn GlobalFunction(comptime options: struct { name: []const u8, length: u32 }) type {
    return struct {
        pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
            return createBuiltinFunction(realm.agent, .{ .function = @field(module, options.name) }, .{
                .length = options.length,
                .name = options.name,
                .realm = realm,
            });
        }
        pub fn init(_: *Realm, _: Object) std.mem.Allocator.Error!void {}
    };
}

pub const eval_function = GlobalFunction(.{ .name = "eval", .length = 1 });
pub const is_finite_function = GlobalFunction(.{ .name = "isFinite", .length = 1 });
pub const is_nan_function = GlobalFunction(.{ .name = "isNaN", .length = 1 });
pub const parse_float_function = GlobalFunction(.{ .name = "parseFloat", .length = 1 });
pub const parse_int_function = GlobalFunction(.{ .name = "parseInt", .length = 2 });
pub const decode_uri_function = GlobalFunction(.{ .name = "decodeURI", .length = 1 });
pub const decode_uri_component_function = GlobalFunction(.{ .name = "decodeURIComponent", .length = 1 });
pub const encode_uri_function = GlobalFunction(.{ .name = "encodeURI", .length = 1 });
pub const encode_uri_component_function = GlobalFunction(.{ .name = "encodeURIComponent", .length = 1 });
pub const escape_function = if (build_options.enable_annex_b)
    GlobalFunction(.{ .name = "escape", .length = 1 })
else
    @compileError("Annex B is not enabled");
pub const unescape_function = if (build_options.enable_annex_b)
    GlobalFunction(.{ .name = "unescape", .length = 1 })
else
    @compileError("Annex B is not enabled");

/// 19.2.1 eval ( x )
/// https://tc39.es/ecma262/#sec-eval-x
fn eval(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const x = arguments.get(0);

    // 1. Return ? PerformEval(x, false, false).
    return performEval(agent, x, false, false);
}

/// 19.2.2 isFinite ( number )
/// https://tc39.es/ecma262/#sec-isfinite-number
fn isFinite(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const number = arguments.get(0);

    // 1. Let num be ? ToNumber(number).
    const num = try number.toNumber(agent);

    // 2. If num is not finite, return false.
    // 3. Otherwise, return true.
    return Value.from(num.isFinite());
}

/// 19.2.3 isNaN ( number )
/// https://tc39.es/ecma262/#sec-isnan-number
fn isNaN(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const number = arguments.get(0);

    // 1. Let num be ? ToNumber(number).
    const num = try number.toNumber(agent);

    // 2. If num is NaN, return true.
    // 3. Otherwise, return false.
    return Value.from(num.isNan());
}

/// 19.2.4 parseFloat ( string )
/// https://tc39.es/ecma262/#sec-parsefloat-string
fn parseFloat(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const string_value = arguments.get(0);

    // 1. Let inputString be ? ToString(string).
    const input_string = try string_value.toString(agent);

    // 2. Let trimmedString be ! TrimString(inputString, start).
    var trimmed_string = try (try input_string.trim(
        agent.gc_allocator,
        .start,
    )).toUtf8(agent.gc_allocator);

    // 3. Let trimmed be StringToCodePoints(trimmedString).
    // 4. Let trimmedPrefix be the longest prefix of trimmed that satisfies the syntax of a
    //    StrDecimalLiteral, which might be trimmed itself. If there is no such prefix, return NaN.
    // 5. Let parsedNumber be ParseText(trimmedPrefix, StrDecimalLiteral).
    // 6. Assert: parsedNumber is a Parse Node.
    // 7. Return the StringNumericValue of parsedNumber.
    if (std.mem.startsWith(u8, trimmed_string, "-Infinity")) return .negative_infinity;
    if (std.mem.startsWith(u8, trimmed_string, "+Infinity")) return .infinity;
    if (std.mem.startsWith(u8, trimmed_string, "Infinity")) return .infinity;
    // Don't pass other strings starting with "inf" to `std.fmt.parseFloat()`
    if (std.ascii.startsWithIgnoreCase(trimmed_string, "inf")) return .nan;
    // Limit to characters valid for StrDecimalLiteral before brute forcing
    var len = for (trimmed_string, 0..) |c, i| {
        if (!std.ascii.isDigit(c) and std.mem.indexOfScalar(u8, "+-.eE", c) == null) break i;
    } else trimmed_string.len;
    while (len != 0) : (len -= 1) {
        if (std.fmt.parseFloat(f64, trimmed_string[0..len])) |result|
            return Value.from(result)
        else |_| {}
    }
    return .nan;
}

/// 19.2.5 parseInt ( string, radix )
/// https://tc39.es/ecma262/#sec-parseint-string-radix
fn parseInt(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const string_value = arguments.get(0);
    const radix_value = arguments.get(1);

    // 1. Let inputString be ? ToString(string).
    const input_string = try string_value.toString(agent);

    // 2. Let S be ! TrimString(inputString, start).
    var string = try (try input_string.trim(
        agent.gc_allocator,
        .start,
    )).toUtf8(agent.gc_allocator);

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
        if (radix < 2 or radix > 36) return .nan;

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

    if (math_int == null) return .nan;

    // 15. If mathInt = 0, then
    //     a. If sign = -1, return -0𝔽.
    //     b. Return +0𝔽.
    // 16. Return 𝔽(sign × mathInt).
    return Value.from(sign * math_int.?);
}

/// 19.2.6.1 decodeURI ( encodedURI )
/// https://tc39.es/ecma262/#sec-decodeuri-encodeduri
fn decodeURI(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const encoded_uri = arguments.get(0);

    // 1. Let uriString be ? ToString(encodedURI).
    const uri_string = try encoded_uri.toString(agent);

    // 2. Let preserveEscapeSet be ";/?:@&=+$,#".
    const preserve_escape_set = ";/?:@&=+$,#";

    // 3. Return ? Decode(uriString, preserveEscapeSet).
    return Value.from(try decode(agent, uri_string, preserve_escape_set));
}

/// 19.2.6.2 decodeURIComponent ( encodedURIComponent )
/// https://tc39.es/ecma262/#sec-decodeuricomponent-encodeduricomponent
fn decodeURIComponent(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const encoded_uri_component = arguments.get(0);

    // 1. Let componentString be ? ToString(encodedURIComponent).
    const component_string = try encoded_uri_component.toString(agent);

    // 2. Let preserveEscapeSet be the empty String.
    const preserve_escape_set = "";

    // 3. Return ? Decode(componentString, preserveEscapeSet).
    return Value.from(try decode(agent, component_string, preserve_escape_set));
}

/// 19.2.6.3 encodeURI ( uri )
/// https://tc39.es/ecma262/#sec-encodeuri-uri
fn encodeURI(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const uri = arguments.get(0);

    // 1. Let uriString be ? ToString(uri).
    const uri_string = try uri.toString(agent);

    // 2. Let extraUnescaped be ";/?:@&=+$,#".
    const extra_unescaped = ";/?:@&=+$,#";

    // 3. Return ? Encode(uriString, extraUnescaped).
    return Value.from(try encode(agent, uri_string, extra_unescaped));
}

/// 19.2.6.4 encodeURIComponent ( uriComponent )
/// https://tc39.es/ecma262/#sec-encodeuricomponent-uricomponent
fn encodeURIComponent(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const uri_component = arguments.get(0);

    // 1. Let componentString be ? ToString(uriComponent).
    const component_string = try uri_component.toString(agent);

    // 2. Let extraUnescaped be the empty String.
    const extra_unescaped = "";

    // 3. Return ? Encode(componentString, extraUnescaped).
    return Value.from(try encode(agent, component_string, extra_unescaped));
}

/// 19.2.6.5 Encode ( string, extraUnescaped )
/// https://tc39.es/ecma262/#sec-encode
fn encode(
    agent: *Agent,
    string: String,
    comptime extra_unescaped: []const u8,
) Agent.Error!String {
    // 3. Let alwaysUnescaped be the string-concatenation of the ASCII word characters and
    //    "-.!~*'()".
    const always_unescaped = String.ascii_word_characters ++ "-.!~*'()";

    // 4. Let unescapedSet be the string-concatenation of alwaysUnescaped and extraUnescaped.
    const unescaped_set = always_unescaped ++ extra_unescaped;

    // OPTIMIZATION: If the string is ASCII we don't have to handle unpaired surrogates.
    if (string.data.slice == .ascii) {
        var buffer = std.ArrayList(u8).init(agent.gc_allocator);
        try std.Uri.Component.percentEncode(buffer.writer(), string.data.slice.ascii, struct {
            fn isValidChar(c: u8) bool {
                return std.mem.indexOfScalar(u8, unescaped_set, c) != null;
            }
        }.isValidChar);
        return String.fromAscii(agent.gc_allocator, try buffer.toOwnedSlice());
    }

    // 1. Let len be the length of string.
    const len = string.length();

    // 2. Let R be the empty String.
    var result = String.Builder.init(agent.gc_allocator);
    defer result.deinit();

    // 5. Let k be 0.
    var k: usize = 0;

    // 6. Repeat, while k < len,
    while (k < len) {
        // a. Let C be the code unit at index k within string.
        const c = string.codeUnitAt(k);

        // b. If unescapedSet contains C, then
        if (c <= std.math.maxInt(u8) and
            std.mem.indexOfScalar(u8, unescaped_set, @intCast(c)) != null)
        {
            // i. Set k to k + 1.
            k += 1;

            // ii. Set R to the string-concatenation of R and C.
            try result.appendChar(@intCast(c));
        }
        // c. Else,
        else {
            // i. Let cp be CodePointAt(string, k).
            const code_point = string.codePointAt(k);

            // ii. If cp.[[IsUnpairedSurrogate]] is true, throw a URIError exception.
            if (code_point.is_unpaired_surrogate) {
                return agent.throwException(.uri_error, "URI contains unpaired surrogate", .{});
            }

            // iii. Set k to k + cp.[[CodeUnitCount]].
            k += code_point.code_unit_count;

            // iv. Let Octets be the List of octets resulting by applying the UTF-8 transformation
            //     to cp.[[CodePoint]].
            var buf: [4]u8 = undefined;
            const size = std.unicode.utf8Encode(code_point.code_point, &buf) catch unreachable;

            // v. For each element octet of Octets, do
            for (buf[0..size]) |byte| {
                // 1. Let hex be the String representation of octet, formatted as an uppercase
                //    hexadecimal number.
                // 2. Set R to the string-concatenation of R, "%", and StringPad(hex, 2, "0", start).
                try result.appendString(
                    try String.fromAscii(
                        agent.gc_allocator,
                        try std.fmt.allocPrint(agent.gc_allocator, "%{X:0>2}", .{byte}),
                    ),
                );
            }
        }
    }

    // 7. Return R.
    return result.build();
}

/// 19.2.6.6 Decode ( string, preserveEscapeSet )
/// https://tc39.es/ecma262/#sec-decode
fn decode(agent: *Agent, string: String, comptime preserve_escape_set: []const u8) Agent.Error!String {
    const input = try string.toUtf16(agent.gc_allocator);

    // 1. Let len be the length of string.
    const len = input.len;

    // 2. Let R be the empty String.
    var result = String.Builder.init(agent.gc_allocator);
    defer result.deinit();

    // 3. Let k be 0.
    var k: usize = 0;

    // 4. Repeat, while k < len,
    while (k < len) : (k += 1) {
        // a. Let C be the code unit at index k within string.
        const c = input[k];

        // b. Let S be C.
        var s: String.Builder.Segment = .{ .code_unit = c };

        // c. If C is the code unit 0x0025 (PERCENT SIGN), then
        if (c == '%') {
            // i. If k + 3 > len, throw a URIError exception.
            if (k + 3 > len) {
                return agent.throwException(.uri_error, "Escape sequence must be of form '%XX'", .{});
            }

            // ii. Let escape be the substring of string from k to k + 3.
            const escape_ = input[k .. k + 3];

            // iii. Let B be ParseHexOctet(string, k + 1).
            const byte = parseHexOctet(input, k + 1) orelse {
                // iv. If B is not an integer, throw a URIError exception.
                return agent.throwException(.uri_error, "Escape sequence must be hex digits", .{});
            };

            // v. Set k to k + 2.
            k += 2;

            // vi. Let n be the number of leading 1 bits in B.
            const byte_sequence_length = std.unicode.utf8ByteSequenceLength(byte) catch null;

            // vii. If n = 0, then
            if (byte_sequence_length == 1) {
                // 1. Let asciiChar be the code unit whose numeric value is B.
                // 2. If preserveEscapeSet contains asciiChar, set S to escape. Otherwise, set S to
                //    asciiChar.
                s = if (std.mem.indexOfScalar(u8, preserve_escape_set, byte) != null)
                    .{ .string = try String.fromUtf16(agent.gc_allocator, escape_) }
                else
                    .{ .code_unit = byte };
            }
            // viii. Else,
            else {
                // 1. If n = 1 or n > 4, throw a URIError exception.
                if (byte_sequence_length == null) {
                    return agent.throwException(.uri_error, "Invalid UTF-8 start byte", .{});
                }

                // 2. Let Octets be « B ».
                var octets = [4]u8{ byte, 0, 0, 0 };

                // 3. Let j be 1.
                var j: u3 = 1;

                // 4. Repeat, while j < n,
                while (j < byte_sequence_length.?) : (j += 1) {
                    // a. Set k to k + 1.
                    k += 1;

                    // b. If k + 3 > len, throw a URIError exception.
                    // c. If the code unit at index k within string is not the code unit 0x0025
                    //    (PERCENT SIGN), throw a URIError exception.
                    if (k + 3 > len or input[k] != '%') {
                        return agent.throwException(.uri_error, "Escape sequence must be of form '%XX'", .{});
                    }

                    // d. Let continuationByte be ParseHexOctet(string, k + 1).
                    const continuation_byte = parseHexOctet(input, k + 1) orelse {
                        // e. If continuationByte is not an integer, throw a URIError exception.
                        return agent.throwException(.uri_error, "Escape sequence must be hex digits", .{});
                    };

                    // f. Append continuationByte to Octets.
                    octets[j] = continuation_byte;

                    // g. Set k to k + 2.
                    k += 2;

                    // h. Set j to j + 1.
                }

                // 5. Assert: The length of Octets is n.
                // 6. If Octets does not contain a valid UTF-8 encoding of a Unicode code point,
                //    throw a URIError exception.
                const code_point = std.unicode.utf8Decode(octets[0..byte_sequence_length.?]) catch {
                    return agent.throwException(.uri_error, "Invalid UTF-8 byte sequence", .{});
                };

                // 7. Let V be the code point obtained by applying the UTF-8 transformation to
                //    Octets, that is, from a List of octets into a 21-bit value.
                // 8. Set S to UTF16EncodeCodePoint(V).
                s = .{ .code_point = code_point };
            }
        }

        // d. Set R to the string-concatenation of R and S.
        try result.appendSegment(s);

        // e. Set k to k + 1.
    }

    // 5. Return R.
    return result.build();
}

/// 19.2.6.7 ParseHexOctet ( string, position )
/// https://tc39.es/ecma262/#sec-parsehexoctet
fn parseHexOctet(string: []const u16, position: usize) ?u8 {
    // 1. Let len be the length of string.
    const len = string.len;

    // 2. Assert: position + 2 ≤ len.
    std.debug.assert(position + 2 <= len);

    // 3. Let hexDigits be the substring of string from position to position + 2.
    const hex_digits = string[position .. position + 2];

    // 4. Let parseResult be ParseText(hexDigits, HexDigits[~Sep]).
    // 5. If parseResult is not a Parse Node, return parseResult.
    // 6. Let n be the MV of parseResult.
    // 7. Assert: n is in the inclusive interval from 0 to 255.
    var buf: [2]u8 = undefined;
    buf[0] = std.math.cast(u8, hex_digits[0]) orelse return null;
    buf[1] = std.math.cast(u8, hex_digits[1]) orelse return null;
    const n = std.fmt.parseInt(u8, &buf, 16) catch return null;

    // 8. Return n.
    return n;
}

/// B.2.1.1 escape ( string )
/// https://tc39.es/ecma262/#sec-escape-string
fn escape(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const string_value = arguments.get(0);

    // 1. Set string to ? ToString(string).
    const string = try string_value.toString(agent);

    // 3. Let R be the empty String.
    // NOTE: This allocates the exact needed capacity upfront
    var result = try String.Builder.initCapacity(agent.gc_allocator, string.length());
    defer result.deinit();

    // 4. Let unescapedSet be the string-concatenation of the ASCII word characters and "@*+-./".
    const unescaped_set = String.ascii_word_characters ++ "@*+-./";

    // 2. Let len be the length of string.
    // 5. Let k be 0.
    // 6. Repeat, while k < len,
    //     a. Let C be the code unit at index k within string.
    var it = string.codeUnitIterator();
    while (it.next()) |c| {
        // b. If unescapedSet contains C, then
        const s: String.Builder.Segment = if (c < 256 and std.mem.indexOfScalar(u8, unescaped_set, @intCast(c)) != null) blk: {
            // i. Let S be C.
            break :blk .{ .char = @intCast(c) };
        }
        // c. Else,
        else blk: {
            // i. Let n be the numeric value of C.
            // ii. If n < 256, then
            if (c < 256) {
                // 1. Let hex be the String representation of n, formatted as an uppercase
                //    hexadecimal number.
                // 2. Let S be the string-concatenation of "%" and StringPad(hex, 2, "0", start).
                break :blk .{
                    .string = try String.fromAscii(
                        agent.gc_allocator,
                        try std.fmt.allocPrint(
                            agent.gc_allocator,
                            "%{}",
                            .{std.fmt.fmtSliceHexUpper(&.{@intCast(c)})},
                        ),
                    ),
                };
            }
            // iii. Else,
            else {
                // 1. Let hex be the String representation of n, formatted as an uppercase
                //    hexadecimal number.
                // 2. Let S be the string-concatenation of "%u" and StringPad(hex, 4, "0", start).
                var bytes = std.mem.toBytes(c);
                std.mem.reverse(u8, &bytes);
                break :blk .{
                    .string = try String.fromAscii(
                        agent.gc_allocator,
                        try std.fmt.allocPrint(
                            agent.gc_allocator,
                            "%u{}",
                            .{std.fmt.fmtSliceHexUpper(&bytes)},
                        ),
                    ),
                };
            }
        };

        // d. Set R to the string-concatenation of R and S.
        result.appendSegmentAssumeCapacity(s);

        // e. Set k to k + 1.
    }

    // 7. Return R.
    return Value.from(try result.build());
}

/// B.2.1.2 unescape ( string )
/// https://tc39.es/ecma262/#sec-unescape-string
fn unescape(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const string_value = arguments.get(0);

    // 1. Set string to ? ToString(string).
    const string = try string_value.toString(agent);

    // 2. Let len be the length of string.
    const len = string.length();

    // 3. Let R be the empty String.
    var result = String.Builder.init(agent.gc_allocator);
    defer result.deinit();

    const code_units = try string.toUtf16(agent.gc_allocator);
    defer agent.gc_allocator.free(code_units);

    // 4. Let k be 0.
    var k: usize = 0;

    // 5. Repeat, while k < len,
    while (k < len) : (k += 1) {
        // a. Let C be the code unit at index k within string.
        var c = code_units[k];

        // b. If C is the code unit 0x0025 (PERCENT SIGN), then
        if (c == '%') {
            // i. Let hexDigits be the empty String.
            var hex_digits: []const u16 = &.{};

            // ii. Let optionalAdvance be 0.
            var optional_advance: usize = 0;

            // iii. If k + 5 < len and the code unit at index k + 1 within string is the code unit
            //      0x0075 (LATIN SMALL LETTER U), then
            if (k + 5 < len and code_units[k + 1] == 'u') {
                // 1. Set hexDigits to the substring of string from k + 2 to k + 6.
                hex_digits = code_units[k + 2 .. k + 6];

                // 2. Set optionalAdvance to 5.
                optional_advance = 5;
            }
            // iv. Else if k + 3 ≤ len, then
            else if (k + 3 <= len) {
                // 1. Set hexDigits to the substring of string from k + 1 to k + 3.
                hex_digits = code_units[k + 1 .. k + 3];

                // 2. Set optionalAdvance to 2.
                optional_advance = 2;
            }

            // NOTE: This will succeed if all code points are ASCII, which is required for the hex
            //       parsing to work anyway.
            var buf: [4]u8 = undefined;
            if (std.unicode.utf16LeToUtf8(&buf, hex_digits) catch null) |end| {
                // v. Let parseResult be ParseText(hexDigits, HexDigits[~Sep]).
                // vi. If parseResult is a Parse Node, then
                if (std.fmt.parseInt(u16, buf[0..end], 16)) |n| {
                    // 1. Let n be the MV of parseResult.
                    // 2. Set C to the code unit whose numeric value is n.
                    c = n;

                    // 3. Set k to k + optionalAdvance.
                    k += optional_advance;
                } else |_| {}
            }
        }

        // c. Set R to the string-concatenation of R and C.
        try result.appendCodeUnit(c);

        // d. Set k to k + 1.
    }

    // 6. Return R.
    return Value.from(try result.build());
}
