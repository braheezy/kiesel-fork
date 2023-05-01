const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../execution.zig");

const Agent = execution.Agent;
const BigInt = @import("BigInt.zig");
const Number = @import("number.zig").Number;
const Object = @import("Object.zig");
const Symbol = @import("Symbol.zig");

const pow_2_7 = std.math.pow(f64, 2, 7);
const pow_2_8 = std.math.pow(f64, 2, 8);
const pow_2_15 = std.math.pow(f64, 2, 15);
const pow_2_16 = std.math.pow(f64, 2, 16);
const pow_2_31 = std.math.pow(f64, 2, 31);
const pow_2_32 = std.math.pow(f64, 2, 32);

/// 6.1 ECMAScript Language Types
/// https://tc39.es/ecma262/#sec-ecmascript-language-types
pub const Value = union(enum) {
    const Self = @This();

    pub const PreferredType = enum { string, number };

    /// 6.1.1 The Undefined Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-undefined-type
    undefined,

    /// 6.1.2 The Null Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-null-type
    null,

    /// 6.1.3 The Boolean Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-boolean-type
    boolean: bool,

    /// 6.1.4 The String Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type
    string: []const u8,

    /// 6.1.5 The Symbol Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type
    symbol: Symbol,

    /// 6.1.6.1 The Number Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-number-type
    number: Number,

    /// 6.1.6.2 The BigInt Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type
    big_int: BigInt,

    /// 6.1.7 The Object Type
    /// https://tc39.es/ecma262/#sec-object-type
    object: Object,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .undefined => try writer.writeAll("undefined"),
            .null => try writer.writeAll("null"),
            .boolean => |boolean| try writer.writeAll(if (boolean) "true" else "false"),
            .string => |string| {
                try writer.writeAll("\"");
                try writer.writeAll(string);
                try writer.writeAll("\"");
            },
            .symbol => |symbol| try writer.print("{}", .{symbol}),
            .number => |number| try writer.print("{}", .{number}),
            .big_int => |big_int| try writer.print("{}", .{big_int}),
            .object => |object| try writer.print("{}", .{object}),
        }
    }

    pub fn nan() Value {
        return .{ .number = Number.from(std.math.nan(f64)) };
    }

    pub fn infinity() Value {
        return .{ .number = Number.from(std.math.inf(f64)) };
    }

    pub fn negativeInfinity() Value {
        return .{ .number = Number.from(-std.math.inf(f64)) };
    }

    pub fn fromBoolean(boolean: bool) Value {
        return .{ .boolean = boolean };
    }

    pub fn fromString(string: []const u8) Value {
        return .{ .string = string };
    }

    pub fn fromSymbol(symbol: Symbol) Value {
        return .{ .symbol = symbol };
    }

    pub fn fromNumber(number: anytype) Value {
        return .{ .number = Number.from(number) };
    }

    pub fn fromBigInt(big_int: BigInt) Value {
        return .{ .big_int = big_int };
    }

    pub fn fromObject(object: Object) Value {
        return .{ .object = object };
    }

    /// 7.1.1 ToPrimitive ( input [ , preferredType ] )
    /// https://tc39.es/ecma262/#sec-toprimitive
    pub fn toPrimitive(self: Self, agent: *Agent, preferred_type: ?PreferredType) !Value {
        // 1. If input is an Object, then
        if (self == .object) {
            // a. Let exoticToPrim be ? GetMethod(input, @@toPrimitive).
            const exotic_to_primitive = Value.undefined;

            // b. If exoticToPrim is not undefined, then
            if (exotic_to_primitive != .undefined) {
                const hint = blk: {
                    // i. If preferredType is not present, then
                    if (preferred_type == null) {
                        // 1. Let hint be "default".
                        break :blk "default";
                    }
                    break :blk switch (preferred_type.?) {
                        // ii. Else if preferredType is string, then
                        //     1. Let hint be "string".
                        .string => "string",
                        // iii. Else,
                        //     1. Assert: preferredType is number.
                        //     2. Let hint be "number".
                        .number => "number",
                    };
                };

                // iv. Let result be ? Call(exoticToPrim, input, « hint »).
                const result = exotic_to_primitive.call(agent, self, [_]Value{hint});

                // v. If result is not an Object, return result.
                if (result != .object)
                    return result;

                // vi. Throw a TypeError exception.
                return agent.throwException(
                    .type_error,
                    "Could not convert object to primitive",
                );
            }

            // c. If preferredType is not present, let preferredType be number.
            // d. Return ? OrdinaryToPrimitive(input, preferredType).
            return self.object.ordinaryToPrimitive(preferred_type orelse .number);
        }

        // 2. Return input.
        return self;
    }

    /// 7.1.2 ToBoolean ( argument )
    /// https://tc39.es/ecma262/#sec-toboolean
    pub fn toBoolean(self: Self) bool {
        // 1. If argument is a Boolean, return argument.
        if (self == .boolean)
            return self.boolean;

        // 2. If argument is one of undefined, null, +0𝔽, -0𝔽, NaN, 0ℤ, or the empty String, return false.
        switch (self) {
            .undefined, .null => return false,
            .number => |number| if (number.asFloat() == 0 or number.isNan()) {
                return false;
            },
            .big_int => |big_int| if (big_int.value.eqZero()) {
                return false;
            },
            .string => |string| if (string.len == 0) {
                return false;
            },
            else => {},
        }

        // 3. NOTE: This step is replaced in section B.3.6.1.

        // 4. Return true.
        return true;
    }

    /// 7.1.3 ToNumeric ( value )
    /// https://tc39.es/ecma262/#sec-tonumeric
    pub fn toNumeric(self: Self, agent: *Agent) !Number {
        // 1. Let primValue be ? ToPrimitive(value, number).
        const primitive_value = try self.toPrimitive(agent, .number);

        // 2. If primValue is a BigInt, return primValue.
        if (primitive_value == .big_int)
            return primitive_value.big_int;

        // 3. Return ? ToNumber(primValue).
        return primitive_value.toNumber(agent);
    }

    /// 7.1.4 ToNumber ( argument )
    /// https://tc39.es/ecma262/#sec-tonumber
    pub fn toNumber(self: Self, agent: *Agent) !Number {
        switch (self) {
            // 1. If argument is a Number, return argument.
            .number => |number| return number,

            // 2. If argument is either a Symbol or a BigInt, throw a TypeError exception.
            .symbol => return agent.throwException(
                .type_error,
                "Cannot convert Symbol to number",
            ),
            .big_int => return agent.throwException(
                .type_error,
                "Cannot convert BigInt to number",
            ),

            // 3. If argument is undefined, return NaN.
            .undefined => return Number.from(std.math.nan(f64)),

            // 4. If argument is either null or false, return +0𝔽.
            // 5. If argument is true, return 1𝔽.
            .null => return Number.from(0),
            .boolean => |boolean| return Number.from(@boolToInt(boolean)),

            // 6. If argument is a String, return StringToNumber(argument).
            .string => |string| return stringToNumber(string),

            // 7. Assert: argument is an Object.
            .object => {
                // 8. Let primValue be ? ToPrimitive(argument, number).
                const primitive_value = try self.toPrimitive(agent, .number);

                // 9. Assert: primValue is not an Object.
                std.debug.assert(primitive_value != .object);

                // 10. Return ? ToNumber(primValue).
                return primitive_value.toNumber(agent);
            },
        }
    }

    /// 7.1.5 ToIntegerOrInfinity ( argument )
    /// https://tc39.es/ecma262/#sec-tointegerorinfinity
    pub fn toIntegerOrInfinity(self: Self, agent: *Agent) !f64 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is one of NaN, +0𝔽, or -0𝔽, return 0.
        if (number.isNan() or number.asFloat() == 0)
            return 0;

        // 3. If number is +∞𝔽, return +∞.
        if (number.isPositiveInf())
            return std.math.inf(f64);

        // 4. If number is -∞𝔽, return -∞.
        if (number.isNegativeInf())
            return -std.math.inf(f64);

        // 5. Return truncate(ℝ(number)).
        return number.truncate();
    }

    /// 7.1.6 ToInt32 ( argument )
    /// https://tc39.es/ecma262/#sec-toint32
    pub fn toInt32(self: Self, agent: *Agent) !i32 {
        // OPTIMIZATION: We may already have an i32 :^)
        if (self == .number and self.number == .i32)
            return self.number.i32;

        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0)
            return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int32bit be int modulo 2^32.
        const int32bit = @mod(int, pow_2_32);

        // 5. If int32bit ≥ 2^31, return 𝔽(int32bit - 2^32); otherwise return 𝔽(int32bit).
        return @floatToInt(i32, if (int32bit >= pow_2_31) int32bit - pow_2_32 else int32bit);
    }

    /// 7.1.7 ToUint32 ( argument )
    /// https://tc39.es/ecma262/#sec-touint32
    pub fn toUint32(self: Self, agent: *Agent) !u32 {
        // OPTIMIZATION: We may already have a positive i32 :^)
        if (self == .number and self.number == .i32 and self.number.i32 >= 0)
            return @intCast(u32, self.number.i32);

        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0)
            return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int32bit be int modulo 2^32.
        const int32bit = @mod(int, pow_2_32);

        // 5. Return 𝔽(int32bit).
        return @floatToInt(u32, int32bit);
    }

    /// 7.1.8 ToInt16 ( argument )
    /// https://tc39.es/ecma262/#sec-toint16
    pub fn toInt16(self: Self, agent: *Agent) !i16 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0)
            return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int16bit be int modulo 2^16.
        const int16bit = @mod(int, pow_2_16);

        // 5. If int16bit ≥ 2^15, return 𝔽(int16bit - 2^16); otherwise return 𝔽(int16bit).
        return @floatToInt(i16, if (int16bit >= pow_2_15) int16bit - pow_2_16 else int16bit);
    }

    /// 7.1.9 ToUint16 ( argument )
    /// https://tc39.es/ecma262/#sec-touint16
    pub fn toUint16(self: Self, agent: *Agent) !u16 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0)
            return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int16bit be int modulo 2^16.
        const int16bit = @mod(int, pow_2_16);

        // 5. Return 𝔽(int16bit).
        return @floatToInt(u16, int16bit);
    }

    /// 7.1.10 ToInt8 ( argument )
    /// https://tc39.es/ecma262/#sec-toint8
    pub fn toInt8(self: Self, agent: *Agent) !i8 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0)
            return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int8bit be int modulo 2^8.
        const int8bit = @mod(int, pow_2_8);

        // 5. If int8bit ≥ 2^7, return 𝔽(int8bit - 2^8); otherwise return 𝔽(int8bit).
        return @floatToInt(i8, if (int8bit >= pow_2_7) int8bit - pow_2_8 else int8bit);
    }

    /// 7.1.11 ToUint8 ( argument )
    /// https://tc39.es/ecma262/#sec-touint8
    pub fn toUint8(self: Self, agent: *Agent) !u8 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0)
            return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int8bit be int modulo 2^8.
        const int8bit = @mod(int, pow_2_8);

        // 5. Return 𝔽(int8bit).
        return @floatToInt(u8, int8bit);
    }

    /// 7.1.12 ToUint8Clamp ( argument )
    /// https://tc39.es/ecma262/#sec-touint8clamp
    pub fn toUint8Clamp(self: Self, agent: *Agent) !u8 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is NaN, return +0𝔽.
        if (number.isNan())
            return 0;

        // 3. If ℝ(number) ≤ 0, return +0𝔽.
        // 4. If ℝ(number) ≥ 255, return 255𝔽.
        switch (number) {
            .f64 => |x| {
                if (x <= 0) return 0;
                if (x >= 255) return 255;
            },
            .i32 => |x| {
                if (x <= 0) return 0;
                if (x >= 255) return 255;
            },
        }

        // 5. Let f be floor(ℝ(number)).
        const f = number.floor().asFloat();
        const f_int = @floatToInt(u8, f);

        // 6. If f + 0.5 < ℝ(number), return 𝔽(f + 1).
        if (f + 0.5 < number.asFloat())
            return f_int + 1;

        // 7. If ℝ(number) < f + 0.5, return 𝔽(f).
        if (number.asFloat() < f + 0.5)
            return f_int;

        // 8. If f is odd, return 𝔽(f + 1).
        if (f_int % 2 != 0)
            return f_int + 1;

        // 9. Return 𝔽(f).
        return f_int;
    }

    /// 7.1.13 ToBigInt ( argument )
    /// https://tc39.es/ecma262/#sec-tobigint
    pub fn toBigInt(self: Self, agent: *Agent) !BigInt {
        // 1. Let prim be ? ToPrimitive(argument, number).
        const primitive = try self.toPrimitive(agent, .number);

        // 2. Return the value that prim corresponds to in Table 12.
        return switch (primitive) {
            // Throw a TypeError exception.
            .undefined => agent.throwException(.type_error, "Cannot convert undefined to BigInt"),
            .null => agent.throwException(.type_error, "Cannot convert null to BigInt"),
            .number => agent.throwException(.type_error, "Cannot convert number to BigInt"),
            .symbol => agent.throwException(.type_error, "Cannot convert symbol to BigInt"),

            // Return 1n if prim is true and 0n if prim is false.
            .boolean => |boolean| BigInt{
                .value = try BigInt.Value.initSet(agent.allocator, @boolToInt(boolean)),
            },

            // Return prim.
            .big_int => |big_int| big_int,

            .string => |string| {
                // 1. Let n be StringToBigInt(prim).
                const n = try stringToBigInt(agent.allocator, string);

                // 2. If n is undefined, throw a SyntaxError exception.
                // 3. Return n.
                return n orelse agent.throwException(
                    .syntax_error,
                    "Cannot convert string to BigInt",
                );
            },

            .object => unreachable,
        };
    }

    /// 7.1.17 ToString ( argument )
    /// https://tc39.es/ecma262/#sec-tostring
    pub fn toString(self: Self, agent: *Agent) ![]const u8 {
        return switch (self) {
            // 1. If argument is a String, return argument.
            .string => |string| string,

            // 2. If argument is a Symbol, throw a TypeError exception.
            .symbol => return agent.throwException(
                .type_error,
                "Cannot convert Symbol to string",
            ),

            // 3. If argument is undefined, return "undefined".
            .undefined => "undefined",

            // 4. If argument is null, return "null".
            .null => "null",

            // 5. If argument is true, return "true".
            // 6. If argument is false, return "false".
            .boolean => |boolean| if (boolean) "true" else "false",

            // 7. If argument is a Number, return Number::toString(argument, 10).
            .number => |number| number.toString(agent.allocator, 10),

            // 8. If argument is a BigInt, return BigInt::toString(argument, 10).
            .big_int => |big_int| big_int.toString(agent.allocator, 10),

            // 9. Assert: argument is an Object.
            .object => {
                // 10. Let primValue be ? ToPrimitive(argument, string).
                const primitive_value = try self.toPrimitive(agent, .string);

                // 11. Assert: primValue is not an Object.
                std.debug.assert(primitive_value != .object);

                // 12. Return ? ToString(primValue).
                return primitive_value.toString(agent);
            },
        };
    }

    /// 7.2.2 IsArray ( argument )
    /// https://tc39.es/ecma262/#sec-isarray
    pub fn isArray(self: Self) !bool {
        // 1. If argument is not an Object, return false.
        if (!self == .object)
            return false;

        // TODO: 2. If argument is an Array exotic object, return true.
        if (false)
            return true;

        // TODO: 3. If argument is a Proxy exotic object, then
        if (false) {
            // TODO: a. Perform ? ValidateNonRevokedProxy(argument).
            // TODO: b. Let proxyTarget be argument.[[ProxyTarget]].
            // TODO: c. Return ? IsArray(proxyTarget).
        }

        // 4. Return false.
        return false;
    }

    /// 7.2.3 IsCallable ( argument )
    /// https://tc39.es/ecma262/#sec-iscallable
    pub fn isCallable(self: Self) bool {
        // 1. If argument is not an Object, return false.
        if (self != .object)
            return false;

        // 2. If argument has a [[Call]] internal method, return true.
        if (self.object.internalMethods().call != null)
            return true;

        // 3. Return false.
        return false;
    }

    /// 7.2.4 IsConstructor ( argument )
    /// https://tc39.es/ecma262/#sec-isconstructor
    pub fn isConstructor(self: Self) bool {
        // 1. If argument is not an Object, return false.
        if (!self == .object)
            return false;

        // 2. If argument has a [[Construct]] internal method, return true.
        if (self.object.internalMethods().construct != null)
            return true;

        // 3. Return false.
        return false;
    }

    /// 7.3.14 Call ( F, V [ , argumentsList ] )
    /// https://tc39.es/ecma262/#sec-call
    pub fn call(self: Self, agent: *Agent, this_value: Value, arguments_list: []const Value) !Value {
        // 1. If argumentsList is not present, set argumentsList to a new empty List.
        // This is done via the NoArgs variant of the function.

        // 2. If IsCallable(F) is false, throw a TypeError exception.
        if (!self.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.allocator, "{} is not callable", .{self}),
            );
        }

        // 3. Return ? F.[[Call]](V, argumentsList).
        return self.object.internalMethods().call.?(self.object, this_value, arguments_list);
    }

    pub fn callNoArgs(self: Self, agent: *Agent, this_value: Value) !Value {
        return self.call(agent, this_value, &[_]Value{});
    }

    pub fn callAssumeCallable(self: Self, this_value: Value, arguments_list: []const Value) !Value {
        return self.object.internalMethods().call.?(self.object, this_value, arguments_list);
    }

    pub fn callAssumeCallableNoArgs(self: Self, this_value: Value) !Value {
        return self.callAssumeCallable(this_value, &[_]Value{});
    }
};

/// 7.1.4.1.1 StringToNumber ( str )
/// https://tc39.es/ecma262/#sec-stringtonumber
pub fn stringToNumber(string: []const u8) Number {
    // 1. Let text be StringToCodePoints(str).

    // 2. Let literal be ParseText(text, StringNumericLiteral).
    // 3. If literal is a List of errors, return NaN.
    // 4. Return StringNumericValue of literal.
    // TODO: Implement the proper string parsing grammar!
    return Number.from(std.fmt.parseFloat(f64, string) catch std.math.nan(f64));
}

/// 7.1.14 StringToBigInt ( str )
/// https://tc39.es/ecma262/#sec-stringtobigint
pub fn stringToBigInt(allocator: Allocator, string: []const u8) !?BigInt {

    // 1. Let text be StringToCodePoints(str).

    // 2. Let literal be ParseText(text, StringIntegerLiteral).
    // 3. If literal is a List of errors, return undefined.
    // 4. Let mv be the MV of literal.
    // 5. Assert: mv is an integer.
    // 6. Return ℤ(mv).
    // TODO: Implement the proper string parsing grammar!
    var value = try BigInt.Value.init(allocator);
    value.setString(10, string) catch |err| return switch (err) {
        error.InvalidCharacter => null,
        else => err,
    };
    return BigInt{ .value = value };
}

test "format" {
    const builtins = @import("../../builtins.zig");
    var agent = Agent.init();
    const object = (try builtins.Object.create(&agent, .{
        .prototype = null,
    })).object();
    const test_cases = [_]struct { Value, []const u8 }{
        .{ Value.undefined, "undefined" },
        .{ Value.null, "null" },
        .{ Value.fromBoolean(true), "true" },
        .{ Value.fromBoolean(false), "false" },
        .{ Value.fromString("foo"), "\"foo\"" },
        .{ Value.fromSymbol(Symbol{ .id = 0, .description = null }), "Symbol()" },
        .{ Value.fromSymbol(Symbol{ .id = 0, .description = "foo" }), "Symbol(\"foo\")" },
        .{ Value.fromBigInt(BigInt{ .value = try BigInt.Value.initSet(std.testing.allocator, 123) }), "123n" },
        .{ Value.fromObject(object), "[object Object]" },
    };
    for (test_cases) |test_case| {
        const value = test_case[0];
        const expected = test_case[1];
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{value});
        defer std.testing.allocator.free(string);
        defer if (value == .big_int) @constCast(&value.big_int).value.deinit();
        try std.testing.expectEqualStrings(expected, string);
    }
}

test "Value.nan" {
    try std.testing.expect(std.math.isNan(Value.nan().number.f64));
}

test "Value.infinity" {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(Value.infinity().number.f64, inf);
}

test "Value.negativeInfinity" {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(Value.negativeInfinity().number.f64, -inf);
}

test "Value.fromBoolean" {
    try std.testing.expectEqual(Value.fromBoolean(true).boolean, true);
    try std.testing.expectEqual(Value.fromBoolean(false).boolean, false);
}

test "Value.fromString" {
    try std.testing.expectEqual(Value.fromString("").string, "");
    try std.testing.expectEqual(Value.fromString("foo").string, "foo");
    try std.testing.expectEqual(Value.fromString("123").string, "123");
}

test "Value.fromNumber" {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(Value.fromNumber(0).number.i32, 0);
    try std.testing.expectEqual(Value.fromNumber(0.0).number.i32, 0);
    try std.testing.expectEqual(Value.fromNumber(123).number.i32, 123);
    try std.testing.expectEqual(Value.fromNumber(123.0).number.i32, 123);
    try std.testing.expectEqual(Value.fromNumber(123.456).number.f64, 123.456);
    try std.testing.expectEqual(Value.fromNumber(std.math.inf(f64)).number.f64, inf);
    try std.testing.expect(std.math.isNan(Value.fromNumber(std.math.nan(f64)).number.f64));
}
