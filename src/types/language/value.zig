const std = @import("std");

const execution = @import("../../execution.zig");

const Agent = execution.Agent;
const BigInt = @import("BigInt.zig");
const Number = @import("number.zig").Number;
const Object = @import("Object.zig");
const Symbol = @import("Symbol.zig");

/// 6.1 ECMAScript Language Types
/// https://tc39.es/ecma262/#sec-ecmascript-language-types
pub const Value = union(enum) {
    const Self = @This();

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

    pub fn nan() Value {
        return .{ .number = .{ .f64 = std.math.nan(f64) } };
    }

    pub fn infinity() Value {
        return .{ .number = .{ .f64 = std.math.inf(f64) } };
    }

    pub fn negativeInfinity() Value {
        return .{ .number = .{ .f64 = -std.math.inf(f64) } };
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
        switch (@typeInfo(@TypeOf(number))) {
            .Int, .ComptimeInt => return .{
                .number = .{ .i32 = @as(i32, number) },
            },
            .Float, .ComptimeFloat => {
                const truncated = std.math.trunc(number);
                if (std.math.isFinite(@as(f64, number)) and
                    truncated == number and
                    truncated <= std.math.maxInt(i32))
                {
                    return .{ .number = .{ .i32 = @floatToInt(i32, truncated) } };
                }
                return .{ .number = .{ .f64 = @as(f64, number) } };
            },
            else => @compileError("Value.fromNumber() called with incompatible type " ++ @typeName(@TypeOf(number))),
        }
    }

    pub fn fromBigInt(big_int: BigInt) Value {
        return .{ .big_int = big_int };
    }

    pub fn fromObject(object: Object) Value {
        return .{ .object = object };
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
        _ = agent;

        // 1. If argumentsList is not present, set argumentsList to a new empty List.
        // This is done via the NoArgs variant of the function.

        // 2. If IsCallable(F) is false, throw a TypeError exception.
        if (!self.isCallable())
            return error.ExceptionThrown;

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
