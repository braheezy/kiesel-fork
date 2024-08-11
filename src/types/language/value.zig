const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const build_options = @import("build-options");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const pretty_printing = @import("../../pretty_printing.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BigInt = @import("BigInt.zig");
const Iterator = types.Iterator;
const Number = @import("number.zig").Number;
const Object = @import("Object.zig");
const PrivateName = types.PrivateName;
const PropertyDescriptor = @import("../spec/PropertyDescriptor.zig");
const PropertyKey = Object.PropertyKey;
const PropertyKeyArrayHashMap = Object.PropertyStorage.PropertyKeyArrayHashMap;
const String = @import("string.zig").String;
const Symbol = @import("Symbol.zig");
const arrayCreate = builtins.arrayCreate;
const getIterator = types.getIterator;
const isZigString = utils.isZigString;
const keyForSymbol = builtins.keyForSymbol;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const prettyPrintValue = pretty_printing.prettyPrintValue;
const stringCreate = builtins.stringCreate;
const validateNonRevokedProxy = builtins.validateNonRevokedProxy;

const pow_2_8 = std.math.pow(f64, 2, 8);
const pow_2_16 = std.math.pow(f64, 2, 16);
const pow_2_32 = std.math.pow(f64, 2, 32);

/// 6.1 ECMAScript Language Types
/// https://tc39.es/ecma262/#sec-ecmascript-language-types
pub const Value = union(enum) {
    const Self = @This();

    pub const PreferredType = enum { string, number };

    pub const Numeric = union(enum) {
        number: Number,
        big_int: BigInt,
    };

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
    string: String,

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
    ) @TypeOf(writer).Error!void {
        _ = options;
        if (std.mem.eql(u8, fmt, "pretty")) {
            return prettyPrintValue(self, writer) catch |err| {
                // NOTE: When targeting Windows the error set contains error.Unexpected (from the
                //       `std.io.tty.Config.setColor()` calls), which `std.fmt.formatType()`
                //       doesn't include in its error set.
                if (builtin.os.tag == .windows) switch (err) {
                    error.Unexpected => {},
                    else => |e| return e,
                } else return err;
            };
        }
        switch (self) {
            .undefined => try writer.writeAll("undefined"),
            .null => try writer.writeAll("null"),
            .boolean => |boolean| try writer.writeAll(if (boolean) "true" else "false"),
            .string => |string| try writer.print("\"{}\"", .{string}),
            .symbol => |symbol| try writer.print("{}", .{symbol}),
            .number => |number| try writer.print("{}", .{number}),
            .big_int => |big_int| try writer.print("{}", .{big_int}),
            .object => |object| try writer.print("{}", .{object}),
        }
    }

    pub inline fn nan() Value {
        return .{ .number = Number.from(std.math.nan(f64)) };
    }

    pub inline fn infinity() Value {
        return .{ .number = Number.from(std.math.inf(f64)) };
    }

    pub inline fn negativeInfinity() Value {
        return .{ .number = Number.from(-std.math.inf(f64)) };
    }

    pub inline fn from(value: anytype) Value {
        const T = @TypeOf(value);
        const is_number = switch (@typeInfo(T)) {
            .Int, .ComptimeInt, .Float, .ComptimeFloat => true,
            else => false,
        };
        if (T == bool) {
            return .{ .boolean = value };
        } else if (isZigString(T)) {
            return .{ .string = String.fromLiteral(value) };
        } else if (is_number) {
            return .{ .number = Number.from(value) };
        } else if (T == BigInt) {
            return .{ .big_int = value };
        } else if (T == Number) {
            return .{ .number = value };
        } else if (T == Object) {
            return .{ .object = value };
        } else if (T == String) {
            return .{ .string = value };
        } else if (T == Symbol) {
            return .{ .symbol = value };
        } else {
            @compileError("Value.from() called with incompatible type " ++ @typeName(T));
        }
    }

    /// 6.2.6.5 ToPropertyDescriptor ( Obj )
    /// https://tc39.es/ecma262/#sec-topropertydescriptor
    pub fn toPropertyDescriptor(self: Self, agent: *Agent) Agent.Error!PropertyDescriptor {
        // 1. If Obj is not an Object, throw a TypeError exception.
        if (self != .object) {
            return agent.throwException(.type_error, "{} is not an Object", .{self});
        }

        // 2. Let desc be a new Property Descriptor that initially has no fields.
        var descriptor: PropertyDescriptor = .{};

        // 3. Let hasEnumerable be ? HasProperty(Obj, "enumerable").
        const has_enumerable = try self.object.hasProperty(PropertyKey.from("enumerable"));

        // 4. If hasEnumerable is true, then
        if (has_enumerable) {
            // a. Let enumerable be ToBoolean(? Get(Obj, "enumerable")).
            const enumerable = (try self.object.get(PropertyKey.from("enumerable"))).toBoolean();

            // b. Set desc.[[Enumerable]] to enumerable.
            descriptor.enumerable = enumerable;
        }

        // 5. Let hasConfigurable be ? HasProperty(Obj, "configurable").
        const has_configurable = try self.object.hasProperty(PropertyKey.from("configurable"));

        // 6. If hasConfigurable is true, then
        if (has_configurable) {
            // a. Let configurable be ToBoolean(? Get(Obj, "configurable")).
            const configurable = (try self.object.get(PropertyKey.from("configurable"))).toBoolean();

            // b. Set desc.[[Configurable]] to configurable.
            descriptor.configurable = configurable;
        }

        // 7. Let hasValue be ? HasProperty(Obj, "value").
        const has_value = try self.object.hasProperty(PropertyKey.from("value"));

        // 8. If hasValue is true, then
        if (has_value) {
            // a. Let value be ? Get(Obj, "value").
            const value = try self.object.get(PropertyKey.from("value"));

            // b. Set desc.[[Value]] to value.
            descriptor.value = value;
        }

        // 9. Let hasWritable be ? HasProperty(Obj, "writable").
        const has_writable = try self.object.hasProperty(PropertyKey.from("writable"));

        // 10. If hasWritable is true, then
        if (has_writable) {
            // a. Let writable be ToBoolean(? Get(Obj, "writable")).
            const writable = (try self.object.get(PropertyKey.from("writable"))).toBoolean();

            // b. Set desc.[[Writable]] to writable.
            descriptor.writable = writable;
        }

        // 11. Let hasGet be ? HasProperty(Obj, "get").
        const has_get = try self.object.hasProperty(PropertyKey.from("get"));

        // 12. If hasGet is true, then
        if (has_get) {
            // a. Let getter be ? Get(Obj, "get").
            const getter = try self.object.get(PropertyKey.from("get"));

            // b. If IsCallable(getter) is false and getter is not undefined, throw a TypeError
            //    exception.
            if (!getter.isCallable() and getter != .undefined) {
                return agent.throwException(.type_error, "{} is not callable", .{getter});
            }

            // c. Set desc.[[Get]] to getter.
            descriptor.get = if (getter != .undefined) getter.object else @as(?Object, null);
        }

        // 13. Let hasSet be ? HasProperty(Obj, "set").
        const has_set = try self.object.hasProperty(PropertyKey.from("set"));

        // 14. If hasSet is true, then
        if (has_set) {
            // a. Let setter be ? Get(Obj, "set").
            const setter = try self.object.get(PropertyKey.from("set"));

            // b. If IsCallable(setter) is false and setter is not undefined, throw a TypeError
            //    exception.
            if (!setter.isCallable() and setter != .undefined) {
                return agent.throwException(.type_error, "{} is not callable", .{setter});
            }

            // c. Set desc.[[Set]] to setter.
            descriptor.set = if (setter != .undefined) setter.object else @as(?Object, null);
        }

        // 15. If desc has a [[Get]] field or desc has a [[Set]] field, then
        if (descriptor.get != null or descriptor.set != null) {
            // a. If desc has a [[Value]] field or desc has a [[Writable]] field, throw a TypeError
            // exception.
            if (descriptor.value != null or descriptor.writable != null) {
                return agent.throwException(
                    .type_error,
                    "Descriptor with 'get' or 'set' property must not have 'value' or 'writable property'",
                    .{},
                );
            }
        }

        // 16. Return desc.
        return descriptor;
    }

    /// 7.1.1 ToPrimitive ( input [ , preferredType ] )
    /// https://tc39.es/ecma262/#sec-toprimitive
    pub fn toPrimitive(self: Self, agent: *Agent, preferred_type: ?PreferredType) Agent.Error!Value {
        // 1. If input is an Object, then
        if (self == .object) {
            // a. Let exoticToPrim be ? GetMethod(input, %Symbol.toPrimitive%).
            const maybe_exotic_to_primitive = try self.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.toPrimitive%"),
            );

            // b. If exoticToPrim is not undefined, then
            if (maybe_exotic_to_primitive) |exotic_to_primitive| {
                const hint = blk: {
                    // i. If preferredType is not present, then
                    if (preferred_type == null) {
                        // 1. Let hint be "default".
                        break :blk String.fromLiteral("default");
                    }
                    break :blk switch (preferred_type.?) {
                        // ii. Else if preferredType is string, then
                        //     1. Let hint be "string".
                        .string => String.fromLiteral("string"),
                        // iii. Else,
                        //     1. Assert: preferredType is number.
                        //     2. Let hint be "number".
                        .number => String.fromLiteral("number"),
                    };
                };

                // iv. Let result be ? Call(exoticToPrim, input, « hint »).
                const result = try Value.from(exotic_to_primitive).callAssumeCallable(
                    self,
                    &.{Value.from(hint)},
                );

                // v. If result is not an Object, return result.
                if (result != .object) return result;

                // vi. Throw a TypeError exception.
                return agent.throwException(
                    .type_error,
                    "Could not convert object to primitive",
                    .{},
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
        if (self == .boolean) return self.boolean;

        // 2. If argument is one of undefined, null, +0𝔽, -0𝔽, NaN, 0ℤ, or the empty String, return
        //    false.
        switch (self) {
            .undefined, .null => return false,
            .number => |number| if (number.asFloat() == 0 or number.isNan()) {
                return false;
            },
            .big_int => |big_int| if (big_int.value.eqlZero()) {
                return false;
            },
            .string => |string| if (string.isEmpty()) {
                return false;
            },
            else => {},
        }

        // B.3.6.1 Changes to ToBoolean
        // https://tc39.es/ecma262/#sec-IsHTMLDDA-internal-slot-to-boolean
        if (build_options.enable_annex_b) {
            // 3. If argument is an Object and argument has an [[IsHTMLDDA]] internal slot, return
            //    false.
            if (self == .object and self.object.isHTMLDDA()) return false;
        } else {
            // 3. NOTE: This step is replaced in section B.3.6.1.
        }

        // 4. Return true.
        return true;
    }

    /// 7.1.3 ToNumeric ( value )
    /// https://tc39.es/ecma262/#sec-tonumeric
    pub fn toNumeric(self: Self, agent: *Agent) Agent.Error!Numeric {
        // 1. Let primValue be ? ToPrimitive(value, number).
        const primitive_value = try self.toPrimitive(agent, .number);

        // 2. If primValue is a BigInt, return primValue.
        if (primitive_value == .big_int) return .{ .big_int = primitive_value.big_int };

        // 3. Return ? ToNumber(primValue).
        return .{ .number = try primitive_value.toNumber(agent) };
    }

    /// 7.1.4 ToNumber ( argument )
    /// https://tc39.es/ecma262/#sec-tonumber
    pub fn toNumber(self: Self, agent: *Agent) Agent.Error!Number {
        switch (self) {
            // 1. If argument is a Number, return argument.
            .number => |number| return number,

            // 2. If argument is either a Symbol or a BigInt, throw a TypeError exception.
            .symbol => return agent.throwException(
                .type_error,
                "Cannot convert Symbol to number",
                .{},
            ),
            .big_int => return agent.throwException(
                .type_error,
                "Cannot convert BigInt to number",
                .{},
            ),

            // 3. If argument is undefined, return NaN.
            .undefined => return Number.from(std.math.nan(f64)),

            // 4. If argument is either null or false, return +0𝔽.
            // 5. If argument is true, return 1𝔽.
            .null => return Number.from(0),
            .boolean => |boolean| return Number.from(@intFromBool(boolean)),

            // 6. If argument is a String, return StringToNumber(argument).
            .string => |string| return stringToNumber(agent.gc_allocator, string),

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
    pub fn toIntegerOrInfinity(self: Self, agent: *Agent) Agent.Error!f64 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is one of NaN, +0𝔽, or -0𝔽, return 0.
        if (number.isNan() or number.asFloat() == 0) return 0;

        // 3. If number is +∞𝔽, return +∞.
        if (number.isPositiveInf()) return std.math.inf(f64);

        // 4. If number is -∞𝔽, return -∞.
        if (number.isNegativeInf()) return -std.math.inf(f64);

        // 5. Return truncate(ℝ(number)).
        const truncated = number.truncate().asFloat();
        // Normalize negative zero
        return if (truncated == 0) 0 else truncated;
    }

    /// 7.1.6 ToInt32 ( argument )
    /// https://tc39.es/ecma262/#sec-toint32
    pub fn toInt32(self: Self, agent: *Agent) Agent.Error!i32 {
        // OPTIMIZATION: We may already have an i32 :^)
        if (self == .number and self.number == .i32) return self.number.i32;

        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0) return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int32bit be int modulo 2**32.
        const int32bit: u32 = @intFromFloat(@mod(int, pow_2_32));

        // 5. If int32bit ≥ 2**31, return 𝔽(int32bit - 2**32); otherwise return 𝔽(int32bit).
        return @bitCast(int32bit);
    }

    /// 7.1.7 ToUint32 ( argument )
    /// https://tc39.es/ecma262/#sec-touint32
    pub fn toUint32(self: Self, agent: *Agent) Agent.Error!u32 {
        // OPTIMIZATION: We may already have a positive i32 :^)
        if (self == .number and self.number == .i32 and self.number.i32 >= 0)
            return @intCast(self.number.i32);

        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0) return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int32bit be int modulo 2**32.
        const int32bit = @mod(int, pow_2_32);

        // 5. Return 𝔽(int32bit).
        return @intFromFloat(int32bit);
    }

    /// 7.1.8 ToInt16 ( argument )
    /// https://tc39.es/ecma262/#sec-toint16
    pub fn toInt16(self: Self, agent: *Agent) Agent.Error!i16 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0) return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int16bit be int modulo 2**16.
        const int16bit: u16 = @intFromFloat(@mod(int, pow_2_16));

        // 5. If int16bit ≥ 2**15, return 𝔽(int16bit - 2**16); otherwise return 𝔽(int16bit).
        return @bitCast(int16bit);
    }

    /// 7.1.9 ToUint16 ( argument )
    /// https://tc39.es/ecma262/#sec-touint16
    pub fn toUint16(self: Self, agent: *Agent) Agent.Error!u16 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0) return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int16bit be int modulo 2**16.
        const int16bit: u16 = @intFromFloat(@mod(int, pow_2_16));

        // 5. Return 𝔽(int16bit).
        return int16bit;
    }

    /// 7.1.10 ToInt8 ( argument )
    /// https://tc39.es/ecma262/#sec-toint8
    pub fn toInt8(self: Self, agent: *Agent) Agent.Error!i8 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0) return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int8bit be int modulo 2**8.
        const int8bit: u8 = @intFromFloat(@mod(int, pow_2_8));

        // 5. If int8bit ≥ 2**7, return 𝔽(int8bit - 2**8); otherwise return 𝔽(int8bit).
        return @bitCast(int8bit);
    }

    /// 7.1.11 ToUint8 ( argument )
    /// https://tc39.es/ecma262/#sec-touint8
    pub fn toUint8(self: Self, agent: *Agent) Agent.Error!u8 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is not finite or number is either +0𝔽 or -0𝔽, return +0𝔽.
        if (!number.isFinite() or number.asFloat() == 0) return 0;

        // 3. Let int be truncate(ℝ(number)).
        const int = number.truncate().asFloat();

        // 4. Let int8bit be int modulo 2**8.
        const int8bit: u8 = @intFromFloat(@mod(int, pow_2_8));

        // 5. Return 𝔽(int8bit).
        return int8bit;
    }

    /// 7.1.12 ToUint8Clamp ( argument )
    /// https://tc39.es/ecma262/#sec-touint8clamp
    pub fn toUint8Clamp(self: Self, agent: *Agent) Agent.Error!u8 {
        // 1. Let number be ? ToNumber(argument).
        const number = try self.toNumber(agent);

        // 2. If number is NaN, return +0𝔽.
        if (number.isNan()) return 0;

        // 3. Let mv be the extended mathematical value of number.
        // 4. Let clamped be the result of clamping mv between 0 and 255.
        const clamped = std.math.clamp(number.asFloat(), 0, 255);

        // 5. Let f be floor(clamped).
        const f = @floor(clamped);
        const f_int: u8 = @intFromFloat(f);

        // 6. If clamped < f + 0.5, return 𝔽(f).
        if (clamped < f + 0.5) return f_int;

        // 7. If clamped > f + 0.5, return 𝔽(f + 1).
        if (clamped > f + 0.5) return f_int + 1;

        // 8. If f is even, return 𝔽(f). Otherwise, return 𝔽(f + 1).
        if (f_int % 2 == 0) return f_int else return f_int + 1;
    }

    /// 7.1.13 ToBigInt ( argument )
    /// https://tc39.es/ecma262/#sec-tobigint
    pub fn toBigInt(self: Self, agent: *Agent) Agent.Error!BigInt {
        // 1. Let prim be ? ToPrimitive(argument, number).
        const primitive = try self.toPrimitive(agent, .number);

        // 2. Return the value that prim corresponds to in Table 12.
        return switch (primitive) {
            // Throw a TypeError exception.
            .undefined => agent.throwException(.type_error, "Cannot convert undefined to BigInt", .{}),
            .null => agent.throwException(.type_error, "Cannot convert null to BigInt", .{}),
            .number => agent.throwException(.type_error, "Cannot convert number to BigInt", .{}),
            .symbol => agent.throwException(.type_error, "Cannot convert symbol to BigInt", .{}),

            // Return 1n if prim is true and 0n if prim is false.
            .boolean => |boolean| if (boolean)
                agent.pre_allocated.one
            else
                agent.pre_allocated.zero,

            // Return prim.
            .big_int => |big_int| big_int,

            .string => |string| {
                // 1. Let n be StringToBigInt(prim).
                const n = try stringToBigInt(agent.gc_allocator, string);

                // 2. If n is undefined, throw a SyntaxError exception.
                // 3. Return n.
                return n orelse agent.throwException(
                    .syntax_error,
                    "Cannot convert string to BigInt",
                    .{},
                );
            },

            .object => unreachable,
        };
    }

    /// 7.1.15 ToBigInt64 ( argument )
    /// https://tc39.es/ecma262/#sec-tobigint64
    pub fn toBigInt64(self: Self, agent: *Agent) Agent.Error!i64 {
        // 1. Let n be ? ToBigInt(argument).
        const n = try self.toBigInt(agent);

        // 2. Let int64bit be ℝ(n) modulo 2**64.
        // 3. If int64bit ≥ 2**63, return ℤ(int64bit - 2**64); otherwise return ℤ(int64bit).
        var n_managed = try n.value.toManaged(agent.gc_allocator);
        defer n_managed.deinit();
        var int64bit = try std.math.big.int.Managed.init(agent.gc_allocator);
        try int64bit.truncate(&n_managed, .signed, 64);
        return int64bit.to(i64) catch unreachable;
    }

    /// 7.1.16 ToBigUint64 ( argument )
    /// https://tc39.es/ecma262/#sec-tobiguint64
    pub fn toBigUint64(self: Self, agent: *Agent) Agent.Error!u64 {
        // 1. Let n be ? ToBigInt(argument).
        const n = try self.toBigInt(agent);

        // 2. Let int64bit be ℝ(n) modulo 2**64.
        // 3. Return ℤ(int64bit).
        var n_managed = try n.value.toManaged(agent.gc_allocator);
        defer n_managed.deinit();
        var int64bit = try std.math.big.int.Managed.init(agent.gc_allocator);
        try int64bit.truncate(&n_managed, .unsigned, 64);
        return int64bit.to(u64) catch unreachable;
    }

    /// 7.1.17 ToString ( argument )
    /// https://tc39.es/ecma262/#sec-tostring
    pub fn toString(self: Self, agent: *Agent) Agent.Error!String {
        return switch (self) {
            // 1. If argument is a String, return argument.
            .string => |string| string,

            // 2. If argument is a Symbol, throw a TypeError exception.
            .symbol => return agent.throwException(
                .type_error,
                "Cannot convert Symbol to string",
                .{},
            ),

            // 3. If argument is undefined, return "undefined".
            .undefined => String.fromLiteral("undefined"),

            // 4. If argument is null, return "null".
            .null => String.fromLiteral("null"),

            // 5. If argument is true, return "true".
            // 6. If argument is false, return "false".
            .boolean => |boolean| if (boolean)
                String.fromLiteral("true")
            else
                String.fromLiteral("false"),

            // 7. If argument is a Number, return Number::toString(argument, 10).
            .number => |number| number.toString(agent.gc_allocator, 10),

            // 8. If argument is a BigInt, return BigInt::toString(argument, 10).
            .big_int => |big_int| big_int.toString(agent.gc_allocator, 10),

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

    /// 7.1.18 ToObject ( argument )
    /// https://tc39.es/ecma262/#sec-toobject
    pub fn toObject(self: Self, agent: *Agent) Agent.Error!Object {
        const realm = agent.currentRealm();
        return switch (self) {
            .undefined => agent.throwException(.type_error, "Cannot convert undefined to Object", .{}),
            .null => agent.throwException(.type_error, "Cannot convert null to Object", .{}),
            .boolean => |boolean| try builtins.Boolean.create(agent, .{
                .fields = .{ .boolean_data = boolean },
                .prototype = try realm.intrinsics.@"%Boolean.prototype%"(),
            }),
            .number => |number| try builtins.Number.create(agent, .{
                .fields = .{ .number_data = number },
                .prototype = try realm.intrinsics.@"%Number.prototype%"(),
            }),
            .string => |string| try stringCreate(
                agent,
                string,
                try realm.intrinsics.@"%String.prototype%"(),
            ),
            .symbol => |symbol| try builtins.Symbol.create(agent, .{
                .fields = .{ .symbol_data = symbol },
                .prototype = try realm.intrinsics.@"%Symbol.prototype%"(),
            }),
            .big_int => |big_int| try builtins.BigInt.create(agent, .{
                .fields = .{ .big_int_data = big_int },
                .prototype = try realm.intrinsics.@"%BigInt.prototype%"(),
            }),
            .object => |object| object,
        };
    }

    /// 7.1.19 ToPropertyKey ( argument )
    /// https://tc39.es/ecma262/#sec-topropertykey
    pub fn toPropertyKey(self: Self, agent: *Agent) Agent.Error!PropertyKey {
        // 1. Let key be ? ToPrimitive(argument, string).
        const key = try self.toPrimitive(agent, .string);

        // 2. If key is a Symbol, then
        if (key == .symbol) {
            // a. Return key.
            return PropertyKey.from(key.symbol);
        }

        // OPTIMIZATION: If we have a number that fits into an `PropertyKey.IntegerIndex` there's
        //               no need to do a string conversion and back.
        if (key == .number and key.number.isIntegral()) {
            switch (key.number) {
                .i32 => |value| if (value >= 0) {
                    return PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(value)));
                },
                .f64 => |value| if (value >= 0 and value <= std.math.maxInt(PropertyKey.IntegerIndex)) {
                    return PropertyKey.from(@as(PropertyKey.IntegerIndex, @intFromFloat(value)));
                },
            }
        }

        // 3. Return ! ToString(key).
        const string = key.toString(agent) catch |err| try noexcept(err);
        return PropertyKey.from(string);
    }

    /// 7.1.20 ToLength ( argument )
    /// https://tc39.es/ecma262/#sec-tolength
    pub fn toLength(self: Self, agent: *Agent) Agent.Error!u53 {
        // 1. Let len be ? ToIntegerOrInfinity(argument).
        const length = try self.toIntegerOrInfinity(agent);

        // 2. If len ≤ 0, return +0𝔽.
        if (length <= 0) return 0;

        // 3. Return 𝔽(min(len, 2**53 - 1)).
        return @intFromFloat(@min(length, std.math.maxInt(u53)));
    }

    /// 7.1.22 ToIndex ( value )
    /// https://tc39.es/ecma262/#sec-toindex
    pub fn toIndex(self: Self, agent: *Agent) Agent.Error!u53 {
        // 1. Let integer be ? ToIntegerOrInfinity(value).
        const integer = try self.toIntegerOrInfinity(agent);

        // 2. If integer is not in the inclusive interval from 0 to 2**53 - 1, throw a RangeError exception.
        if (integer < 0 or integer > std.math.maxInt(u53))
            return agent.throwException(.range_error, "Value is not not a valid index", .{});

        // 3. Return integer.
        return @intFromFloat(integer);
    }

    /// 7.2.1 RequireObjectCoercible ( argument )
    /// https://tc39.es/ecma262/#sec-requireobjectcoercible
    pub fn requireObjectCoercible(self: Self, agent: *Agent) error{ExceptionThrown}!Self {
        switch (self) {
            .undefined => return agent.throwException(.type_error, "Cannot convert undefined to Object", .{}),
            .null => return agent.throwException(.type_error, "Cannot convert null to Object", .{}),
            else => return self,
        }
    }

    /// 7.2.2 IsArray ( argument )
    /// https://tc39.es/ecma262/#sec-isarray
    pub fn isArray(self: Self) error{ExceptionThrown}!bool {
        // 1. If argument is not an Object, return false.
        if (self != .object) return false;

        // 2. If argument is an Array exotic object, return true.
        if (self.object.is(builtins.Array)) return true;

        // 3. If argument is a Proxy exotic object, then
        if (self.object.is(builtins.Proxy)) {
            // a. Perform ? ValidateNonRevokedProxy(argument).
            try validateNonRevokedProxy(self.object.as(builtins.Proxy));

            // b. Let proxyTarget be argument.[[ProxyTarget]].
            const proxy_target = self.object.as(builtins.Proxy).fields.proxy_target;

            // c. Return ? IsArray(proxyTarget).
            return Value.from(proxy_target.?).isArray();
        }

        // 4. Return false.
        return false;
    }

    /// 7.2.3 IsCallable ( argument )
    /// https://tc39.es/ecma262/#sec-iscallable
    pub fn isCallable(self: Self) bool {
        // 1. If argument is not an Object, return false.
        if (self != .object) return false;

        // 2. If argument has a [[Call]] internal method, return true.
        if (self.object.internalMethods().call != null) return true;

        // 3. Return false.
        return false;
    }

    /// 7.2.4 IsConstructor ( argument )
    /// https://tc39.es/ecma262/#sec-isconstructor
    pub fn isConstructor(self: Self) bool {
        // 1. If argument is not an Object, return false.
        if (self != .object) return false;

        // 2. If argument has a [[Construct]] internal method, return true.
        if (self.object.internalMethods().construct != null) return true;

        // 3. Return false.
        return false;
    }

    /// 7.2.6 IsRegExp ( argument )
    /// https://tc39.es/ecma262/#sec-isregexp
    pub fn isRegExp(self: Self) Agent.Error!bool {
        // 1. If argument is not an Object, return false.
        if (self != .object) return false;

        // 2. Let matcher be ? Get(argument, %Symbol.match%).
        const matcher = try self.object.get(
            PropertyKey.from(self.object.agent().well_known_symbols.@"%Symbol.match%"),
        );

        // 3. If matcher is not undefined, return ToBoolean(matcher).
        if (matcher != .undefined) return matcher.toBoolean();

        // 4. If argument has a [[RegExpMatcher]] internal slot, return true.
        if (self.object.is(builtins.RegExp)) return true;

        // 5. Return false.
        return false;
    }

    /// 7.3.3 GetV ( V, P )
    /// https://tc39.es/ecma262/#sec-getv
    pub fn get(self: Self, agent: *Agent, property_key: PropertyKey) Agent.Error!Value {
        // 1. Let O be ? ToObject(V).
        const object = try self.toObject(agent);

        // 2. Return ? O.[[Get]](P, V).
        return object.internalMethods().get(object, property_key, self);
    }

    /// 7.3.10 GetMethod ( V, P )
    /// https://tc39.es/ecma262/#sec-getmethod
    pub fn getMethod(self: Self, agent: *Agent, property_key: PropertyKey) Agent.Error!?Object {
        // 1. Let func be ? GetV(V, P).
        const function = try self.get(agent, property_key);

        // 2. If func is either undefined or null, return undefined.
        if (function == .undefined or function == .null) return null;

        // 3. If IsCallable(func) is false, throw a TypeError exception.
        if (!function.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{self});
        }

        // 4. Return func.
        return function.object;
    }

    /// 7.3.13 Call ( F, V [ , argumentsList ] )
    /// https://tc39.es/ecma262/#sec-call
    pub fn call(
        self: Self,
        agent: *Agent,
        this_value: Value,
        arguments_list: []const Value,
    ) Agent.Error!Value {
        // 1. If argumentsList is not present, set argumentsList to a new empty List.
        // NOTE: This is done via the NoArgs variant of the function.

        // 2. If IsCallable(F) is false, throw a TypeError exception.
        if (!self.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{self});
        }

        // 3. Return ? F.[[Call]](V, argumentsList).
        return self.object.internalMethods().call.?(
            self.object,
            this_value,
            Arguments.from(arguments_list),
        );
    }

    pub inline fn callNoArgs(self: Self, agent: *Agent, this_value: Value) Agent.Error!Value {
        return self.call(agent, this_value, &.{});
    }

    pub inline fn callAssumeCallable(self: Self, this_value: Value, arguments_list: []const Value) Agent.Error!Value {
        return self.object.internalMethods().call.?(
            self.object,
            this_value,
            Arguments.from(arguments_list),
        );
    }

    pub inline fn callAssumeCallableNoArgs(self: Self, this_value: Value) Agent.Error!Value {
        return self.callAssumeCallable(this_value, &.{});
    }

    /// 7.3.19 CreateListFromArrayLike ( obj [ , elementTypes ] )
    /// https://tc39.es/ecma262/#sec-createlistfromarraylike
    pub fn createListFromArrayLike(self: Self, agent: *Agent, args: struct {
        element_types: ?[]const std.meta.Tag(Value) = null,
    }) Agent.Error![]Value {
        // 1. If elementTypes is not present, set elementTypes to « Undefined, Null, Boolean,
        //    String, Symbol, Number, BigInt, Object ».
        const element_types = args.element_types orelse std.enums.values(std.meta.Tag(Value));

        // 2. If obj is not an Object, throw a TypeError exception.
        if (self != .object) {
            return agent.throwException(.type_error, "{} is not an Object", .{self});
        }

        // 3. Let len be ? LengthOfArrayLike(obj).
        const len = try self.object.lengthOfArrayLike();

        // 4. Let list be a new empty List.
        if (len > std.math.maxInt(usize)) return error.OutOfMemory;
        var list = try std.ArrayList(Value).initCapacity(agent.gc_allocator, @intCast(len));
        defer list.deinit();

        // 5. Let index be 0.
        var index: u53 = 0;

        // 6. Repeat, while index < len,
        while (index < len) : (index += 1) {
            // a. Let indexName be ! ToString(𝔽(index)).
            const index_name = PropertyKey.from(index);

            // b. Let next be ? Get(obj, indexName).
            const next = try self.get(agent, index_name);

            // c. If elementTypes does not contain Type(next), throw a TypeError exception.
            if (std.mem.indexOfScalar(std.meta.Tag(Value), element_types, next) == null) {
                return agent.throwException(
                    .type_error,
                    "Array element {} has invalid type '{s}'",
                    .{ next, @tagName(next) },
                );
            }

            // d. Append next to list.
            list.appendAssumeCapacity(next);

            // e. Set index to index + 1.
        }

        // 7. Return list.
        return list.toOwnedSlice();
    }

    /// 7.3.20 Invoke ( V, P [ , argumentsList ] )
    /// https://tc39.es/ecma262/#sec-invoke
    pub fn invoke(
        self: Self,
        agent: *Agent,
        property_key: PropertyKey,
        arguments_list: []const Value,
    ) Agent.Error!Value {
        // 1. If argumentsList is not present, set argumentsList to a new empty List.
        // NOTE: This is done via the NoArgs variant of the function.

        // 2. Let func be ? GetV(V, P).
        const function = try self.get(agent, property_key);

        // 3. Return ? Call(func, V, argumentsList).
        return function.call(agent, self, arguments_list);
    }

    pub inline fn invokeNoArgs(
        self: Self,
        agent: *Agent,
        property_key: PropertyKey,
    ) Agent.Error!Value {
        return self.invoke(agent, property_key, &.{});
    }

    /// 7.3.21 OrdinaryHasInstance ( C, O )
    /// https://tc39.es/ecma262/#sec-ordinaryhasinstance
    pub fn ordinaryHasInstance(self: Self, object_value: Value) Agent.Error!bool {
        // 1. If IsCallable(C) is false, return false.
        if (!self.isCallable()) return false;

        const agent = self.object.agent();

        // 2. If C has a [[BoundTargetFunction]] internal slot, then
        if (self.object.is(builtins.BoundFunction)) {
            // a. Let BC be C.[[BoundTargetFunction]].
            const bound_constructor = self.object.as(builtins.BoundFunction).fields.bound_target_function;

            // b. Return ? InstanceofOperator(O, BC).
            return object_value.instanceofOperator(agent, Value.from(bound_constructor));
        }

        // 3. If O is not an Object, return false.
        if (object_value != .object) return false;

        // 4. Let P be ? Get(C, "prototype").
        const prototype = try self.object.get(PropertyKey.from("prototype"));

        // 5. If P is not an Object, throw a TypeError exception.
        if (prototype != .object) {
            return agent.throwException(.type_error, "'prototype' property must be an object", .{});
        }

        var object: ?Object = object_value.object;

        // 6. Repeat,
        while (true) {
            // a. Set O to ? O.[[GetPrototypeOf]]().
            object = try object.?.internalMethods().getPrototypeOf(object.?);

            // b. If O is null, return false.
            if (object == null) return false;

            // c. If SameValue(P, O) is true, return true.
            if (prototype.object.sameValue(object.?)) return true;
        }
    }

    /// 7.3.35 AddValueToKeyedGroup ( groups, key, value )
    /// https://tc39.es/ecma262/#sec-add-value-to-keyed-group
    fn addValueToKeyedGroup(
        agent: *Agent,
        groups: anytype,
        key: anytype,
        value: Value,
    ) Allocator.Error!void {
        // 1. For each Record { [[Key]], [[Elements]] } g of groups, do
        //     a. If SameValue(g.[[Key]], key) is true, then
        if (groups.getPtr(key)) |group| {
            // i. Assert: Exactly one element of groups meets this criterion.
            // ii. Append value to g.[[Elements]].
            try group.append(value);

            // iii. Return unused.
        } else {
            // 2. Let group be the Record { [[Key]]: key, [[Elements]]: « value » }.
            // 3. Append group to groups.
            var group = std.ArrayList(Value).init(agent.gc_allocator);
            try group.append(value);
            try groups.putNoClobber(key, group);

            // 4. Return unused.
        }
    }

    const KeyCoercion = enum { property, collection };

    fn GroupByContainer(comptime key_coercion: KeyCoercion) type {
        return switch (key_coercion) {
            .property => PropertyKeyArrayHashMap(std.ArrayList(Value)),
            .collection => ValueArrayHashMap(std.ArrayList(Value), sameValue),
        };
    }

    /// 7.3.36 GroupBy ( items, callbackfn, keyCoercion )
    /// https://tc39.es/ecma262/#sec-groupby
    pub fn groupBy(
        self: Self,
        agent: *Agent,
        callback_fn: Value,
        comptime key_coercion: KeyCoercion,
    ) Agent.Error!GroupByContainer(key_coercion) {
        // 1. Perform ? RequireObjectCoercible(items).
        _ = try self.requireObjectCoercible(agent);

        // 2. If IsCallable(callbackfn) is false, throw a TypeError exception.
        if (!callback_fn.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback_fn});
        }

        // 3. Let groups be a new empty List.
        var groups = GroupByContainer(key_coercion).init(agent.gc_allocator);

        // 4. Let iteratorRecord be ? GetIterator(items, sync).
        var iterator = try getIterator(agent, self, .sync);

        // 5. Let k be 0.
        var k: u53 = 0;

        // 6. Repeat,
        while (true) : (k += 1) {
            // a. If k ≥ 2**53 - 1, then
            if (k == std.math.maxInt(u53)) {
                // i. Let error be ThrowCompletion(a newly created TypeError object).
                const @"error" = agent.throwException(
                    .type_error,
                    "Cannot group more than 2^53-1 items",
                    .{},
                );

                // ii. Return ? IteratorClose(iteratorRecord, error).
                return iterator.close(@as(Agent.Error!GroupByContainer(key_coercion), @"error"));
            }

            // b. Let next be ? IteratorStepValue(iteratorRecord).
            const next = try iterator.stepValue();

            // c. If next is done, then
            //     i. Return groups.
            // d. Let value be next.
            const value = next orelse return groups;

            // e. Let key be Completion(Call(callbackfn, undefined, « value, 𝔽(k) »)).
            const key = callback_fn.callAssumeCallable(
                .undefined,
                &.{ value, Value.from(k) },
            ) catch |err| {
                // f. IfAbruptCloseIterator(key, iteratorRecord).
                return iterator.close(@as(Agent.Error!GroupByContainer(key_coercion), err));
            };

            // g. If keyCoercion is property, then
            const coerced_key = if (key_coercion == .property) blk: {
                // i. Set key to Completion(ToPropertyKey(key)).
                break :blk key.toPropertyKey(agent) catch |err| {
                    // ii. IfAbruptCloseIterator(key, iteratorRecord).
                    return iterator.close(@as(Agent.Error!GroupByContainer(key_coercion), err));
                };
            }
            // h. Else,
            else blk: {
                // i. Assert: keyCoercion is collection.
                std.debug.assert(key_coercion == .collection);

                // ii. Set key to CanonicalizeKeyedCollectionKey(key).
                break :blk key.canonicalizeKeyedCollectionKey();
            };

            // i. Perform AddValueToKeyedGroup(groups, key, value).
            try addValueToKeyedGroup(agent, &groups, coerced_key, value);

            // j. Set k to k + 1.
        }
    }

    /// 9.13 CanBeHeldWeakly ( v )
    /// https://tc39.es/ecma262/#sec-canbeheldweakly
    pub fn canBeHeldWeakly(self: Self, agent: *Agent) bool {
        // 1. If v is an Object, return true.
        if (self == .object) return true;

        // 2. If v is a Symbol and KeyForSymbol(v) is undefined, return true.
        if (self == .symbol and keyForSymbol(agent, self.symbol) == null) return true;

        // 3. Return false.
        return false;
    }

    /// 10.1.15 RequireInternalSlot ( O, internalSlot )
    /// https://tc39.es/ecma262/#sec-requireinternalslot
    pub fn requireInternalSlot(
        self: Self,
        agent: *Agent,
        comptime T: type,
    ) error{ExceptionThrown}!*T {
        const name = comptime blk: {
            var name: []const u8 = undefined;

            // "builtins.date.Date__struct_12345" -> "Date__struct_12345"
            var it = std.mem.splitScalar(u8, @typeName(T.Fields), '.');
            while (it.next()) |part| name = part;

            // "Date__struct_12345" -> "Date"
            it = std.mem.splitScalar(u8, name, '_');
            if (it.next()) |part| name = part;

            break :blk name;
        };

        // 1. If O is not an Object, throw a TypeError exception.
        if (self != .object) {
            return agent.throwException(.type_error, "{} is not an Object", .{self});
        }

        // 2. If O does not have an internalSlot internal slot, throw a TypeError exception.
        if (!self.object.is(T)) {
            return agent.throwException(.type_error, "{} is not a {s} object", .{ self, name });
        }

        // 3. Return unused.
        // NOTE: Returning the object here allows for direct assignment of the object at the call site.
        return self.object.as(T);
    }

    /// 13.10.2 InstanceofOperator ( V, target )
    /// https://tc39.es/ecma262/#sec-instanceofoperator
    pub fn instanceofOperator(self: Self, agent: *Agent, target: Self) Agent.Error!bool {
        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                "Right-hand side of 'instanceof' operator must be an object",
                .{},
            );
        }

        // 2. Let instOfHandler be ? GetMethod(target, %Symbol.hasInstance%).
        const maybe_instanceof_handler = try target.getMethod(
            agent,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.hasInstance%"),
        );

        // 3. If instOfHandler is not undefined, then
        if (maybe_instanceof_handler) |instanceof_handler| {
            // a. Return ToBoolean(? Call(instOfHandler, target, « V »)).
            return (try from(instanceof_handler).call(agent, target, &.{self})).toBoolean();
        }

        // 4. If IsCallable(target) is false, throw a TypeError exception.
        if (!target.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{target});
        }

        // 5. Return ? OrdinaryHasInstance(target, V).
        return target.ordinaryHasInstance(self);
    }

    /// 24.5.1 CanonicalizeKeyedCollectionKey ( key )
    /// https://tc39.es/ecma262/#sec-canonicalizekeyedcollectionkey
    pub fn canonicalizeKeyedCollectionKey(self: Self) Self {
        // 1. If key is -0𝔽, return +0𝔽.
        if (self == .number and self.number.isNegativeZero()) return Value.from(0);

        // 2. Return key.
        return self;
    }

    /// 27.2.1.6 IsPromise ( x )
    /// https://tc39.es/ecma262/#sec-ispromise
    pub fn isPromise(self: Self) bool {
        // 1. If x is not an Object, return false.
        if (self != .object) return false;

        // 2. If x does not have a [[PromiseState]] internal slot, return false.
        if (!self.object.is(builtins.Promise)) return false;

        // 3. Return true.
        return true;
    }

    /// Non-standard helper to get the right prototype for a primitive value, if applicable.
    pub fn synthesizePrototype(self: Self, agent: *Agent) Allocator.Error!?Object {
        const realm = agent.currentRealm();

        return switch (self) {
            .null, .undefined => null,
            .boolean => try realm.intrinsics.@"%Boolean.prototype%"(),
            .string => try realm.intrinsics.@"%String.prototype%"(),
            .symbol => try realm.intrinsics.@"%Symbol.prototype%"(),
            .number => try realm.intrinsics.@"%Number.prototype%"(),
            .big_int => try realm.intrinsics.@"%BigInt.prototype%"(),
            .object => null,
        };
    }

    /// Non-standard helper to turn a symbol value into a private name.
    pub fn toPrivateName(self: Self) ?PrivateName {
        if (self != .symbol or !self.symbol.isPrivate()) return null;
        return .{ .symbol = self.symbol };
    }
};

/// 7.1.4.1.1 StringToNumber ( str )
/// https://tc39.es/ecma262/#sec-stringtonumber
pub fn stringToNumber(allocator: Allocator, string: String) Allocator.Error!Number {
    // 1. Let literal be ParseText(str, StringNumericLiteral).
    // 2. If literal is a List of errors, return NaN.
    // 3. Return the StringNumericValue of literal.
    // TODO: Implement the proper string parsing grammar!
    const trimmed_string = try (try string.trim(allocator, .@"start+end")).toUtf8(allocator);
    if (trimmed_string.len == 0) return Number.from(0);
    if (std.mem.eql(u8, trimmed_string, "-Infinity")) return Number.from(-std.math.inf(f64));
    if (std.mem.eql(u8, trimmed_string, "+Infinity")) return Number.from(std.math.inf(f64));
    if (std.mem.eql(u8, trimmed_string, "Infinity")) return Number.from(std.math.inf(f64));
    // Don't pass other strings starting with "inf" to `std.fmt.parseFloat()`
    if (std.ascii.startsWithIgnoreCase(trimmed_string, "inf")) return Number.from(std.math.nan(f64));
    return Number.from(std.fmt.parseFloat(f64, trimmed_string) catch std.math.nan(f64));
}

/// 7.1.14 StringToBigInt ( str )
/// https://tc39.es/ecma262/#sec-stringtobigint
pub fn stringToBigInt(allocator: Allocator, string: String) Allocator.Error!?BigInt {
    // 1. Let literal be ParseText(str, StringIntegerLiteral).
    // 2. If literal is a List of errors, return undefined.
    // 3. Let mv be the MV of literal.
    // 4. Assert: mv is an integer.
    // 5. Return ℤ(mv).
    // TODO: Implement the proper string parsing grammar!
    var trimmed_string = try (try string.trim(allocator, .@"start+end")).toUtf8(allocator);
    if (trimmed_string.len == 0) return try types.BigInt.from(allocator, 0);
    // Unlike std.fmt.parseFloat(), std.math.big.int.Managed.setString() doesn't like the prefix
    // so we have to cut it off manually.
    const base: u8 = if (std.ascii.startsWithIgnoreCase(trimmed_string, "0b")) blk: {
        trimmed_string = trimmed_string[2..];
        if (trimmed_string.len == 0) return null;
        break :blk 2;
    } else if (std.ascii.startsWithIgnoreCase(trimmed_string, "0o")) blk: {
        trimmed_string = trimmed_string[2..];
        if (trimmed_string.len == 0) return null;
        break :blk 8;
    } else if (std.ascii.startsWithIgnoreCase(trimmed_string, "0x")) blk: {
        trimmed_string = trimmed_string[2..];
        if (trimmed_string.len == 0) return null;
        break :blk 16;
    } else blk: {
        break :blk 10;
    };
    var big_int = try std.math.big.int.Managed.init(allocator);
    big_int.setString(
        base,
        trimmed_string,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidCharacter => return null,
        error.InvalidBase => unreachable,
    };
    return types.BigInt.fromConst(big_int.toConst());
}

/// 7.2.10 SameValue ( x, y )
/// https://tc39.es/ecma262/#sec-samevalue
pub fn sameValue(x: Value, y: Value) bool {
    // 1. If Type(x) is not Type(y), return false.
    if (std.meta.activeTag(x) != std.meta.activeTag(y)) return false;

    // 2. If x is a Number, then
    if (x == .number) {
        // a. Return Number::sameValue(x, y).
        return x.number.sameValue(y.number);
    }

    // 3. Return SameValueNonNumber(x, y).
    return sameValueNonNumber(x, y);
}

/// 7.2.11 SameValueZero ( x, y )
/// https://tc39.es/ecma262/#sec-samevaluezero
pub fn sameValueZero(x: Value, y: Value) bool {
    // 1. If Type(x) is not Type(y), return false.
    if (std.meta.activeTag(x) != std.meta.activeTag(y)) return false;

    // 2. If x is a Number, then
    if (x == .number) {
        // a. Return Number::sameValueZero(x, y).
        return x.number.sameValueZero(y.number);
    }

    // 3. Return SameValueNonNumber(x, y).
    return sameValueNonNumber(x, y);
}

/// 7.2.12 SameValueNonNumber ( x, y )
/// https://tc39.es/ecma262/#sec-samevaluenonnumber
pub fn sameValueNonNumber(x: Value, y: Value) bool {
    // 1. Assert: Type(x) is Type(y).
    std.debug.assert(std.meta.activeTag(x) == std.meta.activeTag(y));

    return switch (x) {
        // 2. If x is either null or undefined, return true.
        .null, .undefined => true,

        // 3. If x is a BigInt, then
        //     a. Return BigInt::equal(x, y).
        .big_int => x.big_int.equal(y.big_int),

        // 4. If x is a String, then
        //     a. If x and y have the same length and the same code units in the same positions,
        //        return true; otherwise, return false.
        .string => x.string.eql(y.string),

        // 5. If x is a Boolean, then
        //     a. If x and y are both true or both false, return true; otherwise, return false.
        .boolean => x.boolean == y.boolean,

        // 6. NOTE: All other ECMAScript language values are compared by identity.
        // 7. If x is y, return true; otherwise, return false.
        .symbol => x.symbol.sameValue(y.symbol),
        .object => x.object.sameValue(y.object),

        .number => unreachable,
    };
}

/// 7.2.13 IsLessThan ( x, y, LeftFirst )
/// https://tc39.es/ecma262/#sec-islessthan
pub fn isLessThan(
    agent: *Agent,
    x: Value,
    y: Value,
    order: enum { left_first, right_first },
) Agent.Error!?bool {
    var px: Value = undefined;
    var py: Value = undefined;

    // 1. If LeftFirst is true, then
    if (order == .left_first) {
        // a. Let px be ? ToPrimitive(x, number).
        px = try x.toPrimitive(agent, .number);

        // b. Let py be ? ToPrimitive(y, number).
        py = try y.toPrimitive(agent, .number);
    }
    // 2. Else,
    else {
        // a. NOTE: The order of evaluation needs to be reversed to preserve left to right
        //          evaluation.

        // b. Let py be ? ToPrimitive(y, number).
        py = try y.toPrimitive(agent, .number);

        // c. Let px be ? ToPrimitive(x, number).
        px = try x.toPrimitive(agent, .number);
    }

    // 3. If px is a String and py is a String, then
    if (px == .string and py == .string) {
        // a. Let lx be the length of px.
        const lx = px.string.length();

        // b. Let ly be the length of py.
        const ly = py.string.length();

        // c. For each integer i such that 0 ≤ i < min(lx, ly), in ascending order, do
        for (0..@min(lx, ly)) |i| {
            // i. Let cx be the numeric value of the code unit at index i within px.
            const cx = px.string.codeUnitAt(i);

            // ii. Let cy be the numeric value of the code unit at index i within py.
            const cy = py.string.codeUnitAt(i);

            // iii. If cx < cy, return true.
            if (cx < cy) return true;

            // iv. If cx > cy, return false.
            if (cx > cy) return false;
        }

        // d. If lx < ly, return true. Otherwise, return false.
        return lx < ly;
    }
    // 4. Else,
    else {
        // a. If px is a BigInt and py is a String, then
        if (px == .big_int and py == .string) {
            //     i. Let ny be StringToBigInt(py).
            //     ii. If ny is undefined, return undefined.
            //     iii. Return BigInt::lessThan(px, ny).
        }

        // b. If px is a String and py is a BigInt, then
        if (px == .string and py == .big_int) {
            //     i. Let nx be StringToBigInt(px).
            //     ii. If nx is undefined, return undefined.
            //     iii. Return BigInt::lessThan(nx, py).
        }

        // c. NOTE: Because px and py are primitive values, evaluation order is not important.

        // d. Let nx be ? ToNumeric(px).
        const nx = try px.toNumeric(agent);

        // e. Let ny be ? ToNumeric(py).
        const ny = try py.toNumeric(agent);

        // f. If Type(nx) is Type(ny), then
        if (std.meta.activeTag(nx) == std.meta.activeTag(ny)) {
            // i. If nx is a Number, then
            if (nx == .number) {
                // 1. Return Number::lessThan(nx, ny).
                return nx.number.lessThan(ny.number);
            }
            // ii. Else,
            else {
                // 1. Assert: nx is a BigInt.
                std.debug.assert(nx == .big_int);

                // 2. Return BigInt::lessThan(nx, ny).
                return nx.big_int.lessThan(ny.big_int);
            }
        }

        // g. Assert: nx is a BigInt and ny is a Number, or nx is a Number and ny is a BigInt.
        std.debug.assert((nx == .big_int and ny == .number) or (nx == .number and ny == .big_int));

        // h. If nx or ny is NaN, return undefined.
        if ((nx == .number and nx.number.isNan()) or
            (ny == .number and ny.number.isNan())) return null;

        // i. If nx is -∞𝔽 or ny is +∞𝔽, return true.
        if ((nx == .number and nx.number.isNegativeInf()) or
            (ny == .number and ny.number.isPositiveInf())) return true;

        // j. If nx is +∞𝔽 or ny is -∞𝔽, return false.
        if ((nx == .number and nx.number.isPositiveInf()) or
            (ny == .number and ny.number.isNegativeInf())) return false;

        // k. If ℝ(nx) < ℝ(ny), return true; otherwise return false.
        return switch (nx) {
            .number => nx.number.asFloat() < try ny.big_int.asFloat(agent),
            .big_int => try nx.big_int.asFloat(agent) < ny.number.asFloat(),
        };
    }
}

/// 7.2.14 IsLooselyEqual ( x, y )
/// https://tc39.es/ecma262/#sec-islooselyequal
pub fn isLooselyEqual(agent: *Agent, x: Value, y: Value) Agent.Error!bool {
    // 1. If Type(x) is Type(y), then
    if (std.meta.activeTag(x) == std.meta.activeTag(y)) {
        // a. Return IsStrictlyEqual(x, y).
        return isStrictlyEqual(x, y);
    }

    // 2. If x is null and y is undefined, return true.
    if (x == .null and y == .undefined) return true;

    // 3. If x is undefined and y is null, return true.
    if (x == .undefined and y == .null) return true;

    // B.3.6.2 Changes to IsLooselyEqual
    // https://tc39.es/ecma262/#sec-IsHTMLDDA-internal-slot-aec
    if (build_options.enable_annex_b) {
        // 4. Perform the following steps:
        // a. If x is an Object, x has an [[IsHTMLDDA]] internal slot, and y is either undefined or
        //    null, return true.
        if (x == .object and x.object.isHTMLDDA() and (y == .undefined or y == .null)) return true;

        // b. If x is either undefined or null, y is an Object, and y has an [[IsHTMLDDA]] internal
        //    slot, return true.
        if ((x == .undefined or x == .null) and y == .object and y.object.isHTMLDDA()) return true;
    } else {
        // 4. NOTE: This step is replaced in section B.3.6.2.
    }

    // 5. If x is a Number and y is a String, return ! IsLooselyEqual(x, ! ToNumber(y)).
    if (x == .number and y == .string) {
        return isLooselyEqual(
            agent,
            x,
            Value.from(y.toNumber(agent) catch unreachable),
        ) catch unreachable;
    }

    // 6. If x is a String and y is a Number, return ! IsLooselyEqual(! ToNumber(x), y).
    if (x == .string and y == .number) {
        return isLooselyEqual(
            agent,
            Value.from(x.toNumber(agent) catch unreachable),
            y,
        ) catch unreachable;
    }

    // 7. If x is a BigInt and y is a String, then
    if (x == .big_int and y == .string) {
        // a. Let n be StringToBigInt(y).
        const n = try stringToBigInt(agent.gc_allocator, y.string);

        // b. If n is undefined, return false.
        if (n == null) return false;

        // c. Return ! IsLooselyEqual(x, n).
        return isLooselyEqual(agent, x, Value.from(n.?));
    }

    // 8. If x is a String and y is a BigInt, return ! IsLooselyEqual(y, x).
    if (x == .string and y == .big_int) return isLooselyEqual(agent, y, x) catch unreachable;

    // 9. If x is a Boolean, return ! IsLooselyEqual(! ToNumber(x), y).
    if (x == .boolean) {
        return isLooselyEqual(
            agent,
            Value.from(x.toNumber(agent) catch unreachable),
            y,
        ) catch unreachable;
    }

    // 10. If y is a Boolean, return ! IsLooselyEqual(x, ! ToNumber(y)).
    if (y == .boolean) {
        return isLooselyEqual(
            agent,
            x,
            Value.from(y.toNumber(agent) catch unreachable),
        ) catch unreachable;
    }

    // 11. If x is either a String, a Number, a BigInt, or a Symbol and y is an Object, return
    //     ! IsLooselyEqual(x, ? ToPrimitive(y)).
    if ((x == .string or x == .number or x == .big_int or x == .symbol) and y == .object) {
        return isLooselyEqual(agent, x, try y.toPrimitive(agent, null)) catch unreachable;
    }

    // 12. If x is an Object and y is either a String, a Number, a BigInt, or a Symbol, return
    //     ! IsLooselyEqual(? ToPrimitive(x), y).
    if (x == .object and (y == .string or y == .number or y == .big_int or y == .symbol)) {
        return isLooselyEqual(agent, try x.toPrimitive(agent, null), y) catch unreachable;
    }

    // 13. If x is a BigInt and y is a Number, or if x is a Number and y is a BigInt, then
    if ((x == .big_int and y == .number) or (x == .number and y == .big_int)) {
        // a. If x is not finite or y is not finite, return false.
        if ((x == .number and !x.number.isFinite()) or
            (y == .number and !y.number.isFinite())) return false;

        // b. If ℝ(x) = ℝ(y), return true; otherwise return false.
        // TODO: Implement more efficient BigInt to f64 comparison
        if ((x == .number and !x.number.isIntegral()) or
            (y == .number and !y.number.isIntegral())) return false;
        return (try x.toString(agent)).eql(try y.toString(agent));
    }

    // 14. Return false.
    return false;
}

/// 7.2.15 IsStrictlyEqual ( x, y )
/// https://tc39.es/ecma262/#sec-isstrictlyequal
pub fn isStrictlyEqual(x: Value, y: Value) bool {
    // 1. If Type(x) is not Type(y), return false.
    if (std.meta.activeTag(x) != std.meta.activeTag(y)) return false;

    // 2. If x is a Number, then
    if (x == .number) {
        // a. Return Number::equal(x, y).
        return x.number.equal(y.number);
    }

    // 3. Return SameValueNonNumber(x, y).
    return sameValueNonNumber(x, y);
}

/// 7.3.17 CreateArrayFromList ( elements )
/// https://tc39.es/ecma262/#sec-createarrayfromlist
pub fn createArrayFromList(agent: *Agent, elements: []const Value) Allocator.Error!Object {
    // 1. Let array be ! ArrayCreate(0).
    const array = arrayCreate(agent, 0, null) catch |err| try noexcept(err);

    // 2. Let n be 0.
    // 3. For each element e of elements, do
    for (elements, 0..) |element, n| {
        const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(n)));

        // a. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(n)), e).
        array.createDataPropertyOrThrow(property_key, element) catch |err| try noexcept(err);

        // b. Set n to n + 1.
    }

    // 4. Return array.
    return array;
}

pub fn createArrayFromListMapToValue(
    agent: *Agent,
    comptime T: type,
    elements: []const T,
    mapFn: fn (*Agent, T) Allocator.Error!Value,
) Allocator.Error!Object {
    // 1. Let array be ! ArrayCreate(0).
    const array = arrayCreate(agent, 0, null) catch |err| try noexcept(err);

    // 2. Let n be 0.
    // 3. For each element e of elements, do
    for (elements, 0..) |element, n| {
        const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(n)));

        // a. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(n)), e).
        array.createDataPropertyOrThrow(
            property_key,
            try mapFn(agent, element),
        ) catch |err| try noexcept(err);

        // b. Set n to n + 1.
    }

    // 4. Return array.
    return array;
}

/// 9.2.12 CoerceOptionsToObject ( options )
/// https://tc39.es/ecma402/#sec-coerceoptionstoobject
pub fn coerceOptionsToObject(agent: *Agent, options: Value) Agent.Error!Object {
    // 1. If options is undefined, then
    if (options == .undefined) {
        // a. Return OrdinaryObjectCreate(null).
        return ordinaryObjectCreate(agent, null);
    }

    // 2. Return ? ToObject(options).
    return options.toObject(agent);
}

/// 9.2.13 GetOption ( options, property, type, values, default )
/// https://tc39.es/ecma402/#sec-getoption
pub fn getOption(
    options: Object,
    comptime property: []const u8,
    comptime @"type": enum {
        const Self = @This();

        boolean,
        number,
        string,

        fn T(comptime self: Self) type {
            return switch (self) {
                .boolean => bool,
                .number => Number,
                .string => String,
            };
        }
    },
    comptime values: ?[]const @"type".T(),
    comptime default: anytype,
) Agent.Error!if (@TypeOf(default) == @TypeOf(null)) ?@"type".T() else @"type".T() {
    if (@TypeOf(default) != @TypeOf(null) and @TypeOf(default) != @"type".T() and default != .required) {
        @compileError("Invalid value for default parameter");
    }

    const agent = options.agent();

    // 1. Let value be ? Get(options, property).
    const value = try options.get(PropertyKey.from(property));

    // 2. If value is undefined, then
    if (value == .undefined) {
        // a. If default is required, throw a RangeError exception.
        if (@TypeOf(default) == @TypeOf(.required)) {
            return agent.throwException(
                .range_error,
                "Required option '{s}' must not be undefined",
                .{property},
            );
        }

        // b. Return default.
        return default;
    }

    const coerced_value = switch (@"type") {
        // 3. If type is boolean, then
        .boolean => blk: {
            // a. Set value to ToBoolean(value).
            break :blk value.toBoolean();
        },

        // 4. Else if type is number, then
        .number => blk: {
            // a. Set value to ? ToNumber(value).
            const number = try value.toNumber(agent);

            // b. If value is NaN, throw a RangeError exception.
            if (number.isNan()) {
                return agent.throwException(
                    .range_error,
                    "Number option '{s}' must not be NaN",
                    .{property},
                );
            }

            break :blk number;
        },

        // 5. Else,
        //     a. Assert: type is string.
        .string => blk: {
            // b. Set value to ? ToString(value).
            break :blk try value.toString(agent);
        },
    };

    // 6. If values is not empty and values does not contain value, throw a RangeError exception.
    if (values != null) {
        for (values.?) |permitted_value| {
            if (sameValue(Value.from(coerced_value), Value.from(permitted_value))) break;
        } else {
            return agent.throwException(
                .range_error,
                "Invalid value for option '{s}'",
                .{property},
            );
        }
    }

    // 7. Return value.
    return coerced_value;
}

pub fn ValueArrayHashMap(comptime V: type, comptime eqlFn: fn (Value, Value) bool) type {
    return std.ArrayHashMap(Value, V, struct {
        const Self = @This();

        pub fn hash(_: Self, key: Value) u32 {
            const value_hash = switch (key) {
                .string => |string| @as(u32, @truncate(string.hash())),
                .number => |number| switch (number) {
                    .i32 => |n| std.array_hash_map.getAutoHashFn(i32, void)({}, n),
                    .f64 => |n| std.array_hash_map.getAutoHashFn(i64, void)({}, @bitCast(n)),
                },
                inline else => |value| blk: {
                    const T = @TypeOf(value);
                    if (T == void) return @intFromEnum(key);
                    break :blk std.array_hash_map.getAutoHashStratFn(T, void, .Shallow)({}, value);
                },
            };
            const tag: u32 = @intFromEnum(key);
            return tag ^ value_hash;
        }

        pub fn eql(_: Self, a: Value, b: Value, _: usize) bool {
            return eqlFn(a, b);
        }
    }, false);
}

test "format" {
    const gc = @import("../../gc.zig");
    var agent = try Agent.init(gc.allocator(), .{});
    defer agent.deinit();
    const object = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    var managed = try std.math.big.int.Managed.initSet(std.testing.allocator, 123);
    defer managed.deinit();
    const test_cases = [_]struct { Value, []const u8 }{
        .{ .undefined, "undefined" },
        .{ .null, "null" },
        .{ Value.from(true), "true" },
        .{ Value.from(false), "false" },
        .{ Value.from("foo"), "\"foo\"" },
        .{ Value.from(Symbol{ .id = 0, .description = null }), "Symbol()" },
        .{ Value.from(Symbol{ .id = 0, .description = String.fromLiteral("foo") }), "Symbol(\"foo\")" },
        .{ Value.from(BigInt.fromConst(managed.toConst())), "123n" },
        .{ Value.from(object), "[object Object]" },
    };
    for (test_cases) |test_case| {
        const value, const expected = test_case;
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{value});
        defer std.testing.allocator.free(string);
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

test "Value.from" {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(Value.from(true).boolean, true);
    try std.testing.expectEqual(Value.from(false).boolean, false);
    try std.testing.expectEqual(Value.from("").string.ascii, "");
    try std.testing.expectEqual(Value.from("foo").string.ascii, "foo");
    try std.testing.expectEqual(Value.from("123").string.ascii, "123");
    try std.testing.expectEqual(Value.from(0).number.i32, 0);
    try std.testing.expectEqual(Value.from(0.0).number.i32, 0);
    try std.testing.expectEqual(Value.from(123).number.i32, 123);
    try std.testing.expectEqual(Value.from(123.0).number.i32, 123);
    try std.testing.expectEqual(Value.from(123.456).number.f64, 123.456);
    try std.testing.expectEqual(Value.from(std.math.inf(f64)).number.f64, inf);
    try std.testing.expect(std.math.isNan(Value.from(std.math.nan(f64)).number.f64));
}
