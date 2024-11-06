//! 6.1 ECMAScript Language Types
//! https://tc39.es/ecma262/#sec-ecmascript-language-types

const builtin = @import("builtin");
const std = @import("std");

const build_options = @import("build-options");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const pretty_printing = @import("../../pretty_printing.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BigInt = types.BigInt;
const Number = types.Number;
const Object = types.Object;
const PrivateName = types.PrivateName;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Symbol = types.Symbol;
const arrayCreate = builtins.arrayCreate;
const getIterator = types.getIterator;
const isZigString = utils.isZigString;
const keyForSymbol = builtins.keyForSymbol;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const prettyPrintValue = pretty_printing.prettyPrintValue;
const stringCreate = builtins.stringCreate;
const validateNonRevokedProxy = builtins.validateNonRevokedProxy;

pub const Weak = @import("Value/weak.zig").Weak;

const pow_2_8 = std.math.pow(f64, 2, 8);
const pow_2_16 = std.math.pow(f64, 2, 16);
const pow_2_32 = std.math.pow(f64, 2, 32);

const Value = @This();

pub const PreferredType = enum { string, number };

pub const Numeric = union(enum) {
    number: Number,
    big_int: *const BigInt,

    pub fn sameType(x: Numeric, y: Numeric) bool {
        return std.meta.activeTag(x) == std.meta.activeTag(y);
    }
};

pub const Type = enum {
    /// 6.1.1 The Undefined Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-undefined-type
    undefined,

    /// 6.1.2 The Null Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-null-type
    null,

    /// 6.1.3 The Boolean Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-boolean-type
    boolean,

    /// 6.1.4 The String Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-string-type
    string,

    /// 6.1.5 The Symbol Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type
    symbol,

    /// 6.1.6.1 The Number Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-number-type
    number,

    /// 6.1.6.2 The BigInt Type
    /// https://tc39.es/ecma262/#sec-ecmascript-language-types-bigint-type
    big_int,

    /// 6.1.7 The Object Type
    /// https://tc39.es/ecma262/#sec-object-type
    object,
};

const TaggedUnionImpl = union(enum) {
    undefined,
    null,
    boolean: bool,
    string: *const String,
    symbol: *const Symbol,
    number_i32: i32,
    number_f64: f64,
    big_int: *const BigInt,
    object: *Object,

    pub inline fn from(value: anytype) TaggedUnionImpl {
        const T = @TypeOf(value);
        const is_number = switch (@typeInfo(T)) {
            .int, .comptime_int, .float, .comptime_float => true,
            else => false,
        };
        if (T == bool) {
            return .{ .boolean = value };
        } else if (isZigString(T)) {
            const string = String.fromLiteral(value);
            return .{ .string = string };
        } else if (is_number or T == Number) {
            const number = if (T == Number) value else Number.from(value);
            return switch (number) {
                .i32 => |x| .{ .number_i32 = x },
                .f64 => |x| .{ .number_f64 = x },
            };
        } else if (@typeInfo(T) == .pointer) {
            switch (@typeInfo(T).pointer.child) {
                BigInt => return .{ .big_int = value },
                Object => return .{ .object = value },
                String => return .{ .string = value },
                Symbol => return .{ .symbol = value },
                else => {},
            }
        }
        @compileError("from() called with incompatible type " ++ @typeName(T));
    }

    pub fn @"type"(self: TaggedUnionImpl) Type {
        return switch (self) {
            .undefined => .undefined,
            .null => .null,
            .boolean => .boolean,
            .string => .string,
            .symbol => .symbol,
            .number_i32, .number_f64 => .number,
            .big_int => .big_int,
            .object => .object,
        };
    }

    pub fn asBoolean(self: TaggedUnionImpl) bool {
        return self.boolean;
    }

    pub fn asString(self: TaggedUnionImpl) *const String {
        return self.string;
    }

    pub fn asSymbol(self: TaggedUnionImpl) *const Symbol {
        return self.symbol;
    }

    pub fn asNumber(self: TaggedUnionImpl) Number {
        return switch (self) {
            .number_i32 => |number_i32| .{ .i32 = number_i32 },
            .number_f64 => |number_f64| .{ .f64 = number_f64 },
            else => unreachable,
        };
    }

    pub fn asBigInt(self: TaggedUnionImpl) *const BigInt {
        return self.big_int;
    }

    pub fn asObject(self: TaggedUnionImpl) *Object {
        return self.object;
    }
};

/// NaN boxing is a technique of hiding extra variants and payloads within an
/// f64. For reference, this is the layout of a f64:
///
/// - Sign (1 bit)
/// - Exponent (11 bits)
/// - Fraction (52 bits)
///
/// A NaN value must have all the exponent bits set to 1 and at least one
/// fraction bit set to 1.
const NanBoxingImpl = enum(u64) {
    /// NaN with the quiet bit (highest fraction bit) set.
    const nan_mask: u64 = 0x7ff8000000000000;
    const payload_len = 48;

    undefined = initBits(.undefined, 0, {}),
    null = initBits(.null, 0, {}),
    boolean_false = initBits(.boolean, 0, false),
    boolean_true = initBits(.boolean, 0, true),
    number_nan = nan_mask,
    _,

    /// We always have the highest bit in the 52 bits of the fraction field
    /// (the quiet bit) set. Then, we use the 3 bits below the quiet bit as
    /// a tag for non-f64 values (except f64-NaN itself).
    const Tag = enum(u3) {
        number_f64 = 0,
        undefined,
        null,
        boolean,
        string,
        symbol_or_big_int,
        number_i32,
        object,
    };

    fn TagPayload(comptime tag: Tag, comptime sign_bit: u1) type {
        return switch (tag) {
            .number_f64 => f64,
            .undefined,
            .null,
            => void,
            .boolean => bool,
            .string => *const String,
            .symbol_or_big_int => switch (sign_bit) {
                0 => *const Symbol,
                1 => *const BigInt,
            },
            .number_i32 => i32,
            .object => *Object,
        };
    }

    fn initBits(comptime tag: Tag, comptime sign_bit: u1, payload: TagPayload(tag, sign_bit)) u64 {
        const T = @TypeOf(payload);
        const tag_bits: u64 = @as(u64, @intFromEnum(tag)) << payload_len;
        if (T == f64) {
            return @bitCast(payload);
        } else if (@typeInfo(T) == .pointer) {
            const high_sign_bit = @bitReverse(@as(u64, sign_bit));
            const ptr_bits = @intFromPtr(payload);
            std.debug.assert(nan_mask & ptr_bits == 0);
            return nan_mask | tag_bits | high_sign_bit | ptr_bits;
        } else if (@sizeOf(T) != 0) {
            // @bitCast() doesn't work on void
            const payload_bits: std.meta.Int(.unsigned, @bitSizeOf(T)) = @bitCast(payload);
            return nan_mask | tag_bits | payload_bits;
        } else {
            return nan_mask | tag_bits;
        }
    }

    fn init(comptime tag: Tag, comptime sign_bit: u1, payload: TagPayload(tag, sign_bit)) NanBoxingImpl {
        return @enumFromInt(initBits(tag, sign_bit, payload));
    }

    /// If the NaN bits are set, then parses the tag from the fraction section.
    /// Otherwise, returns number_f64.
    fn getTag(self: NanBoxingImpl) Tag {
        const bits: u64 = @intFromEnum(self);
        const tag_bits: u3 = @truncate(bits >> payload_len);
        return if (bits & nan_mask == nan_mask) @enumFromInt(tag_bits) else .number_f64;
    }

    fn getPayload(self: NanBoxingImpl, comptime tag: Tag, comptime sign_bit: u1) TagPayload(tag, sign_bit) {
        std.debug.assert(self.getTag() == tag);
        const T = TagPayload(tag, sign_bit);
        const bits: u64 = @intFromEnum(self);
        if (@typeInfo(T) == .pointer) {
            const ptr_bits: if (@sizeOf(T) >= 8) u48 else usize = @truncate(bits);
            return @ptrFromInt(ptr_bits);
        } else {
            const payload_bits: std.meta.Int(.unsigned, @bitSizeOf(T)) = @truncate(bits);
            return @bitCast(payload_bits);
        }
    }

    pub inline fn from(value: anytype) NanBoxingImpl {
        const T = @TypeOf(value);
        const is_number = switch (@typeInfo(T)) {
            .int, .comptime_int, .float, .comptime_float => true,
            else => false,
        };
        if (T == bool) {
            return if (value) .boolean_true else .boolean_false;
        } else if (isZigString(T)) {
            const string = String.fromLiteral(value);
            return init(.string, 0, string);
        } else if (is_number or T == Number) {
            const number = if (T == Number) value else Number.from(value);
            switch (number) {
                .i32 => |x| return init(.number_i32, 0, x),
                .f64 => |x| {
                    // Normalize all NaN values to avoid type confusion vulnerabilities.
                    return if (std.math.isNan(x)) .number_nan else init(.number_f64, 0, x);
                },
            }
        } else if (@typeInfo(T) == .pointer) {
            switch (@typeInfo(T).pointer.child) {
                BigInt => return init(.symbol_or_big_int, 1, value),
                Object => return init(.object, 0, value),
                String => return init(.string, 0, value),
                Symbol => return init(.symbol_or_big_int, 0, value),
                else => {},
            }
        }
        @compileError("from() called with incompatible type " ++ @typeName(T));
    }

    pub fn @"type"(self: NanBoxingImpl) Type {
        return switch (self.getTag()) {
            .undefined => .undefined,
            .null => .null,
            .boolean => .boolean,
            .string => .string,
            .symbol_or_big_int => {
                const bits: u64 = @intFromEnum(self);
                const high_bit: u1 = @truncate(@bitReverse(bits));
                return switch (high_bit) {
                    0 => .symbol,
                    1 => .big_int,
                };
            },
            .number_i32, .number_f64 => .number,
            .object => .object,
        };
    }

    pub fn asBoolean(self: NanBoxingImpl) bool {
        return switch (self) {
            .boolean_false => false,
            .boolean_true => true,
            else => unreachable,
        };
    }

    pub fn asNumber(self: NanBoxingImpl) Number {
        return switch (self.getTag()) {
            .number_i32 => .{ .i32 = self.getPayload(.number_i32, 0) },
            .number_f64 => .{ .f64 = self.getPayload(.number_f64, 0) },
            else => unreachable,
        };
    }

    pub fn asString(self: NanBoxingImpl) *const String {
        return self.getPayload(.string, 0);
    }

    pub fn asSymbol(self: NanBoxingImpl) *const Symbol {
        return self.getPayload(.symbol_or_big_int, 0);
    }

    pub fn asBigInt(self: NanBoxingImpl) *const BigInt {
        return self.getPayload(.symbol_or_big_int, 1);
    }

    pub fn asObject(self: NanBoxingImpl) *Object {
        return self.getPayload(.object, 0);
    }
};

comptime {
    // Let's make sure the size doesn't quietly change
    switch (Impl) {
        TaggedUnionImpl => switch (builtin.target.ptrBitWidth()) {
            // Only some 32-bit platforms have certain bitpacking optimizations applied
            32 => std.debug.assert(@sizeOf(Impl) == 12 or @sizeOf(Impl) == 16),
            64 => std.debug.assert(@sizeOf(Impl) == 16),
            else => unreachable,
        },
        NanBoxingImpl => std.debug.assert(@sizeOf(Impl) == 8),
        else => unreachable,
    }
}

const Impl = if (build_options.enable_nan_boxing) NanBoxingImpl else TaggedUnionImpl;
impl: Impl,

pub const @"undefined": Value = .{ .impl = Impl.undefined };
pub const @"null": Value = .{ .impl = Impl.null };
pub const nan: Value = from(std.math.nan(f64));
pub const infinity: Value = from(std.math.inf(f64));
pub const negative_infinity: Value = from(-std.math.inf(f64));

pub fn format(
    self: Value,
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
    switch (self.type()) {
        .undefined => try writer.writeAll("undefined"),
        .null => try writer.writeAll("null"),
        .boolean => try writer.writeAll(if (self.asBoolean()) "true" else "false"),
        .string => try writer.print("\"{}\"", .{self.asString()}),
        .symbol => try writer.print("{}", .{self.asSymbol()}),
        .number => try writer.print("{}", .{self.asNumber()}),
        .big_int => try writer.print("{}", .{self.asBigInt()}),
        .object => try writer.print("{}", .{self.asObject()}),
    }
}

pub inline fn from(value: anytype) Value {
    return .{ .impl = Impl.from(value) };
}

pub fn @"type"(self: Value) Type {
    return self.impl.type();
}

pub fn isUndefined(self: Value) bool {
    return self.impl == .undefined;
}

pub fn isNull(self: Value) bool {
    return self.impl == .null;
}

pub fn isBoolean(self: Value) bool {
    return self.impl.type() == .boolean;
}

pub fn asBoolean(self: Value) bool {
    return self.impl.asBoolean();
}

pub fn isString(self: Value) bool {
    return self.impl.type() == .string;
}

pub fn asString(self: Value) *const String {
    return self.impl.asString();
}

pub fn isSymbol(self: Value) bool {
    return self.impl.type() == .symbol;
}

pub fn asSymbol(self: Value) *const Symbol {
    return self.impl.asSymbol();
}

pub fn isNumber(self: Value) bool {
    return self.impl.type() == .number;
}

pub fn asNumber(self: Value) Number {
    return self.impl.asNumber();
}

pub fn isBigInt(self: Value) bool {
    return self.impl.type() == .big_int;
}

pub fn asBigInt(self: Value) *const BigInt {
    return self.impl.asBigInt();
}

pub fn isObject(self: Value) bool {
    return self.impl.type() == .object;
}

pub fn asObject(self: Value) *Object {
    return self.impl.asObject();
}

/// Return a string according to the 'typeof' operator semantics.
pub fn typeof(self: Value) *const String {
    // Excerpt from https://tc39.es/ecma262/#sec-typeof-operator-runtime-semantics-evaluation
    return switch (self.type()) {
        // 4. If val is undefined, return "undefined".
        .undefined => String.fromLiteral("undefined"),

        // 5. If val is null, return "object".
        .null => String.fromLiteral("object"),

        // 6. If val is a String, return "string".
        .string => String.fromLiteral("string"),

        // 7. If val is a Symbol, return "symbol".
        .symbol => String.fromLiteral("symbol"),

        // 8. If val is a Boolean, return "boolean".
        .boolean => String.fromLiteral("boolean"),

        // 9. If val is a Number, return "number".
        .number => String.fromLiteral("number"),

        // 10. If val is a BigInt, return "bigint".
        .big_int => String.fromLiteral("bigint"),

        // 11. Assert: val is an Object.
        .object => blk: {
            // B.3.6.3 Changes to the typeof Operator
            // https://tc39.es/ecma262/#sec-IsHTMLDDA-internal-slot-typeof
            if (build_options.enable_annex_b) {
                // 12. If val has an [[IsHTMLDDA]] internal slot, return "undefined".
                if (self.asObject().is_htmldda) break :blk String.fromLiteral("undefined");
            } else {
                // 12. NOTE: This step is replaced in section B.3.6.3.
            }

            // 13. If val has a [[Call]] internal slot, return "function".
            if (self.asObject().internal_methods.call) |_| break :blk String.fromLiteral("function");

            // 14. Return "object".
            break :blk String.fromLiteral("object");
        },
    };
}

/// 6.2.6.5 ToPropertyDescriptor ( Obj )
/// https://tc39.es/ecma262/#sec-topropertydescriptor
pub fn toPropertyDescriptor(self: Value, agent: *Agent) Agent.Error!PropertyDescriptor {
    // 1. If Obj is not an Object, throw a TypeError exception.
    if (!self.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{self});
    }

    // 2. Let desc be a new Property Descriptor that initially has no fields.
    var descriptor: PropertyDescriptor = .{};

    // 3. Let hasEnumerable be ? HasProperty(Obj, "enumerable").
    const has_enumerable = try self.asObject().hasProperty(PropertyKey.from("enumerable"));

    // 4. If hasEnumerable is true, then
    if (has_enumerable) {
        // a. Let enumerable be ToBoolean(? Get(Obj, "enumerable")).
        const enumerable = (try self.asObject().get(PropertyKey.from("enumerable"))).toBoolean();

        // b. Set desc.[[Enumerable]] to enumerable.
        descriptor.enumerable = enumerable;
    }

    // 5. Let hasConfigurable be ? HasProperty(Obj, "configurable").
    const has_configurable = try self.asObject().hasProperty(PropertyKey.from("configurable"));

    // 6. If hasConfigurable is true, then
    if (has_configurable) {
        // a. Let configurable be ToBoolean(? Get(Obj, "configurable")).
        const configurable = (try self.asObject().get(PropertyKey.from("configurable"))).toBoolean();

        // b. Set desc.[[Configurable]] to configurable.
        descriptor.configurable = configurable;
    }

    // 7. Let hasValue be ? HasProperty(Obj, "value").
    const has_value = try self.asObject().hasProperty(PropertyKey.from("value"));

    // 8. If hasValue is true, then
    if (has_value) {
        // a. Let value be ? Get(Obj, "value").
        const value = try self.asObject().get(PropertyKey.from("value"));

        // b. Set desc.[[Value]] to value.
        descriptor.value = value;
    }

    // 9. Let hasWritable be ? HasProperty(Obj, "writable").
    const has_writable = try self.asObject().hasProperty(PropertyKey.from("writable"));

    // 10. If hasWritable is true, then
    if (has_writable) {
        // a. Let writable be ToBoolean(? Get(Obj, "writable")).
        const writable = (try self.asObject().get(PropertyKey.from("writable"))).toBoolean();

        // b. Set desc.[[Writable]] to writable.
        descriptor.writable = writable;
    }

    // 11. Let hasGet be ? HasProperty(Obj, "get").
    const has_get = try self.asObject().hasProperty(PropertyKey.from("get"));

    // 12. If hasGet is true, then
    if (has_get) {
        // a. Let getter be ? Get(Obj, "get").
        const getter = try self.asObject().get(PropertyKey.from("get"));

        // b. If IsCallable(getter) is false and getter is not undefined, throw a TypeError
        //    exception.
        if (!getter.isCallable() and !getter.isUndefined()) {
            return agent.throwException(.type_error, "{} is not callable", .{getter});
        }

        // c. Set desc.[[Get]] to getter.
        descriptor.get = if (!getter.isUndefined()) getter.asObject() else @as(?*Object, null);
    }

    // 13. Let hasSet be ? HasProperty(Obj, "set").
    const has_set = try self.asObject().hasProperty(PropertyKey.from("set"));

    // 14. If hasSet is true, then
    if (has_set) {
        // a. Let setter be ? Get(Obj, "set").
        const setter = try self.asObject().get(PropertyKey.from("set"));

        // b. If IsCallable(setter) is false and setter is not undefined, throw a TypeError
        //    exception.
        if (!setter.isCallable() and !setter.isUndefined()) {
            return agent.throwException(.type_error, "{} is not callable", .{setter});
        }

        // c. Set desc.[[Set]] to setter.
        descriptor.set = if (!setter.isUndefined()) setter.asObject() else @as(?*Object, null);
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
pub fn toPrimitive(self: Value, agent: *Agent, preferred_type: ?PreferredType) Agent.Error!Value {
    // 1. If input is an Object, then
    if (self.isObject()) {
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
            const result = try from(exotic_to_primitive).callAssumeCallable(
                self,
                &.{from(hint)},
            );

            // v. If result is not an Object, return result.
            if (!result.isObject()) return result;

            // vi. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Could not convert object to primitive",
                .{},
            );
        }

        // c. If preferredType is not present, let preferredType be number.
        // d. Return ? OrdinaryToPrimitive(input, preferredType).
        return self.asObject().ordinaryToPrimitive(preferred_type orelse .number);
    }

    // 2. Return input.
    return self;
}

/// 7.1.2 ToBoolean ( argument )
/// https://tc39.es/ecma262/#sec-toboolean
pub fn toBoolean(self: Value) bool {
    // 1. If argument is a Boolean, return argument.
    if (self.isBoolean()) return self.asBoolean();

    // 2. If argument is one of undefined, null, +0𝔽, -0𝔽, NaN, 0ℤ, or the empty String, return
    //    false.
    switch (self.type()) {
        .undefined, .null => return false,
        .number => if (self.asNumber().isZero() or self.asNumber().isNan()) {
            return false;
        },
        .big_int => if (self.asBigInt().managed.eqlZero()) {
            return false;
        },
        .string => if (self.asString().isEmpty()) {
            return false;
        },
        else => {},
    }

    // B.3.6.1 Changes to ToBoolean
    // https://tc39.es/ecma262/#sec-IsHTMLDDA-internal-slot-to-boolean
    if (build_options.enable_annex_b) {
        // 3. If argument is an Object and argument has an [[IsHTMLDDA]] internal slot, return
        //    false.
        if (self.isObject() and self.asObject().is_htmldda) return false;
    } else {
        // 3. NOTE: This step is replaced in section B.3.6.1.
    }

    // 4. Return true.
    return true;
}

/// 7.1.3 ToNumeric ( value )
/// https://tc39.es/ecma262/#sec-tonumeric
pub fn toNumeric(self: Value, agent: *Agent) Agent.Error!Numeric {
    // 1. Let primValue be ? ToPrimitive(value, number).
    const primitive_value = try self.toPrimitive(agent, .number);

    // 2. If primValue is a BigInt, return primValue.
    if (primitive_value.isBigInt()) return .{ .big_int = primitive_value.asBigInt() };

    // 3. Return ? ToNumber(primValue).
    return .{ .number = try primitive_value.toNumber(agent) };
}

/// 7.1.4 ToNumber ( argument )
/// https://tc39.es/ecma262/#sec-tonumber
pub fn toNumber(self: Value, agent: *Agent) Agent.Error!Number {
    switch (self.type()) {
        // 1. If argument is a Number, return argument.
        .number => return self.asNumber(),

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
        .boolean => return Number.from(@intFromBool(self.asBoolean())),

        // 6. If argument is a String, return StringToNumber(argument).
        .string => return stringToNumber(agent.gc_allocator, self.asString()),

        // 7. Assert: argument is an Object.
        .object => {
            // 8. Let primValue be ? ToPrimitive(argument, number).
            const primitive_value = try self.toPrimitive(agent, .number);

            // 9. Assert: primValue is not an Object.
            std.debug.assert(!primitive_value.isObject());

            // 10. Return ? ToNumber(primValue).
            return primitive_value.toNumber(agent);
        },
    }
}

/// 7.1.5 ToIntegerOrInfinity ( argument )
/// https://tc39.es/ecma262/#sec-tointegerorinfinity
pub fn toIntegerOrInfinity(self: Value, agent: *Agent) Agent.Error!f64 {
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
pub fn toInt32(self: Value, agent: *Agent) Agent.Error!i32 {
    // OPTIMIZATION: We may already have an i32 :^)
    if (self.isNumber() and self.asNumber() == .i32) return self.asNumber().i32;

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
pub fn toUint32(self: Value, agent: *Agent) Agent.Error!u32 {
    // OPTIMIZATION: We may already have a positive i32 :^)
    if (self.isNumber() and self.asNumber() == .i32 and self.asNumber().i32 >= 0)
        return @intCast(self.asNumber().i32);

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
pub fn toInt16(self: Value, agent: *Agent) Agent.Error!i16 {
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
pub fn toUint16(self: Value, agent: *Agent) Agent.Error!u16 {
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
pub fn toInt8(self: Value, agent: *Agent) Agent.Error!i8 {
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
pub fn toUint8(self: Value, agent: *Agent) Agent.Error!u8 {
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
pub fn toUint8Clamp(self: Value, agent: *Agent) Agent.Error!u8 {
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
pub fn toBigInt(self: Value, agent: *Agent) Agent.Error!*const BigInt {
    // 1. Let prim be ? ToPrimitive(argument, number).
    const primitive = try self.toPrimitive(agent, .number);

    // 2. Return the value that prim corresponds to in Table 12.
    return switch (primitive.type()) {
        // Throw a TypeError exception.
        .undefined => agent.throwException(.type_error, "Cannot convert undefined to BigInt", .{}),
        .null => agent.throwException(.type_error, "Cannot convert null to BigInt", .{}),
        .number => agent.throwException(.type_error, "Cannot convert number to BigInt", .{}),
        .symbol => agent.throwException(.type_error, "Cannot convert symbol to BigInt", .{}),

        // Return 1n if prim is true and 0n if prim is false.
        .boolean => if (primitive.asBoolean())
            agent.pre_allocated.one
        else
            agent.pre_allocated.zero,

        // Return prim.
        .big_int => primitive.asBigInt(),

        .string => {
            // 1. Let n be StringToBigInt(prim).
            const n = try stringToBigInt(agent.gc_allocator, primitive.asString());

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
pub fn toBigInt64(self: Value, agent: *Agent) Agent.Error!i64 {
    // 1. Let n be ? ToBigInt(argument).
    const n = try self.toBigInt(agent);

    // 2. Let int64bit be ℝ(n) modulo 2**64.
    // 3. If int64bit ≥ 2**63, return ℤ(int64bit - 2**64); otherwise return ℤ(int64bit).
    var int64bit = try std.math.big.int.Managed.init(agent.gc_allocator);
    try int64bit.truncate(&n.managed, .signed, 64);
    return int64bit.to(i64) catch unreachable;
}

/// 7.1.16 ToBigUint64 ( argument )
/// https://tc39.es/ecma262/#sec-tobiguint64
pub fn toBigUint64(self: Value, agent: *Agent) Agent.Error!u64 {
    // 1. Let n be ? ToBigInt(argument).
    const n = try self.toBigInt(agent);

    // 2. Let int64bit be ℝ(n) modulo 2**64.
    // 3. Return ℤ(int64bit).
    var int64bit = try std.math.big.int.Managed.init(agent.gc_allocator);
    try int64bit.truncate(&n.managed, .unsigned, 64);
    return int64bit.to(u64) catch unreachable;
}

/// 7.1.17 ToString ( argument )
/// https://tc39.es/ecma262/#sec-tostring
pub fn toString(self: Value, agent: *Agent) Agent.Error!*const String {
    return switch (self.type()) {
        // 1. If argument is a String, return argument.
        .string => self.asString(),

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
        .boolean => if (self.asBoolean())
            String.fromLiteral("true")
        else
            String.fromLiteral("false"),

        // 7. If argument is a Number, return Number::toString(argument, 10).
        .number => self.asNumber().toString(agent.gc_allocator, 10),

        // 8. If argument is a BigInt, return BigInt::toString(argument, 10).
        .big_int => self.asBigInt().toString(agent.gc_allocator, 10),

        // 9. Assert: argument is an Object.
        .object => {
            // 10. Let primValue be ? ToPrimitive(argument, string).
            const primitive_value = try self.toPrimitive(agent, .string);

            // 11. Assert: primValue is not an Object.
            std.debug.assert(!primitive_value.isObject());

            // 12. Return ? ToString(primValue).
            return primitive_value.toString(agent);
        },
    };
}

/// 7.1.18 ToObject ( argument )
/// https://tc39.es/ecma262/#sec-toobject
pub fn toObject(self: Value, agent: *Agent) Agent.Error!*Object {
    const realm = agent.currentRealm();
    return switch (self.type()) {
        .undefined => agent.throwException(.type_error, "Cannot convert undefined to Object", .{}),
        .null => agent.throwException(.type_error, "Cannot convert null to Object", .{}),
        .boolean => try builtins.Boolean.create(agent, .{
            .fields = .{ .boolean_data = self.asBoolean() },
            .prototype = try realm.intrinsics.@"%Boolean.prototype%"(),
        }),
        .number => try builtins.Number.create(agent, .{
            .fields = .{ .number_data = self.asNumber() },
            .prototype = try realm.intrinsics.@"%Number.prototype%"(),
        }),
        .string => try stringCreate(
            agent,
            self.asString(),
            try realm.intrinsics.@"%String.prototype%"(),
        ),
        .symbol => try builtins.Symbol.create(agent, .{
            .fields = .{ .symbol_data = self.asSymbol() },
            .prototype = try realm.intrinsics.@"%Symbol.prototype%"(),
        }),
        .big_int => try builtins.BigInt.create(agent, .{
            .fields = .{ .big_int_data = self.asBigInt() },
            .prototype = try realm.intrinsics.@"%BigInt.prototype%"(),
        }),
        .object => self.asObject(),
    };
}

/// 7.1.19 ToPropertyKey ( argument )
/// https://tc39.es/ecma262/#sec-topropertykey
pub fn toPropertyKey(self: Value, agent: *Agent) Agent.Error!PropertyKey {
    // 1. Let key be ? ToPrimitive(argument, string).
    const key = try self.toPrimitive(agent, .string);

    // 2. If key is a Symbol, then
    if (key.isSymbol()) {
        // a. Return key.
        return PropertyKey.from(key.asSymbol());
    }

    // OPTIMIZATION: If we have a number that fits into an `PropertyKey.IntegerIndex` there's
    //               no need to do a string conversion and back.
    if (key.isNumber() and key.asNumber().isIntegral()) {
        switch (key.asNumber()) {
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
pub fn toLength(self: Value, agent: *Agent) Agent.Error!u53 {
    // 1. Let len be ? ToIntegerOrInfinity(argument).
    const length = try self.toIntegerOrInfinity(agent);

    // 2. If len ≤ 0, return +0𝔽.
    if (length <= 0) return 0;

    // 3. Return 𝔽(min(len, 2**53 - 1)).
    return @intFromFloat(@min(length, std.math.maxInt(u53)));
}

/// 7.1.22 ToIndex ( value )
/// https://tc39.es/ecma262/#sec-toindex
pub fn toIndex(self: Value, agent: *Agent) Agent.Error!u53 {
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
pub fn requireObjectCoercible(self: Value, agent: *Agent) error{ExceptionThrown}!Value {
    switch (self.type()) {
        .undefined => return agent.throwException(.type_error, "Cannot convert undefined to Object", .{}),
        .null => return agent.throwException(.type_error, "Cannot convert null to Object", .{}),
        else => return self,
    }
}

/// 7.2.2 IsArray ( argument )
/// https://tc39.es/ecma262/#sec-isarray
pub fn isArray(self: Value) error{ExceptionThrown}!bool {
    // 1. If argument is not an Object, return false.
    if (!self.isObject()) return false;

    // 2. If argument is an Array exotic object, return true.
    if (self.asObject().is(builtins.Array)) return true;

    // 3. If argument is a Proxy exotic object, then
    if (self.asObject().is(builtins.Proxy)) {
        // a. Perform ? ValidateNonRevokedProxy(argument).
        try validateNonRevokedProxy(self.asObject().as(builtins.Proxy));

        // b. Let proxyTarget be argument.[[ProxyTarget]].
        const proxy_target = self.asObject().as(builtins.Proxy).fields.proxy_target;

        // c. Return ? IsArray(proxyTarget).
        return from(proxy_target.?).isArray();
    }

    // 4. Return false.
    return false;
}

/// 7.2.3 IsCallable ( argument )
/// https://tc39.es/ecma262/#sec-iscallable
pub fn isCallable(self: Value) bool {
    // 1. If argument is not an Object, return false.
    if (!self.isObject()) return false;

    // 2. If argument has a [[Call]] internal method, return true.
    if (self.asObject().internal_methods.call != null) return true;

    // 3. Return false.
    return false;
}

/// 7.2.4 IsConstructor ( argument )
/// https://tc39.es/ecma262/#sec-isconstructor
pub fn isConstructor(self: Value) bool {
    // 1. If argument is not an Object, return false.
    if (!self.isObject()) return false;

    // 2. If argument has a [[Construct]] internal method, return true.
    if (self.asObject().internal_methods.construct != null) return true;

    // 3. Return false.
    return false;
}

/// 7.2.6 IsRegExp ( argument )
/// https://tc39.es/ecma262/#sec-isregexp
pub fn isRegExp(self: Value) Agent.Error!bool {
    // 1. If argument is not an Object, return false.
    if (!self.isObject()) return false;

    const agent = self.asObject().agent;

    // 2. Let matcher be ? Get(argument, %Symbol.match%).
    const matcher = try self.asObject().get(
        PropertyKey.from(agent.well_known_symbols.@"%Symbol.match%"),
    );

    // 3. If matcher is not undefined, return ToBoolean(matcher).
    if (!matcher.isUndefined()) return matcher.toBoolean();

    // 4. If argument has a [[RegExpMatcher]] internal slot, return true.
    if (self.asObject().is(builtins.RegExp)) return true;

    // 5. Return false.
    return false;
}

/// 7.3.3 GetV ( V, P )
/// https://tc39.es/ecma262/#sec-getv
pub fn get(self: Value, agent: *Agent, property_key: PropertyKey) Agent.Error!Value {
    // 1. Let O be ? ToObject(V).
    const object = try self.toObject(agent);

    // 2. Return ? O.[[Get]](P, V).
    return object.internal_methods.get(object, property_key, self);
}

/// 7.3.10 GetMethod ( V, P )
/// https://tc39.es/ecma262/#sec-getmethod
pub fn getMethod(self: Value, agent: *Agent, property_key: PropertyKey) Agent.Error!?*Object {
    // 1. Let func be ? GetV(V, P).
    const function = try self.get(agent, property_key);

    // 2. If func is either undefined or null, return undefined.
    if (function.isUndefined() or function.isNull()) return null;

    // 3. If IsCallable(func) is false, throw a TypeError exception.
    if (!function.isCallable()) {
        return agent.throwException(.type_error, "{} is not callable", .{self});
    }

    // 4. Return func.
    return function.asObject();
}

/// 7.3.13 Call ( F, V [ , argumentsList ] )
/// https://tc39.es/ecma262/#sec-call
pub fn call(
    self: Value,
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
    return self.asObject().internal_methods.call.?(
        self.asObject(),
        this_value,
        Arguments.from(arguments_list),
    );
}

pub fn callNoArgs(self: Value, agent: *Agent, this_value: Value) Agent.Error!Value {
    return self.call(agent, this_value, &.{});
}

pub fn callAssumeCallable(self: Value, this_value: Value, arguments_list: []const Value) Agent.Error!Value {
    return self.asObject().internal_methods.call.?(
        self.asObject(),
        this_value,
        Arguments.from(arguments_list),
    );
}

pub fn callAssumeCallableNoArgs(self: Value, this_value: Value) Agent.Error!Value {
    return self.callAssumeCallable(this_value, &.{});
}

const ValidElementTypes = enum {
    all,
    property_key,
};

/// 7.3.19 CreateListFromArrayLike ( obj [ , validElementTypes ] )
/// https://tc39.es/ecma262/#sec-createlistfromarraylike
pub fn createListFromArrayLike(
    self: Value,
    agent: *Agent,
    maybe_valid_element_types: ?ValidElementTypes,
) Agent.Error![]Value {
    // 1. If validElementTypes is not present, set validElementTypes to all.
    const valid_element_types = maybe_valid_element_types orelse .all;

    // 2. If obj is not an Object, throw a TypeError exception.
    if (!self.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{self});
    }

    // 3. Let len be ? LengthOfArrayLike(obj).
    const len = try self.asObject().lengthOfArrayLike();

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

        // c. If validElementTypes is property-key and next is not a property key, throw a
        //    TypeError exception.
        if (valid_element_types == .property_key and switch (next.type()) {
            .string, .symbol => false,
            else => true,
        }) {
            return agent.throwException(
                .type_error,
                "Array element {} must be a string or symbol",
                .{next},
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
    self: Value,
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

pub fn invokeNoArgs(
    self: Value,
    agent: *Agent,
    property_key: PropertyKey,
) Agent.Error!Value {
    return self.invoke(agent, property_key, &.{});
}

/// 7.3.21 OrdinaryHasInstance ( C, O )
/// https://tc39.es/ecma262/#sec-ordinaryhasinstance
pub fn ordinaryHasInstance(self: Value, object_value: Value) Agent.Error!bool {
    // 1. If IsCallable(C) is false, return false.
    if (!self.isCallable()) return false;

    const agent = self.asObject().agent;

    // 2. If C has a [[BoundTargetFunction]] internal slot, then
    if (self.asObject().is(builtins.BoundFunction)) {
        // a. Let BC be C.[[BoundTargetFunction]].
        const bound_constructor = self.asObject().as(builtins.BoundFunction).fields.bound_target_function;

        // b. Return ? InstanceofOperator(O, BC).
        return object_value.instanceofOperator(agent, from(bound_constructor));
    }

    // 3. If O is not an Object, return false.
    if (!object_value.isObject()) return false;

    // 4. Let P be ? Get(C, "prototype").
    const prototype = try self.asObject().get(PropertyKey.from("prototype"));

    // 5. If P is not an Object, throw a TypeError exception.
    if (!prototype.isObject()) {
        return agent.throwException(.type_error, "'prototype' property must be an object", .{});
    }

    var object: ?*Object = object_value.asObject();

    // 6. Repeat,
    while (true) {
        // a. Set O to ? O.[[GetPrototypeOf]]().
        object = try object.?.internal_methods.getPrototypeOf(object.?);

        // b. If O is null, return false.
        if (object == null) return false;

        // c. If SameValue(P, O) is true, return true.
        if (prototype.asObject() == object.?) return true;
    }
}

/// 7.3.34 AddValueToKeyedGroup ( groups, key, value )
/// https://tc39.es/ecma262/#sec-add-value-to-keyed-group
fn addValueToKeyedGroup(
    agent: *Agent,
    groups: anytype,
    key: anytype,
    value: Value,
) std.mem.Allocator.Error!void {
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
        .property => PropertyKey.ArrayHashMap(std.ArrayList(Value)),
        .collection => Value.ArrayHashMap(std.ArrayList(Value), sameValue),
    };
}

/// 7.3.35 GroupBy ( items, callback, keyCoercion )
/// https://tc39.es/ecma262/#sec-groupby
pub fn groupBy(
    self: Value,
    agent: *Agent,
    callback: Value,
    comptime key_coercion: KeyCoercion,
) Agent.Error!GroupByContainer(key_coercion) {
    // 1. Perform ? RequireObjectCoercible(items).
    _ = try self.requireObjectCoercible(agent);

    // 2. If IsCallable(callback) is false, throw a TypeError exception.
    if (!callback.isCallable()) {
        return agent.throwException(.type_error, "{} is not callable", .{callback});
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

        // e. Let key be Completion(Call(callback, undefined, « value, 𝔽(k) »)).
        const key = callback.callAssumeCallable(
            @"undefined",
            &.{ value, from(k) },
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

/// 7.3.36 SetterThatIgnoresPrototypeProperties ( thisValue, home, p, v )
/// https://tc39.es/ecma262/#sec-SetterThatIgnoresPrototypeProperties
pub fn setterThatIgnoresPrototypeProperties(
    self: Value,
    agent: *Agent,
    home: *Object,
    property_key: PropertyKey,
    value: Value,
) Agent.Error!void {
    // 1. If thisValue is not an Object, then
    if (!self.isObject()) {
        // a. Throw a TypeError exception.
        return agent.throwException(.type_error, "{} is not an Object", .{self});
    }
    const this_value = self.asObject();

    // 2. If SameValue(thisValue, home) is true, then
    if (this_value == home) {
        // a. NOTE: Throwing here emulates assignment to a non-writable data property on the home
        //    object in strict mode code.
        // b. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot set property '{}' on object",
            .{property_key},
        );
    }

    // 3. Let desc be ? thisValue.[[GetOwnProperty]](p).
    const property_descriptor = try this_value.internal_methods.getOwnProperty(
        this_value,
        property_key,
    );

    // 4. If desc is undefined, then
    if (property_descriptor == null) {
        // a. Perform ? CreateDataPropertyOrThrow(thisValue, p, v).
        try this_value.createDataPropertyOrThrow(property_key, value);
    }
    // 5. Else,
    else {
        // a. Perform ? Set(thisValue, p, v, true).
        try this_value.set(property_key, value, .throw);
    }

    // 6. Return unused.
}

/// 9.13 CanBeHeldWeakly ( v )
/// https://tc39.es/ecma262/#sec-canbeheldweakly
pub fn canBeHeldWeakly(self: Value, agent: *Agent) bool {
    // 1. If v is an Object, return true.
    if (self.isObject()) return true;

    // 2. If v is a Symbol and KeyForSymbol(v) is undefined, return true.
    if (self.isSymbol() and keyForSymbol(agent, self.asSymbol()) == null) return true;

    // 3. Return false.
    return false;
}

/// 10.1.15 RequireInternalSlot ( O, internalSlot )
/// https://tc39.es/ecma262/#sec-requireinternalslot
pub fn requireInternalSlot(
    self: Value,
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
    if (!self.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{self});
    }

    // 2. If O does not have an internalSlot internal slot, throw a TypeError exception.
    if (!self.asObject().is(T)) {
        return agent.throwException(.type_error, "{} is not a {s} object", .{ self, name });
    }

    // 3. Return unused.
    // NOTE: Returning the object here allows for direct assignment of the object at the call site.
    return self.asObject().as(T);
}

/// 13.10.2 InstanceofOperator ( V, target )
/// https://tc39.es/ecma262/#sec-instanceofoperator
pub fn instanceofOperator(self: Value, agent: *Agent, target: Value) Agent.Error!bool {
    // 1. If target is not an Object, throw a TypeError exception.
    if (!target.isObject()) {
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
pub fn canonicalizeKeyedCollectionKey(self: Value) Value {
    // 1. If key is -0𝔽, return +0𝔽.
    if (self.isNumber() and self.asNumber().isNegativeZero()) return from(0);

    // 2. Return key.
    return self;
}

/// 27.2.1.6 IsPromise ( x )
/// https://tc39.es/ecma262/#sec-ispromise
pub fn isPromise(self: Value) bool {
    // 1. If x is not an Object, return false.
    if (!self.isObject()) return false;

    // 2. If x does not have a [[PromiseState]] internal slot, return false.
    if (!self.asObject().is(builtins.Promise)) return false;

    // 3. Return true.
    return true;
}

/// Non-standard helper to get the right prototype for a primitive value, if applicable.
pub fn synthesizePrototype(self: Value, agent: *Agent) std.mem.Allocator.Error!?*Object {
    const realm = agent.currentRealm();

    return switch (self.type()) {
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
pub fn toPrivateName(self: Value) ?PrivateName {
    if (!self.isSymbol() or !self.asSymbol().is_private) return null;
    return .{ .symbol = self.asSymbol() };
}

/// 7.1.4.1.1 StringToNumber ( str )
/// https://tc39.es/ecma262/#sec-stringtonumber
pub fn stringToNumber(
    allocator: std.mem.Allocator,
    string: *const String,
) std.mem.Allocator.Error!Number {
    // 1. Let literal be ParseText(str, StringNumericLiteral).
    // 2. If literal is a List of errors, return NaN.
    // 3. Return the StringNumericValue of literal.
    const trimmed_string = try (try string.trim(allocator, .@"start+end")).toUtf8(allocator);
    if (trimmed_string.len == 0) return Number.from(0);
    if (std.mem.eql(u8, trimmed_string, "-Infinity")) return Number.from(-std.math.inf(f64));
    if (std.mem.eql(u8, trimmed_string, "+Infinity")) return Number.from(std.math.inf(f64));
    if (std.mem.eql(u8, trimmed_string, "Infinity")) return Number.from(std.math.inf(f64));
    if (
    // Don't pass other strings starting with "inf" to `std.fmt.parseFloat()`
    std.ascii.startsWithIgnoreCase(trimmed_string, "inf")
    // Don't pass strings with sign and base to `std.fmt.parseInt()`
    or (std.mem.indexOfAny(u8, trimmed_string, "+-") == 0 and
        (std.ascii.startsWithIgnoreCase(trimmed_string[1..], "0b") or
        std.ascii.startsWithIgnoreCase(trimmed_string[1..], "0o") or
        std.ascii.startsWithIgnoreCase(trimmed_string[1..], "0x")))
    // Don't pass strings containing underscores to `std.fmt.parseInt()`
    or std.mem.indexOfScalar(u8, trimmed_string, '_') != null) {
        return Number.from(std.math.nan(f64));
    }
    if (std.fmt.parseFloat(f64, trimmed_string)) |float|
        return Number.from(float)
    else |_| if (std.fmt.parseInt(i32, trimmed_string, 0)) |int|
        return Number.from(int)
    else |_|
        return Number.from(std.math.nan(f64));
}

/// 7.1.14 StringToBigInt ( str )
/// https://tc39.es/ecma262/#sec-stringtobigint
pub fn stringToBigInt(
    allocator: std.mem.Allocator,
    string: *const String,
) std.mem.Allocator.Error!?*const BigInt {
    // 1. Let literal be ParseText(str, StringIntegerLiteral).
    // 2. If literal is a List of errors, return undefined.
    // 3. Let mv be the MV of literal.
    // 4. Assert: mv is an integer.
    // 5. Return ℤ(mv).
    // TODO: Implement the proper string parsing grammar!
    var trimmed_string = try (try string.trim(allocator, .@"start+end")).toUtf8(allocator);
    if (trimmed_string.len == 0) return try types.BigInt.from(allocator, 0);
    // Unlike std.fmt.parseFloat() and std.fmt.parseInt() with base 0, std.math.big.int.Managed.setString()
    // doesn't like the prefix so we have to cut it off manually.
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
    const big_int = try BigInt.from(allocator, 0);
    big_int.managed.setString(
        base,
        trimmed_string,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidCharacter => return null,
        error.InvalidBase => unreachable,
    };
    return big_int;
}

/// 7.2.8 SameType ( x, y )
/// https://tc39.es/ecma262/#sec-sametype
pub fn sameType(x: Value, y: Value) bool {
    // 1. If x is undefined and y is undefined, return true.
    // 2. If x is null and y is null, return true.
    // 3. If x is a Boolean and y is a Boolean, return true.
    // 4. If x is a Number and y is a Number, return true.
    // 5. If x is a BigInt and y is a BigInt, return true.
    // 6. If x is a Symbol and y is a Symbol, return true.
    // 7. If x is a String and y is a String, return true.
    // 8. If x is an Object and y is an Object, return true.
    // 9. Return false.
    return x.type() == y.type();
}

/// 7.2.9 SameValue ( x, y )
/// https://tc39.es/ecma262/#sec-samevalue
pub fn sameValue(x: Value, y: Value) bool {
    // 1. If SameType(x, y) is false, return false.
    if (!sameType(x, y)) return false;

    // 2. If x is a Number, then
    if (x.isNumber()) {
        // a. Return Number::sameValue(x, y).
        return x.asNumber().sameValue(y.asNumber());
    }

    // 3. Return SameValueNonNumber(x, y).
    return sameValueNonNumber(x, y);
}

/// 7.2.10 SameValueZero ( x, y )
/// https://tc39.es/ecma262/#sec-samevaluezero
pub fn sameValueZero(x: Value, y: Value) bool {
    // 1. If SameType(x, y) is false, return false.
    if (!sameType(x, y)) return false;

    // 2. If x is a Number, then
    if (x.isNumber()) {
        // a. Return Number::sameValueZero(x, y).
        return x.asNumber().sameValueZero(y.asNumber());
    }

    // 3. Return SameValueNonNumber(x, y).
    return sameValueNonNumber(x, y);
}

/// 7.2.11 SameValueNonNumber ( x, y )
/// https://tc39.es/ecma262/#sec-samevaluenonnumber
pub fn sameValueNonNumber(x: Value, y: Value) bool {
    // 1. Assert: SameType(x, y) is true.
    std.debug.assert(sameType(x, y));

    return switch (x.type()) {
        // 2. If x is either null or undefined, return true.
        .null, .undefined => true,

        // 3. If x is a BigInt, then
        //     a. Return BigInt::equal(x, y).
        .big_int => x.asBigInt().equal(y.asBigInt()),

        // 4. If x is a String, then
        //     a. If x and y have the same length and the same code units in the same positions,
        //        return true; otherwise, return false.
        .string => x.asString().eql(y.asString()),

        // 5. If x is a Boolean, then
        //     a. If x and y are both true or both false, return true; otherwise, return false.
        .boolean => x.asBoolean() == y.asBoolean(),

        // 6. NOTE: All other ECMAScript language values are compared by identity.
        // 7. If x is y, return true; otherwise, return false.
        .symbol => x.asSymbol() == y.asSymbol(),
        .object => x.asObject() == y.asObject(),

        .number => unreachable,
    };
}

/// 7.2.12 IsLessThan ( x, y, LeftFirst )
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
    if (px.isString() and py.isString()) {
        // a. Let lx be the length of px.
        const lx = px.asString().length();

        // b. Let ly be the length of py.
        const ly = py.asString().length();

        // c. For each integer i such that 0 ≤ i < min(lx, ly), in ascending order, do
        for (0..@min(lx, ly)) |i| {
            // i. Let cx be the numeric value of the code unit at index i within px.
            const cx = px.asString().codeUnitAt(i);

            // ii. Let cy be the numeric value of the code unit at index i within py.
            const cy = py.asString().codeUnitAt(i);

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
        if (px.isBigInt() and py.isString()) {
            //     i. Let ny be StringToBigInt(py).
            //     ii. If ny is undefined, return undefined.
            //     iii. Return BigInt::lessThan(px, ny).
        }

        // b. If px is a String and py is a BigInt, then
        if (px.isString() and py.isBigInt()) {
            //     i. Let nx be StringToBigInt(px).
            //     ii. If nx is undefined, return undefined.
            //     iii. Return BigInt::lessThan(nx, py).
        }

        // c. NOTE: Because px and py are primitive values, evaluation order is not important.

        // d. Let nx be ? ToNumeric(px).
        const nx = try px.toNumeric(agent);

        // e. Let ny be ? ToNumeric(py).
        const ny = try py.toNumeric(agent);

        // f. If SameType(nx, yx) is true, then
        if (Numeric.sameType(nx, ny)) {
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

/// 7.2.13 IsLooselyEqual ( x, y )
/// https://tc39.es/ecma262/#sec-islooselyequal
pub fn isLooselyEqual(agent: *Agent, x: Value, y: Value) Agent.Error!bool {
    // 1. If SameType(x, y) is true, then
    if (sameType(x, y)) {
        // a. Return IsStrictlyEqual(x, y).
        return isStrictlyEqual(x, y);
    }

    // 2. If x is null and y is undefined, return true.
    if (x.isNull() and y.isUndefined()) return true;

    // 3. If x is undefined and y is null, return true.
    if (x.isUndefined() and y.isNull()) return true;

    // B.3.6.2 Changes to IsLooselyEqual
    // https://tc39.es/ecma262/#sec-IsHTMLDDA-internal-slot-aec
    if (build_options.enable_annex_b) {
        // 4. Perform the following steps:
        // a. If x is an Object, x has an [[IsHTMLDDA]] internal slot, and y is either undefined or
        //    null, return true.
        if (x.isObject() and x.asObject().is_htmldda and (y.isUndefined() or y.isNull())) return true;

        // b. If x is either undefined or null, y is an Object, and y has an [[IsHTMLDDA]] internal
        //    slot, return true.
        if ((x.isUndefined() or x.isNull()) and y.isObject() and y.asObject().is_htmldda) return true;
    } else {
        // 4. NOTE: This step is replaced in section B.3.6.2.
    }

    // 5. If x is a Number and y is a String, return ! IsLooselyEqual(x, ! ToNumber(y)).
    if (x.isNumber() and y.isString()) {
        return isLooselyEqual(
            agent,
            x,
            from(y.toNumber(agent) catch unreachable),
        ) catch unreachable;
    }

    // 6. If x is a String and y is a Number, return ! IsLooselyEqual(! ToNumber(x), y).
    if (x.isString() and y.isNumber()) {
        return isLooselyEqual(
            agent,
            from(x.toNumber(agent) catch unreachable),
            y,
        ) catch unreachable;
    }

    // 7. If x is a BigInt and y is a String, then
    if (x.isBigInt() and y.isString()) {
        // a. Let n be StringToBigInt(y).
        const n = try stringToBigInt(agent.gc_allocator, y.asString());

        // b. If n is undefined, return false.
        if (n == null) return false;

        // c. Return ! IsLooselyEqual(x, n).
        return isLooselyEqual(agent, x, from(n.?));
    }

    // 8. If x is a String and y is a BigInt, return ! IsLooselyEqual(y, x).
    if (x.isString() and y.isBigInt()) return isLooselyEqual(agent, y, x) catch unreachable;

    // 9. If x is a Boolean, return ! IsLooselyEqual(! ToNumber(x), y).
    if (x.isBoolean()) {
        return isLooselyEqual(
            agent,
            from(x.toNumber(agent) catch unreachable),
            y,
        ) catch unreachable;
    }

    // 10. If y is a Boolean, return ! IsLooselyEqual(x, ! ToNumber(y)).
    if (y.isBoolean()) {
        return isLooselyEqual(
            agent,
            x,
            from(y.toNumber(agent) catch unreachable),
        ) catch unreachable;
    }

    // 11. If x is either a String, a Number, a BigInt, or a Symbol and y is an Object, return
    //     ! IsLooselyEqual(x, ? ToPrimitive(y)).
    if ((x.isString() or x.isNumber() or x.isBigInt() or x.isSymbol()) and y.isObject()) {
        return isLooselyEqual(agent, x, try y.toPrimitive(agent, null)) catch unreachable;
    }

    // 12. If x is an Object and y is either a String, a Number, a BigInt, or a Symbol, return
    //     ! IsLooselyEqual(? ToPrimitive(x), y).
    if (x.isObject() and (y.isString() or y.isNumber() or y.isBigInt() or y.isSymbol())) {
        return isLooselyEqual(agent, try x.toPrimitive(agent, null), y) catch unreachable;
    }

    // 13. If x is a BigInt and y is a Number, or if x is a Number and y is a BigInt, then
    if ((x.isBigInt() and y.isNumber()) or (x.isNumber() and y.isBigInt())) {
        // a. If x is not finite or y is not finite, return false.
        if ((x.isNumber() and !x.asNumber().isFinite()) or
            (y.isNumber() and !y.asNumber().isFinite())) return false;

        // b. If ℝ(x) = ℝ(y), return true; otherwise return false.
        // TODO: Implement more efficient BigInt to f64 comparison
        if ((x.isNumber() and !x.asNumber().isIntegral()) or
            (y.isNumber() and !y.asNumber().isIntegral())) return false;
        return (try x.toString(agent)).eql(try y.toString(agent));
    }

    // 14. Return false.
    return false;
}

/// 7.2.14 IsStrictlyEqual ( x, y )
/// https://tc39.es/ecma262/#sec-isstrictlyequal
pub fn isStrictlyEqual(x: Value, y: Value) bool {
    // 1. If SameType(x, y) is false, return false.
    if (!sameType(x, y)) return false;

    // 2. If x is a Number, then
    if (x.isNumber()) {
        // a. Return Number::equal(x, y).
        return x.asNumber().equal(y.asNumber());
    }

    // 3. Return SameValueNonNumber(x, y).
    return sameValueNonNumber(x, y);
}

/// 7.3.17 CreateArrayFromList ( elements )
/// https://tc39.es/ecma262/#sec-createarrayfromlist
pub fn createArrayFromList(
    agent: *Agent,
    elements: []const Value,
) std.mem.Allocator.Error!*Object {
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
    mapFn: fn (*Agent, T) std.mem.Allocator.Error!Value,
) std.mem.Allocator.Error!*Object {
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
pub fn coerceOptionsToObject(self: Value, agent: *Agent) Agent.Error!*Object {
    // 1. If options is undefined, then
    if (self.isUndefined()) {
        // a. Return OrdinaryObjectCreate(null).
        return ordinaryObjectCreate(agent, null);
    }

    // 2. Return ? ToObject(options).
    return self.toObject(agent);
}

/// 9.2.13 GetOption ( options, property, type, values, default )
/// https://tc39.es/ecma402/#sec-getoption
pub fn getOption(
    options: *Object,
    comptime property: []const u8,
    comptime type_: enum {
        boolean,
        number,
        string,

        fn T(self: @This()) type {
            return switch (self) {
                .boolean => bool,
                .number => Number,
                .string => *const String,
            };
        }
    },
    comptime values: ?[]const type_.T(),
    comptime default: anytype,
) Agent.Error!if (@TypeOf(default) == @TypeOf(null)) ?type_.T() else type_.T() {
    if (@TypeOf(default) != @TypeOf(null) and @TypeOf(default) != type_.T() and default != .required) {
        @compileError("Invalid value for default parameter");
    }

    const agent = options.agent;

    // 1. Let value be ? Get(options, property).
    const value = try options.get(PropertyKey.from(property));

    // 2. If value is undefined, then
    if (value.isUndefined()) {
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

    const coerced_value = switch (type_) {
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
            if (sameValue(from(coerced_value), from(permitted_value))) break;
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

pub fn ArrayHashMap(comptime V: type, comptime eqlFn: fn (Value, Value) bool) type {
    return std.ArrayHashMap(Value, V, struct {
        pub fn hash(_: @This(), key: Value) u32 {
            const value_hash = switch (key.type()) {
                .undefined, .null => 0,
                .boolean => std.array_hash_map.getAutoHashFn(bool, void)({}, key.asBoolean()),
                .string => @as(u32, @truncate(key.asString().hash)),
                .symbol => std.array_hash_map.getAutoHashStratFn(*const Symbol, void, .Shallow)({}, key.asSymbol()),
                .number => switch (key.asNumber()) {
                    .i32 => |n| std.array_hash_map.getAutoHashFn(i32, void)({}, n),
                    .f64 => |n| std.array_hash_map.getAutoHashFn(i64, void)({}, @bitCast(n)),
                },
                .big_int => std.array_hash_map.getAutoHashStratFn(*const BigInt, void, .Shallow)({}, key.asBigInt()),
                .object => std.array_hash_map.getAutoHashStratFn(*Object, void, .Shallow)({}, key.asObject()),
            };
            const tag: u32 = @intFromEnum(key.type());
            return tag ^ value_hash;
        }

        pub fn eql(_: @This(), a: Value, b: Value, _: usize) bool {
            return eqlFn(a, b);
        }
    }, true);
}

test format {
    const gc = @import("../../gc.zig");
    var agent = try Agent.init(gc.allocator(), .{});
    defer agent.deinit();
    const symbol_without_description: Symbol = .{ .description = null };
    const symbol_with_description: Symbol = .{ .description = String.fromLiteral("foo") };
    var managed = try std.math.big.int.Managed.initSet(std.testing.allocator, 123);
    defer managed.deinit();
    const object = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    const test_cases = [_]struct { Value, []const u8 }{
        .{ @"undefined", "undefined" },
        .{ @"null", "null" },
        .{ from(true), "true" },
        .{ from(false), "false" },
        .{ from("foo"), "\"foo\"" },
        .{ from(&symbol_without_description), "Symbol()" },
        .{ from(&symbol_with_description), "Symbol(\"foo\")" },
        .{ from(try BigInt.from(gc.allocator(), managed)), "123n" },
        .{ from(object), "[object Object]" },
    };
    for (test_cases) |test_case| {
        const value, const expected = test_case;
        const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{value});
        defer std.testing.allocator.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}

test nan {
    try std.testing.expect(std.math.isNan(nan.asNumber().f64));
}

test infinity {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(infinity.asNumber().f64, inf);
}

test negative_infinity {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(negative_infinity.asNumber().f64, -inf);
}

test from {
    const inf = std.math.inf(f64);
    try std.testing.expectEqual(from(true).asBoolean(), true);
    try std.testing.expectEqual(from(false).asBoolean(), false);
    try std.testing.expectEqual(from("").asString().slice.ascii, "");
    try std.testing.expectEqual(from("foo").asString().slice.ascii, "foo");
    try std.testing.expectEqual(from("123").asString().slice.ascii, "123");
    try std.testing.expectEqual(from(0).asNumber().i32, 0);
    try std.testing.expectEqual(from(0.0).asNumber().i32, 0);
    try std.testing.expectEqual(from(123).asNumber().i32, 123);
    try std.testing.expectEqual(from(123.0).asNumber().i32, 123);
    try std.testing.expectEqual(from(123.456).asNumber().f64, 123.456);
    try std.testing.expectEqual(from(std.math.inf(f64)).asNumber().f64, inf);
    try std.testing.expect(std.math.isNan(from(std.math.nan(f64)).asNumber().f64));
}
