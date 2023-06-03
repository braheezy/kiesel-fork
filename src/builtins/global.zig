//! 19 The Global Object
//! https://tc39.es/ecma262/#sec-global-object

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const performEval = @import("eval.zig").performEval;

const Self = @This();

const NameAndPropertyDescriptor = struct {
    []const u8,
    PropertyDescriptor,
};

pub fn globalObjectProperties(realm: *Realm) ![10]NameAndPropertyDescriptor {
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

        // 19.3.7 Boolean ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-boolean
        .{ "Boolean", .{ .value = Value.from(try realm.intrinsics.@"%Boolean%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.15 Function ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-function
        .{ "Function", .{ .value = Value.from(try realm.intrinsics.@"%Function%"()), .writable = true, .enumerable = false, .configurable = true } },

        // 19.3.21 Object ( . . . )
        // https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object-object
        .{ "Object", .{ .value = Value.from(try realm.intrinsics.@"%Object%"()), .writable = true, .enumerable = false, .configurable = true } },
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
