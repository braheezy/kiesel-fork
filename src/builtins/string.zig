//! 22.1 String Objects
//! https://tc39.es/ecma262/#sec-string-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
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
    const result_str = try agent.gc_allocator.dupe(u8, str.value[index .. index + 1]);

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

        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);

        return object;
    }

    /// https://tc39.es/ecma262/#thisstringvalue
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
        );
    }

    /// 22.1.3.29 String.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? thisStringValue(this value).
        return Value.from(try thisStringValue(agent, this_value));
    }

    /// 22.1.3.35 String.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-string.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? thisStringValue(this value).
        return Value.from(try thisStringValue(agent, this_value));
    }
};

/// 22.1.4 Properties of String Instances
/// https://tc39.es/ecma262/#sec-properties-of-string-instances
pub const String = Object.Factory(.{
    .Fields = struct {
        /// [[StringData]]
        string_data: types.String,
    },
    .tag = .string,
});
