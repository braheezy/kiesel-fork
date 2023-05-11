//! 10.2 ECMAScript Function Objects
//! https://tc39.es/ecma262/#sec-ecmascript-function-objects

const std = @import("std");

const builtin_function = @import("builtin_function.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const BuiltinFunction = builtin_function.BuiltinFunction;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const noexcept = utils.noexcept;

/// 10.2.4 AddRestrictedFunctionProperties ( F, realm )
/// https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
pub fn addRestrictedFunctionProperties(function: Object, realm: *Realm) !void {
    // 1. Assert: realm.[[Intrinsics]].[[%ThrowTypeError%]] exists and has been initialized.
    // 2. Let thrower be realm.[[Intrinsics]].[[%ThrowTypeError%]].
    const thrower = try realm.intrinsics.@"%ThrowTypeError%"();

    const property_descriptor = PropertyDescriptor{
        .get = thrower,
        .set = thrower,
        .enumerable = false,
        .configurable = true,
    };

    // 3. Perform ! DefinePropertyOrThrow(F, "caller", PropertyDescriptor {
    //      [[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(
        PropertyKey.from("caller"),
        property_descriptor,
    ) catch |err| try noexcept(err);

    // 4. Perform ! DefinePropertyOrThrow(F, "arguments", PropertyDescriptor {
    //      [[Get]]: thrower, [[Set]]: thrower, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(
        PropertyKey.from("arguments"),
        property_descriptor,
    ) catch |err| try noexcept(err);

    // 5. Return unused.
}

/// 10.2.9 SetFunctionName ( F, name [ , prefix ] )
/// https://tc39.es/ecma262/#sec-setfunctionname
pub fn setFunctionName(
    function: Object,
    name_property_key: PropertyKey,
    prefix: ?[]const u8,
) !void {
    const agent = function.agent();

    // 1. Assert: F is an extensible object that does not have a "name" own property.
    std.debug.assert(
        function.extensible().* and !function.propertyStorage().has(PropertyKey.from("name")),
    );

    var name = switch (name_property_key) {
        .string => |string| string,
        .integer_index => |integer_index| try std.fmt.allocPrint(
            agent.allocator,
            "{d}",
            .{integer_index},
        ),

        // 2. If name is a Symbol, then
        .symbol => |symbol| blk: {
            // a. Let description be name's [[Description]] value.
            const description = symbol.description;

            // b. If description is undefined, set name to the empty String.
            if (description == null)
                break :blk "";

            // c. Else, set name to the string-concatenation of "[", description, and "]".
            break :blk try std.fmt.allocPrint(agent.allocator, "[{s}]", .{description.?});
        },

        // TODO: 3. Else if name is a Private Name, then
        //     a. Set name to name.[[Description]].
    };

    // 4. If F has an [[InitialName]] internal slot, then
    //     a. Set F.[[InitialName]] to name.
    function.as(BuiltinFunction).fields.initial_name = name;

    // 5. If prefix is present, then
    if (prefix != null) {
        // a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and
        //    name.
        name = try std.fmt.allocPrint(agent.allocator, "{s} {s}", .{ prefix.?, name });

        // b. If F has an [[InitialName]] internal slot, then
        //     i. Optionally, set F.[[InitialName]] to name.
        function.as(BuiltinFunction).fields.initial_name = name;
    }

    // 6. Perform ! DefinePropertyOrThrow(F, "name", PropertyDescriptor {
    //      [[Value]]: name, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(PropertyKey.from("name"), PropertyDescriptor{
        .value = Value.from(name),
        .writable = false,
        .enumerable = false,
        .configurable = true,
    }) catch |err| try noexcept(err);

    // 7. Return unused.
}

/// 10.2.10 SetFunctionLength ( F, length )
/// https://tc39.es/ecma262/#sec-setfunctionlength
pub fn setFunctionLength(function: Object, length: f64) !void {
    std.debug.assert(
        std.math.isPositiveInf(length) or
            (std.math.isFinite(length) and std.math.trunc(length) == length and length >= 0),
    );

    // 1. Assert: F is an extensible object that does not have a "length" own property.
    std.debug.assert(
        function.extensible().* and !function.propertyStorage().has(PropertyKey.from("length")),
    );

    // 2. Perform ! DefinePropertyOrThrow(F, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(length), [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    function.definePropertyOrThrow(PropertyKey.from("length"), PropertyDescriptor{
        .value = Value.from(length),
        .writable = false,
        .enumerable = false,
        .configurable = true,
    }) catch |err| try noexcept(err);

    // 3. Return unused.
}
