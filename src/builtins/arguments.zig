//! 10.4.4 Arguments Exotic Objects
//! https://tc39.es/ecma262/#sec-arguments-exotic-objects

const std = @import("std");

const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;
const noexcept = utils.noexcept;

/// 10.4.4.6 CreateUnmappedArgumentsObject ( argumentsList )
/// https://tc39.es/ecma262/#sec-createunmappedargumentsobject
pub fn createUnmappedArgumentsObject(agent: *Agent, arguments_list: []const Value) !Object {
    const realm = agent.currentRealm();

    // 1. Let len be the number of elements in argumentsList.
    const len = arguments_list.len;

    // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%, « [[ParameterMap]] »).
    // 3. Set obj.[[ParameterMap]] to undefined.
    const object = try Arguments.create(agent, .{
        .prototype = try realm.intrinsics.@"%Object.prototype%"(),
    });

    // 4. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor {
    //      [[Value]]: 𝔽(len), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }).
    object.createDataPropertyOrThrow(
        PropertyKey.from("length"),
        Value.from(len),
    ) catch |err| try noexcept(err);

    // 5. Let index be 0.
    // 6. Repeat, while index < len,
    for (arguments_list, 0..) |value, index| {
        // a. Let val be argumentsList[index].
        const property_key = if (index <= std.math.maxInt(PropertyKey.IntegerIndex))
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index)))
        else
            PropertyKey.from(try std.fmt.allocPrint(agent.gc_allocator, "{}", .{index}));

        // b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
        object.createDataPropertyOrThrow(property_key, value) catch |err| try noexcept(err);

        // c. Set index to index + 1.
    }

    // TODO: 7. Perform ! DefinePropertyOrThrow(obj, @@iterator, PropertyDescriptor {
    //      [[Value]]: %Array.prototype.values%, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }).

    // 8. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor {
    //      [[Get]]: %ThrowTypeError%, [[Set]]: %ThrowTypeError%, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    object.definePropertyOrThrow(PropertyKey.from("callee"), .{
        .get = try realm.intrinsics.@"%ThrowTypeError%"(),
        .set = try realm.intrinsics.@"%ThrowTypeError%"(),
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 9. Return obj.
    return object;
}

pub const Arguments = Object.Factory(.{
    .tag = .arguments,
});
