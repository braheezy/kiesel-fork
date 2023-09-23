//! 24.1 Map Objects
//! https://tc39.es/ecma262/#sec-map-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Iterator = types.Iterator;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const ValueHashMap = types.ValueHashMap;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValueZero = types.sameValueZero;

/// 24.1.1.2 AddEntriesFromIterable ( target, iterable, adder )
/// https://tc39.es/ecma262/#sec-add-entries-from-iterable
pub fn addEntriesFromIterable(
    agent: *Agent,
    target: Object,
    iterable: Value,
    adder: Object,
) !Object {
    // 1. Let iteratorRecord be ? GetIterator(iterable, sync).
    const iterator = try getIterator(agent, iterable, .sync);

    // 2. Repeat,
    while (try iterator.step()) |next| {
        // a. Let next be ? IteratorStep(iteratorRecord).
        // b. If next is false, return target.

        // c. Let nextItem be ? IteratorValue(next).
        const next_item = try Iterator.value(next);

        // d. If nextItem is not an Object, then
        if (next_item != .object) {
            // i. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "Iterable must return object items",
            );

            // ii. Return ? IteratorClose(iteratorRecord, error).
            return iterator.close(@as(Agent.Error!Object, @"error"));
        }

        // e. Let k be Completion(Get(nextItem, "0")).
        const k = next_item.object.get(PropertyKey.from(0)) catch |err| {
            // f. IfAbruptCloseIterator(k, iteratorRecord).
            return iterator.close(@as(Agent.Error!Object, err));
        };

        // g. Let v be Completion(Get(nextItem, "1")).
        const v = next_item.object.get(PropertyKey.from(1)) catch |err| {
            // h. IfAbruptCloseIterator(v, iteratorRecord).
            return iterator.close(@as(Agent.Error!Object, err));
        };

        // i. Let status be Completion(Call(adder, target, « k, v »)).
        _ = Value.from(adder).callAssumeCallable(Value.from(target), .{ k, v }) catch |err| {
            // j. IfAbruptCloseIterator(status, iteratorRecord).
            return iterator.close(@as(Agent.Error!Object, err));
        };
    }

    return target;
}

/// 24.1.2 Properties of the Map Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-map-constructor
pub const MapConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 0,
            .name = "Map",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 24.1.2.1 Map.prototype
        // https://tc39.es/ecma262/#sec-map.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Map.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 24.1.2.2 get Map [ @@species ]
        // https://tc39.es/ecma262/#sec-get-map-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        // 24.1.3.2 Map.prototype.constructor
        // https://tc39.es/ecma262/#sec-map.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Map.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 24.1.1.1 Map ( [ iterable ] )
    /// https://tc39.es/ecma262/#sec-map-iterable
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const iterable = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Map must be constructed with 'new'");
        }

        // 2. Let map be ? OrdinaryCreateFromConstructor(NewTarget, "%Map.prototype%", « [[MapData]] »).
        const map = try ordinaryCreateFromConstructor(Map, agent, new_target.?, "%Map.prototype%");

        // 3. Set map.[[MapData]] to a new empty List.
        map.as(Map).fields = .{ .map_data = MapData.init(agent.gc_allocator) };

        // 4. If iterable is either undefined or null, return map.
        if (iterable == .undefined or iterable == .null) return Value.from(map);

        // 5. Let adder be ? Get(map, "set").
        const adder = try map.get(PropertyKey.from("set"));

        // 6. If IsCallable(adder) is false, throw a TypeError exception.
        if (!adder.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{adder}),
            );
        }

        // 7. Return ? AddEntriesFromIterable(map, iterable, adder).
        return Value.from(try addEntriesFromIterable(agent, map, iterable, adder.object));
    }
};

/// 24.1.3 Properties of the Map Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-map-prototype-object
pub const MapPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 24.1.3.13 Map.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-map.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Map"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

pub const MapData = ValueHashMap(Value);

/// 24.1.4 Properties of Map Instances
/// https://tc39.es/ecma262/#sec-properties-of-map-instances
pub const Map = Object.Factory(.{
    .Fields = struct {
        /// [[MapData]]
        map_data: MapData,
    },
    .tag = .map,
});
