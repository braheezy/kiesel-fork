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
const createMapIterator = builtins.createMapIterator;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
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

        try defineBuiltinFunction(object, "clear", clear, 0, realm);
        try defineBuiltinFunction(object, "delete", delete, 1, realm);
        try defineBuiltinFunction(object, "entries", entries, 0, realm);
        try defineBuiltinFunction(object, "get", get, 1, realm);
        try defineBuiltinFunction(object, "has", has, 1, realm);
        try defineBuiltinFunction(object, "keys", keys, 0, realm);
        try defineBuiltinFunction(object, "set", set, 2, realm);
        try defineBuiltinAccessor(object, "size", size, null, realm);
        try defineBuiltinFunction(object, "values", values, 0, realm);

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

    /// 24.1.3.1 Map.prototype.clear ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.clear
    fn clear(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. Set p.[[Key]] to empty.
        //     b. Set p.[[Value]] to empty.
        map.fields.map_data.clearAndFree();
        if (map.fields.iterable_keys) |*iterable_keys| {
            iterable_keys.clearAndFree();
        }

        // 4. Return undefined.
        return .undefined;
    }

    /// 24.1.3.3 Map.prototype.delete ( key )
    /// https://tc39.es/ecma262/#sec-map.prototype.delete
    fn delete(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, then
        //         i. Set p.[[Key]] to empty.
        //         ii. Set p.[[Value]] to empty.
        //         iii. Return true.
        if (map.fields.map_data.getIndex(key)) |index| {
            map.fields.map_data.orderedRemoveAt(index);
            if (map.fields.iterable_keys) |*iterable_keys| {
                iterable_keys.items[index] = null;
            }
            return Value.from(true);
        }

        // 4. Return false.
        return Value.from(false);
    }

    /// 24.1.3.4 Map.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateMapIterator(M, key+value).
        return Value.from(try createMapIterator(agent, map, .@"key+value"));
    }

    /// 24.1.3.6 Map.prototype.get ( key )
    /// https://tc39.es/ecma262/#sec-map.prototype.get
    fn get(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return p.[[Value]].
        if (map.fields.map_data.get(key)) |value| return value;

        // 4. Return undefined.
        return .undefined;
    }

    /// 24.1.3.7 Map.prototype.has ( key )
    /// https://tc39.es/ecma262/#sec-map.prototype.has
    fn has(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, return true.
        // 4. Return false.
        return Value.from(map.fields.map_data.contains(key));
    }

    /// 24.1.3.8 Map.prototype.keys ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.keys
    fn keys(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateMapIterator(M, key).
        return Value.from(try createMapIterator(agent, map, .key));
    }

    /// 24.1.3.9 Map.prototype.set ( key, value )
    /// https://tc39.es/ecma262/#sec-map.prototype.set
    fn set(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        var key = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValueZero(p.[[Key]], key) is true, then
        //         i. Set p.[[Value]] to value.
        //         ii. Return M.

        // 4. If key is -0𝔽, set key to +0𝔽.
        if (key == .number and key.number.isNegativeZero()) key = Value.from(0);

        // 5. Let p be the Record { [[Key]]: key, [[Value]]: value }.
        // 6. Append p to M.[[MapData]].
        const result = try map.fields.map_data.getOrPut(key);
        result.value_ptr.* = value;
        if (!result.found_existing) {
            if (map.fields.iterable_keys) |*iterable_keys| {
                try iterable_keys.append(key);
            }
        }

        // 7. Return M.
        return Value.from(map.object());
    }

    /// 24.1.3.10 get Map.prototype.size
    /// https://tc39.es/ecma262/#sec-get-map.prototype.size
    fn size(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. Let count be 0.
        // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty, set count to count + 1.
        // 5. Return 𝔽(count).
        return Value.from(map.fields.map_data.count());
    }

    /// 24.1.3.11 Map.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.values
    fn values(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateMapIterator(M, value).
        return Value.from(try createMapIterator(agent, map, .value));
    }
};

const MapData = ValueHashMap(Value);
const IterableKeys = std.ArrayList(?Value);

/// 24.1.4 Properties of Map Instances
/// https://tc39.es/ecma262/#sec-properties-of-map-instances
pub const Map = Object.Factory(.{
    .Fields = struct {
        const Self = @This();

        /// [[MapData]]
        map_data: MapData,

        /// List of keys and their deletion status for MapIterator and Map.prototype.forEach(),
        /// created and destroyed on demand.
        iterable_keys: ?IterableKeys = null,
        active_iterators: usize = 0,

        pub fn registerIterator(self: *Self) !*IterableKeys {
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_keys == null);
                self.iterable_keys = try IterableKeys.initCapacity(self.map_data.allocator, self.map_data.count());
                for (self.map_data.keys()) |key| {
                    self.iterable_keys.?.appendAssumeCapacity(key);
                }
            }
            self.active_iterators += 1;
            return &self.iterable_keys.?;
        }

        pub fn unregisterIterator(self: *Self) void {
            self.active_iterators -= 1;
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_keys != null);
                self.iterable_keys.?.deinit();
                self.iterable_keys = null;
            }
        }
    },
    .tag = .map,
});
