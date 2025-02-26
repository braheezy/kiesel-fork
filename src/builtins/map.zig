//! 24.1 Map Objects
//! https://tc39.es/ecma262/#sec-map-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createMapIterator = builtins.createMapIterator;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

/// 24.1.1.2 AddEntriesFromIterable ( target, iterable, adder )
/// https://tc39.es/ecma262/#sec-add-entries-from-iterable
pub fn addEntriesFromIterable(
    agent: *Agent,
    target: *Object,
    iterable: Value,
    adder: *Object,
) Agent.Error!*Object {
    // 1. Let iteratorRecord be ? GetIterator(iterable, sync).
    var iterator = try getIterator(agent, iterable, .sync);

    // 2. Repeat,
    //     a. Let next be ? IteratorStepValue(iteratorRecord).
    //     b. If next is done, return target.
    while (try iterator.stepValue(agent)) |next| {
        // c. If next is not an Object, then
        if (!next.isObject()) {
            // i. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "Iterable must return object items",
                .{},
            );

            // ii. Return ? IteratorClose(iteratorRecord, error).
            return iterator.close(agent, @as(Agent.Error!*Object, @"error"));
        }

        // d. Let k be Completion(Get(next, "0")).
        const k = next.asObject().get(PropertyKey.from(0)) catch |err| {
            // e. IfAbruptCloseIterator(k, iteratorRecord).
            return iterator.close(agent, @as(Agent.Error!*Object, err));
        };

        // f. Let v be Completion(Get(next, "1")).
        const v = next.asObject().get(PropertyKey.from(1)) catch |err| {
            // h. IfAbruptCloseIterator(v, iteratorRecord).
            return iterator.close(agent, @as(Agent.Error!*Object, err));
        };

        // h. Let status be Completion(Call(adder, target, « k, v »)).
        _ = Value.from(adder).callAssumeCallable(Value.from(target), &.{ k, v }) catch |err| {
            // i. IfAbruptCloseIterator(status, iteratorRecord).
            return iterator.close(agent, @as(Agent.Error!*Object, err));
        };
    }

    return target;
}

/// 24.1.2 Properties of the Map Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-map-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 0,
            .name = "Map",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "groupBy", groupBy, 2, realm);
        try defineBuiltinAccessor(object, "%Symbol.species%", @"%Symbol.species%", null, realm);

        // 24.1.2.2 Map.prototype
        // https://tc39.es/ecma262/#sec-map.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Map.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 24.1.1.1 Map ( [ iterable ] )
    /// https://tc39.es/ecma262/#sec-map-iterable
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const iterable = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Map must be constructed with 'new'", .{});
        }

        // 2. Let map be ? OrdinaryCreateFromConstructor(NewTarget, "%Map.prototype%", « [[MapData]] »).
        const map = try ordinaryCreateFromConstructor(
            Map,
            agent,
            new_target.?,
            "%Map.prototype%",
            .{
                // 3. Set map.[[MapData]] to a new empty List.
                .map_data = .empty,
            },
        );

        // 4. If iterable is either undefined or null, return map.
        if (iterable.isUndefined() or iterable.isNull()) return Value.from(map);

        // 5. Let adder be ? Get(map, "set").
        const adder = try map.get(PropertyKey.from("set"));

        // 6. If IsCallable(adder) is false, throw a TypeError exception.
        if (!adder.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{adder});
        }

        // 7. Return ? AddEntriesFromIterable(map, iterable, adder).
        return Value.from(try addEntriesFromIterable(agent, map, iterable, adder.asObject()));
    }

    /// 24.1.2.1 Map.groupBy ( items, callback )
    /// https://tc39.es/ecma262/#sec-map.groupby
    fn groupBy(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const items = arguments.get(0);
        const callback = arguments.get(1);

        // 1. Let groups be ? GroupBy(items, callback, collection).
        const groups = try items.groupBy(agent, callback, .collection);

        // 2. Let map be ! Construct(%Map%).
        const map = (try realm.intrinsics.@"%Map%"()).constructNoArgs() catch |err| try noexcept(err);

        // 3. For each Record { [[Key]], [[Elements]] } g of groups, do
        var it = groups.iterator();
        while (it.next()) |entry| {
            // a. Let elements be CreateArrayFromList(g.[[Elements]]).
            const elements = try createArrayFromList(agent, entry.value_ptr.items);

            // b. Let entry be the Record { [[Key]]: g.[[Key]], [[Value]]: elements }.
            // c. Append entry to map.[[MapData]].
            try map.as(Map).fields.map_data.putNoClobber(agent.gc_allocator, entry.key_ptr.*, Value.from(elements));
        }

        // 4. Return map.
        return Value.from(map);
    }

    /// 24.1.2.3 get Map [ %Symbol.species% ]
    /// https://tc39.es/ecma262/#sec-get-map-%symbol.species%
    fn @"%Symbol.species%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 24.1.3 Properties of the Map Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-map-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "clear", clear, 0, realm);
        try defineBuiltinFunction(object, "delete", delete, 1, realm);
        try defineBuiltinFunction(object, "entries", entries, 0, realm);
        try defineBuiltinFunction(object, "forEach", forEach, 1, realm);
        try defineBuiltinFunction(object, "get", get, 1, realm);
        try defineBuiltinFunction(object, "has", has, 1, realm);
        try defineBuiltinFunction(object, "keys", keys, 0, realm);
        try defineBuiltinFunction(object, "set", set, 2, realm);
        try defineBuiltinAccessor(object, "size", size, null, realm);
        try defineBuiltinFunction(object, "values", values, 0, realm);

        // 24.1.3.2 Map.prototype.constructor
        // https://tc39.es/ecma262/#sec-map.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%Map%"()),
        );

        // 24.1.3.12 Map.prototype [ %Symbol.iterator% ] ( )
        // https://tc39.es/ecma262/#sec-map.prototype-%symbol.iterator%
        const @"%Map.prototype.entries%" = object.getPropertyValueDirect(PropertyKey.from("entries"));
        try defineBuiltinProperty(object, "%Symbol.iterator%", @"%Map.prototype.entries%");

        // 24.1.3.13 Map.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-map.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Map"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 24.1.3.1 Map.prototype.clear ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.clear
    fn clear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. Set p.[[Key]] to empty.
        //     b. Set p.[[Value]] to empty.
        map.fields.map_data.clearAndFree(agent.gc_allocator);
        if (map.fields.iterable_keys) |*iterable_keys| {
            iterable_keys.clearAndFree(agent.gc_allocator);
        }

        // 4. Return undefined.
        return .undefined;
    }

    /// 24.1.3.3 Map.prototype.delete ( key )
    /// https://tc39.es/ecma262/#sec-map.prototype.delete
    fn delete(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. Set key to CanonicalizeKeyedCollectionKey(key).
        key = key.canonicalizeKeyedCollectionKey();

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, then
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

        // 5. Return false.
        return Value.from(false);
    }

    /// 24.1.3.4 Map.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateMapIterator(M, key+value).
        return Value.from(try createMapIterator(agent, map, .@"key+value"));
    }

    /// 24.1.3.5 Map.prototype.forEach ( callback [ , thisArg ] )
    /// https://tc39.es/ecma262/#sec-map.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const callback = arguments.get(0);
        const this_arg = arguments.get(1);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. If IsCallable(callback) is false, throw a TypeError exception.
        if (!callback.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{callback});
        }

        const iterable_keys = try map.fields.registerIterator(agent.gc_allocator);
        defer map.fields.unregisterIterator(agent.gc_allocator);

        // 4. Let entries be M.[[MapData]].
        const entries_ = &map.fields.map_data;

        // 5. Let numEntries be the number of elements in entries.
        var num_entries = iterable_keys.items.len;

        // 6. Let index be 0.
        var index: usize = 0;

        // 7. Repeat, while index < numEntries,
        while (index < num_entries) : (index += 1) {
            // a. Let e be entries[index].
            // b. Set index to index + 1.
            // c. If e.[[Key]] is not empty, then
            if (iterable_keys.items[index]) |key| {
                const value = entries_.get(key).?;

                // i. Perform ? Call(callback, thisArg, « e.[[Value]], e.[[Key]], M »).
                _ = try callback.callAssumeCallable(
                    this_arg,
                    &.{ value, key, Value.from(&map.object) },
                );

                // ii. NOTE: The number of elements in entries may have increased during execution
                //     of callback.
                // iii. Set numEntries to the number of elements in entries.
                num_entries = iterable_keys.items.len;
            }
        }

        // 8. Return undefined.
        return .undefined;
    }

    /// 24.1.3.6 Map.prototype.get ( key )
    /// https://tc39.es/ecma262/#sec-map.prototype.get
    fn get(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. Set key to CanonicalizeKeyedCollectionKey(key).
        key = key.canonicalizeKeyedCollectionKey();

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, return p.[[Value]].
        if (map.fields.map_data.get(key)) |value| return value;

        // 5. Return undefined.
        return .undefined;
    }

    /// 24.1.3.7 Map.prototype.has ( key )
    /// https://tc39.es/ecma262/#sec-map.prototype.has
    fn has(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. Set key to CanonicalizeKeyedCollectionKey(key).
        key = key.canonicalizeKeyedCollectionKey();

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, return true.
        // 5. Return false.
        return Value.from(map.fields.map_data.contains(key));
    }

    /// 24.1.3.8 Map.prototype.keys ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.keys
    fn keys(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateMapIterator(M, key).
        return Value.from(try createMapIterator(agent, map, .key));
    }

    /// 24.1.3.9 Map.prototype.set ( key, value )
    /// https://tc39.es/ecma262/#sec-map.prototype.set
    fn set(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        var key = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. Set key to CanonicalizeKeyedCollectionKey(key).
        key = key.canonicalizeKeyedCollectionKey();

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, then
        //         i. Set p.[[Value]] to value.
        //         ii. Return M.
        // 5. Let p be the Record { [[Key]]: key, [[Value]]: value }.
        // 6. Append p to M.[[MapData]].
        const result = try map.fields.map_data.getOrPut(agent.gc_allocator, key);
        result.value_ptr.* = value;
        if (!result.found_existing) {
            if (map.fields.iterable_keys) |*iterable_keys| {
                try iterable_keys.append(agent.gc_allocator, key);
            }
        }

        // 7. Return M.
        return Value.from(&map.object);
    }

    /// 24.1.3.10 get Map.prototype.size
    /// https://tc39.es/ecma262/#sec-get-map.prototype.size
    fn size(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
        const map = try this_value.requireInternalSlot(agent, Map);

        // 3. Let count be 0.
        // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
        //     a. If p.[[Key]] is not empty, set count to count + 1.
        const count = map.fields.map_data.count();

        // 5. Return 𝔽(count).
        return Value.from(@as(u53, @intCast(count)));
    }

    /// 24.1.3.11 Map.prototype.values ( )
    /// https://tc39.es/ecma262/#sec-map.prototype.values
    fn values(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateMapIterator(M, value).
        return Value.from(try createMapIterator(agent, map, .value));
    }
};

const MapData = Value.ArrayHashMapUnmanaged(Value, sameValue);
const IterableKeys = std.ArrayListUnmanaged(?Value);

/// 24.1.4 Properties of Map Instances
/// https://tc39.es/ecma262/#sec-properties-of-map-instances
pub const Map = MakeObject(.{
    .Fields = struct {
        /// [[MapData]]
        map_data: MapData,

        /// List of keys and their deletion status for MapIterator and Map.prototype.forEach(),
        /// created and destroyed on demand.
        iterable_keys: ?IterableKeys = null,
        active_iterators: usize = 0,

        pub fn registerIterator(
            self: *@This(),
            allocator: std.mem.Allocator,
        ) std.mem.Allocator.Error!*IterableKeys {
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_keys == null);
                self.iterable_keys = try .initCapacity(allocator, self.map_data.count());
                for (self.map_data.keys()) |key| {
                    self.iterable_keys.?.appendAssumeCapacity(key);
                }
            }
            self.active_iterators += 1;
            return &self.iterable_keys.?;
        }

        pub fn unregisterIterator(self: *@This(), allocator: std.mem.Allocator) void {
            self.active_iterators -= 1;
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_keys != null);
                self.iterable_keys.?.deinit(allocator);
                self.iterable_keys = null;
            }
        }
    },
    .tag = .map,
});
