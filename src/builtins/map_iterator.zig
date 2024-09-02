//! 24.1.5 Map Iterator Objects
//! https://tc39.es/ecma262/#sec-map-iterator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Map = builtins.Map;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 24.1.5.1 CreateMapIterator ( map, kind )
/// https://tc39.es/ecma262/#sec-createmapiterator
pub fn createMapIterator(
    agent: *Agent,
    map_value: Value,
    comptime kind: Object.PropertyKind,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Perform ? RequireInternalSlot(map, [[MapData]]).
    const map = try map_value.requireInternalSlot(agent, Map);

    // 2. Let closure be a new Abstract Closure with no parameters that captures map and kind and
    //    performs the following steps when called:
    //    [...]
    // 3. Return CreateIteratorFromClosure(closure, "%MapIteratorPrototype%", %MapIteratorPrototype%).
    return MapIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%MapIteratorPrototype%"(),
        .fields = .{ .state = .{ .map = map, .kind = kind, .index = 0 } },
    });
}

/// 24.1.5.2 The %MapIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%mapiteratorprototype%-object
pub const MapIteratorPrototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 23.1.5.2.2 %ArrayIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Map Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 24.1.5.2.1 %MapIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%mapiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, empty, "%MapIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateMapIterator closure. State is kept track of through the MapIterator
        //       instance instead of as local variables. This should not be observable.

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (!this_value.isObject() or !this_value.asObject().is(MapIterator)) {
            return agent.throwException(.type_error, "This value must be a Map Iterator", .{});
        }
        const map_iterator = this_value.asObject().as(MapIterator);

        // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
        if (map_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        const map = map_iterator.fields.state.map;
        const kind = map_iterator.fields.state.kind;
        var index = map_iterator.fields.state.index;

        if (index == 0) _ = try map.fields.registerIterator();
        const iterable_keys = &map.fields.iterable_keys.?;

        // a. Let entries be map.[[MapData]].
        const entries = &map.fields.map_data;

        // b. Let index be 0.

        // c. Let numEntries be the number of elements in entries.
        const num_entries = iterable_keys.items.len;

        // d. Repeat, while index < numEntries,
        const entry = while (index < num_entries) : (index += 1) {
            // i. Let e be entries[index].
            // ii. Set index to index + 1.
            // iii. If e.[[Key]] is not empty, then
            if (iterable_keys.items[index]) |key| {
                const value = entries.get(key).?;
                index += 1;
                break .{ key, value };
            }
        }
        // e. Return undefined.
        else {
            map_iterator.fields = .completed;
            map.fields.unregisterIterator();
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        };

        map_iterator.fields.state.index = index;
        const key = entry.@"0";
        const value = entry.@"1";

        const result = switch (kind) {
            // 1. If kind is key, then
            //     a. Let result be e.[[Key]].
            .key => key,

            // 2. Else if kind is value, then
            //     a. Let result be e.[[Value]].
            .value => value,

            // 3. Else,
            //     a. Assert: kind is key+value.
            //     b. Let result be CreateArrayFromList(« e.[[Key]], e.[[Value]] »).
            .@"key+value" => Value.from(try createArrayFromList(agent, &.{ key, value })),
        };

        // 4. Perform ? GeneratorYield(CreateIteratorResultObject(result, false)).
        return Value.from(try createIteratorResultObject(agent, result, false));

        // 5. NOTE: The number of elements in entries may have increased while execution of this
        //    abstract operation was paused by Yield.
        // 6. Set numEntries to the number of elements in entries.
    }
};

pub const MapIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            map: *Map,
            kind: Object.PropertyKind,
            index: usize,
        },
        completed,
    },
    .tag = .map_iterator,
});
