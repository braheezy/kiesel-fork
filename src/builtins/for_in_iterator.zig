//! 14.7.5.10 For-In Iterator Objects
//! https://tc39.es/ecma262/#sec-for-in-iterator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;

/// 14.7.5.10.1 CreateForInIterator ( object )
/// https://tc39.es/ecma262/#sec-createforiniterator
pub fn createForInIterator(agent: *Agent, object: *Object) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. let iterator be OrdinaryObjectCreate(%ForInIteratorPrototype%, « [[Object]],
    //    [[ObjectWasVisited]], [[VisitedKeys]], [[RemainingKeys]] »).
    // 6. Return iterator.
    return ForInIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%ForInIteratorPrototype%"(),
        .fields = .{
            .state = .{
                // 2. Set iterator.[[Object]] to object.
                .object = object,

                // 3. Set iterator.[[ObjectWasVisited]] to false.
                .object_was_visited = false,

                // 4. Set iterator.[[VisitedKeys]] to a new empty List.
                .visited_keys = .empty,

                // 5. Set iterator.[[RemainingKeys]] to a new empty List.
                .remaining_keys = .empty,
            },
        },
    });
}

/// 14.7.5.10.2 The %ForInIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%foriniteratorprototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);
    }

    /// 14.7.5.10.2.1 %ForInIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%foriniteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Assert: O is an Object.
        // 3. Assert: O has all of the internal slots of a For-In Iterator instance (14.7.5.10.3).
        std.debug.assert(this_value.isObject());
        std.debug.assert(this_value.asObject().is(ForInIterator));
        const for_in_iterator = this_value.asObject().as(ForInIterator);

        if (for_in_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        // 4. Let object be O.[[Object]].
        var object = for_in_iterator.fields.state.object;

        // 5. Repeat,
        while (true) {
            // a. If O.[[ObjectWasVisited]] is false, then
            if (!for_in_iterator.fields.state.object_was_visited) {
                // i. Let keys be ? object.[[OwnPropertyKeys]]().
                var keys = try object.internal_methods.ownPropertyKeys(agent, object);
                defer keys.deinit(agent.gc_allocator);

                // ii. For each element key of keys, do
                for (keys.items) |key| {
                    // 1. If key is a String, then
                    if (key == .string or key == .integer_index) {
                        // a. Append key to O.[[RemainingKeys]].
                        try for_in_iterator.fields.state.remaining_keys.append(agent.gc_allocator, key);
                    }
                }

                // iii. Set O.[[ObjectWasVisited]] to true.
                for_in_iterator.fields.state.object_was_visited = true;
            }

            // b. Repeat, while O.[[RemainingKeys]] is not empty,
            while (for_in_iterator.fields.state.remaining_keys.items.len != 0) {
                // i. Let r be the first element of O.[[RemainingKeys]].
                // ii. Remove the first element from O.[[RemainingKeys]].
                const remaining_key = for_in_iterator.fields.state.remaining_keys.orderedRemove(0);

                // iii. If O.[[VisitedKeys]] does not contain r, then
                if (!for_in_iterator.fields.state.visited_keys.contains(remaining_key)) {
                    // 1. Let desc be ? object.[[GetOwnProperty]](r).
                    const descriptor = try object.internal_methods.getOwnProperty(
                        agent,
                        object,
                        remaining_key,
                    );

                    // 2. If desc is not undefined, then
                    if (descriptor != null) {
                        // a. Append r to O.[[VisitedKeys]].
                        try for_in_iterator.fields.state.visited_keys.putNoClobber(
                            agent.gc_allocator,
                            remaining_key,
                            {},
                        );

                        // b. If desc.[[Enumerable]] is true, return CreateIteratorResultObject(r, false).
                        if (descriptor.?.enumerable == true) {
                            return Value.from(
                                try createIteratorResultObject(
                                    agent,
                                    try remaining_key.toValue(agent),
                                    false,
                                ),
                            );
                        }
                    }
                }
            }

            // c. Set object to ? object.[[GetPrototypeOf]]().
            object = (try object.internal_methods.getPrototypeOf(agent, object)) orelse {
                // f. If object is null, return CreateIteratorResultObject(undefined, true).
                for_in_iterator.fields = .completed;
                return Value.from(try createIteratorResultObject(agent, .undefined, true));
            };

            // d. Set O.[[Object]] to object.
            for_in_iterator.fields.state.object = object;

            // e. Set O.[[ObjectWasVisited]] to false.
            for_in_iterator.fields.state.object_was_visited = false;
        }
    }
};

/// 14.7.5.10.3 Properties of For-In Iterator Instances
/// https://tc39.es/ecma262/#sec-properties-of-for-in-iterator-instances
pub const ForInIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            /// [[Object]]
            object: *Object,

            /// [[ObjectWasVisited]]
            object_was_visited: bool,

            /// [[VisitedKeys]]
            visited_keys: PropertyKey.ArrayHashMapUnmanaged(void),

            /// [[RemainingKeys]]
            remaining_keys: std.ArrayListUnmanaged(PropertyKey),
        },
        completed,
    },
    .tag = .for_in_iterator,
});
