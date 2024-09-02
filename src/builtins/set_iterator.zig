//! 24.2.5 Set Iterator Objects
//! https://tc39.es/ecma262/#sec-set-iterator-objects

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
const Realm = execution.Realm;
const Set = builtins.Set;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 24.2.5.1 CreateSetIterator ( set, kind )
/// https://tc39.es/ecma262/#sec-createsetiterator
pub fn createSetIterator(
    agent: *Agent,
    set_value: Value,
    comptime kind: Object.PropertyKind,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Perform ? RequireInternalSlot(set, [[SetData]]).
    const set = try set_value.requireInternalSlot(agent, Set);

    // 2. Let closure be a new Abstract Closure with no parameters that captures set and kind and
    //    performs the following steps when called:
    //    [...]
    // 3. Return CreateIteratorFromClosure(closure, "%SetIteratorPrototype%", %SetIteratorPrototype%).
    return SetIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%SetIteratorPrototype%"(),
        .fields = .{ .state = .{ .set = set, .kind = kind, .index = 0 } },
    });
}

/// 24.2.5.2 The %SetIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%setiteratorprototype%-object
pub const SetIteratorPrototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 24.2.5.2.2 %SetIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%setiteratorprototype%-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Set Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 24.2.5.2.1 %SetIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%setiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, empty, "%SetIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateSetIterator closure. State is kept track of through the SetIterator
        //       instance instead of as local variables. This should not be observable.

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (!this_value.isObject() or !this_value.asObject().is(SetIterator)) {
            return agent.throwException(.type_error, "This value must be a Set Iterator", .{});
        }
        const set_iterator = this_value.asObject().as(SetIterator);

        // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
        if (set_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        const set = set_iterator.fields.state.set;
        const kind = set_iterator.fields.state.kind;
        var index = set_iterator.fields.state.index;

        std.debug.assert(kind != .key);

        if (index == 0) _ = try set.fields.registerIterator();
        const iterable_values = &set.fields.iterable_values.?;

        // a. Let index be 0.

        // b. Let entries be set.[[SetData]].
        const entries = &set.fields.set_data;
        _ = entries;

        // c. Let numEntries be the number of elements in entries.
        const num_entries = iterable_values.items.len;

        // d. Repeat, while index < numEntries,
        const value = while (index < num_entries) : (index += 1) {
            // i. Let e be entries[index].
            // ii. Set index to index + 1.
            // iii. If e is not empty, then
            if (iterable_values.items[index]) |value| {
                index += 1;
                break value;
            }
        }
        // e. Return undefined.
        else {
            set_iterator.fields = .completed;
            set.fields.unregisterIterator();
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        };

        set_iterator.fields.state.index = index;

        switch (kind) {
            // 1. If kind is key+value, then
            .@"key+value" => {
                // a. Let result be CreateArrayFromList(« e, e »).
                const result = Value.from(try createArrayFromList(agent, &.{ value, value }));

                // b. Perform ? GeneratorYield(CreateIteratorResultObject(result, false)).
                return Value.from(try createIteratorResultObject(agent, result, false));
            },

            // 2. Else,
            .value => {
                // a. Assert: kind is value.

                // b. Perform ? GeneratorYield(CreateIteratorResultObject(e, false)).
                return Value.from(try createIteratorResultObject(agent, value, false));
            },

            .key => unreachable,
        }

        // 3. NOTE: The number of elements in entries may have increased while execution of this
        //    abstract operation was paused by Yield.
        // 4. Set numEntries to the number of elements in entries.
    }
};

pub const SetIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            set: *Set,
            kind: Object.PropertyKind,
            index: usize,
        },
        completed,
    },
    .tag = .set_iterator,
});
