//! 24.2 Set Objects
//! https://tc39.es/ecma262/#sec-set-objects

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
const createSetIterator = builtins.createSetIterator;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 24.2.2 Properties of the Set Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-set-constructor
pub const SetConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 0,
            .name = "Set",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 24.2.2.1 Set.prototype
        // https://tc39.es/ecma262/#sec-set.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Set.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 24.2.2.2 get Set [ @@species ]
        // https://tc39.es/ecma262/#sec-get-set-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        // 24.2.3.3 Set.prototype.constructor
        // https://tc39.es/ecma262/#sec-set.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Set.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 24.2.1.1 Set ( [ iterable ] )
    /// https://tc39.es/ecma262/#sec-set-iterable
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const iterable = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Set must be constructed with 'new'");
        }

        // 2. Let set be ? OrdinaryCreateFromConstructor(NewTarget, "%Set.prototype%", « [[SetData]] »).
        const set = try ordinaryCreateFromConstructor(Set, agent, new_target.?, "%Set.prototype%");

        // 3. Set set.[[SetData]] to a new empty List.
        set.as(Set).fields = .{ .set_data = SetData.init(agent.gc_allocator) };

        // 4. If iterable is either undefined or null, return set.
        if (iterable == .undefined or iterable == .null) return Value.from(set);

        // 5. Let adder be ? Get(set, "add").
        const adder = try set.get(PropertyKey.from("add"));

        // 6. If IsCallable(adder) is false, throw a TypeError exception.
        if (!adder.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{adder}),
            );
        }

        // 7. Let iteratorRecord be ? GetIterator(iterable, sync).
        const iterator = try getIterator(agent, iterable, .sync);

        // 8. Repeat,
        while (try iterator.step()) |next| {
            // a. Let next be ? IteratorStep(iteratorRecord).
            // b. If next is false, return set.

            // c. Let nextValue be ? IteratorValue(next).
            const next_value = try Iterator.value(next);

            // d. Let status be Completion(Call(adder, set, « nextValue »)).
            _ = adder.callAssumeCallable(Value.from(set), .{next_value}) catch |err| {
                // e. IfAbruptCloseIterator(status, iteratorRecord).
                return iterator.close(@as(Agent.Error!Value, err));
            };
        }

        return Value.from(set);
    }
};

/// 24.2.3 Properties of the Set Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-set-prototype-object
pub const SetPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "add", add, 1, realm);
        try defineBuiltinFunction(object, "clear", clear, 0, realm);
        try defineBuiltinFunction(object, "delete", delete, 1, realm);
        try defineBuiltinFunction(object, "entries", entries, 0, realm);
        try defineBuiltinFunction(object, "has", has, 1, realm);
        try defineBuiltinAccessor(object, "size", size, null, realm);

        // 24.2.3.12 Set.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-set.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Set"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 24.2.3.1 Set.prototype.add ( value )
    /// https://tc39.es/ecma262/#sec-set.prototype.add
    fn add(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        var value = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. For each element e of S.[[SetData]], do
        //     a. If e is not empty and SameValueZero(e, value) is true, then
        //         i. Return S.

        // 4. If value is -0𝔽, set value to +0𝔽.
        if (value == .number and value.number.isNegativeZero()) value = Value.from(0);

        // 5. Append value to S.[[SetData]].
        const result = try set.fields.set_data.getOrPut(value);
        if (!result.found_existing) {
            result.value_ptr.* = {};
            if (set.fields.iterable_values) |*iterable_values| {
                try iterable_values.append(value);
            }
        }

        // 6. Return S.
        return Value.from(set.object());
    }

    /// 24.2.3.2 Set.prototype.clear ( )
    /// https://tc39.es/ecma262/#sec-set.prototype.clear
    fn clear(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. For each element e of S.[[SetData]], do
        //     a. Replace the element of S.[[SetData]] whose value is e with an element whose value
        //        is empty.
        set.fields.set_data.clearAndFree();
        if (set.fields.iterable_values) |*iterable_values| {
            iterable_values.clearAndFree();
        }

        // 4. Return undefined.
        return .undefined;
    }

    /// 24.2.3.4 Set.prototype.delete ( value )
    /// https://tc39.es/ecma262/#sec-set.prototype.delete
    fn delete(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const value = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. For each element e of S.[[SetData]], do
        //     a. If e is not empty and SameValueZero(e, value) is true, then
        //         i. Replace the element of S.[[SetData]] whose value is e with an element whose value is empty.
        //         ii. Return true.
        if (set.fields.set_data.getIndex(value)) |index| {
            set.fields.set_data.orderedRemoveAt(index);
            if (set.fields.iterable_values) |*iterable_values| {
                iterable_values.items[index] = null;
            }
            return Value.from(true);
        }

        // 4. Return false.
        return Value.from(false);
    }

    /// 24.2.3.5 Set.prototype.entries ( )
    /// https://tc39.es/ecma262/#sec-set.prototype.entries
    fn entries(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let M be the this value.
        const map = this_value;

        // 2. Return ? CreateSetIterator(S, key+value).
        return Value.from(try createSetIterator(agent, map, .@"key+value"));
    }

    /// 24.2.3.7 Set.prototype.has ( value )
    /// https://tc39.es/ecma262/#sec-set.prototype.has
    fn has(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const value = arguments.get(0);

        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. For each element e of S.[[SetData]], do
        //     a. If e is not empty and SameValueZero(e, value) is true, return true.
        // 4. Return false.
        return Value.from(set.fields.set_data.contains(value));
    }

    /// 24.2.3.9 get Set.prototype.size
    /// https://tc39.es/ecma262/#sec-get-set.prototype.size
    fn size(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let S be the this value.
        // 2. Perform ? RequireInternalSlot(S, [[SetData]]).
        const set = try this_value.requireInternalSlot(agent, Set);

        // 3. Let count be 0.
        // 4. For each element e of S.[[SetData]], do
        //     a. If e is not empty, set count to count + 1.
        // 5. Return 𝔽(count).
        return Value.from(set.fields.set_data.count());
    }
};

const SetData = ValueHashMap(void);
const IterableValues = std.ArrayList(?Value);

/// 24.2.4 Properties of Set Instances
/// https://tc39.es/ecma262/#sec-properties-of-set-instances
pub const Set = Object.Factory(.{
    .Fields = struct {
        const Self = @This();

        /// [[SetData]]
        set_data: SetData,

        /// List of values and their deletion status for SetIterator and Set.prototype.forEach(),
        /// created and destroyed on demand.
        iterable_values: ?IterableValues = null,
        active_iterators: usize = 0,

        pub fn registerIterator(self: *Self) !*IterableValues {
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_values == null);
                self.iterable_values = try IterableValues.initCapacity(
                    self.set_data.allocator,
                    self.set_data.count(),
                );
                for (self.set_data.keys()) |value| {
                    self.iterable_values.?.appendAssumeCapacity(value);
                }
            }
            self.active_iterators += 1;
            return &self.iterable_values.?;
        }

        pub fn unregisterIterator(self: *Self) void {
            self.active_iterators -= 1;
            if (self.active_iterators == 0) {
                std.debug.assert(self.iterable_values != null);
                self.iterable_values.?.deinit();
                self.iterable_values = null;
            }
        }
    },
    .tag = .set,
});
