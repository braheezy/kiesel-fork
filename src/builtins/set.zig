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

        return object;
    }
};

const SetData = ValueHashMap(void);

/// 24.2.4 Properties of Set Instances
/// https://tc39.es/ecma262/#sec-properties-of-set-instances
pub const Set = Object.Factory(.{
    .Fields = struct {
        /// [[SetData]]
        set_data: SetData,
    },
    .tag = .set,
});
