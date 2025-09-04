//! 24.3 WeakMap Objects
//! https://tc39.es/ecma262/#sec-weakmap-objects

const std = @import("std");

const build_options = @import("build-options");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const gc = @import("../gc.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const addEntriesFromIterable = builtins.addEntriesFromIterable;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 24.3.2 Properties of the WeakMap Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-weakmap-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "WeakMap",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 24.3.2.1 WeakMap.prototype
        // https://tc39.es/ecma262/#sec-weakmap.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%WeakMap.prototype%"()),
            .none,
        );
    }

    /// 24.3.1.1 WeakMap ( [ iterable ] )
    /// https://tc39.es/ecma262/#sec-weakmap-iterable
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const iterable = arguments.get(0);

        const new_target = maybe_new_target orelse {
            // 1. If NewTarget is undefined, throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "WeakMap must be constructed with 'new'",
                .{},
            );
        };

        // 2. Let map be ? OrdinaryCreateFromConstructor(NewTarget, "%WeakMap.prototype%", « [[WeakMapData]] »).
        const map = try ordinaryCreateFromConstructor(
            WeakMap,
            agent,
            new_target,
            "%WeakMap.prototype%",
            .{
                // 3. Set map.[[WeakMapData]] to a new empty List.
                .weak_map_data = .empty,
            },
        );

        // 4. If iterable is either undefined or null, return map.
        if (iterable.isUndefined() or iterable.isNull()) {
            return Value.from(&map.object);
        }

        // 5. Let adder be ? Get(map, "set").
        const adder = try map.object.get(agent, PropertyKey.from("set"));

        // 6. If IsCallable(adder) is false, throw a TypeError exception.
        if (!adder.isCallable()) {
            return agent.throwException(
                .type_error,
                "{f} is not callable",
                .{adder},
            );
        }

        // 7. Return ? AddEntriesFromIterable(map, iterable, adder).
        return Value.from(try addEntriesFromIterable(agent, &map.object, iterable, adder.asObject()));
    }
};

/// 24.3.3 Properties of the WeakMap Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-weakmap-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "delete", delete, 1, realm);
        try object.defineBuiltinFunction(agent, "get", get, 1, realm);
        try object.defineBuiltinFunction(agent, "has", has, 1, realm);
        try object.defineBuiltinFunction(agent, "set", set, 2, realm);

        // 24.3.3.1 WeakMap.prototype.constructor
        // https://tc39.es/ecma262/#sec-weakmap.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%WeakMap%"()),
        );

        // 24.3.3.6 WeakMap.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-weakmap.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("WeakMap"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 24.3.3.2 WeakMap.prototype.delete ( key )
    /// https://tc39.es/ecma262/#sec-weakmap.prototype.delete
    fn delete(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[WeakMapData]]).
        const map = try this_value.requireInternalSlot(agent, WeakMap);

        // 3. If CanBeHeldWeakly(key) is false, return false.
        if (!key.canBeHeldWeakly(agent)) {
            return Value.from(false);
        }

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[WeakMapData]], do
        //    a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, then
        //       i. Set p.[[Key]] to empty.
        //       ii. Set p.[[Value]] to empty.
        //       iii. Return true.
        // 5. Return false.
        const is_removed = map.fields.weak_map_data.remove(Value.Weak.init(key));
        return Value.from(is_removed);
    }

    /// 24.3.3.3 WeakMap.prototype.get ( key )
    /// https://tc39.es/ecma262/#sec-weakmap.prototype.get
    fn get(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[WeakMapData]]).
        const map = try this_value.requireInternalSlot(agent, WeakMap);

        // 3. If CanBeHeldWeakly(key) is false, return undefined.
        if (!key.canBeHeldWeakly(agent)) {
            return .undefined;
        }

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[WeakMapData]], do
        // a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, return p.[[Value]].
        // 5. Return undefined.
        const maybe_value = map.fields.weak_map_data.get(Value.Weak.init(key));
        return maybe_value orelse .undefined;
    }

    /// 24.3.3.4 WeakMap.prototype.has ( key )
    /// https://tc39.es/ecma262/#sec-weakmap.prototype.has
    fn has(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const key = arguments.get(0);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[WeakMapData]]).
        const map = try this_value.requireInternalSlot(agent, WeakMap);

        // 3. If CanBeHeldWeakly(key) is false, return false.
        if (!key.canBeHeldWeakly(agent)) {
            return Value.from(false);
        }

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[WeakMapData]], do
        // a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, return true.
        // 5. Return false.
        const is_present = map.fields.weak_map_data.contains(Value.Weak.init(key));
        return Value.from(is_present);
    }

    /// 24.3.3.5 WeakMap.prototype.set ( key, value )
    /// https://tc39.es/ecma262/#sec-weakmap.prototype.set
    fn set(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const key = arguments.get(0);
        const value = arguments.get(1);

        // 1. Let M be the this value.
        // 2. Perform ? RequireInternalSlot(M, [[WeakMapData]]).
        const map = try this_value.requireInternalSlot(agent, WeakMap);

        // 3. If CanBeHeldWeakly(key) is false, throw a TypeError exception.
        if (!key.canBeHeldWeakly(agent)) {
            return agent.throwException(
                .type_error,
                "Value {f} cannot be held weakly",
                .{key},
            );
        }

        // 4. For each Record { [[Key]], [[Value]] } p of M.[[WeakMapData]], do
        //    a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, then
        //       i. Set p.[[Value]] to value.
        //       ii. Return M.
        // 5. Let p be the Record { [[Key]]: key, [[Value]]: value }.
        // 6. Append p to M.[[WeakMapData]].
        const weak_map_data = &map.fields.weak_map_data;
        const weak_key = Value.Weak.init(key);
        const gop = try weak_map_data.getOrPut(agent.gc_allocator, weak_key);
        gop.value_ptr.* = value;
        if (build_options.enable_libgc and !gop.found_existing) {
            // Implements 9.9.3 Execution step 1.c
            // https://tc39.es/ecma262/#sec-weakref-execution
            const finalizer_data = try agent.gc_allocator.create(gc.FinalizerData(CleanupEntryData));
            finalizer_data.* = .{ .data = .{
                .key = weak_key,
                .weak_map_data = weak_map_data,
            } };
            gc.registerFinalizer(weak_key.getPtr(), finalizer_data, struct {
                pub fn finalizer(_: *anyopaque, data: *CleanupEntryData) void {
                    // i. Set r.[[Key]] to empty.
                    // ii. Set r.[[Value]] to empty.
                    _ = data.weak_map_data.*.remove(data.key);
                }
            }.finalizer);
        }

        // 7. Return M.
        return this_value;
    }

    const CleanupEntryData = struct {
        key: Value.Weak,
        weak_map_data: *WeakMapData,
    };
};

const WeakMapData = Value.Weak.HashMapUnmanaged(Value);

/// 24.3.4 Properties of WeakMap Instances
/// https://tc39.es/ecma262/#sec-properties-of-weakmap-instances
pub const WeakMap = MakeObject(.{
    .Fields = struct {
        /// [[WeakMapData]]
        weak_map_data: WeakMapData,
    },
    .tag = .weak_map,
});
