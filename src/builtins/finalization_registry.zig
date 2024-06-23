//! 26.2 FinalizationRegistry Objects
//! https://tc39.es/ecma262/#sec-finalization-registry-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const JobCallback = execution.JobCallback;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

/// 26.2.1 The FinalizationRegistry Constructor
/// https://tc39.es/ecma262/#sec-finalization-registry-constructor
pub const FinalizationRegistryConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 1,
            .name = "FinalizationRegistry",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 26.2.2.1 FinalizationRegistry.prototype
        // https://tc39.es/ecma262/#sec-finalization-registry.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%FinalizationRegistry.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 26.2.3.1 FinalizationRegistry.prototype.constructor
        // https://tc39.es/ecma262/#sec-finalization-registry.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%FinalizationRegistry.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 26.2.1.1 FinalizationRegistry ( cleanupCallback )
    /// https://tc39.es/ecma262/#sec-finalization-registry-cleanup-callback
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const cleanup_callback = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "FinalizationRegistry must be constructed with 'new'",
                .{},
            );
        }

        // 2. If IsCallable(cleanupCallback) is false, throw a TypeError exception.
        if (!cleanup_callback.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{cleanup_callback});
        }

        // 4. Let fn be the active function object.
        const function = agent.activeFunctionObject();

        // 3. Let finalizationRegistry be ? OrdinaryCreateFromConstructor(NewTarget,
        //    "%FinalizationRegistry.prototype%", « [[Realm]], [[CleanupCallback]], [[Cells]] »).
        const finalization_registry = try ordinaryCreateFromConstructor(
            FinalizationRegistry,
            agent,
            new_target.?,
            "%FinalizationRegistry.prototype%",
            .{
                // 5. Set finalizationRegistry.[[Realm]] to fn.[[Realm]].
                .realm = function.as(builtins.BuiltinFunction).fields.realm,

                // 6. Set finalizationRegistry.[[CleanupCallback]] to HostMakeJobCallback(cleanupCallback).
                .cleanup_callback = agent.host_hooks.hostMakeJobCallback(cleanup_callback.object),

                // 7. Set finalizationRegistry.[[Cells]] to a new empty List.
            },
        );

        // 8. Return finalizationRegistry.
        return Value.from(finalization_registry);
    }
};

/// 26.2.3 Properties of the FinalizationRegistry Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-finalization-registry-prototype-object
pub const FinalizationRegistryPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "register", register, 2, realm);
        try defineBuiltinFunction(object, "unregister", unregister, 1, realm);

        // 26.2.3.4 FinalizationRegistry.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-finalization-registry.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("FinalizationRegistry"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 26.2.3.2 FinalizationRegistry.prototype.register ( target, heldValue [ , unregisterToken ] )
    /// https://tc39.es/ecma262/#sec-finalization-registry.prototype.register
    fn register(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const held_value = arguments.get(1);
        const unregister_token = arguments.get(2);
        var maybe_unregister_token: ?Value = unregister_token;

        // 1. Let finalizationRegistry be the this value.
        // 2. Perform ? RequireInternalSlot(finalizationRegistry, [[Cells]]).
        const finalization_registry = try this_value.requireInternalSlot(agent, FinalizationRegistry);

        // 3. If CanBeHeldWeakly(target) is false, throw a TypeError exception.
        if (!target.canBeHeldWeakly(agent)) {
            return agent.throwException(
                .type_error,
                "Value {} cannot be held weakly",
                .{target},
            );
        }

        // 4. If SameValue(target, heldValue) is true, throw a TypeError exception.
        if (sameValue(target, held_value)) {
            return agent.throwException(
                .type_error,
                "Target and held value must not be the same",
                .{},
            );
        }

        // 5. If CanBeHeldWeakly(unregisterToken) is false, then
        if (!unregister_token.canBeHeldWeakly(agent)) {
            // a. If unregisterToken is not undefined, throw a TypeError exception.
            if (unregister_token != .undefined) {
                return agent.throwException(
                    .type_error,
                    "Value {} cannot be held weakly",
                    .{unregister_token},
                );
            }

            // b. Set unregisterToken to empty.
            maybe_unregister_token = null;
        }

        // TODO: 6-7.
        _ = finalization_registry;

        // 8. Return undefined.
        return .undefined;
    }

    /// 26.2.3.3 FinalizationRegistry.prototype.unregister ( unregisterToken )
    /// https://tc39.es/ecma262/#sec-finalization-registry.prototype.unregister
    fn unregister(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const unregister_token = arguments.get(0);

        // 1. Let finalizationRegistry be the this value.
        // 2. Perform ? RequireInternalSlot(finalizationRegistry, [[Cells]]).
        const finalization_registry = try this_value.requireInternalSlot(agent, FinalizationRegistry);

        // 3. If CanBeHeldWeakly(unregisterToken) is false, throw a TypeError exception.
        if (!unregister_token.canBeHeldWeakly(agent)) {
            return agent.throwException(
                .type_error,
                "Value {} cannot be held weakly",
                .{unregister_token},
            );
        }

        // TODO: 4-6.
        _ = finalization_registry;
        return Value.from(false);
    }
};

/// 26.2.4 Properties of FinalizationRegistry Instances
/// https://tc39.es/ecma262/#sec-properties-of-finalization-registry-instances
pub const FinalizationRegistry = MakeObject(.{
    .Fields = struct {
        /// [[Realm]]
        realm: *Realm,

        /// [[CleanupCallback]]
        cleanup_callback: JobCallback,

        // NOTE: [[Cells]] is implemented via libgc finalizers.
    },
    .tag = .finalization_registry,
});
