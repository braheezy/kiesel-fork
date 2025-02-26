//! 28.2 Proxy Objects
//! https://tc39.es/ecma262/#sec-proxy-objects

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
const defineBuiltinFunction = utils.defineBuiltinFunction;
const isCompatiblePropertyDescriptor = builtins.isCompatiblePropertyDescriptor;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

/// 10.5.1 [[GetPrototypeOf]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-getprototypeof
fn getPrototypeOf(agent: *Agent, object: *Object) Agent.Error!?*Object {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "getPrototypeOf").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("getPrototypeOf")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[GetPrototypeOf]]().
        return target.internal_methods.getPrototypeOf(agent, target);
    };

    // 7. Let handlerProto be ? Call(trap, handler, « target »).
    const handler_prototype = try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{Value.from(target)},
    );

    // 8. If handlerProto is not an Object and handlerProto is not null, throw a TypeError exception.
    if (!handler_prototype.isObject() and !handler_prototype.isNull()) {
        return agent.throwException(
            .type_error,
            "{} is not an Object or null",
            .{handler_prototype},
        );
    }

    // 9. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 10. If extensibleTarget is true, return handlerProto.
    if (extensible_target) {
        return if (handler_prototype.isObject()) handler_prototype.asObject() else null;
    }

    // 11. Let targetProto be ? target.[[GetPrototypeOf]]().
    const target_prototype = try target.internal_methods.getPrototypeOf(agent, target);

    // 12. If SameValue(handlerProto, targetProto) is false, throw a TypeError exception.
    if (!sameValue(
        handler_prototype,
        if (target_prototype != null) Value.from(target_prototype.?) else .null,
    )) {
        return agent.throwException(
            .type_error,
            "Proxy 'getPrototypeOf' trap must return same prototype for non-extensible target",
            .{},
        );
    }

    // 13. Return handlerProto.
    return if (handler_prototype.isObject()) handler_prototype.asObject() else null;
}

/// 10.5.2 [[SetPrototypeOf]] ( V )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-setprototypeof-v
fn setPrototypeOf(agent: *Agent, object: *Object, prototype: ?*Object) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "setPrototypeOf").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("setPrototypeOf")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[SetPrototypeOf]](V).
        return target.internal_methods.setPrototypeOf(agent, target, prototype);
    };

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, V »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), if (prototype != null) Value.from(prototype.?) else .null },
    )).toBoolean();

    // 8. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 9. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 10. If extensibleTarget is true, return true.
    if (extensible_target) return true;

    // 11. Let targetProto be ? target.[[GetPrototypeOf]]().
    const target_prototype = try target.internal_methods.getPrototypeOf(agent, target);

    // 12. If SameValue(V, targetProto) is false, throw a TypeError exception.
    if (prototype != target_prototype) {
        return agent.throwException(
            .type_error,
            "Proxy 'setPrototypeOf' trap must return false or receive same prototype for non-extensible target",
            .{},
        );
    }

    // 13. Return true.
    return true;
}

/// 10.5.3 [[IsExtensible]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-isextensible
fn isExtensible(agent: *Agent, object: *Object) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "isExtensible").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("isExtensible")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? IsExtensible(target).
        return target.isExtensible();
    };

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{Value.from(target)},
    )).toBoolean();

    // 8. Let targetResult be ? IsExtensible(target).
    const target_result = try target.isExtensible();

    // 9. If booleanTrapResult is not targetResult, throw a TypeError exception.
    if (boolean_trap_result != target_result) {
        return agent.throwException(
            .type_error,
            "Proxy 'isExtensible' trap must return same result as target",
            .{},
        );
    }

    // 10. Return booleanTrapResult.
    return boolean_trap_result;
}

/// 10.5.4 [[PreventExtensions]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-preventextensions
fn preventExtensions(agent: *Agent, object: *Object) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "preventExtensions").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("preventExtensions")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[PreventExtensions]]().
        return target.internal_methods.preventExtensions(agent, target);
    };

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{Value.from(target)},
    )).toBoolean();

    // 8. If booleanTrapResult is true, then
    if (boolean_trap_result) {
        // a. Let extensibleTarget be ? IsExtensible(target).
        const extensible_target = try target.isExtensible();

        // b. If extensibleTarget is true, throw a TypeError exception.
        if (extensible_target) {
            return agent.throwException(
                .type_error,
                "Proxy 'preventExtensions' trap must not return true for extensible target",
                .{},
            );
        }
    }

    // 9. Return booleanTrapResult.
    return boolean_trap_result;
}

/// 10.5.5 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-getownproperty-p
fn getOwnProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
) Agent.Error!?PropertyDescriptor {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "getOwnPropertyDescriptor").
    const trap = try Value.from(handler).getMethod(
        agent,
        PropertyKey.from("getOwnPropertyDescriptor"),
    ) orelse {
        // 6. If trap is undefined,
        //     a. Return ? target.[[GetOwnProperty]](P).
        return target.internal_methods.getOwnProperty(agent, target, property_key);
    };

    // 7. Let trapResultObj be ? Call(trap, handler, « target, P »).
    const trap_result_obj = try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), try property_key.toValue(agent) },
    );

    // 8. If trapResultObj is not an Object and trapResultObj is not undefined, throw a TypeError exception.
    if (!trap_result_obj.isObject() and !trap_result_obj.isUndefined()) {
        return agent.throwException(
            .type_error,
            "Proxy 'getOwnPropertyDescriptor' trap must return an object or undefined",
            .{},
        );
    }

    // 9. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internal_methods.getOwnProperty(
        agent,
        target,
        property_key,
    );

    // 10. If trapResultObj is undefined, then
    if (trap_result_obj.isUndefined()) {
        // a. If targetDesc is undefined, return undefined.
        if (target_descriptor == null) return null;

        // b. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
        if (target_descriptor.?.configurable == false) {
            return agent.throwException(
                .type_error,
                "Proxy 'getOwnPropertyDescriptor' trap must not return undefined for non-configurable property on target",
                .{},
            );
        }

        // c. Let extensibleTarget be ? IsExtensible(target).
        const extensible_target = try target.isExtensible();

        // d. If extensibleTarget is false, throw a TypeError exception.
        if (!extensible_target) {
            return agent.throwException(
                .type_error,
                "Proxy 'getOwnPropertyDescriptor' trap must not return undefined for property on non-extensible target",
                .{},
            );
        }

        // e. Return undefined.
        return null;
    }

    // 11. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 12. Let resultDesc be ? ToPropertyDescriptor(trapResultObj).
    var result_descriptor = try trap_result_obj.toPropertyDescriptor(agent);

    // 13. Perform CompletePropertyDescriptor(resultDesc).
    result_descriptor.completePropertyDescriptor();

    // 14. Let valid be IsCompatiblePropertyDescriptor(extensibleTarget, resultDesc, targetDesc).
    const valid = isCompatiblePropertyDescriptor(
        extensible_target,
        result_descriptor,
        target_descriptor,
    );

    // 15. If valid is false, throw a TypeError exception.
    if (!valid) {
        return agent.throwException(
            .type_error,
            "Proxy 'getOwnPropertyDescriptor' trap must return a property descriptor compatible with the target object",
            .{},
        );
    }

    // 16. If resultDesc.[[Configurable]] is false, then
    if (result_descriptor.configurable == false) {
        // a. If targetDesc is undefined or targetDesc.[[Configurable]] is true, then
        if (target_descriptor == null or target_descriptor.?.configurable == true) {
            // i. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Proxy 'getOwnPropertyDescriptor' trap must not return 'configurable: false' for missing or configurable property on target",
                .{},
            );
        }

        // b. If resultDesc has a [[Writable]] field and resultDesc.[[Writable]] is false, then
        if (result_descriptor.writable == false) {
            // i. Assert: targetDesc has a [[Writable]] field.
            std.debug.assert(target_descriptor.?.writable != null);

            // ii. If targetDesc.[[Writable]] is true, throw a TypeError exception.
            if (target_descriptor.?.writable == true) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'getOwnPropertyDescriptor' trap must not return 'writable: false' for non-configurable but writable property on target",
                    .{},
                );
            }
        }
    }

    // 17. Return resultDesc.
    return result_descriptor;
}

/// 10.5.6 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-defineownproperty-p-desc
fn defineOwnProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "defineProperty").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("defineProperty")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[DefineOwnProperty]](P, Desc).
        return target.internal_methods.defineOwnProperty(
            agent,
            target,
            property_key,
            property_descriptor,
        );
    };

    // 7. Let descObj be FromPropertyDescriptor(Desc).
    const property_descriptor_object = try property_descriptor.fromPropertyDescriptor(agent);

    // 8. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P, descObj »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{
            Value.from(target),
            try property_key.toValue(agent),
            Value.from(property_descriptor_object),
        },
    )).toBoolean();

    // 9. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 10. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internal_methods.getOwnProperty(
        agent,
        target,
        property_key,
    );

    // 11. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 12. If Desc has a [[Configurable]] field and Desc.[[Configurable]] is false, then
    //     a. Let settingConfigFalse be true.
    // 13. Else,
    //     a. Let settingConfigFalse be false.
    const setting_config_false = property_descriptor.configurable == false;

    // 14. If targetDesc is undefined, then
    if (target_descriptor == null) {
        // a. If extensibleTarget is false, throw a TypeError exception.
        if (!extensible_target) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must not return true for missing property on non-extensible target",
                .{},
            );
        }

        // b. If settingConfigFalse is true, throw a TypeError exception.
        if (setting_config_false) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must not return 'configurable: false' for missing property on target",
                .{},
            );
        }
    } else {
        // 15. Else,
        // a. If IsCompatiblePropertyDescriptor(extensibleTarget, Desc, targetDesc) is false, throw
        //    a TypeError exception.
        if (!isCompatiblePropertyDescriptor(extensible_target, property_descriptor, target_descriptor)) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must return a property descriptor compatible with the target object",
                .{},
            );
        }

        // b. If settingConfigFalse is true and targetDesc.[[Configurable]] is true, throw a
        //    TypeError exception.
        if (setting_config_false and target_descriptor.?.configurable == true) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must not return 'configurable: false' for configurable property on target",
                .{},
            );
        }

        // c. If IsDataDescriptor(targetDesc) is true, targetDesc.[[Configurable]] is false, and
        //    targetDesc.[[Writable]] is true, then
        if (target_descriptor.?.isDataDescriptor() and
            target_descriptor.?.configurable == false and
            target_descriptor.?.writable == true and
            // i. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, throw a
            //    TypeError exception.
            property_descriptor.writable == false)
        {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must not return 'writable: false' for non-configurable but writable property on target",
                .{},
            );
        }
    }

    // 16. Return true.
    return true;
}

/// 10.5.7 [[HasProperty]] ( P )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-hasproperty-p
fn hasProperty(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "has").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("has")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[HasProperty]](P).
        return target.internal_methods.hasProperty(agent, target, property_key);
    };

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), try property_key.toValue(agent) },
    )).toBoolean();

    // 8. If booleanTrapResult is false, then
    if (!boolean_trap_result) {
        // a. Let targetDesc be ? target.[[GetOwnProperty]](P).
        const target_descriptor = try target.internal_methods.getOwnProperty(
            agent,
            target,
            property_key,
        );

        // b. If targetDesc is not undefined, then
        if (target_descriptor != null) {
            // i. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
            if (target_descriptor.?.configurable == false) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'has' trap must not return false for non-configurable property on target",
                    .{},
                );
            }

            // ii. Let extensibleTarget be ? IsExtensible(target).
            const extensible_target = try target.isExtensible();

            // iii. If extensibleTarget is false, throw a TypeError exception.
            if (!extensible_target) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'has' trap must not return false for property on non-extensible target",
                    .{},
                );
            }
        }
    }

    // 9. Return booleanTrapResult.
    return boolean_trap_result;
}

/// 10.5.8 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-get-p-receiver
fn get(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    receiver: Value,
) Agent.Error!Value {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "get").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("get")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[Get]](P, Receiver).
        return target.internal_methods.get(agent, target, property_key, receiver);
    };

    // 7. Let trapResult be ? Call(trap, handler, « target, P, Receiver »).
    const trap_result = try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), try property_key.toValue(agent), receiver },
    );

    // 8. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internal_methods.getOwnProperty(
        agent,
        target,
        property_key,
    );

    // 9. If targetDesc is not undefined and targetDesc.[[Configurable]] is false, then
    if (target_descriptor != null and target_descriptor.?.configurable == false) {
        // a. If IsDataDescriptor(targetDesc) is true and targetDesc.[[Writable]] is false, then
        if (target_descriptor.?.isDataDescriptor() and target_descriptor.?.writable == false) {
            // i. If SameValue(trapResult, targetDesc.[[Value]]) is false, throw a TypeError exception.
            if (!sameValue(trap_result, target_descriptor.?.value.?)) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'get' trap must not return different value for non-configurable, non-writable property on target",
                    .{},
                );
            }
        }

        // b. If IsAccessorDescriptor(targetDesc) is true and targetDesc.[[Get]] is undefined, then
        if (target_descriptor.?.isAccessorDescriptor() and target_descriptor.?.get.? == null) {
            // i. If trapResult is not undefined, throw a TypeError exception.
            if (!trap_result.isUndefined()) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'get' trap must return undefined for non-configurable accessor property with no getter on target",
                    .{},
                );
            }
        }
    }

    // 10. Return trapResult.
    return trap_result;
}

/// 10.5.9 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-set-p-v-receiver
fn set(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "set").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("set")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[Set]](P, V, Receiver).
        return target.internal_methods.set(agent, target, property_key, value, receiver);
    };

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P, V, Receiver »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), try property_key.toValue(agent), value, receiver },
    )).toBoolean();

    // 8. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 9. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internal_methods.getOwnProperty(
        agent,
        target,
        property_key,
    );

    // 10. If targetDesc is not undefined and targetDesc.[[Configurable]] is false, then
    if (target_descriptor != null and target_descriptor.?.configurable == false) {
        // a. If IsDataDescriptor(targetDesc) is true and targetDesc.[[Writable]] is false, then
        if (target_descriptor.?.isDataDescriptor() and target_descriptor.?.writable == false) {
            // i. If SameValue(V, targetDesc.[[Value]]) is false, throw a TypeError exception.
            if (!sameValue(value, target_descriptor.?.value.?)) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'set' trap must not return true when attempting to change the value of a non-configurable, non-writable property on target",
                    .{},
                );
            }
        }

        // b. If IsAccessorDescriptor(targetDesc) is true, then
        if (target_descriptor.?.isAccessorDescriptor()) {
            // i. If targetDesc.[[Set]] is undefined, throw a TypeError exception.
            if (target_descriptor.?.set.? == null) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'set' trap must not return true when attempting to set the value of a non-configurable accessor property with no setter on target",
                    .{},
                );
            }
        }
    }

    // 11. Return true.
    return true;
}

/// 10.5.10 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-delete-p
fn delete(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "deleteProperty").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("deleteProperty")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[Delete]](P).
        return target.internal_methods.delete(agent, target, property_key);
    };

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P »)).
    const boolean_trap_result = (try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), try property_key.toValue(agent) },
    )).toBoolean();

    // 8. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 9. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internal_methods.getOwnProperty(
        agent,
        target,
        property_key,
    ) orelse {
        // 10. If targetDesc is undefined, return true.
        return true;
    };

    // 11. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
    if (target_descriptor.configurable == false) {
        return agent.throwException(
            .type_error,
            "Proxy 'deleteProperty' trap must not return true for non-configurable property on target",
            .{},
        );
    }

    // 12. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 13. If extensibleTarget is false, throw a TypeError exception.
    if (!extensible_target) {
        return agent.throwException(
            .type_error,
            "Proxy 'deleteProperty' trap must not return true for property on non-extensible target",
            .{},
        );
    }

    // 14. Return true.
    return true;
}

/// 10.5.11 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-ownpropertykeys
fn ownPropertyKeys(
    agent: *Agent,
    object: *Object,
) Agent.Error!std.ArrayListUnmanaged(PropertyKey) {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "ownKeys").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("ownKeys")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? target.[[OwnPropertyKeys]]().
        return target.internal_methods.ownPropertyKeys(agent, target);
    };

    // 7. Let trapResultArray be ? Call(trap, handler, « target »).
    const trap_result_array = try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{Value.from(target)},
    );

    // 8. Let trapResult be ? CreateListFromArrayLike(trapResultArray, property-key).
    const elements = try trap_result_array.createListFromArrayLike(agent, .property_key);
    var trap_result = try std.ArrayListUnmanaged(PropertyKey).initCapacity(
        agent.gc_allocator,
        elements.len,
    );

    var unique_property_keys: PropertyKey.HashMapUnmanaged(void) = .empty;
    defer unique_property_keys.deinit(agent.gc_allocator);
    if (elements.len > std.math.maxInt(u32)) return error.OutOfMemory;
    try unique_property_keys.ensureTotalCapacity(agent.gc_allocator, @intCast(elements.len));

    for (elements) |element| {
        const property_key = switch (element.type()) {
            .string => PropertyKey.from(element.asString()),
            .symbol => PropertyKey.from(element.asSymbol()),
            else => unreachable,
        };
        trap_result.appendAssumeCapacity(property_key);
        unique_property_keys.putAssumeCapacity(property_key, {});
    }

    // 9. If trapResult contains any duplicate entries, throw a TypeError exception.
    if (trap_result.items.len != unique_property_keys.count()) {
        return agent.throwException(
            .type_error,
            "Proxy 'ownKeys' trap must not return duplicate property keys",
            .{},
        );
    }

    // 10. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 11. Let targetKeys be ? target.[[OwnPropertyKeys]]().
    // 12. Assert: targetKeys is a List of property keys.
    // 13. Assert: targetKeys contains no duplicate entries.
    var target_keys = try target.internal_methods.ownPropertyKeys(agent, target);
    defer target_keys.deinit(agent.gc_allocator);

    // 14. Let targetConfigurableKeys be a new empty List.
    var target_configurable_keys: std.ArrayListUnmanaged(PropertyKey) = .empty;
    defer target_configurable_keys.deinit(agent.gc_allocator);

    // 15. Let targetNonconfigurableKeys be a new empty List.
    var target_nonconfigurable_keys: std.ArrayListUnmanaged(PropertyKey) = .empty;
    defer target_nonconfigurable_keys.deinit(agent.gc_allocator);

    // 16. For each element key of targetKeys, do
    for (target_keys.items) |key| {
        // a. Let desc be ? target.[[GetOwnProperty]](key).
        const property_descriptor = try target.internal_methods.getOwnProperty(agent, target, key);

        // b. If desc is not undefined and desc.[[Configurable]] is false, then
        if (property_descriptor != null and property_descriptor.?.configurable == false) {
            // i. Append key to targetNonconfigurableKeys.
            try target_nonconfigurable_keys.append(agent.gc_allocator, key);
        } else {
            // c. Else,
            // i. Append key to targetConfigurableKeys.
            try target_configurable_keys.append(agent.gc_allocator, key);
        }
    }

    // 17. If extensibleTarget is true and targetNonconfigurableKeys is empty, then
    if (extensible_target and target_nonconfigurable_keys.items.len == 0) {
        // a. Return trapResult.
        return trap_result;
    }

    // 18. Let uncheckedResultKeys be a List whose elements are the elements of trapResult.
    var unchecked_result_keys: PropertyKey.HashMapUnmanaged(void) = .empty;
    defer unchecked_result_keys.deinit(agent.gc_allocator);
    try unchecked_result_keys.ensureTotalCapacity(agent.gc_allocator, @intCast(trap_result.items.len));
    for (trap_result.items) |key| {
        unchecked_result_keys.putAssumeCapacity(key, {});
    }

    // 19. For each element key of targetNonconfigurableKeys, do
    for (target_nonconfigurable_keys.items) |key| {
        // a. If uncheckedResultKeys does not contain key, throw a TypeError exception.
        // b. Remove key from uncheckedResultKeys.
        if (!unchecked_result_keys.remove(key)) {
            return agent.throwException(
                .type_error,
                "Proxy 'ownKeys' trap must not omit any non-configurable properties",
                .{},
            );
        }
    }

    // 20. If extensibleTarget is true, return trapResult.
    if (extensible_target) return trap_result;

    // 21. For each element key of targetConfigurableKeys, do
    for (target_configurable_keys.items) |key| {
        // a. If uncheckedResultKeys does not contain key, throw a TypeError exception.
        // b. Remove key from uncheckedResultKeys.
        if (!unchecked_result_keys.remove(key)) {
            return agent.throwException(
                .type_error,
                "Proxy 'ownKeys' trap must not omit any properties of non-extensible target",
                .{},
            );
        }
    }

    // 22. If uncheckedResultKeys is not empty, throw a TypeError exception.
    if (unchecked_result_keys.count() != 0) {
        return agent.throwException(
            .type_error,
            "Proxy 'ownKeys' trap must not include new properties of non-extensible target",
            .{},
        );
    }

    // 23. Return trapResult.
    return trap_result;
}

/// 10.5.12 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-call-thisargument-argumentslist
fn call(
    agent: *Agent,
    object: *Object,
    this_argument: Value,
    arguments_list: Arguments,
) Agent.Error!Value {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "apply").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("apply")) orelse {
        // 6. If trap is undefined, then
        //     a. Return ? Call(target, thisArgument, argumentsList).
        return Value.from(target).callAssumeCallable(agent, this_argument, arguments_list.values);
    };

    // 7. Let argArray be CreateArrayFromList(argumentsList).
    const arg_array = try createArrayFromList(agent, arguments_list.values);

    // 8. Return ? Call(trap, handler, « target, thisArgument, argArray »).
    return Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), this_argument, Value.from(arg_array) },
    );
}

/// 10.5.13 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-construct-argumentslist-newtarget
fn construct(
    agent: *Agent,
    object: *Object,
    arguments_list: Arguments,
    new_target: *Object,
) Agent.Error!*Object {
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(agent, proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Assert: IsConstructor(target) is true.
    std.debug.assert(Value.from(target).isConstructor());

    // 4. Let handler be O.[[ProxyHandler]].
    // 5. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 6. Let trap be ? GetMethod(handler, "construct").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("construct")) orelse {
        // 7. If trap is undefined, then
        //     a. Return ? Construct(target, argumentsList, newTarget).
        return target.construct(arguments_list.values, new_target);
    };

    // 8. Let argArray be CreateArrayFromList(argumentsList).
    const arg_array = try createArrayFromList(agent, arguments_list.values);

    // 9. Let newObj be ? Call(trap, handler, « target, argArray, newTarget »).
    const new_obj = try Value.from(trap).callAssumeCallable(
        agent,
        Value.from(handler),
        &.{ Value.from(target), Value.from(arg_array), Value.from(new_target) },
    );

    // 10. If newObj is not an Object, throw a TypeError exception.
    if (!new_obj.isObject()) {
        return agent.throwException(
            .type_error,
            "Proxy 'construct' trap must return an object",
            .{},
        );
    }

    // 11. Return newObj.
    return new_obj.asObject();
}

/// 10.5.14 ValidateNonRevokedProxy ( proxy )
/// https://tc39.es/ecma262/#sec-validatenonrevokedproxy
pub fn validateNonRevokedProxy(agent: *Agent, proxy: *Proxy) error{ExceptionThrown}!void {
    // 1. If proxy.[[ProxyTarget]] is null, throw a TypeError exception.
    if (proxy.fields.proxy_target == null) {
        return agent.throwException(.type_error, "Proxy has been revoked", .{});
    }

    // 2. Assert: proxy.[[ProxyHandler]] is not null.
    std.debug.assert(proxy.fields.proxy_handler != null);

    // 3. Return unused.
}

/// 10.5.15 ProxyCreate ( target, handler )
/// https://tc39.es/ecma262/#sec-proxycreate
fn proxyCreate(agent: *Agent, target: Value, handler: Value) Agent.Error!*Object {
    // 1. If target is not an Object, throw a TypeError exception.
    if (!target.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{target});
    }

    // 2. If handler is not an Object, throw a TypeError exception.
    if (!handler.isObject()) {
        return agent.throwException(.type_error, "{} is not an Object", .{handler});
    }

    // 3. Let P be MakeBasicObject(« [[ProxyHandler]], [[ProxyTarget]] »).
    const proxy = try Proxy.create(agent, .{
        .prototype = undefined,

        .fields = .{
            // 6. Set P.[[ProxyTarget]] to target.
            .proxy_target = target.asObject(),

            // 7. Set P.[[ProxyHandler]] to handler.
            .proxy_handler = handler.asObject(),
        },

        // 4. Set P's essential internal methods, except for [[Call]] and [[Construct]], to the definitions specified in 10.5.
        .internal_methods = comptime &proxyInternalMethods(false, false),
    });

    // 5. If IsCallable(target) is true, then
    if (target.isCallable()) {
        // a. Set P.[[Call]] as specified in 10.5.12.
        proxy.internal_methods = comptime &proxyInternalMethods(true, false);

        // b. If IsConstructor(target) is true, then
        if (target.isConstructor()) {
            // i. Set P.[[Construct]] as specified in 10.5.13.
            proxy.internal_methods = comptime &proxyInternalMethods(true, true);
        }
    }

    // 8. Return P.
    return proxy;
}

fn proxyInternalMethods(target_is_callable: bool, target_is_constructor: bool) Object.InternalMethods {
    var internal_methods: Object.InternalMethods = .{
        .getPrototypeOf = getPrototypeOf,
        .setPrototypeOf = setPrototypeOf,
        .isExtensible = isExtensible,
        .preventExtensions = preventExtensions,
        .getOwnProperty = getOwnProperty,
        .defineOwnProperty = defineOwnProperty,
        .hasProperty = hasProperty,
        .get = get,
        .set = set,
        .delete = delete,
        .ownPropertyKeys = ownPropertyKeys,
    };
    if (target_is_callable) {
        internal_methods.call = call;
        if (target_is_constructor) {
            internal_methods.construct = construct;
        }
    }
    return internal_methods;
}

/// 28.2.2 Properties of the Proxy Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-proxy-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 2,
            .name = "Proxy",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "revocable", revocable, 2, realm);
    }

    /// 28.2.1.1 Proxy ( target, handler )
    /// https://tc39.es/ecma262/#sec-proxy-target-handler
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const target = arguments.get(0);
        const handler = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Proxy must be constructed with 'new'", .{});
        }

        // 2. Return ? ProxyCreate(target, handler).
        return Value.from(try proxyCreate(agent, target, handler));
    }

    /// 28.2.2.1 Proxy.revocable ( target, handler )
    /// https://tc39.es/ecma262/#sec-proxy.revocable
    fn revocable(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const target = arguments.get(0);
        const handler = arguments.get(1);

        const AdditionalFields = struct {
            revocable_proxy: ?*Object,
        };

        // 1. Let proxy be ? ProxyCreate(target, handler).
        const proxy = try proxyCreate(agent, target, handler);

        // 2. Let revokerClosure be a new Abstract Closure with no parameters that captures nothing
        //    and performs the following steps when called:
        const revoker_closure = struct {
            fn func(agent_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                // a. Let F be the active function object.
                const function = agent_.activeFunctionObject();

                // b. Let p be F.[[RevocableProxy]].
                const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);
                const revocable_proxy = additional_fields.revocable_proxy orelse {
                    // c. If p is null, return undefined.
                    return .undefined;
                };

                // d. Set F.[[RevocableProxy]] to null.
                additional_fields.revocable_proxy = null;

                // e. Assert: p is a Proxy exotic object.
                // f. Set p.[[ProxyTarget]] to null.
                revocable_proxy.as(Proxy).fields.proxy_target = null;

                // g. Set p.[[ProxyHandler]] to null.
                revocable_proxy.as(Proxy).fields.proxy_handler = null;

                // h. Return undefined.
                return .undefined;
            }
        }.func;

        // 3. Let revoker be CreateBuiltinFunction(revokerClosure, 0, "", « [[RevocableProxy]] »).
        const additional_fields = try agent.gc_allocator.create(AdditionalFields);
        const revoker = try createBuiltinFunction(agent, .{ .function = revoker_closure }, .{
            .length = 0,
            .name = "",
            .additional_fields = .make(*AdditionalFields, additional_fields),
        });

        // 4. Set revoker.[[RevocableProxy]] to proxy.
        additional_fields.* = .{ .revocable_proxy = proxy };

        // 5. Let result be OrdinaryObjectCreate(%Object.prototype%).
        const result = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

        // 6. Perform ! CreateDataPropertyOrThrow(result, "proxy", proxy).
        result.createDataPropertyOrThrow(
            PropertyKey.from("proxy"),
            Value.from(proxy),
        ) catch |err| try noexcept(err);

        // 7. Perform ! CreateDataPropertyOrThrow(result, "revoke", revoker).
        result.createDataPropertyOrThrow(
            PropertyKey.from("revoke"),
            Value.from(revoker),
        ) catch |err| try noexcept(err);

        // 8. Return result.
        return Value.from(result);
    }
};

/// 10.5 Proxy Object Internal Methods and Internal Slots
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots
pub const Proxy = MakeObject(.{
    .Fields = struct {
        /// [[ProxyTarget]]
        proxy_target: ?*Object,

        /// [[ProxyHandler]]
        proxy_handler: ?*Object,
    },
    .tag = .proxy,
});
