//! 28.2 Proxy Objects
//! https://tc39.es/ecma262/#sec-proxy-objects

const std = @import("std");

const SafePointer = @import("any-pointer").SafePointer;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const PropertyKeyHashMapContext = Object.PropertyStorage.PropertyKeyHashMapContext;
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
fn getPrototypeOf(object: Object) !?Object {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "getPrototypeOf").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("getPrototypeOf"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[GetPrototypeOf]]().
        return target.internalMethods().getPrototypeOf(target);
    }

    // 7. Let handlerProto be ? Call(trap, handler, « target »).
    const handler_prototype = try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{Value.from(target)},
    );

    // 8. If handlerProto is not an Object and handlerProto is not null, throw a TypeError exception.
    if (handler_prototype != .object and handler_prototype != .null) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(
                agent.gc_allocator,
                "{} is not an Object or null",
                .{handler_prototype},
            ),
        );
    }

    // 9. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 10. If extensibleTarget is true, return handlerProto.
    if (extensible_target) {
        return if (handler_prototype == .object) handler_prototype.object else null;
    }

    // 11. Let targetProto be ? target.[[GetPrototypeOf]]().
    const target_prototype = try target.internalMethods().getPrototypeOf(target);

    // 12. If SameValue(handlerProto, targetProto) is false, throw a TypeError exception.
    if (!sameValue(
        handler_prototype,
        if (target_prototype != null) Value.from(target_prototype.?) else .null,
    )) {
        return agent.throwException(
            .type_error,
            "Proxy 'getPrototypeOf' trap must return same prototype for non-extensible target",
        );
    }

    // 13. Return handlerProto.
    return if (handler_prototype == .object) handler_prototype.object else null;
}

/// 10.5.2 [[SetPrototypeOf]] ( V )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-setprototypeof-v
pub fn setPrototypeOf(object: Object, prototype: ?Object) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "setPrototypeOf").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("setPrototypeOf"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[SetPrototypeOf]](V).
        return target.internalMethods().setPrototypeOf(target, prototype);
    }

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, V »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), if (prototype != null) Value.from(prototype.?) else .null },
    )).toBoolean();

    // 8. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 9. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 10. If extensibleTarget is true, return true.
    if (extensible_target) return true;

    // 11. Let targetProto be ? target.[[GetPrototypeOf]]().
    const target_prototype = try target.internalMethods().getPrototypeOf(target);

    // 12. If SameValue(V, targetProto) is false, throw a TypeError exception.
    if (!sameValue(
        if (prototype != null) Value.from(prototype.?) else .null,
        if (target_prototype != null) Value.from(target_prototype.?) else .null,
    )) {
        return agent.throwException(
            .type_error,
            "Proxy 'setPrototypeOf' trap must return false or receive same prototype for non-extensible target",
        );
    }

    // 13. Return true.
    return true;
}

/// 10.5.3 [[IsExtensible]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-isextensible
fn isExtensible(object: Object) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "isExtensible").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("isExtensible"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? IsExtensible(target).
        return target.isExtensible();
    }

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{Value.from(target)},
    )).toBoolean();

    // 8. Let targetResult be ? IsExtensible(target).
    const target_result = try target.isExtensible();

    // 9. If booleanTrapResult is not targetResult, throw a TypeError exception.
    if (boolean_trap_result != target_result) {
        return agent.throwException(
            .type_error,
            "Proxy 'isExtensible' trap must return same result as target",
        );
    }

    // 10. Return booleanTrapResult.
    return boolean_trap_result;
}

/// 10.5.4 [[PreventExtensions]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-preventextensions
fn preventExtensions(object: Object) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "preventExtensions").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("preventExtensions"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[PreventExtensions]]().
        return target.internalMethods().preventExtensions(target);
    }

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{Value.from(target)},
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
            );
        }
    }

    // 9. Return booleanTrapResult.
    return boolean_trap_result;
}

/// 10.5.5 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-getownproperty-p
fn getOwnProperty(object: Object, property_key: PropertyKey) !?PropertyDescriptor {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "getOwnPropertyDescriptor").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("getOwnPropertyDescriptor"));

    // 6. If trap is undefined,
    if (trap == null) {
        // a. Return ? target.[[GetOwnProperty]](P).
        return target.internalMethods().getOwnProperty(target, property_key);
    }

    // 7. Let trapResultObj be ? Call(trap, handler, « target, P »).
    const trap_result_obj = try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), try property_key.toValue(agent) },
    );

    // 8. If trapResultObj is not an Object and trapResultObj is not undefined, throw a TypeError exception.
    if (trap_result_obj != .object and trap_result_obj != .undefined) {
        return agent.throwException(
            .type_error,
            "Proxy 'getOwnPropertyDescriptor' trap must return an object or undefined",
        );
    }

    // 9. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internalMethods().getOwnProperty(target, property_key);

    // 10. If trapResultObj is undefined, then
    if (trap_result_obj == .undefined) {
        // a. If targetDesc is undefined, return undefined.
        if (target_descriptor == null) return null;

        // b. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
        if (target_descriptor.?.configurable == false) {
            return agent.throwException(
                .type_error,
                "Proxy 'getOwnPropertyDescriptor' trap must not return undefined for non-configurable property on target",
            );
        }

        // c. Let extensibleTarget be ? IsExtensible(target).
        const extensible_target = try target.isExtensible();

        // d. If extensibleTarget is false, throw a TypeError exception.
        if (!extensible_target) {
            return agent.throwException(
                .type_error,
                "Proxy 'getOwnPropertyDescriptor' trap must not return undefined for property on non-extensible target",
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
    object: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "defineProperty").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("defineProperty"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[DefineOwnProperty]](P, Desc).
        return target.internalMethods().defineOwnProperty(target, property_key, property_descriptor);
    }

    // 7. Let descObj be FromPropertyDescriptor(Desc).
    const property_descriptor_object = try property_descriptor.fromPropertyDescriptor(agent);

    // 8. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P, descObj »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{
            Value.from(target),
            try property_key.toValue(agent),
            Value.from(property_descriptor_object),
        },
    )).toBoolean();

    // 9. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 10. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internalMethods().getOwnProperty(target, property_key);

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
            );
        }

        // b. If settingConfigFalse is true, throw a TypeError exception.
        if (setting_config_false) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must not return 'configurable: false' for missing property on target",
            );
        }
    }
    // 15. Else,
    else {
        // a. If IsCompatiblePropertyDescriptor(extensibleTarget, Desc, targetDesc) is false, throw
        //    a TypeError exception.
        if (!isCompatiblePropertyDescriptor(extensible_target, property_descriptor, target_descriptor)) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must return a property descriptor compatible with the target object",
            );
        }

        // b. If settingConfigFalse is true and targetDesc.[[Configurable]] is true, throw a
        //    TypeError exception.
        if (setting_config_false and target_descriptor.?.configurable == true) {
            return agent.throwException(
                .type_error,
                "Proxy 'defineProperty' trap must not return 'configurable: false' for configurable property on target",
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
            );
        }
    }

    // 16. Return true.
    return true;
}

/// 10.5.7 [[HasProperty]] ( P )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-hasproperty-p
fn hasProperty(object: Object, property_key: PropertyKey) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "has").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("has"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[HasProperty]](P).
        return target.internalMethods().hasProperty(target, property_key);
    }

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), try property_key.toValue(agent) },
    )).toBoolean();

    // 8. If booleanTrapResult is false, then
    if (!boolean_trap_result) {
        // a. Let targetDesc be ? target.[[GetOwnProperty]](P).
        const target_descriptor = try target.internalMethods().getOwnProperty(target, property_key);

        // b. If targetDesc is not undefined, then
        if (target_descriptor != null) {
            // i. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
            if (target_descriptor.?.configurable == false) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'has' trap must not return false for non-configurable property on target",
                );
            }

            // ii. Let extensibleTarget be ? IsExtensible(target).
            const extensible_target = try target.isExtensible();

            // iii. If extensibleTarget is false, throw a TypeError exception.
            if (!extensible_target) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'has' trap must not return false for property on non-extensible target",
                );
            }
        }
    }

    // 9. Return booleanTrapResult.
    return boolean_trap_result;
}

/// 10.5.8 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-get-p-receiver
fn get(object: Object, property_key: PropertyKey, receiver: Value) !Value {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "get").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("get"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[Get]](P, Receiver).
        return target.internalMethods().get(target, property_key, receiver);
    }

    // 7. Let trapResult be ? Call(trap, handler, « target, P, Receiver »).
    const trap_result = try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), try property_key.toValue(agent), receiver },
    );

    // 8. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internalMethods().getOwnProperty(target, property_key);

    // 9. If targetDesc is not undefined and targetDesc.[[Configurable]] is false, then
    if (target_descriptor != null and target_descriptor.?.configurable == false) {
        // a. If IsDataDescriptor(targetDesc) is true and targetDesc.[[Writable]] is false, then
        if (target_descriptor.?.isDataDescriptor() and target_descriptor.?.writable == false) {
            // i. If SameValue(trapResult, targetDesc.[[Value]]) is false, throw a TypeError exception.
            if (!sameValue(trap_result, target_descriptor.?.value.?)) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'get' trap must not return different value for non-configurable, non-writable property on target",
                );
            }
        }

        // b. If IsAccessorDescriptor(targetDesc) is true and targetDesc.[[Get]] is undefined, then
        if (target_descriptor.?.isAccessorDescriptor() and target_descriptor.?.get == null) {
            // i. If trapResult is not undefined, throw a TypeError exception.
            if (trap_result != .undefined) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'get' trap must return undefined for non-configurable accessor property with no getter on target",
                );
            }
        }
    }

    // 10. Return trapResult.
    return trap_result;
}

/// 10.5.9 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-set-p-v-receiver
fn set(object: Object, property_key: PropertyKey, value: Value, receiver: Value) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "set").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("set"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[Set]](P, V, Receiver).
        return target.internalMethods().set(target, property_key, value, receiver);
    }

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P, V, Receiver »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), try property_key.toValue(agent), value, receiver },
    )).toBoolean();

    // 8. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 9. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internalMethods().getOwnProperty(target, property_key);

    // 10. If targetDesc is not undefined and targetDesc.[[Configurable]] is false, then
    if (target_descriptor != null and target_descriptor.?.configurable == false) {
        // a. If IsDataDescriptor(targetDesc) is true and targetDesc.[[Writable]] is false, then
        if (target_descriptor.?.isDataDescriptor() and target_descriptor.?.writable == false) {
            // i. If SameValue(V, targetDesc.[[Value]]) is false, throw a TypeError exception.
            if (!sameValue(value, target_descriptor.?.value.?)) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'set' trap must not return true when attempting to change the value of a non-configurable, non-writable property on target",
                );
            }
        }

        // b. If IsAccessorDescriptor(targetDesc) is true, then
        if (target_descriptor.?.isAccessorDescriptor()) {
            // i. If targetDesc.[[Set]] is undefined, throw a TypeError exception.
            if (target_descriptor.?.set == null) {
                return agent.throwException(
                    .type_error,
                    "Proxy 'set' trap must not return true when attempting to set the value of a non-configurable accessor property with no setter on target",
                );
            }
        }
    }

    // 11. Return true.
    return true;
}

/// 10.5.10 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-delete-p
fn delete(object: Object, property_key: PropertyKey) !bool {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "deleteProperty").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("deleteProperty"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[Delete]](P).
        return target.internalMethods().delete(target, property_key);
    }

    // 7. Let booleanTrapResult be ToBoolean(? Call(trap, handler, « target, P »)).
    const boolean_trap_result = (try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), try property_key.toValue(agent) },
    )).toBoolean();

    // 8. If booleanTrapResult is false, return false.
    if (!boolean_trap_result) return false;

    // 9. Let targetDesc be ? target.[[GetOwnProperty]](P).
    const target_descriptor = try target.internalMethods().getOwnProperty(target, property_key);

    // 10. If targetDesc is undefined, return true.
    if (target_descriptor == null) return true;

    // 11. If targetDesc.[[Configurable]] is false, throw a TypeError exception.
    if (target_descriptor.?.configurable == false) {
        return agent.throwException(
            .type_error,
            "Proxy 'deleteProperty' trap must not return true for non-configurable property on target",
        );
    }

    // 12. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 13. If extensibleTarget is false, throw a TypeError exception.
    if (!extensible_target) {
        return agent.throwException(
            .type_error,
            "Proxy 'deleteProperty' trap must not return true for property on non-extensible target",
        );
    }

    // 14. Return true.
    return true;
}

/// 10.5.11 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-ownpropertykeys
fn ownPropertyKeys(object: Object) !std.ArrayList(PropertyKey) {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    const PropertyKeySet = std.HashMap(
        PropertyKey,
        void,
        PropertyKeyHashMapContext,
        80,
    );

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "ownKeys").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("ownKeys"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? target.[[OwnPropertyKeys]]().
        return target.internalMethods().ownPropertyKeys(target);
    }

    // 7. Let trapResultArray be ? Call(trap, handler, « target »).
    const trap_result_array = try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{Value.from(target)},
    );

    // 8. Let trapResult be ? CreateListFromArrayLike(trapResultArray, « String, Symbol »).
    const elements = try trap_result_array.createListFromArrayLike(agent, .{
        .element_types = &.{ .string, .symbol },
    });
    var trap_result = try std.ArrayList(PropertyKey).initCapacity(
        agent.gc_allocator,
        elements.len,
    );

    var unique_property_keys = PropertyKeySet.init(agent.gc_allocator);
    defer unique_property_keys.deinit();
    if (elements.len > std.math.maxInt(u32)) return error.OutOfMemory;
    try unique_property_keys.ensureTotalCapacity(@intCast(elements.len));

    for (elements) |element| {
        const property_key = switch (element) {
            .string => |string| PropertyKey.from(string),
            .symbol => |symbol| PropertyKey.from(symbol),
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
        );
    }

    // 10. Let extensibleTarget be ? IsExtensible(target).
    const extensible_target = try target.isExtensible();

    // 11. Let targetKeys be ? target.[[OwnPropertyKeys]]().
    // 12. Assert: targetKeys is a List of property keys.
    // 13. Assert: targetKeys contains no duplicate entries.
    const target_keys = try target.internalMethods().ownPropertyKeys(target);

    // 14. Let targetConfigurableKeys be a new empty List.
    var target_configurable_keys = std.ArrayList(PropertyKey).init(agent.gc_allocator);
    defer target_configurable_keys.deinit();

    // 15. Let targetNonconfigurableKeys be a new empty List.
    var target_nonconfigurable_keys = std.ArrayList(PropertyKey).init(agent.gc_allocator);
    defer target_nonconfigurable_keys.deinit();

    // 16. For each element key of targetKeys, do
    for (target_keys.items) |key| {
        // a. Let desc be ? target.[[GetOwnProperty]](key).
        const property_descriptor = try target.internalMethods().getOwnProperty(target, key);

        // b. If desc is not undefined and desc.[[Configurable]] is false, then
        if (property_descriptor != null and property_descriptor.?.configurable == false) {
            // i. Append key to targetNonconfigurableKeys.
            try target_nonconfigurable_keys.append(key);
        }
        // c. Else,
        else {
            // i. Append key to targetConfigurableKeys.
            try target_configurable_keys.append(key);
        }
    }

    // 17. If extensibleTarget is true and targetNonconfigurableKeys is empty, then
    if (extensible_target and target_nonconfigurable_keys.items.len == 0) {
        // a. Return trapResult.
        return trap_result;
    }

    // 18. Let uncheckedResultKeys be a List whose elements are the elements of trapResult.
    var unchecked_result_keys = PropertyKeySet.init(agent.gc_allocator);
    defer unchecked_result_keys.deinit();
    try unchecked_result_keys.ensureTotalCapacity(@intCast(trap_result.items.len));
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
            );
        }
    }

    // 22. If uncheckedResultKeys is not empty, throw a TypeError exception.
    if (unchecked_result_keys.count() != 0) {
        return agent.throwException(
            .type_error,
            "Proxy 'ownKeys' trap must not include new properties of non-extensible target",
        );
    }

    // 23. Return trapResult.
    return trap_result;
}

/// 10.5.12 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-call-thisargument-argumentslist
fn call(object: Object, this_argument: Value, arguments_list: ArgumentsList) !Value {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Let handler be O.[[ProxyHandler]].
    // 4. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 5. Let trap be ? GetMethod(handler, "apply").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("apply"));

    // 6. If trap is undefined, then
    if (trap == null) {
        // a. Return ? Call(target, thisArgument, argumentsList).
        return Value.from(target).callAssumeCallable(this_argument, arguments_list.values);
    }

    // 7. Let argArray be CreateArrayFromList(argumentsList).
    const arg_array = try createArrayFromList(agent, arguments_list.values);

    // 8. Return ? Call(trap, handler, « target, thisArgument, argArray »).
    return Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), this_argument, Value.from(arg_array) },
    );
}

/// 10.5.13 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots-construct-argumentslist-newtarget
pub fn construct(object: Object, arguments_list: ArgumentsList, new_target: Object) !Object {
    const agent = object.agent();
    const proxy = object.as(Proxy);

    // 1. Perform ? ValidateNonRevokedProxy(O).
    try validateNonRevokedProxy(proxy);

    // 2. Let target be O.[[ProxyTarget]].
    const target = proxy.fields.proxy_target.?;

    // 3. Assert: IsConstructor(target) is true.
    std.debug.assert(Value.from(target).isConstructor());

    // 4. Let handler be O.[[ProxyHandler]].
    // 5. Assert: handler is an Object.
    const handler = proxy.fields.proxy_handler.?;

    // 6. Let trap be ? GetMethod(handler, "construct").
    const trap = try Value.from(handler).getMethod(agent, PropertyKey.from("construct"));

    // 7. If trap is undefined, then
    if (trap == null) {
        // a. Return ? Construct(target, argumentsList, newTarget).
        return target.construct(arguments_list.values, new_target);
    }

    // 8. Let argArray be CreateArrayFromList(argumentsList).
    const arg_array = try createArrayFromList(agent, arguments_list.values);

    // 9. Let newObj be ? Call(trap, handler, « target, argArray, newTarget »).
    const new_obj = try Value.from(trap.?).callAssumeCallable(
        Value.from(handler),
        .{ Value.from(target), Value.from(arg_array), Value.from(new_target) },
    );

    // 10. If newObj is not an Object, throw a TypeError exception.
    if (new_obj != .object) {
        return agent.throwException(.type_error, "Proxy 'construct' trap must return an object");
    }

    // 11. Return newObj.
    return new_obj.object;
}

/// 10.5.14 ValidateNonRevokedProxy ( proxy )
/// https://tc39.es/ecma262/#sec-validatenonrevokedproxy
pub fn validateNonRevokedProxy(proxy: *Proxy) !void {
    const agent = proxy.object().agent();

    // 1. If proxy.[[ProxyTarget]] is null, throw a TypeError exception.
    if (proxy.fields.proxy_target == null) {
        return agent.throwException(.type_error, "Proxy has been revoked");
    }

    // 2. Assert: proxy.[[ProxyHandler]] is not null.
    std.debug.assert(proxy.fields.proxy_handler != null);

    // 3. Return unused.
}

/// 10.5.15 ProxyCreate ( target, handler )
/// https://tc39.es/ecma262/#sec-proxycreate
fn proxyCreate(agent: *Agent, target: Value, handler: Value) !Object {
    // 1. If target is not an Object, throw a TypeError exception.
    if (target != .object) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
        );
    }

    // 2. If handler is not an Object, throw a TypeError exception.
    if (handler != .object) {
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{handler}),
        );
    }

    // 3. Let P be MakeBasicObject(« [[ProxyHandler]], [[ProxyTarget]] »).
    const proxy = try Proxy.create(agent, .{
        .prototype = undefined,

        .fields = .{
            // 6. Set P.[[ProxyTarget]] to target.
            .proxy_target = target.object,

            // 7. Set P.[[ProxyHandler]] to handler.
            .proxy_handler = handler.object,
        },

        // 4. Set P's essential internal methods, except for [[Call]] and [[Construct]], to the definitions specified in 10.5.
        .internal_methods = .{
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
        },
    });

    // 5. If IsCallable(target) is true, then
    if (target.isCallable()) {
        // a. Set P.[[Call]] as specified in 10.5.12.
        proxy.data.internal_methods.call = call;

        // b. If IsConstructor(target) is true, then
        if (target.isConstructor()) {
            // i. Set P.[[Construct]] as specified in 10.5.13.
            proxy.data.internal_methods.construct = construct;
        }
    }

    // 8. Return P.
    return proxy;
}

/// 28.2.2 Properties of the Proxy Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-proxy-constructor
pub const ProxyConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 2,
            .name = "Proxy",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "revocable", revocable, 2, realm);

        return object;
    }

    /// 28.2.1.1 Proxy ( target, handler )
    /// https://tc39.es/ecma262/#sec-proxy-target-handler
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const target = arguments.get(0);
        const handler = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(.type_error, "Proxy must be constructed with 'new'");
        }

        // 2. Return ? ProxyCreate(target, handler).
        return Value.from(try proxyCreate(agent, target, handler));
    }

    /// 28.2.2.1 Proxy.revocable ( target, handler )
    /// https://tc39.es/ecma262/#sec-proxy.revocable
    fn revocable(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const target = arguments.get(0);
        const handler = arguments.get(1);

        const AdditionalFields = struct {
            revocable_proxy: ?Object,
        };

        // 1. Let proxy be ? ProxyCreate(target, handler).
        const proxy = try proxyCreate(agent, target, handler);

        // 2. Let revokerClosure be a new Abstract Closure with no parameters that captures nothing
        //    and performs the following steps when called:
        const revoker_closure = struct {
            fn func(agent_: *Agent, _: Value, _: ArgumentsList) !Value {
                // a. Let F be the active function object.
                const function = agent_.activeFunctionObject();

                // b. Let p be F.[[RevocableProxy]].
                const additional_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*AdditionalFields);
                const revocable_proxy = additional_fields.revocable_proxy;

                // c. If p is null, return undefined.
                if (revocable_proxy == null) return .undefined;

                // d. Set F.[[RevocableProxy]] to null.
                additional_fields.revocable_proxy = null;

                // e. Assert: p is a Proxy exotic object.
                // f. Set p.[[ProxyTarget]] to null.
                revocable_proxy.?.as(Proxy).fields.proxy_target = null;

                // g. Set p.[[ProxyHandler]] to null.
                revocable_proxy.?.as(Proxy).fields.proxy_handler = null;

                // h. Return undefined.
                return .undefined;
            }
        }.func;

        // 3. Let revoker be CreateBuiltinFunction(revokerClosure, 0, "", « [[RevocableProxy]] »).
        const additional_fields = try agent.gc_allocator.create(AdditionalFields);
        const revoker = try createBuiltinFunction(agent, .{ .regular = revoker_closure }, .{
            .length = 0,
            .name = "",
            .additional_fields = SafePointer.make(*AdditionalFields, additional_fields),
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
        proxy_target: ?Object,

        /// [[ProxyHandler]]
        proxy_handler: ?Object,
    },
    .tag = .proxy,
});
