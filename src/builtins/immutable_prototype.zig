//! 10.4.7 Immutable Prototype Exotic Objects
//! https://tc39.es/ecma262/#sec-immutable-prototype-exotic-objects

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;

/// 10.4.7.1 [[SetPrototypeOf]] ( V )
/// https://tc39.es/ecma262/#sec-immutable-prototype-exotic-objects-setprototypeof-v
pub fn setPrototypeOf(agent: *Agent, object: *Object, prototype: ?*Object) Agent.Error!bool {
    // 1. Return ? SetImmutablePrototype(O, V).
    return setImmutablePrototype(agent, object, prototype);
}

/// 10.4.7.2 SetImmutablePrototype ( O, V )
/// https://tc39.es/ecma262/#sec-set-immutable-prototype
pub fn setImmutablePrototype(
    agent: *Agent,
    object: *Object,
    prototype: ?*Object,
) Agent.Error!bool {
    // 1. Let current be ? O.[[GetPrototypeOf]]().
    const current = try object.internal_methods.getPrototypeOf(agent, object);

    // 2. If SameValue(V, current) is true, return true.
    if (prototype == current) return true;

    // 3. Return false.
    return false;
}
