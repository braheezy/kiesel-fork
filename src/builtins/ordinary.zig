//! 10.1 Ordinary Object Internal Methods and Internal Slots
//! https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = Object.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const sameValue = types.sameValue;

const module = @This();

// Only export these in a namespace so it's clear what they are.
pub const internal_methods = struct {
    pub const getPrototypeOf = module.getPrototypeOf;
    pub const setPrototypeOf = module.setPrototypeOf;
    pub const isExtensible = module.isExtensible;
    pub const preventExtensions = module.preventExtensions;
    pub const getOwnProperty = module.getOwnProperty;
    pub const defineOwnProperty = module.defineOwnProperty;
    pub const hasProperty = module.hasProperty;
    pub const get = module.get;
    pub const set = module.set;
    pub const delete = module.delete;
    pub const ownPropertyKeys = module.ownPropertyKeys;
};

/// 10.1.1 [[GetPrototypeOf]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-getprototypeof
fn getPrototypeOf(_: *Agent, object: *Object) error{}!?*Object {
    // 1. Return OrdinaryGetPrototypeOf(O).
    return ordinaryGetPrototypeOf(object);
}

/// 10.1.1.1 OrdinaryGetPrototypeOf ( O )
/// https://tc39.es/ecma262/#sec-ordinarygetprototypeof
pub fn ordinaryGetPrototypeOf(object: *Object) ?*Object {
    // 1. Return O.[[Prototype]].
    return object.prototype();
}

/// 10.1.2 [[SetPrototypeOf]] ( V )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-setprototypeof-v
fn setPrototypeOf(_: *Agent, object: *Object, prototype: ?*Object) std.mem.Allocator.Error!bool {
    // 1. Return OrdinarySetPrototypeOf(O, V).
    return ordinarySetPrototypeOf(object, prototype);
}

/// 10.1.2.1 OrdinarySetPrototypeOf ( O, V )
/// https://tc39.es/ecma262/#sec-ordinarysetprototypeof
pub fn ordinarySetPrototypeOf(object: *Object, prototype: ?*Object) std.mem.Allocator.Error!bool {
    // 1. Let current be O.[[Prototype]].
    const current = object.prototype();

    // 2. If SameValue(V, current) is true, return true.
    if (prototype == current) return true;

    // 3. Let extensible be O.[[Extensible]].
    const extensible = object.extensible();

    // 4. If extensible is false, return false.
    if (!extensible) return false;

    // 5. Let p be V.
    var parent_prototype = prototype;

    // 6. Let done be false.
    // 7. Repeat, while done is false,
    while (parent_prototype) |parent_prototype_object| {
        // a. If p is null, then
        //     i. Set done to true.

        // b. Else if SameValue(p, O) is true, then
        if (parent_prototype_object == object) {
            // i. Return false.
            return false;
        }

        // c. Else,
        // i. If p.[[GetPrototypeOf]] is not the ordinary object internal method defined in 10.1.1,
        //    set done to true.
        if (parent_prototype_object.internal_methods.getPrototypeOf != getPrototypeOf) break;

        // ii. Else, set p to p.[[Prototype]].
        parent_prototype = parent_prototype_object.prototype();
    }

    // 8. Set O.[[Prototype]] to V.
    try object.setPrototype(prototype);

    // 9. Return true.
    return true;
}

/// 10.1.3 [[IsExtensible]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-isextensible
fn isExtensible(_: *Agent, object: *Object) error{}!bool {
    // 1. Return OrdinaryIsExtensible(O).
    return ordinaryIsExtensible(object);
}

/// 10.1.3.1 OrdinaryIsExtensible ( O )
/// https://tc39.es/ecma262/#sec-ordinaryisextensible
pub fn ordinaryIsExtensible(object: *Object) bool {
    // 1. Return O.[[Extensible]].
    return object.extensible();
}

/// 10.1.4 [[PreventExtensions]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-preventextensions
fn preventExtensions(_: *Agent, object: *Object) std.mem.Allocator.Error!bool {
    // 1. Return OrdinaryPreventExtensions(O).
    return ordinaryPreventExtensions(object);
}

/// 10.1.4.1 OrdinaryPreventExtensions ( O )
/// https://tc39.es/ecma262/#sec-ordinarypreventextensions
pub fn ordinaryPreventExtensions(object: *Object) std.mem.Allocator.Error!bool {
    // 1. Set O.[[Extensible]] to false.
    try object.setNonExtensible();

    // 2. Return true.
    return true;
}

/// 10.1.5 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-getownproperty-p
fn getOwnProperty(
    _: *Agent,
    object: *Object,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    // 1. Return OrdinaryGetOwnProperty(O, P).
    return ordinaryGetOwnProperty(object, property_key);
}

/// 10.1.5.1 OrdinaryGetOwnProperty ( O, P )
/// https://tc39.es/ecma262/#sec-ordinarygetownproperty
pub fn ordinaryGetOwnProperty(
    object: *Object,
    property_key: PropertyKey,
) std.mem.Allocator.Error!?PropertyDescriptor {
    // 1. If O does not have an own property with key P, return undefined.
    // 2. Let D be a newly created Property Descriptor with no fields.
    // 3. Let X be O's own property whose key is P.
    // 4. If X is a data property, then
    //     a. Set D.[[Value]] to the value of X's [[Value]] attribute.
    //     b. Set D.[[Writable]] to the value of X's [[Writable]] attribute.
    // 5. Else,
    //     a. Assert: X is an accessor property.
    //     b. Set D.[[Get]] to the value of X's [[Get]] attribute.
    //     c. Set D.[[Set]] to the value of X's [[Set]] attribute.
    // 6. Set D.[[Enumerable]] to the value of X's [[Enumerable]] attribute.
    // 7. Set D.[[Configurable]] to the value of X's [[Configurable]] attribute.
    // 8. Return D.
    const property_descriptor = (try object.property_storage.getCreateIntrinsicIfNeeded(property_key)) orelse return null;
    return property_descriptor.toPropertyDescriptor();
}

/// 10.1.6 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-defineownproperty-p-desc
fn defineOwnProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    // 1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    return ordinaryDefineOwnProperty(agent, object, property_key, property_descriptor);
}

/// 10.1.6.1 OrdinaryDefineOwnProperty ( O, P, Desc )
/// https://tc39.es/ecma262/#sec-ordinarydefineownproperty
pub fn ordinaryDefineOwnProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    // 1. Let current be ? O.[[GetOwnProperty]](P).
    const current = try object.internal_methods.getOwnProperty(agent, object, property_key);

    // 2. Let extensible be ? IsExtensible(O).
    const extensible = try object.isExtensible();

    // 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
    return validateAndApplyPropertyDescriptor(
        agent.gc_allocator,
        object,
        property_key,
        extensible,
        property_descriptor,
        current,
    );
}

/// 10.1.6.2 IsCompatiblePropertyDescriptor ( Extensible, Desc, Current )
/// https://tc39.es/ecma262/#sec-iscompatiblepropertydescriptor
pub fn isCompatiblePropertyDescriptor(
    extensible: bool,
    descriptor: PropertyDescriptor,
    current: ?PropertyDescriptor,
) bool {
    // 1. Return ValidateAndApplyPropertyDescriptor(undefined, "", Extensible, Desc, Current).
    return validateAndApplyPropertyDescriptor(
        undefined, // No Allocator needed when not passing an object
        null,
        PropertyKey.from(""),
        extensible,
        descriptor,
        current,
    ) catch unreachable;
}

/// 10.1.6.3 ValidateAndApplyPropertyDescriptor ( O, P, extensible, Desc, current )
/// https://tc39.es/ecma262/#sec-validateandapplypropertydescriptor
fn validateAndApplyPropertyDescriptor(
    allocator: std.mem.Allocator,
    maybe_object: ?*Object,
    property_key: PropertyKey,
    extensible: bool,
    descriptor: PropertyDescriptor,
    maybe_current: ?PropertyDescriptor,
) std.mem.Allocator.Error!bool {
    // 1. Assert: P is a property key.

    // 2. If current is undefined, then
    if (maybe_current == null) {
        // a. If extensible is false, return false.
        if (!extensible) return false;

        // b. If O is undefined, return true.
        if (maybe_object == null) return true;

        const object = maybe_object.?;

        // c. If IsAccessorDescriptor(Desc) is true, then
        const property_descriptor: Object.PropertyStorage.CompletePropertyDescriptor = if (descriptor.isAccessorDescriptor()) blk: {
            // i. Create an own accessor property named P of object O whose [[Get]], [[Set]],
            //    [[Enumerable]], and [[Configurable]] attributes are set to the value of the
            //    corresponding field in Desc if Desc has that field, or to the attribute's default
            //    value otherwise.
            break :blk .{
                .value_or_accessor = .{
                    .accessor = .{
                        .get = descriptor.get orelse @as(?*Object, null),
                        .set = descriptor.set orelse @as(?*Object, null),
                    },
                },
                .attributes = .{
                    .writable = false,
                    .enumerable = descriptor.enumerable orelse false,
                    .configurable = descriptor.configurable orelse false,
                },
            };
        }
        // d. Else,
        else blk: {
            // i. Create an own data property named P of object O whose [[Value]], [[Writable]],
            //    [[Enumerable]], and [[Configurable]] attributes are set to the value of the
            //    corresponding field in Desc if Desc has that field, or to the attribute's default
            //    value otherwise.
            break :blk .{
                .value_or_accessor = .{
                    .value = descriptor.value orelse .undefined,
                },
                .attributes = .{
                    .writable = descriptor.writable orelse false,
                    .enumerable = descriptor.enumerable orelse false,
                    .configurable = descriptor.configurable orelse false,
                },
            };
        };

        try object.property_storage.set(
            allocator,
            property_key,
            property_descriptor,
        );

        // e. Return true.
        return true;
    }

    const current = maybe_current.?;

    // 3. Assert: current is a fully populated Property Descriptor.
    std.debug.assert(current.isFullyPopulated());

    // 4. If Desc does not have any fields, return true.
    if (!descriptor.hasFields()) return true;

    // 5. If current.[[Configurable]] is false, then
    if (!current.configurable.?) {
        // a. If Desc has a [[Configurable]] field and Desc.[[Configurable]] is true, return false.
        if (descriptor.configurable) |configurable| if (configurable) return false;

        // b. If Desc has an [[Enumerable]] field and Desc.[[Enumerable]] is not
        //    current.[[Enumerable]], return false.
        if (descriptor.enumerable) |enumerable| if (enumerable != current.enumerable.?) return false;

        // c. If IsGenericDescriptor(Desc) is false and IsAccessorDescriptor(Desc) is not
        //    IsAccessorDescriptor(current), return false.
        if (!descriptor.isGenericDescriptor() and
            descriptor.isAccessorDescriptor() != current.isAccessorDescriptor()) return false;

        // d. If IsAccessorDescriptor(current) is true, then
        if (current.isAccessorDescriptor()) {
            // i. If Desc has a [[Get]] field and SameValue(Desc.[[Get]], current.[[Get]]) is false,
            //    return false.
            if (descriptor.get != null and !(blk: {
                if (descriptor.get.? == null and current.get.? == null) break :blk true;
                if (descriptor.get.?) |a| if (current.get.?) |b| break :blk a == b;
                break :blk false;
            })) return false;

            // ii. If Desc has a [[Set]] field and SameValue(Desc.[[Set]], current.[[Set]]) is
            //     false, return false.
            if (descriptor.set != null and !(blk: {
                if (descriptor.set.? == null and current.set.? == null) break :blk true;
                if (descriptor.set.?) |a| if (current.set.?) |b| break :blk a == b;
                break :blk false;
            })) return false;
        }
        // e. Else if current.[[Writable]] is false, then
        else if (!current.writable.?) {
            // i. If Desc has a [[Writable]] field and Desc.[[Writable]] is true, return false.
            if (descriptor.writable) |writable| if (writable) return false;

            // ii. NOTE: SameValue returns true for NaN values which may be distinguishable by
            //     other means. Returning here ensures that any existing property of O remains unmodified.
            // iii. If Desc has a [[Value]] field, return SameValue(Desc.[[Value]],
            //      current.[[Value]]).
            if (descriptor.value) |value| return sameValue(value, current.value.?);
        }
    }

    // 6. If O is not undefined, then
    if (maybe_object) |object| {
        // a. If IsDataDescriptor(current) is true and IsAccessorDescriptor(Desc) is true, then
        const property_descriptor: Object.PropertyStorage.CompletePropertyDescriptor = if (current.isDataDescriptor() and descriptor.isAccessorDescriptor()) blk: {
            // i. If Desc has a [[Configurable]] field, let configurable be Desc.[[Configurable]];
            //    else let configurable be current.[[Configurable]].
            const configurable = descriptor.configurable orelse current.configurable.?;

            // ii. If Desc has a [[Enumerable]] field, let enumerable be Desc.[[Enumerable]]; else
            //     let enumerable be current.[[Enumerable]].
            const enumerable = descriptor.enumerable orelse current.enumerable.?;

            // iii. Replace the property named P of object O with an accessor property whose
            //      [[Configurable]] and [[Enumerable]] attributes are set to configurable and
            //      enumerable, respectively, and whose [[Get]] and [[Set]] attributes are set to
            //      the value of the corresponding field in Desc if Desc has that field, or to the
            //      attribute's default value otherwise.
            break :blk .{
                .value_or_accessor = .{
                    .accessor = .{
                        .get = descriptor.get orelse @as(?*Object, null),
                        .set = descriptor.set orelse @as(?*Object, null),
                    },
                },
                .attributes = .{
                    .writable = false,
                    .enumerable = enumerable,
                    .configurable = configurable,
                },
            };
        }
        // b. Else if IsAccessorDescriptor(current) is true and IsDataDescriptor(Desc) is true, then
        else if (current.isAccessorDescriptor() and descriptor.isDataDescriptor()) blk: {
            // i. If Desc has a [[Configurable]] field, let configurable be Desc.[[Configurable]];
            //    else let configurable be current.[[Configurable]].
            const configurable = descriptor.configurable orelse current.configurable.?;

            // ii. If Desc has a [[Enumerable]] field, let enumerable be Desc.[[Enumerable]]; else
            //     let enumerable be current.[[Enumerable]].
            const enumerable = descriptor.enumerable orelse current.enumerable.?;

            // iii. Replace the property named P of object O with a data property whose
            //      [[Configurable]] and [[Enumerable]] attributes are set to configurable and
            //      enumerable, respectively, and whose [[Value]] and [[Writable]] attributes are
            //      set to the value of the corresponding field in Desc if Desc has that field, or
            //      to the attribute's default value otherwise.
            break :blk .{
                .value_or_accessor = .{
                    .value = descriptor.value orelse .undefined,
                },
                .attributes = .{
                    .writable = descriptor.writable orelse false,
                    .enumerable = enumerable,
                    .configurable = configurable,
                },
            };
        }
        // c. Else,
        else blk: {
            // i. For each field of Desc, set the corresponding attribute of the property named P
            //    of object O to the value of the field.
            if (current.isDataDescriptor()) {
                break :blk .{
                    .value_or_accessor = .{
                        .value = descriptor.value orelse current.value.?,
                    },
                    .attributes = .{
                        .writable = descriptor.writable orelse current.writable.?,
                        .enumerable = descriptor.enumerable orelse current.enumerable.?,
                        .configurable = descriptor.configurable orelse current.configurable.?,
                    },
                };
            } else {
                std.debug.assert(current.isAccessorDescriptor());
                break :blk .{
                    .value_or_accessor = .{
                        .accessor = .{
                            .get = descriptor.get orelse current.get.?,
                            .set = descriptor.set orelse current.set.?,
                        },
                    },
                    .attributes = .{
                        .writable = false,
                        .enumerable = descriptor.enumerable orelse current.enumerable.?,
                        .configurable = descriptor.configurable orelse current.configurable.?,
                    },
                };
            }
        };

        try object.property_storage.set(
            allocator,
            property_key,
            property_descriptor,
        );
    }

    // 7. Return true.
    return true;
}

/// 10.1.7 [[HasProperty]] ( P )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-hasproperty-p
fn hasProperty(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Return ? OrdinaryHasProperty(O, P).
    return ordinaryHasProperty(agent, object, property_key);
}

/// 10.1.7.1 OrdinaryHasProperty ( O, P )
/// https://tc39.es/ecma262/#sec-ordinaryhasproperty
pub fn ordinaryHasProperty(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
) Agent.Error!bool {
    // OPTIMIZATION: Fast path for ordinary objects
    if (object.internal_methods.getOwnProperty == &getOwnProperty and
        object.internal_methods.getPrototypeOf == &getPrototypeOf)
    {
        if (object.property_storage.contains(property_key)) return true;
        const parent = object.prototype() orelse return false;
        return parent.internal_methods.hasProperty(agent, parent, property_key);
    }

    // 1. Let hasOwn be ? O.[[GetOwnProperty]](P).
    const has_own = try object.internal_methods.getOwnProperty(agent, object, property_key);

    // 2. If hasOwn is not undefined, return true.
    if (has_own != null) return true;

    // 3. Let parent be ? O.[[GetPrototypeOf]]().
    const parent = try object.internal_methods.getPrototypeOf(agent, object);

    // 4. If parent is not null, then
    if (parent) |parent_object| {
        // a. Return ? parent.[[HasProperty]](P).
        return parent_object.internal_methods.hasProperty(agent, parent_object, property_key);
    }

    // 5. Return false.
    return false;
}

/// 10.1.8 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-get-p-receiver
fn get(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    receiver: Value,
) Agent.Error!Value {
    // 1. Return ? OrdinaryGet(O, P, Receiver).
    return ordinaryGet(agent, object, property_key, receiver);
}

/// 10.1.8.1 OrdinaryGet ( O, P, Receiver )
/// https://tc39.es/ecma262/#sec-ordinaryget
pub fn ordinaryGet(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    receiver: Value,
) Agent.Error!Value {
    // OPTIMIZATION: Fast path for ordinary objects
    if (object.internal_methods.getOwnProperty == &getOwnProperty and
        object.internal_methods.getPrototypeOf == &getPrototypeOf)
    {
        const property_descriptor = try object.property_storage.getCreateIntrinsicIfNeeded(property_key) orelse {
            const parent = object.prototype() orelse return .undefined;
            return parent.internal_methods.get(agent, parent, property_key, receiver);
        };
        switch (property_descriptor.value_or_accessor) {
            .value => |value| return value,
            .accessor => |accessor| {
                const getter = accessor.get orelse return .undefined;
                return Value.from(getter).callAssumeCallableNoArgs(receiver);
            },
        }
    }

    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try object.internal_methods.getOwnProperty(agent, object, property_key)

    // 2. If desc is undefined, then
    orelse {
        // a. Let parent be ? O.[[GetPrototypeOf]]().
        const parent = try object.internal_methods.getPrototypeOf(agent, object)

        // b. If parent is null, return undefined.
        orelse return .undefined;

        // c. Return ? parent.[[Get]](P, Receiver).
        return parent.internal_methods.get(agent, parent, property_key, receiver);
    };

    // 3. If IsDataDescriptor(desc) is true, return desc.[[Value]].
    if (descriptor.value) |value| {
        std.debug.assert(descriptor.isDataDescriptor());
        return value;
    }

    // 4. Assert: IsAccessorDescriptor(desc) is true.
    std.debug.assert(descriptor.isAccessorDescriptor());

    // 5. Let getter be desc.[[Get]].
    // 6. If getter is undefined, return undefined.
    const getter = descriptor.get.? orelse return .undefined;

    // 7. Return ? Call(getter, Receiver).
    return Value.from(getter).callAssumeCallableNoArgs(receiver);
}

/// 10.1.9 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-set-p-v-receiver
fn set(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool {
    // 1. Return ? OrdinarySet(O, P, V, Receiver).
    return ordinarySet(agent, object, property_key, value, receiver);
}

/// 10.1.9.1 OrdinarySet ( O, P, V, Receiver )
/// https://tc39.es/ecma262/#sec-ordinaryset
pub fn ordinarySet(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool {
    // OPTIMIZATION: Fast path for ordinary objects and regular properties
    // TODO: Add a fast path for indexed properties
    if (!property_key.isArrayIndex() and
        receiver.isObject() and object == receiver.asObject() and
        object.internal_methods.getOwnProperty == &getOwnProperty and
        object.internal_methods.getPrototypeOf == &getPrototypeOf and
        object.internal_methods.isExtensible == &isExtensible and
        object.internal_methods.defineOwnProperty == &defineOwnProperty)
    {
        const property_metadata = object.property_storage.shape.properties.get(property_key) orelse {
            if (object.prototype()) |parent| {
                return parent.internal_methods.set(agent, parent, property_key, value, receiver);
            }
            if (!object.extensible()) return false;
            try object.property_storage.set(agent.gc_allocator, property_key, .{
                .value_or_accessor = .{
                    .value = value,
                },
                .attributes = .all,
            });
            return true;
        };
        switch (property_metadata.index) {
            .value => |index| {
                if (!property_metadata.attributes.writable) return false;
                object.property_storage.values.items[@intFromEnum(index)] = value;
            },
            .accessor => |index| {
                const accessor = object.property_storage.accessors.items[@intFromEnum(index)];
                const setter = accessor.set orelse return false;
                _ = try Value.from(setter).callAssumeCallable(receiver, &.{value});
            },
        }
        return true;
    }

    // 1. Let ownDesc be ? O.[[GetOwnProperty]](P).
    const own_descriptor = try object.internal_methods.getOwnProperty(agent, object, property_key);

    // 2. Return ? OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
    return ordinarySetWithOwnDescriptor(
        agent,
        object,
        property_key,
        value,
        receiver,
        own_descriptor,
    );
}

/// 10.1.9.2 OrdinarySetWithOwnDescriptor ( O, P, V, Receiver, ownDesc )
/// https://tc39.es/ecma262/#sec-ordinarysetwithowndescriptor
pub fn ordinarySetWithOwnDescriptor(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    value: Value,
    receiver_value: Value,
    maybe_own_descriptor: ?PropertyDescriptor,
) Agent.Error!bool {
    // 1. If ownDesc is undefined, then
    const own_descriptor = maybe_own_descriptor orelse blk: {
        // a. Let parent be ? O.[[GetPrototypeOf]]().
        const parent = try object.internal_methods.getPrototypeOf(agent, object);

        // b. If parent is not null, then
        if (parent) |parent_object| {
            // i. Return ? parent.[[Set]](P, V, Receiver).
            return parent_object.internal_methods.set(
                agent,
                parent_object,
                property_key,
                value,
                receiver_value,
            );
        }
        // c. Else,
        else {
            // i. Set ownDesc to the PropertyDescriptor {
            //      [[Value]]: undefined, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true
            //    }.
            break :blk PropertyDescriptor{
                .value = .undefined,
                .writable = true,
                .enumerable = true,
                .configurable = true,
            };
        }
    };

    // 2. If IsDataDescriptor(ownDesc) is true, then
    if (own_descriptor.isDataDescriptor()) {
        // a. If ownDesc.[[Writable]] is false, return false.
        if (own_descriptor.writable == false) return false;

        // b. If Receiver is not an Object, return false.
        if (!receiver_value.isObject()) return false;
        const receiver = receiver_value.asObject();

        // c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
        const maybe_existing_descriptor = try receiver.internal_methods.getOwnProperty(
            agent,
            receiver,
            property_key,
        );

        // d. If existingDescriptor is not undefined, then
        if (maybe_existing_descriptor) |existing_descriptor| {
            // i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
            if (existing_descriptor.isAccessorDescriptor()) return false;

            // ii. If existingDescriptor.[[Writable]] is false, return false.
            if (existing_descriptor.writable == false) return false;

            // iii. Let valueDesc be the PropertyDescriptor { [[Value]]: V }.
            const value_descriptor: PropertyDescriptor = .{ .value = value };

            // iv. Return ? Receiver.[[DefineOwnProperty]](P, valueDesc).
            return receiver.internal_methods.defineOwnProperty(
                agent,
                receiver,
                property_key,
                value_descriptor,
            );
        }
        // e. Else,
        else {
            // i. Assert: Receiver does not currently have a property P.
            std.debug.assert(!receiver.property_storage.contains(property_key));

            // ii. Return ? CreateDataProperty(Receiver, P, V).
            return receiver.createDataProperty(property_key, value);
        }
    }

    // 3. Assert: IsAccessorDescriptor(ownDesc) is true.
    std.debug.assert(own_descriptor.isAccessorDescriptor());

    // 4. Let setter be ownDesc.[[Set]].
    // 5. If setter is undefined, return false.
    const setter = own_descriptor.set.? orelse return false;

    // 6. Perform ? Call(setter, Receiver, « V »).
    _ = try Value.from(setter).callAssumeCallable(receiver_value, &.{value});

    // 7. Return true.
    return true;
}

/// 10.1.10 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-delete-p
fn delete(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Return ? OrdinaryDelete(O, P).
    return ordinaryDelete(agent, object, property_key);
}

/// 10.1.10.1 OrdinaryDelete ( O, P )
/// https://tc39.es/ecma262/#sec-ordinarydelete
pub fn ordinaryDelete(agent: *Agent, object: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try object.internal_methods.getOwnProperty(agent, object, property_key);

    // 2. If desc is undefined, return true.
    if (descriptor == null) return true;

    // 3. If desc.[[Configurable]] is true, then
    if (descriptor.?.configurable == true) {
        // a. Remove the own property with name P from O.
        try object.property_storage.remove(agent.gc_allocator, property_key);

        // b. Return true.
        return true;
    }

    // 4. Return false.
    return false;
}

/// 10.1.11 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-ownpropertykeys
fn ownPropertyKeys(
    agent: *Agent,
    object: *Object,
) std.mem.Allocator.Error!std.ArrayListUnmanaged(PropertyKey) {
    // 1. Return OrdinaryOwnPropertyKeys(O).
    return ordinaryOwnPropertyKeys(agent, object);
}

/// 10.1.11.1 OrdinaryOwnPropertyKeys ( O )
/// https://tc39.es/ecma262/#sec-ordinaryownpropertykeys
pub fn ordinaryOwnPropertyKeys(
    agent: *Agent,
    object: *Object,
) std.mem.Allocator.Error!std.ArrayListUnmanaged(PropertyKey) {
    // 1. Let keys be a new empty List.
    var keys = try std.ArrayListUnmanaged(PropertyKey).initCapacity(
        agent.gc_allocator,
        object.property_storage.count(),
    );

    // 2. For each own property key P of O such that P is an array index, in ascending numeric
    //    index order, do
    //     a. Append P to keys.
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .sparse => |sparse| {
            var it = sparse.keyIterator();
            while (it.next()) |index| {
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index.*)));
                keys.appendAssumeCapacity(property_key);
            }
            std.mem.sortUnstable(PropertyKey, keys.items, {}, struct {
                fn lessThanFn(_: void, a: PropertyKey, b: PropertyKey) bool {
                    return a.integer_index < b.integer_index;
                }
            }.lessThanFn);
        },
        else => {
            for (0..object.property_storage.indexed_properties.count()) |index| {
                const property_key = PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(index)));
                keys.appendAssumeCapacity(property_key);
            }
        },
    }

    // 3. For each own property key P of O such that P is a String and P is not an array index, in
    //    ascending chronological order of property creation, do
    for (object.property_storage.shape.properties.keys()) |property_key| {
        if (property_key == .string or property_key == .integer_index) {
            std.debug.assert(!property_key.isArrayIndex());

            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 4. For each own property key P of O such that P is a Symbol, in ascending chronological
    //    order of property creation, do
    for (object.property_storage.shape.properties.keys()) |property_key| {
        if (property_key == .symbol) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 5. Return keys.
    return keys;
}

pub fn ordinaryObjectCreate(agent: *Agent, prototype: ?*Object) std.mem.Allocator.Error!*Object {
    return ordinaryObjectCreateWithType(builtins.Object, agent, prototype, {});
}

/// 10.1.12 OrdinaryObjectCreate ( proto [ , additionalInternalSlotsList ] )
/// https://tc39.es/ecma262/#sec-ordinaryobjectcreate
pub fn ordinaryObjectCreateWithType(
    comptime T: type,
    agent: *Agent,
    prototype: ?*Object,
    fields: T.Fields,
) std.mem.Allocator.Error!*Object {
    // 1. Let internalSlotsList be « [[Prototype]], [[Extensible]] ».
    // 2. If additionalInternalSlotsList is present, set internalSlotsList to the list-concatenation
    //    of internalSlotsList and additionalInternalSlotsList.

    // 3. Let O be MakeBasicObject(internalSlotsList).
    // 4. Set O.[[Prototype]] to proto.
    // 5. Return O.
    return try T.create(agent, if (T.Fields != void) .{
        .prototype = prototype,
        .fields = fields,
    } else .{
        .prototype = prototype,
    });
}

/// 10.1.13 OrdinaryCreateFromConstructor ( constructor, intrinsicDefaultProto [ , internalSlotsList ] )
/// https://tc39.es/ecma262/#sec-ordinarycreatefromconstructor
pub fn ordinaryCreateFromConstructor(
    comptime T: type,
    agent: *Agent,
    constructor: *Object,
    comptime intrinsic_default_proto: []const u8,
    fields: T.Fields,
) Agent.Error!*Object {
    // 1. Assert: intrinsicDefaultProto is this specification's name of an intrinsic
    //    object. The corresponding object must be an intrinsic that is intended to be used
    //    as the [[Prototype]] value of an object.
    comptime std.debug.assert(@hasDecl(Realm.Intrinsics, intrinsic_default_proto));

    // 2. Let proto be ? GetPrototypeFromConstructor(constructor, intrinsicDefaultProto).
    const prototype = try getPrototypeFromConstructor(constructor, intrinsic_default_proto);

    // 3. If internalSlotsList is present, let slotsList be internalSlotsList.
    // 4. Else, let slotsList be a new empty List.
    // 5. Return OrdinaryObjectCreate(proto, slotsList).
    return ordinaryObjectCreateWithType(T, agent, prototype, fields);
}

/// 10.1.14 GetPrototypeFromConstructor ( constructor, intrinsicDefaultProto )
/// https://tc39.es/ecma262/#sec-getprototypefromconstructor
pub fn getPrototypeFromConstructor(
    constructor: *Object,
    comptime intrinsic_default_proto: []const u8,
) Agent.Error!*Object {
    // 1. Assert: intrinsicDefaultProto is this specification's name of an intrinsic object. The
    //    corresponding object must be an intrinsic that is intended to be used as the
    //    [[Prototype]] value of an object.
    comptime std.debug.assert(@hasDecl(Realm.Intrinsics, intrinsic_default_proto));

    // 2. Let proto be ? Get(constructor, "prototype").
    const prototype = try constructor.get(PropertyKey.from("prototype"));

    const prototype_object = switch (prototype.type()) {
        .object => prototype.asObject(),

        // 3. If proto is not an Object, then
        else => blk: {
            // a. Let realm be ? GetFunctionRealm(constructor).
            const realm = try constructor.getFunctionRealm();

            // b. Set proto to realm's intrinsic object named intrinsicDefaultProto.
            break :blk try @field(Realm.Intrinsics, intrinsic_default_proto)(&realm.intrinsics);
        },
    };

    // 4. Return proto.
    return prototype_object;
}
