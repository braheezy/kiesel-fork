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

const Self = @This();

// Only export these in a namespace so it's clear what they are.
pub const ordinary_internal_methods = struct {
    pub const getPrototypeOf = Self.getPrototypeOf;
    pub const setPrototypeOf = Self.setPrototypeOf;
    pub const isExtensible = Self.isExtensible;
    pub const preventExtensions = Self.preventExtensions;
    pub const getOwnProperty = Self.getOwnProperty;
    pub const defineOwnProperty = Self.defineOwnProperty;
    pub const hasProperty = Self.hasProperty;
    pub const get = Self.get;
    pub const set = Self.set;
    pub const delete = Self.delete;
    pub const ownPropertyKeys = Self.ownPropertyKeys;
};

/// 10.1.1 [[GetPrototypeOf]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-getprototypeof
fn getPrototypeOf(object: Object) !?Object {
    // 1. Return OrdinaryGetPrototypeOf(O).
    return ordinaryGetPrototypeOf(object);
}

/// 10.1.1.1 OrdinaryGetPrototypeOf ( O )
/// https://tc39.es/ecma262/#sec-ordinarygetprototypeof
pub fn ordinaryGetPrototypeOf(object: Object) ?Object {
    // 1. Return O.[[Prototype]].
    return object.prototype().*;
}

/// 10.1.2 [[SetPrototypeOf]] ( V )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-setprototypeof-v
fn setPrototypeOf(object: Object, prototype: ?Object) !bool {
    // 1. Return OrdinarySetPrototypeOf(O, V).
    return ordinarySetPrototypeOf(object, prototype);
}

/// 10.1.2.1 OrdinarySetPrototypeOf ( O, V )
/// https://tc39.es/ecma262/#sec-ordinarysetprototypeof
pub fn ordinarySetPrototypeOf(object: Object, prototype: ?Object) bool {
    // 1. Let current be O.[[Prototype]].
    const current = object.prototype().*;

    // 2. If SameValue(V, current) is true, return true.
    if (prototype) |prototype_object| {
        if (current) |current_object| {
            if (current_object.ptr == prototype_object.ptr)
                return true;
        }
    } else if (current == null) {
        return true;
    }

    // 3. Let extensible be O.[[Extensible]].
    const extensible = object.extensible().*;

    // 4. If extensible is false, return false.
    if (!extensible)
        return false;

    // 5. Let p be V.
    var parent_prototype = prototype;

    // 6. Let done be false.
    // 7. Repeat, while done is false,
    while (parent_prototype) |parent_prototype_object| {
        // a. If p is null, then
        //     i. Set done to true.

        // b. Else if SameValue(p, O) is true, then
        if (parent_prototype_object.ptr == object.ptr) {
            // i. Return false.
            return false;
        }

        // c. Else,
        // i. If p.[[GetPrototypeOf]] is not the ordinary object internal method defined in 10.1.1, set done to true.
        if (parent_prototype_object.internalMethods().getPrototypeOf != getPrototypeOf)
            break;

        // ii. Else, set p to p.[[Prototype]].
        parent_prototype = parent_prototype_object.prototype().*;
    }

    // 8. Set O.[[Prototype]] to V.
    object.prototype().* = prototype;

    // 9. Return true.
    return true;
}

/// 10.1.3 [[IsExtensible]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-isextensible
fn isExtensible(object: Object) !bool {
    // 1. Return OrdinaryIsExtensible(O).
    return ordinaryIsExtensible(object);
}

/// 10.1.3.1 OrdinaryIsExtensible ( O )
/// https://tc39.es/ecma262/#sec-ordinaryisextensible
pub fn ordinaryIsExtensible(object: Object) bool {
    // 1. Return O.[[Extensible]].
    return object.extensible().*;
}

/// 10.1.4 [[PreventExtensions]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-preventextensions
fn preventExtensions(object: Object) !bool {
    // 1. Return OrdinaryPreventExtensions(O).
    return ordinaryPreventExtensions(object);
}

/// 10.1.4.1 OrdinaryPreventExtensions ( O )
/// https://tc39.es/ecma262/#sec-ordinarypreventextensions
pub fn ordinaryPreventExtensions(object: Object) bool {
    // 1. Set O.[[Extensible]] to false.
    object.extensible().* = false;

    // 2. Return true.
    return true;
}

/// 10.1.5 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-getownproperty-p
fn getOwnProperty(object: Object, property_key: PropertyKey) !?PropertyDescriptor {
    // 1. Return OrdinaryGetOwnProperty(O, P).
    return ordinaryGetOwnProperty(object, property_key);
}

/// 10.1.5.1 OrdinaryGetOwnProperty ( O, P )
/// https://tc39.es/ecma262/#sec-ordinarygetownproperty
pub fn ordinaryGetOwnProperty(object: Object, property_key: PropertyKey) ?PropertyDescriptor {
    // 1. If O does not have an own property with key P, return undefined.
    // 3. Let X be O's own property whose key is P.
    const x = object.propertyStorage().get(property_key) orelse return null;

    // 2. Let D be a newly created Property Descriptor with no fields.
    var descriptor = PropertyDescriptor{};

    // 4. If X is a data property, then
    if (x.isDataDescriptor()) {
        // a. Set D.[[Value]] to the value of X's [[Value]] attribute.
        descriptor.value = x.value;

        // b. Set D.[[Writable]] to the value of X's [[Writable]] attribute.
        descriptor.writable = x.writable;
    }
    // 5. Else,
    else {
        // a. Assert: X is an accessor property.
        std.debug.assert(x.isAccessorDescriptor());

        // b. Set D.[[Get]] to the value of X's [[Get]] attribute.
        descriptor.get = x.get;

        // c. Set D.[[Set]] to the value of X's [[Set]] attribute.
        descriptor.set = x.set;
    }

    // 6. Set D.[[Enumerable]] to the value of X's [[Enumerable]] attribute.
    descriptor.enumerable = x.enumerable;

    // 7. Set D.[[Configurable]] to the value of X's [[Configurable]] attribute.
    descriptor.configurable = x.configurable;

    // 8. Return D.
    return descriptor;
}

/// 10.1.6 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-defineownproperty-p-desc
fn defineOwnProperty(object: Object, property_key: PropertyKey, property_descriptor: PropertyDescriptor) !bool {
    // 1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    return ordinaryDefineOwnProperty(object, property_key, property_descriptor);
}

/// 10.1.6.1 OrdinaryDefineOwnProperty ( O, P, Desc )
/// https://tc39.es/ecma262/#sec-ordinarydefineownproperty
pub fn ordinaryDefineOwnProperty(object: Object, property_key: PropertyKey, property_descriptor: PropertyDescriptor) !bool {
    // 1. Let current be ? O.[[GetOwnProperty]](P).
    const current = try object.internalMethods().getOwnProperty(object, property_key);

    // 2. Let extensible be ? IsExtensible(O).
    const extensible = try object.isExtensible();

    // 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
    return validateAndApplyPropertyDescriptor(object, property_key, extensible, property_descriptor, current);
}

/// 10.1.6.3 ValidateAndApplyPropertyDescriptor ( O, P, extensible, Desc, current )
/// https://tc39.es/ecma262/#sec-validateandapplypropertydescriptor
fn validateAndApplyPropertyDescriptor(object: Object, property_key: PropertyKey, extensible: bool, property_descriptor: PropertyDescriptor, current: ?PropertyDescriptor) !bool {
    _ = current;
    _ = extensible;
    // TODO: Implement me :^)
    try object.propertyStorage().set(property_key, property_descriptor);
    return true;
}

/// 10.1.7 [[HasProperty]] ( P )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-hasproperty-p
fn hasProperty(object: Object, property_key: PropertyKey) !bool {
    // 1. Return ? OrdinaryHasProperty(O, P).
    return ordinaryHasProperty(object, property_key);
}

/// 10.1.7.1 OrdinaryHasProperty ( O, P )
/// https://tc39.es/ecma262/#sec-ordinaryhasproperty
pub fn ordinaryHasProperty(object: Object, property_key: PropertyKey) !bool {
    // 1. Let hasOwn be ? O.[[GetOwnProperty]](P).
    const has_own = try object.internalMethods().getOwnProperty(object, property_key);

    // 2. If hasOwn is not undefined, return true.
    if (has_own != null)
        return true;

    // 3. Let parent be ? O.[[GetPrototypeOf]]().
    const parent = try object.internalMethods().getPrototypeOf(object);

    // 4. If parent is not null, then
    if (parent) |parent_object| {
        // a. Return ? parent.[[HasProperty]](P).
        return parent_object.internalMethods().hasProperty(parent_object, property_key);
    }

    // 5. Return false.
    return false;
}

/// 10.1.8 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-get-p-receiver
fn get(object: Object, property_key: PropertyKey, receiver: Value) !Value {
    // 1. Return ? OrdinaryGet(O, P, Receiver).
    return ordinaryGet(object, property_key, receiver);
}

/// 10.1.8.1 OrdinaryGet ( O, P, Receiver )
/// https://tc39.es/ecma262/#sec-ordinaryget
pub fn ordinaryGet(object: Object, property_key: PropertyKey, receiver: Value) !Value {
    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try object.internalMethods().getOwnProperty(object, property_key)

    // 2. If desc is undefined, then
    orelse {
        // a. Let parent be ? O.[[GetPrototypeOf]]().
        const parent = try object.internalMethods().getPrototypeOf(object)

        // b. If parent is null, return undefined.
        orelse return .undefined;

        // c. Return ? parent.[[Get]](P, Receiver).
        return parent.internalMethods().get(parent, property_key, receiver);
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
    const getter = descriptor.get orelse return .undefined;

    // 7. Return ? Call(getter, Receiver).
    return Value.fromObject(getter).callAssumeCallableNoArgs(receiver);
}

/// 10.1.9 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-set-p-v-receiver
fn set(object: Object, property_key: PropertyKey, value: Value, receiver: Value) !bool {
    // 1. Return ? OrdinarySet(O, P, V, Receiver).
    return ordinarySet(object, property_key, value, receiver);
}

/// 10.1.9.1 OrdinarySet ( O, P, V, Receiver )
/// https://tc39.es/ecma262/#sec-ordinaryset
pub fn ordinarySet(object: Object, property_key: PropertyKey, value: Value, receiver: Value) !bool {
    // 1. Let ownDesc be ? O.[[GetOwnProperty]](P).
    const own_descriptor = try object.internalMethods().getOwnProperty(object, property_key);

    // 2. Return ? OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
    return ordinarySetWithOwnDescriptor(object, property_key, value, receiver, own_descriptor);
}

/// 10.1.9.2 OrdinarySetWithOwnDescriptor ( O, P, V, Receiver, ownDesc )
/// https://tc39.es/ecma262/#sec-ordinarysetwithowndescriptor
pub fn ordinarySetWithOwnDescriptor(
    object: Object,
    property_key: PropertyKey,
    value: Value,
    receiver_value: Value,
    maybe_own_descriptor: ?PropertyDescriptor,
) !bool {
    var own_descriptor: PropertyDescriptor = undefined;

    // 1. If ownDesc is undefined, then
    if (maybe_own_descriptor == null) {
        // a. Let parent be ? O.[[GetPrototypeOf]]().
        const parent = try object.internalMethods().getPrototypeOf(object);

        // b. If parent is not null, then
        if (parent) |parent_object| {
            // i. Return ? parent.[[Set]](P, V, Receiver).
            return parent_object.internalMethods().set(parent_object, property_key, value, receiver_value);
        }
        // c. Else,
        else {
            // i. Set ownDesc to the PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }.
            own_descriptor = PropertyDescriptor{
                .value = .undefined,
                .writable = true,
                .enumerable = true,
                .configurable = true,
            };
        }
    } else {
        own_descriptor = maybe_own_descriptor.?;
    }

    // 2. If IsDataDescriptor(ownDesc) is true, then
    if (own_descriptor.isDataDescriptor()) {
        // a. If ownDesc.[[Writable]] is false, return false.
        if (own_descriptor.writable == false)
            return false;

        // b. If Receiver is not an Object, return false.
        if (receiver_value != .object)
            return false;
        const receiver = receiver_value.object;

        // c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
        const maybe_existing_descriptor = try receiver.internalMethods().getOwnProperty(receiver, property_key);

        // d. If existingDescriptor is not undefined, then
        if (maybe_existing_descriptor) |existing_descriptor| {
            // i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
            if (existing_descriptor.isAccessorDescriptor())
                return false;

            // ii. If existingDescriptor.[[Writable]] is false, return false.
            if (existing_descriptor.writable == false)
                return false;

            // iii. Let valueDesc be the PropertyDescriptor { [[Value]]: V }.
            const value_descriptor = PropertyDescriptor{ .value = value };

            // iv. Return ? Receiver.[[DefineOwnProperty]](P, valueDesc).
            return receiver.internalMethods().defineOwnProperty(receiver, property_key, value_descriptor);
        }
        // e. Else,
        else {
            // i. Assert: Receiver does not currently have a property P.
            std.debug.assert(!receiver.propertyStorage().has(property_key));

            // ii. Return ? CreateDataProperty(Receiver, P, V).
            return receiver.createDataProperty(property_key, value);
        }
    }

    // 3. Assert: IsAccessorDescriptor(ownDesc) is true.
    std.debug.assert(own_descriptor.isAccessorDescriptor());

    // 4. Let setter be ownDesc.[[Set]].
    // 5. If setter is undefined, return false.
    const setter = own_descriptor.set orelse return false;

    // 6. Perform ? Call(setter, Receiver, « V »).
    _ = try Value.fromObject(setter).callAssumeCallable(receiver_value, &[_]Value{value});

    // 7. Return true.
    return true;
}

/// 10.1.10 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-delete-p
fn delete(object: Object, property_key: PropertyKey) !bool {
    // 1. Return ? OrdinaryDelete(O, P).
    return ordinaryDelete(object, property_key);
}

/// 10.1.10.1 OrdinaryDelete ( O, P )
/// https://tc39.es/ecma262/#sec-ordinarydelete
pub fn ordinaryDelete(object: Object, property_key: PropertyKey) !bool {
    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try object.internalMethods().getOwnProperty(object, property_key);

    // 2. If desc is undefined, return true.
    if (descriptor == null)
        return true;

    // 3. If desc.[[Configurable]] is true, then
    if (descriptor.?.configurable == true) {
        // a. Remove the own property with name P from O.
        object.propertyStorage().remove(property_key);

        // b. Return true.
        return true;
    }

    // 4. Return false.
    return false;
}

/// 10.1.11 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-ownpropertykeys
fn ownPropertyKeys(object: Object) !std.ArrayList(PropertyKey) {
    // 1. Return OrdinaryOwnPropertyKeys(O).
    return ordinaryOwnPropertyKeys(object);
}

/// 10.1.11.1 OrdinaryOwnPropertyKeys ( O )
/// https://tc39.es/ecma262/#sec-ordinaryownpropertykeys
pub fn ordinaryOwnPropertyKeys(object: Object) !std.ArrayList(PropertyKey) {
    const agent = object.agent();
    const property_storage_hash_map = object.propertyStorage().hash_map;

    // 1. Let keys be a new empty List.
    var keys = std.ArrayList(PropertyKey).init(agent.allocator);
    try keys.ensureTotalCapacity(property_storage_hash_map.count());

    // 2. For each own property key P of O such that P is an array index, in ascending numeric index order, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key.isArrayIndex()) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 3. For each own property key P of O such that P is a String and P is not an array index, in ascending chronological order of property creation, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key == .string or (property_key == .integer_index and !property_key.isArrayIndex())) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 4. For each own property key P of O such that P is a Symbol, in ascending chronological order of property creation, do
    for (object.propertyStorage().hash_map.keys()) |property_key| {
        if (property_key == .symbol) {
            // a. Append P to keys.
            keys.appendAssumeCapacity(property_key);
        }
    }

    // 5. Return keys.
    return keys;
}

pub fn ordinaryObjectCreate(agent: *Agent, prototype: ?Object) !Object {
    return ordinaryObjectCreateWithType(builtins.Object, agent, prototype);
}

/// 10.1.12 OrdinaryObjectCreate ( proto [ , additionalInternalSlotsList ] )
/// https://tc39.es/ecma262/#sec-ordinaryobjectcreate
pub fn ordinaryObjectCreateWithType(comptime T: type, agent: *Agent, prototype: ?Object) !Object {
    // 1. Let internalSlotsList be « [[Prototype]], [[Extensible]] ».
    // 2. If additionalInternalSlotsList is present, set internalSlotsList to the list-concatenation
    //    of internalSlotsList and additionalInternalSlotsList.

    // 3. Let O be MakeBasicObject(internalSlotsList).
    // 4. Set O.[[Prototype]] to proto.
    // 5. Return O.
    return try T.create(agent, if (T.Fields != void) .{
        .prototype = prototype,
        .fields = undefined,
    } else .{
        .prototype = prototype,
    });
}

/// 10.1.13 OrdinaryCreateFromConstructor ( constructor, intrinsicDefaultProto [ , internalSlotsList ] )
/// https://tc39.es/ecma262/#sec-ordinarycreatefromconstructor
pub fn ordinaryCreateFromConstructor(comptime T: type, agent: *Agent, constructor: Object, comptime intrinsic_default_proto: []const u8) !Object {
    // 1. Assert: intrinsicDefaultProto is this specification's name of an intrinsic
    //    object. The corresponding object must be an intrinsic that is intended to be used
    //    as the [[Prototype]] value of an object.
    comptime std.debug.assert(@hasField(Realm.Intrinsics, intrinsic_default_proto));

    // 2. Let proto be ? GetPrototypeFromConstructor(constructor, intrinsicDefaultProto).
    const prototype = try getPrototypeFromConstructor(constructor, intrinsic_default_proto);

    // 3. If internalSlotsList is present, let slotsList be internalSlotsList.
    // 4. Else, let slotsList be a new empty List.
    // 5. Return OrdinaryObjectCreate(proto, slotsList).
    return ordinaryObjectCreateWithType(T, agent, prototype);
}

/// 10.1.14 GetPrototypeFromConstructor ( constructor, intrinsicDefaultProto )
/// https://tc39.es/ecma262/#sec-getprototypefromconstructor
pub fn getPrototypeFromConstructor(constructor: Object, comptime intrinsic_default_proto: []const u8) !?Object {
    // 1. Assert: intrinsicDefaultProto is this specification's name of an intrinsic object. The
    //    corresponding object must be an intrinsic that is intended to be used as the
    //    [[Prototype]] value of an object.
    comptime std.debug.assert(@hasField(Realm.Intrinsics, intrinsic_default_proto));

    // 2. Let proto be ? Get(constructor, "prototype").
    const prototype = try constructor.get(PropertyKey.fromString("prototype"));

    const prototype_object: ?Object = switch (prototype) {
        .object => prototype.object,

        // 3. If proto is not an Object, then
        else => blk: {
            // a. Let realm be ? GetFunctionRealm(constructor).
            const realm = try constructor.getFunctionRealm();

            // b. Set proto to realm's intrinsic object named intrinsicDefaultProto.
            break :blk @field(realm.intrinsics, intrinsic_default_proto);
        },
    };

    // 4. Return proto.
    return prototype_object;
}
