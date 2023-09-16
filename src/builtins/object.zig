//! 20.1 Object Objects
//! https://tc39.es/ecma262/#sec-object-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const immutable_prototype = @import("immutable_prototype.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object_ = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

/// 20.1.1 The Object Constructor
/// https://tc39.es/ecma262/#sec-object-constructor
pub const ObjectConstructor = struct {
    pub fn create(realm: *Realm) !Object_ {
        // 20.1.2 Properties of the Object Constructor
        // https://tc39.es/ecma262/#sec-properties-of-the-object-constructor
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Object",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "assign", assign, 2, realm);
        try defineBuiltinFunction(object, "create", create_, 2, realm);
        try defineBuiltinFunction(object, "defineProperties", defineProperties, 2, realm);
        try defineBuiltinFunction(object, "defineProperty", defineProperty, 3, realm);
        try defineBuiltinFunction(object, "entries", entries, 1, realm);
        try defineBuiltinFunction(object, "freeze", freeze, 1, realm);
        try defineBuiltinFunction(object, "getOwnPropertyDescriptor", getOwnPropertyDescriptor, 2, realm);
        try defineBuiltinFunction(object, "getOwnPropertyDescriptors", getOwnPropertyDescriptors, 1, realm);
        try defineBuiltinFunction(object, "getOwnPropertyNames", getOwnPropertyNames, 1, realm);
        try defineBuiltinFunction(object, "getOwnPropertySymbols", getOwnPropertySymbols, 1, realm);
        try defineBuiltinFunction(object, "getPrototypeOf", getPrototypeOf, 1, realm);
        try defineBuiltinFunction(object, "hasOwn", hasOwn, 2, realm);
        try defineBuiltinFunction(object, "is", is, 2, realm);
        try defineBuiltinFunction(object, "isExtensible", isExtensible, 1, realm);
        try defineBuiltinFunction(object, "isFrozen", isFrozen, 1, realm);
        try defineBuiltinFunction(object, "isSealed", isSealed, 1, realm);
        try defineBuiltinFunction(object, "keys", keys, 1, realm);
        try defineBuiltinFunction(object, "preventExtensions", preventExtensions, 1, realm);
        try defineBuiltinFunction(object, "seal", seal, 1, realm);
        try defineBuiltinFunction(object, "setPrototypeOf", setPrototypeOf, 2, realm);
        try defineBuiltinFunction(object, "values", values, 1, realm);

        // 20.1.2.20 Object.prototype
        // https://tc39.es/ecma262/#sec-object.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Object.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.1.3.1 Object.prototype.constructor
        // https://tc39.es/ecma262/#sec-object.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Object.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.1.1.1 Object ( [ value ] )
    /// https://tc39.es/ecma262/#sec-object-value
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object_) !Value {
        const realm = agent.currentRealm();
        const value = arguments.get(0);

        // 1. If NewTarget is neither undefined nor the active function object, then
        if (new_target != null and !new_target.?.sameValue(agent.activeFunctionObject())) {
            // a. Return ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
            return Value.from(try ordinaryCreateFromConstructor(
                Object,
                agent,
                new_target.?,
                "%Object.prototype%",
            ));
        }

        // 2. If value is either undefined or null, return OrdinaryObjectCreate(%Object.prototype%).
        if (value == .undefined or value == .null) return Value.from(try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        ));

        // 3. Return ! ToObject(value).
        // TODO: Use `catch |err| try noexcept(err)` once Value.toObject() is fully implemented
        return Value.from(try value.toObject(agent));
    }

    /// 20.1.2.1 Object.assign ( target, ...sources )
    /// https://tc39.es/ecma262/#sec-object.assign
    fn assign(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const sources = arguments.values[1..];

        // 1. Let to be ? ToObject(target).
        const to = try target.toObject(agent);

        // 2. If only one argument was passed, return to.
        if (arguments.count() == 1) return Value.from(to);

        // 3. For each element nextSource of sources, do
        for (sources) |next_source| {
            // a. If nextSource is neither undefined nor null, then
            if (next_source != .undefined and next_source != .null) {
                // i. Let from be ! ToObject(nextSource).
                const from = next_source.toObject(agent) catch |err| try noexcept(err);

                // ii. Let keys be ? from.[[OwnPropertyKeys]]().
                const keys_ = try from.internalMethods().ownPropertyKeys(from);
                defer keys_.deinit();

                // iii. For each element nextKey of keys, do
                for (keys_.items) |next_key| {
                    // 1. Let desc be ? from.[[GetOwnProperty]](nextKey).
                    const descriptor = try from.internalMethods().getOwnProperty(from, next_key);

                    // 2. If desc is not undefined and desc.[[Enumerable]] is true, then
                    if (descriptor != null and descriptor.?.enumerable == true) {
                        // a. Let propValue be ? Get(from, nextKey).
                        const property_value = try from.get(next_key);

                        // b. Perform ? Set(to, nextKey, propValue, true).
                        try to.set(next_key, property_value, .throw);
                    }
                }
            }
        }

        // 4. Return to.
        return Value.from(to);
    }

    /// 20.1.2.2 Object.create ( O, Properties )
    /// https://tc39.es/ecma262/#sec-object.create
    fn create_(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);
        const properties = arguments.get(1);

        // 1. If O is not an Object and O is not null, throw a TypeError exception.
        if (object != .object and object != .null) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "{} is not an Object or null",
                    .{object},
                ),
            );
        }

        // 2. Let obj be OrdinaryObjectCreate(O).
        const obj = try ordinaryObjectCreate(agent, if (object == .object) object.object else null);

        // 3. If Properties is not undefined, then
        if (properties != .undefined) {
            // a. Return ? ObjectDefineProperties(obj, Properties).
            return Value.from(try objectDefineProperties(agent, obj, properties));
        }

        // 4. Return obj.
        return Value.from(obj);
    }

    /// 20.1.2.3 Object.defineProperties ( O, Properties )
    /// https://tc39.es/ecma262/#sec-object.defineproperties
    fn defineProperties(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);
        const properties = arguments.get(1);

        // 1. If O is not an Object, throw a TypeError exception.
        if (object != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{object}),
            );
        }

        // 2. Return ? ObjectDefineProperties(O, Properties).
        return Value.from(try objectDefineProperties(agent, object.object, properties));
    }

    /// 20.1.2.3.1 ObjectDefineProperties ( O, Properties )
    /// https://tc39.es/ecma262/#sec-objectdefineproperties
    fn objectDefineProperties(agent: *Agent, object: Object_, properties: Value) !Object_ {
        // 1. Let props be ? ToObject(Properties).
        const props = try properties.toObject(agent);

        // 2. Let keys be ? props.[[OwnPropertyKeys]]().
        const keys_ = try props.internalMethods().ownPropertyKeys(props);
        defer keys_.deinit();

        const Property = struct {
            key: PropertyKey,
            descriptor: PropertyDescriptor,
        };

        // 3. Let descriptors be a new empty List.
        var descriptors = std.ArrayList(Property).init(agent.gc_allocator);
        defer descriptors.deinit();

        // 4. For each element nextKey of keys, do
        for (keys_.items) |next_key| {
            // a. Let propDesc be ? props.[[GetOwnProperty]](nextKey).
            const maybe_property_descriptor = try props.internalMethods().getOwnProperty(props, next_key);

            // b. If propDesc is not undefined and propDesc.[[Enumerable]] is true, then
            if (maybe_property_descriptor) |property_descriptor| if (property_descriptor.enumerable == true) {
                // i. Let descObj be ? Get(props, nextKey).
                const descriptor_object = try props.get(next_key);

                // ii. Let desc be ? ToPropertyDescriptor(descObj).
                const descriptor = try descriptor_object.toPropertyDescriptor(agent);

                // iii. Append the Record { [[Key]]: nextKey, [[Descriptor]]: desc } to descriptors.
                try descriptors.append(.{ .key = next_key, .descriptor = descriptor });
            };
        }

        // 5. For each element property of descriptors, do
        for (descriptors.items) |property| {
            // a. Perform ? DefinePropertyOrThrow(O, property.[[Key]], property.[[Descriptor]]).
            try object.definePropertyOrThrow(property.key, property.descriptor);
        }

        // 6. Return O.
        return object;
    }

    /// 20.1.2.4 Object.defineProperty ( O, P, Attributes )
    /// https://tc39.es/ecma262/#sec-object.defineproperty
    fn defineProperty(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);
        const property = arguments.get(1);
        const attributes = arguments.get(2);

        // 1. If O is not an Object, throw a TypeError exception.
        if (object != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{object}),
            );
        }

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Let desc be ? ToPropertyDescriptor(Attributes).
        const property_descriptor = try attributes.toPropertyDescriptor(agent);

        // 4. Perform ? DefinePropertyOrThrow(O, key, desc).
        try object.object.definePropertyOrThrow(property_key, property_descriptor);

        // 5. Return O.
        return object;
    }

    /// 20.1.2.5 Object.entries ( O )
    /// https://tc39.es/ecma262/#sec-object.entries
    fn entries(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let entryList be ? EnumerableOwnProperties(obj, key+value).
        const entry_list = try obj.enumerableOwnProperties(.@"key+value");
        defer entry_list.deinit();

        // 3. Return CreateArrayFromList(entryList).
        return Value.from(try createArrayFromList(agent, entry_list.items));
    }

    /// 20.1.2.6 Object.freeze ( O )
    /// https://tc39.es/ecma262/#sec-object.freeze
    fn freeze(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (object != .object) return object;

        // 2. Let status be ? SetIntegrityLevel(O, frozen).
        const status = try object.object.setIntegrityLevel(.frozen);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not freeze object");

        // 4. Return O.
        return object;
    }

    /// 20.1.2.8 Object.getOwnPropertyDescriptor ( O, P )
    /// https://tc39.es/ecma262/#sec-object.getownpropertydescriptor
    fn getOwnPropertyDescriptor(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);
        const property = arguments.get(1);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Let desc be ? obj.[[GetOwnProperty]](key).
        const maybe_descriptor = try obj.internalMethods().getOwnProperty(obj, property_key);

        // 4. Return FromPropertyDescriptor(desc).
        if (maybe_descriptor) |descriptor|
            return Value.from(try descriptor.fromPropertyDescriptor(agent))
        else
            return .undefined;
    }

    /// 20.1.2.9 Object.getOwnPropertyDescriptors ( O )
    /// https://tc39.es/ecma262/#sec-object.getownpropertydescriptors
    fn getOwnPropertyDescriptors(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let ownKeys be ? obj.[[OwnPropertyKeys]]().
        const own_keys = try obj.internalMethods().ownPropertyKeys(obj);
        defer own_keys.deinit();

        // 3. Let descriptors be OrdinaryObjectCreate(%Object.prototype%).
        const descriptors = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each element key of ownKeys, do
        for (own_keys.items) |key| {
            // a. Let desc be ? obj.[[GetOwnProperty]](key).
            if (try obj.internalMethods().getOwnProperty(obj, key)) |property_descriptor| {
                // b. Let descriptor be FromPropertyDescriptor(desc).
                const descriptor = try property_descriptor.fromPropertyDescriptor(agent);

                // c. If descriptor is not undefined, perform ! CreateDataPropertyOrThrow(descriptors, key, descriptor).
                descriptors.createDataPropertyOrThrow(
                    key,
                    Value.from(descriptor),
                ) catch |err| try noexcept(err);
            }
        }

        // 5. Return descriptors.
        return Value.from(descriptors);
    }

    /// 20.1.2.10 Object.getOwnPropertyNames ( O )
    /// https://tc39.es/ecma262/#sec-object.getownpropertynames
    fn getOwnPropertyNames(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. Return CreateArrayFromList(? GetOwnPropertyKeys(O, string)).
        const property_keys = try getOwnPropertyKeys(agent, object, .string);
        defer property_keys.deinit();
        return Value.from(
            try createArrayFromListMapToValue(agent, PropertyKey, property_keys.items, struct {
                fn mapFn(agent_: *Agent, property_key: PropertyKey) !Value {
                    return property_key.toValue(agent_);
                }
            }.mapFn),
        );
    }

    /// 20.1.2.11 Object.getOwnPropertySymbols ( O )
    /// https://tc39.es/ecma262/#sec-object.getownpropertysymbols
    fn getOwnPropertySymbols(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. Return CreateArrayFromList(? GetOwnPropertyKeys(O, symbol)).
        const property_keys = try getOwnPropertyKeys(agent, object, .symbol);
        defer property_keys.deinit();
        return Value.from(
            try createArrayFromListMapToValue(agent, PropertyKey, property_keys.items, struct {
                fn mapFn(agent_: *Agent, property_key: PropertyKey) !Value {
                    return property_key.toValue(agent_);
                }
            }.mapFn),
        );
    }

    /// 20.1.2.11.1 GetOwnPropertyKeys ( O, type )
    /// https://tc39.es/ecma262/#sec-getownpropertykeys
    fn getOwnPropertyKeys(
        agent: *Agent,
        object: Value,
        comptime @"type": enum { string, symbol },
    ) !std.ArrayList(PropertyKey) {
        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let keys be ? obj.[[OwnPropertyKeys]]().
        const keys_ = try obj.internalMethods().ownPropertyKeys(obj);

        // 3. Let nameList be a new empty List.
        var name_list = std.ArrayList(PropertyKey).init(agent.gc_allocator);

        // 4. For each element nextKey of keys, do
        for (keys_.items) |next_key| {
            // a. If nextKey is a Symbol and type is symbol, or if nextKey is a String and type is
            //    string, then
            if ((next_key == .symbol and @"type" == .symbol) or
                ((next_key == .string or next_key == .integer_index) and @"type" == .string))
            {
                // i. Append nextKey to nameList.
                try name_list.append(next_key);
            }
        }

        // 5. Return nameList.
        return name_list;
    }

    /// 20.1.2.12 Object.getPrototypeOf ( O )
    /// https://tc39.es/ecma262/#sec-object.getprototypeof
    fn getPrototypeOf(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Return ? obj.[[GetPrototypeOf]]().
        return Value.from(try obj.internalMethods().getPrototypeOf(obj) orelse return .null);
    }

    /// 20.1.2.13 Object.hasOwn ( O, P )
    /// https://tc39.es/ecma262/#sec-object.hasown
    fn hasOwn(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);
        const property = arguments.get(1);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Return ? HasOwnProperty(obj, key).
        return Value.from(try obj.hasOwnProperty(property_key));
    }

    /// 20.1.2.14 Object.is ( value1, value2 )
    /// https://tc39.es/ecma262/#sec-object.is
    fn is(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const value1 = arguments.get(0);
        const value2 = arguments.get(1);

        // 1. Return SameValue(value1, value2).
        return Value.from(sameValue(value1, value2));
    }

    /// 20.1.2.15 Object.isExtensible ( O )
    /// https://tc39.es/ecma262/#sec-object.isextensible
    fn isExtensible(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? IsExtensible(O).
        return Value.from(try object.object.isExtensible());
    }

    /// 20.1.2.16 Object.isFrozen ( O )
    /// https://tc39.es/ecma262/#sec-object.isfrozen
    fn isFrozen(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, frozen).
        return Value.from(try object.object.testIntegrityLevel(.frozen));
    }

    /// 20.1.2.17 Object.isSealed ( O )
    /// https://tc39.es/ecma262/#sec-object.issealed
    fn isSealed(_: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (object != .object) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, sealed).
        return Value.from(try object.object.testIntegrityLevel(.sealed));
    }

    /// 20.1.2.18 Object.keys ( O )
    /// https://tc39.es/ecma262/#sec-object.keys
    fn keys(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let keyList be ? EnumerableOwnProperties(obj, key).
        const key_list = try obj.enumerableOwnProperties(.key);
        defer key_list.deinit();

        // 3. Return CreateArrayFromList(keyList).
        return Value.from(try createArrayFromList(agent, key_list.items));
    }

    /// 20.1.2.19 Object.preventExtensions ( O )
    /// https://tc39.es/ecma262/#sec-object.preventextensions
    fn preventExtensions(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (object != .object) return object;

        // 2. Let status be ? O.[[PreventExtensions]]().
        const status = try object.object.internalMethods().preventExtensions(object.object);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not prevent extensions");

        // 4. Return O.
        return object;
    }

    /// 20.1.2.21 Object.seal ( O )
    /// https://tc39.es/ecma262/#sec-object.seal
    fn seal(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (object != .object) return Value.from(true);

        // 2. Let status be ? SetIntegrityLevel(O, sealed).
        const status = try object.object.setIntegrityLevel(.sealed);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not seal object");

        // 4. Return O.
        return object;
    }

    /// 20.1.2.22 Object.setPrototypeOf ( O, proto )
    /// https://tc39.es/ecma262/#sec-object.seal
    fn setPrototypeOf(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);
        const prototype = arguments.get(1);

        // 1. Set O to ? RequireObjectCoercible(O).
        _ = try object.requireObjectCoercible(agent);

        // 2. If proto is not an Object and proto is not null, throw a TypeError exception.
        if (prototype != .object and prototype != .null) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "{} is not an Object or null",
                    .{prototype},
                ),
            );
        }

        // 3. If O is not an Object, return O.
        if (object != .object) return object;

        // 4. Let status be ? O.[[SetPrototypeOf]](proto).
        const status = try object.object.internalMethods().setPrototypeOf(
            object.object,
            if (prototype == .object) prototype.object else null,
        );

        // 5. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not set prototype");

        // 6. Return O.
        return object;
    }

    /// 20.1.2.23 Object.values ( O )
    /// https://tc39.es/ecma262/#sec-object.values
    fn values(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let valueList be ? EnumerableOwnProperties(obj, value).
        const value_list = try obj.enumerableOwnProperties(.value);
        defer value_list.deinit();

        // 3. Return CreateArrayFromList(valueList).
        return Value.from(try createArrayFromList(agent, value_list.items));
    }
};

/// 20.1.3 Properties of the Object Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object
pub const ObjectPrototype = struct {
    pub fn create(realm: *Realm) !Object_ {
        const object = try createNoinit(realm);
        init(realm, object);
        return object;
    }

    pub fn createNoinit(realm: *Realm) !Object_ {
        return Object.create(realm.agent, .{
            .prototype = null,
            .internal_methods = .{
                .setPrototypeOf = immutable_prototype.setPrototypeOf,
            },
        });
    }

    pub fn init(realm: *Realm, object: Object_) !void {
        try defineBuiltinFunction(object, "hasOwnProperty", hasOwnProperty, 1, realm);
        try defineBuiltinFunction(object, "isPrototypeOf", isPrototypeOf, 1, realm);
        try defineBuiltinFunction(object, "propertyIsEnumerable", propertyIsEnumerable, 1, realm);
        try defineBuiltinFunction(object, "toLocaleString", toLocaleString, 0, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);
    }

    /// 20.1.3.2 Object.prototype.hasOwnProperty ( V )
    /// https://tc39.es/ecma262/#sec-object.prototype.hasownproperty
    fn hasOwnProperty(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const value = arguments.get(0);

        // 1. Let P be ? ToPropertyKey(V).
        const property_key = try value.toPropertyKey(agent);

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 3. Return ? HasOwnProperty(O, P).
        return Value.from(try object.hasOwnProperty(property_key));
    }

    /// 20.1.3.3 Object.prototype.isPrototypeOf ( V )
    /// https://tc39.es/ecma262/#sec-object.prototype.isprototypeof
    fn isPrototypeOf(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const value = arguments.get(0);

        // 1. If V is not an Object, return false.
        if (value != .object) return Value.from(false);

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        var prototype: ?Object_ = value.object;

        // 3. Repeat,
        while (true) {
            // a. Set V to ? V.[[GetPrototypeOf]]().
            prototype = try prototype.?.internalMethods().getPrototypeOf(prototype.?);

            // b. If V is null, return false.
            if (prototype == null) return Value.from(false);

            // c. If SameValue(O, V) is true, return true.
            if (object.sameValue(prototype.?)) return Value.from(true);
        }
    }

    /// 20.1.3.4 Object.prototype.propertyIsEnumerable ( V )
    /// https://tc39.es/ecma262/#sec-object.prototype.propertyisenumerable
    fn propertyIsEnumerable(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const value = arguments.get(0);

        // 1. Let P be ? ToPropertyKey(V).
        const property_key = try value.toPropertyKey(agent);

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 3. Let desc be ? O.[[GetOwnProperty]](P).
        const property_descriptor = try object.internalMethods().getOwnProperty(object, property_key);

        // 4. If desc is undefined, return false.
        if (property_descriptor == null) return Value.from(false);

        // 5. Return desc.[[Enumerable]].
        return Value.from(property_descriptor.?.enumerable.?);
    }

    /// 20.1.3.5 Object.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-object.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be the this value.
        const object = try this_value.toObject(agent);

        // 2. Return ? Invoke(O, "toString").
        return Value.from(object).invoke(agent, PropertyKey.from("toString"), .{});
    }

    /// 20.1.3.6 Object.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-object.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. If the this value is undefined, return "[object Undefined]".
        if (this_value == .undefined) return Value.from("[object Undefined]");

        // 2. If the this value is null, return "[object Null]".
        if (this_value == .null) return Value.from("[object Null]");

        // 3. Let O be ! ToObject(this value).
        const object = this_value.toObject(agent) catch |err| try noexcept(err);

        // 4. Let isArray be ? IsArray(O).
        const is_array = try this_value.isArray();

        // zig fmt: off
        // 5. If isArray is true, let builtinTag be "Array".
        const builtin_tag = if (is_array)
            "Array"
        // 6. Else if O has a [[ParameterMap]] internal slot, let builtinTag be "Arguments".
        else if (object.is(builtins.Arguments))
            "Arguments"
        // 7. Else if O has a [[Call]] internal method, let builtinTag be "Function".
        else if (object.internalMethods().call) |_|
            "Function"
        // 8. Else if O has an [[ErrorData]] internal slot, let builtinTag be "Error".
        else if (object.is(builtins.Error))
            "Error"
        // 9. Else if O has a [[BooleanData]] internal slot, let builtinTag be "Boolean".
        else if (object.is(builtins.Boolean))
            "Boolean"
        // 10. Else if O has a [[NumberData]] internal slot, let builtinTag be "Number".
        else if (object.is(builtins.Number))
            "Number"
        // 11. Else if O has a [[StringData]] internal slot, let builtinTag be "String".
        else if (object.is(builtins.String))
            "String"
        // TODO: 12. Else if O has a [[DateValue]] internal slot, let builtinTag be "Date".
        // TODO: 13. Else if O has a [[RegExpMatcher]] internal slot, let builtinTag be "RegExp".
        // 14. Else, let builtinTag be "Object".
        else
            "Object";
        // zig fmt: on

        // 15. Let tag be ? Get(O, @@toStringTag).
        const tag_value = try object.get(PropertyKey.from(agent.well_known_symbols.@"@@toStringTag"));

        // 16. If tag is not a String, set tag to builtinTag.
        const tag = switch (tag_value) {
            .string => |string| string.utf8,
            else => builtin_tag,
        };

        // 17. Return the string-concatenation of "[object ", tag, and "]".
        return Value.from(try std.fmt.allocPrint(agent.gc_allocator, "[object {s}]", .{tag}));
    }

    /// 20.1.3.7 Object.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-object.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? ToObject(this value).
        return Value.from(try this_value.toObject(agent));
    }
};

/// 20.1.4 Properties of Object Instances
/// https://tc39.es/ecma262/#sec-properties-of-object-instances
pub const Object = Object_.Factory(.{});
