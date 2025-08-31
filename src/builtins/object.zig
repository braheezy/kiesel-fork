//! 20.1 Object Objects
//! https://tc39.es/ecma262/#sec-object-objects

const std = @import("std");

const build_options = @import("build-options");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const addEntriesFromIterable = builtins.addEntriesFromIterable;
const createArrayFromList = types.createArrayFromList;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const createBuiltinFunction = builtins.createBuiltinFunction;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

/// 20.1.2 Properties of the Object Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-object-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*types.Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Object",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *types.Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "assign", assign, 2, realm);
        try object.defineBuiltinFunction(agent, "create", create_, 2, realm);
        try object.defineBuiltinFunction(agent, "defineProperties", defineProperties, 2, realm);
        try object.defineBuiltinFunction(agent, "defineProperty", defineProperty, 3, realm);
        try object.defineBuiltinFunction(agent, "entries", entries, 1, realm);
        try object.defineBuiltinFunction(agent, "freeze", freeze, 1, realm);
        try object.defineBuiltinFunction(agent, "fromEntries", fromEntries, 1, realm);
        try object.defineBuiltinFunction(agent, "getOwnPropertyDescriptor", getOwnPropertyDescriptor, 2, realm);
        try object.defineBuiltinFunction(agent, "getOwnPropertyDescriptors", getOwnPropertyDescriptors, 1, realm);
        try object.defineBuiltinFunction(agent, "getOwnPropertyNames", getOwnPropertyNames, 1, realm);
        try object.defineBuiltinFunction(agent, "getOwnPropertySymbols", getOwnPropertySymbols, 1, realm);
        try object.defineBuiltinFunction(agent, "getPrototypeOf", getPrototypeOf, 1, realm);
        try object.defineBuiltinFunction(agent, "groupBy", groupBy, 2, realm);
        try object.defineBuiltinFunction(agent, "hasOwn", hasOwn, 2, realm);
        try object.defineBuiltinFunction(agent, "is", is, 2, realm);
        try object.defineBuiltinFunction(agent, "isExtensible", isExtensible, 1, realm);
        try object.defineBuiltinFunction(agent, "isFrozen", isFrozen, 1, realm);
        try object.defineBuiltinFunction(agent, "isSealed", isSealed, 1, realm);
        try object.defineBuiltinFunction(agent, "keys", keys, 1, realm);
        try object.defineBuiltinFunction(agent, "preventExtensions", preventExtensions, 1, realm);
        try object.defineBuiltinFunction(agent, "seal", seal, 1, realm);
        try object.defineBuiltinFunction(agent, "setPrototypeOf", setPrototypeOf, 2, realm);
        try object.defineBuiltinFunction(agent, "values", values, 1, realm);

        // 20.1.2.21 Object.prototype
        // https://tc39.es/ecma262/#sec-object.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Object.prototype%"()),
            .none,
        );
    }

    /// 20.1.1.1 Object ( [ value ] )
    /// https://tc39.es/ecma262/#sec-object-value
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*types.Object) Agent.Error!Value {
        const realm = agent.currentRealm();
        const value = arguments.get(0);

        // 1. If NewTarget is neither undefined nor the active function object, then
        if (new_target != null and new_target.? != agent.activeFunctionObject()) {
            // a. Return ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
            return Value.from(try ordinaryCreateFromConstructor(
                Object,
                agent,
                new_target.?,
                "%Object.prototype%",
                {},
            ));
        }

        // 2. If value is either undefined or null, return OrdinaryObjectCreate(%Object.prototype%).
        if (value.isUndefined() or value.isNull()) {
            return Value.from(try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%Object.prototype%"(),
            ));
        }

        // 3. Return ! ToObject(value).
        return Value.from(value.toObject(agent) catch |err| try noexcept(err));
    }

    /// 20.1.2.1 Object.assign ( target, ...sources )
    /// https://tc39.es/ecma262/#sec-object.assign
    fn assign(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const sources = if (arguments.count() <= 1) &[_]Value{} else arguments.values[1..];

        // 1. Let to be ? ToObject(target).
        const to = try target.toObject(agent);

        // 2. If only one argument was passed, return to.
        if (arguments.count() == 1) return Value.from(to);

        // 3. For each element nextSource of sources, do
        for (sources) |next_source| {
            // a. If nextSource is neither undefined nor null, then
            if (!next_source.isUndefined() and !next_source.isNull()) {
                // i. Let from be ! ToObject(nextSource).
                const from = next_source.toObject(agent) catch |err| try noexcept(err);

                // ii. Let keys be ? from.[[OwnPropertyKeys]]().
                const keys_ = try from.internal_methods.ownPropertyKeys(agent, from);
                defer agent.gc_allocator.free(keys_);

                // iii. For each element nextKey of keys, do
                for (keys_) |next_key| {
                    // 1. Let desc be ? from.[[GetOwnProperty]](nextKey).
                    const descriptor = try from.internal_methods.getOwnProperty(
                        agent,
                        from,
                        next_key,
                    );

                    // 2. If desc is not undefined and desc.[[Enumerable]] is true, then
                    if (descriptor != null and descriptor.?.enumerable == true) {
                        // a. Let propValue be ? Get(from, nextKey).
                        const property_value = try from.get(agent, next_key);

                        // b. Perform ? Set(to, nextKey, propValue, true).
                        try to.set(agent, next_key, property_value, .throw);
                    }
                }
            }
        }

        // 4. Return to.
        return Value.from(to);
    }

    /// 20.1.2.2 Object.create ( O, Properties )
    /// https://tc39.es/ecma262/#sec-object.create
    fn create_(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);
        const properties = arguments.get(1);

        // 1. If O is not an Object and O is not null, throw a TypeError exception.
        if (!object.isObject() and !object.isNull()) {
            return agent.throwException(.type_error, "{f} is not an Object or null", .{object});
        }

        // 2. Let obj be OrdinaryObjectCreate(O).
        const obj = try ordinaryObjectCreate(
            agent,
            if (object.isObject()) object.asObject() else null,
        );

        // 3. If Properties is not undefined, then
        if (!properties.isUndefined()) {
            // a. Return ? ObjectDefineProperties(obj, Properties).
            return Value.from(try objectDefineProperties(agent, obj, properties));
        }

        // 4. Return obj.
        return Value.from(obj);
    }

    /// 20.1.2.3 Object.defineProperties ( O, Properties )
    /// https://tc39.es/ecma262/#sec-object.defineproperties
    fn defineProperties(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);
        const properties = arguments.get(1);

        // 1. If O is not an Object, throw a TypeError exception.
        if (!object.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{object});
        }

        // 2. Return ? ObjectDefineProperties(O, Properties).
        return Value.from(try objectDefineProperties(agent, object.asObject(), properties));
    }

    /// 20.1.2.3.1 ObjectDefineProperties ( O, Properties )
    /// https://tc39.es/ecma262/#sec-objectdefineproperties
    fn objectDefineProperties(
        agent: *Agent,
        object: *types.Object,
        properties: Value,
    ) Agent.Error!*types.Object {
        // 1. Let props be ? ToObject(Properties).
        const props = try properties.toObject(agent);

        // 2. Let keys be ? props.[[OwnPropertyKeys]]().
        const keys_ = try props.internal_methods.ownPropertyKeys(agent, props);
        defer agent.gc_allocator.free(keys_);

        const Property = struct {
            key: PropertyKey,
            descriptor: PropertyDescriptor,
        };

        // 3. Let descriptors be a new empty List.
        var descriptors: std.ArrayList(Property) = .empty;
        defer descriptors.deinit(agent.gc_allocator);

        // 4. For each element nextKey of keys, do
        for (keys_) |next_key| {
            // a. Let propDesc be ? props.[[GetOwnProperty]](nextKey).
            const maybe_property_descriptor = try props.internal_methods.getOwnProperty(
                agent,
                props,
                next_key,
            );

            // b. If propDesc is not undefined and propDesc.[[Enumerable]] is true, then
            if (maybe_property_descriptor) |property_descriptor| if (property_descriptor.enumerable == true) {
                // i. Let descObj be ? Get(props, nextKey).
                const descriptor_object = try props.get(agent, next_key);

                // ii. Let desc be ? ToPropertyDescriptor(descObj).
                const descriptor = try descriptor_object.toPropertyDescriptor(agent);

                // iii. Append the Record { [[Key]]: nextKey, [[Descriptor]]: desc } to descriptors.
                try descriptors.append(
                    agent.gc_allocator,
                    .{ .key = next_key, .descriptor = descriptor },
                );
            };
        }

        // 5. For each element property of descriptors, do
        for (descriptors.items) |property| {
            // a. Perform ? DefinePropertyOrThrow(O, property.[[Key]], property.[[Descriptor]]).
            try object.definePropertyOrThrow(agent, property.key, property.descriptor);
        }

        // 6. Return O.
        return object;
    }

    /// 20.1.2.4 Object.defineProperty ( O, P, Attributes )
    /// https://tc39.es/ecma262/#sec-object.defineproperty
    fn defineProperty(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);
        const property = arguments.get(1);
        const attributes = arguments.get(2);

        // 1. If O is not an Object, throw a TypeError exception.
        if (!object.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{object});
        }

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Let desc be ? ToPropertyDescriptor(Attributes).
        const property_descriptor = try attributes.toPropertyDescriptor(agent);

        // 4. Perform ? DefinePropertyOrThrow(O, key, desc).
        try object.asObject().definePropertyOrThrow(agent, property_key, property_descriptor);

        // 5. Return O.
        return object;
    }

    /// 20.1.2.5 Object.entries ( O )
    /// https://tc39.es/ecma262/#sec-object.entries
    fn entries(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let entryList be ? EnumerableOwnProperties(obj, key+value).
        var entry_list = try obj.enumerableOwnProperties(agent, .@"key+value");
        defer entry_list.deinit(agent.gc_allocator);

        // 3. Return CreateArrayFromList(entryList).
        return Value.from(try createArrayFromList(agent, entry_list.items));
    }

    /// 20.1.2.6 Object.freeze ( O )
    /// https://tc39.es/ecma262/#sec-object.freeze
    fn freeze(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (!object.isObject()) return object;

        // 2. Let status be ? SetIntegrityLevel(O, frozen).
        const status = try object.asObject().setIntegrityLevel(agent, .frozen);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not freeze object", .{});

        // 4. Return O.
        return object;
    }

    /// 20.1.2.7 Object.fromEntries ( iterable )
    /// https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.fromentries
    fn fromEntries(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const iterable = arguments.get(0);

        // 1. Perform ? RequireObjectCoercible(iterable).
        try iterable.requireObjectCoercible(agent);

        // 2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
        // 3. Assert: obj is an extensible ordinary object with no own properties.
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        const Captures = struct {
            object: *types.Object,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{ .object = object };

        // 4. Let closure be a new Abstract Closure with parameters (key, value) that captures obj
        //    and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
                const function = agent_.activeFunctionObject();
                const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
                const object_ = captures_.object;
                const key = arguments_.get(0);
                const value = arguments_.get(1);

                // a. Let propertyKey be ? ToPropertyKey(key).
                const property_key = try key.toPropertyKey(agent_);

                // b. Perform ! CreateDataPropertyOrThrow(obj, propertyKey, value).
                try object_.createDataPropertyDirect(agent_, property_key, value);

                // c. Return NormalCompletion(undefined).
                return .undefined;
            }
        }.func;

        // 5. Let adder be CreateBuiltinFunction(closure, 2, "", « »).
        const adder = try createBuiltinFunction(
            agent,
            .{ .function = closure },
            2,
            "",
            .{ .additional_fields = .make(*Captures, captures) },
        );

        // 6. Return ? AddEntriesFromIterable(obj, iterable, adder).
        return Value.from(try addEntriesFromIterable(agent, object, iterable, adder));
    }

    /// 20.1.2.8 Object.getOwnPropertyDescriptor ( O, P )
    /// https://tc39.es/ecma262/#sec-object.getownpropertydescriptor
    fn getOwnPropertyDescriptor(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);
        const property = arguments.get(1);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Let desc be ? obj.[[GetOwnProperty]](key).
        const maybe_descriptor = try obj.internal_methods.getOwnProperty(agent, obj, property_key);

        // 4. Return FromPropertyDescriptor(desc).
        if (maybe_descriptor) |descriptor|
            return Value.from(try descriptor.fromPropertyDescriptor(agent))
        else
            return .undefined;
    }

    /// 20.1.2.9 Object.getOwnPropertyDescriptors ( O )
    /// https://tc39.es/ecma262/#sec-object.getownpropertydescriptors
    fn getOwnPropertyDescriptors(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let ownKeys be ? obj.[[OwnPropertyKeys]]().
        const own_keys = try obj.internal_methods.ownPropertyKeys(agent, obj);
        defer agent.gc_allocator.free(own_keys);

        // 3. Let descriptors be OrdinaryObjectCreate(%Object.prototype%).
        const descriptors = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each element key of ownKeys, do
        for (own_keys) |key| {
            // a. Let desc be ? obj.[[GetOwnProperty]](key).
            if (try obj.internal_methods.getOwnProperty(agent, obj, key)) |property_descriptor| {
                // b. Let descriptor be FromPropertyDescriptor(desc).
                const descriptor = try property_descriptor.fromPropertyDescriptor(agent);

                // c. If descriptor is not undefined, perform ! CreateDataPropertyOrThrow(descriptors, key, descriptor).
                try descriptors.createDataPropertyDirect(agent, key, Value.from(descriptor));
            }
        }

        // 5. Return descriptors.
        return Value.from(descriptors);
    }

    /// 20.1.2.10 Object.getOwnPropertyNames ( O )
    /// https://tc39.es/ecma262/#sec-object.getownpropertynames
    fn getOwnPropertyNames(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. Return CreateArrayFromList(? GetOwnPropertyKeys(O, string)).
        const property_keys = try getOwnPropertyKeys(agent, object, .string);
        defer agent.gc_allocator.free(property_keys);
        return Value.from(
            try createArrayFromListMapToValue(agent, PropertyKey, property_keys, struct {
                fn mapFn(agent_: *Agent, property_key: PropertyKey) std.mem.Allocator.Error!Value {
                    return property_key.toValue(agent_);
                }
            }.mapFn),
        );
    }

    /// 20.1.2.11 Object.getOwnPropertySymbols ( O )
    /// https://tc39.es/ecma262/#sec-object.getownpropertysymbols
    fn getOwnPropertySymbols(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. Return CreateArrayFromList(? GetOwnPropertyKeys(O, symbol)).
        const property_keys = try getOwnPropertyKeys(agent, object, .symbol);
        defer agent.gc_allocator.free(property_keys);
        return Value.from(
            try createArrayFromListMapToValue(agent, PropertyKey, property_keys, struct {
                fn mapFn(agent_: *Agent, property_key: PropertyKey) std.mem.Allocator.Error!Value {
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
    ) Agent.Error![]PropertyKey {
        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let keys be ? obj.[[OwnPropertyKeys]]().
        const keys_ = try obj.internal_methods.ownPropertyKeys(agent, obj);
        defer agent.gc_allocator.free(keys_);

        // 3. Let nameList be a new empty List.
        var name_list: std.ArrayList(PropertyKey) = .empty;

        // 4. For each element nextKey of keys, do
        for (keys_) |next_key| {
            // a. If nextKey is a Symbol and type is symbol, or if nextKey is a String and type is
            //    string, then
            if ((next_key == .symbol and @"type" == .symbol) or
                ((next_key == .string or next_key == .integer_index) and @"type" == .string))
            {
                // i. Append nextKey to nameList.
                try name_list.append(agent.gc_allocator, next_key);
            }
        }

        // 5. Return nameList.
        return name_list.toOwnedSlice(agent.gc_allocator);
    }

    /// 20.1.2.12 Object.getPrototypeOf ( O )
    /// https://tc39.es/ecma262/#sec-object.getprototypeof
    fn getPrototypeOf(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Return ? obj.[[GetPrototypeOf]]().
        return Value.from(try obj.internal_methods.getPrototypeOf(agent, obj) orelse return .null);
    }

    /// 20.1.2.13 Object.groupBy ( items, callback )
    /// https://tc39.es/ecma262/#sec-object.groupby
    fn groupBy(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const items = arguments.get(0);
        const callback = arguments.get(1);

        // 1. Let groups be ? GroupBy(items, callback, property).
        const groups = try items.groupBy(agent, callback, .property);

        // 2. Let obj be OrdinaryObjectCreate(null).
        const object = try ordinaryObjectCreate(agent, null);

        // 3. For each Record { [[Key]], [[Elements]] } g of groups, do
        var it = groups.iterator();
        while (it.next()) |entry| {
            // a. Let elements be CreateArrayFromList(g.[[Elements]]).
            const elements = try createArrayFromList(agent, entry.value_ptr.items);

            // b. Perform ! CreateDataPropertyOrThrow(obj, g.[[Key]], elements).
            try object.createDataPropertyDirect(agent, entry.key_ptr.*, Value.from(elements));
        }

        // 4. Return obj.
        return Value.from(object);
    }

    /// 20.1.2.14 Object.hasOwn ( O, P )
    /// https://tc39.es/ecma262/#sec-object.hasown
    fn hasOwn(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);
        const property = arguments.get(1);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Return ? HasOwnProperty(obj, key).
        return Value.from(try obj.hasOwnProperty(agent, property_key));
    }

    /// 20.1.2.15 Object.is ( value1, value2 )
    /// https://tc39.es/ecma262/#sec-object.is
    fn is(_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const value1 = arguments.get(0);
        const value2 = arguments.get(1);

        // 1. Return SameValue(value1, value2).
        return Value.from(sameValue(value1, value2));
    }

    /// 20.1.2.16 Object.isExtensible ( O )
    /// https://tc39.es/ecma262/#sec-object.isextensible
    fn isExtensible(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (!object.isObject()) return Value.from(true);

        // 2. Return ? IsExtensible(O).
        return Value.from(try object.asObject().isExtensible(agent));
    }

    /// 20.1.2.17 Object.isFrozen ( O )
    /// https://tc39.es/ecma262/#sec-object.isfrozen
    fn isFrozen(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (!object.isObject()) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, frozen).
        return Value.from(try object.asObject().testIntegrityLevel(agent, .frozen));
    }

    /// 20.1.2.18 Object.isSealed ( O )
    /// https://tc39.es/ecma262/#sec-object.issealed
    fn isSealed(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return true.
        if (!object.isObject()) return Value.from(true);

        // 2. Return ? TestIntegrityLevel(O, sealed).
        return Value.from(try object.asObject().testIntegrityLevel(agent, .sealed));
    }

    /// 20.1.2.19 Object.keys ( O )
    /// https://tc39.es/ecma262/#sec-object.keys
    fn keys(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let keyList be ? EnumerableOwnProperties(obj, key).
        var key_list = try obj.enumerableOwnProperties(agent, .key);
        defer key_list.deinit(agent.gc_allocator);

        // 3. Return CreateArrayFromList(keyList).
        return Value.from(try createArrayFromList(agent, key_list.items));
    }

    /// 20.1.2.20 Object.preventExtensions ( O )
    /// https://tc39.es/ecma262/#sec-object.preventextensions
    fn preventExtensions(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (!object.isObject()) return object;

        // 2. Let status be ? O.[[PreventExtensions]]().
        const status = try object.asObject().internal_methods.preventExtensions(agent, object.asObject());

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not prevent extensions", .{});

        // 4. Return O.
        return object;
    }

    /// 20.1.2.22 Object.seal ( O )
    /// https://tc39.es/ecma262/#sec-object.seal
    fn seal(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. If O is not an Object, return O.
        if (!object.isObject()) return Value.from(true);

        // 2. Let status be ? SetIntegrityLevel(O, sealed).
        const status = try object.asObject().setIntegrityLevel(agent, .sealed);

        // 3. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not seal object", .{});

        // 4. Return O.
        return object;
    }

    /// 20.1.2.23 Object.setPrototypeOf ( O, proto )
    /// https://tc39.es/ecma262/#sec-object.seal
    fn setPrototypeOf(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);
        const prototype_ = arguments.get(1);

        // 1. Perform ? RequireObjectCoercible(O).
        try object.requireObjectCoercible(agent);

        // 2. If proto is not an Object and proto is not null, throw a TypeError exception.
        if (!prototype_.isObject() and !prototype_.isNull()) {
            return agent.throwException(.type_error, "{f} is not an Object or null", .{prototype_});
        }

        // 3. If O is not an Object, return O.
        if (!object.isObject()) return object;

        // 4. Let status be ? O.[[SetPrototypeOf]](proto).
        const status = try object.asObject().internal_methods.setPrototypeOf(
            agent,
            object.asObject(),
            if (prototype_.isObject()) prototype_.asObject() else null,
        );

        // 5. If status is false, throw a TypeError exception.
        if (!status) return agent.throwException(.type_error, "Could not set prototype", .{});

        // 6. Return O.
        return object;
    }

    /// 20.1.2.24 Object.values ( O )
    /// https://tc39.es/ecma262/#sec-object.values
    fn values(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const object = arguments.get(0);

        // 1. Let obj be ? ToObject(O).
        const obj = try object.toObject(agent);

        // 2. Let valueList be ? EnumerableOwnProperties(obj, value).
        var value_list = try obj.enumerableOwnProperties(agent, .value);
        defer value_list.deinit(agent.gc_allocator);

        // 3. Return CreateArrayFromList(valueList).
        return Value.from(try createArrayFromList(agent, value_list.items));
    }
};

/// 20.1.3 Properties of the Object Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, _: *Realm) std.mem.Allocator.Error!*types.Object {
        return Object.create(agent, .{
            .prototype = null,
            .internal_methods = .initComptime(.{
                .setPrototypeOf = builtins.immutable_prototype.setPrototypeOf,
            }),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *types.Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "hasOwnProperty", hasOwnProperty, 1, realm);
        try object.defineBuiltinFunction(agent, "isPrototypeOf", isPrototypeOf, 1, realm);
        try object.defineBuiltinFunction(agent, "propertyIsEnumerable", propertyIsEnumerable, 1, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

        // 20.1.3.1 Object.prototype.constructor
        // https://tc39.es/ecma262/#sec-object.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Object%"()),
        );

        if (build_options.enable_legacy) {
            try object.defineBuiltinAccessor(agent, "__proto__", @"get __proto__", @"set __proto__", realm);
            try object.defineBuiltinFunction(agent, "__defineGetter__", __defineGetter__, 2, realm);
            try object.defineBuiltinFunction(agent, "__defineSetter__", __defineSetter__, 2, realm);
            try object.defineBuiltinFunction(agent, "__lookupGetter__", __lookupGetter__, 1, realm);
            try object.defineBuiltinFunction(agent, "__lookupSetter__", __lookupSetter__, 1, realm);
        }

        // Ensure function intrinsics are set right after the object is created
        _ = try realm.intrinsics.@"%Object.prototype.toString%"();
    }

    /// 20.1.3.2 Object.prototype.hasOwnProperty ( V )
    /// https://tc39.es/ecma262/#sec-object.prototype.hasownproperty
    fn hasOwnProperty(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Let P be ? ToPropertyKey(V).
        const property_key = try value.toPropertyKey(agent);

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 3. Return ? HasOwnProperty(O, P).
        return Value.from(try object.hasOwnProperty(agent, property_key));
    }

    /// 20.1.3.3 Object.prototype.isPrototypeOf ( V )
    /// https://tc39.es/ecma262/#sec-object.prototype.isprototypeof
    fn isPrototypeOf(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. If V is not an Object, return false.
        if (!value.isObject()) return Value.from(false);

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        var prototype_ = value.asObject();

        // 3. Repeat,
        while (true) {
            // a. Set V to ? V.[[GetPrototypeOf]]().
            prototype_ = try prototype_.internal_methods.getPrototypeOf(agent, prototype_) orelse {
                // b. If V is null, return false.
                return Value.from(false);
            };

            // c. If SameValue(O, V) is true, return true.
            if (object == prototype_) return Value.from(true);
        }
    }

    /// 20.1.3.4 Object.prototype.propertyIsEnumerable ( V )
    /// https://tc39.es/ecma262/#sec-object.prototype.propertyisenumerable
    fn propertyIsEnumerable(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Let P be ? ToPropertyKey(V).
        const property_key = try value.toPropertyKey(agent);

        // 2. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 3. Let desc be ? O.[[GetOwnProperty]](P).
        const property_descriptor = try object.internal_methods.getOwnProperty(
            agent,
            object,
            property_key,
        ) orelse {
            // 4. If desc is undefined, return false.
            return Value.from(false);
        };

        // 5. Return desc.[[Enumerable]].
        return Value.from(property_descriptor.enumerable.?);
    }

    /// 20.1.3.5 Object.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-object.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. Return ? Invoke(O, "toString").
        return this_value.invoke(agent, PropertyKey.from("toString"), &.{});
    }

    /// 20.1.3.6 Object.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-object.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. If the this value is undefined, return "[object Undefined]".
        if (this_value.isUndefined()) return Value.from("[object Undefined]");

        // 2. If the this value is null, return "[object Null]".
        if (this_value.isNull()) return Value.from("[object Null]");

        // 3. Let O be ! ToObject(this value).
        const object = this_value.toObject(agent) catch |err| try noexcept(err);

        // 4. Let isArray be ? IsArray(O).
        const is_array = try this_value.isArray(agent);

        // zig fmt: off
        // 5. If isArray is true, let builtinTag be "Array".
        const builtin_tag = if (is_array)
            String.fromLiteral("Array")
        // 6. Else if O has a [[ParameterMap]] internal slot, let builtinTag be "Arguments".
        else if (object.is(builtins.Arguments))
            String.fromLiteral("Arguments")
        // 7. Else if O has a [[Call]] internal method, let builtinTag be "Function".
        else if (object.internal_methods.call) |_|
            String.fromLiteral("Function")
        // 8. Else if O has an [[ErrorData]] internal slot, let builtinTag be "Error".
        else if (object.is(builtins.Error))
            String.fromLiteral("Error")
        // 9. Else if O has a [[BooleanData]] internal slot, let builtinTag be "Boolean".
        else if (object.is(builtins.Boolean))
            String.fromLiteral("Boolean")
        // 10. Else if O has a [[NumberData]] internal slot, let builtinTag be "Number".
        else if (object.is(builtins.Number))
            String.fromLiteral("Number")
        // 11. Else if O has a [[StringData]] internal slot, let builtinTag be "String".
        else if (object.is(builtins.String))
            String.fromLiteral("String")
        // 12. Else if O has a [[DateValue]] internal slot, let builtinTag be "Date".
        else if (object.is(builtins.Date))
            String.fromLiteral("Date")
        // 13. Else if O has a [[RegExpMatcher]] internal slot, let builtinTag be "RegExp".
        else if (object.is(builtins.RegExp))
            String.fromLiteral("RegExp")
        // 14. Else, let builtinTag be "Object".
        else
            String.fromLiteral("Object");
        // zig fmt: on

        // 15. Let tag be ? Get(O, %Symbol.toStringTag%).
        const tag_value = try object.get(agent, PropertyKey.from(agent.well_known_symbols.@"%Symbol.toStringTag%"));

        // 16. If tag is not a String, set tag to builtinTag.
        const tag = if (tag_value.isString()) tag_value.asString() else builtin_tag;

        // 17. Return the string-concatenation of "[object ", tag, and "]".
        return Value.from(
            try String.concat(agent, &.{
                String.fromLiteral("[object "),
                tag,
                String.fromLiteral("]"),
            }),
        );
    }

    /// 20.1.3.7 Object.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-object.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ToObject(this value).
        return Value.from(try this_value.toObject(agent));
    }

    /// 20.1.3.8.1 get Object.prototype.__proto__
    /// https://tc39.es/ecma262/#sec-get-object.prototype.__proto__
    fn @"get __proto__"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Return ? O.[[GetPrototypeOf]]().
        return Value.from(
            try object.internal_methods.getPrototypeOf(agent, object) orelse return .null,
        );
    }

    /// 20.1.3.8.2 set Object.prototype.__proto__
    /// https://tc39.es/ecma262/#sec-set-object.prototype.__proto__
    fn @"set __proto__"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const prototype_ = arguments.get(0);

        // 1. Let O be the this value.
        const object = this_value;

        // 2. Perform ? RequireObjectCoercible(O).
        try object.requireObjectCoercible(agent);

        // 3. If proto is not an Object and proto is not null, return undefined.
        if (!prototype_.isObject() and !prototype_.isNull()) return .undefined;

        // 4. If O is not an Object, return undefined.
        if (!object.isObject()) return .undefined;

        // 5. Let status be ? O.[[SetPrototypeOf]](proto).
        const status = try object.asObject().internal_methods.setPrototypeOf(
            agent,
            object.asObject(),
            if (prototype_.isObject()) prototype_.asObject() else null,
        );

        // 6. If status is false, throw a TypeError exception.
        if (!status) {
            return agent.throwException(.type_error, "Could not set prototype", .{});
        }

        // 7. Return undefined.
        return .undefined;
    }

    /// 20.1.3.9.1 Object.prototype.__defineGetter__ ( P, getter )
    /// https://tc39.es/ecma262/#sec-object.prototype.__defineGetter__
    fn __defineGetter__(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const property = arguments.get(0);
        const getter = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. If IsCallable(getter) is false, throw a TypeError exception.
        if (!getter.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{getter});
        }

        // 3. Let desc be PropertyDescriptor {
        //      [[Get]]: getter, [[Enumerable]]: true, [[Configurable]]: true
        //    }.
        const property_descriptor: PropertyDescriptor = .{
            .get = getter.asObject(),
            .enumerable = true,
            .configurable = true,
        };

        // 4. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 5. Perform ? DefinePropertyOrThrow(O, key, desc).
        try object.definePropertyOrThrow(agent, property_key, property_descriptor);

        // 6. Return undefined.
        return .undefined;
    }

    /// 20.1.3.9.2 Object.prototype.__defineSetter__ ( P, setter )
    /// https://tc39.es/ecma262/#sec-object.prototype.__defineSetter__
    fn __defineSetter__(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const property = arguments.get(0);
        const setter = arguments.get(1);

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. If IsCallable(setter) is false, throw a TypeError exception.
        if (!setter.isCallable()) {
            return agent.throwException(.type_error, "{f} is not callable", .{setter});
        }

        // 3. Let desc be PropertyDescriptor {
        //      [[Set]]: setter, [[Enumerable]]: true, [[Configurable]]: true
        //    }.
        const property_descriptor: PropertyDescriptor = .{
            .set = setter.asObject(),
            .enumerable = true,
            .configurable = true,
        };

        // 4. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 5. Perform ? DefinePropertyOrThrow(O, key, desc).
        try object.definePropertyOrThrow(agent, property_key, property_descriptor);

        // 6. Return undefined.
        return .undefined;
    }

    /// 20.1.3.9.3 Object.prototype.__lookupGetter__ ( P )
    /// https://tc39.es/ecma262/#sec-object.prototype.__lookupGetter__
    fn __lookupGetter__(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const property = arguments.get(0);

        // 1. Let O be ? ToObject(this value).
        var object = try this_value.toObject(agent);

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Repeat,
        while (true) {
            // a. Let desc be ? O.[[GetOwnProperty]](key).
            // b. If desc is not undefined, then
            if (try object.internal_methods.getOwnProperty(
                agent,
                object,
                property_key,
            )) |property_descriptor| {
                // i. If IsAccessorDescriptor(desc) is true, return desc.[[Get]].
                if (property_descriptor.isAccessorDescriptor()) {
                    return Value.from(property_descriptor.get.? orelse return .undefined);
                }

                // ii. Return undefined.
                return .undefined;
            }

            // c. Set O to ? O.[[GetPrototypeOf]]().
            object = try object.internal_methods.getPrototypeOf(agent, object) orelse {
                // d. If O is null, return undefined.
                return .undefined;
            };
        }
    }

    /// 20.1.3.9.4 Object.prototype.__lookupSetter__ ( P )
    /// https://tc39.es/ecma262/#sec-object.prototype.__lookupSetter__
    fn __lookupSetter__(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const property = arguments.get(0);

        // 1. Let O be ? ToObject(this value).
        var object = try this_value.toObject(agent);

        // 2. Let key be ? ToPropertyKey(P).
        const property_key = try property.toPropertyKey(agent);

        // 3. Repeat,
        while (true) {
            // a. Let desc be ? O.[[GetOwnProperty]](key).
            // b. If desc is not undefined, then
            if (try object.internal_methods.getOwnProperty(
                agent,
                object,
                property_key,
            )) |property_descriptor| {
                // i. If IsAccessorDescriptor(desc) is true, return desc.[[Set]].
                if (property_descriptor.isAccessorDescriptor()) {
                    return Value.from(property_descriptor.set.? orelse return .undefined);
                }

                // ii. Return undefined.
                return .undefined;
            }

            // c. Set O to ? O.[[GetPrototypeOf]]().
            object = try object.internal_methods.getPrototypeOf(agent, object) orelse {
                // d. If O is null, return undefined.
                return .undefined;
            };
        }
    }
};

/// 20.1.4 Properties of Object Instances
/// https://tc39.es/ecma262/#sec-properties-of-object-instances
pub const Object = MakeObject(.{});
