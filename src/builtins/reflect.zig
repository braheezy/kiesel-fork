//! 28.1 The Reflect Object
//! https://tc39.es/ecma262/#sec-reflect-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

pub const Reflect = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "apply", apply, 3, realm);
        try defineBuiltinFunction(object, "construct", construct, 2, realm);
        try defineBuiltinFunction(object, "defineProperty", defineProperty, 3, realm);
        try defineBuiltinFunction(object, "deleteProperty", deleteProperty, 2, realm);
        try defineBuiltinFunction(object, "get", get, 2, realm);
        try defineBuiltinFunction(object, "getOwnPropertyDescriptor", getOwnPropertyDescriptor, 2, realm);
        try defineBuiltinFunction(object, "getPrototypeOf", getPrototypeOf, 1, realm);
        try defineBuiltinFunction(object, "has", has, 2, realm);
        try defineBuiltinFunction(object, "isExtensible", isExtensible, 1, realm);
        try defineBuiltinFunction(object, "ownKeys", ownKeys, 1, realm);
        try defineBuiltinFunction(object, "preventExtensions", preventExtensions, 1, realm);
        try defineBuiltinFunction(object, "set", set, 3, realm);
        try defineBuiltinFunction(object, "setPrototypeOf", setPrototypeOf, 2, realm);

        // 28.1.14 Reflect [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-reflect-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Reflect"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 28.1.1 Reflect.apply ( target, thisArgument, argumentsList )
    /// https://tc39.es/ecma262/#sec-reflect.apply
    fn apply(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const this_argument = arguments.get(1);
        const arguments_list = arguments.get(2);

        // 1. If IsCallable(target) is false, throw a TypeError exception.
        if (!target.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{target});
        }

        // 2. Let args be ? CreateListFromArrayLike(argumentsList).
        const args = try arguments_list.createListFromArrayLike(agent, .{});

        // TODO: 3. Perform PrepareForTailCall().

        // 4. Return ? Call(target, thisArgument, args).
        return target.call(agent, this_argument, args);
    }

    /// 28.1.2 Reflect.construct ( target, argumentsList [ , newTarget ] )
    /// https://tc39.es/ecma262/#sec-reflect.construct
    fn construct(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const arguments_list = arguments.get(1);
        var new_target = arguments.get(2);

        // 1. If IsConstructor(target) is false, throw a TypeError exception.
        if (!target.isConstructor()) {
            return agent.throwException(.type_error, "{} is not a constructor", .{target});
        }

        // 2. If newTarget is not present, set newTarget to target.
        if (arguments.count() <= 2) {
            new_target = target;
        }
        // 3. Else if IsConstructor(newTarget) is false, throw a TypeError exception.
        else if (!new_target.isConstructor()) {
            return agent.throwException(.type_error, "{} is not a constructor", .{new_target});
        }

        // 4. Let args be ? CreateListFromArrayLike(argumentsList).
        const args = try arguments_list.createListFromArrayLike(agent, .{});

        // 5. Return ? Construct(target, args, newTarget).
        return Value.from(try target.asObject().construct(args, new_target.asObject()));
    }

    /// 28.1.3 Reflect.defineProperty ( target, propertyKey, attributes )
    /// https://tc39.es/ecma262/#sec-reflect.defineproperty
    fn defineProperty(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);
        const attributes = arguments.get(2);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Let desc be ? ToPropertyDescriptor(attributes).
        const descriptor = try attributes.toPropertyDescriptor(agent);

        // 4. Return ? target.[[DefineOwnProperty]](key, desc).
        return Value.from(
            try target.asObject().internalMethods().defineOwnProperty(target.asObject(), key, descriptor),
        );
    }

    /// 28.1.4 Reflect.deleteProperty ( target, propertyKey )
    /// https://tc39.es/ecma262/#sec-reflect.deleteproperty
    fn deleteProperty(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Return ? target.[[Delete]](key).
        return Value.from(try target.asObject().internalMethods().delete(target.asObject(), key));
    }

    /// 28.1.5 Reflect.get ( target, propertyKey [ , receiver ] )
    /// https://tc39.es/ecma262/#sec-reflect.get
    fn get(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. If receiver is not present, then
        //     a. Set receiver to target.
        const receiver = arguments.getOrNull(2) orelse target;

        // 4. Return ? target.[[Get]](key, receiver).
        return try target.asObject().internalMethods().get(target.asObject(), key, receiver);
    }

    /// 28.1.6 Reflect.getOwnPropertyDescriptor ( target, propertyKey )
    /// https://tc39.es/ecma262/#sec-reflect.getownpropertydescriptor
    fn getOwnPropertyDescriptor(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Let desc be ? target.[[GetOwnProperty]](key).
        const maybe_descriptor = try target.asObject().internalMethods().getOwnProperty(target.asObject(), key);

        // 4. Return FromPropertyDescriptor(desc).
        if (maybe_descriptor) |descriptor|
            return Value.from(try descriptor.fromPropertyDescriptor(agent))
        else
            return Value.undefined;
    }

    /// 28.1.7 Reflect.getPrototypeOf ( target )
    /// https://tc39.es/ecma262/#sec-reflect.getprototypeof
    fn getPrototypeOf(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Return ? target.[[GetPrototypeOf]]().
        return Value.from(
            try target.asObject().internalMethods().getPrototypeOf(target.asObject()) orelse return Value.null,
        );
    }

    /// 28.1.8 Reflect.has ( target, propertyKey )
    /// https://tc39.es/ecma262/#sec-reflect.has
    fn has(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Return ? target.[[HasProperty]](key).
        return Value.from(try target.asObject().internalMethods().hasProperty(target.asObject(), key));
    }

    /// 28.1.9 Reflect.isExtensible ( target )
    /// https://tc39.es/ecma262/#sec-reflect.isextensible
    fn isExtensible(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Return ? target.[[IsExtensible]]().
        return Value.from(try target.asObject().internalMethods().isExtensible(target.asObject()));
    }

    /// 28.1.10 Reflect.ownKeys ( target )
    /// https://tc39.es/ecma262/#sec-reflect.ownkeys
    fn ownKeys(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let keys be ? target.[[OwnPropertyKeys]]().
        const keys = try target.asObject().internalMethods().ownPropertyKeys(target.asObject());
        defer keys.deinit();

        // 3. Return CreateArrayFromList(keys).
        return Value.from(
            try createArrayFromListMapToValue(agent, PropertyKey, keys.items, struct {
                fn mapFn(agent_: *Agent, property_key: PropertyKey) Allocator.Error!Value {
                    return property_key.toValue(agent_);
                }
            }.mapFn),
        );
    }

    /// 28.1.11 Reflect.preventExtensions ( target )
    /// https://tc39.es/ecma262/#sec-reflect.preventextensions
    fn preventExtensions(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Return ? target.[[PreventExtensions]]().
        return Value.from(try target.asObject().internalMethods().preventExtensions(target.asObject()));
    }

    /// 28.1.12 Reflect.set ( target, propertyKey, V [ , receiver ] )
    /// https://tc39.es/ecma262/#sec-reflect.set
    fn set(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);
        const value = arguments.get(2);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. If receiver is not present, then
        //     a. Set receiver to target.
        const receiver = arguments.getOrNull(3) orelse target;

        // 4. Return ? target.[[Set]](key, V, receiver).
        return Value.from(
            try target.asObject().internalMethods().set(target.asObject(), key, value, receiver),
        );
    }

    /// 28.1.13 Reflect.setPrototypeOf ( target, proto )
    /// https://tc39.es/ecma262/#sec-reflect.setprototypeof
    fn setPrototypeOf(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const target = arguments.get(0);
        const prototype = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (!target.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{target});
        }

        // 2. If proto is not an Object and proto is not null, throw a TypeError exception.
        if (!prototype.isObject() and !prototype.isNull()) {
            return agent.throwException(.type_error, "{} is not an Object or null", .{prototype});
        }

        // 3. Return ? target.[[SetPrototypeOf]](proto).
        return Value.from(
            try target.asObject().internalMethods().setPrototypeOf(
                target.asObject(),
                if (prototype.isObject()) prototype.asObject() else null,
            ),
        );
    }
};
