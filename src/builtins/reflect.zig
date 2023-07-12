//! 28.1 The Reflect Object
//! https://tc39.es/ecma262/#sec-reflect-object

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const defineBuiltinFunction = utils.defineBuiltinFunction;

pub const Reflect = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "apply", apply, 3, realm);
        try defineBuiltinFunction(object, "construct", construct, 3, realm);
        try defineBuiltinFunction(object, "defineProperty", defineProperty, 3, realm);
        try defineBuiltinFunction(object, "deleteProperty", deleteProperty, 2, realm);
        try defineBuiltinFunction(object, "get", get, 1, realm);
        try defineBuiltinFunction(object, "getOwnPropertyDescriptor", getOwnPropertyDescriptor, 3, realm);
        try defineBuiltinFunction(object, "getPrototypeOf", getPrototypeOf, 1, realm);
        try defineBuiltinFunction(object, "has", has, 2, realm);
        try defineBuiltinFunction(object, "isExtensible", isExtensible, 1, realm);
        try defineBuiltinFunction(object, "ownKeys", ownKeys, 1, realm);
        try defineBuiltinFunction(object, "preventExtensions", preventExtensions, 1, realm);

        return object;
    }

    /// 28.1.1 Reflect.apply ( target, thisArgument, argumentsList )
    /// https://tc39.es/ecma262/#sec-reflect.apply
    fn apply(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const this_argument = arguments.get(1);
        const arguments_list = arguments.get(2);

        // 1. If IsCallable(target) is false, throw a TypeError exception.
        if (!target.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not callable", .{target}),
            );
        }

        // 2. Let args be ? CreateListFromArrayLike(argumentsList).
        const args = try arguments_list.createListFromArrayLike(agent, .{});

        // TODO: 3. Perform PrepareForTailCall().

        // 4. Return ? Call(target, thisArgument, args).
        return target.call(agent, this_argument, args);
    }

    /// 28.1.2 Reflect.construct ( target, argumentsList [ , newTarget ] )
    /// https://tc39.es/ecma262/#sec-reflect.construct
    fn construct(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const arguments_list = arguments.get(1);
        var new_target = arguments.get(2);

        // 1. If IsConstructor(target) is false, throw a TypeError exception.
        if (!target.isConstructor()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not a constructor", .{target}),
            );
        }

        // 2. If newTarget is not present, set newTarget to target.
        if (arguments.count() <= 2) {
            new_target = target;
        }
        // 3. Else if IsConstructor(newTarget) is false, throw a TypeError exception.
        else if (!new_target.isConstructor()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not a constructor", .{new_target}),
            );
        }

        // 4. Let args be ? CreateListFromArrayLike(argumentsList).
        const args = try arguments_list.createListFromArrayLike(agent, .{});

        // 5. Return ? Construct(target, args, newTarget).
        return Value.from(try target.object.construct(.{
            .arguments_list = ArgumentsList.from(args),
            .new_target = new_target.object,
        }));
    }

    /// 28.1.3 Reflect.defineProperty ( target, propertyKey, attributes )
    /// https://tc39.es/ecma262/#sec-reflect.defineproperty
    fn defineProperty(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);
        const attributes = arguments.get(2);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Let desc be ? ToPropertyDescriptor(attributes).
        const descriptor = try attributes.toPropertyDescriptor(agent);

        // 4. Return ? target.[[DefineOwnProperty]](key, desc).
        return Value.from(
            try target.object.internalMethods().defineOwnProperty(target.object, key, descriptor),
        );
    }

    /// 28.1.4 Reflect.deleteProperty ( target, propertyKey )
    /// https://tc39.es/ecma262/#sec-reflect.deleteproperty
    fn deleteProperty(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Return ? target.[[Delete]](key).
        return Value.from(try target.object.internalMethods().delete(target.object, key));
    }

    /// 28.1.5 Reflect.get ( target, propertyKey [ , receiver ] )
    /// https://tc39.es/ecma262/#sec-reflect.get
    fn get(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. If receiver is not present, then
        //     a. Set receiver to target.
        const receiver = arguments.getOrNull(2) orelse target;

        // 4. Return ? target.[[Get]](key, receiver).
        return try target.object.internalMethods().get(target.object, key, receiver);
    }

    /// 28.1.6 Reflect.getOwnPropertyDescriptor ( target, propertyKey )
    /// https://tc39.es/ecma262/#sec-reflect.getownpropertydescriptor
    fn getOwnPropertyDescriptor(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Let desc be ? target.[[GetOwnProperty]](key).
        const maybe_descriptor = try target.object.internalMethods().getOwnProperty(target.object, key);

        // 4. Return FromPropertyDescriptor(desc).
        if (maybe_descriptor) |descriptor|
            return Value.from(
                try descriptor.fromPropertyDescriptor(agent) orelse return .undefined,
            )
        else
            return .undefined;
    }

    /// 28.1.7 Reflect.getPrototypeOf ( target )
    /// https://tc39.es/ecma262/#sec-reflect.getprototypeof
    fn getPrototypeOf(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Return ? target.[[GetPrototypeOf]]().
        return Value.from(
            try target.object.internalMethods().getPrototypeOf(target.object) orelse return .undefined,
        );
    }

    /// 28.1.8 Reflect.has ( target, propertyKey )
    /// https://tc39.es/ecma262/#sec-reflect.has
    fn has(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);
        const property_key = arguments.get(1);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Let key be ? ToPropertyKey(propertyKey).
        const key = try property_key.toPropertyKey(agent);

        // 3. Return ? target.[[HasProperty]](key).
        return Value.from(try target.object.internalMethods().hasProperty(target.object, key));
    }

    /// 28.1.9 Reflect.isExtensible ( target )
    /// https://tc39.es/ecma262/#sec-reflect.isextensible
    fn isExtensible(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Return ? target.[[IsExtensible]]().
        return Value.from(try target.object.internalMethods().isExtensible(target.object));
    }

    /// 28.1.10 Reflect.ownKeys ( target )
    /// https://tc39.es/ecma262/#sec-reflect.ownkeys
    fn ownKeys(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Let keys be ? target.[[OwnPropertyKeys]]().
        const property_keys = try target.object.internalMethods().ownPropertyKeys(target.object);
        // TODO: Add createArrayFromList() overload for type conversions
        var keys = try std.ArrayList(Value).initCapacity(agent.gc_allocator, property_keys.items.len);
        for (property_keys.items) |property_key| {
            keys.appendAssumeCapacity(switch (property_key) {
                .string => |string| Value.from(string),
                .symbol => |symbol| Value.from(symbol),
                .integer_index => |integer_index| Value.from(try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "{}",
                    .{integer_index},
                )),
            });
        }

        // 3. Return CreateArrayFromList(keys).
        return Value.from(try createArrayFromList(agent, keys.items));
    }

    /// 28.1.11 Reflect.preventExtensions ( target )
    /// https://tc39.es/ecma262/#sec-reflect.preventextensions
    fn preventExtensions(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const target = arguments.get(0);

        // 1. If target is not an Object, throw a TypeError exception.
        if (target != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{target}),
            );
        }

        // 2. Return ? target.[[PreventExtensions]]().
        return Value.from(try target.object.internalMethods().preventExtensions(target.object));
    }
};
