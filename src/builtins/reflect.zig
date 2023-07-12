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
const defineBuiltinFunction = utils.defineBuiltinFunction;

pub const Reflect = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "apply", apply, 3, realm);
        try defineBuiltinFunction(object, "construct", construct, 3, realm);

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
};
