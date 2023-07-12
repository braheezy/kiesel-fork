//! 28.1 The Reflect Object
//! https://tc39.es/ecma262/#sec-reflect-object

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
};
