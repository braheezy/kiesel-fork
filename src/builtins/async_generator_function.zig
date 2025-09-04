//! 27.4 AsyncGeneratorFunction Objects
//! https://tc39.es/ecma262/#sec-asyncgeneratorfunction-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createDynamicFunction = builtins.createDynamicFunction;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 27.4.2 Properties of the AsyncGeneratorFunction Constructor
/// https://tc39.es/ecma262/#sec-properties-of-asyncgeneratorfunction
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "AsyncGeneratorFunction",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 27.4.2.1 AsyncGeneratorFunction.prototype
        // https://tc39.es/ecma262/#sec-asyncgeneratorfunction-prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"()),
            .none,
        );
    }

    /// 27.4.1.1 AsyncGeneratorFunction ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-asyncgeneratorfunction
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const parameter_args = arguments.values[0..arguments.count() -| 1];
        const maybe_body_arg = arguments.getOrNull(arguments.count() -| 1);

        // 1. Let C be the active function object.
        const constructor_ = agent.activeFunctionObject();

        // 2. If bodyArg is not present, set bodyArg to the empty String.
        const body_arg = maybe_body_arg orelse Value.from("");

        // 3. Return ? CreateDynamicFunction(C, NewTarget, async-generator, parameterArgs, bodyArg).
        const ecmascript_function = try createDynamicFunction(
            agent,
            constructor_,
            new_target,
            .async_generator,
            parameter_args,
            body_arg,
        );
        return Value.from(&ecmascript_function.object);
    }
};

/// 27.4.3 Properties of the AsyncGeneratorFunction Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-asyncgeneratorfunction-prototype
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Function.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 27.4.3.1 AsyncGeneratorFunction.prototype.constructor
        // https://tc39.es/ecma262/#sec-asyncgeneratorfunction-prototype-constructor
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%AsyncGeneratorFunction%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 27.4.3.2 AsyncGeneratorFunction.prototype.prototype
        // https://tc39.es/ecma262/#sec-asyncgeneratorfunction-prototype-prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%AsyncGeneratorPrototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 27.4.3.3 AsyncGeneratorFunction.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-asyncgeneratorfunction-prototype-tostringtag
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("AsyncGeneratorFunction"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }
};
