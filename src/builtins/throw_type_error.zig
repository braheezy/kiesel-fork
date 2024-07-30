//! 10.2.4.1 %ThrowTypeError% ( )
//! https://tc39.es/ecma262/#sec-%throwtypeerror%

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
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

pub const ThrowTypeError = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return createBuiltinFunction(realm.agent, .{ .function = function }, .{
            .length = 0,
            .name = "",
            .realm = realm,
        });
    }

    pub fn init(_: *Realm, object: Object) Allocator.Error!void {
        // The value of the [[Extensible]] internal slot of this function is false.
        object.data.extensible = false;

        // The "length" property of this function has the attributes {
        //   [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
        // }.
        try defineBuiltinProperty(object, "length", PropertyDescriptor{
            .value = Value.from(0),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // The "name" property of this function has the attributes {
        //   [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false
        // }.
        try defineBuiltinProperty(object, "name", PropertyDescriptor{
            .value = Value.from(""),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    fn function(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(.type_error, "Forbidden property access", .{});
    }
};
