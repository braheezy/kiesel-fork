//! 4 Temporal.PlainTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plaintime-objects

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;

/// 4.2 Properties of the Temporal.PlainTime Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaintime-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "PlainTime",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 4.2.1 Temporal.PlainTime.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.PlainTime.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 4.1.1 Temporal.PlainTime ( [ hour [ , minute [ , second [ , millisecond [ , microsecond [ , nanosecond ] ] ] ] ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaintime
    fn impl(agent: *Agent, _: Arguments, _: ?*Object) Agent.Error!Value {
        return agent.throwException(.internal_error, "Not implemented", .{});
    }
};

/// 4.3 Properties of the Temporal.PlainTime Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaintime-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 4.3.1 Temporal.PlainTime.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.PlainTime%"()),
        );

        // 4.3.2 Temporal.PlainTime.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.PlainTime"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }
};

/// 4.4 Properties of Temporal.PlainTime Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plaintime-instances
pub const PlainTime = MakeObject(.{
    .tag = .temporal_plain_time,
});
