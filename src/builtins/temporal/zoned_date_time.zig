//! 6 Temporal.ZonedDateTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime-objects

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

/// 6.2 Properties of the Temporal.ZonedDateTime Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-zoneddatetime-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            2,
            "ZonedDateTime",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 6.2.1 Temporal.ZonedDateTime.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.ZonedDateTime.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 6.1.1 Temporal.ZonedDateTime ( epochNanoseconds, timeZone [ , calendar ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime
    fn impl(agent: *Agent, _: Arguments, _: ?*Object) Agent.Error!Value {
        return agent.throwException(.internal_error, "Not implemented", .{});
    }
};

/// 6.3 Properties of the Temporal.ZonedDateTime Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-zoneddatetime-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 6.3.1 Temporal.ZonedDateTime.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.ZonedDateTime%"()),
        );

        // 6.3.2 Temporal.ZonedDateTime.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.ZonedDateTime"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }
};

/// 6.4 Properties of Temporal.ZonedDateTime Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-zoneddatetime-instances
pub const ZonedDateTime = MakeObject(.{
    .tag = .temporal_zoned_date_time,
});
