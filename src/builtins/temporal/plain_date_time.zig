//! 5 Temporal.PlainDateTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime-objects

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

/// 5.2 Properties of the Temporal.PlainDateTime Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindatetime-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            3,
            "PlainDateTime",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 5.2.1 Temporal.PlainDateTime.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDateTime.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 5.1.1 Temporal.PlainDateTime ( isoYear, isoMonth, isoDay [ , hour [ , minute [ , second [ , millisecond [ , microsecond [ , nanosecond [ , calendar ] ] ] ] ] ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime
    fn impl(agent: *Agent, _: Arguments, _: ?*Object) Agent.Error!Value {
        return agent.throwException(.internal_error, "Not implemented", .{});
    }
};

/// 5.3 Properties of the Temporal.PlainDateTime Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindatetime-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 5.3.1 Temporal.PlainDateTime.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDateTime%"()),
        );

        // 5.3.2 Temporal.PlainDateTime.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.PlainDateTime"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }
};

/// 5.4 Properties of Temporal.PlainDateTime Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plaindatetime-instances
pub const PlainDateTime = MakeObject(.{
    .tag = .temporal_plain_date_time,
});
