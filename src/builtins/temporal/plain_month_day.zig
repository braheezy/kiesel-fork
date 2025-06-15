//! 10 Temporal.PlainMonthDay Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday-objects

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

/// 10.2 Properties of the Temporal.PlainMonthDay Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainmonthday-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            2,
            "PlainMonthDay",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 10.2.1 Temporal.PlainMonthDay.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.PlainMonthDay.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 10.1.1 Temporal.PlainMonthDay ( isoMonth, isoDay [ , calendar [ , referenceISOYear ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday
    fn impl(agent: *Agent, _: Arguments, _: ?*Object) Agent.Error!Value {
        return agent.throwException(.internal_error, "Not implemented", .{});
    }
};

/// 10.3 Properties of the Temporal.PlainMonthDay Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainmonthday-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 10.3.1 Temporal.PlainMonthDay.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.PlainMonthDay%"()),
        );

        // 10.3.2 Temporal.PlainMonthDay.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.PlainMonthDay"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }
};

/// 10.4 Properties of Temporal.PlainMonthDay Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plainmonthday-instances
pub const PlainMonthDay = MakeObject(.{
    .tag = .temporal_plain_month_day,
});
