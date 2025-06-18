//! 4 Temporal.PlainTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plaintime-objects

const std = @import("std");

const temporal_rs = @import("../../c/temporal_rs.zig");

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
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

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
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const hour_value = arguments.get(0);
        const minute_value = arguments.get(1);
        const second_value = arguments.get(2);
        const millisecond_value = arguments.get(3);
        const microsecond_value = arguments.get(4);
        const nanosecond_value = arguments.get(5);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.PlainTime must be constructed with 'new'",
                .{},
            );
        };

        // 2. If hour is undefined, set hour to 0; else set hour to ? ToIntegerWithTruncation(hour).
        const hour = if (hour_value.isUndefined()) 0 else try hour_value.toIntegerWithTruncation(agent);

        // 3. If minute is undefined, set minute to 0; else set minute to ? ToIntegerWithTruncation(minute).
        const minute = if (minute_value.isUndefined()) 0 else try minute_value.toIntegerWithTruncation(agent);

        // 4. If second is undefined, set second to 0; else set second to ? ToIntegerWithTruncation(second).
        const second = if (second_value.isUndefined()) 0 else try second_value.toIntegerWithTruncation(agent);

        // 5. If millisecond is undefined, set millisecond to 0; else set millisecond to ? ToIntegerWithTruncation(millisecond).
        const millisecond = if (millisecond_value.isUndefined()) 0 else try millisecond_value.toIntegerWithTruncation(agent);

        // 6. If microsecond is undefined, set microsecond to 0; else set microsecond to ? ToIntegerWithTruncation(microsecond).
        const microsecond = if (microsecond_value.isUndefined()) 0 else try microsecond_value.toIntegerWithTruncation(agent);

        // 7. If nanosecond is undefined, set nanosecond to 0; else set nanosecond to ? ToIntegerWithTruncation(nanosecond).
        const nanosecond = if (nanosecond_value.isUndefined()) 0 else try nanosecond_value.toIntegerWithTruncation(agent);

        // 8. If IsValidTime(hour, minute, second, millisecond, microsecond, nanosecond) is false,
        //    throw a RangeError exception.
        // 9. Let time be CreateTimeRecord(hour, minute, second, millisecond, microsecond, nanosecond).
        // 10. Return ? CreateTemporalTime(time, NewTarget).
        const temporal_rs_plain_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainTime_try_new(
                std.math.lossyCast(u8, hour),
                std.math.lossyCast(u8, minute),
                std.math.lossyCast(u8, second),
                std.math.lossyCast(u16, millisecond),
                std.math.lossyCast(u16, microsecond),
                std.math.lossyCast(u16, nanosecond),
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid time", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_PlainTime_destroy(temporal_rs_plain_time.?);
        return Value.from(
            try createTemporalTime(agent, temporal_rs_plain_time.?, new_target),
        );
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
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.PlainTime,
    },
    .tag = .temporal_plain_time,
});

/// 4.5.11 CreateTemporalTime ( time [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaltime
pub fn createTemporalTime(
    agent: *Agent,
    inner: *temporal_rs.c.PlainTime,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If newTarget is not present, set newTarget to %Temporal.PlainTime%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.PlainTime%"();

    // 2. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.PlainTime.prototype%",
    //    « [[InitializedTemporalTime]], [[Time]] »).
    // 3. Set object.[[Time]] to time.
    // 4. Return object.
    return ordinaryCreateFromConstructor(
        PlainTime,
        agent,
        new_target,
        "%Temporal.PlainTime.prototype%",
        .{ .inner = inner },
    );
}
