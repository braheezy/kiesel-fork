//! 3 Temporal.PlainDate Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plaindate-objects

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
const canonicalizeCalendar = builtins.canonicalizeCalendar;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 3.2 Properties of the Temporal.PlainDate Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            3,
            "PlainDate",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 3.2.1 Temporal.PlainDate.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDate.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 3.1.1 Temporal.PlainDate ( isoYear, isoMonth, isoDay [ , calendar ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const iso_year_value = arguments.get(0);
        const iso_month_value = arguments.get(1);
        const iso_day_value = arguments.get(2);
        var calendar_value = arguments.get(3);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.PlainDate must be constructed with 'new'",
                .{},
            );
        };

        // 2. Let y be ? ToIntegerWithTruncation(isoYear).
        const iso_year = try iso_year_value.toIntegerWithTruncation(agent);

        // 3. Let m be ? ToIntegerWithTruncation(isoMonth).
        const iso_month = try iso_month_value.toIntegerWithTruncation(agent);

        // 4. Let d be ? ToIntegerWithTruncation(isoDay).
        const iso_day = try iso_day_value.toIntegerWithTruncation(agent);

        // 5. If calendar is undefined, set calendar to "iso8601".
        if (calendar_value.isUndefined()) calendar_value = Value.from("iso8601");

        // 6. If calendar is not a String, throw a TypeError exception.
        if (!calendar_value.isString()) {
            return agent.throwException(.type_error, "Calendar is not a string", .{});
        }

        // 7. Set calendar to ? CanonicalizeCalendar(calendar).
        const calendar = try canonicalizeCalendar(agent, calendar_value.asString());

        // 8. If IsValidISODate(y, m, d) is false, throw a RangeError exception.
        // 9. Let isoDate be CreateISODateRecord(y, m, d).
        // 10. Return ? CreateTemporalDate(isoDate, calendar, NewTarget).
        const temporal_rs_plain_date = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDate_try_new(
                std.math.lossyCast(i32, iso_year),
                std.math.lossyCast(u8, iso_month),
                std.math.lossyCast(u8, iso_day),
                calendar,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid date", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_PlainDate_destroy(temporal_rs_plain_date.?);
        std.debug.assert(temporal_rs.c.temporal_rs_PlainDate_is_valid(temporal_rs_plain_date.?));
        return Value.from(
            try createTemporalDate(agent, temporal_rs_plain_date.?, new_target),
        );
    }
};

/// 3.3 Properties of the Temporal.PlainDate Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

        // 3.3.1 Temporal.PlainDate.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDate%"()),
        );

        // 3.3.2 Temporal.PlainDate.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.PlainDate"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 3.3.33 Temporal.PlainDate.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.PlainDate to primitive value",
            .{},
        );
    }
};

/// 3.4 Properties of Temporal.PlainDate Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plaindate-instances
pub const PlainDate = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.PlainDate,
    },
    .tag = .temporal_plain_date,
});

/// 3.5.3 CreateTemporalDate ( isoDate, calendar [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaldate
pub fn createTemporalDate(
    agent: *Agent,
    inner: *temporal_rs.c.PlainDate,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If ISODateWithinLimits(isoDate) is false, throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.PlainDate%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.PlainDate%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.PlainDate.prototype%",
    //    « [[InitializedTemporalDate]], [[ISODate]], [[Calendar]] »).
    // 4. Set object.[[ISODate]] to isoDate.
    // 5. Set object.[[Calendar]] to calendar.
    // 6. Return object.
    return ordinaryCreateFromConstructor(
        PlainDate,
        agent,
        new_target,
        "%Temporal.PlainDate.prototype%",
        .{ .inner = inner },
    );
}
