//! 10 Temporal.PlainMonthDay Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday-objects

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
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const iso_month_value = arguments.get(0);
        const iso_day_value = arguments.get(1);
        var calendar_value = arguments.get(2);
        var reference_iso_year_value = arguments.get(3);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.PlainMonthDay must be constructed with 'new'",
                .{},
            );
        };

        // 2. If referenceISOYear is undefined, then
        //     a. Set referenceISOYear to 1972𝔽 (the first ISO 8601 leap year after the epoch).
        if (reference_iso_year_value.isUndefined()) reference_iso_year_value = Value.from(1972);

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

        // 8. Let y be ? ToIntegerWithTruncation(referenceISOYear).
        const reference_iso_year = try reference_iso_year_value.toIntegerWithTruncation(agent);

        // 9. If IsValidISODate(y, m, d) is false, throw a RangeError exception.
        // 10. Let isoDate be CreateISODateRecord(y, m, d).
        // 11. Return ? CreateTemporalMonthDay(isoDate, calendar, NewTarget).
        const temporal_rs_plain_month_day = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainMonthDay_try_new_with_overflow(
                std.math.lossyCast(u8, iso_month),
                std.math.lossyCast(u8, iso_day),
                calendar,
                temporal_rs.c.ArithmeticOverflow_Reject,
                .{
                    .is_ok = true,
                    .unnamed_0 = .{ .ok = std.math.lossyCast(i32, reference_iso_year) },
                },
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid month day", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_PlainMonthDay_destroy(temporal_rs_plain_month_day.?);
        return Value.from(
            try createTemporalMonthDay(agent, temporal_rs_plain_month_day.?, new_target),
        );
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
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

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

    /// 10.3.11 Temporal.PlainMonthDay.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.PlainMonthDay to primitive value",
            .{},
        );
    }
};

/// 10.4 Properties of Temporal.PlainMonthDay Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plainmonthday-instances
pub const PlainMonthDay = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.PlainMonthDay,
    },
    .tag = .temporal_plain_month_day,
});

/// 10.5.2 CreateTemporalMonthDay ( isoDate, calendar [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalmonthday
pub fn createTemporalMonthDay(
    agent: *Agent,
    inner: *temporal_rs.c.PlainMonthDay,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If ISODateWithinLimits(isoDate) is false, throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.PlainMonthDay%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.PlainMonthDay%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.PlainMonthDay.prototype%",
    //    « [[InitializedTemporalMonthDay]], [[ISODate]], [[Calendar]] »).
    // 4. Set object.[[ISODate]] to isoDate.
    // 5. Set object.[[Calendar]] to calendar.
    // 6. Return object.
    return ordinaryCreateFromConstructor(
        PlainMonthDay,
        agent,
        new_target,
        "%Temporal.PlainMonthDay.prototype%",
        .{ .inner = inner },
    );
}
