//! 5 Temporal.PlainDateTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime-objects

const std = @import("std");

const temporal_rs = @import("../../c/temporal_rs.zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeCalendar = builtins.canonicalizeCalendar;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createTemporalDate = builtins.createTemporalDate;
const createTemporalTime = builtins.createTemporalTime;
const getTemporalCalendarIdentifierWithISODefault = builtins.getTemporalCalendarIdentifierWithISODefault;
const getTemporalFractionalSecondDigitsOption = builtins.getTemporalFractionalSecondDigitsOption;
const getTemporalOverflowOption = builtins.getTemporalOverflowOption;
const getTemporalRoundingModeOption = builtins.getTemporalRoundingModeOption;
const getTemporalShowCalendarNameOption = builtins.getTemporalShowCalendarNameOption;
const getTemporalUnitValuedOption = builtins.getTemporalUnitValuedOption;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const toMonthCode = builtins.toMonthCode;
const validateTemporalUnitValue = builtins.validateTemporalUnitValue;

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
        try object.defineBuiltinFunction(agent, "compare", compare, 2, realm);
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);

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
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const iso_year_value = arguments.get(0);
        const iso_month_value = arguments.get(1);
        const iso_day_value = arguments.get(2);
        const hour_value = arguments.get(3);
        const minute_value = arguments.get(4);
        const second_value = arguments.get(5);
        const millisecond_value = arguments.get(6);
        const microsecond_value = arguments.get(7);
        const nanosecond_value = arguments.get(8);
        var calendar_value = arguments.get(9);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.PlainDateTime must be constructed with 'new'",
                .{},
            );
        };

        // 2. Set isoYear to ? ToIntegerWithTruncation(isoYear).
        const iso_year = try iso_year_value.toIntegerWithTruncation(agent);

        // 3. Set isoMonth to ? ToIntegerWithTruncation(isoMonth).
        const iso_month = try iso_month_value.toIntegerWithTruncation(agent);

        // 4. Set isoDay to ? ToIntegerWithTruncation(isoDay).
        const iso_day = try iso_day_value.toIntegerWithTruncation(agent);

        // 5. If hour is undefined, set hour to 0; else set hour to ? ToIntegerWithTruncation(hour).
        const hour = if (hour_value.isUndefined()) 0 else try hour_value.toIntegerWithTruncation(agent);

        // 6. If minute is undefined, set minute to 0; else set minute to ? ToIntegerWithTruncation(minute).
        const minute = if (minute_value.isUndefined()) 0 else try minute_value.toIntegerWithTruncation(agent);

        // 7. If second is undefined, set second to 0; else set second to ? ToIntegerWithTruncation(second).
        const second = if (second_value.isUndefined()) 0 else try second_value.toIntegerWithTruncation(agent);

        // 8. If millisecond is undefined, set millisecond to 0; else set millisecond to ? ToIntegerWithTruncation(millisecond).
        const millisecond = if (millisecond_value.isUndefined()) 0 else try millisecond_value.toIntegerWithTruncation(agent);

        // 9. If microsecond is undefined, set microsecond to 0; else set microsecond to ? ToIntegerWithTruncation(microsecond).
        const microsecond = if (microsecond_value.isUndefined()) 0 else try microsecond_value.toIntegerWithTruncation(agent);

        // 10. If nanosecond is undefined, set nanosecond to 0; else set nanosecond to ? ToIntegerWithTruncation(nanosecond).
        const nanosecond = if (nanosecond_value.isUndefined()) 0 else try nanosecond_value.toIntegerWithTruncation(agent);

        // 11. If calendar is undefined, set calendar to "iso8601".
        if (calendar_value.isUndefined()) calendar_value = Value.from("iso8601");

        // 12. If calendar is not a String, throw a TypeError exception.
        if (!calendar_value.isString()) {
            return agent.throwException(.type_error, "Calendar is not a string", .{});
        }

        // 13. Set calendar to ? CanonicalizeCalendar(calendar).
        const calendar = try canonicalizeCalendar(agent, calendar_value.asString());

        // 14. If IsValidISODate(isoYear, isoMonth, isoDay) is false, throw a RangeError exception.
        // 15. Let isoDate be CreateISODateRecord(isoYear, isoMonth, isoDay).
        // 16. If IsValidTime(hour, minute, second, millisecond, microsecond, nanosecond) is false, throw a RangeError exception.
        // 17. Let time be CreateTimeRecord(hour, minute, second, millisecond, microsecond, nanosecond).
        // 18. Let isoDateTime be CombineISODateAndTimeRecord(isoDate, time).
        // 19. Return ? CreateTemporalDateTime(isoDateTime, calendar, NewTarget).
        const temporal_rs_plain_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_try_new(
                std.math.lossyCast(i32, iso_year),
                std.math.lossyCast(u8, iso_month),
                std.math.lossyCast(u8, iso_day),
                std.math.lossyCast(u8, hour),
                std.math.lossyCast(u8, minute),
                std.math.lossyCast(u8, second),
                std.math.lossyCast(u16, millisecond),
                std.math.lossyCast(u16, microsecond),
                std.math.lossyCast(u16, nanosecond),
                calendar,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid date time", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_PlainDateTime_destroy(temporal_rs_plain_date_time.?);
        return Value.from(
            try createTemporalDateTime(agent, temporal_rs_plain_date_time.?, new_target),
        );
    }

    /// 5.2.3 Temporal.PlainDateTime.compare ( one, two )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.compare
    fn compare(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const one_value = arguments.get(0);
        const two_value = arguments.get(1);

        // 1. Set one to ? ToTemporalDateTime(one).
        const one = try toTemporalPlainDateTime(agent, one_value, null);

        // 2. Set two to ? ToTemporalDateTime(two).
        const two = try toTemporalPlainDateTime(agent, two_value, null);

        // 3. Return 𝔽(CompareISODateTime(one.[[ISODateTime]], two.[[ISODateTime]])).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_compare(
                one.as(PlainDateTime).fields.inner,
                two.as(PlainDateTime).fields.inner,
            ),
        );
    }

    /// 5.2.2 Temporal.PlainDateTime.from ( item [ , options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.from
    fn from(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const item = arguments.get(0);
        const options = arguments.get(1);

        // 1. Return ? ToTemporalDateTime(item, options).
        return Value.from(try toTemporalPlainDateTime(agent, item, options));
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
        try object.defineBuiltinAccessor(agent, "calendarId", calendarId, null, realm);
        try object.defineBuiltinAccessor(agent, "day", day, null, realm);
        try object.defineBuiltinAccessor(agent, "dayOfWeek", dayOfWeek, null, realm);
        try object.defineBuiltinAccessor(agent, "dayOfYear", dayOfYear, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInMonth", daysInMonth, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInWeek", daysInWeek, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInYear", daysInYear, null, realm);
        try object.defineBuiltinAccessor(agent, "era", era, null, realm);
        try object.defineBuiltinAccessor(agent, "eraYear", eraYear, null, realm);
        try object.defineBuiltinAccessor(agent, "hour", hour, null, realm);
        try object.defineBuiltinAccessor(agent, "inLeapYear", inLeapYear, null, realm);
        try object.defineBuiltinAccessor(agent, "microsecond", microsecond, null, realm);
        try object.defineBuiltinAccessor(agent, "millisecond", millisecond, null, realm);
        try object.defineBuiltinAccessor(agent, "minute", minute, null, realm);
        try object.defineBuiltinAccessor(agent, "month", month, null, realm);
        try object.defineBuiltinAccessor(agent, "monthCode", monthCode, null, realm);
        try object.defineBuiltinAccessor(agent, "monthsInYear", monthsInYear, null, realm);
        try object.defineBuiltinAccessor(agent, "nanosecond", nanosecond, null, realm);
        try object.defineBuiltinAccessor(agent, "second", second, null, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toPlainDate", toPlainDate, 0, realm);
        try object.defineBuiltinFunction(agent, "toPlainTime", toPlainTime, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);
        try object.defineBuiltinAccessor(agent, "weekOfYear", weekOfYear, null, realm);
        try object.defineBuiltinAccessor(agent, "year", year, null, realm);
        try object.defineBuiltinAccessor(agent, "yearOfWeek", yearOfWeek, null, realm);

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

    /// 5.3.3 get Temporal.PlainDateTime.prototype.calendarId
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.calendarid
    fn calendarId(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return plainDateTime.[[Calendar]].
        const temporal_rs_calendar = temporal_rs.c.temporal_rs_PlainDateTime_calendar(
            plain_date_time.fields.inner,
        );
        const calendar_id = temporal_rs.fromDiplomatStringView(
            temporal_rs.c.temporal_rs_Calendar_identifier(temporal_rs_calendar.?),
        );
        return Value.from(
            try String.fromAscii(agent, try agent.gc_allocator.dupe(u8, calendar_id)),
        );
    }

    /// 5.3.9 get Temporal.PlainDateTime.prototype.day
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.day
    fn day(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[Day]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_day(plain_date_time.fields.inner),
        );
    }

    /// 5.3.16 get Temporal.PlainDateTime.prototype.dayOfWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.dayofweek
    fn dayOfWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[DayOfWeek]]).
        const day_of_week = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_day_of_week(plain_date_time.fields.inner),
        ) catch |err| switch (err) {
            // https://github.com/boa-dev/temporal/blob/531cee14769e8c077c59a1faf67d5465e85f5afa/src/builtins/core/calendar.rs#L406-L407
            error.RangeError => {
                return agent.throwException(.internal_error, "Not implemented", .{});
            },
            else => unreachable,
        };
        return Value.from(day_of_week);
    }

    /// 5.3.17 get Temporal.PlainDateTime.prototype.dayOfYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.dayofyear
    fn dayOfYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[DayOfYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_day_of_year(plain_date_time.fields.inner),
        );
    }

    /// 5.3.21 get Temporal.PlainDateTime.prototype.daysInMonth
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.daysinmonth
    fn daysInMonth(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[DaysInMonth]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_days_in_month(plain_date_time.fields.inner),
        );
    }

    /// 5.3.20 get Temporal.PlainDateTime.prototype.daysInWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.daysinweek
    fn daysInWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[DaysInWeek]]).
        const days_in_week = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_days_in_week(plain_date_time.fields.inner),
        ) catch |err| switch (err) {
            // https://github.com/boa-dev/temporal/blob/531cee14769e8c077c59a1faf67d5465e85f5afa/src/builtins/core/calendar.rs#L442-L443
            error.RangeError => {
                return agent.throwException(.internal_error, "Not implemented", .{});
            },
            else => unreachable,
        };
        return Value.from(days_in_week);
    }

    /// 5.3.22 get Temporal.PlainDateTime.prototype.daysInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.daysinyear
    fn daysInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[DaysInYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_days_in_year(plain_date_time.fields.inner),
        );
    }

    /// 5.3.4 get Temporal.PlainDateTime.prototype.era
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.era
    fn era(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[Era]].
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_PlainDateTime_era(plain_date_time.fields.inner, &write.inner);
        if (write.inner.len == 0) {
            std.debug.assert(write.inner.cap == 0); // Nothing to free
            return .undefined;
        }
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 5.3.5 get Temporal.PlainDateTime.prototype.eraYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.erayear
    fn eraYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Let result be CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[EraYear]].
        const result = temporal_rs.c.temporal_rs_PlainDateTime_era_year(
            plain_date_time.fields.inner,
        );

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 5.3.10 get Temporal.PlainDateTime.prototype.hour
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.hour
    fn hour(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(plainDateTime.[[ISODateTime]].[[Time]].[[Hour]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_hour(plain_date_time.fields.inner),
        );
    }

    /// 5.3.24 get Temporal.PlainDateTime.prototype.inLeapYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.inleapyear
    fn inLeapYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[InLeapYear]].
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_in_leap_year(plain_date_time.fields.inner),
        );
    }

    /// 5.3.14 get Temporal.PlainDateTime.prototype.microsecond
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.microsecond
    fn microsecond(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(plainDateTime.[[ISODateTime]].[[Time]].[[Microsecond]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_microsecond(plain_date_time.fields.inner),
        );
    }

    /// 5.3.13 get Temporal.PlainDateTime.prototype.millisecond
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.millisecond
    fn millisecond(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(plainDateTime.[[ISODateTime]].[[Time]].[[Millisecond]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_millisecond(plain_date_time.fields.inner),
        );
    }

    /// 5.3.11 get Temporal.PlainDateTime.prototype.minute
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.minute
    fn minute(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(plainDateTime.[[ISODateTime]].[[Time]].[[Minute]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_minute(plain_date_time.fields.inner),
        );
    }

    /// 5.3.7 get Temporal.PlainDateTime.prototype.month
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.month
    fn month(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[Month]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_month(plain_date_time.fields.inner),
        );
    }

    /// 5.3.8 get Temporal.PlainDateTime.prototype.monthCode
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.monthcode
    fn monthCode(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[MonthCode]].
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_PlainDateTime_month_code(
            plain_date_time.fields.inner,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 5.3.23 get Temporal.PlainDateTime.prototype.monthsInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.monthsinyear
    fn monthsInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[MonthsInYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_months_in_year(plain_date_time.fields.inner),
        );
    }

    /// 5.3.15 get Temporal.PlainDateTime.prototype.nanosecond
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.nanosecond
    fn nanosecond(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(plainDateTime.[[ISODateTime]].[[Time]].[[Nanosecond]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_nanosecond(plain_date_time.fields.inner),
        );
    }

    /// 5.3.12 get Temporal.PlainDateTime.prototype.second
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.second
    fn second(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(plainDateTime.[[ISODateTime]].[[Time]].[[Second]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_second(plain_date_time.fields.inner),
        );
    }

    /// 5.3.36 Temporal.PlainDateTime.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return ISODateTimeToString(plainDateTime.[[ISODateTime]], plainDateTime.[[Calendar]],
        //    auto, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_to_ixdtf_string(
                plain_date_time.fields.inner,
                temporal_rs.to_string_rounding_options_auto,
                temporal_rs.c.DisplayCalendar_Auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 5.3.35 Temporal.PlainDateTime.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return ISODateTimeToString(plainDateTime.[[ISODateTime]], plainDateTime.[[Calendar]],
        //    auto, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_to_ixdtf_string(
                plain_date_time.fields.inner,
                temporal_rs.to_string_rounding_options_auto,
                temporal_rs.c.DisplayCalendar_Auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 5.3.39 Temporal.PlainDateTime.prototype.toPlainDate ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.toplaindate
    fn toPlainDate(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return ! CreateTemporalDate(plainDateTime.[[ISODateTime]].[[ISODate]],
        //    plainDateTime.[[Calendar]]).
        const temporal_rs_plain_date = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_to_plain_date(plain_date_time.fields.inner),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_PlainDate_destroy(temporal_rs_plain_date.?);
        return Value.from(
            createTemporalDate(
                agent,
                temporal_rs_plain_date.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 5.3.40 Temporal.PlainDateTime.prototype.toPlainTime ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.toplaintime
    fn toPlainTime(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return ! CreateTemporalTime(plainDateTime.[[ISODateTime]].[[Time]]).
        const temporal_rs_plain_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_to_plain_time(plain_date_time.fields.inner),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_PlainTime_destroy(temporal_rs_plain_time.?);
        return Value.from(
            createTemporalTime(
                agent,
                temporal_rs_plain_time.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 5.3.34 Temporal.PlainDateTime.prototype.toString ( [ options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const options_value = arguments.get(0);

        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 4. NOTE: The following steps read options and perform independent validation in
        //    alphabetical order (GetTemporalShowCalendarNameOption reads "calendarName",
        //    GetTemporalFractionalSecondDigitsOption reads "fractionalSecondDigits", and
        //    GetRoundingModeOption reads "roundingMode").

        // 5. Let showCalendar be ? GetTemporalShowCalendarNameOption(resolvedOptions).
        const show_calendar = try getTemporalShowCalendarNameOption(agent, options);

        // 6. Let digits be ? GetTemporalFractionalSecondDigitsOption(resolvedOptions).
        const precision = try getTemporalFractionalSecondDigitsOption(agent, options);

        // 7. Let roundingMode be ? GetRoundingModeOption(resolvedOptions, trunc).
        const rounding_mode = try getTemporalRoundingModeOption(
            agent,
            options,
            temporal_rs.c.RoundingMode_Trunc,
        );

        // 8. Let smallestUnit be ? GetTemporalUnitValuedOption(resolvedOptions, "smallestUnit",
        //    unset).
        const smallest_unit = try getTemporalUnitValuedOption(
            agent,
            options,
            "smallestUnit",
            .unset,
        );

        // 9. Perform ? ValidateTemporalUnitValue(smallestUnit, time).
        try validateTemporalUnitValue(agent, smallest_unit, "smallestUnit", .time, &.{});

        // 10. If smallestUnit is hour, throw a RangeError exception.
        if (smallest_unit == temporal_rs.c.Unit_Hour) {
            return agent.throwException(
                .range_error,
                "Invalid value for option 'smallestUnit'",
                .{},
            );
        }

        // 11. Let precision be ToSecondsStringPrecisionRecord(smallestUnit, digits).
        // 12. Let result be RoundISODateTime(plainDateTime.[[ISODateTime]],
        //     precision.[[Increment]], precision.[[Unit]], roundingMode).
        // 13. If ISODateTimeWithinLimits(result) is false, throw a RangeError exception.
        // 14. Return ISODateTimeToString(result, plainDateTime.[[Calendar]],
        //     precision.[[Precision]], showCalendar).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_to_ixdtf_string(
                plain_date_time.fields.inner,
                .{
                    .precision = precision,
                    .smallest_unit = if (smallest_unit) |ok|
                        .{ .is_ok = true, .unnamed_0 = .{ .ok = ok } }
                    else
                        .{ .is_ok = false },
                    .rounding_mode = .{ .is_ok = true, .unnamed_0 = .{ .ok = rounding_mode } },
                },
                show_calendar,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 5.3.37 Temporal.PlainDateTime.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.PlainDateTime to primitive value",
            .{},
        );
    }

    /// 5.3.18 get Temporal.PlainDateTime.prototype.weekOfYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.weekofyear
    fn weekOfYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Let result be CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[WeekOfYear]].[[Week]].
        const result = temporal_rs.c.temporal_rs_PlainDateTime_week_of_year(
            plain_date_time.fields.inner,
        );

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 5.3.6 get Temporal.PlainDateTime.prototype.year
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.year
    fn year(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Return 𝔽(CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[Year]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainDateTime_year(plain_date_time.fields.inner),
        );
    }

    /// 5.3.19 get Temporal.PlainDateTime.prototype.yearOfWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.yearofweek
    fn yearOfWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(plainDateTime, [[InitializedTemporalDateTime]]).
        const plain_date_time = try this_value.requireInternalSlot(agent, PlainDateTime);

        // 3. Let result be CalendarISOToDate(plainDateTime.[[Calendar]],
        //    plainDateTime.[[ISODateTime]].[[ISODate]]).[[WeekOfYear]].[[Year]].
        const result = temporal_rs.c.temporal_rs_PlainDateTime_year_of_week(
            plain_date_time.fields.inner,
        );

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }
};

/// 5.4 Properties of Temporal.PlainDateTime Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plaindatetime-instances
pub const PlainDateTime = MakeObject(.{
    .Fields = struct {
        inner: *temporal_rs.c.PlainDateTime,
    },
    .finalizer = struct {
        fn finalizer(object: *Object) void {
            temporal_rs.c.temporal_rs_PlainDateTime_destroy(object.as(PlainDateTime).fields.inner);
        }
    }.finalizer,
    .tag = .temporal_plain_date_time,
});

/// 5.5.8 CreateTemporalDateTime ( isoDateTime, calendar [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaldatetime
pub fn createTemporalDateTime(
    agent: *Agent,
    inner: *temporal_rs.c.PlainDateTime,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If ISODateTimeWithinLimits(isoDateTime) is false, then
    //     a. Throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.PlainDateTime%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.PlainDateTime%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.PlainDateTime.prototype%",
    //    « [[InitializedTemporalDateTime]], [[ISODateTime]], [[Calendar]] »).
    // 4. Set object.[[ISODateTime]] to isoDateTime.
    // 5. Set object.[[Calendar]] to calendar.
    // 6. Return object.
    return ordinaryCreateFromConstructor(
        PlainDateTime,
        agent,
        new_target,
        "%Temporal.PlainDateTime.prototype%",
        .{ .inner = inner },
    );
}

/// 5.5.6 ToTemporalDateTime ( item [ , options ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporaldatetime
pub fn toTemporalPlainDateTime(
    agent: *Agent,
    item: Value,
    maybe_options_value: ?Value,
) Agent.Error!*Object {
    // 1. If options is not present, set options to undefined.
    const options_value: Value = maybe_options_value orelse .undefined;

    // 2. If item is an Object, then
    const temporal_rs_plain_date_time = if (item.isObject()) blk: {
        // a. If item has an [[InitializedTemporalDateTime]] internal slot, then
        if (item.asObject().is(PlainDateTime)) {
            // i. Let resolvedOptions be ? GetOptionsObject(options).
            const options = try options_value.getOptionsObject(agent);

            // ii. Perform ? GetTemporalOverflowOption(resolvedOptions).
            _ = try getTemporalOverflowOption(agent, options);

            // iii. Return ! CreateTemporalDateTime(item.[[ISODateTime]], item.[[Calendar]]).
            const plain_date_time = item.asObject().as(PlainDateTime);
            break :blk temporal_rs.c.temporal_rs_PlainDateTime_clone(plain_date_time.fields.inner);
        }

        // b. If item has an [[InitializedTemporalZonedDateTime]] internal slot, then
        if (item.asObject().is(builtins.temporal.ZonedDateTime)) {
            // i. Let isoDateTime be GetISODateTimeFor(item.[[TimeZone]], item.[[EpochNanoseconds]]).

            // ii. Let resolvedOptions be ? GetOptionsObject(options).
            const options = try options_value.getOptionsObject(agent);

            // iii. Perform ? GetTemporalOverflowOption(resolvedOptions).
            _ = try getTemporalOverflowOption(agent, options);

            // iv. Return ! CreateTemporalDateTime(isoDateTime, item.[[Calendar]]).
            const zoned_date_time = item.asObject().as(builtins.temporal.ZonedDateTime);
            break :blk temporal_rs.temporalErrorResult(
                temporal_rs.c.temporal_rs_ZonedDateTime_to_plain_datetime(
                    zoned_date_time.fields.inner,
                ),
            ) catch unreachable;
        }

        // c. If item has an [[InitializedTemporalDate]] internal slot, then
        if (item.asObject().is(builtins.temporal.PlainDate)) {
            // i. Let resolvedOptions be ? GetOptionsObject(options).
            const options = try options_value.getOptionsObject(agent);

            // ii. Perform ? GetTemporalOverflowOption(resolvedOptions).
            _ = try getTemporalOverflowOption(agent, options);

            // iii. Let isoDateTime be CombineISODateAndTimeRecord(item.[[ISODate]], MidnightTimeRecord()).
            // iv. Return ? CreateTemporalDateTime(isoDateTime, item.[[Calendar]]).
            const plain_date = item.asObject().as(builtins.temporal.PlainDate);
            const midnight = temporal_rs.temporalErrorResult(
                temporal_rs.c.temporal_rs_PlainTime_try_new(0, 0, 0, 0, 0, 0),
            ) catch unreachable;
            defer temporal_rs.c.temporal_rs_PlainTime_destroy(midnight.?);
            break :blk temporal_rs.temporalErrorResult(
                temporal_rs.c.temporal_rs_PlainDate_to_plain_date_time(
                    plain_date.fields.inner,
                    midnight.?,
                ),
            ) catch unreachable;
        }

        // d. Let calendar be ? GetTemporalCalendarIdentifierWithISODefault(item).
        // e. Let fields be ? PrepareCalendarFields(calendar, item, « year, month, month-code, day »,
        //    « hour, minute, second, millisecond, microsecond, nanosecond », «»).
        const partial_date_time = try toTemporalPartialDateTime(agent, item.asObject());

        // f. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // g. Let overflow be ? GetTemporalOverflowOption(resolvedOptions).
        const overflow = try getTemporalOverflowOption(agent, options);

        // h. Let result be ? InterpretTemporalDateTimeFields(calendar, fields, overflow).
        // i. Return ? CreateTemporalDateTime(result, calendar).
        break :blk temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_from_partial(
                partial_date_time,
                .{ .is_ok = true, .unnamed_0 = .{ .ok = overflow } },
            ),
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(.range_error, "Invalid plain datetime", .{});
            },
            error.TypeError => {
                return agent.throwException(.type_error, "Missing plain datetime field", .{});
            },
            else => unreachable,
        };
    } else blk: {
        // 3. If item is not a String, throw a TypeError exception.
        if (!item.isString()) {
            return agent.throwException(
                .type_error,
                "Plain date must be a string or object",
                .{},
            );
        }

        // 4. Let result be ? ParseISODateTime(item, « TemporalDateTimeString[~Zoned] »).
        // 5. If result.[[Time]] is start-of-day, let time be MidnightTimeRecord(); else let time
        //    be result.[[Time]].
        // 6. Let calendar be result.[[Calendar]].
        // 7. If calendar is empty, set calendar to "iso8601".
        // 8. Set calendar to ? CanonicalizeCalendar(calendar).
        const plain_date_time_utf8 = try item.asString().toUtf8(agent.gc_allocator);
        defer agent.gc_allocator.free(plain_date_time_utf8);
        const temporal_rs_plain_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDateTime_from_utf8(
                temporal_rs.toDiplomatStringView(plain_date_time_utf8),
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(
                .range_error,
                "Invalid plain datetime string",
                .{},
            ),
            else => unreachable,
        };

        // 9. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 10. Perform ? GetTemporalOverflowOption(resolvedOptions).
        _ = try getTemporalOverflowOption(agent, options);

        // 11. Let isoDate be CreateISODateRecord(result.[[Year]], result.[[Month]], result.[[Day]]).
        // 12. Let isoDateTime be CombineISODateAndTimeRecord(isoDate, time).
        // 13. Return ? CreateTemporalDateTime(isoDateTime, calendar).
        break :blk temporal_rs_plain_date_time;
    };
    errdefer temporal_rs.c.temporal_rs_PlainDateTime_destroy(temporal_rs_plain_date_time.?);

    return createTemporalDateTime(
        agent,
        temporal_rs_plain_date_time.?,
        null,
    ) catch |err| try noexcept(err);
}

/// Custom function to create a PartialDateTime based on:
///
/// 12.2.3 PrepareCalendarFields ( calendar, fields, calendarFieldNames, nonCalendarFieldNames, requiredFieldNames )
/// https://tc39.es/proposal-temporal/#sec-temporal-preparecalendarfields
fn toTemporalPartialDateTime(
    agent: *Agent,
    object: *Object,
) Agent.Error!temporal_rs.c.PartialDateTime {
    var result: temporal_rs.c.PartialDateTime = .{
        .date = .{
            .year = .{ .is_ok = false },
            .month = .{ .is_ok = false },
            .month_code = .{ .data = null },
            .day = .{ .is_ok = false },
            .era = .{ .data = null },
            .era_year = .{ .is_ok = false },
            .calendar = undefined,
        },
        .time = .{
            .hour = .{ .is_ok = false },
            .minute = .{ .is_ok = false },
            .second = .{ .is_ok = false },
            .millisecond = .{ .is_ok = false },
            .microsecond = .{ .is_ok = false },
            .nanosecond = .{ .is_ok = false },
        },
    };

    const calendar = try getTemporalCalendarIdentifierWithISODefault(agent, object);
    result.date.calendar = calendar;

    const day = try object.get(agent, PropertyKey.from("day"));
    if (!day.isUndefined()) {
        result.date.day = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u8, try day.toPositiveIntegerWithTruncation(agent)) },
        };
    }

    if (calendar != temporal_rs.c.AnyCalendarKind_Iso) {
        const era = try object.get(agent, PropertyKey.from("era"));
        if (!era.isUndefined()) {
            const era_string = try era.toString(agent);
            const era_utf8 = try era_string.toUtf8(agent.gc_allocator);
            result.date.era = temporal_rs.toDiplomatStringView(era_utf8);
        }

        const era_year = try object.get(agent, PropertyKey.from("eraYear"));
        if (!era_year.isUndefined()) {
            result.date.era_year = .{
                .is_ok = true,
                .unnamed_0 = .{ .ok = std.math.lossyCast(i32, try era_year.toIntegerWithTruncation(agent)) },
            };
        }
    }

    const hour = try object.get(agent, PropertyKey.from("hour"));
    if (!hour.isUndefined()) {
        result.time.hour = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u8, try hour.toIntegerWithTruncation(agent)) },
        };
    }

    const microsecond = try object.get(agent, PropertyKey.from("microsecond"));
    if (!microsecond.isUndefined()) {
        result.time.microsecond = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u16, try microsecond.toIntegerWithTruncation(agent)) },
        };
    }

    const millisecond = try object.get(agent, PropertyKey.from("millisecond"));
    if (!millisecond.isUndefined()) {
        result.time.millisecond = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u16, try millisecond.toIntegerWithTruncation(agent)) },
        };
    }

    const minute = try object.get(agent, PropertyKey.from("minute"));
    if (!minute.isUndefined()) {
        result.time.minute = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u8, try minute.toIntegerWithTruncation(agent)) },
        };
    }

    const month = try object.get(agent, PropertyKey.from("month"));
    if (!month.isUndefined()) {
        result.date.month = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u8, try month.toPositiveIntegerWithTruncation(agent)) },
        };
    }

    const month_code = try object.get(agent, PropertyKey.from("monthCode"));
    if (!month_code.isUndefined()) {
        const month_code_utf8 = try toMonthCode(agent, month_code);
        result.date.month_code = temporal_rs.toDiplomatStringView(month_code_utf8);
    }

    const nanosecond = try object.get(agent, PropertyKey.from("nanosecond"));
    if (!nanosecond.isUndefined()) {
        result.time.nanosecond = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u16, try nanosecond.toIntegerWithTruncation(agent)) },
        };
    }

    const second = try object.get(agent, PropertyKey.from("second"));
    if (!second.isUndefined()) {
        result.time.second = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u8, try second.toIntegerWithTruncation(agent)) },
        };
    }

    const year = try object.get(agent, PropertyKey.from("year"));
    if (!year.isUndefined()) {
        result.date.year = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i32, try year.toIntegerWithTruncation(agent)) },
        };
    }

    return result;
}
