//! 6 Temporal.ZonedDateTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime-objects

const std = @import("std");

const temporal_rs = @import("../../c/temporal_rs.zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BigInt = types.BigInt;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeCalendar = builtins.canonicalizeCalendar;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createTemporalInstant = builtins.createTemporalInstant;
const createTemporalDate = builtins.createTemporalDate;
const createTemporalDateTime = builtins.createTemporalDateTime;
const createTemporalTime = builtins.createTemporalTime;
const getTemporalCalendarIdentifierWithISODefault = builtins.getTemporalCalendarIdentifierWithISODefault;
const getTemporalDirectionOption = builtins.getTemporalDirectionOption;
const getTemporalDisambiguationOption = builtins.getTemporalDisambiguationOption;
const getTemporalFractionalSecondDigitsOption = builtins.getTemporalFractionalSecondDigitsOption;
const getTemporalOffsetOption = builtins.getTemporalOffsetOption;
const getTemporalOverflowOption = builtins.getTemporalOverflowOption;
const getTemporalRoundingModeOption = builtins.getTemporalRoundingModeOption;
const getTemporalShowCalendarNameOption = builtins.getTemporalShowCalendarNameOption;
const getTemporalShowOffsetOption = builtins.getTemporalShowOffsetOption;
const getTemporalShowTimeZoneNameOption = builtins.getTemporalShowTimeZoneNameOption;
const getTemporalUnitValuedOption = builtins.getTemporalUnitValuedOption;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const toMonthCode = builtins.toMonthCode;
const toOffsetString = builtins.toOffsetString;
const toTemporalTimeZoneIdentifier = builtins.toTemporalTimeZoneIdentifier;
const validateTemporalUnitValue = builtins.validateTemporalUnitValue;

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
        try object.defineBuiltinFunction(agent, "compare", compare, 2, realm);
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);

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
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const epoch_nanoseconds_value = arguments.get(0);
        const time_zone_value = arguments.get(1);
        var calendar_value = arguments.get(2);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.ZonedDateTime must be constructed with 'new'",
                .{},
            );
        };

        // 2. Set epochNanoseconds to ? ToBigInt(epochNanoseconds).
        const epoch_nanoseconds_bigint = try epoch_nanoseconds_value.toBigInt(agent);
        const epoch_nanoseconds = temporal_rs.toI128Nanoseconds(
            epoch_nanoseconds_bigint.managed.toInt(i128) catch std.math.maxInt(i128),
        );

        // 3. If IsValidEpochNanoseconds(epochNanoseconds) is false, throw a RangeError exception.
        if (!temporal_rs.c.temporal_rs_I128Nanoseconds_is_valid(epoch_nanoseconds)) {
            return agent.throwException(
                .range_error,
                "Invalid epoch nanoseconds {}",
                .{epoch_nanoseconds_bigint.managed},
            );
        }

        // 4. If timeZone is not a String, throw a TypeError exception.
        if (!time_zone_value.isString()) {
            return agent.throwException(.type_error, "Time zone is not a string", .{});
        }

        // 5. Let timeZoneParse be ? ParseTimeZoneIdentifier(timeZone).
        // 6. If timeZoneParse.[[OffsetMinutes]] is empty, then
        //     a. Let identifierRecord be GetAvailableNamedTimeZoneIdentifier(timeZoneParse.[[Name]]).
        //     b. If identifierRecord is empty, throw a RangeError exception.
        //     c. Set timeZone to identifierRecord.[[Identifier]].
        // 7. Else,
        //     a. Set timeZone to FormatOffsetTimeZoneIdentifier(timeZoneParse.[[OffsetMinutes]]).
        const time_zone = try time_zone_value.asString().toUtf8(agent.gc_allocator);
        defer agent.gc_allocator.free(time_zone);
        const temporal_rs_time_zone = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_TimeZone_try_from_identifier_str(
                temporal_rs.toDiplomatStringView(time_zone),
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid time zone", .{}),
            else => unreachable,
        };
        if (!temporal_rs.c.temporal_rs_TimeZone_is_valid(temporal_rs_time_zone.?)) {
            return agent.throwException(.range_error, "Invalid time zone", .{});
        }
        errdefer temporal_rs.c.temporal_rs_TimeZone_destroy(temporal_rs_time_zone.?);

        // 8. If calendar is undefined, set calendar to "iso8601".
        if (calendar_value.isUndefined()) calendar_value = Value.from("iso8601");

        // 9. If calendar is not a String, throw a TypeError exception.
        if (!calendar_value.isString()) {
            return agent.throwException(.type_error, "Calendar is not a string", .{});
        }

        // 10. Set calendar to ? CanonicalizeCalendar(calendar).
        const calendar = try canonicalizeCalendar(agent, calendar_value.asString());

        // 11. Return ? CreateTemporalZonedDateTime(epochNanoseconds, timeZone, calendar, NewTarget).
        const temporal_rs_zoned_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_try_new(
                epoch_nanoseconds,
                calendar,
                temporal_rs_time_zone,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid duration", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_ZonedDateTime_destroy(temporal_rs_zoned_date_time.?);
        return Value.from(
            try createTemporalZonedDateTime(agent, temporal_rs_zoned_date_time.?, new_target),
        );
    }

    /// 6.2.3 Temporal.ZonedDateTime.compare ( one, two )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.compare
    fn compare(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const one_value = arguments.get(0);
        const two_value = arguments.get(1);

        // 1. Set one to ? ToTemporalZonedDateTime(one).
        const one = try toTemporalZonedDateTime(agent, one_value, null);

        // 2. Set two to ? ToTemporalZonedDateTime(two).
        const two = try toTemporalZonedDateTime(agent, two_value, null);

        // 3. Return 𝔽(CompareEpochNanoseconds(one.[[EpochNanoseconds]], two.[[EpochNanoseconds]])).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_compare_instant(
                one.as(ZonedDateTime).fields.inner,
                two.as(ZonedDateTime).fields.inner,
            ),
        );
    }

    /// 6.2.2 Temporal.ZonedDateTime.from ( item [ , options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.from
    fn from(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const item = arguments.get(0);
        const options = arguments.get(1);

        // 1. Return ? ToTemporalZonedDateTime(item, options).
        return Value.from(try toTemporalZonedDateTime(agent, item, options));
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
        try object.defineBuiltinAccessor(agent, "calendarId", calendarId, null, realm);
        try object.defineBuiltinAccessor(agent, "day", day, null, realm);
        try object.defineBuiltinAccessor(agent, "dayOfWeek", dayOfWeek, null, realm);
        try object.defineBuiltinAccessor(agent, "dayOfYear", dayOfYear, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInMonth", daysInMonth, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInWeek", daysInWeek, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInYear", daysInYear, null, realm);
        try object.defineBuiltinAccessor(agent, "epochMilliseconds", epochMilliseconds, null, realm);
        try object.defineBuiltinAccessor(agent, "epochNanoseconds", epochNanoseconds, null, realm);
        try object.defineBuiltinFunction(agent, "equals", equals, 1, realm);
        try object.defineBuiltinAccessor(agent, "era", era, null, realm);
        try object.defineBuiltinAccessor(agent, "eraYear", eraYear, null, realm);
        try object.defineBuiltinFunction(agent, "getTimeZoneTransition", getTimeZoneTransition, 1, realm);
        try object.defineBuiltinAccessor(agent, "hour", hour, null, realm);
        try object.defineBuiltinAccessor(agent, "hoursInDay", hoursInDay, null, realm);
        try object.defineBuiltinAccessor(agent, "inLeapYear", inLeapYear, null, realm);
        try object.defineBuiltinAccessor(agent, "microsecond", microsecond, null, realm);
        try object.defineBuiltinAccessor(agent, "millisecond", millisecond, null, realm);
        try object.defineBuiltinAccessor(agent, "minute", minute, null, realm);
        try object.defineBuiltinAccessor(agent, "month", month, null, realm);
        try object.defineBuiltinAccessor(agent, "monthCode", monthCode, null, realm);
        try object.defineBuiltinAccessor(agent, "monthsInYear", monthsInYear, null, realm);
        try object.defineBuiltinAccessor(agent, "nanosecond", nanosecond, null, realm);
        try object.defineBuiltinAccessor(agent, "offset", offset, null, realm);
        try object.defineBuiltinAccessor(agent, "offsetNanoseconds", offsetNanoseconds, null, realm);
        try object.defineBuiltinAccessor(agent, "second", second, null, realm);
        try object.defineBuiltinFunction(agent, "startOfDay", startOfDay, 0, realm);
        try object.defineBuiltinAccessor(agent, "timeZoneId", timeZoneId, null, realm);
        try object.defineBuiltinFunction(agent, "toInstant", toInstant, 0, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toPlainDate", toPlainDate, 0, realm);
        try object.defineBuiltinFunction(agent, "toPlainDateTime", toPlainDateTime, 0, realm);
        try object.defineBuiltinFunction(agent, "toPlainTime", toPlainTime, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);
        try object.defineBuiltinAccessor(agent, "weekOfYear", weekOfYear, null, realm);
        try object.defineBuiltinAccessor(agent, "year", year, null, realm);
        try object.defineBuiltinAccessor(agent, "yearOfWeek", yearOfWeek, null, realm);

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

    /// 6.3.3 get Temporal.ZonedDateTime.prototype.calendarId
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.calendarid
    fn calendarId(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return zonedDateTime.[[Calendar]].
        const temporal_rs_calendar = temporal_rs.c.temporal_rs_ZonedDateTime_calendar(
            zoned_date_time.fields.inner,
        );
        const calendar_id = temporal_rs.fromDiplomatStringView(
            temporal_rs.c.temporal_rs_Calendar_identifier(temporal_rs_calendar.?),
        );
        return Value.from(
            try String.fromAscii(agent, try agent.gc_allocator.dupe(u8, calendar_id)),
        );
    }

    /// 6.3.10 get Temporal.ZonedDateTime.prototype.day
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.day
    fn day(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[Day]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_day(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.25 get Temporal.ZonedDateTime.prototype.daysInMonth
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.daysinmonth
    fn daysInMonth(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[DaysInMonth]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_days_in_month(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.19 get Temporal.ZonedDateTime.prototype.dayOfWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.dayoftheweek
    fn dayOfWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[DayOfWeek]]).
        const day_of_week = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_day_of_week(zoned_date_time.fields.inner),
        ) catch unreachable;
        return Value.from(day_of_week);
    }

    /// 6.3.20 get Temporal.ZonedDateTime.prototype.dayOfYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.dayoftheyear
    fn dayOfYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[DayOfYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_day_of_year(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.24 get Temporal.ZonedDateTime.prototype.daysInWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.daysinweek
    fn daysInWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[DaysInWeek]]).
        const days_in_week = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_days_in_week(zoned_date_time.fields.inner),
        ) catch unreachable;
        return Value.from(days_in_week);
    }

    /// 6.3.26 get Temporal.ZonedDateTime.prototype.daysInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.daysinyear
    fn daysInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[DaysInYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_days_in_year(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.17 get Temporal.ZonedDateTime.prototype.epochMilliseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.epochmilliseconds
    fn epochMilliseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let ns be zonedDateTime.[[EpochNanoseconds]].
        // 4. Let ms be floor(ℝ(ns) / 10**6).
        const ms = temporal_rs.c.temporal_rs_ZonedDateTime_epoch_milliseconds(zoned_date_time.fields.inner);

        // 5. Return 𝔽(ms).
        return Value.from(@as(f64, @floatFromInt(ms)));
    }

    /// 6.3.18 get Temporal.ZonedDateTime.prototype.epochNanoseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.epochnanoseconds
    fn epochNanoseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return zonedDateTime.[[EpochNanoseconds]].
        const ns = temporal_rs.fromI128Nanoseconds(
            temporal_rs.c.temporal_rs_ZonedDateTime_epoch_nanoseconds(zoned_date_time.fields.inner),
        );
        const managed = try std.math.big.int.Managed.initSet(agent.gc_allocator, ns);
        return Value.from(try BigInt.from(agent.gc_allocator, managed));
    }

    /// 6.3.40 Temporal.ZonedDateTime.prototype.equals ( other )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.equals
    fn equals(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other_value = arguments.get(0);

        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Set other to ? ToTemporalZonedDateTime(other).
        const other = try toTemporalZonedDateTime(agent, other_value, null);

        // 4. If zonedDateTime.[[EpochNanoseconds]] ≠ other.[[EpochNanoseconds]], return false.
        // 5. If TimeZoneEquals(zonedDateTime.[[TimeZone]], other.[[TimeZone]]) is false, return false.
        // 6. Return CalendarEquals(zonedDateTime.[[Calendar]], other.[[Calendar]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_equals(
                zoned_date_time.fields.inner,
                other.as(ZonedDateTime).fields.inner,
            ),
        );
    }

    /// 6.3.5 get Temporal.ZonedDateTime.prototype.era
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.era
    fn era(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[Era]].
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_ZonedDateTime_era(zoned_date_time.fields.inner, &write.inner);
        if (write.inner.len == 0) {
            std.debug.assert(write.inner.cap == 0); // Nothing to free
            return .undefined;
        }
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.6 get Temporal.ZonedDateTime.prototype.eraYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.erayear
    fn eraYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Let result be CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[EraYear]].
        const result = temporal_rs.c.temporal_rs_ZonedDateTime_era_year(
            zoned_date_time.fields.inner,
        );

        // 5. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 6. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 6.3.46 Temporal.ZonedDateTime.prototype.getTimeZoneTransition ( directionParam )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.gettimezonetransition
    fn getTimeZoneTransition(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const direction_param = arguments.get(0);

        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let timeZone be zonedDateTime.[[TimeZone]].

        // 4. If directionParam is undefined, throw a TypeError exception.
        if (direction_param.isUndefined()) {
            return agent.throwException(.type_error, "Direction must not be undefined", .{});
        }

        // 5. If directionParam is a String, then
        const options = if (direction_param.isString()) blk: {
            // a. Let paramString be directionParam.

            // b. Set directionParam to OrdinaryObjectCreate(null).
            const options = try ordinaryObjectCreate(agent, null);

            // c. Perform ! CreateDataPropertyOrThrow(directionParam, "direction", paramString).
            try options.createDataPropertyDirect(
                agent,
                PropertyKey.from("direction"),
                Value.from(direction_param.asString()),
            );

            break :blk options;
        } else blk: {
            // 6. Else,
            // a. Set directionParam to ? GetOptionsObject(directionParam).
            break :blk try direction_param.getOptionsObject(agent);
        };

        // 7. Let direction be ? GetDirectionOption(directionParam).
        const direction = try getTemporalDirectionOption(agent, options);

        // 8. If IsOffsetTimeZoneIdentifier(timeZone) is true, return null.
        // 9. If direction is next, then
        //     a. Let transition be GetNamedTimeZoneNextTransition(timeZone, zonedDateTime.[[EpochNanoseconds]]).
        // 10. Else,
        //     a. Assert: direction is previous.
        //     b. Let transition be GetNamedTimeZonePreviousTransition(timeZone, zonedDateTime.[[EpochNanoseconds]]).
        // 11. If transition is null, return null.
        // 12. Return ! CreateTemporalZonedDateTime(transition, timeZone, zonedDateTime.[[Calendar]]).
        const temporal_rs_zoned_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_get_time_zone_transition(
                zoned_date_time.fields.inner,
                direction,
            ),
        ) catch unreachable;
        if (temporal_rs_zoned_date_time == null) return .null;
        errdefer temporal_rs.c.temporal_rs_ZonedDateTime_destroy(temporal_rs_zoned_date_time.?);
        return Value.from(
            createTemporalZonedDateTime(
                agent,
                temporal_rs_zoned_date_time.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 6.3.11 get Temporal.ZonedDateTime.prototype.hour
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.hour
    fn hour(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(isoDateTime.[[Time]].[[Hour]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_hour(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.23 get Temporal.ZonedDateTime.prototype.hoursInDay
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.hoursinday
    fn hoursInDay(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let timeZone be zonedDateTime.[[TimeZone]].
        // 4. Let isoDateTime be GetISODateTimeFor(timeZone, zonedDateTime.[[EpochNanoseconds]]).
        // 5. Let today be isoDateTime.[[ISODate]].
        // 6. Let tomorrow be BalanceISODate(today.[[Year]], today.[[Month]], today.[[Day]] + 1).
        // 7. Let todayNs be ? GetStartOfDay(timeZone, today).
        // 8. Let tomorrowNs be ? GetStartOfDay(timeZone, tomorrow).
        // 9. Let diff be TimeDurationFromEpochNanosecondsDifference(tomorrowNs, todayNs).
        // 10. Return 𝔽(TotalTimeDuration(diff, hour)).
        const hours_in_day = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_hours_in_day(zoned_date_time.fields.inner),
        ) catch |err| switch (err) {
            error.RangeError => {
                // TODO: Improve error message, not sure what this should say
                return agent.throwException(
                    .range_error,
                    "Can't get hours in day for ZonedDateTime",
                    .{},
                );
            },
            else => unreachable,
        };
        return Value.from(hours_in_day);
    }

    /// 6.3.28 get Temporal.ZonedDateTime.prototype.inLeapYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.inleapyear
    fn inLeapYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[InLeapYear]].
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_in_leap_year(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.15 get Temporal.ZonedDateTime.prototype.microsecond
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.microsecond
    fn microsecond(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(isoDateTime.[[Time]].[[Microsecond]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_microsecond(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.14 get Temporal.ZonedDateTime.prototype.millisecond
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.millisecond
    fn millisecond(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(isoDateTime.[[Time]].[[Millisecond]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_millisecond(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.12 get Temporal.ZonedDateTime.prototype.minute
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.minute
    fn minute(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(isoDateTime.[[Time]].[[Minute]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_minute(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.8 get Temporal.ZonedDateTime.prototype.month
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.month
    fn month(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[Month]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_month(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.9 get Temporal.ZonedDateTime.prototype.monthCode
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.monthcode
    fn monthCode(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[MonthCode]].
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_ZonedDateTime_month_code(zoned_date_time.fields.inner, &write.inner);
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.27 get Temporal.ZonedDateTime.prototype.monthsInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.monthsinyear
    fn monthsInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[MonthsInYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_months_in_year(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.16 get Temporal.ZonedDateTime.prototype.nanosecond
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.nanosecond
    fn nanosecond(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(isoDateTime.[[Time]].[[Nanosecond]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_nanosecond(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.30 get Temporal.ZonedDateTime.prototype.offset
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.offset
    fn offset(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let offsetNanoseconds be GetOffsetNanosecondsFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return FormatUTCOffsetNanoseconds(offsetNanoseconds).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        // NOTE: I don't think this is actually fallible
        // https://github.com/boa-dev/temporal/blob/34522ae99c9d6e2ac2782162eaf01b36494951ca/src/builtins/core/timezone.rs#L183-L198
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_offset(zoned_date_time.fields.inner, &write.inner),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.29 get Temporal.ZonedDateTime.prototype.offsetNanoseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.offsetnanoseconds
    fn offsetNanoseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return 𝔽(GetOffsetNanosecondsFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]])).
        // NOTE: I don't think this is actually fallible
        // https://github.com/boa-dev/temporal/blob/34522ae99c9d6e2ac2782162eaf01b36494951ca/src/builtins/core/timezone.rs#L183-L198
        const offset_nanoseconds = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_offset_nanoseconds(zoned_date_time.fields.inner),
        ) catch unreachable;
        return Value.from(@as(f64, @floatFromInt(offset_nanoseconds)));
    }

    /// 6.3.13 get Temporal.ZonedDateTime.prototype.second
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.second
    fn second(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(isoDateTime.[[Time]].[[Second]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_second(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.45 Temporal.ZonedDateTime.prototype.startOfDay ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.startofday
    fn startOfDay(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let timeZone be zonedDateTime.[[TimeZone]].
        // 4. Let calendar be zonedDateTime.[[Calendar]].
        // 5. Let isoDateTime be GetISODateTimeFor(timeZone, zonedDateTime.[[EpochNanoseconds]]).
        // 6. Let epochNanoseconds be ? GetStartOfDay(timeZone, isoDateTime.[[ISODate]]).
        // 7. Return ! CreateTemporalZonedDateTime(epochNanoseconds, timeZone, calendar).
        const temporal_rs_zoned_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_start_of_day(zoned_date_time.fields.inner),
        ) catch |err| switch (err) {
            error.RangeError => {
                // TODO: Improve error message, not sure what this should say
                return agent.throwException(
                    .range_error,
                    "Can't get start of day for ZonedDateTime",
                    .{},
                );
            },
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_ZonedDateTime_destroy(temporal_rs_zoned_date_time.?);
        return Value.from(
            createTemporalZonedDateTime(
                agent,
                temporal_rs_zoned_date_time.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 6.3.4 get Temporal.ZonedDateTime.prototype.timeZoneId
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.timezoneid
    fn timeZoneId(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return zonedDateTime.[[TimeZone]].
        const temporal_rs_time_zone = temporal_rs.c.temporal_rs_ZonedDateTime_timezone(
            zoned_date_time.fields.inner,
        );
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_TimeZone_identifier(temporal_rs_time_zone.?, &write.inner);
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.47 Temporal.ZonedDateTime.prototype.toInstant ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toinstant
    fn toInstant(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return ! CreateTemporalInstant(zonedDateTime.[[EpochNanoseconds]]).
        const temporal_rs_instant = temporal_rs.c.temporal_rs_ZonedDateTime_to_instant(
            zoned_date_time.fields.inner,
        );
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        return Value.from(
            createTemporalInstant(
                agent,
                temporal_rs_instant.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 6.3.43 Temporal.ZonedDateTime.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return TemporalZonedDateTimeToString(zonedDateTime, auto, auto, auto, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_to_ixdtf_string(
                zoned_date_time.fields.inner,
                temporal_rs.c.DisplayOffset_Auto,
                temporal_rs.c.DisplayTimeZone_Auto,
                temporal_rs.c.DisplayCalendar_Auto,
                temporal_rs.to_string_rounding_options_auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.42 Temporal.ZonedDateTime.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Return TemporalZonedDateTimeToString(zonedDateTime, auto, auto, auto, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_to_ixdtf_string(
                zoned_date_time.fields.inner,
                temporal_rs.c.DisplayOffset_Auto,
                temporal_rs.c.DisplayTimeZone_Auto,
                temporal_rs.c.DisplayCalendar_Auto,
                temporal_rs.to_string_rounding_options_auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.48 Temporal.ZonedDateTime.prototype.toPlainDate ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toplaindate
    fn toPlainDate(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]],
        //    zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return ! CreateTemporalDate(isoDateTime.[[ISODate]], zonedDateTime.[[Calendar]]).
        const temporal_rs_plain_date = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_to_plain_date(
                zoned_date_time.fields.inner,
            ),
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

    /// 6.3.50 Temporal.ZonedDateTime.prototype.toPlainDateTime ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toplaindatetime
    fn toPlainDateTime(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]],
        //    zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return ! CreateTemporalDateTime(isoDateTime, zonedDateTime.[[Calendar]]).
        const temporal_rs_plain_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_to_plain_datetime(
                zoned_date_time.fields.inner,
            ),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_PlainDateTime_destroy(temporal_rs_plain_date_time.?);
        return Value.from(
            createTemporalDateTime(
                agent,
                temporal_rs_plain_date_time.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 6.3.49 Temporal.ZonedDateTime.prototype.toPlainTime ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toplaintime
    fn toPlainTime(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]],
        //    zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return ! CreateTemporalTime(isoDateTime.[[Time]]).
        const temporal_rs_plain_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_to_plain_time(
                zoned_date_time.fields.inner,
            ),
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

    /// 6.3.41 Temporal.ZonedDateTime.prototype.toString ( [ options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const options_value = arguments.get(0);

        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 4. NOTE: The following steps read options and perform independent validation in
        //    alphabetical order (GetTemporalShowCalendarNameOption reads "calendarName",
        //    GetTemporalFractionalSecondDigitsOption reads "fractionalSecondDigits",
        //    GetTemporalShowOffsetOption reads "offset", and GetRoundingModeOption reads
        //    "roundingMode").

        // 5. Let showCalendar be ? GetTemporalShowCalendarNameOption(resolvedOptions).
        const show_calendar = try getTemporalShowCalendarNameOption(agent, options);

        // 6. Let digits be ? GetTemporalFractionalSecondDigitsOption(resolvedOptions).
        const precision = try getTemporalFractionalSecondDigitsOption(agent, options);

        // 7. Let showOffset be ? GetTemporalShowOffsetOption(resolvedOptions).
        const show_offset = try getTemporalShowOffsetOption(agent, options);

        // 8. Let roundingMode be ? GetRoundingModeOption(resolvedOptions, trunc).
        const rounding_mode = try getTemporalRoundingModeOption(
            agent,
            options,
            temporal_rs.c.RoundingMode_Trunc,
        );

        // 9. Let smallestUnit be ? GetTemporalUnitValuedOption(resolvedOptions, "smallestUnit",
        //    unset).
        const smallest_unit = try getTemporalUnitValuedOption(
            agent,
            options,
            "smallestUnit",
            .unset,
        );

        // 10. Perform ? ValidateTemporalUnitValue(smallestUnit, time).
        try validateTemporalUnitValue(agent, smallest_unit, "smallestUnit", .time, &.{});

        // 11. If smallestUnit is hour, throw a RangeError exception.
        if (smallest_unit == temporal_rs.c.Unit_Hour) {
            return agent.throwException(
                .range_error,
                "Invalid value for option 'smallestUnit'",
                .{},
            );
        }

        // 12. Let showTimeZone be ? GetTemporalShowTimeZoneNameOption(resolvedOptions).
        const show_time_zone = try getTemporalShowTimeZoneNameOption(agent, options);

        // 13. Let precision be ToSecondsStringPrecisionRecord(smallestUnit, digits).
        // 14. Return TemporalZonedDateTimeToString(zonedDateTime, precision.[[Precision]],
        //     showCalendar, showTimeZone, showOffset, precision.[[Increment]], precision.[[Unit]],
        //     roundingMode).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_to_ixdtf_string(
                zoned_date_time.fields.inner,
                show_offset,
                show_time_zone,
                show_calendar,
                .{
                    .precision = precision,
                    .smallest_unit = if (smallest_unit) |ok|
                        .{ .is_ok = true, .unnamed_0 = .{ .ok = ok } }
                    else
                        .{ .is_ok = false },
                    .rounding_mode = .{ .is_ok = true, .unnamed_0 = .{ .ok = rounding_mode } },
                },
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 6.3.44 Temporal.ZonedDateTime.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.ZonedDateTime to primitive value",
            .{},
        );
    }

    /// 6.3.21 get Temporal.ZonedDateTime.prototype.weekOfYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.weekoftheyear
    fn weekOfYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Let result be CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[WeekOfYear]].[[Week]].
        const result = temporal_rs.c.temporal_rs_ZonedDateTime_week_of_year(
            zoned_date_time.fields.inner,
        );

        // 5. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 6. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 6.3.7 get Temporal.ZonedDateTime.prototype.year
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.year
    fn year(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Return 𝔽(CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[Year]]).
        return Value.from(
            temporal_rs.c.temporal_rs_ZonedDateTime_year(zoned_date_time.fields.inner),
        );
    }

    /// 6.3.22 get Temporal.ZonedDateTime.prototype.yearOfWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.yearofweek
    fn yearOfWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let zonedDateTime be the this value.
        // 2. Perform ? RequireInternalSlot(zonedDateTime, [[InitializedTemporalZonedDateTime]]).
        const zoned_date_time = try this_value.requireInternalSlot(agent, ZonedDateTime);

        // 3. Let isoDateTime be GetISODateTimeFor(zonedDateTime.[[TimeZone]], zonedDateTime.[[EpochNanoseconds]]).
        // 4. Let result be CalendarISOToDate(zonedDateTime.[[Calendar]], isoDateTime.[[ISODate]]).[[WeekOfYear]].[[Year]].
        const result = temporal_rs.c.temporal_rs_ZonedDateTime_year_of_week(
            zoned_date_time.fields.inner,
        );

        // 5. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 6. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }
};

/// 6.4 Properties of Temporal.ZonedDateTime Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-zoneddatetime-instances
pub const ZonedDateTime = MakeObject(.{
    .Fields = struct {
        inner: *temporal_rs.c.ZonedDateTime,
    },
    .finalizer = struct {
        fn finalizer(object: *Object) void {
            temporal_rs.c.temporal_rs_ZonedDateTime_destroy(object.as(ZonedDateTime).fields.inner);
        }
    }.finalizer,
    .tag = .temporal_zoned_date_time,
});

/// 6.5.2 ToTemporalZonedDateTime ( item [ , options ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporalzoneddatetime
pub fn toTemporalZonedDateTime(
    agent: *Agent,
    item: Value,
    maybe_options_value: ?Value,
) Agent.Error!*Object {
    // 1. If options is not present, set options to undefined.
    const options_value: Value = maybe_options_value orelse .undefined;

    // 2. Let offsetBehaviour be option.
    // 2. Let hasUTCDesignator be false.
    // 4. If item is an Object, then
    const temporal_rs_zoned_date_time = if (item.isObject()) blk: {
        // a. If item has an [[InitializedTemporalZonedDateTime]] internal slot, then
        if (item.asObject().is(ZonedDateTime)) {
            // i. NOTE: The following steps, and similar ones below, read options and perform
            //    independent validation in alphabetical order (GetTemporalDisambiguationOption
            //    reads "disambiguation", GetTemporalOffsetOption reads "offset", and
            //    GetTemporalOverflowOption reads "overflow").

            // ii. Let resolvedOptions be ? GetOptionsObject(options).
            const options = try options_value.getOptionsObject(agent);

            // iii. Perform ? GetTemporalDisambiguationOption(resolvedOptions).
            _ = try getTemporalDisambiguationOption(agent, options);

            // iv. Perform ? GetTemporalOffsetOption(resolvedOptions, reject).
            _ = try getTemporalOffsetOption(
                agent,
                options,
                temporal_rs.c.OffsetDisambiguation_Reject,
            );

            // v. Perform ? GetTemporalOverflowOption(resolvedOptions).
            _ = try getTemporalOverflowOption(agent, options);

            // vi. Return ! CreateTemporalZonedDateTime(item.[[EpochNanoseconds]],
            //     item.[[TimeZone]], item.[[Calendar]]).
            const zoned_date_time = item.asObject().as(ZonedDateTime);
            break :blk temporal_rs.c.temporal_rs_ZonedDateTime_clone(
                zoned_date_time.fields.inner,
            );
        }

        // b. Let calendar be ? GetTemporalCalendarIdentifierWithISODefault(item).
        // c. Let fields be ? PrepareCalendarFields(calendar, item, « year, month, month-code, day »,
        //    « hour, minute, second, millisecond, microsecond, nanosecond, offset, time-zone »,
        //    « time-zone »).
        // d. Let timeZone be fields.[[TimeZone]].
        // e. Let offsetString be fields.[[OffsetString]].
        const partial_zoned_date_time = try toTemporalPartialZonedDateTime(agent, item.asObject(), true);

        // f. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // g. Let disambiguation be ? GetTemporalDisambiguationOption(resolvedOptions).
        const disambiguation = try getTemporalDisambiguationOption(agent, options);

        // h. Let offsetOption be ? GetTemporalOffsetOption(resolvedOptions, reject).
        const offset_option = try getTemporalOffsetOption(
            agent,
            options,
            temporal_rs.c.Disambiguation_Reject,
        );

        // i. Let overflow be ? GetTemporalOverflowOption(resolvedOptions).
        const overflow = try getTemporalOverflowOption(agent, options);

        // j. Let result be ? InterpretTemporalDateTimeFields(calendar, fields, overflow).
        // k. Let isoDate be result.[[ISODate]].
        // l. Let time be result.[[Time]].
        break :blk temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_from_partial(
                partial_zoned_date_time,
                .{ .is_ok = true, .unnamed_0 = .{ .ok = overflow } },
                .{ .is_ok = true, .unnamed_0 = .{ .ok = disambiguation } },
                .{ .is_ok = true, .unnamed_0 = .{ .ok = offset_option } },
            ),
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(.range_error, "Invalid zoned datetime", .{});
            },
            error.TypeError => {
                return agent.throwException(.type_error, "Missing zoned datetime field", .{});
            },
            else => unreachable,
        };
    } else blk: {
        // 5. Else,

        // a. If item is not a String, throw a TypeError exception.
        if (!item.isString()) {
            return agent.throwException(
                .type_error,
                "Zoned datetime must be a string or object",
                .{},
            );
        }

        // b. Let result be ? ParseISODateTime(item, « TemporalDateTimeString[+Zoned] »).
        // c. Let annotation be result.[[TimeZone]].[[TimeZoneAnnotation]].
        // d. Assert: annotation is not empty.
        // e. Let timeZone be ? ToTemporalTimeZoneIdentifier(annotation).
        // f. Let offsetString be result.[[TimeZone]].[[OffsetString]].
        // g. If result.[[TimeZone]].[[Z]] is true, then
        //     i. Set hasUTCDesignator to true.
        // h. Let calendar be result.[[Calendar]].
        // i. If calendar is empty, set calendar to "iso8601".
        // j. Set calendar to ? CanonicalizeCalendar(calendar).
        // k. Set matchBehaviour to match-minutes.
        // l. If offsetString is not empty, then
        //     i. Let offsetParseResult be ParseText(StringToCodePoints(offsetString),
        //        UTCOffset[+SubMinutePrecision]).
        //     ii. Assert: offsetParseResult is a Parse Node.
        //     iii. If offsetParseResult contains more than one MinuteSecond Parse Node, set
        //          matchBehaviour to match-exactly.

        // m. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // n. Let disambiguation be ? GetTemporalDisambiguationOption(resolvedOptions).
        const disambiguation = try getTemporalDisambiguationOption(agent, options);

        // o. Let offsetOption be ? GetTemporalOffsetOption(resolvedOptions, reject).
        const offset_option = try getTemporalOffsetOption(
            agent,
            options,
            temporal_rs.c.Disambiguation_Reject,
        );

        // p. Perform ? GetTemporalOverflowOption(resolvedOptions).
        _ = try getTemporalOverflowOption(agent, options);

        // q. Let isoDate be CreateISODateRecord(result.[[Year]], result.[[Month]], result.[[Day]]).
        // r. Let time be result.[[Time]].
        const zoned_date_time_utf8 = try item.asString().toUtf8(agent.gc_allocator);
        defer agent.gc_allocator.free(zoned_date_time_utf8);
        break :blk temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_from_utf8(
                temporal_rs.toDiplomatStringView(zoned_date_time_utf8),
                disambiguation,
                offset_option,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(
                .range_error,
                "Invalid zoned datetime string",
                .{},
            ),
            else => unreachable,
        };
    };
    errdefer temporal_rs.c.temporal_rs_ZonedDateTime_destroy(temporal_rs_zoned_date_time.?);

    // 6. If hasUTCDesignator is true, then
    //     a. Let offsetBehaviour be exact.
    // 7. Else if offsetString is empty or offsetString is unset, then
    //     a. Let offsetBehaviour be wall.
    // 8. Else,
    //     a. Let offsetBehaviour be option.
    // 9. Let offsetNanoseconds be 0.
    // 10. If offsetBehaviour is option, then
    //     a. Set offsetNanoseconds to ! ParseDateTimeUTCOffset(offsetString).
    // 11. Let epochNanoseconds be ? InterpretISODateTimeOffset(isoDate, time, offsetBehaviour,
    //    offsetNanoseconds, timeZone, disambiguation, offsetOption, matchBehaviour).
    // 12. Return ! CreateTemporalZonedDateTime(epochNanoseconds, timeZone, calendar).
    return createTemporalZonedDateTime(
        agent,
        temporal_rs_zoned_date_time.?,
        null,
    ) catch |err| try noexcept(err);
}

/// Custom function to create a PartialZonedDateTime based on:
///
/// 12.2.3 PrepareCalendarFields ( calendar, fields, calendarFieldNames, nonCalendarFieldNames, requiredFieldNames )
/// https://tc39.es/proposal-temporal/#sec-temporal-preparecalendarfields
pub fn toTemporalPartialZonedDateTime(
    agent: *Agent,
    object: *Object,
    time_zone_required: bool,
) Agent.Error!temporal_rs.c.PartialZonedDateTime {
    var result: temporal_rs.c.PartialZonedDateTime = .{
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
        .offset = .{ .is_ok = false },
        .timezone = null,
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

    const offset = try object.get(agent, PropertyKey.from("offset"));
    if (!offset.isUndefined()) {
        const offset_utf8 = try toOffsetString(agent, offset);
        result.offset = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = temporal_rs.toDiplomatStringView(offset_utf8) },
        };
    }

    const second = try object.get(agent, PropertyKey.from("second"));
    if (!second.isUndefined()) {
        result.time.second = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(u8, try second.toIntegerWithTruncation(agent)) },
        };
    }

    var maybe_temporal_rs_time_zone: ?*temporal_rs.c.TimeZone = null;
    errdefer if (maybe_temporal_rs_time_zone) |temporal_rs_time_zone| {
        temporal_rs.c.temporal_rs_TimeZone_destroy(temporal_rs_time_zone);
    };

    const time_zone = try object.get(agent, PropertyKey.from("timeZone"));
    if (!time_zone.isUndefined()) {
        maybe_temporal_rs_time_zone = try toTemporalTimeZoneIdentifier(agent, time_zone);
        result.timezone = maybe_temporal_rs_time_zone.?;
    } else if (time_zone_required) {
        return agent.throwException(.type_error, "Missing required 'timeZone' field", .{});
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

/// 6.5.3 CreateTemporalZonedDateTime ( epochNanoseconds, timeZone, calendar [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalzoneddatetime
pub fn createTemporalZonedDateTime(
    agent: *Agent,
    inner: *temporal_rs.c.ZonedDateTime,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Assert: IsValidEpochNanoseconds(epochNanoseconds) is true.
    // 2. If newTarget is not present, set newTarget to %Temporal.ZonedDateTime%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.ZonedDateTime%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.ZonedDateTime.prototype%",
    //    « [[InitializedTemporalZonedDateTime]], [[EpochNanoseconds]], [[TimeZone]], [[Calendar]] »).
    // 4. Set object.[[EpochNanoseconds]] to epochNanoseconds.
    // 5. Set object.[[TimeZone]] to timeZone.
    // 6. Set object.[[Calendar]] to calendar.
    // 7. Return object.
    return ordinaryCreateFromConstructor(
        ZonedDateTime,
        agent,
        new_target,
        "%Temporal.ZonedDateTime.prototype%",
        .{ .inner = inner },
    );
}
