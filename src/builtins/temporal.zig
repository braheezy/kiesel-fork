//! 1 The Temporal Object
//! https://tc39.es/proposal-temporal/#sec-temporal-objects

const std = @import("std");

const temporal_rs = @import("../c/temporal_rs.zig");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;

comptime {
    const build_options = @import("build-options");
    if (!build_options.enable_temporal) @compileError("Temporal is not enabled");
}

pub const duration = @import("./temporal/duration.zig");
pub const instant = @import("./temporal/instant.zig");
pub const now = @import("./temporal/now.zig");
pub const plain_date = @import("./temporal/plain_date.zig");
pub const plain_date_time = @import("./temporal/plain_date_time.zig");
pub const plain_month_day = @import("./temporal/plain_month_day.zig");
pub const plain_time = @import("./temporal/plain_time.zig");
pub const plain_year_month = @import("./temporal/plain_year_month.zig");
pub const zoned_date_time = @import("./temporal/zoned_date_time.zig");

pub const Duration = duration.Duration;
pub const Instant = instant.Instant;
pub const PlainDate = plain_date.PlainDate;
pub const PlainDateTime = plain_date_time.PlainDateTime;
pub const PlainMonthDay = plain_month_day.PlainMonthDay;
pub const PlainTime = plain_time.PlainTime;
pub const PlainYearMonth = plain_year_month.PlainYearMonth;
pub const ZonedDateTime = zoned_date_time.ZonedDateTime;

pub const namespace = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 1.1.1 Temporal [ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 1.2.7 Temporal.Duration ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-duration
        try object.defineBuiltinProperty(
            agent,
            "Duration",
            Value.from(try realm.intrinsics.@"%Temporal.Duration%"()),
        );

        // 1.2.1 Temporal.Instant ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-instant
        try object.defineBuiltinProperty(
            agent,
            "Instant",
            Value.from(try realm.intrinsics.@"%Temporal.Instant%"()),
        );

        // 1.2.3 Temporal.PlainDate ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plaindate
        try object.defineBuiltinProperty(
            agent,
            "PlainDate",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDate%"()),
        );

        // 1.2.2 Temporal.PlainDateTime ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime
        try object.defineBuiltinProperty(
            agent,
            "PlainDateTime",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDateTime%"()),
        );

        // 1.2.6 Temporal.PlainMonthDay ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday
        try object.defineBuiltinProperty(
            agent,
            "PlainMonthDay",
            Value.from(try realm.intrinsics.@"%Temporal.PlainMonthDay%"()),
        );

        // 1.2.4 Temporal.PlainTime ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plaintime
        try object.defineBuiltinProperty(
            agent,
            "PlainTime",
            Value.from(try realm.intrinsics.@"%Temporal.PlainTime%"()),
        );

        // 1.2.5 Temporal.PlainYearMonth ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth
        try object.defineBuiltinProperty(
            agent,
            "PlainYearMonth",
            Value.from(try realm.intrinsics.@"%Temporal.PlainYearMonth%"()),
        );

        // 1.2.8 Temporal.ZonedDateTime ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime
        try object.defineBuiltinProperty(
            agent,
            "ZonedDateTime",
            Value.from(try realm.intrinsics.@"%Temporal.ZonedDateTime%"()),
        );

        // 1.3.1 Temporal.Now
        // https://tc39.es/proposal-temporal/#sec-temporal-now
        try object.defineBuiltinProperty(
            agent,
            "Now",
            Value.from(try realm.intrinsics.@"%Temporal.Now%"()),
        );
    }
};

/// 11.1.8 ToTemporalTimeZoneIdentifier ( temporalTimeZoneLike )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimezoneidentifier
pub fn toTemporalTimeZoneIdentifier(
    agent: *Agent,
    temporal_time_zone_like: Value,
) Agent.Error!*temporal_rs.c.TimeZone {
    // 1. If temporalTimeZoneLike is an Object, then
    if (temporal_time_zone_like.isObject()) {
        // a. If temporalTimeZoneLike has an [[InitializedTemporalZonedDateTime]] internal slot, then
        if (temporal_time_zone_like.asObject().is(ZonedDateTime)) {
            // i. Return temporalTimeZoneLike.[[TimeZone]].
            const temporal_rs_time_zone = temporal_rs.c.temporal_rs_TimeZone_clone(
                temporal_rs.c.temporal_rs_ZonedDateTime_timezone(
                    temporal_time_zone_like.asObject().as(ZonedDateTime).fields.inner,
                ),
            );
            return temporal_rs_time_zone.?;
        }
    }

    // 2. If temporalTimeZoneLike is not a String, throw a TypeError exception.
    if (!temporal_time_zone_like.isString()) {
        return agent.throwException(
            .type_error,
            "Time zone must be a string or Temporal.ZonedDateTime object",
            .{},
        );
    }

    // 3. Let parseResult be ? ParseTemporalTimeZoneString(temporalTimeZoneLike).
    // 4. Let offsetMinutes be parseResult.[[OffsetMinutes]].
    // 5. If offsetMinutes is not empty, return FormatOffsetTimeZoneIdentifier(offsetMinutes).
    // 6. Let name be parseResult.[[Name]].
    // 7. Let timeZoneIdentifierRecord be GetAvailableNamedTimeZoneIdentifier(name).
    // 8. If timeZoneIdentifierRecord is empty, throw a RangeError exception.
    // 9. Return timeZoneIdentifierRecord.[[Identifier]].
    const time_zone = try temporal_time_zone_like.asString().toUtf8(agent.gc_allocator);
    defer agent.gc_allocator.free(time_zone);
    const temporal_rs_time_zone = temporal_rs.temporalErrorResult(
        temporal_rs.c.temporal_rs_TimeZone_try_from_str(
            temporal_rs.toDiplomatStringView(time_zone),
        ),
    ) catch |err| switch (err) {
        error.RangeError => return agent.throwException(.range_error, "Invalid time zone", .{}),
        else => unreachable,
    };
    if (!temporal_rs.c.temporal_rs_TimeZone_is_valid(temporal_rs_time_zone.?)) {
        return agent.throwException(.range_error, "Invalid time zone", .{});
    }
    return temporal_rs_time_zone.?;
}

/// 12.1.1 CanonicalizeCalendar ( id )
/// https://tc39.es/proposal-temporal/#sec-temporal-canonicalizecalendar
pub fn canonicalizeCalendar(
    agent: *Agent,
    id: *const String,
) Agent.Error!temporal_rs.c.AnyCalendarKind {
    const temporal_rs_calendar = blk: {
        const string = switch (id.slice) {
            .ascii => |ascii| ascii,
            .utf16 => break :blk error.RangeError,
        };
        break :blk temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Calendar_from_utf8(.{
                .data = string.ptr,
                .len = string.len,
            }),
        );
    } catch |err| switch (err) {
        error.RangeError => {
            return agent.throwException(.range_error, "Invalid calendar {}", .{id});
        },
        else => unreachable,
    };
    defer temporal_rs.c.temporal_rs_Calendar_destroy(temporal_rs_calendar);
    return temporal_rs.c.temporal_rs_Calendar_kind(temporal_rs_calendar);
}

/// 13.10 GetTemporalShowCalendarNameOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalshowcalendarnameoption
pub fn getTemporalShowCalendarNameOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.DisplayCalendar {
    // 1. Let stringValue be ? GetOption(options, "calendarName", string, « "auto", "always",
    //    "never", "critical" », "auto").
    const string_value = try options.getOption(
        agent,
        "calendarName",
        .string,
        &.{
            String.fromLiteral("auto"),
            String.fromLiteral("always"),
            String.fromLiteral("never"),
            String.fromLiteral("critical"),
        },
        String.fromLiteral("auto"),
    );

    // 2. If stringValue is "always", return always.
    if (string_value.eql(String.fromLiteral("always"))) {
        return temporal_rs.c.DisplayCalendar_Always;
    }

    // 3. If stringValue is "never", return never.
    if (string_value.eql(String.fromLiteral("never"))) {
        return temporal_rs.c.DisplayCalendar_Never;
    }

    // 4. If stringValue is "critical", return critical.
    if (string_value.eql(String.fromLiteral("critical"))) {
        return temporal_rs.c.DisplayCalendar_Critical;
    }

    // 5. Return auto.
    return temporal_rs.c.DisplayCalendar_Auto;
}

/// 13.15 GetTemporalFractionalSecondDigitsOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalfractionalseconddigitsoption
pub fn getTemporalFractionalSecondDigitsOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.Precision {
    // 1. Let digitsValue be ? Get(options, "fractionalSecondDigits").
    const digits_value = try options.get(agent, PropertyKey.from("fractionalSecondDigits"));

    // 2. If digitsValue is undefined, return auto.
    if (digits_value.isUndefined()) {
        return .{
            .is_minute = false,
            .precision = .{ .is_ok = false },
        };
    }

    // 3. If digitsValue is not a Number, then
    if (!digits_value.isNumber()) {
        // a. If ? ToString(digitsValue) is not "auto", throw a RangeError exception.
        if (!(try digits_value.toString(agent)).eql(String.fromLiteral("auto"))) {
            return agent.throwException(
                .range_error,
                "Invalid fractionalSecondDigits option {}",
                .{digits_value},
            );
        }

        // b. Return auto.
        return .{
            .is_minute = false,
            .precision = .{ .is_ok = false },
        };
    }

    // 4. If digitsValue is NaN, +∞𝔽, or -∞𝔽, throw a RangeError exception.
    if (!digits_value.asNumber().isFinite()) {
        return agent.throwException(
            .range_error,
            "Invalid fractionalSecondDigits option {}",
            .{digits_value},
        );
    }

    // 5. Let digitCount be floor(ℝ(digitsValue)).
    const digit_count = digits_value.asNumber().floor().asFloat();

    // 6. If digitCount < 0 or digitCount > 9, throw a RangeError exception.
    if (digit_count < 0 or digit_count > 9) {
        return agent.throwException(
            .range_error,
            "Invalid fractionalSecondDigits option {}",
            .{digits_value},
        );
    }

    // 7. Return digitCount.
    return .{
        .is_minute = false,
        .precision = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = @intFromFloat(digit_count) },
        },
    };
}

const TemporalUnit = enum {
    required,
    unset,
    auto,

    year,
    month,
    week,
    day,
    hour,
    minute,
    second,
    millisecond,
    microsecond,
    nanosecond,
};

/// 13.17 GetTemporalUnitValuedOption ( options, key, unitGroup, default [ , extraValues ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalunitvaluedoption
pub fn getTemporalUnitValuedOption(
    agent: *Agent,
    options: *Object,
    comptime key: []const u8,
    comptime unit_group: enum { date, time, datetime },
    comptime default: TemporalUnit,
    comptime extra_values: []const TemporalUnit,
) Agent.Error!?temporal_rs.c.Unit {
    const allowed_strings, const default_value = comptime result: {
        // 1. Let allowedValues be a new empty List.
        // 2. For each row of Table 21, except the header row, in table order, do
        //     a. Let unit be the value in the "Value" column of the row.
        //     b. If the "Category" column of the row is date and unitGroup is date or datetime, append unit to allowedValues.
        //     c. Else if the "Category" column of the row is time and unitGroup is time or datetime, append unit to allowedValues.
        var allowed_values: []const TemporalUnit = switch (unit_group) {
            .date => &.{
                .year,
                .month,
                .week,
                .day,
            },
            .time => &.{
                .hour,
                .minute,
                .second,
                .millisecond,
                .microsecond,
                .nanosecond,
            },
            .datetime => &.{
                .year,
                .month,
                .week,
                .day,
                .hour,
                .minute,
                .second,
                .millisecond,
                .microsecond,
                .nanosecond,
            },
        };

        // 3. If extraValues is present, then
        //     a. Set allowedValues to the list-concatenation of allowedValues and extraValues.
        allowed_values = allowed_values ++ extra_values;

        const default_value = switch (default) {
            // 4. If default is unset, then
            .unset => blk: {
                // a. Let defaultValue be undefined.
                break :blk null;
            },
            // 5. Else if default is required, then
            .required => blk: {
                // a. Let defaultValue be required.
                break :blk .required;
            },
            // 6. Else if default is auto, then
            .auto => blk: {
                // a. Append default to allowedValues.
                allowed_values = allowed_values ++ .{.auto};

                // b. Let defaultValue be "auto".
                break :blk String.fromLiteral("auto");
            },
            // 7. Else,
            else => |value| blk: {
                // a. Assert: allowedValues contains default.
                std.debug.assert(std.mem.indexOf(TemporalUnit, allowed_values, default) != null);

                // b. Let defaultValue be the value in the "Singular property name" column of Table 21
                //    corresponding to the row with default in the "Value" column.
                break :blk String.fromLiteral(@tagName(value));
            },
        };

        // 8. Let allowedStrings be a new empty List.
        var allowed_strings: []const *const String = &.{};

        // 9. For each element value of allowedValues, do
        for (allowed_values) |value| {
            // a. If value is auto, then
            if (value == .auto) {
                // i. Append "auto" to allowedStrings.
                allowed_strings = allowed_strings ++ .{String.fromLiteral("auto")};
            } else {
                // b. Else,
                // i. Let singularName be the value in the "Singular property name" column of Table 21
                //    corresponding to the row with value in the "Value" column.
                const singular_name: *const String = switch (value) {
                    .year => String.fromLiteral("year"),
                    .month => String.fromLiteral("month"),
                    .week => String.fromLiteral("week"),
                    .day => String.fromLiteral("day"),
                    .hour => String.fromLiteral("hour"),
                    .minute => String.fromLiteral("minute"),
                    .second => String.fromLiteral("second"),
                    .millisecond => String.fromLiteral("millisecond"),
                    .microsecond => String.fromLiteral("microsecond"),
                    .nanosecond => String.fromLiteral("nanosecond"),
                    else => unreachable,
                };

                // ii. Append singularName to allowedStrings.
                allowed_strings = allowed_strings ++ .{singular_name};

                // iii. Let pluralName be the value in the "Plural property name" column of the
                //      corresponding row.
                const plural_name: *const String = switch (value) {
                    .year => String.fromLiteral("years"),
                    .month => String.fromLiteral("months"),
                    .week => String.fromLiteral("weeks"),
                    .day => String.fromLiteral("days"),
                    .hour => String.fromLiteral("hours"),
                    .minute => String.fromLiteral("minutes"),
                    .second => String.fromLiteral("seconds"),
                    .millisecond => String.fromLiteral("milliseconds"),
                    .microsecond => String.fromLiteral("microseconds"),
                    .nanosecond => String.fromLiteral("nanoseconds"),
                    else => unreachable,
                };

                // iv. Append pluralName to allowedStrings.
                allowed_strings = allowed_strings ++ .{plural_name};
            }
        }

        break :result .{ allowed_strings, default_value };
    };

    // 10. NOTE: For each singular Temporal unit name that is contained within allowedStrings, the
    //     corresponding plural name is also contained within it.

    // 11. Let value be ? GetOption(options, key, string, allowedStrings, defaultValue).
    const value = try options.getOption(
        agent,
        key,
        .string,
        allowed_strings,
        default_value,
    ) orelse {
        // 12. If value is undefined, return unset.
        return null;
    };

    // 13. If value is "auto", return auto.
    // 14. Return the value in the "Value" column of Table 21 corresponding to the row with value
    //     in its "Singular property name" or "Plural property name" column.
    const unit_map = std.StaticStringMap(temporal_rs.c.Unit).initComptime(&.{
        .{ "auto", temporal_rs.c.Unit_Auto },
        .{ "year", temporal_rs.c.Unit_Year },
        .{ "years", temporal_rs.c.Unit_Year },
        .{ "month", temporal_rs.c.Unit_Month },
        .{ "months", temporal_rs.c.Unit_Month },
        .{ "week", temporal_rs.c.Unit_Week },
        .{ "weeks", temporal_rs.c.Unit_Week },
        .{ "day", temporal_rs.c.Unit_Day },
        .{ "days", temporal_rs.c.Unit_Day },
        .{ "hour", temporal_rs.c.Unit_Hour },
        .{ "hours", temporal_rs.c.Unit_Hour },
        .{ "minute", temporal_rs.c.Unit_Minute },
        .{ "minutes", temporal_rs.c.Unit_Minute },
        .{ "second", temporal_rs.c.Unit_Second },
        .{ "seconds", temporal_rs.c.Unit_Second },
        .{ "millisecond", temporal_rs.c.Unit_Millisecond },
        .{ "milliseconds", temporal_rs.c.Unit_Millisecond },
        .{ "microsecond", temporal_rs.c.Unit_Microsecond },
        .{ "microseconds", temporal_rs.c.Unit_Microsecond },
        .{ "nanosecond", temporal_rs.c.Unit_Nanosecond },
        .{ "nanoseconds", temporal_rs.c.Unit_Nanosecond },
    });
    return unit_map.get(value.slice.ascii).?;
}

/// 14.5.2.3 GetRoundingModeOption ( options, fallback )
/// https://tc39.es/proposal-temporal/#sec-temporal-getroundingmodeoption
pub fn getTemporalRoundingModeOption(
    agent: *Agent,
    options: *Object,
    fallback: temporal_rs.c.RoundingMode,
) Agent.Error!temporal_rs.c.RoundingMode {
    // 1. Let allowedStrings be the List of Strings from the "String Identifier" column of Table 27.
    // 2. Let stringFallback be the value from the "String Identifier" column of the row with
    //    fallback in its "Rounding Mode" column.
    // 3. Let stringValue be ? GetOption(options, "roundingMode", string, allowedStrings, stringFallback).
    const rounding_mode = try options.getOption(
        agent,
        "roundingMode",
        .string,
        &.{
            String.fromLiteral("ceil"),
            String.fromLiteral("floor"),
            String.fromLiteral("expand"),
            String.fromLiteral("trunc"),
            String.fromLiteral("halfCeil"),
            String.fromLiteral("halfFloor"),
            String.fromLiteral("halfExpand"),
            String.fromLiteral("halfTrunc"),
            String.fromLiteral("halfEven"),
        },
        switch (fallback) {
            temporal_rs.c.RoundingMode_Ceil => String.fromLiteral("ceil"),
            temporal_rs.c.RoundingMode_Floor => String.fromLiteral("floor"),
            temporal_rs.c.RoundingMode_Expand => String.fromLiteral("expand"),
            temporal_rs.c.RoundingMode_Trunc => String.fromLiteral("trunc"),
            temporal_rs.c.RoundingMode_HalfCeil => String.fromLiteral("halfCeil"),
            temporal_rs.c.RoundingMode_HalfFloor => String.fromLiteral("halfFloor"),
            temporal_rs.c.RoundingMode_HalfExpand => String.fromLiteral("halfExpand"),
            temporal_rs.c.RoundingMode_HalfTrunc => String.fromLiteral("halfTrunc"),
            temporal_rs.c.RoundingMode_HalfEven => String.fromLiteral("halfEven"),
            else => unreachable,
        },
    );

    // 4. Return the value from the "Rounding Mode" column of the row with stringValue in its
    //    "String Identifier" column.
    const rounding_mode_map = std.StaticStringMap(temporal_rs.c.RoundingMode).initComptime(&.{
        .{ "ceil", temporal_rs.c.RoundingMode_Ceil },
        .{ "floor", temporal_rs.c.RoundingMode_Floor },
        .{ "expand", temporal_rs.c.RoundingMode_Expand },
        .{ "trunc", temporal_rs.c.RoundingMode_Trunc },
        .{ "halfCeil", temporal_rs.c.RoundingMode_HalfCeil },
        .{ "halfFloor", temporal_rs.c.RoundingMode_HalfFloor },
        .{ "halfExpand", temporal_rs.c.RoundingMode_HalfExpand },
        .{ "halfTrunc", temporal_rs.c.RoundingMode_HalfTrunc },
        .{ "halfEven", temporal_rs.c.RoundingMode_HalfEven },
    });
    return rounding_mode_map.get(rounding_mode.slice.ascii).?;
}
