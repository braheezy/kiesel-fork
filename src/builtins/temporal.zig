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

/// 12.2.8 ToTemporalCalendarIdentifier ( temporalCalendarLike )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporalcalendaridentifier
pub fn toTemporalCalendarIdentifier(
    agent: *Agent,
    temporal_calendar_like: Value,
) Agent.Error!temporal_rs.c.AnyCalendarKind {
    // 1. If temporalCalendarLike is an Object, then
    if (temporal_calendar_like.isObject()) {
        // a. If temporalCalendarLike has an [[InitializedTemporalDate]],
        //    [[InitializedTemporalDateTime]], [[InitializedTemporalMonthDay]],
        //    [[InitializedTemporalYearMonth]], or [[InitializedTemporalZonedDateTime]] internal
        //    slot, then
        const item = temporal_calendar_like.asObject();
        if (switch (item.tag) {
            .temporal_plain_date => temporal_rs.c.temporal_rs_PlainDate_calendar(
                item.as(builtins.temporal.PlainDate).fields.inner,
            ),
            .temporal_plain_date_time => temporal_rs.c.temporal_rs_PlainDateTime_calendar(
                item.as(builtins.temporal.PlainDateTime).fields.inner,
            ),
            .temporal_plain_month_day => temporal_rs.c.temporal_rs_PlainMonthDay_calendar(
                item.as(builtins.temporal.PlainMonthDay).fields.inner,
            ),
            .temporal_plain_year_month => temporal_rs.c.temporal_rs_PlainYearMonth_calendar(
                item.as(builtins.temporal.PlainYearMonth).fields.inner,
            ),
            .temporal_zoned_date_time => temporal_rs.c.temporal_rs_ZonedDateTime_calendar(
                item.as(builtins.temporal.ZonedDateTime).fields.inner,
            ),
            else => null,
        }) |calendar| {
            // i. Return temporalCalendarLike.[[Calendar]].
            return temporal_rs.c.temporal_rs_Calendar_kind(calendar);
        }
    }

    // 2. If temporalCalendarLike is not a String, throw a TypeError exception.
    if (!temporal_calendar_like.isString()) {
        return agent.throwException(
            .type_error,
            "Calendar must be a string or object",
            .{},
        );
    }

    // 3. Let identifier be ? ParseTemporalCalendarString(temporalCalendarLike).
    // 4. Return ? CanonicalizeCalendar(identifier).
    const calendar_utf8 = try temporal_calendar_like.asString().toUtf8(agent.gc_allocator);
    defer agent.gc_allocator.free(calendar_utf8);
    const temporal_rs_calendar = temporal_rs.temporalErrorResult(
        temporal_rs.c.temporal_rs_Calendar_from_utf8(
            temporal_rs.toDiplomatStringView(calendar_utf8),
        ),
    ) catch |err| switch (err) {
        error.RangeError => {
            return agent.throwException(
                .range_error,
                "Invalid calendar {}",
                .{temporal_calendar_like},
            );
        },
        else => unreachable,
    };
    defer temporal_rs.c.temporal_rs_Calendar_destroy(temporal_rs_calendar.?);
    return temporal_rs.c.temporal_rs_Calendar_kind(temporal_rs_calendar.?);
}

/// 12.2.9 GetTemporalCalendarIdentifierWithISODefault ( item )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalcalendarslotvaluewithisodefault
pub fn getTemporalCalendarIdentifierWithISODefault(
    agent: *Agent,
    item: *Object,
) Agent.Error!temporal_rs.c.AnyCalendarKind {
    // 1. If item has an [[InitializedTemporalDate]], [[InitializedTemporalDateTime]],
    //    [[InitializedTemporalMonthDay]], [[InitializedTemporalYearMonth]], or
    //    [[InitializedTemporalZonedDateTime]] internal slot, then
    if (switch (item.tag) {
        .temporal_plain_date => temporal_rs.c.temporal_rs_PlainDate_calendar(
            item.as(builtins.temporal.PlainDate).fields.inner,
        ),
        .temporal_plain_date_time => temporal_rs.c.temporal_rs_PlainDateTime_calendar(
            item.as(builtins.temporal.PlainDateTime).fields.inner,
        ),
        .temporal_plain_month_day => temporal_rs.c.temporal_rs_PlainMonthDay_calendar(
            item.as(builtins.temporal.PlainMonthDay).fields.inner,
        ),
        .temporal_plain_year_month => temporal_rs.c.temporal_rs_PlainYearMonth_calendar(
            item.as(builtins.temporal.PlainYearMonth).fields.inner,
        ),
        .temporal_zoned_date_time => temporal_rs.c.temporal_rs_ZonedDateTime_calendar(
            item.as(builtins.temporal.ZonedDateTime).fields.inner,
        ),
        else => null,
    }) |calendar| {
        // a. Return item.[[Calendar]].
        return temporal_rs.c.temporal_rs_Calendar_kind(calendar);
    }

    // 2. Let calendarLike be ? Get(item, "calendar").
    const calendar_like = try item.get(agent, PropertyKey.from("calendar"));

    // 3. If calendarLike is undefined, then
    if (calendar_like.isUndefined()) {
        // a. Return "iso8601".
        return temporal_rs.c.AnyCalendarKind_Iso;
    }

    // 4. Return ? ToTemporalCalendarIdentifier(calendarLike).
    return toTemporalCalendarIdentifier(agent, calendar_like);
}

/// 13.6 GetTemporalOverflowOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporaloverflowoption
pub fn getTemporalOverflowOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.ArithmeticOverflow {
    // 1. Let stringValue be ? GetOption(options, "overflow", string, « "constrain", "reject" »,
    //    "constrain").
    const string_value = try options.getOption(
        agent,
        "overflow",
        .string,
        &.{
            String.fromLiteral("constrain"),
            String.fromLiteral("reject"),
        },
        String.fromLiteral("constrain"),
    );

    // 2. If stringValue is "constrain", return constrain.
    // 3. Return reject.
    const arithmetic_overflow_map = std.StaticStringMap(
        temporal_rs.c.ArithmeticOverflow,
    ).initComptime(&.{
        .{ "constrain", temporal_rs.c.ArithmeticOverflow_Constrain },
        .{ "reject", temporal_rs.c.ArithmeticOverflow_Reject },
    });
    return arithmetic_overflow_map.get(string_value.slice.ascii).?;
}

/// 13.7 GetTemporalDisambiguationOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporaldisambiguationoption
pub fn getTemporalDisambiguationOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.Disambiguation {
    // 1. Let stringValue be ? GetOption(options, "disambiguation", string, « "compatible",
    //    "earlier", "later", "reject" », "compatible").
    const string_value = try options.getOption(
        agent,
        "disambiguation",
        .string,
        &.{
            String.fromLiteral("compatible"),
            String.fromLiteral("earlier"),
            String.fromLiteral("later"),
            String.fromLiteral("reject"),
        },
        String.fromLiteral("compatible"),
    );

    // 2. If stringValue is "compatible", return compatible.
    // 3. If stringValue is "earlier", return earlier.
    // 4. If stringValue is "later", return later.
    // 5. Return reject.
    const disambiguation_map = std.StaticStringMap(
        temporal_rs.c.Disambiguation,
    ).initComptime(&.{
        .{ "compatible", temporal_rs.c.Disambiguation_Compatible },
        .{ "earlier", temporal_rs.c.Disambiguation_Earlier },
        .{ "later", temporal_rs.c.Disambiguation_Later },
        .{ "reject", temporal_rs.c.Disambiguation_Reject },
    });
    return disambiguation_map.get(string_value.slice.ascii).?;
}

/// 13.9 GetTemporalOffsetOption ( options, fallback )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporaloffsetoption
pub fn getTemporalOffsetOption(
    agent: *Agent,
    options: *Object,
    fallback: temporal_rs.c.OffsetDisambiguation,
) Agent.Error!temporal_rs.c.OffsetDisambiguation {
    // 1. If fallback is prefer, let stringFallback be "prefer".
    // 2. Else if fallback is use, let stringFallback be "use".
    // 3. Else if fallback is ignore, let stringFallback be "ignore".
    // 4. Else, let stringFallback be "reject".
    // 5. Let stringValue be ? GetOption(options, "offset", string, « "prefer", "use", "ignore",
    //    "reject" », stringFallback).
    const string_value = try options.getOption(
        agent,
        "offset",
        .string,
        &.{
            String.fromLiteral("prefer"),
            String.fromLiteral("use"),
            String.fromLiteral("ignore"),
            String.fromLiteral("reject"),
        },
        switch (fallback) {
            temporal_rs.c.OffsetDisambiguation_Prefer => String.fromLiteral("prefer"),
            temporal_rs.c.OffsetDisambiguation_Use => String.fromLiteral("use"),
            temporal_rs.c.OffsetDisambiguation_Ignore => String.fromLiteral("ignore"),
            temporal_rs.c.OffsetDisambiguation_Reject => String.fromLiteral("reject"),
            else => unreachable,
        },
    );

    // 6. If stringValue is "prefer", return prefer.
    // 7. If stringValue is "use", return use.
    // 8. If stringValue is "ignore", return ignore.
    // 9. Return reject.
    const offset_disambiguation_map = std.StaticStringMap(
        temporal_rs.c.OffsetDisambiguation,
    ).initComptime(&.{
        .{ "prefer", temporal_rs.c.OffsetDisambiguation_Prefer },
        .{ "use", temporal_rs.c.OffsetDisambiguation_Use },
        .{ "ignore", temporal_rs.c.OffsetDisambiguation_Ignore },
        .{ "reject", temporal_rs.c.OffsetDisambiguation_Reject },
    });
    return offset_disambiguation_map.get(string_value.slice.ascii).?;
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
    // 3. If stringValue is "never", return never.
    // 4. If stringValue is "critical", return critical.
    // 5. Return auto.
    const display_calendar_map = std.StaticStringMap(
        temporal_rs.c.DisplayCalendar,
    ).initComptime(&.{
        .{ "auto", temporal_rs.c.DisplayCalendar_Auto },
        .{ "always", temporal_rs.c.DisplayCalendar_Always },
        .{ "never", temporal_rs.c.DisplayCalendar_Never },
        .{ "critical", temporal_rs.c.DisplayCalendar_Critical },
    });
    return display_calendar_map.get(string_value.slice.ascii).?;
}

/// 13.11 GetTemporalShowTimeZoneNameOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalshowtimezonenameoption
pub fn getTemporalShowTimeZoneNameOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.DisplayTimeZone {
    // 1. Let stringValue be ? GetOption(options, "timeZoneName", string, « "auto", "never", "critical" », "auto").
    const string_value = try options.getOption(
        agent,
        "timeZoneName",
        .string,
        &.{
            String.fromLiteral("auto"),
            String.fromLiteral("never"),
            String.fromLiteral("critical"),
        },
        String.fromLiteral("auto"),
    );

    // 2. If stringValue is "never", return never.
    // 3. If stringValue is "critical", return critical.
    // 4. Return auto.
    const display_time_zone_map = std.StaticStringMap(
        temporal_rs.c.DisplayTimeZone,
    ).initComptime(&.{
        .{ "auto", temporal_rs.c.DisplayTimeZone_Auto },
        .{ "never", temporal_rs.c.DisplayTimeZone_Never },
        .{ "critical", temporal_rs.c.DisplayTimeZone_Critical },
    });
    return display_time_zone_map.get(string_value.slice.ascii).?;
}

/// 13.12 GetTemporalShowOffsetOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalshowoffsetoption
pub fn getTemporalShowOffsetOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.DisplayOffset {
    // 1. Let stringValue be ? GetOption(options, "offset", string, « "auto", "never" », "auto").
    const string_value = try options.getOption(
        agent,
        "offset",
        .string,
        &.{
            String.fromLiteral("auto"),
            String.fromLiteral("never"),
        },
        String.fromLiteral("auto"),
    );

    // 2. If stringValue is "never", return never.
    // 3. Return auto.
    const display_offset_map = std.StaticStringMap(
        temporal_rs.c.DisplayOffset,
    ).initComptime(&.{
        .{ "auto", temporal_rs.c.DisplayOffset_Auto },
        .{ "never", temporal_rs.c.DisplayOffset_Never },
    });
    return display_offset_map.get(string_value.slice.ascii).?;
}

/// 13.13 GetDirectionOption ( options )
/// https://tc39.es/proposal-temporal/#sec-temporal-getdirectionoption
pub fn getTemporalDirectionOption(
    agent: *Agent,
    options: *Object,
) Agent.Error!temporal_rs.c.TransitionDirection {
    // 1. Let stringValue be ? GetOption(options, "direction", string, « "next", "previous" »,
    //    required).
    const string_value = try options.getOption(
        agent,
        "direction",
        .string,
        &.{
            String.fromLiteral("next"),
            String.fromLiteral("previous"),
        },
        .required,
    );

    // 2. If stringValue is "next", return next.
    // 3. Return previous.
    const transition_direction_map = std.StaticStringMap(
        temporal_rs.c.TransitionDirection,
    ).initComptime(&.{
        .{ "next", temporal_rs.c.TransitionDirection_Next },
        .{ "previous", temporal_rs.c.TransitionDirection_Previous },
    });
    return transition_direction_map.get(string_value.slice.ascii).?;
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

/// 13.17 GetTemporalUnitValuedOption ( options, key, default )
/// https://tc39.es/proposal-temporal/#sec-temporal-gettemporalunitvaluedoption
pub fn getTemporalUnitValuedOption(
    agent: *Agent,
    options: *Object,
    comptime key: []const u8,
    comptime default: enum { required, unset },
) Agent.Error!?temporal_rs.c.Unit {
    // 1. Let allowedStrings be a List containing all values in the "Singular property name" and
    //    "Plural property name" columns of Table 21, except the header row.
    // 2. Append "auto" to allowedStrings.
    // 3. NOTE: For each singular Temporal unit name that is contained within allowedStrings, the
    //    corresponding plural name is also contained within it.
    const allowed_strings: []const *const String = &.{
        String.fromLiteral("auto"),
        String.fromLiteral("year"),
        String.fromLiteral("years"),
        String.fromLiteral("month"),
        String.fromLiteral("months"),
        String.fromLiteral("week"),
        String.fromLiteral("weeks"),
        String.fromLiteral("day"),
        String.fromLiteral("days"),
        String.fromLiteral("hour"),
        String.fromLiteral("hours"),
        String.fromLiteral("minute"),
        String.fromLiteral("minutes"),
        String.fromLiteral("second"),
        String.fromLiteral("seconds"),
        String.fromLiteral("millisecond"),
        String.fromLiteral("milliseconds"),
        String.fromLiteral("microsecond"),
        String.fromLiteral("microseconds"),
        String.fromLiteral("nanosecond"),
        String.fromLiteral("nanoseconds"),
    };

    // 4. If default is unset, then
    //     a. Let defaultValue be undefined.
    // 5. Else,
    //     a. Let defaultValue be default.
    const default_value = switch (default) {
        .unset => null,
        .required => .required,
    };

    // 6. Let value be ? GetOption(options, key, string, allowedStrings, defaultValue).
    const value = try options.getOption(
        agent,
        key,
        .string,
        allowed_strings,
        default_value,
    ) orelse {
        // 7. If value is undefined, return unset.
        return null;
    };

    // 8. If value is "auto", return auto.
    // 9. Return the value in the "Value" column of Table 21 corresponding to the row with value in
    //    its "Singular property name" or "Plural property name" column.
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

/// 13.18 ValidateTemporalUnitValue ( value, unitGroup [ , extraValues ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-validatetemporalunitvaluedoption
pub fn validateTemporalUnitValue(
    agent: *Agent,
    maybe_value: ?temporal_rs.c.Unit,
    comptime key: []const u8,
    comptime unit_group: enum { date, time, datetime },
    comptime extra_values: []const temporal_rs.c.Unit,
) Agent.Error!void {
    // 1. If value is unset, return unused.
    const value = maybe_value orelse return;

    // 2. If extraValues is present and extraValues contains value, return unused.
    if (std.mem.indexOfScalar(temporal_rs.c.Unit, extra_values, value) != null) return;

    // 3. Let category be the value in the “Category” column of the row of Table 21 whose “Value”
    //    column contains value. If there is no such row, throw a RangeError exception.
    const category: enum { date, time } = switch (value) {
        temporal_rs.c.Unit_Auto => {
            return agent.throwException(.range_error, "Invalid value for option '{s}'", .{key});
        },
        temporal_rs.c.Unit_Year,
        temporal_rs.c.Unit_Month,
        temporal_rs.c.Unit_Week,
        temporal_rs.c.Unit_Day,
        => .date,
        temporal_rs.c.Unit_Hour,
        temporal_rs.c.Unit_Minute,
        temporal_rs.c.Unit_Second,
        temporal_rs.c.Unit_Millisecond,
        temporal_rs.c.Unit_Microsecond,
        temporal_rs.c.Unit_Nanosecond,
        => .time,
        else => unreachable,
    };

    // 4. If category is date and unitGroup is date or datetime, return unused.
    // 5. If category is time and unitGroup is time or datetime, return unused.
    switch (category) {
        .date => if (unit_group == .date or unit_group == .datetime) return,
        .time => if (unit_group == .time or unit_group == .datetime) return,
    }

    // 6. Throw a RangeError exception.
    return agent.throwException(.range_error, "Invalid value for option '{s}'", .{key});
}

/// 13.40 ToMonthCode ( argument )
/// https://tc39.es/proposal-temporal/#sec-temporal-tomonthcode
pub fn toMonthCode(agent: *Agent, argument: Value) Agent.Error![]const u8 {
    // 1. Let monthCode be ? ToPrimitive(argument, string).
    const month_code = try argument.toPrimitive(agent, .string);

    // 2. If monthCode is not a String, throw a TypeError exception.
    if (!month_code.isString()) {
        return agent.throwException(.type_error, "Month code must be a string", .{});
    }

    const month_code_utf8 = try month_code.asString().toUtf8(agent.gc_allocator);

    // 3. If the length of monthCode is not 3 or 4, throw a RangeError exception.
    // 4. If the first code unit of monthCode is not 0x004D (LATIN CAPITAL LETTER M), throw a
    //    RangeError exception.
    // 5. If the second code unit of monthCode is not in the inclusive interval from 0x0030 (DIGIT
    //    ZERO) to 0x0039 (DIGIT NINE), throw a RangeError exception.
    // 6. If the third code unit of monthCode is not in the inclusive interval from 0x0030 (DIGIT
    //    ZERO) to 0x0039 (DIGIT NINE), throw a RangeError exception.
    // 7. If the length of monthCode is 4 and the fourth code unit of monthCode is not 0x004C
    //    (LATIN CAPITAL LETTER L), throw a RangeError exception.
    // 8. Let monthCodeDigits be the substring of monthCode from 1 to 3.
    // 9. Let monthCodeInteger be ℝ(StringToNumber(monthCodeDigits)).
    // 10. If monthCodeInteger is 0 and the length of monthCode is not 4, throw a RangeError
    //     exception.
    const valid =
        (month_code_utf8.len == 3 or month_code_utf8.len == 4) and
        month_code_utf8[0] == 'M' and
        std.ascii.isDigit(month_code_utf8[1]) and
        std.ascii.isDigit(month_code_utf8[2]) and
        (month_code_utf8.len != 4 or (month_code_utf8[3] == 'L' and
            (month_code_utf8[1] != '0' or month_code_utf8[2] != '0')));
    if (!valid) {
        return agent.throwException(.range_error, "Invalid month code", .{});
    }

    // 11. Return monthCode.
    return month_code_utf8;
}

/// 13.41 ToOffsetString ( argument )
/// https://tc39.es/proposal-temporal/#sec-temporal-tooffsetstring
pub fn toOffsetString(agent: *Agent, argument: Value) Agent.Error![]const u8 {
    // 1. Let offset be ? ToPrimitive(argument, string).
    const offset = try argument.toPrimitive(agent, .string);

    // 2. If offset is not a String, throw a TypeError exception.
    if (!offset.isString()) {
        return agent.throwException(.type_error, "Offset must be a string", .{});
    }

    // 3. Perform ? ParseDateTimeUTCOffset(offset).
    // 4. Return offset.
    const offset_utf8 = try offset.asString().toUtf8(agent.gc_allocator);
    const temporal_rs_time_zone = temporal_rs.temporalErrorResult(
        temporal_rs.c.temporal_rs_TimeZone_try_from_offset_str(
            temporal_rs.toDiplomatStringView(offset_utf8),
        ),
    ) catch |err| switch (err) {
        error.RangeError => {
            return agent.throwException(.range_error, "Invalid offset string", .{});
        },
        else => unreachable,
    };
    defer temporal_rs.c.temporal_rs_TimeZone_destroy(temporal_rs_time_zone.?);
    return offset_utf8;
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
