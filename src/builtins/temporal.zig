//! 1 The Temporal Object
//! https://tc39.es/proposal-temporal/#sec-temporal-objects

const std = @import("std");

const temporal_rs = @import("../c/temporal_rs.zig");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
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
) Agent.Error!*const temporal_rs.c.TimeZone {
    // 1. If temporalTimeZoneLike is an Object, then
    if (temporal_time_zone_like.isObject()) {
        // a. If temporalTimeZoneLike has an [[InitializedTemporalZonedDateTime]] internal slot, then
        if (temporal_time_zone_like.asObject().is(ZonedDateTime)) {
            // i. Return temporalTimeZoneLike.[[TimeZone]].
            const temporal_rs_time_zone = temporal_rs.c.temporal_rs_ZonedDateTime_timezone(
                temporal_time_zone_like.asObject().as(ZonedDateTime).fields.inner,
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
    if (!temporal_rs.c.temporal_rs_TimeZone_is_valid(temporal_rs_time_zone.?) or
        // https://github.com/boa-dev/temporal/blob/3455373e250dc3c3c5ee0112f379bbc97d7c351c/src/builtins/core/timezone.rs#L109-L111
        std.mem.eql(u8, time_zone, "Z"))
    {
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
