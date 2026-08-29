//! 21.4 Date Objects
//! https://tc39.es/ecma262/#sec-date-objects

const std = @import("std");

const temporal_rs = @import("temporal_rs");

const build_options = @import("build-options");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PreferredType = Value.PreferredType;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const StringParser = utils.StringParser;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createTemporalInstant = builtins.createTemporalInstant;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

const hours_per_day = 24;
const minutes_per_hour = 60;

/// Table 62: Names of days of the week
/// https://tc39.es/ecma262/#sec-todatestring-day-names
const week_day_names = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

/// Table 63: Names of months of the year
/// https://tc39.es/ecma262/#sec-todatestring-month-names
const month_names = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

const Year = std.math.IntFittingRange(1970 - 273_790, 1970 + 273_790);
const Month = std.math.IntFittingRange(0, 11);
const Date_ = std.math.IntFittingRange(1, 31);
const Hour = std.math.IntFittingRange(0, 23);
const Minute = std.math.IntFittingRange(0, 59);
const Second = std.math.IntFittingRange(0, 59);
const Millisecond = std.math.IntFittingRange(0, 999);
const WeekDay = std.math.IntFittingRange(0, 6);
const DaysInYear = std.math.IntFittingRange(365, 366);
const DayWithinYear = std.math.IntFittingRange(0, 365);

/// Simplified infallible variant of `Value.toIntegerOrInfinity()`
fn toIntegerOrInfinity(x: f64) f64 {
    if (std.math.isNan(x)) return 0;
    if (std.math.isInf(x)) return x;
    const truncated = @trunc(x);
    // Normalize negative zero
    return if (truncated == 0) 0 else truncated;
}

/// 22.1.3.17.3 ToZeroPaddedDecimalString ( n, minLength )
/// https://tc39.es/ecma262/#sec-tozeropaddeddecimalstring
fn toZeroPaddedDecimalString(buf: []u8, n: anytype, min_length: usize) []const u8 {
    // NOTE: std.fmt does a weird thing where padded 1 becomes '00+1', so we do this ourselves.
    var tmp: [100]u8 = undefined;
    const s = std.fmt.bufPrint(&tmp, "{d}", .{@abs(n)}) catch unreachable;

    @memset(buf[0 .. buf.len - s.len], '0');
    @memcpy(buf[buf.len - s.len ..], s);

    const start_index = @min(buf.len - s.len, buf.len - min_length);
    return buf[start_index..];
}

/// https://howardhinnant.github.io/date_algorithms.html#days_from_civil
fn daysFromCivil(y_: i64, m: i32, d: i32) i64 {
    var y = y_;
    if (m <= 2) y -= 1;
    const era = @divTrunc(if (y >= 0) y else y - 399, 400);
    const yoe = y - era * 400;
    const doy = @divTrunc(153 * (m + @as(i32, if (m > 2) -3 else 9)) + 2, 5 + d - 1);
    const doe = yoe * 365 + @divTrunc(yoe, 4) - @divTrunc(yoe, 100) + doy;
    return era * 146097 + doe - 719468;
}

/// 21.4.1.3 Day ( tv )
/// https://tc39.es/ecma262/#sec-day
pub fn day(tv: f64) f64 {
    // 1. Return 𝔽(floor(ℝ(tv / msPerDay))).
    return std.math.floor(tv / std.time.ms_per_day);
}

/// 21.4.1.4 TimeWithinDay ( tv )
/// https://tc39.es/ecma262/#sec-timewithinday
pub fn timeWithinDay(tv: f64) f64 {
    // 1. Return 𝔽(ℝ(tv) modulo ℝ(msPerDay)).
    return @mod(tv, std.time.ms_per_day);
}

/// 21.4.1.5 DaysInYear ( y )
/// https://tc39.es/ecma262/#sec-daysinyear
pub fn daysInYear(year: Year) DaysInYear {
    // 1. Let ry be ℝ(y).

    // 2. If (ry modulo 400) = 0, return 366𝔽.
    if (@mod(year, 400) == 0) return 366;

    // 3. If (ry modulo 100) = 0, return 365𝔽.
    if (@mod(year, 100) == 0) return 365;

    // 4. If (ry modulo 4) = 0, return 366𝔽.
    if (@mod(year, 4) == 0) return 366;

    // 5. Return 365𝔽.
    return 365;
}

/// 21.4.1.6 DayFromYear ( y )
/// https://tc39.es/ecma262/#sec-dayfromyear
pub fn dayFromYear(year: Year) f64 {
    // 1. Let ry be ℝ(y).
    // 2. NOTE: In the following steps, numberYears1, numberYears4, numberYears100, and
    //    numberYears400 represent the number of years divisible by 1, 4, 100, and 400,
    //    respectively, that occur between the epoch and the start of year y. The number is negative
    //    if y is before the epoch.

    // 3. Let numberYears1 be (ry - 1970).
    const number_years_1: f64 = @floatFromInt(year - 1970);

    // 4. Let numberYears4 be floor((ry - 1969) / 4).
    const number_years_4: f64 = @floatFromInt(@divFloor(year - 1969, 4));

    // 5. Let numberYears100 be floor((ry - 1901) / 100).
    const number_years_100: f64 = @floatFromInt(@divFloor(year - 1901, 100));

    // 6. Let numberYears400 be floor((ry - 1601) / 400).
    const number_years_400: f64 = @floatFromInt(@divFloor(year - 1601, 400));

    // 7. Return 𝔽(365 × numberYears1 + numberYears4 - numberYears100 + numberYears400).
    return 365 * number_years_1 + number_years_4 - number_years_100 + number_years_400;
}

/// 21.4.1.7 TimeFromYear ( y )
/// https://tc39.es/ecma262/#sec-timefromyear
pub fn timeFromYear(year: Year) f64 {
    // 1. Return msPerDay × DayFromYear(y).
    return std.time.ms_per_day * dayFromYear(year);
}

/// 21.4.1.8 YearFromTime ( tv )
/// https://tc39.es/ecma262/#sec-yearfromtime
pub fn yearFromTime(tv: f64) Year {
    // 1. Return the largest integral Number y (closest to +∞) such that TimeFromYear(y) ≤ tv.
    const year: Year = @intFromFloat(@divFloor(tv, (365.2425 * std.time.ms_per_day)) + 1970);
    const t2 = timeFromYear(year);
    if (t2 > tv) return year - 1;
    if (t2 + @as(f64, @floatFromInt(daysInYear(year))) * std.time.ms_per_day <= tv) return year + 1;
    return year;
}

/// 21.4.1.9 DayWithinYear ( tv )
/// https://tc39.es/ecma262/#sec-daywithinyear
pub fn dayWithinYear(tv: f64) DayWithinYear {
    // 1. Return Day(tv) - DayFromYear(YearFromTime(tv)).
    return @intFromFloat(day(tv) - dayFromYear(yearFromTime(tv)));
}

/// 21.4.1.10 InLeapYear ( tv )
/// https://tc39.es/ecma262/#sec-inleapyear
pub fn inLeapYear(tv: f64) bool {
    // 1. If DaysInYear(YearFromTime(tv)) is 366𝔽, return 1𝔽.
    // 2. Return +0𝔽.
    return daysInYear(yearFromTime(tv)) == 366;
}

/// 21.4.1.11 MonthFromTime ( tv )
/// https://tc39.es/ecma262/#sec-monthfromtime
pub fn monthFromTime(tv: f64) Month {
    // 1. Let inLeapYear be InLeapYear(tv).
    const in_leap_year: DayWithinYear = @intFromBool(inLeapYear(tv));

    // 2. Let dayWithinYear be DayWithinYear(tv).
    const day_within_year = dayWithinYear(tv);

    // 3. If dayWithinYear < 31𝔽, return +0𝔽.
    if (day_within_year < 31) return 0;

    // 4. If dayWithinYear < 59𝔽 + inLeapYear, return 1𝔽.
    if (day_within_year < 59 + in_leap_year) return 1;

    // 5. If dayWithinYear < 90𝔽 + inLeapYear, return 2𝔽.
    if (day_within_year < 90 + in_leap_year) return 2;

    // 6. If dayWithinYear < 120𝔽 + inLeapYear, return 3𝔽.
    if (day_within_year < 120 + in_leap_year) return 3;

    // 7. If dayWithinYear < 151𝔽 + inLeapYear, return 4𝔽.
    if (day_within_year < 151 + in_leap_year) return 4;

    // 8. If dayWithinYear < 181𝔽 + inLeapYear, return 5𝔽.
    if (day_within_year < 181 + in_leap_year) return 5;

    // 9. If dayWithinYear < 212𝔽 + inLeapYear, return 6𝔽.
    if (day_within_year < 212 + in_leap_year) return 6;

    // 10. If dayWithinYear < 243𝔽 + inLeapYear, return 7𝔽.
    if (day_within_year < 243 + in_leap_year) return 7;

    // 11. If dayWithinYear < 273𝔽 + inLeapYear, return 8𝔽.
    if (day_within_year < 273 + in_leap_year) return 8;

    // 12. If dayWithinYear < 304𝔽 + inLeapYear, return 9𝔽.
    if (day_within_year < 304 + in_leap_year) return 9;

    // 13. If dayWithinYear < 334𝔽 + inLeapYear, return 10𝔽.
    if (day_within_year < 334 + in_leap_year) return 10;

    // 14. Assert: dayWithinYear < 365𝔽 + inLeapYear.
    std.debug.assert(day_within_year < 365 + in_leap_year);

    // 15. Return 11𝔽.
    return 11;
}

/// 21.4.1.12 DateFromTime ( tv )
/// https://tc39.es/ecma262/#sec-datefromtime
pub fn dateFromTime(tv: f64) Date_ {
    // 1. Let inLeapYear be InLeapYear(tv).
    const in_leap_year: DayWithinYear = @intFromBool(inLeapYear(tv));

    // 2. Let dayWithinYear be DayWithinYear(tv).
    const day_within_year = dayWithinYear(tv);

    // 3. Let month be MonthFromTime(tv).
    const month = monthFromTime(tv);

    // 4. If month is +0𝔽, return dayWithinYear + 1𝔽.
    if (month == 0) return @intCast(day_within_year + 1);

    // 5. If month is 1𝔽, return dayWithinYear - 30𝔽.
    if (month == 1) return @intCast(day_within_year - 30);

    // 6. If month is 2𝔽, return dayWithinYear - 58𝔽 - inLeapYear.
    if (month == 2) return @intCast(day_within_year - 58 - in_leap_year);

    // 7. If month is 3𝔽, return dayWithinYear - 89𝔽 - inLeapYear.
    if (month == 3) return @intCast(day_within_year - 89 - in_leap_year);

    // 8. If month is 4𝔽, return dayWithinYear - 119𝔽 - inLeapYear.
    if (month == 4) return @intCast(day_within_year - 119 - in_leap_year);

    // 9. If month is 5𝔽, return dayWithinYear - 150𝔽 - inLeapYear.
    if (month == 5) return @intCast(day_within_year - 150 - in_leap_year);

    // 10. If month is 6𝔽, return dayWithinYear - 180𝔽 - inLeapYear.
    if (month == 6) return @intCast(day_within_year - 180 - in_leap_year);

    // 11. If month is 7𝔽, return dayWithinYear - 211𝔽 - inLeapYear.
    if (month == 7) return @intCast(day_within_year - 211 - in_leap_year);

    // 12. If month is 8𝔽, return dayWithinYear - 242𝔽 - inLeapYear.
    if (month == 8) return @intCast(day_within_year - 242 - in_leap_year);

    // 13. If month is 9𝔽, return dayWithinYear - 272𝔽 - inLeapYear.
    if (month == 9) return @intCast(day_within_year - 272 - in_leap_year);

    // 14. If month is 10𝔽, return dayWithinYear - 303𝔽 - inLeapYear.
    if (month == 10) return @intCast(day_within_year - 303 - in_leap_year);

    // 15. Assert: month is 11𝔽.
    std.debug.assert(month == 11);

    // 16. Return dayWithinYear - 333𝔽 - inLeapYear.
    return @intCast(day_within_year - 333 - in_leap_year);
}

/// 21.4.1.13 WeekDay ( tv )
/// https://tc39.es/ecma262/#sec-weekday
pub fn weekDay(tv: f64) WeekDay {
    // 1. Return 𝔽(ℝ(Day(tv) + 4𝔽) modulo 7).
    return @intFromFloat(@mod(day(tv) + 4, 7));
}

/// 21.4.1.14 HourFromTime ( tv )
/// https://tc39.es/ecma262/#sec-hourfromtime
pub fn hourFromTime(tv: f64) Hour {
    // 1. Return 𝔽(floor(ℝ(tv / msPerHour)) modulo HoursPerDay).
    return @intFromFloat(@mod(std.math.floor(tv / std.time.ms_per_hour), hours_per_day));
}

/// 21.4.1.15 MinFromTime ( tv )
/// https://tc39.es/ecma262/#sec-minfromtime
pub fn minFromTime(tv: f64) Minute {
    // 1. Return 𝔽(floor(ℝ(tv / msPerMinute)) modulo MinutesPerHour).
    return @intFromFloat(@mod(std.math.floor(tv / std.time.ms_per_min), minutes_per_hour));
}

/// 21.4.1.16 SecFromTime ( tv )
/// https://tc39.es/ecma262/#sec-secfromtime
pub fn secFromTime(tv: f64) Second {
    // 1. Return 𝔽(floor(ℝ(tv / msPerSecond)) modulo SecondsPerMinute).
    return @intFromFloat(@mod(std.math.floor(tv / std.time.ms_per_s), std.time.s_per_min));
}

/// 21.4.1.17 msFromTime ( tv )
/// https://tc39.es/ecma262/#sec-msfromtime
pub fn msFromTime(tv: f64) Millisecond {
    // 1. Return 𝔽(ℝ(tv) modulo ℝ(msPerSecond)).
    return @intFromFloat(@mod(tv, std.time.ms_per_s));
}

/// 21.4.1.21 GetNamedTimeZoneOffsetNanoseconds ( timeZoneIdentifier, epochNanoseconds )
/// https://tc39.es/ecma262/#sec-getnamedtimezoneoffsetnanoseconds
pub fn getNamedTimeZoneOffsetNanoseconds(
    time_zone: temporal_rs.c.TimeZone,
    epoch_nanoseconds: i128,
) i64 {
    const zoned_date_time = temporal_rs.success(
        temporal_rs.c.temporal_rs_ZonedDateTime_try_new(
            temporal_rs.toI128Nanoseconds(epoch_nanoseconds),
            temporal_rs.c.AnyCalendarKind_Iso,
            time_zone,
        ),
    ).?;
    defer temporal_rs.c.temporal_rs_ZonedDateTime_destroy(zoned_date_time);
    return temporal_rs.c.temporal_rs_ZonedDateTime_offset_nanoseconds(zoned_date_time);
}

/// 21.4.1.24 SystemTimeZoneIdentifier ( )
/// https://tc39.es/ecma262/#sec-systemtimezoneidentifier
pub fn systemTimeZoneIdentifier(platform: *const Agent.Platform) Agent.Platform.TimeZone {
    // 1. If the implementation only supports the UTC time zone, return "UTC".
    // 2. Let systemTimeZoneString be the String representing the host environment's current time
    //    zone, either a primary time zone identifier or an offset time zone identifier.
    // 3. Return systemTimeZoneString.
    return platform.default_time_zone;
}

/// 21.4.1.25 LocalTime ( tv )
/// https://tc39.es/ecma262/#sec-localtime
pub fn localTime(platform: *const Agent.Platform, tv: f64) f64 {
    // 1. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    const time_zone = systemTimeZoneIdentifier(platform);

    // 2. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    const offset_ns = if (@TypeOf(time_zone) == void) blk: {
        break :blk 0;
    } else if (isTimeZoneOffsetString(time_zone)) blk: {
        // a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
        break :blk parseTimeZoneOffsetString(time_zone);
    } else blk: {
        // 3. Else,
        // a. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier,
        //    ℤ(ℝ(tv) × nsPerMillisecond)).
        break :blk getNamedTimeZoneOffsetNanoseconds(time_zone, @intFromFloat(tv * std.time.ns_per_ms));
    };

    // 4. Let offsetMs be truncate(offsetNs / nsPerMillisecond).
    const offset_ms = @trunc(@as(f64, @floatFromInt(offset_ns)) / std.time.ns_per_ms);

    // 5. Return tv + 𝔽(offsetMs).
    return tv + offset_ms;
}

/// 21.4.1.26 UTC ( t )
/// https://tc39.es/ecma262/#sec-utc-t
pub fn utc(platform: *const Agent.Platform, t: f64) f64 {
    // 1. If t is not finite, return NaN.
    if (!std.math.isFinite(t)) return std.math.nan(f64);

    // 2. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    const time_zone = systemTimeZoneIdentifier(platform);

    // 3. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    const offset_ns = if (@TypeOf(time_zone) == void) blk: {
        break :blk 0;
    } else if (isTimeZoneOffsetString(time_zone)) blk: {
        // a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
        break :blk parseTimeZoneOffsetString(time_zone);
    } else blk: {
        // 4. Else,
        // a-e.
        // TODO: Implement named time zone offset resolution
        break :blk 0;
    };

    // 5. Let offsetMs be truncate(offsetNs / nsPerMillisecond).
    const offset_ms = @trunc(@as(f64, @floatFromInt(offset_ns)) / std.time.ns_per_ms);

    // 6. Return t - 𝔽(offsetMs).
    return t - offset_ms;
}

/// 21.4.1.27 MakeTime ( hour, min, sec, ms )
/// https://tc39.es/ecma262/#sec-maketime
pub fn makeTime(hour: f64, minute: f64, second: f64, millisecond: f64) f64 {
    // 1. If hour is not finite, min is not finite, sec is not finite, or ms is not finite, return
    //    NaN.
    if (!std.math.isFinite(hour) or
        !std.math.isFinite(minute) or
        !std.math.isFinite(second) or
        !std.math.isFinite(millisecond))
    {
        return std.math.nan(f64);
    }

    // 2. Let h be 𝔽(! ToIntegerOrInfinity(hour)).
    const h = toIntegerOrInfinity(hour);

    // 3. Let m be 𝔽(! ToIntegerOrInfinity(min)).
    const m = toIntegerOrInfinity(minute);

    // 4. Let s be 𝔽(! ToIntegerOrInfinity(sec)).
    const s = toIntegerOrInfinity(second);

    // 5. Let milli be 𝔽(! ToIntegerOrInfinity(ms)).
    const ms = toIntegerOrInfinity(millisecond);

    // 6. Return ((h × msPerHour + m × msPerMinute) + s × msPerSecond) + milli.
    return ((h * std.time.ms_per_hour + m * std.time.ms_per_min) + s * std.time.ms_per_s) + ms;
}

/// 21.4.1.28 MakeDay ( year, month, date )
/// https://tc39.es/ecma262/#sec-makeday
pub fn makeDay(year: f64, month: f64, date: f64) f64 {
    // 1. If year is not finite, month is not finite, or date is not finite, return NaN.
    if (!std.math.isFinite(year) or !std.math.isFinite(month) or !std.math.isFinite(date)) {
        return std.math.nan(f64);
    }

    // 2. Let y be 𝔽(! ToIntegerOrInfinity(year)).
    const y = toIntegerOrInfinity(year);

    // 3. Let m be 𝔽(! ToIntegerOrInfinity(month)).
    const m = toIntegerOrInfinity(month);

    // 4. Let dt be 𝔽(! ToIntegerOrInfinity(date)).
    const dt = toIntegerOrInfinity(date);

    // 5. Let ym be y + 𝔽(floor(ℝ(m) / 12)).
    const ym = y + std.math.floor(m / 12);

    // 6. If ym is not finite, return NaN.
    if (!std.math.isFinite(ym)) return std.math.nan(f64);

    // 7. Let mn be 𝔽(ℝ(m) modulo 12).
    const mn = @mod(m, 12);

    // 8. Find a finite time value tv such that YearFromTime(tv) is ym, MonthFromTime(tv) is mn, and
    //    DateFromTime(tv) is 1𝔽; but if this is not possible (because some argument is out of
    //    range), return NaN.
    if (ym < @as(f64, @floatFromInt(std.math.minInt(i64))) or
        ym > @as(f64, @floatFromInt(std.math.maxInt(i64))) or
        (mn + 1) > std.math.maxInt(i32))
    {
        return std.math.nan(f64);
    }
    const tv = @as(f64, @floatFromInt(
        daysFromCivil(
            @intFromFloat(ym),
            @intFromFloat(mn + 1),
            1,
        ),
    )) * std.time.ms_per_day;

    // 9. Return Day(tv) + dt - 1𝔽.
    return day(tv) + dt - 1;
}

/// 21.4.1.29 MakeDate ( day, time )
/// https://tc39.es/ecma262/#sec-makedate
pub fn makeDate(day_: f64, time: f64) f64 {
    // 1. If day is not finite or time is not finite, return NaN.
    if (!std.math.isFinite(day_) or !std.math.isFinite(time)) return std.math.nan(f64);

    // 2. Let tv be day × msPerDay + time.
    const time_value = day_ * std.time.ms_per_day + time;

    // 3. If tv is not finite, return NaN.
    if (!std.math.isFinite(time_value)) return std.math.nan(f64);

    // 4. Return tv.
    return time_value;
}

/// 21.4.1.30 MakeFullYear ( year )
/// https://tc39.es/ecma262/#sec-makefullyear
pub fn makeFullYear(year: f64) f64 {
    // 1. If year is one of NaN, +∞𝔽, or -∞𝔽, return NaN.
    if (!std.math.isFinite(year)) return std.math.nan(f64);

    // 2. Let truncated be ! ToIntegerOrInfinity(year).
    const truncated = toIntegerOrInfinity(year);

    // 3. If truncated is in the inclusive interval from 0 to 99, return 1900𝔽 + 𝔽(truncated).
    if (truncated >= 0 and truncated <= 99) return 1900 + truncated;

    // 4. Return 𝔽(truncated).
    return truncated;
}

/// 21.4.1.31 TimeClip ( time )
/// https://tc39.es/ecma262/#sec-timeclip
pub fn timeClip(time: f64) f64 {
    // 1. If time is not finite, return NaN.
    if (!std.math.isFinite(time)) return std.math.nan(f64);

    // 2. If abs(ℝ(time)) > 8.64 × 10**15, return NaN.
    if (@abs(time) > 8.64e15) return std.math.nan(f64);

    // 3. Return 𝔽(! ToIntegerOrInfinity(time)).
    return toIntegerOrInfinity(time);
}

/// 21.4.1.32 Date Time String Format
/// https://tc39.es/ecma262/#sec-date-time-string-format
pub fn parseDateTimeString(string: []const u8) error{InvalidFormat}!f64 {
    var parser = StringParser.init(string);
    var date_only = true;
    const year, const month, const date = blk: {
        const year = switch (parser.peek() orelse return error.InvalidFormat) {
            '+', '-' => |sign| year: {
                _ = parser.consume() orelse unreachable;
                var value = parser.consumeDigits(Year, 6) orelse return error.InvalidFormat;
                if (sign == '-') {
                    if (value == 0) return error.InvalidFormat;
                    value *= -1;
                }
                break :year value;
            },
            else => parser.consumeDigits(Year, 4) orelse return error.InvalidFormat,
        };
        if (parser.peek() != '-') break :blk .{ year, 1, 1 };
        _ = parser.consume() orelse unreachable;
        const month = parser.consumeDigits(Month, 2) orelse return error.InvalidFormat;
        if (month < 1 or month > 12) return error.InvalidFormat;
        if (parser.peek() != '-') break :blk .{ year, month, 1 };
        _ = parser.consume() orelse unreachable;
        const date = parser.consumeDigits(Date_, 2) orelse return error.InvalidFormat;
        if (date < 1 or date > 31) return error.InvalidFormat;
        break :blk .{ year, month, date };
    };
    const hour, const minute, const second, const millisecond = blk: {
        if ((parser.consume() orelse break :blk .{ 0, 0, 0, 0 }) != 'T') return error.InvalidFormat;
        date_only = false;
        const hour = parser.consumeDigits(Hour, 2) orelse return error.InvalidFormat;
        if (hour > 24) return error.InvalidFormat;
        if (parser.consume() != ':') return error.InvalidFormat;
        const minute = parser.consumeDigits(Minute, 2) orelse return error.InvalidFormat;
        if (minute > 59) return error.InvalidFormat;
        if (parser.consume() != ':') break :blk .{ hour, minute, 0, 0 };
        const second = parser.consumeDigits(Second, 2) orelse return error.InvalidFormat;
        if (second > 59) return error.InvalidFormat;
        if (parser.peek() != '.') break :blk .{ hour, minute, second, 0 };
        _ = parser.consume().?;
        const millisecond = parser.consumeDigits(Millisecond, 3) orelse return error.InvalidFormat;
        break :blk .{ hour, minute, second, millisecond };
    };
    const offset_ms = blk: {
        if (date_only) break :blk 0;
        switch (parser.consume() orelse {
            // TODO: Use local time offset
            break :blk 0;
        }) {
            '+', '-' => |sign| {
                const offset_hour = parser.consumeDigits(Hour, 2) orelse return error.InvalidFormat;
                if (offset_hour > 23) return error.InvalidFormat;
                if (parser.consume() != ':') return error.InvalidFormat;
                const offset_minute = parser.consumeDigits(Minute, 2) orelse return error.InvalidFormat;
                if (offset_minute > 59) return error.InvalidFormat;
                var value =
                    @as(f64, @floatFromInt(offset_hour)) * std.time.ms_per_hour +
                    @as(f64, @floatFromInt(offset_minute)) * std.time.ms_per_min;
                // Offset sign is negated
                if (sign == '+') value *= -1;
                break :blk value;
            },
            'Z' => break :blk 0,
            else => return error.InvalidFormat,
        }
    };
    // Did we reach the end of the string?
    if (parser.peek() != null) return error.InvalidFormat;
    const time_value = makeDate(
        makeDay(
            @floatFromInt(year),
            @floatFromInt(month - 1),
            @floatFromInt(date),
        ),
        makeTime(
            @floatFromInt(hour),
            @floatFromInt(minute),
            @floatFromInt(second),
            @floatFromInt(millisecond),
        ),
    );
    return timeClip(time_value + offset_ms);
}

/// Supports the `toString()` and `toUTCString()` formats:
///
/// - `Thu Jan 01 1970 01:00:00 GMT+0100 (Greenwich Mean Time)`
/// - `Thu, 01 Jan 1970 00:00:00 GMT`
pub fn parseOtherString(string: []const u8) error{InvalidFormat}!f64 {
    var parser = StringParser.init(string);
    const weekday = parser.consumeSlice(3) orelse return error.InvalidFormat;
    for (week_day_names) |w| {
        if (std.mem.eql(u8, weekday, w)) break;
    } else return error.InvalidFormat;
    const is_utc_string = parser.peek() == ',';
    if (is_utc_string) _ = parser.consume() orelse unreachable;
    if (parser.consume() != ' ') return error.InvalidFormat;
    const date, const month = if (is_utc_string) blk: {
        const date = parser.consumeDigits(Date_, 2) orelse return error.InvalidFormat;
        if (parser.consume() != ' ') return error.InvalidFormat;
        const month_string = parser.consumeSlice(3) orelse return error.InvalidFormat;
        const month: Month = for (month_names, 1..) |m, i| {
            if (std.mem.eql(u8, month_string, m)) break @intCast(i);
        } else return error.InvalidFormat;
        break :blk .{ date, month };
    } else blk: {
        const month_string = parser.consumeSlice(3) orelse return error.InvalidFormat;
        const month: Month = for (month_names, 1..) |m, i| {
            if (std.mem.eql(u8, month_string, m)) break @intCast(i);
        } else return error.InvalidFormat;
        if (parser.consume() != ' ') return error.InvalidFormat;
        const date = parser.consumeDigits(Date_, 2) orelse return error.InvalidFormat;
        break :blk .{ date, month };
    };
    if (parser.consume() != ' ') return error.InvalidFormat;
    const year = blk: {
        const sign = if (parser.peek() == '-')
            parser.consume() orelse unreachable
        else
            '+';
        var value = parser.consumeDigits(Year, 6) orelse
            parser.consumeDigits(Year, 5) orelse
            parser.consumeDigits(Year, 4) orelse
            return error.InvalidFormat;
        if (sign == '-') {
            if (value == 0) return error.InvalidFormat;
            value *= -1;
        }
        break :blk value;
    };
    if (parser.consume() != ' ') return error.InvalidFormat;
    const hour = parser.consumeDigits(Hour, 2) orelse return error.InvalidFormat;
    if (hour > 24) return error.InvalidFormat;
    if (parser.consume() != ':') return error.InvalidFormat;
    const minute = parser.consumeDigits(Minute, 2) orelse return error.InvalidFormat;
    if (minute > 59) return error.InvalidFormat;
    if (parser.consume() != ':') return error.InvalidFormat;
    const second = parser.consumeDigits(Second, 2) orelse return error.InvalidFormat;
    if (second > 59) return error.InvalidFormat;
    if (parser.consume() != ' ') return error.InvalidFormat;
    const gmt = parser.consumeSlice(3) orelse return error.InvalidFormat;
    if (!std.mem.eql(u8, gmt, "GMT")) return error.InvalidFormat;
    const offset_ms = blk: {
        if (is_utc_string) break :blk 0;
        switch (parser.consume() orelse return error.InvalidFormat) {
            '+', '-' => |sign| {
                const offset_hour = parser.consumeDigits(Hour, 2) orelse return error.InvalidFormat;
                if (offset_hour > 23) return error.InvalidFormat;
                const offset_minute = parser.consumeDigits(Minute, 2) orelse return error.InvalidFormat;
                if (offset_minute > 59) return error.InvalidFormat;
                var value =
                    @as(f64, @floatFromInt(offset_hour)) * std.time.ms_per_hour +
                    @as(f64, @floatFromInt(offset_minute)) * std.time.ms_per_min;
                // Offset sign is negated
                if (sign == '+') value *= -1;
                break :blk value;
            },
            else => return error.InvalidFormat,
        }
    };
    // Time zone in parenthesis is optional
    if (!is_utc_string and parser.peek() == ' ') {
        _ = parser.consume() orelse unreachable;
        if (parser.consume() != '(') return error.InvalidFormat;
        if (parser.consume() == ')') return error.InvalidFormat;
        while (parser.consume()) |c| {
            if (c == ')') break;
        }
    }
    // Did we reach the end of the string?
    if (parser.peek() != null) return error.InvalidFormat;
    const time_value = makeDate(
        makeDay(
            @floatFromInt(year),
            @floatFromInt(month - 1),
            @floatFromInt(date),
        ),
        makeTime(
            @floatFromInt(hour),
            @floatFromInt(minute),
            @floatFromInt(second),
            0,
        ),
    );
    return timeClip(time_value + offset_ms);
}

/// 21.4.1.33.1 IsTimeZoneOffsetString ( offsetString )
/// https://tc39.es/ecma262/#sec-istimezoneoffsetstring
pub fn isTimeZoneOffsetString(time_zone: temporal_rs.c.TimeZone) bool {
    return !time_zone.is_iana_id;
}

/// 21.4.1.33.2 ParseTimeZoneOffsetString ( offsetString )
/// https://tc39.es/ecma262/#sec-parsetimezoneoffsetstring
pub fn parseTimeZoneOffsetString(time_zone: temporal_rs.c.TimeZone) i64 {
    std.debug.assert(!time_zone.is_iana_id);
    return @as(i64, @intCast(time_zone.offset_minutes)) * std.time.ns_per_min;
}

pub fn fmtTimeString(time_value: f64) std.fmt.Alt(f64, formatTimeString) {
    return .{ .data = time_value };
}

/// 21.4.4.41.1 TimeString ( tv )
/// https://tc39.es/ecma262/#sec-timestring
fn formatTimeString(time_value: f64, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    // 1. Let hour be ToZeroPaddedDecimalString(ℝ(HourFromTime(tv)), 2).
    // 2. Let minute be ToZeroPaddedDecimalString(ℝ(MinFromTime(tv)), 2).
    // 3. Let second be ToZeroPaddedDecimalString(ℝ(SecFromTime(tv)), 2).
    // 4. Return the string-concatenation of hour, ":", minute, ":", second, the code unit 0x0020
    //    (SPACE), and "GMT".
    try writer.print("{d:0>2}:{d:0>2}:{d:0>2} GMT", .{
        hourFromTime(time_value),
        minFromTime(time_value),
        secFromTime(time_value),
    });
}

pub fn fmtDateString(time_value: f64) std.fmt.Alt(f64, formatDateString) {
    return .{ .data = time_value };
}

/// 21.4.4.41.2 DateString ( tv )
/// https://tc39.es/ecma262/#sec-datestring
fn formatDateString(time_value: f64, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    // 1. Let weekday be the Name of the entry in Table 61 with the Number WeekDay(tv).
    const weekday = week_day_names[weekDay(time_value)];

    // 2. Let month be the Name of the entry in Table 62 with the Number MonthFromTime(tv).
    const month = month_names[monthFromTime(time_value)];

    // 3. Let day be ToZeroPaddedDecimalString(ℝ(DateFromTime(tv)), 2).
    const day_ = dateFromTime(time_value);

    // 4. Let yv be YearFromTime(tv).
    const year = yearFromTime(time_value);

    // 5. If yv is +0𝔽 or yv > +0𝔽, let yearSign be the empty String; else let yearSign be "-".
    const year_sign = if (year >= 0) "" else "-";

    // 6. Let paddedYear be ToZeroPaddedDecimalString(abs(ℝ(yv)), 4).
    var buf: [6]u8 = undefined;
    const padded_year = toZeroPaddedDecimalString(&buf, @abs(year), 4);

    // 7. Return the string-concatenation of weekday, the code unit 0x0020 (SPACE), month, the code
    //    unit 0x0020 (SPACE), day, the code unit 0x0020 (SPACE), yearSign, and paddedYear.
    try writer.print("{s} {s} {d:0>2} {s}{s}", .{ weekday, month, day_, year_sign, padded_year });
}

pub fn fmtTimeZoneString(
    platform: *const Agent.Platform,
    time_value: f64,
) std.fmt.Alt(FormatTimeZoneStringData, formatTimeZoneString) {
    return .{ .data = .{ .platform = platform, .time_value = time_value } };
}

const FormatTimeZoneStringData = struct {
    platform: *const Agent.Platform,
    time_value: f64,
};

/// 21.4.4.41.3 TimeZoneString ( tv )
/// https://tc39.es/ecma262/#sec-timezoneestring
pub fn formatTimeZoneString(data: FormatTimeZoneStringData, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const platform = data.platform;
    const time_value = data.time_value;

    // 1. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    const time_zone = systemTimeZoneIdentifier(platform);

    // 2. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    const offset_ns = if (@TypeOf(time_zone) == void) blk: {
        break :blk 0;
    } else if (isTimeZoneOffsetString(time_zone)) blk: {
        // a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
        break :blk parseTimeZoneOffsetString(time_zone);
    } else blk: {
        // 3. Else,
        // a. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier,
        //    ℤ(ℝ(tv) × nsPerMillisecond)).
        break :blk getNamedTimeZoneOffsetNanoseconds(time_zone, @intFromFloat(time_value * std.time.ns_per_ms));
    };

    // 4. Let offset be 𝔽(truncate(offsetNs / nsPerMillisecond)).
    const offset = @trunc(@as(f64, @floatFromInt(offset_ns)) / std.time.ns_per_ms);

    // 5. If offset is +0𝔽 or offset > +0𝔽, then
    //     a. Let offsetSign be "+".
    //     b. Let absOffset be offset.
    // 6. Else,
    //     a. Let offsetSign be "-".
    //     b. Let absOffset be -offset.
    const offset_sign: u8 = if (offset >= 0) '+' else '-';
    const abs_offset = @abs(offset);

    // 7. Let offsetMin be ToZeroPaddedDecimalString(ℝ(MinFromTime(absOffset)), 2).
    const offset_min = minFromTime(abs_offset);

    // 8. Let offsetHour be ToZeroPaddedDecimalString(ℝ(HourFromTime(absOffset)), 2).
    const offset_hour = hourFromTime(abs_offset);

    // 9. Let tzName be an implementation-defined string that is either the empty String or the
    //    string-concatenation of the code unit 0x0020 (SPACE), the code unit 0x0028 (LEFT
    //    PARENTHESIS), an implementation-defined timezone name, and the code unit 0x0029 (RIGHT
    //    PARENTHESIS).
    const tz_name = if (offset == 0) " (GMT)" else "";

    // 10. Return the string-concatenation of offsetSign, offsetHour, offsetMin, and tzName.
    try writer.print("{c}{d:0>2}{d:0>2}{s}", .{ offset_sign, offset_hour, offset_min, tz_name });
}

pub fn fmtToDateString(
    platform: *const Agent.Platform,
    time_value: f64,
) std.fmt.Alt(FormatToDateStringData, formatToDateString) {
    return .{ .data = .{ .platform = platform, .time_value = time_value } };
}

const FormatToDateStringData = struct {
    platform: *const Agent.Platform,
    time_value: f64,
};

/// 21.4.4.41.4 ToDateString ( tv )
/// https://tc39.es/ecma262/#sec-todatestring
fn formatToDateString(data: FormatToDateStringData, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const platform = data.platform;
    const time_value = data.time_value;

    // 1. If tv is NaN, return "Invalid Date".
    if (std.math.isNan(time_value)) {
        try writer.writeAll("Invalid Date");
        return;
    }

    // 2. Let localTime be LocalTime(tv).
    const local_time = localTime(platform, time_value);

    // 3. Return the string-concatenation of DateString(localTime), the code unit 0x0020 (SPACE),
    //    TimeString(localTime), and TimeZoneString(tv).
    try writer.print("{f} {f}{f}", .{
        fmtDateString(local_time),
        fmtTimeString(local_time),
        fmtTimeZoneString(platform, time_value),
    });
}

/// 21.4.3 Properties of the Date Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-date-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            7,
            "Date",
            .{ .realm = realm, .proto = try realm.intrinsic(.function_prototype) },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "now", now, 0, realm);
        try object.defineBuiltinFunction(agent, "parse", parse, 1, realm);
        try object.defineBuiltinFunction(agent, "UTC", UTC, 7, realm);

        // 21.4.3.3 Date.prototype
        // https://tc39.es/ecma262/#sec-date.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsic(.date_prototype)),
            .none,
        );
    }

    /// 21.4.2.1 Date ( ...values )
    /// https://tc39.es/ecma262/#sec-date
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const io = agent.io;

        // 1. If NewTarget is undefined, then
        if (new_target == null) {
            // a. Let now be the time value (UTC) identifying the current time.
            const timestamp: std.Io.Timestamp = .now(io, .real);
            const now_: f64 = @floatFromInt(timestamp.toMilliseconds());

            // b. Return ToDateString(now).
            return Value.from(try String.fromAscii(agent, try std.fmt.allocPrint(
                agent.gc_allocator,
                "{f}",
                .{fmtToDateString(agent.platform, now_)},
            )));
        }

        // 2. Let numberOfArgs be the number of elements in values.
        const number_of_args = arguments.count();

        // 3. If numberOfArgs = 0, then
        const date_value = if (number_of_args == 0) blk: {
            // a. Let dv be the time value (UTC) identifying the current time.
            const timestamp: std.Io.Timestamp = .now(io, .real);
            break :blk @as(f64, @floatFromInt(timestamp.toMilliseconds()));
        } else if (number_of_args == 1) blk: {
            // 4. Else if numberOfArgs = 1, then
            // a. Let value be values[0].
            const value = arguments.get(0);

            // b. If value is an Object and value has a [[DateValue]] internal slot, then
            const time_value = if (value.castObject(Date)) |date| blk_tv: {
                // i. Let tv be value.[[DateValue]].
                break :blk_tv date.fields.date_value;
            } else blk_tv: {
                // c. Else,
                // i. Let v be ? ToPrimitive(value).
                const primitive_value = try value.toPrimitive(agent, null);

                // ii. If v is a String, then
                if (primitive_value.isString()) {
                    // 1. Assert: The next step never returns an abrupt completion because v is a
                    //    String.
                    // 2. Let tv be the result of parsing v as a date, in exactly the same manner as
                    //    for the `parse` method (21.4.3.2).
                    break :blk_tv parseImpl(primitive_value.asString());
                } else {
                    // iii. Else,
                    // 1. Let tv be ? ToNumber(v).
                    break :blk_tv (try primitive_value.toNumber(agent)).asFloat();
                }
            };

            // d. Let dv be TimeClip(tv).
            break :blk timeClip(time_value);
        } else blk: {
            // 5. Else,
            // a. Assert: numberOfArgs ≥ 2.
            std.debug.assert(number_of_args >= 2);

            // b. Let y be ? ToNumber(values[0]).
            var year = (try arguments.get(0).toNumber(agent)).asFloat();

            // c. Let m be ? ToNumber(values[1]).
            const month = (try arguments.get(1).toNumber(agent)).asFloat();

            // d. If numberOfArgs > 2, let dt be ? ToNumber(values[2]); else let dt be 1𝔽.
            const date = if (number_of_args > 2) (try arguments.get(2).toNumber(agent)).asFloat() else 1;

            // e. If numberOfArgs > 3, let h be ? ToNumber(values[3]); else let h be +0𝔽.
            const hour = if (number_of_args > 3) (try arguments.get(3).toNumber(agent)).asFloat() else 0;

            // f. If numberOfArgs > 4, let min be ? ToNumber(values[4]); else let min be +0𝔽.
            const minute = if (number_of_args > 4) (try arguments.get(4).toNumber(agent)).asFloat() else 0;

            // g. If numberOfArgs > 5, let s be ? ToNumber(values[5]); else let s be +0𝔽.
            const second = if (number_of_args > 5) (try arguments.get(5).toNumber(agent)).asFloat() else 0;

            // h. If numberOfArgs > 6, let milli be ? ToNumber(values[6]); else let milli be +0𝔽.
            const millisecond = if (number_of_args > 6) (try arguments.get(6).toNumber(agent)).asFloat() else 0;

            // i. Let yr be MakeFullYear(y).
            year = makeFullYear(year);

            // j. Let finalDate be MakeDate(MakeDay(yr, m, dt), MakeTime(h, min, s, milli)).
            const final_date = makeDate(
                makeDay(year, month, date),
                makeTime(hour, minute, second, millisecond),
            );

            // k. Let dv be TimeClip(UTC(finalDate)).
            break :blk timeClip(utc(agent.platform, final_date));
        };

        // 6. Let obj be ? OrdinaryCreateFromConstructor(NewTarget, "%Date.prototype%",
        //    « [[DateValue]] »).
        const date = try ordinaryCreateFromConstructor(
            Date,
            agent,
            new_target.?,
            .date_prototype,
            .{
                // 7. Set obj.[[DateValue]] to dv.
                .date_value = date_value,
            },
        );

        // 8. Return obj.
        return Value.from(&date.object);
    }

    /// 21.4.3.1 Date.now ( )
    /// https://tc39.es/ecma262/#sec-date.now
    fn now(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // This function returns the time value designating the UTC date and time of the occurrence
        // of the call to it.
        const io = agent.io;
        const timestamp: std.Io.Timestamp = .now(io, .real);
        return Value.from(@as(f64, @floatFromInt(timestamp.toMilliseconds())));
    }

    /// 21.4.3.2 Date.parse ( string )
    /// https://tc39.es/ecma262/#sec-date.parse
    fn parse(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const string = try arguments.get(0).toString(agent);
        return Value.from(parseImpl(string));
    }

    fn parseImpl(string: *const String) f64 {
        const ascii = switch (string.asAsciiOrUtf16()) {
            .ascii => |ascii| ascii,
            .utf16 => return std.math.nan(f64),
        };
        return parseDateTimeString(ascii) catch
            parseOtherString(ascii) catch
            std.math.nan(f64);
    }

    /// 21.4.3.4 Date.UTC ( year [ , month [ , date [ , hours [ , minutes [ , seconds [ , ms ] ] ] ] ] ] )
    /// https://tc39.es/ecma262/#sec-date.utc
    fn UTC(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let y be ? ToNumber(year).
        var year = (try arguments.get(0).toNumber(agent)).asFloat();

        // 2. If month is present, let m be ? ToNumber(month); else let m be +0𝔽.
        const month = if (arguments.getOrNull(1)) |month| (try month.toNumber(agent)).asFloat() else 0;

        // 3. If date is present, let dt be ? ToNumber(date); else let dt be 1𝔽.
        const date = if (arguments.getOrNull(2)) |date| (try date.toNumber(agent)).asFloat() else 1;

        // 4. If hours is present, let h be ? ToNumber(hours); else let h be +0𝔽.
        const hour = if (arguments.getOrNull(3)) |hours| (try hours.toNumber(agent)).asFloat() else 0;

        // 5. If minutes is present, let min be ? ToNumber(minutes); else let min be +0𝔽.
        const minute = if (arguments.getOrNull(4)) |minutes| (try minutes.toNumber(agent)).asFloat() else 0;

        // 6. If seconds is present, let s be ? ToNumber(seconds); else let s be +0𝔽.
        const second = if (arguments.getOrNull(5)) |seconds| (try seconds.toNumber(agent)).asFloat() else 0;

        // 7. If ms is present, let milli be ? ToNumber(ms); else let milli be +0𝔽.
        const millisecond = if (arguments.getOrNull(6)) |ms| (try ms.toNumber(agent)).asFloat() else 0;

        // 8. Let yr be MakeFullYear(y).
        year = makeFullYear(year);

        // 9. Return TimeClip(MakeDate(MakeDay(yr, m, dt), MakeTime(h, min, s, milli))).
        return Value.from(timeClip(makeDate(
            makeDay(year, month, date),
            makeTime(hour, minute, second, millisecond),
        )));
    }
};

/// 21.4.4 Properties of the Date Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-date-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsic(.object_prototype));
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "getDate", getDate, 0, realm);
        try object.defineBuiltinFunction(agent, "getDay", getDay, 0, realm);
        try object.defineBuiltinFunction(agent, "getFullYear", getFullYear, 0, realm);
        try object.defineBuiltinFunction(agent, "getHours", getHours, 0, realm);
        try object.defineBuiltinFunction(agent, "getMilliseconds", getMilliseconds, 0, realm);
        try object.defineBuiltinFunction(agent, "getMinutes", getMinutes, 0, realm);
        try object.defineBuiltinFunction(agent, "getMonth", getMonth, 0, realm);
        try object.defineBuiltinFunction(agent, "getSeconds", getSeconds, 0, realm);
        try object.defineBuiltinFunction(agent, "getTime", getTime, 0, realm);
        try object.defineBuiltinFunction(agent, "getTimezoneOffset", getTimezoneOffset, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCDate", getUTCDate, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCDay", getUTCDay, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCFullYear", getUTCFullYear, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCHours", getUTCHours, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCMilliseconds", getUTCMilliseconds, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCMinutes", getUTCMinutes, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCMonth", getUTCMonth, 0, realm);
        try object.defineBuiltinFunction(agent, "getUTCSeconds", getUTCSeconds, 0, realm);
        try object.defineBuiltinFunction(agent, "setDate", setDate, 1, realm);
        try object.defineBuiltinFunction(agent, "setFullYear", setFullYear, 3, realm);
        try object.defineBuiltinFunction(agent, "setHours", setHours, 4, realm);
        try object.defineBuiltinFunction(agent, "setMilliseconds", setMilliseconds, 1, realm);
        try object.defineBuiltinFunction(agent, "setMinutes", setMinutes, 3, realm);
        try object.defineBuiltinFunction(agent, "setMonth", setMonth, 2, realm);
        try object.defineBuiltinFunction(agent, "setSeconds", setSeconds, 2, realm);
        try object.defineBuiltinFunction(agent, "setTime", setTime, 1, realm);
        try object.defineBuiltinFunction(agent, "setUTCDate", setDate, 1, realm);
        try object.defineBuiltinFunction(agent, "setUTCFullYear", setFullYear, 3, realm);
        try object.defineBuiltinFunction(agent, "setUTCHours", setHours, 4, realm);
        try object.defineBuiltinFunction(agent, "setUTCMilliseconds", setMilliseconds, 1, realm);
        try object.defineBuiltinFunction(agent, "setUTCMinutes", setMinutes, 3, realm);
        try object.defineBuiltinFunction(agent, "setUTCMonth", setMonth, 2, realm);
        try object.defineBuiltinFunction(agent, "setUTCSeconds", setSeconds, 2, realm);
        try object.defineBuiltinFunction(agent, "toDateString", toDateString_, 0, realm);
        try object.defineBuiltinFunction(agent, "toISOString", toISOString, 0, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 1, realm);
        try object.defineBuiltinFunction(agent, "toLocaleDateString", toLocaleDateString, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleTimeString", toLocaleTimeString, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "toTimeString", toTimeString, 0, realm);
        try object.defineBuiltinFunction(agent, "toUTCString", toUTCString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);
        try object.defineBuiltinFunctionWithAttributes(
            agent,
            "Symbol.toPrimitive",
            @"Symbol.toPrimitive",
            1,
            realm,
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 21.4.4.1 Date.prototype.constructor
        // https://tc39.es/ecma262/#sec-date.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsic(.date)),
        );

        if (build_options.enable_annex_b) {
            try object.defineBuiltinFunction(agent, "getYear", getYear, 0, realm);
            try object.defineBuiltinFunction(agent, "setYear", setYear, 1, realm);

            // B.2.3.3 Date.prototype.toGMTString ( )
            // https://tc39.es/ecma262/#sec-date.prototype.togmtstring
            const date_prototype_to_utc_string = object.getPropertyValueDirect(PropertyKey.from("toUTCString"));
            try object.defineBuiltinProperty(agent, "toGMTString", date_prototype_to_utc_string);
        }

        if (build_options.enable_temporal) {
            try object.defineBuiltinFunction(agent, "toTemporalInstant", toTemporalInstant, 0, realm);
        }
    }

    /// 21.4.4.2 Date.prototype.getDate ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getdate
    fn getDate(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return DateFromTime(LocalTime(tv)).
        return Value.from(dateFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.3 Date.prototype.getDay ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getday
    fn getDay(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return WeekDay(LocalTime(tv)).
        return Value.from(weekDay(localTime(agent.platform, tv)));
    }

    /// 21.4.4.4 Date.prototype.getFullYear ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getfullyear
    fn getFullYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return YearFromTime(LocalTime(tv)).
        return Value.from(yearFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.5 Date.prototype.getHours ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.gethours
    fn getHours(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return HourFromTime(LocalTime(tv)).
        return Value.from(hourFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.6 Date.prototype.getMilliseconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getmilliseconds
    fn getMilliseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return msFromTime(LocalTime(tv)).
        return Value.from(msFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.7 Date.prototype.getMinutes ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getminutes
    fn getMinutes(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return MinFromTime(LocalTime(tv)).
        return Value.from(minFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.8 Date.prototype.getMonth ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getmonth
    fn getMonth(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return MonthFromTime(LocalTime(tv)).
        return Value.from(monthFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.9 Date.prototype.getSeconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getseconds
    fn getSeconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return SecFromTime(LocalTime(tv)).
        return Value.from(secFromTime(localTime(agent.platform, tv)));
    }

    /// 21.4.4.10 Date.prototype.getTime ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.gettime
    fn getTime(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Return dateObj.[[DateValue]].
        return Value.from(date_object.fields.date_value);
    }

    /// 21.4.4.11 Date.prototype.getTimezoneOffset ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.gettimezoneoffset
    fn getTimezoneOffset(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return (tv - LocalTime(tv)) / msPerMinute.
        return Value.from((tv - localTime(agent.platform, tv)) / std.time.ms_per_min);
    }

    /// 21.4.4.12 Date.prototype.getUTCDate ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcdate
    fn getUTCDate(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return DateFromTime(tv).
        return Value.from(dateFromTime(tv));
    }

    /// 21.4.4.13 Date.prototype.getUTCDay ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcday
    fn getUTCDay(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return WeekDay(tv).
        return Value.from(weekDay(tv));
    }

    /// 21.4.4.14 Date.prototype.getUTCFullYear ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcfullyear
    fn getUTCFullYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return YearFromTime(tv).
        return Value.from(yearFromTime(tv));
    }

    /// 21.4.4.15 Date.prototype.getUTCHours ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutchours
    fn getUTCHours(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return HourFromTime(tv).
        return Value.from(hourFromTime(tv));
    }

    /// 21.4.4.16 Date.prototype.getUTCMilliseconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcmilliseconds
    fn getUTCMilliseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return msFromTime(tv).
        return Value.from(msFromTime(tv));
    }

    /// 21.4.4.17 Date.prototype.getUTCMinutes ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcminutes
    fn getUTCMinutes(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return MinFromTime(tv).
        return Value.from(minFromTime(tv));
    }

    /// 21.4.4.18 Date.prototype.getUTCMonth ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcmonth
    fn getUTCMonth(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return MonthFromTime(tv).
        return Value.from(monthFromTime(tv));
    }

    /// 21.4.4.19 Date.prototype.getUTCSeconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcseconds
    fn getUTCSeconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return SecFromTime(tv).
        return Value.from(secFromTime(tv));
    }

    /// 21.4.4.20 Date.prototype.setDate ( date )
    /// https://tc39.es/ecma262/#sec-date.prototype.setdate
    fn setDate(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const date_value = arguments.get(0);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Let dt be ? ToNumber(date).
        const date = (try date_value.toNumber(agent)).asFloat();

        // 5. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 6. Set tv to LocalTime(tv).
        tv = localTime(agent.platform, tv);

        // 7. Let newDate be MakeDate(MakeDay(YearFromTime(tv), MonthFromTime(tv), dt),
        //    TimeWithinDay(tv)).
        const new_date = makeDate(
            makeDay(
                @floatFromInt(yearFromTime(tv)),
                @floatFromInt(monthFromTime(tv)),
                date,
            ),
            timeWithinDay(tv),
        );

        // 8. Let u be TimeClip(UTC(newDate)).
        const date_value_utc = timeClip(utc(agent.platform, new_date));

        // 9. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 10. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.21 Date.prototype.setFullYear ( year [ , month [ , date ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setfullyear
    fn setFullYear(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const year_value = arguments.get(0);
        const month_value = arguments.getOrNull(1);
        const date_value = arguments.getOrNull(2);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Let y be ? ToNumber(year).
        const year = (try year_value.toNumber(agent)).asFloat();

        // 5. If tv is NaN, set tv to +0𝔽; else set tv to LocalTime(tv).
        tv = if (std.math.isNan(tv)) 0 else localTime(agent.platform, tv);

        // 6. If month is present, let m be ? ToNumber(month); else let m be MonthFromTime(tv).
        const month = if (month_value) |month|
            (try month.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(monthFromTime(tv)));

        // 7. If date is present, let dt be ? ToNumber(date); else let dt be DateFromTime(tv).
        const date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(dateFromTime(tv)));

        // 8. Let newDate be MakeDate(MakeDay(y, m, dt), TimeWithinDay(tv)).
        const new_date = makeDate(makeDay(year, month, date), timeWithinDay(tv));

        // 9. Let u be TimeClip(UTC(newDate)).
        const date_value_utc = timeClip(utc(agent.platform, new_date));

        // 10. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 11. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.22 Date.prototype.setHours ( hour [ , min [ , sec [ , ms ] ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.sethours
    fn setHours(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const hour_value = arguments.get(0);
        const minute_value = arguments.getOrNull(1);
        const second_value = arguments.getOrNull(2);
        const millisecond_value = arguments.getOrNull(3);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Let h be ? ToNumber(hour).
        const hour = (try hour_value.toNumber(agent)).asFloat();

        // 5. If min is present, let m be ? ToNumber(min).
        var minute = if (minute_value) |minute|
            (try minute.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If sec is present, let s be ? ToNumber(sec).
        var second = if (second_value) |second|
            (try second.toNumber(agent)).asFloat()
        else
            undefined;

        // 7. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 8. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 9. Set tv to LocalTime(tv).
        tv = localTime(agent.platform, tv);

        // 10. If min is not present, let m be MinFromTime(tv).
        if (minute_value == null) minute = @floatFromInt(minFromTime(tv));

        // 11. If sec is not present, let s be SecFromTime(tv).
        if (second_value == null) second = @floatFromInt(secFromTime(tv));

        // 12. If ms is not present, let milli be msFromTime(tv).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(tv));

        // 13. Let date be MakeDate(Day(tv), MakeTime(h, m, s, milli)).
        const date = makeDate(day(tv), makeTime(hour, minute, second, millisecond));

        // 14. Let u be TimeClip(UTC(date)).
        const date_value_utc = timeClip(utc(agent.platform, date));

        // 15. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 16. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.23 Date.prototype.setMilliseconds ( ms )
    /// https://tc39.es/ecma262/#sec-date.prototype.setmilliseconds
    fn setMilliseconds(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const millisecond_value = arguments.get(0);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Set ms to ? ToNumber(ms).
        const millisecond = (try millisecond_value.toNumber(agent)).asFloat();

        // 5. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 6. Set tv to LocalTime(tv).
        tv = localTime(agent.platform, tv);

        // 7. Let time be MakeTime(HourFromTime(tv), MinFromTime(tv), SecFromTime(tv), ms).
        const time = makeTime(
            @floatFromInt(hourFromTime(tv)),
            @floatFromInt(minFromTime(tv)),
            @floatFromInt(secFromTime(tv)),
            millisecond,
        );

        // 8. Let u be TimeClip(UTC(MakeDate(Day(tv), time))).
        const date_value_utc = timeClip(utc(agent.platform, makeDate(day(tv), time)));

        // 9. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 10. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.24 Date.prototype.setMinutes ( min [ , sec [ , ms ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setminutes
    fn setMinutes(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const minute_value = arguments.get(0);
        const second_value = arguments.getOrNull(1);
        const millisecond_value = arguments.getOrNull(2);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Let m be ? ToNumber(min).
        const minute = (try minute_value.toNumber(agent)).asFloat();

        // 5. If sec is present, let s be ? ToNumber(sec).
        var second = if (second_value) |second|
            (try second.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 7. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 8. Set tv to LocalTime(tv).
        tv = localTime(agent.platform, tv);

        // 9. If sec is not present, let s be SecFromTime(tv).
        if (second_value == null) second = @floatFromInt(secFromTime(tv));

        // 10. If ms is not present, let milli be msFromTime(tv).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(tv));

        // 11. Let date be MakeDate(Day(tv), MakeTime(HourFromTime(tv), m, s, milli)).
        const date = makeDate(
            day(tv),
            makeTime(@floatFromInt(hourFromTime(tv)), minute, second, millisecond),
        );

        // 12. Let u be TimeClip(UTC(date)).
        const date_value_utc = timeClip(utc(agent.platform, date));

        // 13. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 14. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.25 Date.prototype.setMonth ( month [ , date ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setmonth
    fn setMonth(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const month_value = arguments.get(0);
        const date_value = arguments.getOrNull(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Let m be ? ToNumber(month).
        const month = (try month_value.toNumber(agent)).asFloat();

        // 5. If date is present, let dt be ? ToNumber(date).
        var date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 7. Set tv to LocalTime(tv).
        tv = localTime(agent.platform, tv);

        // 8. If date is not present, let dt be DateFromTime(tv).
        if (date_value == null) date = @floatFromInt(dateFromTime(tv));

        // 9. Let newDate be MakeDate(MakeDay(YearFromTime(tv), m, dt), TimeWithinDay(tv)).
        const new_date = makeDate(
            makeDay(@floatFromInt(yearFromTime(tv)), month, date),
            timeWithinDay(tv),
        );

        // 10. Let u be TimeClip(UTC(newDate)).
        const date_value_utc = timeClip(utc(agent.platform, new_date));

        // 11. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 12. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.26 Date.prototype.setSeconds ( sec [ , ms ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setseconds
    fn setSeconds(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const second_value = arguments.get(0);
        const millisecond_value = arguments.getOrNull(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. Let s be ? ToNumber(sec).
        const second = (try second_value.toNumber(agent)).asFloat();

        // 5. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 7. Set tv to LocalTime(tv).
        tv = localTime(agent.platform, tv);

        // 8. If ms is not present, let milli be msFromTime(tv).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(tv));

        // 9. Let date be MakeDate(Day(tv), MakeTime(HourFromTime(tv), MinFromTime(tv), s, milli)).
        const date = makeDate(
            day(tv),
            makeTime(
                @floatFromInt(hourFromTime(tv)),
                @floatFromInt(minFromTime(tv)),
                second,
                millisecond,
            ),
        );

        // 10. Let u be TimeClip(UTC(date)).
        const date_value_utc = timeClip(utc(agent.platform, date));

        // 11. Set dateObj.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 12. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.27 Date.prototype.setTime ( time )
    /// https://tc39.es/ecma262/#sec-date.prototype.settime
    fn setTime(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const time = arguments.get(0);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be ? ToNumber(time).
        const time_value = (try time.toNumber(agent)).asFloat();

        // 4. Let v be TimeClip(t).
        const date_value = timeClip(time_value);

        // 5. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 6. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.28 Date.prototype.setUTCDate ( date )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcdate
    fn setUTCDate(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const date_value = arguments.get(0);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. Let dt be ? ToNumber(date).
        const date = (try date_value.toNumber(agent)).asFloat();

        // 5. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 6. Let newDate be MakeDate(MakeDay(YearFromTime(tv), MonthFromTime(tv), dt),
        //    TimeWithinDay(tv)).
        const new_date = makeDate(
            makeDay(
                @floatFromInt(yearFromTime(tv)),
                @floatFromInt(monthFromTime(tv)),
                date,
            ),
            timeWithinDay(tv),
        );

        // 7. Let v be TimeClip(newDate).
        const date_value_ = timeClip(new_date);

        // 8. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value_;

        // 9. Return v.
        return Value.from(date_value_);
    }

    /// 21.4.4.29 Date.prototype.setUTCFullYear ( year [ , month [ , date ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcfullyear
    fn setUTCFullYear(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const year_value = arguments.get(0);
        const month_value = arguments.getOrNull(1);
        const date_value = arguments.getOrNull(2);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        var tv = date_object.fields.date_value;

        // 4. If tv is NaN, set tv to +0𝔽.
        if (std.math.isNan(tv)) tv = 0;

        // 5. Let y be ? ToNumber(year).
        const year = (try year_value.toNumber(agent)).asFloat();

        // 6. If month is present, let m be ? ToNumber(month); else let m be MonthFromTime(tv).
        const month = if (month_value) |month|
            (try month.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(monthFromTime(tv)));

        // 7. If date is present, let dt be ? ToNumber(date); else let dt be DateFromTime(tv).
        const date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(dateFromTime(tv)));

        // 8. Let newDate be MakeDate(MakeDay(y, m, dt), TimeWithinDay(tv)).
        const new_date = makeDate(makeDay(year, month, date), timeWithinDay(tv));

        // 9. Let v be TimeClip(newDate).
        const date_value_ = timeClip(new_date);

        // 10. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value_;

        // 11. Return v.
        return Value.from(date_value_);
    }

    /// 21.4.4.30 Date.prototype.setUTCHours ( hour [ , min [ , sec [ , ms ] ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutchours
    fn setUTCHours(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const hour_value = arguments.get(0);
        const minute_value = arguments.getOrNull(1);
        const second_value = arguments.getOrNull(2);
        const millisecond_value = arguments.getOrNull(3);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. Let h be ? ToNumber(hour).
        const hour = (try hour_value.toNumber(agent)).asFloat();

        // 5. If min is present, let m be ? ToNumber(min).
        var minute = if (minute_value) |minute|
            (try minute.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If sec is present, let s be ? ToNumber(sec).
        var second = if (second_value) |second|
            (try second.toNumber(agent)).asFloat()
        else
            undefined;

        // 7. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 8. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 9. If min is not present, let m be MinFromTime(tv).
        if (minute_value == null) minute = @floatFromInt(minFromTime(tv));

        // 10. If sec is not present, let s be SecFromTime(tv).
        if (second_value == null) second = @floatFromInt(secFromTime(tv));

        // 11. If ms is not present, let milli be msFromTime(tv).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(tv));

        // 12. Let date be MakeDate(Day(tv), MakeTime(h, m, s, milli)).
        const date = makeDate(day(tv), makeTime(hour, minute, second, millisecond));

        // 13. Let v be TimeClip(date).
        const date_value = timeClip(date);

        // 14. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 15. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.31 Date.prototype.setUTCMilliseconds ( ms )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcmilliseconds
    fn setUTCMilliseconds(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        const millisecond_value = arguments.get(0);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. Set ms to ? ToNumber(ms).
        const millisecond = (try millisecond_value.toNumber(agent)).asFloat();

        // 5. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 6. Let time be MakeTime(HourFromTime(tv), MinFromTime(tv), SecFromTime(tv), ms).
        const time = makeTime(
            @floatFromInt(hourFromTime(tv)),
            @floatFromInt(minFromTime(tv)),
            @floatFromInt(secFromTime(tv)),
            millisecond,
        );

        // 7. Let v be TimeClip(MakeDate(Day(tv), time)).
        const date_value = timeClip(makeDate(day(tv), time));

        // 8. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 9. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.32 Date.prototype.setUTCMinutes ( min [ , sec [ , ms ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcminutes
    fn setUTCMinutes(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const minute_value = arguments.get(0);
        const second_value = arguments.getOrNull(1);
        const millisecond_value = arguments.getOrNull(2);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. Let m be ? ToNumber(min).
        const minute = (try minute_value.toNumber(agent)).asFloat();

        // 5. If sec is present, let s be ? ToNumber(sec).
        var second = if (second_value) |second|
            (try second.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 7. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 8. If sec is not present, let s be SecFromTime(tv).
        if (second_value == null) second = @floatFromInt(secFromTime(tv));

        // 9. If ms is not present, let milli be msFromTime(tv).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(tv));

        // 10. Let date be MakeDate(Day(tv), MakeTime(HourFromTime(tv), m, s, milli)).
        const date = makeDate(
            day(tv),
            makeTime(@floatFromInt(hourFromTime(tv)), minute, second, millisecond),
        );

        // 11. Let v be TimeClip(date).
        const date_value = timeClip(date);

        // 12. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 13. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.33 Date.prototype.setUTCMonth ( month [ , date ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcmonth
    fn setUTCMonth(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const month_value = arguments.get(0);
        const date_value = arguments.getOrNull(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. Let m be ? ToNumber(month).
        const month = (try month_value.toNumber(agent)).asFloat();

        // 5. If date is present, let dt be ? ToNumber(date).
        var date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 7. If date is not present, let dt be DateFromTime(tv).
        if (date_value == null) date = @floatFromInt(dateFromTime(tv));

        // 8. Let newDate be MakeDate(MakeDay(YearFromTime(tv), m, dt), TimeWithinDay(tv)).
        const new_date = makeDate(
            makeDay(@floatFromInt(yearFromTime(tv)), month, date),
            timeWithinDay(tv),
        );

        // 9. Let v be TimeClip(newDate).
        const date_value_ = timeClip(new_date);

        // 10. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value_;

        // 11. Return v.
        return Value.from(date_value_);
    }

    /// 21.4.4.34 Date.prototype.setUTCSeconds ( sec [ , ms ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcseconds
    fn setUTCSeconds(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const second_value = arguments.get(0);
        const millisecond_value = arguments.getOrNull(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. Let s be ? ToNumber(sec).
        const second = (try second_value.toNumber(agent)).asFloat();

        // 5. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 7. If ms is not present, let milli be msFromTime(tv).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(tv));

        // 8. Let date be MakeDate(Day(tv), MakeTime(HourFromTime(tv), MinFromTime(tv), s, milli)).
        const date = makeDate(
            day(tv),
            makeTime(
                @floatFromInt(hourFromTime(tv)),
                @floatFromInt(minFromTime(tv)),
                second,
                millisecond,
            ),
        );

        // 9. Let v be TimeClip(date).
        const date_value = timeClip(date);

        // 10. Set dateObj.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 11. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.35 Date.prototype.toDateString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.todatestring
    fn toDateString_(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return "Invalid Date".
        if (std.math.isNan(tv)) return Value.from("Invalid Date");

        // 5. Let t be LocalTime(tv).
        const t = localTime(agent.platform, tv);

        // 6. Return DateString(t).
        return Value.from(try String.fromAscii(agent, try std.fmt.allocPrint(
            agent.gc_allocator,
            "{f}",
            .{fmtDateString(t)},
        )));
    }

    /// 21.4.4.36 Date.prototype.toISOString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.toisostring
    fn toISOString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, throw a RangeError exception.
        if (!std.math.isFinite(tv)) {
            return agent.throwException(.range_error, "Invalid Date object", .{});
        }

        // 5. Assert: tv is an integral Number.
        // 6. If tv corresponds with a year that cannot be represented in the Date Time String
        //    Format, throw a RangeError exception.
        // 7. Return a String representation of tv in the Date Time String Format on the UTC time
        //    scale, including all format elements and the UTC offset representation "Z".
        const year = yearFromTime(tv);
        const year_sign = if (year >= 0 and year <= 9999) "" else if (year > 9999) "+" else "-";

        var buf: [6]u8 = undefined;
        const padded_year = toZeroPaddedDecimalString(
            &buf,
            @abs(year),
            if (year >= 0 and year <= 9999) 4 else 6,
        );

        return Value.from(try String.fromAscii(agent, try std.fmt.allocPrint(
            agent.gc_allocator,
            "{s}{s}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}Z",
            .{
                year_sign,
                padded_year,
                monthFromTime(tv) + 1,
                dateFromTime(tv),
                hourFromTime(tv),
                minFromTime(tv),
                secFromTime(tv),
                msFromTime(tv),
            },
        )));
    }

    /// 21.4.4.37 Date.prototype.toJSON ( key )
    /// https://tc39.es/ecma262/#sec-date.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // NOTE: The argument is ignored.

        // 1. Let obj be ? ToObject(this value).
        const obj = try this_value.toObject(agent);

        // 2. Let tv be ? ToPrimitive(obj, number).
        const tv = try Value.from(obj).toPrimitive(agent, .number);

        // 3. If tv is a Number and tv is not finite, return null.
        if (tv.isNumber() and !tv.asNumber().isFinite()) return .null;

        // 4. Return ? Invoke(obj, "toISOString").
        return obj.invoke(agent, PropertyKey.from("toISOString"), &.{});
    }

    /// 21.4.4.38 Date.prototype.toLocaleDateString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.tolocaledatestring
    fn toLocaleDateString(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        if (build_options.enable_intl) {
            return toLocaleDateStringIntl(agent, this_value, arguments);
        }
        return toDateString_(agent, this_value, arguments);
    }

    /// 20.4.2 Date.prototype.toLocaleDateString ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sup-date.prototype.tolocaledatestring
    fn toLocaleDateStringIntl(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        const realm = agent.currentRealm();
        const locales = arguments.get(0);
        const options = arguments.get(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_obj = try this_value.requireInternalSlot(agent, Date);

        // 3. Let x be dateObj.[[DateValue]].
        const time_value = date_obj.fields.date_value;

        // 4. If x is NaN, return "Invalid Date".
        if (std.math.isNan(time_value)) return Value.from("Invalid Date");

        // 5. Let dateFormat be ? CreateDateTimeFormat(%Intl.DateTimeFormat%, locales, options,
        //    date, date).
        const date_format = try builtins.intl.date_time_format.createDateTimeFormat(
            agent,
            try realm.intrinsic(.intl_date_time_format),
            locales,
            options,
            .date,
            .date,
        );

        // 6. Return ! FormatDateTime(dateFormat, x).
        return builtins.intl.date_time_format.formatDateTime(agent, date_format, time_value);
    }

    /// 21.4.4.39 Date.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.tolocalestring
    fn toLocaleString(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        if (build_options.enable_intl) {
            return toLocaleStringIntl(agent, this_value, arguments);
        }
        return toString(agent, this_value, arguments);
    }

    /// 20.4.1 Date.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sup-date.prototype.tolocalestring
    fn toLocaleStringIntl(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        const realm = agent.currentRealm();
        const locales = arguments.get(0);
        const options = arguments.get(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_obj = try this_value.requireInternalSlot(agent, Date);

        // 3. Let x be dateObj.[[DateValue]].
        const time_value = date_obj.fields.date_value;

        // 4. If x is NaN, return "Invalid Date".
        if (std.math.isNan(time_value)) return Value.from("Invalid Date");

        // 5. Let dateFormat be ? CreateDateTimeFormat(%Intl.DateTimeFormat%, locales, options, any,
        //    all).
        const date_format = try builtins.intl.date_time_format.createDateTimeFormat(
            agent,
            try realm.intrinsic(.intl_date_time_format),
            locales,
            options,
            .any,
            .all,
        );

        // 6. Return ! FormatDateTime(dateFormat, x).
        return builtins.intl.date_time_format.formatDateTime(agent, date_format, time_value);
    }

    /// 21.4.4.40 Date.prototype.toLocaleTimeString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.tolocaletimestring
    fn toLocaleTimeString(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        if (build_options.enable_intl) {
            return toLocaleTimeStringIntl(agent, this_value, arguments);
        }
        return toTimeString(agent, this_value, arguments);
    }

    /// 20.4.3 Date.prototype.toLocaleTimeString ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sup-date.prototype.tolocaletimestring
    fn toLocaleTimeStringIntl(
        agent: *Agent,
        this_value: Value,
        arguments: Arguments,
    ) Agent.Error!Value {
        const realm = agent.currentRealm();
        const locales = arguments.get(0);
        const options = arguments.get(1);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_obj = try this_value.requireInternalSlot(agent, Date);

        // 3. Let x be dateObj.[[DateValue]].
        const time_value = date_obj.fields.date_value;

        // 4. If x is NaN, return "Invalid Date".
        if (std.math.isNan(time_value)) return Value.from("Invalid Date");

        // 5. Let timeFormat be ? CreateDateTimeFormat(%Intl.DateTimeFormat%, locales, options,
        //    time, time).
        const time_format = try builtins.intl.date_time_format.createDateTimeFormat(
            agent,
            try realm.intrinsic(.intl_date_time_format),
            locales,
            options,
            .time,
            .time,
        );

        // 6. Return ! FormatDateTime(timeFormat, x).
        return builtins.intl.date_time_format.formatDateTime(agent, time_format, time_value);
    }

    /// 21.4.4.41 Date.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. Return ToDateString(tv).
        return Value.from(try String.fromAscii(agent, try std.fmt.allocPrint(
            agent.gc_allocator,
            "{f}",
            .{fmtToDateString(agent.platform, time_value)},
        )));
    }

    /// 14.9.1 Date.prototype.toTemporalInstant ( )
    /// https://tc39.es/proposal-temporal/#sec-date.prototype.totemporalinstant
    fn toTemporalInstant(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. Let ns be ? NumberToBigInt(t) × ℤ(10**6).
        if (std.math.isNan(time_value)) {
            return agent.throwException(
                .range_error,
                "Cannot convert invalid date to Temporal.Instant",
                .{},
            );
        }
        std.debug.assert(@trunc(time_value) == time_value);
        const ns = temporal_rs.toI128Nanoseconds(@as(i128, @intFromFloat(time_value)) * 1_000_000);

        // 5. Return ! CreateTemporalInstant(ns).
        const temporal_rs_instant = try builtins.temporal.extractResult(
            agent,
            temporal_rs.c.temporal_rs_Instant_try_new(ns),
        );
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        const instant = createTemporalInstant(
            agent,
            temporal_rs_instant.?,
            null,
        ) catch |err| try noexcept(err);
        return Value.from(&instant.object);
    }

    /// 21.4.4.42 Date.prototype.toTimeString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.totimestring
    fn toTimeString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return "Invalid Date".
        if (std.math.isNan(tv)) return Value.from("Invalid Date");

        // 5. Let localTime be LocalTime(tv).
        const local_time = localTime(agent.platform, tv);

        // 6. Return the string-concatenation of TimeString(localTime) and TimeZoneString(tv).
        return Value.from(try String.fromAscii(agent, try std.fmt.allocPrint(
            agent.gc_allocator,
            "{f}{f}",
            .{
                fmtTimeString(local_time),
                fmtTimeZoneString(agent.platform, tv),
            },
        )));
    }

    /// 21.4.4.43 Date.prototype.toUTCString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.toutcstring
    fn toUTCString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return "Invalid Date".
        if (std.math.isNan(tv)) return Value.from("Invalid Date");

        // 5. Let weekday be the Name of the entry in Table 61 with the Number WeekDay(tv).
        const weekday = week_day_names[weekDay(tv)];

        // 6. Let month be the Name of the entry in Table 62 with the Number MonthFromTime(tv).
        const month = month_names[monthFromTime(tv)];

        // 7. Let day be ToZeroPaddedDecimalString(ℝ(DateFromTime(tv)), 2).
        const day_ = dateFromTime(tv);

        // 8. Let yv be YearFromTime(tv).
        const year = yearFromTime(tv);

        // 9. If yv is +0𝔽 or yv > +0𝔽, let yearSign be the empty String; else let yearSign be "-".
        const year_sign = if (year >= 0) "" else "-";

        // 10. Let paddedYear be ToZeroPaddedDecimalString(abs(ℝ(yv)), 4).
        var buf: [6]u8 = undefined;
        const padded_year = toZeroPaddedDecimalString(&buf, @abs(year), 4);

        // 11. Return the string-concatenation of weekday, ",", the code unit 0x0020 (SPACE), day,
        //     the code unit 0x0020 (SPACE), month, the code unit 0x0020 (SPACE), yearSign,
        //     paddedYear, the code unit 0x0020 (SPACE), and TimeString(tv).
        return Value.from(try String.fromAscii(agent, try std.fmt.allocPrint(
            agent.gc_allocator,
            "{s}, {d:0>2} {s} {s}{s} {f}",
            .{
                weekday,
                day_,
                month,
                year_sign,
                padded_year,
                fmtTimeString(tv),
            },
        )));
    }

    /// 21.4.4.44 Date.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Return dateObj.[[DateValue]].
        return Value.from(date_object.fields.date_value);
    }

    /// 21.4.4.45 Date.prototype [ %Symbol.toPrimitive% ] ( hint )
    /// https://tc39.es/ecma262/#sec-date.prototype-%symbol.toprimitive%
    fn @"Symbol.toPrimitive"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const hint_value = arguments.get(0);

        // 1. Let obj be the this value.
        // 2. If obj is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const obj = this_value.asObject();

        if (!hint_value.isString()) {
            return agent.throwException(.type_error, "{f} is not a string", .{hint_value});
        }
        const hint = hint_value.asString();

        // 3. If hint is either "string" or "default", then
        const try_first: PreferredType = if (hint.eql(String.fromLiteral("string")) or hint.eql(String.fromLiteral("default"))) blk: {
            // a. Let tryFirst be string.
            break :blk .string;
        } else if (hint.eql(String.fromLiteral("number"))) blk: {
            // 4. Else if hint is "number", then
            // a. Let tryFirst be number.
            break :blk .number;
        } else {
            // 5. Else,
            // a. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Hint must be one of 'string', 'number', or 'default'",
                .{},
            );
        };

        // 6. Return ? OrdinaryToPrimitive(obj, tryFirst).
        return obj.ordinaryToPrimitive(agent, try_first);
    }

    /// B.2.3.1 Date.prototype.getYear ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getyear
    fn getYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObj.[[DateValue]].
        const tv = date_object.fields.date_value;

        // 4. If tv is NaN, return NaN.
        if (std.math.isNan(tv)) return .nan;

        // 5. Return YearFromTime(LocalTime(tv)) - 1900𝔽.
        return Value.from(yearFromTime(localTime(agent.platform, tv)) - 1900);
    }

    /// B.2.3.2 Date.prototype.setYear ( year )
    /// https://tc39.es/ecma262/#sec-date.prototype.setyear
    fn setYear(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const year_value = arguments.get(0);

        // 1. Let dateObj be the this value.
        // 2. Perform ? RequireInternalSlot(dateObj, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let time be dateObj.[[DateValue]].
        var time = date_object.fields.date_value;

        // 4. Let year be ? ToNumber(year).
        const year = try year_value.toNumber(agent);

        // 5. If time is NaN, set time to +0𝔽; else set time to LocalTime(time).
        time = if (std.math.isNan(time)) 0 else localTime(agent.platform, time);

        // 6. Let fullYear be MakeFullYear(year).
        const full_year = makeFullYear(year.asFloat());

        // 7. Let day be MakeDay(fullYear, MonthFromTime(time), DateFromTime(time)).
        const day_ = makeDay(
            full_year,
            @floatFromInt(monthFromTime(time)),
            @floatFromInt(dateFromTime(time)),
        );

        // 8. Let date be MakeDate(day, TimeWithinDay(time)).
        const date = makeDate(day_, timeWithinDay(time));

        // 9. Let utcTimestamp be TimeClip(UTC(date)).
        const utc_timestamp = timeClip(utc(agent.platform, date));

        // 10. Set dateObj.[[DateValue]] to utcTimestamp.
        date_object.fields.date_value = utc_timestamp;

        // 11. Return utcTimestamp.
        return Value.from(utc_timestamp);
    }
};

/// 21.4.5 Properties of Date Instances
/// https://tc39.es/ecma262/#sec-properties-of-date-instances
pub const Date = MakeObject(.{
    .Fields = struct {
        /// [[DateValue]]
        date_value: f64,
    },
    .tag = .date,
    .display_name = "Date",
});

test parseDateTimeString {
    const test_cases = [_]struct { []const u8, f64 }{
        .{ "1970", 0 },
        .{ "1970-01", 0 },
        .{ "1970-01-01", 0 },
        .{ "+001970-01-01", 0 },
        .{ "1970-01-01T01:00", 3_600_000 },
        .{ "1970-01-01T01:00:00", 3_600_000 },
        .{ "1970-01-01T01:00:00.123", 3_600_123 },
        .{ "1970-01-01T01:00:00+00:00", 3_600_000 },
        .{ "1970-01-01T01:00:00Z", 3_600_000 },
        .{ "1970-01-01T01:00:00+05:00", -14_400_000 },
        .{ "1970-01-01T01:00:00-05:00", 21_600_000 },
        .{ "2025-03-15T12:34:56Z", 1742042096000 },
    };
    for (test_cases) |test_case| {
        const value, const expected = test_case;
        try std.testing.expectEqual(expected, parseDateTimeString(value));
    }
    inline for (.{
        "",
        "abc",
        "1970-13-01",
        "1970-01-99",
        "1970-01-01T99:00:00",
        "1970-01-01T01:99:00",
        "1970-01-01T01:00:99",
        "1970-01-01T01:00:00+99:00",
        "1970-01-01T01:00:00+00:99",
        "1970-01-01T01:00:00+00:00x",
    }) |value| {
        try std.testing.expectError(error.InvalidFormat, parseDateTimeString(value));
    }
}

test parseOtherString {
    const test_cases = [_]struct { []const u8, f64 }{
        // toString()
        .{ "Thu Jan 01 1970 00:00:00 GMT+0000", 0 },
        .{ "Thu Jan 01 1970 00:00:00 GMT+0100", -3_600_000 },
        .{ "Thu Jan 01 1970 00:00:00 GMT-0100", 3_600_000 },
        .{ "Sat Mar 15 2025 12:34:56 GMT+0000 (Greenwich Mean Time)", 1742042096000 },

        // toUTCString()
        .{ "Thu, 01 Jan 1970 00:00:00 GMT", 0 },
        .{ "Sat, 15 Mar 2025 12:34:56 GMT", 1742042096000 },
    };
    for (test_cases) |test_case| {
        const value, const expected = test_case;
        try std.testing.expectEqual(expected, parseOtherString(value));
    }
    inline for (.{
        "",
        "abc",
        "Abc Jan 01 1970 00:00:00 GMT+0000",
        "Thu Abc 01 1970 00:00:00 GMT+0000",
        "Thu Jan 99 1970 00:00:00 GMT+0000",
        "Thu Jan 01 1970 99:99:99 GMT+0000",
        "Thu Jan 01 1970 00:00:00 GMT+0000 ()",
        "Thu Jan 01 1970 00:00:00 GMT+9999",
        "Thu 01 Jan 1970 00:00:00 GMT",
    }) |value| {
        try std.testing.expectError(error.InvalidFormat, parseOtherString(value));
    }
}
