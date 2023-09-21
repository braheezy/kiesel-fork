//! 21.4 Date Objects
//! https://tc39.es/ecma262/#sec-date-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PreferredType = Value.PreferredType;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinFunctionWithAttributes = utils.defineBuiltinFunctionWithAttributes;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

const hours_per_day = 24;
const minutes_per_hour = 60;

/// Table 62: Names of days of the week
/// https://tc39.es/ecma262/#sec-todatestring-day-names
const week_day_names = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

/// Table 63: Names of months of the year
/// https://tc39.es/ecma262/#sec-todatestring-month-names
const month_names = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

const Year = std.math.IntFittingRange(1970 - 273_790, 1970 + 273_790);
const Month = std.math.IntFittingRange(0, 366);
const Day = std.math.IntFittingRange(0, 366);
const Hour = std.math.IntFittingRange(0, 23);
const Minute = std.math.IntFittingRange(0, 59);
const Second = std.math.IntFittingRange(0, 59);
const Millisecond = std.math.IntFittingRange(0, 999);
const WeekDay = std.math.IntFittingRange(0, 6);

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
    const s = std.fmt.bufPrint(&tmp, "{}", .{std.math.absInt(n) catch unreachable}) catch unreachable;

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

/// 21.4.1.3 Day ( t )
/// https://tc39.es/ecma262/#sec-day
pub fn day(t: f64) f64 {
    // 1. Return 𝔽(floor(ℝ(t / msPerDay))).
    return std.math.floor(t / std.time.ms_per_day);
}

/// 21.4.1.4 TimeWithinDay ( t )
/// https://tc39.es/ecma262/#sec-timewithinday
pub fn timeWithinDay(t: f64) f64 {
    // 1. Return 𝔽(ℝ(t) modulo ℝ(msPerDay)).
    return @mod(t, std.time.ms_per_day);
}

/// 21.4.1.5 DaysInYear ( y )
/// https://tc39.es/ecma262/#sec-daysinyear
pub fn daysInYear(year: Year) Day {
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
    // 2. NOTE: In the following steps, each _numYearsN_ is the number of years divisible by N
    //    that occur between the epoch and the start of year y. (The number is negative if y is
    //    before the epoch.)

    // 3. Let numYears1 be (ry - 1970).
    const num_years_1: f64 = @floatFromInt(year - 1970);

    // 4. Let numYears4 be floor((ry - 1969) / 4).
    const num_years_4: f64 = @floatFromInt(@divFloor(year - 1969, 4));

    // 5. Let numYears100 be floor((ry - 1901) / 100).
    const num_years_100: f64 = @floatFromInt(@divFloor(year - 1901, 100));

    // 6. Let numYears400 be floor((ry - 1601) / 400).
    const num_years_400: f64 = @floatFromInt(@divFloor(year - 1601, 400));

    // 7. Return 𝔽(365 × numYears1 + numYears4 - numYears100 + numYears400).
    return 365 * num_years_1 + num_years_4 - num_years_100 + num_years_400;
}

/// 21.4.1.7 TimeFromYear ( y )
/// https://tc39.es/ecma262/#sec-timefromyear
pub fn timeFromYear(year: Year) f64 {
    // 1. Return msPerDay × DayFromYear(y).
    return std.time.ms_per_day * dayFromYear(year);
}

/// 21.4.1.8 YearFromTime ( t )
/// https://tc39.es/ecma262/#sec-yearfromtime
pub fn yearFromTime(t: f64) Year {
    // 1. Return the largest integral Number y (closest to +∞) such that TimeFromYear(y) ≤ t.
    const year: Year = @intFromFloat(t / (365.2425 * std.time.ms_per_day) + 1970);
    const t2 = timeFromYear(year);
    if (t2 > t) return year - 1;
    if (t2 + @as(f64, @floatFromInt(daysInYear(year))) * std.time.ms_per_day <= t) return year + 1;
    return year;
}

/// 21.4.1.9 DayWithinYear ( t )
/// https://tc39.es/ecma262/#sec-daywithinyear
pub fn dayWithinYear(t: f64) Day {
    const d = day(t) - dayFromYear(yearFromTime(t));
    // FIXME: This should not be necessary, but for `new Date(-1111, 0, 0)` this underflows to -1 -
    //        possible spec issue?
    if (d < 0) return @intFromFloat(365 + d);
    return @intFromFloat(d);
}

/// 21.4.1.10 InLeapYear ( t )
/// https://tc39.es/ecma262/#sec-inleapyear
pub fn inLeapYear(t: f64) bool {
    // 1. If DaysInYear(YearFromTime(t)) is 366𝔽, return 1𝔽; else return +0𝔽.
    return daysInYear(yearFromTime(t)) == 366;
}

/// 21.4.1.11 MonthFromTime ( t )
/// https://tc39.es/ecma262/#sec-monthfromtime
pub fn monthFromTime(t: f64) Month {
    // 1. Let inLeapYear be InLeapYear(t).
    const in_leap_year: Day = @intFromBool(inLeapYear(t));

    // 2. Let dayWithinYear be DayWithinYear(t).
    const day_within_year = dayWithinYear(t);

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

/// 21.4.1.12 DateFromTime ( t )
/// https://tc39.es/ecma262/#sec-datefromtime
pub fn dateFromTime(t: f64) Day {
    // 1. Let inLeapYear be InLeapYear(t).
    const in_leap_year: Day = @intFromBool(inLeapYear(t));

    // 2. Let dayWithinYear be DayWithinYear(t).
    const day_within_year = dayWithinYear(t);

    // 3. Let month be MonthFromTime(t).
    const month = monthFromTime(t);

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

/// 21.4.1.13 WeekDay ( t )
/// https://tc39.es/ecma262/#sec-weekday
pub fn weekDay(t: f64) WeekDay {
    // 1. Return 𝔽(ℝ(Day(t) + 4𝔽) modulo 7).
    return @intFromFloat(@mod(day(t) + 4, 7));
}

/// 21.4.1.14 HourFromTime ( t )
/// https://tc39.es/ecma262/#sec-hourfromtime
pub fn hourFromTime(t: f64) Hour {
    // 1. Return 𝔽(floor(ℝ(t / msPerHour)) modulo HoursPerDay).
    return @intFromFloat(@mod(std.math.floor(t / std.time.ms_per_hour), hours_per_day));
}

/// 21.4.1.15 MinFromTime ( t )
/// https://tc39.es/ecma262/#sec-minfromtime
pub fn minFromTime(t: f64) Minute {
    // 1. Return 𝔽(floor(ℝ(t / msPerMinute)) modulo MinutesPerHour).
    return @intFromFloat(@mod(std.math.floor(t / std.time.ms_per_min), minutes_per_hour));
}

/// 21.4.1.16 SecFromTime ( t )
/// https://tc39.es/ecma262/#sec-secfromtime
pub fn secFromTime(t: f64) Second {
    // 1. Return 𝔽(floor(ℝ(t / msPerSecond)) modulo SecondsPerMinute).
    return @intFromFloat(@mod(std.math.floor(t / std.time.ms_per_s), std.time.s_per_min));
}

/// 21.4.1.17 msFromTime ( t )
/// https://tc39.es/ecma262/#sec-msfromtime
pub fn msFromTime(t: f64) Millisecond {
    // 1. Return 𝔽(ℝ(t) modulo ℝ(msPerSecond)).
    return @intFromFloat(@mod(t, std.time.ms_per_s));
}

/// 21.4.1.21 GetNamedTimeZoneOffsetNanoseconds ( timeZoneIdentifier, epochNanoseconds )
/// https://tc39.es/ecma262/#sec-getnamedtimezoneoffsetnanoseconds
pub fn getNamedTimeZoneOffsetNanoseconds(time_zone_identifier: []const u8, _: f64) i32 {
    // 1. Assert: timeZoneIdentifier is "UTC".
    std.debug.assert(std.mem.eql(u8, time_zone_identifier, "UTC"));

    // 2. Return 0.
    return 0;
}

/// 21.4.1.24 SystemTimeZoneIdentifier ( )
/// https://tc39.es/ecma262/#sec-systemtimezoneidentifier
pub fn systemTimeZoneIdentifier() []const u8 {
    // 1. If the implementation only supports the UTC time zone, return "UTC".
    // 2. Let systemTimeZoneString be the String representing the host environment's current time
    //    zone, either a primary time zone identifier or an offset time zone identifier.
    // 3. Return systemTimeZoneString.
    return "UTC";
}

/// 21.4.1.25 LocalTime ( t )
/// https://tc39.es/ecma262/#sec-localtime
pub fn localTime(t: f64) f64 {
    // 1. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    const system_time_zone_identifier = systemTimeZoneIdentifier();

    // 2. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    const offset_ns = if (false) {
        // a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
        unreachable;
    }
    // 3. Else,
    else blk: {
        // a. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier, ℤ(ℝ(t) × 10**6)).
        break :blk getNamedTimeZoneOffsetNanoseconds(system_time_zone_identifier, t * 10e6);
    };

    // 4. Let offsetMs be truncate(offsetNs / 10**6).
    const offset_ms = @trunc(@as(f64, @floatFromInt(offset_ns)) / 10e6);

    // 5. Return t + 𝔽(offsetMs).
    return t + offset_ms;
}

/// 21.4.1.26 UTC ( t )
/// https://tc39.es/ecma262/#sec-utc-t
pub fn utc(t: f64) f64 {
    // 1. If t is not finite, return NaN.
    if (!std.math.isFinite(t)) return std.math.nan(f64);

    // 2. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    const system_time_zone_identifier = systemTimeZoneIdentifier();

    // TODO: 3-4
    _ = system_time_zone_identifier;
    const offset_ns: i32 = 0;

    // 5. Let offsetMs be truncate(offsetNs / 10**6).
    const offset_ms = @trunc(@as(f64, @floatFromInt(offset_ns)) / 10e6);

    // 6. Return t - 𝔽(offsetMs).
    return t - offset_ms;
}

/// 21.4.1.27 MakeTime ( hour, min, sec, ms )
/// https://tc39.es/ecma262/#sec-maketime
pub fn makeTime(hour: f64, minute: f64, second: f64, millisecond: f64) f64 {
    // 1. If hour is not finite, min is not finite, sec is not finite, or ms is not finite, return NaN.
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

    // 8. Find a finite time value t such that YearFromTime(t) is ym, MonthFromTime(t) is mn, and
    //    DateFromTime(t) is 1𝔽; but if this is not possible (because some argument is out of
    //    range), return NaN.
    if (ym < std.math.minInt(i64) or ym > std.math.maxInt(i64) or (mn + 1) > std.math.maxInt(i32)) {
        return std.math.nan(f64);
    }
    const t = @as(f64, @floatFromInt(
        daysFromCivil(
            @intFromFloat(ym),
            @intFromFloat(mn + 1),
            1,
        ),
    )) * std.time.ms_per_day;

    // 9. Return Day(t) + dt - 1𝔽.
    return day(t) + dt - 1;
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
    // 1. If year is NaN, return NaN.
    if (std.math.isNan(year)) return std.math.nan(f64);

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
    if (std.math.fabs(time) > 8.64e15) return std.math.nan(f64);

    // 3. Return 𝔽(! ToIntegerOrInfinity(time)).
    return toIntegerOrInfinity(time);
}

/// 21.4.4.41.1 TimeString ( tv )
/// https://tc39.es/ecma262/#sec-timestring
pub fn timeString(allocator: Allocator, time_value: f64) ![]const u8 {
    // 1. Let hour be ToZeroPaddedDecimalString(ℝ(HourFromTime(tv)), 2).
    // 2. Let minute be ToZeroPaddedDecimalString(ℝ(MinFromTime(tv)), 2).
    // 3. Let second be ToZeroPaddedDecimalString(ℝ(SecFromTime(tv)), 2).
    // 4. Return the string-concatenation of hour, ":", minute, ":", second, the code unit 0x0020
    //    (SPACE), and "GMT".
    return std.fmt.allocPrint(
        allocator,
        "{:0>2}:{:0>2}:{:0>2} GMT",
        .{ hourFromTime(time_value), minFromTime(time_value), secFromTime(time_value) },
    );
}

/// 21.4.4.41.2 DateString ( tv )
/// https://tc39.es/ecma262/#sec-datestring
pub fn dateString(allocator: Allocator, time_value: f64) ![]const u8 {
    // 1. Let weekday be the Name of the entry in Table 62 with the Number WeekDay(tv).
    const weekday = week_day_names[weekDay(time_value)];

    // 2. Let month be the Name of the entry in Table 63 with the Number MonthFromTime(tv).
    const month = month_names[monthFromTime(time_value)];

    // 3. Let day be ToZeroPaddedDecimalString(ℝ(DateFromTime(tv)), 2).
    const day_ = dateFromTime(time_value);

    // 4. Let yv be YearFromTime(tv).
    const year = yearFromTime(time_value);

    // 5. If yv is +0𝔽 or yv > +0𝔽, let yearSign be the empty String; otherwise, let yearSign be "-".
    const year_sign = if (year >= 0) "" else "-";

    // 6. Let paddedYear be ToZeroPaddedDecimalString(abs(ℝ(yv)), 4).
    var buf: [6]u8 = undefined;
    const padded_year = toZeroPaddedDecimalString(&buf, std.math.absInt(year) catch unreachable, 4);

    // 7. Return the string-concatenation of weekday, the code unit 0x0020 (SPACE), month, the code
    //    unit 0x0020 (SPACE), day, the code unit 0x0020 (SPACE), yearSign, and paddedYear.
    return std.fmt.allocPrint(
        allocator,
        "{s} {s} {:0>2} {s}{s}",
        .{ weekday, month, day_, year_sign, padded_year },
    );
}

/// 21.4.4.41.3 TimeZoneString ( tv )
/// https://tc39.es/ecma262/#sec-timezoneestring
pub fn timeZoneString(allocator: Allocator, time_value: f64) ![]const u8 {
    // 1. Let systemTimeZoneIdentifier be SystemTimeZoneIdentifier().
    const system_time_zone_identifier = systemTimeZoneIdentifier();

    // 2. If IsTimeZoneOffsetString(systemTimeZoneIdentifier) is true, then
    const offset_ns = if (false) {
        // a. Let offsetNs be ParseTimeZoneOffsetString(systemTimeZoneIdentifier).
        unreachable;
    }
    // 3. Else,
    else blk: {
        // a. Let offsetNs be GetNamedTimeZoneOffsetNanoseconds(systemTimeZoneIdentifier, ℤ(ℝ(tv) × 10**6)).
        break :blk getNamedTimeZoneOffsetNanoseconds(system_time_zone_identifier, time_value * 10e6);
    };

    // 4. Let offset be 𝔽(truncate(offsetNs / 10**6)).
    const offset = @trunc(@as(f64, @floatFromInt(offset_ns)) / 10e6);

    // 5. If offset is +0𝔽 or offset > +0𝔽, then
    //     a. Let offsetSign be "+".
    //     b. Let absOffset be offset.
    // 6. Else,
    //     a. Let offsetSign be "-".
    //     b. Let absOffset be -offset.
    const offset_sign = if (offset >= 0) "+" else "-";
    const abs_offset = std.math.fabs(offset);

    // 7. Let offsetMin be ToZeroPaddedDecimalString(ℝ(MinFromTime(absOffset)), 2).
    const offset_min = minFromTime(abs_offset);

    // 8. Let offsetHour be ToZeroPaddedDecimalString(ℝ(HourFromTime(absOffset)), 2).
    const offset_hour = hourFromTime(abs_offset);

    // 9. Let tzName be an implementation-defined string that is either the empty String or the
    //    string-concatenation of the code unit 0x0020 (SPACE), the code unit 0x0028 (LEFT
    //    PARENTHESIS), an implementation-defined timezone name, and the code unit 0x0029 (RIGHT
    //    PARENTHESIS).
    const tz_name = " (UTC)";

    // 10. Return the string-concatenation of offsetSign, offsetHour, offsetMin, and tzName.
    return std.fmt.allocPrint(
        allocator,
        "{s}{:0>2}{:0>2}{s}",
        .{ offset_sign, offset_hour, offset_min, tz_name },
    );
}

/// 21.4.4.41.4 ToDateString ( tv )
/// https://tc39.es/ecma262/#sec-todatestring
pub fn toDateString(allocator: Allocator, time_value: f64) ![]const u8 {
    // 1. If tv is NaN, return "Invalid Date".
    if (std.math.isNan(time_value)) return "Invalid Date";

    // 2. Let t be LocalTime(tv).
    const t = localTime(time_value);

    // 3. Return the string-concatenation of DateString(t), the code unit 0x0020 (SPACE), TimeString(t), and TimeZoneString(tv).
    return std.fmt.allocPrint(allocator, "{s} {s}{s}", .{
        try dateString(allocator, t),
        try timeString(allocator, t),
        try timeZoneString(allocator, time_value),
    });
}

/// 21.4.3 Properties of the Date Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-date-constructor
pub const DateConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 7,
            .name = "Date",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        try defineBuiltinFunction(object, "now", now, 0, realm);
        try defineBuiltinFunction(object, "UTC", UTC, 7, realm);

        // 21.4.3.3 Date.prototype
        // https://tc39.es/ecma262/#sec-date.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Date.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 21.4.4.1 Date.prototype.constructor
        // https://tc39.es/ecma262/#sec-date.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Date.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 21.4.2.1 Date ( ...values )
    /// https://tc39.es/ecma262/#sec-date
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        // 1. If NewTarget is undefined, then
        if (new_target == null) {
            // a. Let now be the time value (UTC) identifying the current time.
            const now_: f64 = @floatFromInt(std.time.milliTimestamp());

            // b. Return ToDateString(now).
            return Value.from(try toDateString(agent.gc_allocator, now_));
        }

        // 2. Let numberOfArgs be the number of elements in values.
        const number_of_args = arguments.count();

        // 3. If numberOfArgs = 0, then
        const date_value = if (number_of_args == 0) blk: {
            // a. Let dv be the time value (UTC) identifying the current time.
            break :blk @as(f64, @floatFromInt(std.time.milliTimestamp()));
        }
        // 4. Else if numberOfArgs = 1, then
        else if (number_of_args == 1) blk: {
            // a. Let value be values[0].
            const value = arguments.get(0);

            // b. If value is an Object and value has a [[DateValue]] internal slot, then
            const time_value = if (value == .object and value.object.is(Date)) blk_tv: {
                // i. Let tv be value.[[DateValue]].
                break :blk_tv value.object.as(Date).fields.date_value;
            }
            // c. Else,
            else blk_tv: {
                // i. Let v be ? ToPrimitive(value).
                const primitive_value = try value.toPrimitive(agent, null);

                // ii. If v is a String, then
                if (primitive_value == .string) {
                    // 1. Assert: The next step never returns an abrupt completion because v is a
                    //    String.
                    // TODO: 2. Let tv be the result of parsing v as a date, in exactly the same manner
                    //    as for the parse method (21.4.3.2).
                    return agent.throwException(
                        .internal_error,
                        "Date string parsing is not implemented",
                    );
                }
                // iii. Else,
                else {
                    // 1. Let tv be ? ToNumber(v).
                    break :blk_tv (try primitive_value.toNumber(agent)).asFloat();
                }
            };

            // d. Let dv be TimeClip(tv).
            break :blk timeClip(time_value);
        }
        // 5. Else,
        else blk: {
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
            break :blk timeClip(utc(final_date));
        };

        // 6. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Date.prototype%", « [[DateValue]] »).
        // 7. Set O.[[DateValue]] to dv.
        const object = try ordinaryCreateFromConstructor(
            Date,
            agent,
            new_target.?,
            "%Date.prototype%",
        );
        object.as(Date).fields = .{
            .date_value = date_value,
        };

        // 8. Return O.
        return Value.from(object);
    }

    /// 21.4.3.1 Date.now ( )
    /// https://tc39.es/ecma262/#sec-date.now
    fn now(_: *Agent, _: Value, _: ArgumentsList) !Value {
        // This function returns the time value designating the UTC date and time of the occurrence
        // of the call to it.
        return Value.from(std.time.milliTimestamp());
    }

    /// 21.4.3.4 Date.UTC ( year [ , month [ , date [ , hours [ , minutes [ , seconds [ , ms ] ] ] ] ] ] )
    /// https://tc39.es/ecma262/#sec-date.utc
    fn UTC(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        // 1. Let y be ? ToNumber(year).
        var year = (try arguments.get(0).toNumber(agent)).asFloat();

        // 2. If month is present, let m be ? ToNumber(month); else let m be +0𝔽.
        const month = if (arguments.getOrNull(1)) |month| (try month.toNumber(agent)).asFloat() else 0;

        // 3. If date is present, let dt be ? ToNumber(date); else let dt be 1𝔽.
        const date = if (arguments.getOrNull(2)) |date| (try date.toNumber(agent)).asFloat() else 0;

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
pub const DatePrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "getDate", getDate, 0, realm);
        try defineBuiltinFunction(object, "getDay", getDay, 0, realm);
        try defineBuiltinFunction(object, "getFullYear", getFullYear, 0, realm);
        try defineBuiltinFunction(object, "getHours", getHours, 0, realm);
        try defineBuiltinFunction(object, "getMilliseconds", getMilliseconds, 0, realm);
        try defineBuiltinFunction(object, "getMinutes", getMinutes, 0, realm);
        try defineBuiltinFunction(object, "getMonth", getMonth, 0, realm);
        try defineBuiltinFunction(object, "getSeconds", getSeconds, 0, realm);
        try defineBuiltinFunction(object, "getTime", getTime, 0, realm);
        try defineBuiltinFunction(object, "getTimezoneOffset", getTimezoneOffset, 0, realm);
        try defineBuiltinFunction(object, "getUTCDate", getUTCDate, 0, realm);
        try defineBuiltinFunction(object, "getUTCDay", getUTCDay, 0, realm);
        try defineBuiltinFunction(object, "getUTCFullYear", getUTCFullYear, 0, realm);
        try defineBuiltinFunction(object, "getUTCHours", getUTCHours, 0, realm);
        try defineBuiltinFunction(object, "getUTCMilliseconds", getUTCMilliseconds, 0, realm);
        try defineBuiltinFunction(object, "getUTCMinutes", getUTCMinutes, 0, realm);
        try defineBuiltinFunction(object, "getUTCMonth", getUTCMonth, 0, realm);
        try defineBuiltinFunction(object, "getUTCSeconds", getUTCSeconds, 0, realm);
        try defineBuiltinFunction(object, "setDate", setDate, 1, realm);
        try defineBuiltinFunction(object, "setFullYear", setFullYear, 3, realm);
        try defineBuiltinFunction(object, "setHours", setHours, 4, realm);
        try defineBuiltinFunction(object, "setMilliseconds", setMilliseconds, 1, realm);
        try defineBuiltinFunction(object, "setMinutes", setMinutes, 3, realm);
        try defineBuiltinFunction(object, "setMonth", setMonth, 2, realm);
        try defineBuiltinFunction(object, "setSeconds", setSeconds, 2, realm);
        try defineBuiltinFunction(object, "setTime", setTime, 1, realm);
        try defineBuiltinFunction(object, "setUTCDate", setDate, 1, realm);
        try defineBuiltinFunction(object, "setUTCFullYear", setFullYear, 3, realm);
        try defineBuiltinFunction(object, "setUTCHours", setHours, 4, realm);
        try defineBuiltinFunction(object, "setUTCMilliseconds", setMilliseconds, 1, realm);
        try defineBuiltinFunction(object, "setUTCMinutes", setMinutes, 3, realm);
        try defineBuiltinFunction(object, "toDateString", toDateString_, 0, realm);
        try defineBuiltinFunction(object, "toISOString", toISOString, 0, realm);
        try defineBuiltinFunction(object, "toJSON", toJSON, 1, realm);
        try defineBuiltinFunction(object, "toLocaleDateString", toLocaleDateString, 0, realm);
        try defineBuiltinFunction(object, "toLocaleString", toLocaleString, 0, realm);
        try defineBuiltinFunction(object, "toLocaleTimeString", toLocaleTimeString, 0, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "toTimeString", toTimeString, 0, realm);
        try defineBuiltinFunction(object, "toUTCString", toUTCString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);
        try defineBuiltinFunctionWithAttributes(object, "@@toPrimitive", @"@@toPrimitive", 1, realm, .{
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 21.4.4.2 Date.prototype.getDate ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getdate
    fn getDate(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return DateFromTime(LocalTime(t)).
        return Value.from(dateFromTime(localTime(time_value)));
    }

    /// 21.4.4.3 Date.prototype.getDay ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getday
    fn getDay(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return WeekDay(LocalTime(t)).
        return Value.from(weekDay(localTime(time_value)));
    }

    /// 21.4.4.4 Date.prototype.getFullYear ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getfullyear
    fn getFullYear(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return YearFromTime(LocalTime(t)).
        return Value.from(yearFromTime(localTime(time_value)));
    }

    /// 21.4.4.5 Date.prototype.getHours ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.gethours
    fn getHours(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return HourFromTime(LocalTime(t)).
        return Value.from(hourFromTime(localTime(time_value)));
    }

    /// 21.4.4.6 Date.prototype.getMilliseconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getmilliseconds
    fn getMilliseconds(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return msFromTime(LocalTime(t)).
        return Value.from(msFromTime(localTime(time_value)));
    }

    /// 21.4.4.7 Date.prototype.getMinutes ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getminutes
    fn getMinutes(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return MinFromTime(LocalTime(t)).
        return Value.from(minFromTime(localTime(time_value)));
    }

    /// 21.4.4.8 Date.prototype.getMonth ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getmonth
    fn getMonth(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return MonthFromTime(LocalTime(t)).
        return Value.from(monthFromTime(localTime(time_value)));
    }

    /// 21.4.4.9 Date.prototype.getSeconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getseconds
    fn getSeconds(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return SecFromTime(LocalTime(t)).
        return Value.from(secFromTime(localTime(time_value)));
    }

    /// 21.4.4.10 Date.prototype.getTime ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.gettime
    fn getTime(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Return dateObject.[[DateValue]].
        return Value.from(date_object.fields.date_value);
    }

    /// 21.4.4.11 Date.prototype.getTimezoneOffset ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.gettimezoneoffset
    fn getTimezoneOffset(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return (t - LocalTime(t)) / msPerMinute.
        return Value.from((time_value - localTime(time_value)) / std.time.ms_per_min);
    }

    /// 21.4.4.12 Date.prototype.getUTCDate ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcdate
    fn getUTCDate(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return DateFromTime(t).
        return Value.from(dateFromTime(time_value));
    }

    /// 21.4.4.13 Date.prototype.getUTCDay ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcday
    fn getUTCDay(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return WeekDay(t).
        return Value.from(weekDay(time_value));
    }

    /// 21.4.4.14 Date.prototype.getUTCFullYear ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcfullyear
    fn getUTCFullYear(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return YearFromTime(t).
        return Value.from(yearFromTime(time_value));
    }

    /// 21.4.4.15 Date.prototype.getUTCHours ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutchours
    fn getUTCHours(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return HourFromTime(t).
        return Value.from(hourFromTime(time_value));
    }

    /// 21.4.4.16 Date.prototype.getUTCMilliseconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcmilliseconds
    fn getUTCMilliseconds(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return msFromTime(t).
        return Value.from(msFromTime(time_value));
    }

    /// 21.4.4.17 Date.prototype.getUTCMinutes ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcminutes
    fn getUTCMinutes(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return MinFromTime(t).
        return Value.from(minFromTime(time_value));
    }

    /// 21.4.4.18 Date.prototype.getUTCMonth ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcmonth
    fn getUTCMonth(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return MonthFromTime(t).
        return Value.from(monthFromTime(time_value));
    }

    /// 21.4.4.19 Date.prototype.getUTCSeconds ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.getutcseconds
    fn getUTCSeconds(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 5. Return SecFromTime(t).
        return Value.from(secFromTime(time_value));
    }

    /// 21.4.4.20 Date.prototype.setDate ( date )
    /// https://tc39.es/ecma262/#sec-date.prototype.setdate
    fn setDate(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const date_value = arguments.get(0);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

        // 4. Let dt be ? ToNumber(date).
        const date = (try date_value.toNumber(agent)).asFloat();

        // 5. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 6. Set t to LocalTime(t).
        time_value = localTime(time_value);

        // 7. Let newDate be MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), dt), TimeWithinDay(t)).
        const new_date = makeDate(
            makeDay(
                @floatFromInt(yearFromTime(time_value)),
                @floatFromInt(monthFromTime(time_value)),
                date,
            ),
            timeWithinDay(time_value),
        );

        // 8. Let u be TimeClip(UTC(newDate)).
        const date_value_utc = timeClip(utc(new_date));

        // 9. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 10. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.21 Date.prototype.setFullYear ( year [ , month [ , date ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setfullyear
    fn setFullYear(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const year_value = arguments.get(0);
        const month_value = arguments.getOrNull(1);
        const date_value = arguments.getOrNull(2);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

        // 4. Let y be ? ToNumber(year).
        const year = (try year_value.toNumber(agent)).asFloat();

        // 5. If t is NaN, set t to +0𝔽; otherwise, set t to LocalTime(t).
        time_value = if (std.math.isNan(time_value)) 0 else localTime(time_value);

        // 6. If month is not present, let m be MonthFromTime(t); otherwise, let m be ? ToNumber(month).
        const month = if (month_value) |month|
            (try month.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(monthFromTime(time_value)));

        // 7. If date is not present, let dt be DateFromTime(t); otherwise, let dt be ? ToNumber(date).
        const date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(dateFromTime(time_value)));

        // 8. Let newDate be MakeDate(MakeDay(y, m, dt), TimeWithinDay(t)).
        const new_date = makeDate(makeDay(year, month, date), timeWithinDay(time_value));

        // 9. Let u be TimeClip(UTC(newDate)).
        const date_value_utc = timeClip(utc(new_date));

        // 10. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 11. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.22 Date.prototype.setHours ( hour [ , min [ , sec [ , ms ] ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.sethours
    fn setHours(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const hour_value = arguments.get(0);
        const minute_value = arguments.getOrNull(1);
        const second_value = arguments.getOrNull(2);
        const millisecond_value = arguments.getOrNull(3);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

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

        // 8. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 9. Set t to LocalTime(t).
        time_value = localTime(time_value);

        // 10. If min is not present, let m be MinFromTime(t).
        if (minute_value == null) minute = @floatFromInt(minFromTime(time_value));

        // 11. If sec is not present, let s be SecFromTime(t).
        if (second_value == null) second = @floatFromInt(secFromTime(time_value));

        // 12. If ms is not present, let milli be msFromTime(t).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(time_value));

        // 13. Let date be MakeDate(Day(t), MakeTime(h, m, s, milli)).
        const date = makeDate(day(time_value), makeTime(hour, minute, second, millisecond));

        // 14. Let u be TimeClip(UTC(date)).
        const date_value_utc = timeClip(utc(date));

        // 15. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 16. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.23 Date.prototype.setMilliseconds ( ms )
    /// https://tc39.es/ecma262/#sec-date.prototype.setmilliseconds
    fn setMilliseconds(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const millisecond_value = arguments.get(0);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

        // 4. Set ms to ? ToNumber(ms).
        const millisecond = (try millisecond_value.toNumber(agent)).asFloat();

        // 5. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 6. Set t to LocalTime(t).
        time_value = localTime(time_value);

        // 7. Let time be MakeTime(HourFromTime(t), MinFromTime(t), SecFromTime(t), ms).
        const time = makeTime(
            @floatFromInt(hourFromTime(time_value)),
            @floatFromInt(minFromTime(time_value)),
            @floatFromInt(secFromTime(time_value)),
            millisecond,
        );

        // 8. Let u be TimeClip(UTC(MakeDate(Day(t), time))).
        const date_value_utc = timeClip(utc(makeDate(day(time_value), time)));

        // 9. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 10. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.24 Date.prototype.setMinutes ( min [ , sec [ , ms ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setminutes
    fn setMinutes(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const minute_value = arguments.get(0);
        const second_value = arguments.getOrNull(1);
        const millisecond_value = arguments.getOrNull(2);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

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

        // 7. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 8. Set t to LocalTime(t).
        time_value = localTime(time_value);

        // 9. If sec is not present, let s be SecFromTime(t).
        if (second_value == null) second = @floatFromInt(secFromTime(time_value));

        // 10. If ms is not present, let milli be msFromTime(t).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(time_value));

        // 11. Let date be MakeDate(Day(t), MakeTime(HourFromTime(t), m, s, milli)).
        const date = makeDate(
            day(time_value),
            makeTime(@floatFromInt(hourFromTime(time_value)), minute, second, millisecond),
        );

        // 12. Let u be TimeClip(UTC(date)).
        const date_value_utc = timeClip(utc(date));

        // 13. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 14. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.25 Date.prototype.setMonth ( month [ , date ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setmonth
    fn setMonth(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const month_value = arguments.get(0);
        const date_value = arguments.getOrNull(1);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

        // 4. Let m be ? ToNumber(month).
        const month = (try month_value.toNumber(agent)).asFloat();

        // 5. If date is present, let dt be ? ToNumber(date).
        var date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 7. Set t to LocalTime(t).
        time_value = localTime(time_value);

        // 8. If date is not present, let dt be DateFromTime(t).
        if (date_value == null) date = @floatFromInt(dateFromTime(time_value));

        // 9. Let newDate be MakeDate(MakeDay(YearFromTime(t), m, dt), TimeWithinDay(t)).
        const new_date = makeDate(
            makeDay(@floatFromInt(yearFromTime(time_value)), month, date),
            timeWithinDay(time_value),
        );

        // 10. Let u be TimeClip(UTC(newDate)).
        const date_value_utc = timeClip(utc(new_date));

        // 11. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 12. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.26 Date.prototype.setSeconds ( sec [ , ms ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setseconds
    fn setSeconds(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const second_value = arguments.get(0);
        const millisecond_value = arguments.getOrNull(1);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

        // 4. Let s be ? ToNumber(sec).
        const second = (try second_value.toNumber(agent)).asFloat();

        // 5. If ms is present, let milli be ? ToNumber(ms).
        var millisecond = if (millisecond_value) |millisecond|
            (try millisecond.toNumber(agent)).asFloat()
        else
            undefined;

        // 6. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 7. Set t to LocalTime(t).
        time_value = localTime(time_value);

        // 8. If ms is not present, let milli be msFromTime(t).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(time_value));

        // 9. Let date be MakeDate(Day(t), MakeTime(HourFromTime(t), MinFromTime(t), s, milli)).
        const date = makeDate(
            day(time_value),
            makeTime(
                @floatFromInt(hourFromTime(time_value)),
                @floatFromInt(minFromTime(time_value)),
                second,
                millisecond,
            ),
        );

        // 10. Let u be TimeClip(UTC(date)).
        const date_value_utc = timeClip(utc(date));

        // 11. Set dateObject.[[DateValue]] to u.
        date_object.fields.date_value = date_value_utc;

        // 12. Return u.
        return Value.from(date_value_utc);
    }

    /// 21.4.4.27 Date.prototype.setTime ( time )
    /// https://tc39.es/ecma262/#sec-date.prototype.settime
    fn setTime(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const time = arguments.get(0);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be ? ToNumber(time).
        const time_value = (try time.toNumber(agent)).asFloat();

        // 4. Let v be TimeClip(t).
        const date_value = timeClip(time_value);

        // 5. Set dateObject.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 6. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.28 Date.prototype.setUTCDate ( date )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcdate
    fn setUTCDate(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const date_value = arguments.get(0);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. Let dt be ? ToNumber(date).
        const date = (try date_value.toNumber(agent)).asFloat();

        // 5. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 6. Let newDate be MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), dt), TimeWithinDay(t)).
        const new_date = makeDate(
            makeDay(
                @floatFromInt(yearFromTime(time_value)),
                @floatFromInt(monthFromTime(time_value)),
                date,
            ),
            timeWithinDay(time_value),
        );

        // 7. Let v be TimeClip(newDate).
        const date_value_ = timeClip(new_date);

        // 8. Set dateObject.[[DateValue]] to v.
        date_object.fields.date_value = date_value_;

        // 9. Return v.
        return Value.from(date_value_);
    }

    /// 21.4.4.29 Date.prototype.setUTCFullYear ( year [ , month [ , date ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcfullyear
    fn setUTCFullYear(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const year_value = arguments.get(0);
        const month_value = arguments.getOrNull(1);
        const date_value = arguments.getOrNull(2);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        var time_value = date_object.fields.date_value;

        // 4. If t is NaN, set t to +0𝔽.
        if (std.math.isNan(time_value)) time_value = 0;

        // 5. Let y be ? ToNumber(year).
        const year = (try year_value.toNumber(agent)).asFloat();

        // 6. If month is not present, let m be MonthFromTime(t); otherwise, let m be ? ToNumber(month).
        const month = if (month_value) |month|
            (try month.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(monthFromTime(time_value)));

        // 7. If date is not present, let dt be DateFromTime(t); otherwise, let dt be ? ToNumber(date).
        const date = if (date_value) |date|
            (try date.toNumber(agent)).asFloat()
        else
            @as(f64, @floatFromInt(dateFromTime(time_value)));

        // 8. Let newDate be MakeDate(MakeDay(y, m, dt), TimeWithinDay(t)).
        const new_date = makeDate(makeDay(year, month, date), timeWithinDay(time_value));

        // 9. Let v be TimeClip(newDate).
        const date_value_ = timeClip(new_date);

        // 10. Set dateObject.[[DateValue]] to v.
        date_object.fields.date_value = date_value_;

        // 11. Return v.
        return Value.from(date_value_);
    }

    /// 21.4.4.30 Date.prototype.setUTCHours ( hour [ , min [ , sec [ , ms ] ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.sethours
    fn setUTCHours(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const hour_value = arguments.get(0);
        const minute_value = arguments.getOrNull(1);
        const second_value = arguments.getOrNull(2);
        const millisecond_value = arguments.getOrNull(3);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

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

        // 8. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 9. If min is not present, let m be MinFromTime(t).
        if (minute_value == null) minute = @floatFromInt(minFromTime(time_value));

        // 10. If sec is not present, let s be SecFromTime(t).
        if (second_value == null) second = @floatFromInt(secFromTime(time_value));

        // 11. If ms is not present, let milli be msFromTime(t).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(time_value));

        // 12. Let date be MakeDate(Day(t), MakeTime(h, m, s, milli)).
        const date = makeDate(day(time_value), makeTime(hour, minute, second, millisecond));

        // 13. Let v be TimeClip(date).
        const date_value = timeClip(date);

        // 14. Set dateObject.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 15. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.31 Date.prototype.setUTCMilliseconds ( ms )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcmilliseconds
    fn setUTCMilliseconds(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const millisecond_value = arguments.get(0);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. Set ms to ? ToNumber(ms).
        const millisecond = (try millisecond_value.toNumber(agent)).asFloat();

        // 5. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 6. Let time be MakeTime(HourFromTime(t), MinFromTime(t), SecFromTime(t), ms).
        const time = makeTime(
            @floatFromInt(hourFromTime(time_value)),
            @floatFromInt(minFromTime(time_value)),
            @floatFromInt(secFromTime(time_value)),
            millisecond,
        );

        // 7. Let v be TimeClip(MakeDate(Day(t), time)).
        const date_value = timeClip(makeDate(day(time_value), time));

        // 8. Set dateObject.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 9. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.32 Date.prototype.setUTCMinutes ( min [ , sec [ , ms ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.setutcminutes
    fn setUTCMinutes(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const minute_value = arguments.get(0);
        const second_value = arguments.getOrNull(1);
        const millisecond_value = arguments.getOrNull(2);

        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let t be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

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

        // 7. If t is NaN, return NaN.
        if (std.math.isNan(time_value)) return Value.nan();

        // 8. If sec is not present, let s be SecFromTime(t).
        if (second_value == null) second = @floatFromInt(secFromTime(time_value));

        // 9. If ms is not present, let milli be msFromTime(t).
        if (millisecond_value == null) millisecond = @floatFromInt(msFromTime(time_value));

        // 10. Let date be MakeDate(Day(t), MakeTime(HourFromTime(t), m, s, milli)).
        const date = makeDate(
            day(time_value),
            makeTime(@floatFromInt(hourFromTime(time_value)), minute, second, millisecond),
        );

        // 11. Let v be TimeClip(date).
        const date_value = timeClip(date);

        // 12. Set dateObject.[[DateValue]] to v.
        date_object.fields.date_value = date_value;

        // 13. Return v.
        return Value.from(date_value);
    }

    /// 21.4.4.35 Date.prototype.toDateString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.todatestring
    fn toDateString_(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If tv is NaN, return "Invalid Date".
        if (std.math.isNan(time_value)) return Value.from("Invalid Date");

        // 5. Let t be LocalTime(tv).
        const t = localTime(time_value);

        // 6. Return DateString(t).
        return Value.from(try dateString(agent.gc_allocator, t));
    }

    /// 21.4.4.36 Date.prototype.toISOString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.toisostring
    fn toISOString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If tv is not finite, throw a RangeError exception.
        if (!std.math.isFinite(time_value)) {
            return agent.throwException(.range_error, "Invalid Date object");
        }

        // 5. If tv corresponds with a year that cannot be represented in the Date Time String
        //    Format, throw a RangeError exception.
        // 6. Return a String representation of tv in the Date Time String Format on the UTC time
        //    scale, including all format elements and the UTC offset representation "Z".
        const year = yearFromTime(time_value);
        const year_sign = if (year >= 0 and year <= 9999) "" else if (year > 9999) "+" else "-";

        var buf: [6]u8 = undefined;
        const padded_year = toZeroPaddedDecimalString(
            &buf,
            std.math.absInt(year) catch unreachable,
            if (year >= 0 and year <= 9999) 4 else 6,
        );

        return Value.from(try std.fmt.allocPrint(
            agent.gc_allocator,
            "{s}{s}-{:0>2}-{:0>2}T{:0>2}:{:0>2}:{:0>2}.{:0>3}Z",
            .{
                year_sign,
                padded_year,
                monthFromTime(time_value) + 1,
                dateFromTime(time_value),
                hourFromTime(time_value),
                minFromTime(time_value),
                secFromTime(time_value),
                msFromTime(time_value),
            },
        ));
    }

    /// 21.4.4.37 Date.prototype.toJSON ( key )
    /// https://tc39.es/ecma262/#sec-date.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // NOTE: The argument is ignored.

        // 1. Let O be ? ToObject(this value).
        const object = try this_value.toObject(agent);

        // 2. Let tv be ? ToPrimitive(O, number).
        const time_value = try Value.from(object).toPrimitive(agent, .number);

        // 3. If tv is a Number and tv is not finite, return null.
        if (time_value == .number and !time_value.number.isFinite()) return .null;

        // 4. Return ? Invoke(O, "toISOString").
        return Value.from(object).invoke(agent, PropertyKey.from("toISOString"), .{});
    }

    /// 21.4.4.38 Date.prototype.toLocaleDateString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.tolocaledatestring
    fn toLocaleDateString(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        return toDateString_(agent, this_value, arguments);
    }

    /// 21.4.4.39 Date.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        return toString(agent, this_value, arguments);
    }

    /// 21.4.4.40 Date.prototype.toLocaleTimeString ( [ reserved1 [ , reserved2 ] ] )
    /// https://tc39.es/ecma262/#sec-date.prototype.tolocaledatestring
    fn toLocaleTimeString(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        return toTimeString(agent, this_value, arguments);
    }

    /// 21.4.4.41 Date.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. Return ToDateString(tv).
        return Value.from(try toDateString(agent.gc_allocator, time_value));
    }

    /// 21.4.4.42 Date.prototype.toTimeString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.totimestring
    fn toTimeString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If tv is NaN, return "Invalid Date".
        if (std.math.isNan(time_value)) return Value.from("Invalid Date");

        // 5. Let t be LocalTime(tv).
        const t = localTime(time_value);

        // 6. Return the string-concatenation of TimeString(t) and TimeZoneString(tv).
        return Value.from(try std.mem.concat(agent.gc_allocator, u8, &.{
            try timeString(agent.gc_allocator, t),
            try timeZoneString(agent.gc_allocator, time_value),
        }));
    }

    /// 21.4.4.43 Date.prototype.toUTCString ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.toutcstring
    fn toUTCString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Let tv be dateObject.[[DateValue]].
        const time_value = date_object.fields.date_value;

        // 4. If tv is NaN, return "Invalid Date".
        if (std.math.isNan(time_value)) return Value.from("Invalid Date");

        // 5. Let weekday be the Name of the entry in Table 62 with the Number WeekDay(tv).
        const weekday = week_day_names[weekDay(time_value)];

        // 6. Let month be the Name of the entry in Table 63 with the Number MonthFromTime(tv).
        const month = month_names[monthFromTime(time_value)];

        // 7. Let day be ToZeroPaddedDecimalString(ℝ(DateFromTime(tv)), 2).
        const day_ = dateFromTime(time_value);

        // 8. Let yv be YearFromTime(tv).
        const year = yearFromTime(time_value);

        // 9. If yv is +0𝔽 or yv > +0𝔽, let yearSign be the empty String; otherwise, let yearSign
        //    be "-".
        const year_sign = if (year >= 0) "" else "-";

        // 10. Let paddedYear be ToZeroPaddedDecimalString(abs(ℝ(yv)), 4).
        var buf: [6]u8 = undefined;
        const padded_year = toZeroPaddedDecimalString(&buf, std.math.absInt(year) catch unreachable, 4);

        // 11. Return the string-concatenation of weekday, ",", the code unit 0x0020 (SPACE), day,
        //     the code unit 0x0020 (SPACE), month, the code unit 0x0020 (SPACE), yearSign,
        //     paddedYear, the code unit 0x0020 (SPACE), and TimeString(tv).
        return Value.from(try std.fmt.allocPrint(
            agent.gc_allocator,
            "{s}, {:0>2} {s} {s}{s} {s}",
            .{
                weekday,
                day_,
                month,
                year_sign,
                padded_year,
                try timeString(agent.gc_allocator, time_value),
            },
        ));
    }

    /// 21.4.4.44 Date.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-date.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let dateObject be the this value.
        // 2. Perform ? RequireInternalSlot(dateObject, [[DateValue]]).
        const date_object = try this_value.requireInternalSlot(agent, Date);

        // 3. Return dateObject.[[DateValue]].
        return Value.from(date_object.fields.date_value);
    }

    /// 21.4.4.45 Date.prototype [ @@toPrimitive ] ( hint )
    /// https://tc39.es/ecma262/#sec-date.prototype-@@toprimitive
    fn @"@@toPrimitive"(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const hint_value = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (this_value != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{this_value}),
            );
        }
        const object = this_value.object;

        if (hint_value != .string) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not a string", .{hint_value}),
            );
        }
        const hint = hint_value.string;

        // 3. If hint is either "string" or "default", then
        const try_first: PreferredType = if (hint.eql(String.from("string")) or hint.eql(String.from("default"))) blk: {
            // a. Let tryFirst be string.
            break :blk .string;
        }
        // 4. Else if hint is "number", then
        else if (hint.eql(String.from("number"))) blk: {
            // a. Let tryFirst be number.
            break :blk .number;
        }
        // 5. Else,
        else {
            // a. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Hint must be one of 'string', 'number', or 'default'",
            );
        };

        // 6. Return ? OrdinaryToPrimitive(O, tryFirst).
        return object.ordinaryToPrimitive(try_first);
    }
};

/// 21.4.5 Properties of Date Instances
/// https://tc39.es/ecma262/#sec-properties-of-date-instances
pub const Date = Object.Factory(.{
    .Fields = struct {
        /// [[DateValue]]
        date_value: f64,
    },
    .tag = .date,
});
