//! 21.4 Date Objects
//! https://tc39.es/ecma262/#sec-date-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// Simplified infallible variant of `Value.toIntegerOrInfinity()`
fn toIntegerOrInfinity(x: f64) f64 {
    if (std.math.isNan(x)) return 0;
    if (std.math.isInf(x)) return x;
    const truncated = @trunc(x);
    // Normalize negative zero
    return if (truncated == 0) 0 else truncated;
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

/// 21.4.1.26 UTC ( t )
/// https://tc39.es/ecma262/#sec-utc-t
pub fn utc(t: f64) f64 {
    // 1. If t is not finite, return NaN.
    if (!std.math.isFinite(t)) return std.math.nan(f64);

    // TODO: 2-4.
    const offset_ns = 0;

    // 5. Let offsetMs be truncate(offsetNs / 10**6).
    const offset_ms = @trunc(offset_ns / 10e6);

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
    if (ym < std.math.minInt(i32) or ym > std.math.maxInt(i32) or (mn + 1) > std.math.maxInt(i32)) {
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
            const now = std.time.milliTimestamp();
            _ = now;

            // TODO: b. Return ToDateString(now).
            return agent.throwException(
                .internal_error,
                "Calling Date() as a function is not implemented",
            );
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

        return object;
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
