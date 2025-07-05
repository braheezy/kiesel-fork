//! 7 Temporal.Duration Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-duration-objects

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
const createBuiltinFunction = builtins.createBuiltinFunction;
const getTemporalFractionalSecondDigitsOption = builtins.getTemporalFractionalSecondDigitsOption;
const getTemporalRoundingModeOption = builtins.getTemporalRoundingModeOption;
const getTemporalUnitValuedOption = builtins.getTemporalUnitValuedOption;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 7.2 Properties of the Temporal.Duration Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-duration-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Duration",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);

        // 7.2.1 Temporal.Duration.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.Duration.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 7.1.1 Temporal.Duration ( [ years [ , months [ , weeks [ , days [ , hours [ , minutes [ , seconds [ , milliseconds [ , microseconds [ , nanoseconds ] ] ] ] ] ] ] ] ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const years_value = arguments.get(0);
        const months_value = arguments.get(1);
        const weeks_value = arguments.get(2);
        const days_value = arguments.get(3);
        const hours_value = arguments.get(4);
        const minutes_value = arguments.get(5);
        const seconds_value = arguments.get(6);
        const milliseconds_value = arguments.get(7);
        const microseconds_value = arguments.get(8);
        const nanoseconds_value = arguments.get(9);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.Duration must be constructed with 'new'",
                .{},
            );
        };

        // 2. If years is undefined, let y be 0; else let y be ? ToIntegerIfIntegral(years).
        const years = if (years_value.isUndefined()) 0 else try years_value.toIntegerIfIntegral(agent);

        // 3. If months is undefined, let mo be 0; else let mo be ? ToIntegerIfIntegral(months).
        const months = if (months_value.isUndefined()) 0 else try months_value.toIntegerIfIntegral(agent);

        // 4. If weeks is undefined, let w be 0; else let w be ? ToIntegerIfIntegral(weeks).
        const weeks = if (weeks_value.isUndefined()) 0 else try weeks_value.toIntegerIfIntegral(agent);

        // 5. If days is undefined, let d be 0; else let d be ? ToIntegerIfIntegral(days).
        const days = if (days_value.isUndefined()) 0 else try days_value.toIntegerIfIntegral(agent);

        // 6. If hours is undefined, let h be 0; else let h be ? ToIntegerIfIntegral(hours).
        const hours = if (hours_value.isUndefined()) 0 else try hours_value.toIntegerIfIntegral(agent);

        // 7. If minutes is undefined, let m be 0; else let m be ? ToIntegerIfIntegral(minutes).
        const minutes = if (minutes_value.isUndefined()) 0 else try minutes_value.toIntegerIfIntegral(agent);

        // 8. If seconds is undefined, let s be 0; else let s be ? ToIntegerIfIntegral(seconds).
        const seconds = if (seconds_value.isUndefined()) 0 else try seconds_value.toIntegerIfIntegral(agent);

        // 9. If milliseconds is undefined, let ms be 0; else let ms be ? ToIntegerIfIntegral(milliseconds).
        const milliseconds = if (milliseconds_value.isUndefined()) 0 else try milliseconds_value.toIntegerIfIntegral(agent);

        // 10. If microseconds is undefined, let mis be 0; else let mis be ? ToIntegerIfIntegral(microseconds).
        const microseconds = if (microseconds_value.isUndefined()) 0 else try microseconds_value.toIntegerIfIntegral(agent);

        // 11. If nanoseconds is undefined, let ns be 0; else let ns be ? ToIntegerIfIntegral(nanoseconds).
        const nanoseconds = if (nanoseconds_value.isUndefined()) 0 else try nanoseconds_value.toIntegerIfIntegral(agent);

        // 12. Return ? CreateTemporalDuration(y, mo, w, d, h, m, s, ms, mis, ns, NewTarget).
        const temporal_rs_duration = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_try_new(
                std.math.lossyCast(i64, years),
                std.math.lossyCast(i64, months),
                std.math.lossyCast(i64, weeks),
                std.math.lossyCast(i64, days),
                std.math.lossyCast(i64, hours),
                std.math.lossyCast(i64, minutes),
                std.math.lossyCast(i64, seconds),
                std.math.lossyCast(i64, milliseconds),
                microseconds,
                nanoseconds,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid duration", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
        return Value.from(
            try createTemporalDuration(agent, temporal_rs_duration.?, new_target),
        );
    }

    /// 7.2.2 Temporal.Duration.from ( item )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.from
    fn from(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const item = arguments.get(0);

        // 1. Return ? ToTemporalDuration(item).
        return Value.from(try toTemporalDuration(agent, item));
    }
};

/// 7.3 Properties of the Temporal.Duration Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-duration-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "abs", abs, 0, realm);
        try object.defineBuiltinAccessor(agent, "blank", blank, null, realm);
        try object.defineBuiltinAccessor(agent, "days", days, null, realm);
        try object.defineBuiltinAccessor(agent, "hours", hours, null, realm);
        try object.defineBuiltinAccessor(agent, "microseconds", microseconds, null, realm);
        try object.defineBuiltinAccessor(agent, "milliseconds", milliseconds, null, realm);
        try object.defineBuiltinAccessor(agent, "minutes", minutes, null, realm);
        try object.defineBuiltinAccessor(agent, "months", months, null, realm);
        try object.defineBuiltinAccessor(agent, "nanoseconds", nanoseconds, null, realm);
        try object.defineBuiltinFunction(agent, "negated", negated, 0, realm);
        try object.defineBuiltinAccessor(agent, "seconds", seconds, null, realm);
        try object.defineBuiltinAccessor(agent, "sign", sign, null, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);
        try object.defineBuiltinAccessor(agent, "weeks", weeks, null, realm);
        try object.defineBuiltinFunction(agent, "with", with, 1, realm);
        try object.defineBuiltinAccessor(agent, "years", years, null, realm);

        // 7.3.1 Temporal.Duration.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.Duration%"()),
        );

        // 7.3.2 Temporal.Duration.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.Duration"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 7.3.17 Temporal.Duration.prototype.abs ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.abs
    fn abs(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return ! CreateTemporalDuration(abs(duration.[[Years]]), abs(duration.[[Months]]),
        //    abs(duration.[[Weeks]]), abs(duration.[[Days]]), abs(duration.[[Hours]]),
        //    abs(duration.[[Minutes]]), abs(duration.[[Seconds]]), abs(duration.[[Milliseconds]]),
        //    abs(duration.[[Microseconds]]), abs(duration.[[Nanoseconds]])).
        const temporal_rs_duration = temporal_rs.c.temporal_rs_Duration_abs(duration.fields.inner);
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration);
        return Value.from(
            createTemporalDuration(
                agent,
                temporal_rs_duration.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 7.3.14 get Temporal.Duration.prototype.blank
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.blank
    fn blank(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. If DurationSign(duration) = 0, return true.
        // 4. Return false.
        return Value.from(temporal_rs.c.temporal_rs_Duration_is_zero(duration.fields.inner));
    }

    /// 7.3.6 get Temporal.Duration.prototype.days
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.days
    fn days(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Days]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_days(duration.fields.inner))),
        );
    }

    /// 7.3.7 get Temporal.Duration.prototype.hours
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.hours
    fn hours(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Hours]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_hours(duration.fields.inner))),
        );
    }

    /// 7.3.11 get Temporal.Duration.prototype.microseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.microseconds
    fn microseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Microseconds]]).
        return Value.from(temporal_rs.c.temporal_rs_Duration_microseconds(duration.fields.inner));
    }

    /// 7.3.10 get Temporal.Duration.prototype.milliseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.milliseconds
    fn milliseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Milliseconds]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_milliseconds(duration.fields.inner))),
        );
    }

    /// 7.3.8 get Temporal.Duration.prototype.minutes
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.minutes
    fn minutes(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Minutes]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_minutes(duration.fields.inner))),
        );
    }

    /// 7.3.4 get Temporal.Duration.prototype.months
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.months
    fn months(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Months]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_months(duration.fields.inner))),
        );
    }

    /// 7.3.12 get Temporal.Duration.prototype.nanoseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.nanoseconds
    fn nanoseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Nanoseconds]]).
        return Value.from(temporal_rs.c.temporal_rs_Duration_nanoseconds(duration.fields.inner));
    }

    /// 7.3.16 Temporal.Duration.prototype.negated ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.negated
    fn negated(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return CreateNegatedTemporalDuration(duration).
        const temporal_rs_duration = temporal_rs.c.temporal_rs_Duration_negated(duration.fields.inner);
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
        return Value.from(
            createTemporalDuration(
                agent,
                temporal_rs_duration.?,
                null,
            ) catch |err| try noexcept(err),
        );
    }

    /// 7.3.9 get Temporal.Duration.prototype.seconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.seconds
    fn seconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Seconds]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_seconds(duration.fields.inner))),
        );
    }

    /// 7.3.13 get Temporal.Duration.prototype.sign
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.sign
    fn sign(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(DurationSign(duration)).
        return Value.from(temporal_rs.c.temporal_rs_Duration_sign(duration.fields.inner));
    }

    /// 7.3.23 Temporal.Duration.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return TemporalDurationToString(duration, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_to_string(
                duration.fields.inner,
                temporal_rs.to_string_rounding_options_auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 7.3.24 Temporal.Duration.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return TemporalDurationToString(duration, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_to_string(
                duration.fields.inner,
                temporal_rs.to_string_rounding_options_auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 7.3.22 Temporal.Duration.prototype.toString ( [ options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const options_value = arguments.get(0);

        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 4. NOTE: The following steps read options and perform independent validation in
        //    alphabetical order (GetTemporalFractionalSecondDigitsOption reads
        //    "fractionalSecondDigits" and GetRoundingModeOption reads "roundingMode").

        // 5. Let digits be ? GetTemporalFractionalSecondDigitsOption(resolvedOptions).
        const precision = try getTemporalFractionalSecondDigitsOption(agent, options);

        // 6. Let roundingMode be ? GetRoundingModeOption(resolvedOptions, trunc).
        const rounding_mode = try getTemporalRoundingModeOption(
            agent,
            options,
            temporal_rs.c.RoundingMode_Trunc,
        );

        // 7. Let smallestUnit be ? GetTemporalUnitValuedOption(resolvedOptions, "smallestUnit", time, unset).
        const smallest_unit = try getTemporalUnitValuedOption(
            agent,
            options,
            "smallestUnit",
            .time,
            .unset,
            &.{},
        );

        // 8. If smallestUnit is hour or minute, throw a RangeError exception.
        // 9. Let precision be ToSecondsStringPrecisionRecord(smallestUnit, digits).
        // 10. If precision.[[Unit]] is nanosecond and precision.[[Increment]] = 1, then
        //     a. Return TemporalDurationToString(duration, precision.[[Precision]]).
        // 11. Let largestUnit be DefaultTemporalLargestUnit(duration).
        // 12. Let internalDuration be ToInternalDurationRecord(duration).
        // 13. Let timeDuration be ? RoundTimeDuration(internalDuration.[[Time]], precision.[[Increment]], precision.[[Unit]], roundingMode).
        // 14. Set internalDuration to CombineDateAndTimeDuration(internalDuration.[[Date]], timeDuration).
        // 15. Let roundedLargestUnit be LargerOfTwoTemporalUnits(largestUnit, second).
        // 16. Let roundedDuration be ? TemporalDurationFromInternal(internalDuration, roundedLargestUnit).
        // 17. Return TemporalDurationToString(roundedDuration, precision.[[Precision]]).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_to_string(
                duration.fields.inner,
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
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(
                    .range_error,
                    "Invalid duration string options",
                    .{},
                );
            },
            else => unreachable,
        };
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 7.3.25 Temporal.Duration.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.Duration to primitive value",
            .{},
        );
    }

    /// 7.3.5 get Temporal.Duration.prototype.weeks
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.weeks
    fn weeks(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Weeks]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_weeks(duration.fields.inner))),
        );
    }

    /// 7.3.15 Temporal.Duration.prototype.with ( temporalDurationLike )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.with
    fn with(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const temporal_duration_like_value = arguments.get(0);

        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Let temporalDurationLike be ? ToTemporalPartialDurationRecord(temporalDurationLike).
        const temporal_duration_like = try toTemporalPartialDuration(
            agent,
            temporal_duration_like_value,
        );

        // 4. If temporalDurationLike.[[Years]] is not undefined, then
        //     a. Let years be temporalDurationLike.[[Years]].
        // 5. Else,
        //     a. Let years be duration.[[Years]].
        const years_ = temporal_rs.fromOptional(temporal_duration_like.years) orelse
            temporal_rs.c.temporal_rs_Duration_years(duration.fields.inner);

        // 6. If temporalDurationLike.[[Months]] is not undefined, then
        //     a. Let months be temporalDurationLike.[[Months]].
        // 7. Else,
        //     a. Let months be duration.[[Months]].
        const months_ = temporal_rs.fromOptional(temporal_duration_like.months) orelse
            temporal_rs.c.temporal_rs_Duration_months(duration.fields.inner);

        // 8. If temporalDurationLike.[[Weeks]] is not undefined, then
        //     a. Let weeks be temporalDurationLike.[[Weeks]].
        // 9. Else,
        //     a. Let weeks be duration.[[Weeks]].
        const weeks_ = temporal_rs.fromOptional(temporal_duration_like.weeks) orelse
            temporal_rs.c.temporal_rs_Duration_weeks(duration.fields.inner);

        // 10. If temporalDurationLike.[[Days]] is not undefined, then
        //     a. Let days be temporalDurationLike.[[Days]].
        // 11. Else,
        //     a. Let days be duration.[[Days]].
        const days_ = temporal_rs.fromOptional(temporal_duration_like.days) orelse
            temporal_rs.c.temporal_rs_Duration_days(duration.fields.inner);

        // 12. If temporalDurationLike.[[Hours]] is not undefined, then
        //     a. Let hours be temporalDurationLike.[[Hours]].
        // 13. Else,
        //     a. Let hours be duration.[[Hours]].
        const hours_ = temporal_rs.fromOptional(temporal_duration_like.hours) orelse
            temporal_rs.c.temporal_rs_Duration_hours(duration.fields.inner);

        // 14. If temporalDurationLike.[[Minutes]] is not undefined, then
        //     a. Let minutes be temporalDurationLike.[[Minutes]].
        // 15. Else,
        //     a. Let minutes be duration.[[Minutes]].
        const minutes_ = temporal_rs.fromOptional(temporal_duration_like.minutes) orelse
            temporal_rs.c.temporal_rs_Duration_minutes(duration.fields.inner);

        // 16. If temporalDurationLike.[[Seconds]] is not undefined, then
        //     a. Let seconds be temporalDurationLike.[[Seconds]].
        // 17. Else,
        //     a. Let seconds be duration.[[Seconds]].
        const seconds_ = temporal_rs.fromOptional(temporal_duration_like.seconds) orelse
            temporal_rs.c.temporal_rs_Duration_seconds(duration.fields.inner);

        // 18. If temporalDurationLike.[[Milliseconds]] is not undefined, then
        //     a. Let milliseconds be temporalDurationLike.[[Milliseconds]].
        // 19. Else,
        //     a. Let milliseconds be duration.[[Milliseconds]].
        const milliseconds_ = temporal_rs.fromOptional(temporal_duration_like.milliseconds) orelse
            temporal_rs.c.temporal_rs_Duration_milliseconds(duration.fields.inner);

        // 20. If temporalDurationLike.[[Microseconds]] is not undefined, then
        //     a. Let microseconds be temporalDurationLike.[[Microseconds]].
        // 21. Else,
        //     a. Let microseconds be duration.[[Microseconds]].
        const microseconds_ = temporal_rs.fromOptional(temporal_duration_like.microseconds) orelse
            temporal_rs.c.temporal_rs_Duration_microseconds(duration.fields.inner);

        // 22. If temporalDurationLike.[[Nanoseconds]] is not undefined, then
        //     a. Let nanoseconds be temporalDurationLike.[[Nanoseconds]].
        // 23. Else,
        //     a. Let nanoseconds be duration.[[Nanoseconds]].
        const nanoseconds_ = temporal_rs.fromOptional(temporal_duration_like.nanoseconds) orelse
            temporal_rs.c.temporal_rs_Duration_nanoseconds(duration.fields.inner);

        // 24. Return ? CreateTemporalDuration(years, months, weeks, days, hours, minutes, seconds,
        //     milliseconds, microseconds, nanoseconds).
        const temporal_rs_duration = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_try_new(
                years_,
                months_,
                weeks_,
                days_,
                hours_,
                minutes_,
                seconds_,
                milliseconds_,
                microseconds_,
                nanoseconds_,
            ),
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(.range_error, "Invalid duration", .{});
            },
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
        return Value.from(try createTemporalDuration(agent, temporal_rs_duration.?, null));
    }

    /// 7.3.3 get Temporal.Duration.prototype.years
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.years
    fn years(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let duration be the this value.
        // 2. Perform ? RequireInternalSlot(duration, [[InitializedTemporalDuration]]).
        const duration = try this_value.requireInternalSlot(agent, Duration);

        // 3. Return 𝔽(duration.[[Years]]).
        return Value.from(
            @as(f64, @floatFromInt(temporal_rs.c.temporal_rs_Duration_years(duration.fields.inner))),
        );
    }
};

/// 7.4 Properties of Temporal.Duration Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-duration-instances
pub const Duration = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.Duration,
    },
    .tag = .temporal_duration,
});

/// 7.5.12 ToTemporalDuration ( item )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporalduration
pub fn toTemporalDuration(agent: *Agent, item: Value) Agent.Error!*Object {
    // 1. If item is an Object and item has an [[InitializedTemporalDuration]] internal slot, then
    if (item.isObject() and item.asObject().is(Duration)) {
        // a. Return ! CreateTemporalDuration(item.[[Years]], item.[[Months]], item.[[Weeks]],
        //    item.[[Days]], item.[[Hours]], item.[[Minutes]], item.[[Seconds]],
        //    item.[[Milliseconds]], item.[[Microseconds]], item.[[Nanoseconds]]).
        const duration = item.asObject().as(Duration);
        const temporal_rs_duration = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_try_new(
                temporal_rs.c.temporal_rs_Duration_years(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_months(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_weeks(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_days(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_hours(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_minutes(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_seconds(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_milliseconds(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_microseconds(duration.fields.inner),
                temporal_rs.c.temporal_rs_Duration_nanoseconds(duration.fields.inner),
            ),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
        return createTemporalDuration(agent, temporal_rs_duration.?, null);
    }

    // 2. If item is not an Object, then
    if (!item.isObject()) {
        // a. If item is not a String, throw a TypeError exception.
        if (!item.isString()) {
            return agent.throwException(.type_error, "Duration must be a string or object", .{});
        }

        // b. Return ? ParseTemporalDurationString(item).
        const duration_utf8 = try item.asString().toUtf8(agent.gc_allocator);
        defer agent.gc_allocator.free(duration_utf8);
        const temporal_rs_duration = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_from_utf8(
                temporal_rs.toDiplomatStringView(duration_utf8),
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid duration string", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
        return createTemporalDuration(agent, temporal_rs_duration.?, null);
    }

    // 3. Let result be a new Partial Duration Record with each field set to 0.
    // 4. Let partial be ? ToTemporalPartialDurationRecord(item).
    const partial = try toTemporalPartialDuration(agent, item);

    // 5. If partial.[[Years]] is not undefined, set result.[[Years]] to partial.[[Years]].
    // 6. If partial.[[Months]] is not undefined, set result.[[Months]] to partial.[[Months]].
    // 7. If partial.[[Weeks]] is not undefined, set result.[[Weeks]] to partial.[[Weeks]].
    // 8. If partial.[[Days]] is not undefined, set result.[[Days]] to partial.[[Days]].
    // 9. If partial.[[Hours]] is not undefined, set result.[[Hours]] to partial.[[Hours]].
    // 10. If partial.[[Minutes]] is not undefined, set result.[[Minutes]] to partial.[[Minutes]].
    // 11. If partial.[[Seconds]] is not undefined, set result.[[Seconds]] to partial.[[Seconds]].
    // 12. If partial.[[Milliseconds]] is not undefined, set result.[[Milliseconds]] to partial.[[Milliseconds]].
    // 13. If partial.[[Microseconds]] is not undefined, set result.[[Microseconds]] to partial.[[Microseconds]].
    // 14. If partial.[[Nanoseconds]] is not undefined, set result.[[Nanoseconds]] to partial.[[Nanoseconds]].
    // 15. Return ? CreateTemporalDuration(result.[[Years]], result.[[Months]], result.[[Weeks]],
    //     result.[[Days]], result.[[Hours]], result.[[Minutes]], result.[[Seconds]],
    //     result.[[Milliseconds]], result.[[Microseconds]], result.[[Nanoseconds]]).
    const temporal_rs_duration = temporal_rs.temporalErrorResult(
        temporal_rs.c.temporal_rs_Duration_from_partial_duration(partial),
    ) catch |err| switch (err) {
        error.RangeError => return agent.throwException(.range_error, "Invalid duration", .{}),
        else => unreachable,
    };
    errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
    return createTemporalDuration(agent, temporal_rs_duration.?, null);
}

/// 7.5.18 ToTemporalPartialDurationRecord ( temporalDurationLike )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporalpartialdurationrecord
pub fn toTemporalPartialDuration(
    agent: *Agent,
    temporal_duration_like_value: Value,
) Agent.Error!temporal_rs.c.PartialDuration {
    // 1. If temporalDurationLike is not an Object, then
    if (!temporal_duration_like_value.isObject()) {
        // a. Throw a TypeError exception.
        return agent.throwException(.type_error, "Duration-like must be an object", .{});
    }
    const temporal_duration_like = temporal_duration_like_value.asObject();

    // 2. Let result be a new partial Duration Record with each field set to undefined.
    var result: temporal_rs.c.PartialDuration = .{
        .years = .{ .is_ok = false },
        .months = .{ .is_ok = false },
        .weeks = .{ .is_ok = false },
        .days = .{ .is_ok = false },
        .hours = .{ .is_ok = false },
        .minutes = .{ .is_ok = false },
        .seconds = .{ .is_ok = false },
        .milliseconds = .{ .is_ok = false },
        .microseconds = .{ .is_ok = false },
        .nanoseconds = .{ .is_ok = false },
    };

    // 3. NOTE: The following steps read properties and perform independent validation in
    //    alphabetical order.

    // 4. Let days be ? Get(temporalDurationLike, "days").
    const days = try temporal_duration_like.get(agent, PropertyKey.from("days"));

    // 5. If days is not undefined, set result.[[Days]] to ? ToIntegerIfIntegral(days).
    if (!days.isUndefined()) {
        result.days = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try days.toIntegerIfIntegral(agent)) },
        };
    }

    // 6. Let hours be ? Get(temporalDurationLike, "hours").
    const hours = try temporal_duration_like.get(agent, PropertyKey.from("hours"));

    // 7. If hours is not undefined, set result.[[Hours]] to ? ToIntegerIfIntegral(hours).
    if (!hours.isUndefined()) {
        result.hours = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try hours.toIntegerIfIntegral(agent)) },
        };
    }

    // 8. Let microseconds be ? Get(temporalDurationLike, "microseconds").
    const microseconds = try temporal_duration_like.get(agent, PropertyKey.from("microseconds"));

    // 9. If microseconds is not undefined, set result.[[Microseconds]] to ? ToIntegerIfIntegral(microseconds).
    if (!microseconds.isUndefined()) {
        result.microseconds = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = try microseconds.toIntegerIfIntegral(agent) },
        };
    }

    // 10. Let milliseconds be ? Get(temporalDurationLike, "milliseconds").
    const milliseconds = try temporal_duration_like.get(agent, PropertyKey.from("milliseconds"));

    // 11. If milliseconds is not undefined, set result.[[Milliseconds]] to ? ToIntegerIfIntegral(milliseconds).
    if (!milliseconds.isUndefined()) {
        result.milliseconds = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try milliseconds.toIntegerIfIntegral(agent)) },
        };
    }

    // 12. Let minutes be ? Get(temporalDurationLike, "minutes").
    const minutes = try temporal_duration_like.get(agent, PropertyKey.from("minutes"));

    // 13. If minutes is not undefined, set result.[[Minutes]] to ? ToIntegerIfIntegral(minutes).
    if (!minutes.isUndefined()) {
        result.minutes = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try minutes.toIntegerIfIntegral(agent)) },
        };
    }

    // 14. Let months be ? Get(temporalDurationLike, "months").
    const months = try temporal_duration_like.get(agent, PropertyKey.from("months"));

    // 15. If months is not undefined, set result.[[Months]] to ? ToIntegerIfIntegral(months).
    if (!months.isUndefined()) {
        result.months = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try months.toIntegerIfIntegral(agent)) },
        };
    }

    // 16. Let nanoseconds be ? Get(temporalDurationLike, "nanoseconds").
    const nanoseconds = try temporal_duration_like.get(agent, PropertyKey.from("nanoseconds"));

    // 17. If nanoseconds is not undefined, set result.[[Nanoseconds]] to ? ToIntegerIfIntegral(nanoseconds).
    if (!nanoseconds.isUndefined()) {
        result.nanoseconds = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = try nanoseconds.toIntegerIfIntegral(agent) },
        };
    }

    // 18. Let seconds be ? Get(temporalDurationLike, "seconds").
    const seconds = try temporal_duration_like.get(agent, PropertyKey.from("seconds"));

    // 19. If seconds is not undefined, set result.[[Seconds]] to ? ToIntegerIfIntegral(seconds).
    if (!seconds.isUndefined()) {
        result.seconds = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try seconds.toIntegerIfIntegral(agent)) },
        };
    }

    // 20. Let weeks be ? Get(temporalDurationLike, "weeks").
    const weeks = try temporal_duration_like.get(agent, PropertyKey.from("weeks"));

    // 21. If weeks is not undefined, set result.[[Weeks]] to ? ToIntegerIfIntegral(weeks).
    if (!weeks.isUndefined()) {
        result.weeks = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try weeks.toIntegerIfIntegral(agent)) },
        };
    }

    // 22. Let years be ? Get(temporalDurationLike, "years").
    const years = try temporal_duration_like.get(agent, PropertyKey.from("years"));

    // 23. If years is not undefined, set result.[[Years]] to ? ToIntegerIfIntegral(years).
    if (!years.isUndefined()) {
        result.years = .{
            .is_ok = true,
            .unnamed_0 = .{ .ok = std.math.lossyCast(i64, try years.toIntegerIfIntegral(agent)) },
        };
    }

    // 24. If years is undefined, and months is undefined, and weeks is undefined, and days is
    //     undefined, and hours is undefined, and minutes is undefined, and seconds is undefined,
    //     and milliseconds is undefined, and microseconds is undefined, and nanoseconds is
    //     undefined, throw a TypeError exception.
    if (!result.years.is_ok and !result.months.is_ok and !result.weeks.is_ok and
        !result.days.is_ok and !result.hours.is_ok and !result.minutes.is_ok and
        !result.seconds.is_ok and !result.milliseconds.is_ok and !result.microseconds.is_ok and
        !result.nanoseconds.is_ok)
    {
        return agent.throwException(
            .type_error,
            "Duration-like object must have at least one field defined",
            .{},
        );
    }

    // 25. Return result.
    return result;
}

/// 7.5.19 CreateTemporalDuration ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalduration
pub fn createTemporalDuration(
    agent: *Agent,
    inner: *temporal_rs.c.Duration,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If IsValidDuration(years, months, weeks, days, hours, minutes, seconds, milliseconds,
    //    microseconds, nanoseconds) is false, throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.Duration%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.Duration%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.Duration.prototype%",
    //    « [[InitializedTemporalDuration]], [[Years]], [[Months]], [[Weeks]], [[Days]], [[Hours]],
    //    [[Minutes]], [[Seconds]], [[Milliseconds]], [[Microseconds]], [[Nanoseconds]] »).
    // 4. Set object.[[Years]] to ℝ(𝔽(years)).
    // 5. Set object.[[Months]] to ℝ(𝔽(months)).
    // 6. Set object.[[Weeks]] to ℝ(𝔽(weeks)).
    // 7. Set object.[[Days]] to ℝ(𝔽(days)).
    // 8. Set object.[[Hours]] to ℝ(𝔽(hours)).
    // 9. Set object.[[Minutes]] to ℝ(𝔽(minutes)).
    // 10. Set object.[[Seconds]] to ℝ(𝔽(seconds)).
    // 11. Set object.[[Milliseconds]] to ℝ(𝔽(milliseconds)).
    // 12. Set object.[[Microseconds]] to ℝ(𝔽(microseconds)).
    // 13. Set object.[[Nanoseconds]] to ℝ(𝔽(nanoseconds)).
    // 14. Return object.
    return ordinaryCreateFromConstructor(
        Duration,
        agent,
        new_target,
        "%Temporal.Duration.prototype%",
        .{ .inner = inner },
    );
}
