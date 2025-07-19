//! 8 Temporal.Instant Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-instant-objects

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
const createBuiltinFunction = builtins.createBuiltinFunction;
const createTemporalZonedDateTime = builtins.createTemporalZonedDateTime;
const getTemporalFractionalSecondDigitsOption = builtins.getTemporalFractionalSecondDigitsOption;
const getTemporalRoundingModeOption = builtins.getTemporalRoundingModeOption;
const getTemporalUnitValuedOption = builtins.getTemporalUnitValuedOption;
const noexcept = utils.noexcept;
const numberToBigInt = builtins.numberToBigInt;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const toTemporalTimeZoneIdentifier = builtins.toTemporalTimeZoneIdentifier;
const validateTemporalUnitValue = builtins.validateTemporalUnitValue;

/// 8.2 Properties of the Temporal.Instant Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-instant-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Instant",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "compare", compare, 2, realm);
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);
        try object.defineBuiltinFunction(agent, "fromEpochMilliseconds", fromEpochMilliseconds, 1, realm);
        try object.defineBuiltinFunction(agent, "fromEpochNanoseconds", fromEpochNanoseconds, 1, realm);

        // 8.2.1 Temporal.Instant.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.Instant.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 8.1.1 Temporal.Instant ( epochNanoseconds )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const epoch_nanoseconds_value = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.Instant must be constructed with 'new'",
                .{},
            );
        };

        // 2. Let epochNanoseconds be ? ToBigInt(epochNanoseconds).
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

        // 4. Return ? CreateTemporalInstant(epochNanoseconds, NewTarget).
        const temporal_rs_instant = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_try_new(epoch_nanoseconds),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        return Value.from(
            try createTemporalInstant(agent, temporal_rs_instant.?, new_target),
        );
    }

    /// 8.2.5 Temporal.Instant.compare ( one, two )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.compare
    fn compare(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const one_value = arguments.get(0);
        const two_value = arguments.get(1);

        // 1. Set one to ? ToTemporalInstant(one).
        const one = try toTemporalInstant(agent, one_value);

        // 2. Set two to ? ToTemporalInstant(two).
        const two = try toTemporalInstant(agent, two_value);

        // 3. Return 𝔽(CompareEpochNanoseconds(one.[[EpochNanoseconds]], two.[[EpochNanoseconds]])).
        return Value.from(
            temporal_rs.c.temporal_rs_Instant_compare(
                one.as(Instant).fields.inner,
                two.as(Instant).fields.inner,
            ),
        );
    }

    /// 8.2.2 Temporal.Instant.from ( item )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.from
    fn from(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const item = arguments.get(0);

        // 1. Return ? ToTemporalInstant(item).
        return Value.from(try toTemporalInstant(agent, item));
    }

    /// 8.2.3 Temporal.Instant.fromEpochMilliseconds ( epochMilliseconds )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.fromepochmilliseconds
    fn fromEpochMilliseconds(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const epoch_milliseconds_value = arguments.get(0);

        // 1. Set epochMilliseconds to ? ToNumber(epochMilliseconds).
        const epoch_milliseconds_number = try epoch_milliseconds_value.toNumber(agent);

        // 2. Set epochMilliseconds to ? NumberToBigInt(epochMilliseconds).
        const epoch_milliseconds_bigint = try numberToBigInt(agent, epoch_milliseconds_number);
        const epoch_milliseconds = epoch_milliseconds_bigint.managed.toInt(i64) catch std.math.maxInt(i64);

        // 3. Let epochNanoseconds be epochMilliseconds × ℤ(10**6).
        // 4. If IsValidEpochNanoseconds(epochNanoseconds) is false, throw a RangeError exception.
        // 5. Return ! CreateTemporalInstant(epochNanoseconds).
        const temporal_rs_instant = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_from_epoch_milliseconds(epoch_milliseconds),
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(
                    .range_error,
                    "Invalid epoch milliseconds {}",
                    .{epoch_milliseconds_bigint.managed},
                );
            },
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        return Value.from(
            createTemporalInstant(agent, temporal_rs_instant.?, null) catch |err| try noexcept(err),
        );
    }

    /// 8.2.4 Temporal.Instant.fromEpochNanoseconds ( epochNanoseconds )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.fromepochnanoseconds
    fn fromEpochNanoseconds(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const epoch_nanoseconds_value = arguments.get(0);

        // 1. Set epochNanoseconds to ? ToBigInt(epochNanoseconds).
        const epoch_nanoseconds_bigint = try epoch_nanoseconds_value.toBigInt(agent);
        const epoch_nanoseconds = temporal_rs.toI128Nanoseconds(
            epoch_nanoseconds_bigint.managed.toInt(i128) catch std.math.maxInt(i128),
        );

        // 2. If IsValidEpochNanoseconds(epochNanoseconds) is false, throw a RangeError exception.
        // 3. Return ! CreateTemporalInstant(epochNanoseconds).
        const temporal_rs_instant = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_try_new(epoch_nanoseconds),
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(
                    .range_error,
                    "Invalid epoch nanoseconds {}",
                    .{epoch_nanoseconds_bigint.managed},
                );
            },
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        return Value.from(
            createTemporalInstant(agent, temporal_rs_instant.?, null) catch |err| try noexcept(err),
        );
    }
};

/// 8.3 Properties of the Temporal.Instant Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-instant-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinAccessor(agent, "epochMilliseconds", epochMilliseconds, null, realm);
        try object.defineBuiltinAccessor(agent, "epochNanoseconds", epochNanoseconds, null, realm);
        try object.defineBuiltinFunction(agent, "equals", equals, 1, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "toZonedDateTimeISO", toZonedDateTimeISO, 1, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

        // 8.3.1 Temporal.Instant.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.Instant%"()),
        );

        // 8.3.2 Temporal.Instant.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.Instant"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 8.3.3 get Temporal.Instant.prototype.epochMilliseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.instant.prototype.epochmilliseconds
    fn epochMilliseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

        // 3. Let ns be instant.[[EpochNanoseconds]].
        // 4. Let ms be floor(ℝ(ns) / 10**6).
        const ms = temporal_rs.c.temporal_rs_Instant_epoch_milliseconds(instant.fields.inner);

        // 5. Return 𝔽(ms).
        return Value.from(@as(f64, @floatFromInt(ms)));
    }

    /// 8.3.4 get Temporal.Instant.prototype.epochNanoseconds
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.instant.prototype.epochnanoseconds
    fn epochNanoseconds(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

        // 3. Return instant.[[EpochNanoseconds]].
        const ns = temporal_rs.fromI128Nanoseconds(
            temporal_rs.c.temporal_rs_Instant_epoch_nanoseconds(instant.fields.inner),
        );
        const managed = try std.math.big.int.Managed.initSet(agent.gc_allocator, ns);
        return Value.from(try BigInt.from(agent.gc_allocator, managed));
    }

    /// 8.3.10 Temporal.Instant.prototype.equals ( other )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.equals
    fn equals(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other_value = arguments.get(0);

        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

        // 3. Set other to ? ToTemporalInstant(other).
        const other = try toTemporalInstant(agent, other_value);

        // 4. If instant.[[EpochNanoseconds]] ≠ other.[[EpochNanoseconds]], return false.
        // 5. Return true.
        return Value.from(
            temporal_rs.c.temporal_rs_Instant_equals(
                instant.fields.inner,
                other.as(Instant).fields.inner,
            ),
        );
    }

    /// 8.3.13 Temporal.Instant.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

        // 3. Return TemporalInstantToString(instant, undefined, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_to_ixdtf_string_with_compiled_data(
                instant.fields.inner,
                null,
                temporal_rs.to_string_rounding_options_auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 8.3.12 Temporal.Instant.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

        // 3. Return TemporalInstantToString(instant, undefined, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_to_ixdtf_string_with_compiled_data(
                instant.fields.inner,
                null,
                temporal_rs.to_string_rounding_options_auto,
                &write.inner,
            ),
        ) catch unreachable;
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 8.3.11 Temporal.Instant.prototype.toString ( [ options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const options_value = arguments.get(0);

        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

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

        // 7. Let smallestUnit be ? GetTemporalUnitValuedOption(resolvedOptions, "smallestUnit",
        //    unset).
        const smallest_unit = try getTemporalUnitValuedOption(
            agent,
            options,
            "smallestUnit",
            .unset,
        );

        // 8. Perform ? ValidateTemporalUnitValue(smallestUnit, time).
        try validateTemporalUnitValue(agent, smallest_unit, "smallestUnit", .time, &.{});

        // 9. If smallestUnit is hour, throw a RangeError exception.
        if (smallest_unit == temporal_rs.c.Unit_Hour) {
            return agent.throwException(
                .range_error,
                "Invalid value for option 'smallestUnit'",
                .{},
            );
        }

        // 10. Let timeZone be ? Get(resolvedOptions, "timeZone").
        const time_zone_value = try options.get(agent, PropertyKey.from("timeZone"));

        var maybe_time_zone: ?*temporal_rs.c.TimeZone = null;
        defer if (maybe_time_zone) |time_zone| temporal_rs.c.temporal_rs_TimeZone_destroy(time_zone);

        // 11. If timeZone is not undefined, then
        if (!time_zone_value.isUndefined()) {
            // a. Set timeZone to ? ToTemporalTimeZoneIdentifier(timeZone).
            maybe_time_zone = try toTemporalTimeZoneIdentifier(agent, time_zone_value);
        }

        // 12. Let precision be ToSecondsStringPrecisionRecord(smallestUnit, digits).
        // 13. Let roundedNs be RoundTemporalInstant(instant.[[EpochNanoseconds]], precision.[[Increment]], precision.[[Unit]], roundingMode).
        // 14. Let roundedInstant be ! CreateTemporalInstant(roundedNs).
        // 15. Return TemporalInstantToString(roundedInstant, timeZone, precision.[[Precision]]).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_to_ixdtf_string_with_compiled_data(
                instant.fields.inner,
                maybe_time_zone,
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

    /// 8.3.15 Temporal.Instant.prototype.toZonedDateTimeISO ( timeZone )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tozoneddatetimeiso
    fn toZonedDateTimeISO(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const time_zone_value = arguments.get(0);

        // 1. Let instant be the this value.
        // 2. Perform ? RequireInternalSlot(instant, [[InitializedTemporalInstant]]).
        const instant = try this_value.requireInternalSlot(agent, Instant);

        // 3. Set timeZone to ? ToTemporalTimeZoneIdentifier(timeZone).
        const time_zone = try toTemporalTimeZoneIdentifier(agent, time_zone_value);
        errdefer temporal_rs.c.temporal_rs_TimeZone_destroy(time_zone);

        // 4. Return ! CreateTemporalZonedDateTime(instant.[[EpochNanoseconds]], timeZone, "iso8601").
        const temporal_rs_zoned_date_time = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_ZonedDateTime_try_new(
                temporal_rs.c.temporal_rs_Instant_epoch_nanoseconds(instant.fields.inner),
                temporal_rs.c.AnyCalendarKind_Iso,
                time_zone,
            ),
        ) catch |err| switch (err) {
            error.RangeError => {
                return agent.throwException(
                    .range_error,
                    "Invalid time zone identifier: {}",
                    .{time_zone_value},
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

    /// 8.3.14 Temporal.Instant.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.Instant to primitive value",
            .{},
        );
    }
};

/// 8.4 Properties of Temporal.Instant Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-instant-instances
pub const Instant = MakeObject(.{
    .Fields = struct {
        inner: *temporal_rs.c.Instant,
    },
    .finalizer = struct {
        fn finalizer(object: *Object) void {
            temporal_rs.c.temporal_rs_Instant_destroy(object.as(Instant).fields.inner);
        }
    }.finalizer,
    .tag = .temporal_instant,
});

/// nsMaxInstant = 10**8 × nsPerDay = 8.64 × 10**21
/// https://tc39.es/proposal-temporal/#eqn-nsMaxInstant
pub const ns_max_instant = 8640000000000000000000;

/// nsPerDay = 10**6 × ℝ(msPerDay) = 8.64 × 10**13
/// https://tc39.es/proposal-temporal/#eqn-nsPerDay
pub const ns_per_day = 86400000000000;

/// nsMinInstant = -nsMaxInstant = -8.64 × 10**21
/// https://tc39.es/proposal-temporal/#eqn-nsMinInstant
pub const ns_min_instant = -8640000000000000000000;

/// 8.5.2 CreateTemporalInstant ( epochNanoseconds [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalinstant
pub fn createTemporalInstant(
    agent: *Agent,
    inner: *temporal_rs.c.Instant,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Assert: IsValidEpochNanoseconds(epochNanoseconds) is true.

    // 2. If newTarget is not present, set newTarget to %Temporal.Instant%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.Instant%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.Instant.prototype%",
    //    « [[InitializedTemporalInstant]], [[EpochNanoseconds]] »).
    // 4. Set object.[[EpochNanoseconds]] to epochNanoseconds.
    // 5. Return object.
    return ordinaryCreateFromConstructor(
        Instant,
        agent,
        new_target,
        "%Temporal.Instant.prototype%",
        .{ .inner = inner },
    );
}

/// 8.5.3 ToTemporalInstant ( item )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporalinstant
pub fn toTemporalInstant(agent: *Agent, item_: Value) Agent.Error!*Object {
    var item = item_;

    // 1. If item is an Object, then
    if (item.isObject()) {
        // a. If item has an [[InitializedTemporalInstant]] or [[InitializedTemporalZonedDateTime]]
        //    internal slot, then
        if (item.asObject().is(Instant) or item.asObject().is(builtins.temporal.ZonedDateTime)) {
            // i. Return ! CreateTemporalInstant(item.[[EpochNanoseconds]]).
            const epoch_nanoseconds = if (item.asObject().is(Instant))
                temporal_rs.c.temporal_rs_Instant_epoch_nanoseconds(
                    item.asObject().as(Instant).fields.inner,
                )
            else
                temporal_rs.c.temporal_rs_ZonedDateTime_epoch_nanoseconds(
                    item.asObject().as(builtins.temporal.ZonedDateTime).fields.inner,
                );
            const temporal_rs_instant = temporal_rs.temporalErrorResult(
                temporal_rs.c.temporal_rs_Instant_try_new(epoch_nanoseconds),
            ) catch unreachable;
            errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
            return createTemporalInstant(agent, temporal_rs_instant.?, null);
        }

        // b. NOTE: This use of ToPrimitive allows Instant-like objects to be converted.
        // c. Set item to ? ToPrimitive(item, string).
        item = try item.toPrimitive(agent, .string);
    }

    // 2. If item is not a String, throw a TypeError exception.
    if (!item.isString()) {
        return agent.throwException(.type_error, "Instant must be a string or object", .{});
    }

    // 3. Let parsed be ? ParseISODateTime(item, « TemporalInstantString »).
    // 4. Assert: Either parsed.[[TimeZone]].[[OffsetString]] is not empty or
    //    parsed.[[TimeZone]].[[Z]] is true, but not both.
    // 5. If parsed.[[TimeZone]].[[Z]] is true, let offsetNanoseconds be 0; otherwise, let
    //    offsetNanoseconds be ! ParseDateTimeUTCOffset(parsed.[[TimeZone]].[[OffsetString]]).
    // 6. If parsed.[[Time]] is start-of-day, let time be MidnightTimeRecord(); else let time be
    //    parsed.[[Time]].
    // 7. Let balanced be BalanceISODateTime(parsed.[[Year]], parsed.[[Month]], parsed.[[Day]],
    //    time.[[Hour]], time.[[Minute]], time.[[Second]], time.[[Millisecond]],
    //    time.[[Microsecond]], time.[[Nanosecond]] - offsetNanoseconds).
    // 8. Perform ? CheckISODaysRange(balanced.[[ISODate]]).
    // 9. Let epochNanoseconds be GetUTCEpochNanoseconds(balanced).
    // 10. If IsValidEpochNanoseconds(epochNanoseconds) is false, throw a RangeError exception.
    // 11. Return ! CreateTemporalInstant(epochNanoseconds).
    const instant_utf8 = try item.asString().toUtf8(agent.gc_allocator);
    defer agent.gc_allocator.free(instant_utf8);
    const temporal_rs_instant = temporal_rs.temporalErrorResult(
        temporal_rs.c.temporal_rs_Instant_from_utf8(
            temporal_rs.toDiplomatStringView(instant_utf8),
        ),
    ) catch |err| switch (err) {
        error.RangeError => return agent.throwException(.range_error, "Invalid instant string", .{}),
        else => unreachable,
    };
    errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
    return createTemporalInstant(agent, temporal_rs_instant.?, null);
}
