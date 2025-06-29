//! 6 Temporal.ZonedDateTime Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime-objects

const std = @import("std");

const temporal_rs = @import("../../c/temporal_rs.zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeCalendar = builtins.canonicalizeCalendar;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

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
        if (!temporal_rs.c.temporal_rs_TimeZone_is_valid(temporal_rs_time_zone.?) or
            // https://github.com/boa-dev/temporal/blob/3455373e250dc3c3c5ee0112f379bbc97d7c351c/src/builtins/core/timezone.rs#L109-L111
            std.mem.eql(u8, time_zone, "Z"))
        {
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
        try object.defineBuiltinAccessor(agent, "timeZoneId", timeZoneId, null, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

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
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_TimeZone_identifier(temporal_rs_time_zone.?, &write.inner),
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
};

/// 6.4 Properties of Temporal.ZonedDateTime Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-zoneddatetime-instances
pub const ZonedDateTime = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.ZonedDateTime,
    },
    .tag = .temporal_zoned_date_time,
});

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
