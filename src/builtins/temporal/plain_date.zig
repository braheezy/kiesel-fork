//! 3 Temporal.PlainDate Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plaindate-objects

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
const getTemporalShowCalendarNameOption = builtins.getTemporalShowCalendarNameOption;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 3.2 Properties of the Temporal.PlainDate Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            3,
            "PlainDate",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 3.2.1 Temporal.PlainDate.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDate.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 3.1.1 Temporal.PlainDate ( isoYear, isoMonth, isoDay [ , calendar ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const iso_year_value = arguments.get(0);
        const iso_month_value = arguments.get(1);
        const iso_day_value = arguments.get(2);
        var calendar_value = arguments.get(3);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.PlainDate must be constructed with 'new'",
                .{},
            );
        };

        // 2. Let y be ? ToIntegerWithTruncation(isoYear).
        const iso_year = try iso_year_value.toIntegerWithTruncation(agent);

        // 3. Let m be ? ToIntegerWithTruncation(isoMonth).
        const iso_month = try iso_month_value.toIntegerWithTruncation(agent);

        // 4. Let d be ? ToIntegerWithTruncation(isoDay).
        const iso_day = try iso_day_value.toIntegerWithTruncation(agent);

        // 5. If calendar is undefined, set calendar to "iso8601".
        if (calendar_value.isUndefined()) calendar_value = Value.from("iso8601");

        // 6. If calendar is not a String, throw a TypeError exception.
        if (!calendar_value.isString()) {
            return agent.throwException(.type_error, "Calendar is not a string", .{});
        }

        // 7. Set calendar to ? CanonicalizeCalendar(calendar).
        const calendar = try canonicalizeCalendar(agent, calendar_value.asString());

        // 8. If IsValidISODate(y, m, d) is false, throw a RangeError exception.
        // 9. Let isoDate be CreateISODateRecord(y, m, d).
        // 10. Return ? CreateTemporalDate(isoDate, calendar, NewTarget).
        const temporal_rs_plain_date = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDate_try_new(
                std.math.lossyCast(i32, iso_year),
                std.math.lossyCast(u8, iso_month),
                std.math.lossyCast(u8, iso_day),
                calendar,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid date", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_PlainDate_destroy(temporal_rs_plain_date.?);
        std.debug.assert(temporal_rs.c.temporal_rs_PlainDate_is_valid(temporal_rs_plain_date.?));
        return Value.from(
            try createTemporalDate(agent, temporal_rs_plain_date.?, new_target),
        );
    }
};

/// 3.3 Properties of the Temporal.PlainDate Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-prototype-object
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
        try object.defineBuiltinAccessor(agent, "era", era, null, realm);
        try object.defineBuiltinAccessor(agent, "eraYear", eraYear, null, realm);
        try object.defineBuiltinAccessor(agent, "inLeapYear", inLeapYear, null, realm);
        try object.defineBuiltinAccessor(agent, "month", month, null, realm);
        try object.defineBuiltinAccessor(agent, "monthCode", monthCode, null, realm);
        try object.defineBuiltinAccessor(agent, "monthsInYear", monthsInYear, null, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);
        try object.defineBuiltinAccessor(agent, "weekOfYear", weekOfYear, null, realm);
        try object.defineBuiltinAccessor(agent, "year", year, null, realm);
        try object.defineBuiltinAccessor(agent, "yearOfWeek", yearOfWeek, null, realm);

        // 3.3.1 Temporal.PlainDate.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDate%"()),
        );

        // 3.3.2 Temporal.PlainDate.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.PlainDate"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 3.3.3 get Temporal.PlainDate.prototype.calendarId
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.calendarid
    fn calendarId(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return plainDate.[[Calendar]].
        const temporal_rs_calendar = temporal_rs.c.temporal_rs_PlainDate_calendar(
            plain_date.fields.inner,
        );
        const calendar_id = temporal_rs.fromDiplomatStringView(
            temporal_rs.c.temporal_rs_Calendar_identifier(temporal_rs_calendar.?),
        );
        return Value.from(
            try String.fromAscii(agent, try agent.gc_allocator.dupe(u8, calendar_id)),
        );
    }

    /// 3.3.9 get Temporal.PlainDate.prototype.day
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.day
    fn day(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[Day]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_day(plain_date.fields.inner));
    }

    /// 3.3.10 get Temporal.PlainDate.prototype.dayOfWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.dayofweek
    fn dayOfWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[DayOfWeek]]).
        const day_of_week = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDate_day_of_week(plain_date.fields.inner),
        ) catch |err| switch (err) {
            // https://github.com/boa-dev/temporal/blob/531cee14769e8c077c59a1faf67d5465e85f5afa/src/builtins/core/calendar.rs#L406-L407
            error.RangeError => {
                return agent.throwException(.internal_error, "Not implemented", .{});
            },
            else => unreachable,
        };
        return Value.from(day_of_week);
    }

    /// 3.3.11 get Temporal.PlainDate.prototype.dayOfYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.dayofyear
    fn dayOfYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[DayOfYear]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_day_of_year(plain_date.fields.inner));
    }

    /// 3.3.15 get Temporal.PlainDate.prototype.daysInMonth
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinmonth
    fn daysInMonth(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[DaysInMonth]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_days_in_month(plain_date.fields.inner));
    }

    /// 3.3.14 get Temporal.PlainDate.prototype.daysInWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinweek
    fn daysInWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[DaysInWeek]]).
        const days_in_week = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainDate_days_in_week(plain_date.fields.inner),
        ) catch |err| switch (err) {
            // https://github.com/boa-dev/temporal/blob/531cee14769e8c077c59a1faf67d5465e85f5afa/src/builtins/core/calendar.rs#L442-L443
            error.RangeError => {
                return agent.throwException(.internal_error, "Not implemented", .{});
            },
            else => unreachable,
        };
        return Value.from(days_in_week);
    }

    /// 3.3.16 get Temporal.PlainDate.prototype.daysInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinyear
    fn daysInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[DaysInYear]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_days_in_year(plain_date.fields.inner));
    }

    /// 3.3.4 get Temporal.PlainDate.prototype.era
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.era
    fn era(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[Era]].
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainDate_era(plain_date.fields.inner, &write.inner);
        if (write.inner.len == 0) {
            std.debug.assert(write.inner.cap == 0); // Nothing to free
            return .undefined;
        }
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 3.3.5 get Temporal.PlainDate.prototype.eraYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.erayear
    fn eraYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Let result be CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[EraYear]].
        const result = temporal_rs.c.temporal_rs_PlainDate_era_year(plain_date.fields.inner);

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 3.3.18 get Temporal.PlainDate.prototype.inLeapYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.inleapyear
    fn inLeapYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[inLeapYear]].
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_in_leap_year(plain_date.fields.inner));
    }

    /// 3.3.7 get Temporal.PlainDate.prototype.month
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.month
    fn month(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[Month]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_month(plain_date.fields.inner));
    }

    /// 3.3.8 get Temporal.PlainDate.prototype.monthCode
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.monthcode
    fn monthCode(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[MonthCode]].
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainDate_month_code(plain_date.fields.inner, &write.inner);
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 3.3.17 get Temporal.PlainDate.prototype.monthsInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.monthsinyear
    fn monthsInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[MonthsInYear]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_months_in_year(plain_date.fields.inner));
    }

    /// 3.3.32 Temporal.PlainDate.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return TemporalDateToString(plainDate, auto).
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainDate_to_ixdtf_string(
            plain_date.fields.inner,
            temporal_rs.c.DisplayCalendar_Auto,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 3.3.31 Temporal.PlainDate.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return TemporalDateToString(plainDate, auto).
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainDate_to_ixdtf_string(
            plain_date.fields.inner,
            temporal_rs.c.DisplayCalendar_Auto,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 3.3.30 Temporal.PlainDate.prototype.toString ( [ options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const options_value = arguments.get(0);

        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 4. Let showCalendar be ? GetTemporalShowCalendarNameOption(resolvedOptions).
        const show_calendar = try getTemporalShowCalendarNameOption(agent, options);

        // 5. Return TemporalDateToString(plainDate, showCalendar).
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainDate_to_ixdtf_string(
            plain_date.fields.inner,
            show_calendar,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 3.3.33 Temporal.PlainDate.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.PlainDate to primitive value",
            .{},
        );
    }

    /// 3.3.12 get Temporal.PlainDate.prototype.weekOfYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.weekofyear
    fn weekOfYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Let result be CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[WeekOfYear]].[[Week]].
        const result = temporal_rs.c.temporal_rs_PlainDate_week_of_year(plain_date.fields.inner);

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 3.3.6 get Temporal.PlainDate.prototype.year
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.year
    fn year(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Return 𝔽(CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[Year]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainDate_year(plain_date.fields.inner));
    }

    /// 3.3.13 get Temporal.PlainDate.prototype.yearOfWeek
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.yearofweek
    fn yearOfWeek(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainDate be the this value.
        // 2. Perform ? RequireInternalSlot(plainDate, [[InitializedTemporalDate]]).
        const plain_date = try this_value.requireInternalSlot(agent, PlainDate);

        // 3. Let result be CalendarISOToDate(plainDate.[[Calendar]], plainDate.[[ISODate]]).[[WeekOfYear]].[[Year]].
        const result = temporal_rs.c.temporal_rs_PlainDate_year_of_week(plain_date.fields.inner);

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }
};

/// 3.4 Properties of Temporal.PlainDate Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plaindate-instances
pub const PlainDate = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.PlainDate,
    },
    .tag = .temporal_plain_date,
});

/// 3.5.3 CreateTemporalDate ( isoDate, calendar [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporaldate
pub fn createTemporalDate(
    agent: *Agent,
    inner: *temporal_rs.c.PlainDate,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If ISODateWithinLimits(isoDate) is false, throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.PlainDate%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.PlainDate%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.PlainDate.prototype%",
    //    « [[InitializedTemporalDate]], [[ISODate]], [[Calendar]] »).
    // 4. Set object.[[ISODate]] to isoDate.
    // 5. Set object.[[Calendar]] to calendar.
    // 6. Return object.
    return ordinaryCreateFromConstructor(
        PlainDate,
        agent,
        new_target,
        "%Temporal.PlainDate.prototype%",
        .{ .inner = inner },
    );
}
