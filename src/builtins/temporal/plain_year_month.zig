//! 9 Temporal.PlainYearMonth Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth-objects

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

/// 9.2 Properties of the Temporal.PlainYearMonth Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainyearmonth-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            2,
            "PlainYearMonth",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 9.2.1 Temporal.PlainYearMonth.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.PlainYearMonth.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 9.1.1 Temporal.PlainYearMonth ( isoYear, isoMonth [ , calendar [ , referenceISODay ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const iso_year_value = arguments.get(0);
        const iso_month_value = arguments.get(1);
        var calendar_value = arguments.get(2);
        var reference_iso_day_value = arguments.get(3);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.PlainYearMonth must be constructed with 'new'",
                .{},
            );
        };

        // 2. If referenceISODay is undefined, then
        //     a. Set referenceISODay to 1𝔽.
        if (reference_iso_day_value.isUndefined()) reference_iso_day_value = Value.from(1);

        // 3. Let y be ? ToIntegerWithTruncation(isoYear).
        const iso_year = try iso_year_value.toIntegerWithTruncation(agent);

        // 4. Let m be ? ToIntegerWithTruncation(isoMonth).
        const iso_month = try iso_month_value.toIntegerWithTruncation(agent);

        // 5. If calendar is undefined, set calendar to "iso8601".
        if (calendar_value.isUndefined()) calendar_value = Value.from("iso8601");

        // 6. If calendar is not a String, throw a TypeError exception.
        if (!calendar_value.isString()) {
            return agent.throwException(.type_error, "Calendar is not a string", .{});
        }

        // 7. Set calendar to ? CanonicalizeCalendar(calendar).
        const calendar = try canonicalizeCalendar(agent, calendar_value.asString());

        // 8. Let ref be ? ToIntegerWithTruncation(referenceISODay).
        const reference_iso_day = try reference_iso_day_value.toIntegerWithTruncation(agent);

        // 9. If IsValidISODate(y, m, ref) is false, throw a RangeError exception.
        // 10. Let isoDate be CreateISODateRecord(y, m, ref).
        // 11. Return ? CreateTemporalYearMonth(isoDate, calendar, NewTarget).
        const temporal_rs_plain_year_month = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_PlainYearMonth_try_new_with_overflow(
                std.math.lossyCast(i32, iso_year),
                std.math.lossyCast(u8, iso_month),
                .{
                    .is_ok = true,
                    .unnamed_0 = .{ .ok = std.math.lossyCast(u8, reference_iso_day) },
                },
                calendar,
                temporal_rs.c.ArithmeticOverflow_Reject,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid year month", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_PlainYearMonth_destroy(temporal_rs_plain_year_month.?);
        return Value.from(
            try createTemporalYearMonth(agent, temporal_rs_plain_year_month.?, new_target),
        );
    }
};

/// 9.3 Properties of the Temporal.PlainYearMonth Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainyearmonth-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinAccessor(agent, "calendarId", calendarId, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInMonth", daysInMonth, null, realm);
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
        try object.defineBuiltinAccessor(agent, "year", year, null, realm);

        // 9.3.1 Temporal.PlainYearMonth.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.PlainYearMonth%"()),
        );

        // 9.3.2 Temporal.PlainYearMonth.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.PlainYearMonth"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 9.3.3 get Temporal.PlainYearMonth.prototype.calendarId
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.calendarid
    fn calendarId(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return plainYearMonth.[[Calendar]].
        const temporal_rs_calendar = temporal_rs.c.temporal_rs_PlainYearMonth_calendar(
            plain_year_month.fields.inner,
        );
        const calendar_id = temporal_rs.fromDiplomatStringView(
            temporal_rs.c.temporal_rs_Calendar_identifier(temporal_rs_calendar.?),
        );
        return Value.from(
            try String.fromAscii(agent, try agent.gc_allocator.dupe(u8, calendar_id)),
        );
    }

    /// 9.3.10 get Temporal.PlainYearMonth.prototype.daysInMonth
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.daysinmonth
    fn daysInMonth(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return 𝔽(CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[DaysInMonth]]).
        return Value.from(temporal_rs.c.temporal_rs_PlainYearMonth_days_in_month(plain_year_month.fields.inner));
    }

    /// 9.3.9 get Temporal.PlainYearMonth.prototype.daysInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.daysinyear
    fn daysInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return 𝔽(CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[DaysInYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_days_in_year(plain_year_month.fields.inner),
        );
    }

    /// 9.3.4 get Temporal.PlainYearMonth.prototype.era
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.era
    fn era(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[Era]].
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainYearMonth_era(plain_year_month.fields.inner, &write.inner);
        if (write.inner.len == 0) {
            std.debug.assert(write.inner.cap == 0); // Nothing to free
            return .undefined;
        }
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 9.3.5 get Temporal.PlainYearMonth.prototype.eraYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.erayear
    fn eraYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Let result be CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[EraYear]].
        const result = temporal_rs.c.temporal_rs_PlainYearMonth_era_year(
            plain_year_month.fields.inner,
        );

        // 4. If result is undefined, return undefined.
        if (!result.is_ok) return .undefined;

        // 5. Return 𝔽(result).
        return Value.from(result.unnamed_0.ok);
    }

    /// 9.3.12 get Temporal.PlainYearMonth.prototype.inLeapYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.inleapyear
    fn inLeapYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[InLeapYear]].
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_in_leap_year(plain_year_month.fields.inner),
        );
    }

    /// 9.3.7 get Temporal.PlainYearMonth.prototype.month
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.month
    fn month(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return 𝔽(CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[MOnth]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_month(plain_year_month.fields.inner),
        );
    }

    /// 9.3.8 get Temporal.PlainYearMonth.prototype.monthCode
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.monthcode
    fn monthCode(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[MonthCode]].
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainYearMonth_month_code(
            plain_year_month.fields.inner,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 9.3.11 get Temporal.PlainYearMonth.prototype.monthsInYear
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.monthsinyear
    fn monthsInYear(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return 𝔽(CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[MonthsInYear]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_months_in_year(plain_year_month.fields.inner),
        );
    }

    /// 9.3.21 Temporal.PlainYearMonth.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return TemporalYearMonthToString(plainYearMonth, auto).
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainYearMonth_to_ixdtf_string(
            plain_year_month.fields.inner,
            temporal_rs.c.DisplayCalendar_Auto,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 9.3.20 Temporal.PlainYearMonth.prototype.toLocaleString ( [ locales [ , options ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tolocalestring
    fn toLocaleString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return TemporalYearMonthToString(plainYearMonth, auto).
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainYearMonth_to_ixdtf_string(
            plain_year_month.fields.inner,
            temporal_rs.c.DisplayCalendar_Auto,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 9.3.19 Temporal.PlainYearMonth.prototype.toString ( [ options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const options_value = arguments.get(0);

        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 4. Let showCalendar be ? GetTemporalShowCalendarNameOption(resolvedOptions).
        const show_calendar = try getTemporalShowCalendarNameOption(agent, options);

        // 5. Return TemporalYearMonthToString(plainYearMonth, showCalendar).
        var context: temporal_rs.DiplomatWrite.Context = .{ .gpa = agent.gc_allocator };
        var write = temporal_rs.DiplomatWrite.init(&context);
        temporal_rs.c.temporal_rs_PlainYearMonth_to_ixdtf_string(
            plain_year_month.fields.inner,
            show_calendar,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 9.3.22 Temporal.PlainYearMonth.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.PlainYearMonth to primitive value",
            .{},
        );
    }

    /// 9.3.6 get Temporal.PlainYearMonth.prototype.year
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.year
    fn year(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return 𝔽(CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[Year]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_year(plain_year_month.fields.inner),
        );
    }
};

/// 9.4 Properties of Temporal.PlainYearMonth Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-plainyearmonth-instances
pub const PlainYearMonth = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.PlainYearMonth,
    },
    .tag = .temporal_plain_year_month,
});

/// 9.5.5 CreateTemporalYearMonth ( isoDate, calendar [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalyearmonth
pub fn createTemporalYearMonth(
    agent: *Agent,
    inner: *temporal_rs.c.PlainYearMonth,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If ISOYearMonthWithinLimits(isoDate) is false, throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.PlainYearMonth%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.PlainYearMonth%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.PlainYearMonth.prototype%",
    //    « [[InitializedTemporalYearMonth]], [[ISODate]], [[Calendar]] »).
    // 4. Set object.[[ISODate]] to isoDate.
    // 5. Set object.[[Calendar]] to calendar.
    // 6. Return object.
    return ordinaryCreateFromConstructor(
        PlainYearMonth,
        agent,
        new_target,
        "%Temporal.PlainYearMonth.prototype%",
        .{ .inner = inner },
    );
}
