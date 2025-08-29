//! 9 Temporal.PlainYearMonth Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth-objects

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
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeCalendar = builtins.canonicalizeCalendar;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createTemporalDate = builtins.createTemporalDate;
const getTemporalCalendarIdentifierWithISODefault = builtins.getTemporalCalendarIdentifierWithISODefault;
const getTemporalOverflowOption = builtins.getTemporalOverflowOption;
const getTemporalShowCalendarNameOption = builtins.getTemporalShowCalendarNameOption;
const isValidISODate = builtins.isValidISODate;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const prepareCalendarFields = builtins.prepareCalendarFields;
const toTemporalDuration = builtins.toTemporalDuration;

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
        try object.defineBuiltinFunction(agent, "compare", compare, 2, realm);
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);

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
        if (!isValidISODate(iso_year, iso_month, reference_iso_day)) {
            return agent.throwException(.range_error, "Invalid ISO date", .{});
        }

        // 10. Let isoDate be CreateISODateRecord(y, m, ref).
        // 11. Return ? CreateTemporalYearMonth(isoDate, calendar, NewTarget).
        const temporal_rs_plain_year_month = try temporal_rs.extractResult(
            agent,
            temporal_rs.c.temporal_rs_PlainYearMonth_try_new_with_overflow(
                @intFromFloat(iso_year),
                @intFromFloat(iso_month),
                .{
                    .is_ok = true,
                    .unnamed_0 = .{ .ok = @intFromFloat(reference_iso_day) },
                },
                calendar,
                temporal_rs.c.ArithmeticOverflow_Reject,
            ),
        );
        errdefer temporal_rs.c.temporal_rs_PlainYearMonth_destroy(temporal_rs_plain_year_month.?);
        return Value.from(
            try createTemporalYearMonth(agent, temporal_rs_plain_year_month.?, new_target),
        );
    }

    /// 9.2.3 Temporal.PlainYearMonth.compare ( one, two )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.compare
    fn compare(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const one_value = arguments.get(0);
        const two_value = arguments.get(1);

        // 1. Set one to ? ToTemporalYearMonth(one).
        const one = try toTemporalPlainYearMonth(agent, one_value, null);

        // 2. Set two to ? ToTemporalYearMonth(two).
        const two = try toTemporalPlainYearMonth(agent, two_value, null);

        // 3. Return 𝔽(CompareISODate(one.[[ISODate]], two.[[ISODate]])).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_compare(
                one.as(PlainYearMonth).fields.inner,
                two.as(PlainYearMonth).fields.inner,
            ),
        );
    }

    /// 9.2.2 Temporal.PlainYearMonth.from ( item [ , options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.from
    fn from(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const item = arguments.get(0);
        const options = arguments.get(1);

        // 1. Return ? ToTemporalYearMonth(item, options).
        return Value.from(try toTemporalPlainYearMonth(agent, item, options));
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
        try object.defineBuiltinFunction(agent, "add", add, 1, realm);
        try object.defineBuiltinAccessor(agent, "calendarId", calendarId, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInMonth", daysInMonth, null, realm);
        try object.defineBuiltinAccessor(agent, "daysInYear", daysInYear, null, realm);
        try object.defineBuiltinFunction(agent, "equals", equals, 1, realm);
        try object.defineBuiltinAccessor(agent, "era", era, null, realm);
        try object.defineBuiltinAccessor(agent, "eraYear", eraYear, null, realm);
        try object.defineBuiltinAccessor(agent, "inLeapYear", inLeapYear, null, realm);
        try object.defineBuiltinAccessor(agent, "month", month, null, realm);
        try object.defineBuiltinAccessor(agent, "monthCode", monthCode, null, realm);
        try object.defineBuiltinAccessor(agent, "monthsInYear", monthsInYear, null, realm);
        try object.defineBuiltinFunction(agent, "subtract", subtract, 1, realm);
        try object.defineBuiltinFunction(agent, "toJSON", toJSON, 0, realm);
        try object.defineBuiltinFunction(agent, "toLocaleString", toLocaleString, 0, realm);
        try object.defineBuiltinFunction(agent, "toPlainDate", toPlainDate, 1, realm);
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

    /// 9.3.14 Temporal.PlainYearMonth.prototype.add ( temporalDurationLike [ , options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.add
    fn add(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const temporal_duration_like = arguments.get(0);
        const options = arguments.get(1);

        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return ? AddDurationToYearMonth(add, plainYearMonth, temporalDurationLike, options).
        return Value.from(
            try addDurationToYearMonth(
                agent,
                .add,
                plain_year_month.fields.inner,
                temporal_duration_like,
                options,
            ),
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

    /// 9.3.18 Temporal.PlainYearMonth.prototype.equals ( other )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.equals
    fn equals(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const other_value = arguments.get(0);

        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Set other to ? ToTemporalYearMonth(other).
        const other = try toTemporalPlainYearMonth(agent, other_value, null);

        // 4. If CompareISODate(plainYearMonth.[[ISODate]], other.[[ISODate]]) ≠ 0, return false.
        // 5. Return CalendarEquals(plainYearMonth.[[Calendar]], other.[[Calendar]]).
        return Value.from(
            temporal_rs.c.temporal_rs_PlainYearMonth_equals(
                plain_year_month.fields.inner,
                other.as(PlainYearMonth).fields.inner,
            ),
        );
    }

    /// 9.3.4 get Temporal.PlainYearMonth.prototype.era
    /// https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.era
    fn era(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return CalendarISOToDate(plainYearMonth.[[Calendar]], plainYearMonth.[[ISODate]]).[[Era]].
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
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
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
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

    /// 9.3.15 Temporal.PlainYearMonth.prototype.subtract ( temporalDurationLike [ , options ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.subtract
    fn subtract(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const temporal_duration_like = arguments.get(0);
        const options = arguments.get(1);

        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return ? AddDurationToYearMonth(subtract, plainYearMonth, temporalDurationLike, options).
        return Value.from(
            try addDurationToYearMonth(
                agent,
                .subtract,
                plain_year_month.fields.inner,
                temporal_duration_like,
                options,
            ),
        );
    }

    /// 9.3.21 Temporal.PlainYearMonth.prototype.toJSON ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tojson
    fn toJSON(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. Return TemporalYearMonthToString(plainYearMonth, auto).
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
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
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_PlainYearMonth_to_ixdtf_string(
            plain_year_month.fields.inner,
            temporal_rs.c.DisplayCalendar_Auto,
            &write.inner,
        );
        return Value.from(try String.fromAscii(agent, try write.toOwnedSlice()));
    }

    /// 9.3.23 Temporal.PlainYearMonth.prototype.toPlainDate ( item )
    /// https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.toplaindate
    fn toPlainDate(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const item = arguments.get(0);

        // 1. Let plainYearMonth be the this value.
        // 2. Perform ? RequireInternalSlot(plainYearMonth, [[InitializedTemporalYearMonth]]).
        const plain_year_month = try this_value.requireInternalSlot(agent, PlainYearMonth);

        // 3. If item is not an Object, then
        if (!item.isObject()) {
            // a. Throw a TypeError exception.
            return agent.throwException(.type_error, "Item must be an object", .{});
        }

        // 4. Let calendar be plainYearMonth.[[Calendar]].
        const calendar = temporal_rs.c.temporal_rs_Calendar_kind(
            temporal_rs.c.temporal_rs_PlainYearMonth_calendar(plain_year_month.fields.inner),
        );

        // 5. Let fields be ISODateToFields(calendar, plainYearMonth.[[ISODate]], year-month).
        // 6. Let inputFields be ? PrepareCalendarFields(calendar, item, « day », « », « »).
        // 7. Let mergedFields be CalendarMergeFields(calendar, fields, inputFields).
        const fields = try prepareCalendarFields(
            agent,
            calendar,
            item.asObject(),
            .initOne(.day),
            .none,
        );
        const partial = fields.date;

        // 8. Let isoDate be ? CalendarDateFromFields(calendar, mergedFields, constrain).
        const temporal_rs_plain_date = try temporal_rs.extractResult(
            agent,
            temporal_rs.c.temporal_rs_PlainYearMonth_to_plain_date(
                plain_year_month.fields.inner,
                .{ .is_ok = true, .unnamed_0 = .{ .ok = partial } },
            ),
        );
        errdefer temporal_rs.c.temporal_rs_PlainDate_destroy(temporal_rs_plain_date.?);

        // 9. Return ! CreateTemporalDate(isoDate, calendar).
        return Value.from(
            createTemporalDate(
                agent,
                temporal_rs_plain_date.?,
                null,
            ) catch |err| try noexcept(err),
        );
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
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
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
        inner: *temporal_rs.c.PlainYearMonth,
    },
    .finalizer = struct {
        fn finalizer(object: *Object) void {
            temporal_rs.c.temporal_rs_PlainYearMonth_destroy(object.as(PlainYearMonth).fields.inner);
        }
    }.finalizer,
    .tag = .temporal_plain_year_month,
});

/// 9.5.2 ToTemporalYearMonth ( item [ , options ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-totemporalyearmonth
pub fn toTemporalPlainYearMonth(
    agent: *Agent,
    item: Value,
    maybe_options_value: ?Value,
) Agent.Error!*Object {
    // 1. If options is not present, set options to undefined.
    const options_value: Value = maybe_options_value orelse .undefined;

    // 2. If item is an Object, then
    const temporal_rs_plain_year_month = if (item.isObject()) blk: {
        // a. If item has an [[InitializedTemporalYearMonth]] internal slot, then
        if (item.asObject().is(PlainYearMonth)) {
            // i. Let resolvedOptions be ? GetOptionsObject(options).
            const options = try options_value.getOptionsObject(agent);

            // ii. Perform ? GetTemporalOverflowOption(resolvedOptions).
            _ = try getTemporalOverflowOption(agent, options);

            // iii. Return ! CreateTemporalYearMonth(item.[[ISODate]], item.[[Calendar]]).
            const plain_year_month = item.asObject().as(PlainYearMonth);
            break :blk temporal_rs.c.temporal_rs_PlainYearMonth_clone(
                plain_year_month.fields.inner,
            );
        }

        // b. Let calendar be ? GetTemporalCalendarIdentifierWithISODefault(item).
        const calendar = try getTemporalCalendarIdentifierWithISODefault(agent, item.asObject());

        // c. Let fields be ? PrepareCalendarFields(calendar, item, « year, month, month-code », «», «»).
        const fields = try prepareCalendarFields(
            agent,
            calendar,
            item.asObject(),
            .initMany(&.{ .year, .month, .month_code }),
            .none,
        );
        const partial = fields.date;

        // d. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // e. Let overflow be ? GetTemporalOverflowOption(resolvedOptions).
        const overflow = try getTemporalOverflowOption(agent, options);

        // f. Let isoDate be ? CalendarYearMonthFromFields(calendar, fields, overflow).
        // g. Return ! CreateTemporalYearMonth(isoDate, calendar).
        break :blk try temporal_rs.extractResult(
            agent,
            temporal_rs.c.temporal_rs_PlainYearMonth_from_partial(
                partial,
                .{ .is_ok = true, .unnamed_0 = .{ .ok = overflow } },
            ),
        );
    } else blk: {
        // 3. If item is not a String, throw a TypeError exception.
        if (!item.isString()) {
            return agent.throwException(
                .type_error,
                "Plain yearmonth must be a string or object",
                .{},
            );
        }

        // 4. Let result be ? ParseISODateTime(item, « TemporalYearMonthString »).
        // 5. Let calendar be result.[[Calendar]].
        // 6. If calendar is empty, set calendar to "iso8601".
        // 7. Set calendar to ? CanonicalizeCalendar(calendar).
        const parsed_year_month = switch (item.asString().slice) {
            .ascii => |ascii| try temporal_rs.extractResult(
                agent,
                temporal_rs.c.temporal_rs_ParsedDate_year_month_from_utf8(
                    temporal_rs.toDiplomatStringView(ascii),
                ),
            ),
            .utf16 => |utf16| try temporal_rs.extractResult(
                agent,
                temporal_rs.c.temporal_rs_ParsedDate_year_month_from_utf16(
                    temporal_rs.toDiplomatString16View(utf16),
                ),
            ),
        };
        defer temporal_rs.c.temporal_rs_ParsedDate_destroy(parsed_year_month.?);

        // 8. Let resolvedOptions be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 9. Perform ? GetTemporalOverflowOption(resolvedOptions).
        _ = try getTemporalOverflowOption(agent, options);

        // 10. Let isoDate be CreateISODateRecord(result.[[Year]], result.[[Month]], result.[[Day]]).
        // 11. If ISOYearMonthWithinLimits(isoDate) is false, throw a RangeError exception.
        // 12. Set result to ISODateToFields(calendar, isoDate, year-month).
        // 13. NOTE: The following operation is called with constrain regardless of the value of
        //     overflow, in order for the calendar to store a canonical value in the [[Day]] field
        //     of the [[ISODate]] internal slot of the result.
        // 14. Set isoDate to ? CalendarYearMonthFromFields(calendar, result, constrain).
        // 15. Return ! CreateTemporalYearMonth(isoDate, calendar).
        break :blk try temporal_rs.extractResult(
            agent,
            temporal_rs.c.temporal_rs_PlainYearMonth_from_parsed(parsed_year_month.?),
        );
    };
    errdefer temporal_rs.c.temporal_rs_PlainYearMonth_destroy(temporal_rs_plain_year_month.?);

    return createTemporalYearMonth(
        agent,
        temporal_rs_plain_year_month.?,
        null,
    ) catch |err| try noexcept(err);
}

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

/// 9.5.8 AddDurationToYearMonth ( operation, yearMonth, temporalDurationLike, options )
/// https://tc39.es/proposal-temporal/#sec-temporal-adddurationtoyearmonth
pub fn addDurationToYearMonth(
    agent: *Agent,
    comptime operation: enum { add, subtract },
    plain_year_month: *const temporal_rs.c.PlainYearMonth,
    temporal_duration_like: Value,
    options_value: Value,
) Agent.Error!*Object {
    // 1. Let duration be ? ToTemporalDuration(temporalDurationLike).
    const duration = try toTemporalDuration(agent, temporal_duration_like);

    // 2. If operation is subtract, set duration to CreateNegatedTemporalDuration(duration).

    // 3. Let resolvedOptions be ? GetOptionsObject(options).
    const options = try options_value.getOptionsObject(agent);

    // 4. Let overflow be ? GetTemporalOverflowOption(resolvedOptions).
    const overflow = try getTemporalOverflowOption(agent, options);

    // 5. Let sign be DurationSign(duration).
    // 6. Let calendar be yearMonth.[[Calendar]].
    // 7. Let fields be ISODateToFields(calendar, yearMonth.[[ISODate]], year-month).
    // 8. Set fields.[[Day]] to 1.
    // 9. Let intermediateDate be ? CalendarDateFromFields(calendar, fields, constrain).
    // 10. If sign < 0, then
    //     a. Let oneMonthDuration be ! CreateDateDurationRecord(0, 1, 0, 0).
    //     b. Let nextMonth be ? CalendarDateAdd(calendar, intermediateDate, oneMonthDuration, constrain).
    //     c. Let date be BalanceISODate(nextMonth.[[Year]], nextMonth.[[Month]], nextMonth.[[Day]] - 1).
    //     d. Assert: ISODateWithinLimits(date) is true.
    // 11. Else,
    //     a. Let date be intermediateDate.
    // 12. Let durationToAdd be ToDateDurationRecordWithoutTime(duration).
    // 13. Let addedDate be ? CalendarDateAdd(calendar, date, durationToAdd, overflow).
    // 14. Let addedDateFields be ISODateToFields(calendar, addedDate, year-month).
    // 15. Let isoDate be ? CalendarYearMonthFromFields(calendar, addedDateFields, overflow).
    // 16. Return ! CreateTemporalYearMonth(isoDate, calendar).
    const temporal_rs_plain_year_month = switch (operation) {
        .add => try temporal_rs.extractResult(
            agent,
            temporal_rs.c.temporal_rs_PlainYearMonth_add(
                plain_year_month,
                duration.as(builtins.temporal.Duration).fields.inner,
                overflow,
            ),
        ),
        .subtract => try temporal_rs.extractResult(
            agent,
            temporal_rs.c.temporal_rs_PlainYearMonth_subtract(
                plain_year_month,
                duration.as(builtins.temporal.Duration).fields.inner,
                overflow,
            ),
        ),
    };
    errdefer temporal_rs.c.temporal_rs_PlainYearMonth_destroy(temporal_rs_plain_year_month.?);
    return createTemporalYearMonth(
        agent,
        temporal_rs_plain_year_month.?,
        null,
    ) catch |err| try noexcept(err);
}
