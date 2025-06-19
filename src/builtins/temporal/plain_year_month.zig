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
const Value = types.Value;
const canonicalizeCalendar = builtins.canonicalizeCalendar;
const createBuiltinFunction = builtins.createBuiltinFunction;
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
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

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
