//! 13 DurationFormat Objects
//! https://tc39.es/ecma402/#durationformat-objects

const std = @import("std");

const icu4zig = @import("icu4zig");

const abstract_operations = @import("abstract_operations.zig");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getNumberOption = abstract_operations.getNumberOption;
const matchUnicodeLocaleIdentifierType = abstract_operations.matchUnicodeLocaleIdentifierType;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 13.2 Properties of the Intl.DurationFormat Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-durationformat-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "DurationFormat",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 13.2.1 Intl.DurationFormat.prototype
        // https://tc39.es/ecma402/#sec-Intl.DurationFormat.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Intl.DurationFormat.prototype%"()),
            .none,
        );
    }

    /// 13.1.1 Intl.DurationFormat ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl-durationformat-constructor
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Intl.DurationFormat must be constructed with 'new'",
                .{},
            );
        };

        // 2. Let durationFormat be ? OrdinaryCreateFromConstructor(NewTarget,
        //    "%Intl.DurationFormatPrototype%", « [[InitializedDurationFormat]], [[Locale]],
        //    [[NumberingSystem]], [[Style]], [[YearsOptions]], [[MonthsOptions]],
        //    [[WeeksOptions]], [[DaysOptions]], [[HoursOptions]], [[MinutesOptions]],
        //    [[SecondsOptions]], [[MillisecondsOptions]], [[MicrosecondsOptions]],
        //    [[NanosecondsOptions]], [[HourMinuteSeparator]], [[MinuteSecondSeparator]],
        //    [[FractionalDigits]] »).
        const duration_format = try ordinaryCreateFromConstructor(
            DurationFormat,
            agent,
            new_target,
            "%Intl.DurationFormat.prototype%",
            .{
                .locale = undefined,
                .numbering_system = undefined,
                .style = undefined,
                .years_options = undefined,
                .months_options = undefined,
                .weeks_options = undefined,
                .days_options = undefined,
                .hours_options = undefined,
                .minutes_options = undefined,
                .seconds_options = undefined,
                .milliseconds_options = undefined,
                .microseconds_options = undefined,
                .nanoseconds_options = undefined,
                .hour_minute_separator = undefined,
                .minute_second_separator = undefined,
                .fractional_digits = undefined,
            },
        );

        // 3. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 4. Let options be ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 5. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
        //    "best fit").
        const matcher = try options.getOption(
            agent,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // 6. Let numberingSystem be ? GetOption(options, "numberingSystem", string, empty,
        //    undefined).
        const maybe_numbering_system = try options.getOption(
            agent,
            "numberingSystem",
            .string,
            null,
            null,
        );

        // 7. If numberingSystem is not undefined, then
        if (maybe_numbering_system) |numbering_system| {
            // a. If numberingSystem cannot be matched by the type Unicode locale nonterminal, throw a
            //    RangeError exception.
            if (!matchUnicodeLocaleIdentifierType(try numbering_system.toUtf8(agent.gc_allocator))) {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier type '{f}'",
                    .{numbering_system.fmtUnquoted()},
                );
            }
        }

        // 8. Let opt be the Record { [[localeMatcher]]: matcher, [[nu]]: numberingSystem }.
        // TODO: 9. Let r be ResolveLocale(%Intl.DurationFormat%.[[AvailableLocales]], requestedLocales,
        //          opt, %Intl.DurationFormat%.[[RelevantExtensionKeys]], %Intl.DurationFormat%.[[LocaleData]]).
        _ = matcher;
        const resolved_locale = if (requested_locales.items.len != 0)
            requested_locales.items[0]
        else
            agent.platform.default_locale;
        const resolved = .{
            .locale = resolved_locale,
            .numbering_system = if (try resolved_locale.getUnicodeExtension(agent.gc_allocator, "nu")) |nu|
                try String.fromAscii(agent, nu)
            else
                maybe_numbering_system orelse String.fromLiteral("latn"),
        };

        // 10. Set durationFormat.[[Locale]] to r.[[Locale]].
        duration_format.as(DurationFormat).fields.locale = resolved.locale;

        // TODO: 11. Let resolvedLocaleData be r.[[LocaleData]].
        // 12. Let digitalFormat be resolvedLocaleData.[[DigitalFormat]].
        const digital_format = .{
            .hour_minute_separator = ':',
            .minute_second_separator = ':',
            .two_digit_hours = false,
        };

        // 13. Set durationFormat.[[HourMinuteSeparator]] to digitalFormat.[[HourMinuteSeparator]].
        duration_format.as(DurationFormat).fields.hour_minute_separator = digital_format.hour_minute_separator;

        // 14. Set durationFormat.[[MinuteSecondSeparator]] to digitalFormat.[[MinuteSecondSeparator]].
        duration_format.as(DurationFormat).fields.minute_second_separator = digital_format.minute_second_separator;

        // 15. Set durationFormat.[[NumberingSystem]] to r.[[nu]].
        duration_format.as(DurationFormat).fields.numbering_system = resolved.numbering_system;

        // 16. Let style be ? GetOption(options, "style", string, « "long", "short", "narrow",
        //     "digital" », "short").
        const style = try options.getOption(
            agent,
            "style",
            .string,
            &.{
                String.fromLiteral("long"),
                String.fromLiteral("short"),
                String.fromLiteral("narrow"),
                String.fromLiteral("digital"),
            },
            String.fromLiteral("short"),
        );

        // 17. Set durationFormat.[[Style]] to style.
        const style_map = std.StaticStringMap(
            DurationFormat.Fields.Style,
        ).initComptime(&.{
            .{ "long", .long },
            .{ "short", .short },
            .{ "narrow", .narrow },
            .{ "digital", .digital },
        });
        duration_format.as(DurationFormat).fields.style = style_map.get(style.slice.ascii).?;

        // 18. Let prevStyle be the empty String.
        var prev_style = String.empty;

        // 19. For each row of Table 20, except the header row, in table order, do
        const Row = struct { Unit, []const *const String, *const String };
        inline for (comptime [_]Row{
            .{
                .years,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                },
                String.fromLiteral("short"),
            },
            .{
                .months,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                },
                String.fromLiteral("short"),
            },
            .{
                .weeks,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                },
                String.fromLiteral("short"),
            },
            .{
                .days,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                },
                String.fromLiteral("short"),
            },
            .{
                .hours,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                    String.fromLiteral("numeric"),
                    String.fromLiteral("2-digit"),
                },
                String.fromLiteral("numeric"),
            },
            .{
                .minutes,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                    String.fromLiteral("numeric"),
                    String.fromLiteral("2-digit"),
                },
                String.fromLiteral("numeric"),
            },
            .{
                .seconds,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                    String.fromLiteral("numeric"),
                    String.fromLiteral("2-digit"),
                },
                String.fromLiteral("numeric"),
            },
            .{
                .milliseconds,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                    String.fromLiteral("numeric"),
                },
                String.fromLiteral("numeric"),
            },
            .{
                .microseconds,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                    String.fromLiteral("numeric"),
                },
                String.fromLiteral("numeric"),
            },
            .{
                .nanoseconds,
                &.{
                    String.fromLiteral("long"),
                    String.fromLiteral("short"),
                    String.fromLiteral("narrow"),
                    String.fromLiteral("numeric"),
                },
                String.fromLiteral("numeric"),
            },
        }) |row| {
            // a. Let slot be the Internal Slot value of the current row.
            // b. Let unit be the Unit value of the current row.
            // c. Let styles be the Styles value of the current row.
            // d. Let digitalBase be the Digital Default value of the current row.
            const unit, const styles, const digital_base = row;
            const slot = std.fmt.comptimePrint("{t}_options", .{unit});

            // e. Let unitOptions be ? GetDurationUnitOptions(unit, options, style, styles,
            //    digitalBase, prevStyle, digitalFormat.[[TwoDigitHours]]).
            const unit_options = try getDurationUnitOptions(
                agent,
                unit,
                options,
                style,
                styles,
                digital_base,
                prev_style,
                digital_format.two_digit_hours,
            );

            const unit_style_map = std.StaticStringMap(
                @FieldType(@FieldType(DurationFormat.Fields, slot), "style"),
            ).initComptime(switch (unit) {
                .years, .months, .weeks, .days => &.{
                    .{ "long", .long },
                    .{ "short", .short },
                    .{ "narrow", .narrow },
                },
                .hours, .minutes, .seconds => &.{
                    .{ "long", .long },
                    .{ "short", .short },
                    .{ "narrow", .narrow },
                    .{ "numeric", .numeric },
                    .{ "2-digit", .@"2-digit" },
                },
                .milliseconds, .microseconds, .nanoseconds => &.{
                    .{ "long", .long },
                    .{ "short", .short },
                    .{ "narrow", .narrow },
                    .{ "fractional", .fractional },
                },
            });
            const unit_display_map = std.StaticStringMap(
                @FieldType(@FieldType(DurationFormat.Fields, slot), "display"),
            ).initComptime(&.{
                .{ "auto", .auto },
                .{ "always", .always },
            });

            // f. Set the value of durationFormat's internal slot whose name is slot to unitOptions.
            @field(duration_format.as(DurationFormat).fields, slot) = .{
                .style = unit_style_map.get(unit_options.style.slice.ascii).?,
                .display = unit_display_map.get(unit_options.display.slice.ascii).?,
            };

            switch (unit) {
                // g. If unit is one of "hours", "minutes", "seconds", "milliseconds", or
                //    "microseconds", then
                .hours, .minutes, .seconds, .milliseconds, .microseconds => {
                    // i. Set prevStyle to unitOptions.[[Style]].
                    prev_style = unit_options.style;
                },
                else => {},
            }
        }

        // 20. Set durationFormat.[[FractionalDigits]] to ? GetNumberOption(options,
        //     "fractionalDigits", 0, 9, undefined).
        duration_format.as(DurationFormat).fields.fractional_digits = if (try getNumberOption(
            agent,
            options,
            "fractionalDigits",
            0,
            9,
            null,
        )) |fractional_digits|
            @enumFromInt(fractional_digits)
        else
            null;

        // 21. Return durationFormat.
        return Value.from(duration_format);
    }
};

/// 13.3 Properties of the Intl.DurationFormat Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-durationformat-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "resolvedOptions", resolvedOptions, 0, realm);
        try object.defineBuiltinFunction(agent, "format", format, 1, realm);

        // 13.3.1 Intl.DurationFormat.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.DurationFormat.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.DurationFormat%"()),
        );

        // 13.3.5 Intl.DurationFormat.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-Intl.DurationFormat.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Intl.DurationFormat"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 13.3.2 Intl.DurationFormat.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-Intl.DurationFormat.prototype.resolvedOptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let df be the this value.
        // 2. Perform ? RequireInternalSlot(df, [[InitializedDurationFormat]]).
        const duration_format = try this_value.requireInternalSlot(agent, DurationFormat);

        // 3. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each row of Table 21, except the header row, in table order, do
        //     a. Let p be the Property value of the current row.
        //     b. Let v be the value of df's internal slot whose name is the Internal Slot value of
        //        the current row.
        //     c. If v is not undefined, then
        //         i. If there is a Conversion value in the current row, let conversion be that
        //            value; else let conversion be empty.
        //         ii. If conversion is number, then
        //             1. Set v to 𝔽(v).
        //         iii. Else if conversion is not empty, then
        //             1. Assert: conversion is style+display and v is a Duration Unit Options
        //                Record.
        //             2. NOTE: v.[[Style]] will be represented with a property named p (a plural
        //                Temporal unit), then v.[[Display]] will be represented with a property
        //                whose name suffixes p with "Display".
        //             3. Let style be v.[[Style]].
        //             4. If style is "fractional", then
        //                 a. Assert: IsFractionalSecondUnitName(p) is true.
        //                 b. Set style to "numeric".
        //             5. Perform ! CreateDataPropertyOrThrow(options, p, style).
        //             6. Set p to the string-concatenation of p and "Display".
        //             7. Set v to v.[[Display]].
        //         iv. Perform ! CreateDataPropertyOrThrow(options, p, v).
        const resolved_options = duration_format.fields.resolvedOptions();
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("locale"),
            Value.from(
                try String.fromAscii(
                    agent,
                    try duration_format.fields.locale.toString(agent.gc_allocator),
                ),
            ),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("numberingSystem"),
            Value.from(resolved_options.numbering_system),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("style"),
            Value.from(resolved_options.style),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("years"),
            Value.from(resolved_options.years),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("yearsDisplay"),
            Value.from(resolved_options.years_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("months"),
            Value.from(resolved_options.months),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("monthsDisplay"),
            Value.from(resolved_options.months_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("weeks"),
            Value.from(resolved_options.weeks),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("weeksDisplay"),
            Value.from(resolved_options.weeks_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("days"),
            Value.from(resolved_options.days),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("daysDisplay"),
            Value.from(resolved_options.days_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("hours"),
            Value.from(resolved_options.hours),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("hoursDisplay"),
            Value.from(resolved_options.hours_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("minutes"),
            Value.from(resolved_options.minutes),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("minutesDisplay"),
            Value.from(resolved_options.minutes_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("seconds"),
            Value.from(resolved_options.seconds),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("secondsDisplay"),
            Value.from(resolved_options.seconds_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("milliseconds"),
            Value.from(resolved_options.milliseconds),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("millisecondsDisplay"),
            Value.from(resolved_options.milliseconds_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("microseconds"),
            Value.from(resolved_options.microseconds),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("microsecondsDisplay"),
            Value.from(resolved_options.microseconds_display),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("nanoseconds"),
            Value.from(resolved_options.nanoseconds),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("nanosecondsDisplay"),
            Value.from(resolved_options.nanoseconds_display),
        );
        if (resolved_options.fractional_digits) |fractional_digits| {
            try options.createDataPropertyDirect(
                agent,
                PropertyKey.from("fractionalDigits"),
                Value.from(fractional_digits),
            );
        }

        // 5. Return options.
        return Value.from(options);
    }

    /// 13.3.3 Intl.DurationFormat.prototype.format ( duration )
    /// https://tc39.es/ecma402/#sec-Intl.DurationFormat.prototype.format
    fn format(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const duration_value = arguments.get(0);

        // 1. Let df be the this value.
        // 2. Perform ? RequireInternalSlot(df, [[InitializedDurationFormat]]).
        const duration_format = try this_value.requireInternalSlot(agent, DurationFormat);

        // 3. Let record be ? ToDurationRecord(duration).
        const duration = try Duration.from(agent, duration_value);

        // 4. Let parts be PartitionDurationFormatPattern(df, record).
        // 5. Let result be the empty String.
        // 6. For each Record { [[Type]], [[Value]], [[Unit]] } part in parts, do
        //    a. Set result to the string-concatenation of result and part.[[Value]].
        // 7. Return result.
        // TODO: This is blocked on missing C APIs in ICU4X. https://github.com/unicode-org/icu4x/issues/801
        _ = duration_format;
        _ = duration;
        return agent.throwException(.internal_error, "Duration formatting not implemented", .{});
    }
};

/// 13.4 Properties of Intl.DurationFormat Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-durationformat-instances
pub const DurationFormat = MakeObject(.{
    .Fields = struct {
        pub const Style = enum { long, short, narrow, digital };
        pub const YearsOptions = struct {
            style: enum { long, short, narrow },
            display: enum { auto, always },
        };
        pub const MonthsOptions = struct {
            style: enum { long, short, narrow },
            display: enum { auto, always },
        };
        pub const WeeksOptions = struct {
            style: enum { long, short, narrow },
            display: enum { auto, always },
        };
        pub const DaysOptions = struct {
            style: enum { long, short, narrow },
            display: enum { auto, always },
        };
        pub const HoursOptions = struct {
            style: enum { long, short, narrow, @"2-digit", numeric },
            display: enum { auto, always },
        };
        pub const MinutesOptions = struct {
            style: enum { long, short, narrow, @"2-digit", numeric },
            display: enum { auto, always },
        };
        pub const SecondsOptions = struct {
            style: enum { long, short, narrow, @"2-digit", numeric },
            display: enum { auto, always },
        };
        pub const MillisecondsOptions = struct {
            style: enum { long, short, narrow, fractional },
            display: enum { auto, always },
        };
        pub const MicrosecondsOptions = struct {
            style: enum { long, short, narrow, fractional },
            display: enum { auto, always },
        };
        pub const NanosecondsOptions = struct {
            style: enum { long, short, narrow, fractional },
            display: enum { auto, always },
        };
        pub const FractionalDigits = enum(u4) { @"0", @"1", @"2", @"3", @"4", @"5", @"6", @"7", @"8", @"9" };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[NumberingSystem]]
        numbering_system: *const String,

        /// [[Style]]
        style: Style,

        /// [[YearsOptions]]
        years_options: YearsOptions,

        /// [[MonthsOptions]]
        months_options: MonthsOptions,

        /// [[WeeksOptions]]
        weeks_options: WeeksOptions,

        /// [[DaysOptions]]
        days_options: DaysOptions,

        /// [[HoursOptions]]
        hours_options: HoursOptions,

        /// [[MinutesOptions]]
        minutes_options: MinutesOptions,

        /// [[SecondsOptions]]
        seconds_options: SecondsOptions,

        /// [[MillisecondsOptions]]
        milliseconds_options: MillisecondsOptions,

        /// [[MicrosecondsOptions]]
        microseconds_options: MicrosecondsOptions,

        /// [[NanosecondsOptions]]
        nanoseconds_options: NanosecondsOptions,

        /// [[HourMinuteSeparator]]
        hour_minute_separator: u8,

        /// [[MinuteSecondSeparator]]
        minute_second_separator: u8,

        /// [[FractionalDigits]]
        fractional_digits: ?FractionalDigits,

        pub const ResolvedOptions = struct {
            numbering_system: *const String,
            style: *const String,
            years: *const String,
            years_display: *const String,
            months: *const String,
            months_display: *const String,
            weeks: *const String,
            weeks_display: *const String,
            days: *const String,
            days_display: *const String,
            hours: *const String,
            hours_display: *const String,
            minutes: *const String,
            minutes_display: *const String,
            seconds: *const String,
            seconds_display: *const String,
            milliseconds: *const String,
            milliseconds_display: *const String,
            microseconds: *const String,
            microseconds_display: *const String,
            nanoseconds: *const String,
            nanoseconds_display: *const String,
            fractional_digits: ?u4,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            return .{
                .numbering_system = self.numbering_system,
                .style = switch (self.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .digital => String.fromLiteral("digital"),
                },
                .years = switch (self.years_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                },
                .years_display = switch (self.years_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .months = switch (self.months_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                },
                .months_display = switch (self.months_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .weeks = switch (self.weeks_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                },
                .weeks_display = switch (self.weeks_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .days = switch (self.days_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                },
                .days_display = switch (self.days_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .hours = switch (self.hours_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .@"2-digit" => String.fromLiteral("2-digit"),
                    .numeric => String.fromLiteral("numeric"),
                },
                .hours_display = switch (self.hours_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .minutes = switch (self.minutes_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .@"2-digit" => String.fromLiteral("2-digit"),
                    .numeric => String.fromLiteral("numeric"),
                },
                .minutes_display = switch (self.minutes_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .seconds = switch (self.seconds_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .@"2-digit" => String.fromLiteral("2-digit"),
                    .numeric => String.fromLiteral("numeric"),
                },
                .seconds_display = switch (self.seconds_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .milliseconds = switch (self.milliseconds_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .fractional => String.fromLiteral("numeric"),
                },
                .milliseconds_display = switch (self.milliseconds_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .microseconds = switch (self.microseconds_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .fractional => String.fromLiteral("numeric"),
                },
                .microseconds_display = switch (self.microseconds_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .nanoseconds = switch (self.nanoseconds_options.style) {
                    .long => String.fromLiteral("long"),
                    .short => String.fromLiteral("short"),
                    .narrow => String.fromLiteral("narrow"),
                    .fractional => String.fromLiteral("numeric"),
                },
                .nanoseconds_display = switch (self.nanoseconds_options.display) {
                    .auto => String.fromLiteral("auto"),
                    .always => String.fromLiteral("always"),
                },
                .fractional_digits = if (self.fractional_digits) |fractional_digits|
                    @intFromEnum(fractional_digits)
                else
                    null,
            };
        }
    },
    .tag = .intl_duration_format,
});

/// 13.5.1 Duration Records
/// https://tc39.es/ecma402/#sec-duration-records
const Duration = struct {
    /// [[Years]]
    years: f64,

    /// [[Months]]
    months: f64,

    /// [[Weeks]]
    weeks: f64,

    /// [[Days]]
    days: f64,

    /// [[Hours]]
    hours: f64,

    /// [[Minutes]]
    minutes: f64,

    /// [[Seconds]]
    seconds: f64,

    /// [[Milliseconds]]
    milliseconds: f64,

    /// [[Microseconds]]
    microseconds: f64,

    /// [[Nanoseconds]]
    nanoseconds: f64,

    /// 13.5.3 ToDurationRecord ( input )
    /// https://tc39.es/ecma402/#sec-todurationrecord
    pub fn from(agent: *Agent, input_value: Value) Agent.Error!Duration {
        // 1. If input is not an Object, then
        if (!input_value.isObject()) {
            // a. If input is a String, throw a RangeError exception.
            // Note: This is for a future Temporal integration.
            if (input_value.isString()) {
                return agent.throwException(
                    .range_error,
                    "Invalid duration string {f}",
                    .{input_value},
                );
            }

            // b. Throw a TypeError exception.
            return agent.throwException(.type_error, "{f} is not an Object", .{input_value});
        }
        const input = input_value.asObject();

        // 2. Let result be a new Duration Record with each field set to 0.
        var result: Duration = .{
            .years = 0,
            .months = 0,
            .weeks = 0,
            .days = 0,
            .hours = 0,
            .minutes = 0,
            .seconds = 0,
            .milliseconds = 0,
            .microseconds = 0,
            .nanoseconds = 0,
        };

        // 3. Let days be ? Get(input, "days").
        const days = try input.get(agent, PropertyKey.from("days"));

        // 4. If days is not undefined, set result.[[Days]] to ? ToIntegerIfIntegral(days).
        if (!days.isUndefined()) {
            result.days = try toIntegerIfIntegral(agent, days);
        }

        // 5. Let hours be ? Get(input, "hours").
        const hours = try input.get(agent, PropertyKey.from("hours"));

        // 6. If hours is not undefined, set result.[[Hours]] to ? ToIntegerIfIntegral(hours).
        if (!hours.isUndefined()) {
            result.hours = try toIntegerIfIntegral(agent, hours);
        }

        // 7. Let microseconds be ? Get(input, "microseconds").
        const microseconds = try input.get(agent, PropertyKey.from("microseconds"));

        // 8. If microseconds is not undefined, set result.[[Microseconds]] to ? ToIntegerIfIntegral(microseconds).
        if (!microseconds.isUndefined()) {
            result.microseconds = try toIntegerIfIntegral(agent, microseconds);
        }

        // 9. Let milliseconds be ? Get(input, "milliseconds").
        const milliseconds = try input.get(agent, PropertyKey.from("milliseconds"));

        // 10. If milliseconds is not undefined, set result.[[Milliseconds]] to ? ToIntegerIfIntegral(milliseconds).
        if (!milliseconds.isUndefined()) {
            result.milliseconds = try toIntegerIfIntegral(agent, milliseconds);
        }

        // 11. Let minutes be ? Get(input, "minutes").
        const minutes = try input.get(agent, PropertyKey.from("minutes"));

        // 12. If minutes is not undefined, set result.[[Minutes]] to ? ToIntegerIfIntegral(minutes).
        if (!minutes.isUndefined()) {
            result.minutes = try toIntegerIfIntegral(agent, minutes);
        }

        // 13. Let months be ? Get(input, "months").
        const months = try input.get(agent, PropertyKey.from("months"));

        // 14. If months is not undefined, set result.[[Months]] to ? ToIntegerIfIntegral(months).
        if (!months.isUndefined()) {
            result.months = try toIntegerIfIntegral(agent, months);
        }

        // 15. Let nanoseconds be ? Get(input, "nanoseconds").
        const nanoseconds = try input.get(agent, PropertyKey.from("nanoseconds"));

        // 16. If nanoseconds is not undefined, set result.[[Nanoseconds]] to ? ToIntegerIfIntegral(nanoseconds).
        if (!nanoseconds.isUndefined()) {
            result.nanoseconds = try toIntegerIfIntegral(agent, nanoseconds);
        }

        // 17. Let seconds be ? Get(input, "seconds").
        const seconds = try input.get(agent, PropertyKey.from("seconds"));

        // 18. If seconds is not undefined, set result.[[Seconds]] to ? ToIntegerIfIntegral(seconds).
        if (!seconds.isUndefined()) {
            result.seconds = try toIntegerIfIntegral(agent, seconds);
        }

        // 19. Let weeks be ? Get(input, "weeks").
        const weeks = try input.get(agent, PropertyKey.from("weeks"));

        // 20. If weeks is not undefined, set result.[[Weeks]] to ? ToIntegerIfIntegral(weeks).
        if (!weeks.isUndefined()) {
            result.weeks = try toIntegerIfIntegral(agent, weeks);
        }

        // 21. Let years be ? Get(input, "years").
        const years = try input.get(agent, PropertyKey.from("years"));

        // 22. If years is not undefined, set result.[[Years]] to ? ToIntegerIfIntegral(years).
        if (!years.isUndefined()) {
            result.years = try toIntegerIfIntegral(agent, years);
        }

        // 23. If years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds,
        //     and nanoseconds are all undefined, throw a TypeError exception.

        // TODO: 24. If IsValidDuration( result.[[Years]], result.[[Months]], result.[[Weeks]],
        //            result.[[Days]], result.[[Hours]], result.[[Minutes]], result.[[Seconds]],
        //            result.[[Milliseconds]], result.[[Microseconds]], result.[[Nanoseconds]]) is false,
        //            then
        //     a. Throw a RangeError exception.

        // 25. Return result.
        return result;
    }
};

/// 13.5.2 ToIntegerIfIntegral ( argument )
/// https://tc39.es/ecma402/#sec-tointegerifintegral
fn toIntegerIfIntegral(agent: *Agent, argument: Value) Agent.Error!f64 {
    // 1. Let number be ? ToNumber(argument).
    const number = try argument.toNumber(agent);

    // 2. If number is not an integral Number, throw a RangeError exception.
    if (!number.isIntegral()) {
        return agent.throwException(.range_error, "{f} is not an integral number", .{argument});
    }

    // 3. Return ℝ(number).
    return number.asFloat();
}

/// https://tc39.es/ecma402/#durationformat-unit-options-record
const DurationUnitOptions = struct {
    /// [[Style]]
    style: *const String,

    /// [[Display]]
    display: *const String,
};

const Unit = enum {
    years,
    months,
    weeks,
    days,
    hours,
    minutes,
    seconds,
    milliseconds,
    microseconds,
    nanoseconds,

    /// 13.5.13 IsFractionalSecondUnitName ( unit )
    /// https://tc39.es/ecma402/#sec-isfractionalsecondunitname
    fn isFractionalSecondUnitName(unit: Unit) bool {
        // 1. If unit is one of "milliseconds", "microseconds", or "nanoseconds", return true.
        // 2. Return false.
        return unit == .milliseconds or unit == .microseconds or unit == .nanoseconds;
    }
};

/// 13.5.6 GetDurationUnitOptions ( unit, options, baseStyle, stylesList, digitalBase, prevStyle, twoDigitHours )
/// https://tc39.es/ecma402/#sec-getdurationunitoptions
fn getDurationUnitOptions(
    agent: *Agent,
    comptime unit: Unit,
    options: *Object,
    base_style: *const String,
    comptime styles_list: []const *const String,
    digital_base: *const String,
    prev_style: *const String,
    two_digit_hours: bool,
) Agent.Error!DurationUnitOptions {
    // 1. Let style be ? GetOption(options, unit, string, stylesList, undefined).
    var style = try options.getOption(agent, @tagName(unit), .string, styles_list, null);

    // 2. Let displayDefault be "always".
    var display_default = String.fromLiteral("always");

    // 3. If style is undefined, then
    if (style == null) {
        // a. If baseStyle is "digital", then
        if (base_style.eql(String.fromLiteral("digital"))) {
            // i. Set style to digitalBase.
            style = digital_base;

            // ii. If unit is not one of "hours", "minutes", or "seconds", set displayDefault to "auto".
            if (!(unit == .hours or unit == .minutes or unit == .seconds)) {
                display_default = String.fromLiteral("auto");
            }
        }
        // b. Else if prevStyle is one of "fractional", "numeric" or "2-digit", then
        else if (prev_style.eql(String.fromLiteral("fractional")) or
            prev_style.eql(String.fromLiteral("numeric")) or
            prev_style.eql(String.fromLiteral("2-digit")))
        {
            // i. Set style to "numeric".
            style = String.fromLiteral("numeric");

            // ii. If unit is not "minutes" or "seconds", set displayDefault to "auto".
            if (!(unit == .minutes or unit == .seconds)) {
                display_default = String.fromLiteral("auto");
            }
        }
        // c. Else,
        else {
            // i. Set style to baseStyle.
            style = base_style;

            // ii. Set displayDefault to "auto".
            display_default = String.fromLiteral("auto");
        }
    }

    // 4. If style is "numeric" and IsFractionalSecondUnitName(unit) is true, then
    if (style.?.eql(String.fromLiteral("numeric")) and unit.isFractionalSecondUnitName()) {
        // a. Set style to "fractional".
        style = String.fromLiteral("fractional");

        // b. Set displayDefault to "auto".
        display_default = String.fromLiteral("auto");
    }

    // 5. Let displayField be the string-concatenation of unit and "Display".
    const display_field = std.fmt.comptimePrint("{t}Display", .{unit});

    // 6. Let display be ? GetOption(options, displayField, string, « "auto", "always" », displayDefault).
    const display = try options.getOption(
        agent,
        display_field,
        .string,
        &.{ String.fromLiteral("auto"), String.fromLiteral("always") },
        display_default,
    );

    // 7. Perform ? ValidateDurationUnitStyle(unit, style, display, prevStyle).
    try validateDurationUnitStyle(agent, unit, style.?, display, prev_style);

    // 8. If unit is "hours" and twoDigitHours is true, set style to "2-digit".
    if (unit == .hours and two_digit_hours) {
        style = String.fromLiteral("2-digit");
    }

    // 9. If unit is "minutes" or "seconds" and prevStyle is "numeric" or "2-digit", set style to "2-digit".
    if ((unit == .minutes or unit == .seconds) and
        (prev_style.eql(String.fromLiteral("numeric")) or prev_style.eql(String.fromLiteral("2-digit"))))
    {
        style = String.fromLiteral("2-digit");
    }

    // 10. Return the Duration Unit Options Record { [[Style]]: style, [[Display]]: display }.
    return .{ .style = style.?, .display = display };
}

/// 13.5.6.1 ValidateDurationUnitStyle ( unit, style, display, prevStyle )
/// https://tc39.es/ecma402/#sec-validatedurationunitstyle
fn validateDurationUnitStyle(
    agent: *Agent,
    unit: Unit,
    style: *const String,
    display: *const String,
    prev_style: *const String,
) Agent.Error!void {
    // 1. If display is "always" and style is "fractional", throw a RangeError exception.
    if (display.eql(String.fromLiteral("always")) and style.eql(String.fromLiteral("fractional"))) {
        return agent.throwException(
            .range_error,
            "Option '{[0]t}' with value 'numeric' and '{[0]t}Display' with value 'always' are incompatible",
            .{unit},
        );
    }

    // 2. If prevStyle is "fractional" and style is not "fractional", throw a RangeError exception.
    if (prev_style.eql(String.fromLiteral("fractional")) and !style.eql(String.fromLiteral("fractional"))) {
        return agent.throwException(
            .range_error,
            "Option '{t}' following 'numeric' option must be 'numeric'",
            .{unit},
        );
    }

    // 3. If prevStyle is "numeric" or "2-digit" and style is not one of "fractional", "numeric" or
    //    "2-digit", throw a RangeError exception.
    if ((prev_style.eql(String.fromLiteral("numeric")) or
        prev_style.eql(String.fromLiteral("2-digit"))) and
        !(style.eql(String.fromLiteral("fractional")) or
            style.eql(String.fromLiteral("numeric")) or
            style.eql(String.fromLiteral("2-digit"))))
    {
        return agent.throwException(
            .range_error,
            "Option '{t}' following 'numeric' or '2-digit' option must be 'fractional', 'numeric', or '2-digit'",
            .{unit},
        );
    }

    // 4. Return unused.
}
