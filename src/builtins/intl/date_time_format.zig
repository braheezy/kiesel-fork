//! 11 DateTimeFormat Objects
//! https://tc39.es/ecma402/#datetimeformat-objects

const std = @import("std");

const icu4zig = @import("icu4zig");
const temporal_rs = @import("temporal_rs");

const abstract_operations = @import("abstract_operations.zig");
const build_options = @import("build-options");
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
const calendarToBcp47 = abstract_operations.calendarToBcp47;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getNumberOption = abstract_operations.getNumberOption;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const resolveOptions = abstract_operations.resolveOptions;
const systemTimeZoneIdentifier = builtins.systemTimeZoneIdentifier;

/// 11.1.1.1 ChainDateTimeFormat ( dateTimeFormat, newTarget, this )
/// https://tc39.es/ecma402/#sec-chaindatetimeformat
fn chainDateTimeFormat(
    agent: *Agent,
    date_time_format: *DateTimeFormat,
    new_target: ?*Object,
    this: ?Value,
) Agent.Error!Value {
    const realm = agent.currentRealm();

    // 1. If newTarget is undefined and ? OrdinaryHasInstance(%Intl.DateTimeFormat%, this) is true,
    //    then
    if (new_target == null and
        try Value.from(try realm.intrinsic(.intl_date_time_format)).ordinaryHasInstance(
            agent,
            this.?,
        ))
    {
        // a. Perform ? DefinePropertyOrThrow(this, %Intl%.[[FallbackSymbol]], PropertyDescriptor {
        //    [[Value]]: dateTimeFormat, [[Writable]]: false, [[Enumerable]]: false,
        //    [[Configurable]]: false }).
        try this.?.asObject().definePropertyOrThrow(
            agent,
            PropertyKey.from(try realm.intrinsic(.intl_fallback_symbol)),
            .{
                .value = Value.from(&date_time_format.object),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );

        // b. Return this.
        return this.?;
    }

    // 2. Return dateTimeFormat.
    return Value.from(&date_time_format.object);
}

/// 11.1.2 CreateDateTimeFormat ( newTarget, locales, options, required, defaults )
/// https://tc39.es/ecma402/#sec-createdatetimeformat
pub fn createDateTimeFormat(
    agent: *Agent,
    new_target: *Object,
    locales: Value,
    options_value: Value,
    required: enum { date, time, any },
    defaults: enum { date, time, all },
) Agent.Error!*DateTimeFormat {
    const iana_parser = icu4zig.IanaParser.init();
    defer iana_parser.deinit();

    // 1. Let dateTimeFormat be ? OrdinaryCreateFromConstructor(newTarget,
    //    "%Intl.DateTimeFormat.prototype%", « [[InitializedDateTimeFormat]], [[Locale]],
    //    [[Calendar]], [[NumberingSystem]], [[TimeZone]], [[HourCycle]], [[DateStyle]],
    //    [[TimeStyle]], [[DateTimeFormat]], [[BoundFormat]] »).
    const date_time_format = try ordinaryCreateFromConstructor(
        DateTimeFormat,
        agent,
        new_target,
        .intl_date_time_format_prototype,
        .{
            .locale = undefined,
            .calendar = undefined,
            .numbering_system = undefined,
            .time_zone = undefined,
            .date_style = undefined,
            .time_style = undefined,
            .bound_format = null,
        },
    );

    // TODO: 2-3.

    // 4. Let optionsResolution be ? ResolveOptions(%Intl.DateTimeFormat%,
    //    %Intl.DateTimeFormat%.[[LocaleData]], locales, options, « coerce-options »,
    //    modifyResolutionOptions).
    const options_resolution = try resolveOptions(
        agent,
        &.{
            .{ .key = "ca", .property = "calendar" },
            .{ .key = "nu", .property = "numberingSystem" },
            .{ .key = "hour12", .property = "hour12", .type = .boolean },
            .{ .key = "hc", .property = "hourCycle", .values = &.{
                String.fromLiteral("h11"),
                String.fromLiteral("h12"),
                String.fromLiteral("h23"),
                String.fromLiteral("h24"),
            } },
        },
        locales,
        options_value,
        .{ .coerce_options = true },
    );

    // TODO: 5. Let hour12 be specialOptions.[[Hour12]].

    // 6. Set options to optionsResolution.[[Options]].
    const options = options_resolution.options;

    // 7. Let resolvedLocale be optionsResolution.[[ResolvedLocale]].
    const resolved_locale = options_resolution.resolved_locale;
    const locale = resolved_locale.locale;

    // 8. Set dateTimeFormat.[[Locale]] to resolvedLocale.[[Locale]].
    date_time_format.fields.locale = locale;

    // 9. Let resolvedCalendar be resolvedLocale.[[ca]].
    const resolved_calendar = if (try locale.getUnicodeExtension(agent.gc_allocator, "ca")) |ca|
        try String.fromAscii(agent, ca)
    else
        resolved_locale.options.ca orelse String.fromLiteral("gregory");
    const calendar = std.StaticStringMap(icu4zig.Calendar.Kind).initComptime(&.{
        .{ "buddhist", .buddhist },
        .{ "chinese", .chinese },
        .{ "coptic", .coptic },
        .{ "dangi", .dangi },
        .{ "ethioaa", .ethiopian_amete_alem },
        .{ "ethiopic", .ethiopian },
        .{ "gregory", .gregorian },
        .{ "hebrew", .hebrew },
        .{ "indian", .indian },
        .{ "islamic-civil", .hijri_tabular_type_ii_friday },
        .{ "islamic-tbla", .hijri_tabular_type_ii_thursday },
        .{ "islamic-umalqura", .hijri_umm_al_qura },
        .{ "islamic", .hijri_simulated_mecca },
        .{ "iso8601", .iso },
        .{ "japanese", .japanese },
        .{ "persian", .persian },
        .{ "roc", .roc },
    }).get(resolved_calendar.asAscii()) orelse .gregorian;

    // 10. Set dateTimeFormat.[[Calendar]] to resolvedCalendar.
    date_time_format.fields.calendar = calendar;

    // 11. Set dateTimeFormat.[[NumberingSystem]] to resolvedLocale.[[nu]].
    const numbering_system = if (try locale.getUnicodeExtension(agent.gc_allocator, "nu")) |nu|
        try String.fromAscii(agent, nu)
    else
        resolved_locale.options.nu orelse String.fromLiteral("latn");
    date_time_format.fields.numbering_system = numbering_system;

    // TODO: 12-15.

    // 16. Let timeZone be ? Get(options, "timeZone").
    const time_zone_value = try options.get(agent, PropertyKey.from("timeZone"));

    // 17. If timeZone is undefined, then
    const time_zone_string = if (time_zone_value.isUndefined()) blk: {
        // a. Set timeZone to SystemTimeZoneIdentifier().
        const time_zone = systemTimeZoneIdentifier(agent.platform);
        if (@TypeOf(time_zone) == void) break :blk "UTC";
        var write = temporal_rs.DiplomatWrite.init(agent.gc_allocator);
        temporal_rs.c.temporal_rs_TimeZone_identifier(time_zone, &write.inner);
        break :blk try write.toOwnedSlice();
    } else blk: {
        // 18. Else,
        // a. Set timeZone to ? ToString(timeZone).
        break :blk try (try time_zone_value.toString(agent)).toUtf8(agent.gc_allocator);
    };

    // 19. If IsTimeZoneOffsetString(timeZone) is true, then
    //     a. Let parseResult be ParseText(StringToCodePoints(timeZone), UTCOffset).
    //     b. Assert: parseResult is a Parse Node.
    //     c. If parseResult contains more than one MinuteSecond Parse Node, throw a RangeError
    //        exception.
    //     d. Let offsetNanoseconds be ParseTimeZoneOffsetString(timeZone).
    //     e. Let offsetMinutes be offsetNanoseconds / (6 × 10**10).
    //     f. Assert: offsetMinutes is an integer.
    //     g. Set timeZone to FormatOffsetTimeZoneIdentifier(offsetMinutes).
    // 20. Else,
    //     a. Let timeZoneIdentifierRecord be GetAvailableNamedTimeZoneIdentifier(timeZone).
    //     b. If timeZoneIdentifierRecord is empty, throw a RangeError exception.
    //     c. Set timeZone to timeZoneIdentifierRecord.[[PrimaryIdentifier]].
    // TODO: Detect invalid time zone

    // 21. Set dateTimeFormat.[[TimeZone]] to timeZone.
    date_time_format.fields.time_zone = try String.fromAscii(agent, time_zone_string);

    // TODO: 22. Let formatOptions be a new Record.
    // TODO: 23. Set formatOptions.[[hourCycle]] to hc.

    // 24. Let hasExplicitFormatComponents be false.
    var has_explicit_format_components = false;

    // 25. For each row of Table 16, except the header row, in table order, do
    inline for (comptime .{
        .{ "weekday", &.{
            String.fromLiteral("narrow"),
            String.fromLiteral("short"),
            String.fromLiteral("long"),
        } },
        .{ "era", &.{
            String.fromLiteral("narrow"),
            String.fromLiteral("short"),
            String.fromLiteral("long"),
        } },
        .{ "year", &.{
            String.fromLiteral("2-digit"),
            String.fromLiteral("numeric"),
        } },
        .{ "month", &.{
            String.fromLiteral("2-digit"),
            String.fromLiteral("numeric"),
            String.fromLiteral("narrow"),
            String.fromLiteral("short"),
            String.fromLiteral("long"),
        } },
        .{ "day", &.{
            String.fromLiteral("2-digit"),
            String.fromLiteral("numeric"),
        } },
        .{ "dayPeriod", &.{
            String.fromLiteral("narrow"),
            String.fromLiteral("short"),
            String.fromLiteral("long"),
        } },
        .{ "hour", &.{
            String.fromLiteral("2-digit"),
            String.fromLiteral("numeric"),
        } },
        .{ "minute", &.{
            String.fromLiteral("2-digit"),
            String.fromLiteral("numeric"),
        } },
        .{ "second", &.{
            String.fromLiteral("2-digit"),
            String.fromLiteral("numeric"),
        } },
        .{
            "fractionalSecondDigits",
            &.{},
        },
        .{ "timeZoneName", &.{
            String.fromLiteral("short"),
            String.fromLiteral("long"),
            String.fromLiteral("shortOffset"),
            String.fromLiteral("longOffset"),
            String.fromLiteral("shortGeneric"),
            String.fromLiteral("longGeneric"),
        } },
    }) |property_and_values| {
        // a. Let propertyKey be the name given in the Property column of the current row.
        const property_key, const values = property_and_values;

        // b. If propertyKey is "fractionalSecondDigits", then
        const value = if (comptime std.mem.eql(u8, property_key, "fractionalSecondDigits")) blk: {
            // i. Let value be ? GetNumberOption(options, "fractionalSecondDigits", 1, 3,
            //    undefined).
            break :blk try getNumberOption(
                agent,
                options,
                "fractionalSecondDigits",
                1,
                3,
                null,
            );
        } else blk: {
            // c. Else,
            // i. Let values be a List whose elements are the strings given in the Values column of
            //    the current row.
            // ii. Let value be ? GetOption(options, propertyKey, string, values, undefined).
            break :blk try options.getOption(agent, property_key, .string, values, null);
        };

        // TODO: d. Set formatOptions.[[<propertyKey>]] to value.

        // e. If value is not undefined, then
        if (value != null) {
            // i. Set hasExplicitFormatComponents to true.
            has_explicit_format_components = true;
        }
    }

    // 26. Let formatMatcher be ? GetOption(options, "formatMatcher", string, « "basic",
    //     "best fit" », "best fit").
    const format_matcher = try options.getOption(
        agent,
        "formatMatcher",
        .string,
        &.{ String.fromLiteral("basic"), String.fromLiteral("best fit") },
        String.fromLiteral("best fit"),
    );

    // 27. Let dateStyle be ? GetOption(options, "dateStyle", string, « "full", "long", "medium",
    //     "short" », undefined).
    const maybe_date_style_string = try options.getOption(
        agent,
        "dateStyle",
        .string,
        &.{
            String.fromLiteral("full"),
            String.fromLiteral("long"),
            String.fromLiteral("medium"),
            String.fromLiteral("short"),
        },
        null,
    );
    const date_style = if (maybe_date_style_string) |date_style_string|
        std.StaticStringMap(DateTimeFormat.Fields.DateStyle).initComptime(&.{
            .{ "full", .full },
            .{ "long", .long },
            .{ "medium", .medium },
            .{ "short", .short },
        }).get(date_style_string.asAscii()).?
    else
        null;

    // 28. Set dateTimeFormat.[[DateStyle]] to dateStyle.
    date_time_format.fields.date_style = date_style;

    // 29. Let timeStyle be ? GetOption(options, "timeStyle", string, « "full", "long", "medium",
    //     "short" », undefined).
    const maybe_time_style_string = try options.getOption(
        agent,
        "timeStyle",
        .string,
        &.{
            String.fromLiteral("full"),
            String.fromLiteral("long"),
            String.fromLiteral("medium"),
            String.fromLiteral("short"),
        },
        null,
    );
    const time_style = if (maybe_time_style_string) |time_style_string|
        std.StaticStringMap(DateTimeFormat.Fields.TimeStyle).initComptime(&.{
            .{ "full", .full },
            .{ "long", .long },
            .{ "medium", .medium },
            .{ "short", .short },
        }).get(time_style_string.asAscii()).?
    else
        null;

    // 30. Set dateTimeFormat.[[TimeStyle]] to timeStyle.
    date_time_format.fields.time_style = time_style;

    // 31. If dateStyle is not undefined or timeStyle is not undefined, then
    if (date_style != null or time_style != null) {
        // a. If hasExplicitFormatComponents is true, then
        if (has_explicit_format_components) {
            // i. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Option 'dateStyle'/'timeStyle' must not be used with explicit format components",
                .{},
            );
        }

        // b. If required is date and timeStyle is not undefined, then
        if (required == .date and time_style != null) {
            // i. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Option 'timeStyle' must not be used for date-only formatting",
                .{},
            );
        }

        // c. If required is time and dateStyle is not undefined, then
        if (required == .time and date_style != null) {
            // i. Throw a TypeError exception.
            return agent.throwException(
                .type_error,
                "Option 'dateStyle' must not be used for time-only formatting",
                .{},
            );
        }

        // TODO: d. Let styles be resolvedLocaleData.[[styles]].[[<resolvedCalendar>]].
        // TODO: e. Let bestFormat be DateTimeStyleFormat(dateStyle, timeStyle, styles).
    } else {
        // 32. Else,
        // TODO: a-f.
        _ = defaults;

        // g. If formatMatcher is "basic", then
        if (format_matcher.eql(String.fromLiteral("basic"))) {
            // TODO: i. Let bestFormat be BasicFormatMatcher(formatOptions, formats).
        } else {
            // h. Else,
            // TODO: i. Let bestFormat be BestFitFormatMatcher(formatOptions, formats).
        }
    }

    // TODO: 33. Set dateTimeFormat.[[DateTimeFormat]] to bestFormat.
    // TODO: 34. If bestFormat has a field [[hour]], then
    //           a. Set dateTimeFormat.[[HourCycle]] to hc.

    // 35. Return dateTimeFormat.
    return date_time_format;
}

/// 11.2 Properties of the Intl.DateTimeFormat Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-datetimeformat-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor_with_this = impl },
            0,
            "DateTimeFormat",
            .{ .realm = realm, .proto = try realm.intrinsic(.function_prototype) },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 11.2.1 Intl.DateTimeFormat.prototype
        // https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsic(.intl_date_time_format_prototype)),
            .none,
        );
    }

    /// 11.1.1 Intl.DateTimeFormat ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.datetimeformat
    fn impl(
        agent: *Agent,
        this_value: ?Value,
        arguments: Arguments,
        new_target: ?*Object,
    ) Agent.Error!Value {
        const locales = arguments.get(0);
        const options = arguments.get(1);

        // 1. If NewTarget is undefined, let newTarget be the active function object, else let
        //    newTarget be NewTarget.
        const new_target_ = new_target orelse agent.activeFunctionObject();

        // 2. Let dateTimeFormat be ? CreateDateTimeFormat(newTarget, locales, options, any, date).
        const date_time_format = try createDateTimeFormat(
            agent,
            new_target_,
            locales,
            options,
            .any,
            .date,
        );

        // 3. If the implementation supports the normative optional constructor mode of 4.3 Note 1,
        //    then
        if (build_options.enable_annex_b) {
            // a. Let this be the this value.
            // b. Return ? ChainDateTimeFormat(dateTimeFormat, NewTarget, this).
            return chainDateTimeFormat(agent, date_time_format, new_target, this_value);
        }

        // 4. Return dateTimeFormat.
        return Value.from(&date_time_format.object);
    }
};

/// 11.3 Properties of the Intl.DateTimeFormat Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-datetimeformat-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsic(.object_prototype));
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "resolvedOptions", resolvedOptions, 0, realm);
        try object.defineBuiltinAccessor(agent, "format", format, null, realm);
        try object.defineBuiltinFunction(agent, "formatRange", formatRange, 1, realm);

        // 11.3.1 Intl.DateTimeFormat.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsic(.intl_date_time_format)),
        );

        // 11.3.7 Intl.DateTimeFormat.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "Symbol.toStringTag",
            Value.from("Intl.DateTimeFormat"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 11.3.2 Intl.DateTimeFormat.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype.resolvedoptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let dtf be the this value.
        // 2. If the implementation supports the normative optional constructor mode of 4.3 Note 1,
        //    then
        //     a. Set dtf to ? UnwrapDateTimeFormat(dtf).
        const date_time_format_value = if (build_options.enable_annex_b)
            try unwrapDateTimeFormat(agent, this_value)
        else
            this_value;

        // 3. Perform ? RequireInternalSlot(dtf, [[InitializedDateTimeFormat]]).
        const date_time_format = try date_time_format_value.requireInternalSlot(agent, DateTimeFormat);

        // 4. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsic(.object_prototype),
        );

        // 5. For each row of Table 15, except the header row, in table order, do
        //     a. Let propertyKey be the Property value of the current row.
        //     b. If there is an Internal Slot value in the current row, then
        //         i. Let value be the value of dtf's internal slot whose name is the Internal Slot
        //            value of the current row.
        //     c. Else,
        //         i. Let format be dtf.[[DateTimeFormat]].
        //         ii. If format has a field [[<propertyKey>]] and dtf.[[DateStyle]] is undefined
        //             and dtf.[[TimeStyle]] is undefined, then
        //             1. Let value be format.[[<propertyKey>]].
        //         iii. Else,
        //             1. Let value be undefined.
        //     d. If value is not undefined, then
        //         i. If there is a Conversion value in the current row, then
        //             1. Let conversion be the Conversion value of the current row.
        //             2. If conversion is hour12, then
        //                 a. If value is "h11" or "h12", set value to true. Otherwise, set value to
        //                    false.
        //             3. Else,
        //                 a. Assert: conversion is number.
        //                 b. Set value to 𝔽(value).
        //         ii. Perform ! CreateDataPropertyOrThrow(options, propertyKey, value).
        const resolved_options = date_time_format.fields.resolvedOptions();
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("locale"),
            Value.from(
                try String.fromAscii(
                    agent,
                    try date_time_format.fields.locale.toString(agent.gc_allocator),
                ),
            ),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("calendar"),
            Value.from(resolved_options.calendar),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("numberingSystem"),
            Value.from(resolved_options.numbering_system),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("timeZone"),
            Value.from(resolved_options.time_zone),
        );
        if (resolved_options.date_style) |date_style| {
            try options.createDataPropertyDirect(
                agent,
                PropertyKey.from("dateStyle"),
                Value.from(date_style),
            );
        }
        if (resolved_options.time_style) |time_style| {
            try options.createDataPropertyDirect(
                agent,
                PropertyKey.from("timeStyle"),
                Value.from(time_style),
            );
        }
        // TODO: hourCycle, hour12, weekday, era, year, month, day, dayPeriod, hour, minute,
        //       second, fractionalSecondDigits, timeZoneName

        // 6. Return options.
        return Value.from(options);
    }

    /// 11.3.3 get Intl.DateTimeFormat.prototype.format
    /// https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype.format
    fn format(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let dtf be the this value.
        // 2. If the implementation supports the normative optional constructor mode of 4.3 Note 1,
        //    then
        //     a. Set dtf to ? UnwrapDateTimeFormat(dtf).
        const date_time_format_value = if (build_options.enable_annex_b)
            try unwrapDateTimeFormat(agent, this_value)
        else
            this_value;

        // 3. Perform ? RequireInternalSlot(dtf, [[InitializedDateTimeFormat]]).
        const date_time_format = try date_time_format_value.requireInternalSlot(agent, DateTimeFormat);

        // 4. If dtf.[[BoundFormat]] is undefined, then
        if (date_time_format.fields.bound_format == null) {
            // a. Let func be a new built-in function object as defined in DateTime Format Functions
            //    (11.5.4).
            // b. Set func.[[DateTimeFormat]] to dtf.
            const Captures = struct {
                date_time_format: *DateTimeFormat,
            };
            const captures = try agent.gc_allocator.create(Captures);
            captures.* = .{ .date_time_format = date_time_format };

            const dateTimeFormatFunction = struct {
                /// 11.5.4 DateTime Format Functions
                /// https://tc39.es/ecma402/#sec-datetime-format-functions
                fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
                    const io = agent_.io;
                    const function = agent_.activeFunctionObject();
                    const captures_ = function.as(builtins.BuiltinFunction).fields.additionalFieldsAs(Captures);
                    const date = arguments.get(0);

                    // 1. Let dtf be func.[[DateTimeFormat]].
                    // 2. Assert: dtf is an Object and dtf has an [[InitializedDateTimeFormat]]
                    //    internal slot.
                    const date_time_format_ = captures_.date_time_format;

                    // 3. If date is not provided or is undefined, then
                    const x = if (date.isUndefined()) blk: {
                        // a. Let x be ! Call(%Date.now%, undefined).
                        const timestamp: std.Io.Timestamp = .now(io, .real);
                        break :blk @as(f64, @floatFromInt(timestamp.toMilliseconds()));
                    } else blk: {
                        // 4. Else,
                        // a. Let x be ? ToNumber(date).
                        break :blk (try date.toNumber(agent_)).asFloat();
                    };

                    // 5. Return ? FormatDateTime(dtf, x).
                    return formatDateTime(agent_, date_time_format_, x);
                }
            }.func;

            const bound_format = try createBuiltinFunction(
                agent,
                .{ .function = dateTimeFormatFunction },
                1,
                "",
                .{ .additional_fields = captures },
            );

            // c. Set dtf.[[BoundFormat]] to func.
            date_time_format.fields.bound_format = bound_format;
        }

        // 5. Return dtf.[[BoundFormat]].
        return Value.from(&date_time_format.fields.bound_format.?.object);
    }

    /// 11.3.4 Intl.DateTimeFormat.prototype.formatRange ( startDate, endDate )
    /// https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype.formatRange
    fn formatRange(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const start_date = arguments.get(0);
        const end_date = arguments.get(1);

        // 1. Let dtf be this value.
        // 2. Perform ? RequireInternalSlot(dtf, [[InitializedDateTimeFormat]]).
        const date_time_format = try this_value.requireInternalSlot(agent, DateTimeFormat);

        // 3. If startDate is undefined or endDate is undefined, throw a TypeError exception.
        if (start_date.isUndefined() or end_date.isUndefined()) {
            return agent.throwException(.type_error, "Argument must not be undefined", .{});
        }

        // 4. Let x be ? ToNumber(startDate).
        const x = try start_date.toNumber(agent);

        // 5. Let y be ? ToNumber(endDate).
        const y = try end_date.toNumber(agent);

        // 6. Return ? FormatDateTimeRange(dtf, x, y).
        return formatDateTimeRange(agent, date_time_format, x.asFloat(), y.asFloat());
    }
};

/// 11.4 Properties of Intl.DateTimeFormat Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-datetimeformat-instances
pub const DateTimeFormat = MakeObject(.{
    .Fields = struct {
        pub const DateStyle = enum {
            full,
            long,
            medium,
            short,
        };
        pub const TimeStyle = enum {
            full,
            long,
            medium,
            short,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Calendar]]
        calendar: icu4zig.Calendar.Kind,

        /// [[NumberingSystem]]
        numbering_system: *const String,

        /// [[TimeZone]]
        time_zone: *const String,

        // TODO: [[HourCycle]]

        /// [[DateStyle]]
        date_style: ?DateStyle,

        /// [[TimeStyle]]
        time_style: ?TimeStyle,

        /// [[BoundFormat]]
        bound_format: ?*builtins.BuiltinFunction,

        pub const ResolvedOptions = struct {
            calendar: *const String,
            numbering_system: *const String,
            time_zone: *const String,
            date_style: ?*const String,
            time_style: ?*const String,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            return .{
                .calendar = calendarToBcp47(self.calendar),
                .numbering_system = self.numbering_system,
                .time_zone = self.time_zone,
                .date_style = if (self.date_style) |date_style| switch (date_style) {
                    .full => String.fromLiteral("full"),
                    .long => String.fromLiteral("long"),
                    .medium => String.fromLiteral("medium"),
                    .short => String.fromLiteral("short"),
                } else null,
                .time_style = if (self.time_style) |time_style| switch (time_style) {
                    .full => String.fromLiteral("full"),
                    .long => String.fromLiteral("long"),
                    .medium => String.fromLiteral("medium"),
                    .short => String.fromLiteral("short"),
                } else null,
            };
        }
    },
    .tag = .intl_date_time_format,
    .display_name = "Intl.DateTimeFormat",
});

/// 11.5.7 FormatDateTime ( dateTimeFormat, x )
/// https://tc39.es/ecma402/#sec-formatdatetime
pub fn formatDateTime(agent: *Agent, date_time_format: *const DateTimeFormat, x_: f64) Agent.Error!Value {
    // 1. Let parts be ? PartitionDateTimePattern(dateTimeFormat, x).
    // 2. Let result be the empty String.
    // 3. For each Record { [[Type]], [[Value]] } part of parts, do
    //     a. Set result to the string-concatenation of result and part.[[Value]].
    // 4. Return result.
    const date = @import("../date.zig");
    const x = date.timeClip(x_);
    if (std.math.isNan(x)) return agent.throwException(.range_error, "Invalid time value", .{});
    const result = formatDateTimeImpl(
        agent.gc_allocator,
        date_time_format,
        x,
    ) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        else => return agent.throwException(.internal_error, "Unhandled ICU4X error: {t}", .{err}),
    };
    return Value.from(try String.fromUtf8(agent, result));
}

const FormatDateTimeError =
    std.mem.Allocator.Error ||
    icu4zig.CalendarError ||
    icu4zig.DateTimeFormatterLoadError ||
    icu4zig.DateTimeWriteError ||
    icu4zig.Rfc9557ParseError;

fn formatDateTimeImpl(
    allocator: std.mem.Allocator,
    date_time_format: *const DateTimeFormat,
    x: f64,
) FormatDateTimeError![]const u8 {
    const date = try icu4zig.IsoDate.init(
        builtins.date.yearFromTime(x),
        builtins.date.monthFromTime(x) + 1,
        builtins.date.dateFromTime(x),
    );
    defer date.deinit();

    const time = try icu4zig.Time.init(
        builtins.date.hourFromTime(x),
        builtins.date.minFromTime(x),
        builtins.date.secFromTime(x),
        @as(u32, @intCast(builtins.date.msFromTime(x))) * 1_000_000,
    );
    defer time.deinit();

    const iana_parser = icu4zig.IanaParser.init();
    defer iana_parser.deinit();
    const time_zone = iana_parser.parse(date_time_format.fields.time_zone.asAscii());
    time_zone.deinit();
    const offset = icu4zig.UtcOffset.fromString(date_time_format.fields.time_zone.asAscii()) catch
        icu4zig.UtcOffset.fromSeconds(0) catch
        unreachable;
    defer offset.deinit();
    const time_zone_info = time_zone.withOffset(offset);
    defer time_zone_info.deinit();

    // TODO: Implement DateTimeFormatter in a way where the underlying format is selectable, currently hardcoded to ymdt
    const date_time_formatter = try icu4zig.DateTimeFormatter.init(
        date_time_format.fields.locale,
        switch (date_time_format.fields.date_style orelse .short) {
            .full, .long => .long,
            .medium => .medium,
            .short => .short,
        },
        switch (date_time_format.fields.time_style orelse .short) {
            .full => .subsecond9,
            .long => .second,
            .medium => .minute,
            .short => .hour,
        },
        .auto,
        .auto,
    );
    defer date_time_formatter.deinit();
    const zoned_date_time_formatter = try icu4zig.ZonedDateTimeFormatter.init(
        date_time_format.fields.locale,
        date_time_formatter,
    );
    defer zoned_date_time_formatter.deinit();
    const result = try zoned_date_time_formatter.formatIso(
        allocator,
        date,
        time,
        time_zone_info,
    );

    return result;
}

/// 11.5.10 FormatDateTimeRange ( dateTimeFormat, x, y )
/// https://tc39.es/ecma402/#sec-formatdatetimerange
pub fn formatDateTimeRange(agent: *Agent, date_time_format: *const DateTimeFormat, x_: f64, y_: f64) Agent.Error!Value {
    // 1. Let parts be ? PartitionDateTimeRangePattern(dateTimeFormat, x, y).
    // 2. Let result be the empty String.
    // 3. For each Record { [[Type]], [[Value]], [[Source]] } part of parts, do
    //     a. Set result to the string-concatenation of result and part.[[Value]].
    // 4. Return result.
    const date = @import("../date.zig");
    const x = date.timeClip(x_);
    if (std.math.isNan(x)) return agent.throwException(.range_error, "Invalid time value", .{});
    const y = date.timeClip(y_);
    if (std.math.isNan(y)) return agent.throwException(.range_error, "Invalid time value", .{});
    const result_x = formatDateTimeImpl(
        agent.gc_allocator,
        date_time_format,
        x,
    ) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        else => return agent.throwException(.internal_error, "Unhandled ICU4X error: {t}", .{err}),
    };
    const result_y = formatDateTimeImpl(
        agent.gc_allocator,
        date_time_format,
        y,
    ) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        else => return agent.throwException(.internal_error, "Unhandled ICU4X error: {t}", .{err}),
    };
    if (std.mem.eql(u8, result_x, result_y)) {
        return Value.from(try String.fromUtf8(agent, result_x));
    } else {
        const result = try std.mem.concat(agent.gc_allocator, u8, &.{ result_x, " – ", result_y });
        return Value.from(try String.fromUtf8(agent, result));
    }
}

/// 11.5.14 UnwrapDateTimeFormat ( dtf )
/// https://tc39.es/ecma402/#sec-unwrapdatetimeformat
fn unwrapDateTimeFormat(agent: *Agent, date_time_format_value: Value) Agent.Error!Value {
    const realm = agent.currentRealm();

    // 1. If dtf is not an Object, throw a TypeError exception.
    if (!date_time_format_value.isObject()) {
        return agent.throwException(.type_error, "this value must be an object", .{});
    }
    const date_time_format = date_time_format_value.asObject();

    // 2. If dtf does not have an [[InitializedDateTimeFormat]] internal slot and
    //    ? OrdinaryHasInstance(%Intl.DateTimeFormat%, dtf) is true, then
    if (!date_time_format.is(DateTimeFormat) and
        try Value.from(try realm.intrinsic(.intl_date_time_format)).ordinaryHasInstance(
            agent,
            date_time_format_value,
        ))
    {
        // a. Return ? Get(dtf, %Intl%.[[FallbackSymbol]]).
        return date_time_format.get(
            agent,
            PropertyKey.from(try realm.intrinsic(.intl_fallback_symbol)),
        );
    }

    // 3. Return dtf.
    return date_time_format_value;
}
