//! 11 DateTimeFormat Objects
//! https://tc39.es/ecma402/#datetimeformat-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const icu4zig = @import("icu4zig");

const abstract_operations = @import("abstract_operations.zig");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const coerceOptionsToObject = abstract_operations.coerceOptionsToObject;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getNumberOption = abstract_operations.getNumberOption;
const getOption = types.getOption;
const matchUnicodeLocaleIdentifierType = abstract_operations.matchUnicodeLocaleIdentifierType;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const systemTimeZoneIdentifier = builtins.systemTimeZoneIdentifier;

/// 11.1.2 CreateDateTimeFormat ( newTarget, locales, options, required, defaults )
/// https://tc39.es/ecma402/#sec-createdatetimeformat
pub fn createDateTimeFormat(
    agent: *Agent,
    new_target: Object,
    locales: Value,
    options_value: Value,
    required: enum { date, time, any },
    defaults: enum { date, time, all },
) Agent.Error!Object {
    const data_provider = icu4zig.DataProvider.init();
    defer data_provider.deinit();
    const time_zone_id_mapper = icu4zig.TimeZoneIdMapper.init(data_provider);
    defer time_zone_id_mapper.deinit();

    // 1. Let dateTimeFormat be ? OrdinaryCreateFromConstructor(newTarget,
    //    "%Intl.DateTimeFormat.prototype%", « [[InitializedDateTimeFormat]], [[Locale]],
    //    [[Calendar]], [[NumberingSystem]], [[TimeZone]], [[HourCycle]], [[DateStyle]],
    //    [[TimeStyle]], [[DateTimeFormat]], [[BoundFormat]] »).
    const date_time_format = try ordinaryCreateFromConstructor(
        DateTimeFormat,
        agent,
        new_target,
        "%Intl.DateTimeFormat.prototype%",
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

    // 2. Let requestedLocales be ? CanonicalizeLocaleList(locales).
    const requested_locales = try canonicalizeLocaleList(agent, locales);

    // 3. Set options to ? CoerceOptionsToObject(options).
    const options = try coerceOptionsToObject(agent, options_value);

    // 4. Let opt be a new Record.

    // 5. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
    //    "best fit").
    const matcher = try getOption(
        options,
        "localeMatcher",
        .string,
        &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
        String.fromLiteral("best fit"),
    );

    // TODO: 6. Set opt.[[localeMatcher]] to matcher.
    _ = matcher;

    // 7. Let calendar be ? GetOption(options, "calendar", string, empty, undefined).
    //    "best fit").
    const calendar = try getOption(options, "calendar", .string, null, null);

    // 8. If calendar is not undefined, then
    if (calendar != null) {
        // a. If calendar cannot be matched by the type Unicode locale nonterminal, throw a
        //    RangeError exception.
        if (!matchUnicodeLocaleIdentifierType(try calendar.?.toUtf8(agent.gc_allocator))) {
            return agent.throwException(
                .range_error,
                "Invalid locale identifier type '{}'",
                .{calendar.?},
            );
        }
    }

    // TODO: 9. Set opt.[[ca]] to calendar.

    // 10. Let numberingSystem be ? GetOption(options, "numberingSystem", string, empty, undefined).
    const numbering_system = try getOption(options, "numberingSystem", .string, null, null);

    // 11. If numberingSystem is not undefined, then
    if (numbering_system != null) {
        // a. If numberingSystem cannot be matched by the type Unicode locale nonterminal, throw a
        //    RangeError exception.
        if (!matchUnicodeLocaleIdentifierType(try numbering_system.?.toUtf8(agent.gc_allocator))) {
            return agent.throwException(
                .range_error,
                "Invalid locale identifier type '{}'",
                .{numbering_system.?},
            );
        }
    }

    // TODO: 12. Set opt.[[nu]] to numberingSystem.

    // 13. Let hour12 be ? GetOption(options, "hour12", boolean, empty, undefined).
    const hour12 = try getOption(options, "hour12", .boolean, null, null);

    // 14. Let hourCycle be ? GetOption(options, "hourCycle", string, « "h11", "h12", "h23", "h24" », undefined).
    var hour_cycle = try getOption(
        options,
        "hourCycle",
        .string,
        &.{
            String.fromLiteral("h11"),
            String.fromLiteral("h12"),
            String.fromLiteral("h23"),
            String.fromLiteral("h24"),
        },
        null,
    );

    // 15. If hour12 is not undefined, then
    if (hour12 != null) {
        // a. Set hourCycle to null.
        hour_cycle = null;
    }

    // TODO: 16. Set opt.[[hc]] to hourCycle.

    // TODO: 17. Let r be ResolveLocale(%Intl.DateTimeFormat%.[[AvailableLocales]], requestedLocales,
    //           opt, %Intl.DateTimeFormat%.[[RelevantExtensionKeys]], %Intl.DateTimeFormat%.[[LocaleData]]).
    const resolved_locale = if (requested_locales.items.len != 0)
        requested_locales.items[0]
    else
        agent.platform.default_locale;
    const resolved = .{
        .locale = resolved_locale,
        .calendar = (try resolved_locale.getUnicodeExtension(agent.gc_allocator, "ca")) orelse
            if (calendar != null and calendar.? == .ascii) calendar.?.ascii else "gregory",
        .numbering_system = (try resolved_locale.getUnicodeExtension(agent.gc_allocator, "nu")) orelse
            if (numbering_system != null and numbering_system.? == .ascii) numbering_system.?.ascii else "latn",
    };

    // 18. Set dateTimeFormat.[[Locale]] to r.[[Locale]].
    date_time_format.as(DateTimeFormat).fields.locale = resolved.locale;

    // 19. Let resolvedCalendar be r.[[ca]].
    // 20. Set dateTimeFormat.[[Calendar]] to resolvedCalendar.
    const calendar_map = std.StaticStringMap(
        icu4zig.Calendar.Kind,
    ).initComptime(&.{
        .{ "buddhist", .buddhist },
        .{ "chinese", .chinese },
        .{ "coptic", .coptic },
        .{ "dangi", .dangi },
        .{ "ethioaa", .ethiopian_amete_alem },
        .{ "ethiopic", .ethiopian },
        .{ "gregory", .gregorian },
        .{ "hebrew", .hebrew },
        .{ "indian", .indian },
        .{ "islamic-civil", .islamic_civil },
        .{ "islamic-tbla", .islamic_tabular },
        .{ "islamic-umalqura", .islamic_umm_al_qura },
        .{ "islamic", .islamic_observational },
        .{ "iso8601", .iso },
        .{ "japanese", .japanese },
        .{ "persian", .persian },
        .{ "roc", .roc },
    });
    date_time_format.as(DateTimeFormat).fields.calendar = calendar_map.get(resolved.calendar) orelse .gregorian;

    // 21. Set dateTimeFormat.[[NumberingSystem]] to r.[[nu]].
    date_time_format.as(DateTimeFormat).fields.numbering_system = resolved.numbering_system;

    // TODO: 22-25.

    // 26. Let timeZone be ? Get(options, "timeZone").
    const time_zone_value = try options.get(PropertyKey.from("timeZone"));

    // 27. If timeZone is undefined, then
    var time_zone_string = if (time_zone_value == .undefined) blk: {
        // a. Set timeZone to SystemTimeZoneIdentifier().
        break :blk systemTimeZoneIdentifier();
    }
    // 28. Else,
    else blk: {
        // a. Set timeZone to ? ToString(timeZone).
        break :blk try (try time_zone_value.toString(agent)).toUtf8(agent.gc_allocator);
    };

    // 29. If IsTimeZoneOffsetString(timeZone) is true, then
    //     a. Let parseResult be ParseText(StringToCodePoints(timeZone), UTCOffset).
    //     b. Assert: parseResult is a Parse Node.
    //     c. If parseResult contains more than one MinuteSecond Parse Node, throw a RangeError exception.
    //     d. Let offsetNanoseconds be ParseTimeZoneOffsetString(timeZone).
    //     e. Let offsetMinutes be offsetNanoseconds / (6 × 10**10).
    //     f. Assert: offsetMinutes is an integer.
    //     g. Set timeZone to FormatOffsetTimeZoneIdentifier(offsetMinutes).
    // 30. Else,
    //     a. Let timeZoneIdentifierRecord be GetAvailableNamedTimeZoneIdentifier(timeZone).
    //     b. If timeZoneIdentifierRecord is empty, throw a RangeError exception.
    //     c. Set timeZone to timeZoneIdentifierRecord.[[PrimaryIdentifier]].
    if (icu4zig.CustomTimeZone.fromOffset(time_zone_string)) |custom_time_zone| {
        // TODO: Normalize time zone offset string
        custom_time_zone.deinit();
    } else |_| if (icu4zig.CustomTimeZone.fromIanaId(data_provider, time_zone_string)) |custom_time_zone| {
        time_zone_string = time_zone_id_mapper.normalizeIana(
            agent.gc_allocator,
            time_zone_string,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidId => unreachable,
        };
        custom_time_zone.deinit();
    } else |_| {
        return agent.throwException(.range_error, "Invalid time zone '{s}'", .{time_zone_string});
    }

    // 31. Set dateTimeFormat.[[TimeZone]] to timeZone.
    date_time_format.as(DateTimeFormat).fields.time_zone = time_zone_string;

    // TODO: 32. Let formatOptions be a new Record.
    // TODO: 33. Set formatOptions.[[hourCycle]] to hc.

    // 34. Let hasExplicitFormatComponents be false.
    var has_explicit_format_components = false;

    // 35. For each row of Table 16, except the header row, in table order, do
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
        // a. Let prop be the name given in the Property column of the current row.
        const property, const values = property_and_values;

        // b. If prop is "fractionalSecondDigits", then
        const value = if (comptime std.mem.eql(u8, property, "fractionalSecondDigits")) blk: {
            // i. Let value be ? GetNumberOption(options, "fractionalSecondDigits", 1, 3, undefined).
            break :blk try getNumberOption(
                agent,
                options,
                "fractionalSecondDigits",
                1,
                3,
                null,
            );
        }
        // c. Else,
        else blk: {
            // i. Let values be a List whose elements are the strings given in the Values column of
            //    the current row.
            // ii. Let value be ? GetOption(options, prop, string, values, undefined).
            break :blk try getOption(options, property, .string, values, null);
        };

        // TODO: d. Set formatOptions.[[<prop>]] to value.

        // e. If value is not undefined, then
        if (value != null) {
            // i. Set hasExplicitFormatComponents to true.
            has_explicit_format_components = true;
        }
    }

    // 36. Let formatMatcher be ? GetOption(options, "formatMatcher", string, « "basic", "best fit" »,
    //     "best fit").
    const format_matcher = try getOption(
        options,
        "formatMatcher",
        .string,
        &.{ String.fromLiteral("basic"), String.fromLiteral("best fit") },
        String.fromLiteral("best fit"),
    );

    // 37. Let dateStyle be ? GetOption(options, "dateStyle", string, « "full", "long", "medium",
    //     "short" », undefined).
    const date_style = try getOption(
        options,
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

    // 38. Set dateTimeFormat.[[DateStyle]] to dateStyle.
    const date_style_map = std.StaticStringMap(
        icu4zig.ZonedDateTimeFormatter.DateLength,
    ).initComptime(&.{
        .{ "full", .full },
        .{ "long", .long },
        .{ "medium", .medium },
        .{ "short", .short },
    });
    date_time_format.as(DateTimeFormat).fields.date_style = if (date_style) |s|
        date_style_map.get(s.ascii).?
    else
        null;

    // 39. Let timeStyle be ? GetOption(options, "timeStyle", string, « "full", "long", "medium",
    //     "short" », undefined).
    const time_style = try getOption(
        options,
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

    // 40. Set dateTimeFormat.[[TimeStyle]] to timeStyle.
    const time_style_map = std.StaticStringMap(
        icu4zig.ZonedDateTimeFormatter.TimeLength,
    ).initComptime(&.{
        .{ "full", .full },
        .{ "long", .long },
        .{ "medium", .medium },
        .{ "short", .short },
    });
    date_time_format.as(DateTimeFormat).fields.time_style = if (time_style) |s|
        time_style_map.get(s.ascii).?
    else
        null;

    // 41. If dateStyle is not undefined or timeStyle is not undefined, then
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
    }
    // 42. Else,
    else {
        // TODO: a-f.
        _ = defaults;

        // g. If formatMatcher is "basic", then
        if (format_matcher.eql(String.fromLiteral("basic"))) {
            // TODO: i. Let bestFormat be BasicFormatMatcher(formatOptions, formats).
        }
        // h. Else,
        else {
            // TODO: i. Let bestFormat be BestFitFormatMatcher(formatOptions, formats).
        }
    }

    // TODO: 43. Set dateTimeFormat.[[DateTimeFormat]] to bestFormat.
    // TODO: 44. If bestFormat has a field [[hour]], then
    //           a. Set dateTimeFormat.[[HourCycle]] to hc.

    // 45. Return dateTimeFormat.
    return date_time_format;
}

/// 11.2 Properties of the Intl.DateTimeFormat Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-datetimeformat-constructor
pub const DateTimeFormatConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "DateTimeFormat",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 11.2.1 Intl.DateTimeFormat.prototype
        // https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.DateTimeFormat.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 11.3.1 Intl.DateTimeFormat.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Intl.DateTimeFormat.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 11.1.1 Intl.DateTimeFormat ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.datetimeformat
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
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

        // 3. If the implementation supports the normative optional constructor mode of 4.3 Note 1, then
        //    a. Let this be the this value.
        //    b. Return ? ChainDateTimeFormat(dateTimeFormat, NewTarget, this).

        // 4. Return dateTimeFormat.
        return Value.from(date_time_format);
    }
};

/// 11.3 Properties of the Intl.DateTimeFormat Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-datetimeformat-prototype-object
pub const DateTimeFormatPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 11.3.2 Intl.DateTimeFormat.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.datetimeformat.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Intl.DateTimeFormat"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

/// 11.4 Properties of Intl.DateTimeFormat Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-datetimeformat-instances
pub const DateTimeFormat = MakeObject(.{
    .Fields = struct {
        pub const ResolvedOptions = struct {
            calendar: String,
            numbering_system: String,
            time_zone: String,
            date_style: ?String,
            time_style: ?String,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Calendar]]
        calendar: icu4zig.Calendar.Kind,

        /// [[NumberingSystem]]
        numbering_system: []const u8,

        /// [[TimeZone]]
        time_zone: []const u8,

        // TODO: [[HourCycle]]

        /// [[DateStyle]]
        date_style: ?icu4zig.ZonedDateTimeFormatter.DateLength,

        /// [[TimeStyle]]
        time_style: ?icu4zig.ZonedDateTimeFormatter.TimeLength,

        /// [[BoundFormat]]
        bound_format: ?Object,

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            // See: https://www.unicode.org/repos/cldr/tags/latest/common/bcp47/calendar.xml
            const calendar = switch (self.calendar) {
                .buddhist => String.fromLiteral("buddhist"),
                .chinese => String.fromLiteral("chinese"),
                .coptic => String.fromLiteral("coptic"),
                .dangi => String.fromLiteral("dangi"),
                .ethiopian => String.fromLiteral("ethiopic"),
                .ethiopian_amete_alem => String.fromLiteral("ethioaa"),
                .gregorian => String.fromLiteral("gregory"),
                .hebrew => String.fromLiteral("hebrew"),
                .indian => String.fromLiteral("indian"),
                .islamic_civil => String.fromLiteral("islamic-civil"),
                .islamic_observational => String.fromLiteral("islamic"),
                .islamic_tabular => String.fromLiteral("islamic-tbla"),
                .islamic_umm_al_qura => String.fromLiteral("islamic-umalqura"),
                .iso => String.fromLiteral("iso8601"),
                .japanese => String.fromLiteral("japanese"),
                .japanese_extended => unreachable, // Not listed?
                .persian => String.fromLiteral("persian"),
                .roc => String.fromLiteral("roc"),
            };
            const numbering_system = String.fromAscii(self.numbering_system);
            const time_zone = String.fromAscii(self.time_zone);
            return .{
                .calendar = calendar,
                .numbering_system = numbering_system,
                .time_zone = time_zone,
                .date_style = if (self.date_style) |date_style|
                    String.fromAscii(@tagName(date_style))
                else
                    null,
                .time_style = if (self.time_style) |time_style|
                    String.fromAscii(@tagName(time_style))
                else
                    null,
            };
        }
    },
    .tag = .intl_date_time_format,
});
