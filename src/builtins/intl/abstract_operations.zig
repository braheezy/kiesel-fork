//! 9.2 Abstract Operations
//! https://tc39.es/ecma402/#sec-abstract-operations

const std = @import("std");

const icu4zig = @import("icu4zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

const LocaleList = std.ArrayList(icu4zig.Locale);

/// https://unicode.org/reports/tr35/#Unicode_locale_identifier
/// type = alphanum{3,8} (sep alphanum{3,8})*
pub fn matchUnicodeLocaleIdentifierType(str: []const u8) bool {
    var it = std.mem.splitScalar(u8, str, '-');
    while (it.next()) |part| {
        if (part.len < 3 or part.len > 8) return false;
        for (part) |c| {
            if (!std.ascii.isAlphanumeric(c)) return false;
        }
    }
    return true;
}

pub fn calendarToBcp47(calendar_kind: icu4zig.Calendar.Kind) []const u8 {
    // See: https://www.unicode.org/repos/cldr/tags/latest/common/bcp47/calendar.xml
    return switch (calendar_kind) {
        .buddhist => "buddhist",
        .chinese => "chinese",
        .coptic => "coptic",
        .dangi => "dangi",
        .ethiopian => "ethiopic",
        .ethiopian_amete_alem => "ethioaa",
        .gregorian => "gregory",
        .hebrew => "hebrew",
        .indian => "indian",
        .islamic_civil => "islamic-civil",
        .islamic_observational => "islamic",
        .islamic_tabular => "islamic-tbla",
        .islamic_umm_al_qura => "islamic-umalqura",
        .iso => "iso8601",
        .japanese => "japanese",
        .japanese_extended => unreachable, // Not listed?
        .persian => "persian",
        .roc => "roc",
    };
}

/// 6.9.1 AvailableCalendars ( )
/// https://tc39.es/ecma402/#sec-availablecalendars
pub inline fn availableCalendars() []const []const u8 {
    // The implementation-defined abstract operation AvailableCalendars takes no arguments and
    // returns a List of Strings. The returned List is sorted according to lexicographic code unit
    // order, and contains unique calendar types in canonical form (6.9) identifying the calendars
    // for which the implementation provides the functionality of Intl.DateTimeFormat objects,
    // including their aliases (e.g., either both or neither of "islamicc" and "islamic-civil").
    // The List must include "iso8601".
    // NOTE: For now we only include the canonical BCP 47 language tags, so this isn't spec compliant.
    comptime {
        const calendar_kinds = std.enums.values(icu4zig.Calendar.Kind);
        var result: [calendar_kinds.len - 1][]const u8 = undefined;
        var i = 0;
        for (calendar_kinds) |calendar_kind| {
            if (calendar_kind == .japanese_extended) continue;
            result[i] = calendarToBcp47(calendar_kind);
            i += 1;
        }
        const final = result; // Load bearing const assignment
        return &final;
    }
}

/// 6.6.3 AvailableCanonicalUnits ( )
/// https://tc39.es/ecma402/#sec-availablecanonicalunits
pub fn availableCanonicalUnits() []const []const u8 {
    // The abstract operation AvailableCanonicalUnits takes no arguments and returns a List of
    // Strings. The returned List is sorted according to lexicographic code unit order, and
    // consists of the unique values of simple unit identifiers listed in every row of Table 2,
    // except the header row.
    return comptime &.{
        "acre",        "bit",      "byte",              "celsius",     "centimeter",
        "day",         "degree",   "fahrenheit",        "fluid-ounce", "foot",
        "gallon",      "gigabit",  "gigabyte",          "gram",        "hectare",
        "hour",        "inch",     "kilobit",           "kilobyte",    "kilogram",
        "kilometer",   "liter",    "megabit",           "megabyte",    "meter",
        "microsecond", "mile",     "mile-scandinavian", "milliliter",  "millimeter",
        "millisecond", "minute",   "month",             "nanosecond",  "ounce",
        "percent",     "petabyte", "pound",             "second",      "stone",
        "terabit",     "terabyte", "week",              "yard",        "year",
    };
}

/// 6.7.1 AvailableCanonicalNumberingSystems ( )
/// https://tc39.es/ecma402/#sec-availablecanonicalnumberingsystems
pub fn availableCanonicalNumberingSystems() []const []const u8 {
    // The implementation-defined abstract operation AvailableCanonicalNumberingSystems takes no
    // arguments and returns a List of Strings. The returned List is sorted according to
    // lexicographic code unit order, and contains unique canonical numbering systems identifiers
    // identifying the numbering systems for which the implementation provides the functionality of
    // Intl.DateTimeFormat, Intl.NumberFormat, and Intl.RelativeTimeFormat objects. The List must
    // include the Numbering System value of every row of Table 23, except the header row.
    return comptime &.{
        "adlm",    "ahom",     "arab",     "arabext",  "bali",     "beng", "bhks", "brah",
        "cakm",    "cham",     "deva",     "diak",     "fullwide", "gong", "gonm", "gujr",
        "guru",    "hanidec",  "hmng",     "hmnp",     "java",     "kali", "kawi", "khmr",
        "knda",    "lana",     "lanatham", "laoo",     "latn",     "lepc", "limb", "mathbold",
        "mathdbl", "mathmono", "mathsanb", "mathsans", "mlym",     "modi", "mong", "mroo",
        "mtei",    "mymr",     "mymrshan", "mymrtlng", "nagm",     "newa", "nkoo", "olck",
        "orya",    "osma",     "rohg",     "saur",     "segment",  "shrd", "sind", "sinh",
        "sora",    "sund",     "takr",     "talu",     "tamldec",  "telu", "thai", "tibt",
        "tirh",    "tnsa",     "vaii",     "wara",     "wcho",
    };
}

/// 9.2.1 CanonicalizeLocaleList ( locales )
/// https://tc39.es/ecma402/#sec-canonicalizelocalelist
pub fn canonicalizeLocaleList(agent: *Agent, locales: Value) Agent.Error!LocaleList {
    // 1. If locales is undefined, then
    if (locales.isUndefined()) {
        // a. Return a new empty List.
        return LocaleList.init(agent.gc_allocator);
    }

    // 2. Let seen be a new empty List.
    var seen = LocaleList.init(agent.gc_allocator);

    // 3. If Type(locales) is String or Type(locales) is Object and locales has an
    //    [[InitializedLocale]] internal slot, then
    const object = if (locales.isString() or
        (locales.isObject() and locales.asObject().is(builtins.Intl.Locale)))
    blk: {
        // a. Let O be CreateArrayFromList(« locales »).
        break :blk try createArrayFromList(agent, &.{locales});
    }
    // 4. Else,
    else blk: {
        // a. Let O be ? ToObject(locales).
        break :blk try locales.toObject(agent);
    };

    // 5. Let len be ? LengthOfArrayLike(O).
    const len = try object.lengthOfArrayLike();

    // 6. Let k be 0.
    var k: u53 = 0;

    // 7. Repeat, while k < len,
    while (k < len) : (k += 1) {
        // a. Let Pk be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k);

        // b. Let kPresent be ? HasProperty(O, Pk).
        const k_present = try object.hasProperty(property_key);

        // c. If kPresent is true, then
        if (k_present) {
            // i. Let kValue be ? Get(O, Pk).
            const k_value = try object.get(property_key);

            // ii. If Type(kValue) is not String or Object, throw a TypeError exception.
            if (!k_value.isString() and !k_value.isObject()) {
                return agent.throwException(
                    .type_error,
                    "Locale list items must be strings or objects",
                    .{},
                );
            }

            // iii. If Type(kValue) is Object and kValue has an [[InitializedLocale]] internal slot, then
            const tag = if (k_value.isObject() and k_value.asObject().is(builtins.Intl.Locale)) blk: {
                // 1. Let tag be kValue.[[Locale]].
                break :blk String.fromAscii(
                    try k_value.asObject().as(builtins.Intl.Locale).fields.locale.toString(agent.gc_allocator),
                );
            }
            // iv. Else,
            else blk: {
                // 1. Let tag be ? ToString(kValue).
                break :blk try k_value.toString(agent);
            };

            // v. If IsStructurallyValidLanguageTag(tag) is false, throw a RangeError exception.
            // vi. Let canonicalizedTag be CanonicalizeUnicodeLocaleId(tag).
            // NOTE: Underscore separators are not BCP 47-compatible and must be rejected here.
            if (tag.indexOf(String.fromLiteral("_"), 0) != null) {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier '{}'",
                    .{tag},
                );
            }
            const canonicalized_tag = icu4zig.Locale.init(try tag.toUtf8(agent.gc_allocator)) catch {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier '{}'",
                    .{tag},
                );
            };

            // vii. If seen does not contain canonicalizedTag, append canonicalizedTag to seen.
            for (seen.items) |locale| {
                if (locale.normalizingEq(try tag.toUtf8(agent.gc_allocator))) break;
            } else {
                try seen.append(canonicalized_tag);
            }
        }

        // d. Set k to k + 1.
    }

    // 8. Return seen.
    return seen;
}

/// 9.2.9 GetOptionsObject ( options )
/// https://tc39.es/ecma402/#sec-getoptionsobject
pub fn getOptionsObject(agent: *Agent, options: Value) Agent.Error!Object {
    // 1. If options is undefined, then
    if (options.isUndefined()) {
        // a. Return OrdinaryObjectCreate(null).
        return ordinaryObjectCreate(agent, null);
    }

    // 2. If options is an Object, then
    if (options.isObject()) {
        // a. Return options.
        return options.asObject();
    }

    // 3. Throw a TypeError exception.
    return agent.throwException(
        .type_error,
        "Options must either be an object or undefined",
        .{},
    );
}

/// 9.2.13 DefaultNumberOption ( value, minimum, maximum, fallback )
/// https://tc39.es/ecma402/#sec-defaultnumberoption
pub fn defaultNumberOption(
    agent: *Agent,
    value: Value,
    property: []const u8,
    minimum: i32,
    maximum: i32,
    fallback: ?i32,
) Agent.Error!?i32 {
    // 1. If value is undefined, return fallback.
    if (value.isUndefined()) return fallback;

    // 2. Set value to ? ToNumber(value).
    const number = try value.toNumber(agent);

    // 3. If value is not finite or ℝ(value) < minimum or ℝ(value) > maximum, throw a RangeError
    //    exception.
    if (!number.isFinite() or
        number.asFloat() < @as(f64, @floatFromInt(minimum)) or
        number.asFloat() > @as(f64, @floatFromInt(maximum)))
    {
        return agent.throwException(
            .range_error,
            "Number option '{s}' must be in range {}-{}",
            .{ property, minimum, maximum },
        );
    }

    // 4. Return floor(ℝ(value)).
    return @intFromFloat(@floor(number.asFloat()));
}

/// 9.2.14 GetNumberOption ( options, property, minimum, maximum, fallback )
/// https://tc39.es/ecma402/#sec-getnumberoption
pub fn getNumberOption(
    agent: *Agent,
    options: Object,
    comptime property: []const u8,
    minimum: i32,
    maximum: i32,
    fallback: ?i32,
) Agent.Error!?i32 {
    // 1. Let value be ? Get(options, property).
    const value = try options.get(PropertyKey.from(property));

    // 2. Return ? DefaultNumberOption(value, minimum, maximum, fallback).
    return defaultNumberOption(agent, value, property, minimum, maximum, fallback);
}
