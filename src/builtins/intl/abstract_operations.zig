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

const LocaleList = std.ArrayListUnmanaged(icu4zig.Locale);

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

pub fn calendarToBcp47(calendar_kind: icu4zig.Calendar.Kind) *const String {
    // See: https://www.unicode.org/repos/cldr/tags/latest/common/bcp47/calendar.xml
    return switch (calendar_kind) {
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
}

/// 6.9.1 AvailableCalendars ( )
/// https://tc39.es/ecma402/#sec-availablecalendars
pub inline fn availableCalendars() []const *const String {
    // The implementation-defined abstract operation AvailableCalendars takes no arguments and
    // returns a List of Strings. The returned List is sorted according to lexicographic code unit
    // order, and contains unique calendar types in canonical form (6.9) identifying the calendars
    // for which the implementation provides the functionality of Intl.DateTimeFormat objects,
    // including their aliases (e.g., either both or neither of "islamicc" and "islamic-civil").
    // The List must include "iso8601".
    // NOTE: For now we only include the canonical BCP 47 language tags, so this isn't spec compliant.
    comptime {
        const calendar_kinds = std.enums.values(icu4zig.Calendar.Kind);
        var result: [calendar_kinds.len - 1]*const String = undefined;
        var i = 0;
        for (calendar_kinds) |calendar_kind| {
            if (calendar_kind == .japanese_extended) continue;
            result[i] = calendarToBcp47(calendar_kind);
            i += 1;
        }
        std.mem.sortUnstable(*const String, &result, {}, struct {
            fn lessThanFn(_: void, lhs: *const String, rhs: *const String) bool {
                return std.mem.lessThan(u8, lhs.slice.ascii, rhs.slice.ascii);
            }
        }.lessThanFn);
        const final = result; // Load bearing const assignment
        return &final;
    }
}

/// 6.6.3 AvailableCanonicalUnits ( )
/// https://tc39.es/ecma402/#sec-availablecanonicalunits
pub fn availableCanonicalUnits() []const *const String {
    // The abstract operation AvailableCanonicalUnits takes no arguments and returns a List of
    // Strings. The returned List is sorted according to lexicographic code unit order, and
    // consists of the unique values of simple unit identifiers listed in every row of Table 2,
    // except the header row.
    return comptime &.{
        String.fromLiteral("acre"),              String.fromLiteral("bit"),         String.fromLiteral("byte"),
        String.fromLiteral("celsius"),           String.fromLiteral("centimeter"),  String.fromLiteral("day"),
        String.fromLiteral("degree"),            String.fromLiteral("fahrenheit"),  String.fromLiteral("fluid-ounce"),
        String.fromLiteral("foot"),              String.fromLiteral("gallon"),      String.fromLiteral("gigabit"),
        String.fromLiteral("gigabyte"),          String.fromLiteral("gram"),        String.fromLiteral("hectare"),
        String.fromLiteral("hour"),              String.fromLiteral("inch"),        String.fromLiteral("kilobit"),
        String.fromLiteral("kilobyte"),          String.fromLiteral("kilogram"),    String.fromLiteral("kilometer"),
        String.fromLiteral("liter"),             String.fromLiteral("megabit"),     String.fromLiteral("megabyte"),
        String.fromLiteral("meter"),             String.fromLiteral("microsecond"), String.fromLiteral("mile"),
        String.fromLiteral("mile-scandinavian"), String.fromLiteral("milliliter"),  String.fromLiteral("millimeter"),
        String.fromLiteral("millisecond"),       String.fromLiteral("minute"),      String.fromLiteral("month"),
        String.fromLiteral("nanosecond"),        String.fromLiteral("ounce"),       String.fromLiteral("percent"),
        String.fromLiteral("petabyte"),          String.fromLiteral("pound"),       String.fromLiteral("second"),
        String.fromLiteral("stone"),             String.fromLiteral("terabit"),     String.fromLiteral("terabyte"),
        String.fromLiteral("week"),              String.fromLiteral("yard"),        String.fromLiteral("year"),
    };
}

/// 6.7.1 AvailableCanonicalNumberingSystems ( )
/// https://tc39.es/ecma402/#sec-availablecanonicalnumberingsystems
pub fn availableCanonicalNumberingSystems() []const *const String {
    // The implementation-defined abstract operation AvailableCanonicalNumberingSystems takes no
    // arguments and returns a List of Strings. The returned List is sorted according to
    // lexicographic code unit order, and contains unique canonical numbering systems identifiers
    // identifying the numbering systems for which the implementation provides the functionality of
    // Intl.DateTimeFormat, Intl.NumberFormat, and Intl.RelativeTimeFormat objects. The List must
    // include the Numbering System value of every row of Table 23, except the header row.
    return comptime &.{
        String.fromLiteral("adlm"),     String.fromLiteral("ahom"),     String.fromLiteral("arab"),
        String.fromLiteral("arabext"),  String.fromLiteral("bali"),     String.fromLiteral("beng"),
        String.fromLiteral("bhks"),     String.fromLiteral("brah"),     String.fromLiteral("cakm"),
        String.fromLiteral("cham"),     String.fromLiteral("deva"),     String.fromLiteral("diak"),
        String.fromLiteral("fullwide"), String.fromLiteral("gong"),     String.fromLiteral("gonm"),
        String.fromLiteral("gujr"),     String.fromLiteral("guru"),     String.fromLiteral("hanidec"),
        String.fromLiteral("hmng"),     String.fromLiteral("hmnp"),     String.fromLiteral("java"),
        String.fromLiteral("kali"),     String.fromLiteral("kawi"),     String.fromLiteral("khmr"),
        String.fromLiteral("knda"),     String.fromLiteral("lana"),     String.fromLiteral("lanatham"),
        String.fromLiteral("laoo"),     String.fromLiteral("latn"),     String.fromLiteral("lepc"),
        String.fromLiteral("limb"),     String.fromLiteral("mathbold"), String.fromLiteral("mathdbl"),
        String.fromLiteral("mathmono"), String.fromLiteral("mathsanb"), String.fromLiteral("mathsans"),
        String.fromLiteral("mlym"),     String.fromLiteral("modi"),     String.fromLiteral("mong"),
        String.fromLiteral("mroo"),     String.fromLiteral("mtei"),     String.fromLiteral("mymr"),
        String.fromLiteral("mymrshan"), String.fromLiteral("mymrtlng"), String.fromLiteral("nagm"),
        String.fromLiteral("newa"),     String.fromLiteral("nkoo"),     String.fromLiteral("olck"),
        String.fromLiteral("orya"),     String.fromLiteral("osma"),     String.fromLiteral("rohg"),
        String.fromLiteral("saur"),     String.fromLiteral("segment"),  String.fromLiteral("shrd"),
        String.fromLiteral("sind"),     String.fromLiteral("sinh"),     String.fromLiteral("sora"),
        String.fromLiteral("sund"),     String.fromLiteral("takr"),     String.fromLiteral("talu"),
        String.fromLiteral("tamldec"),  String.fromLiteral("telu"),     String.fromLiteral("thai"),
        String.fromLiteral("tibt"),     String.fromLiteral("tirh"),     String.fromLiteral("tnsa"),
        String.fromLiteral("vaii"),     String.fromLiteral("wara"),     String.fromLiteral("wcho"),
    };
}

/// 9.2.1 CanonicalizeLocaleList ( locales )
/// https://tc39.es/ecma402/#sec-canonicalizelocalelist
pub fn canonicalizeLocaleList(agent: *Agent, locales: Value) Agent.Error!LocaleList {
    // 1. If locales is undefined, then
    if (locales.isUndefined()) {
        // a. Return a new empty List.
        return .empty;
    }

    // 2. Let seen be a new empty List.
    var seen: LocaleList = .empty;

    // 3. If locales is a String or locales is an Object and locales has an [[InitializedLocale]]
    //    internal slot, then
    const object = if (locales.isString() or
        (locales.isObject() and locales.asObject().is(builtins.intl.Locale)))
    blk: {
        // a. Let O be CreateArrayFromList(« locales »).
        break :blk try createArrayFromList(agent, &.{locales});
    } else blk: {
        // 4. Else,
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

            // ii. If kValue is not a String and kValue is not an Object, throw a TypeError
            //     exception.
            if (!k_value.isString() and !k_value.isObject()) {
                return agent.throwException(
                    .type_error,
                    "Locale list items must be strings or objects",
                    .{},
                );
            }

            // iii. If kValue is an Object and kValue has an [[InitializedLocale]] internal slot,
            //      then
            const tag = if (k_value.isObject() and k_value.asObject().is(builtins.intl.Locale)) blk: {
                // 1. Let tag be kValue.[[Locale]].
                break :blk try String.fromAscii(
                    agent.gc_allocator,
                    try k_value.asObject().as(builtins.intl.Locale).fields.locale.toString(agent.gc_allocator),
                );
            } else blk: {
                // iv. Else,
                // 1. Let tag be ? ToString(kValue).
                break :blk try k_value.toString(agent);
            };

            // v. If IsStructurallyValidLanguageTag(tag) is false, throw a RangeError exception.
            // vi. Let canonicalizedTag be CanonicalizeUnicodeLocaleId(tag).
            const canonicalized_tag = icu4zig.Locale.fromString(try tag.toUtf8(agent.gc_allocator)) catch {
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
                try seen.append(agent.gc_allocator, canonicalized_tag);
            }
        }

        // d. Set k to k + 1.
    }

    // 8. Return seen.
    return seen;
}

/// 9.2.9 GetOptionsObject ( options )
/// https://tc39.es/ecma402/#sec-getoptionsobject
pub fn getOptionsObject(agent: *Agent, options: Value) Agent.Error!*Object {
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
    options: *Object,
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
