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
        .hijri_tabular_type_ii_friday => String.fromLiteral("islamic-civil"),
        .hijri_simulated_mecca => String.fromLiteral("islamic"),
        .hijri_tabular_type_ii_thursday => String.fromLiteral("islamic-tbla"),
        .hijri_umm_al_qura => String.fromLiteral("islamic-umalqura"),
        .iso => String.fromLiteral("iso8601"),
        .japanese => String.fromLiteral("japanese"),
        .japanese_extended => unreachable, // Not listed?
        .persian => String.fromLiteral("persian"),
        .roc => String.fromLiteral("roc"),
    };
}

/// 6.3.1 IsWellFormedCurrencyCode ( currency )
/// https://tc39.es/ecma402/#sec-iswellformedcurrencycode
pub fn isWellFormedCurrencyCode(currency: *const String) bool {
    // 1. If the length of currency is not 3, return false.
    if (currency.length != 3) return false;

    // 2. Let normalized be the ASCII-uppercase of currency.
    // 3. If normalized contains any code unit outside of 0x0041 through 0x005A (corresponding to
    //    Unicode characters LATIN CAPITAL LETTER A through LATIN CAPITAL LETTER Z), return false.
    // 4. Return true.
    return switch (currency.asAsciiOrUtf16()) {
        .ascii => |ascii| std.ascii.isAlphabetic(ascii[0]) and
            std.ascii.isAlphabetic(ascii[1]) and
            std.ascii.isAlphabetic(ascii[2]),
        .utf16 => false,
    };
}

/// 6.6.1 IsWellFormedUnitIdentifier ( unitIdentifier )
/// https://tc39.es/ecma402/#sec-iswellformedunitidentifier
pub fn isWellFormedUnitIdentifier(unit_identifier: *const String) bool {
    const ascii = switch (unit_identifier.asAsciiOrUtf16()) {
        .ascii => |ascii| ascii,
        .utf16 => return false,
    };

    // 1. If IsSanctionedSingleUnitIdentifier(unitIdentifier) is true, then
    if (isSanctionedSingleUnitIdentifier(ascii)) {
        // a. Return true.
        return true;
    }

    // 2. Let numeratorAndDenominator be StringSplitToList(unitIdentifier, "-per-").
    // 3. If numeratorAndDenominator does not have exactly 2 elements, return false.
    var it = std.mem.splitSequence(u8, ascii, "-per-");
    const numerator = it.next() orelse return false;
    const denominator = it.next() orelse return false;
    if (it.peek() != null) return false;

    // 4. If IsSanctionedSingleUnitIdentifier(numeratorAndDenominator[0]) is false, return false.
    if (!isSanctionedSingleUnitIdentifier(numerator)) return false;

    // 5. If IsSanctionedSingleUnitIdentifier(numeratorAndDenominator[1]) is false, return false.
    if (!isSanctionedSingleUnitIdentifier(denominator)) return false;

    // 6. Return true.
    return true;
}

/// 6.6.2 IsSanctionedSingleUnitIdentifier ( unitIdentifier )
/// https://tc39.es/ecma402/#sec-issanctionedsingleunitidentifier
fn isSanctionedSingleUnitIdentifier(unit_identifier: []const u8) bool {
    // 1. If unitIdentifier is listed in Table 2 below, return true.
    // 2. Else, return false.
    for (availableCanonicalUnits()) |canonical_unit| {
        if (std.mem.eql(u8, canonical_unit.asAscii(), unit_identifier)) return true;
    }
    return false;
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
                return std.mem.lessThan(u8, lhs.asAscii(), rhs.asAscii());
            }
        }.lessThanFn);
        const final = result; // Load bearing const assignment
        return &final;
    }
}

/// 9.2.1 CanonicalizeLocaleList ( locales )
/// https://tc39.es/ecma402/#sec-canonicalizelocalelist
pub fn canonicalizeLocaleList(agent: *Agent, locales: Value) Agent.Error!LocaleList {
    const gpa = agent.gpa;

    // 1. If locales is undefined, then
    if (locales.isUndefined()) {
        // a. Return a new empty List.
        return .empty;
    }

    // 2. Let seen be a new empty List.
    var seen: LocaleList = .empty;

    // 3. If locales is a String or locales is an Object and locales has an [[InitializedLocale]]
    //    internal slot, then
    const locales_obj = if (locales.isString() or
        (locales.isObject() and locales.asObject().is(builtins.intl.Locale)))
    blk: {
        // a. Let localesObj be CreateArrayFromList(« locales »).
        const array = try createArrayFromList(agent, &.{locales});
        break :blk &array.object;
    } else blk: {
        // 4. Else,
        // a. Let localesObj be ? ToObject(locales).
        break :blk try locales.toObject(agent);
    };

    // 5. Let length be ? LengthOfArrayLike(localesObj).
    const length = try locales_obj.lengthOfArrayLike(agent);

    // 6. Let k be 0.
    var k: u53 = 0;

    // 7. Repeat, while k < length,
    while (k < length) : (k += 1) {
        // a. Let propertyKey be ! ToString(𝔽(k)).
        const property_key = PropertyKey.from(k);

        // b. Let exists be ? HasProperty(localesObj, propertyKey).
        const exists = try locales_obj.hasProperty(agent, property_key);

        // c. If exists is true, then
        if (exists) {
            // i. Let element be ? Get(localesObj, propertyKey).
            const element = try locales_obj.get(agent, property_key);

            // ii. If element is neither a String nor an Object, throw a TypeError exception.
            if (!element.isString() and !element.isObject()) {
                return agent.throwException(
                    .type_error,
                    "Locale list items must be strings or objects",
                    .{},
                );
            }

            // iii. If element is an Object and element has an [[InitializedLocale]] internal slot,
            //      then
            const tag = if (element.castObject(builtins.intl.Locale)) |locale| blk: {
                // 1. Let tag be element.[[Locale]].
                break :blk try String.fromAscii(
                    agent,
                    try locale.fields.locale.toString(agent.gc_allocator),
                );
            } else blk: {
                // iv. Else,
                // 1. Let tag be ? ToString(element).
                break :blk try element.toString(agent);
            };

            // v. If IsWellFormedLanguageTag(tag) is false, throw a RangeError exception.
            // vi. Let canonicalizedTag be CanonicalizeUnicodeLocaleId(tag).
            const tag_utf8 = try tag.toUtf8(gpa);
            defer gpa.free(tag_utf8);
            const canonicalized_tag = icu4zig.Locale.fromString(tag_utf8) catch {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier '{f}'",
                    .{tag.fmtEscaped()},
                );
            };

            // vii. If seen does not contain canonicalizedTag, append canonicalizedTag to seen.
            for (seen.items) |locale| {
                if (locale.normalizingEq(tag_utf8)) break;
            } else {
                try seen.append(agent.gc_allocator, canonicalized_tag);
            }
        }

        // d. Set k to k + 1.
    }

    // 8. Return seen.
    return seen;
}

fn OptionsResolution(comptime ResolutionType: type) type {
    return struct {
        options: *Object,
        resolved_locale: ResolutionType,
    };
}

fn Resolution(comptime ResolutionOptionsType: type) type {
    return struct {
        locale: icu4zig.Locale,
        options: ResolutionOptionsType,
    };
}

fn ResolutionOptions(comptime resolution_option_descriptors: anytype) type {
    const fields_len = resolution_option_descriptors.len;
    var field_names: [fields_len][]const u8 = undefined;
    var field_types: [fields_len]type = undefined;
    const field_attrs: [fields_len]std.builtin.Type.StructField.Attributes = @splat(.{});
    for (resolution_option_descriptors, &field_names, &field_types) |desc, *name, *Type| {
        const @"type": Object.OptionType = if (@hasField(@TypeOf(desc), "type")) desc.type else .string;
        name.* = desc.key;
        Type.* = ?@"type".T();
    }
    return @Struct(.auto, null, &field_names, &field_types, &field_attrs);
}

const Matcher = enum { lookup, best_fit };

/// 9.2.7 ResolveLocale ( availableLocales, requestedLocales, options, relevantExtensionKeys, localeData )
/// tc39.es/ecma402/#sec-resolvelocale
fn resolveLocale(
    agent: *Agent,
    requested_locales: []const icu4zig.Locale,
    matcher: Matcher,
    resolution_options: anytype,
) Resolution(@TypeOf(resolution_options)) {
    _ = matcher;
    const locale = if (requested_locales.len != 0)
        requested_locales[0]
    else
        agent.platform.default_locale;
    return .{
        .locale = locale,
        .options = resolution_options,
    };
}

/// 9.2.8 ResolveOptions ( ctor, localeData, locales, options [ , specialBehaviours [ , modifyResolutionOptions ] ] )
/// https://tc39.es/ecma402/#sec-resolveoptions
pub fn resolveOptions(
    agent: *Agent,
    comptime resolution_option_descriptors: anytype,
    locales: Value,
    options_value: Value,
    special_behaviours: struct {
        require_options: bool = false,
        coerce_options: bool = false,
    },
) Agent.Error!OptionsResolution(Resolution(ResolutionOptions(resolution_option_descriptors))) {
    const gpa = agent.gpa;

    // 1. Let requestedLocales be ? CanonicalizeLocaleList(locales).
    const requested_locales = try canonicalizeLocaleList(agent, locales);

    // 2. If specialBehaviours is present and contains require-options and options is undefined,
    //    throw a TypeError exception.
    if (special_behaviours.require_options and options_value.isUndefined()) {
        return agent.throwException(.type_error, "Options object must not be undefined", .{});
    }

    // 3. If specialBehaviours is present and contains coerce-options, set options to
    //    ? CoerceOptionsToObject(options). Otherwise, set options to ? GetOptionsObject(options).
    const options = if (special_behaviours.coerce_options)
        try options_value.coerceOptionsToObject(agent)
    else
        try options_value.getOptionsObject(agent);

    // 4. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
    //    "best fit").
    const matcher_string = try options.getOption(
        agent,
        "localeMatcher",
        .string,
        &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
        String.fromLiteral("best fit"),
    );
    const matcher = std.StaticStringMap(Matcher).initComptime(&.{
        .{ "lookup", .lookup },
        .{ "best fit", .best_fit },
    }).get(matcher_string.asAscii()).?;

    // 5. Let opt be the Record { [[localeMatcher]]: matcher }.
    var resolution_options: ResolutionOptions(resolution_option_descriptors) = undefined;

    // 6. For each Resolution Option Descriptor desc of ctor.[[ResolutionOptionDescriptors]], do
    inline for (resolution_option_descriptors) |desc| {
        const Desc = @TypeOf(desc);

        // a. If desc has a [[Type]] field, let type be desc.[[Type]]. Otherwise, let type be
        //    string.
        const @"type": Object.OptionType = if (@hasField(Desc, "type")) desc.type else .string;

        // b. If desc has a [[Values]] field, let values be desc.[[Values]]. Otherwise, let values
        //    be empty.
        const values: ?[]const @"type".T() = if (@hasField(Desc, "values")) desc.values else null;

        // c. Let value be ? GetOption(options, desc.[[Property]], type, values, undefined).
        const maybe_value = try options.getOption(agent, desc.property, @"type", values, null);

        // d. If value is not undefined, then
        if (maybe_value) |value| {
            // i. Set value to ! ToString(value).
            // ii. If value cannot be matched by the `type` Unicode locale nonterminal, throw a
            //     RangeError exception.
            if (@"type" == .string) {
                const value_utf8 = try value.toUtf8(gpa);
                defer gpa.free(value_utf8);
                if (!matchUnicodeLocaleIdentifierType(value_utf8)) {
                    return agent.throwException(
                        .range_error,
                        "Invalid locale identifier type '{f}'",
                        .{value.fmtEscaped()},
                    );
                }
            }
        }

        // e. Let key be desc.[[Key]].
        // f. Set opt.[[<key>]] to value.
        @field(resolution_options, desc.key) = maybe_value;
    }

    // TODO: 7. If modifyResolutionOptions is present, perform ! modifyResolutionOptions(opt).

    // 8. Let resolution be ResolveLocale(ctor.[[AvailableLocales]], requestedLocales, opt,
    //    ctor.[[RelevantExtensionKeys]], localeData).
    const resolution = resolveLocale(
        agent,
        requested_locales.items,
        matcher,
        resolution_options,
    );

    // 9. Return the Record { [[Options]]: options, [[ResolvedLocale]]: resolution,
    //    [[ResolutionOptions]]: opt }.
    return .{ .options = options, .resolved_locale = resolution };
}

/// 9.2.12 GetBooleanOrStringNumberFormatOption ( options, propertyKey, stringValues, fallback )
/// https://tc39.es/ecma402/#sec-getbooleanorstringnumberformatoption
pub fn getBooleanOrStringNumberFormatOption(
    agent: *Agent,
    options: *Object,
    comptime property_key: []const u8,
    string_values: []const *const String,
    fallback: *const String,
) Agent.Error!union(enum) {
    bool: bool,
    string: *const String,
} {
    // 1. Let value be ? Get(options, propertyKey).
    const value = try options.get(agent, PropertyKey.from(property_key));

    // 2. If value is undefined, return fallback.
    if (value.isUndefined()) return .{ .string = fallback };

    // 3. If value is true, return true.
    if (value.isBoolean() and value.asBoolean()) return .{ .bool = true };

    // 4. If ToBoolean(value) is false, return false.
    if (!value.toBoolean()) return .{ .bool = false };

    // 5. Set value to ? ToString(value).
    const string_value = try value.toString(agent);

    // 6. If stringValues does not contain value, throw a RangeError exception.
    for (string_values) |allowed_string_value| {
        if (string_value.eql(allowed_string_value)) break;
    } else {
        return agent.throwException(.range_error, "Invalid value for option '{s}'", .{property_key});
    }

    // 7. Return value.
    return .{ .string = string_value };
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

/// 9.2.14 GetNumberOption ( options, propertyKey, minimum, maximum, fallback )
/// https://tc39.es/ecma402/#sec-getnumberoption
pub fn getNumberOption(
    agent: *Agent,
    options: *Object,
    comptime property_key: []const u8,
    minimum: i32,
    maximum: i32,
    fallback: ?i32,
) Agent.Error!?i32 {
    // 1. Let value be ? Get(options, propertyKey).
    const value = try options.get(agent, PropertyKey.from(property_key));

    // 2. Return ? DefaultNumberOption(value, minimum, maximum, fallback).
    return defaultNumberOption(agent, value, property_key, minimum, maximum, fallback);
}
