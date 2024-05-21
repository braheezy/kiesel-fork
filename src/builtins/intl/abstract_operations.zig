//! 9.2 Abstract Operations
//! https://tc39.es/ecma402/#sec-abstract-operations

const std = @import("std");

const icu4zig = @import("icu4zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;

const LocaleList = std.ArrayList(icu4zig.Locale);

/// 9.2.1 CanonicalizeLocaleList ( locales )
/// https://tc39.es/ecma402/#sec-canonicalizelocalelist
pub fn canonicalizeLocaleList(agent: *Agent, locales: Value) Agent.Error!LocaleList {
    // 1. If locales is undefined, then
    if (locales == .undefined) {
        // a. Return a new empty List.
        return LocaleList.init(agent.gc_allocator);
    }

    // 2. Let seen be a new empty List.
    var seen = LocaleList.init(agent.gc_allocator);

    // 3. If Type(locales) is String or Type(locales) is Object and locales has an
    //    [[InitializedLocale]] internal slot, then
    const object = if (locales == .string or
        locales == .object and locales.object.is(builtins.Intl.Locale))
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
            if (k_value != .string and k_value != .object) {
                return agent.throwException(
                    .type_error,
                    "Locale list items must be strings or objects",
                    .{},
                );
            }

            // iii. If Type(kValue) is Object and kValue has an [[InitializedLocale]] internal slot, then
            const tag = if (k_value == .object and k_value.object.is(builtins.Intl.Locale)) blk: {
                // 1. Let tag be kValue.[[Locale]].
                break :blk String.fromAscii(
                    try k_value.object.as(builtins.Intl.Locale).fields.locale.toString(agent.gc_allocator),
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
            const canonicalized_tag = icu4zig.Locale.init(try tag.toUtf8(agent.gc_allocator)) catch |err| switch (err) {
                error.LocaleParserLanguageError,
                error.LocaleParserSubtagError,
                error.LocaleParserExtensionError,
                => return agent.throwException(
                    .range_error,
                    "Invalid locale identifier '{}'",
                    .{tag},
                ),
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
