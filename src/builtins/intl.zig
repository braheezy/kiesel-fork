//! 8 The Intl Object
//! https://tc39.es/ecma402/#intl-object

const std = @import("std");

const icu4zig = @import("icu4zig");

const abstract_operations = @import("intl/abstract_operations.zig");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const availableCalendars = abstract_operations.availableCalendars;
const availableCanonicalNumberingSystems = abstract_operations.availableCanonicalNumberingSystems;
const availableCanonicalUnits = abstract_operations.availableCanonicalUnits;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

comptime {
    const build_options = @import("build-options");
    if (!build_options.enable_intl) @compileError("Intl is not enabled");
}

pub const collator = @import("./intl/collator.zig");
pub const date_time_format = @import("./intl/date_time_format.zig");
pub const display_names = @import("./intl/display_names.zig");
pub const list_format = @import("./intl/list_format.zig");
pub const locale = @import("./intl/locale.zig");
pub const plural_rules = @import("./intl/plural_rules.zig");
pub const segment_iterator = @import("./intl/segment_iterator.zig");
pub const segmenter = @import("./intl/segmenter.zig");
pub const segments = @import("./intl/segments.zig");

pub const Collator = collator.Collator;
pub const DateTimeFormat = date_time_format.DateTimeFormat;
pub const DisplayNames = display_names.DisplayNames;
pub const ListFormat = list_format.ListFormat;
pub const Locale = locale.Locale;
pub const PluralRules = plural_rules.PluralRules;
pub const SegmentIterator = segment_iterator.SegmentIterator;
pub const Segmenter = segmenter.Segmenter;
pub const Segments = segments.Segments;

pub const createSegmentDataObject = segment_iterator.createSegmentDataObject;
pub const createSegmentIterator = segment_iterator.createSegmentIterator;
pub const createSegmentsObject = segments.createSegmentsObject;
pub const findBoundary = segmenter.findBoundary;

pub const namespace = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "getCanonicalLocales", getCanonicalLocales, 1, realm);
        try defineBuiltinFunction(object, "supportedValuesOf", supportedValuesOf, 1, realm);

        // 8.1.1 Intl [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-Intl-toStringTag
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Intl"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.1 Intl.Collator ( . . . )
        // https://tc39.es/ecma402/#sec-intl.collator-intro
        try defineBuiltinProperty(object, "Collator", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Collator%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.2 Intl.DateTimeFormat ( . . . )
        // https://tc39.es/ecma402/#sec-intl.datetimeformat-intro
        try defineBuiltinProperty(object, "DateTimeFormat", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.DateTimeFormat%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.3 Intl.DisplayNames ( . . . )
        // https://tc39.es/ecma402/#sec-intl.displaynames-intro
        try defineBuiltinProperty(object, "DisplayNames", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.DisplayNames%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.4 Intl.ListFormat ( . . . )
        // https://tc39.es/ecma402/#sec-intl.listformat-intro
        try defineBuiltinProperty(object, "ListFormat", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.ListFormat%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.5 Intl.Locale ( . . . )
        // https://tc39.es/ecma402/#sec-intl.locale-intro
        try defineBuiltinProperty(object, "Locale", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Locale%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.7 Intl.PluralRules ( . . . )
        // https://tc39.es/ecma402/#sec-intl.pluralrules-intro
        try defineBuiltinProperty(object, "PluralRules", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.PluralRules%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.9 Intl.Segmenter ( . . . )
        // https://tc39.es/ecma402/#sec-intl.segmenter-intro
        try defineBuiltinProperty(object, "Segmenter", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Segmenter%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 8.3.1 Intl.getCanonicalLocales ( locales )
    /// https://tc39.es/ecma402/#sec-intl.getcanonicallocales
    fn getCanonicalLocales(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const locales = arguments.get(0);

        // 1. Let ll be ? CanonicalizeLocaleList(locales).
        const locale_list = try canonicalizeLocaleList(agent, locales);
        defer locale_list.deinit();

        // 2. Return CreateArrayFromList(ll).
        return Value.from(
            try createArrayFromListMapToValue(agent, icu4zig.Locale, locale_list.items, struct {
                fn mapFn(agent_: *Agent, locale_: icu4zig.Locale) std.mem.Allocator.Error!Value {
                    return Value.from(
                        try String.fromAscii(
                            agent_.gc_allocator,
                            try locale_.toString(agent_.gc_allocator),
                        ),
                    );
                }
            }.mapFn),
        );
    }

    /// 8.3.2 Intl.supportedValuesOf ( key )
    /// https://tc39.es/ecma402/#sec-intl.supportedvaluesof
    fn supportedValuesOf(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let key be ? ToString(key).
        const key = try arguments.get(0).toString(agent);

        var list: []const *const String = &.{};

        // 2. If key is "calendar", then
        if (key.eql(String.fromLiteral("calendar"))) {
            // a. Let list be a new empty List.
            // b. For each element identifier of AvailableCalendars(), do
            //     i. Let canonical be CanonicalizeUValue("ca", identifier).
            //     ii. If identifier is canonical, then
            //         1. Append identifier to list.
            list = availableCalendars();
        }
        // 3. Else if key is "collation", then
        else if (key.eql(String.fromLiteral("collation"))) {
            // TODO: a. Let list be AvailableCanonicalCollations( ).
        }
        // 4. Else if key is "currency", then
        else if (key.eql(String.fromLiteral("currency"))) {
            // TODO: a. Let list be AvailableCanonicalCurrencies( ).
        }
        // 5. Else if key is "numberingSystem", then
        else if (key.eql(String.fromLiteral("numberingSystem"))) {
            // a. Let list be AvailableCanonicalNumberingSystems( ).
            list = availableCanonicalNumberingSystems();
        }
        // 6. Else if key is "timeZone", then
        else if (key.eql(String.fromLiteral("timeZone"))) {
            // TODO: a. Let list be AvailablePrimaryTimeZoneIdentifiers( ).
            // See https://github.com/unicode-org/icu4x/issues/3970
        }
        // 7. Else if key is "unit", then
        else if (key.eql(String.fromLiteral("unit"))) {
            // a. Let list be AvailableCanonicalUnits( ).
            list = availableCanonicalUnits();
        }
        // 8. Else,
        else {
            // a. Throw a RangeError exception.
            return agent.throwException(.range_error, "Invalid key '{}'", .{key});
        }

        // 9. Return CreateArrayFromList( list ).
        return Value.from(
            try createArrayFromListMapToValue(agent, *const String, list, struct {
                fn mapFn(_: *Agent, string: *const String) std.mem.Allocator.Error!Value {
                    return Value.from(string);
                }
            }.mapFn),
        );
    }
};
