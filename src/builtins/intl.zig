//! 8 The Intl Object
//! https://tc39.es/ecma402/#intl-object

const std = @import("std");

const Allocator = std.mem.Allocator;

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
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

comptime {
    const build_options = @import("build-options");
    if (!build_options.enable_intl) @compileError("Intl is not enabled");
}

pub const Intl = struct {
    pub const Collator = @import("./intl/collator.zig").Collator;
    pub const CollatorConstructor = @import("./intl/collator.zig").CollatorConstructor;
    pub const CollatorPrototype = @import("./intl/collator.zig").CollatorPrototype;
    pub const DateTimeFormat = @import("./intl/date_time_format.zig").DateTimeFormat;
    pub const DateTimeFormatConstructor = @import("./intl/date_time_format.zig").DateTimeFormatConstructor;
    pub const DateTimeFormatPrototype = @import("./intl/date_time_format.zig").DateTimeFormatPrototype;
    pub const DisplayNames = @import("./intl/display_names.zig").DisplayNames;
    pub const DisplayNamesConstructor = @import("./intl/display_names.zig").DisplayNamesConstructor;
    pub const DisplayNamesPrototype = @import("./intl/display_names.zig").DisplayNamesPrototype;
    pub const ListFormat = @import("./intl/list_format.zig").ListFormat;
    pub const ListFormatConstructor = @import("./intl/list_format.zig").ListFormatConstructor;
    pub const ListFormatPrototype = @import("./intl/list_format.zig").ListFormatPrototype;
    pub const Locale = @import("./intl/locale.zig").Locale;
    pub const LocaleConstructor = @import("./intl/locale.zig").LocaleConstructor;
    pub const LocalePrototype = @import("./intl/locale.zig").LocalePrototype;
    pub const PluralRules = @import("./intl/plural_rules.zig").PluralRules;
    pub const PluralRulesConstructor = @import("./intl/plural_rules.zig").PluralRulesConstructor;
    pub const PluralRulesPrototype = @import("./intl/plural_rules.zig").PluralRulesPrototype;
    pub const Segmenter = @import("./intl/segmenter.zig").Segmenter;
    pub const SegmenterConstructor = @import("./intl/segmenter.zig").SegmenterConstructor;
    pub const SegmenterPrototype = @import("./intl/segmenter.zig").SegmenterPrototype;
    pub const IntlSegmentsPrototype = @import("./intl/segmenter.zig").IntlSegmentsPrototype;
    pub const IntlSegmentIteratorPrototype = @import("./intl/segmenter.zig").IntlSegmentIteratorPrototype;

    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "getCanonicalLocales", getCanonicalLocales, 1, realm);

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

        return object;
    }

    fn getCanonicalLocales(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const locales = arguments.get(0);

        // 1. Let ll be ? CanonicalizeLocaleList(locales).
        const locale_list = try canonicalizeLocaleList(agent, locales);
        defer locale_list.deinit();

        // 2. Return CreateArrayFromList(ll).
        return Value.from(
            try createArrayFromListMapToValue(agent, icu4zig.Locale, locale_list.items, struct {
                fn mapFn(agent_: *Agent, locale: icu4zig.Locale) Allocator.Error!Value {
                    return Value.from(String.fromAscii(try locale.toString(agent_.gc_allocator)));
                }
            }.mapFn),
        );
    }
};
