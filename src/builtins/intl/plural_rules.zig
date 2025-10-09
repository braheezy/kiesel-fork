//! 17 PluralRules Objects
//! hthttps://tc39.es/ecma402/#pluralrules-objects

const std = @import("std");

const icu4zig = @import("icu4zig");

const abstract_operations = @import("abstract_operations.zig");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Number = types.Number;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 17.2 Properties of the Intl.PluralRules Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-pluralrules-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "PluralRules",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 17.2.1 Intl.PluralRules.prototype
        // https://tc39.es/ecma402/#sec-intl.pluralrules.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Intl.PluralRules.prototype%"()),
            .none,
        );
    }

    /// 17.1.1 Intl.PluralRules ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.pluralrules
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "Intl.PluralRules must be constructed with 'new'",
                .{},
            );
        }

        // 2. Let pluralRules be ? OrdinaryCreateFromConstructor(NewTarget,
        //    "%Intl.PluralRules.prototype%", « [[InitializedPluralRules]], [[Locale]], [[Type]],
        //    [[Notation]], [[MinimumIntegerDigits]], [[MinimumFractionDigits]],
        //    [[MaximumFractionDigits]], [[MinimumSignificantDigits]],
        //    [[MaximumSignificantDigits]], [[RoundingType]], [[RoundingIncrement]],
        //    [[RoundingMode]], [[ComputedRoundingPriority]], [[TrailingZeroDisplay]] »).
        const plural_rules = try ordinaryCreateFromConstructor(
            PluralRules,
            agent,
            new_target.?,
            "%Intl.PluralRules.prototype%",
            .{
                .locale = undefined,
                .type = undefined,
                .notation = undefined,
            },
        );

        // 3. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 4. Set options to ? CoerceOptionsToObject(options).
        const options = try options_value.coerceOptionsToObject(agent);

        // 5. Let opt be a new Record.

        // 6. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup",
        //    "best fit" », "best fit").
        const matcher = try options.getOption(
            agent,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 7. Set opt.[[localeMatcher]] to matcher.
        _ = matcher;

        // TODO: 8. Let r be ResolveLocale(%Intl.PluralRules%.[[AvailableLocales]], requestedLocales,
        //          opt, %Intl.PluralRules%.[[RelevantExtensionKeys]], %Intl.PluralRules%.[[LocaleData]]).
        const resolved_locale = if (requested_locales.items.len != 0)
            requested_locales.items[0]
        else
            agent.platform.default_locale;

        // 9. Set pluralRules.[[Locale]] to r.[[Locale]].
        plural_rules.fields.locale = resolved_locale;

        // 10. Let t be ? GetOption(options, "type", string, « "cardinal", "ordinal" », "cardinal").
        const type_ = try options.getOption(
            agent,
            "type",
            .string,
            &.{ String.fromLiteral("cardinal"), String.fromLiteral("ordinal") },
            String.fromLiteral("cardinal"),
        );

        // 11. Set pluralRules.[[Type]] to t.
        const type_map = std.StaticStringMap(
            PluralRules.Fields.Type,
        ).initComptime(&.{
            .{ "cardinal", .cardinal },
            .{ "ordinal", .ordinal },
        });
        plural_rules.fields.type = type_map.get(type_.asAscii()).?;

        // 12. Let notation be ? GetOption(options, "notation", string, « "standard", "scientific",
        //     "engineering", "compact" », "standard").
        const notation = try options.getOption(
            agent,
            "notation",
            .string,
            &.{
                String.fromLiteral("standard"),
                String.fromLiteral("scientific"),
                String.fromLiteral("engineering"),
                String.fromLiteral("compact"),
            },
            String.fromLiteral("standard"),
        );

        // 13. Set pluralRules.[[Notation]] to notation.
        const notation_map = std.StaticStringMap(
            PluralRules.Fields.Notation,
        ).initComptime(&.{
            .{ "standard", .standard },
            .{ "scientific", .scientific },
            .{ "engineering", .engineering },
            .{ "compact", .compact },
        });
        plural_rules.fields.notation = notation_map.get(notation.asAscii()).?;

        // TODO: 14. Perform ? SetNumberFormatDigitOptions(pluralRules, options, 0, 3, "standard").

        // 15. Return pluralRules.
        return Value.from(&plural_rules.object);
    }
};

/// 17.3 Properties of the Intl.PluralRules Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-pluralrules-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "resolvedOptions", resolvedOptions, 0, realm);
        try object.defineBuiltinFunction(agent, "select", select, 1, realm);

        // 17.3.1 Intl.PluralRules.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.pluralrules.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.PluralRules%"()),
        );

        // 17.3.5 Intl.PluralRules.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.pluralrules.prototype-tostringtag
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Intl.PluralRules"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 17.3.2 Intl.PluralRules.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-intl.pluralrules.prototype.resolvedoptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let pr be the this value.
        // 2. Perform ? RequireInternalSlot(pr, [[InitializedPluralRules]]).
        const plural_rules = try this_value.requireInternalSlot(agent, PluralRules);

        // 3. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. Let pluralCategories be a List of Strings containing all possible results of
        //    PluralRuleSelect for the selected locale pr.[[Locale]], sorted according to the
        //    following order: "zero", "one", "two", "few", "many", "other".
        var plural_categories = blk: {
            const plural_rules_ = icu4zig.PluralRules.init(
                plural_rules.fields.locale,
                switch (plural_rules.fields.type) {
                    .cardinal => .cardinal,
                    .ordinal => .ordinal,
                },
            );
            defer plural_rules_.deinit();
            const plural_categories = plural_rules_.categories();
            var array_list: std.ArrayList(Value) = .empty;
            if (plural_categories.zero) try array_list.append(agent.gc_allocator, Value.from("zero"));
            if (plural_categories.one) try array_list.append(agent.gc_allocator, Value.from("one"));
            if (plural_categories.two) try array_list.append(agent.gc_allocator, Value.from("two"));
            if (plural_categories.few) try array_list.append(agent.gc_allocator, Value.from("few"));
            if (plural_categories.many) try array_list.append(agent.gc_allocator, Value.from("many"));
            if (plural_categories.other) try array_list.append(agent.gc_allocator, Value.from("other"));
            break :blk array_list;
        };
        defer plural_categories.deinit(agent.gc_allocator);

        // 5. For each row of Table 25, except the header row, in table order, do
        //     a. Let p be the Property value of the current row.
        //     b. If p is "pluralCategories", then
        //         i. Let v be CreateArrayFromList(pluralCategories).
        //     c. Else,
        //         i. Let v be the value of pr's internal slot whose name is the Internal Slot value of the current row.
        //     d. If v is not undefined, then
        //         i. If there is a Conversion value in the current row, then
        //             1. Assert: The Conversion value of the current row is number.
        //             2. Set v to 𝔽(v).
        //         ii. Perform ! CreateDataPropertyOrThrow(options, p, v).
        const resolved_options = plural_rules.fields.resolvedOptions();
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("locale"),
            Value.from(
                try String.fromAscii(
                    agent,
                    try plural_rules.fields.locale.toString(agent.gc_allocator),
                ),
            ),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("type"),
            Value.from(resolved_options.type),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("notation"),
            Value.from(resolved_options.notation),
        );
        const plural_categories_array = try createArrayFromList(agent, plural_categories.items);
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("pluralCategories"),
            Value.from(&plural_categories_array.object),
        );

        // TODO: minimumIntegerDigits, minimumFractionDigits, maximumFractionDigits,
        //       minimumSignificantDigits, maximumSignificantDigits, roundingIncrement,
        //       roundingMode, roundingPriority, trailingZeroDisplay

        // 6. Return options.
        return Value.from(options);
    }

    /// 17.3.3 Intl.PluralRules.prototype.select ( value )
    /// https://tc39.es/ecma402/#sec-intl.pluralrules.prototype.select
    fn select(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Let pr be the this value.
        // 2. Perform ? RequireInternalSlot(pr, [[InitializedPluralRules]]).
        const plural_rules = try this_value.requireInternalSlot(agent, PluralRules);

        // 3. Let n be ? ToNumber(value).
        const n = try value.toNumber(agent);

        // 4. Return ResolvePlural(pr, n).[[PluralCategory]].
        return Value.from(
            try String.fromAscii(agent, @tagName(resolvePlural(plural_rules, n).plural_category)),
        );
    }
};

/// 17.4 Properties of Intl.PluralRules Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-pluralrules-instances
pub const PluralRules = MakeObject(.{
    .Fields = struct {
        pub const Type = enum {
            cardinal,
            ordinal,
        };

        pub const Notation = enum {
            standard,
            scientific,
            engineering,
            compact,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Type]]
        type: Type,

        /// [[Notation]]
        notation: Notation,

        pub const ResolvedOptions = struct {
            type: *const String,
            notation: *const String,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            const @"type" = switch (self.type) {
                .cardinal => String.fromLiteral("cardinal"),
                .ordinal => String.fromLiteral("ordinal"),
            };
            const notation = switch (self.notation) {
                .standard => String.fromLiteral("standard"),
                .scientific => String.fromLiteral("scientific"),
                .engineering => String.fromLiteral("engineering"),
                .compact => String.fromLiteral("compact"),
            };
            return .{
                .type = @"type",
                .notation = notation,
            };
        }
    },
    .tag = .intl_plural_rules,
});

/// 17.5.4 ResolvePlural ( pluralRules, n )
/// https://tc39.es/ecma402/#sec-resolveplural
pub fn resolvePlural(plural_rules_object: *const PluralRules, n: Number) struct {
    /// [[PluralCategory]]
    plural_category: icu4zig.PluralRules.PluralCategory,

    // TODO: [[FormattedString]]
} {
    // 1. If n is not a finite Number, then
    if (!n.isFinite()) {
        // a. Let s be ! ToString(n).
        // b. Return the Record { [[PluralCategory]]: "other", [[FormattedString]]: s }.
        return .{ .plural_category = .other };
    }

    // TODO: 2. Let res be FormatNumericToString(pluralRules, ℝ(n)).
    // TODO: 3. Let s be res.[[FormattedString]].

    // 4. Let locale be pluralRules.[[Locale]].
    const locale = plural_rules_object.fields.locale;

    // 5. Let type be pluralRules.[[Type]].
    const @"type" = plural_rules_object.fields.type;

    // 6. Let notation be pluralRules.[[Notation]].
    const notation = plural_rules_object.fields.notation;

    // 7. Let p be PluralRuleSelect(locale, type, notation, s).
    _ = notation; // TODO: Use this once ICU4X supports it.
    const plural_rules = icu4zig.PluralRules.init(locale, switch (@"type") {
        .cardinal => .cardinal,
        .ordinal => .ordinal,
    });
    defer plural_rules.deinit();
    const plural_category = plural_rules.categoryFor(switch (n) {
        .i32 => |value| .{ .i32 = value },
        .f64 => |value| .{ .f64 = value },
    }) catch unreachable;

    // 8. Return the Record { [[PluralCategory]]: p, [[FormattedString]]: s }.
    return .{ .plural_category = plural_category };
}
