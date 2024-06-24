//! 16 PluralRules Objects
//! hthttps://tc39.es/ecma402/#pluralrules-objects

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
const Number = types.Number;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const coerceOptionsToObject = abstract_operations.coerceOptionsToObject;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getOption = types.getOption;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 16.2 Properties of the Intl.PluralRules Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-pluralrules-constructor
pub const PluralRulesConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "PluralRules",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 16.2.1 Intl.PluralRules.prototype
        // https://tc39.es/ecma402/#sec-intl.pluralrules.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.PluralRules.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 16.3.1 Intl.PluralRules.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.pluralrules.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Intl.PluralRules.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 16.1.1 Intl.PluralRules ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.pluralrules
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
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
        //    [[MinimumIntegerDigits]], [[MinimumFractionDigits]], [[MaximumFractionDigits]],
        //    [[MinimumSignificantDigits]], [[MaximumSignificantDigits]], [[RoundingType]],
        //    [[RoundingIncrement]], [[RoundingMode]], [[ComputedRoundingPriority]],
        //    [[TrailingZeroDisplay]] »).
        const plural_rules = try ordinaryCreateFromConstructor(
            PluralRules,
            agent,
            new_target.?,
            "%Intl.PluralRules.prototype%",
            .{
                .locale = undefined,
                .type = undefined,
            },
        );

        // 3. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 4. Set options to ? CoerceOptionsToObject(options).
        const options = try coerceOptionsToObject(agent, options_value);

        // 5. Let opt be a new Record.

        // 6. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup",
        //    "best fit" », "best fit").
        const matcher = try getOption(
            options,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 7. Set opt.[[localeMatcher]] to matcher.
        _ = matcher;

        // 8. Let localeData be %Intl.PluralRules%.[[LocaleData]].
        // TODO: 9. Let r be ResolveLocale(%Intl.PluralRules%.[[AvailableLocales]], requestedLocales,
        //          opt, %Intl.PluralRules%.[[RelevantExtensionKeys]], localeData).
        const resolved_locale = if (requested_locales.items.len != 0)
            requested_locales.items[0]
        else
            icu4zig.Locale.init(null) catch unreachable;

        // 10. Set pluralRules.[[Locale]] to r.[[locale]].
        plural_rules.as(PluralRules).fields.locale = resolved_locale;

        // 11. Let t be ? GetOption(options, "type", string, « "cardinal", "ordinal" », "cardinal").
        const type_ = try getOption(
            options,
            "type",
            .string,
            &.{ String.fromLiteral("cardinal"), String.fromLiteral("ordinal") },
            String.fromLiteral("cardinal"),
        );

        // 12. Set pluralRules.[[Type]] to t.
        const type_map = std.StaticStringMap(
            PluralRules.Fields.Type,
        ).initComptime(&.{
            .{ "cardinal", .cardinal },
            .{ "ordinal", .ordinal },
        });
        plural_rules.as(PluralRules).fields.type = type_map.get(type_.ascii).?;

        // TODO: 13. Perform ? SetNumberFormatDigitOptions(pluralRules, options, 0, 3, "standard").

        // 14. Return pluralRules.
        return Value.from(plural_rules);
    }
};

/// 16.3 Properties of the Intl.PluralRules Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-pluralrules-prototype-object
pub const PluralRulesPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "select", select, 1, realm);

        // 16.3.2 Intl.PluralRules.prototype [ @@toStringTag ]
        // https://tc39.es/ecma402/#sec-intl.pluralrules.prototype-tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Intl.PluralRules"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 16.3.3 Intl.PluralRules.prototype.select ( value )
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
            String.fromAscii(@tagName(resolvePlural(plural_rules, n).plural_category)),
        );
    }
};

/// 16.4 Properties of Intl.PluralRules Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-pluralrules-instances
pub const PluralRules = MakeObject(.{
    .Fields = struct {
        pub const Type = enum {
            cardinal,
            ordinal,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Type]]
        type: Type,
    },
    .tag = .intl_plural_rules,
});

/// 16.5.4 ResolvePlural ( pluralRules, n )
/// https://tc39.es/ecma402/#sec-resolveplural
pub fn resolvePlural(plural_rules_object: *const PluralRules, n: Number) struct {
    /// [[PluralCategory]]
    plural_category: icu4zig.PluralRules.Category,

    // TODO: [[FormattedString]]
} {
    // 1. If n is not a finite Number, then
    if (!n.isFinite()) {
        // a. Let s be ! ToString(n).
        // b. Return the Record { [[PluralCategory]]: "other", [[FormattedString]]: s }.
        return .{ .plural_category = .other };
    }

    // 2. Let locale be pluralRules.[[Locale]].
    const locale = plural_rules_object.fields.locale;

    // 3. Let type be pluralRules.[[Type]].
    const @"type" = plural_rules_object.fields.type;

    // TODO: 4. Let res be FormatNumericToString(pluralRules, ℝ(n)).
    // TODO: 5. Let s be res.[[FormattedString]].

    // 6. Let operands be GetOperands(s).
    // 7. Let p be PluralRuleSelect(locale, type, n, operands).
    const data_provider = icu4zig.DataProvider.init();
    defer data_provider.deinit();
    const plural_rules = icu4zig.PluralRules.init(data_provider, locale, switch (@"type") {
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
