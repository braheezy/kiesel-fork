//! 12 DisplayNames Objects
//! https://tc39.es/ecma402/#intl-displaynames-objects

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
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getOption = types.getOption;
const getOptionsObject = abstract_operations.getOptionsObject;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 12.2 Properties of the Intl.DisplayNames Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-displaynames-constructor
pub const DisplayNamesConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 2,
            .name = "DisplayNames",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 12.2.1 Intl.DisplayNames.prototype
        // https://tc39.es/ecma402/#sec-Intl.DisplayNames.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.DisplayNames.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 12.3.1 Intl.DisplayNames.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.DisplayNames.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Intl.DisplayNames.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 12.1.1 Intl.DisplayNames ( locales, options )
    /// https://tc39.es/ecma402/#sec-Intl.DisplayNames
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "Intl.DisplayNames must be constructed with 'new'",
                .{},
            );
        }

        // 2. Let displayNames be ? OrdinaryCreateFromConstructor(NewTarget,
        //    "%Intl.DisplayNames.prototype%", « [[InitializedDisplayNames]], [[Locale]],
        //    [[Style]], [[Type]], [[Fallback]], [[LanguageDisplay]], [[Fields]] »).
        const display_names = try ordinaryCreateFromConstructor(
            DisplayNames,
            agent,
            new_target.?,
            "%Intl.DisplayNames.prototype%",
            .{
                .locale = undefined,
                .type = undefined,
                .options = .{
                    .style = undefined,
                    .fallback = undefined,
                    .language_display = undefined,
                },
            },
        );

        // 3. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 4. If options is undefined, throw a TypeError exception.
        if (options_value == .undefined) {
            return agent.throwException(.type_error, "Options object must not be undefined", .{});
        }

        // 5. Set options to ? GetOptionsObject(options).
        const options = try getOptionsObject(agent, options_value);

        // 6. Let opt be a new Record.

        // 7. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
        //    "best fit").
        const matcher = try getOption(
            options,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 8. Set opt.[[localeMatcher]] to matcher.
        // TODO: 9. Let r be ResolveLocale(%Intl.DisplayNames%.[[AvailableLocales]], requestedLocales,
        //          opt, %Intl.DisplayNames%.[[RelevantExtensionKeys]], %Intl.DisplayNames%.[[LocaleData]]).
        _ = matcher;
        const resolved_locale = if (requested_locales.items.len != 0)
            requested_locales.items[0]
        else
            agent.platform.default_locale;

        // 10. Let style be ? GetOption(options, "style", string, « "narrow", "short", "long" »,
        //     "long").
        const style = try getOption(
            options,
            "style",
            .string,
            &.{
                String.fromLiteral("narrow"),
                String.fromLiteral("short"),
                String.fromLiteral("long"),
            },
            String.fromLiteral("long"),
        );

        // 11. Set displayNames.[[Style]] to style.
        const style_map = std.StaticStringMap(
            icu4zig.LocaleDisplayNamesFormatter.Options.Style,
        ).initComptime(&.{
            .{ "narrow", .narrow },
            .{ "short", .short },
            .{ "long", .long },
        });
        display_names.as(DisplayNames).fields.options.style = style_map.get(style.ascii).?;

        // 12. Let type be ? GetOption(options, "type", string, « "language", "region", "script",
        //     "currency", "calendar", "dateTimeField" », undefined).
        const @"type" = try getOption(
            options,
            "type",
            .string,
            &.{
                String.fromLiteral("language"),
                String.fromLiteral("region"),
                String.fromLiteral("script"),
                String.fromLiteral("currency"),
                String.fromLiteral("calendar"),
                String.fromLiteral("dateTimeField"),
            },
            null,
        );

        // 13. If type is undefined, throw a TypeError exception.
        if (@"type" == null) {
            return agent.throwException(.type_error, "'type' option must not be undefined", .{});
        }

        // 14. Set displayNames.[[Type]] to type.
        const type_map = std.StaticStringMap(
            DisplayNames.Fields.Type,
        ).initComptime(&.{
            .{ "language", .language },
            .{ "region", .region },
            .{ "script", .script },
            .{ "currency", .currency },
            .{ "calendar", .calendar },
            .{ "dateTimeField", .date_time_field },
        });
        display_names.as(DisplayNames).fields.type = type_map.get(@"type".?.ascii).?;

        // 15. Let fallback be ? GetOption(options, "fallback", string, « "code", "none" », "code").
        const fallback = try getOption(
            options,
            "fallback",
            .string,
            &.{
                String.fromLiteral("code"),
                String.fromLiteral("none"),
            },
            String.fromLiteral("code"),
        );

        // 16. Set displayNames.[[Fallback]] to fallback.
        const fallback_map = std.StaticStringMap(
            icu4zig.LocaleDisplayNamesFormatter.Options.Fallback,
        ).initComptime(&.{
            .{ "code", .code },
            .{ "none", .none },
        });
        display_names.as(DisplayNames).fields.options.fallback = fallback_map.get(fallback.ascii).?;

        // 17. Set displayNames.[[Locale]] to r.[[Locale]].
        display_names.as(DisplayNames).fields.locale = resolved_locale;

        // 18. Let resolvedLocaleData be r.[[LocaleData]].
        // 19. Let types be resolvedLocaleData.[[types]].
        // 20. Assert: types is a Record (see 12.2.3).

        // 21. Let languageDisplay be ? GetOption(options, "languageDisplay", string, « "dialect",
        //     "standard" », "dialect").
        const language_display = try getOption(
            options,
            "languageDisplay",
            .string,
            &.{
                String.fromLiteral("dialect"),
                String.fromLiteral("standard"),
            },
            String.fromLiteral("dialect"),
        );

        // 22. Let typeFields be types.[[<type>]].
        // 23. Assert: typeFields is a Record (see 12.2.3).
        // 24. If type is "language", then
        //     a. Set displayNames.[[LanguageDisplay]] to languageDisplay.
        //     b. Set typeFields to typeFields.[[<languageDisplay>]].
        //     c. Assert: typeFields is a Record (see 12.2.3).
        // NOTE: We do this unconditionally as it's part of the options struct.
        const language_display_map = std.StaticStringMap(
            icu4zig.LocaleDisplayNamesFormatter.Options.LanguageDisplay,
        ).initComptime(&.{
            .{ "dialect", .dialect },
            .{ "standard", .standard },
        });
        display_names.as(DisplayNames).fields.options.language_display = language_display_map.get(language_display.ascii).?;

        // 25. Let styleFields be typeFields.[[<style>]].
        // 26. Assert: styleFields is a Record (see 12.2.3).
        // 27. Set displayNames.[[Fields]] to styleFields.

        // 28. Return displayNames.
        return Value.from(display_names);
    }
};

/// 12.3 Properties of the Intl.DisplayNames Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-displaynames-prototype-object
pub const DisplayNamesPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        return object;
    }
};

/// 12.4 Properties of Intl.DisplayNames Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-displaynames-instances
pub const DisplayNames = MakeObject(.{
    .Fields = struct {
        pub const Type = enum {
            language,
            region,
            script,
            currency,
            calendar,
            date_time_field,
        };

        pub const ResolvedOptions = struct {
            style: String,
            type: String,
            fallback: String,
            language_display: String,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Type]]
        type: Type,

        /// [[Style]], [[Fallback]], [[LanguageDisplay]]
        options: icu4zig.LocaleDisplayNamesFormatter.Options,

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            const @"type" = switch (self.type) {
                .date_time_field => String.fromLiteral("dateTimeField"),
                else => String.fromAscii(@tagName(self.type)),
            };
            const style = String.fromAscii(@tagName(self.options.style));
            const fallback = String.fromAscii(@tagName(self.options.fallback));
            const language_display = String.fromAscii(@tagName(self.options.language_display));
            return .{
                .style = style,
                .type = @"type",
                .fallback = fallback,
                .language_display = language_display,
            };
        }
    },
    .tag = .intl_display_names,
});
