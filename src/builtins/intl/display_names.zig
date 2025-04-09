//! 12 DisplayNames Objects
//! https://tc39.es/ecma402/#intl-displaynames-objects

const std = @import("std");

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
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getOptionsObject = abstract_operations.getOptionsObject;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 12.2 Properties of the Intl.DisplayNames Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-displaynames-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            realm.agent,
            .{ .constructor = impl },
            2,
            "DisplayNames",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 12.2.1 Intl.DisplayNames.prototype
        // https://tc39.es/ecma402/#sec-Intl.DisplayNames.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.DisplayNames.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 12.1.1 Intl.DisplayNames ( locales, options )
    /// https://tc39.es/ecma402/#sec-Intl.DisplayNames
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
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
        if (options_value.isUndefined()) {
            return agent.throwException(.type_error, "Options object must not be undefined", .{});
        }

        // 5. Set options to ? GetOptionsObject(options).
        const options = try getOptionsObject(agent, options_value);

        // 6. Let opt be a new Record.

        // 7. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
        //    "best fit").
        const matcher = try options.getOption(
            agent,
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
        const style = try options.getOption(
            agent,
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
            icu4zig.DisplayNamesOptions.Style,
        ).initComptime(&.{
            .{ "narrow", .narrow },
            .{ "short", .short },
            .{ "long", .long },
        });
        display_names.as(DisplayNames).fields.options.style = style_map.get(style.slice.ascii).?;

        // 12. Let type be ? GetOption(options, "type", string, « "language", "region", "script",
        //     "currency", "calendar", "dateTimeField" », undefined).
        const @"type" = try options.getOption(
            agent,
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
        display_names.as(DisplayNames).fields.type = type_map.get(@"type".?.slice.ascii).?;

        // 15. Let fallback be ? GetOption(options, "fallback", string, « "code", "none" », "code").
        const fallback = try options.getOption(
            agent,
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
            icu4zig.DisplayNamesOptions.Fallback,
        ).initComptime(&.{
            .{ "code", .code },
            .{ "none", .none },
        });
        display_names.as(DisplayNames).fields.options.fallback = fallback_map.get(fallback.slice.ascii).?;

        // 17. Set displayNames.[[Locale]] to r.[[Locale]].
        display_names.as(DisplayNames).fields.locale = resolved_locale;

        // 18. Let resolvedLocaleData be r.[[LocaleData]].
        // 19. Let types be resolvedLocaleData.[[types]].
        // 20. Assert: types is a Record (see 12.2.3).

        // 21. Let languageDisplay be ? GetOption(options, "languageDisplay", string, « "dialect",
        //     "standard" », "dialect").
        const language_display = try options.getOption(
            agent,
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
            icu4zig.DisplayNamesOptions.LanguageDisplay,
        ).initComptime(&.{
            .{ "dialect", .dialect },
            .{ "standard", .standard },
        });
        display_names.as(DisplayNames).fields.options.language_display = language_display_map.get(language_display.slice.ascii).?;

        // 25. Let styleFields be typeFields.[[<style>]].
        // 26. Assert: styleFields is a Record (see 12.2.3).
        // 27. Set displayNames.[[Fields]] to styleFields.

        // 28. Return displayNames.
        return Value.from(display_names);
    }
};

/// 12.3 Properties of the Intl.DisplayNames Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-displaynames-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "resolvedOptions", resolvedOptions, 0, realm);
        try defineBuiltinFunction(object, "of", of, 1, realm);

        // 12.3.1 Intl.DisplayNames.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.DisplayNames.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.DisplayNames%"()),
        );

        // 12.3.4 Intl.DisplayNames.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.displaynames.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Intl.DisplayNames"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 12.3.2 Intl.DisplayNames.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-Intl.DisplayNames.prototype.resolvedOptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let displayNames be this value.
        // 2. Perform ? RequireInternalSlot(displayNames, [[InitializedDisplayNames]]).
        const display_names = try this_value.requireInternalSlot(agent, DisplayNames);

        // 3. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each row of Table 18, except the header row, in table order, do
        //     a. Let p be the Property value of the current row.
        //     b. Let v be the value of displayNames's internal slot whose name is the Internal Slot value of the current row.
        //     c. Assert: v is not undefined.
        //     d. Perform ! CreateDataPropertyOrThrow(options, p, v).
        const resolved_options = display_names.fields.resolvedOptions();
        try options.createDataPropertyDirect(
            PropertyKey.from("locale"),
            Value.from(
                try String.fromAscii(
                    agent,
                    try display_names.fields.locale.toString(agent.gc_allocator),
                ),
            ),
        );
        try options.createDataPropertyDirect(
            PropertyKey.from("style"),
            Value.from(resolved_options.style),
        );
        try options.createDataPropertyDirect(
            PropertyKey.from("type"),
            Value.from(resolved_options.type),
        );
        try options.createDataPropertyDirect(
            PropertyKey.from("fallback"),
            Value.from(resolved_options.fallback),
        );
        if (display_names.fields.type == .language) {
            try options.createDataPropertyDirect(
                PropertyKey.from("languageDisplay"),
                Value.from(resolved_options.language_display),
            );
        }

        // 5. Return options.
        return Value.from(options);
    }

    /// 12.3.3 Intl.DisplayNames.prototype.of ( code )
    /// https://tc39.es/ecma402/#sec-Intl.DisplayNames.prototype.of
    fn of(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const code_value = arguments.get(0);

        // 1. Let displayNames be this value.
        // 2. Perform ? RequireInternalSlot(displayNames, [[InitializedDisplayNames]]).
        const display_names = try this_value.requireInternalSlot(agent, DisplayNames);

        // 3. Let code be ? ToString(code).
        const code = try code_value.toString(agent);

        // 4. Set code to ? CanonicalCodeForDisplayNames(displayNames.[[Type]], code).
        // 5. Let fields be displayNames.[[Fields]].
        // 6. If fields has a field [[<code>]], return fields.[[<code>]].
        // 7. If displayNames.[[Fallback]] is "code", return code.
        // 8. Return undefined.
        const code_utf8 = try code.toUtf8(agent.gc_allocator);
        defer agent.gc_allocator.free(code_utf8);
        // ICU4X LocaleDisplayNamesFormatter and RegionDisplayNames return an error for at least
        // the 'und' locale, other engines seem to fall back to 'en' in that case.
        const fallback_locale = icu4zig.Locale.fromString("en") catch unreachable;
        defer fallback_locale.deinit();
        const value = switch (display_names.fields.type) {
            .language => blk: {
                const locale_display_names_formatter = icu4zig.LocaleDisplayNamesFormatter.init(
                    display_names.fields.locale,
                    display_names.fields.options,
                ) catch icu4zig.LocaleDisplayNamesFormatter.init(
                    fallback_locale,
                    display_names.fields.options,
                ) catch unreachable;
                defer locale_display_names_formatter.deinit();
                const locale = icu4zig.Locale.fromString(code_utf8) catch {
                    return agent.throwException(
                        .range_error,
                        "Invalid language '{}'",
                        .{code},
                    );
                };
                break :blk try locale_display_names_formatter.of(agent.gc_allocator, locale);
            },
            .region => blk: {
                const region_display_names = icu4zig.RegionDisplayNames.init(
                    display_names.fields.locale,
                    display_names.fields.options,
                ) catch icu4zig.RegionDisplayNames.init(
                    fallback_locale,
                    display_names.fields.options,
                ) catch unreachable;
                defer region_display_names.deinit();
                break :blk region_display_names.of(agent.gc_allocator, code_utf8) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.Subtag => return agent.throwException(
                        .range_error,
                        "Invalid region '{}'",
                        .{code},
                    ),
                };
            },
            else => return agent.throwException(
                .internal_error,
                "Unsupported Intl.DisplayNames type '{s}'",
                .{@tagName(display_names.fields.type)},
            ),
        };
        if (value.len == 0) return .undefined;
        return Value.from(try String.fromUtf8(agent, value));
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

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Type]]
        type: Type,

        /// [[Style]], [[Fallback]], [[LanguageDisplay]]
        options: icu4zig.DisplayNamesOptions,

        pub const ResolvedOptions = struct {
            style: *const String,
            type: *const String,
            fallback: *const String,
            language_display: *const String,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            const @"type" = switch (self.type) {
                .language => String.fromLiteral("language"),
                .region => String.fromLiteral("region"),
                .script => String.fromLiteral("script"),
                .currency => String.fromLiteral("currency"),
                .calendar => String.fromLiteral("calendar"),
                .date_time_field => String.fromLiteral("dateTimeField"),
            };
            const style = switch (self.options.style.?) {
                .narrow => String.fromLiteral("narrow"),
                .short => String.fromLiteral("short"),
                .long => String.fromLiteral("long"),
                .menu => String.fromLiteral("menu"),
            };
            const fallback = switch (self.options.fallback.?) {
                .code => String.fromLiteral("code"),
                .none => String.fromLiteral("none"),
            };
            const language_display = switch (self.options.language_display.?) {
                .dialect => String.fromLiteral("dialect"),
                .standard => String.fromLiteral("standard"),
            };
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
