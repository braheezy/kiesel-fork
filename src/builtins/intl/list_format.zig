//! 13 ListFormat Objects
//! https://tc39.es/ecma402/#listformat-objects

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

/// 13.2 Properties of the Intl.ListFormat Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-listformat-constructor
pub const ListFormatConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "ListFormat",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 13.2.1 Intl.ListFormat.prototype
        // https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.ListFormat.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 13.3.1 Intl.ListFormat.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Intl.ListFormat.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 13.1.1 Intl.ListFormat ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-Intl.ListFormat
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "Intl.ListFormat must be constructed with 'new'",
                .{},
            );
        }

        // 2. Let listFormat be ? OrdinaryCreateFromConstructor(NewTarget,
        //    "%Intl.ListFormat.prototype%", « [[InitializedListFormat]], [[Locale]], [[Type]],
        //    [[Style]], [[Templates]] »).
        const list_format = try ordinaryCreateFromConstructor(
            ListFormat,
            agent,
            new_target.?,
            "%Intl.ListFormat.prototype%",
            .{
                .locale = undefined,
                .type = undefined,
                .style = undefined,
            },
        );

        // 3. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 4. Set options to ? GetOptionsObject(options).
        const options = try getOptionsObject(agent, options_value);

        // 5. Let opt be a new Record.

        // 6. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" », "best fit").
        const matcher = try getOption(
            options,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 7. Set opt.[[localeMatcher]] to matcher.
        // TODO: 8. Let r be ResolveLocale(%Intl.ListFormat%.[[AvailableLocales]], requestedLocales,
        //          opt, %Intl.ListFormat%.[[RelevantExtensionKeys]], %Intl.ListFormat%.[[LocaleData]]).
        _ = matcher;
        const resolved_locale = if (requested_locales.items.len != 0)
            requested_locales.items[0]
        else
            icu4zig.Locale.init(null) catch unreachable;

        // 9. Set listFormat.[[Locale]] to r.[[Locale]].
        list_format.as(ListFormat).fields.locale = resolved_locale;

        // 10. Let type be ? GetOption(options, "type", string, « "conjunction", "disjunction",
        //     "unit" », "conjunction").
        const type_ = try getOption(
            options,
            "type",
            .string,
            &.{
                String.fromLiteral("conjunction"),
                String.fromLiteral("disjunction"),
                String.fromLiteral("unit"),
            },
            String.fromLiteral("conjunction"),
        );

        // 11. Set listFormat.[[Type]] to type.
        const type_map = std.StaticStringMap(
            ListFormat.Fields.Type,
        ).initComptime(&.{
            .{ "conjunction", .conjunction },
            .{ "disjunction", .disjunction },
            .{ "unit", .unit },
        });
        list_format.as(ListFormat).fields.type = type_map.get(type_.ascii).?;

        // 12. Let style be ? GetOption(options, "style", string, « "long", "short", "narrow" »,
        //     "long").
        const style = try getOption(
            options,
            "style",
            .string,
            &.{
                String.fromLiteral("long"),
                String.fromLiteral("short"),
                String.fromLiteral("narrow"),
            },
            String.fromLiteral("long"),
        );

        // 13. Set listFormat.[[Style]] to style.
        const style_map = std.StaticStringMap(
            ListFormat.Fields.Style,
        ).initComptime(&.{
            .{ "long", .long },
            .{ "short", .short },
            .{ "narrow", .narrow },
        });
        list_format.as(ListFormat).fields.style = style_map.get(style.ascii).?;

        // TODO: 14. Let resolvedLocaleData be r.[[LocaleData]].
        // TODO: 15. Let dataLocaleTypes be resolvedLocaleData.[[<type>]].
        // TODO: 16. Set listFormat.[[Templates]] to dataLocaleTypes.[[<style>]].

        // 17. Return listFormat.
        return Value.from(list_format);
    }
};

/// 13.3 Properties of the Intl.ListFormat Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-listformat-prototype-object
pub const ListFormatPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        return object;
    }
};

/// 13.4 Properties of Intl.ListFormat Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-listformat-instances
pub const ListFormat = MakeObject(.{
    .Fields = struct {
        pub const Type = enum {
            conjunction,
            disjunction,
            unit,
        };

        pub const Style = enum {
            long,
            short,
            narrow,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Type]]
        type: Type,

        /// [[Style]]
        style: Style,
    },
    .tag = .intl_list_format,
});
