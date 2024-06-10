//! 18 Segmenter Objects
//! https://tc39.es/ecma402/#segmenter-objects

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

/// 18.2 Properties of the Intl.Segmenter Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-segmenter-constructor
pub const SegmenterConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "Segmenter",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 18.2.1 Intl.Segmenter.prototype
        // https://tc39.es/ecma402/#sec-intl.segmenter.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Segmenter.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 18.3.1 Intl.Segmenter.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.segmenter.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Intl.Segmenter.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 18.1.1 Intl.Segmenter ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.segmenter
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "Intl.Segmenter must be constructed with 'new'",
                .{},
            );
        }

        // 2. Let internalSlotsList be « [[InitializedSegmenter]], [[Locale]], [[SegmenterGranularity]] ».
        // 3. Let segmenter be ? OrdinaryCreateFromConstructor(NewTarget, "%Intl.Segmenter.prototype%", internalSlotsList).
        const segmenter = try ordinaryCreateFromConstructor(
            Segmenter,
            agent,
            new_target.?,
            "%Intl.Segmenter.prototype%",
            .{ .locale = undefined, .segmenter_granularity = undefined },
        );

        // 4. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 5. Set options to ? GetOptionsObject(options).
        const options = try getOptionsObject(agent, options_value);

        // 6. Let opt be a new Record.

        // 7. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" », "best fit").
        const matcher = try getOption(
            options,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 8. Set opt.[[localeMatcher]] to matcher.
        // TODO: 9. Let r be ResolveLocale(%Intl.Segmenter%.[[AvailableLocales]], requestedLocales,
        //          opt, %Intl.Segmenter%.[[RelevantExtensionKeys]], %Intl.Segmenter%.[[LocaleData]]).
        _ = matcher;
        const resolved_locale = if (requested_locales.items.len != 0)
            requested_locales.items[0]
        else
            icu4zig.Locale.init(null) catch unreachable;

        // 10. Set segmenter.[[Locale]] to r.[[Locale]].
        segmenter.as(Segmenter).fields.locale = resolved_locale;

        // 11. Let granularity be ? GetOption(options, "granularity", string, « "grapheme", "word",
        //     "sentence" », "grapheme").
        const granularity = try getOption(
            options,
            "granularity",
            .string,
            &.{
                String.fromLiteral("grapheme"),
                String.fromLiteral("word"),
                String.fromLiteral("sentence"),
            },
            String.fromLiteral("grapheme"),
        );

        // 12. Set segmenter.[[SegmenterGranularity]] to granularity.
        const granularity_map = std.StaticStringMap(
            Segmenter.Fields.SegmenterGranularity,
        ).initComptime(&.{
            .{ "grapheme", .grapheme },
            .{ "word", .word },
            .{ "sentence", .sentence },
        });
        segmenter.as(Segmenter).fields.segmenter_granularity = granularity_map.get(granularity.ascii).?;

        // 13. Return segmenter.
        return Value.from(segmenter);
    }
};

/// 18.3 Properties of the Intl.Segmenter Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-segmenter-prototype-object
pub const SegmenterPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 18.3.2 Intl.Segmenter.prototype [ @@toStringTag ]
        // https://tc39.es/ecma402/#sec-intl.segmenter.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Intl.Segmenter"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

/// 18.4 Properties of Intl.Segmenter Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-segmenter-instances
pub const Segmenter = MakeObject(.{
    .Fields = struct {
        pub const SegmenterGranularity = enum {
            grapheme,
            word,
            sentence,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[SegmenterGranularity]]
        segmenter_granularity: SegmenterGranularity,
    },
    .tag = .intl_segmenter,
});
