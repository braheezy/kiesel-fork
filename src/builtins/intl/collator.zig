//! 10 Collator Objects
//! https://tc39.es/ecma402/#collator-objects

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
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getOption = types.getOption;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 10.2 Properties of the Intl.Collator Constructor
/// https://tc39.es/ecma402/#sec-properties-of-the-intl-collator-constructor
pub const CollatorConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "Collator",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        // 10.2.1 Intl.Collator.prototype
        // https://tc39.es/ecma402/#sec-intl.collator.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Collator.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 10.1.1 Intl.Collator ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.collator
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, let newTarget be the active function object, else let
        //    newTarget be NewTarget.
        const new_target_ = new_target orelse agent.activeFunctionObject();

        // 2. Let internalSlotsList be « [[InitializedCollator]], [[Locale]], [[Usage]],
        //    [[Sensitivity]], [[IgnorePunctuation]], [[Collation]], [[BoundCompare]] ».
        // 3. If %Intl.Collator%.[[RelevantExtensionKeys]] contains "kn", then
        //     a. Append [[Numeric]] to internalSlotsList.
        // 4. If %Intl.Collator%.[[RelevantExtensionKeys]] contains "kf", then
        //     a. Append [[CaseFirst]] to internalSlotsList.
        // 5. Let collator be ? OrdinaryCreateFromConstructor(newTarget,
        //    "%Intl.Collator.prototype%", internalSlotsList).
        const collator = try ordinaryCreateFromConstructor(
            Collator,
            agent,
            new_target_,
            "%Intl.Collator.prototype%",
            .{
                .locale = undefined,
                .usage = undefined,
                .options = .{},
                .bound_compare = null,
            },
        );

        // 6. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 7. Set options to ? CoerceOptionsToObject(options).
        const options = try options_value.coerceOptionsToObject(agent);

        // 8. Let usage be ? GetOption(options, "usage", string, « "sort", "search" », "sort").
        const usage = try getOption(
            options,
            "usage",
            .string,
            &.{ String.fromLiteral("sort"), String.fromLiteral("search") },
            String.fromLiteral("sort"),
        );

        // 9. Set collator.[[Usage]] to usage.
        const usage_map = std.StaticStringMap(Collator.Fields.Usage).initComptime(&.{
            .{ "sort", .sort },
            .{ "search", .search },
        });
        collator.as(Collator).fields.usage = usage_map.get(usage.data.slice.ascii).?;

        // TODO: 10-11.

        // 12. Let opt be a new Record.

        // 13. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
        //     "best fit").
        const matcher = try getOption(
            options,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 14. Set opt.[[localeMatcher]] to matcher.
        _ = matcher;

        // 15. Let collation be ? GetOption(options, "collation", string, empty, undefined).
        const maybe_collation = try getOption(options, "collation", .string, null, null);

        // 16. If collation is not undefined, then
        if (maybe_collation) |_| {
            // TODO: a. If collation cannot be matched by the type Unicode locale nonterminal,
            //          throw a RangeError exception.
        }
        // TODO: 17. Set opt.[[co]] to collation.

        // 18. Let numeric be ? GetOption(options, "numeric", boolean, empty, undefined).
        const maybe_numeric = try getOption(options, "numeric", .boolean, null, null);

        // 19. If numeric is not undefined, then
        //     a. Set numeric to ! ToString(numeric).
        // 20. Set opt.[[kn]] to numeric.
        if (maybe_numeric) |numeric| {
            collator.as(Collator).fields.options.numeric = if (numeric) .on else .off;
        }

        // 21. Let caseFirst be ? GetOption(options, "caseFirst", string, « "upper", "lower", "false" », undefined).
        const maybe_case_first = try getOption(
            options,
            "caseFirst",
            .string,
            &.{
                String.fromLiteral("upper"),
                String.fromLiteral("lower"),
                String.fromLiteral("false"),
            },
            null,
        );

        // 22. Set opt.[[kf]] to caseFirst.
        const case_first_map = std.StaticStringMap(
            icu4zig.Collator.Options.CaseFirst,
        ).initComptime(&.{
            .{ "upper", .upper_first },
            .{ "lower", .lower_first },
            .{ "false", .off },
        });
        if (maybe_case_first) |case_first| {
            collator.as(Collator).fields.options.case_first = case_first_map.get(case_first.data.slice.ascii).?;
        }

        // 23. Let relevantExtensionKeys be %Intl.Collator%.[[RelevantExtensionKeys]].
        // TODO: 24. Let r be ResolveLocale(%Intl.Collator%.[[AvailableLocales]], requestedLocales,
        //           opt, relevantExtensionKeys, localeData).
        const resolved_locale = if (requested_locales.items.len != 0) blk: {
            const resolved_locale_string = try requested_locales.items[0].toString(agent.gc_allocator);
            var it = std.mem.splitSequence(u8, resolved_locale_string, "-x-");
            break :blk icu4zig.Locale.init(it.next().?) catch unreachable;
        } else agent.platform.default_locale;

        // 25. Set collator.[[Locale]] to r.[[Locale]].
        collator.as(Collator).fields.locale = resolved_locale;

        // TODO: 26-31.

        // 32. Let sensitivity be ? GetOption(options, "sensitivity", string, « "base", "accent",
        //     "case", "variant" », undefined).
        var maybe_sensitivity = try getOption(
            options,
            "sensitivity",
            .string,
            &.{
                String.fromLiteral("base"),
                String.fromLiteral("accent"),
                String.fromLiteral("case"),
                String.fromLiteral("variant"),
            },
            null,
        );

        // 33. If sensitivity is undefined, then
        if (maybe_sensitivity == null) {
            // a. If usage is "sort", then
            if (usage.eql(String.fromLiteral("sort"))) {
                // i. Set sensitivity to "variant".
                maybe_sensitivity = String.fromLiteral("variant");
            }
            // b. Else,
            else {
                // i. Set sensitivity to resolvedLocaleData.[[sensitivity]].
            }
        }

        // 34. Set collator.[[Sensitivity]] to sensitivity.
        const sensitivity_map = std.StaticStringMap(
            struct { icu4zig.Collator.Options.Strength, icu4zig.Collator.Options.CaseLevel },
        ).initComptime(&.{
            // See https://docs.rs/icu/latest/icu/collator/enum.Strength.html#variants for the
            // mapping of ECMA-402 sensitivity to ICU4X collator options.
            .{ "base", .{ .primary, .off } },
            .{ "accent", .{ .secondary, .auto } },
            .{ "case", .{ .primary, .on } },
            .{ "variant", .{ .tertiary, .auto } },
        });
        if (maybe_sensitivity) |sensitivity| {
            const strength, const case_level = sensitivity_map.get(sensitivity.data.slice.ascii).?;
            collator.as(Collator).fields.options.strength = strength;
            collator.as(Collator).fields.options.case_level = case_level;
        }

        // 35. Let defaultIgnorePunctuation be resolvedLocaleData.[[ignorePunctuation]].
        // 36. Let ignorePunctuation be ? GetOption(options, "ignorePunctuation", boolean, empty,
        //     defaultIgnorePunctuation).
        const maybe_ignore_punctuation = try getOption(
            options,
            "ignorePunctuation",
            .boolean,
            null,
            null,
        );

        // 37. Set collator.[[IgnorePunctuation]] to ignorePunctuation.
        if (maybe_ignore_punctuation) |ignore_punctuation| {
            collator.as(Collator).fields.options.max_variable = if (ignore_punctuation) .space else .punctuation;
        }

        // 38. Return collator.
        return Value.from(collator);
    }
};

/// 10.3 Properties of the Intl.Collator Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-the-intl-collator-prototype-object
pub const CollatorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinAccessor(object, "compare", compare, null, realm);
        try defineBuiltinFunction(object, "resolvedOptions", resolvedOptions, 0, realm);

        // 10.3.1 Intl.Collator.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.collator.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.Collator%"()),
        );

        // 10.3.2 Intl.Collator.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.collator.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Intl.Collator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 10.3.3 get Intl.Collator.prototype.compare
    /// https://tc39.es/ecma402/#sec-intl.collator.prototype.compare
    fn compare(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let collator be the this value.
        // 2. Perform ? RequireInternalSlot(collator, [[InitializedCollator]]).
        const collator = try this_value.requireInternalSlot(agent, Collator);

        // 3. If collator.[[BoundCompare]] is undefined, then
        if (collator.fields.bound_compare == null) {
            // a. Let F be a new built-in function object as defined in 10.3.3.1.
            // b. Set F.[[Collator]] to collator.
            const Captures = struct {
                collator: *Collator,
            };
            const captures = try agent.gc_allocator.create(Captures);
            captures.* = .{ .collator = collator };

            const collator_compare_function = struct {
                /// 10.3.3.1 Collator Compare Functions
                /// https://tc39.es/ecma402/#sec-collator-compare-functions
                fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
                    const function = agent_.activeFunctionObject();
                    const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);

                    // 1. Let collator be F.[[Collator]].
                    // 2. Assert: collator is an Object and collator has an [[InitializedCollator]]
                    //    internal slot.
                    const collator_ = captures_.collator;

                    // 3. If x is not provided, let x be undefined.
                    // 4. If y is not provided, let y be undefined.
                    // 5. Let X be ? ToString(x).
                    // 6. Let Y be ? ToString(y).
                    const x = try arguments.get(0).toString(agent_);
                    const y = try arguments.get(1).toString(agent_);

                    // 7. Return CompareStrings(collator, X, Y).
                    return compareStrings(agent_.gc_allocator, collator_, x, y);
                }
            }.func;

            const bound_compare = try createBuiltinFunction(agent, .{
                .function = collator_compare_function,
            }, .{
                .length = 2,
                .name = "",
                .additional_fields = SafePointer.make(*Captures, captures),
            });

            // c. Set collator.[[BoundCompare]] to F.
            collator.fields.bound_compare = bound_compare;
        }

        // 4. Return collator.[[BoundCompare]].
        return Value.from(collator.fields.bound_compare.?);
    }

    /// 10.3.4 Intl.Collator.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-intl.collator.prototype.resolvedoptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let collator be the this value.
        // 2. Perform ? RequireInternalSlot(collator, [[InitializedCollator]]).
        const collator = try this_value.requireInternalSlot(agent, Collator);

        // 3. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each row of Table 4, except the header row, in table order, do
        //     a. Let p be the Property value of the current row.
        //     b. Let v be the value of collator's internal slot whose name is the Internal Slot value of the current row.
        //     c. If the current row has an Extension Key value, then
        //         i. Let extensionKey be the Extension Key value of the current row.
        //         ii. If %Intl.Collator%.[[RelevantExtensionKeys]] does not contain extensionKey, then
        //             1. Set v to undefined.
        //     d. If v is not undefined, then
        //         i. Perform ! CreateDataPropertyOrThrow(options, p, v).
        const resolved_options = collator.fields.resolvedOptions();
        options.createDataPropertyOrThrow(
            PropertyKey.from("locale"),
            Value.from(
                try String.fromAscii(
                    agent.gc_allocator,
                    try collator.fields.locale.toString(agent.gc_allocator),
                ),
            ),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("usage"),
            Value.from(resolved_options.usage),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("sensitivity"),
            Value.from(resolved_options.sensitivity),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("ignorePunctuation"),
            Value.from(resolved_options.ignore_punctuation),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("collation"),
            Value.from(resolved_options.collation),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("numeric"),
            Value.from(resolved_options.numeric),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("caseFirst"),
            Value.from(resolved_options.case_first),
        ) catch |err| try noexcept(err);

        // 5. Return options.
        return Value.from(options);
    }
};

/// 10.3.3.2 CompareStrings ( collator, x, y )
/// https://tc39.es/ecma402/#sec-collator-comparestrings
pub fn compareStrings(allocator: Allocator, collator_object: *const Collator, x: String, y: String) Allocator.Error!Value {
    const data_provider = icu4zig.DataProvider.init();
    defer data_provider.deinit();
    const collator = icu4zig.Collator.init(
        data_provider,
        collator_object.fields.locale,
        collator_object.fields.options,
    );
    defer collator.deinit();

    const order = if (x.data.slice == .ascii and y.data.slice == .ascii) blk: {
        break :blk collator.compare(
            .{ .utf8 = x.data.slice.ascii },
            .{ .utf8 = y.data.slice.ascii },
        );
    } else if (x.data.slice == .utf16 and y.data.slice == .utf16) blk: {
        break :blk collator.compare(
            .{ .utf16 = x.data.slice.utf16 },
            .{ .utf16 = y.data.slice.utf16 },
        );
    } else if (x.data.slice == .ascii and y.data.slice == .utf16) blk: {
        const x_utf16 = try x.toUtf16(allocator);
        defer allocator.free(x_utf16);
        break :blk collator.compare(
            .{ .utf16 = x_utf16 },
            .{ .utf16 = y.data.slice.utf16 },
        );
    } else if (x.data.slice == .utf16 and y.data.slice == .ascii) blk: {
        const y_utf16 = try y.toUtf16(allocator);
        defer allocator.free(y_utf16);
        break :blk collator.compare(
            .{ .utf16 = x.data.slice.utf16 },
            .{ .utf16 = y_utf16 },
        );
    } else unreachable;
    return switch (order) {
        .lt => Value.from(-1),
        .gt => Value.from(1),
        .eq => Value.from(0),
    };
}

/// 10.4 Properties of Intl.Collator Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-collator-instances
pub const Collator = MakeObject(.{
    .Fields = struct {
        pub const Usage = enum {
            sort,
            search,
        };

        /// [[Locale]]
        locale: icu4zig.Locale,

        /// [[Usage]]
        usage: Usage,

        /// [[Sensitivity]], [[IgnorePunctuation]], [[Collation]], [[Numeric]], [[CaseFirst]]
        options: icu4zig.Collator.Options,

        /// [[BoundCompare]]
        bound_compare: ?Object,

        pub const ResolvedOptions = struct {
            usage: String,
            sensitivity: String,
            ignore_punctuation: bool,
            collation: String,
            numeric: bool,
            case_first: String,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            const data_provider = icu4zig.DataProvider.init();
            defer data_provider.deinit();
            const collator = icu4zig.Collator.init(data_provider, self.locale, self.options);
            defer collator.deinit();
            const resolved_options = collator.resolvedOptions();

            const usage = switch (self.usage) {
                .sort => String.fromLiteral("sort"),
                .search => String.fromLiteral("search"),
            };
            const sensitivity = if (resolved_options.strength == .primary and resolved_options.case_level == .off)
                String.fromLiteral("base")
            else if (resolved_options.strength == .primary and resolved_options.case_level == .on)
                String.fromLiteral("case")
            else if (resolved_options.strength == .secondary)
                String.fromLiteral("accent")
            else if (resolved_options.strength == .tertiary)
                String.fromLiteral("variant")
            else
                unreachable;
            const ignore_punctuation = resolved_options.max_variable == .space;
            const collation = String.fromLiteral("default");
            const numeric = resolved_options.numeric == .on;
            const case_first = switch (resolved_options.case_first) {
                .upper_first => String.fromLiteral("upper"),
                .lower_first => String.fromLiteral("lower"),
                .off => String.fromLiteral("false"),
            };
            return .{
                .usage = usage,
                .sensitivity = sensitivity,
                .ignore_punctuation = ignore_punctuation,
                .collation = collation,
                .numeric = numeric,
                .case_first = case_first,
            };
        }
    },
    .tag = .intl_collator,
});
