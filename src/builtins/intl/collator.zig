//! 10 Collator Objects
//! https://tc39.es/ecma402/#collator-objects

const std = @import("std");

const icu4zig = @import("icu4zig");

const abstract_operations = @import("abstract_operations.zig");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 10.2 Properties of the Intl.Collator Constructor
/// https://tc39.es/ecma402/#sec-properties-of-the-intl-collator-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Collator",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 10.2.1 Intl.Collator.prototype
        // https://tc39.es/ecma402/#sec-intl.collator.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Intl.Collator.prototype%"()),
            .none,
        );
    }

    /// 10.1.1 Intl.Collator ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-intl.collator
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const locales = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, let newTarget be the active function object, else let
        //    newTarget be NewTarget.
        const new_target_ = new_target orelse agent.activeFunctionObject();

        // 2. Let internalSlotsList be « [[InitializedCollator]], [[Locale]], [[Usage]],
        //    [[Collation]], [[Numeric]], [[CaseFirst]], [[Sensitivity]], [[IgnorePunctuation]],
        //    [[BoundCompare]] ».
        // 3. Let collator be ? OrdinaryCreateFromConstructor(newTarget,
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

        // 4. Let requestedLocales be ? CanonicalizeLocaleList(locales).
        const requested_locales = try canonicalizeLocaleList(agent, locales);

        // 5. Set options to ? CoerceOptionsToObject(options).
        const options = try options_value.coerceOptionsToObject(agent);

        // 6. Let usage be ? GetOption(options, "usage", string, « "sort", "search" », "sort").
        const usage = try options.getOption(
            agent,
            "usage",
            .string,
            &.{ String.fromLiteral("sort"), String.fromLiteral("search") },
            String.fromLiteral("sort"),
        );

        // 7. Set collator.[[Usage]] to usage.
        const usage_map = std.StaticStringMap(Collator.Fields.Usage).initComptime(&.{
            .{ "sort", .sort },
            .{ "search", .search },
        });
        collator.fields.usage = usage_map.get(usage.asAscii()).?;

        // TODO: 8-9.

        // 10. Let opt be a new Record.

        // 11. Let matcher be ? GetOption(options, "localeMatcher", string, « "lookup", "best fit" »,
        //     "best fit").
        const matcher = try options.getOption(
            agent,
            "localeMatcher",
            .string,
            &.{ String.fromLiteral("lookup"), String.fromLiteral("best fit") },
            String.fromLiteral("best fit"),
        );

        // TODO: 12. Set opt.[[localeMatcher]] to matcher.
        _ = matcher;

        // 13. Let collation be ? GetOption(options, "collation", string, empty, undefined).
        const maybe_collation = try options.getOption(agent, "collation", .string, null, null);

        // 14. If collation is not undefined, then
        if (maybe_collation) |_| {
            // TODO: a. If collation cannot be matched by the type Unicode locale nonterminal,
            //          throw a RangeError exception.
        }
        // TODO: 15. Set opt.[[co]] to collation.

        // 16. Let numeric be ? GetOption(options, "numeric", boolean, empty, undefined).
        const maybe_numeric = try options.getOption(agent, "numeric", .boolean, null, null);

        // 17. If numeric is not undefined, then
        //     a. Set numeric to ! ToString(numeric).
        // TODO: 18. Set opt.[[kn]] to numeric.
        _ = maybe_numeric;

        // 19. Let caseFirst be ? GetOption(options, "caseFirst", string, « "upper", "lower", "false" », undefined).
        const maybe_case_first = try options.getOption(
            agent,
            "caseFirst",
            .string,
            &.{
                String.fromLiteral("upper"),
                String.fromLiteral("lower"),
                String.fromLiteral("false"),
            },
            null,
        );

        // TODO: 20. Set opt.[[kf]] to caseFirst.
        _ = maybe_case_first;

        // 21. Let r be ResolveLocale(%Intl.Collator%.[[AvailableLocales]], requestedLocales,
        //           opt, %Intl.Collator%.[[RelevantExtensionKeys]], localeData).
        const resolved_locale = if (requested_locales.items.len != 0) blk: {
            const resolved_locale_string = try requested_locales.items[0].toString(agent.gc_allocator);
            var it = std.mem.splitSequence(u8, resolved_locale_string, "-x-");
            break :blk icu4zig.Locale.fromString(it.next().?) catch unreachable;
        } else agent.platform.default_locale;

        // 22. Set collator.[[Locale]] to r.[[Locale]].
        collator.fields.locale = resolved_locale;

        // TODO: 23-28.

        // 29. If usage is "sort", let defaultSensitivity be "variant". Otherwise, let
        //     defaultSensitivity be resolvedLocaleData.[[sensitivity]].
        // 30. Set collator.[[Sensitivity]] to ? GetOption(options, "sensitivity", string, «
        //     "base", "accent", "case", "variant" », defaultSensitivity).
        var maybe_sensitivity = try options.getOption(
            agent,
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
        if (maybe_sensitivity == null and usage.eql(String.fromLiteral("sort"))) {
            maybe_sensitivity = String.fromLiteral("variant");
        }
        const sensitivity_map = std.StaticStringMap(
            struct { icu4zig.Collator.Options.Strength, ?icu4zig.Collator.Options.CaseLevel },
        ).initComptime(&.{
            // See https://docs.rs/icu/latest/icu/collator/enum.Strength.html#variants for the
            // mapping of ECMA-402 sensitivity to ICU4X collator options.
            .{ "base", .{ .primary, .off } },
            .{ "accent", .{ .secondary, null } },
            .{ "case", .{ .primary, .on } },
            .{ "variant", .{ .tertiary, null } },
        });
        if (maybe_sensitivity) |sensitivity| {
            const strength, const case_level = sensitivity_map.get(sensitivity.asAscii()).?;
            collator.fields.options.strength = strength;
            collator.fields.options.case_level = case_level;
        }

        // 31. Let defaultIgnorePunctuation be resolvedLocaleData.[[ignorePunctuation]].
        // 32. Set collator.[[IgnorePunctuation]] to ? GetOption(options, "ignorePunctuation",
        //     boolean, empty, defaultIgnorePunctuation).
        const maybe_ignore_punctuation = try options.getOption(
            agent,
            "ignorePunctuation",
            .boolean,
            null,
            null,
        );
        if (maybe_ignore_punctuation) |ignore_punctuation| {
            collator.fields.options.max_variable = if (ignore_punctuation) .space else .punctuation;
        }

        // 33. Return collator.
        return Value.from(&collator.object);
    }
};

/// 10.3 Properties of the Intl.Collator Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-the-intl-collator-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "resolvedOptions", resolvedOptions, 0, realm);
        try object.defineBuiltinAccessor(agent, "compare", compare, null, realm);

        // 10.3.1 Intl.Collator.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.collator.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.Collator%"()),
        );

        // 10.3.4 Intl.Collator.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.collator.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Intl.Collator"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 10.3.2 Intl.Collator.prototype.resolvedOptions ( )
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
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("locale"),
            Value.from(
                try String.fromAscii(
                    agent,
                    try collator.fields.locale.toString(agent.gc_allocator),
                ),
            ),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("usage"),
            Value.from(resolved_options.usage),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("sensitivity"),
            Value.from(resolved_options.sensitivity),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("ignorePunctuation"),
            Value.from(resolved_options.ignore_punctuation),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("collation"),
            Value.from(resolved_options.collation),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("numeric"),
            Value.from(resolved_options.numeric),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("caseFirst"),
            Value.from(resolved_options.case_first),
        );

        // 5. Return options.
        return Value.from(options);
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

            const bound_compare = try createBuiltinFunction(
                agent,
                .{ .function = collator_compare_function },
                2,
                "",
                .{ .additional_fields = .make(*Captures, captures) },
            );

            // c. Set collator.[[BoundCompare]] to F.
            collator.fields.bound_compare = bound_compare;
        }

        // 4. Return collator.[[BoundCompare]].
        return Value.from(&collator.fields.bound_compare.?.object);
    }
};

/// 10.3.3.2 CompareStrings ( collator, x, y )
/// https://tc39.es/ecma402/#sec-collator-comparestrings
pub fn compareStrings(
    allocator: std.mem.Allocator,
    collator_object: *const Collator,
    x: *const String,
    y: *const String,
) std.mem.Allocator.Error!Value {
    const collator = icu4zig.Collator.init(
        collator_object.fields.locale,
        collator_object.fields.options,
    );
    defer collator.deinit();

    const order = if (x.isAscii() and y.isAscii()) blk: {
        break :blk collator.compareUtf8(x.asAscii(), y.asAscii());
    } else if (x.isUtf16() and y.isUtf16()) blk: {
        break :blk collator.compareUtf16(x.asUtf16(), y.asUtf16());
    } else if (x.isAscii() and y.isUtf16()) blk: {
        const x_utf16 = try x.toUtf16(allocator);
        defer allocator.free(x_utf16);
        break :blk collator.compareUtf16(x_utf16, y.asUtf16());
    } else if (x.isUtf16() and y.isAscii()) blk: {
        const y_utf16 = try y.toUtf16(allocator);
        defer allocator.free(y_utf16);
        break :blk collator.compareUtf16(x.asUtf16(), y_utf16);
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
        bound_compare: ?*builtins.BuiltinFunction,

        pub const ResolvedOptions = struct {
            usage: *const String,
            sensitivity: *const String,
            ignore_punctuation: bool,
            collation: *const String,
            numeric: bool,
            case_first: *const String,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            const collator = icu4zig.Collator.init(self.locale, self.options);
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
                .upper => String.fromLiteral("upper"),
                .lower => String.fromLiteral("lower"),
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
    .display_name = "Intl.Collator",
});
