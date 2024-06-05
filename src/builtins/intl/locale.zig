//! 14 Locale Objects
//! https://tc39.es/ecma402/#locale-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const icu4zig = @import("icu4zig");

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
const coerceOptionsToObject = types.coerceOptionsToObject;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getOption = types.getOption;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

const UnicodeExtensions = struct {
    ca: ?String = null,
    co: ?String = null,
    hc: ?String = null,
    kf: ?String = null,
    kn: ?String = null,
    nu: ?String = null,
};

/// https://unicode.org/reports/tr35/#Unicode_locale_identifier
/// type = alphanum{3,8} (sep alphanum{3,8})*
fn matchUnicodeLocaleIdentifierType(str: []const u8) bool {
    var it = std.mem.splitScalar(u8, str, '-');
    while (it.next()) |part| {
        if (part.len < 3 or part.len > 8) return false;
        for (part) |c| {
            if (!std.ascii.isAlphanumeric(c)) return false;
        }
    }
    return true;
}

/// 14.1.2 ApplyOptionsToTag ( tag, options )
/// https://tc39.es/ecma402/#sec-apply-options-to-tag
fn applyOptionsToTag(agent: *Agent, tag: String, options: Object) Agent.Error!icu4zig.Locale {
    // 1. If IsStructurallyValidLanguageTag(tag) is false, throw a RangeError exception.
    // NOTE: Underscore separators are not BCP 47-compatible and must be rejected here.
    if (tag.indexOf(String.fromLiteral("_"), 0) != null) {
        return agent.throwException(.range_error, "Invalid locale identifier '{}'", .{tag});
    }
    var canonicalized_tag = icu4zig.Locale.init(try tag.toUtf8(agent.gc_allocator)) catch |err| switch (err) {
        error.LocaleParserLanguageError,
        error.LocaleParserSubtagError,
        error.LocaleParserExtensionError,
        => return agent.throwException(.range_error, "Invalid locale identifier '{}'", .{tag}),
    };

    // 2. Let language be ? GetOption(options, "language", string, empty, undefined).
    const maybe_language = try getOption(options, "language", .string, null, null);

    // 3. If language is not undefined, then
    if (maybe_language) |language| {
        // a. If language cannot be matched by the unicode_language_subtag Unicode locale
        //    nonterminal, throw a RangeError exception.
        canonicalized_tag.setLanguage(try language.toUtf8(agent.gc_allocator)) catch {
            return agent.throwException(.range_error, "Invalid language subtag '{}'", .{language});
        };
    }

    // 4. Let script be ? GetOption(options, "script", string, empty, undefined).
    const maybe_script = try getOption(options, "script", .string, null, null);

    // 5. If script is not undefined, then
    if (maybe_script) |script| {
        // a. If script cannot be matched by the unicode_script_subtag Unicode locale nonterminal,
        //    throw a RangeError exception.
        canonicalized_tag.setScript(try script.toUtf8(agent.gc_allocator)) catch {
            return agent.throwException(.range_error, "Invalid script subtag '{}'", .{script});
        };
    }

    // 6. Let region be ? GetOption(options, "region", string, empty, undefined).
    const maybe_region = try getOption(options, "region", .string, null, null);

    // 7. If region is not undefined, then
    if (maybe_region) |region| {
        // a. If region cannot be matched by the unicode_region_subtag Unicode locale nonterminal,
        //    throw a RangeError exception.
        canonicalized_tag.setRegion(try region.toUtf8(agent.gc_allocator)) catch {
            return agent.throwException(.range_error, "Invalid region subtag '{}'", .{region});
        };
    }

    // 8. Set tag to CanonicalizeUnicodeLocaleId(tag).
    // 9. Assert: tag can be matched by the unicode_locale_id Unicode locale nonterminal.
    // NOTE: This is done continuously when using the setters above.

    // 10-19.
    // NOTE: These are done as part of step 3.a., 5.a., and 7.a.

    // 20. Return CanonicalizeUnicodeLocaleId(tag).
    return canonicalized_tag;
}

/// 14.1.3 ApplyUnicodeExtensionToTag ( tag, options, relevantExtensionKeys )
/// https://tc39.es/ecma402/#sec-apply-unicode-extension-to-tag
fn applyUnicodeExtensionToTag(agent: *Agent, tag: icu4zig.Locale, options: UnicodeExtensions) Allocator.Error!icu4zig.Locale {
    // 1-10.
    // This code ain't nice, I know.
    const str = try tag.toString(agent.gc_allocator);
    var unicode_extensions = str;
    const end = if (std.mem.indexOf(u8, str, "-x-")) |end| blk: {
        unicode_extensions = unicode_extensions[0..end];
        break :blk end;
    } else null;
    const start = if (std.mem.indexOf(u8, str, "-u-")) |start| blk: {
        unicode_extensions = unicode_extensions[start + 3 ..];
        break :blk start;
    } else return tag.clone();
    var parts = std.ArrayList([]const u8).init(agent.gc_allocator);
    defer parts.deinit();
    try parts.append(str[0 .. start + 2]);
    var it = std.mem.splitScalar(u8, unicode_extensions, '-');
    outer: while (it.next()) |key| {
        const value: ?[]const u8 = blk: {
            var value_parts = std.ArrayList([]const u8).init(agent.gc_allocator);
            defer value_parts.deinit();
            while (it.peek()) |next| {
                if (next.len == 2) break;
                try value_parts.append(it.next().?);
            }
            break :blk if (value_parts.items.len > 0)
                try std.mem.join(agent.gc_allocator, "-", value_parts.items)
            else
                null;
        };
        inline for (comptime std.meta.fieldNames(UnicodeExtensions)) |field_name| {
            if (std.mem.eql(u8, key, field_name)) {
                if (@field(options, field_name) != null) continue :outer;
            }
        }
        try parts.append(key);
        if (value != null) try parts.append(value.?);
    }
    inline for (comptime std.meta.fieldNames(UnicodeExtensions)) |field_name| {
        if (@field(options, field_name)) |new_value| {
            try parts.append(field_name);
            try parts.append(new_value.ascii); // All extensions have been validated and should be ASCII at this point
        }
    }
    if (end != null) try parts.append(str[end.? + 1 ..]);
    const new_str = try std.mem.join(agent.gc_allocator, "-", parts.items);
    return icu4zig.Locale.init(new_str) catch unreachable;
}

/// 14.2 Properties of the Intl.Locale Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-locale-constructor
pub const LocaleConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 1,
            .name = "Locale",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 14.2.1 Intl.Locale.prototype
        // https://tc39.es/ecma402/#sec-Intl.Locale.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Locale.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 14.3.1 Intl.Locale.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.Locale.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Intl.Locale.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 14.1.1 Intl.Locale ( tag [ , options ] )
    /// https://tc39.es/ecma402/#sec-Intl.Locale
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const tag_value = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        if (new_target == null) {
            return agent.throwException(
                .type_error,
                "Intl.Locale must be constructed with 'new'",
                .{},
            );
        }

        // 2. Let relevantExtensionKeys be %Intl.Locale%.[[RelevantExtensionKeys]].
        // 3. Let internalSlotsList be « [[InitializedLocale]], [[Locale]], [[Calendar]],
        //    [[Collation]], [[HourCycle]], [[NumberingSystem]] ».
        // TODO: 4. If relevantExtensionKeys contains "kf", then
        //     a. Append [[CaseFirst]] to internalSlotsList.
        // TODO: 5. If relevantExtensionKeys contains "kn", then
        //     a. Append [[Numeric]] to internalSlotsList.
        // 6. Let locale be ? OrdinaryCreateFromConstructor(NewTarget, "%Intl.Locale.prototype%",
        //    internalSlotsList).
        const locale = try ordinaryCreateFromConstructor(
            Locale,
            agent,
            new_target.?,
            "%Intl.Locale.prototype%",
            .{ .locale = undefined },
        );

        // 7. If Type(tag) is not String or Object, throw a TypeError exception.
        if (tag_value != .string and tag_value != .object) {
            return agent.throwException(.type_error, "Locale must be string or object", .{});
        }

        // 8. If Type(tag) is Object and tag has an [[InitializedLocale]] internal slot, then
        const tag = if (tag_value == .object and tag_value.object.is(Locale)) blk: {
            // a. Let tag be tag.[[Locale]].
            break :blk String.fromAscii(try tag_value.object.as(Locale).fields.locale.toString(agent.gc_allocator));
        }
        // 9. Else,
        else blk: {
            // a. Let tag be ? ToString(tag).
            break :blk try tag_value.toString(agent);
        };

        // 10. Set options to ? CoerceOptionsToObject(options).
        const options = try coerceOptionsToObject(agent, options_value);

        // 11. Set tag to ? ApplyOptionsToTag(tag, options).
        var tag_locale = try applyOptionsToTag(agent, tag, options);

        // 12. Let opt be a new Record.
        var opt: UnicodeExtensions = .{};

        // 13. Let calendar be ? GetOption(options, "calendar", string, empty, undefined).
        const calendar = try getOption(options, "calendar", .string, null, null);

        // 14. If calendar is not undefined, then
        if (calendar != null) {
            // a. If calendar cannot be matched by the type Unicode locale nonterminal, throw a
            //    RangeError exception.
            if (!matchUnicodeLocaleIdentifierType(try calendar.?.toUtf8(agent.gc_allocator))) {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier type '{}'",
                    .{calendar.?},
                );
            }
        }

        // 15. Set opt.[[ca]] to calendar.
        opt.ca = calendar;

        // 16. Let collation be ? GetOption(options, "collation", string, empty, undefined).
        const collation = try getOption(options, "collation", .string, null, null);

        // 17. If collation is not undefined, then
        if (collation != null) {
            // a. If collation cannot be matched by the type Unicode locale nonterminal, throw a
            //    RangeError exception.
            if (!matchUnicodeLocaleIdentifierType(try collation.?.toUtf8(agent.gc_allocator))) {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier type '{}'",
                    .{collation.?},
                );
            }
        }

        // 18. Set opt.[[co]] to collation.
        opt.co = collation;

        // 19. Let hc be ? GetOption(options, "hourCycle", string, « "h11", "h12", "h23", "h24" »,
        //     undefined).
        const hc = try getOption(
            options,
            "hourCycle",
            .string,
            &.{ String.fromLiteral("h11"), String.fromLiteral("h12"), String.fromLiteral("h23"), String.fromLiteral("h24") },
            null,
        );

        // 20. Set opt.[[hc]] to hc.
        opt.hc = hc;

        // 21. Let kf be ? GetOption(options, "caseFirst", string, « "upper", "lower", "false" »,
        //     undefined).
        const kf = try getOption(
            options,
            "caseFirst",
            .string,
            &.{ String.fromLiteral("upper"), String.fromLiteral("lower"), String.fromLiteral("false") },
            null,
        );

        // 22. Set opt.[[kf]] to kf.
        opt.kf = kf;

        // 23. Let kn be ? GetOption(options, "numeric", boolean, empty, undefined).
        const kn_value = try getOption(options, "numeric", .boolean, null, null);

        // 24. If kn is not undefined, set kn to ! ToString(kn).
        const kn = if (kn_value == true) String.fromLiteral("true") else if (kn_value == false) String.fromLiteral("false") else null;

        // 25. Set opt.[[kn]] to kn.
        opt.kn = kn;

        // 26. Let numberingSystem be ? GetOption(options, "numberingSystem", string, empty,
        //     undefined).
        const numbering_system = try getOption(options, "numberingSystem", .string, null, null);

        // 27. If numberingSystem is not undefined, then
        if (numbering_system != null) {
            // a. If numberingSystem cannot be matched by the type Unicode locale nonterminal,
            //    throw a RangeError exception.
            if (!matchUnicodeLocaleIdentifierType(try numbering_system.?.toUtf8(agent.gc_allocator))) {
                return agent.throwException(
                    .range_error,
                    "Invalid locale identifier type '{}'",
                    .{numbering_system.?},
                );
            }
        }

        // 28. Set opt.[[nu]] to numberingSystem.
        opt.nu = numbering_system;

        // 29. Let r be ApplyUnicodeExtensionToTag(tag, opt, relevantExtensionKeys).
        tag_locale = try applyUnicodeExtensionToTag(agent, tag_locale, opt);

        locale.as(Locale).fields = .{
            // NOTE: The ICU4X locale stores all of this for us :)
            // 30. Set locale.[[Locale]] to r.[[locale]].
            // 31. Set locale.[[Calendar]] to r.[[ca]].
            // 32. Set locale.[[Collation]] to r.[[co]].
            // 33. Set locale.[[HourCycle]] to r.[[hc]].
            // 34. If relevantExtensionKeys contains "kf", then
            //     a. Set locale.[[CaseFirst]] to r.[[kf]].
            // 35. If relevantExtensionKeys contains "kn", then
            //     a. If SameValue(r.[[kn]], "true") is true or r.[[kn]] is the empty String, then
            //         i. Set locale.[[Numeric]] to true.
            //     b. Else,
            //         i. Set locale.[[Numeric]] to false.
            // 36. Set locale.[[NumberingSystem]] to r.[[nu]].
            .locale = tag_locale,
        };

        // 37. Return locale.
        return Value.from(locale);
    }
};

/// 14.3 Properties of the Intl.Locale Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-locale-prototype-object
pub const LocalePrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "maximize", maximize, 0, realm);
        try defineBuiltinFunction(object, "minimize", minimize, 0, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinAccessor(object, "baseName", baseName, null, realm);
        try defineBuiltinAccessor(object, "calendar", calendar, null, realm);
        try defineBuiltinAccessor(object, "caseFirst", caseFirst, null, realm);
        try defineBuiltinAccessor(object, "collation", collation, null, realm);
        try defineBuiltinAccessor(object, "hourCycle", hourCycle, null, realm);
        try defineBuiltinAccessor(object, "numeric", numeric, null, realm);
        try defineBuiltinAccessor(object, "numberingSystem", numberingSystem, null, realm);
        try defineBuiltinAccessor(object, "language", language, null, realm);
        try defineBuiltinAccessor(object, "script", script, null, realm);
        try defineBuiltinAccessor(object, "region", region, null, realm);

        // 14.3.2 Intl.Locale.prototype[ @@toStringTag ]
        // https://tc39.es/ecma402/#sec-Intl.Locale.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Intl.Locale"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 14.3.3 Intl.Locale.prototype.maximize ( )
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.maximize
    fn maximize(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Let maximal be the result of the Add Likely Subtags algorithm applied to
        //    loc.[[Locale]]. If an error is signaled, set maximal to loc.[[Locale]].
        const data_provider = icu4zig.DataProvider.init();
        defer data_provider.deinit();
        const locale_expander = icu4zig.LocaleExpander.init(data_provider);
        defer locale_expander.deinit();
        var maximal = locale.fields.locale.clone();
        _ = locale_expander.maximize(&maximal);

        // 4. Return ! Construct(%Intl.Locale%, maximal).
        const object = ordinaryCreateFromConstructor(
            Locale,
            agent,
            try realm.intrinsics.@"%Intl.Locale%"(),
            "%Intl.Locale.prototype%",
            .{ .locale = maximal },
        ) catch |err| try noexcept(err);
        return Value.from(object);
    }

    /// 14.3.4 Intl.Locale.prototype.minimize ( )
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.minimize
    fn minimize(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 2. Let minimal be the result of the Remove Likely Subtags algorithm applied to
        //    loc.[[Locale]]. If an error is signaled, set minimal to loc.[[Locale]].
        const data_provider = icu4zig.DataProvider.init();
        defer data_provider.deinit();
        const locale_expander = icu4zig.LocaleExpander.init(data_provider);
        defer locale_expander.deinit();
        var minimal = locale.fields.locale.clone();
        _ = locale_expander.minimize(&minimal);

        // 4. Return ! Construct(%Intl.Locale%, minimal).
        const object = ordinaryCreateFromConstructor(
            Locale,
            agent,
            try realm.intrinsics.@"%Intl.Locale%"(),
            "%Intl.Locale.prototype%",
            .{ .locale = minimal },
        ) catch |err| try noexcept(err);
        return Value.from(object);
    }

    /// 14.3.5 Intl.Locale.prototype.toString ( )
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.toString
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Locale]].
        return Value.from(
            String.fromAscii(try locale.fields.locale.toString(agent.gc_allocator)),
        );
    }

    /// 14.3.6 get Intl.Locale.prototype.baseName
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.baseName
    fn baseName(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Let locale be loc.[[Locale]].
        // 4. Return the longest prefix of locale matched by the unicode_language_id Unicode locale nonterminal.
        return Value.from(
            String.fromAscii(try locale.fields.locale.basename(agent.gc_allocator)),
        );
    }

    /// 14.3.7 get Intl.Locale.prototype.calendar
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.calendar
    fn calendar(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Calendar]].
        return Value.from(
            String.fromAscii(
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "ca",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.LocaleParserExtensionError => unreachable,
                } orelse return .undefined,
            ),
        );
    }

    /// 14.3.8 get Intl.Locale.prototype.caseFirst
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.caseFirst
    fn caseFirst(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[CaseFirst]].
        return Value.from(
            String.fromAscii(
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "kf",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.LocaleParserExtensionError => unreachable,
                } orelse return .undefined,
            ),
        );
    }

    /// 14.3.9 get Intl.Locale.prototype.collation
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.collation
    fn collation(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Collation]].
        return Value.from(
            String.fromAscii(
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "co",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.LocaleParserExtensionError => unreachable,
                } orelse return .undefined,
            ),
        );
    }

    /// 14.3.10 get Intl.Locale.prototype.hourCycle
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.hourCycle
    fn hourCycle(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[HourCycle]].
        return Value.from(
            String.fromAscii(
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "hc",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.LocaleParserExtensionError => unreachable,
                } orelse return .undefined,
            ),
        );
    }

    /// 14.3.11 get Intl.Locale.prototype.numeric
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.numeric
    fn numeric(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Numeric]].
        const value = locale.fields.locale.getUnicodeExtension(
            agent.gc_allocator,
            "kn",
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.LocaleParserExtensionError => unreachable,
        } orelse return Value.from(false);
        return Value.from(value.len == 0 or std.mem.eql(u8, value, "true"));
    }

    /// 14.3.12 get Intl.Locale.prototype.numberingSystem
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.numberingSystem
    fn numberingSystem(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[NumberingSystem]].
        return Value.from(
            String.fromAscii(
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "nu",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.LocaleParserExtensionError => unreachable,
                } orelse return .undefined,
            ),
        );
    }

    /// 14.3.13 get Intl.Locale.prototype.language
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.language
    fn language(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleLanguage(loc.[[Locale]]).
        return Value.from(
            String.fromAscii(try locale.fields.locale.language(agent.gc_allocator)),
        );
    }

    /// 14.3.14 get Intl.Locale.prototype.script
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.script
    fn script(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleScript(loc.[[Locale]]).
        return Value.from(
            String.fromAscii(
                try locale.fields.locale.script(agent.gc_allocator) orelse return .undefined,
            ),
        );
    }

    /// 14.3.15 get Intl.Locale.prototype.region
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.region
    fn region(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleRegion(loc.[[Locale]]).
        return Value.from(
            String.fromAscii(
                try locale.fields.locale.region(agent.gc_allocator) orelse return .undefined,
            ),
        );
    }
};

/// 14.4 Properties of Intl.Locale Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-locale-instances
pub const Locale = MakeObject(.{
    .Fields = struct {
        /// [[Locale]]
        locale: icu4zig.Locale,
    },
    .tag = .intl_locale,
});
