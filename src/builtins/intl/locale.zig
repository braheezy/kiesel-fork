//! 15 Locale Objects
//! https://tc39.es/ecma402/#locale-objects

const std = @import("std");

const icu4zig = @import("icu4zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 15.1.2 UpdateLanguageId ( tag, options )
/// https://tc39.es/ecma402/#sec-updatelanguageid
fn updateLanguageId(
    agent: *Agent,
    tag: icu4zig.Locale,
    options: *Object,
) Agent.Error!icu4zig.Locale {
    var new_tag = tag.clone();

    // 1. Let baseName be GetLocaleBaseName(tag).
    // 2. Let language be ? GetOption(options, "language", string, empty, GetLocaleLanguage(baseName)).
    const maybe_language = try options.getOption(agent, "language", .string, null, null);

    // 3. If language cannot be matched by the unicode_language_subtag Unicode locale nonterminal,
    //    throw a RangeError exception.
    if (maybe_language) |language| {
        const value = try language.toUtf8(agent.gc_allocator);
        new_tag.setLanguage(value) catch {
            return agent.throwException(.range_error, "Invalid language subtag '{s}'", .{value});
        };
    }

    // 4. Let script be ? GetOption(options, "script", string, empty, GetLocaleScript(baseName)).
    const maybe_script = try options.getOption(agent, "script", .string, null, null);

    // 5. If script is not undefined, then
    if (maybe_script) |script| {
        // a. If script cannot be matched by the unicode_script_subtag Unicode locale nonterminal,
        //    throw a RangeError exception.
        const value = try script.toUtf8(agent.gc_allocator);
        new_tag.setScript(value) catch {
            return agent.throwException(.range_error, "Invalid script subtag '{s}'", .{value});
        };
    }

    // 6. Let region be ? GetOption(options, "region", string, empty, GetLocaleRegion(baseName)).
    const maybe_region = try options.getOption(agent, "region", .string, null, null);

    // 7. If region is not undefined, then
    if (maybe_region) |region| {
        // a. If region cannot be matched by the unicode_region_subtag Unicode locale nonterminal,
        //    throw a RangeError exception.
        const value = try region.toUtf8(agent.gc_allocator);
        new_tag.setRegion(value) catch {
            return agent.throwException(.range_error, "Invalid region subtag '{s}'", .{value});
        };
    }

    // 8-14.
    // NOTE: These are done as part of step 3.a., 5.a., and 7.a.

    // 15. Return newTag.
    return new_tag;
}

/// 15.2 Properties of the Intl.Locale Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-locale-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Locale",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 15.2.1 Intl.Locale.prototype
        // https://tc39.es/ecma402/#sec-Intl.Locale.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Intl.Locale.prototype%"()),
            .none,
        );
    }

    /// 15.1.1 Intl.Locale ( tag [ , options ] )
    /// https://tc39.es/ecma402/#sec-Intl.Locale
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
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

        // 2. Let localeExtensionKeys be %Intl.Locale%.[[LocaleExtensionKeys]].
        // 3. Let internalSlotsList be « [[InitializedLocale]], [[Locale]], [[Calendar]],
        //    [[Collation]], [[HourCycle]], [[NumberingSystem]] ».
        // TODO: 4. If localeExtensionKeys contains "kf", then
        //     a. Append [[CaseFirst]] to internalSlotsList.
        // TODO: 5. If localeExtensionKeys contains "kn", then
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

        // 7. If tag is not a String and tag is not an Object, throw a TypeError exception.
        if (!tag_value.isString() and !tag_value.isObject()) {
            return agent.throwException(.type_error, "Locale must be string or object", .{});
        }

        // 8. If tag an is Object and tag has an [[InitializedLocale]] internal slot, then
        const tag_string = if (tag_value.castObject(Locale)) |tag_locale| blk: {
            // a. Let tag be tag.[[Locale]].
            break :blk try String.fromAscii(
                agent,
                try tag_locale.fields.locale.toString(agent.gc_allocator),
            );
        } else blk: {
            // 9. Else,
            // a. Let tag be ? ToString(tag).
            break :blk try tag_value.toString(agent);
        };

        // 10. Set options to ? CoerceOptionsToObject(options).
        const options = try options_value.coerceOptionsToObject(agent);

        // 11. If IsStructurallyValidLanguageTag(tag) is false, throw a RangeError exception.
        // 12. Set tag to CanonicalizeUnicodeLocaleId(tag).
        var tag = icu4zig.Locale.fromString(try tag_string.toUtf8(agent.gc_allocator)) catch {
            return agent.throwException(
                .range_error,
                "Invalid locale identifier {f}",
                .{tag_string},
            );
        };

        // 13. Set tag to ? UpdateLanguageId(tag, options).
        tag = try updateLanguageId(agent, tag, options);

        // 14. Let opt be a new Record.

        // 15. Let calendar be ? GetOption(options, "calendar", string, empty, undefined).
        const maybe_calendar = try options.getOption(
            agent,
            "calendar",
            .string,
            null,
            null,
        );

        // 16. If calendar is not undefined, then
        if (maybe_calendar) |calendar| {
            // a. If calendar cannot be matched by the type Unicode locale nonterminal, throw a
            //    RangeError exception.
            var value: []const u8 = try calendar.toUtf8(agent.gc_allocator);
            // NOTE: Valid strings are length 3-8 but ICU4X doesn't reject length 0 or 2
            var it = std.mem.splitScalar(u8, value, '-');
            while (it.next()) |part| if (part.len < 3) {
                value = "x";
                break;
            };
            tag.setUnicodeExtension("ca", value) catch {
                return agent.throwException(.range_error, "Invalid u-ca subtag '{s}'", .{value});
            };
        }

        // 17. Set opt.[[ca]] to calendar.
        // NOTE: This is done as part of step 16.a.

        // 18. Let collation be ? GetOption(options, "collation", string, empty, undefined).
        const maybe_collation = try options.getOption(
            agent,
            "collation",
            .string,
            null,
            null,
        );

        // 19. If collation is not undefined, then
        if (maybe_collation) |collation| {
            // a. If collation cannot be matched by the type Unicode locale nonterminal, throw a
            //    RangeError exception.
            var value: []const u8 = try collation.toUtf8(agent.gc_allocator);
            // NOTE: Valid strings are length 3-8 but ICU4X doesn't reject length 0 or 2
            var it = std.mem.splitScalar(u8, value, '-');
            while (it.next()) |part| if (part.len < 3) {
                value = "x";
                break;
            };
            tag.setUnicodeExtension("co", value) catch {
                return agent.throwException(.range_error, "Invalid u-co subtag '{s}'", .{value});
            };
        }

        // 20. Set opt.[[co]] to collation.
        // NOTE: This is done as part of step 19.a.

        // 21. Let hc be ? GetOption(options, "hourCycle", string, « "h11", "h12", "h23", "h24" »,
        //     undefined).
        const maybe_hc = try options.getOption(
            agent,
            "hourCycle",
            .string,
            &.{
                String.fromLiteral("h11"),
                String.fromLiteral("h12"),
                String.fromLiteral("h23"),
                String.fromLiteral("h24"),
            },
            null,
        );

        // 22. Set opt.[[hc]] to hc.
        if (maybe_hc) |hc| {
            tag.setUnicodeExtension("hc", hc.slice.ascii) catch unreachable;
        }

        // 23. Let kf be ? GetOption(options, "caseFirst", string, « "upper", "lower", "false" »,
        //     undefined).
        const maybe_kf = try options.getOption(
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

        // 24. Set opt.[[kf]] to kf.
        if (maybe_kf) |kf| {
            tag.setUnicodeExtension("kf", kf.slice.ascii) catch unreachable;
        }

        // 25. Let kn be ? GetOption(options, "numeric", boolean, empty, undefined).
        const maybe_kn = try options.getOption(agent, "numeric", .boolean, null, null);

        // 26. If kn is not undefined, set kn to ! ToString(kn).
        // 27. Set opt.[[kn]] to kn.
        if (maybe_kn) |kn| {
            tag.setUnicodeExtension("kn", if (kn) "true" else "false") catch unreachable;
        }

        // 28. Let numberingSystem be ? GetOption(options, "numberingSystem", string, empty,
        //     undefined).
        const maybe_numbering_system = try options.getOption(
            agent,
            "numberingSystem",
            .string,
            null,
            null,
        );

        // 29. If numberingSystem is not undefined, then
        if (maybe_numbering_system) |numbering_system| {
            // a. If numberingSystem cannot be matched by the type Unicode locale nonterminal,
            //    throw a RangeError exception.
            var value: []const u8 = try numbering_system.toUtf8(agent.gc_allocator);
            // NOTE: Valid strings are length 3-8 but ICU4X doesn't reject length 0 or 2
            var it = std.mem.splitScalar(u8, value, '-');
            while (it.next()) |part| if (part.len < 3) {
                value = "x";
                break;
            };
            tag.setUnicodeExtension("nu", value) catch {
                return agent.throwException(.range_error, "Invalid u-nu subtag '{s}'", .{value});
            };
        }

        // 30. Set opt.[[nu]] to numberingSystem.
        // NOTE: This is done as part of step 29.a.

        locale.fields = .{
            // 31. Let r be MakeLocaleRecord(tag, opt, localeExtensionKeys).
            // 32. Set locale.[[Locale]] to r.[[locale]].
            // 33. Set locale.[[Calendar]] to r.[[ca]].
            // 34. Set locale.[[Collation]] to r.[[co]].
            // 35. Set locale.[[HourCycle]] to r.[[hc]].
            // 36. If localeExtensionKeys contains "kf", then
            //     a. Set locale.[[CaseFirst]] to r.[[kf]].
            // 37. If localeExtensionKeys contains "kn", then
            //     a. If SameValue(r.[[kn]], "true") is true or r.[[kn]] is the empty String, then
            //         i. Set locale.[[Numeric]] to true.
            //     b. Else,
            //         i. Set locale.[[Numeric]] to false.
            // 38. Set locale.[[NumberingSystem]] to r.[[nu]].
            .locale = tag,
        };

        // 39. Return locale.
        return Value.from(&locale.object);
    }
};

/// 15.3 Properties of the Intl.Locale Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-locale-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinAccessor(agent, "baseName", baseName, null, realm);
        try object.defineBuiltinAccessor(agent, "calendar", calendar, null, realm);
        try object.defineBuiltinAccessor(agent, "caseFirst", caseFirst, null, realm);
        try object.defineBuiltinAccessor(agent, "collation", collation, null, realm);
        try object.defineBuiltinAccessor(agent, "hourCycle", hourCycle, null, realm);
        try object.defineBuiltinAccessor(agent, "language", language, null, realm);
        try object.defineBuiltinFunction(agent, "maximize", maximize, 0, realm);
        try object.defineBuiltinFunction(agent, "minimize", minimize, 0, realm);
        try object.defineBuiltinAccessor(agent, "numberingSystem", numberingSystem, null, realm);
        try object.defineBuiltinAccessor(agent, "numeric", numeric, null, realm);
        try object.defineBuiltinAccessor(agent, "region", region, null, realm);
        try object.defineBuiltinAccessor(agent, "script", script, null, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);

        // 15.3.1 Intl.Locale.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.Locale.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.Locale%"()),
        );

        // 15.3.15 Intl.Locale.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-Intl.Locale.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Intl.Locale"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 15.3.2 get Intl.Locale.prototype.baseName
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.baseName
    fn baseName(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleBaseName(loc.[[Locale]]).
        return Value.from(
            try String.fromAscii(agent, try locale.fields.locale.basename(agent.gc_allocator)),
        );
    }

    /// 15.3.3 get Intl.Locale.prototype.calendar
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.calendar
    fn calendar(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Calendar]].
        return Value.from(
            try String.fromAscii(
                agent,
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "ca",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                } orelse return .undefined,
            ),
        );
    }

    /// 15.3.4 get Intl.Locale.prototype.caseFirst
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.caseFirst
    fn caseFirst(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[CaseFirst]].
        return Value.from(
            try String.fromAscii(
                agent,
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "kf",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                } orelse return .undefined,
            ),
        );
    }

    /// 15.3.5 get Intl.Locale.prototype.collation
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.collation
    fn collation(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Collation]].
        return Value.from(
            try String.fromAscii(
                agent,
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "co",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                } orelse return .undefined,
            ),
        );
    }

    /// 15.3.6 get Intl.Locale.prototype.hourCycle
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.hourCycle
    fn hourCycle(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[HourCycle]].
        return Value.from(
            try String.fromAscii(
                agent,
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "hc",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                } orelse return .undefined,
            ),
        );
    }

    /// 15.3.7 get Intl.Locale.prototype.language
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.language
    fn language(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleLanguage(loc.[[Locale]]).
        return Value.from(
            try String.fromAscii(
                agent,
                try locale.fields.locale.language(agent.gc_allocator),
            ),
        );
    }

    /// 15.3.8 Intl.Locale.prototype.maximize ( )
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.maximize
    fn maximize(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Let maximal be the result of the Add Likely Subtags algorithm applied to
        //    loc.[[Locale]]. If an error is signaled, set maximal to loc.[[Locale]].
        const locale_expander = icu4zig.LocaleExpander.init();
        defer locale_expander.deinit();
        var maximal = locale.fields.locale.clone();
        _ = locale_expander.maximize(&maximal);

        // 4. Return ! Construct(%Intl.Locale%, maximal).
        const new_locale = ordinaryCreateFromConstructor(
            Locale,
            agent,
            try realm.intrinsics.@"%Intl.Locale%"(),
            "%Intl.Locale.prototype%",
            .{ .locale = maximal },
        ) catch |err| try noexcept(err);
        return Value.from(&new_locale.object);
    }

    /// 15.3.9 Intl.Locale.prototype.minimize ( )
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.minimize
    fn minimize(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 2. Let minimal be the result of the Remove Likely Subtags algorithm applied to
        //    loc.[[Locale]]. If an error is signaled, set minimal to loc.[[Locale]].
        const locale_expander = icu4zig.LocaleExpander.init();
        defer locale_expander.deinit();
        var minimal = locale.fields.locale.clone();
        _ = locale_expander.minimize(&minimal);

        // 4. Return ! Construct(%Intl.Locale%, minimal).
        const new_locale = ordinaryCreateFromConstructor(
            Locale,
            agent,
            try realm.intrinsics.@"%Intl.Locale%"(),
            "%Intl.Locale.prototype%",
            .{ .locale = minimal },
        ) catch |err| try noexcept(err);
        return Value.from(&new_locale.object);
    }

    /// 15.3.10 get Intl.Locale.prototype.numberingSystem
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.numberingSystem
    fn numberingSystem(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[NumberingSystem]].
        return Value.from(
            try String.fromAscii(
                agent,
                locale.fields.locale.getUnicodeExtension(
                    agent.gc_allocator,
                    "nu",
                ) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                } orelse return .undefined,
            ),
        );
    }

    /// 15.3.11 get Intl.Locale.prototype.numeric
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
        } orelse return Value.from(false);
        return Value.from(value.len == 0 or std.mem.eql(u8, value, "true"));
    }

    /// 15.3.12 get Intl.Locale.prototype.region
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.region
    fn region(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleRegion(loc.[[Locale]]).
        return Value.from(
            try String.fromAscii(
                agent,
                try locale.fields.locale.region(agent.gc_allocator) orelse return .undefined,
            ),
        );
    }

    /// 15.3.13 get Intl.Locale.prototype.script
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.script
    fn script(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return GetLocaleScript(loc.[[Locale]]).
        return Value.from(
            try String.fromAscii(
                agent,
                try locale.fields.locale.script(agent.gc_allocator) orelse return .undefined,
            ),
        );
    }

    /// 15.3.14 Intl.Locale.prototype.toString ( )
    /// https://tc39.es/ecma402/#sec-Intl.Locale.prototype.toString
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let loc be the this value.
        // 2. Perform ? RequireInternalSlot(loc, [[InitializedLocale]]).
        const locale = try this_value.requireInternalSlot(agent, Locale);

        // 3. Return loc.[[Locale]].
        return Value.from(
            try String.fromAscii(agent, try locale.fields.locale.toString(agent.gc_allocator)),
        );
    }
};

/// 15.4 Properties of Intl.Locale Instances
/// https://tc39.es/ecma402/#sec-properties-of-intl-locale-instances
pub const Locale = MakeObject(.{
    .Fields = struct {
        /// [[Locale]]
        locale: icu4zig.Locale,
    },
    .tag = .intl_locale,
});
