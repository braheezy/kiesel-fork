//! 14 ListFormat Objects
//! https://tc39.es/ecma402/#listformat-objects

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
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getIterator = types.getIterator;
const getOptionsObject = abstract_operations.getOptionsObject;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 14.2 Properties of the Intl.ListFormat Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-listformat-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "ListFormat",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 14.2.1 Intl.ListFormat.prototype
        // https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype
        try object.defineBuiltinProperty(agent, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.ListFormat.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 14.1.1 Intl.ListFormat ( [ locales [ , options ] ] )
    /// https://tc39.es/ecma402/#sec-Intl.ListFormat
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
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
        const matcher = try options.getOption(
            agent,
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
            agent.platform.default_locale;

        // 9. Set listFormat.[[Locale]] to r.[[Locale]].
        list_format.as(ListFormat).fields.locale = resolved_locale;

        // 10. Let type be ? GetOption(options, "type", string, « "conjunction", "disjunction",
        //     "unit" », "conjunction").
        const type_ = try options.getOption(
            agent,
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
        list_format.as(ListFormat).fields.type = type_map.get(type_.slice.ascii).?;

        // 12. Let style be ? GetOption(options, "style", string, « "long", "short", "narrow" »,
        //     "long").
        const style = try options.getOption(
            agent,
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
        list_format.as(ListFormat).fields.style = style_map.get(style.slice.ascii).?;

        // TODO: 14. Let resolvedLocaleData be r.[[LocaleData]].
        // TODO: 15. Let dataLocaleTypes be resolvedLocaleData.[[<type>]].
        // TODO: 16. Set listFormat.[[Templates]] to dataLocaleTypes.[[<style>]].

        // 17. Return listFormat.
        return Value.from(list_format);
    }
};

/// 14.3 Properties of the Intl.ListFormat Prototype Object
/// https://tc39.es/ecma402/#sec-properties-of-intl-listformat-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "resolvedOptions", resolvedOptions, 0, realm);
        try object.defineBuiltinFunction(agent, "format", format, 1, realm);

        // 14.3.1 Intl.ListFormat.prototype.constructor
        // https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.ListFormat%"()),
        );

        // 14.3.5 Intl.ListFormat.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype-toStringTag
        try object.defineBuiltinProperty(agent, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Intl.ListFormat"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 14.3.2 Intl.ListFormat.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype.resolvedoptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let lf be the this value.
        // 2. Perform ? RequireInternalSlot(lf, [[InitializedListFormat]]).
        const list_format = try this_value.requireInternalSlot(agent, ListFormat);

        // 3. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each row of Table 20, except the header row, in table order, do
        //     a. Let p be the Property value of the current row.
        //     b. Let v be the value of lf's internal slot whose name is the Internal Slot value of the current row.
        //     c. Assert: v is not undefined.
        //     d. Perform ! CreateDataPropertyOrThrow(options, p, v).
        const resolved_options = list_format.fields.resolvedOptions();
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("locale"),
            Value.from(try String.fromAscii(agent, try list_format.fields.locale.toString(agent.gc_allocator))),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("type"),
            Value.from(resolved_options.type),
        );
        try options.createDataPropertyDirect(
            agent,
            PropertyKey.from("style"),
            Value.from(resolved_options.style),
        );

        // 5. Return options.
        return Value.from(options);
    }

    /// 14.3.3 Intl.ListFormat.prototype.format ( list )
    /// https://tc39.es/ecma402/#sec-Intl.ListFormat.prototype.format
    fn format(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const list = arguments.get(0);

        // 1. Let lf be the this value.
        // 2. Perform ? RequireInternalSlot(lf, [[InitializedListFormat]]).
        const list_format = try this_value.requireInternalSlot(agent, ListFormat);

        // 3. Let stringList be ? StringListFromIterable(list).
        const string_list = try stringListFromIterable(agent, list);
        defer {
            for (string_list) |string| agent.gc_allocator.free(string);
            agent.gc_allocator.free(string_list);
        }

        // 4. Return FormatList(lf, stringList).
        return Value.from(try formatList(agent, list_format, string_list));
    }
};

/// 14.4 Properties of Intl.ListFormat Instances
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

        pub const ResolvedOptions = struct {
            type: *const String,
            style: *const String,
        };

        pub fn resolvedOptions(self: @This()) ResolvedOptions {
            const @"type" = switch (self.type) {
                .conjunction => String.fromLiteral("conjunction"),
                .disjunction => String.fromLiteral("disjunction"),
                .unit => String.fromLiteral("unit"),
            };
            const style = switch (self.style) {
                .long => String.fromLiteral("long"),
                .short => String.fromLiteral("short"),
                .narrow => String.fromLiteral("narrow"),
            };
            return .{
                .type = @"type",
                .style = style,
            };
        }
    },
    .tag = .intl_list_format,
});

/// 14.5.3 FormatList ( listFormat, list )
/// https://tc39.es/ecma402/#sec-formatlist
fn formatList(
    agent: *Agent,
    list_format: *const ListFormat,
    list: []const []const u8,
) std.mem.Allocator.Error!*const String {
    const list_formatter = icu4zig.ListFormatter.init(
        list_format.fields.locale,
        .{
            .type = switch (list_format.fields.type) {
                .conjunction => .@"and",
                .disjunction => .@"or",
                .unit => .unit,
            },
            .length = switch (list_format.fields.style) {
                .long => .wide,
                .short => .short,
                .narrow => .narrow,
            },
        },
    );
    defer list_formatter.deinit();
    return String.fromUtf8(agent, try list_formatter.formatUtf8(agent.gc_allocator, list));
}

/// 14.5.5 StringListFromIterable ( iterable )
/// https://tc39.es/ecma402/#sec-createstringlistfromiterable
fn stringListFromIterable(agent: *Agent, iterable: Value) Agent.Error![]const []const u8 {
    // 1. If iterable is undefined, then
    if (iterable.isUndefined()) {
        // a. Return a new empty List.
        return &.{};
    }

    // 2. Let iteratorRecord be ? GetIterator(iterable, sync).
    var iterator = try getIterator(agent, iterable, .sync);

    // 3. Let list be a new empty List.
    var list: std.ArrayListUnmanaged([]const u8) = .empty;

    // 4. Repeat,
    //     a. Let next be ? IteratorStepValue(iteratorRecord).
    //     b. If next is done, then
    //         i. Return list.
    while (try iterator.stepValue(agent)) |next| {
        // c. If next is not a String, then
        if (!next.isString()) {
            // i. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "Iterable must return string items",
                .{},
            );

            // ii. Return ? IteratorClose(iteratorRecord, error).
            return iterator.close(agent, @as(Agent.Error![]const []const u8, @"error"));
        }

        // d. Append next to list.
        try list.append(agent.gc_allocator, try next.asString().toUtf8(agent.gc_allocator));
    }
    return list.toOwnedSlice(agent.gc_allocator);
}
