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
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getOption = types.getOption;
const getOptionsObject = abstract_operations.getOptionsObject;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const ordinaryObjectCreateWithType = builtins.ordinaryObjectCreateWithType;

const AnySegmenter = union(enum) {
    grapheme: icu4zig.GraphemeClusterSegmenter,
    word: icu4zig.WordSegmenter,
    sentence: icu4zig.SentenceSegmenter,

    const BreakIterator = union(enum) {
        grapheme: icu4zig.GraphemeClusterSegmenter.GraphemeClusterBreakIterator,
        word: icu4zig.WordSegmenter.WordBreakIterator,
        sentence: icu4zig.SentenceSegmenter.SentenceBreakIterator,

        fn deinit(self: BreakIterator) void {
            switch (self) {
                .grapheme => |iterator| iterator.deinit(),
                .word => |iterator| iterator.deinit(),
                .sentence => |iterator| iterator.deinit(),
            }
        }
    };

    fn segment(self: AnySegmenter, string: String) BreakIterator {
        return switch (self) {
            .grapheme => |segmenter| .{
                .grapheme = segmenter.segment(switch (string) {
                    .ascii => |utf8| .{ .utf8 = utf8 },
                    .utf16 => |utf16| .{ .utf16 = utf16 },
                }),
            },
            .word => |segmenter| .{
                .word = segmenter.segment(switch (string) {
                    .ascii => |utf8| .{ .utf8 = utf8 },
                    .utf16 => |utf16| .{ .utf16 = utf16 },
                }),
            },
            .sentence => |segmenter| .{
                .sentence = segmenter.segment(switch (string) {
                    .ascii => |utf8| .{ .utf8 = utf8 },
                    .utf16 => |utf16| .{ .utf16 = utf16 },
                }),
            },
        };
    }

    fn deinit(self: AnySegmenter) void {
        switch (self) {
            .grapheme => |segmenter| segmenter.deinit(),
            .word => |segmenter| segmenter.deinit(),
            .sentence => |segmenter| segmenter.deinit(),
        }
    }
};

/// 18.2 Properties of the Intl.Segmenter Constructor
/// https://tc39.es/ecma402/#sec-properties-of-intl-segmenter-constructor
pub const SegmenterConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 0,
            .name = "Segmenter",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        // 18.2.1 Intl.Segmenter.prototype
        // https://tc39.es/ecma402/#sec-intl.segmenter.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Segmenter.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
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
            agent.platform.default_locale;

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
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "segment", segment, 1, realm);
        try defineBuiltinFunction(object, "resolvedOptions", resolvedOptions, 0, realm);

        // 18.3.1 Intl.Segmenter.prototype.constructor
        // https://tc39.es/ecma402/#sec-intl.segmenter.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%Intl.Segmenter%"()),
        );

        // 18.3.2 Intl.Segmenter.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-intl.segmenter.prototype-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Intl.Segmenter"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 18.3.3 Intl.Segmenter.prototype.segment ( string )
    /// https://tc39.es/ecma402/#sec-intl.segmenter.prototype.segment
    fn segment(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const string_value = arguments.get(0);

        // 1. Let segmenter be the this value.
        // 2. Perform ? RequireInternalSlot(segmenter, [[InitializedSegmenter]]).
        const segmenter = try this_value.requireInternalSlot(agent, Segmenter);

        // 3. Let string be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Return CreateSegmentsObject(segmenter, string).
        return Value.from(try createSegmentsObject(agent, segmenter, string));
    }

    /// 18.3.4 Intl.Segmenter.prototype.resolvedOptions ( )
    /// https://tc39.es/ecma402/#sec-intl.segmenter.prototype.resolvedoptions
    fn resolvedOptions(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let segmenter be the this value.
        // 2. Perform ? RequireInternalSlot(segmenter, [[InitializedSegmenter]]).
        const segmenter = try this_value.requireInternalSlot(agent, Segmenter);

        // 3. Let options be OrdinaryObjectCreate(%Object.prototype%).
        const options = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        // 4. For each row of Table 28, except the header row, in table order, do
        //     a. Let p be the Property value of the current row.
        //     b. Let v be the value of segmenter's internal slot whose name is the Internal Slot value of the current row.
        //     c. Assert: v is not undefined.
        //     d. Perform ! CreateDataPropertyOrThrow(options, p, v).
        options.createDataPropertyOrThrow(
            PropertyKey.from("locale"),
            Value.from(String.fromAscii(try segmenter.fields.locale.toString(agent.gc_allocator))),
        ) catch |err| try noexcept(err);
        options.createDataPropertyOrThrow(
            PropertyKey.from("granularity"),
            Value.from(String.fromAscii(@tagName(segmenter.fields.segmenter_granularity))),
        ) catch |err| try noexcept(err);

        // 5. Return options.
        return Value.from(options);
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

/// 18.5.1 CreateSegmentsObject ( segmenter, string )
/// https://tc39.es/ecma402/#sec-createsegmentsobject
fn createSegmentsObject(
    agent: *Agent,
    segmenter: *Segmenter,
    string: String,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let internalSlotsList be « [[SegmentsSegmenter]], [[SegmentsString]] ».
    // 2. Let segments be OrdinaryObjectCreate(%IntlSegmentsPrototype%, internalSlotsList).
    const segments = try ordinaryObjectCreateWithType(
        Segments,
        agent,
        try realm.intrinsics.@"%IntlSegmentsPrototype%"(),
        .{
            // 3. Set segments.[[SegmentsSegmenter]] to segmenter.
            .segments_segmenter = segmenter,

            // 4. Set segments.[[SegmentsString]] to string.
            .segments_string = string,
        },
    );

    // 5. Return segments.
    return segments;
}

/// 18.5.2 The %IntlSegmentsPrototype% Object
/// https://tc39.es/ecma402/#sec-%intlsegmentsprototype%-object
pub const IntlSegmentsPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "containing", containing, 1, realm);
        try defineBuiltinFunction(object, "%Symbol.iterator%", @"%Symbol.iterator%", 0, realm);
    }

    /// 18.5.2.1 %IntlSegmentsPrototype%.containing ( index )
    /// https://tc39.es/ecma402/#sec-%intlsegmentsprototype%.containing
    fn containing(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const index = arguments.get(0);

        // 1. Let segments be the this value.
        // 2. Perform ? RequireInternalSlot(segments, [[SegmentsSegmenter]]).
        const segments = try this_value.requireInternalSlot(agent, Segments);

        // 3. Let segmenter be segments.[[SegmentsSegmenter]].
        const segmenter = segments.fields.segments_segmenter;

        // 4. Let string be segments.[[SegmentsString]].
        const string = segments.fields.segments_string;

        // 5. Let len be the length of string.
        const len = string.length();

        // 6. Let n be ? ToIntegerOrInfinity(index).
        const n = try index.toIntegerOrInfinity(agent);

        // 7. If n < 0 or n ≥ len, return undefined.
        if (n < 0 or n >= @as(f64, @floatFromInt(len))) return Value.undefined;

        // 8. Let startIndex be FindBoundary(segmenter, string, n, before).
        const boundary_before = findBoundary(segmenter, string, @intFromFloat(n), .before);
        const start_index = boundary_before.index;

        // 9. Let endIndex be FindBoundary(segmenter, string, n, after).
        const boundary_after = findBoundary(segmenter, string, @intFromFloat(n), .after);
        const end_index = boundary_after.index;
        const is_word_like = boundary_after.is_word_like orelse false;

        // 10. Return CreateSegmentDataObject(segmenter, string, startIndex, endIndex).
        return Value.from(
            try createSegmentDataObject(
                agent,
                segmenter,
                string,
                start_index,
                end_index,
                is_word_like,
            ),
        );
    }

    /// 18.5.2.2 %IntlSegmentsPrototype% [ %Symbol.iterator% ] ( )
    /// https://tc39.es/ecma402/#sec-%intlsegmentsprototype%-%symbol.iterator%
    fn @"%Symbol.iterator%"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let segments be the this value.
        // 2. Perform ? RequireInternalSlot(segments, [[SegmentsSegmenter]]).
        const segments = try this_value.requireInternalSlot(agent, Segments);

        // 3. Let segmenter be segments.[[SegmentsSegmenter]].
        const segmenter = segments.fields.segments_segmenter;

        // 4. Let string be segments.[[SegmentsString]].
        const string = segments.fields.segments_string;

        // 5. Return CreateSegmentIterator(segmenter, string).
        return Value.from(try createSegmentIterator(agent, segmenter, string));
    }
};

/// 18.5.3 Properties of Segments Instances
/// https://tc39.es/ecma402/#sec-properties-of-segments-instances
pub const Segments = MakeObject(.{
    .Fields = struct {
        /// [[SegmentsSegmenter]]
        segments_segmenter: *Segmenter,

        /// [[SegmentsString]]
        segments_string: String,
    },
    .tag = .intl_segments,
});

/// 18.6.1 CreateSegmentIterator ( segmenter, string )
/// https://tc39.es/ecma402/#sec-createsegmentiterator
pub fn createSegmentIterator(
    agent: *Agent,
    segmenter: *Segmenter,
    string: String,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let internalSlotsList be « [[IteratingSegmenter]], [[IteratedString]], [[IteratedStringNextSegmentCodeUnitIndex]] ».
    // 2. Let iterator be OrdinaryObjectCreate(%IntlSegmentIteratorPrototype%, internalSlotsList).
    const iterator = SegmentIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%IntlSegmentIteratorPrototype%"(),
        .fields = .{
            // 3. Set iterator.[[IteratingSegmenter]] to segmenter.
            .iterating_segmenter = segmenter,

            // 4. Set iterator.[[IteratedString]] to string.
            .iterated_string = string,

            // 5. Set iterator.[[IteratedStringNextSegmentCodeUnitIndex]] to 0.
            .iterated_string_next_segment_code_unit_index = 0,
        },
    });

    // 6. Return iterator.
    return iterator;
}

/// 18.6.2 The %IntlSegmentIteratorPrototype% Object
/// https://tc39.es/ecma402/#sec-%intlsegmentiteratorprototype%-object
pub const IntlSegmentIteratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 18.6.2.2 %IntlSegmentIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma402/#sec-%intlsegmentiteratorprototype%.%Symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Segmenter String Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 18.6.2.1 %IntlSegmentIteratorPrototype%.next ( )
    /// https://tc39.es/ecma402/#sec-%intlsegmentiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. let iterator be the this value.
        // 2. Perform ? RequireInternalSlot(iterator, [[IteratingSegmenter]]).
        const iterator = try this_value.requireInternalSlot(agent, SegmentIterator);

        // 3. Let segmenter be iterator.[[IteratingSegmenter]].
        const segmenter = iterator.fields.iterating_segmenter;

        // 4. Let string be iterator.[[IteratedString]].
        const string = iterator.fields.iterated_string;

        // 5. Let startIndex be iterator.[[IteratedStringNextSegmentCodeUnitIndex]].
        const start_index = iterator.fields.iterated_string_next_segment_code_unit_index;

        // 6. Let len be the length of string.
        const len = string.length();

        // 7. If startIndex ≥ len, then
        if (start_index >= len) {
            // a. Return CreateIterResultObject(undefined, true).
            return Value.from(try createIterResultObject(agent, Value.undefined, true));
        }

        // 8. Let endIndex be FindBoundary(segmenter, string, startIndex, after).
        const boundary_after = findBoundary(segmenter, string, start_index, .after);
        const end_index = boundary_after.index;
        const is_word_like = boundary_after.is_word_like orelse false;

        // 9. Set iterator.[[IteratedStringNextSegmentCodeUnitIndex]] to endIndex.
        iterator.fields.iterated_string_next_segment_code_unit_index = end_index;

        // 10. Let segmentData be CreateSegmentDataObject(segmenter, string, startIndex, endIndex).
        const segment_data = try createSegmentDataObject(
            agent,
            segmenter,
            string,
            start_index,
            end_index,
            is_word_like,
        );

        // 11. Return CreateIterResultObject(segmentData, false).
        return Value.from(try createIterResultObject(agent, Value.from(segment_data), false));
    }
};

pub const SegmentIterator = MakeObject(.{
    .Fields = struct {
        iterating_segmenter: *Segmenter,
        iterated_string: String,
        iterated_string_next_segment_code_unit_index: usize,
    },
    .tag = .intl_segment_iterator,
});

/// 18.7.1 CreateSegmentDataObject ( segmenter, string, startIndex, endIndex )
/// https://tc39.es/ecma402/#sec-createsegmentdataobject
fn createSegmentDataObject(
    agent: *Agent,
    segmenter: *Segmenter,
    string: String,
    start_index: usize,
    end_index: usize,
    is_word_like: bool,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let len be the length of string.
    const len = string.length();

    // 2. Assert: endIndex ≤ len.
    std.debug.assert(end_index <= len);

    // 3. Assert: startIndex < endIndex.
    std.debug.assert(start_index < end_index);

    // 4. Let result be OrdinaryObjectCreate(%Object.prototype%).
    const result = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());

    // 5. Let segment be the substring of string from startIndex to endIndex.
    const segment = try string.substring(agent.gc_allocator, start_index, end_index);

    // 6. Perform ! CreateDataPropertyOrThrow(result, "segment", segment).
    result.createDataPropertyOrThrow(
        PropertyKey.from("segment"),
        Value.from(segment),
    ) catch |err| try noexcept(err);

    // 7. Perform ! CreateDataPropertyOrThrow(result, "index", 𝔽(startIndex)).
    result.createDataPropertyOrThrow(
        PropertyKey.from("index"),
        Value.from(@as(u53, @intCast(start_index))),
    ) catch |err| try noexcept(err);

    // 8. Perform ! CreateDataPropertyOrThrow(result, "input", string).
    result.createDataPropertyOrThrow(
        PropertyKey.from("input"),
        Value.from(string),
    ) catch |err| try noexcept(err);

    // 9. Let granularity be segmenter.[[SegmenterGranularity]].
    const granularity = segmenter.fields.segmenter_granularity;

    // 10. If granularity is "word", then
    if (granularity == .word) {
        // a. Let isWordLike be a Boolean value indicating whether the segment in string is
        //    "word-like" according to locale segmenter.[[Locale]].

        // b. Perform ! CreateDataPropertyOrThrow(result, "isWordLike", isWordLike).
        result.createDataPropertyOrThrow(
            PropertyKey.from("isWordLike"),
            Value.from(is_word_like),
        ) catch |err| try noexcept(err);
    }

    // 11. Return result.
    return result;
}

const Boundary = struct {
    index: usize,
    is_word_like: ?bool = null,
};

/// 18.8.1 FindBoundary ( segmenter, string, startIndex, direction )
/// https://tc39.es/ecma402/#sec-findboundary
fn findBoundary(
    segmenter: *Segmenter,
    string: String,
    start_index: usize,
    direction: enum { before, after },
) Boundary {
    // 1. Let len be the length of string.
    const len = string.length();

    // 2. Assert: startIndex < len.
    std.debug.assert(start_index < len);

    // 3. Let locale be segmenter.[[Locale]].

    // 4. Let granularity be segmenter.[[SegmenterGranularity]].
    const granularity = segmenter.fields.segmenter_granularity;

    // 5. If direction is before, then
    if (direction == .before) {
        // a. Search string for the last segmentation boundary that is preceded by at most
        //    startIndex code units from the beginning, using locale locale and text element
        //    granularity granularity.
        // b. If a boundary is found, return the count of code units in string preceding it.
        if (findBoundaryBefore(string, start_index, granularity)) |boundary| return boundary;

        // c. Return 0.
        return .{ .index = 0 };
    }

    // 6. Assert: direction is after.
    std.debug.assert(direction == .after);

    // 7. Search string for the first segmentation boundary that follows the code unit at index
    //    startIndex, using locale locale and text element granularity granularity.
    // 8. If a boundary is found, return the count of code units in string preceding it.
    if (findBoundaryAfter(string, start_index, granularity)) |boundary| return boundary;

    // 9. Return len.
    return .{ .index = len };
}

fn findBoundaryBefore(
    string: String,
    start_index: usize,
    granularity: Segmenter.Fields.SegmenterGranularity,
) ?Boundary {
    const data_provider = icu4zig.DataProvider.init();
    defer data_provider.deinit();
    const segmenter: AnySegmenter = switch (granularity) {
        .grapheme => .{ .grapheme = icu4zig.GraphemeClusterSegmenter.init(data_provider) },
        .word => .{ .word = icu4zig.WordSegmenter.init(data_provider) },
        .sentence => .{ .sentence = icu4zig.SentenceSegmenter.init(data_provider) },
    };
    defer segmenter.deinit();
    var iterator = segmenter.segment(string);
    defer iterator.deinit();
    var previous_index: usize = 0;
    return switch (iterator) {
        .grapheme => |*it| while (it.next()) |index| : (previous_index = index) {
            if (index > start_index) break .{ .index = previous_index };
        } else null,
        .word => |*it| while (it.next()) |index| : (previous_index = index) {
            if (index > start_index) break .{ .index = previous_index, .is_word_like = it.isWordLike() };
        } else null,
        .sentence => |*it| while (it.next()) |index| : (previous_index = index) {
            if (index > start_index) break .{ .index = previous_index };
        } else null,
    };
}

fn findBoundaryAfter(
    string: String,
    start_index: usize,
    granularity: Segmenter.Fields.SegmenterGranularity,
) ?Boundary {
    const data_provider = icu4zig.DataProvider.init();
    defer data_provider.deinit();
    const segmenter: AnySegmenter = switch (granularity) {
        .grapheme => .{ .grapheme = icu4zig.GraphemeClusterSegmenter.init(data_provider) },
        .word => .{ .word = icu4zig.WordSegmenter.init(data_provider) },
        .sentence => .{ .sentence = icu4zig.SentenceSegmenter.init(data_provider) },
    };
    defer segmenter.deinit();
    var iterator = segmenter.segment(string);
    defer iterator.deinit();
    return switch (iterator) {
        .grapheme => |*it| while (it.next()) |index| {
            if (index > start_index) break .{ .index = index };
        } else null,
        .word => |*it| while (it.next()) |index| {
            if (index > start_index) return .{ .index = index, .is_word_like = it.isWordLike() };
        } else null,
        .sentence => |*it| while (it.next()) |index| {
            if (index > start_index) return .{ .index = index };
        } else null,
    };
}
