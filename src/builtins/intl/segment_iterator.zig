//! 18.6 Segment Iterator Objects
//! https://tc39.es/ecma402/#sec-segment-iterator-objects

const std = @import("std");

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
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const findBoundary = builtins.intl.findBoundary;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 18.6.1 CreateSegmentIterator ( segmenter, string )
/// https://tc39.es/ecma402/#sec-createsegmentiterator
pub fn createSegmentIterator(
    agent: *Agent,
    segmenter: *builtins.intl.Segmenter,
    string: String,
) std.mem.Allocator.Error!Object {
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
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
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
            // a. Return CreateIteratorResultObject(undefined, true).
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
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

        // 11. Return CreateIteratorResultObject(segmentData, false).
        return Value.from(try createIteratorResultObject(agent, Value.from(segment_data), false));
    }
};

pub const SegmentIterator = MakeObject(.{
    .Fields = struct {
        iterating_segmenter: *builtins.intl.Segmenter,
        iterated_string: String,
        iterated_string_next_segment_code_unit_index: usize,
    },
    .tag = .intl_segment_iterator,
});

/// 18.7.1 CreateSegmentDataObject ( segmenter, string, startIndex, endIndex )
/// https://tc39.es/ecma402/#sec-createsegmentdataobject
pub fn createSegmentDataObject(
    agent: *Agent,
    segmenter: *builtins.intl.Segmenter,
    string: String,
    start_index: usize,
    end_index: usize,
    is_word_like: bool,
) std.mem.Allocator.Error!Object {
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
