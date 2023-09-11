//! 23.1.5 Array Iterator Objects
//! https://tc39.es/ecma262/#sec-array-iterator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 23.1.5.1 CreateArrayIterator ( array, kind )
/// https://tc39.es/ecma262/#sec-createarrayiterator
pub fn createArrayIterator(
    agent: *Agent,
    array: Object,
    comptime kind: Object.PropertyKind,
) !Object {
    const realm = agent.currentRealm();

    // 1. Let closure be a new Abstract Closure with no parameters that captures kind and array and
    //    performs the following steps when called:
    //    [...]
    // 2. Return CreateIteratorFromClosure(closure, "%ArrayIteratorPrototype%", %ArrayIteratorPrototype%).
    return ArrayIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%ArrayIteratorPrototype%"(),
        .fields = .{ .array = array, .kind = kind, .index = 0 },
    });
}

/// 23.1.5.2 The %ArrayIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-object
pub const ArrayIteratorPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });

        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 23.1.5.2.2 %ArrayIteratorPrototype% [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Array Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 23.1.5.2.1 %ArrayIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%arrayiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        if (this_value != .object or !this_value.object.is(ArrayIterator)) {
            return agent.throwException(.type_error, "This value must be an Array Iterator");
        }
        const array_iterator = this_value.object.as(ArrayIterator);
        const array = array_iterator.fields.array;
        const kind = array_iterator.fields.kind;
        const index = array_iterator.fields.index;

        // 1. Return ? GeneratorResume(this value, empty, "%ArrayIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateArrayIterator closure. State is kept track of through the ArrayIterator
        //       instance instead of as local variables. This should not be observable.

        // TODO: i. If array has a [[TypedArrayName]] internal slot, then
        const len = if (false) {
            // 1. If IsDetachedBuffer(array.[[ViewedArrayBuffer]]) is true, throw a TypeError exception.
            // 2. Let len be array.[[ArrayLength]].
        }
        // ii. Else,
        else blk: {
            // 1. Let len be ? LengthOfArrayLike(array).
            break :blk try array.lengthOfArrayLike();
        };

        // iii. If index ≥ len, return NormalCompletion(undefined).
        if (index >= len) {
            return Value.from(try createIterResultObject(agent, .undefined, true));
        }

        // iv. Let indexNumber be 𝔽(index).
        const index_number = Value.from(index);

        // v. If kind is key, then
        const result = if (kind == .key) blk: {
            // 1. Let result be indexNumber.
            break :blk index_number;
        }
        // vi. Else,
        else blk: {
            // 1. Let elementKey be ! ToString(indexNumber).
            const element_key = PropertyKey.from(index);

            // 2. Let elementValue be ? Get(array, elementKey).
            const element_value = try array.get(element_key);
            // 3. If kind is value, then
            if (kind == .value) {
                // a. Let result be elementValue.
                break :blk element_value;
            }
            // 4. Else,
            else {
                // a. Assert: kind is key+value.
                std.debug.assert(kind == .@"key+value");

                // b. Let result be CreateArrayFromList(« indexNumber, elementValue »).
                break :blk Value.from(
                    try createArrayFromList(agent, &.{ index_number, element_value }),
                );
            }
        };

        // viii. Set index to index + 1.
        array_iterator.fields.index += 1;

        // vii. Perform ? GeneratorYield(CreateIterResultObject(result, false)).
        return Value.from(try createIterResultObject(agent, result, false));
    }
};

pub const ArrayIterator = Object.Factory(.{
    .Fields = struct {
        array: Object,
        kind: Object.PropertyKind,
        index: u53,
    },
    .tag = .array_iterator,
});
