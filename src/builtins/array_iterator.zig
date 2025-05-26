//! 23.1.5 Array Iterator Objects
//! https://tc39.es/ecma262/#sec-array-iterator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createIteratorResultObject = types.createIteratorResultObject;
const isTypedArrayOutOfBounds = builtins.isTypedArrayOutOfBounds;
const makeTypedArrayWithBufferWitnessRecord = builtins.makeTypedArrayWithBufferWitnessRecord;
const typedArrayLength = builtins.typedArrayLength;

/// 23.1.5.1 CreateArrayIterator ( array, kind )
/// https://tc39.es/ecma262/#sec-createarrayiterator
pub fn createArrayIterator(
    agent: *Agent,
    array: *Object,
    comptime kind: Object.PropertyKind,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Let iterator be OrdinaryObjectCreate(%ArrayIteratorPrototype%, « [[IteratedArrayLike]],
    //    [[ArrayLikeNextIndex]], [[ArrayLikeIterationKind]] »).
    const iterator = try ArrayIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%ArrayIteratorPrototype%"(),
        .fields = .{
            .state = .{
                // 2. Set iterator.[[IteratedArrayLike]] to array.
                .iterated_array_like = array,

                // 3. Set iterator.[[ArrayLikeNextIndex]] to 0.
                .array_like_next_index = 0,

                // 4. Set iterator.[[ArrayLikeIterationKind]] to kind.
                .array_like_iteration_kind = kind,
            },
        },
    });

    // 5. Return iterator.
    return iterator;
}

/// 23.1.5.2 The %ArrayIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "next", next, 0, realm);

        // 23.1.5.2.2 %ArrayIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Array Iterator"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 23.1.5.2.1 %ArrayIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%arrayiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        // 3. If O does not have all of the internal slots of an Array Iterator Instance
        //    (23.1.5.2.3), throw a TypeError exception.
        if (!this_value.isObject() or !this_value.asObject().is(ArrayIterator)) {
            return agent.throwException(.type_error, "This value must be an Array Iterator", .{});
        }
        const array_iterator = this_value.asObject().as(ArrayIterator);

        // 5. If array is undefined, return CreateIteratorResultObject(undefined, true).
        if (array_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        // 4. Let array be O.[[IteratedArrayLike]].
        const array = array_iterator.fields.state.iterated_array_like;

        // 6. Let index be O.[[ArrayLikeNextIndex]].
        const index = array_iterator.fields.state.array_like_next_index;

        // 7. Let kind be O.[[ArrayLikeIterationKind]].
        const kind = array_iterator.fields.state.array_like_iteration_kind;

        // 8. If array has a [[TypedArrayName]] internal slot, then
        const len = if (array.is(builtins.TypedArray)) blk: {
            // a. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(array, seq-cst).
            const ta = makeTypedArrayWithBufferWitnessRecord(
                array.as(builtins.TypedArray),
                .seq_cst,
            );

            // b. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
            if (isTypedArrayOutOfBounds(ta)) {
                return agent.throwException(.type_error, "Typed array is out of bounds", .{});
            }

            // c. Let len be TypedArrayLength(taRecord).
            break :blk typedArrayLength(ta);
        } else blk: {
            // 9. Else,
            // a. Let len be ? LengthOfArrayLike(array).
            break :blk try array.lengthOfArrayLike(agent);
        };

        // 10. If index ≥ len, then
        if (index >= len) {
            // a. Set O.[[IteratedArrayLike]] to undefined.
            array_iterator.fields = .completed;

            // b. Return CreateIteratorResultObject(undefined, true).
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        // 11. Set O.[[ArrayLikeNextIndex]] to index + 1.
        array_iterator.fields.state.array_like_next_index += 1;

        // 12. Let indexNumber be 𝔽(index).
        const index_number = Value.from(index);

        // 13. If kind is key, then
        const result = if (kind == .key) blk: {
            // a. Let result be indexNumber.
            break :blk index_number;
        } else blk: {
            // 14. Else,
            // a. Let elementKey be ! ToString(indexNumber).
            const element_key = PropertyKey.from(index);

            // b. Let elementValue be ? Get(array, elementKey).
            const element_value = try array.get(agent, element_key);

            // c. If kind is value, then
            if (kind == .value) {
                // i. Let result be elementValue.
                break :blk element_value;
            } else {
                // d. Else,
                // i. Assert: kind is key+value.
                std.debug.assert(kind == .@"key+value");

                // ii. Let result be CreateArrayFromList(« indexNumber, elementValue »).
                break :blk Value.from(
                    try createArrayFromList(agent, &.{ index_number, element_value }),
                );
            }
        };

        // 15. Return CreateIteratorResultObject(result, false).
        return Value.from(try createIteratorResultObject(agent, result, false));
    }
};

/// 23.1.5.2.3 Properties of Array Iterator Instances
/// https://tc39.es/ecma262/#sec-properties-of-array-iterator-instances
pub const ArrayIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            /// [[IteratedArrayLike]]
            iterated_array_like: *Object,

            /// [[ArrayLikeNextIndex]]
            array_like_next_index: u53,

            /// [[ArrayLikeIterationKind]]
            array_like_iteration_kind: Object.PropertyKind,
        },
        completed,
    },
    .tag = .array_iterator,
});
