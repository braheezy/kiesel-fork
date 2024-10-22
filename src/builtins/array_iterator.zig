//! 23.1.5 Array Iterator Objects
//! https://tc39.es/ecma262/#sec-array-iterator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const isTypedArrayOutOfBounds = builtins.isTypedArrayOutOfBounds;
const makeTypedArrayWithBufferWitnessRecord = builtins.makeTypedArrayWithBufferWitnessRecord;
const typedArrayLength = builtins.typedArrayLength;

/// 23.1.5.1 CreateArrayIterator ( array, kind )
/// https://tc39.es/ecma262/#sec-createarrayiterator
pub fn createArrayIterator(
    agent: *Agent,
    array: Object,
    comptime kind: Object.PropertyKind,
) std.mem.Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let closure be a new Abstract Closure with no parameters that captures kind and array and
    //    performs the following steps when called:
    //    [...]
    // 2. Return CreateIteratorFromClosure(closure, "%ArrayIteratorPrototype%", %ArrayIteratorPrototype%).
    return ArrayIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%ArrayIteratorPrototype%"(),
        .fields = .{
            .state = .{ .array = array, .kind = kind, .index = 0 },
        },
    });
}

/// 23.1.5.2 The %ArrayIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 23.1.5.2.2 %ArrayIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Array Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 23.1.5.2.1 %ArrayIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%arrayiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, empty, "%ArrayIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateArrayIterator closure. State is kept track of through the ArrayIterator
        //       instance instead of as local variables. This should not be observable.

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (!this_value.isObject() or !this_value.asObject().is(ArrayIterator)) {
            return agent.throwException(.type_error, "This value must be an Array Iterator", .{});
        }
        const array_iterator = this_value.asObject().as(ArrayIterator);

        // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
        if (array_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        const array = array_iterator.fields.state.array;
        const kind = array_iterator.fields.state.kind;
        const index = array_iterator.fields.state.index;

        // i. If array has a [[TypedArrayName]] internal slot, then
        const len = if (array.is(builtins.TypedArray)) blk: {
            // 1. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(array, seq-cst).
            const ta = makeTypedArrayWithBufferWitnessRecord(
                array.as(builtins.TypedArray),
                .seq_cst,
            );

            // 2. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
            if (isTypedArrayOutOfBounds(ta)) {
                return agent.throwException(.type_error, "Typed array is out of bounds", .{});
            }

            // 3. Let len be TypedArrayLength(taRecord).
            break :blk typedArrayLength(ta);
        }
        // ii. Else,
        else blk: {
            // 1. Let len be ? LengthOfArrayLike(array).
            break :blk try array.lengthOfArrayLike();
        };

        // iii. If index ≥ len, return NormalCompletion(undefined).
        if (index >= len) {
            array_iterator.fields = .completed;
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
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
        array_iterator.fields.state.index += 1;

        // vii. Perform ? GeneratorYield(CreateIteratorResultObject(result, false)).
        return Value.from(try createIteratorResultObject(agent, result, false));
    }
};

pub const ArrayIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            array: Object,
            kind: Object.PropertyKind,
            index: u53,
        },
        completed,
    },
    .tag = .array_iterator,
});
