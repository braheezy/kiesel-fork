//! 22.1.5 String Iterator Objects
//! https://tc39.es/ecma262/#sec-string-iterator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 22.1.5.1 The %StringIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%stringiteratorprototype%-object
pub const StringIteratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });

        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 22.1.5.1.2 %StringIteratorPrototype% [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-%stringiteratorprototype%-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("String Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// 22.1.5.1.1 %StringIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%stringiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: ArgumentsList) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, empty, "%StringIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateArrayIterator closure. State is kept track of through the ArrayIterator
        //       instance instead of as local variables. This should not be observable.

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (this_value != .object or !this_value.object.is(StringIterator)) {
            return agent.throwException(.type_error, "This value must be an Array Iterator", .{});
        }
        const string_iterator = this_value.object.as(StringIterator);

        // 2. If state is completed, return CreateIterResultObject(undefined, true).
        if (string_iterator.fields == .completed) {
            return Value.from(try createIterResultObject(agent, .undefined, true));
        }

        var it = &string_iterator.fields.state.it;

        // a. Let len be the length of s.
        // b. Let position be 0.
        // c. Repeat, while position < len,
        if (it.nextCodepoint() catch unreachable) |code_point| {
            // i. Let cp be CodePointAt(s, position).
            // ii. Let nextIndex be position + cp.[[CodeUnitCount]].
            // iii. Let resultString be the substring of s from position to nextIndex.
            // iv. Set position to nextIndex.
            // TODO: This is a bit messy, especially the `catch unreachable`s. UTF-8/UTF-16 interop
            //       in the String type needs some love :)
            const byte_length = std.unicode.utf8CodepointSequenceLength(code_point) catch unreachable;
            const utf8 = try agent.gc_allocator.alloc(u8, byte_length);
            _ = std.unicode.utf8Encode(code_point, utf8) catch unreachable;
            const result_string = String.from(utf8);

            // v. Perform ? GeneratorYield(CreateIterResultObject(resultString, false)).
            return Value.from(try createIterResultObject(agent, Value.from(result_string), false));
        }

        // d. Return undefined.
        string_iterator.fields = .completed;
        return Value.from(try createIterResultObject(agent, .undefined, true));
    }
};

pub const StringIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            it: std.unicode.Utf16LeIterator,
        },
        completed,
    },
    .tag = .string_iterator,
});
