//! 22.1.5 String Iterator Objects
//! https://tc39.es/ecma262/#sec-string-iterator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
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
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 22.1.5.1.2 %StringIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%stringiteratorprototype%-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("String Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 22.1.5.1.1 %StringIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%stringiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
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

        const string = string_iterator.fields.state.string;
        const position = string_iterator.fields.state.position;

        // a. Let len be the length of s.
        const len = string.length();

        // b. Let position be 0.
        // c. Repeat, while position < len,
        if (position < len) {
            // i. Let cp be CodePointAt(s, position).
            const code_point = string.codePointAt(position);

            // ii. Let nextIndex be position + cp.[[CodeUnitCount]].
            const next_index = position + code_point.code_unit_count;

            // iii. Let resultString be the substring of s from position to nextIndex.
            const result_string = try string.substring(agent.gc_allocator, position, next_index);

            // iv. Set position to nextIndex.
            string_iterator.fields.state.position = next_index;

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
            string: String,
            position: usize,
        },
        completed,
    },
    .tag = .string_iterator,
});
