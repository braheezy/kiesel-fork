//! 22.1.5 String Iterator Objects
//! https://tc39.es/ecma262/#sec-string-iterator-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createIteratorResultObject = types.createIteratorResultObject;

/// 22.1.5.1 The %StringIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%stringiteratorprototype%-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "next", next, 0, realm);

        // 22.1.5.1.2 %StringIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%stringiteratorprototype%-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("String Iterator"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 22.1.5.1.1 %StringIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%stringiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, empty, "%StringIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateStringIterator closure. State is kept track of through the ArrayIterator
        //       instance instead of as local variables. This should not be observable.

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (!this_value.isObject() or !this_value.asObject().is(StringIterator)) {
            return agent.throwException(.type_error, "This value must be an Array Iterator", .{});
        }
        const string_iterator = this_value.asObject().as(StringIterator);

        // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
        if (string_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
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
            const result_string = try string.substring(agent, position, next_index);

            // iv. Set position to nextIndex.
            string_iterator.fields.state.position = next_index;

            // v. Perform ? GeneratorYield(CreateIteratorResultObject(resultString, false)).
            return Value.from(try createIteratorResultObject(agent, Value.from(result_string), false));
        }

        // d. Return undefined.
        string_iterator.fields = .completed;
        return Value.from(try createIteratorResultObject(agent, .undefined, true));
    }
};

pub const StringIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            string: *const String,
            position: usize,
        },
        completed,
    },
    .tag = .string_iterator,
});
