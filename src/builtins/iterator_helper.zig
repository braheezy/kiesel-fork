//! 27.1.2 Iterator Helper Objects
//! https://tc39.es/ecma262/#sec-iterator-helper-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Iterator = types.Iterator;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const createIteratorResultObject = types.createIteratorResultObject;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 27.1.2.1 The %IteratorHelperPrototype% Object
/// https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Iterator.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "next", next, 0, realm);
        try object.defineBuiltinFunction(agent, "return", @"return", 0, realm);

        // 27.1.2.1.3 %IteratorHelperPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Iterator Helper"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 27.1.2.1.1 %IteratorHelperPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%iteratorhelperprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, undefined, "Iterator Helper").

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        const iterator_helper = try this_value.requireInternalSlot(agent, IteratorHelper);

        // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
        if (iterator_helper.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        if (iterator_helper.fields.state.executing) {
            return agent.throwException(.type_error, "Generator is already executing", .{});
        }

        iterator_helper.fields.state.executing = true;
        defer if (iterator_helper.fields != .completed) {
            iterator_helper.fields.state.executing = false;
        };

        const value = try iterator_helper.fields.state.closure(agent, iterator_helper) orelse {
            iterator_helper.fields = .completed;
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        };

        return Value.from(try createIteratorResultObject(agent, value, false));
    }

    /// 27.1.2.1.2 %IteratorHelperPrototype%.return ( )
    /// https://tc39.es/ecma262/#sec-%iteratorhelperprototype%.return
    fn @"return"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be this value.
        // 2. Perform ? RequireInternalSlot(O, [[UnderlyingIterators]]).
        // 3. Assert: O has a [[GeneratorState]] internal slot.
        const object = try this_value.requireInternalSlot(agent, IteratorHelper);

        // 4. If O.[[GeneratorState]] is suspended-start, then
        if (object.fields != .completed) {
            const underlying_iterators = object.fields.state.underlying_iterators;

            // a. Set O.[[GeneratorState]] to completed.
            object.fields = .completed;

            // b. NOTE: Once a generator enters the completed state it never leaves it and its
            //    associated execution context is never resumed. Any execution state associated
            //    with O can be discarded at this point.

            // c. Perform ? IteratorClose(O.[[UnderlyingIterators]], NormalCompletion(unused)).
            _ = try Iterator.closeAll(agent, underlying_iterators, @as(Agent.Error!void, {}));

            // d. Return CreateIteratorResultObject(undefined, true).
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        // 5. Let C be ReturnCompletion(undefined).
        // 6. Return ? GeneratorResumeAbrupt(O, C, "Iterator Helper").
        return Value.from(try createIteratorResultObject(agent, .undefined, true));
    }
};

pub const IteratorHelper = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            /// [[UnderlyingIterators]]
            underlying_iterators: []Iterator,

            closure: *const fn (*Agent, *IteratorHelper) Agent.Error!?Value,
            captures: SafePointer = .null_pointer,
            executing: bool = false,
        },
        completed,
    },
    .tag = .iterator_helper,
    .display_name = "Iterator Helper",
});
