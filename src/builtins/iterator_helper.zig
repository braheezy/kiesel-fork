//! 27.1.2 Iterator Helper Objects
//! https://tc39.es/ecma262/#sec-iterator-helper-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Iterator = types.Iterator;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 27.1.2.1 The %IteratorHelperPrototype% Object
/// https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);
        try defineBuiltinFunction(object, "return", @"return", 0, realm);

        // 27.1.2.1.3 %IteratorHelperPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Iterator Helper"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 27.1.2.1.1 %IteratorHelperPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%iteratorhelperprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, undefined, "Iterator Helper").

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (!this_value.isObject() or !this_value.asObject().is(IteratorHelper)) {
            return agent.throwException(.type_error, "This value must be an Iterator Helper", .{});
        }
        const iterator_helper = this_value.asObject().as(IteratorHelper);

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
        // 2. Perform ? RequireInternalSlot(O, [[UnderlyingIterator]]).
        // 3. Assert: O has a [[GeneratorState]] internal slot.
        const object = try this_value.requireInternalSlot(agent, IteratorHelper);

        // 4. If O.[[GeneratorState]] is suspended-start, then
        if (object.fields != .completed) {
            const underlying_iterator = object.fields.state.underlying_iterator;

            // a. Set O.[[GeneratorState]] to completed.
            object.fields = .completed;

            // b. NOTE: Once a generator enters the completed state it never leaves it and its
            //    associated execution context is never resumed. Any execution state associated
            //    with O can be discarded at this point.

            // c. Perform ? IteratorClose(O.[[UnderlyingIterator]], NormalCompletion(unused)).
            _ = try underlying_iterator.close(@as(Agent.Error!void, {}));

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
            /// [[UnderlyingIterator]]
            underlying_iterator: Iterator,

            closure: *const fn (*Agent, *IteratorHelper) Agent.Error!?Value,
            captures: SafePointer = .null_pointer,
            executing: bool = false,
        },
        completed,
    },
    .tag = .iterator_helper,
});
