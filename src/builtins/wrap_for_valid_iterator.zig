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
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;

/// 27.1.3.2.1.1 The %WrapForValidIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);
        try defineBuiltinFunction(object, "return", @"return", 0, realm);
    }

    /// 22.1.5.1.1 %StringIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%stringiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be this value.
        // 2. Perform ? RequireInternalSlot(O, [[Iterated]]).
        const object = try this_value.requireInternalSlot(agent, WrapForValidIterator);

        // 3. Let iteratorRecord be O.[[Iterated]].
        const iterator = object.fields.iterated;

        // 4. Return ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
        return iterator.next_method.callNoArgs(agent, Value.from(iterator.iterator));
    }

    /// 27.1.3.2.1.1.2 %WrapForValidIteratorPrototype%.return ( )
    /// https://tc39.es/ecma262/#sec-%wrapforvaliditeratorprototype%.return
    fn @"return"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be this value.
        // 2. Perform ? RequireInternalSlot(O, [[Iterated]]).
        const object = try this_value.requireInternalSlot(agent, WrapForValidIterator);

        // 3. Let iterator be O.[[Iterated]].[[Iterator]].
        // 4. Assert: iterator is an Object.
        const iterator = object.fields.iterated.iterator;

        // 5. Let returnMethod be ? GetMethod(iterator, "return").
        const return_method = try Value.from(iterator).getMethod(
            agent,
            PropertyKey.from("return"),
        ) orelse {
            // 6. If returnMethod is undefined, then
            //     a. Return CreateIteratorResultObject(undefined, true).
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        };

        // 7. Return ? Call(returnMethod, iterator).
        return Value.from(return_method).callAssumeCallableNoArgs(Value.from(iterator));
    }
};

pub const WrapForValidIterator = MakeObject(.{
    .Fields = struct {
        /// [[Iterated]]
        iterated: Iterator,
    },
    .tag = .wrap_for_valid_iterator,
});
