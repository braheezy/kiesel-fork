//! 22.2.9 RegExp String Iterator Objects
//! https://tc39.es/ecma262/#sec-regexp-string-iterator-objects

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
const String = types.String;
const Value = types.Value;
const advanceStringIndex = builtins.advanceStringIndex;
const createIteratorResultObject = types.createIteratorResultObject;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const regExpExec = builtins.regExpExec;

/// 22.2.9.1 CreateRegExpStringIterator ( R, S, global, fullUnicode )
/// https://tc39.es/ecma262/#sec-createregexpstringiterator
pub fn createRegExpStringIterator(
    agent: *Agent,
    reg_exp: *Object,
    string: *const String,
    global: bool,
    full_unicode: bool,
) std.mem.Allocator.Error!*RegExpStringIterator {
    const realm = agent.currentRealm();

    // 1. Let iterator be OrdinaryObjectCreate(%RegExpStringIteratorPrototype%, «
    //    [[IteratingRegExp]], [[IteratedString]], [[Global]], [[Unicode]], [[Done]] »).
    const iterator = try RegExpStringIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%RegExpStringIteratorPrototype%"(),
        .fields = .{
            .state = .{
                // 2. Set iterator.[[IteratingRegExp]] to R.
                .iterating_reg_exp = reg_exp,

                // 3. Set iterator.[[IteratedString]] to S.
                .iterated_string = string,

                // 4. Set iterator.[[Global]] to global.
                .global = global,

                // 5. Set iterator.[[Unicode]] to fullUnicode.
                .unicode = full_unicode,

                // 6. Set iterator.[[Done]] to false.
            },
        },
    });

    // 7. Return iterator.
    return iterator;
}

/// 22.2.9.2 The %RegExpStringIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Iterator.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "next", next, 0, realm);

        // 22.2.9.2.2 %RegExpStringIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("RegExp String Iterator"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 22.2.9.2.1 %RegExpStringIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        // 3. If O does not have all of the internal slots of a RegExp String Iterator Object
        //    Instance (see 22.2.9.2.3), throw a TypeError exception.
        const reg_exp_string_iterator = try this_value.requireInternalSlot(agent, RegExpStringIterator);

        // 4. If O.[[Done]] is true, then
        if (reg_exp_string_iterator.fields == .completed) {
            // a. Return CreateIteratorResultObject(undefined, true).
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        // 5. Let R be O.[[IteratingRegExp]].
        const reg_exp = reg_exp_string_iterator.fields.state.iterating_reg_exp;

        // 6. Let S be O.[[IteratedString]].
        const string = reg_exp_string_iterator.fields.state.iterated_string;

        // 7. Let global be O.[[Global]].
        const global = reg_exp_string_iterator.fields.state.global;

        // 8. Let fullUnicode be O.[[Unicode]].
        const full_unicode = reg_exp_string_iterator.fields.state.unicode;

        // 9. Let match be ? RegExpExec(R, S).
        const match = try regExpExec(agent, reg_exp, string) orelse {
            // 10. If match is null, then

            // a. Set O.[[Done]] to true.
            reg_exp_string_iterator.fields = .completed;

            // b. Return CreateIteratorResultObject(undefined, true).
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        };

        // 11. If global is false, then
        if (!global) {
            // a. Set O.[[Done]] to true.
            reg_exp_string_iterator.fields = .completed;

            // b. Return CreateIteratorResultObject(match, false).
            return Value.from(try createIteratorResultObject(agent, Value.from(match), false));
        }

        // 12. Let matchStr be ? ToString(? Get(match, "0")).
        const match_str = try (try match.get(agent, PropertyKey.from(0))).toString(agent);

        // 13. If matchStr is the empty String, then
        if (match_str.isEmpty()) {
            // a. Let thisIndex be ℝ(? ToLength(? Get(R, "lastIndex"))).
            const this_index = try (try reg_exp.get(agent, PropertyKey.from("lastIndex"))).toLength(agent);

            // b. Let nextIndex be AdvanceStringIndex(S, thisIndex, fullUnicode).
            const next_index = advanceStringIndex(string, this_index, full_unicode);

            // c. Perform ? Set(R, "lastIndex", 𝔽(nextIndex), true).
            try reg_exp.set(agent, PropertyKey.from("lastIndex"), Value.from(next_index), .throw);
        }

        // 14. Return CreateIteratorResultObject(match, false).
        return Value.from(try createIteratorResultObject(agent, Value.from(match), false));
    }
};

/// 22.2.9.2.3 Properties of RegExp String Iterator Instances
/// https://tc39.es/ecma262/#sec-properties-of-regexp-string-iterator-instances
pub const RegExpStringIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            /// [[IteratingRegExp]]
            iterating_reg_exp: *Object,

            /// [[IteratedString]]
            iterated_string: *const String,

            /// [[Global]]
            global: bool,

            /// [[Unicode]]
            unicode: bool,
        },
        completed,
    },
    .tag = .reg_exp_string_iterator,
});
