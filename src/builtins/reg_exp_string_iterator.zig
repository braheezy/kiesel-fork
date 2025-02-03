//! 22.2.9 RegExp String Iterator Objects
//! https://tc39.es/ecma262/#sec-regexp-string-iterator-objects

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
const String = types.String;
const Value = types.Value;
const advanceStringIndex = builtins.advanceStringIndex;
const createIteratorResultObject = types.createIteratorResultObject;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const regExpExec = builtins.regExpExec;

/// 22.2.9.1 CreateRegExpStringIterator ( R, S, global, fullUnicode )
/// https://tc39.es/ecma262/#sec-createregexpstringiterator
pub fn createRegExpStringIterator(
    agent: *Agent,
    reg_exp: *Object,
    string: *const String,
    global: bool,
    full_unicode: bool,
) std.mem.Allocator.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Let closure be a new Abstract Closure with no parameters that captures R, S, global, and
    //    fullUnicode and performs the following steps when called:
    //    [...]
    // 2. Return CreateIteratorFromClosure(closure, "%RegExpStringIteratorPrototype%",
    //    %RegExpStringIteratorPrototype%).
    return RegExpStringIterator.create(agent, .{
        .prototype = try realm.intrinsics.@"%RegExpStringIteratorPrototype%"(),
        .fields = .{
            .state = .{
                .reg_exp = reg_exp,
                .string = string,
                .global = global,
                .full_unicode = full_unicode,
            },
        },
    });
}

/// 22.2.9.2 The %RegExpStringIteratorPrototype% Object
/// https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Iterator.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "next", next, 0, realm);

        // 22.2.9.2.2 %RegExpStringIteratorPrototype% [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%-%symbol.tostringtag%
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("RegExp String Iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }

    /// 22.2.9.2.1 %RegExpStringIteratorPrototype%.next ( )
    /// https://tc39.es/ecma262/#sec-%regexpstringiteratorprototype%.next
    fn next(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? GeneratorResume(this value, empty, "%RegExpStringIteratorPrototype%").
        // NOTE: In the absence of generators this implements one loop iteration of the
        //       CreateRegExpStringIterator closure. State is kept track of through the
        //       RegExpStringIterator instance instead of as local variables. This should not be
        //       observable.

        // 1. Let state be ? GeneratorValidate(generator, generatorBrand).
        if (!this_value.isObject() or !this_value.asObject().is(RegExpStringIterator)) {
            return agent.throwException(
                .type_error,
                "This value must be a RegExp String Iterator",
                .{},
            );
        }
        const reg_exp_string_iterator = this_value.asObject().as(RegExpStringIterator);

        // 2. If state is completed, return CreateIteratorResultObject(undefined, true).
        if (reg_exp_string_iterator.fields == .completed) {
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        }

        const reg_exp = reg_exp_string_iterator.fields.state.reg_exp;
        const string = reg_exp_string_iterator.fields.state.string;
        const global = reg_exp_string_iterator.fields.state.global;
        const full_unicode = reg_exp_string_iterator.fields.state.full_unicode;

        // i. Let match be ? RegExpExec(R, S).
        const match = try regExpExec(agent, reg_exp, string) orelse {
            // ii. If match is null, return undefined.
            reg_exp_string_iterator.fields = .completed;
            return Value.from(try createIteratorResultObject(agent, .undefined, true));
        };

        // iii. If global is false, then
        if (!global) {
            // 1. Perform ? GeneratorYield(CreateIteratorResultObject(match, false)).
            // 2. Return undefined.
            reg_exp_string_iterator.fields = .completed;
            return Value.from(try createIteratorResultObject(agent, Value.from(match), false));
        }

        // iv. Let matchStr be ? ToString(? Get(match, "0")).
        const match_str = try (try match.get(PropertyKey.from(0))).toString(agent);

        // v. If matchStr is the empty String, then
        if (match_str.isEmpty()) {
            // 1. Let thisIndex be ℝ(? ToLength(? Get(R, "lastIndex"))).
            const this_index = try (try reg_exp.get(PropertyKey.from("lastIndex"))).toLength(agent);

            // 2. Let nextIndex be AdvanceStringIndex(S, thisIndex, fullUnicode).
            const next_index = advanceStringIndex(string, this_index, full_unicode);

            // 3. Perform ? Set(R, "lastIndex", 𝔽(nextIndex), true).
            try reg_exp.set(PropertyKey.from("lastIndex"), Value.from(next_index), .throw);
        }

        // vi. Perform ? GeneratorYield(CreateIteratorResultObject(match, false)).
        return Value.from(try createIteratorResultObject(agent, Value.from(match), false));
    }
};

pub const RegExpStringIterator = MakeObject(.{
    .Fields = union(enum) {
        state: struct {
            reg_exp: *Object,
            string: *const String,
            global: bool,
            full_unicode: bool,
        },
        completed,
    },
    .tag = .reg_exp_string_iterator,
});
