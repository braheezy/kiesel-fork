//! 27.1.3 Iterator Objects
//! https://tc39.es/ecma262/#sec-iterator-objects

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
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getIterator = types.getIterator;
const getIteratorDirect = types.getIteratorDirect;
const getIteratorFlattenable = types.getIteratorFlattenable;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 27.1.3.1 The Iterator Constructor
/// https://tc39.es/ecma262/#sec-iterator-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Iterator",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "concat", concat, 0, realm);
        try object.defineBuiltinFunction(agent, "from", from, 1, realm);
        try object.defineBuiltinFunction(agent, "zip", zip, 1, realm);
        try object.defineBuiltinFunction(agent, "zipKeyed", zipKeyed, 1, realm);

        // 27.1.3.2.2 Iterator.prototype
        // https://tc39.es/ecma262/#sec-iterator.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Iterator.prototype%"()),
            .none,
        );
    }

    /// 27.1.3.1.1 Iterator ( )
    /// https://tc39.es/ecma262/#sec-iterator
    fn impl(agent: *Agent, _: Arguments, new_target: ?*Object) Agent.Error!Value {
        // 1. If NewTarget is either undefined or the active function object, throw a TypeError
        //    exception.
        if (new_target == null or new_target.? == agent.activeFunctionObject()) {
            return agent.throwException(
                .type_error,
                "Iterator must not be constructed directly",
                .{},
            );
        }

        // 2. Return ? OrdinaryCreateFromConstructor(NewTarget, "%Iterator.prototype%").
        const iterator = try ordinaryCreateFromConstructor(
            Iterator,
            agent,
            new_target.?,
            "%Iterator.prototype%",
            {},
        );
        return Value.from(&iterator.object);
    }

    /// 27.1.3.2.1 Iterator.concat ( ...items )
    /// https://tc39.es/ecma262/#sec-iterator.concat
    fn concat(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();

        const Iterable = struct {
            /// [[OpenMethod]]
            open_method: *Object,

            /// [[Iterable]]
            iterable: *Object,
        };

        // 1. Let iterables be a new empty List.
        const iterables = try agent.gc_allocator.alloc(Iterable, arguments.count());
        errdefer agent.gc_allocator.free(iterables);

        // 2. For each element item of items, do
        for (arguments.values, 0..) |item, i| {
            // a. If item is not an Object, throw a TypeError exception.
            if (!item.isObject()) {
                return agent.throwException(.type_error, "{f} is not an Object", .{item});
            }

            // b. Let method be ? GetMethod(item, %Symbol.iterator%).
            const method = try item.getMethod(
                agent,
                PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
            ) orelse {
                // c. If method is undefined, throw a TypeError exception.
                return agent.throwException(
                    .type_error,
                    "Object has no Symbol.iterator method",
                    .{},
                );
            };

            // d. Append the Record { [[OpenMethod]]: method, [[Iterable]]: item } to iterables.
            iterables[i] = .{ .open_method = method, .iterable = item.asObject() };
        }

        const Captures = struct {
            iterables: []const Iterable,
            index: usize,
            inner_iterator: ?types.Iterator,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iterables = iterables,
            .index = 0,
            .inner_iterator = null,
        };

        // 3. Let closure be a new Abstract Closure with no parameters that captures iterables and
        //    performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iterables_ = captures_.iterables;
                const index = &captures_.index;
                const inner_iterator_ = &captures_.inner_iterator;

                const State = enum { outer, inner };
                const state: State = if (inner_iterator_.* == null)
                    .outer
                else
                    .inner;

                // a. For each Record iterable of iterables, do
                loop: switch (state) {
                    .outer => {
                        if (index.* >= iterables_.len) return null;
                        const iterable = iterables_[index.*];
                        index.* += 1;

                        // i. Let iter be ? Call(iterable.[[OpenMethod]], iterable.[[Iterable]]).
                        const iter = try Value.from(iterable.open_method).callAssumeCallable(
                            agent_,
                            Value.from(iterable.iterable),
                            &.{},
                        );

                        // ii. If iter is not an Object, throw a TypeError exception.
                        if (!iter.isObject()) {
                            return agent_.throwException(.type_error, "{f} is not an Object", .{iter});
                        }

                        // iii. Let iteratorRecord be ? GetIteratorDirect(iter).
                        inner_iterator_.* = try getIteratorDirect(agent_, iter.asObject());

                        continue :loop .inner;
                    },
                    .inner => {
                        // iv. Let innerAlive be true.
                        // v. Repeat, while innerAlive is true,
                        //     1. Let innerValue be ? IteratorStepValue(iteratorRecord).
                        //     2. If innerValue is done, then
                        //         a. Set innerAlive to false.
                        //     3. Else,
                        //         a. Let completion be Completion(Yield(innerValue)).
                        //         b. If completion is an abrupt completion, then
                        //             i. Return ? IteratorClose(iteratorRecord, completion).
                        if (try inner_iterator_.*.?.stepValue(agent_)) |value| return value;
                        inner_iterator_.* = null;
                        continue :loop .outer;
                    },
                }

                // b. Return ReturnCompletion(undefined).
            }
        }.func;

        const abruptClosure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!void {
                // 3.a.v.3.b.
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                if (captures_.inner_iterator) |inner_iterator| {
                    _ = try inner_iterator.close(agent_, @as(Agent.Error!void, {}));
                }
            }
        }.func;

        // 4. Let gen be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const gen = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 5. Set gen.[[UnderlyingIterators]] to a new empty List.
                    .underlying_iterators = &.{},

                    .closure = closure,
                    .abruptClosure = abruptClosure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 6. Return gen.
        return Value.from(&gen.object);
    }

    /// 27.1.3.2.1 Iterator.from ( O )
    /// https://tc39.es/ecma262/#sec-iterator.from
    fn from(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const object = arguments.get(0);

        // 1. Let iteratorRecord be ? GetIteratorFlattenable(O, iterate-string-primitives).
        const iterator = try getIteratorFlattenable(agent, object, .iterate_string_primitives);

        // 2. Let hasInstance be ? OrdinaryHasInstance(%Iterator%, iteratorRecord.[[Iterator]]).
        const has_instance = try Value.from(
            try realm.intrinsics.@"%Iterator%"(),
        ).ordinaryHasInstance(agent, Value.from(iterator.iterator));

        // 3. If hasInstance is true, then
        if (has_instance) {
            // a. Return iteratorRecord.[[Iterator]].
            return Value.from(iterator.iterator);
        }

        // 4. Let wrapper be OrdinaryObjectCreate(%WrapForValidIteratorPrototype%, « [[Iterated]] »).
        const wrapper = try builtins.WrapForValidIterator.create(agent, .{
            .prototype = try realm.intrinsics.@"%WrapForValidIteratorPrototype%"(),
            .fields = .{
                // 5. Set wrapper.[[Iterated]] to iteratorRecord.
                .iterated = iterator,
            },
        });

        // 6. Return wrapper.
        return Value.from(&wrapper.object);
    }

    /// 1 Iterator.zip ( iterables [ , options ] )
    /// https://tc39.es/proposal-joint-iteration/#sec-iterator.zip
    fn zip(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const iterables = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If iterables is not an Object, throw a TypeError exception.
        if (!iterables.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{iterables});
        }

        // 2. Set options to ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 3. Let mode be ? Get(options, "mode").
        var mode_value = try options.get(agent, PropertyKey.from("mode"));

        // 4. If mode is undefined, set mode to "shortest".
        // 5. If mode is not one of "shortest", "longest", or "strict", throw a TypeError
        //    exception.
        const mode: ZipMode = blk: {
            if (mode_value.isUndefined()) break :blk .shortest;
            if (mode_value.isString()) {
                if (mode_value.asString().eql(String.fromLiteral("shortest"))) break :blk .shortest;
                if (mode_value.asString().eql(String.fromLiteral("longest"))) break :blk .longest;
                if (mode_value.asString().eql(String.fromLiteral("strict"))) break :blk .strict;
            }
            return agent.throwException(.type_error, "Invalid mode {f}", .{mode_value});
        };

        // 6. Let paddingOption be undefined.
        var padding_option: ?*Object = null;

        // 7. If mode is "longest", then
        if (mode == .longest) {
            // a. Set paddingOption to ? Get(options, "padding").
            const padding_option_value = try options.get(agent, PropertyKey.from("padding"));

            // b. If paddingOption is not undefined and paddingOption is not an Object, throw a
            //    TypeError exception.
            if (!padding_option_value.isUndefined() and !padding_option_value.isObject()) {
                return agent.throwException(
                    .type_error,
                    "{f} is not an Object",
                    .{padding_option_value},
                );
            }
            if (padding_option_value.isObject()) {
                padding_option = padding_option_value.asObject();
            }
        }

        // 8. Let iters be a new empty List.
        var iters: std.ArrayList(types.Iterator) = .empty;
        defer iters.deinit(agent.gc_allocator);

        // 9. Let padding be a new empty List.
        var padding: std.ArrayList(Value) = .empty;
        defer padding.deinit(agent.gc_allocator);

        // 10. Let inputIter be ? GetIterator(iterables, SYNC).
        var input_iter = try getIterator(agent, iterables, .sync);

        // 11. Let next be NOT-STARTED.
        // 12. Repeat, while next is not DONE,
        while (true) {
            // a. Set next to Completion(IteratorStepValue(inputIter)).
            const next = input_iter.stepValue(agent) catch |err| {
                // b. IfAbruptCloseIterators(next, iters).
                return types.Iterator.closeAll(agent, iters.items, @as(Agent.Error!Value, err));
            } orelse break;

            // c. If next is not DONE, then
            // i. Let iter be Completion(GetIteratorFlattenable(next, REJECT-PRIMITIVES)).
            const iter = getIteratorFlattenable(agent, next, .reject_primitives) catch |err| {
                // ii. IfAbruptCloseIterators(iter, the list-concatenation of « inputIter » and iters).
                var all_iters: std.ArrayList(types.Iterator) = .empty;
                defer all_iters.deinit(agent.gc_allocator);
                try all_iters.append(agent.gc_allocator, input_iter);
                try all_iters.appendSlice(agent.gc_allocator, iters.items);
                return types.Iterator.closeAll(
                    agent,
                    all_iters.items,
                    @as(Agent.Error!Value, err),
                );
            };

            // iii. Append iter to iters.
            try iters.append(agent.gc_allocator, iter);
        }

        // 13. Let iterCount be the number of elements in iters.
        const iter_count = iters.items.len;

        // 14. If mode is "longest", then
        if (mode == .longest) {
            // a. If paddingOption is undefined, then
            if (padding_option == null) {
                // i. Perform the following steps iterCount times:
                for (0..iter_count) |_| {
                    // 1. Append undefined to padding.
                    try padding.append(agent.gc_allocator, .undefined);
                }
            } else {
                // b. Else,
                // i. Let paddingIter be Completion(GetIterator(paddingOption, SYNC)).
                var padding_iter = getIterator(
                    agent,
                    Value.from(padding_option.?),
                    .sync,
                ) catch |err| {
                    // ii. IfAbruptCloseIterators(paddingIter, iters).
                    return types.Iterator.closeAll(
                        agent,
                        iters.items,
                        @as(Agent.Error!Value, err),
                    );
                };

                // iii. Let usingIterator be true.
                var using_iterator = true;

                // iv. Perform the following steps iterCount times:
                for (0..iter_count) |_| {
                    // 1. If usingIterator is true, then
                    if (using_iterator) {
                        // a. Set next to Completion(IteratorStepValue(paddingIter)).
                        const maybe_next = padding_iter.stepValue(agent) catch |err| {
                            // b. IfAbruptCloseIterators(next, iters).
                            return types.Iterator.closeAll(
                                agent,
                                iters.items,
                                @as(Agent.Error!Value, err),
                            );
                        };

                        // c. If next is done, then
                        //     i. Set usingIterator to false.
                        // d. Else,
                        //     i. Append next to padding.
                        if (maybe_next) |next| {
                            try padding.append(agent.gc_allocator, next);
                        } else {
                            using_iterator = false;
                        }
                    }

                    // 2. If usingIterator is false, append undefined to padding.
                    if (!using_iterator) {
                        try padding.append(agent.gc_allocator, .undefined);
                    }
                }

                // v. If usingIterator is true, then
                if (using_iterator) {
                    // 1. Let completion be Completion(IteratorClose(paddingIter, NormalCompletion(UNUSED))).
                    padding_iter.close(agent, @as(Agent.Error!void, {})) catch |err| {
                        // 2. IfAbruptCloseIterators(completion, iters).
                        return types.Iterator.closeAll(
                            agent,
                            iters.items,
                            @as(Agent.Error!Value, err),
                        );
                    };
                }
            }
        }

        // 15. Let finishResults be a new Abstract Closure with parameters (results) that captures
        //     nothing and performs the following steps when called:
        //     a. Return CreateArrayFromList(results).
        // 16. Return IteratorZip(iters, mode, padding, finishResults).
        return iteratorZip(
            agent,
            try iters.toOwnedSlice(agent.gc_allocator),
            mode,
            try padding.toOwnedSlice(agent.gc_allocator),
            .array,
        );
    }

    /// 2 Iterator.zipKeyed ( iterables [ , options ] )
    /// https://tc39.es/proposal-joint-iteration/#sec-iterator.zipkeyed
    fn zipKeyed(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const iterables_value = arguments.get(0);
        const options_value = arguments.get(1);

        // 1. If iterables is not an Object, throw a TypeError exception.
        if (!iterables_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{iterables_value});
        }
        const iterables = iterables_value.asObject();

        // 2. Set options to ? GetOptionsObject(options).
        const options = try options_value.getOptionsObject(agent);

        // 3. Let mode be ? Get(options, "mode").
        var mode_value = try options.get(agent, PropertyKey.from("mode"));

        // 4. If mode is undefined, set mode to "shortest".
        // 5. If mode is not one of "shortest", "longest", or "strict", throw a TypeError
        //    exception.
        const mode: ZipMode = blk: {
            if (mode_value.isUndefined()) break :blk .shortest;
            if (mode_value.isString()) {
                if (mode_value.asString().eql(String.fromLiteral("shortest"))) break :blk .shortest;
                if (mode_value.asString().eql(String.fromLiteral("longest"))) break :blk .longest;
                if (mode_value.asString().eql(String.fromLiteral("strict"))) break :blk .strict;
            }
            return agent.throwException(.type_error, "Invalid mode {f}", .{mode_value});
        };

        // 6. Let paddingOption be undefined.
        var padding_option: ?*Object = null;

        // 7. If mode is "longest", then
        if (mode == .longest) {
            // a. Set paddingOption to ? Get(options, "padding").
            const padding_option_value = try options.get(agent, PropertyKey.from("padding"));

            // b. If paddingOption is not undefined and paddingOption is not an Object, throw a
            //    TypeError exception.
            if (!padding_option_value.isUndefined() and !padding_option_value.isObject()) {
                return agent.throwException(.type_error, "padding must be an Object", .{});
            }
            if (padding_option_value.isObject()) {
                padding_option = padding_option_value.asObject();
            }
        }

        // 8. Let iters be a new empty List.
        var iters: std.ArrayList(types.Iterator) = .empty;
        defer iters.deinit(agent.gc_allocator);

        // 9. Let padding be a new empty List.
        var padding: std.ArrayList(Value) = .empty;
        defer padding.deinit(agent.gc_allocator);

        // 10. Let allKeys be ? iterables.[[OwnPropertyKeys]]().
        const all_keys = try iterables.internal_methods.ownPropertyKeys(
            agent,
            iterables,
        );
        defer agent.gc_allocator.free(all_keys);

        // 11. Let keys be a new empty List.
        var keys: std.ArrayList(PropertyKey) = .empty;
        defer keys.deinit(agent.gc_allocator);

        // 12. For each element key of allKeys, do
        for (all_keys) |key| {
            // a. Let desc be Completion(iterables.[[GetOwnProperty]](key)).
            const descriptor = iterables.internal_methods.getOwnProperty(
                agent,
                iterables,
                key,
            ) catch |err| {
                // b. IfAbruptCloseIterators(desc, iters).
                return types.Iterator.closeAll(agent, iters.items, @as(Agent.Error!Value, err));
            };

            // c. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // i. Let value be Completion(Get(iterables, key)).
                const value = iterables.get(agent, key) catch |err| {
                    // ii. IfAbruptCloseIterators(value, iters).
                    return types.Iterator.closeAll(
                        agent,
                        iters.items,
                        @as(Agent.Error!Value, err),
                    );
                };

                // iii. If value is not undefined, then
                if (!value.isUndefined()) {
                    // 1. Append key to keys.
                    try keys.append(agent.gc_allocator, key);

                    // 2. Let iter be Completion(GetIteratorFlattenable(value, REJECT-PRIMITIVES)).
                    const iter = getIteratorFlattenable(
                        agent,
                        value,
                        .reject_primitives,
                    ) catch |err| {
                        // 3. IfAbruptCloseIterators(iter, iters).
                        return types.Iterator.closeAll(
                            agent,
                            iters.items,
                            @as(Agent.Error!Value, err),
                        );
                    };

                    // 4. Append iter to iters.
                    try iters.append(agent.gc_allocator, iter);
                }
            }
        }

        // 13. Let iterCount be the number of elements in iters.
        const iter_count = iters.items.len;

        // 14. If mode is "longest", then
        if (mode == .longest) {
            // a. If paddingOption is undefined, then
            if (padding_option == null) {
                // i. Perform the following steps iterCount times:
                for (0..iter_count) |_| {
                    // 1. Append undefined to padding.
                    try padding.append(agent.gc_allocator, .undefined);
                }
            } else {
                // b. Else,
                // i. For each element key of keys, do
                for (keys.items) |key| {
                    // 1. Let value be Completion(Get(paddingOption, key)).
                    const value = padding_option.?.get(agent, key) catch |err| {
                        // 2. IfAbruptCloseIterators(value, iters).
                        return types.Iterator.closeAll(
                            agent,
                            iters.items,
                            @as(Agent.Error!Value, err),
                        );
                    };

                    // 3. Append value to padding.
                    try padding.append(agent.gc_allocator, value);
                }
            }
        }

        // 15. Let finishResults be a new Abstract Closure with parameters (results) that captures
        //     keys and iterCount and performs the following steps when called:
        //     a. Let obj be OrdinaryObjectCreate(null).
        //     b. For each integer i such that 0 ≤ i < iterCount, in ascending order, do
        //         i. Perform ! CreateDataPropertyOrThrow(obj, keys[i], results[i]).
        //     c. Return obj.
        // 16. Return IteratorZip(iters, mode, padding, finishResults).
        return iteratorZip(
            agent,
            try iters.toOwnedSlice(agent.gc_allocator),
            mode,
            try padding.toOwnedSlice(agent.gc_allocator),
            .{ .object = try keys.toOwnedSlice(agent.gc_allocator) },
        );
    }

    const ZipMode = enum { shortest, longest, strict };
    const FinishResults = union(enum) {
        array,
        object: []PropertyKey,
    };

    fn removeFromOpenIters(open_iters: *std.ArrayList(types.Iterator), iter: types.Iterator) void {
        for (open_iters.items, 0..) |open_iter, idx| {
            if (open_iter.iterator == iter.iterator) {
                _ = open_iters.orderedRemove(idx);
                break;
            }
        }
    }

    /// 3 IteratorZip ( iters, mode, padding, finishResults )
    /// https://tc39.es/proposal-joint-iteration/#sec-iteratorzip
    fn iteratorZip(
        agent: *Agent,
        iters: []types.Iterator,
        mode: ZipMode,
        padding: []Value,
        finish_results: FinishResults,
    ) Agent.Error!Value {
        const realm = agent.currentRealm();

        // 1. Let iterCount be the number of elements in iters.
        // 2. Let openIters be a copy of iters.
        var open_iters: std.ArrayList(types.Iterator) = .empty;
        try open_iters.appendSlice(agent.gc_allocator, iters);

        const Captures = struct {
            iters: []types.Iterator,
            open_iters: std.ArrayList(types.Iterator),
            mode: ZipMode,
            padding: []Value,
            finish_results: FinishResults,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iters = iters,
            .open_iters = open_iters,
            .mode = mode,
            .padding = padding,
            .finish_results = finish_results,
        };

        // 3. Let closure be a new Abstract Closure with no parameters that captures iters,
        //    iterCount, openIters, mode, padding, and finishResults, and performs the following
        //    steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iters_ = captures_.iters;
                const open_iters_ = &captures_.open_iters;
                const mode_ = captures_.mode;
                const padding_ = captures_.padding;
                const finish_results_ = captures_.finish_results;
                const iter_count = iters_.len;

                // a. If iterCount = 0, return ReturnCompletion(undefined).
                if (iter_count == 0) return null;

                // b. Repeat,
                // i. Let results be a new empty List.
                var results: std.ArrayList(Value) = .empty;
                defer results.deinit(agent_.gc_allocator);

                // ii. Assert: openIters is not empty.
                std.debug.assert(open_iters_.items.len > 0);

                // iii. For each integer i such that 0 ≤ i < iterCount, in ascending order, do
                for (0..iter_count) |i| {
                    var result: Value = undefined;

                    // 1. Let iter be iters[i].
                    var iter = iters_[i];

                    // 2. If iter is null, then
                    if (iter.done) {
                        // a. Assert: mode is "longest".
                        std.debug.assert(mode_ == .longest);

                        // b. Let result be padding[i].
                        result = padding_[i];
                    } else {
                        // 3. Else,
                        // a. Let result be Completion(IteratorStepValue(iter)).
                        const maybe_result = iter.stepValue(agent_) catch |err| {
                            // b. If result is an abrupt completion, then
                            // i. Remove iter from openIters.
                            removeFromOpenIters(open_iters_, iter);
                            // ii. Return ? IteratorCloseAll(openIters, result).
                            return types.Iterator.closeAll(
                                agent_,
                                open_iters_.items,
                                @as(Agent.Error!?Value, err),
                            );
                        };

                        // c. Set result to ! result.
                        // d. If result is done, then
                        if (maybe_result) |value| {
                            result = value;
                        } else {
                            // i. Remove iter from openIters.
                            removeFromOpenIters(open_iters_, iter);

                            switch (mode_) {
                                // ii. If mode is "shortest", then
                                .shortest => {
                                    // 1. Return ? IteratorCloseAll(openIters, ReturnCompletion(undefined)).
                                    iterator_helper.fields = .completed;
                                    return types.Iterator.closeAll(
                                        agent_,
                                        open_iters_.items,
                                        @as(Agent.Error!?Value, null),
                                    );
                                },
                                // iii. Else if mode is "strict", then
                                .strict => {
                                    // i. If i ≠ 0, then
                                    if (i != 0) {
                                        // 1. Return ? IteratorCloseAll(openIters, ThrowCompletion(
                                        //    a newly created TypeError object)).
                                        const @"error" = agent_.throwException(
                                            .type_error,
                                            "Iterators finished at different lengths in strict mode",
                                            .{},
                                        );
                                        return types.Iterator.closeAll(
                                            agent_,
                                            open_iters_.items,
                                            @as(Agent.Error!?Value, @"error"),
                                        );
                                    }

                                    // ii. For each integer k such that 1 ≤ k < iterCount, in ascending order, do
                                    for (1..iter_count) |k| {
                                        // i. Assert: iters[k] is not null.
                                        std.debug.assert(!iters_[k].done);

                                        // ii. Let open be Completion(IteratorStep(iters[k])).
                                        const open = iters_[k].step(agent_) catch |err| {
                                            // iii. If open is an abrupt completion, then
                                            // 1. Remove iters[k] from openIters.
                                            removeFromOpenIters(open_iters_, iters_[k]);

                                            // 2. Return ? IteratorCloseAll(openIters, open).
                                            return types.Iterator.closeAll(
                                                agent_,
                                                open_iters_.items,
                                                @as(Agent.Error!?Value, err),
                                            );
                                        };

                                        // iv. Set open to ! open.
                                        // v. If open is done, then
                                        if (open == null) {
                                            // i. Remove iters[k] from openIters.
                                            removeFromOpenIters(open_iters_, iters_[k]);
                                        } else {
                                            // vi. Else,
                                            // i. Return ? IteratorCloseAll(openIters,
                                            //    ThrowCompletion(a newly created TypeError object)).
                                            const @"error" = agent_.throwException(
                                                .type_error,
                                                "Iterators finished at different lengths in strict mode",
                                                .{},
                                            );
                                            return types.Iterator.closeAll(
                                                agent_,
                                                open_iters_.items,
                                                @as(Agent.Error!?Value, @"error"),
                                            );
                                        }
                                    }

                                    // iii. Return ReturnCompletion(undefined).
                                    return null;
                                },
                                .longest => {
                                    // iv. Else,
                                    // i. Assert: mode is "longest".
                                    std.debug.assert(mode_ == .longest);

                                    // ii. If openIters is empty, return ReturnCompletion(undefined).
                                    if (open_iters_.items.len == 0) return null;

                                    // iii. Set iters[i] to null.
                                    // (We mark done flag, which we check above)

                                    // iv. Set result to padding[i].
                                    result = padding_[i];
                                },
                            }
                        }
                    }

                    // 4. Append result to results.
                    try results.append(agent_.gc_allocator, result);
                }

                // iv. Set results to finishResults(results).
                const final_result = switch (finish_results_) {
                    // Iterator.zip, step 15.
                    .array => blk: {
                        // a. Return CreateArrayFromList(results).
                        const array = try createArrayFromList(agent_, results.items);
                        break :blk Value.from(&array.object);
                    },
                    // Iterator.zipKeyed, step 15.
                    .object => |keys| blk: {
                        // a. Let obj be OrdinaryObjectCreate(null).
                        const obj = try ordinaryObjectCreate(agent_, null);

                        // b. For each integer i such that 0 ≤ i < iterCount, in ascending order, do
                        for (0..iter_count) |i| {
                            // i. Perform ! CreateDataPropertyOrThrow(obj, keys[i], results[i]).
                            try obj.createDataPropertyOrThrow(agent_, keys[i], results.items[i]);
                        }

                        // c. Return obj.
                        break :blk Value.from(obj);
                    },
                };

                // v. Let completion be Completion(Yield(results)).
                // vi. If completion is an abrupt completion, then
                //     1. Return ? IteratorCloseAll(openIters, completion).
                return final_result;
            }
        }.func;

        const abruptClosure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!void {
                // 3.b.vi.
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const open_iters_ = captures_.open_iters;
                _ = try types.Iterator.closeAll(
                    agent_,
                    open_iters_.items,
                    @as(Agent.Error!void, {}),
                );
            }
        }.func;

        // 4. Let gen be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const gen = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 5. Set gen.[[UnderlyingIterators]] to openIters.
                    .underlying_iterators = open_iters.items,

                    .closure = closure,
                    .abruptClosure = abruptClosure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 6. Return gen.
        return Value.from(&gen.object);
    }
};

/// 27.1.3.3 Properties of the Iterator Prototype Object
/// https://tc39.es/ecma262/#sec-%iterator.prototype%-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "drop", drop, 1, realm);
        try object.defineBuiltinFunction(agent, "every", every, 1, realm);
        try object.defineBuiltinFunction(agent, "filter", filter, 1, realm);
        try object.defineBuiltinFunction(agent, "find", find, 1, realm);
        try object.defineBuiltinFunction(agent, "flatMap", flatMap, 1, realm);
        try object.defineBuiltinFunction(agent, "forEach", forEach, 1, realm);
        try object.defineBuiltinFunction(agent, "map", map, 1, realm);
        try object.defineBuiltinFunction(agent, "reduce", reduce, 1, realm);
        try object.defineBuiltinFunction(agent, "some", some, 1, realm);
        try object.defineBuiltinFunction(agent, "take", take, 1, realm);
        try object.defineBuiltinFunction(agent, "toArray", toArray, 0, realm);
        try object.defineBuiltinFunction(agent, "%Symbol.iterator%", @"%Symbol.iterator%", 0, realm);

        // 27.1.3.3.1 Iterator.prototype.constructor
        // https://tc39.es/ecma262/#sec-iterator.prototype.constructor
        try object.defineBuiltinAccessor(
            agent,
            "constructor",
            struct {
                /// 27.1.3.3.1.1 get Iterator.prototype.constructor
                /// https://tc39.es/ecma262/#sec-get-iterator.prototype.constructor
                fn get(agent_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                    // 1. Return %Iterator%.
                    return Value.from(try agent_.currentRealm().intrinsics.@"%Iterator%"());
                }
            }.get,
            struct {
                /// 27.1.3.3.1.2 set Iterator.prototype.constructor
                /// https://tc39.es/ecma262/#sec-set-iterator.prototype.constructor
                fn set(agent_: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
                    const value = arguments.get(0);

                    // 1. Perform ? SetterThatIgnoresPrototypeProperties(this value,
                    //    %Iterator.prototype%, "constructor", v).
                    try this_value.setterThatIgnoresPrototypeProperties(
                        agent_,
                        try agent_.currentRealm().intrinsics.@"%Iterator.prototype%"(),
                        PropertyKey.from("constructor"),
                        value,
                    );

                    // 2. Return undefined.
                    return .undefined;
                }
            }.set,
            realm,
        );

        // 27.1.3.3.14 Iterator.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.tostringtag%
        try object.defineBuiltinAccessor(
            agent,
            "%Symbol.toStringTag%",
            struct {
                /// 27.1.3.3.14.1 get Iterator.prototype [ %Symbol.toStringTag% ]
                /// https://tc39.es/ecma262/#sec-get-iterator.prototype-%symbol.tostringtag%
                fn get(_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                    // 1. Return "Iterator".
                    return Value.from("Iterator");
                }
            }.get,
            struct {
                /// 27.1.3.3.14.2 set Iterator.prototype [ %Symbol.toStringTag% ]
                /// https://tc39.es/ecma262/#sec-set-iterator.prototype-%symbol.tostringtag%
                fn set(agent_: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
                    const value = arguments.get(0);

                    // 1. Perform ? SetterThatIgnoresPrototypeProperties(this value,
                    //    %Iterator.prototype%, %Symbol.toStringTag%, v).
                    try this_value.setterThatIgnoresPrototypeProperties(
                        agent_,
                        try agent_.currentRealm().intrinsics.@"%Iterator.prototype%"(),
                        PropertyKey.from(agent_.well_known_symbols.@"%Symbol.toStringTag%"),
                        value,
                    );

                    // 2. Return undefined.
                    return .undefined;
                }
            }.set,
            realm,
        );
    }

    /// 27.1.3.3.2 Iterator.prototype.drop ( limit )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.drop
    fn drop(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const limit = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. Let numLimit be Completion(ToNumber(limit)).
        const num_limit = limit.toNumber(agent) catch |err| {
            // 5. IfAbruptCloseIterator(numLimit, iterated).
            return iterated.close(agent, @as(Agent.Error!Value, err));
        };

        // 6. If numLimit is NaN, then
        if (num_limit.isNan()) {
            // a. Let error be ThrowCompletion(a newly created RangeError object).
            const @"error" = agent.throwException(
                .range_error,
                "Limit must not be NaN",
                .{},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 7. Let integerLimit be ! ToIntegerOrInfinity(numLimit).
        const integer_limit = Value.from(num_limit).toIntegerOrInfinity(agent) catch unreachable;

        // 8. If integerLimit < 0, then
        if (integer_limit < 0) {
            // a. Let error be ThrowCompletion(a newly created RangeError object).
            const @"error" = agent.throwException(
                .range_error,
                "Limit must be a positive number",
                .{},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 9. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        const iterated_list = try agent.gc_allocator.alloc(types.Iterator, 1);
        iterated_list[0] = iterated;

        const Captures = struct {
            iterated: *types.Iterator,
            integer_limit: f64,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iterated = &iterated_list[0],
            .integer_limit = integer_limit,
        };

        // 10. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    integerLimit and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iterated_ = captures_.iterated;

                // a. Let remaining be integerLimit.
                const remaining = &captures_.integer_limit;

                // b. Repeat, while remaining > 0,
                while (remaining.* > 0) {
                    // i. If remaining ≠ +∞, then
                    if (!std.math.isInf(remaining.*)) {
                        // 1. Set remaining to remaining - 1.
                        remaining.* -= 1;
                    }

                    // ii. Let next be ? IteratorStep(iterated).
                    _ = try iterated_.step(agent_) orelse {
                        // iii. If next is done, return ReturnCompletion(undefined).
                        return null;
                    };
                }

                // c. Repeat,

                // i. Let value be ? IteratorStepValue(iterated).
                // ii. If value is done, return ReturnCompletion(undefined).
                const value = (try iterated_.stepValue(agent_)) orelse return null;

                // iii. Let completion be Completion(Yield(value)).
                // iv. IfAbruptCloseIterator(completion, iterated).
                return value;
            }
        }.func;

        // 11. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 12. Set result.[[UnderlyingIterators]] to « iterated ».
                    .underlying_iterators = iterated_list,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 13. Return result.
        return Value.from(&result.object);
    }

    /// 27.1.3.3.3 Iterator.prototype.every ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.every
    fn every(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(predicate) is false, then
        if (!predicate.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{predicate},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        // 6. Let counter be 0.
        var counter: u53 = 0;

        // 7. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return true.
        while (try iterated.stepValue(agent)) |value| {
            // c. Let result be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
            const result = predicate.callAssumeCallable(
                agent,
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(agent, @as(Agent.Error!Value, err));
            };

            // e. If ToBoolean(result) is false, return ? IteratorClose(iterated, NormalCompletion(false)).
            if (!result.toBoolean()) {
                return try iterated.close(agent, @as(Agent.Error!Value, .false));
            }

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return .true;
    }

    /// 27.1.3.3.4 Iterator.prototype.filter ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.filter
    fn filter(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(predicate) is false, then
        if (!predicate.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{predicate},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        const iterated_list = try agent.gc_allocator.alloc(types.Iterator, 1);
        iterated_list[0] = iterated;

        const Captures = struct {
            iterated: *types.Iterator,
            predicate: Value,
            counter: u53,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iterated = &iterated_list[0],
            .predicate = predicate,
            .counter = 0,
        };

        // 6. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    predicate and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iterated_ = captures_.iterated;

                // a. Let remaining be integerLimit.
                const predicate_ = captures_.predicate;

                // a. Let counter be 0.
                const counter = &captures_.counter;

                // b. Repeat,
                //     i. Let value be ? IteratorStepValue(iterated).
                //     ii. If value is done, return ReturnCompletion(undefined).
                while (try iterated_.stepValue(agent_)) |value| {
                    // iii. Let selected be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
                    const selected = predicate_.callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{ value, Value.from(counter.*) },
                    ) catch |err| {
                        // iv. IfAbruptCloseIterator(selected, iterated).
                        return iterated_.close(agent_, @as(Agent.Error!?Value, err));
                    };

                    // vi. Set counter to counter + 1.
                    defer counter.* += 1;

                    // v. If ToBoolean(selected) is true, then
                    if (selected.toBoolean()) {
                        // 1. Let completion be Completion(Yield(value)).
                        // 2. IfAbruptCloseIterator(completion, iterated).
                        return value;
                    }
                }
                return null;
            }
        }.func;

        // 7. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 8. Set result.[[UnderlyingIterators]] to « iterated ».
                    .underlying_iterators = iterated_list,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 9. Return result.
        return Value.from(&result.object);
    }

    /// 27.1.3.3.5 Iterator.prototype.find ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.find
    fn find(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(predicate) is false, then
        if (!predicate.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{predicate},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        // 6. Let counter be 0.
        var counter: u53 = 0;

        // 7. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return undefined.
        while (try iterated.stepValue(agent)) |value| {
            // c. Let result be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
            const result = predicate.callAssumeCallable(
                agent,
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(agent, @as(Agent.Error!Value, err));
            };

            // e. If ToBoolean(result) is true, return ? IteratorClose(iterated, NormalCompletion(value)).
            if (result.toBoolean()) {
                return iterated.close(agent, @as(Agent.Error!Value, value));
            }

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return .undefined;
    }

    /// 27.1.3.3.6 Iterator.prototype.flatMap ( mapper )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.flatmap
    fn flatMap(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const mapper = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(mapper) is false, then
        if (!mapper.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{mapper},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        const iterated_list = try agent.gc_allocator.alloc(types.Iterator, 1);
        iterated_list[0] = iterated;

        const Captures = struct {
            iterated: *types.Iterator,
            mapper: Value,
            counter: u53,
            inner_iterator: ?types.Iterator,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iterated = &iterated_list[0],
            .mapper = mapper,
            .counter = 0,
            .inner_iterator = null,
        };

        // 6. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    mapper and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iterated_ = captures_.iterated;
                const mapper_ = captures_.mapper;
                const inner_iterator_ = &captures_.inner_iterator;

                // a. Let counter be 0.
                const counter_ = &captures_.counter;

                const State = enum { outer, inner };
                const state: State = if (inner_iterator_.* == null)
                    .outer
                else
                    .inner;

                // b. Repeat,
                loop: switch (state) {
                    .outer => {
                        // i. Let value be ? IteratorStepValue(iterated).
                        // ii. If value is done, return ReturnCompletion(undefined).
                        const value = (try iterated_.stepValue(agent_)) orelse return null;

                        // iii. Let mapped be Completion(Call(mapper, undefined, « value, 𝔽(counter) »)).
                        const mapped = mapper_.callAssumeCallable(
                            agent_,
                            .undefined,
                            &.{ value, Value.from(counter_.*) },
                        ) catch |err| {
                            // iv. IfAbruptCloseIterator(mapped, iterated).
                            return iterated_.close(agent_, @as(Agent.Error!?Value, err));
                        };

                        // v. Let innerIterator be Completion(GetIteratorFlattenable(mapped, reject-primitives)).
                        inner_iterator_.* = getIteratorFlattenable(
                            agent_,
                            mapped,
                            .reject_primitives,
                        ) catch |err| {
                            // vi. IfAbruptCloseIterator(innerIterator, iterated).
                            return iterated_.close(agent_, @as(Agent.Error!?Value, err));
                        };

                        continue :loop .inner;
                    },
                    .inner => {
                        // vii. Let innerAlive be true.
                        // viii. Repeat, while innerAlive is true,

                        // 1. Let innerValue be Completion(IteratorStepValue(innerIterator)).
                        const inner_value = inner_iterator_.*.?.stepValue(agent_) catch |err| {
                            // 2. IfAbruptCloseIterator(innerValue, iterated).
                            return iterated_.close(agent_, @as(Agent.Error!?Value, err));
                        };

                        // 3. If innerValue is done, then
                        //     a. Set innerAlive to false.
                        // 4. Else,
                        //     a. Let completion be Completion(Yield(innerValue)).
                        //     b. If completion is an abrupt completion, then
                        //         i. Let backupCompletion be Completion(IteratorClose(innerIterator, completion)).
                        //         ii. IfAbruptCloseIterator(backupCompletion, iterated).
                        //         iii. Return ? IteratorClose(iterated, completion).
                        if (inner_value) |value| return value;

                        // ix. Set counter to counter + 1.
                        counter_.* += 1;

                        continue :loop .outer;
                    },
                }
            }
        }.func;

        const abruptClosure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!void {
                // 6.b.viii.4.b.
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                if (captures_.inner_iterator) |inner_iterator| {
                    inner_iterator.close(agent_, @as(Agent.Error!void, {})) catch |err| {
                        return captures_.iterated.close(agent_, @as(Agent.Error!void, err));
                    };
                }
                try captures_.iterated.close(agent_, @as(Agent.Error!void, {}));
            }
        }.func;

        // 7. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 8. Set result.[[UnderlyingIterators]] to « iterated ».
                    .underlying_iterators = iterated_list,

                    .closure = closure,
                    .abruptClosure = abruptClosure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 9. Return result.
        return Value.from(&result.object);
    }

    /// 27.1.3.3.7 Iterator.prototype.forEach ( procedure )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const procedure = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(procedure) is false, then
        if (!procedure.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{procedure},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        // 6. Let counter be 0.
        var counter: u53 = 0;

        // 7. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return undefined.
        while (try iterated.stepValue(agent)) |value| {
            // c. Let result be Completion(Call(procedure, undefined, « value, 𝔽(counter) »)).
            _ = procedure.callAssumeCallable(
                agent,
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(agent, @as(Agent.Error!Value, err));
            };

            // e. Set counter to counter + 1.
            counter += 1;
        }
        return .undefined;
    }

    /// 27.1.3.3.8 Iterator.prototype.map ( mapper )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.map
    fn map(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const mapper = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(mapper) is false, then
        if (!mapper.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{mapper},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        const iterated_list = try agent.gc_allocator.alloc(types.Iterator, 1);
        iterated_list[0] = iterated;

        const Captures = struct {
            iterated: *types.Iterator,
            mapper: Value,
            counter: u53,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iterated = &iterated_list[0],
            .mapper = mapper,
            .counter = 0,
        };

        // 6. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    mapper and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iterated_ = captures_.iterated;
                const mapper_ = captures_.mapper;

                // a. Let counter be 0.
                const counter = &captures_.counter;

                // b. Repeat,

                // i. Let value be ? IteratorStepValue(iterated).
                // ii. If value is done, return ReturnCompletion(undefined).
                const value = (try iterated_.stepValue(agent_)) orelse return null;

                // iii. Let mapped be Completion(Call(mapper, undefined, « value, 𝔽(counter) »)).
                const mapped = mapper_.callAssumeCallable(
                    agent_,
                    .undefined,
                    &.{ value, Value.from(counter.*) },
                ) catch |err| {
                    // iv. IfAbruptCloseIterator(mapped, iterated).
                    return iterated_.close(agent_, @as(Agent.Error!?Value, err));
                };

                // vii. Set counter to counter + 1.
                defer counter.* += 1;

                // v. Let completion be Completion(Yield(mapped)).
                // vi. IfAbruptCloseIterator(completion, iterated).
                return mapped;
            }
        }.func;

        // 7. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 8. Set result.[[UnderlyingIterators]] to « iterated ».
                    .underlying_iterators = iterated_list,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 9. Return result.
        return Value.from(&result.object);
    }

    /// 27.1.3.3.9 Iterator.prototype.reduce ( reducer [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.reduce
    fn reduce(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const reducer = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(reducer) is false, then
        if (!reducer.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{reducer},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        var accumulator: Value = undefined;
        var counter: u53 = undefined;

        // 6. If initialValue is not present, then
        if (initial_value == null) {
            // a. Let accumulator be ? IteratorStepValue(iterated).
            accumulator = (try iterated.stepValue(agent)) orelse {
                // b. If accumulator is done, throw a TypeError exception.
                return agent.throwException(
                    .type_error,
                    "Cannot reduce empty iterator without initial value",
                    .{},
                );
            };

            // c. Let counter be 1.
            counter = 1;
        } else {
            // 7. Else,
            // a. Let accumulator be initialValue.
            accumulator = initial_value.?;

            // b. Let counter be 0.
            counter = 0;
        }

        // 8. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return accumulator.
        while (try iterated.stepValue(agent)) |value| {
            // c. Let result be Completion(Call(reducer, undefined, « accumulator, value, 𝔽(counter) »)).
            const result = reducer.callAssumeCallable(
                agent,
                .undefined,
                &.{ accumulator, value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(agent, @as(Agent.Error!Value, err));
            };

            // e. Set accumulator to result.
            accumulator = result;

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return accumulator;
    }

    /// 27.1.3.3.10 Iterator.prototype.some ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.some
    fn some(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. If IsCallable(predicate) is false, then
        if (!predicate.isCallable()) {
            // a. Let error be ThrowCompletion(a newly created TypeError object).
            const @"error" = agent.throwException(
                .type_error,
                "{f} is not callable",
                .{predicate},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 5. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        // 6. Let counter be 0.
        var counter: u53 = 0;

        // 7. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return false.
        while (try iterated.stepValue(agent)) |value| {
            // c. Let result be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
            const result = predicate.callAssumeCallable(
                agent,
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(agent, @as(Agent.Error!Value, err));
            };

            // e. If ToBoolean(result) is true, return ? IteratorClose(iterated, NormalCompletion(true)).
            if (result.toBoolean()) {
                return try iterated.close(agent, @as(Agent.Error!Value, .true));
            }

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return .false;
    }

    /// 27.1.3.3.11 Iterator.prototype.take ( limit )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.take
    fn take(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const limit = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be the Iterator Record {
        //      [[Iterator]]: O, [[NextMethod]]: undefined, [[Done]]: false
        //    }.
        var iterated: types.Iterator = .{
            .iterator = object,
            .next_method = .undefined,
            .done = false,
        };

        // 4. Let numLimit be Completion(ToNumber(limit)).
        const num_limit = limit.toNumber(agent) catch |err| {
            // 5. IfAbruptCloseIterator(numLimit, iterated).
            return iterated.close(agent, @as(Agent.Error!Value, err));
        };

        // 6. If numLimit is NaN, then
        if (num_limit.isNan()) {
            // a. Let error be ThrowCompletion(a newly created RangeError object).
            const @"error" = agent.throwException(
                .range_error,
                "Limit must not be NaN",
                .{},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 7. Let integerLimit be ! ToIntegerOrInfinity(numLimit).
        const integer_limit = Value.from(num_limit).toIntegerOrInfinity(agent) catch unreachable;

        // 8. If integerLimit < 0, then
        if (integer_limit < 0) {
            // a. Let error be ThrowCompletion(a newly created RangeError object).
            const @"error" = agent.throwException(
                .range_error,
                "Limit must be a positive number",
                .{},
            );

            // b. Return ? IteratorClose(iterated, error).
            return iterated.close(agent, @as(Agent.Error!Value, @"error"));
        }

        // 9. Set iterated to ? GetIteratorDirect(O).
        iterated = try getIteratorDirect(agent, object);

        const iterated_list = try agent.gc_allocator.alloc(types.Iterator, 1);
        iterated_list[0] = iterated;

        const Captures = struct {
            iterated: *types.Iterator,
            integer_limit: f64,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{
            .iterated = &iterated_list[0],
            .integer_limit = integer_limit,
        };

        // 10. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    integerLimit and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const captures_ = iterator_helper.fields.state.captures.cast(*Captures);
                const iterated_ = captures_.iterated;

                // a. Let remaining be integerLimit.
                const remaining = &captures_.integer_limit;

                // b. Repeat,
                while (true) {
                    // i. If remaining = 0, then
                    if (remaining.* == 0) {
                        // 1. Return ? IteratorClose(iterated, ReturnCompletion(undefined)).
                        iterator_helper.fields = .completed;
                        return iterated_.close(agent_, @as(Agent.Error!?Value, null));
                    }

                    // ii. If remaining ≠ +∞, then
                    if (!std.math.isInf(remaining.*)) {
                        // 1. Set remaining to remaining - 1.
                        remaining.* -= 1;
                    }

                    // iii. Let value be ? IteratorStepValue(iterated).
                    // iv. If value is done, return ReturnCompletion(undefined).
                    const value = (try iterated_.stepValue(agent_)) orelse return null;

                    // v. Let completion be Completion(Yield(value)).
                    // vi. IfAbruptCloseIterator(completion, iterated).
                    return value;
                }
            }
        }.func;

        // 11. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterators]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 12. Set result.[[UnderlyingIterators]] to « iterated ».
                    .underlying_iterators = iterated_list,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 13. Return result.
        return Value.from(&result.object);
    }

    /// 27.1.3.3.12 Iterator.prototype.toArray ( )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.toarray
    fn toArray(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(agent, object);

        // 4. Let items be a new empty List.
        var items: std.ArrayList(Value) = .empty;
        defer items.deinit(agent.gc_allocator);

        // 5. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return CreateArrayFromList(items).
        while (try iterated.stepValue(agent)) |value| {
            // c. Append value to items.
            try items.append(agent.gc_allocator, value);
        }
        const array = try createArrayFromList(agent, items.items);
        return Value.from(&array.object);
    }

    /// 27.1.3.3.13 Iterator.prototype [ %Symbol.iterator% ] ( )
    /// https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.iterator%
    fn @"%Symbol.iterator%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

pub const Iterator = MakeObject(.{
    .tag = .iterator,
    .display_name = "Iterator",
});
