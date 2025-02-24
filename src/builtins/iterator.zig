//! 27.1.3 Iterator Objects
//! https://tc39.es/ecma262/#sec-iterator-objects

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
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIteratorDirect = types.getIteratorDirect;
const getIteratorFlattenable = types.getIteratorFlattenable;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 27.1.3.1 The Iterator Constructor
/// https://tc39.es/ecma262/#sec-iterator-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 0,
            .name = "Iterator",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "from", from, 1, realm);

        // 27.1.3.2.2 Iterator.prototype
        // https://tc39.es/ecma262/#sec-iterator.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Iterator.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
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
        return Value.from(
            try ordinaryCreateFromConstructor(
                Iterator,
                agent,
                new_target.?,
                "%Iterator.prototype%",
                {},
            ),
        );
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
        ).ordinaryHasInstance(Value.from(iterator.iterator));

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
        return Value.from(wrapper);
    }
};

/// 27.1.4 The %Iterator.prototype% Object
/// https://tc39.es/ecma262/#sec-%iterator.prototype%-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "drop", drop, 1, realm);
        try defineBuiltinFunction(object, "every", every, 1, realm);
        try defineBuiltinFunction(object, "filter", filter, 1, realm);
        try defineBuiltinFunction(object, "find", find, 1, realm);
        try defineBuiltinFunction(object, "flatMap", flatMap, 1, realm);
        try defineBuiltinFunction(object, "forEach", forEach, 1, realm);
        try defineBuiltinFunction(object, "map", map, 1, realm);
        try defineBuiltinFunction(object, "reduce", reduce, 1, realm);
        try defineBuiltinFunction(object, "some", some, 1, realm);
        try defineBuiltinFunction(object, "take", take, 1, realm);
        try defineBuiltinFunction(object, "toArray", toArray, 0, realm);
        try defineBuiltinFunction(object, "%Symbol.iterator%", @"%Symbol.iterator%", 0, realm);

        // 27.1.4.1 Iterator.prototype.constructor
        // https://tc39.es/ecma262/#sec-iterator.prototype.constructor
        try defineBuiltinAccessor(
            object,
            "constructor",
            struct {
                /// 27.1.4.1.1 get Iterator.prototype.constructor
                /// https://tc39.es/ecma262/#sec-get-iterator.prototype.constructor
                fn get(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                    // 1. Return %Iterator%.
                    return Value.from(try agent.currentRealm().intrinsics.@"%Iterator%"());
                }
            }.get,
            struct {
                /// 27.1.4.1.2 set Iterator.prototype.constructor
                /// https://tc39.es/ecma262/#sec-set-iterator.prototype.constructor
                fn set(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
                    const value = arguments.get(0);

                    // 1. Perform ? SetterThatIgnoresPrototypeProperties(this value,
                    //    %Iterator.prototype%, "constructor", v).
                    try this_value.setterThatIgnoresPrototypeProperties(
                        agent,
                        try agent.currentRealm().intrinsics.@"%Iterator.prototype%"(),
                        PropertyKey.from("constructor"),
                        value,
                    );

                    // 2. Return undefined.
                    return .undefined;
                }
            }.set,
            realm,
        );

        // 27.1.4.14 Iterator.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.tostringtag%
        try defineBuiltinAccessor(
            object,
            "%Symbol.toStringTag%",
            struct {
                /// 27.1.4.14.1 get Iterator.prototype [ %Symbol.toStringTag% ]
                /// https://tc39.es/ecma262/#sec-get-iterator.prototype-%symbol.tostringtag%
                fn get(_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                    // 1. Return "Iterator".
                    return Value.from("Iterator");
                }
            }.get,
            struct {
                /// 27.1.4.14.2 set Iterator.prototype [ %Symbol.toStringTag% ]
                /// https://tc39.es/ecma262/#sec-set-iterator.prototype-%symbol.tostringtag%
                fn set(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
                    const value = arguments.get(0);

                    // 1. Perform ? SetterThatIgnoresPrototypeProperties(this value,
                    //    %Iterator.prototype%, %Symbol.toStringTag%, v).
                    try this_value.setterThatIgnoresPrototypeProperties(
                        agent,
                        try agent.currentRealm().intrinsics.@"%Iterator.prototype%"(),
                        PropertyKey.from(agent.well_known_symbols.@"%Symbol.toStringTag%"),
                        value,
                    );

                    // 2. Return undefined.
                    return .undefined;
                }
            }.set,
            realm,
        );
    }

    /// 27.1.4.2 Iterator.prototype.drop ( limit )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.drop
    fn drop(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const limit = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let numLimit be ? ToNumber(limit).
        const num_limit = try limit.toNumber(agent);

        // 4. If numLimit is NaN, throw a RangeError exception.
        if (num_limit.isNan()) {
            return agent.throwException(.range_error, "Limit must not be NaN", .{});
        }

        // 5. Let integerLimit be ! ToIntegerOrInfinity(numLimit).
        const integer_limit = Value.from(num_limit).toIntegerOrInfinity(agent) catch unreachable;

        // 6. If integerLimit < 0, throw a RangeError exception.
        if (integer_limit < 0) {
            return agent.throwException(.range_error, "Limit must be a positive number", .{});
        }

        // 7. Let iterated be ? GetIteratorDirect(O).
        const iterated = try getIteratorDirect(object);

        const Captures = struct {
            integer_limit: f64,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{ .integer_limit = integer_limit };

        // 8. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    integerLimit and performs the following steps when called:
        const closure = struct {
            fn func(_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const iterated_ = &iterator_helper.fields.state.underlying_iterator;

                // a. Let remaining be integerLimit.
                const remaining = &iterator_helper.fields.state.captures.cast(*Captures).integer_limit;

                // b. Repeat, while remaining > 0,
                while (remaining.* > 0) {
                    // i. If remaining ≠ +∞, then
                    if (!std.math.isInf(remaining.*)) {
                        // 1. Set remaining to remaining - 1.
                        remaining.* -= 1;
                    }

                    // ii. Let next be ? IteratorStep(iterated).
                    _ = try iterated_.step() orelse {
                        // iii. If next is done, return ReturnCompletion(undefined).
                        return null;
                    };
                }

                // c. Repeat,

                // i. Let value be ? IteratorStepValue(iterated).
                // ii. If value is done, return ReturnCompletion(undefined).
                const value = (try iterated_.stepValue()) orelse return null;

                // iii. Let completion be Completion(Yield(value)).
                // iv. IfAbruptCloseIterator(completion, iterated).
                return value;
            }
        }.func;

        // 9. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterator]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 10. Set result.[[UnderlyingIterator]] to iterated.
                    .underlying_iterator = iterated,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 11. Return result.
        return Value.from(result);
    }

    /// 27.1.4.3 Iterator.prototype.every ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.every
    fn every(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(predicate) is false, throw a TypeError exception.
        if (!predicate.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{predicate});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(object);

        // 5. Let counter be 0.
        var counter: u53 = 0;

        // 6. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return true.
        while (try iterated.stepValue()) |value| {
            // c. Let result be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
            const result = predicate.callAssumeCallable(
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(@as(Agent.Error!Value, err));
            };

            // e. If ToBoolean(result) is false, return ? IteratorClose(iterated, NormalCompletion(false)).
            if (!result.toBoolean()) {
                return try iterated.close(@as(Agent.Error!Value, Value.from(false)));
            }

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return Value.from(true);
    }

    /// 27.1.4.4 Iterator.prototype.filter ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.filter
    fn filter(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(predicate) is false, throw a TypeError exception.
        if (!predicate.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{predicate});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        const iterated = try getIteratorDirect(object);

        const Captures = struct {
            predicate: Value,
            counter: u53,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{ .predicate = predicate, .counter = 0 };

        // 5. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    predicate and performs the following steps when called:
        const closure = struct {
            fn func(_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const iterated_ = &iterator_helper.fields.state.underlying_iterator;

                // a. Let remaining be integerLimit.
                const predicate_ = iterator_helper.fields.state.captures.cast(*Captures).predicate;

                // a. Let counter be 0.
                const counter = &iterator_helper.fields.state.captures.cast(*Captures).counter;

                // b. Repeat,
                //     i. Let value be ? IteratorStepValue(iterated).
                //     ii. If value is done, return ReturnCompletion(undefined).
                while (try iterated_.stepValue()) |value| {
                    // iii. Let selected be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
                    const selected = predicate_.callAssumeCallable(
                        .undefined,
                        &.{ value, Value.from(counter.*) },
                    ) catch |err| {
                        // iv. IfAbruptCloseIterator(selected, iterated).
                        return iterated_.close(@as(Agent.Error!?Value, err));
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

        // 6. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterator]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 7. Set result.[[UnderlyingIterator]] to iterated.
                    .underlying_iterator = iterated,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 8. Return result.
        return Value.from(result);
    }

    /// 27.1.4.5 Iterator.prototype.find ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.find
    fn find(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(predicate) is false, throw a TypeError exception.
        if (!predicate.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{predicate});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(object);

        // 5. Let counter be 0.
        var counter: u53 = 0;

        // 6. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return undefined.
        while (try iterated.stepValue()) |value| {
            // c. Let result be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
            const result = predicate.callAssumeCallable(
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(@as(Agent.Error!Value, err));
            };

            // e. If ToBoolean(result) is true, return ? IteratorClose(iterated, NormalCompletion(value)).
            if (result.toBoolean()) {
                return iterated.close(@as(Agent.Error!Value, value));
            }

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return .undefined;
    }

    /// 27.1.4.6 Iterator.prototype.flatMap ( mapper )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.flatmap
    fn flatMap(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const mapper = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(mapper) is false, throw a TypeError exception.
        if (!mapper.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{mapper});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        const iterated = try getIteratorDirect(object);

        const Captures = struct {
            mapper: Value,
            counter: u53,
            inner_iterator: ?types.Iterator,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{ .mapper = mapper, .counter = 0, .inner_iterator = null };

        // 5. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    mapper and performs the following steps when called:
        const closure = struct {
            fn func(agent_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const iterated_ = &iterator_helper.fields.state.underlying_iterator;
                const mapper_ = &iterator_helper.fields.state.captures.cast(*Captures).mapper;

                // a. Let counter be 0.
                const counter_ = &iterator_helper.fields.state.captures.cast(*Captures).counter;

                const State = enum { outer, inner };
                const state: State = if (iterator_helper.fields.state.captures.cast(*Captures).inner_iterator == null)
                    .outer
                else
                    .inner;

                // b. Repeat,
                loop: switch (state) {
                    .outer => {
                        // i. Let value be ? IteratorStepValue(iterated).
                        // ii. If value is done, return ReturnCompletion(undefined).
                        const value = (try iterated_.stepValue()) orelse return null;

                        // iii. Let mapped be Completion(Call(mapper, undefined, « value, 𝔽(counter) »)).
                        const mapped = mapper_.callAssumeCallable(
                            .undefined,
                            &.{ value, Value.from(counter_.*) },
                        ) catch |err| {
                            // iv. IfAbruptCloseIterator(mapped, iterated).
                            return iterated_.close(@as(Agent.Error!?Value, err));
                        };

                        // v. Let innerIterator be Completion(GetIteratorFlattenable(mapped, reject-primitives)).
                        const inner_iterator = getIteratorFlattenable(
                            agent_,
                            mapped,
                            .reject_primitives,
                        ) catch |err| {
                            // vi. IfAbruptCloseIterator(innerIterator, iterated).
                            return iterated_.close(@as(Agent.Error!?Value, err));
                        };

                        iterator_helper.fields.state.captures.cast(*Captures).inner_iterator = inner_iterator;
                        continue :loop .inner;
                    },
                    .inner => {
                        const inner_iterator = &iterator_helper.fields.state.captures.cast(*Captures).inner_iterator.?;

                        // ix. Set counter to counter + 1.
                        defer counter_.* += 1;

                        // vii. Let innerAlive be true.
                        // viii. Repeat, while innerAlive is true,

                        // 1. Let innerValue be Completion(IteratorStepValue(innerIterator)).
                        const inner_value = inner_iterator.stepValue() catch |err| {
                            // 2. IfAbruptCloseIterator(innerValue, iterated).
                            return iterated_.close(@as(Agent.Error!?Value, err));
                        };

                        // 3. If innerValue is done, then
                        if (inner_value == null) {
                            // a. Set innerAlive to false.
                            continue :loop .outer;
                        } else {
                            // 4. Else,
                            // a. Let completion be Completion(Yield(innerValue)).
                            // b. If completion is an abrupt completion, then
                            //     i. Let backupCompletion be Completion(IteratorClose(innerIterator, completion)).
                            //     ii. IfAbruptCloseIterator(backupCompletion, iterated).
                            //     iii. Return ? IteratorClose(iterated, completion).
                            return inner_value;
                        }
                    },
                }
            }
        }.func;

        // 6. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterator]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 7. Set result.[[UnderlyingIterator]] to iterated.
                    .underlying_iterator = iterated,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 8. Return result.
        return Value.from(result);
    }

    /// 27.1.4.7 Iterator.prototype.forEach ( procedure )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.foreach
    fn forEach(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const procedure = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(procedure) is false, throw a TypeError exception.
        if (!procedure.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{procedure});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(object);

        // 5. Let counter be 0.
        var counter: u53 = 0;

        // 6. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return undefined.
        while (try iterated.stepValue()) |value| {
            // c. Let result be Completion(Call(procedure, undefined, « value, 𝔽(counter) »)).
            _ = procedure.callAssumeCallable(
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(@as(Agent.Error!Value, err));
            };

            // e. Set counter to counter + 1.
            counter += 1;
        }
        return .undefined;
    }

    /// 27.1.4.8 Iterator.prototype.map ( mapper )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.map
    fn map(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const mapper = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(mapper) is false, throw a TypeError exception.
        if (!mapper.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{mapper});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        const iterated = try getIteratorDirect(object);

        const Captures = struct {
            mapper: Value,
            counter: u53,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{ .mapper = mapper, .counter = 0 };

        // 5. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    mapper and performs the following steps when called:
        const closure = struct {
            fn func(_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const iterated_ = &iterator_helper.fields.state.underlying_iterator;
                const mapper_ = &iterator_helper.fields.state.captures.cast(*Captures).mapper;

                // a. Let counter be 0.
                const counter = &iterator_helper.fields.state.captures.cast(*Captures).counter;

                // b. Repeat,

                // i. Let value be ? IteratorStepValue(iterated).
                // ii. If value is done, return ReturnCompletion(undefined).
                const value = (try iterated_.stepValue()) orelse return null;

                // iii. Let mapped be Completion(Call(mapper, undefined, « value, 𝔽(counter) »)).
                const mapped = mapper_.callAssumeCallable(
                    .undefined,
                    &.{ value, Value.from(counter.*) },
                ) catch |err| {
                    // iv. IfAbruptCloseIterator(mapped, iterated).
                    return iterated_.close(@as(Agent.Error!?Value, err));
                };

                // vii. Set counter to counter + 1.
                defer counter.* += 1;

                // v. Let completion be Completion(Yield(mapped)).
                // vi. IfAbruptCloseIterator(completion, iterated).
                return mapped;
            }
        }.func;

        // 6. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterator]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 7. Set result.[[UnderlyingIterator]] to iterated.
                    .underlying_iterator = iterated,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 8. Return result.
        return Value.from(result);
    }

    /// 27.1.4.9 Iterator.prototype.reduce ( reducer [ , initialValue ] )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.reduce
    fn reduce(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const reducer = arguments.get(0);
        const initial_value = arguments.getOrNull(1);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(reducer) is false, throw a TypeError exception.
        if (!reducer.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{reducer});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(object);

        var accumulator: Value = undefined;
        var counter: u53 = undefined;

        // 5. If initialValue is not present, then
        if (initial_value == null) {
            // a. Let accumulator be ? IteratorStepValue(iterated).
            accumulator = (try iterated.stepValue()) orelse {
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
            // 6. Else,
            // a. Let accumulator be initialValue.
            accumulator = initial_value.?;

            // b. Let counter be 0.
            counter = 0;
        }

        // 7. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return accumulator.
        while (try iterated.stepValue()) |value| {
            // c. Let result be Completion(Call(reducer, undefined, « accumulator, value, 𝔽(counter) »)).
            const result = reducer.callAssumeCallable(
                .undefined,
                &.{ accumulator, value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(@as(Agent.Error!Value, err));
            };

            // e. Set accumulator to result.
            accumulator = result;

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return accumulator;
    }

    /// 27.1.4.10 Iterator.prototype.some ( predicate )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.some
    fn some(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const predicate = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. If IsCallable(predicate) is false, throw a TypeError exception.
        if (!predicate.isCallable()) {
            return agent.throwException(.type_error, "{} is not callable", .{predicate});
        }

        // 4. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(object);

        // 5. Let counter be 0.
        var counter: u53 = 0;

        // 6. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return false.
        while (try iterated.stepValue()) |value| {
            // c. Let result be Completion(Call(predicate, undefined, « value, 𝔽(counter) »)).
            const result = predicate.callAssumeCallable(
                .undefined,
                &.{ value, Value.from(counter) },
            ) catch |err| {
                // d. IfAbruptCloseIterator(result, iterated).
                return iterated.close(@as(Agent.Error!Value, err));
            };

            // e. If ToBoolean(result) is true, return ? IteratorClose(iterated, NormalCompletion(false)).
            if (result.toBoolean()) {
                return try iterated.close(@as(Agent.Error!Value, Value.from(true)));
            }

            // f. Set counter to counter + 1.
            counter += 1;
        }
        return Value.from(false);
    }

    /// 27.1.4.11 Iterator.prototype.take ( limit )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.take
    fn take(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const limit = arguments.get(0);

        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let numLimit be ? ToNumber(limit).
        const num_limit = try limit.toNumber(agent);

        // 4. If numLimit is NaN, throw a RangeError exception.
        if (num_limit.isNan()) {
            return agent.throwException(.range_error, "Limit must not be NaN", .{});
        }

        // 5. Let integerLimit be ! ToIntegerOrInfinity(numLimit).
        const integer_limit = Value.from(num_limit).toIntegerOrInfinity(agent) catch unreachable;

        // 6. If integerLimit < 0, throw a RangeError exception.
        if (integer_limit < 0) {
            return agent.throwException(.range_error, "Limit must be a positive number", .{});
        }

        // 7. Let iterated be ? GetIteratorDirect(O).
        const iterated = try getIteratorDirect(object);

        const Captures = struct {
            integer_limit: f64,
        };
        const captures = try agent.gc_allocator.create(Captures);
        captures.* = .{ .integer_limit = integer_limit };

        // 8. Let closure be a new Abstract Closure with no parameters that captures iterated and
        //    integerLimit and performs the following steps when called:
        const closure = struct {
            fn func(_: *Agent, iterator_helper: *builtins.IteratorHelper) Agent.Error!?Value {
                const iterated_ = &iterator_helper.fields.state.underlying_iterator;

                // a. Let remaining be integerLimit.
                const remaining = &iterator_helper.fields.state.captures.cast(*Captures).integer_limit;

                // b. Repeat,
                while (true) {
                    // i. If remaining = 0, then
                    if (remaining.* == 0) {
                        // 1. Return ? IteratorClose(iterated, ReturnCompletion(undefined)).
                        return iterated_.close(@as(Agent.Error!?Value, null));
                    }

                    // ii. If remaining ≠ +∞, then
                    if (!std.math.isInf(remaining.*)) {
                        // 1. Set remaining to remaining - 1.
                        remaining.* -= 1;
                    }

                    // iii. Let value be ? IteratorStepValue(iterated).
                    // iv. If value is done, return ReturnCompletion(undefined).
                    const value = (try iterated_.stepValue()) orelse return null;

                    // v. Let completion be Completion(Yield(value)).
                    // vi. IfAbruptCloseIterator(completion, iterated).
                    return value;
                }
            }
        }.func;

        // 9. Let result be CreateIteratorFromClosure(closure, "Iterator Helper",
        //    %IteratorHelperPrototype%, « [[UnderlyingIterator]] »).
        const result = try builtins.IteratorHelper.create(agent, .{
            .prototype = try realm.intrinsics.@"%IteratorHelperPrototype%"(),
            .fields = .{
                .state = .{
                    // 10. Set result.[[UnderlyingIterator]] to iterated.
                    .underlying_iterator = iterated,

                    .closure = closure,
                    .captures = .make(*Captures, captures),
                },
            },
        });

        // 11. Return result.
        return Value.from(result);
    }

    /// 27.1.4.12 Iterator.prototype.toArray ( )
    /// https://tc39.es/ecma262/#sec-iterator.prototype.toarray
    fn toArray(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let iterated be ? GetIteratorDirect(O).
        var iterated = try getIteratorDirect(object);

        // 4. Let items be a new empty List.
        var items: std.ArrayListUnmanaged(Value) = .empty;
        defer items.deinit(agent.gc_allocator);

        // 5. Repeat,
        //     a. Let value be ? IteratorStepValue(iterated).
        //     b. If value is done, return CreateArrayFromList(items).
        while (try iterated.stepValue()) |value| {
            // c. Append value to items.
            try items.append(agent.gc_allocator, value);
        }
        return Value.from(try createArrayFromList(agent, items.items));
    }

    /// 27.1.4.13 Iterator.prototype [ %Symbol.iterator% ] ( )
    /// https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.iterator%
    fn @"%Symbol.iterator%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

pub const Iterator = MakeObject(.{
    .tag = .iterator,
});
