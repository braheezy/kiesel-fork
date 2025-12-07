//! 3.2.21. Sequences — sequence<T>
//! https://webidl.spec.whatwg.org/#js-sequence

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Object = kiesel.types.Object;
const PropertyKey = kiesel.types.PropertyKey;
const Value = kiesel.types.Value;
const getIteratorFromMethod = kiesel.types.getIteratorFromMethod;

pub fn convertValueToSequence(agent: *Agent, value: Value) Agent.Error![]const Value {
    // 1. If Type(V) is not Object, throw a TypeError.
    if (!value.isObject()) {
        return agent.throwException(.type_error, "{f} is not an Object", .{value});
    }

    // 2. Let method be ? GetMethod(V, %Symbol.iterator%).
    const method = try value.getMethod(
        agent,
        PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
    );

    // 3. If method is undefined, throw a TypeError.
    if (method == null) {
        return agent.throwException(.type_error, "Object has no Symbol.iterator method", .{});
    }

    // 4. Return the result of creating a sequence from V and method.
    return createSequenceFromIterable(agent, value, method.?);
}

/// 3.2.21.1. Creating a sequence from an iterable
/// https://webidl.spec.whatwg.org/#create-sequence-from-iterable
fn createSequenceFromIterable(
    agent: *Agent,
    iterable: Value,
    method: *Object,
) Agent.Error![]const Value {
    // 1. Let iteratorRecord be ? GetIteratorFromMethod(iterable, method).
    var iterator = try getIteratorFromMethod(agent, iterable, method);

    var sequence: std.ArrayList(Value) = .empty;

    // 2. Initialize i to be 0.
    // 3. Repeat
    while (try iterator.stepValue(agent)) |next| {
        // 1. Let next be ? IteratorStepValue(iteratorRecord).
        // 2. If next is done, then return an IDL sequence value of type sequence<T> of length i,
        //    where the value of the element at index j is Sj.
        // 4. Initialize Si to the result of converting next to an IDL value of type T.
        // 5. Set i to i + 1.
        try sequence.append(agent.gc_allocator, next);
    }
    return sequence.toOwnedSlice(agent.gc_allocator);
}
