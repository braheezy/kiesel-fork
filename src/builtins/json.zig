//! 25.5 The JSON Object
//! https://tc39.es/ecma262/#sec-json-object

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const arrayCreate = builtins.arrayCreate;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// Recursively convert a `std.json.Value` to a JS `Value`.
fn convertJsonValue(agent: *Agent, value: std.json.Value) !Value {
    return switch (value) {
        .null => .null,
        inline .bool, .integer, .float => |x| Value.from(x),
        .string => |x| Value.from(try agent.gc_allocator.dupe(u8, x)),
        .number_string => |x| Value.from(std.fmt.parseFloat(f64, x) catch unreachable),
        .array => |x| blk: {
            const array = try arrayCreate(agent, 0, null);
            for (x.items, 0..) |value_i, i| {
                array.createDataPropertyOrThrow(
                    PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))),
                    try convertJsonValue(agent, value_i),
                ) catch |err| try noexcept(err);
            }
            break :blk Value.from(array);
        },
        .object => |x| blk: {
            const realm = agent.currentRealm();
            const object = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%Object.prototype%"(),
            );
            var it = x.iterator();
            while (it.next()) |entry| {
                object.createDataPropertyOrThrow(
                    PropertyKey.from(entry.key_ptr.*),
                    try convertJsonValue(agent, entry.value_ptr.*),
                ) catch |err| try noexcept(err);
            }
            break :blk Value.from(object);
        },
    };
}

/// 25.5.1.1 InternalizeJSONProperty ( holder, name, reviver )
/// https://tc39.es/ecma262/#sec-internalizejsonproperty
fn internalizeJSONProperty(
    agent: *Agent,
    holder: Object,
    name: PropertyKey,
    reviver: Object,
) !Value {
    // 1. Let val be ? Get(holder, name).
    const value = try holder.get(name);

    // 2. If val is an Object, then
    if (value == .object) {
        // a. Let isArray be ? IsArray(val).
        const is_array = try value.isArray();

        // b. If isArray is true, then
        if (is_array) {
            // i. Let len be ? LengthOfArrayLike(val).
            const len = try value.object.lengthOfArrayLike();

            // ii. Let I be 0.
            var i: u53 = 0;

            // iii. Repeat, while I < len,
            while (i < len) : (i += 1) {
                // 1. Let prop be ! ToString(𝔽(I)).
                const property_key = PropertyKey.from(i);

                // 2. Let newElement be ? InternalizeJSONProperty(val, prop, reviver).
                const new_element = try internalizeJSONProperty(
                    agent,
                    value.object,
                    property_key,
                    reviver,
                );

                // 3. If newElement is undefined, then
                if (new_element == .undefined) {
                    // a. Perform ? val.[[Delete]](prop).
                    _ = try value.object.internalMethods().delete(value.object, property_key);
                }
                // 4. Else,
                else {
                    // a. Perform ? CreateDataProperty(val, prop, newElement).
                    _ = try value.object.createDataProperty(property_key, new_element);
                }

                // 5. Set I to I + 1.
            }
        }
        // c. Else,
        else {
            // i. Let keys be ? EnumerableOwnProperties(val, key).
            const keys = try value.object.enumerableOwnProperties(.key);
            defer keys.deinit();

            // ii. For each String P of keys, do
            for (keys.items) |key| {
                const property_key = try key.toPropertyKey(agent);

                // 1. Let newElement be ? InternalizeJSONProperty(val, P, reviver).
                const new_element = try internalizeJSONProperty(
                    agent,
                    value.object,
                    property_key,
                    reviver,
                );

                // 2. If newElement is undefined, then
                if (new_element == .undefined) {
                    // a. Perform ? val.[[Delete]](P).
                    _ = try value.object.internalMethods().delete(value.object, property_key);
                }
                // 3. Else,
                else {
                    // a. Perform ? CreateDataProperty(val, P, newElement).
                    _ = try value.object.createDataProperty(property_key, new_element);
                }
            }
        }
    }

    // 3. Return ? Call(reviver, holder, « name, val »).
    return Value.from(reviver).callAssumeCallable(
        Value.from(holder),
        .{ name.toValue(agent) catch unreachable, value },
    );
}

pub const JSON = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 25.5.3 JSON [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-json-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("JSON"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        try defineBuiltinFunction(object, "parse", parse, 2, realm);

        return object;
    }

    /// 25.5.1 JSON.parse ( text [ , reviver ] )
    /// https://tc39.es/ecma262/#sec-json.parse
    fn parse(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const realm = agent.currentRealm();
        const text = arguments.get(0);
        const reviver = arguments.get(1);

        // 1. Let jsonString be ? ToString(text).
        const json_string = try text.toString(agent);

        // 2. Parse StringToCodePoints(jsonString) as a JSON text as specified in ECMA-404. Throw
        //    a SyntaxError exception if it is not a valid JSON text as defined in that specification.
        // 3. Let scriptString be the string-concatenation of "(", jsonString, and ");".
        // 4. Let script be ParseText(StringToCodePoints(scriptString), Script).
        // 5. NOTE: The early error rules defined in 13.2.5.1 have special handling for the above
        //    invocation of ParseText.
        // 6. Assert: script is a Parse Node.
        // 7. Let completion be Completion(Evaluation of script).
        // 8. NOTE: The PropertyDefinitionEvaluation semantics defined in 13.2.5.5 have special
        //    handling for the above evaluation.
        const completion = std.json.parseFromSlice(
            std.json.Value,
            agent.gc_allocator,
            json_string.utf8,
            .{},
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return agent.throwException(.syntax_error, "Invalid JSON document"),
        };
        defer completion.deinit();

        // 9. Let unfiltered be completion.[[Value]].
        const unfiltered = completion.value;

        // 10. Assert: unfiltered is either a String, a Number, a Boolean, an Object that is
        //     defined by either an ArrayLiteral or an ObjectLiteral, or null.

        // 11. If IsCallable(reviver) is true, then
        if (reviver.isCallable()) {
            // a. Let root be OrdinaryObjectCreate(%Object.prototype%).
            const root = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%Object.prototype%"(),
            );

            // b. Let rootName be the empty String.
            const root_name = PropertyKey.from("");

            // c. Perform ! CreateDataPropertyOrThrow(root, rootName, unfiltered).
            root.createDataPropertyOrThrow(
                root_name,
                try convertJsonValue(agent, unfiltered),
            ) catch |err| try noexcept(err);

            // d. Return ? InternalizeJSONProperty(root, rootName, reviver).
            return internalizeJSONProperty(agent, root, root_name, reviver.object);
        }
        // 12. Else,
        else {
            // a. Return unfiltered.
            return convertJsonValue(agent, unfiltered);
        }
    }
};
