//! 20.5 Error Objects
//! https://tc39.es/ecma262/#sec-error-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

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
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

const Self = @This();

/// 20.5.2 Properties of the Error Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-error-constructor
pub const ErrorConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 1,
            .name = "Error",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 20.5.2.1 Error.prototype
        // https://tc39.es/ecma262/#sec-error.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Error.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.5.3.1 Error.prototype.constructor
        // https://tc39.es/ecma262/#sec-error.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Error.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.5.1.1 Error ( message [ , options ] )
    /// https://tc39.es/ecma262/#sec-error-message
    fn behaviour(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const message = arguments.get(0);
        const options = arguments.get(1);

        // 1. If NewTarget is undefined, let newTarget be the active function object; else let
        //    newTarget be NewTarget.
        const new_target_ = new_target orelse agent.activeFunctionObject();

        // 2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%Error.prototype%", « [[ErrorData]] »).
        const object = try ordinaryCreateFromConstructor(
            Error,
            agent,
            new_target_,
            "%Error.prototype%",
            .{
                // Non-standard
                .error_data = .{ .name = String.fromLiteral("Error"), .message = String.empty },
            },
        );

        // Non-standard
        object.data.internal_methods.set = internalSet;

        // 3. If message is not undefined, then
        if (message != .undefined) {
            // a. Let msg be ? ToString(message).
            const msg = try message.toString(agent);

            // b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
            object.createNonEnumerableDataPropertyOrThrow(
                PropertyKey.from("message"),
                Value.from(msg),
            ) catch |err| try noexcept(err);

            object.as(Error).fields.error_data.message = msg;
        }

        // 4. Perform ? InstallErrorCause(O, options).
        try installErrorCause(agent, object, options);

        // 5. Return O.
        return Value.from(object);
    }
};

/// Custom [[Set]] to intercept .name and .message property changes
///
/// NOTE: Ignoring non-string values matches SpiderMonkey, V8 only remembers the original name and
///       message and doesn't act on property changes.
fn internalSet(
    object: Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool {
    if (property_key == .string and value == .string) {
        if (property_key.string.eql(String.fromLiteral("name"))) {
            object.as(Error).fields.error_data.name = value.string;
        } else if (property_key.string.eql(String.fromLiteral("message"))) {
            object.as(Error).fields.error_data.message = value.string;
        }
    }
    return builtins.ordinarySet(object, property_key, value, receiver);
}

/// 20.5.3 Properties of the Error Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-error-prototype-object
pub const ErrorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        // 20.5.3.2 Error.prototype.message
        // https://tc39.es/ecma262/#sec-error.prototype.message
        try defineBuiltinProperty(
            object,
            "message",
            Value.from(""),
        );

        // 20.5.3.3 Error.prototype.name
        // https://tc39.es/ecma262/#sec-error.prototype.name
        try defineBuiltinProperty(
            object,
            "name",
            Value.from("Error"),
        );

        try defineBuiltinFunction(object, "toString", toString, 0, realm);

        return object;
    }

    /// 20.5.3.4 Error.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-error.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (this_value != .object) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.object;

        // 3. Let name be ? Get(O, "name").
        const name = try object.get(PropertyKey.from("name"));

        // 4. If name is undefined, set name to "Error"; otherwise set name to ? ToString(name).
        const name_string = if (name == .undefined) String.fromLiteral("Error") else try name.toString(agent);

        // 5. Let msg be ? Get(O, "message").
        const msg = try object.get(PropertyKey.from("message"));

        // 6. If msg is undefined, set msg to the empty String; otherwise set msg to ? ToString(msg).
        const msg_string = if (msg == .undefined) String.empty else try msg.toString(agent);

        // 7. If name is the empty String, return msg.
        if (name_string.isEmpty()) return Value.from(msg_string);

        // 8. If msg is the empty String, return name.
        if (msg_string.isEmpty()) return Value.from(name_string);

        // 9. Return the string-concatenation of name, the code unit 0x003A (COLON), the code unit
        //    0x0020 (SPACE), and msg.
        return Value.from(
            try String.concat(
                agent.gc_allocator,
                &.{ name_string, String.fromLiteral(": "), msg_string },
            ),
        );
    }
};

/// 20.5.4 Properties of Error Instances
/// https://tc39.es/ecma262/#sec-properties-of-error-instances
pub const Error = MakeObject(.{
    .Fields = struct {
        // NOTE: [[ErrorData]] is undefined in the spec, we use it to store the name and message
        //       for pretty-printing purposes without property lookup.
        error_data: struct {
            name: String,
            message: String,
        },
    },
    .tag = .@"error",
});

/// 20.5.5.1 EvalError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-evalerror
pub const EvalError = MakeNativeError();
pub const EvalErrorConstructor = MakeNativeErrorConstructor("EvalError");
pub const EvalErrorPrototype = MakeNativeErrorPrototype("EvalError");

/// 20.5.5.2 RangeError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-rangeerror
pub const RangeError = MakeNativeError();
pub const RangeErrorConstructor = MakeNativeErrorConstructor("RangeError");
pub const RangeErrorPrototype = MakeNativeErrorPrototype("RangeError");

/// 20.5.5.3 ReferenceError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-referenceerror
pub const ReferenceError = MakeNativeError();
pub const ReferenceErrorConstructor = MakeNativeErrorConstructor("ReferenceError");
pub const ReferenceErrorPrototype = MakeNativeErrorPrototype("ReferenceError");

/// 20.5.5.4 SyntaxError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-syntaxerror
pub const SyntaxError = MakeNativeError();
pub const SyntaxErrorConstructor = MakeNativeErrorConstructor("SyntaxError");
pub const SyntaxErrorPrototype = MakeNativeErrorPrototype("SyntaxError");

/// 20.5.5.5 TypeError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-typeerror
pub const TypeError = MakeNativeError();
pub const TypeErrorConstructor = MakeNativeErrorConstructor("TypeError");
pub const TypeErrorPrototype = MakeNativeErrorPrototype("TypeError");

/// 20.5.5.6 URIError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-urierror
pub const URIError = MakeNativeError();
pub const URIErrorConstructor = MakeNativeErrorConstructor("URIError");
pub const URIErrorPrototype = MakeNativeErrorPrototype("URIError");

/// 20.5.6.2 Properties of the NativeError Constructors
/// https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-constructors
fn MakeNativeErrorConstructor(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) Allocator.Error!Object {
            const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
                .length = 1,
                .name = name,
                .realm = realm,
                .prototype = try realm.intrinsics.@"%Error%"(),
            });

            const prototypeFn = @field(Realm.Intrinsics, "%" ++ name ++ ".prototype%");

            // 20.5.6.2.1 NativeError.prototype
            // https://tc39.es/ecma262/#sec-nativeerror.prototype
            try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
                .value = Value.from(try prototypeFn(&realm.intrinsics)),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            });

            // 20.5.6.3.1 NativeError.prototype.constructor
            // https://tc39.es/ecma262/#sec-nativeerror.prototype.constructor
            try defineBuiltinProperty(
                prototypeFn(&realm.intrinsics) catch unreachable,
                "constructor",
                Value.from(object),
            );

            return object;
        }

        /// 20.5.6.1.1 NativeError ( message [ , options ] )
        /// https://tc39.es/ecma262/#sec-nativeerror
        fn behaviour(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
            const message = arguments.get(0);
            const options = arguments.get(1);

            const T = @field(Self, name);

            // 1. If NewTarget is undefined, let newTarget be the active function object; else let
            //    newTarget be NewTarget.
            const new_target_ = new_target orelse agent.activeFunctionObject();

            // 2. Let O be ? OrdinaryCreateFromConstructor(
            //      newTarget, "%NativeError.prototype%", « [[ErrorData]] »
            //    ).
            const object = try ordinaryCreateFromConstructor(
                T,
                agent,
                new_target_,
                "%" ++ name ++ ".prototype%",
                .{
                    // Non-standard
                    .error_data = .{ .name = String.fromLiteral(name), .message = String.empty },
                },
            );

            // Non-standard
            object.data.internal_methods.set = internalSet;

            // 3. If message is not undefined, then
            if (message != .undefined) {
                // a. Let msg be ? ToString(message).
                const msg = try message.toString(agent);

                // b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
                object.createNonEnumerableDataPropertyOrThrow(
                    PropertyKey.from("message"),
                    Value.from(msg),
                ) catch |err| try noexcept(err);

                object.as(T).fields.error_data.message = msg;
            }

            // 4. Perform ? InstallErrorCause(O, options).
            try installErrorCause(agent, object, options);

            // 5. Return O.
            return Value.from(object);
        }
    };
}

/// 20.5.6.3 Properties of the NativeError Prototype Objects
/// https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-prototype-objects
fn MakeNativeErrorPrototype(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) Allocator.Error!Object {
            const object = try builtins.Object.create(realm.agent, .{
                .prototype = try realm.intrinsics.@"%Error.prototype%"(),
            });

            // 20.5.6.3.2 NativeError.prototype.message
            // https://tc39.es/ecma262/#sec-nativeerror.prototype.message
            try defineBuiltinProperty(
                object,
                "message",
                Value.from(""),
            );

            // 20.5.6.3.3 NativeError.prototype.name
            // https://tc39.es/ecma262/#sec-nativeerror.prototype.name
            try defineBuiltinProperty(
                object,
                "name",
                Value.from(name),
            );

            return object;
        }
    };
}

/// 20.5.6.4 Properties of NativeError Instances
/// https://tc39.es/ecma262/#sec-properties-of-nativeerror-instances
fn MakeNativeError() type {
    return MakeObject(.{
        // NOTE: This shares a tag with the plain Error objects as it is identified by the same
        //       internal slot in the spec and thus subtypes are not distinguishable. For this
        //       reason the Fields type must be identical for Object.as() casts to work.
        .Fields = Error.Fields,
        .tag = .@"error",
    });
}

/// 20.5.7.1 The AggregateError Constructor
/// https://tc39.es/ecma262/#sec-aggregate-error-constructor
pub const AggregateErrorConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 2,
            .name = "AggregateError",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Error%"(),
        });

        // 20.5.7.2.1 AggregateError.prototype
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AggregateError.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.5.7.3.1 AggregateError.prototype.constructor
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%AggregateError.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.5.7.1.1 AggregateError ( errors, message [ , options ] )
    /// https://tc39.es/ecma262/#sec-aggregate-error
    fn behaviour(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const errors = arguments.get(0);
        const message = arguments.get(1);
        const options = arguments.get(2);

        // 1. If NewTarget is undefined, let newTarget be the active function object; else let
        //    newTarget be NewTarget.
        const new_target_ = new_target orelse agent.activeFunctionObject();

        // 2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%AggregateError.prototype%", « [[ErrorData]] »).
        const object = try ordinaryCreateFromConstructor(
            AggregateError,
            agent,
            new_target_,
            "%AggregateError.prototype%",
            .{
                // Non-standard
                .error_data = .{ .name = String.fromLiteral("AggregateError"), .message = String.empty },
            },
        );

        // Non-standard
        object.data.internal_methods.set = internalSet;

        // 3. If message is not undefined, then
        if (message != .undefined) {
            // a. Let msg be ? ToString(message).
            const msg = try message.toString(agent);

            // b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
            object.createNonEnumerableDataPropertyOrThrow(
                PropertyKey.from("message"),
                Value.from(msg),
            ) catch |err| try noexcept(err);

            object.as(Error).fields.error_data.message = msg;
        }

        // 4. Perform ? InstallErrorCause(O, options).
        try installErrorCause(agent, object, options);

        // 5. Let errorsList be ? IteratorToList(? GetIterator(errors, sync)).
        var iterator = try getIterator(agent, errors, .sync);
        const errors_list = try iterator.toList();
        defer agent.gc_allocator.free(errors_list);

        // 6. Perform ! DefinePropertyOrThrow(O, "errors", PropertyDescriptor {
        //      [[Configurable]]: true, [[Enumerable]]: false, [[Writable]]: true,
        //      [[Value]]: CreateArrayFromList(errorsList)
        //    }).
        object.definePropertyOrThrow(
            PropertyKey.from("errors"),
            .{
                .configurable = true,
                .enumerable = false,
                .writable = true,
                .value = Value.from(try createArrayFromList(agent, errors_list)),
            },
        ) catch |err| try noexcept(err);

        // 7. Return O.
        return Value.from(object);
    }
};

/// 20.5.7.3 Properties of the AggregateError Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-aggregate-error-prototype-objects
pub const AggregateErrorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Error.prototype%"(),
        });

        // 20.5.7.3.2 AggregateError.prototype.message
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.message
        try defineBuiltinProperty(
            object,
            "message",
            Value.from(""),
        );

        // 20.5.7.3.3 AggregateError.prototype.name
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.name
        try defineBuiltinProperty(
            object,
            "name",
            Value.from("AggregateError"),
        );

        return object;
    }
};

/// 20.5.7.4 Properties of AggregateError Instances
/// https://tc39.es/ecma262/#sec-properties-of-aggregate-error-instances
pub const AggregateError = MakeObject(.{
    // NOTE: This shares a tag with the plain Error objects as it is identified by the same
    //       internal slot in the spec and thus subtypes are not distinguishable. For this
    //       reason the Fields type must be identical for Object.as() casts to work.
    .Fields = Error.Fields,
    .tag = .@"error",
});

/// 20.5.8.1 InstallErrorCause ( O, options )
/// https://tc39.es/ecma262/#sec-installerrorcause
fn installErrorCause(agent: *Agent, object: Object, options: Value) Agent.Error!void {
    // 1. If options is an Object and ? HasProperty(options, "cause") is true, then
    if (options == .object and try options.object.hasProperty(PropertyKey.from("cause"))) {
        // a. Let cause be ? Get(options, "cause").
        const cause = try options.get(agent, PropertyKey.from("cause"));

        // b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "cause", cause).
        object.createNonEnumerableDataPropertyOrThrow(
            PropertyKey.from("cause"),
            cause,
        ) catch |err| try noexcept(err);
    }

    // 2. Return unused.
}
