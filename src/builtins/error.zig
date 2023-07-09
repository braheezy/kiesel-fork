//! 20.5 Error Objects
//! https://tc39.es/ecma262/#sec-error-objects

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
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

const Self = @This();

/// 20.5.2 Properties of the Error Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-error-constructor
pub const ErrorConstructor = struct {
    pub fn create(realm: *Realm) !Object {
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
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, maybe_new_target: ?Object) !Value {
        const message = arguments.get(0);
        const options = arguments.get(1);

        // 1. If NewTarget is undefined, let newTarget be the active function object; else let
        //    newTarget be NewTarget.
        const new_target = maybe_new_target orelse agent.activeFunctionObject();

        // 2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%Error.prototype%", « [[ErrorData]] »).
        const object = try ordinaryCreateFromConstructor(
            Error,
            agent,
            new_target,
            "%Error.prototype%",
        );

        // Non-standard
        object.as(Error).fields = .{ .error_data = .{ .name = "Error", .message = "" } };
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
fn internalSet(object: Object, property_key: PropertyKey, value: Value, receiver: Value) !bool {
    if (property_key == .string and value == .string) {
        if (std.mem.eql(u8, property_key.string, "name")) {
            object.as(Error).fields.error_data.name = value.string;
        } else if (std.mem.eql(u8, property_key.string, "message")) {
            object.as(Error).fields.error_data.message = value.string;
        }
    }
    return builtins.ordinarySet(object, property_key, value, receiver);
}

/// 20.5.3 Properties of the Error Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-error-prototype-object
pub const ErrorPrototype = struct {
    pub fn create(realm: *Realm) !Object {
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
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (this_value != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{this_value}),
            );
        }
        const object = this_value.object;

        // 3. Let name be ? Get(O, "name").
        const name = try object.get(PropertyKey.from("name"));

        // 4. If name is undefined, set name to "Error"; otherwise set name to ? ToString(name).
        const name_string = if (name == .undefined) "Error" else try name.toString(agent);

        // 5. Let msg be ? Get(O, "message").
        const msg = try object.get(PropertyKey.from("message"));

        // 6. If msg is undefined, set msg to the empty String; otherwise set msg to ? ToString(msg).
        const msg_string = if (msg == .undefined) "" else try msg.toString(agent);

        // 7. If name is the empty String, return msg.
        if (name_string.len == 0) return Value.from(msg_string);

        // 8. If msg is the empty String, return name.
        if (msg_string.len == 0) return Value.from(name_string);

        // 9. Return the string-concatenation of name, the code unit 0x003A (COLON), the code unit
        //    0x0020 (SPACE), and msg.
        return Value.from(
            try std.fmt.allocPrint(agent.gc_allocator, "{s}: {s}", .{ name_string, msg_string }),
        );
    }
};

/// 20.5.4 Properties of Error Instances
/// https://tc39.es/ecma262/#sec-properties-of-error-instances
pub const Error = Object.Factory(.{
    .Fields = struct {
        // NOTE: [[ErrorData]] is undefined in the spec, we use it to store the name and message
        //       for pretty-printing purposes without property lookup.
        error_data: struct {
            name: []const u8,
            message: []const u8,
        },
    },
    .tag = .@"error",
});

/// 20.5.5.1 EvalError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-evalerror
pub const EvalError = NativeError();
pub const EvalErrorConstructor = NativeErrorConstructor("EvalError");
pub const EvalErrorPrototype = NativeErrorPrototype("EvalError");

/// 20.5.5.2 RangeError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-rangeerror
pub const RangeError = NativeError();
pub const RangeErrorConstructor = NativeErrorConstructor("RangeError");
pub const RangeErrorPrototype = NativeErrorPrototype("RangeError");

/// 20.5.5.3 ReferenceError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-referenceerror
pub const ReferenceError = NativeError();
pub const ReferenceErrorConstructor = NativeErrorConstructor("ReferenceError");
pub const ReferenceErrorPrototype = NativeErrorPrototype("ReferenceError");

/// 20.5.5.4 SyntaxError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-syntaxerror
pub const SyntaxError = NativeError();
pub const SyntaxErrorConstructor = NativeErrorConstructor("SyntaxError");
pub const SyntaxErrorPrototype = NativeErrorPrototype("SyntaxError");

/// 20.5.5.5 TypeError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-typeerror
pub const TypeError = NativeError();
pub const TypeErrorConstructor = NativeErrorConstructor("TypeError");
pub const TypeErrorPrototype = NativeErrorPrototype("TypeError");

/// 20.5.5.6 URIError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-urierror
pub const URIError = NativeError();
pub const URIErrorConstructor = NativeErrorConstructor("URIError");
pub const URIErrorPrototype = NativeErrorPrototype("URIError");

/// 20.5.6.2 Properties of the NativeError Constructors
/// https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-constructors
fn NativeErrorConstructor(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) !Object {
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
        fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, maybe_new_target: ?Object) !Value {
            const message = arguments.get(0);
            const options = arguments.get(1);

            const T = @field(Self, name);

            // 1. If NewTarget is undefined, let newTarget be the active function object; else let
            //    newTarget be NewTarget.
            const new_target = maybe_new_target orelse agent.activeFunctionObject();

            // 2. Let O be ? OrdinaryCreateFromConstructor(
            //      newTarget, "%NativeError.prototype%", « [[ErrorData]] »
            //    ).
            const object = try ordinaryCreateFromConstructor(
                T,
                agent,
                new_target,
                "%" ++ name ++ ".prototype%",
            );

            const native_error = @as(*T, @ptrCast(@alignCast(object.ptr)));

            // Non-standard
            native_error.fields = .{ .error_data = .{ .name = name, .message = "" } };
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

                native_error.fields.error_data.message = msg;
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
fn NativeErrorPrototype(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) !Object {
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
fn NativeError() type {
    return Object.Factory(.{
        // NOTE: This shares a tag with the plain Error objects as it is identified by the same
        //       internal slot in the spec and thus subtypes are not distinguishable. For this
        //       reason the Fields type must be identical for Object.as() casts to work.
        .Fields = Error.Fields,
        .tag = .@"error",
    });
}

/// 20.5.8.1 InstallErrorCause ( O, options )
/// https://tc39.es/ecma262/#sec-installerrorcause
fn installErrorCause(agent: *Agent, object: Object, options: Value) !void {
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
