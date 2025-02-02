//! 20.5 Error Objects
//! https://tc39.es/ecma262/#sec-error-objects

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
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 20.5.2 Properties of the Error Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-error-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 1,
            .name = "Error",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "isError", isError, 1, realm);

        // 20.5.2.2 Error.prototype
        // https://tc39.es/ecma262/#sec-error.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Error.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 20.5.1.1 Error ( message [ , options ] )
    /// https://tc39.es/ecma262/#sec-error-message
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
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
                .error_data = .{ .name = String.fromLiteral("Error"), .message = .empty },
            },
        );

        // Non-standard
        object.internal_methods = try Object.InternalMethods.create(agent.gc_allocator, object.internal_methods, &.{ .set = internalSet });

        // 3. If message is not undefined, then
        if (!message.isUndefined()) {
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

    /// 20.5.2.1 Error.isError ( arg )
    /// https://tc39.es/ecma262/#sec-error.iserror
    fn isError(_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const arg = arguments.get(0);

        // 1. If arg is not an Object, return false.
        if (!arg.isObject()) return Value.from(false);

        // 2. If arg does not have an [[ErrorData]] internal slot, return false.
        // 3. Return true.
        return Value.from(arg.asObject().is(builtins.Error));
    }
};

/// Custom [[Set]] to intercept .name and .message property changes
///
/// NOTE: Ignoring non-string values matches SpiderMonkey, V8 only remembers the original name and
///       message and doesn't act on property changes.
pub fn internalSet(
    agent: *Agent,
    object: *Object,
    property_key: PropertyKey,
    value: Value,
    receiver: Value,
) Agent.Error!bool {
    if (property_key == .string and value.isString()) {
        if (property_key.string.eql(String.fromLiteral("name"))) {
            object.as(Error).fields.error_data.name = value.asString();
        } else if (property_key.string.eql(String.fromLiteral("message"))) {
            object.as(Error).fields.error_data.message = value.asString();
        }
    }
    return builtins.ordinarySet(agent, object, property_key, value, receiver);
}

/// 20.5.3 Properties of the Error Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-error-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.5.3.1 Error.prototype.constructor
        // https://tc39.es/ecma262/#sec-error.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%Error%"()),
        );

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
    }

    /// 20.5.3.4 Error.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-error.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let O be the this value.
        // 2. If O is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{} is not an Object", .{this_value});
        }
        const object = this_value.asObject();

        // 3. Let name be ? Get(O, "name").
        const name = try object.get(PropertyKey.from("name"));

        // 4. If name is undefined, set name to "Error"; otherwise set name to ? ToString(name).
        const name_string = if (name.isUndefined()) String.fromLiteral("Error") else try name.toString(agent);

        // 5. Let msg be ? Get(O, "message").
        const msg = try object.get(PropertyKey.from("message"));

        // 6. If msg is undefined, set msg to the empty String; otherwise set msg to ? ToString(msg).
        const msg_string: *const String = if (msg.isUndefined()) .empty else try msg.toString(agent);

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
            name: *const String,
            message: *const String,
        },
    },
    .tag = .@"error",
});

/// 20.5.5.1 EvalError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-evalerror
pub const eval_error = struct {
    pub const constructor = MakeNativeErrorConstructor("EvalError");
    pub const prototype = MakeNativeErrorPrototype("EvalError");
    pub const EvalError = MakeNativeError("EvalError");
};

/// 20.5.5.2 RangeError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-rangeerror
pub const range_error = struct {
    pub const constructor = MakeNativeErrorConstructor("RangeError");
    pub const prototype = MakeNativeErrorPrototype("RangeError");
    pub const RangeError = MakeNativeError("RangeError");
};

/// 20.5.5.3 ReferenceError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-referenceerror
pub const reference_error = struct {
    pub const constructor = MakeNativeErrorConstructor("ReferenceError");
    pub const prototype = MakeNativeErrorPrototype("ReferenceError");
    pub const ReferenceError = MakeNativeError("ReferenceError");
};

/// 20.5.5.4 SyntaxError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-syntaxerror
pub const syntax_error = struct {
    pub const constructor = MakeNativeErrorConstructor("SyntaxError");
    pub const prototype = MakeNativeErrorPrototype("SyntaxError");
    pub const SyntaxError = MakeNativeError("SyntaxError");
};

/// 20.5.5.5 TypeError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-typeerror
pub const type_error = struct {
    pub const constructor = MakeNativeErrorConstructor("TypeError");
    pub const prototype = MakeNativeErrorPrototype("TypeError");
    pub const TypeError = MakeNativeError("TypeError");
};

/// 20.5.5.6 URIError
/// https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-urierror
pub const uri_error = struct {
    pub const constructor = MakeNativeErrorConstructor("URIError");
    pub const prototype = MakeNativeErrorPrototype("URIError");
    pub const URIError = MakeNativeError("URIError");
};

/// 20.5.6.2 Properties of the NativeError Constructors
/// https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-constructors
fn MakeNativeErrorConstructor(comptime name: []const u8) type {
    return struct {
        pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
            return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
                .length = 1,
                .name = name,
                .realm = realm,
                .prototype = try realm.intrinsics.@"%Error%"(),
            });
        }

        pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
            const prototypeFn = @field(Realm.Intrinsics, "%" ++ name ++ ".prototype%");

            // 20.5.6.2.1 NativeError.prototype
            // https://tc39.es/ecma262/#sec-nativeerror.prototype
            try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
                .value = Value.from(try prototypeFn(&realm.intrinsics)),
                .writable = false,
                .enumerable = false,
                .configurable = false,
            });
        }

        /// 20.5.6.1.1 NativeError ( message [ , options ] )
        /// https://tc39.es/ecma262/#sec-nativeerror
        fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
            const message = arguments.get(0);
            const options = arguments.get(1);

            const namespace = std.StaticStringMap(type).initComptime(.{
                .{ "EvalError", eval_error },
                .{ "RangeError", range_error },
                .{ "ReferenceError", reference_error },
                .{ "SyntaxError", syntax_error },
                .{ "TypeError", type_error },
                .{ "URIError", uri_error },
            }).get(name).?;
            const T = @field(namespace, name);

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
                    .error_data = .{ .name = String.fromLiteral(name), .message = .empty },
                },
            );

            // Non-standard
            object.internal_methods = try Object.InternalMethods.create(agent.gc_allocator, object.internal_methods, &.{ .set = internalSet });

            // 3. If message is not undefined, then
            if (!message.isUndefined()) {
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
        pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
            return builtins.Object.create(realm.agent, .{
                .prototype = try realm.intrinsics.@"%Error.prototype%"(),
            });
        }

        pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
            const constructorFn = @field(Realm.Intrinsics, "%" ++ name ++ "%");

            // 20.5.6.3.1 NativeError.prototype.constructor
            // https://tc39.es/ecma262/#sec-nativeerror.prototype.constructor
            try defineBuiltinProperty(
                object,
                "constructor",
                Value.from(try constructorFn(&realm.intrinsics)),
            );

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
        }
    };
}

/// 20.5.6.4 Properties of NativeError Instances
/// https://tc39.es/ecma262/#sec-properties-of-nativeerror-instances
fn MakeNativeError(comptime _: []const u8) type {
    return MakeObject(.{
        // NOTE: This shares a tag with the plain Error objects as it is identified by the same
        //       internal slot in the spec and thus subtypes are not distinguishable. For this
        //       reason the Fields type must be identical for Object.as() casts to work.
        .Fields = Error.Fields,
        .tag = .@"error",
    });
}

/// 20.5.8.1 InstallErrorCause ( O, options )
/// https://tc39.es/ecma262/#sec-installerrorcause
pub fn installErrorCause(agent: *Agent, object: *Object, options: Value) Agent.Error!void {
    // 1. If options is an Object and ? HasProperty(options, "cause") is true, then
    if (options.isObject() and try options.asObject().hasProperty(PropertyKey.from("cause"))) {
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
