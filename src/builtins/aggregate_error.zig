//! 20.5.7 AggregateError Objects
//! https://tc39.es/ecma262/#sec-aggregate-error-objects

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
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getIterator = types.getIterator;
const installErrorCause = builtins.installErrorCause;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 20.5.7.1 The AggregateError Constructor
/// https://tc39.es/ecma262/#sec-aggregate-error-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 2,
            .name = "AggregateError",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Error%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.5.7.2.1 AggregateError.prototype
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AggregateError.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 20.5.7.1.1 AggregateError ( errors, message [ , options ] )
    /// https://tc39.es/ecma262/#sec-aggregate-error
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
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
                .error_data = .{ .name = String.fromLiteral("AggregateError"), .message = .empty },
            },
        );

        // Non-standard
        object.internal_methods = try Object.InternalMethods.create(
            agent.gc_allocator,
            object.internal_methods,
            &.{ .set = builtins.@"error".internalSet },
        );

        // 3. If message is not undefined, then
        if (!message.isUndefined()) {
            // a. Let msg be ? ToString(message).
            const msg = try message.toString(agent);

            // b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
            object.createNonEnumerableDataPropertyOrThrow(
                PropertyKey.from("message"),
                Value.from(msg),
            ) catch |err| try noexcept(err);

            object.as(builtins.Error).fields.error_data.message = msg;
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
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Error.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.5.7.3.1 AggregateError.prototype.constructor
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%AggregateError%"()),
        );

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
    }
};

/// 20.5.7.4 Properties of AggregateError Instances
/// https://tc39.es/ecma262/#sec-properties-of-aggregate-error-instances
pub const AggregateError = MakeObject(.{
    // NOTE: This shares a tag with the plain Error objects as it is identified by the same
    //       internal slot in the spec and thus subtypes are not distinguishable. For this
    //       reason the Fields type must be identical for Object.as() casts to work.
    .Fields = builtins.Error.Fields,
    .tag = .@"error",
});
