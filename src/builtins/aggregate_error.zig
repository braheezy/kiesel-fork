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
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getIterator = types.getIterator;
const installErrorCause = builtins.installErrorCause;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

/// 20.5.7.1 The AggregateError Constructor
/// https://tc39.es/ecma262/#sec-aggregate-error-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            2,
            "AggregateError",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Error%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.5.7.2.1 AggregateError.prototype
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%AggregateError.prototype%"()),
            .none,
        );
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
        const aggregate_error = try ordinaryCreateFromConstructor(
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
        std.debug.assert(aggregate_error.object.internal_methods == Object.InternalMethods.default);
        aggregate_error.object.internal_methods = .initComptime(.{ .set = builtins.@"error".internalSet });

        // 3. If message is not undefined, then
        if (!message.isUndefined()) {
            // a. Let msg be ? ToString(message).
            const msg = try message.toString(agent);

            // b. Perform CreateNonEnumerableDataPropertyOrThrow(O, "message", msg).
            aggregate_error.object.createNonEnumerableDataPropertyOrThrow(
                agent,
                PropertyKey.from("message"),
                Value.from(msg),
            ) catch |err| try noexcept(err);

            aggregate_error.fields.error_data.message = msg;
        }

        // 4. Perform ? InstallErrorCause(O, options).
        try installErrorCause(agent, &aggregate_error.object, options);

        // 5. Let errorsList be ? IteratorToList(? GetIterator(errors, sync)).
        var iterator = try getIterator(agent, errors, .sync);
        const errors_list = try iterator.toList(agent);
        defer agent.gc_allocator.free(errors_list);

        // 6. Perform ! DefinePropertyOrThrow(O, "errors", PropertyDescriptor {
        //      [[Configurable]]: true, [[Enumerable]]: false, [[Writable]]: true,
        //      [[Value]]: CreateArrayFromList(errorsList)
        //    }).
        const errors_list_array = try createArrayFromList(agent, errors_list);
        try aggregate_error.object.definePropertyDirect(
            agent,
            PropertyKey.from("errors"),
            .{
                .value_or_accessor = .{
                    .value = Value.from(&errors_list_array.object),
                },
                .attributes = .builtin_default,
            },
        );

        // 7. Return O.
        return Value.from(&aggregate_error.object);
    }
};

/// 20.5.7.3 Properties of the AggregateError Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-aggregate-error-prototype-objects
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Error.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.5.7.3.1 AggregateError.prototype.constructor
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%AggregateError%"()),
        );

        // 20.5.7.3.2 AggregateError.prototype.message
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.message
        try object.defineBuiltinProperty(
            agent,
            "message",
            Value.from(""),
        );

        // 20.5.7.3.3 AggregateError.prototype.name
        // https://tc39.es/ecma262/#sec-aggregate-error.prototype.name
        try object.defineBuiltinProperty(
            agent,
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
    .display_name = "AggregateError",
});
