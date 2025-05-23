//! 10.4.1 Bound Function Exotic Objects
//! https://tc39.es/ecma262/#sec-bound-function-exotic-objects

const std = @import("std");

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Value = types.Value;

/// 10.4.1.1 [[Call]] ( thisArgument, argumentsList )
/// https://tc39.es/ecma262/#sec-bound-function-exotic-objects-call-thisargument-argumentslist
fn call(agent: *Agent, object: *Object, _: Value, arguments_list: Arguments) Agent.Error!Value {
    const function = object.as(BoundFunction);

    // 1. Let target be F.[[BoundTargetFunction]].
    const target = function.fields.bound_target_function;

    // 2. Let boundThis be F.[[BoundThis]].
    const bound_this = function.fields.bound_this;

    // 3. Let boundArgs be F.[[BoundArguments]].
    const bound_args = function.fields.bound_arguments;

    // 4. Let args be the list-concatenation of boundArgs and argumentsList.
    const args = try std.mem.concat(
        agent.gc_allocator,
        Value,
        &.{ bound_args, arguments_list.values },
    );
    defer agent.gc_allocator.free(args);

    // 5. Return ? Call(target, boundThis, args).
    return Value.from(target).callAssumeCallable(agent, bound_this, args);
}

/// 10.4.1.2 [[Construct]] ( argumentsList, newTarget )
/// https://tc39.es/ecma262/#sec-bound-function-exotic-objects-construct-argumentslist-newtarget
fn construct(
    agent: *Agent,
    object: *Object,
    arguments_list: Arguments,
    new_target: *Object,
) Agent.Error!*Object {
    const function = object.as(BoundFunction);

    // 1. Let target be F.[[BoundTargetFunction]].
    const target = function.fields.bound_target_function;

    // 2. Assert: IsConstructor(target) is true.
    std.debug.assert(Value.from(target).isConstructor());

    // 3. Let boundArgs be F.[[BoundArguments]].
    const bound_args = function.fields.bound_arguments;

    // 4. Let args be the list-concatenation of boundArgs and argumentsList.
    const args = try std.mem.concat(
        agent.gc_allocator,
        Value,
        &.{ bound_args, arguments_list.values },
    );
    defer agent.gc_allocator.free(args);

    // 5. If SameValue(F, newTarget) is true, set newTarget to target.
    const new_target_ = if (object == new_target) target else new_target;

    // 6. Return ? Construct(target, args, newTarget).
    return target.construct(agent, args, new_target_);
}

/// 10.4.1.3 BoundFunctionCreate ( targetFunction, boundThis, boundArgs )
/// https://tc39.es/ecma262/#sec-boundfunctioncreate
pub fn boundFunctionCreate(
    agent: *Agent,
    target_function: *Object,
    bound_this: Value,
    bound_args: []const Value,
) Agent.Error!*Object {
    // 1. Let proto be ? targetFunction.[[GetPrototypeOf]]().
    const prototype = try target_function.internal_methods.getPrototypeOf(agent, target_function);

    // 2. Let internalSlotsList be the list-concatenation of « [[Prototype]], [[Extensible]] » and
    //    the internal slots listed in Table 31.
    // 3. Let obj be MakeBasicObject(internalSlotsList).
    const object = try BoundFunction.create(agent, .{
        // 4. Set obj.[[Prototype]] to proto.
        .prototype = prototype,

        .internal_methods = &.{
            // 5. Set obj.[[Call]] as described in 10.4.1.1.
            .call = call,
        },

        .fields = .{
            // 7. Set obj.[[BoundTargetFunction]] to targetFunction.
            .bound_target_function = target_function,

            // 8. Set obj.[[BoundThis]] to boundThis.
            .bound_this = bound_this,

            // 9. Set obj.[[BoundArguments]] to boundArgs.
            .bound_arguments = try agent.gc_allocator.dupe(Value, bound_args),
        },
    });

    // 6. If IsConstructor(targetFunction) is true, then
    if (Value.from(target_function).isConstructor()) {
        object.internal_methods = &.{
            // NOTE: We have to duplicate step 5 to create a static vtable.
            .call = call,
            // a. Set obj.[[Construct]] as described in 10.4.1.2.
            .construct = construct,
        };
    }

    // 10. Return obj.
    return object;
}

/// Table 31: Internal Slots of Bound Function Exotic Objects
/// https://tc39.es/ecma262/#table-internal-slots-of-bound-function-exotic-objects
pub const BoundFunction = MakeObject(.{
    .Fields = struct {
        /// [[BoundTargetFunction]]
        bound_target_function: *Object,

        /// [[BoundThis]]
        bound_this: Value,

        /// [[BoundArguments]]
        bound_arguments: []const Value,
    },
    .tag = .bound_function,
});
