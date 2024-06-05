//! 27.7 AsyncFunction Objects
//! https://tc39.es/ecma262/#sec-async-function-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ExecutionContext = execution.ExecutionContext;
const Object = types.Object;
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createDynamicFunction = builtins.createDynamicFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;

/// 27.7.2 Properties of the AsyncFunction Constructor
/// https://tc39.es/ecma262/#sec-async-function-constructor-properties
pub const AsyncFunctionConstructor = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = constructor }, .{
            .length = 1,
            .name = "AsyncFunction",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function%"(),
        });

        // 27.7.2.1 AsyncFunction.prototype
        // https://tc39.es/ecma262/#sec-async-function-constructor-prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AsyncFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 27.7.3.1 AsyncFunction.prototype.constructor
        // https://tc39.es/ecma262/#sec-async-function-prototype-properties-constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%AsyncFunction.prototype%"() catch unreachable,
            "constructor",
            PropertyDescriptor{
                .value = Value.from(object),
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        return object;
    }

    /// 27.7.1.1 AsyncFunction ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-async-function-constructor-arguments
    fn constructor(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const parameter_args = arguments.values[0..arguments.count() -| 1];
        const maybe_body_arg = arguments.getOrNull(arguments.count() -| 1);

        // 1. Let C be the active function object.
        const constructor_ = agent.activeFunctionObject();

        // 2. If bodyArg is not present, set bodyArg to the empty String.
        const body_arg = maybe_body_arg orelse Value.from("");

        // 3. Return ? CreateDynamicFunction(C, NewTarget, async, parameterArgs, bodyArg).
        return Value.from(try createDynamicFunction(
            agent,
            constructor_,
            new_target,
            .@"async",
            parameter_args,
            body_arg,
        ));
    }
};

/// 27.7.3 Properties of the AsyncFunction Prototype Object
/// https://tc39.es/ecma262/#sec-async-function-prototype-properties
pub const AsyncFunctionPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 27.7.3.2 AsyncFunction.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-async-function-prototype-properties-toStringTag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("AsyncFunction"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

/// 27.7.5.1 AsyncFunctionStart ( promiseCapability, asyncFunctionBody )
/// https://tc39.es/ecma262/#sec-async-functions-abstract-operations-async-function-start
pub fn asyncFunctionStart(
    agent: *Agent,
    promise_capability: PromiseCapability,
    async_function: *builtins.ECMAScriptFunction,
) Allocator.Error!void {
    // 1. Let runningContext be the running execution context.
    const running_context = agent.runningExecutionContext();

    // 2. Let asyncContext be a copy of runningContext.
    // 3. NOTE: Copying the execution state is required for AsyncBlockStart to resume its execution.
    //    It is ill-defined to resume a currently executing context.
    const async_context = running_context.*;

    // 4. Perform AsyncBlockStart(promiseCapability, asyncFunctionBody, asyncContext).
    try asyncBlockStart(agent, promise_capability, async_function, async_context);

    // 5. Return unused.
}

/// 27.7.5.2 AsyncBlockStart ( promiseCapability, asyncBody, asyncContext )
/// https://tc39.es/ecma262/#sec-asyncblockstart
pub fn asyncBlockStart(
    agent: *Agent,
    promise_capability: PromiseCapability,
    async_function: *builtins.ECMAScriptFunction,
    async_context: ExecutionContext,
) Allocator.Error!void {
    // 1. Let runningContext be the running execution context.
    const running_context = agent.runningExecutionContext();

    // 2. Let closure be a new Abstract Closure with no parameters that captures promiseCapability
    //    and asyncBody and performs the following steps when called:
    const closure = struct {
        fn func(
            agent_: *Agent,
            promise_capability_: PromiseCapability,
            async_function_: *builtins.ECMAScriptFunction,
        ) Allocator.Error!void {
            // a. Let acAsyncContext be the running execution context.

            // b. Let result be Completion(Evaluation of asyncBody).
            const result = async_function_.fields.generateAndRunBytecode(agent_);

            // c. Assert: If we return here, the async function either threw an exception or
            //    performed an implicit or explicit return; all awaiting is done.

            // d. Remove acAsyncContext from the execution context stack and restore the execution
            //    context that is at the top of the execution context stack as the running execution
            //    context.
            _ = agent_.execution_context_stack.pop();

            if (result) |completion| {
                std.debug.assert(completion.type == .normal or completion.type == .@"return");
                const value = completion.value orelse .undefined;

                // e. If result is a normal completion, then
                //     i. Perform ! Call(promiseCapability.[[Resolve]], undefined, « undefined »).
                // f. Else if result is a return completion, then
                //     i. Perform ! Call(promiseCapability.[[Resolve]], undefined, « result.[[Value]] »).
                _ = Value.from(promise_capability_.resolve).callAssumeCallable(
                    .undefined,
                    &.{value},
                ) catch |err| try noexcept(err);
            }
            // g. Else,
            else |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                // i. Assert: result is a throw completion.
                error.ExceptionThrown => {
                    const exception = agent_.clearException();

                    // ii. Perform ! Call(promiseCapability.[[Reject]], undefined, « result.[[Value]] »).
                    _ = Value.from(promise_capability_.reject).callAssumeCallable(
                        .undefined,
                        &.{exception},
                    ) catch |err_| try noexcept(err_);
                },
            }

            // h. Return unused.
        }
    }.func;

    // 3. Set the code evaluation state of asyncContext such that when evaluation is resumed for
    //    that execution context, closure will be called with no arguments.

    // 4. Push asyncContext onto the execution context stack; asyncContext is now the running
    //    execution context.
    try agent.execution_context_stack.append(async_context);

    // 5. Resume the suspended evaluation of asyncContext. Let result be the value returned by the
    //    resumed computation.
    const result = closure(agent, promise_capability, async_function);

    // 6. Assert: When we return here, asyncContext has already been removed from the execution
    //    context stack and runningContext is the currently running execution context.
    std.debug.assert(running_context == agent.runningExecutionContext());

    // 7. Assert: result is a normal completion with a value of unused. The possible sources of
    //    this value are Await or, if the async function doesn't await anything, step 3.h above.
    result catch |err| try noexcept(err);

    // 8. Return unused.
}
