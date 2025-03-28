//! 27.7 AsyncFunction Objects
//! https://tc39.es/ecma262/#sec-async-function-objects

const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const bytecode = @import("../language/bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Completion = types.Completion;
const ExecutionContext = execution.ExecutionContext;
const Object = types.Object;
const PromiseCapability = builtins.promise.PromiseCapability;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createDynamicFunction = builtins.createDynamicFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const noexcept = utils.noexcept;
const performPromiseThen = builtins.performPromiseThen;
const promiseResolve = builtins.promiseResolve;

/// 27.7.2 Properties of the AsyncFunction Constructor
/// https://tc39.es/ecma262/#sec-async-function-constructor-properties
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            realm.agent,
            .{ .constructor = impl },
            1,
            "AsyncFunction",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function%"() },
        );
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 27.7.2.1 AsyncFunction.prototype
        // https://tc39.es/ecma262/#sec-async-function-constructor-prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%AsyncFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 27.7.1.1 AsyncFunction ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-async-function-constructor-arguments
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
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
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 27.7.3.1 AsyncFunction.prototype.constructor
        // https://tc39.es/ecma262/#sec-async-function-prototype-properties-constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            PropertyDescriptor{
                .value = Value.from(try realm.intrinsics.@"%AsyncFunction%"()),
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 27.7.3.2 AsyncFunction.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-async-function-prototype-properties-toStringTag
        try defineBuiltinProperty(object, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("AsyncFunction"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });
    }
};

/// 27.7.5.1 AsyncFunctionStart ( promiseCapability, asyncFunctionBody )
/// https://tc39.es/ecma262/#sec-async-functions-abstract-operations-async-function-start
pub fn asyncFunctionStart(
    agent: *Agent,
    promise_capability: PromiseCapability,
    async_body: AsyncBody,
) std.mem.Allocator.Error!void {
    // 1. Let runningContext be the running execution context.
    const running_context = agent.runningExecutionContext();

    // 2. Let asyncContext be a copy of runningContext.
    // 3. NOTE: Copying the execution state is required for AsyncBlockStart to resume its execution.
    //    It is ill-defined to resume a currently executing context.
    const async_context = try agent.gc_allocator.create(ExecutionContext);
    async_context.* = running_context.*;

    // 4. Perform AsyncBlockStart(promiseCapability, asyncFunctionBody, asyncContext).
    try asyncBlockStart(
        agent,
        promise_capability,
        async_body,
        async_context,
    );

    // 5. Return unused.
}

const AsyncBody = union(enum) {
    ecmascript_function: *builtins.ECMAScriptFunction,
    abstract_closure: struct {
        func: *const fn (*Agent, SafePointer) Agent.Error!Completion,
        captures: SafePointer,
    },
    module: ast.Module,
};

/// 27.7.5.2 AsyncBlockStart ( promiseCapability, asyncBody, asyncContext )
/// https://tc39.es/ecma262/#sec-asyncblockstart
pub fn asyncBlockStart(
    agent: *Agent,
    promise_capability: PromiseCapability,
    async_body: AsyncBody,
    async_context: *ExecutionContext,
) std.mem.Allocator.Error!void {
    // 1. Let runningContext be the running execution context.
    const running_context = agent.runningExecutionContext();

    // 2. Let closure be a new Abstract Closure with no parameters that captures promiseCapability
    //    and asyncBody and performs the following steps when called:
    const closure = struct {
        fn func(
            agent_: *Agent,
            promise_capability_: PromiseCapability,
            async_body_: AsyncBody,
        ) std.mem.Allocator.Error!void {
            // a. Let acAsyncContext be the running execution context.

            // b. If asyncBody is a Parse Node, then
            //     i. Let result be Completion(Evaluation of asyncBody).
            // c. Else,
            //     i. Assert: asyncBody is an Abstract Closure with no parameters.
            //     ii. Let result be asyncBody().
            const result = switch (async_body_) {
                .ecmascript_function => |ecmascript_function| ecmascript_function.fields.evaluateBody(agent_),
                .abstract_closure => |abstract_closure| abstract_closure.func(agent_, abstract_closure.captures),
                .module => |module| generateAndRunBytecode(agent_, module, .{}),
            };

            // d. Assert: If we return here, the async function either threw an exception or
            //    performed an implicit or explicit return; all awaiting is done.

            // e. Remove acAsyncContext from the execution context stack and restore the execution
            //    context that is at the top of the execution context stack as the running execution
            //    context.
            _ = agent_.execution_context_stack.pop().?;

            if (result) |completion| {
                std.debug.assert(completion.type == .normal or completion.type == .@"return");
                const value: Value = completion.value orelse .undefined;

                // f. If result is a normal completion, then
                //     i. Perform ! Call(promiseCapability.[[Resolve]], undefined, « undefined »).
                // g. Else if result is a return completion, then
                //     i. Perform ! Call(promiseCapability.[[Resolve]], undefined, « result.[[Value]] »).
                _ = Value.from(promise_capability_.resolve).callAssumeCallable(
                    agent_,
                    .undefined,
                    &.{value},
                ) catch |err| try noexcept(err);
            }
            // h. Else,
            else |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                // i. Assert: result is a throw completion.
                error.ExceptionThrown => {
                    const exception = agent_.clearException();

                    // ii. Perform ! Call(promiseCapability.[[Reject]], undefined, « result.[[Value]] »).
                    _ = Value.from(promise_capability_.reject).callAssumeCallable(
                        agent_,
                        .undefined,
                        &.{exception.value},
                    ) catch |err_| try noexcept(err_);
                },
            }

            // i. Return unused.
        }
    }.func;

    // 3. Set the code evaluation state of asyncContext such that when evaluation is resumed for
    //    that execution context, closure will be called with no arguments.

    // 4. Push asyncContext onto the execution context stack; asyncContext is now the running
    //    execution context.
    try agent.execution_context_stack.append(agent.gc_allocator, async_context);

    // 5. Resume the suspended evaluation of asyncContext. Let result be the value returned by the
    //    resumed computation.
    const result = closure(agent, promise_capability, async_body);

    // 6. Assert: When we return here, asyncContext has already been removed from the execution
    //    context stack and runningContext is the currently running execution context.
    std.debug.assert(running_context == agent.runningExecutionContext());

    // 7. Assert: result is a normal completion with a value of unused. The possible sources of
    //    this value are Await or, if the async function doesn't await anything, step 3.h above.
    result catch |err| try noexcept(err);

    // 8. Return unused.
}

/// 27.7.5.3 Await ( value )
/// https://tc39.es/ecma262/#await
pub fn @"await"(agent: *Agent, value: Value) Agent.Error!Value {
    const realm = agent.currentRealm();
    // 1. Let asyncContext be the running execution context.
    const async_context = agent.runningExecutionContext();

    // 2. Let promise be ? PromiseResolve(%Promise%, value).
    const promise = try promiseResolve(agent, try realm.intrinsics.@"%Promise%"(), value);

    const Captures = struct {
        async_context: *ExecutionContext,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .async_context = async_context };

    // 3. Let fulfilledClosure be a new Abstract Closure with parameters (v) that captures
    //    asyncContext and performs the following steps when called:
    const fulfilled_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const async_context_ = captures_.async_context;
            const v = arguments_.get(0);

            // a. Let prevContext be the running execution context.
            const previous_context = agent_.runningExecutionContext();

            // TODO: b-e.
            _ = v;
            _ = async_context_;
            _ = previous_context;

            // f. Return undefined.
            return .undefined;
        }
    }.func;

    // 4. Let onFulfilled be CreateBuiltinFunction(fulfilledClosure, 1, "", « »).
    const on_fulfilled = Value.from(
        try createBuiltinFunction(
            agent,
            .{ .function = fulfilled_closure },
            1,
            "",
            .{ .additional_fields = .make(*Captures, captures) },
        ),
    );

    // 5. Let rejectedClosure be a new Abstract Closure with parameters (reason) that captures
    //    asyncContext and performs the following steps when called:
    const rejected_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function_ = agent_.activeFunctionObject();
            const captures_ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const async_context_ = captures_.async_context;
            const reason = arguments_.get(0);

            // a. Let prevContext be the running execution context.
            const previous_context = agent_.runningExecutionContext();

            // TODO: b-e.
            _ = reason;
            _ = async_context_;
            _ = previous_context;

            // f. Return undefined.
            return .undefined;
        }
    }.func;

    // 6. Let onRejected be CreateBuiltinFunction(rejectedClosure, 1, "", « »).
    const on_rejected = Value.from(
        try createBuiltinFunction(
            agent,
            .{ .function = rejected_closure },
            1,
            "",
            .{ .additional_fields = .make(*Captures, captures) },
        ),
    );

    // 7. Perform PerformPromiseThen(promise, onFulfilled, onRejected).
    _ = try performPromiseThen(
        agent,
        promise.as(builtins.Promise),
        on_fulfilled,
        on_rejected,
        null,
    );

    // TODO: 8-12.
    agent.drainJobQueue();
    switch (promise.as(builtins.Promise).fields.promise_state) {
        .pending => return Value.from(promise), // `await properAwait()` :)
        .rejected => {
            agent.exception = .{
                .value = promise.as(builtins.Promise).fields.promise_result,
                // TODO: Capture stack when rejecting a promise
                .stack_trace = &.{},
            };
            return error.ExceptionThrown;
        },
        .fulfilled => return promise.as(builtins.Promise).fields.promise_result,
    }
}
