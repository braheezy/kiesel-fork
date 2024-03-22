//! 27.5 Generator Objects
//! https://tc39.es/ecma262/#sec-generator-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ExecutionContext = execution.ExecutionContext;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createIterResultObject = types.createIterResultObject;
const defineBuiltinProperty = utils.defineBuiltinProperty;

/// 27.5.1 The %GeneratorPrototype% Object
/// https://tc39.es/ecma262/#sec-properties-of-generator-prototype
pub const GeneratorPrototype = struct {
    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%IteratorPrototype%"(),
        });

        // 27.5.1.1 %GeneratorPrototype%.constructor
        // https://tc39.es/ecma262/#sec-generator.prototype.constructor
        try defineBuiltinProperty(object, "constructor", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%GeneratorFunction.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 27.5.1.5 %GeneratorPrototype% [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-generator.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Generator"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }
};

/// 27.5.2 Properties of Generator Instances
/// https://tc39.es/ecma262/#sec-properties-of-generator-instances
pub const Generator = MakeObject(.{
    .Fields = struct {
        pub const State = enum {
            suspended_start,
            suspended_yield,
            executing,
            completed,
        };

        /// [[GeneratorState]]
        generator_state: ?State,

        /// [[GeneratorContext]]
        generator_context: ExecutionContext,

        // Non-standard
        state: struct {
            closure: *const fn (*Agent, *builtins.ECMAScriptFunction) Agent.Error!Object,
            generator_function: *builtins.ECMAScriptFunction,
        },
    },
    .tag = .generator,
});

/// 27.5.3.1 GeneratorStart ( generator, generatorBody )
/// https://tc39.es/ecma262/#sec-generatorstart
pub fn generatorStart(
    agent: *Agent,
    generator: *Generator,
    generator_function: *builtins.ECMAScriptFunction,
) Allocator.Error!void {
    // 1. Assert: The value of generator.[[GeneratorState]] is undefined.
    std.debug.assert(generator.fields.generator_state == null);

    // 2. Let genContext be the running execution context.
    const generator_context = agent.runningExecutionContext();

    // 3. Set the Generator component of genContext to generator.
    generator_context.generator = generator;

    // 4. Let closure be a new Abstract Closure with no parameters that captures generatorBody and performs the following steps when called:
    const closure = struct {
        fn func(
            agent_: *Agent,
            generator_function_: *builtins.ECMAScriptFunction,
        ) Agent.Error!Object {
            // a. Let acGenContext be the running execution context.
            const closure_generator_context = agent_.runningExecutionContext();

            // b. Let acGenerator be the Generator component of acGenContext.
            const closure_generator = closure_generator_context.generator.?;

            // c. If generatorBody is a Parse Node, then
            const result = if (true) blk: {
                // i. Let result be Completion(Evaluation of generatorBody).
                break :blk generator_function_.fields.generateAndRunBytecode(agent_);
            }
            // TODO: d. Else,
            else {
                // i. Assert: generatorBody is an Abstract Closure with no parameters.
                // ii. Let result be generatorBody().
            };

            // e. Assert: If we return here, the generator either threw an exception or performed
            //    either an implicit or explicit return.

            // f. Remove acGenContext from the execution context stack and restore the execution
            //    context that is at the top of the execution context stack as the running
            //    execution context.
            _ = agent_.execution_context_stack.pop();

            // g. Set acGenerator.[[GeneratorState]] to completed.
            closure_generator.fields.generator_state = .completed;

            // h. NOTE: Once a generator enters the completed state it never leaves it and its
            //    associated execution context is never resumed. Any execution state associated
            //    with acGenerator can be discarded at this point.

            // i. If result is a normal completion, then
            //     i. Let resultValue be undefined.
            // j. Else if result is a return completion, then
            //     i. Let resultValue be result.[[Value]].
            const result_value = if (result) |completion| blk: {
                std.debug.assert(completion.type == .normal or completion.type == .@"return");
                break :blk completion.value orelse .undefined;
            }
            // k. Else,
            else |err| {
                // i. Assert: result is a throw completion.
                // ii. Return ? result.
                return err;
            };

            // l. Return CreateIterResultObject(resultValue, true).
            return createIterResultObject(agent_, result_value, true);
        }
    }.func;

    // 5. Set the code evaluation state of genContext such that when evaluation is resumed for that
    //    execution context, closure will be called with no arguments.
    generator.fields.state = .{ .closure = closure, .generator_function = generator_function };

    // 6. Set generator.[[GeneratorContext]] to genContext.
    generator.fields.generator_context = generator_context.*;

    // 7. Set generator.[[GeneratorState]] to suspended-start.
    generator.fields.generator_state = .suspended_start;

    // 8. Return unused.
}
