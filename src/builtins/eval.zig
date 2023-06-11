const std = @import("std");

const ast = @import("../language/ast.zig");
const bytecode = @import("../language/bytecode.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Diagnostics = language.Diagnostics;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const Parser = @import("../language/Parser.zig");
const PrivateEnvironment = execution.PrivateEnvironment;
const Value = types.Value;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;

/// 19.2.1.1 PerformEval ( x, strictCaller, direct )
/// https://tc39.es/ecma262/#sec-performeval
pub fn performEval(agent: *Agent, x: Value, strict_caller: bool, direct: bool) !Value {
    // 1. Assert: If direct is false, then strictCaller is also false.
    if (!direct) std.debug.assert(!strict_caller);

    // 2. If x is not a String, return x.
    if (x != .string) return x;

    // 3. Let evalRealm be the current Realm Record.
    // 4. NOTE: In the case of a direct eval, evalRealm is the realm of both the caller of eval and
    //          of the eval function itself.
    const eval_realm = agent.currentRealm();

    // 5. Perform ? HostEnsureCanCompileStrings(evalRealm).
    try agent.host_hooks.hostEnsureCanCompileStrings(eval_realm);

    // TODO: 6-10.

    // 11. Perform the following substeps in an implementation-defined order, possibly interleaving
    //     parsing and error detection:

    var diagnostics = Diagnostics.init(agent.gc_allocator);
    defer diagnostics.deinit();

    // a. Let script be ParseText(StringToCodePoints(x), Script).
    const script = Parser.parse(ast.Script, agent.gc_allocator, x.string, .{
        .diagnostics = &diagnostics,
        .file_name = "eval",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // b. If script is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(.syntax_error, parse_error.message);
        },
    };

    // c. If script Contains ScriptBody is false, return undefined.
    if (script.statement_list.items.len == 0) return .undefined;

    // d. Let body be the ScriptBody of script.
    // NOTE: No-op, we evaluate the script directly.

    // TODO: e-h.

    // 12. If strictCaller is true, let strictEval be true.
    // 13. Else, let strictEval be IsStrict of script.
    const strict_eval = strict_caller or script.isStrict();

    // 14. Let runningContext be the running execution context.
    // 15. NOTE: If direct is true, runningContext will be the execution context that performed the
    //           direct eval. If direct is false, runningContext will be the execution context for
    //           the invocation of the eval function.
    const running_context = agent.runningExecutionContext();

    var lexical_environment: Environment = undefined;
    var variable_environment: Environment = undefined;
    var private_environment: ?*PrivateEnvironment = undefined;

    // 16. If direct is true, then
    if (direct) {
        // a. Let lexEnv be NewDeclarativeEnvironment(runningContext's LexicalEnvironment).
        lexical_environment = .{
            .declarative_environment = try newDeclarativeEnvironment(
                agent.gc_allocator,
                running_context.ecmascript_code.?.lexical_environment,
            ),
        };

        // b. Let varEnv be runningContext's VariableEnvironment.
        variable_environment = running_context.ecmascript_code.?.variable_environment;

        // c. Let privateEnv be runningContext's PrivateEnvironment.
        private_environment = running_context.ecmascript_code.?.private_environment;
    }
    // 17. Else,
    else {
        // a. Let lexEnv be NewDeclarativeEnvironment(evalRealm.[[GlobalEnv]]).
        lexical_environment = .{
            .declarative_environment = try newDeclarativeEnvironment(
                agent.gc_allocator,
                .{ .global_environment = eval_realm.global_env },
            ),
        };

        // b. Let varEnv be evalRealm.[[GlobalEnv]].
        variable_environment = .{ .global_environment = eval_realm.global_env };

        // c. Let privateEnv be null.
        private_environment = null;
    }

    // 18. If strictEval is true, set varEnv to lexEnv.
    if (strict_eval) variable_environment = lexical_environment;

    // TODO: 19. If runningContext is not already suspended, suspend runningContext.

    // 20. Let evalContext be a new ECMAScript code execution context.
    const eval_context = ExecutionContext{
        // 21. Set evalContext's Function to null.
        .function = null,

        // 22. Set evalContext's Realm to evalRealm.
        .realm = eval_realm,

        // 23. Set evalContext's ScriptOrModule to runningContext's ScriptOrModule.
        .script_or_module = running_context.script_or_module,

        .ecmascript_code = .{
            // 24. Set evalContext's VariableEnvironment to varEnv.
            .variable_environment = variable_environment,

            // 25. Set evalContext's LexicalEnvironment to lexEnv.
            .lexical_environment = lexical_environment,

            // 26. Set evalContext's PrivateEnvironment to privateEnv.
            .private_environment = private_environment,
        },
    };

    // 27. Push evalContext onto the execution context stack; evalContext is now the running
    //     execution context.
    try agent.execution_context_stack.append(eval_context);

    // TODO: 28. Let result be Completion(EvalDeclarationInstantiation(body, varEnv, lexEnv, privateEnv, strictEval)).
    const result_no_value: error{ExceptionThrown}!void = {};

    // 29. If result.[[Type]] is normal, then
    const result = if (result_no_value) |_| blk: {
        // a. Set result to Completion(Evaluation of body).
        // 30. If result.[[Type]] is normal and result.[[Value]] is empty, then
        if (generateAndRunBytecode(agent, script)) |completion|
            // a. Set result to NormalCompletion(undefined).
            break :blk completion.value orelse .undefined
        else |err|
            break :blk err;
    } else |err| err;

    // 31. Suspend evalContext and remove it from the execution context stack.
    _ = agent.execution_context_stack.pop();

    // TODO: 32. Resume the context that is now on the top of the execution context stack as the running
    //     execution context.

    // 33. Return ? result.
    return result;
}
