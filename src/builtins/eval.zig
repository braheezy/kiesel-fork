const std = @import("std");

const ast = @import("../language/ast.zig");
const bytecode = @import("../language/bytecode.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Diagnostics = language.Diagnostics;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const Parser = @import("../language/Parser.zig");
const PrivateEnvironment = execution.PrivateEnvironment;
const Value = types.Value;
const formatParseError = utils.formatParseError;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const noexcept = utils.noexcept;

/// 19.2.1.1 PerformEval ( x, strictCaller, direct )
/// https://tc39.es/ecma262/#sec-performeval
pub fn performEval(agent: *Agent, x: Value, strict_caller: bool, direct: bool) Agent.Error!Value {
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
    const script = Parser.parse(ast.Script, agent.gc_allocator, x.string.utf8, .{
        .diagnostics = &diagnostics,
        .file_name = "eval",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // b. If script is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                "{s}",
                .{try formatParseError(agent.gc_allocator, parse_error)},
            );
        },
    };

    // c. If script Contains ScriptBody is false, return undefined.
    if (script.statement_list.items.len == 0) return .undefined;

    // d. Let body be the ScriptBody of script.
    const body = script.statement_list;

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

    // 28. Let result be Completion(EvalDeclarationInstantiation(body, varEnv, lexEnv, privateEnv, strictEval)).
    const result_no_value = evalDeclarationInstantiation(
        agent,
        body,
        variable_environment,
        lexical_environment,
        private_environment,
        strict_eval,
    );

    // 29. If result is a normal completion, then
    const result = if (result_no_value) |_| blk: {
        // a. Set result to Completion(Evaluation of body).
        // 30. If result is a normal completion and result.[[Value]] is empty, then
        if (generateAndRunBytecode(agent, body, .{
            .contained_in_strict_mode_code = strict_eval,
        })) |completion|
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

/// 19.2.1.3 EvalDeclarationInstantiation ( body, varEnv, lexEnv, privateEnv, strict )
/// https://tc39.es/ecma262/#sec-evaldeclarationinstantiation
fn evalDeclarationInstantiation(
    agent: *Agent,
    body: ast.StatementList,
    var_env: Environment,
    lex_env: Environment,
    private_env: ?*PrivateEnvironment,
    strict: bool,
) Agent.Error!void {
    _ = lex_env;
    _ = private_env;

    // TODO: 1. Let varNames be the VarDeclaredNames of body.

    // 2. Let varDeclarations be the VarScopedDeclarations of body.
    const var_declarations = try body.varScopedDeclarations(agent.gc_allocator);

    // 3. If strict is false, then
    if (!strict) {
        // a. If varEnv is a Global Environment Record, then
        if (var_env == .global_environment) {
            // TODO: i. For each element name of varNames, do
            for (var_declarations) |var_declaration| {
                const name = var_declaration.binding_identifier;

                // 1. If varEnv.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
                if (var_env.global_environment.hasLexicalDeclaration(name)) {
                    return agent.throwException(
                        .syntax_error,
                        "Global environment already has a lexical declaration '{s}'",
                        .{name},
                    );
                }

                // 2. NOTE: eval will not create a global var declaration that would be shadowed by
                //    a global lexical declaration.
            }
        }

        // TODO: b-c.
    }

    // TODO: 4-10.

    // 11. Let declaredVarNames be a new empty List.
    var declared_var_names = std.StringHashMap(void).init(agent.gc_allocator);
    defer declared_var_names.deinit();

    // 12. For each element d of varDeclarations, do
    for (var_declarations) |var_declaration| {
        // a. If d is either a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        //     i. For each String vn of the BoundNames of d, do
        for ([_]ast.Identifier{var_declaration.binding_identifier}) |var_name| {
            // TODO: 1. If declaredFunctionNames does not contain vn, then
            // a. If varEnv is a Global Environment Record, then
            if (var_env == .global_environment) {
                // TODO: i. Let vnDefinable be ? varEnv.CanDeclareGlobalVar(vn).
                // TODO: ii. If vnDefinable is false, throw a TypeError exception.
            }

            // b. If declaredVarNames does not contain vn, then
            if (!declared_var_names.contains(var_name)) {
                // i. Append vn to declaredVarNames.
                try declared_var_names.putNoClobber(var_name, {});
            }
        }
    }

    // 13. NOTE: Annex B.3.2.3 adds additional steps at this point.
    // 14. NOTE: No abnormal terminations occur after this algorithm step unless varEnv is a Global
    //     Environment Record and the global object is a Proxy exotic object.

    // TODO: 15-17.

    // 18. For each String vn of declaredVarNames, do
    var it = declared_var_names.keyIterator();
    while (it.next()) |ptr| {
        const var_name = ptr.*;

        // a. If varEnv is a Global Environment Record, then
        if (var_env == .global_environment) {
            // i. Perform ? varEnv.CreateGlobalVarBinding(vn, true).
            try var_env.global_environment.createGlobalVarBinding(agent, var_name, true);
        }
        // b. Else,
        else {
            // i. Let bindingExists be ! varEnv.HasBinding(vn).
            const binding_exists = var_env.hasBinding(var_name) catch |err| try noexcept(err);

            // ii. If bindingExists is false, then
            if (!binding_exists) {
                // 1. NOTE: The following invocation cannot return an abrupt completion because of
                //    the validation preceding step 14.
                // 2. Perform ! varEnv.CreateMutableBinding(vn, true).
                var_env.createMutableBinding(agent, var_name, true) catch |err| try noexcept(err);

                // 3. Perform ! varEnv.InitializeBinding(vn, undefined).
                var_env.initializeBinding(agent, var_name, .undefined) catch |err| try noexcept(err);
            }
        }
    }

    // 19. Return unused.
}
