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
const String = types.String;
const Value = types.Value;
const formatParseError = utils.formatParseError;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const instantiateAsyncFunctionObject = language.instantiateAsyncFunctionObject;
const instantiateAsyncGeneratorFunctionObject = language.instantiateAsyncGeneratorFunctionObject;
const instantiateGeneratorFunctionObject = language.instantiateGeneratorFunctionObject;
const instantiateOrdinaryFunctionObject = language.instantiateOrdinaryFunctionObject;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const noexcept = utils.noexcept;

/// 19.2.1.1 PerformEval ( x, strictCaller, direct )
/// https://tc39.es/ecma262/#sec-performeval
pub fn performEval(agent: *Agent, x: Value, strict_caller: bool, direct: bool) Agent.Error!Value {
    // 1. Assert: If direct is false, then strictCaller is also false.
    if (!direct) std.debug.assert(!strict_caller);

    // 2. If x is not a String, return x.
    if (!x.isString()) return x;

    // 3. Let evalRealm be the current Realm Record.
    // 4. NOTE: In the case of a direct eval, evalRealm is the realm of both the caller of eval and
    //          of the eval function itself.
    const eval_realm = agent.currentRealm();

    // 5. Perform ? HostEnsureCanCompileStrings(evalRealm, « », x, direct).
    try agent.host_hooks.hostEnsureCanCompileStrings(eval_realm, &.{}, x.asString(), direct);

    // TODO: 6-10.

    // 11. Perform the following substeps in an implementation-defined order, possibly interleaving
    //     parsing and error detection:

    var diagnostics = Diagnostics.init(agent.gc_allocator);
    defer diagnostics.deinit();

    // a. Let script be ParseText(x, Script).
    const script = Parser.parse(ast.Script, agent.gc_allocator, try x.asString().toUtf8(agent.gc_allocator), .{
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
    const body = script;

    // TODO: e-h.

    // 12. If strictCaller is true, let strictEval be true.
    // 13. Else, let strictEval be ScriptIsStrict of script.
    const strict_eval = strict_caller or script.scriptIsStrict();

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
    const eval_context: ExecutionContext = .{
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
    const result: Agent.Error!Value = if (result_no_value) |_| blk: {
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
    body: ast.Script,
    var_env: Environment,
    lex_env: Environment,
    private_env: ?*PrivateEnvironment,
    strict: bool,
) Agent.Error!void {
    // 1. Let varNames be the VarDeclaredNames of body.
    var var_names = std.ArrayList(ast.Identifier).init(agent.gc_allocator);
    defer var_names.deinit();
    try body.collectVarDeclaredNames(&var_names);

    // 2. Let varDeclarations be the VarScopedDeclarations of body.
    var var_declarations = std.ArrayList(ast.VarScopedDeclaration).init(agent.gc_allocator);
    defer var_declarations.deinit();
    try body.collectVarScopedDeclarations(&var_declarations);

    // 3. If strict is false, then
    if (!strict) {
        // a. If varEnv is a Global Environment Record, then
        if (var_env == .global_environment) {
            // i. For each element name of varNames, do
            for (var_names.items) |name_utf8| {
                const name = try String.fromUtf8(agent.gc_allocator, name_utf8);

                // 1. If varEnv.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
                if (var_env.global_environment.hasLexicalDeclaration(name)) {
                    return agent.throwException(
                        .syntax_error,
                        "Global environment already has a lexical declaration '{}'",
                        .{name},
                    );
                }

                // 2. NOTE: eval will not create a global var declaration that would be shadowed by
                //    a global lexical declaration.
            }
        }

        // b. Let thisEnv be lexEnv.
        var this_env = lex_env;

        // c. Assert: The following loop will terminate.
        // d. Repeat, while thisEnv and varEnv are not the same Environment Record,
        while (!std.meta.eql(this_env, var_env)) {
            // i. If thisEnv is not an Object Environment Record, then
            if (this_env != .object_environment) {
                // 1. NOTE: The environment of with statements cannot contain any lexical
                //    declaration so it doesn't need to be checked for var/let hoisting conflicts.

                // 2. For each element name of varNames, do
                for (var_names.items) |name_utf8| {
                    const name = try String.fromUtf8(agent.gc_allocator, name_utf8);

                    // a. If ! thisEnv.HasBinding(name) is true, then
                    if (this_env.hasBinding(name) catch |err| try noexcept(err)) {
                        // i. Throw a SyntaxError exception.
                        return agent.throwException(.syntax_error, "idk", .{});

                        // ii. NOTE: Annex B.3.4 defines alternate semantics for the above step.
                    }

                    // b. NOTE: A direct eval will not hoist var declaration over a like-named
                    //    lexical declaration.
                }
            }

            // ii. Set thisEnv to thisEnv.[[OuterEnv]].
            this_env = this_env.outerEnv().?;
        }
    }

    // TODO: 4-7.

    // 8. Let functionsToInitialize be a new empty List.
    var functions_to_initialize = std.ArrayList(ast.HoistableDeclaration).init(agent.gc_allocator);
    defer functions_to_initialize.deinit();

    // 9. Let declaredFunctionNames be a new empty List.
    var declared_function_names = String.HashMap(void).init(agent.gc_allocator);
    defer declared_function_names.deinit();

    // 10. For each element d of varDeclarations, in reverse List order, do
    var it = std.mem.reverseIterator(var_declarations.items);
    while (it.next()) |var_declaration| {
        // a. If d is not either a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        if (var_declaration == .hoistable_declaration) {
            // i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an
            //    AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
            const hoistable_declaration = var_declaration.hoistable_declaration;

            // ii. NOTE: If there are multiple function declarations for the same name, the last
            //     declaration is used.

            // iii. Let fn be the sole element of the BoundNames of d.
            const function_name = switch (hoistable_declaration) {
                inline else => |function_declaration| try String.fromUtf8(agent.gc_allocator, function_declaration.identifier.?),
            };

            // iv. If declaredFunctionNames does not contain fn, then
            if (!declared_function_names.contains(function_name)) {
                // 1. If varEnv is a Global Environment Record, then
                if (var_env == .global_environment) {
                    // a. Let fnDefinable be ? varEnv.CanDeclareGlobalFunction(fn).
                    const function_definable = try var_env.global_environment.canDeclareGlobalFunction(function_name);

                    // b. If fnDefinable is false, throw a TypeError exception.
                    if (!function_definable) {
                        return agent.throwException(
                            .type_error,
                            "Cannot declare '{}' in global environment",
                            .{function_name},
                        );
                    }
                }

                // 2. Append fn to declaredFunctionNames.
                try declared_function_names.putNoClobber(function_name, {});

                // 3. Insert d as the first element of functionsToInitialize.
                // NOTE: AFAICT the order isn't observable, so we can append.
                try functions_to_initialize.append(hoistable_declaration);
            }
        }
    }

    // 11. Let declaredVarNames be a new empty List.
    var declared_var_names = String.HashMap(void).init(agent.gc_allocator);
    defer declared_var_names.deinit();

    var bound_names = std.ArrayList(ast.Identifier).init(agent.gc_allocator);
    defer bound_names.deinit();

    // 12. For each element d of varDeclarations, do
    for (var_declarations.items) |var_declaration| {
        // a. If d is either a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        if (var_declaration == .variable_declaration) {
            bound_names.clearRetainingCapacity();
            try var_declaration.variable_declaration.collectBoundNames(&bound_names);

            // i. For each String vn of the BoundNames of d, do
            for (bound_names.items) |var_name_utf8| {
                const var_name = try String.fromUtf8(agent.gc_allocator, var_name_utf8);

                // 1. If declaredFunctionNames does not contain vn, then
                if (!declared_function_names.contains(var_name)) {
                    // a. If varEnv is a Global Environment Record, then
                    if (var_env == .global_environment) {
                        // i. Let vnDefinable be ? varEnv.CanDeclareGlobalVar(vn).
                        const var_name_definable = try var_env.global_environment.canDeclareGlobalVar(var_name);

                        // ii. If vnDefinable is false, throw a TypeError exception.
                        if (!var_name_definable) {
                            return agent.throwException(
                                .type_error,
                                "Cannot declare '{}' in global environment",
                                .{var_name},
                            );
                        }
                    }

                    // b. If declaredVarNames does not contain vn, then
                    if (!declared_var_names.contains(var_name)) {
                        // i. Append vn to declaredVarNames.
                        try declared_var_names.putNoClobber(var_name, {});
                    }
                }
            }
        }
    }

    // 13. NOTE: Annex B.3.2.3 adds additional steps at this point.
    // 14. NOTE: No abnormal terminations occur after this algorithm step unless varEnv is a Global
    //     Environment Record and the global object is a Proxy exotic object.

    // 15. Let lexDeclarations be the LexicallyScopedDeclarations of body.
    var lex_declarations = std.ArrayList(ast.LexicallyScopedDeclaration).init(agent.gc_allocator);
    defer lex_declarations.deinit();
    try body.collectLexicallyScopedDeclarations(&lex_declarations);

    // 16. For each element d of lexDeclarations, do
    for (lex_declarations.items) |declaration| {
        // a. NOTE: Lexically declared names are only instantiated here but not initialized.

        bound_names.clearRetainingCapacity();
        try declaration.collectBoundNames(&bound_names);

        // b. For each element dn of the BoundNames of d, do
        for (bound_names.items) |name_utf8| {
            const name = try String.fromUtf8(agent.gc_allocator, name_utf8);

            // i. If IsConstantDeclaration of d is true, then
            if (declaration.isConstantDeclaration()) {
                // 1. Perform ? lexEnv.CreateImmutableBinding(dn, true).
                try lex_env.createImmutableBinding(agent, name, true);
            }
            // ii. Else,
            else {
                // 1. Perform ? lexEnv.CreateMutableBinding(dn, false).
                try lex_env.createMutableBinding(agent, name, false);
            }
        }
    }

    // 17. For each Parse Node f of functionsToInitialize, do
    for (functions_to_initialize.items) |hoistable_declaration| {
        // a. Let fn be the sole element of the BoundNames of f.
        const function_name = switch (hoistable_declaration) {
            inline else => |function_declaration| try String.fromUtf8(agent.gc_allocator, function_declaration.identifier.?),
        };

        // b. Let fo be InstantiateFunctionObject of f with arguments lexEnv and privateEnv.
        const function_object = try switch (hoistable_declaration) {
            .function_declaration => |function_declaration| instantiateOrdinaryFunctionObject(agent, function_declaration, lex_env, private_env),
            .generator_declaration => |generator_declaration| instantiateGeneratorFunctionObject(agent, generator_declaration, lex_env, private_env),
            .async_function_declaration => |async_function_declaration| instantiateAsyncFunctionObject(agent, async_function_declaration, lex_env, private_env),
            .async_generator_declaration => |async_generator_declaration| instantiateAsyncGeneratorFunctionObject(agent, async_generator_declaration, lex_env, private_env),
        };

        // c. If varEnv is a Global Environment Record, then
        if (var_env == .global_environment) {
            // i. Perform ? varEnv.CreateGlobalFunctionBinding(fn, fo, true).
            try var_env.global_environment.createGlobalFunctionBinding(
                function_name,
                Value.from(function_object),
                true,
            );
        }
        // d. Else,
        else {
            // i. Let bindingExists be ! varEnv.HasBinding(fn).
            const binding_exists = var_env.hasBinding(function_name) catch |err| try noexcept(err);

            // ii. If bindingExists is false, then
            if (!binding_exists) {
                // 1. NOTE: The following invocation cannot return an abrupt completion because of
                //    the validation preceding step 14.

                // 2. Perform ! varEnv.CreateMutableBinding(fn, true).
                var_env.createMutableBinding(
                    agent,
                    function_name,
                    true,
                ) catch |err| try noexcept(err);

                // 3. Perform ! varEnv.InitializeBinding(fn, fo).
                var_env.initializeBinding(
                    agent,
                    function_name,
                    Value.from(function_object),
                ) catch |err| try noexcept(err);
            }
            // iii. Else,
            else {
                // 1. Perform ! varEnv.SetMutableBinding(fn, fo, false).
                var_env.setMutableBinding(
                    agent,
                    function_name,
                    Value.from(function_object),
                    false,
                ) catch |err| try noexcept(err);
            }
        }
    }

    // 18. For each String vn of declaredVarNames, do
    var it_ = declared_var_names.keyIterator();
    while (it_.next()) |ptr| {
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
