//! 16.1.4 Script Records
//! https://tc39.es/ecma262/#sec-script-records

const std = @import("std");

const ast = @import("ast.zig");
const ast_printing = @import("ast_printing.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const GlobalEnvironment = execution.GlobalEnvironment;
const Module = language.Module;
const Parser = @import("Parser.zig");
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Value = types.Value;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;

const Self = @This();

/// [[Realm]]
realm: *Realm,

/// [[ECMAScriptCode]]
ecmascript_code: ast.Script,

/// [[LoadedModules]]
loaded_modules: std.StringHashMap(Module),

/// [[HostDefined]]
host_defined: SafePointer,

pub fn print(self: Self, writer: anytype) @TypeOf(writer).Error!void {
    try ast_printing.printScript(self.ecmascript_code, writer, 0);
}

/// 16.1.5 ParseScript ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parse-script
pub fn parse(
    source_text: []const u8,
    realm: *Realm,
    host_defined: ?SafePointer,
    ctx: Parser.ParseContext,
) Parser.Error!*Self {
    const agent = realm.agent;

    // 1. Let script be ParseText(sourceText, Script).
    // 2. If script is a List of errors, return script.
    const script = try Parser.parse(ast.Script, agent.gc_allocator, source_text, ctx);

    // 3. Return Script Record {
    //      [[Realm]]: realm, [[ECMAScriptCode]]: script, [[LoadedModules]]: « », [[HostDefined]]: hostDefined
    //    }.
    const self = try agent.gc_allocator.create(Self);
    self.* = .{
        .realm = realm,
        .ecmascript_code = script,
        .loaded_modules = std.StringHashMap(Module).init(agent.gc_allocator),
        .host_defined = host_defined orelse SafePointer.null_pointer,
    };
    return self;
}

/// 16.1.6 ScriptEvaluation ( scriptRecord )
/// https://tc39.es/ecma262/#sec-runtime-semantics-scriptevaluation
pub fn evaluate(self: *Self) Agent.Error!Value {
    const agent = self.realm.agent;

    // 1. Let globalEnv be scriptRecord.[[Realm]].[[GlobalEnv]].
    const global_env = self.realm.global_env;

    // 1. Let scriptContext be a new ECMAScript code execution context.
    const script_context = ExecutionContext{
        // 3. Set the Function of scriptContext to null.
        .function = null,

        // 4. Set the Realm of scriptContext to scriptRecord.[[Realm]].
        .realm = self.realm,

        // 5. Set the ScriptOrModule of scriptContext to scriptRecord.
        .script_or_module = .{ .script = self },

        .ecmascript_code = .{
            // 6. Set the VariableEnvironment of scriptContext to globalEnv.
            .variable_environment = .{ .global_environment = global_env },

            // 7. Set the LexicalEnvironment of scriptContext to globalEnv.
            .lexical_environment = .{ .global_environment = global_env },

            // 8. Set the PrivateEnvironment of scriptContext to null.
            .private_environment = null,
        },
    };

    // TODO: 9. Suspend the running execution context.

    // 10. Push scriptContext onto the execution context stack; scriptContext is now the running execution context.
    try agent.execution_context_stack.append(script_context);

    // 11. Let script be scriptRecord.[[ECMAScriptCode]].
    const script = self.ecmascript_code;

    // 12. Let result be Completion(GlobalDeclarationInstantiation(script, globalEnv)).
    const result_no_value = globalDeclarationInstantiation(agent, script, global_env);

    // 13. If result is a normal completion, then
    const result = if (result_no_value) |_| blk: {
        // a. Set result to Completion(Evaluation of script).
        // b. If result is a normal completion and result.[[Value]] is empty, then
        if (generateAndRunBytecode(agent, script, .{})) |completion|
            // i. Set result to NormalCompletion(undefined).
            break :blk completion.value orelse .undefined
        else |err|
            break :blk err;
    } else |err| err;

    // 14. Suspend scriptContext and remove it from the execution context stack.
    _ = agent.execution_context_stack.pop();

    // 15. Assert: The execution context stack is not empty.
    std.debug.assert(agent.execution_context_stack.items.len > 0);

    // TODO: 16. Resume the context that is now on the top of the execution context stack as the
    //     running execution context.

    // 17. Return ? result.
    return try result;
}

/// 16.1.7 GlobalDeclarationInstantiation ( script, env )
/// https://tc39.es/ecma262/#sec-globaldeclarationinstantiation
fn globalDeclarationInstantiation(agent: *Agent, script: ast.Script, env: *GlobalEnvironment) Agent.Error!void {
    // TODO: 1. Let lexNames be the LexicallyDeclaredNames of script.

    // 2. Let varNames be the VarDeclaredNames of script.
    const var_names = try script.varDeclaredNames(agent.gc_allocator);
    defer agent.gc_allocator.free(var_names);

    // TODO: 3. For each element name of lexNames, do

    // 4. For each element name of varNames, do
    for (var_names) |name| {
        // a. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
        if (env.hasLexicalDeclaration(name)) {
            return agent.throwException(
                .syntax_error,
                "Global environment already has a lexical declaration '{s}'",
                .{name},
            );
        }
    }

    // 5. Let varDeclarations be the VarScopedDeclarations of script.
    const var_declarations = try script.varScopedDeclarations(agent.gc_allocator);
    defer agent.gc_allocator.free(var_declarations);

    // 6. Let functionsToInitialize be a new empty List.
    var functions_to_initialize = std.ArrayList(ast.HoistableDeclaration).init(agent.gc_allocator);
    defer functions_to_initialize.deinit();

    // 7. Let declaredFunctionNames be a new empty List.
    var declared_function_names = std.StringHashMap(void).init(agent.gc_allocator);
    defer declared_function_names.deinit();

    // 8. For each element d of varDeclarations, in reverse List order, do
    var it = std.mem.reverseIterator(var_declarations);
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
                inline else => |function_declaration| function_declaration.identifier,
            }.?;

            // iv. If declaredFunctionNames does not contain fn, then
            if (!declared_function_names.contains(function_name)) {
                // TODO: 1. Let fnDefinable be ? env.CanDeclareGlobalFunction(fn).
                // TODO: 2. If fnDefinable is false, throw a TypeError exception.

                // 3. Append fn to declaredFunctionNames.
                try declared_function_names.putNoClobber(function_name, {});

                // 4. Insert d as the first element of functionsToInitialize.
                // NOTE: AFAICT the order isn't observable, so we can append.
                try functions_to_initialize.append(hoistable_declaration);
            }
        }
    }

    // 9. Let declaredVarNames be a new empty List.
    var declared_var_names = std.StringHashMap(void).init(agent.gc_allocator);
    defer declared_var_names.deinit();

    // 10. For each element d of varDeclarations, do
    for (var_declarations) |var_declaration| {
        // a. If d is either a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        if (var_declaration == .variable_declaration) {
            // TODO: Update this when binding patterns are supported.
            const bound_names: []const ast.Identifier = &.{
                var_declaration.variable_declaration.binding_identifier,
            };

            // i. For each String vn of the BoundNames of d, do
            for (bound_names) |var_name| {
                // 1. If declaredFunctionNames does not contain vn, then
                if (!declared_function_names.contains(var_name)) {
                    // TODO: a. Let vnDefinable be ? env.CanDeclareGlobalVar(vn).
                    // TODO: b. If vnDefinable is false, throw a TypeError exception.

                    // c. If declaredVarNames does not contain vn, then
                    if (!declared_var_names.contains(var_name)) {
                        // i. Append vn to declaredVarNames.
                        try declared_var_names.putNoClobber(var_name, {});
                    }
                }
            }
        }
    }

    // 11. NOTE: No abnormal terminations occur after this algorithm step if the global object is
    //     an ordinary object. However, if the global object is a Proxy exotic object it may
    //     exhibit behaviours that cause abnormal terminations in some of the following steps.

    // 12. NOTE: Annex B.3.2.2 adds additional steps at this point.

    // TODO: 13. Let lexDeclarations be the LexicallyScopedDeclarations of script.

    // 14. Let privateEnv be null.
    const private_env = null;

    // TODO: 15. For each element d of lexDeclarations, do

    // 16. For each Parse Node f of functionsToInitialize, do
    for (functions_to_initialize.items) |*ptr| {
        var hoistable_declaration = ptr.*;

        switch (hoistable_declaration) {
            inline else => |*function_declaration| {
                // Assign the function body's strictness, which is needed for the deferred bytecode generation.
                // FIXME: This should ideally happen at parse time.
                function_declaration.function_body.strict = script.isStrict() or
                    function_declaration.function_body.functionBodyContainsUseStrict();
            },
        }

        // a. Let fn be the sole element of the BoundNames of f.
        const function_name = switch (hoistable_declaration) {
            inline else => |function_declaration| function_declaration.identifier,
        }.?;

        // b. Let fo be InstantiateFunctionObject of f with arguments env and privateEnv.
        const function_object = try switch (hoistable_declaration) {
            .function_declaration => |function_declaration| function_declaration.instantiateOrdinaryFunctionObject(agent, .{ .global_environment = env }, private_env),
            .generator_declaration => |generator_declaration| generator_declaration.instantiateGeneratorFunctionObject(agent, .{ .global_environment = env }, private_env),
            .async_function_declaration => |async_function_declaration| async_function_declaration.instantiateAsyncFunctionObject(agent, .{ .global_environment = env }, private_env),
            .async_generator_declaration => |async_generator_declaration| async_generator_declaration.instantiateAsyncGeneratorFunctionObject(agent, .{ .global_environment = env }, private_env),
        };

        // c. Perform ? env.CreateGlobalFunctionBinding(fn, fo, false).
        try env.createGlobalFunctionBinding(function_name, Value.from(function_object), false);
    }

    // 17. For each String vn of declaredVarNames, do
    var it_ = declared_var_names.keyIterator();
    while (it_.next()) |ptr| {
        const var_name = ptr.*;

        // a. Perform ? env.CreateGlobalVarBinding(vn, false).
        try env.createGlobalVarBinding(agent, var_name, false);
    }

    // 18. Return unused.
}
