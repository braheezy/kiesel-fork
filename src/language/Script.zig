//! 16.1.4 Script Records
//! https://tc39.es/ecma262/#sec-script-records

const std = @import("std");

const ast = @import("ast.zig");
const ast_printing = @import("ast_printing.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
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

// TODO: [[LoadedModules]]

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

    // TODO: 12. Let result be Completion(GlobalDeclarationInstantiation(script, globalEnv)).
    // NOTE: This is totally ad-hoc for now.
    const var_scoped_declarations = try script.varScopedDeclarations(agent.gc_allocator);
    defer agent.gc_allocator.free(var_scoped_declarations);
    var seen = std.StringHashMap(void).init(agent.gc_allocator);
    defer seen.deinit();
    for (var_scoped_declarations) |var_declaration| {
        const var_name = switch (var_declaration) {
            .variable_declaration => |variable_declaration| variable_declaration.binding_identifier,
            .hoistable_declaration => |hoistable_declaration| switch (hoistable_declaration) {
                inline else => |function_declaration| function_declaration.identifier,
            },
        }.?;
        if (!seen.contains(var_name)) {
            if (var_declaration == .hoistable_declaration) {
                const env = Environment{ .global_environment = global_env };
                const private_env = null;

                var hoistable_declaration = var_declaration.hoistable_declaration;
                switch (hoistable_declaration) {
                    inline else => |*function_declaration| {
                        // Assign the function body's strictness, which is needed for the deferred bytecode generation.
                        // FIXME: This should ideally happen at parse time.
                        function_declaration.function_body.strict = self.ecmascript_code.isStrict() or
                            function_declaration.function_body.functionBodyContainsUseStrict();
                    },
                }

                const function_object = try switch (hoistable_declaration) {
                    .function_declaration => |function_declaration| function_declaration.instantiateOrdinaryFunctionObject(agent, env, private_env),
                    .generator_declaration => |generator_declaration| generator_declaration.instantiateGeneratorFunctionObject(agent, env, private_env),
                    .async_function_declaration => |async_function_declaration| async_function_declaration.instantiateAsyncFunctionObject(agent, env, private_env),
                    .async_generator_declaration => |async_generator_declaration| async_generator_declaration.instantiateAsyncGeneratorFunctionObject(agent, env, private_env),
                };

                try global_env.createGlobalFunctionBinding(var_name, Value.from(function_object), false);
            } else {
                try global_env.createGlobalVarBinding(agent, var_name, false);
            }
            try seen.putNoClobber(var_name, {});
        }
    }

    const result_no_value: error{ExceptionThrown}!void = {};

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
