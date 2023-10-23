//! 16.2.1.6 Source Text Module Records
//! https://tc39.es/ecma262/#sec-source-text-module-records

const std = @import("std");

const SafePointer = @import("any-pointer").SafePointer;

const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");

const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const Parser = @import("Parser.zig");
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const Realm = execution.Realm;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;

const Self = @This();

/// [[Realm]]
realm: *Realm,

/// [[Environment]]
environment: ?Environment,

/// [[ECMAScriptCode]]
ecmascript_code: ast.Module,

/// [[Context]]
context: ?ExecutionContext,

// TODO: [[ImportMeta]], [[ImportEntries]], [[LocalExportEntries]], [[IndirectExportEntries]], [[StarExportEntries]]

/// [[HostDefined]]
host_defined: SafePointer = SafePointer.null_pointer,

// [[HasTLA]]
has_tla: bool,

/// 16.2.1.6.1 ParseModule ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parsemodule
pub fn parse(
    source_text: []const u8,
    realm: *Realm,
    host_defined: SafePointer,
    ctx: Parser.ParseContext,
) !*Self {
    const agent = realm.agent;

    // 1. Let body be ParseText(sourceText, Module).
    // 2. If body is a List of errors, return body.
    const body = try Parser.parse(ast.Module, agent.gc_allocator, source_text, ctx);

    // TODO: 3-11.
    const @"async" = false;

    // 12. Return Source Text Module Record {
    //       [[Realm]]: realm, [[Environment]]: empty, [[Namespace]]: empty, [[CycleRoot]]: empty,
    //       [[HasTLA]]: async, [[AsyncEvaluation]]: false, [[TopLevelCapability]]: empty,
    //       [[AsyncParentModules]]: « », [[PendingAsyncDependencies]]: empty, [[Status]]: new,
    //       [[EvaluationError]]: empty, [[HostDefined]]: hostDefined, [[ECMAScriptCode]]: body,
    //       [[Context]]: empty, [[ImportMeta]]: empty, [[RequestedModules]]: requestedModules,
    //       [[LoadedModules]]: « », [[ImportEntries]]: importEntries, [[LocalExportEntries]]: localExportEntries,
    //       [[IndirectExportEntries]]: indirectExportEntries, [[StarExportEntries]]: starExportEntries,
    //       [[DFSIndex]]: empty, [[DFSAncestorIndex]]: empty
    //     }.
    var self = try agent.gc_allocator.create(Self);
    self.* = .{
        .realm = realm,
        .environment = null,
        .has_tla = @"async",
        .host_defined = host_defined,
        .ecmascript_code = body,
        .context = null,
    };
    return self;
}

/// 16.2.1.6.4 InitializeEnvironment ( )
pub fn initializeEnvironment(self: *Self) !void {
    const agent = self.realm.agent;
    _ = agent;

    // TODO: 1-4.

    // TODO: 5. Let env be NewModuleEnvironment(realm.[[GlobalEnv]]).
    const env = Environment{ .global_environment = self.realm.global_env };

    // 6. Set module.[[Environment]] to env.
    self.environment = env;

    // TODO: 7-25.

    // 26. Return unused.
}

/// 16.2.1.6.5 ExecuteModule ( [ capability ] )
/// https://tc39.es/ecma262/#sec-source-text-module-record-execute-module
pub fn executeModule(self: *Self, capability: ?PromiseCapability) !void {
    const agent = self.realm.agent;

    // 1. Let moduleContext be a new ECMAScript code execution context.
    const module_context = ExecutionContext{
        // 2. Set the Function of moduleContext to null.
        .function = null,

        // 3. Set the Realm of moduleContext to module.[[Realm]].
        .realm = self.realm,

        // 4. Set the ScriptOrModule of moduleContext to module.
        .script_or_module = .{ .module = self },

        // 5. Assert: module has been linked and declarations in its module environment have been
        //    instantiated.
        .ecmascript_code = .{
            // 6. Set the VariableEnvironment of moduleContext to module.[[Environment]].
            .variable_environment = self.environment.?,

            // 7. Set the LexicalEnvironment of moduleContext to module.[[Environment]].
            .lexical_environment = self.environment.?,

            .private_environment = null,
        },
    };

    // TODO: 8. Suspend the running execution context.

    // 9. If module.[[HasTLA]] is false, then
    if (!self.has_tla) {
        // a. Assert: capability is not present.
        std.debug.assert(capability == null);

        // b. Push moduleContext onto the execution context stack; moduleContext is now the running
        //    execution context.
        try agent.execution_context_stack.append(module_context);

        // c. Let result be Completion(Evaluation of module.[[ECMAScriptCode]]).
        const result = generateAndRunBytecode(agent, self.ecmascript_code);

        // d. Suspend moduleContext and remove it from the execution context stack.
        _ = agent.execution_context_stack.pop();

        // TODO: e. Resume the context that is now on the top of the execution context stack as the
        //    running execution context.

        // f. If result is an abrupt completion, then
        //     i. Return ? result.
        _ = try result;
    }
    // 10. Else,
    else {
        // 1. Assert: capability is a PromiseCapability Record.
        std.debug.assert(capability != null);

        // TODO: b. Perform AsyncBlockStart(capability, module.[[ECMAScriptCode]], moduleContext).
    }

    // 11. Return unused.
}
