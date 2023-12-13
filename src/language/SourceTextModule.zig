//! 16.2.1.6 Source Text Module Records
//! https://tc39.es/ecma262/#sec-source-text-module-records

const std = @import("std");

const Allocator = std.mem.Allocator;

const SafePointer = @import("any-pointer").SafePointer;

const ast = @import("ast.zig");
const ast_printing = @import("ast_printing.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const Object = types.Object;
const Parser = @import("Parser.zig");
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const Realm = execution.Realm;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const newModuleEnvironment = execution.newModuleEnvironment;
const noexcept = utils.noexcept;

const Self = @This();

/// [[Realm]]
realm: *Realm,

/// [[Environment]]
environment: ?Environment,

/// [[Namespace]]
namespace: ?Object,

/// [[ECMAScriptCode]]
ecmascript_code: ast.Module,

/// [[Context]]
context: ?ExecutionContext,

/// [[ImportMeta]]
import_meta: ?Object,

// TODO: [[ImportEntries]], [[LocalExportEntries]], [[IndirectExportEntries]], [[StarExportEntries]]

/// [[HostDefined]]
host_defined: SafePointer = SafePointer.null_pointer,

// [[HasTLA]]
has_tla: bool,

pub fn print(self: Self, writer: anytype) @TypeOf(writer).Error!void {
    try ast_printing.printModule(self.ecmascript_code, writer, 0);
}

/// 16.2.1.6.1 ParseModule ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parsemodule
pub fn parse(
    source_text: []const u8,
    realm: *Realm,
    host_defined: SafePointer,
    ctx: Parser.ParseContext,
) Parser.Error!*Self {
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
    const self = try agent.gc_allocator.create(Self);
    self.* = .{
        .realm = realm,
        .environment = null,
        .namespace = null,
        .has_tla = @"async",
        .host_defined = host_defined,
        .ecmascript_code = body,
        .context = null,
        .import_meta = null,
    };
    return self;
}

/// 16.2.1.6.4 InitializeEnvironment ( )
pub fn initializeEnvironment(self: *Self) Allocator.Error!void {
    const agent = self.realm.agent;

    // TODO: 1. For each ExportEntry Record e of module.[[IndirectExportEntries]], do
    //     [...]
    // TODO: 2. Assert: All named exports from module are resolvable.

    // 3. Let realm be module.[[Realm]].
    // 4. Assert: realm is not undefined.
    const realm = self.realm;

    // 5. Let env be NewModuleEnvironment(realm.[[GlobalEnv]]).
    const env: Environment = .{
        .module_environment = try newModuleEnvironment(agent.gc_allocator, .{
            .global_environment = realm.global_env,
        }),
    };

    // 6. Set module.[[Environment]] to env.
    self.environment = env;

    // TODO: 7. For each ImportEntry Record in of module.[[ImportEntries]], do
    //     [...]

    // 8. Let moduleContext be a new ECMAScript code execution context.
    const module_context = ExecutionContext{
        // 9. Set the Function of moduleContext to null.
        .function = null,

        // 10. Assert: module.[[Realm]] is not undefined.
        // 11. Set the Realm of moduleContext to module.[[Realm]].
        .realm = self.realm,

        // 12. Set the ScriptOrModule of moduleContext to module.
        .script_or_module = .{ .module = self },

        .ecmascript_code = .{
            // 13. Set the VariableEnvironment of moduleContext to module.[[Environment]].
            .variable_environment = self.environment.?,

            // 14. Set the LexicalEnvironment of moduleContext to module.[[Environment]].
            .lexical_environment = self.environment.?,

            // 15. Set the PrivateEnvironment of moduleContext to null.
            .private_environment = null,
        },
    };

    // 16. Set module.[[Context]] to moduleContext.
    self.context = module_context;

    // 17. Push moduleContext onto the execution context stack; moduleContext is now the running
    //     execution context.
    try agent.execution_context_stack.append(module_context);

    // 18. Let code be module.[[ECMAScriptCode]].
    const code = self.ecmascript_code;

    // 19. Let varDeclarations be the VarScopedDeclarations of code.
    const var_declarations = try code.module_item_list.varScopedDeclarations(agent.gc_allocator);
    defer agent.gc_allocator.free(var_declarations);

    // 20. Let declaredVarNames be a new empty List.
    var declared_var_names = std.StringHashMap(void).init(agent.gc_allocator);
    defer declared_var_names.deinit();

    // 21. For each element d of varDeclarations, do
    for (var_declarations) |var_declaration| {
        // TODO: a. For each element dn of the BoundNames of d, do
        for ([_]ast.Identifier{var_declaration.binding_identifier}) |var_name| {
            // i. If declaredVarNames does not contain dn, then
            if (!declared_var_names.contains(var_name)) {
                // 1. Perform ! env.CreateMutableBinding(dn, false).
                env.createMutableBinding(agent, var_name, false) catch |err| try noexcept(err);

                // 2. Perform ! env.InitializeBinding(dn, undefined).
                env.initializeBinding(agent, var_name, .undefined) catch |err| try noexcept(err);

                // 3. Append dn to declaredVarNames.
                try declared_var_names.putNoClobber(var_name, {});
            }
        }
    }

    // TODO: 22. Let lexDeclarations be the LexicallyScopedDeclarations of code.
    // TODO: 23. Let privateEnv be null.
    // TODO: 24. For each element d of lexDeclarations, do
    //     [...]

    // 25. Remove moduleContext from the execution context stack.
    _ = agent.execution_context_stack.pop();

    // 26. Return unused.
}

/// 16.2.1.6.5 ExecuteModule ( [ capability ] )
/// https://tc39.es/ecma262/#sec-source-text-module-record-execute-module
pub fn executeModule(self: *Self, capability: ?PromiseCapability) Agent.Error!void {
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
