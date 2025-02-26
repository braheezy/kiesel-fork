//! 16.2.1.8 Synthetic Module Records
//! https://tc39.es/ecma262/#sec-synthetic-module-records

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const Object = types.Object;
const Realm = execution.Realm;
const ResolvedBindingOrAmbiguous = language.ResolvedBindingOrAmbiguous;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const containsSlice = utils.containsSlice;
const newModuleEnvironment = execution.newModuleEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const promiseResolve = builtins.promiseResolve;

const SyntheticModule = @This();

/// [[Realm]]
realm: *Realm,

/// [[Environment]]
environment: ?Environment,

/// [[Namespace]]
namespace: ?*Object,

/// [[HostDefined]]
host_defined: SafePointer,

/// [[ExportNames]]
export_names: []const []const u8,

/// [[EvaluationSteps]]
evaluation_steps: EvaluationSteps,

pub const EvaluationSteps = struct {
    func: *const fn (module: *SyntheticModule, captures: SafePointer) Agent.Error!void,
    captures: SafePointer,
};

/// 16.2.1.8.1 CreateDefaultExportSyntheticModule ( defaultExport )
/// https://tc39.es/ecma262/#sec-create-default-export-synthetic-module
pub fn createDefaultExportSyntheticModule(
    agent: *Agent,
    default_export: Value,
) std.mem.Allocator.Error!*SyntheticModule {
    // 1. Let realm be the current Realm Record.
    const realm = agent.currentRealm();

    const Captures = struct {
        agent: *Agent,
        default_export: Value,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .agent = agent, .default_export = default_export };

    // 2. Let setDefaultExport be a new Abstract Closure with parameters (module) that captures
    //    defaultExport and performs the following steps when called:
    const setDefaultExport = struct {
        fn func(
            module: *SyntheticModule,
            captures_: SafePointer,
        ) Agent.Error!void {
            const agent_ = captures_.cast(*Captures).agent;
            const default_export_ = captures_.cast(*Captures).default_export;

            // a. Perform SetSyntheticModuleExport(module, "default", defaultExport).
            try module.setSyntheticModuleExport(agent_, "default", default_export_);

            // b. Return NormalCompletion(unused).
        }
    }.func;

    // 3. Return the Synthetic Module Record {
    //      [[Realm]]: realm, [[Environment]]: empty, [[Namespace]]: empty,
    //      [[HostDefined]]: undefined, [[ExportNames]]: « "default" »,
    //      [[EvaluationSteps]]: setDefaultExport
    //    }.
    const self = try agent.gc_allocator.create(SyntheticModule);
    self.* = .{
        .realm = realm,
        .environment = null,
        .namespace = null,
        .host_defined = .null_pointer,
        .export_names = &.{"default"},
        .evaluation_steps = .{ .func = setDefaultExport, .captures = .make(*Captures, captures) },
    };
    return self;
}

/// 16.2.1.8.2 ParseJSONModule ( source )
/// https://tc39.es/ecma262/#sec-parse-json-module
pub fn parseJSONModule(agent: *Agent, source: *const String) Agent.Error!*SyntheticModule {
    const realm = agent.currentRealm();

    // 1. Let json be ? Call(%JSON.parse%, undefined, « source »).
    const json = try Value.from(try realm.intrinsics.@"%JSON.parse%"()).callAssumeCallable(
        agent,
        .undefined,
        &.{Value.from(source)},
    );

    // 2. Return CreateDefaultExportSyntheticModule(json).
    return createDefaultExportSyntheticModule(agent, json);
}

/// 16.2.1.8.3 SetSyntheticModuleExport ( module, exportName, exportValue )
/// https://tc39.es/ecma262/#sec-setsyntheticmoduleexport
fn setSyntheticModuleExport(
    self: *SyntheticModule,
    agent: *Agent,
    export_name: []const u8,
    export_value: Value,
) std.mem.Allocator.Error!void {
    // 1. Assert: module.[[ExportNames]] contains exportName.
    std.debug.assert(containsSlice(self.export_names, export_name));

    // 2. Let envRec be module.[[Environment]].
    // 3. Assert: envRec is not empty.
    const env = self.environment.?;

    // 4. Perform envRec.SetMutableBinding(exportName, exportValue, true).
    env.setMutableBinding(
        agent,
        try String.fromUtf8(agent.gc_allocator, export_name),
        export_value,
        true,
    ) catch |err| try noexcept(err);

    // 5. Return unused.
}

/// 16.2.1.8.4.1 LoadRequestedModules ( )
/// https://tc39.es/ecma262/#sec-smr-LoadRequestedModules
pub fn loadRequestedModules(
    _: *SyntheticModule,
    agent: *Agent,
) std.mem.Allocator.Error!*builtins.Promise {
    const realm = agent.currentRealm();

    // 1. Return ! PromiseResolve(%Promise%, undefined).
    const promise = promiseResolve(
        agent,
        try realm.intrinsics.@"%Promise%"(),
        .undefined,
    ) catch |err| try noexcept(err);
    return promise.as(builtins.Promise);
}

/// 16.2.1.8.4.2 GetExportedNames ( )
/// https://tc39.es/ecma262/#sec-smr-getexportednames
pub fn getExportedNames(
    self: *SyntheticModule,
    agent: *Agent,
) std.mem.Allocator.Error![]const []const u8 {
    // 1. Return module.[[ExportNames]].
    // NOTE: The caller owns the returned memory so we have to dupe this.
    return agent.gc_allocator.dupe([]const u8, self.export_names);
}

/// 16.2.1.8.4.3 ResolveExport ( exportName )
/// https://tc39.es/ecma262/#sec-smr-resolveexport
pub fn resolveExport(
    self: *SyntheticModule,
    export_name: []const u8,
) error{}!?ResolvedBindingOrAmbiguous {
    // 1. If module.[[ExportNames]] does not contain exportName, return null.
    if (!containsSlice(self.export_names, export_name)) return null;

    // 2. Return ResolvedBinding Record { [[Module]]: module, [[BindingName]]: exportName }.
    return .{
        .resolved_binding = .{
            .module = .{ .synthetic_module = self },
            .binding_name = .{ .string = export_name },
        },
    };
}

/// 16.2.1.8.4.4 Link ( )
/// https://tc39.es/ecma262/#sec-smr-Link
pub fn link(self: *SyntheticModule, agent: *Agent) std.mem.Allocator.Error!void {
    // 1. Let realm be module.[[Realm]].
    const realm = self.realm;

    // 2. Let env be NewModuleEnvironment(realm.[[GlobalEnv]]).
    const env: Environment = .{
        .module_environment = try newModuleEnvironment(agent.gc_allocator, .{
            .global_environment = realm.global_env,
        }),
    };

    // 3. Set module.[[Environment]] to env.
    self.environment = env;

    // 4. For each String exportName of module.[[ExportNames]], do
    for (self.export_names) |export_name| {
        const name = try String.fromUtf8(agent.gc_allocator, export_name);

        // a. Perform ! env.CreateMutableBinding(exportName, false).
        env.createMutableBinding(agent, name, false) catch |err| try noexcept(err);

        // b. Perform ! env.InitializeBinding(exportName, undefined).
        env.initializeBinding(agent, name, .undefined) catch |err| try noexcept(err);
    }

    // 5. Return NormalCompletion(unused).
}

/// 16.2.1.8.4.5 Evaluate ( )
/// https://tc39.es/ecma262/#sec-smr-Evaluate
pub fn evaluate(self: *SyntheticModule, agent: *Agent) std.mem.Allocator.Error!*builtins.Promise {
    const realm = agent.currentRealm();

    // 1. Let moduleContext be a new ECMAScript code execution context.
    const module_context = try agent.gc_allocator.create(ExecutionContext);
    module_context.* = .{
        // 2. Set the Function of moduleContext to null.
        .origin = .module,

        // 3. Set the Realm of moduleContext to module.[[Realm]].
        .realm = self.realm,

        // 4. Set the ScriptOrModule of moduleContext to module.
        .script_or_module = .{ .module = .{ .synthetic_module = self } },

        .ecmascript_code = .{
            // 5. Set the VariableEnvironment of moduleContext to module.[[Environment]].
            .variable_environment = self.environment.?,

            // 6. Set the LexicalEnvironment of moduleContext to module.[[Environment]].
            .lexical_environment = self.environment.?,

            .private_environment = null,
        },
    };

    // 7. Suspend the running execution context.

    // 8. Push moduleContext onto the execution context stack; moduleContext is now the running
    //    execution context.
    try agent.execution_context_stack.append(agent.gc_allocator, module_context);

    // 9. Let steps be module.[[EvaluationSteps]].
    const steps = self.evaluation_steps;

    // 10. Let result be Completion(steps(module)).
    const result = steps.func(self, steps.captures);

    // 11. Suspend moduleContext and remove it from the execution context stack.
    _ = agent.execution_context_stack.pop().?;

    // 12. Resume the context that is now on the top of the execution context stack as the running
    //     execution context.

    // 13. Let pc be ! NewPromiseCapability(%Promise%).
    const promise_capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 14. IfAbruptRejectPromise(result, pc).
    result catch |err| {
        const promise = promise_capability.rejectPromise(agent, err) catch |err_| try noexcept(err_);
        return promise.as(builtins.Promise);
    };

    // 15. Perform ! Call(pc.[[Resolve]], undefined, « undefined »).
    _ = Value.from(promise_capability.resolve).callAssumeCallable(
        agent,
        .undefined,
        &.{.undefined},
    ) catch |err| try noexcept(err);

    // 16. Return pc.[[Promise]].
    return promise_capability.promise.as(builtins.Promise);
}
