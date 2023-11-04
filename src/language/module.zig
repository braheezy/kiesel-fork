const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const Realm = execution.Realm;
const Script = language.Script;
const SourceTextModule = language.SourceTextModule;
const String = types.String;
const Value = types.Value;
const moduleNamespaceCreate = builtins.moduleNamespaceCreate;
const noexcept = utils.noexcept;

pub const Module = union(enum) {
    source_text_module: *SourceTextModule,
};

pub const GraphLoadingState = struct {};

pub const ImportedModuleReferrer = union(enum) {
    script: *Script,
    module: *SourceTextModule,
    realm: *Realm,
};

pub const ImportedModulePayload = union(enum) {
    graph_loading_state: GraphLoadingState,
    promise_capability: PromiseCapability,
};

/// 13.3.10.1.1 ContinueDynamicImport ( promiseCapability, moduleCompletion )
/// https://tc39.es/ecma262/#sec-ContinueDynamicImport
fn continueDynamicImport(
    agent: *Agent,
    promise_capability: PromiseCapability,
    module_completion: Agent.Error!Module,
) !void {
    // 1. If moduleCompletion is an abrupt completion, then
    // 2. Let module be moduleCompletion.[[Value]].
    const module = module_completion catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ExceptionThrown => {
            const exception = agent.exception.?;
            agent.exception = null;

            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « moduleCompletion.[[Value]] »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                .undefined,
                .{exception},
            ) catch |err_| try noexcept(err_);

            // b. Return unused.
            return;
        },
    };

    // TODO: 3-8.
    _ = module;

    // 9. Return unused.
}

/// 16.2.1.9 FinishLoadingImportedModule ( referrer, specifier, payload, result )
/// https://tc39.es/ecma262/#sec-FinishLoadingImportedModule
pub fn finishLoadingImportedModule(
    agent: *Agent,
    referrer: ImportedModuleReferrer,
    specifier: String,
    payload: ImportedModulePayload,
    result: Agent.Error!Module,
) error{OutOfMemory}!void {
    _ = specifier;
    _ = referrer;
    // 1. If result is a normal completion, then
    if (!std.meta.isError(result)) {
        // TODO: a. If referrer.[[LoadedModules]] contains a Record whose [[Specifier]] is specifier, then
        //     i. Assert: That Record's [[Module]] is result.[[Value]].
        // TODO: b. Else,
        //     i. Append the Record { [[Specifier]]: specifier, [[Module]]: result.[[Value]] } to referrer.[[LoadedModules]].
    }

    switch (payload) {
        // 2. If payload is a GraphLoadingState Record, then
        .graph_loading_state => {
            // TODO: a. Perform ContinueModuleLoading(payload, result).
        },
        // 3. Else,
        .promise_capability => |promise_capability| {
            // a. Perform ContinueDynamicImport(payload, result).
            try continueDynamicImport(agent, promise_capability, result);
        },
    }

    // 4. Return unused.
}

/// 16.2.1.10 GetModuleNamespace ( module )
/// https://tc39.es/ecma262/#sec-getmodulenamespace
pub fn getModuleNamespace(agent: *Agent, module: Module) !Object {
    // TODO: 1. Assert: If module is a Cyclic Module Record, then module.[[Status]] is not new or unlinked.

    // 2. Let namespace be module.[[Namespace]].
    var namespace = switch (module) {
        .source_text_module => |source_text_module| source_text_module.namespace,
    };

    // 3. If namespace is empty, then
    if (namespace == null) {
        // TODO: a. Let exportedNames be module.GetExportedNames().
        const exported_names = &[_][]const u8{};

        // b. Let unambiguousNames be a new empty List.
        var unambiguous_names = std.ArrayList([]const u8).init(agent.gc_allocator);
        defer unambiguous_names.deinit();

        // c. For each element name of exportedNames, do
        for (exported_names) |name| {
            // TODO: i. Let resolution be module.ResolveExport(name).
            // TODO: ii. If resolution is a ResolvedBinding Record, append name to unambiguousNames.
            _ = name;
        }

        // d. Set namespace to ModuleNamespaceCreate(module, unambiguousNames).
        namespace = try moduleNamespaceCreate(agent, module, unambiguous_names.items);
    }

    // 4. Return namespace.
    return namespace.?;
}
