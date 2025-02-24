//! 16.2.1.7 Source Text Module Records
//! https://tc39.es/ecma262/#sec-source-text-module-records

const std = @import("std");

const ast = @import("ast.zig");
const ast_printing = @import("ast_printing.zig");
const builtins = @import("../builtins.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const GraphLoadingState = language.GraphLoadingState;
const Module = language.Module;
const ModuleRequest = language.ModuleRequest;
const Object = types.Object;
const Parser = @import("Parser.zig");
const PromiseCapability = builtins.promise.PromiseCapability;
const Realm = execution.Realm;
const ResolvedBinding = language.ResolvedBinding;
const ResolvedBindingOrAmbiguous = language.ResolvedBindingOrAmbiguous;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const allImportAttributesSupported = language.allImportAttributesSupported;
const asyncBlockStart = builtins.asyncBlockStart;
const containsSlice = utils.containsSlice;
const createBuiltinFunction = builtins.createBuiltinFunction;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const getImportedModule = language.getImportedModule;
const getModuleNamespace = language.getModuleNamespace;
const instantiateAsyncFunctionObject = language.instantiateAsyncFunctionObject;
const instantiateAsyncGeneratorFunctionObject = language.instantiateAsyncGeneratorFunctionObject;
const instantiateGeneratorFunctionObject = language.instantiateGeneratorFunctionObject;
const instantiateOrdinaryFunctionObject = language.instantiateOrdinaryFunctionObject;
const newModuleEnvironment = execution.newModuleEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const performPromiseThen = builtins.performPromiseThen;

const SourceTextModule = @This();

/// [[Realm]]
realm: *Realm,

/// [[Environment]]
environment: ?Environment,

/// [[Namespace]]
namespace: ?*Object,

/// [[ECMAScriptCode]]
ecmascript_code: ast.Module,

/// [[Context]]
context: ?*ExecutionContext,

/// [[ImportMeta]]
import_meta: ?*Object,

/// [[ImportEntries]]
import_entries: std.ArrayListUnmanaged(ImportEntry),

/// [[LocalExportEntries]]
local_export_entries: std.ArrayListUnmanaged(ExportEntry),

/// [[IndirectExportEntries]]
indirect_export_entries: std.ArrayListUnmanaged(ExportEntry),

/// [[StarExportEntries]]
star_export_entries: std.ArrayListUnmanaged(ExportEntry),

/// [[HostDefined]]
host_defined: SafePointer,

/// [[Status]]
status: Status,

/// [[EvaluationError]]
evaluation_error: ?Agent.Exception,

/// [[DFSIndex]]
dfs_index: ?usize,

/// [[DFSAncestorIndex]]
dfs_ancestor_index: ?usize,

/// [[RequestedModules]]
requested_modules: std.ArrayListUnmanaged(ModuleRequest),

/// [[LoadedModules]]
loaded_modules: ModuleRequest.HashMapUnmanaged(Module),

/// [[CycleRoot]]
cycle_root: ?*SourceTextModule,

/// [[HasTLA]]
has_tla: bool,

/// [[AsyncEvaluation]]
async_evaluation: bool,

/// [[TopLevelCapability]]
top_level_capability: ?PromiseCapability,

/// [[AsyncParentModules]]
async_parent_modules: std.ArrayListUnmanaged(*SourceTextModule),

/// [[PendingAsyncDependencies]]
pending_async_dependencies: ?usize,

const Status = enum {
    new,
    unlinked,
    linking,
    linked,
    evaluating,
    evaluating_async,
    evaluated,
};

/// https://tc39.es/ecma262/#importentry-record
pub const ImportEntry = struct {
    /// [[ModuleRequest]]
    module_request: ModuleRequest,

    /// [[ImportName]]
    import_name: ?union(enum) {
        string: []const u8,
        namespace_object,
    },

    /// [[LocalName]]
    local_name: []const u8,
};

/// https://tc39.es/ecma262/#exportentry-record
pub const ExportEntry = struct {
    /// [[ExportName]]
    export_name: ?[]const u8,

    /// [[ModuleRequest]]
    module_request: ?ModuleRequest,

    /// [[ImportName]]
    import_name: ?union(enum) {
        string: []const u8,
        all,
        all_but_default,
    },

    /// [[LocalName]]
    local_name: ?[]const u8,
};

pub fn print(self: SourceTextModule, writer: anytype) @TypeOf(writer).Error!void {
    try ast_printing.printModule(self.ecmascript_code, writer, 0);
}

/// 16.2.1.6.1 LoadRequestedModules ( [ hostDefined ] )
/// https://tc39.es/ecma262/#sec-LoadRequestedModules
pub fn loadRequestedModules(
    self: *SourceTextModule,
    agent: *Agent,
    host_defined: ?SafePointer,
) std.mem.Allocator.Error!*builtins.Promise {
    const realm = agent.currentRealm();

    // 1. If hostDefined is not present, let hostDefined be empty.

    // 2. Let pc be ! NewPromiseCapability(%Promise%).
    const promise_capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 3. Let state be the GraphLoadingState Record {
    //      [[IsLoading]]: true, [[PendingModulesCount]]: 1, [[Visited]]: « »,
    //      [[PromiseCapability]]: pc, [[HostDefined]]: hostDefined
    //    }.
    const state = try agent.gc_allocator.create(GraphLoadingState);
    state.* = .{
        .is_loading = true,
        .pending_modules_count = 1,
        .visited = .empty,
        .promise_capability = promise_capability,
        .host_defined = host_defined orelse .null_pointer,
    };

    // 4. Perform InnerModuleLoading(state, module).
    try innerModuleLoading(agent, state, .{ .source_text_module = self });

    // 5. Return pc.[[Promise]].
    return promise_capability.promise.as(builtins.Promise);
}

/// 16.2.1.6.1.1 InnerModuleLoading ( state, module )
fn innerModuleLoading(
    agent: *Agent,
    state: *GraphLoadingState,
    module: Module,
) std.mem.Allocator.Error!void {
    // 1. Assert: state.[[IsLoading]] is true.
    std.debug.assert(state.is_loading);

    // 2. If module is a Cyclic Module Record, module.[[Status]] is new, and state.[[Visited]] does
    //    not contain module, then
    if (module == .source_text_module and
        module.source_text_module.status == .new and
        !state.visited.contains(module.source_text_module))
    {
        // a. Append module to state.[[Visited]].
        try state.visited.putNoClobber(agent.gc_allocator, module.source_text_module, {});

        // b. Let requestedModulesCount be the number of elements in module.[[RequestedModules]].
        const requested_modules_count = module.source_text_module.requested_modules.items.len;

        // c. Set state.[[PendingModulesCount]] to state.[[PendingModulesCount]] +
        //    requestedModulesCount.
        state.pending_modules_count += requested_modules_count;

        // d. For each ModuleRequest Record request of module.[[RequestedModules]], do
        for (module.source_text_module.requested_modules.items) |request| {
            // a. If AllImportAttributesSupported(request.[[Attributes]]) is false, then
            if (try allImportAttributesSupported(agent, request.attributes)) |unsupported| {
                // 1. Let error be ThrowCompletion(a newly created SyntaxError object).
                const @"error" = agent.throwException(
                    .syntax_error,
                    "Import attribute '{}' is not supported",
                    .{unsupported},
                );

                // 2. Perform ContinueModuleLoading(state, error).
                try continueModuleLoading(agent, state, @as(Agent.Error!Module, @"error"));
            }
            // ii. Else if module.[[LoadedModules]] contains a LoadedModuleRequest Record record
            //     such that ModuleRequestsEqual(record, request) is true, then
            else if (module.source_text_module.loaded_modules.get(request)) |loaded_module| {
                // 1. Perform InnerModuleLoading(state, record.[[Module]]).
                try innerModuleLoading(agent, state, loaded_module);
            } else {
                // iii. Else,
                // 1. Perform HostLoadImportedModule(module, request, state.[[HostDefined]], state).
                // 2. NOTE: HostLoadImportedModule will call FinishLoadingImportedModule, which
                //    re-enters the graph loading process through ContinueModuleLoading.
                try agent.host_hooks.hostLoadImportedModule(
                    agent,
                    .{ .module = module.source_text_module },
                    request,
                    state.host_defined,
                    .{ .graph_loading_state = state },
                );
            }

            // iv. If state.[[IsLoading]] is false, return unused.
            if (!state.is_loading) return;
        }
    }

    // 3. Assert: state.[[PendingModulesCount]] ≥ 1.
    std.debug.assert(state.pending_modules_count >= 1);

    // 4. Set state.[[PendingModulesCount]] to state.[[PendingModulesCount]] - 1.
    state.pending_modules_count -= 1;

    // 5. If state.[[PendingModulesCount]] = 0, then
    if (state.pending_modules_count == 0) {
        // a. Set state.[[IsLoading]] to false.
        state.is_loading = false;

        // b. For each Cyclic Module Record loaded of state.[[Visited]], do
        var it = state.visited.keyIterator();
        while (it.next()) |ptr| {
            const loaded = ptr.*;

            // i. If loaded.[[Status]] is new, set loaded.[[Status]] to unlinked.
            if (loaded.status == .new) loaded.status = .unlinked;
        }

        // c. Perform ! Call(state.[[PromiseCapability]].[[Resolve]], undefined, « undefined »).
        _ = Value.from(state.promise_capability.resolve).callAssumeCallable(
            .undefined,
            &.{.undefined},
        ) catch |err| try noexcept(err);
    }

    // 6. Return unused.
}

/// 16.2.1.6.1.2 ContinueModuleLoading ( state, moduleCompletion )
/// https://tc39.es/ecma262/#sec-ContinueModuleLoading
pub fn continueModuleLoading(
    agent: *Agent,
    state: *GraphLoadingState,
    module_completion: Agent.Error!Module,
) std.mem.Allocator.Error!void {
    // 1. If state.[[IsLoading]] is false, return unused.
    if (!state.is_loading) return;

    // 2. If moduleCompletion is a normal completion, then
    if (module_completion) |module| {
        // a. Perform InnerModuleLoading(state, moduleCompletion.[[Value]]).
        try innerModuleLoading(agent, state, module);
    }
    // 3. Else,
    else |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,

        error.ExceptionThrown => {
            const exception = agent.clearException();

            // a. Set state.[[IsLoading]] to false.
            state.is_loading = false;

            // b. Perform ! Call(state.[[PromiseCapability]].[[Reject]], undefined, « moduleCompletion.[[Value]] »).
            _ = Value.from(state.promise_capability.reject).callAssumeCallable(
                .undefined,
                &.{exception.value},
            ) catch |err_| try noexcept(err_);
        },
    }

    // 4. Return unused.
}

/// 16.2.1.6.2 Link ( )
/// https://tc39.es/ecma262/#sec-moduledeclarationlinking
pub fn link(self: *SourceTextModule, agent: *Agent) Agent.Error!void {
    // 1. Assert: module.[[Status]] is one of unlinked, linked, evaluating-async, or evaluated.
    std.debug.assert(switch (self.status) {
        .unlinked, .linked, .evaluating_async, .evaluated => true,
        else => false,
    });

    // 2. Let stack be a new empty List.
    var stack: std.ArrayListUnmanaged(*SourceTextModule) = .empty;
    defer stack.deinit(agent.gc_allocator);

    // 3. Let result be Completion(InnerModuleLinking(module, stack, 0)).
    const result = innerModuleLinking(agent, .{ .source_text_module = self }, &stack, 0);

    // 4. If result is an abrupt completion, then
    _ = result catch |err| {
        // a. For each Cyclic Module Record m of stack, do
        for (stack.items) |module| {
            // i. Assert: m.[[Status]] is linking.
            std.debug.assert(module.status == .linking);

            // ii. Set m.[[Status]] to unlinked.
            module.status = .unlinked;
        }

        // b. Assert: module.[[Status]] is unlinked.
        std.debug.assert(self.status == .unlinked);

        // c. Return ? result.
        return err;
    };

    // 5. Assert: module.[[Status]] is one of linked, evaluating-async, or evaluated.
    std.debug.assert(switch (self.status) {
        .linked, .evaluating_async, .evaluated => true,
        else => false,
    });

    // 6. Assert: stack is empty.
    std.debug.assert(stack.items.len == 0);

    // 7. Return unused.
}

/// 16.2.1.6.2.1 InnerModuleLinking ( module, stack, index )
/// https://tc39.es/ecma262/#sec-InnerModuleLinking
fn innerModuleLinking(
    agent: *Agent,
    abstract_module: Module,
    stack: *std.ArrayListUnmanaged(*SourceTextModule),
    index: usize,
) Agent.Error!usize {
    // 1. If module is not a Cyclic Module Record, then
    if (abstract_module != .source_text_module) {
        // a. Perform ? module.Link().
        try abstract_module.link(agent);

        // b. Return index.
        return index;
    }

    const module = abstract_module.source_text_module;

    // 2. If module.[[Status]] is one of linking, linked, evaluating-async, or evaluated, then
    if (switch (module.status) {
        .linking, .linked, .evaluating_async, .evaluated => true,
        else => false,
    }) {
        // a. Return index.
        return index;
    }

    // 3. Assert: module.[[Status]] is unlinked.
    std.debug.assert(module.status == .unlinked);

    // 4. Set module.[[Status]] to linking.
    module.status = .linking;

    // 5. Set module.[[DFSIndex]] to index.
    module.dfs_index = index;

    // 6. Set module.[[DFSAncestorIndex]] to index.
    module.dfs_ancestor_index = index;

    // 7. Set index to index + 1.
    var new_index = index + 1;

    // 8. Append module to stack.
    try stack.append(agent.gc_allocator, module);

    // 9. For each ModuleRequest Record request of module.[[RequestedModules]], do
    for (module.requested_modules.items) |request| {
        // a. Let requiredModule be GetImportedModule(module, request).
        const abstract_required_module = getImportedModule(module, request);

        // b. Set index to ? InnerModuleLinking(requiredModule, stack, index).
        new_index = try innerModuleLinking(agent, abstract_required_module, stack, new_index);

        // c. If requiredModule is a Cyclic Module Record, then
        if (abstract_required_module == .source_text_module) {
            const required_module = abstract_required_module.source_text_module;

            // i. Assert: requiredModule.[[Status]] is one of linking, linked, evaluating-async, or
            //    evaluated.
            std.debug.assert(switch (required_module.status) {
                .linking, .linked, .evaluating_async, .evaluated => true,
                else => false,
            });

            // ii. Assert: requiredModule.[[Status]] is linking if and only if stack contains
            //     requiredModule.
            std.debug.assert((required_module.status == .linking) ==
                (std.mem.indexOfScalar(*SourceTextModule, stack.items, required_module) != null));

            // iii. If requiredModule.[[Status]] is linking, then
            if (required_module.status == .linking) {
                // 1. Set module.[[DFSAncestorIndex]] to min(module.[[DFSAncestorIndex]],
                //    requiredModule.[[DFSAncestorIndex]]).
                module.dfs_ancestor_index = @min(
                    module.dfs_ancestor_index.?,
                    required_module.dfs_ancestor_index.?,
                );
            }
        }
    }

    // 10. Perform ? module.InitializeEnvironment().
    try module.initializeEnvironment();

    // 11. Assert: module occurs exactly once in stack.

    // 12. Assert: module.[[DFSAncestorIndex]] ≤ module.[[DFSIndex]].
    std.debug.assert(module.dfs_ancestor_index.? <= module.dfs_index.?);

    // 13. If module.[[DFSAncestorIndex]] = module.[[DFSIndex]], then
    if (module.dfs_ancestor_index.? == module.dfs_index.?) {
        // a. Let done be false.
        // b. Repeat, while done is false,
        while (true) {
            // i. Let requiredModule be the last element of stack.
            // ii. Remove the last element of stack.
            // iii. Assert: requiredModule is a Cyclic Module Record.
            const required_module = stack.pop().?;

            // iv. Set requiredModule.[[Status]] to linked.
            required_module.status = .linked;

            // v. If requiredModule and module are the same Module Record, set done to true.
            if (required_module == module) break;
        }
    }

    // 14. Return index.
    return new_index;
}

/// 16.2.1.7.1 ParseModule ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parsemodule
pub fn parse(
    source_text: []const u8,
    realm: *Realm,
    host_defined: ?SafePointer,
    options: Parser.Options,
) Parser.Error!*SourceTextModule {
    const agent = realm.agent;

    // 1. Let body be ParseText(sourceText, Module).
    // 2. If body is a List of errors, return body.
    const body = try Parser.parse(ast.Module, agent.gc_allocator, source_text, options);

    // 3. Let requestedModules be the ModuleRequests of body.
    var requested_modules: std.ArrayListUnmanaged(ModuleRequest) = .empty;
    {
        const tmp = try body.moduleRequests(agent.gc_allocator);
        defer agent.gc_allocator.free(tmp);
        try requested_modules.appendSlice(agent.gc_allocator, tmp);
    }

    // 4. Let importEntries be the ImportEntries of body.
    var import_entries: std.ArrayListUnmanaged(ImportEntry) = .empty;
    try body.collectImportEntries(agent.gc_allocator, &import_entries);

    // 5. Let importedBoundNames be ImportedLocalNames(importEntries).
    // NOTE: This is lazily done with a for loop below.

    // 6. Let indirectExportEntries be a new empty List.
    var indirect_export_entries: std.ArrayListUnmanaged(ExportEntry) = .empty;

    // 7. Let localExportEntries be a new empty List.
    var local_export_entries: std.ArrayListUnmanaged(ExportEntry) = .empty;

    // 8. Let starExportEntries be a new empty List.
    var star_export_entries: std.ArrayListUnmanaged(ExportEntry) = .empty;

    // 9. Let exportEntries be the ExportEntries of body.
    var export_entries: std.ArrayListUnmanaged(ExportEntry) = .empty;
    defer export_entries.deinit(agent.gc_allocator);
    try body.collectExportEntries(agent.gc_allocator, &export_entries);

    // 10. For each ExportEntry Record ee of exportEntries, do
    for (export_entries.items) |export_entry| {
        // a. If ee.[[ModuleRequest]] is null, then
        if (export_entry.module_request == null) {
            const import_entry_with_bound_name: ?ImportEntry = for (import_entries.items) |import_entry| {
                if (export_entry.local_name != null and
                    std.mem.eql(u8, import_entry.local_name, export_entry.local_name.?))
                    break import_entry;
            } else null;

            // i. If importedBoundNames does not contain ee.[[LocalName]], then
            if (import_entry_with_bound_name == null) {
                // 1. Append ee to localExportEntries.
                try local_export_entries.append(agent.gc_allocator, export_entry);
            } else {
                // ii. Else,
                // 1. Let ie be the element of importEntries whose [[LocalName]] is ee.[[LocalName]].
                const import_entry = import_entry_with_bound_name.?;

                // 2. If ie.[[ImportName]] is namespace-object, then
                if (import_entry.import_name != null and import_entry.import_name.? == .namespace_object) {
                    // a. NOTE: This is a re-export of an imported module namespace object.
                    // b. Append ee to localExportEntries.
                    try local_export_entries.append(agent.gc_allocator, export_entry);
                } else {
                    // 3. Else,
                    // a. NOTE: This is a re-export of a single name.
                    // b. Append the ExportEntry Record {
                    //      [[ModuleRequest]]: ie.[[ModuleRequest]], [[ImportName]]: ie.[[ImportName]],
                    //      [[LocalName]]: null, [[ExportName]]: ee.[[ExportName]]
                    //    } to indirectExportEntries.
                    try indirect_export_entries.append(agent.gc_allocator, .{
                        .module_request = import_entry.module_request,
                        .import_name = if (import_entry.import_name) |import_name|
                            switch (import_name) {
                                .string => |string| .{ .string = string },
                                .namespace_object => unreachable,
                            }
                        else
                            null,
                        .local_name = null,
                        .export_name = export_entry.export_name,
                    });
                }
            }
        }
        // b. Else if ee.[[ImportName]] is all-but-default, then
        else if (export_entry.import_name != null and export_entry.import_name.? == .all_but_default) {
            // i. Assert: ee.[[ExportName]] is null.
            std.debug.assert(export_entry.export_name == null);

            // ii. Append ee to starExportEntries.
            try star_export_entries.append(agent.gc_allocator, export_entry);
        } else {
            // c. Else,
            // i. Append ee to indirectExportEntries.
            try indirect_export_entries.append(agent.gc_allocator, export_entry);
        }
    }

    // 11. Let async be body Contains await.
    const @"async" = body.hasTla();

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
    const self = try agent.gc_allocator.create(SourceTextModule);
    self.* = .{
        .realm = realm,
        .environment = null,
        .namespace = null,
        .cycle_root = null,
        .has_tla = @"async",
        .async_evaluation = false,
        .top_level_capability = null,
        .async_parent_modules = .empty,
        .pending_async_dependencies = null,
        .status = .new,
        .evaluation_error = null,
        .host_defined = host_defined orelse .null_pointer,
        .ecmascript_code = body,
        .context = null,
        .import_meta = null,
        .requested_modules = requested_modules,
        .import_entries = import_entries,
        .local_export_entries = local_export_entries,
        .indirect_export_entries = indirect_export_entries,
        .star_export_entries = star_export_entries,
        .loaded_modules = .empty,
        .dfs_index = null,
        .dfs_ancestor_index = null,
    };
    return self;
}

/// 16.2.1.6.3 Evaluate ( )
/// https://tc39.es/ecma262/#sec-moduleevaluation
pub fn evaluate(self: *SourceTextModule, agent: *Agent) std.mem.Allocator.Error!*builtins.Promise {
    const realm = agent.currentRealm();
    var module = self;

    // TODO: 1. Assert: This call to Evaluate is not happening at the same time as another call to
    //          Evaluate within the surrounding agent.

    // 2. Assert: module.[[Status]] is one of linked, evaluating-async, or evaluated.
    std.debug.assert(switch (self.status) {
        .linked, .evaluating_async, .evaluated => true,
        else => false,
    });

    // 3. If module.[[Status]] is either evaluating-async or evaluated, set module to
    //    module.[[CycleRoot]].
    if (switch (self.status) {
        .evaluating_async, .evaluated => true,
        else => false,
    }) {
        // FIXME: [[CycleRoot]] is still empty if the module evaluation previously threw.
        // See: https://github.com/tc39/ecma262/issues/2823
        if (module.cycle_root) |cycle_root| module = cycle_root;
    }

    // 4. If module.[[TopLevelCapability]] is not empty, then
    if (module.top_level_capability) |top_level_capability| {
        // a. Return module.[[TopLevelCapability]].[[Promise]].
        return top_level_capability.promise.as(builtins.Promise);
    }

    // 5. Let stack be a new empty List.
    var stack: std.ArrayListUnmanaged(*SourceTextModule) = .empty;
    defer stack.deinit(agent.gc_allocator);

    // 6. Let capability be ! NewPromiseCapability(%Promise%).
    const capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    // 7. Set module.[[TopLevelCapability]] to capability.
    module.top_level_capability = capability;

    // 8. Let result be Completion(InnerModuleEvaluation(module, stack, 0)).
    const result = innerModuleEvaluation(agent, .{ .source_text_module = module }, &stack, 0);

    // 9. If result is an abrupt completion, then
    if (std.meta.isError(result)) _ = result catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,

        error.ExceptionThrown => {
            const exception = agent.clearException();

            // a. For each Cyclic Module Record m of stack, do
            for (stack.items) |m| {
                // i. Assert: m.[[Status]] is evaluating.
                std.debug.assert(m.status == .evaluating);

                // ii. Set m.[[Status]] to evaluated.
                m.status = .evaluated;

                // iii. Set m.[[EvaluationError]] to result.
                m.evaluation_error = exception;
            }

            // b. Assert: module.[[Status]] is evaluated.
            std.debug.assert(module.status == .evaluated);

            // c. Assert: module.[[EvaluationError]] and result are the same Completion Record.

            // d. Perform ! Call(capability.[[Reject]], undefined, « result.[[Value]] »).
            _ = Value.from(capability.reject).callAssumeCallable(
                .undefined,
                &.{exception.value},
            ) catch |err_| try noexcept(err_);
        },
    } else {
        // 10. Else,
        // a. Assert: module.[[Status]] is either evaluating-async or evaluated.
        std.debug.assert(switch (module.status) {
            .evaluating_async, .evaluated => true,
            else => false,
        });

        // b. Assert: module.[[EvaluationError]] is empty.
        std.debug.assert(module.evaluation_error == null);

        // c. If module.[[AsyncEvaluation]] is false, then
        if (!module.async_evaluation) {
            // i. Assert: module.[[Status]] is evaluated.
            std.debug.assert(module.status == .evaluated);

            // ii. Perform ! Call(capability.[[Resolve]], undefined, « undefined »).
            _ = Value.from(capability.resolve).callAssumeCallable(
                .undefined,
                &.{.undefined},
            ) catch |err| try noexcept(err);
        }

        // d. Assert: stack is empty.
        std.debug.assert(stack.items.len == 0);
    }

    // Ensures the promise returned by an async module is resolved
    agent.drainJobQueue();

    // 11. Return capability.[[Promise]].
    return capability.promise.as(builtins.Promise);
}

/// 16.2.1.6.3.1 InnerModuleEvaluation ( module, stack, index )
/// https://tc39.es/ecma262/#sec-innermoduleevaluation
fn innerModuleEvaluation(
    agent: *Agent,
    abstract_module: Module,
    stack: *std.ArrayListUnmanaged(*SourceTextModule),
    index: usize,
) Agent.Error!usize {
    // 1. If module is not a Cyclic Module Record, then
    if (abstract_module != .source_text_module) {
        // a. Let promise be ! module.Evaluate().
        const promise = abstract_module.evaluate(agent) catch |err| try noexcept(err);

        // b. Assert: promise.[[PromiseState]] is not pending.
        std.debug.assert(promise.fields.promise_state != .pending);

        // c. If promise.[[PromiseState]] is rejected, then
        if (promise.fields.promise_state == .rejected) {
            // i. Return ThrowCompletion(promise.[[PromiseResult]]).
            agent.exception = .{
                .value = promise.fields.promise_result,
                // TODO: Capture stack when rejecting a promise
                .stack_trace = &.{},
            };
            return error.ExceptionThrown;
        }

        // d. Return index.
        return index;
    }

    const module = abstract_module.source_text_module;

    // 2. If module.[[Status]] is either evaluating-async or evaluated, then
    if (switch (module.status) {
        .evaluating_async, .evaluated => true,
        else => false,
    }) {
        // a. If module.[[EvaluationError]] is empty, return index.
        if (module.evaluation_error == null) return index;

        // b. Otherwise, return ? module.[[EvaluationError]].
        agent.exception = module.evaluation_error.?;
        return error.ExceptionThrown;
    }

    // 3. If module.[[Status]] is evaluating, return index.
    if (module.status == .evaluating) return index;

    // 4. Assert: module.[[Status]] is linked.
    std.debug.assert(module.status == .linked);

    // 5. Set module.[[Status]] to evaluating.
    module.status = .evaluating;

    // 6. Set module.[[DFSIndex]] to index.
    module.dfs_index = index;

    // 7. Set module.[[DFSAncestorIndex]] to index.
    module.dfs_ancestor_index = index;

    // 8. Set module.[[PendingAsyncDependencies]] to 0.
    module.pending_async_dependencies = 0;

    // 9. Set index to index + 1.
    var new_index = index + 1;

    // 10. Append module to stack.
    try stack.append(agent.gc_allocator, module);

    // 11. For each ModuleRequest Record request of module.[[RequestedModules]], do
    for (module.requested_modules.items) |request| {
        // a. Let requiredModule be GetImportedModule(module, request).
        const abstract_required_module = getImportedModule(module, request);

        // b. Set index to ? InnerModuleEvaluation(requiredModule, stack, index).
        new_index = try innerModuleEvaluation(agent, abstract_required_module, stack, new_index);

        // c. If requiredModule is a Cyclic Module Record, then
        if (abstract_required_module == .source_text_module) {
            var required_module = abstract_required_module.source_text_module;

            // i. Assert: requiredModule.[[Status]] is one of evaluating, evaluating-async, or
            //    evaluated.
            std.debug.assert(switch (required_module.status) {
                .evaluating, .evaluating_async, .evaluated => true,
                else => false,
            });

            // ii. Assert: requiredModule.[[Status]] is evaluating if and only if stack contains
            //     requiredModule.
            std.debug.assert((required_module.status == .evaluating) ==
                (std.mem.indexOfScalar(*SourceTextModule, stack.items, required_module) != null));

            // iii. If requiredModule.[[Status]] is evaluating, then
            if (required_module.status == .evaluating) {
                // 1. Set module.[[DFSAncestorIndex]] to min(module.[[DFSAncestorIndex]],
                //    requiredModule.[[DFSAncestorIndex]]).
                module.dfs_ancestor_index = @min(
                    module.dfs_ancestor_index.?,
                    required_module.dfs_ancestor_index.?,
                );
            } else {
                // iv. Else,
                // 1. Set requiredModule to requiredModule.[[CycleRoot]].
                required_module = required_module.cycle_root.?;

                // 2. Assert: requiredModule.[[Status]] is either evaluating-async or evaluated.
                std.debug.assert(switch (required_module.status) {
                    .evaluating_async, .evaluated => true,
                    else => false,
                });

                // 3. If requiredModule.[[EvaluationError]] is not empty, return
                //    ? requiredModule.[[EvaluationError]].
                if (required_module.evaluation_error) |evaluation_error| {
                    agent.exception = evaluation_error;
                    return error.ExceptionThrown;
                }
            }

            // v. If requiredModule.[[AsyncEvaluation]] is true, then
            if (required_module.async_evaluation) {
                // 1. Set module.[[PendingAsyncDependencies]] to module.[[PendingAsyncDependencies]] + 1.
                module.pending_async_dependencies.? += 1;

                // 2. Append module to requiredModule.[[AsyncParentModules]].
                try required_module.async_parent_modules.append(agent.gc_allocator, module);
            }
        }
    }

    // 12. If module.[[PendingAsyncDependencies]] > 0 or module.[[HasTLA]] is true, then
    if (module.pending_async_dependencies.? > 0 or module.has_tla) {
        // a. Assert: module.[[AsyncEvaluation]] is false and was never previously set to true.
        std.debug.assert(!module.async_evaluation);

        // b. Set module.[[AsyncEvaluation]] to true.
        // c. NOTE: The order in which module records have their [[AsyncEvaluation]] fields
        //    transition to true is significant. (See 16.2.1.6.3.4.)
        module.async_evaluation = true;

        // d. If module.[[PendingAsyncDependencies]] = 0, perform ExecuteAsyncModule(module).
        if (module.pending_async_dependencies.? == 0) {
            try executeAsyncModule(agent, module);
        }
    } else {
        // 13. Else,
        // a. Perform ? module.ExecuteModule().
        try module.executeModule(null);
    }

    // 14. Assert: module occurs exactly once in stack.

    // 15. Assert: module.[[DFSAncestorIndex]] ≤ module.[[DFSIndex]].
    std.debug.assert(module.dfs_ancestor_index.? <= module.dfs_index.?);

    // 16. If module.[[DFSAncestorIndex]] = module.[[DFSIndex]], then
    if (module.dfs_ancestor_index == module.dfs_index) {
        // a. Let done be false.
        // b. Repeat, while done is false,
        while (true) {
            // i. Let requiredModule be the last element of stack.
            // ii. Remove the last element of stack.
            // iii. Assert: requiredModule is a Cyclic Module Record.
            const required_module = stack.pop().?;

            // iv. If requiredModule.[[AsyncEvaluation]] is false, set requiredModule.[[Status]] to evaluated.
            // v. Otherwise, set requiredModule.[[Status]] to evaluating-async.
            required_module.status = if (!required_module.async_evaluation) .evaluated else .evaluating_async;

            // vii. Set requiredModule.[[CycleRoot]] to module.
            required_module.cycle_root = module;

            // vi. If requiredModule and module are the same Module Record, set done to true.
            if (required_module == module) break;
        }
    }

    // 17. Return index.
    return new_index;
}

/// 16.2.1.6.3.2 ExecuteAsyncModule ( module )
/// https://tc39.es/ecma262/#sec-execute-async-module
fn executeAsyncModule(agent: *Agent, module: *SourceTextModule) std.mem.Allocator.Error!void {
    const realm = agent.currentRealm();

    // 1. Assert: module.[[Status]] is either evaluating or evaluating-async.
    std.debug.assert(module.status == .evaluating or module.status == .evaluating_async);

    // 2. Assert: module.[[HasTLA]] is true.
    std.debug.assert(module.has_tla);

    // 3. Let capability be ! NewPromiseCapability(%Promise%).
    const capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);

    const Captures = struct {
        module: *SourceTextModule,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{ .module = module };

    // 4. Let fulfilledClosure be a new Abstract Closure with no parameters that captures module
    //    and performs the following steps when called:
    const fulfilled_closure = struct {
        fn func(agent_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const module_ = captures_.module;

            // a. Perform AsyncModuleExecutionFulfilled(module).
            try asyncModuleExecutionFulfilled(agent_, module_);

            // b. Return undefined.
            return .undefined;
        }
    }.func;

    // 5. Let onFulfilled be CreateBuiltinFunction(fulfilledClosure, 0, "", « »).
    const on_fulfilled = try createBuiltinFunction(agent, .{ .function = fulfilled_closure }, .{
        .length = 0,
        .name = "",
        .additional_fields = .make(*Captures, captures),
    });

    // 6. Let rejectedClosure be a new Abstract Closure with parameters (error) that captures
    //    module and performs the following steps when called:
    const rejected_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*Captures);
            const module_ = captures_.module;
            const @"error" = arguments.get(0);

            // a. Perform AsyncModuleExecutionRejected(module, error).
            try asyncModuleExecutionRejected(module_, .{
                .value = @"error",
                .stack_trace = try agent_.captureStackTrace(),
            });

            // b. Return undefined.
            return .undefined;
        }
    }.func;

    // 7. Let onRejected be CreateBuiltinFunction(rejectedClosure, 0, "", « »).
    const on_rejected = try createBuiltinFunction(agent, .{ .function = rejected_closure }, .{
        .length = 0,
        .name = "",
        .additional_fields = .make(*Captures, captures),
    });

    // 8. Perform PerformPromiseThen(capability.[[Promise]], onFulfilled, onRejected).
    _ = try performPromiseThen(
        agent,
        capability.promise.as(builtins.Promise),
        Value.from(on_fulfilled),
        Value.from(on_rejected),
        null,
    );

    // 9. Perform ! module.ExecuteModule(capability).
    module.executeModule(capability) catch |err| try noexcept(err);

    // 10. Return unused.
}

/// 16.2.1.6.3.3 GatherAvailableAncestors ( module, execList )
/// https://tc39.es/ecma262/#sec-gather-available-ancestors
fn gatherAvailableAncestors(
    agent: *Agent,
    module: *SourceTextModule,
    exec_list: *std.ArrayListUnmanaged(*SourceTextModule),
) std.mem.Allocator.Error!void {
    // 1. For each Cyclic Module Record m of module.[[AsyncParentModules]], do
    for (module.async_parent_modules.items) |m| {
        // a. If execList does not contain m and m.[[CycleRoot]].[[EvaluationError]] is empty, then
        if (std.mem.indexOfScalar(*SourceTextModule, exec_list.items, m) == null and
            m.cycle_root.?.evaluation_error == null)
        {
            // i. Assert: m.[[Status]] is evaluating-async.
            std.debug.assert(m.status == .evaluating_async);

            // ii. Assert: m.[[EvaluationError]] is empty.
            std.debug.assert(m.evaluation_error == null);

            // iii. Assert: m.[[AsyncEvaluation]] is true.
            std.debug.assert(m.async_evaluation == true);

            // iv. Assert: m.[[PendingAsyncDependencies]] > 0.
            std.debug.assert(m.pending_async_dependencies.? > 0);

            // v. Set m.[[PendingAsyncDependencies]] to m.[[PendingAsyncDependencies]] - 1.
            m.pending_async_dependencies.? -= 1;

            // vi. If m.[[PendingAsyncDependencies]] = 0, then
            if (m.pending_async_dependencies.? == 0) {
                // 1. Append m to execList.
                try exec_list.append(agent.gc_allocator, m);

                // 2. If m.[[HasTLA]] is false, perform GatherAvailableAncestors(m, execList).
                if (!m.has_tla) {
                    try gatherAvailableAncestors(agent, m, exec_list);
                }
            }
        }
    }

    // 2. Return unused.
}

/// 16.2.1.6.3.4 AsyncModuleExecutionFulfilled ( module )
/// https://tc39.es/ecma262/#sec-async-module-execution-fulfilled
fn asyncModuleExecutionFulfilled(
    agent: *Agent,
    module: *SourceTextModule,
) std.mem.Allocator.Error!void {
    // 1. If module.[[Status]] is evaluated, then
    if (module.status == .evaluated) {
        // a. Assert: module.[[EvaluationError]] is not empty.
        std.debug.assert(module.evaluation_error != null);

        // b. Return unused.
        return;
    }

    // 2. Assert: module.[[Status]] is evaluating-async.
    std.debug.assert(module.status == .evaluating_async);

    // 3. Assert: module.[[AsyncEvaluation]] is true.
    std.debug.assert(module.async_evaluation == true);

    // 4. Assert: module.[[EvaluationError]] is empty.
    std.debug.assert(module.evaluation_error == null);

    // 5. Set module.[[AsyncEvaluation]] to false.
    module.async_evaluation = false;

    // 6. Set module.[[Status]] to evaluated.
    module.status = .evaluated;

    // 7. If module.[[TopLevelCapability]] is not empty, then
    if (module.top_level_capability) |top_level_capability| {
        // a. Assert: module.[[CycleRoot]] and module are the same Module Record.
        std.debug.assert(module.cycle_root == module);

        // b. Perform ! Call(module.[[TopLevelCapability]].[[Resolve]], undefined, « undefined »).
        _ = Value.from(top_level_capability.resolve).callAssumeCallable(
            .undefined,
            &.{.undefined},
        ) catch |err| try noexcept(err);
    }

    // 8. Let execList be a new empty List.
    var exec_list: std.ArrayListUnmanaged(*SourceTextModule) = .empty;
    defer exec_list.deinit(agent.gc_allocator);

    // 9. Perform GatherAvailableAncestors(module, execList).
    try gatherAvailableAncestors(agent, module, &exec_list);

    // 10. Let sortedExecList be a List whose elements are the elements of execList, in the order
    //     in which they had their [[AsyncEvaluation]] fields set to true in InnerModuleEvaluation.
    // TODO: Implement https://github.com/tc39/ecma262/pull/3353 which does this in a nicer way.

    // 11. Assert: All elements of sortedExecList have their [[AsyncEvaluation]] field set to true,
    //     [[PendingAsyncDependencies]] field set to 0, and [[EvaluationError]] field set to empty.
    for (exec_list.items) |m| {
        std.debug.assert(m.async_evaluation == true);
        std.debug.assert(m.pending_async_dependencies == 0);
        std.debug.assert(m.evaluation_error == null);
    }

    // 12. For each Cyclic Module Record m of sortedExecList, do
    for (exec_list.items) |m| {
        // a. If m.[[Status]] is evaluated, then
        if (m.status == .evaluated) {
            // i. Assert: m.[[EvaluationError]] is not empty.
            std.debug.assert(m.evaluation_error != null);
        }
        // b. Else if m.[[HasTLA]] is true, then
        else if (m.has_tla) {
            // i. Perform ExecuteAsyncModule(m).
            try executeAsyncModule(agent, m);
        } else {
            // c. Else,
            // i. Let result be m.ExecuteModule().
            const result = m.executeModule(null);

            // ii. If result is an abrupt completion, then
            _ = result catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                error.ExceptionThrown => {
                    const exception = agent.clearException();

                    // 1. Perform AsyncModuleExecutionRejected(m, result.[[Value]]).
                    try asyncModuleExecutionRejected(m, exception);
                },
            };
            // iii. Else,

            // 1. Set m.[[AsyncEvaluation]] to false.
            m.async_evaluation = false;

            // 2. Set m.[[Status]] to evaluated.
            m.status = .evaluated;

            // 3. If m.[[TopLevelCapability]] is not empty, then
            if (m.top_level_capability) |top_level_capability| {
                // a. Assert: m.[[CycleRoot]] and m are the same Module Record.
                std.debug.assert(m.cycle_root == m);

                // b. Perform ! Call(m.[[TopLevelCapability]].[[Resolve]], undefined, « undefined »).
                _ = Value.from(top_level_capability.resolve).callAssumeCallable(
                    .undefined,
                    &.{.undefined},
                ) catch |err| try noexcept(err);
            }
        }
    }

    // 13. Return unused.
}

/// 16.2.1.6.3.5 AsyncModuleExecutionRejected ( module, error )
/// https://tc39.es/ecma262/#sec-async-module-execution-rejected
fn asyncModuleExecutionRejected(
    module: *SourceTextModule,
    @"error": Agent.Exception,
) std.mem.Allocator.Error!void {
    // 1. If module.[[Status]] is evaluated, then
    if (module.status == .evaluated) {
        // a. Assert: module.[[EvaluationError]] is not empty.
        std.debug.assert(module.evaluation_error != null);

        // b. Return unused.
        return;
    }

    // 2. Assert: module.[[Status]] is evaluating-async.
    std.debug.assert(module.status == .evaluating_async);

    // 3. Assert: module.[[AsyncEvaluation]] is true.
    std.debug.assert(module.async_evaluation == true);

    // 4. Assert: module.[[EvaluationError]] is empty.
    std.debug.assert(module.evaluation_error == null);

    // 5. Set module.[[EvaluationError]] to ThrowCompletion(error).
    module.evaluation_error = @"error";

    // 6. Set module.[[Status]] to evaluated.
    module.status = .evaluated;

    // 7. Set module.[[AsyncEvaluation]] to false.
    // 8. NOTE: module.[[AsyncEvaluation]] is set to false for symmetry with
    //    AsyncModuleExecutionFulfilled. In InnerModuleEvaluation, the value of a module's
    //    [[AsyncEvaluation]] internal slot is unused when its [[EvaluationError]] internal slot is
    //    not empty.
    module.async_evaluation = false;

    // 9. For each Cyclic Module Record m of module.[[AsyncParentModules]], do
    for (module.async_parent_modules.items) |m| {
        // a. Perform AsyncModuleExecutionRejected(m, error).
        try asyncModuleExecutionRejected(m, @"error");
    }

    // 10. If module.[[TopLevelCapability]] is not empty, then
    if (module.top_level_capability) |top_level_capability| {
        // a. Assert: module.[[CycleRoot]] and module are the same Module Record.
        std.debug.assert(module.cycle_root == module);

        // b. Perform ! Call(module.[[TopLevelCapability]].[[Reject]], undefined, « error »).
        _ = Value.from(top_level_capability.reject).callAssumeCallable(
            .undefined,
            &.{@"error".value},
        ) catch |err| try noexcept(err);
    }

    // 11. Return unused.
}

/// 16.2.1.7.2 GetExportedNames ( [ exportStarSet ] )
/// https://tc39.es/ecma262/#sec-getexportednames
pub fn getExportedNames(
    self: *const SourceTextModule,
    agent: *Agent,
    maybe_export_star_set: ?*Module.ExportStarSet,
) std.mem.Allocator.Error![]const []const u8 {
    // 1. Assert: module.[[Status]] is not new.
    std.debug.assert(self.status != .new);

    // 2. If exportStarSet is not present, set exportStarSet to a new empty List.
    var new_export_star_set: Module.ExportStarSet = undefined;
    defer if (maybe_export_star_set == null) new_export_star_set.deinit(agent.gc_allocator);
    var export_star_set = maybe_export_star_set orelse blk: {
        new_export_star_set = .empty;
        break :blk &new_export_star_set;
    };

    const export_star_set_key: @FieldType(Module.ExportStarSet.KV, "key") = self;

    // 3. If exportStarSet contains module, then
    if (export_star_set.contains(export_star_set_key)) {
        // a. Assert: We've reached the starting point of an export * circularity.
        // b. Return a new empty List.
        return &.{};
    }

    // 4. Append module to exportStarSet.
    try export_star_set.putNoClobber(agent.gc_allocator, export_star_set_key, {});

    // 5. Let exportedNames be a new empty List.
    var exported_names: std.ArrayListUnmanaged([]const u8) = .empty;

    // 6. For each ExportEntry Record e of module.[[LocalExportEntries]], do
    for (self.local_export_entries.items) |export_entry| {
        // a. Assert: module provides the direct binding for this export.
        // b. Assert: e.[[ExportName]] is not null.
        // c. Append e.[[ExportName]] to exportedNames.
        try exported_names.append(agent.gc_allocator, export_entry.export_name.?);
    }

    // 7. For each ExportEntry Record e of module.[[IndirectExportEntries]], do
    for (self.indirect_export_entries.items) |export_entry| {
        // a. Assert: module imports a specific binding for this export.
        // b. Assert: e.[[ExportName]] is not null.
        // c. Append e.[[ExportName]] to exportedNames.
        try exported_names.append(agent.gc_allocator, export_entry.export_name.?);
    }

    // 8. For each ExportEntry Record e of module.[[StarExportEntries]], do
    for (self.star_export_entries.items) |export_entry| {
        // a. Assert: e.[[ModuleRequest]] is not null.
        std.debug.assert(export_entry.module_request != null);

        // b. Let requestedModule be GetImportedModule(module, e.[[ModuleRequest]]).
        const requested_module = getImportedModule(self, export_entry.module_request.?);

        // c. Let starNames be requestedModule.GetExportedNames(exportStarSet).
        const star_names = try requested_module.getExportedNames(agent, export_star_set);

        // d. For each element n of starNames, do
        for (star_names) |name| {
            // i. If n is not "default", then
            if (!std.mem.eql(u8, name, "default")) {
                // 1. If exportedNames does not contain n, then
                if (!containsSlice(exported_names.items, name)) {
                    // a. Append n to exportedNames.
                    try exported_names.append(agent.gc_allocator, name);
                }
            }
        }
    }

    // 9. Return exportedNames.
    return exported_names.toOwnedSlice(agent.gc_allocator);
}

/// 16.2.1.6.3 ResolveExport ( exportName [ , resolveSet ] )
/// https://tc39.es/ecma262/#sec-resolveexport
pub fn resolveExport(
    self: *SourceTextModule,
    agent: *Agent,
    export_name: []const u8,
    maybe_resolve_set: ?*Module.ResolveSet,
) std.mem.Allocator.Error!?ResolvedBindingOrAmbiguous {
    // 1. Assert: module.[[Status]] is not new.
    std.debug.assert(self.status != .new);

    // 2. If resolveSet is not present, set resolveSet to a new empty List.
    var new_resolve_set: Module.ResolveSet = undefined;
    defer if (maybe_resolve_set == null) new_resolve_set.deinit(agent.gc_allocator);
    var resolve_set = maybe_resolve_set orelse blk: {
        new_resolve_set = .empty;
        break :blk &new_resolve_set;
    };

    const resolve_set_key: @FieldType(Module.ResolveSet.KV, "key") = .{
        .module = self,
        .export_name = export_name,
    };

    // 3. For each Record { [[Module]], [[ExportName]] } r of resolveSet, do
    //     a. If module and r.[[Module]] are the same Module Record and exportName is r.[[ExportName]], then
    if (resolve_set.contains(resolve_set_key)) {
        // i. Assert: This is a circular import request.
        // ii. Return null.
        return null;
    }

    // 4. Append the Record { [[Module]]: module, [[ExportName]]: exportName } to resolveSet.
    try resolve_set.putNoClobber(agent.gc_allocator, resolve_set_key, {});

    // 5. For each ExportEntry Record e of module.[[LocalExportEntries]], do
    for (self.local_export_entries.items) |export_entry| {
        // a. If e.[[ExportName]] is exportName, then
        if (std.mem.eql(u8, export_entry.export_name.?, export_name)) {
            // i. Assert: module provides the direct binding for this export.
            // ii. Return ResolvedBinding Record {
            //       [[Module]]: module, [[BindingName]]: e.[[LocalName]]
            //     }.
            return .{
                .resolved_binding = .{
                    .module = .{ .source_text_module = self },
                    .binding_name = .{ .string = export_entry.local_name.? },
                },
            };
        }
    }

    // 6. For each ExportEntry Record e of module.[[IndirectExportEntries]], do
    for (self.indirect_export_entries.items) |export_entry| {
        // a. If e.[[ExportName]] is exportName, then
        if (std.mem.eql(u8, export_entry.export_name.?, export_name)) {
            // i. Assert: e.[[ModuleRequest]] is not null.
            std.debug.assert(export_entry.module_request != null);

            // ii. Let importedModule be GetImportedModule(module, e.[[ModuleRequest]]).
            const imported_module = getImportedModule(self, export_entry.module_request.?);

            // iii. If e.[[ImportName]] is all, then
            if (export_entry.import_name != null and export_entry.import_name.? == .all) {
                // 1. Assert: module does not provide the direct binding for this export.
                // 2. Return ResolvedBinding Record { [[Module]]: importedModule, [[BindingName]]: namespace }.
                return .{
                    .resolved_binding = .{
                        .module = imported_module,
                        .binding_name = .namespace,
                    },
                };
            } else {
                // iv. Else,
                // 1. Assert: module imports a specific binding for this export.
                // 2. Assert: e.[[ImportName]] is a String.
                // 3. Return importedModule.ResolveExport(e.[[ImportName]], resolveSet).
                return imported_module.resolveExport(
                    agent,
                    export_entry.import_name.?.string,
                    resolve_set,
                );
            }
        }
    }

    // 7. If exportName is "default", then
    if (std.mem.eql(u8, export_name, "default")) {
        // a. Assert: A default export was not explicitly defined by this module.
        // b. Return null.
        // c. NOTE: A default export cannot be provided by an export * from "mod" declaration.
        return null;
    }

    // 8. Let starResolution be null.
    var maybe_star_resolution: ?ResolvedBinding = null;

    // 9. For each ExportEntry Record e of module.[[StarExportEntries]], do
    for (self.star_export_entries.items) |export_entry| {
        // a. Assert: e.[[ModuleRequest]] is not null.
        std.debug.assert(export_entry.module_request != null);

        // b. Let importedModule be GetImportedModule(module, e.[[ModuleRequest]]).
        const imported_module = getImportedModule(self, export_entry.module_request.?);

        // c. Let resolution be importedModule.ResolveExport(exportName, resolveSet).
        const maybe_resolution = try imported_module.resolveExport(
            agent,
            export_name,
            resolve_set,
        );

        // d. If resolution is ambiguous, return ambiguous.
        if (maybe_resolution != null and maybe_resolution.? == .ambiguous) return .ambiguous;

        // e. If resolution is not null, then
        if (maybe_resolution != null) {
            // i. Assert: resolution is a ResolvedBinding Record.
            const resolution = maybe_resolution.?.resolved_binding;

            // ii. If starResolution is null, then
            const star_resolution = maybe_star_resolution orelse {
                // 1. Set starResolution to resolution.
                maybe_star_resolution = resolution;
                continue;
            };

            // iii. Else,
            // 1. Assert: There is more than one * import that includes the requested name.

            // 2. If resolution.[[Module]] and starResolution.[[Module]] are not the same Module
            //    Record, return ambiguous.
            if (!std.meta.eql(resolution.module, star_resolution.module)) {
                return .ambiguous;
            }

            // 3. If resolution.[[BindingName]] is not starResolution.[[BindingName]] and either
            //    resolution.[[BindingName]] or starResolution.[[BindingName]] is namespace, return
            //    ambiguous.
            if (std.meta.activeTag(resolution.binding_name) != std.meta.activeTag(star_resolution.binding_name)) {
                std.debug.assert(resolution.binding_name == .namespace or star_resolution.binding_name == .namespace);
                return .ambiguous;
            }

            // 4. If resolution.[[BindingName]] is a String, starResolution.[[BindingName]] is a
            //    String, and resolution.[[BindingName]] is not starResolution.[[BindingName]], return ambiguous.
            if (resolution.binding_name == .string and
                star_resolution.binding_name == .string and
                !std.mem.eql(u8, resolution.binding_name.string, star_resolution.binding_name.string))
            {
                return .ambiguous;
            }
        }
    }

    // 10. Return starResolution.
    return if (maybe_star_resolution) |resolved_binding|
        .{ .resolved_binding = resolved_binding }
    else
        null;
}

/// 16.2.1.6.4 InitializeEnvironment ( )
/// https://tc39.es/ecma262/#sec-source-text-module-record-initialize-environment
pub fn initializeEnvironment(self: *SourceTextModule) Agent.Error!void {
    const agent = self.realm.agent;

    // 1. For each ExportEntry Record e of module.[[IndirectExportEntries]], do
    for (self.indirect_export_entries.items) |export_entry| {
        // a. Assert: e.[[ExportName]] is not null.
        // b. Let resolution be module.ResolveExport(e.[[ExportName]]).
        const maybe_resolution = try self.resolveExport(agent, export_entry.export_name.?, null);

        // c. If resolution is either null or ambiguous, throw a SyntaxError exception.
        if (maybe_resolution) |resolution| switch (resolution) {
            .ambiguous => return agent.throwException(
                .syntax_error,
                "Ambiguous star export '{s}' in module '{}'",
                .{ export_entry.export_name.?, export_entry.module_request.?.specifier },
            ),

            // d. Assert: resolution is a ResolvedBinding Record.
            .resolved_binding => {},
        } else {
            return agent.throwException(
                .syntax_error,
                "No export named '{s}' in module '{}'",
                .{ export_entry.export_name.?, export_entry.module_request.?.specifier },
            );
        }
    }

    // 2. Assert: All named exports from module are resolvable.

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

    // 7. For each ImportEntry Record in of module.[[ImportEntries]], do
    for (self.import_entries.items) |import_entry| {
        // a. Let importedModule be GetImportedModule(module, in.[[ModuleRequest]]).
        const imported_module = getImportedModule(self, import_entry.module_request);

        const import_name = import_entry.import_name.?;
        const local_name = try String.fromUtf8(agent.gc_allocator, import_entry.local_name);

        // b. If in.[[ImportName]] is namespace-object, then
        if (import_name == .namespace_object) {
            // i. Let namespace be GetModuleNamespace(importedModule).
            const namespace = try getModuleNamespace(agent, imported_module);

            // ii. Perform ! env.CreateImmutableBinding(in.[[LocalName]], true).
            env.createImmutableBinding(
                agent,
                local_name,
                true,
            ) catch |err| try noexcept(err);

            // iii. Perform ! env.InitializeBinding(in.[[LocalName]], namespace).
            env.initializeBinding(
                agent,
                local_name,
                Value.from(namespace),
            ) catch |err| try noexcept(err);
        } else {
            // c. Else,
            // i. Let resolution be importedModule.ResolveExport(in.[[ImportName]]).
            const maybe_resolution = try imported_module.resolveExport(
                agent,
                import_name.string,
                null,
            );

            // ii. If resolution is either null or ambiguous, throw a SyntaxError exception.
            const resolution = if (maybe_resolution) |resolution| switch (resolution) {
                .ambiguous => return agent.throwException(
                    .syntax_error,
                    "Ambiguous star export '{s}' in module '{}'",
                    .{ import_name.string, import_entry.module_request.specifier },
                ),
                .resolved_binding => |resolved_binding| resolved_binding,
            } else {
                return agent.throwException(
                    .syntax_error,
                    "No export named '{s}' in module '{}'",
                    .{ import_name.string, import_entry.module_request.specifier },
                );
            };

            // iii. If resolution.[[BindingName]] is namespace, then
            if (resolution.binding_name == .namespace) {
                // 1. Let namespace be GetModuleNamespace(resolution.[[Module]]).
                const namespace = try getModuleNamespace(agent, resolution.module);

                // 2. Perform ! env.CreateImmutableBinding(in.[[LocalName]], true).
                env.createImmutableBinding(
                    agent,
                    local_name,
                    true,
                ) catch |err| try noexcept(err);

                // 3. Perform ! env.InitializeBinding(in.[[LocalName]], namespace).
                env.initializeBinding(
                    agent,
                    local_name,
                    Value.from(namespace),
                ) catch |err| try noexcept(err);
            } else {
                // iv. Else,
                // 1. Perform CreateImportBinding(env, in.[[LocalName]], resolution.[[Module]],
                //    resolution.[[BindingName]]).
                try env.module_environment.createImportBinding(
                    agent,
                    local_name,
                    resolution.module,
                    try String.fromUtf8(agent.gc_allocator, resolution.binding_name.string),
                );
            }
        }
    }

    // 8. Let moduleContext be a new ECMAScript code execution context.
    const module_context = try agent.gc_allocator.create(ExecutionContext);
    module_context.* = .{
        // 9. Set the Function of moduleContext to null.
        .origin = .module,

        // 10. Assert: module.[[Realm]] is not undefined.
        // 11. Set the Realm of moduleContext to module.[[Realm]].
        .realm = self.realm,

        // 12. Set the ScriptOrModule of moduleContext to module.
        .script_or_module = .{ .module = .{ .source_text_module = self } },

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
    try agent.execution_context_stack.append(agent.gc_allocator, module_context);

    // 18. Let code be module.[[ECMAScriptCode]].
    const code = self.ecmascript_code;

    // 19. Let varDeclarations be the VarScopedDeclarations of code.
    var var_declarations: std.ArrayListUnmanaged(ast.VarScopedDeclaration) = .empty;
    defer var_declarations.deinit(agent.gc_allocator);
    try code.collectVarScopedDeclarations(agent.gc_allocator, &var_declarations);

    // 20. Let declaredVarNames be a new empty List.
    var declared_var_names: String.HashMapUnmanaged(void) = .empty;
    defer declared_var_names.deinit(agent.gc_allocator);

    var bound_names: std.ArrayListUnmanaged(ast.Identifier) = .empty;
    defer bound_names.deinit(agent.gc_allocator);

    // 21. For each element d of varDeclarations, do
    for (var_declarations.items) |var_declaration| {
        bound_names.clearRetainingCapacity();
        switch (var_declaration) {
            .variable_declaration => |variable_declaration| try variable_declaration.collectBoundNames(agent.gc_allocator, &bound_names),
            .hoistable_declaration => |hoistable_declaration| switch (hoistable_declaration) {
                inline else => |function_declaration| try bound_names.append(agent.gc_allocator, function_declaration.identifier.?),
            },
        }

        // a. For each element dn of the BoundNames of d, do
        for (bound_names.items) |var_name_utf8| {
            const var_name = try String.fromUtf8(agent.gc_allocator, var_name_utf8);

            // i. If declaredVarNames does not contain dn, then
            if (!declared_var_names.contains(var_name)) {
                // 1. Perform ! env.CreateMutableBinding(dn, false).
                env.createMutableBinding(agent, var_name, false) catch |err| try noexcept(err);

                // 2. Perform ! env.InitializeBinding(dn, undefined).
                env.initializeBinding(agent, var_name, .undefined) catch |err| try noexcept(err);

                // 3. Append dn to declaredVarNames.
                try declared_var_names.putNoClobber(agent.gc_allocator, var_name, {});
            }
        }
    }

    // 22. Let lexDeclarations be the LexicallyScopedDeclarations of code.
    var lex_declarations: std.ArrayListUnmanaged(ast.LexicallyScopedDeclaration) = .empty;
    defer lex_declarations.deinit(agent.gc_allocator);
    try code.collectLexicallyScopedDeclarations(agent.gc_allocator, &lex_declarations);

    // 23. Let privateEnv be null.
    const private_env = null;

    // 24. For each element d of lexDeclarations, do
    for (lex_declarations.items) |declaration| {
        bound_names.clearRetainingCapacity();
        try declaration.collectBoundNames(agent.gc_allocator, &bound_names);

        // a. For each element dn of the BoundNames of d, do
        for (bound_names.items) |name_utf8| {
            const name = try String.fromUtf8(agent.gc_allocator, name_utf8);

            // i. If IsConstantDeclaration of d is true, then
            if (declaration.isConstantDeclaration()) {
                // 1. Perform ! env.CreateImmutableBinding(dn, true).
                env.createImmutableBinding(agent, name, true) catch |err| try noexcept(err);
            } else {
                // ii. Else,
                // 1. Perform ! env.CreateMutableBinding(dn, false).
                env.createMutableBinding(agent, name, false) catch |err| try noexcept(err);
            }

            // iii. If d is either a FunctionDeclaration, a GeneratorDeclaration, an
            //      AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration, then
            if (declaration == .hoistable_declaration) {
                const hoistable_declaration = declaration.hoistable_declaration;

                // 1. Let fo be InstantiateFunctionObject of d with arguments env and privateEnv.
                const function_object = try switch (hoistable_declaration) {
                    .function_declaration => |function_declaration| instantiateOrdinaryFunctionObject(agent, function_declaration, env, private_env),
                    .generator_declaration => |generator_declaration| instantiateGeneratorFunctionObject(agent, generator_declaration, env, private_env),
                    .async_function_declaration => |async_function_declaration| instantiateAsyncFunctionObject(agent, async_function_declaration, env, private_env),
                    .async_generator_declaration => |async_generator_declaration| instantiateAsyncGeneratorFunctionObject(agent, async_generator_declaration, env, private_env),
                };

                // 2. Perform ! env.InitializeBinding(dn, fo).
                env.initializeBinding(
                    agent,
                    name,
                    Value.from(function_object),
                ) catch |err| try noexcept(err);
            }
        }
    }

    // 25. Remove moduleContext from the execution context stack.
    _ = agent.execution_context_stack.pop().?;

    // 26. Return unused.
}

/// 16.2.1.6.5 ExecuteModule ( [ capability ] )
/// https://tc39.es/ecma262/#sec-source-text-module-record-execute-module
pub fn executeModule(self: *SourceTextModule, capability: ?PromiseCapability) Agent.Error!void {
    const agent = self.realm.agent;

    // 1. Let moduleContext be a new ECMAScript code execution context.
    const module_context = try agent.gc_allocator.create(ExecutionContext);
    module_context.* = .{
        // 2. Set the Function of moduleContext to null.
        .origin = .module,

        // 3. Set the Realm of moduleContext to module.[[Realm]].
        .realm = self.realm,

        // 4. Set the ScriptOrModule of moduleContext to module.
        .script_or_module = .{ .module = .{ .source_text_module = self } },

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

    // 8. Suspend the running execution context.

    // 9. If module.[[HasTLA]] is false, then
    if (!self.has_tla) {
        // a. Assert: capability is not present.
        std.debug.assert(capability == null);

        // b. Push moduleContext onto the execution context stack; moduleContext is now the running
        //    execution context.
        try agent.execution_context_stack.append(agent.gc_allocator, module_context);

        // c. Let result be Completion(Evaluation of module.[[ECMAScriptCode]]).
        const result = generateAndRunBytecode(agent, self.ecmascript_code, .{});

        // d. Suspend moduleContext and remove it from the execution context stack.
        _ = agent.execution_context_stack.pop().?;

        // e. Resume the context that is now on the top of the execution context stack as the
        //    running execution context.

        // f. If result is an abrupt completion, then
        //     i. Return ? result.
        _ = try result;
    } else {
        // 10. Else,
        // 1. Assert: capability is a PromiseCapability Record.
        std.debug.assert(capability != null);

        // b. Perform AsyncBlockStart(capability, module.[[ECMAScriptCode]], moduleContext).
        try asyncBlockStart(
            agent,
            capability.?,
            .{ .module = self.ecmascript_code },
            module_context,
        );
    }

    // 11. Return unused.
}
