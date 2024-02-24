//! 16.2.1.6 Source Text Module Records
//! https://tc39.es/ecma262/#sec-source-text-module-records

const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const ast_printing = @import("ast_printing.zig");
const builtins = @import("../builtins.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const ExecutionContext = execution.ExecutionContext;
const GraphLoadingState = language.GraphLoadingState;
const Module = language.Module;
const Object = types.Object;
const Parser = @import("Parser.zig");
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const getImportedModule = language.getImportedModule;
const newModuleEnvironment = execution.newModuleEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
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
host_defined: SafePointer,

/// [[Status]]
status: Status,

/// [[EvaluationError]]
evaluation_error: ?Value,

/// [[DFSIndex]]
dfs_index: ?usize,

/// [[DFSAncestorIndex]]
dfs_ancestor_index: ?usize,

/// [[RequestedModules]]
requested_modules: std.StringArrayHashMap(void),

/// [[LoadedModules]]
loaded_modules: std.StringHashMap(Module),

/// [[CycleRoot]]
cycle_root: ?*Self,

/// [[HasTLA]]
has_tla: bool,

/// [[AsyncEvaluation]]
async_evaluation: bool,

/// [[TopLevelCapability]]
top_level_capability: ?PromiseCapability,

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

pub fn print(self: Self, writer: anytype) @TypeOf(writer).Error!void {
    try ast_printing.printModule(self.ecmascript_code, writer, 0);
}

/// 16.2.1.5.1 LoadRequestedModules ( [ hostDefined ] )
/// https://tc39.es/ecma262/#sec-LoadRequestedModules
pub fn loadRequestedModules(
    self: *Self,
    agent: *Agent,
    host_defined: ?SafePointer,
) Allocator.Error!*builtins.Promise {
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
        .visited = GraphLoadingState.Visited.init(agent.gc_allocator),
        .promise_capability = promise_capability,
        .host_defined = host_defined orelse SafePointer.null_pointer,
    };

    // 4. Perform InnerModuleLoading(state, module).
    try innerModuleLoading(agent, state, .{ .source_text_module = self });

    // 5. Return pc.[[Promise]].
    return promise_capability.promise.as(builtins.Promise);
}

/// 16.2.1.5.1.1 InnerModuleLoading ( state, module )
fn innerModuleLoading(
    agent: *Agent,
    state: *GraphLoadingState,
    module: Module,
) Allocator.Error!void {
    // 1. Assert: state.[[IsLoading]] is true.
    std.debug.assert(state.is_loading);

    // 2. If module is a Cyclic Module Record, module.[[Status]] is new, and state.[[Visited]] does
    //    not contain module, then
    if (module == .source_text_module and
        module.source_text_module.status == .new and
        !state.visited.contains(module.source_text_module))
    {
        // a. Append module to state.[[Visited]].
        try state.visited.putNoClobber(module.source_text_module, {});

        // b. Let requestedModulesCount be the number of elements in module.[[RequestedModules]].
        const requested_modules_count = module.source_text_module.requested_modules.count();

        // c. Set state.[[PendingModulesCount]] to state.[[PendingModulesCount]] +
        //    requestedModulesCount.
        state.pending_modules_count += requested_modules_count;

        // d. For each String required of module.[[RequestedModules]], do
        for (module.source_text_module.requested_modules.keys()) |required| {
            // i. If module.[[LoadedModules]] contains a Record whose [[Specifier]] is required, then
            if (module.source_text_module.loaded_modules.get(required)) |loaded_module| {
                // 1. Let record be that Record.
                // 2. Perform InnerModuleLoading(state, record.[[Module]]).
                try innerModuleLoading(agent, state, loaded_module);
            }
            // ii. Else,
            else {
                // 1. Perform HostLoadImportedModule(module, required, state.[[HostDefined]], state).
                // 2. NOTE: HostLoadImportedModule will call FinishLoadingImportedModule, which
                //    re-enters the graph loading process through ContinueModuleLoading.
                try agent.host_hooks.hostLoadImportedModule(
                    agent,
                    .{ .module = module.source_text_module },
                    String.from(required),
                    state.host_defined,
                    .{ .graph_loading_state = state },
                );
            }

            // iii. If state.[[IsLoading]] is false, return unused.
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

/// 16.2.1.5.1.2 ContinueModuleLoading ( state, moduleCompletion )
/// https://tc39.es/ecma262/#sec-ContinueModuleLoading
pub fn continueModuleLoading(
    agent: *Agent,
    state: *GraphLoadingState,
    module_completion: Agent.Error!Module,
) Allocator.Error!void {
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
                &.{exception},
            ) catch |err_| try noexcept(err_);
        },
    }

    // 4. Return unused.
}

/// 16.2.1.5.2 Link ( )
/// https://tc39.es/ecma262/#sec-moduledeclarationlinking
pub fn link(self: *Self, agent: *Agent) Agent.Error!void {
    // 1. Assert: module.[[Status]] is one of unlinked, linked, evaluating-async, or evaluated.
    std.debug.assert(switch (self.status) {
        .unlinked, .linked, .evaluating_async, .evaluated => true,
        else => false,
    });

    // 2. Let stack be a new empty List.
    var stack = std.ArrayList(*Self).init(agent.gc_allocator);
    defer stack.deinit();

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

/// 16.2.1.5.2.1 InnerModuleLinking ( module, stack, index )
/// https://tc39.es/ecma262/#sec-InnerModuleLinking
fn innerModuleLinking(agent: *Agent, module: Module, stack: *std.ArrayList(*Self), index: usize) Agent.Error!usize {
    // 1. If module is not a Cyclic Module Record, then
    if (module != .source_text_module) {
        // a. Perform ? module.Link().
        try module.link(agent);

        // b. Return index.
        return index;
    }

    // 2. If module.[[Status]] is one of linking, linked, evaluating-async, or evaluated, then
    if (switch (module.source_text_module.status) {
        .linking, .linked, .evaluating_async, .evaluated => true,
        else => false,
    }) {
        // a. Return index.
        return index;
    }

    // 3. Assert: module.[[Status]] is unlinked.
    std.debug.assert(module.source_text_module.status == .unlinked);

    // 4. Set module.[[Status]] to linking.
    module.source_text_module.status = .linking;

    // 5. Set module.[[DFSIndex]] to index.
    module.source_text_module.dfs_index = index;

    // 6. Set module.[[DFSAncestorIndex]] to index.
    module.source_text_module.dfs_ancestor_index = index;

    // 7. Set index to index + 1.
    var new_index = index + 1;

    // 8. Append module to stack.
    try stack.append(module.source_text_module);

    // 9. For each String required of module.[[RequestedModules]], do
    for (module.source_text_module.requested_modules.keys()) |required| {
        // a. Let requiredModule be GetImportedModule(module, required).
        const required_module = getImportedModule(module.source_text_module, String.from(required));

        // b. Set index to ? InnerModuleLinking(requiredModule, stack, index).
        new_index = try innerModuleLinking(agent, required_module, stack, new_index);

        // c. If requiredModule is a Cyclic Module Record, then
        if (required_module == .source_text_module) {
            // i. Assert: requiredModule.[[Status]] is one of linking, linked, evaluating-async, or
            //    evaluated.
            std.debug.assert(switch (required_module.source_text_module.status) {
                .linking, .linked, .evaluating_async, .evaluated => true,
                else => false,
            });

            // ii. Assert: requiredModule.[[Status]] is linking if and only if stack contains
            //     requiredModule.
            std.debug.assert(if (std.mem.indexOfScalar(*Self, stack.items, required_module.source_text_module) != null)
                required_module.source_text_module.status == .linking
            else
                required_module.source_text_module.status != .linking);

            // iii. If requiredModule.[[Status]] is linking, then
            if (required_module.source_text_module.status == .linking) {
                // 1. Set module.[[DFSAncestorIndex]] to min(module.[[DFSAncestorIndex]],
                //    requiredModule.[[DFSAncestorIndex]]).
                module.source_text_module.dfs_ancestor_index = @min(
                    module.source_text_module.dfs_ancestor_index.?,
                    required_module.source_text_module.dfs_ancestor_index.?,
                );
            }
        }
    }

    // 10. Perform ? module.InitializeEnvironment().
    try module.source_text_module.initializeEnvironment();

    // 11. Assert: module occurs exactly once in stack.

    // 12. Assert: module.[[DFSAncestorIndex]] ≤ module.[[DFSIndex]].
    std.debug.assert(module.source_text_module.dfs_ancestor_index.? <= module.source_text_module.dfs_index.?);

    // 13. If module.[[DFSAncestorIndex]] = module.[[DFSIndex]], then
    if (module.source_text_module.dfs_ancestor_index.? == module.source_text_module.dfs_index.?) {
        // a. Let done be false.
        // b. Repeat, while done is false,
        while (true) {
            // i. Let requiredModule be the last element of stack.
            // ii. Remove the last element of stack.
            // iii. Assert: requiredModule is a Cyclic Module Record.
            const required_module = stack.pop();

            // iv. Set requiredModule.[[Status]] to linked.
            required_module.status = .linked;

            // v. If requiredModule and module are the same Module Record, set done to true.
            if (required_module == module.source_text_module) break;
        }
    }

    // 14. Return index.
    return new_index;
}

/// 16.2.1.6.1 ParseModule ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parsemodule
pub fn parse(
    source_text: []const u8,
    realm: *Realm,
    host_defined: ?SafePointer,
    ctx: Parser.ParseContext,
) Parser.Error!*Self {
    const agent = realm.agent;

    // 1. Let body be ParseText(sourceText, Module).
    // 2. If body is a List of errors, return body.
    const body = try Parser.parse(ast.Module, agent.gc_allocator, source_text, ctx);

    // TODO: 3-11.
    const requested_modules = std.StringArrayHashMap(void).init(agent.gc_allocator);
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
        .cycle_root = null,
        .has_tla = @"async",
        .async_evaluation = false,
        .top_level_capability = null,
        .pending_async_dependencies = null,
        .status = .new,
        .evaluation_error = null,
        .host_defined = host_defined orelse SafePointer.null_pointer,
        .ecmascript_code = body,
        .context = null,
        .import_meta = null,
        .requested_modules = requested_modules,
        .loaded_modules = std.StringHashMap(Module).init(agent.gc_allocator),
        .dfs_index = null,
        .dfs_ancestor_index = null,
    };
    return self;
}

/// 16.2.1.5.3 Evaluate ( )
/// https://tc39.es/ecma262/#sec-moduleevaluation
pub fn evaluate(self: *Self, agent: *Agent) Allocator.Error!*builtins.Promise {
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
    var stack = std.ArrayList(*Self).init(agent.gc_allocator);
    defer stack.deinit();

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
                &.{exception},
            ) catch |err_| try noexcept(err_);
        },
    }
    // 10. Else,
    else {
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

    // 11. Return capability.[[Promise]].
    return capability.promise.as(builtins.Promise);
}

/// 16.2.1.5.3.1 InnerModuleEvaluation ( module, stack, index )
/// https://tc39.es/ecma262/#sec-innermoduleevaluation
fn innerModuleEvaluation(
    agent: *Agent,
    module: Module,
    stack: *std.ArrayList(*Self),
    index: usize,
) Agent.Error!usize {
    // 1. If module is not a Cyclic Module Record, then
    if (module != .source_text_module) {
        // a. Let promise be ! module.Evaluate().
        const promise = module.evaluate(agent) catch |err| try noexcept(err);

        // b. Assert: promise.[[PromiseState]] is not pending.
        std.debug.assert(promise.fields.promise_state != .pending);

        // c. If promise.[[PromiseState]] is rejected, then
        if (promise.fields.promise_state == .rejected) {
            // i. Return ThrowCompletion(promise.[[PromiseResult]]).
            agent.exception = promise.fields.promise_result;
            return error.ExceptionThrown;
        }

        // d. Return index.
        return index;
    }

    // 2. If module.[[Status]] is either evaluating-async or evaluated, then
    if (switch (module.source_text_module.status) {
        .evaluating_async, .evaluated => true,
        else => false,
    }) {
        // a. If module.[[EvaluationError]] is empty, return index.
        if (module.source_text_module.evaluation_error == null) return index;

        // b. Otherwise, return ? module.[[EvaluationError]].
        agent.exception = module.source_text_module.evaluation_error.?;
        return error.ExceptionThrown;
    }

    // 3. If module.[[Status]] is evaluating, return index.
    if (module.source_text_module.status == .evaluating) return index;

    // 4. Assert: module.[[Status]] is linked.
    std.debug.assert(module.source_text_module.status == .linked);

    // 5. Set module.[[Status]] to evaluating.
    module.source_text_module.status = .evaluating;

    // 6. Set module.[[DFSIndex]] to index.
    module.source_text_module.dfs_index = index;

    // 7. Set module.[[DFSAncestorIndex]] to index.
    module.source_text_module.dfs_ancestor_index = index;

    // 8. Set module.[[PendingAsyncDependencies]] to 0.
    module.source_text_module.pending_async_dependencies = 0;

    // 9. Set index to index + 1.
    var new_index = index + 1;

    // 10. Append module to stack.
    try stack.append(module.source_text_module);

    // 11. For each String required of module.[[RequestedModules]], do
    for (module.source_text_module.requested_modules.keys()) |required| {
        // a. Let requiredModule be GetImportedModule(module, required).
        var required_module = getImportedModule(module.source_text_module, String.from(required));

        // b. Set index to ? InnerModuleEvaluation(requiredModule, stack, index).
        new_index = try innerModuleEvaluation(agent, required_module, stack, new_index);

        // c. If requiredModule is a Cyclic Module Record, then
        if (required_module == .source_text_module) {
            // i. Assert: requiredModule.[[Status]] is one of evaluating, evaluating-async, or
            //    evaluated.

            // ii. Assert: requiredModule.[[Status]] is evaluating if and only if stack contains
            //     requiredModule.
            std.debug.assert(if (std.mem.indexOfScalar(*Self, stack.items, required_module.source_text_module) != null)
                required_module.source_text_module.status == .evaluating
            else
                required_module.source_text_module.status != .evaluating);

            // iii. If requiredModule.[[Status]] is evaluating, then
            if (required_module.source_text_module.status == .evaluating) {
                // 1. Set module.[[DFSAncestorIndex]] to min(module.[[DFSAncestorIndex]],
                //    requiredModule.[[DFSAncestorIndex]]).
                module.source_text_module.dfs_ancestor_index = @min(
                    module.source_text_module.dfs_ancestor_index.?,
                    required_module.source_text_module.dfs_ancestor_index.?,
                );
            }
            // iv. Else,
            else {
                // 1. Set requiredModule to requiredModule.[[CycleRoot]].
                required_module = .{ .source_text_module = required_module.source_text_module.cycle_root.? };

                // 2. Assert: requiredModule.[[Status]] is either evaluating-async or evaluated.
                std.debug.assert(switch (required_module.source_text_module.status) {
                    .evaluating_async, .evaluated => true,
                    else => false,
                });

                // 3. If requiredModule.[[EvaluationError]] is not empty, return
                //    ? requiredModule.[[EvaluationError]].
                if (required_module.source_text_module.evaluation_error) |evaluation_error| {
                    agent.exception = evaluation_error;
                    return error.ExceptionThrown;
                }
            }

            // v. If requiredModule.[[AsyncEvaluation]] is true, then
            if (required_module.source_text_module.async_evaluation) {
                // 1. Set module.[[PendingAsyncDependencies]] to module.[[PendingAsyncDependencies]] + 1.
                module.source_text_module.pending_async_dependencies.? += 1;

                // TODO: 2. Append module to requiredModule.[[AsyncParentModules]].
            }
        }
    }

    // 12. If module.[[PendingAsyncDependencies]] > 0 or module.[[HasTLA]] is true, then
    if (module.source_text_module.pending_async_dependencies.? > 0 or module.source_text_module.has_tla) {
        // a. Assert: module.[[AsyncEvaluation]] is false and was never previously set to true.
        // b. Set module.[[AsyncEvaluation]] to true.
        // c. NOTE: The order in which module records have their [[AsyncEvaluation]] fields
        //    transition to true is significant. (See 16.2.1.5.3.4.)
        module.source_text_module.async_evaluation = true;

        // TODO: d. If module.[[PendingAsyncDependencies]] = 0, perform ExecuteAsyncModule(module).
    }
    // 13. Else,
    else {
        // a. Perform ? module.ExecuteModule().
        try module.source_text_module.executeModule(null);
    }

    // 14. Assert: module occurs exactly once in stack.

    // 15. Assert: module.[[DFSAncestorIndex]] ≤ module.[[DFSIndex]].
    std.debug.assert(module.source_text_module.dfs_ancestor_index.? <= module.source_text_module.dfs_index.?);

    // 16. If module.[[DFSAncestorIndex]] = module.[[DFSIndex]], then
    if (module.source_text_module.dfs_ancestor_index == module.source_text_module.dfs_index) {
        // a. Let done be false.
        // b. Repeat, while done is false,
        while (true) {
            // i. Let requiredModule be the last element of stack.
            // ii. Remove the last element of stack.
            // iii. Assert: requiredModule is a Cyclic Module Record.
            const required_module = stack.pop();

            // iv. If requiredModule.[[AsyncEvaluation]] is false, set requiredModule.[[Status]] to evaluated.
            // v. Otherwise, set requiredModule.[[Status]] to evaluating-async.
            required_module.status = if (!required_module.async_evaluation) .evaluated else .evaluating_async;

            // vi. If requiredModule and module are the same Module Record, set done to true.
            if (required_module == module.source_text_module) break;

            // vii. Set requiredModule.[[CycleRoot]] to module.
            required_module.cycle_root = module.source_text_module;
        }
    }

    // 17. Return index.
    return new_index;
}

/// 16.2.1.6.4 InitializeEnvironment ( )
/// https://tc39.es/ecma262/#sec-source-text-module-record-initialize-environment
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
        const bound_name = switch (var_declaration) {
            .variable_declaration => |variable_declaration| variable_declaration.binding_identifier,
            .hoistable_declaration => |hoistable_declaration| switch (hoistable_declaration) {
                inline else => |function_declaration| function_declaration.identifier,
            },
        }.?;

        // TODO: a. For each element dn of the BoundNames of d, do
        for ([_]ast.Identifier{bound_name}) |var_name| {
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
        const result = generateAndRunBytecode(agent, self.ecmascript_code, .{});

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
