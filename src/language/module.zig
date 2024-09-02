const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const PromiseCapability = @import("../builtins/promise.zig").PromiseCapability;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const Script = language.Script;
const SourceTextModule = language.SourceTextModule;
const String = types.String;
const Value = types.Value;
const continueModuleLoading = language.continueModuleLoading;
const createBuiltinFunction = builtins.createBuiltinFunction;
const moduleNamespaceCreate = builtins.moduleNamespaceCreate;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const performPromiseThen = builtins.performPromiseThen;

/// 16.2.1.4 Abstract Module Records
/// https://tc39.es/ecma262/#sec-abstract-module-records
pub const Module = union(enum) {
    source_text_module: *SourceTextModule,

    pub fn loadRequestedModules(
        self: Module,
        agent: *Agent,
        host_defined: ?SafePointer,
    ) std.mem.Allocator.Error!*builtins.Promise {
        return switch (self) {
            inline else => |module| module.loadRequestedModules(agent, host_defined),
        };
    }

    pub fn getExportedNames(
        self: Module,
        agent: *Agent,
    ) std.mem.Allocator.Error![]const []const u8 {
        return switch (self) {
            inline else => |module| module.getExportedNames(agent),
        };
    }

    pub fn resolveExport(
        self: Module,
        agent: *Agent,
        export_name: []const u8,
    ) std.mem.Allocator.Error!?ResolvedBindingOrAmbiguous {
        return switch (self) {
            inline else => |module| module.resolveExport(agent, export_name),
        };
    }

    pub fn link(self: Module, agent: *Agent) Agent.Error!void {
        return switch (self) {
            inline else => |module| module.link(agent),
        };
    }

    pub fn evaluate(self: Module, agent: *Agent) std.mem.Allocator.Error!*builtins.Promise {
        return switch (self) {
            inline else => |module| module.evaluate(agent),
        };
    }
};

/// https://tc39.es/ecma262/#graphloadingstate-record
pub const GraphLoadingState = struct {
    pub const Visited = std.AutoHashMap(*SourceTextModule, void);

    /// [[PromiseCapability]]
    promise_capability: PromiseCapability,

    /// [[IsLoading]]
    is_loading: bool,

    /// [[PendingModulesCount]]
    pending_modules_count: usize,

    /// [[Visited]]
    visited: Visited,

    /// [[HostDefined]]
    host_defined: SafePointer,
};

pub const ImportedModuleReferrer = union(enum) {
    script: *Script,
    module: *SourceTextModule,
    realm: *Realm,
};

pub const ImportedModulePayload = union(enum) {
    graph_loading_state: *GraphLoadingState,
    promise_capability: PromiseCapability,
};

/// https://tc39.es/ecma262/#resolvedbinding-record
pub const ResolvedBinding = struct {
    /// [[Module]]
    module: Module,

    /// [[BindingName]]
    binding_name: union(enum) {
        string: []const u8,
        namespace,
    },
};

pub const ResolvedBindingOrAmbiguous = union(enum) {
    resolved_binding: ResolvedBinding,
    ambiguous,
};

/// 13.3.10.1.1 ContinueDynamicImport ( promiseCapability, moduleCompletion )
/// https://tc39.es/ecma262/#sec-ContinueDynamicImport
fn continueDynamicImport(
    agent: *Agent,
    promise_capability: PromiseCapability,
    module_completion: Agent.Error!Module,
) std.mem.Allocator.Error!void {
    // 1. If moduleCompletion is an abrupt completion, then
    // 2. Let module be moduleCompletion.[[Value]].
    const module = module_completion catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,

        error.ExceptionThrown => {
            const exception = agent.clearException();

            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « moduleCompletion.[[Value]] »).
            _ = Value.from(promise_capability.reject).callAssumeCallable(
                .undefined,
                &.{exception},
            ) catch |err_| try noexcept(err_);

            // b. Return unused.
            return;
        },
    };

    // 3. Let loadPromise be module.LoadRequestedModules().
    const load_promise = try module.loadRequestedModules(agent, null);

    const RejectedClosureCaptures = struct {
        promise_capability: PromiseCapability,
    };
    const rejected_closure_captures = try agent.gc_allocator.create(RejectedClosureCaptures);
    rejected_closure_captures.* = .{ .promise_capability = promise_capability };

    // 3. Let rejectedClosure be a new Abstract Closure with parameters (reason) that captures
    //    promiseCapability and performs the following steps when called:
    const rejected_closure = struct {
        fn func(agent_: *Agent, _: Value, arguments_: Arguments) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*RejectedClosureCaptures);
            const promise_capability_ = captures_.promise_capability;
            const reason = arguments_.get(0);

            // a. Perform ! Call(promiseCapability.[[Reject]], undefined, « reason »).
            _ = Value.from(promise_capability_.reject).callAssumeCallable(
                .undefined,
                &.{reason},
            ) catch |err| try noexcept(err);

            // b. Return unused.
            return .undefined;
        }
    }.func;

    // 5. Let onRejected be CreateBuiltinFunction(rejectedClosure, 1, "", « »).
    const on_rejected = try createBuiltinFunction(agent, .{ .function = rejected_closure }, .{
        .length = 1,
        .name = "",
        .additional_fields = .make(*RejectedClosureCaptures, rejected_closure_captures),
    });

    const LinkAndEvaluateClosureCaptures = struct {
        module: Module,
        promise_capability: PromiseCapability,
        on_rejected: Object,
    };
    const link_and_evaluate_closure_captures = try agent.gc_allocator.create(LinkAndEvaluateClosureCaptures);
    link_and_evaluate_closure_captures.* = .{
        .module = module,
        .promise_capability = promise_capability,
        .on_rejected = on_rejected,
    };

    // 6. Let linkAndEvaluateClosure be a new Abstract Closure with no parameters that captures
    //    module, promiseCapability, and onRejected and performs the following steps when called:
    const link_and_evaluate_closure = struct {
        fn func(agent_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
            const function = agent_.activeFunctionObject();
            const captures_ = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*LinkAndEvaluateClosureCaptures);
            const promise_capability_ = captures_.promise_capability;
            const module_ = captures_.module;
            const on_rejected_ = captures_.on_rejected;

            // a. Let link be Completion(module.Link()).
            const link = module_.link(agent_);

            // b. If link is an abrupt completion, then
            link catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,

                error.ExceptionThrown => {
                    const exception = agent_.clearException();

                    // i. Perform ! Call(promiseCapability.[[Reject]], undefined, « link.[[Value]] »).
                    _ = Value.from(promise_capability_.reject).callAssumeCallable(
                        .undefined,
                        &.{exception},
                    ) catch |err_| try noexcept(err_);

                    // ii. Return unused.
                    return .undefined;
                },
            };

            // c. Let evaluatePromise be module.Evaluate().
            const evaluate_promise = try module_.evaluate(agent_);

            const FulfilledClosureCaptures = struct {
                module: Module,
                promise_capability: PromiseCapability,
            };
            const fulfilled_closure_captures = try agent_.gc_allocator.create(FulfilledClosureCaptures);
            fulfilled_closure_captures.* = .{
                .module = module_,
                .promise_capability = promise_capability_,
            };

            // d. Let fulfilledClosure be a new Abstract Closure with no parameters that captures module and promiseCapability and performs the following steps when called:
            const fulfilled_closure = struct {
                fn func(agent__: *Agent, _: Value, _: Arguments) Agent.Error!Value {
                    const function_ = agent__.activeFunctionObject();
                    const captures__ = function_.as(builtins.BuiltinFunction).fields.additional_fields.cast(*FulfilledClosureCaptures);
                    const promise_capability__ = captures__.promise_capability;
                    const module__ = captures__.module;

                    // i. Let namespace be GetModuleNamespace(module).
                    const namespace = try getModuleNamespace(agent__, module__);

                    // ii. Perform ! Call(promiseCapability.[[Resolve]], undefined, « namespace »).
                    _ = Value.from(promise_capability__.resolve).callAssumeCallable(
                        .undefined,
                        &.{Value.from(namespace)},
                    ) catch |err| try noexcept(err);

                    // iii. Return unused.
                    return .undefined;
                }
            }.func;

            // e. Let onFulfilled be CreateBuiltinFunction(fulfilledClosure, 0, "", « »).
            const on_fulfilled = try createBuiltinFunction(agent_, .{ .function = fulfilled_closure }, .{
                .length = 0,
                .name = "",
                .additional_fields = .make(*FulfilledClosureCaptures, fulfilled_closure_captures),
            });

            // f. Perform PerformPromiseThen(evaluatePromise, onFulfilled, onRejected).
            _ = try performPromiseThen(
                agent_,
                evaluate_promise,
                Value.from(on_fulfilled),
                Value.from(on_rejected_),
                null,
            );

            // g. Return unused.
            return .undefined;
        }
    }.func;

    // 7. Let linkAndEvaluate be CreateBuiltinFunction(linkAndEvaluateClosure, 0, "", « »).
    const link_and_evaluate = try createBuiltinFunction(agent, .{ .function = link_and_evaluate_closure }, .{
        .length = 0,
        .name = "",
        .additional_fields = .make(
            *LinkAndEvaluateClosureCaptures,
            link_and_evaluate_closure_captures,
        ),
    });

    // 8. Perform PerformPromiseThen(loadPromise, linkAndEvaluate, onRejected).
    _ = try performPromiseThen(
        agent,
        load_promise,
        Value.from(link_and_evaluate),
        Value.from(on_rejected),
        null,
    );

    // 9. Return unused.
}

/// 16.2.1.7 GetImportedModule ( referrer, specifier )
/// https://tc39.es/ecma262/#sec-GetImportedModule
pub fn getImportedModule(referrer: *SourceTextModule, specifier: String) Module {
    // 1. Assert: Exactly one element of referrer.[[LoadedModules]] is a Record whose [[Specifier]]
    //    is specifier, since LoadRequestedModules has completed successfully on referrer prior to
    //    invoking this abstract operation.
    // 2. Let record be the Record in referrer.[[LoadedModules]] whose [[Specifier]] is specifier.
    // 3. Return record.[[Module]].
    return referrer.loaded_modules.get(specifier).?;
}

/// 16.2.1.9 FinishLoadingImportedModule ( referrer, specifier, payload, result )
/// https://tc39.es/ecma262/#sec-FinishLoadingImportedModule
pub fn finishLoadingImportedModule(
    agent: *Agent,
    referrer: ImportedModuleReferrer,
    specifier: String,
    payload: ImportedModulePayload,
    result: Agent.Error!Module,
) std.mem.Allocator.Error!void {
    // 1. If result is a normal completion, then
    if (!std.meta.isError(result)) {
        switch (referrer) {
            inline else => |r| {
                const module = result catch unreachable;
                const get_or_put_result = try r.loaded_modules.getOrPut(specifier);

                // a. If referrer.[[LoadedModules]] contains a Record whose [[Specifier]] is
                //    specifier, then
                if (get_or_put_result.found_existing) {
                    // i. Assert: That Record's [[Module]] is result.[[Value]].
                    std.debug.assert(get_or_put_result.value_ptr.source_text_module == module.source_text_module);
                }
                // b. Else,
                else {
                    // i. Append the Record {
                    //      [[Specifier]]: specifier, [[Module]]: result.[[Value]]
                    //    } to referrer.[[LoadedModules]].
                    get_or_put_result.value_ptr.* = module;
                }
            },
        }
    }

    switch (payload) {
        // 2. If payload is a GraphLoadingState Record, then
        .graph_loading_state => |graph_loading_state| {
            // a. Perform ContinueModuleLoading(payload, result).
            try continueModuleLoading(agent, graph_loading_state, result);
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
pub fn getModuleNamespace(agent: *Agent, module: Module) std.mem.Allocator.Error!Object {
    // 1. Assert: If module is a Cyclic Module Record, then module.[[Status]] is not new or unlinked.
    if (module == .source_text_module) {
        std.debug.assert(switch (module.source_text_module.status) {
            .new, .unlinked => false,
            else => true,
        });
    }

    // 2. Let namespace be module.[[Namespace]].
    var namespace = switch (module) {
        .source_text_module => |source_text_module| source_text_module.namespace,
    };

    // 3. If namespace is empty, then
    if (namespace == null) {
        // a. Let exportedNames be module.GetExportedNames().
        const exported_names = try module.getExportedNames(agent);
        defer agent.gc_allocator.free(exported_names);

        // b. Let unambiguousNames be a new empty List.
        var unambiguous_names = std.ArrayList([]const u8).init(agent.gc_allocator);
        defer unambiguous_names.deinit();

        // c. For each element name of exportedNames, do
        for (exported_names) |name| {
            // i. Let resolution be module.ResolveExport(name).
            const resolution = try module.resolveExport(agent, name);

            // ii. If resolution is a ResolvedBinding Record, append name to unambiguousNames.
            if (resolution != null and resolution.? == .resolved_binding) {
                try unambiguous_names.append(name);
            }
        }

        // d. Set namespace to ModuleNamespaceCreate(module, unambiguousNames).
        namespace = try moduleNamespaceCreate(agent, module, unambiguous_names.items);
    }

    // 4. Return namespace.
    return namespace.?;
}
