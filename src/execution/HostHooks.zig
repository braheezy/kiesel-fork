//! D.1 Host Hooks
//! https://tc39.es/ecma262/#sec-host-hooks-summary

const std = @import("std");

const builtins = @import("../builtins.zig");
const default_host_hooks = @import("default_host_hooks.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Cell = builtins.finalization_registry.Cell;
const ImportedModulePayload = language.ImportedModulePayload;
const ImportedModuleReferrer = language.ImportedModuleReferrer;
const Job = @import("Job.zig");
const JobCallback = @import("JobCallback.zig");
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const SourceTextModule = language.SourceTextModule;
const String = types.String;
const Value = types.Value;

pub const ImportMetaProperties = PropertyKey.ArrayHashMap(Value);

pub const ResizeArrayBufferHandled = enum {
    handled,
    unhandled,
};

pub const GrowSharedArrayBufferHandled = enum {
    handled,
    unhandled,
};

pub const PromiseRejectionTrackerOperation = enum {
    reject,
    handle,
};

hostCallJobCallback: *const fn (
    job_callback: JobCallback,
    this_value: Value,
    arguments_list: []const Value,
) Agent.Error!Value = default_host_hooks.hostCallJobCallback,
hostEnqueueFinalizationRegistryCleanupJob: *const fn (
    agent: *Agent,
    cell: *Cell,
) std.mem.Allocator.Error!void = default_host_hooks.hostEnqueueFinalizationRegistryCleanupJob,
hostEnqueueGenericJob: *const fn (
    agent: *Agent,
    job: Job,
    realm: *Realm,
) std.mem.Allocator.Error!void = default_host_hooks.hostEnqueueGenericJob,
hostEnqueuePromiseJob: *const fn (
    agent: *Agent,
    job: Job,
    realm: ?*Realm,
) std.mem.Allocator.Error!void = default_host_hooks.hostEnqueuePromiseJob,
hostEnsureCanAddPrivateElement: *const fn (
    agent: *Agent,
    object: *Object,
) Agent.Error!void = default_host_hooks.hostEnsureCanAddPrivateElement,
hostEnsureCanCompileStrings: *const fn (
    callee_realm: *Realm,
    parameter_strings: []const *const String,
    body_string: *const String,
    direct: bool,
) Agent.Error!void = default_host_hooks.hostEnsureCanCompileStrings,
hostFinalizeImportMeta: *const fn (
    import_meta: *Object,
    module: *SourceTextModule,
) void = default_host_hooks.hostFinalizeImportMeta,
hostGetImportMetaProperties: *const fn (
    module: *SourceTextModule,
) std.mem.Allocator.Error!ImportMetaProperties = default_host_hooks.hostGetImportMetaProperties,
hostGrowSharedArrayBuffer: *const fn (
    buffer: *builtins.SharedArrayBuffer,
    new_byte_length: u53,
) Agent.Error!GrowSharedArrayBufferHandled = default_host_hooks.hostGrowSharedArrayBuffer,
hostHasSourceTextAvailable: *const fn (
    func: *Object,
) bool = default_host_hooks.hostHasSourceTextAvailable,
hostLoadImportedModule: *const fn (
    agent: *Agent,
    referrer: ImportedModuleReferrer,
    specifier: *const String,
    host_defined: SafePointer,
    payload: ImportedModulePayload,
) std.mem.Allocator.Error!void = default_host_hooks.hostLoadImportedModule,
hostMakeJobCallback: *const fn (
    callback: *Object,
) JobCallback = default_host_hooks.hostMakeJobCallback,
hostPromiseRejectionTracker: *const fn (
    promise: *builtins.Promise,
    operation: PromiseRejectionTrackerOperation,
) void = default_host_hooks.hostPromiseRejectionTracker,
hostResizeArrayBuffer: *const fn (
    buffer: *builtins.ArrayBuffer,
    new_byte_length: u53,
) Agent.Error!ResizeArrayBufferHandled = default_host_hooks.hostResizeArrayBuffer,
