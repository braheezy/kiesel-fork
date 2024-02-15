const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const ImportedModulePayload = language.ImportedModulePayload;
const ImportedModuleReferrer = language.ImportedModuleReferrer;
const Job = execution.Job;
const JobCallback = execution.JobCallback;
const Object = types.Object;
const Realm = execution.Realm;
const SafePointer = types.SafePointer;
const SourceTextModule = language.SourceTextModule;
const String = types.String;
const Value = types.Value;
const finishLoadingImportedModule = language.finishLoadingImportedModule;

/// 9.5.2 HostMakeJobCallback ( callback )
/// https://tc39.es/ecma262/#sec-hostmakejobcallback
pub fn hostMakeJobCallback(callback: Object) JobCallback {
    // 1. Return the JobCallback Record { [[Callback]]: callback, [[HostDefined]]: empty }.
    return .{ .callback = callback, .host_defined = SafePointer.null_pointer };
}

/// 9.5.3 HostCallJobCallback ( jobCallback, V, argumentsList )
/// https://tc39.es/ecma262/#sec-hostcalljobcallback
pub fn hostCallJobCallback(
    job_callback: JobCallback,
    this_value: Value,
    arguments_list: []const Value,
) Agent.Error!Value {
    // 1. Assert: IsCallable(jobCallback.[[Callback]]) is true.
    std.debug.assert(Value.from(job_callback.callback).isCallable());

    // 2. Return ? Call(jobCallback.[[Callback]], V, argumentsList).
    return Value.from(job_callback.callback).callAssumeCallable(this_value, arguments_list);
}

/// 9.5.5 HostEnqueuePromiseJob ( job, realm )
/// https://tc39.es/ecma262/#sec-hostenqueuepromisejob
pub fn hostEnqueuePromiseJob(agent: *Agent, job: Job, realm: ?*Realm) Allocator.Error!void {
    try agent.queued_promise_jobs.append(.{ .job = job, .realm = realm });
}

/// 13.3.12.1.1 HostGetImportMetaProperties ( moduleRecord )
/// https://tc39.es/ecma262/#sec-hostgetimportmetaproperties
pub fn hostGetImportMetaProperties(
    module: *SourceTextModule,
) error{}!Agent.HostHooks.ImportMetaProperties {
    // The default implementation of HostGetImportMetaProperties is to return a new empty List.
    const agent = module.realm.agent;
    return Agent.HostHooks.ImportMetaProperties.init(agent.gc_allocator);
}

/// 13.3.12.1.2 HostFinalizeImportMeta ( importMeta, moduleRecord )
/// https://tc39.es/ecma262/#sec-hostfinalizeimportmeta
pub fn hostFinalizeImportMeta(_: Object, _: *SourceTextModule) void {
    // The default implementation of HostFinalizeImportMeta is to return unused.
}

/// 16.2.1.8 HostLoadImportedModule ( referrer, specifier, hostDefined, payload )
/// https://tc39.es/ecma262/#sec-HostLoadImportedModule
pub fn hostLoadImportedModule(
    agent: *Agent,
    referrer: ImportedModuleReferrer,
    specifier: String,
    _: SafePointer,
    payload: ImportedModulePayload,
) Allocator.Error!void {
    const result = agent.throwException(.internal_error, "Module loading is disabled", .{});
    try finishLoadingImportedModule(agent, referrer, specifier, payload, result);
}

/// 19.2.1.2 HostEnsureCanCompileStrings ( calleeRealm, parameterStrings, bodyString, direct )
/// https://tc39.es/ecma262/#sec-hostensurecancompilestrings
pub fn hostEnsureCanCompileStrings(
    _: *Realm,
    _: []const String,
    _: String,
    _: bool,
) error{}!void {
    // The default implementation of HostEnsureCanCompileStrings is to return NormalCompletion(unused).
}

/// 20.2.5 HostHasSourceTextAvailable ( func )
/// https://tc39.es/ecma262/#sec-hosthassourcetextavailable
pub fn hostHasSourceTextAvailable(_: Object) bool {
    // The default implementation of HostHasSourceTextAvailable is to return true.
    return true;
}

/// 25.1.3.7 HostResizeArrayBuffer ( buffer, newByteLength )
/// https://tc39.es/ecma262/#sec-hostresizearraybuffer
pub fn hostResizeArrayBuffer(
    _: *builtins.ArrayBuffer,
    _: u53,
) error{}!Agent.HostHooks.ResizeArrayBufferHandled {
    // The default implementation of HostResizeArrayBuffer is to return NormalCompletion(unhandled).
    return .unhandled;
}

/// 27.2.1.9 HostPromiseRejectionTracker ( promise, operation )
/// https://tc39.es/ecma262/#sec-host-promise-rejection-tracker
pub fn hostPromiseRejectionTracker(
    _: *builtins.Promise,
    _: Agent.HostHooks.PromiseRejectionTrackerOperation,
) void {
    // The default implementation of HostPromiseRejectionTracker is to return unused.
}
