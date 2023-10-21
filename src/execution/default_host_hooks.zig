const std = @import("std");

const SafePointer = @import("any-pointer").SafePointer;

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Job = execution.Job;
const JobCallback = execution.JobCallback;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;

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
pub fn hostEnqueuePromiseJob(agent: *Agent, job: Job, realm: ?*Realm) !void {
    try agent.queued_promise_jobs.append(.{ .job = job, .realm = realm });
}

/// 19.2.1.2 HostEnsureCanCompileStrings ( calleeRealm )
/// https://tc39.es/ecma262/#sec-hostensurecancompilestrings
pub fn hostEnsureCanCompileStrings(_: *Realm) !void {
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
) Agent.Error!Agent.HostHooks.ResizeArrayBufferHandled {
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
