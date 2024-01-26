//! 9.5 Jobs and Host Operations to Enqueue Jobs
//! https://tc39.es/ecma262/#sec-jobs

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const SafePointer = types.SafePointer;
const Value = types.Value;

/// https://tc39.es/ecma262/#job
/// A Job is an Abstract Closure with no parameters that initiates an ECMAScript computation when
/// no other ECMAScript computation is currently in progress.
pub const Job = struct {
    func: *const fn (captures: SafePointer) Agent.Error!Value,
    captures: SafePointer,
};

/// 9.5.1 JobCallback Records
/// https://tc39.es/ecma262/#sec-jobcallback-records
pub const JobCallback = struct {
    /// [[Callback]]
    callback: Object,

    /// [[HostDefined]]
    host_defined: SafePointer,
};
