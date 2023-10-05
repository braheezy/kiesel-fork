//! 9.5 Jobs and Host Operations to Enqueue Jobs
//! https://tc39.es/ecma262/#sec-jobs

const SafePointer = @import("any-pointer").SafePointer;

const types = @import("../types.zig");

const Object = types.Object;

/// 9.5.1 JobCallback Records
/// https://tc39.es/ecma262/#sec-jobcallback-records
pub const JobCallback = struct {
    /// [[Callback]]
    callback: Object,

    /// [[HostDefined]]
    host_defined: SafePointer = SafePointer.null_pointer,
};
