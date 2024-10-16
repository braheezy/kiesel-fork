//! 9.5.1 JobCallback Records
//! https://tc39.es/ecma262/#sec-jobcallback-records

const types = @import("../types.zig");

const Object = types.Object;
const SafePointer = types.SafePointer;

/// [[Callback]]
callback: Object,

/// [[HostDefined]]
host_defined: SafePointer,
