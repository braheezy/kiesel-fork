//! 9.5 Jobs and Host Operations to Enqueue Jobs
//! https://tc39.es/ecma262/#sec-jobs

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const SafePointer = types.SafePointer;
const Value = types.Value;

func: *const fn (captures: SafePointer) Agent.Error!Value,
captures: SafePointer,
