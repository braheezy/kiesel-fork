//! 9.2 PrivateEnvironment Records
//! https://tc39.es/ecma262/#sec-privateenvironment-records

const std = @import("std");

const Self = @This();

/// [[OuterPrivateEnvironment]]
outer_private_environment: ?*Self,

/// [[Names]]
names: std.ArrayList(void), // TODO: Implement private names
