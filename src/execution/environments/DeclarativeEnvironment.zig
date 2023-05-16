//! 9.1.1.1 Declarative Environment Records
//! https://tc39.es/ecma262/#sec-declarative-environment-records

const environments = @import("../environments.zig");

const Environment = environments.Environment;

/// [[OuterEnv]]
outer_env: ?Environment,
