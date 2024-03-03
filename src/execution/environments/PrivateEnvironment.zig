//! 9.2 PrivateEnvironment Records
//! https://tc39.es/ecma262/#sec-privateenvironment-records

const std = @import("std");

const types = @import("../../types.zig");

const PrivateName = types.PrivateName;
const String = types.String;

const Self = @This();

/// [[OuterPrivateEnvironment]]
outer_private_environment: ?*Self,

/// [[Names]]
names: std.StringHashMap(PrivateName),

/// 9.2.1.2 ResolvePrivateIdentifier ( privEnv, identifier )
/// https://tc39.es/ecma262/#sec-resolve-private-identifier
pub fn resolvePrivateIdentifier(self: Self, identifier: []const u8) PrivateName {
    // 1. Let names be privEnv.[[Names]].
    // 2. For each Private Name pn of names, do
    //     a. If pn.[[Description]] is identifier, then
    if (self.names.get(identifier)) |private_name| {
        // i. Return pn.
        return private_name;
    }

    // 3. Let outerPrivEnv be privEnv.[[OuterPrivateEnvironment]].
    // 4. Assert: outerPrivEnv is not null.
    const outer_private_environment = self.outer_private_environment.?;

    // 5. Return ResolvePrivateIdentifier(outerPrivEnv, identifier).
    return outer_private_environment.resolvePrivateIdentifier(identifier);
}
