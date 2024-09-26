//! 6.2.4 The Completion Record Specification Type
//! https://tc39.es/ecma262/#sec-completion-record-specification-type

const std = @import("std");

const Value = @import("../language.zig").Value;

const Completion = @This();

pub const Type = enum {
    normal,
    @"break",
    @"continue",
    @"return",
    throw,
};

/// [[Type]]
type: Type,

/// [[Value]]
value: ?Value,

/// [[Target]]
target: ?[]const u8 = null,

/// 6.2.4.1 NormalCompletion ( value )
/// https://tc39.es/ecma262/#sec-normalcompletion
pub fn normal(value: ?Value) Completion {
    // 1. Return Completion Record { [[Type]]: normal, [[Value]]: value, [[Target]]: empty }.
    return .{ .type = .normal, .value = value, .target = null };
}

/// 6.2.4.2 ThrowCompletion ( value )
/// https://tc39.es/ecma262/#sec-throwcompletion
pub fn throw(value: Value) Completion {
    // 1. Return Completion Record { [[Type]]: throw, [[Value]]: value, [[Target]]: empty }.
    return .{ .type = .throw, .value = value, .target = null };
}

/// 6.2.4.3 ReturnCompletion ( value )
/// https://tc39.es/ecma262/#sec-returncompletion
pub fn @"return"(value: Value) Completion {
    // 1. Return Completion Record { [[Type]]: return, [[Value]]: value, [[Target]]: empty }.
    return .{ .type = .@"return", .value = value, .target = null };
}

test normal {
    const completion = normal(.undefined);
    try std.testing.expectEqualDeep(
        Completion{ .type = .normal, .value = .undefined, .target = null },
        completion,
    );
}

test throw {
    const completion = throw(.undefined);
    try std.testing.expectEqualDeep(
        Completion{ .type = .throw, .value = .undefined, .target = null },
        completion,
    );
}

test @"return" {
    const completion = @"return"(.undefined);
    try std.testing.expectEqualDeep(
        Completion{ .type = .@"return", .value = .undefined, .target = null },
        completion,
    );
}
