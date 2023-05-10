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
value: Value,

/// [[Target]]
target: ?[]const u8 = null,

/// 6.2.4.1 NormalCompletion ( value )
/// https://tc39.es/ecma262/#sec-normalcompletion
pub fn normal(value: Value) Completion {
    // 1. Return Completion Record { [[Type]]: normal, [[Value]]: value, [[Target]]: empty }.
    return Completion{ .type = .normal, .value = value, .target = null };
}

/// 6.2.4.2 ThrowCompletion ( value )
/// https://tc39.es/ecma262/#sec-throwcompletion
pub fn throw(value: Value) Completion {
    // 1. Return Completion Record { [[Type]]: throw, [[Value]]: value, [[Target]]: empty }.
    return Completion{ .type = .throw, .value = value, .target = null };
}

test "normal" {
    const completion = normal(.undefined);
    try std.testing.expectEqual(completion.type, .normal);
    try std.testing.expectEqual(completion.value, .undefined);
    try std.testing.expectEqual(completion.target, null);
}

test "throw" {
    const completion = throw(.undefined);
    try std.testing.expectEqual(completion.type, .throw);
    try std.testing.expectEqual(completion.value, .undefined);
    try std.testing.expectEqual(completion.target, null);
}
