//! 6.2.6 The Property Descriptor Specification Type
//! https://tc39.es/ecma262/#sec-property-descriptor-specification-type

const std = @import("std");

const language = @import("../language.zig");

const Object = language.Object;
const Value = language.Value;

const Self = @This();

/// [[Value]]
value: ?Value = null,

/// [[Writable]]
writable: ?bool = false,

/// [[Get]]
get: ?Object = null,

/// [[Set]]
set: ?Object = null,

/// [[Enumerable]]
enumerable: ?bool = false,

/// [[Configurable]]
configurable: ?bool = false,

/// 6.2.6.1 IsAccessorDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isaccessordescriptor
pub inline fn isAccessorDescriptor(self: Self) bool {
    // 1. If Desc is undefined, return false.
    // 2. If Desc has a [[Get]] field, return true.
    // 3. If Desc has a [[Set]] field, return true.
    // 4. Return false.
    return self.get != null or self.set != null;
}

/// 6.2.6.2 IsDataDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isdatadescriptor
pub inline fn isDataDescriptor(self: Self) bool {
    // 1. If Desc is undefined, return false.
    // 2. If Desc has a [[Value]] field, return true.
    // 3. If Desc has a [[Writable]] field, return true.
    // 4. Return false.
    return self.value != null or self.writable != null;
}

/// 6.2.6.3 IsGenericDescriptor ( Desc )
/// https://tc39.es/ecma262/#sec-isgenericdescriptor
pub inline fn isGenericDescriptor(self: Self) bool {
    // 1. If Desc is undefined, return false.
    // 2. If IsAccessorDescriptor(Desc) is true, return false.
    // 3. If IsDataDescriptor(Desc) is true, return false.
    // 4. Return true.
    return !(self.isAccessorDescriptor() or self.isDataDescriptor());
}

test "isAccessorDescriptor" {
    const builtins = @import("../../builtins.zig");
    const Agent = @import("../../execution.zig").Agent;
    var agent = try Agent.init();
    const getter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    const setter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    try std.testing.expect((Self{ .get = getter }).isAccessorDescriptor());
    try std.testing.expect((Self{ .set = setter }).isAccessorDescriptor());
    try std.testing.expect((Self{ .get = getter, .set = setter }).isAccessorDescriptor());
    try std.testing.expect(!(Self{ .value = .undefined }).isAccessorDescriptor());
}

test "isDataDescriptor" {
    try std.testing.expect((Self{ .value = .undefined }).isDataDescriptor());
    try std.testing.expect((Self{ .writable = true }).isDataDescriptor());
    try std.testing.expect(!(Self{ .writable = null }).isDataDescriptor());
}

test "isGenericDescriptor" {
    const builtins = @import("../../builtins.zig");
    const Agent = @import("../../execution.zig").Agent;
    var agent = try Agent.init();
    const setter = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    try std.testing.expect((Self{ .writable = null }).isGenericDescriptor());
    try std.testing.expect(!(Self{ .set = setter }).isGenericDescriptor());
    try std.testing.expect(!(Self{}).isGenericDescriptor());
}
