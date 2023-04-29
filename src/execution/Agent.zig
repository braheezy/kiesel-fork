//! 9.7 Agents
//! https://tc39.es/ecma262/#sec-agents

const gc = @import("gc");
const std = @import("std");

const Allocator = std.mem.Allocator;

const Self = @This();

allocator: Allocator,

pub fn init() Self {
    return .{
        .allocator = gc.allocator(),
    };
}
