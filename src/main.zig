const std = @import("std");
const kiesel = @import("kiesel");

const Agent = kiesel.Agent;

pub fn main() !void {
    const agent = Agent.init();
    _ = agent;

    std.debug.print("Hello world!\n", .{});
}
