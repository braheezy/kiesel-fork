const std = @import("std");
const kiesel = @import("kiesel");

const Agent = kiesel.Agent;
const Realm = kiesel.Realm;

pub fn main() !void {
    const agent = Agent.init();

    const realm = try Realm.create(agent.allocator);
    _ = realm;

    std.debug.print("Hello world!\n", .{});
}
