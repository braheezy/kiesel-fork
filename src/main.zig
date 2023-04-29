const std = @import("std");
const kiesel = @import("kiesel");

const Agent = kiesel.Agent;
const Realm = kiesel.Realm;
const Script = kiesel.Script;

pub fn main() !void {
    const agent = Agent.init();
    const realm = try Realm.create(agent.allocator);
    const script = try Script.parse(agent.allocator, "", realm, null);
    _ = script;

    std.debug.print("Hello world!\n", .{});
}
