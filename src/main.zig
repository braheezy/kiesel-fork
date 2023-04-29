const std = @import("std");
const kiesel = @import("kiesel");

const builtins = kiesel.builtins;

const Agent = kiesel.Agent;
const PropertyDescriptor = kiesel.PropertyDescriptor;
const PropertyKey = kiesel.PropertyKey;
const Realm = kiesel.Realm;
const Script = kiesel.Script;
const Value = kiesel.Value;

pub fn main() !void {
    var agent = Agent.init();
    const realm = try Realm.create(agent.allocator);
    const script = try Script.parse(agent.allocator, "", realm, null);
    _ = script;

    const object1 = (try builtins.Object.create(&agent, .{
        .prototype = null,
    })).object();
    _ = try object1.internalMethods().defineOwnProperty(
        object1,
        PropertyKey.fromString("foo"),
        PropertyDescriptor{ .value = Value.fromNumber(123) },
    );
    const object2 = (try builtins.Object.create(&agent, .{
        .prototype = object1,
    })).object();
    const value = try object2.internalMethods().get(object2, PropertyKey.fromString("foo"), Value.fromObject(object2));

    std.debug.print("object2.foo = {any}\n", .{value});
}
