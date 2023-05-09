const std = @import("std");
const kiesel = @import("kiesel");

const builtins = kiesel.builtins;

const Agent = kiesel.execution.Agent;
const PropertyDescriptor = kiesel.types.PropertyDescriptor;
const PropertyKey = kiesel.types.PropertyKey;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const Value = kiesel.types.Value;

pub fn main() !void {
    var agent = try Agent.init();
    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();
    const script = try Script.parse(agent.allocator, "", realm, null);
    _ = script;

    const object1 = try builtins.Object.create(&agent, .{
        .prototype = null,
    });
    _ = try object1.internalMethods().defineOwnProperty(
        object1,
        PropertyKey.fromString("foo"),
        PropertyDescriptor{ .value = Value.fromNumber(123) },
    );
    const object2 = try builtins.Object.create(&agent, .{
        .prototype = object1,
    });
    const value = try object2.internalMethods().get(object2, PropertyKey.fromString("foo"), Value.fromObject(object2));

    std.debug.print("object2.foo = {any}\n", .{value});
}
