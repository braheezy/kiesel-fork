const std = @import("std");
const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
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

    const boolean_constructor = try realm.global_object.get(PropertyKey.from("Boolean"));
    const boolean_object = try boolean_constructor.object.construct(.{
        .arguments_list = &[_]Value{Value.from(true)},
    });
    const value_of = try boolean_object.get(PropertyKey.from("valueOf"));
    const value = try value_of.callAssumeCallableNoArgs(Value.from(boolean_object));
    std.debug.print("new Boolean(true).valueOf() = {}\n", .{value});
}
