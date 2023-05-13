const std = @import("std");
const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const PropertyKey = kiesel.types.PropertyKey;
const Realm = kiesel.execution.Realm;
const Diagnostics = kiesel.language.Diagnostics;
const Script = kiesel.language.Script;
const Value = kiesel.types.Value;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var agent = try Agent.init();
    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();

    const source_text = "\t{true; false\u{2028} ;;;}\r\nnull\u{FEFF}";
    var diagnostics = Diagnostics.init(agent.allocator);
    const script = Script.parse(source_text, realm, null, .{
        .diagnostics = &diagnostics,
        .file_name = "file.js",
    }) catch {
        try diagnostics.print(stderr);
        return;
    };
    try script.ecmascript_code.print(stdout);

    const boolean_constructor = try realm.global_object.get(PropertyKey.from("Boolean"));
    const boolean_object = try boolean_constructor.object.construct(.{
        .arguments_list = &[_]Value{Value.from(true)},
    });
    const value_of = try boolean_object.get(PropertyKey.from("valueOf"));
    const value = try value_of.callAssumeCallableNoArgs(Value.from(boolean_object));
    std.debug.print("new Boolean(true).valueOf() = {}\n", .{value});
}
