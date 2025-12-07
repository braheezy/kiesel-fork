const std = @import("std");

const kiesel = @import("kiesel");
const kiesel_runtime = @import("kiesel-runtime");

const Agent = kiesel.execution.Agent;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const Diagnostics = kiesel.language.Diagnostics;
const Value = kiesel.types.Value;

const script_text =
    "console.log(\"timer light start\");\n" ++
    "setTimeout(() => console.log(\"timeout fired\"), 1);\n" ++
    "setTimeout(() => console.log(\"timeout done\"), 3);\n" ++
    "console.log(\"timer light setup complete\");";

pub fn main() !void {
    var platform = Agent.Platform.default();
    defer platform.deinit();

    var agent = try Agent.init(&platform, .{});
    defer agent.deinit();

    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();
    try kiesel_runtime.addBindings(&agent, realm, realm.global_object);

    var diagnostics = Diagnostics.init(std.heap.page_allocator);
    defer diagnostics.deinit();

    const script = try Script.parse(
        script_text,
        realm,
        null,
        .{
            .diagnostics = &diagnostics,
            .file_name = "timer-light-example",
        },
    );

    const result = try script.evaluate();
    std.debug.print("Script result: {f}\n", .{result.fmtPretty()});
    try kiesel_runtime.runEventLoop(&agent);
}
