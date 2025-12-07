const std = @import("std");

const kiesel = @import("kiesel");
const cli = @import("cli");

const Agent = kiesel.execution.Agent;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;

const script_text =
    "console.log(\"timer example start\");\n" ++
    "setTimeout(() => console.log(\"timer fired\"), 1);\n" ++
    "setTimeout(() => console.log(\"another timer\"), 3);\n" ++
    "console.log(\"timer example end\");";

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var platform = Agent.Platform.default();
    defer platform.deinit();
    var agent = try Agent.init(&platform, .{});
    defer agent.deinit();

    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();
    try cli.initializeGlobalObject(&agent, realm, realm.global_object);

    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);

    const result = try cli.run(allocator, realm, script_text, .{
        .base_dir = cwd,
        .origin = .command,
        .module = false,
        .print_promise_rejection_warnings = true,
    });
    std.debug.print("Script finished with {f}\n", .{result.fmtPretty()});
}
