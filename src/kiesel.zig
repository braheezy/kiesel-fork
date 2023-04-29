const execution = @import("execution.zig");
const language = @import("language.zig");

pub const Agent = execution.Agent;
pub const Realm = execution.Realm;
pub const Script = language.Script;

test {
    _ = execution;
    _ = language;
}
