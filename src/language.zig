pub const Diagnostics = @import("ptk").Diagnostics;
pub const Script = @import("language/Script.zig");

test {
    _ = Script;
    _ = @import("language/literals.zig");
}
