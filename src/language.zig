pub const Diagnostics = @import("ptk").Diagnostics;
pub const Script = @import("language/Script.zig");
pub const SourceTextModule = @import("language/SourceTextModule.zig");

test {
    _ = Script;
    _ = SourceTextModule;
    _ = @import("language/literals.zig");
}
