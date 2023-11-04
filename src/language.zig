const module = @import("language/module.zig");

pub const Diagnostics = @import("ptk").Diagnostics;
pub const ImportedModulePayload = module.ImportedModulePayload;
pub const ImportedModuleReferrer = module.ImportedModuleReferrer;
pub const Module = module.Module;
pub const Script = @import("language/Script.zig");
pub const SourceTextModule = @import("language/SourceTextModule.zig");
pub const finishLoadingImportedModule = module.finishLoadingImportedModule;
pub const getModuleNamespace = module.getModuleNamespace;

test {
    _ = module;

    _ = Script;
    _ = SourceTextModule;
    _ = @import("language/literals.zig");
}
