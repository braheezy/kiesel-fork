const module = @import("language/module.zig");
const runtime = @import("language/runtime.zig");

pub const Diagnostics = @import("ptk").Diagnostics;
pub const ExportEntry = SourceTextModule.ExportEntry;
pub const GraphLoadingState = module.GraphLoadingState;
pub const ImportEntry = SourceTextModule.ImportEntry;
pub const ImportedModulePayload = module.ImportedModulePayload;
pub const ImportedModuleReferrer = module.ImportedModuleReferrer;
pub const Module = module.Module;
pub const ResolvedBinding = module.ResolvedBinding;
pub const ResolvedBindingOrAmbiguous = module.ResolvedBindingOrAmbiguous;
pub const Script = @import("language/Script.zig");
pub const SourceTextModule = @import("language/SourceTextModule.zig");
pub const continueModuleLoading = SourceTextModule.continueModuleLoading;
pub const finishLoadingImportedModule = module.finishLoadingImportedModule;
pub const fmtParseError = @import("language/Parser.zig").fmtParseError;
pub const fmtParseErrorHint = @import("language/Parser.zig").fmtParseErrorHint;
pub const getImportedModule = module.getImportedModule;
pub const getModuleNamespace = module.getModuleNamespace;
pub const instantiateAsyncFunctionObject = runtime.instantiateAsyncFunctionObject;
pub const instantiateAsyncGeneratorFunctionObject = runtime.instantiateAsyncGeneratorFunctionObject;
pub const instantiateGeneratorFunctionObject = runtime.instantiateGeneratorFunctionObject;
pub const instantiateOrdinaryFunctionObject = runtime.instantiateOrdinaryFunctionObject;
pub const tokenizer = @import("language/tokenizer.zig");

test {
    _ = module;

    _ = Script;
    _ = SourceTextModule;
    _ = tokenizer;
    _ = @import("language/literals.zig");
}
