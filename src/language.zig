const module = @import("language/module.zig");

pub const ast = @import("language/ast.zig");
pub const literals = @import("language/literals.zig");
pub const runtime = @import("language/runtime.zig");
pub const tokenizer = @import("language/tokenizer.zig");

pub const Diagnostics = @import("ptk").Diagnostics;
pub const ExportEntry = SourceTextModule.ExportEntry;
pub const GraphLoadingState = module.GraphLoadingState;
pub const ImportAttribute = module.ImportAttribute;
pub const ImportEntry = SourceTextModule.ImportEntry;
pub const ImportedModulePayload = module.ImportedModulePayload;
pub const ImportedModuleReferrer = module.ImportedModuleReferrer;
pub const Module = module.Module;
pub const ModuleRequest = module.ModuleRequest;
pub const Parser = @import("language/Parser.zig");
pub const ResolvedBinding = module.ResolvedBinding;
pub const ResolvedBindingOrAmbiguous = module.ResolvedBindingOrAmbiguous;
pub const Script = @import("language/Script.zig");
pub const SourceTextModule = @import("language/SourceTextModule.zig");
pub const SyntheticModule = @import("language/SyntheticModule.zig");
pub const allImportAttributesSupported = module.allImportAttributesSupported;
pub const continueModuleLoading = SourceTextModule.continueModuleLoading;
pub const createDefaultExportSyntheticModule = SyntheticModule.createDefaultExportSyntheticModule;
pub const finishLoadingImportedModule = module.finishLoadingImportedModule;
pub const fmtParseError = Parser.fmtParseError;
pub const fmtParseErrorHint = Parser.fmtParseErrorHint;
pub const getImportedModule = module.getImportedModule;
pub const getModuleNamespace = module.getModuleNamespace;
pub const instantiateAsyncFunctionObject = runtime.instantiateAsyncFunctionObject;
pub const instantiateAsyncGeneratorFunctionObject = runtime.instantiateAsyncGeneratorFunctionObject;
pub const instantiateGeneratorFunctionObject = runtime.instantiateGeneratorFunctionObject;
pub const instantiateOrdinaryFunctionObject = runtime.instantiateOrdinaryFunctionObject;
pub const parseJSONModule = SyntheticModule.parseJSONModule;

test {
    _ = module;

    _ = Script;
    _ = SourceTextModule;
    _ = SyntheticModule;
    _ = literals;
    _ = tokenizer;
}
