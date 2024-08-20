const build_options = @import("build-options");

pub const libgc = if (build_options.enable_libgc) @cImport({
    @cInclude("gc/gc.h");
    if (build_options.enable_nan_boxing) {
        @cInclude("gc/gc_mark.h");
    }
}) else @compileError("libgc not enabled");
