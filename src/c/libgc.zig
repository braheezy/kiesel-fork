const build_options = @import("build-options");

pub const c = @cImport({
    if (!build_options.enable_libgc) {
        @compileError("libgc not enabled");
    }
    @cInclude("gc/gc.h");
    if (build_options.enable_nan_boxing) {
        @cInclude("gc/gc_mark.h");
    }
});
