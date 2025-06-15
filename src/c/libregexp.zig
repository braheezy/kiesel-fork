const build_options = @import("build-options");

pub const c = @cImport({
    if (!build_options.enable_libregexp) {
        @compileError("libregexp not enabled");
    }
    @cInclude("libregexp.h");
});
