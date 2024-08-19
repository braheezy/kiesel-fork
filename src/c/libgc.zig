const build_options = @import("build-options");

pub const libgc = if (build_options.enable_libgc)
    @cImport({
        @cInclude("gc/gc.h");
        if (build_options.enable_nan_boxing) {
            @cInclude("gc/gc_mark.h");
        }
    })
else
    libgc_stub;

const libgc_stub = struct {
    pub const GC_VERSION_MAJOR: c_int = 0;
    pub const GC_VERSION_MINOR: c_int = 0;
    pub const GC_VERSION_MICRO: c_int = 0;

    pub const GC_word = c_ulong;
    pub const GC_warn_proc = ?*const fn ([*c]u8, GC_word) callconv(.C) void;

    pub fn GC_init() void {}
    pub fn GC_is_init_called() c_int {
        return 0;
    }
    pub fn GC_disable() void {}
    pub fn GC_gcollect() void {}
    pub fn GC_malloc(_: usize) ?*anyopaque {
        return null;
    }
    pub fn GC_free(_: ?*anyopaque) void {}
    pub fn GC_size(_: ?*const anyopaque) usize {
        return 0;
    }
    pub fn GC_set_pointer_mask(_: usize) void {}
    pub fn GC_set_warn_proc(_: GC_warn_proc) void {}
    pub fn GC_start_mark_threads() void {}
};
