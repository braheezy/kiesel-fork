const build_options = @import("build-options");

pub const libregexp = if (build_options.enable_libregexp)
    @cImport({
        @cInclude("cutils.h"); // For the BOOL typedef
        @cInclude("libregexp.h");
    })
else
    libregexp_stub;

const libregexp_stub = struct {
    pub const LRE_FLAG_INDICES: c_int = 0;
    pub const LRE_FLAG_GLOBAL: c_int = 0;
    pub const LRE_FLAG_IGNORECASE: c_int = 0;
    pub const LRE_FLAG_MULTILINE: c_int = 0;
    pub const LRE_FLAG_DOTALL: c_int = 0;
    pub const LRE_FLAG_UNICODE: c_int = 0;
    pub const LRE_FLAG_UNICODE_SETS: c_int = 0;
    pub const LRE_FLAG_STICKY: c_int = 0;

    pub fn lre_get_capture_count(_: [*c]const u8) c_int {
        return 0;
    }
    pub fn lre_get_flags(_: [*c]const u8) c_int {
        return 0;
    }
    pub fn lre_get_groupnames(_: [*c]const u8) [*c]const u8 {
        return null;
    }
    pub fn lre_compile(_: [*c]c_int, _: [*c]u8, _: c_int, _: [*c]const u8, _: usize, _: c_int, _: ?*anyopaque) [*c]u8 {
        return null;
    }
    pub fn lre_exec(_: [*c][*c]u8, _: [*c]const u8, _: [*c]const u8, _: c_int, _: c_int, _: c_int, _: ?*anyopaque) c_int {
        return 0;
    }
};
