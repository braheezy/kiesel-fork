const std = @import("std");

const Allocator = std.mem.Allocator;

const libgc = @import("./c/libgc.zig").libgc;

pub const libgc_version_string = std.fmt.comptimePrint("{}.{}.{}", .{
    libgc.GC_VERSION_MAJOR,
    libgc.GC_VERSION_MINOR,
    libgc.GC_VERSION_MICRO,
});

pub const GcAllocator = @import("gc/GcAllocator.zig");

pub fn allocator() Allocator {
    libgc.GC_init();
    return .{
        .ptr = undefined,
        .vtable = &GcAllocator.vtable,
    };
}

pub fn enable() void {
    libgc.GC_enable();
}

pub fn disable() void {
    libgc.GC_disable();
}

pub fn collect() void {
    libgc.GC_gcollect();
}

pub fn disableWarnings() void {
    libgc.GC_set_warn_proc(struct {
        fn func(_: [*c]const u8, _: libgc.GC_word) callconv(.C) void {}
    }.func);
}
