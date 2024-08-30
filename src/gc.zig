const std = @import("std");

const build_options = @import("build-options");
const libgc = @import("./c/libgc.zig").libgc;

pub const libgc_version: std.SemanticVersion = .{
    .major = libgc.GC_VERSION_MAJOR,
    .minor = libgc.GC_VERSION_MINOR,
    .patch = libgc.GC_VERSION_MICRO,
};

pub const GcAllocator = @import("gc/GcAllocator.zig");

pub fn allocator() std.mem.Allocator {
    if (libgc.GC_is_init_called() == 0) {
        libgc.GC_init();
        if (build_options.enable_nan_boxing) {
            libgc.GC_set_pointer_mask(std.math.maxInt(u48));
        }
        libgc.GC_start_mark_threads();
    }
    return .{
        .ptr = undefined,
        .vtable = &GcAllocator.vtable,
    };
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

pub fn FinalizerData(comptime T: type) type {
    return struct {
        nextFinalizerFunc: ?*const fn (?*anyopaque, ?*anyopaque) callconv(.C) void = null,
        next_finalizer_data: ?*anyopaque = null,
        data: T,

        pub const Data = T;
    };
}

pub fn registerFinalizer(
    target: *anyopaque,
    data: anytype,
    comptime finalizer: *const fn (data: *@TypeOf(data.*).Data) void,
) void {
    libgc.GC_register_finalizer(target, struct {
        fn func(func_target: ?*anyopaque, func_data: ?*anyopaque) callconv(.C) void {
            const finalizer_data: @TypeOf(data) = @alignCast(@ptrCast(func_data));
            finalizer(&finalizer_data.data);
            if (finalizer_data.nextFinalizerFunc) |nextFinalizerFunc| {
                nextFinalizerFunc(func_target, finalizer_data.next_finalizer_data);
            }
        }
    }.func, data, &data.nextFinalizerFunc, &data.next_finalizer_data);
}
