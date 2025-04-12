const std = @import("std");

const libgc = @import("./c/libgc.zig").libgc;

pub const libgc_version: std.SemanticVersion = .{
    .major = libgc.GC_VERSION_MAJOR,
    .minor = libgc.GC_VERSION_MINOR,
    .patch = libgc.GC_VERSION_MICRO,
};

pub const GcAllocator = @import("gc/GcAllocator.zig");

pub fn disable() void {
    libgc.GC_disable();
}

pub fn collect() void {
    libgc.GC_gcollect();
}

pub fn disableWarnings() void {
    libgc.GC_set_warn_proc(@ptrCast(&struct {
        fn func(_: [*:0]const u8, _: libgc.GC_word) callconv(.c) void {}
    }.func));
}

pub fn FinalizerData(comptime T: type) type {
    return struct {
        nextFinalizerFunc: ?*const fn (?*anyopaque, ?*anyopaque) callconv(.c) void = null,
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
        fn func(func_target: ?*anyopaque, func_data: ?*anyopaque) callconv(.c) void {
            const finalizer_data: @TypeOf(data) = @alignCast(@ptrCast(func_data));
            finalizer(&finalizer_data.data);
            if (finalizer_data.nextFinalizerFunc) |nextFinalizerFunc| {
                nextFinalizerFunc(func_target, finalizer_data.next_finalizer_data);
            }
        }
    }.func, data, &data.nextFinalizerFunc, &data.next_finalizer_data);
}

/// Asserts that the link has not already been registered with an object.
pub fn registerDisappearingLink(link: *?*const anyopaque, object: *const anyopaque) std.mem.Allocator.Error!void {
    const status = libgc.GC_general_register_disappearing_link(@ptrCast(link), object);
    switch (status) {
        libgc.GC_SUCCESS, libgc.GC_UNIMPLEMENTED => {},
        libgc.GC_DUPLICATE => unreachable,
        libgc.GC_NO_MEMORY => return error.OutOfMemory,
        else => unreachable,
    }
}
