const std = @import("std");

const libgc = @import("./c/libgc.zig");

pub const libgc_version: std.SemanticVersion = .{
    .major = libgc.c.GC_VERSION_MAJOR,
    .minor = libgc.c.GC_VERSION_MINOR,
    .patch = libgc.c.GC_VERSION_MICRO,
};

pub const GcAllocator = @import("gc/GcAllocator.zig");

pub fn disable() void {
    libgc.c.GC_disable();
}

pub fn collect() void {
    libgc.c.GC_gcollect();
}

pub fn disableWarnings() void {
    libgc.c.GC_set_warn_proc(@ptrCast(&struct {
        fn func(_: [*:0]const u8, _: libgc.c.GC_word) callconv(.c) void {}
    }.func));
}

pub fn FinalizerData(comptime T: type) type {
    return struct {
        nextFinalizerFunc: ?*const fn (?*anyopaque, ?*anyopaque) callconv(.c) void = null,
        next_finalizer_data: ?*anyopaque = null,
        data: T,
    };
}

pub fn registerFinalizer(
    object: *anyopaque,
    /// Must be a `*FinalizerData(T)`.
    finalizer_data: anytype,
    comptime finalizer: *const fn (object: *anyopaque, data: *@TypeOf(finalizer_data.data)) void,
) void {
    libgc.c.GC_register_finalizer(
        object,
        struct {
            fn func(object_: ?*anyopaque, client_data: ?*anyopaque) callconv(.c) void {
                const finalizer_data_: @TypeOf(finalizer_data) = @alignCast(@ptrCast(client_data));
                finalizer(object_.?, &finalizer_data_.data);
                if (finalizer_data_.nextFinalizerFunc) |nextFinalizerFunc| {
                    nextFinalizerFunc(object_, finalizer_data_.next_finalizer_data);
                }
            }
        }.func,
        finalizer_data,
        &finalizer_data.nextFinalizerFunc,
        &finalizer_data.next_finalizer_data,
    );
}

/// Asserts that the link has not already been registered with an object.
pub fn registerDisappearingLink(link: *?*const anyopaque, object: *const anyopaque) std.mem.Allocator.Error!void {
    const status = libgc.c.GC_general_register_disappearing_link(@ptrCast(link), object);
    switch (status) {
        libgc.c.GC_SUCCESS, libgc.c.GC_UNIMPLEMENTED => {},
        libgc.c.GC_DUPLICATE => unreachable,
        libgc.c.GC_NO_MEMORY => return error.OutOfMemory,
        else => unreachable,
    }
}
