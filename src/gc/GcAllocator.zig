//! Garbage-collecting allocator using libgc.
//!
//! The implementation is largely copied from `std.heap.CAllocator`.

const std = @import("std");

const build_options = @import("build-options");
const libgc = @import("../c/libgc.zig");

const GcAllocator = @This();

kind: Kind,

pub const Kind = enum { normal, atomic };

pub const vtable: std.mem.Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

pub fn init(kind: Kind) GcAllocator {
    if (libgc.c.GC_is_init_called() == 0) {
        libgc.c.GC_init();
        if (build_options.enable_nan_boxing) {
            libgc.c.GC_set_pointer_mask(std.math.maxInt(u48));
        }
        libgc.c.GC_start_mark_threads();
    }
    return .{ .kind = kind };
}

pub fn allocator(self: *GcAllocator) std.mem.Allocator {
    return .{
        .ptr = self,
        .vtable = &vtable,
    };
}

fn getHeader(ptr: [*]u8) *[*]u8 {
    return @ptrCast(@alignCast(ptr - @sizeOf(usize)));
}

fn alignedAlloc(self: *GcAllocator, len: usize, alignment: std.mem.Alignment) ?[*]u8 {
    const alignment_bytes = alignment.toByteUnits();

    // Thin wrapper around regular malloc, overallocate to account for
    // alignment padding and store the original malloc()'ed pointer before
    // the aligned address.
    const total_bytes = len + alignment_bytes - 1 + @sizeOf(usize);
    const unaligned_ptr: [*]u8 = @ptrCast(switch (self.kind) {
        .normal => libgc.c.GC_malloc(total_bytes),
        .atomic => libgc.c.GC_malloc_atomic(total_bytes),
    } orelse return null);
    const unaligned_addr = @intFromPtr(unaligned_ptr);
    const aligned_addr = std.mem.alignForward(usize, unaligned_addr + @sizeOf(usize), alignment_bytes);
    const aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
    getHeader(aligned_ptr).* = unaligned_ptr;

    return aligned_ptr;
}

fn alignedFree(ptr: [*]u8) void {
    const unaligned_ptr = getHeader(ptr).*;
    libgc.c.GC_free(unaligned_ptr);
}

pub fn alignedAllocSize(ptr: [*]u8) usize {
    const unaligned_ptr = getHeader(ptr).*;
    const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
    return libgc.c.GC_size(unaligned_ptr) - delta;
}

fn alloc(
    ctx: *anyopaque,
    len: usize,
    alignment: std.mem.Alignment,
    return_address: usize,
) ?[*]u8 {
    _ = return_address;
    std.debug.assert(len > 0);
    return alignedAlloc(@ptrCast(ctx), len, alignment);
}

fn resize(
    _: *anyopaque,
    buf: []u8,
    alignment: std.mem.Alignment,
    new_len: usize,
    return_address: usize,
) bool {
    _ = alignment;
    _ = return_address;
    if (new_len <= buf.len) {
        return true;
    }
    const full_len = alignedAllocSize(buf.ptr);
    if (new_len <= full_len) {
        return true;
    }
    return false;
}

fn remap(
    context: *anyopaque,
    memory: []u8,
    alignment: std.mem.Alignment,
    new_len: usize,
    return_address: usize,
) ?[*]u8 {
    return if (resize(context, memory, alignment, new_len, return_address)) memory.ptr else null;
}

fn free(
    _: *anyopaque,
    buf: []u8,
    alignment: std.mem.Alignment,
    return_address: usize,
) void {
    _ = alignment;
    _ = return_address;
    alignedFree(buf.ptr);
}

test "gc" {
    var gc_allocator: GcAllocator = .init(.normal);
    try testGc(gc_allocator.allocator());
}

test "gc atomic" {
    var gc_allocator_atomic: GcAllocator = .init(.atomic);
    try testGc(gc_allocator_atomic.allocator());
}

fn testGc(gc: std.mem.Allocator) !void {
    try std.heap.testAllocator(gc);
    try std.heap.testAllocatorAligned(gc);
    try std.heap.testAllocatorLargeAlignment(gc);
    try std.heap.testAllocatorAlignedShrink(gc);
}
