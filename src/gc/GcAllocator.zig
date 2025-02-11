//! Garbage-collecting allocator using libgc.
//!
//! The implementation is largely copied from `std.heap.CAllocator`.

const std = @import("std");

const libgc = @import("../c/libgc.zig").libgc;

pub const vtable: std.mem.Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn getHeader(ptr: [*]u8) *[*]u8 {
    return @alignCast(@ptrCast(ptr - @sizeOf(usize)));
}

fn alignedAlloc(len: usize, alignment: std.mem.Alignment) ?[*]u8 {
    const alignment_bytes = alignment.toByteUnits();

    // Thin wrapper around regular malloc, overallocate to account for
    // alignment padding and store the original malloc()'ed pointer before
    // the aligned address.
    const unaligned_ptr = @as([*]u8, @ptrCast(libgc.GC_malloc(len + alignment_bytes - 1 + @sizeOf(usize)) orelse return null));
    const unaligned_addr = @intFromPtr(unaligned_ptr);
    const aligned_addr = std.mem.alignForward(usize, unaligned_addr + @sizeOf(usize), alignment_bytes);
    const aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
    getHeader(aligned_ptr).* = unaligned_ptr;

    return aligned_ptr;
}

fn alignedFree(ptr: [*]u8) void {
    const unaligned_ptr = getHeader(ptr).*;
    libgc.GC_free(unaligned_ptr);
}

pub fn alignedAllocSize(ptr: [*]u8) usize {
    const unaligned_ptr = getHeader(ptr).*;
    const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
    return libgc.GC_size(unaligned_ptr) - delta;
}

fn alloc(
    _: *anyopaque,
    len: usize,
    alignment: std.mem.Alignment,
    return_address: usize,
) ?[*]u8 {
    _ = return_address;
    std.debug.assert(len > 0);
    return alignedAlloc(len, alignment);
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

test {
    const allocator: std.mem.Allocator = .{
        .ptr = undefined,
        .vtable = &vtable,
    };
    try std.heap.testAllocator(allocator);
    try std.heap.testAllocatorAligned(allocator);
    try std.heap.testAllocatorLargeAlignment(allocator);
    try std.heap.testAllocatorAlignedShrink(allocator);
}
