//! Ported from https://github.com/googleprojectzero/fuzzilli/blob/main/Targets/coverage.c
//!
//! Variable names and control flow have been retained so this is not idiomatic Zig.

const std = @import("std");

pub const REPRL_CRFD = 100;
pub const REPRL_CWFD = 101;
pub const REPRL_DRFD = 102;
pub const REPRL_DWFD = 103;
pub const REPRL_MAX_DATA_SIZE = 16 << 20;

const SHM_SIZE = 0x200000;
const MAX_EDGES = (SHM_SIZE - 4) * 8;

const shmem_data = extern struct {
    num_edges: u32,

    pub fn edges(self: *shmem_data) [*]u8 {
        return @ptrFromInt(@intFromPtr(self) + 4);
    }
};

var __shmem: *shmem_data = undefined;
var __edges_start: ?[*]u32 = null;
var __edges_stop: ?[*]u32 = null;

pub fn __sanitizer_cov_reset_edgeguards() void {
    var N: u32 = 0;
    var x = __edges_start.?;
    while (@intFromPtr(x) < @intFromPtr(__edges_stop.?) and N < MAX_EDGES) : (x += 1) {
        N += 1;
        x[0] = N;
    }
}

export fn __sanitizer_cov_trace_pc_guard_init(start: [*]u32, stop: [*]u32) void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // Avoid duplicate initialization
    if (start == stop or start[0] != 0) return;

    if (__edges_start != null or __edges_stop != null) {
        stderr.writeAll("Coverage instrumentation is only supported for a single module\n") catch {};
        std.c._exit(-1);
    }

    __edges_start = start;
    __edges_stop = stop;

    // Map the shared memory region
    const shm_key = std.c.getenv("SHM_ID");
    if (shm_key == null) {
        stdout.writeAll("[COV] no shared memory bitmap available, skipping\n") catch {};
        const ptr = std.heap.page_allocator.alloc(u8, SHM_SIZE) catch @panic("OOM");
        __shmem = @alignCast(@ptrCast(ptr));
    } else {
        const fd = std.c.shm_open(
            shm_key.?,
            @bitCast(std.c.O{ .ACCMODE = .RDWR }),
            std.c.S.IRUSR | std.c.S.IWUSR,
        );
        defer _ = std.c.close(fd);
        if (fd <= -1) {
            const err: std.c.E = @enumFromInt(std.c._errno().*);
            stderr.print("Failed to open shared memory region: {s}\n", .{@tagName(err)}) catch {};
            std.c._exit(-1);
        }

        const ptr = std.c.mmap(
            null,
            SHM_SIZE,
            std.c.PROT.READ | std.c.PROT.WRITE,
            std.c.MAP{ .TYPE = .SHARED },
            fd,
            0,
        );
        if (ptr == std.c.MAP_FAILED) {
            stderr.writeAll("Failed to mmap shared memory region\n") catch {};
            std.c._exit(-1);
        }
        __shmem = @alignCast(@ptrCast(ptr));
    }

    __sanitizer_cov_reset_edgeguards();

    __shmem.num_edges = @intCast(stop - start);
    stdout.print(
        "[COV] edge counters initialized. Shared memory: {s} with {} edges\n",
        .{ shm_key orelse "(null)", __shmem.num_edges },
    ) catch {};
}

export fn __sanitizer_cov_trace_pc_guard(guard: *u32) void {
    // There's a small race condition here: if this function executes in two threads for the same
    // edge at the same time, the first thread might disable the edge (by setting the guard to zero)
    // before the second thread fetches the guard value (and thus the index). However, our
    // instrumentation ignores the first edge (see libcoverage.c) and so the race is unproblematic.
    const index = guard.*;
    // If this function is called before coverage instrumentation is properly initialized we want to return early.
    if (index == 0) return;
    __shmem.edges()[index / 8] |= @as(u8, 1) << @intCast(index % 8);
    guard.* = 0;
}
