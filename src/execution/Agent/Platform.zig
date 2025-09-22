const builtin = @import("builtin");
const std = @import("std");

const icu4zig = @import("icu4zig");
const stackinfo = @import("stackinfo");
const temporal_rs = @import("../../c/temporal_rs.zig");

const build_options = @import("build-options");
const gc = @import("../../gc.zig");

const GcAllocator = gc.GcAllocator;

const Platform = @This();

gc_allocator: std.mem.Allocator,
gc_allocator_atomic: std.mem.Allocator,
stdout: *std.Io.Writer,
stderr: *std.Io.Writer,
tty_config: std.Io.tty.Config,
stack_info: ?stackinfo.StackInfo,
default_locale: Locale,
default_time_zone: TimeZone,
currentTimeMs: *const fn () i64,
currentTimeNs: *const fn () i128,

pub const Locale = if (build_options.enable_intl) icu4zig.Locale else void;
pub const TimeZone = if (build_options.enable_temporal) temporal_rs.c.TimeZone else void;

/// Whether `fd_t` is defined
const has_fd_t = std.posix.system.fd_t != void;

/// Whether `clockid_t` is defined
const has_clockid_t = @hasDecl(std.posix.system, "clockid_t") and
    std.posix.system.clockid_t != void;

/// Whether `std.time.nanoTimestamp()` is usable, which is what `std.time.milliTimestamp()` uses
const has_std_time_nanotimestamp = switch (builtin.os.tag) {
    // These have custom implementations
    .windows, .wasi, .uefi => true,
    // The fallback is `posix.clock_gettime()`, which is always defined when linking libc but
    // relies on `clockid_t` existing.
    else => has_clockid_t,
};

const State = struct {
    gc_allocator: if (build_options.enable_libgc) GcAllocator else void,
    gc_allocator_atomic: if (build_options.enable_libgc) GcAllocator else void,
    stdout_buffer: if (has_fd_t) [1024]u8 else void,
    stdout_writer: if (has_fd_t) std.fs.File.Writer else void,
    stderr_buffer: if (has_fd_t) [1024]u8 else void,
    stderr_writer: if (has_fd_t) std.fs.File.Writer else void,
};

var state: State = undefined;

pub fn default() Platform {
    if (comptime !(has_fd_t and has_std_time_nanotimestamp)) {
        @compileError("Platform.default() is not supported on this platform");
    }
    state = .{
        .gc_allocator = if (@FieldType(State, "gc_allocator") != void) .init(.normal),
        .gc_allocator_atomic = if (@FieldType(State, "gc_allocator_atomic") != void) .init(.atomic),
        .stdout_buffer = undefined,
        .stdout_writer = std.fs.File.stdout().writer(&state.stdout_buffer),
        .stderr_buffer = undefined,
        .stderr_writer = std.fs.File.stderr().writer(&state.stderr_buffer),
    };
    return .{
        .gc_allocator = if (@FieldType(State, "gc_allocator") != void)
            state.gc_allocator.allocator()
        else
            std.heap.page_allocator,
        .gc_allocator_atomic = if (@FieldType(State, "gc_allocator_atomic") != void)
            state.gc_allocator_atomic.allocator()
        else
            std.heap.page_allocator,
        .stdout = &state.stdout_writer.interface,
        .stderr = &state.stderr_writer.interface,
        .tty_config = .detect(.stdout()),
        .stack_info = stackinfo.StackInfo.init() catch null,
        .default_locale = if (Locale != void) icu4zig.Locale.unknown(),
        .default_time_zone = if (TimeZone != void) temporal_rs.c.temporal_rs_TimeZone_utc(),
        .currentTimeMs = std.time.milliTimestamp,
        .currentTimeNs = std.time.nanoTimestamp,
    };
}

pub fn deinit(self: Platform) void {
    if (Locale != void) self.default_locale.deinit();
}
