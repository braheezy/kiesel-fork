const builtin = @import("builtin");
const std = @import("std");

const icu4zig = @import("icu4zig");
const stackinfo = @import("stackinfo");

const build_options = @import("build-options");

const Self = @This();

stdout: std.io.AnyWriter,
stderr: std.io.AnyWriter,
tty_config: std.io.tty.Config,
stack_info: ?stackinfo.StackInfo,
default_locale: if (build_options.enable_intl) icu4zig.Locale else void,
currentTime: *const fn () i64,

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

// `any()` captures a pointer to the writer, so these have to stick around.
var stdout_writer: if (has_fd_t) std.fs.File.Writer else void = undefined;
var stderr_writer: if (has_fd_t) std.fs.File.Writer else void = undefined;

pub const default = if (has_fd_t and has_std_time_nanotimestamp) defaultImpl else {};

// Ensure the default implementation doesn't silently break on known supported platforms
comptime {
    switch (builtin.os.tag) {
        .linux, .macos, .windows, .wasi => std.debug.assert(@TypeOf(default) != void),
        else => {},
    }
}

fn defaultImpl() Self {
    stdout_writer = std.io.getStdOut().writer();
    stderr_writer = std.io.getStdErr().writer();
    return .{
        .stdout = stdout_writer.any(),
        .stderr = stderr_writer.any(),
        .tty_config = std.io.tty.detectConfig(std.io.getStdOut()),
        .stack_info = stackinfo.StackInfo.init() catch null,
        .default_locale = if (build_options.enable_intl)
            icu4zig.Locale.init(null) catch unreachable
        else {},
        .currentTime = std.time.milliTimestamp,
    };
}

pub fn deinit(self: Self) void {
    if (build_options.enable_intl) self.default_locale.deinit();
}
