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

const has_fd_t = @hasDecl(std.posix.system, "fd_t");
const has_clockid_t = @hasDecl(std.posix.system, "clockid_t") and
    std.posix.system.clockid_t != void;

// `any()` captures a pointer to the writer, so these have to stick around.
var stdout_writer: if (has_fd_t) std.fs.File.Writer else void = undefined;
var stderr_writer: if (has_fd_t) std.fs.File.Writer else void = undefined;

pub const default = if (has_clockid_t and has_fd_t) defaultImpl else {};

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
