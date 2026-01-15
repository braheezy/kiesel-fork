const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const quickjs = b.dependency("quickjs", .{});

    const module = b.addModule("libregexp", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    module.addIncludePath(quickjs.path("."));
    module.addCSourceFiles(.{
        .root = quickjs.path("."),
        .files = &.{
            "cutils.c",
            "libregexp.c",
            "libunicode.c",
        },
    });
}
