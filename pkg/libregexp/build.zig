const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const quickjs = b.dependency("quickjs", .{});

    const translate_c = b.addTranslateC(.{
        .root_source_file = b.path("src/c.h"),
        .target = target,
        .optimize = optimize,
    });
    translate_c.addIncludePath(quickjs.path("."));

    const module = b.addModule("libregexp", .{
        .root_source_file = b.path("src/root.zig"),
        .imports = &.{
            .{ .name = "c", .module = translate_c.createModule() },
        },
        .target = target,
        .optimize = optimize,
    });
    module.addCSourceFiles(.{
        .root = quickjs.path("."),
        .files = &.{
            "cutils.c",
            "libregexp.c",
            "libunicode.c",
        },
    });
}
