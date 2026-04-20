const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const temporal_rs = b.dependency("temporal_rs", .{});

    const translate_c = b.addTranslateC(.{
        .root_source_file = b.path("src/c.h"),
        .target = target,
        .optimize = optimize,
    });
    translate_c.addIncludePath(temporal_rs.path("temporal_capi/bindings/c"));

    _ = b.addModule("temporal_rs", .{
        .root_source_file = b.path("src/root.zig"),
        .imports = &.{
            .{ .name = "c", .module = translate_c.createModule() },
        },
        .target = target,
        .optimize = optimize,
    });
}
