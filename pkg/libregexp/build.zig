const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const quickjs = b.dependency("quickjs", .{});

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "regexp",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    lib.root_module.addCSourceFiles(.{
        .root = quickjs.path("."),
        .files = &.{
            "cutils.c",
            "libregexp.c",
            "libunicode.c",
        },
    });
    lib.installHeadersDirectory(quickjs.path("."), "", .{
        .include_extensions = &.{
            "cutils.h",
            "libregexp.h",
            "libregexp-opcode.h",
            "libunicode.h",
            "libunicode-table.h",
        },
    });
    b.installArtifact(lib);
}
