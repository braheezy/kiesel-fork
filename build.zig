const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Copied from zig-libgc's build.zig
    const libgc = b.addStaticLibrary(.{
        .name = "libgc",
        .target = target,
        .optimize = optimize,
    });
    {
        const cflags = [_][]const u8{};
        const libgc_srcs = [_][]const u8{
            "alloc.c",    "reclaim.c", "allchblk.c", "misc.c",     "mach_dep.c", "os_dep.c",
            "mark_rts.c", "headers.c", "mark.c",     "obj_map.c",  "blacklst.c", "finalize.c",
            "new_hblk.c", "dbg_mlc.c", "malloc.c",   "dyn_load.c", "typd_mlc.c", "ptr_chck.c",
            "mallocx.c",
        };

        libgc.linkLibC();
        libgc.addIncludePath("vendor/zig-libgc/vendor/bdwgc/include");
        inline for (libgc_srcs) |src| {
            libgc.addCSourceFile("vendor/zig-libgc/vendor/bdwgc/" ++ src, &cflags);
        }
    }

    const gc = b.createModule(.{
        .source_file = .{ .path = "vendor/zig-libgc/src/gc.zig" },
    });

    const kiesel = b.addModule("kiesel", .{
        .source_file = .{ .path = "src/kiesel.zig" },
        .dependencies = &.{std.Build.ModuleDependency{
            .module = gc,
            .name = "gc",
        }},
    });

    const exe = b.addExecutable(.{
        .name = "kiesel",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibrary(libgc);
    exe.addIncludePath("vendor/zig-libgc/vendor/bdwgc/include");
    exe.addModule("kiesel", kiesel);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/kiesel.zig" },
        .target = target,
        .optimize = optimize,
    });
    unit_tests.linkLibrary(libgc);
    unit_tests.addIncludePath("vendor/zig-libgc/vendor/bdwgc/include");
    unit_tests.addModule("gc", gc);
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
