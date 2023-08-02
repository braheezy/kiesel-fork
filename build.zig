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
        const cflags_common = [_][]const u8{
            // NOTE: These two are set by default when building libgc with CMake
            // https://github.com/ivmai/bdwgc/blob/3f419b623643adedb11d260afc92cdd347dba5d6/CMakeLists.txt#L137
            "-DALL_INTERIOR_POINTERS",
            "-DNO_EXECUTE_PERMISSION",
        };
        const cflags_wasi = [_][]const u8{
            "-D__wasi__",
            "-D_WASI_EMULATED_SIGNAL",
            "-Wl,wasi-emulated-signal",
        };
        const cflags = if (target.getOsTag() == .wasi) &(cflags_common ++ cflags_wasi) else &cflags_common;
        const libgc_srcs = [_][]const u8{
            "alloc.c",    "reclaim.c", "allchblk.c", "misc.c",     "mach_dep.c", "os_dep.c",
            "mark_rts.c", "headers.c", "mark.c",     "obj_map.c",  "blacklst.c", "finalize.c",
            "new_hblk.c", "dbg_mlc.c", "malloc.c",   "dyn_load.c", "typd_mlc.c", "ptr_chck.c",
            "mallocx.c",
        };

        libgc.linkLibC();
        libgc.addIncludePath(.{ .path = "vendor/zig-libgc/vendor/bdwgc/include" });
        inline for (libgc_srcs) |src| {
            libgc.addCSourceFile(.{
                .file = .{ .path = "vendor/zig-libgc/vendor/bdwgc/" ++ src },
                .flags = cflags,
            });
        }
    }

    const gc_module = b.createModule(.{
        .source_file = .{ .path = "vendor/zig-libgc/src/gc.zig" },
    });

    const linenoise = b.dependency("linenoise", .{});
    const parser_toolkit = b.dependency("parser_toolkit", .{});
    const zig_args = b.dependency("zig_args", .{});

    const kiesel_module = b.addModule("kiesel", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = &.{
            std.Build.ModuleDependency{
                .module = gc_module,
                .name = "gc",
            },
            std.Build.ModuleDependency{
                .module = parser_toolkit.module("parser-toolkit"),
                .name = "ptk",
            },
        },
    });

    const exe = b.addExecutable(.{
        .name = "kiesel",
        .root_source_file = .{ .path = "src/cli.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibrary(libgc);
    exe.addIncludePath(.{ .path = "vendor/zig-libgc/vendor/bdwgc/include" });
    exe.addModule("kiesel", kiesel_module);
    exe.addModule("args", zig_args.module("args"));
    exe.addModule("linenoise", linenoise.module("linenoise"));

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    unit_tests.linkLibrary(libgc);
    unit_tests.addIncludePath(.{ .path = "vendor/zig-libgc/vendor/bdwgc/include" });
    unit_tests.addModule("gc", gc_module);
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
