const std = @import("std");

const build_icu4zig = @import("icu4zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const enable_intl = b.option(
        bool,
        "enable-intl",
        "Enable the Intl built-in object (requires Rust-based icu4x build)",
    ) orelse true;

    const options = b.addOptions();
    options.addOption(bool, "enable_intl", enable_intl);

    const any_pointer = b.dependency("any_pointer", .{});
    const libgc = b.dependency("libgc", .{
        .target = target,
        .optimize = optimize,
    });
    const libregexp = b.dependency("libregexp", .{
        .target = target,
        .optimize = optimize,
    });
    const linenoise = b.dependency("linenoise", .{});
    const parser_toolkit = b.dependency("parser_toolkit", .{});
    const zig_args = b.dependency("zig_args", .{});

    var dependencies = std.ArrayList(std.Build.ModuleDependency).init(b.allocator);
    defer dependencies.deinit();
    dependencies.appendSlice(&.{
        .{
            .module = options.createModule(),
            .name = "build-options",
        },
        .{
            .module = any_pointer.module("any-pointer"),
            .name = "any-pointer",
        },
        .{
            .module = libgc.module("gc"),
            .name = "gc",
        },
        .{
            .module = parser_toolkit.module("parser-toolkit"),
            .name = "ptk",
        },
    }) catch @panic("OOM");
    if (enable_intl) {
        const icu4zig = b.dependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
        });
        dependencies.append(.{
            .module = icu4zig.module("icu4zig"),
            .name = "icu4zig",
        }) catch @panic("OOM");
    }

    const kiesel_module = b.addModule("kiesel", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = dependencies.items,
    });

    _ = std.process.Child.run(.{
        .allocator = b.allocator,
        .argv = &.{
            "patch",
            "--forward",
            "--reject-file=-",
            libregexp.builder.dependency("quickjs", .{}).path("libregexp.c").getPath(b),
            "patches/libregexp.patch",
        },
    }) catch |err| @panic(@errorName(err));

    const exe = b.addExecutable(.{
        .name = "kiesel",
        .root_source_file = .{ .path = "src/cli.zig" },
        .target = target,
        .optimize = optimize,
        .single_threaded = true,
    });
    if (enable_intl) {
        const icu4zig = b.dependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
        });
        const icu4x = icu4zig.builder.dependency("icu4x", .{
            .target = target,
            .optimize = optimize,
        });
        build_icu4zig.link(exe, icu4x);
    }
    exe.linkLibrary(libgc.artifact("gc"));
    exe.linkLibrary(libregexp.artifact("regexp"));
    exe.addModule("kiesel", kiesel_module);
    exe.addModule("any-pointer", any_pointer.module("any-pointer"));
    exe.addModule("args", zig_args.module("args"));
    exe.addModule("gc", libgc.module("gc"));
    exe.addModule("linenoise", linenoise.module("linenoise"));
    if (optimize != .Debug) exe.strip = true;

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
    unit_tests.linkLibrary(libgc.artifact("gc"));
    unit_tests.linkLibrary(libregexp.artifact("regexp"));
    unit_tests.addModule("build-options", options.createModule());
    unit_tests.addModule("any-pointer", any_pointer.module("any-pointer"));
    unit_tests.addModule("gc", libgc.module("gc"));
    unit_tests.addModule("ptk", parser_toolkit.module("parser-toolkit"));
    if (enable_intl) {
        const icu4zig = b.dependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
        });
        const icu4x = icu4zig.builder.dependency("icu4x", .{
            .target = target,
            .optimize = optimize,
        });
        build_icu4zig.link(unit_tests, icu4x);
        unit_tests.addModule("icu4zig", icu4zig.module("icu4zig"));
    }
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
