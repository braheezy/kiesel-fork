const builtin = @import("builtin");
const std = @import("std");

const build_icu4zig = @import("icu4zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Stop people from trying to build with an outdated Zig compiler, e.g. for 0.11 this would fail
    // on build.zig.zon hash validation which isn't obvious.
    if (builtin.zig_version.order(.{ .major = 0, .minor = 12, .patch = 0, .pre = "dev" }) == .lt) {
        std.debug.print("\n    {s}\n    {s}\n\n", .{
            "Zig compiler version is " ++ builtin.zig_version_string ++ " but must be at least 0.12-dev.",
            "Please \u{1B}]8;;https://ziglang.org/download/\u{1B}\\download\u{1B}]8;;\u{1B}\\ or otherwise install a newer build and try again.",
        });
        std.process.exit(1);
    }

    const enable_annex_b = b.option(
        bool,
        "enable-annex-b",
        "Enable Annex B features",
    ) orelse true;
    const enable_intl = b.option(
        bool,
        "enable-intl",
        "Enable the Intl built-in object (requires Rust-based icu4x build)",
    ) orelse true;

    const options = b.addOptions();
    options.addOption(bool, "enable_annex_b", enable_annex_b);
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
    const parser_toolkit = b.dependency("parser_toolkit", .{});
    const zig_args = b.dependency("zig_args", .{});
    const zigline = b.dependency("zigline", .{});

    var imports = std.ArrayList(std.Build.Module.Import).init(b.allocator);
    defer imports.deinit();
    imports.appendSlice(&.{
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
        imports.append(.{
            .module = icu4zig.module("icu4zig"),
            .name = "icu4zig",
        }) catch @panic("OOM");
    }

    const kiesel_module = b.addModule("kiesel", .{
        .root_source_file = .{ .path = "src/kiesel.zig" },
        .imports = imports.items,
    });
    kiesel_module.linkLibrary(libgc.artifact("gc"));
    kiesel_module.linkLibrary(libregexp.artifact("regexp"));

    const exe = b.addExecutable(.{
        .name = "kiesel",
        .root_source_file = .{ .path = "src/cli.zig" },
        .target = target,
        .optimize = optimize,
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
    exe.root_module.addImport("kiesel", kiesel_module);
    exe.root_module.addImport("args", zig_args.module("args"));
    exe.root_module.addImport("gc", libgc.module("gc"));
    exe.root_module.addImport("zigline", zigline.module("zigline"));
    if (optimize != .Debug) exe.root_module.strip = true;

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
    unit_tests.linkLibrary(libgc.artifact("gc"));
    unit_tests.linkLibrary(libregexp.artifact("regexp"));
    unit_tests.root_module.addImport("build-options", options.createModule());
    unit_tests.root_module.addImport("any-pointer", any_pointer.module("any-pointer"));
    unit_tests.root_module.addImport("gc", libgc.module("gc"));
    unit_tests.root_module.addImport("ptk", parser_toolkit.module("parser-toolkit"));
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
        unit_tests.root_module.addImport("icu4zig", icu4zig.module("icu4zig"));
    }
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const docs = b.addObject(.{
        .name = "kiesel",
        .root_source_file = .{ .path = "src/kiesel.zig" },
        .target = target,
        .optimize = optimize,
    });
    for (imports.items) |import| docs.root_module.addImport(import.name, import.module);
    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Build and install documentation");
    docs_step.dependOn(&install_docs.step);
}
