const builtin = @import("builtin");
const std = @import("std");

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
    const enable_legacy = b.option(
        bool,
        "enable-legacy",
        "Enable features marked as 'Legacy' in the spec",
    ) orelse true;

    const run_result = std.ChildProcess.run(.{
        .allocator = b.allocator,
        .argv = &.{ "git", "rev-parse", "HEAD" },
        .cwd_dir = b.build_root.handle,
    }) catch @panic("Failed to get commit hash");
    const git_rev = run_result.stdout[0..9];

    const version_string = std.fmt.allocPrint(
        b.allocator,
        "0.1.0-dev+{s}",
        .{git_rev},
    ) catch @panic("OOM");

    const options = b.addOptions();
    options.addOption(bool, "enable_annex_b", enable_annex_b);
    options.addOption(bool, "enable_intl", enable_intl);
    options.addOption(bool, "enable_legacy", enable_legacy);
    options.addOption([]const u8, "version_string", version_string);

    const any_pointer = b.dependency("any_pointer", .{});
    const libgc = b.dependency("libgc", .{
        .target = target,
        .optimize = optimize,
        .BUILD_SHARED_LIBS = false,
    });
    const libregexp = b.dependency("libregexp", .{
        .target = target,
        .optimize = optimize,
    });
    const parser_toolkit = b.dependency("parser_toolkit", .{});
    const zig_args = b.dependency("zig_args", .{});
    const zig_stackinfo = b.dependency("zig_stackinfo", .{});
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
            .module = parser_toolkit.module("parser-toolkit"),
            .name = "ptk",
        },
        .{
            .module = zig_stackinfo.module("stackinfo"),
            .name = "stackinfo",
        },
    }) catch @panic("OOM");
    if (enable_intl) {
        if (b.lazyDependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
        })) |icu4zig| {
            imports.append(.{
                .module = icu4zig.module("icu4zig"),
                .name = "icu4zig",
            }) catch @panic("OOM");
        }
    }

    const kiesel = b.addModule("kiesel", .{
        .root_source_file = b.path("src/kiesel.zig"),
        .imports = imports.items,
    });
    kiesel.addIncludePath(.{
        .cwd_relative = libgc.builder.getInstallPath(.header, ""),
    });
    if (target.result.os.tag == .macos) {
        if (b.lazyImport(@This(), "macos_sdk")) |build_macos_sdk| {
            build_macos_sdk.addPaths(libgc.artifact("gc"));
        }
    }
    kiesel.linkLibrary(libgc.artifact("gc"));
    kiesel.linkLibrary(libregexp.artifact("regexp"));

    const exe = b.addExecutable(.{
        .name = "kiesel",
        .root_source_file = b.path("src/cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (enable_intl) {
        if (b.lazyDependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
        })) |icu4zig| {
            const icu4x = icu4zig.builder.dependency("icu4x", .{
                .target = target,
                .optimize = optimize,
            });
            if (b.lazyImport(@This(), "icu4zig")) |build_icu4zig| {
                build_icu4zig.link(exe, icu4x);
            }
        }
    }
    exe.root_module.addImport("kiesel", kiesel);
    if (b.lazyDependency("kiesel_runtime", .{})) |kiesel_runtime| {
        // Ensure the runtime uses the kiesel module defined above.
        kiesel_runtime.module("kiesel-runtime").addImport("kiesel", kiesel);
        exe.root_module.addImport("kiesel-runtime", kiesel_runtime.module("kiesel-runtime"));
    }
    exe.root_module.addImport("args", zig_args.module("args"));
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
        .root_source_file = b.path("src/kiesel.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.linkLibrary(libgc.artifact("gc"));
    unit_tests.linkLibrary(libregexp.artifact("regexp"));
    for (imports.items) |import| unit_tests.root_module.addImport(import.name, import.module);
    if (enable_intl) {
        if (b.lazyDependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
        })) |icu4zig| {
            const icu4x = icu4zig.builder.dependency("icu4x", .{
                .target = target,
                .optimize = optimize,
            });
            if (b.lazyImport(@This(), "icu4zig")) |build_icu4zig| {
                build_icu4zig.link(unit_tests, icu4x);
            }
            unit_tests.root_module.addImport("icu4zig", icu4zig.module("icu4zig"));
        }
    }
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const docs = b.addObject(.{
        .name = "kiesel",
        .root_source_file = b.path("src/kiesel.zig"),
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
