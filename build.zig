const builtin = @import("builtin");
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Stop people from trying to build with an outdated Zig compiler
    if (builtin.zig_version.order(.{ .major = 0, .minor = 14, .patch = 0, .pre = "dev" }) == .lt) {
        std.debug.print("\n    {s}\n    {s}\n\n", .{
            "Zig compiler version is " ++ builtin.zig_version_string ++ " but must be at least 0.14-dev.",
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
    const enable_libgc = b.option(
        bool,
        "enable-libgc",
        "Enable building with libgc (uses stubs otherwise)",
    ) orelse true;
    const enable_libregexp = b.option(
        bool,
        "enable-libregexp",
        "Enable building with libregexp (uses stubs otherwise)",
    ) orelse true;

    var version = std.SemanticVersion.parse("0.1.0-dev") catch unreachable;
    var code: u8 = undefined;
    if (b.runAllowFail(
        &.{ "git", "rev-parse", "HEAD" },
        &code,
        .Ignore,
    )) |output| {
        version.build = output[0..9];
    } else |_| {}

    const options = b.addOptions();
    options.addOption(bool, "enable_annex_b", enable_annex_b);
    options.addOption(bool, "enable_intl", enable_intl);
    options.addOption(bool, "enable_legacy", enable_legacy);
    options.addOption(bool, "enable_libgc", enable_libgc);
    options.addOption(bool, "enable_libregexp", enable_libregexp);
    options.addOption(std.SemanticVersion, "version", version);

    const any_pointer = b.dependency("any_pointer", .{});
    const libgc = b.dependency("libgc", .{
        .target = target,
        .optimize = optimize,
        .BUILD_SHARED_LIBS = false,
        .enable_gcj_support = false,
        .enable_java_finalization = false,
        .disable_gc_debug = true,
    });
    const libregexp = b.dependency("libregexp", .{
        .target = target,
        .optimize = optimize,
    });
    const parser_toolkit = b.dependency("parser_toolkit", .{});
    const zig_args = b.dependency("zig_args", .{});
    const zig_stackinfo = b.dependency("zig_stackinfo", .{ .target = target });
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
    if (enable_libgc) {
        kiesel.addIncludePath(.{
            .cwd_relative = libgc.builder.getInstallPath(.header, ""),
        });
        if (target.result.os.tag == .macos) {
            if (b.lazyImport(@This(), "macos_sdk")) |build_macos_sdk| {
                build_macos_sdk.addPaths(libgc.artifact("gc"));
            }
        }
        kiesel.linkLibrary(libgc.artifact("gc"));
        if (optimize == .ReleaseSafe) {
            // This started triggering a SIGILL in ReleaseSafe mode at some point, likely UB that
            // needs to be fixed upstream. Revisit after next libgc dependency update.
            libgc.artifact("gc").root_module.sanitize_c = false;
        }
    }
    if (enable_libregexp) {
        kiesel.linkLibrary(libregexp.artifact("regexp"));
    }

    const exe = switch (target.result.os.tag) {
        .uefi => b.addExecutable(.{
            .name = "bootx64",
            .root_source_file = b.path("src/uefi.zig"),
            .target = target,
            .optimize = optimize,
        }),
        else => b.addExecutable(.{
            .name = "kiesel",
            .root_source_file = b.path("src/cli.zig"),
            .target = target,
            .optimize = optimize,
        }),
    };
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

    b.getInstallStep().dependOn(&b.addInstallArtifact(exe, .{
        .dest_dir = switch (target.result.os.tag) {
            .uefi => .{ .override = .{ .custom = "EFI/BOOT" } },
            else => .default,
        },
    }).step);

    const run_cmd = switch (target.result.os.tag) {
        .uefi => b.addSystemCommand(&.{
            "qemu-system-x86_64",
            "-bios",
            "OVMF.fd",
            "-drive",
            "format=raw,file=fat:rw:zig-out",
        }),
        else => b.addRunArtifact(exe),
    };
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
