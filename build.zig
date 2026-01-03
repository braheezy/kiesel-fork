const builtin = @import("builtin");
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Stop people from trying to build with an outdated Zig compiler
    if (builtin.zig_version.order(.{ .major = 0, .minor = 15, .patch = 2 }) == .lt) {
        std.debug.print("\n    {s}\n    {s}\n\n", .{
            "Zig version 0.15.2 is required, found " ++ builtin.zig_version_string ++ ".",
            "Please \u{1B}]8;;https://ziglang.org/download/\u{1B}\\download\u{1B}]8;;\u{1B}\\ or otherwise install a newer build and try again.",
        });
        std.process.exit(1);
    }

    const enable_annex_b = b.option(
        bool,
        "enable-annex-b",
        "Enable Annex B features",
    ) orelse true;
    const enable_intl_baseline = b.option(
        bool,
        "enable-intl",
        "Enable the Intl built-in object (requires cargo)",
    ) orelse true;
    const enable_legacy = b.option(
        bool,
        "enable-legacy",
        "Enable features marked as 'Legacy' in the spec",
    ) orelse true;
    const enable_libgc = b.option(
        bool,
        "enable-libgc",
        "Enable building with libgc",
    ) orelse true;
    const enable_libregexp = b.option(
        bool,
        "enable-libregexp",
        "Enable building with libregexp",
    ) orelse true;
    const enable_nan_boxing = b.option(
        bool,
        "enable-nan-boxing",
        "Enable NaN-boxing (requires a maximum of 48 bits of addressable memory and enabled by default on x86_64 and aarch64)",
    ) orelse switch (target.result.cpu.arch) {
        .x86_64, .aarch64 => true,
        else => false,
    };
    const enable_runtime = b.option(
        bool,
        "enable-runtime",
        "Enable the web-compatible runtime",
    ) orelse true;
    const enable_temporal_baseline = b.option(
        bool,
        "enable-temporal",
        "Enable the Temporal built-in object (requires cargo)",
    ) orelse true;

    const cli_light = b.option(
        bool,
        "cli-light",
        "Build only the CLI without optional dependencies (disables Intl/Temporal)",
    ) orelse false;

    const cli_step_requested = blk: {
        if (b.args) |args_| {
            var found = false;
            for (args_) |arg| {
                if (std.mem.eql(u8, arg, "cli")) {
                    found = true;
                    break;
                }
            }
            break :blk found;
        }
        break :blk false;
    };

    const disable_optional_deps = cli_light or cli_step_requested;

    const enable_intl = if (disable_optional_deps) false else enable_intl_baseline;
    const enable_temporal = if (disable_optional_deps) false else enable_temporal_baseline;

    const strip = b.option(bool, "strip", "Strip debug symbols") orelse (optimize != .Debug);
    // Defaults to true for now as the self-hosted backend in 0.15 fails with a linker error.
    const use_llvm = b.option(bool, "use-llvm", "Use the LLVM backend") orelse true;
    const version_string = b.option(
        []const u8,
        "version-string",
        "Version string, read from git by default",
    ) orelse blk: {
        const base_version: []const u8 = "0.1.0";
        var code: u8 = undefined;
        const output = b.runAllowFail(
            &.{ "git", "-C", b.build_root.path orelse ".", "rev-parse", "--short=9", "HEAD" },
            &code,
            .Ignore,
        ) catch break :blk base_version;
        const git_revision = std.mem.trim(u8, output, "\n");
        break :blk b.fmt("{s}-dev+{s}", .{ base_version, git_revision });
    };

    const version = std.SemanticVersion.parse(version_string) catch @panic("Invalid version");

    const options = b.addOptions();
    options.addOption(bool, "enable_annex_b", enable_annex_b);
    options.addOption(bool, "enable_intl", enable_intl);
    options.addOption(bool, "enable_legacy", enable_legacy);
    options.addOption(bool, "enable_libgc", enable_libgc);
    options.addOption(bool, "enable_libregexp", enable_libregexp);
    options.addOption(bool, "enable_nan_boxing", enable_nan_boxing);
    options.addOption(bool, "enable_runtime", enable_runtime);
    options.addOption(bool, "enable_temporal", enable_temporal);
    options.addOption(std.SemanticVersion, "version", version);

    const any_pointer = b.dependency("any_pointer", .{});
    const args = b.dependency("args", .{});
    const kiesel_runtime = b.dependency("kiesel_runtime", .{});
    const parser_toolkit = b.dependency("parser_toolkit", .{});
    const stackinfo = b.dependency("stackinfo", .{ .target = target });
    const unicode_id = b.dependency("unicode_id", .{});
    const zigline = b.dependency("zigline", .{});

    const kiesel = b.addModule("kiesel", .{
        .root_source_file = b.path("src/kiesel.zig"),
        .imports = &.{
            .{ .name = "build-options", .module = options.createModule() },
            .{ .name = "any-pointer", .module = any_pointer.module("any-pointer") },
            .{ .name = "ptk", .module = parser_toolkit.module("parser-toolkit") },
            .{ .name = "stackinfo", .module = stackinfo.module("stackinfo") },
            .{ .name = "unicode-id", .module = unicode_id.module("unicode-id") },
        },
        .target = target,
        .optimize = optimize,
    });

    if (enable_intl) {
        if (b.lazyDependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
            .@"link-icu4x" = false,
        })) |icu4zig| {
            kiesel.addImport("icu4zig", icu4zig.module("icu4zig"));
        }
    }

    if (enable_libgc) {
        const cflags_extra: []const u8 = blk: {
            var cflags: std.ArrayList([]const u8) = .empty;
            defer cflags.deinit(b.allocator);
            cflags.append(b.allocator, "-DNO_MSGBOX_ON_ERROR") catch @panic("OOM");
            if (optimize != .Debug) {
                cflags.append(b.allocator, "-DNO_GETENV") catch @panic("OOM");
            }
            break :blk std.mem.join(b.allocator, " ", cflags.items) catch @panic("OOM");
        };
        if (b.lazyDependency("bdwgc_zig", .{
            .target = target,
            .optimize = optimize,
            .linkage = .static,
            .CFLAGS_EXTRA = cflags_extra,
            .enable_gcj_support = false,
            .enable_java_finalization = false,
            .enable_large_config = true,
            .enable_dynamic_pointer_mask = enable_nan_boxing,
        })) |bdwgc| {
            kiesel.addImport("bdwgc", bdwgc.module("bdwgc"));
        }
    }

    if (enable_libregexp) {
        if (b.lazyDependency("libregexp", .{
            .target = target,
            .optimize = optimize,
        })) |libregexp| {
            kiesel.linkLibrary(libregexp.artifact("regexp"));
        }
    }

    if (enable_temporal) {
        if (b.lazyDependency("temporal_rs", .{
            .target = target,
            .optimize = optimize,
        })) |temporal_rs| {
            kiesel.addIncludePath(temporal_rs.path("temporal_capi/bindings/c"));
        }
    }

    if (enable_intl or enable_temporal) {
        if (b.lazyDependency("zement", .{
            .target = target,
            .optimize = optimize,
            .@"enable-intl" = enable_intl,
        })) |zement| {
            kiesel.addLibraryPath(zement.namedLazyPath("lib"));
            kiesel.linkSystemLibrary("zement", .{
                .preferred_link_mode = .static,
                .use_pkg_config = .no,
            });
            kiesel.linkLibrary(zement.artifact("unwind_stubs"));
            if (target.result.os.tag == .windows) {
                // For GetUserProfileDirectoryW
                kiesel.linkSystemLibrary("userenv", .{});
            }
        }
    }

    // Ensure the runtime uses the kiesel module defined above.
    kiesel_runtime.module("kiesel-runtime").addImport("kiesel", kiesel);

    const cli_module = b.createModule(.{
        .root_source_file = b.path("src/cli.zig"),
        .imports = &.{
            .{ .name = "args", .module = args.module("args") },
            .{ .name = "kiesel", .module = kiesel },
            .{ .name = "kiesel-runtime", .module = kiesel_runtime.module("kiesel-runtime") },
            .{ .name = "zigline", .module = zigline.module("zigline") },
        },
        .target = target,
        .optimize = optimize,
        .strip = strip,
    });
    if (enable_intl) {
        if (b.lazyDependency("icu4zig", .{
            .target = target,
            .optimize = optimize,
            .@"link-icu4x" = false,
        })) |icu4zig| {
            cli_module.addImport("icu4zig", icu4zig.module("icu4zig"));
        }
    }

    const cli_exe = switch (target.result.os.tag) {
        .uefi => b.addExecutable(.{
            .name = "bootx64",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/uefi.zig"),
                .imports = &.{
                    .{ .name = "kiesel", .module = kiesel },
                    .{ .name = "kiesel-runtime", .module = kiesel_runtime.module("kiesel-runtime") },
                    .{ .name = "zigline", .module = zigline.module("zigline") },
                },
                .target = target,
                .optimize = optimize,
                .strip = strip,
            }),
            .use_llvm = use_llvm,
        }),
        else => b.addExecutable(.{
            .name = "kiesel",
            .root_module = cli_module,
            .use_llvm = use_llvm,
        }),
    };

    const timer_example_module = b.createModule(.{
        .root_source_file = b.path("src/timer_example_cli.zig"),
        .imports = &.{
            .{ .name = "kiesel", .module = kiesel },
            .{ .name = "kiesel-runtime", .module = kiesel_runtime.module("kiesel-runtime") },
            .{ .name = "cli", .module = cli_module },
        },
        .target = target,
        .optimize = optimize,
        .strip = strip,
    });
    const timer_example_exe = b.addExecutable(.{
        .name = "kiesel-timer-example",
        .root_module = timer_example_module,
        .use_llvm = use_llvm,
    });

    const timer_light_module = b.createModule(.{
        .root_source_file = b.path("src/timer_cli_minimal.zig"),
        .imports = &.{
            .{ .name = "kiesel", .module = kiesel },
            .{ .name = "kiesel-runtime", .module = kiesel_runtime.module("kiesel-runtime") },
        },
        .target = target,
        .optimize = optimize,
        .strip = strip,
    });
    const timer_light_exe = b.addExecutable(.{
        .name = "kiesel-timer-light",
        .root_module = timer_light_module,
        .use_llvm = use_llvm,
    });

    const install_exe = b.addInstallArtifact(cli_exe, .{
        .dest_dir = switch (target.result.os.tag) {
            .uefi => .{ .override = .{ .custom = "EFI/BOOT" } },
            else => .default,
        },
    });
    b.getInstallStep().dependOn(&install_exe.step);

    const install_timer_example = b.addInstallArtifact(timer_example_exe, .{});
    b.getInstallStep().dependOn(&install_timer_example.step);

    const install_timer_light = b.addInstallArtifact(timer_light_exe, .{});
    b.getInstallStep().dependOn(&install_timer_light.step);

    const run_cmd = switch (target.result.os.tag) {
        .uefi => b.addSystemCommand(&.{
            "qemu-system-x86_64",
            "-bios",
            "OVMF.fd",
            "-drive",
            "format=raw,file=fat:rw:zig-out",
        }),
        else => b.addRunArtifact(cli_exe),
    };
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args_| {
        run_cmd.addArgs(args_);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const cli_step = b.step("cli", "Build the CLI without optional dependencies");
    cli_step.dependOn(&cli_exe.step);
    cli_step.dependOn(b.getInstallStep());

    const unit_tests = b.addTest(.{
        .root_module = kiesel,
        .use_llvm = use_llvm,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const docs = b.addObject(.{
        .name = "kiesel",
        .root_module = kiesel,
    });
    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Build and install documentation");
    docs_step.dependOn(&install_docs.step);

    const fuzzilli = b.addExecutable(.{
        .name = "kiesel-fuzzilli",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/fuzzilli/main.zig"),
            .imports = &.{
                .{ .name = "kiesel", .module = kiesel },
            },
            .link_libc = true,
            .target = target,
            .optimize = optimize,
            .strip = strip,
        }),
        .use_llvm = use_llvm,
    });
    fuzzilli.sanitize_coverage_trace_pc_guard = true;

    const fuzzilli_step = b.step("fuzzilli", "Build and install fuzzilli shell");
    fuzzilli_step.dependOn(&b.addInstallArtifact(fuzzilli, .{}).step);
}
