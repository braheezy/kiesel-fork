const std = @import("std");

const build_crab = @import("build_crab");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const enable_intl = b.option(bool, "enable-intl", "Enable Intl") orelse true;
    const enable_temporal = b.option(bool, "enable-temporal", "Enable Temporal") orelse true;

    const features = blk: {
        var features: std.ArrayListUnmanaged([]const u8) = .empty;
        defer features.deinit(b.allocator);
        if (enable_intl) {
            features.append(b.allocator, "intl") catch @panic("OOM");
        }
        if (enable_temporal) {
            features.append(b.allocator, "temporal") catch @panic("OOM");
        }
        break :blk (std.mem.join(b.allocator, ",", features.items) catch @panic("OOM"));
    };

    var cargo_args: std.ArrayListUnmanaged([]const u8) = .empty;
    defer cargo_args.deinit(b.allocator);
    cargo_args.appendSlice(b.allocator, &.{ "--features", features }) catch @panic("OOM");
    if (optimize != .Debug) {
        cargo_args.append(b.allocator, "--release") catch @panic("OOM");
    }
    if (!target.query.isNative()) {
        // Required for cross-compilation, most targets won't be installed.
        // -Z requires nightly so we don't enforce this for native builds.
        cargo_args.appendSlice(b.allocator, &.{ "-Z", "build-std=std,panic_abort" }) catch @panic("OOM");
    }

    const build_dir = build_crab.addCargoBuild(
        b,
        .{
            .manifest_path = b.path("Cargo.toml"),
            .cargo_args = cargo_args.items,
        },
        .{
            .target = target,
            .optimize = .ReleaseSafe,
        },
    );

    // Exporting a static library as a build system artifact here would be nicer but linking a
    // static "system" library (libtemporal_capi.a) to another static library (the artifact) causes
    // issues: https://github.com/ziglang/zig/issues/20476
    // So for now we export the paths and do a bit of manual setup in the main build.zig.
    b.addNamedLazyPath("lib", build_dir);

    const unwind_stubs = b.addLibrary(.{
        .linkage = .static,
        .name = "unwind_stubs",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/unwind_stubs.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    b.installArtifact(unwind_stubs);
}
