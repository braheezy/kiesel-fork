const std = @import("std");

const build_crab = @import("build_crab");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const enable_intl = b.option(bool, "enable-intl", "Enable Intl") orelse true;

    const features = blk: {
        var features = std.ArrayList([]const u8).init(b.allocator);
        defer features.deinit();
        if (enable_intl) {
            features.append("intl") catch @panic("OOM");
        }
        break :blk (std.mem.join(b.allocator, ",", features.items) catch @panic("OOM"));
    };

    var cargo_args = std.ArrayList([]const u8).init(b.allocator);
    defer cargo_args.deinit();
    cargo_args.appendSlice(&.{
        "--features",
        features,
        // Required for cross-compilation, most targets won't be installed
        "-Z",
        "build-std=std,panic_abort",
    }) catch @panic("OOM");
    if (optimize != .Debug) {
        cargo_args.append("--release") catch @panic("OOM");
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
}
