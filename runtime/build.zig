const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // NOTE: The user will have to add their own kiesel import module to this.
    const libxev = b.dependency("libxev", .{});
    const module = b.addModule("kiesel-runtime", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // NOTE: This just exists for ZLS to pick up the kiesel dependency.
    if (b.build_root.handle.openDir("../kiesel", .{}) catch null) |_| {
        const kiesel = b.addModule("kiesel", .{
            .root_source_file = b.path("../kiesel/src/kiesel.zig"),
            .target = target,
            .optimize = optimize,
        });
        const lib = b.addLibrary(.{
            .linkage = .static,
            .name = "kiesel-runtime",
            .root_module = module,
        });
        lib.root_module.addImport("kiesel", kiesel);
        b.installArtifact(lib);
    }

    module.addImport("xev", libxev.module("xev"));
}
