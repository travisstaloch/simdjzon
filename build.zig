const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const step_128 = b.option(
        bool,
        "step-128",
        "process 128 bytes of input per StructuralIndexer step.  defaults to 64 if this flag is not present.  ",
    ) orelse false;

    const ondemand = b.option(
        bool,
        "ondemand",
        "use the ondemand parser for validation",
    ) orelse false;

    const ondemand_read_cap = b.option(
        u16,
        "ondemand-read-cap",
        "the capacity of the ondemand read buffer. defaults to 4096 (4 Kb)",
    ) orelse 4096;

    const options = b.addOptions();
    options.addOption(bool, "step_128", step_128);
    options.addOption(bool, "ondemand", ondemand);
    options.addOption(u16, "ondemand_read_cap", ondemand_read_cap);

    const lib = b.addStaticLibrary(.{
        .name = "simdjzon",
        .root_source_file = .{ .path = "src/simdjzon.zig" },
        .target = target,
        .optimize = optimize,
    });
    lib.addOptions("build_options", options);
    b.installArtifact(lib);

    var main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/tests.zig" },
        .target = target,
        .optimize = optimize,
    });
    main_tests.addOptions("build_options", options);
    // main_tests.setFilter("tape build 1");

    const test_step = b.step("test", "Run tests");
    const main_tests_run = b.addRunArtifact(main_tests);
    main_tests_run.has_side_effects = true;
    test_step.dependOn(&main_tests_run.step);

    const exe = b.addExecutable(.{
        .name = "simdjzon",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addOptions("build_options", options);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args|
        run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
