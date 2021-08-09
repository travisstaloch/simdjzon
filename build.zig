const std = @import("std");

fn setup(step: *std.build.LibExeObjStep, mode: std.builtin.Mode, target: anytype, step_size: u8) void {
    step.addCSourceFile("src/utils.c", &[_][]const u8{ "-Wall", "-Wextra", "-Werror", "-O3" });
    step.setTarget(target);
    step.linkLibC();
    step.setBuildMode(mode);
    step.addBuildOption(u8, "step_size", step_size);
}

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    const step_size = b.option(
        u8,
        "step-size",
        "how many bytes of input to read at per StructuralIndexer step.  must be either 64 or 128",
    ) orelse 64;

    const lib = b.addStaticLibrary("simdjzon", "src/main.zig");
    setup(lib, mode, target, step_size);
    lib.install();

    var main_tests = b.addTest("src/main.zig");
    setup(main_tests, mode, target, step_size);
    // main_tests.setFilter("search tape");

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&main_tests.step);

    const exe = b.addExecutable("simdjzon", "src/main.zig");
    setup(exe, mode, target, step_size);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args|
        run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
