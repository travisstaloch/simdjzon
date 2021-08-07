const std = @import("std");

fn setup(step: *std.build.LibExeObjStep, mode: std.builtin.Mode, target: anytype) void {
    step.addCSourceFile("src/utils.c", &[_][]const u8{ "-Wall", "-Wextra", "-Werror", "-O3" });
    step.setTarget(target);
    step.linkLibC();
    step.setBuildMode(mode);
}

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary("zimdjson", "src/main.zig");
    setup(lib, mode, target);
    lib.install();

    var main_tests = b.addTest("src/main.zig");
    setup(main_tests, mode, target);
    // main_tests.setFilter("search tape");

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&main_tests.step);

    const exe = b.addExecutable("zimdjson", "src/main.zig");
    setup(exe, mode, target);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args|
        run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
