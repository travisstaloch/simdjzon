const std = @import("std");

pub fn build(b: *std.Build) void {
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

    const log_level = b.option(
        std.log.Level,
        "log-level",
        "The log level for the application. default .err",
    ) orelse .err;

    const options = b.addOptions();
    options.addOption(bool, "step_128", step_128);
    options.addOption(bool, "ondemand", ondemand);
    options.addOption(u16, "ondemand_read_cap", ondemand_read_cap);
    options.addOption(std.log.Level, "log_level", log_level);
    const options_mod = options.createModule();

    const mod = b.addModule("simdjzon", .{
        .root_source_file = b.path("src/simdjzon.zig"),
        .imports = &.{
            .{ .name = "build_options", .module = options_mod },
        },
    });

    var main_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/tests.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    main_tests.root_module.addImport("simdjzon", mod);
    main_tests.use_llvm = true; // TODO remove when #26 is resolved
    b.installArtifact(main_tests);

    const test_step = b.step("test", "Run tests");
    const main_tests_run = b.addRunArtifact(main_tests);
    main_tests_run.has_side_effects = true;
    test_step.dependOn(&main_tests_run.step);

    const exe = b.addExecutable(.{
        .name = "simdjzon",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.root_module.addImport("simdjzon", mod);
    exe.use_llvm = true; // TODO remove when #26 is resolved
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args|
        run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const twitterbench = b.addExecutable(.{
        .name = "twitterbench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench/twitter/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    twitterbench.root_module.addImport("simdjzon", mod);
    twitterbench.use_llvm = true; // TODO remove when #26 is resolved
    b.installArtifact(twitterbench);
    const run_twitter_bench = b.addRunArtifact(twitterbench);
    if (b.args) |args| run_twitter_bench.addArgs(args);
    const run_twitter_bench_step = b.step("twitter-bench", "Run the twitter-bench");
    run_twitter_bench_step.dependOn(&run_twitter_bench.step);
}
