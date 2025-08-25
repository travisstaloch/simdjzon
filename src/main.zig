const std = @import("std");

const os = std.os;
const simdjzon = @import("simdjzon");
pub const build_options = simdjzon.build_options;
const common = simdjzon.common;
const dom = simdjzon.dom;
const ondemand = simdjzon.ondemand;
pub const step_size = if (build_options.step_128) 128 else 64;
pub const read_buf_cap = build_options.ondemand_read_cap;

const Args = struct {
    verbose: bool = false,
    filename: []const u8 = "",
};

pub fn domMain(allocator: std.mem.Allocator, args: Args) !u8 {
    var parser: dom.Parser = undefined;

    if (args.filename.len == 0) {
        const stdin = std.fs.File.stdin();
        const input = try stdin.readToEndAlloc(allocator, std.math.maxInt(u32));
        parser = try dom.Parser.initFixedBuffer(allocator, input, .{});
    } else {
        parser = try dom.Parser.initFile(allocator, args.filename, .{});
    }
    defer parser.deinit();

    parser.parse() catch |err| switch (err) {
        else => {
            std.log.err("parse failed. {s}", .{@errorName(err)});
            return 1;
        },
    };

    return 0;
}

pub fn ondemandMain(allocator: std.mem.Allocator, args: Args) !u8 {
    if (args.filename.len == 0) return error.MissingFilenameArg;

    const file = try std.fs.cwd().openFile(args.filename, .{ .mode = .read_only });
    defer file.close();
    var read_buf: [read_buf_cap]u8 = undefined;
    var parser = try ondemand.Parser.init(file, try file.getEndPos(), allocator, args.filename, .{}, &read_buf);
    defer parser.deinit();

    var doc = try parser.iterate();
    var string_buf: [0x1000]u8 = undefined;
    var end_index: u32 = 0;
    recursive_iterate_json(&doc, 1, parser.parser.max_depth, &string_buf, &end_index) catch |err| switch (err) {
        else => {
            std.log.err("{s}:{} parse failed. {s}", .{ parser.parser.filename, end_index, @errorName(err) });
            return 1;
        },
    };

    if (end_index != doc.iter.last_document_position()[0]) {
        std.log.err("More than one JSON value at the root of the document, or extra characters at the end of the JSON!", .{});
        return 1;
    }
    return 0;
}

fn recursive_iterate_json(element_: anytype, depth: u16, max_depth: u16, string_buf: []u8, end_index: *u32) common.Error!void {
    if (depth >= max_depth) return error.DEPTH_ERROR;
    var element = element_.*;
    var iter = switch (@TypeOf(element)) {
        ondemand.Document => element_.iter,
        ondemand.Value => element_.iter.iter,
        else => unreachable,
    };
    switch (try element.get_type()) {
        .array => {
            var arr = try element.get_array();
            var it = arr.iterator();
            while (try it.next()) |*child| {
                try recursive_iterate_json(child, depth + 1, max_depth, string_buf, end_index);
            }
            end_index.* = (it.iter.iter.token.index - 1)[0];
            return;
        },
        .object => {
            var obj = try element.get_object();
            var it = obj.iterator();
            var key: [0x1000]u8 = undefined;
            while (try it.next(&key)) |*field| {
                try recursive_iterate_json(&field.value, depth + 1, max_depth, string_buf, end_index);
            }
            end_index.* = (it.iter.iter.token.index - 1)[0];
            return;
        },
        .number => {
            // FIXME: clean this up to match dom behavior of big int values
            // failing to match JSONTestSuite on:
            //   i_number_too_big_neg_int.json
            //   i_number_too_big_pos_int.json
            //   i_number_very_big_negative_int.json

            switch ((try iter.peek_delta(0, 1))[0]) {
                '-' => _ = element.get_int(i64) catch {
                    _ = try element.get_double();
                },
                else => _ = element.get_int(u64) catch {
                    _ = try element.get_double();
                },
            }
        },
        .string => _ = try element.get_string(string_buf),
        .bool => _ = try element.get_bool(),
        .nul => if (!(try element.is_null())) return error.INCORRECT_TYPE,
    }
    end_index.* = (iter.token.index - 1)[0];
    return;
}

fn usage(exename: []const u8) void {
    std.debug.print(
        \\usage: {s} <?options> <jsonfilename> <?options>
        \\  options: -v --verbose : prints 'valid parse in <time>'
        \\
    , .{exename});
}

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buf: [4096]u8 = undefined;
    var stdoutw = std.fs.File.stdout().writer(&buf);
    const stdout = &stdoutw.interface;
    var args = try std.process.argsAlloc(allocator);
    const exename = args[0];
    args = args[1..];
    var aargs: Args = .{};
    while (args.len > 0) : (args = args[1..]) {
        if (std.mem.eql(u8, args[0], "-v") or std.mem.eql(u8, args[0], "--verbose")) {
            aargs.verbose = true;
            continue;
        }
        if (aargs.filename.len == 0)
            aargs.filename = std.mem.sliceTo(args[0], 0)
        else {
            usage(exename);
            return 1;
        }
    }
    std.debug.print("args {}\n", .{aargs});

    var timer: std.time.Timer = if (aargs.verbose)
        try std.time.Timer.start()
    else
        undefined;

    const result = if (build_options.ondemand)
        ondemandMain(allocator, aargs)
    else
        domMain(allocator, aargs);

    if (result) |_| {
        if (aargs.verbose) {
            try stdout.print("valid parse in {D}\n", .{timer.lap()});
            try stdout.flush();
        }
    } else |e| {
        std.debug.print("error {t}\n", .{e});
    }
    return result;
}
