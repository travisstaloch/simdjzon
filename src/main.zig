const build_options = @import("build_options");
const std = @import("std");

const os = std.os;
const common = @import("common.zig");
const simdjzon = @import("simdjzon.zig");
const dom = simdjzon.dom;
const ondemand = simdjzon.ondemand;
pub const step_size = build_options.step_size;
pub const read_buf_size = build_options.ondemand_read_size;

pub fn domMain() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    var parser: dom.Parser = undefined;

    if (os.argv.len == 1) {
        var stdin = std.io.getStdIn().reader();
        const input = try stdin.readAllAlloc(allocator, std.math.maxInt(u32));
        parser = try dom.Parser.initFixedBuffer(allocator, input, .{});
    } else if (os.argv.len == 2) {
        const filename = std.mem.span(os.argv[1]);
        parser = try dom.Parser.initFile(allocator, filename, .{});
    } else {
        std.log.err("Too many arguments.  Please provide input via filename or stdin", .{});
        return 1;
    }

    defer parser.deinit();
    parser.parse() catch |err| switch (err) {
        // error.EndOfStream => {},
        else => {
            std.log.err("parse failed. {s}", .{@errorName(err)});
            return 1;
        },
    };
    std.log.debug("parse valid", .{});
    return 0;
}

pub fn ondemandMain() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    // const allocator = std.heap.c_allocator;
    var parser: ondemand.Parser = undefined;
    defer if (parser.src.* == .file) parser.src.file.close();

    if (os.argv.len == 1) {
        var stdin = std.io.getStdIn().reader();
        const input = try stdin.readAllAlloc(allocator, std.math.maxInt(u32));
        var src = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(input) };
        parser = try ondemand.Parser.init(&src, allocator, "<stdin>", .{});
    } else if (os.argv.len == 2) {
        const filepath = std.mem.span(os.argv[1]);
        const file = try std.fs.cwd().openFile(filepath, .{ .read = true });

        var src = std.io.StreamSource{ .file = file };
        parser = try ondemand.Parser.init(&src, allocator, filepath, .{});
    } else {
        std.log.err("Too many arguments.  Please provide input via filename or stdin", .{});
        return 1;
    }

    defer parser.deinit();

    var doc = try parser.iterate();
    // common.debug = true;
    var string_buf: [0x1000]u8 = undefined;
    var end_index: u32 = 0;
    recursive_iterate_json(&doc, 1, parser.parser.max_depth, &string_buf, &end_index) catch |err| switch (err) {
        // error.EndOfStream => {},
        else => {
            std.log.err("{s}:{} parse failed. {s}", .{ parser.parser.filename, end_index, @errorName(err) });
            return 1;
        },
    };

    if (end_index != doc.iter.last_document_position()[0]) {
        std.log.err("More than one JSON value at the root of the document, or extra characters at the end of the JSON!", .{});
        return 1;
    }
    std.log.debug("parse valid", .{});
    return 0;
}

inline fn recursive_iterate_json(element: anytype, depth: u16, max_depth: u16, string_buf: []u8, end_index: *u32) common.Error!void {
    if (depth >= max_depth) return error.DEPTH_ERROR;
    var iter = switch (@TypeOf(element)) {
        *ondemand.Document => element.iter,
        *ondemand.Value => element.iter.iter,
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
        .string => _ = try element.get_string([]u8, string_buf),
        .bool => _ = try element.get_bool(),
        .nul => if (!(try element.is_null())) return error.INCORRECT_TYPE,
    }
    end_index.* = (iter.token.index - 1)[0];
    return;
}

pub fn main() !u8 {
    return if (build_options.ondemand)
        ondemandMain()
    else
        domMain();
}
