const build_options = @import("build_options");
const std = @import("std");

const os = std.os;
usingnamespace @import("common.zig");
const simdjzon = @import("simdjzon.zig");
const dom = simdjzon.dom;
pub const step_size = build_options.step_size;

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    var parser: dom.Parser = undefined;
    // debug = true;

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
