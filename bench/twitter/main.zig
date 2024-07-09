const std = @import("std");
const dom = @import("simdjzon").dom;

pub const read_buf_cap = std.mem.page_size;

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const args = try std.process.argsAlloc(allocator);
    if (args.len != 2) {
        std.debug.print("USAGE: ./simdjson <file.json>\n", .{});
    }
    var parser = try dom.Parser.initFile(allocator, args[1], .{});
    defer parser.deinit();
    parser.parse() catch |err| {
        std.log.err("parse failed. {s}", .{@errorName(err)});
        return 1;
    };
    const statuses = try parser.element().at_pointer("/statuses");
    const array = try statuses.get_array();
    var i: usize = 0;
    while (array.at(i)) |status| : (i += 1) {
        _ = status; // autofix
        // const id = try status.at_pointer("/id");
        // std.debug.print("{}\n", .{try id.get_int64()});
    }
    // std.debug.print("i={}\n", .{i});
    if (i != 100) {
        std.debug.print("error. expected i=100. found i={}\n", .{i});
        return 1;
    }

    return 0;
}
