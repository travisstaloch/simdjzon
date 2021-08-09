const std = @import("std");
const json = std.json;

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = &arena.allocator;

pub fn main() !void {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len <= 1) {
        std.debug.warn("usage: {s} file.json file2.json ...\n", .{std.fs.path.basename(args[0])});
        std.process.exit(1);
    }

    var parser = json.Parser.init(allocator, false);
    for (args[1..]) |arg| {
        var content = try std.fs.cwd().readFileAlloc(allocator, arg, std.math.maxInt(u32));
        defer allocator.free(content);

        var tree = try parser.parse(content);
        defer tree.deinit();
    }
}
