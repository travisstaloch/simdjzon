const std = @import("std");
const Iterator = @import("main.zig").Iterator;
const atom_true = std.mem.readIntLittle(u24, "rue");
const atom_false = std.mem.readIntLittle(u32, "alse");
const atom_null = std.mem.readIntLittle(u24, "ull");
pub inline fn is_valid_true_atom(src: *std.io.StreamSource) !bool {
    return is_valid_atom(src, 3, atom_true);
}
pub inline fn is_valid_false_atom(src: *std.io.StreamSource) !bool {
    return is_valid_atom(src, 4, atom_false);
}
pub inline fn is_valid_null_atom(src: *std.io.StreamSource) !bool {
    return is_valid_atom(src, 3, atom_null);
}

pub inline fn is_valid_atom(src: *std.io.StreamSource, comptime len: u8, atom: std.meta.Int(.unsigned, len * 8)) !bool {
    var buf: [len]u8 = undefined;
    const nbytes = try src.read(&buf);
    return nbytes == buf.len and std.mem.readIntLittle(std.meta.Int(.unsigned, len * 8), &buf) == atom;
}
