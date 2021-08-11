const std = @import("std");
const Iterator = @import("main.zig").Iterator;
const atom_rue = std.mem.readIntLittle(u24, "rue");
const atom_alse = std.mem.readIntLittle(u32, "alse");
const atom_ull = std.mem.readIntLittle(u24, "ull");
pub const atom_true = std.mem.readIntLittle(u32, "true");
pub const atom_false = std.mem.readIntLittle(u40, "false");
pub const atom_null = std.mem.readIntLittle(u32, "null");
pub inline fn is_valid_true_atom(value: [*]const u8) bool {
    return is_valid_atom(value, 3, atom_rue);
}
pub inline fn is_valid_false_atom(value: [*]const u8) bool {
    return is_valid_atom(value, 4, atom_alse);
}
pub inline fn is_valid_null_atom(value: [*]const u8) bool {
    return is_valid_atom(value, 3, atom_ull);
}

pub inline fn is_valid_atom(value: [*]const u8, comptime len: u8, atom: std.meta.Int(.unsigned, len * 8)) bool {
    return std.mem.readIntLittle(std.meta.Int(.unsigned, len * 8), value[0..len]) == atom;
}
