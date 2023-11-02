const std = @import("std");
const atom_rue = std.mem.readInt(u24, "rue", .little);
const atom_alse = std.mem.readInt(u32, "alse", .little);
const atom_ull = std.mem.readInt(u24, "ull", .little);
pub const atom_true = std.mem.readInt(u32, "true", .little);
pub const atom_false = std.mem.readInt(u40, "false", .little);
pub const atom_fals = std.mem.readInt(u32, "fals", .little);
pub const atom_null = std.mem.readInt(u32, "null", .little);
pub inline fn is_valid_rue_atom(value: [*]const u8) bool {
    return is_valid_atom(value, 3, atom_rue);
}
pub inline fn is_valid_alse_atom(value: [*]const u8) bool {
    return is_valid_atom(value, 4, atom_alse);
}
pub inline fn is_valid_ull_atom(value: [*]const u8) bool {
    return is_valid_atom(value, 3, atom_ull);
}

pub inline fn is_valid_atom(value: [*]const u8, comptime len: u8, atom: std.meta.Int(.unsigned, len * 8)) bool {
    return std.mem.readInt(std.meta.Int(.unsigned, len * 8), value[0..len], .little) == atom;
}
