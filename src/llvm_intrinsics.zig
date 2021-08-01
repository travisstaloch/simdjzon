const std = @import("std");
usingnamespace @import("vector_types.zig");

extern fn @"llvm.x86.pclmulqdq"(i64x2, i64x2, i8) i64x2;
pub inline fn carrylessMul(a: u64x2, b: u64x2) u64x2 {
    return @bitCast(
        u64x2,
        @"llvm.x86.pclmulqdq"(@bitCast(i64x2, a), @bitCast(i64x2, b), 0),
    );
}
test "pclmulqdq" {
    const a = 0b10100010;
    const b = 0b10010110;
    const expected = @as(u64, 0b101100011101100);
    const av: u64x2 = .{ a, 0 };
    const bv: u64x2 = .{ b, 0 };
    const r = carrylessMul(av, bv);
    try std.testing.expectEqual(expected, r[0]);
}

extern fn @"llvm.x86.avx2.pshuf.b"(a: u8x32, b: u8x32) u8x32;
pub inline fn shuffleEpi8(a: u8x32, b: u8x32) u8x32 {
    return @"llvm.x86.avx2.pshuf.b"(a, b);
}

extern fn @"llvm.x86.ssse3.pshuf.b.128"(a: u8x16, b: u8x16) u8x16;
pub inline fn shuffleEpi32(a: u8x16, b: u8x16) u8x16 {
    return @"llvm.x86.ssse3.pshuf.b.128"(a, b);
}

test "pshufb" {
    const x = shuffleEpi8(("a" ** 32).*, ("b" ** 32).*);
    _ = x;
    // std.debug.print("x {s}\n", .{@as([32]u8, x)});
}

extern fn @"llvm.x86.vpalignr"(i64x2, i64x2, i8) i64x2;
pub inline fn vpalignr(a: i64x2, b: i64x2, c: i8) i64x2 {
    return @"llvm.x86.vpalignr"(a, b, c);
}

extern fn @"llvm.ssub.sat.v8i8"(u8x8, u8x8) u8x8;
pub inline fn _mm256_subs_epu8(a: u8x8, b: u8x8) u8x8 {
    return @"llvm.ssub.sat.v8i8"(a, b);
}

extern fn @"llvm.usub.sat.v16i8"(u8x16, u8x16) u8x16;
pub inline fn _mm_subs_epu8(a: u8x16, b: u8x16) u8x16 {
    return @"llvm.usub.sat.v16i8"(a, b);
}

extern fn @"llvm.uadd.sat.i8"(u8, u8) u8;
pub fn sat_add_u8(a: u8, b: u8) u8 {
    return @"llvm.uadd.sat.i8"(a, b);
}
extern fn @"llvm.usub.sat.i8"(u8, u8) u8;
pub fn sat_sub_u8(a: u8, b: u8) u8 {
    return @"llvm.usub.sat.i8"(a, b);
}

extern fn @"llvm.x86.avx2.pmovmskb"(u8x32) u32;
pub inline fn _mm256_movemask_epi8(a: u8x32) u32 {
    return @"llvm.x86.avx2.pmovmskb"(a);
}

extern fn @"llvm.x86.ssse3.pmadd.ub.sw.128"(u8x16, u8x16) u16x8;
pub inline fn _mm_maddubs_epi16(a: u8x16, b: u8x16) u16x8 {
    return @"llvm.x86.ssse3.pmadd.ub.sw.128"(a, b);
}

extern fn @"llvm.x86.sse2.pmadd.wd"(u16x8, u16x8) u32x4;
pub inline fn _mm_madd_epi16(a: u16x8, b: u16x8) u32x4 {
    return @"llvm.x86.sse2.pmadd.wd"(a, b);
}

extern fn @"llvm.x86.sse41.packusdw"(a: u32x4, b: u32x4) u16x8;
pub inline fn _mm_packus_epi32(a: u32x4, b: u32x4) u16x8 {
    return @"llvm.x86.sse41.packusdw"(a, b);
}

extern fn @"icmp"(u8x16, u8x16) u8x16;
pub inline fn _mm_cmpeq_epi8(a: u8x16, b: u8x16) u8x16 {
    return @"icmp"(a, b);
}
