const std = @import("std");
const v = @import("vector_types.zig");

extern fn @"llvm.x86.pclmulqdq"(v.i64x2, v.i64x2, i8) v.i64x2;
pub inline fn carrylessMul(a: v.u64x2, b: v.u64x2) v.u64x2 {
    return @bitCast(
        v.u64x2,
        @"llvm.x86.pclmulqdq"(@bitCast(v.i64x2, a), @bitCast(v.i64x2, b), 0),
    );
}
test "pclmulqdq" {
    const a = 0b10100010;
    const b = 0b10010110;
    const expected = @as(u64, 0b101100011101100);
    const av: v.u64x2 = .{ a, 0 };
    const bv: v.u64x2 = .{ b, 0 };
    const r = carrylessMul(av, bv);
    try std.testing.expectEqual(expected, r[0]);
}

extern fn @"llvm.x86.avx2.pshuf.b"(a: v.u8x32, b: v.u8x32) v.u8x32;
pub inline fn shuffleEpi8(a: v.u8x32, b: v.u8x32) v.u8x32 {
    return @"llvm.x86.avx2.pshuf.b"(a, b);
}

extern fn @"llvm.x86.ssse3.pshuf.b.128"(a: v.u8x16, b: v.u8x16) v.u8x16;
pub inline fn shuffleEpi32(a: v.u8x16, b: v.u8x16) v.u8x16 {
    return @"llvm.x86.ssse3.pshuf.b.128"(a, b);
}

test "pshufb" {
    const x = shuffleEpi8(("a" ** 32).*, ("b" ** 32).*);
    _ = x;
    // std.debug.print("x {s}\n", .{@as([32]u8, x)});
}

extern fn @"llvm.x86.vpalignr"(v.i64x2, v.i64x2, i8) v.i64x2;
pub inline fn vpalignr(a: v.i64x2, b: v.i64x2, c: i8) v.i64x2 {
    return @"llvm.x86.vpalignr"(a, b, c);
}

extern fn @"llvm.ssub.sat.v8i8"(v.u8x8, v.u8x8) v.u8x8;
pub inline fn _mm256_subs_epu8(a: v.u8x8, b: v.u8x8) v.u8x8 {
    return @"llvm.ssub.sat.v8i8"(a, b);
}

extern fn @"llvm.usub.sat.v32i8"(v.i8x32, v.i8x32) v.i8x32;
pub inline fn _mm512_subs_epu8(a: v.i8x32, b: v.i8x32) v.i8x32 {
    return @"llvm.usub.sat.v32i8"(a, b);
}

extern fn @"llvm.usub.sat.v16i8"(v.u8x16, v.u8x16) v.u8x16;
pub inline fn _mm_subs_epu8(a: v.u8x16, b: v.u8x16) v.u8x16 {
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

extern fn @"llvm.x86.avx2.pmovmskb"(v.u8x32) u32;
pub inline fn _mm256_movemask_epi8(a: v.u8x32) u32 {
    return @"llvm.x86.avx2.pmovmskb"(a);
}

extern fn @"llvm.x86.ssse3.pmadd.ub.sw.128"(v.u8x16, v.u8x16) v.u16x8;
pub inline fn _mm_maddubs_epi16(a: v.u8x16, b: v.u8x16) v.u16x8 {
    return @"llvm.x86.ssse3.pmadd.ub.sw.128"(a, b);
}

extern fn @"llvm.x86.sse2.pmadd.wd"(v.u16x8, v.u16x8) v.u32x4;
pub inline fn _mm_madd_epi16(a: v.u16x8, b: v.u16x8) v.u32x4 {
    return @"llvm.x86.sse2.pmadd.wd"(a, b);
}

extern fn @"llvm.x86.sse41.packusdw"(a: v.u32x4, b: v.u32x4) v.u16x8;
pub inline fn _mm_packus_epi32(a: v.u32x4, b: v.u32x4) v.u16x8 {
    return @"llvm.x86.sse41.packusdw"(a, b);
}

// extern fn @"llvm.x86.sse2.mov.dqu"(v.u8x16, v.u8x16, [*]u8) void;
// pub inline fn _mm256_storeu_si256(a: [*]u8, b: v.u8x32) void {
//     const chunk: [32]u8 = b;
//     @"llvm.x86.sse2.mov.dqu"(chunk[0..16].*, chunk[16..32].*, a);
// }

