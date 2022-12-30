const std = @import("std");
const builtin = @import("builtin");
const v = @import("vector_types.zig");
const cmn = @import("common.zig");
const Chunk = cmn.Chunk;
const chunk_len = cmn.chunk_len;
const half_chunk_len = chunk_len / 2;

// ---
// from https://gist.github.com/sharpobject/80dc1b6f3aaeeada8c0e3a04ebc4b60a
// ---
// thanks to sharpobject for these implementations which make it possible to get
// rid of old utils.c and stop linking libc.
// ---

fn __mm256_permute2x128_si256_0x21(comptime V: type, a: V, b: V) V {
    var ret: V = undefined;
    ret[0] = a[2];
    ret[1] = a[3];
    ret[2] = b[0];
    ret[3] = b[1];
    return ret;
}

fn _mm256_permute2x128_si256_0x21(a: Chunk, b: Chunk) Chunk {
    const V = if (chunk_len == 32) v.u64x4 else v.u32x4;
    return @bitCast(Chunk, __mm256_permute2x128_si256_0x21(V, @bitCast(V, a), @bitCast(V, b)));
}

fn _mm256_alignr_epi8(a: Chunk, b: Chunk, comptime imm8: comptime_int) Chunk {
    var ret: Chunk = undefined;
    var i: usize = 0;
    while (i + imm8 < half_chunk_len) : (i += 1) {
        ret[i] = b[i + imm8];
    }
    while (i < half_chunk_len) : (i += 1) {
        ret[i] = a[i + imm8 - half_chunk_len];
    }
    while (i + imm8 < chunk_len) : (i += 1) {
        ret[i] = b[i + imm8];
    }
    while (i < chunk_len) : (i += 1) {
        ret[i] = a[i + imm8 - half_chunk_len];
    }
    return ret;
}

pub fn _prev1(a: Chunk, b: Chunk) Chunk {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256_0x21(b, a), half_chunk_len - 1);
}

pub fn _prev2(a: Chunk, b: Chunk) Chunk {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256_0x21(b, a), half_chunk_len - 2);
}

pub fn _prev3(a: Chunk, b: Chunk) Chunk {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256_0x21(b, a), half_chunk_len - 3);
}

// ---
// --- end from https://gist.github.com/sharpobject/80dc1b6f3aaeeada8c0e3a04ebc4b60a
// ---

pub fn shuffle_fallback(x: v.u8x32, mask: v.u8x32) v.u8x32 {
    _ = x;
    _ = mask;
    unreachable;
}

pub fn mm256_shuffle_epi8(x: v.u8x32, mask: v.u8x32) v.u8x32 {
    return asm (
        \\ vpshufb %[mask], %[x], %[out]
        : [out] "=x" (-> v.u8x32),
        : [x] "+x" (x),
          [mask] "x" (mask),
    );
}

// https://developer.arm.com/architectures/instruction-sets/intrinsics/vqtbl1q_s8
pub fn lookup_16_aarch64(x: v.u8x16, mask: v.u8x16) v.u8x16 {
    // tbl     v0.16b, { v0.16b }, v1.16b
    return asm (
        \\tbl  %[out].16b, {%[x].16b}, %[mask].16b
        : [out] "=&x" (-> v.u8x16),
        : [x] "x" (x),
          [mask] "x" (mask),
    );
}
// https://developer.arm.com/architectures/instruction-sets/intrinsics/vtstq_u8
pub fn any_bits_set_aarch64(x: v.u8x16, mask: v.u8x16) v.u8x16 {
    // cmtst   v0.16b, v1.16b, v0.16b
    return asm (
        \\cmtst  %[out].16b, %[x].16b, %[mask].16b
        : [out] "=&x" (-> v.u8x16),
        : [x] "x" (x),
          [mask] "x" (mask),
    );
}

pub fn prefix_xor(bitmask: u64) u64 {
    // There should be no such thing with a processor supporting avx2
    // but not clmul.
    // const has_pclmul = comptime std.Target.x86.featureSetHas(builtin.cpu.features, .pclmul);
    const all_ones = @bitCast(u128, @splat(16, @as(u8, 0xff)));
    const x = @bitCast(u128, [2]u64{ bitmask, 0 });
    const m = clmul(x, all_ones, .lo);

    return @bitCast([2]u64, m)[0];
}

const clmul = if (cmn.is_x86_64 and cmn.has_pclmul and cmn.has_avx)
    clmulPclmul
else if (cmn.is_arm64 and cmn.has_armaes)
    clmulPmull
else
    clmulSoft;

// ---
// from zig/lib/std/crypto/ghash_polyval.zig
// ---
const Selector = enum { lo, hi, hi_lo };
// Carryless multiplication of two 64-bit integers for x86_64.
inline fn clmulPclmul(x: u128, y: u128, comptime half: Selector) u128 {
    switch (half) {
        .hi => {
            const product = asm (
                \\ vpclmulqdq $0x11, %[x], %[y], %[out]
                : [out] "=x" (-> @Vector(2, u64)),
                : [x] "x" (@bitCast(@Vector(2, u64), x)),
                  [y] "x" (@bitCast(@Vector(2, u64), y)),
            );
            return @bitCast(u128, product);
        },
        .lo => {
            const product = asm (
                \\ vpclmulqdq $0x00, %[x], %[y], %[out]
                : [out] "=x" (-> @Vector(2, u64)),
                : [x] "x" (@bitCast(@Vector(2, u64), x)),
                  [y] "x" (@bitCast(@Vector(2, u64), y)),
            );
            return @bitCast(u128, product);
        },
        .hi_lo => {
            const product = asm (
                \\ vpclmulqdq $0x10, %[x], %[y], %[out]
                : [out] "=x" (-> @Vector(2, u64)),
                : [x] "x" (@bitCast(@Vector(2, u64), x)),
                  [y] "x" (@bitCast(@Vector(2, u64), y)),
            );
            return @bitCast(u128, product);
        },
    }
}

// Carryless multiplication of two 64-bit integers for ARM crypto.
inline fn clmulPmull(x: u128, y: u128, comptime half: Selector) u128 {
    switch (half) {
        .hi => {
            const product = asm (
                \\ pmull2 %[out].1q, %[x].2d, %[y].2d
                : [out] "=w" (-> @Vector(2, u64)),
                : [x] "w" (@bitCast(@Vector(2, u64), x)),
                  [y] "w" (@bitCast(@Vector(2, u64), y)),
            );
            return @bitCast(u128, product);
        },
        .lo => {
            const product = asm (
                \\ pmull %[out].1q, %[x].1d, %[y].1d
                : [out] "=w" (-> @Vector(2, u64)),
                : [x] "w" (@bitCast(@Vector(2, u64), x)),
                  [y] "w" (@bitCast(@Vector(2, u64), y)),
            );
            return @bitCast(u128, product);
        },
        .hi_lo => {
            const product = asm (
                \\ pmull %[out].1q, %[x].1d, %[y].1d
                : [out] "=w" (-> @Vector(2, u64)),
                : [x] "w" (@bitCast(@Vector(2, u64), x >> 64)),
                  [y] "w" (@bitCast(@Vector(2, u64), y)),
            );
            return @bitCast(u128, product);
        },
    }
}

// Software carryless multiplication of two 64-bit integers.
fn clmulSoft(x_: u128, y_: u128, comptime half: Selector) u128 {
    const x = @truncate(u64, if (half == .hi or half == .hi_lo) x_ >> 64 else x_);
    const y = @truncate(u64, if (half == .hi) y_ >> 64 else y_);

    const x0 = x & 0x1111111111111110;
    const x1 = x & 0x2222222222222220;
    const x2 = x & 0x4444444444444440;
    const x3 = x & 0x8888888888888880;
    const y0 = y & 0x1111111111111111;
    const y1 = y & 0x2222222222222222;
    const y2 = y & 0x4444444444444444;
    const y3 = y & 0x8888888888888888;
    const z0 = (x0 * @as(u128, y0)) ^ (x1 * @as(u128, y3)) ^ (x2 * @as(u128, y2)) ^ (x3 * @as(u128, y1));
    const z1 = (x0 * @as(u128, y1)) ^ (x1 * @as(u128, y0)) ^ (x2 * @as(u128, y3)) ^ (x3 * @as(u128, y2));
    const z2 = (x0 * @as(u128, y2)) ^ (x1 * @as(u128, y1)) ^ (x2 * @as(u128, y0)) ^ (x3 * @as(u128, y3));
    const z3 = (x0 * @as(u128, y3)) ^ (x1 * @as(u128, y2)) ^ (x2 * @as(u128, y1)) ^ (x3 * @as(u128, y0));

    const x0_mask = @as(u64, 0) -% (x & 1);
    const x1_mask = @as(u64, 0) -% ((x >> 1) & 1);
    const x2_mask = @as(u64, 0) -% ((x >> 2) & 1);
    const x3_mask = @as(u64, 0) -% ((x >> 3) & 1);
    const extra = (x0_mask & y) ^ (@as(u128, x1_mask & y) << 1) ^
        (@as(u128, x2_mask & y) << 2) ^ (@as(u128, x3_mask & y) << 3);

    return (z0 & 0x11111111111111111111111111111111) ^
        (z1 & 0x22222222222222222222222222222222) ^
        (z2 & 0x44444444444444444444444444444444) ^
        (z3 & 0x88888888888888888888888888888888) ^ extra;
}

// ---
// end from zig/lib/std/crypto/ghash_polyval.zig
// ---
