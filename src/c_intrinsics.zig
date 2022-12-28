const std = @import("std");
const builtin = @import("builtin");

// from https://gist.github.com/sharpobject/80dc1b6f3aaeeada8c0e3a04ebc4b60a

const v = @import("vector_types.zig");

fn __mm256_permute2x128_si256_0x21(a: v.u64x4, b: v.u64x4) v.u64x4 {
    var ret: v.u64x4 = undefined;
    ret[0] = a[2];
    ret[1] = a[3];
    ret[2] = b[0];
    ret[3] = b[1];
    return ret;
}

fn _mm256_permute2x128_si256_0x21(a: v.u8x32, b: v.u8x32) v.u8x32 {
    return @bitCast(v.u8x32, __mm256_permute2x128_si256_0x21(@bitCast(v.u64x4, a), @bitCast(v.u64x4, b)));
}

fn _mm256_alignr_epi8(a: v.u8x32, b: v.u8x32, comptime imm8: comptime_int) v.u8x32 {
    var ret: v.u8x32 = undefined;
    var i: usize = 0;
    while (i + imm8 < 16) : (i += 1) {
        ret[i] = b[i + imm8];
    }
    while (i < 16) : (i += 1) {
        ret[i] = a[i + imm8 - 16];
    }
    while (i + imm8 < 32) : (i += 1) {
        ret[i] = b[i + imm8];
    }
    while (i < 32) : (i += 1) {
        ret[i] = a[i + imm8 - 16];
    }
    return ret;
}

pub fn _prev1(a: v.u8x32, b: v.u8x32) v.u8x32 {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256_0x21(b, a), 16 - 1);
}

pub fn _prev2(a: v.u8x32, b: v.u8x32) v.u8x32 {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256_0x21(b, a), 16 - 2);
}

pub fn _prev3(a: v.u8x32, b: v.u8x32) v.u8x32 {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256_0x21(b, a), 16 - 3);
}

// end from https://gist.github.com/sharpobject/80dc1b6f3aaeeada8c0e3a04ebc4b60a

pub fn mm256_shuffle_epi8(x: v.u8x32, mask: v.u8x32) v.u8x32 {
    const has_sse2 = comptime std.Target.x86.featureSetHas(builtin.cpu.features, .sse2);
    if (builtin.cpu.arch != .x86_64 or !has_sse2)
        @compileError("missing cpu feature set: x86_64+sse2. please provide -mcpu=x86_64+sse2");
    return asm (
        \\ vpshufb %[mask], %[x], %[out]
        : [out] "=x" (-> v.u8x32),
        : [x] "+x" (x),
          [mask] "x" (mask),
    );
}

pub fn prefix_xor(bitmask: u64) u64 {
    // There should be no such thing with a processor supporting avx2
    // but not clmul.
    const has_pclmul = comptime std.Target.x86.featureSetHas(builtin.cpu.features, .pclmul);
    if (builtin.cpu.arch != .x86_64 or !has_pclmul)
        @compileError("missing cpu feature set: x86_64+pclmul. please provide -mcpu=x86_64+pclmul");
    const all_ones = @bitCast(u128, @splat(16, @as(u8, 0xff)));
    const x = @bitCast(u128, [2]u64{ bitmask, 0 });
    const m = clmulPclmul(x, all_ones, .lo);

    return @bitCast([2]u64, m)[0];
}

// from zig/lib/std/crypto/ghash_polyval.zig
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

const I256 = struct {
    hi: u128,
    lo: u128,
    mid: u128,
};

pub extern fn _prev1_old(a: v.u8x32, b: v.u8x32) v.u8x32;
pub extern fn _prev2_old(a: v.u8x32, b: v.u8x32) v.u8x32;
pub extern fn _prev3_old(a: v.u8x32, b: v.u8x32) v.u8x32;
