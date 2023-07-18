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
    return @bitCast(__mm256_permute2x128_si256_0x21(V, @as(V, @bitCast(a)), @as(V, @bitCast(b))));
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
        \\tbl  %[out].16b, {%[mask].16b}, %[x].16b
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
    const has_native_carryless_multiply = switch (builtin.cpu.arch) {
        // There should be no such thing with a processor supporting avx but not clmul.
        .x86_64 => cmn.has_pclmul and cmn.has_avx,
        .aarch64 => cmn.has_armaes,
        else => false,
    };

    if (!has_native_carryless_multiply) {
        var z = bitmask;
        z ^= z << 1;
        z ^= z << 2;
        z ^= z << 4;
        z ^= z << 8;
        z ^= z << 16;
        z ^= z << 32;
        return z;
    }

    // do a carryless multiply by all 1's
    // adapted from zig/lib/std/crypto/ghash_polyval.zig
    const x: u128 = @bitCast([2]u64{ bitmask, 0 });
    const y: u128 = @bitCast(@as(v.u8x16, @splat(0xff)));

    return switch (builtin.cpu.arch) {
        .x86_64 => asm (
            \\ vpclmulqdq $0x00, %[x], %[y], %[out]
            : [out] "=x" (-> @Vector(2, u64)),
            : [x] "x" (@as(@Vector(2, u64), @bitCast(x))),
              [y] "x" (@as(@Vector(2, u64), @bitCast(y))),
        ),

        .aarch64 => asm (
            \\ pmull %[out].1q, %[x].1d, %[y].1d
            : [out] "=w" (-> @Vector(2, u64)),
            : [x] "w" (@as(@Vector(2, u64), @bitCast(x))),
              [y] "w" (@as(@Vector(2, u64), @bitCast(y))),
        ),

        else => unreachable,
    }[0];
}
