const std = @import("std");
const builtin = @import("builtin");
const v = @import("vector_types.zig");
const cmn = @import("common.zig");
const Chunk = cmn.Chunk;
const chunk_len = cmn.chunk_len;
const half_chunk_len = chunk_len / 2;

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
