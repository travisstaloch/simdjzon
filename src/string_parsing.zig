const std = @import("std");
const mem = std.mem;
usingnamespace @import("vector_types.zig");
const _mm256_storeu_si256 = @import("llvm_intrinsics.zig")._mm256_storeu_si256;
const common = @import("common.zig");

const escape_map = [256]u8{
    0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x0.
    0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0x22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x2f,
    0, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x4.
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x5c, 0, 0, 0, // 0x5.
    0, 0, 0x08, 0, 0, 0, 0x0c, 0, 0, 0, 0, 0, 0, 0, 0x0a, 0, // 0x6.
    0, 0, 0x0d, 0, 0x09, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x7.

    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

// handle a unicode codepoint
// write appropriate values into dest
// src will advance 6 bytes or 12 bytes
// dest will advance a variable amount (return via pointer)
// return true if the unicode codepoint was valid
// We work in little-endian then swap at write time

// inline fn handle_unicode_codepoint(src: *std.io.StreamSource, dst: *std.ArrayListUnmanaged(u8), allocator: *mem.Allocator) !bool {
inline fn handle_unicode_codepoint2(dst: []u8) !bool {
    // jsoncharutils::hex_to_u32_nocheck fills high 16 bits of the return value with 1s if the
    // conversion isn't valid; we defer the check for this to inside the
    // multilingual plane check

    var buf: [6]u8 = undefined;
    var dst_buf: [4]u8 = undefined;
    {
        // const nbytes = try src.read(&buf);
        // if (nbytes != 6) return error.EndOfStream;
        buf[0..6].* = dst[0..6].*;
    }
    var code_point = CharUtils.hex_to_u32_nocheck(buf[2..6].*);
    // check for low surrogate for characters outside the Basic
    // Multilingual Plane.
    if (code_point >= 0xd800 and code_point < 0xdc00) {
        if ((buf[0] != '\\') or buf[1] != 'u') {
            return false;
        }
        const code_point_2 = CharUtils.hex_to_u32_nocheck(buf[2..6].*);

        // if the first code point is invalid we will get here, as we will go past
        // the check for being outside the Basic Multilingual plane. If we don't
        // find a \u immediately afterwards we fail out anyhow, but if we do,
        // this check catches both the case of the first code point being invalid
        // or the second code point being invalid.
        if ((code_point | code_point_2) >> 16 != 0) {
            return false;
        }

        code_point =
            (((code_point - 0xd800) << 10) | (code_point_2 -% 0xdc00)) +% 0x10000;
        // const nbytes = try src.read(&buf);
        // if (nbytes != 6) return error.EndOfStream;
        buf[0..6].* = dst[6..12].*;
    }
    const offset = CharUtils.codepoint_to_utf8(code_point, &dst_buf);
    // try dst.appendSlice(allocator, dst_buf[0..offset]);
    mem.copy(u8, dst, dst_buf[0..offset]);
    return offset > 0;
}

inline fn handle_unicode_codepoint(src_ptr: *[*]const u8, dst_ptr: *[*]u8) bool {
    // jsoncharutils::hex_to_u32_nocheck fills high 16 bits of the return value with 1s if the
    // conversion isn't valid; we defer the check for this to inside the
    // multilingual plane check
    var code_point = CharUtils.hex_to_u32_nocheck(src_ptr.* + 2);
    src_ptr.* += 6;
    // check for low surrogate for characters outside the Basic
    // Multilingual Plane.
    if (code_point >= 0xd800 and code_point < 0xdc00) {
        if ((src_ptr.*[0] != '\\') or src_ptr.*[1] != 'u') {
            return false;
        }
        const code_point_2 = CharUtils.hex_to_u32_nocheck(src_ptr.* + 2);

        // if the first code point is invalid we will get here, as we will go past
        // the check for being outside the Basic Multilingual plane. If we don't
        // find a \u immediately afterwards we fail out anyhow, but if we do,
        // this check catches both the case of the first code point being invalid
        // or the second code point being invalid.
        if ((code_point | code_point_2) >> 16 != 0) {
            return false;
        }

        code_point =
            (((code_point - 0xd800) << 10) | (code_point_2 -% 0xdc00)) +% 0x10000;
        src_ptr.* += 6;
    }
    const offset = CharUtils.codepoint_to_utf8(code_point, dst_ptr.*);
    dst_ptr.* += offset;
    return offset > 0;
}

/// Unescape a string from src to dst, stopping at a final unescaped quote. E.g., if src points at 'joe"', then
/// dst needs to have four free bytes.
pub inline fn parse_string(src_: [*]const u8, dst_: [*]u8) ?[*]u8 {
    var src = src_;
    var dst = dst_;
    while (true) {
        // Copy the next n bytes, and find the backslash and quote in them.
        const bs_quote = try BackslashAndQuote.copy_and_find(src, dst);

        // std.log.debug("bs_quote {b}", .{bs_quote});
        // If the next thing is the end quote, copy and return
        if (bs_quote.has_quote_first()) {
            // we encountered quotes first. Move dst to point to quotes and exit
            // std.log.debug("has_quote_first quote_index {} dst.items.len {}", .{ bs_quote.quote_index(), dst.items.len });
            return dst + bs_quote.quote_index();
        }
        if (bs_quote.has_backslash()) {
            //    find out where the backspace is */
            const bs_dist = bs_quote.backslash_index();
            const escape_char = src[bs_dist + 1];
            //    we encountered backslash first. Handle backslash */
            if (escape_char == 'u') {
                //  move src/dst up to the start; they will be further adjusted
                //    within the unicode codepoint handling code. */
                src += bs_dist;
                dst += bs_dist;
                if (!handle_unicode_codepoint(&src, &dst)) {
                    return null;
                }
            } else {
                //  simple 1:1 conversion. Will eat bs_dist+2 characters in input and
                //  * write bs_dist+1 characters to output
                //  * note this may reach beyond the part of the buffer we've actually
                //  * seen. I think this is ok */
                const escape_result = escape_map[escape_char];
                if (escape_result == 0) {
                    return null; // bogus escape value is an error */
                }
                // std.debug.panic("TODO put escape result into dest", .{});
                dst[bs_dist] = escape_result;
                src += bs_dist + 2;
                dst += bs_dist + 1;
            }
        } else {
            //   /* they are the same. Since they can't co-occur, it means we
            //    * encountered neither. */
            src += BackslashAndQuote.BYTES_PROCESSED;
            dst += BackslashAndQuote.BYTES_PROCESSED;
        }
    }
    //   /* can't be reached */
    unreachable;
}

/// allocates and returns an unescaped a string from src, stopping at a final unescaped quote. 
/// e.g., if src points at 'joe"', returns 'joe'.
/// caller owns the memory. 
pub inline fn parse_string_alloc(comptime T: type, src_: [*]const u8, allocator: *mem.Allocator, comptime max_str_len: u16) !T {
    var src = src_;
    var dst_list = std.ArrayListUnmanaged(u8){};
    try dst_list.ensureTotalCapacity(allocator, BackslashAndQuote.BYTES_PROCESSED);
    errdefer dst_list.deinit(allocator);

    while (true) {
        if (dst_list.items.len > max_str_len) return error.CAPACITY;
        // Copy the next n bytes, and find the backslash and quote in them.

        const bs_quote = try BackslashAndQuote.copy_and_find(src, dst_list.items.ptr + dst_list.items.len);

        // std.log.debug("bs_quote {b}", .{bs_quote});
        // If the next thing is the end quote, copy and return
        if (bs_quote.has_quote_first()) {
            // we encountered quotes first. Move dst to point to quotes and exit
            // std.log.debug("has_quote_first quote_index {} dst.items.len {}", .{ bs_quote.quote_index(), dst.items.len });
            // return dst + bs_quote.quote_index();
            dst_list.items.len += bs_quote.quote_index();
            return dst_list.toOwnedSlice(allocator);
        }
        if (bs_quote.has_backslash()) {
            //    find out where the backspace is */
            const bs_dist = bs_quote.backslash_index();
            const escape_char = src[bs_dist + 1];
            //    we encountered backslash first. Handle backslash */
            if (escape_char == 'u') {
                //  move src/dst up to the start; they will be further adjusted
                //    within the unicode codepoint handling code. */
                src += bs_dist;
                dst_list.items.len += bs_dist;
                var dst = dst_list.items.ptr + dst_list.items.len;
                if (!handle_unicode_codepoint(&src, &dst))
                    return error.STRING_ERROR;
                dst_list.items.len += try common.ptr_diff(u8, dst, dst_list.items.ptr + dst_list.items.len);
            } else {
                //  simple 1:1 conversion. Will eat bs_dist+2 characters in input and
                //  * write bs_dist+1 characters to output
                //  * note this may reach beyond the part of the buffer we've actually
                //  * seen. I think this is ok */
                const escape_result = escape_map[escape_char];
                if (escape_result == 0) {
                    return error.STRING_ERROR; // bogus escape value is an error */
                }
                (dst_list.items.ptr + dst_list.items.len)[bs_dist] = escape_result;
                src += bs_dist + 2;
                dst_list.items.len += 1;
            }
        } else {
            //   /* they are the same. Since they can't co-occur, it means we
            //    * encountered neither. */
            src += BackslashAndQuote.BYTES_PROCESSED;
            dst_list.items.len += BackslashAndQuote.BYTES_PROCESSED;
        }
        try dst_list.ensureUnusedCapacity(allocator, BackslashAndQuote.BYTES_PROCESSED);
    }
    //   /* can't be reached */
    unreachable;
}

pub const BackslashAndQuote = struct {
    bs_bits: u32,
    quote_bits: u32,
    pub const BYTES_PROCESSED = 32;

    pub inline fn copy_and_find(src: [*]const u8, dst: [*]u8) !BackslashAndQuote {
        // std.log.debug("nbytes {} s: '{s}'", .{ nbytes, s[0..nbytes] });
        const v: u8x32 = src[0..BYTES_PROCESSED].*;
        // store to dest unconditionally - we can overwrite the bits we don't like later
        // v.store(dst);
        // VMOVDQU
        // _mm256_storeu_si256(dst, v);
        dst[0..BYTES_PROCESSED].* = v;
        const bs = v == @splat(BYTES_PROCESSED, @as(u8, '\\'));
        const qs = v == @splat(BYTES_PROCESSED, @as(u8, '"'));
        return BackslashAndQuote{ .bs_bits = @ptrCast(*const u32, &bs).*, .quote_bits = @ptrCast(*const u32, &qs).* };
    }

    fn has_quote_first(bsq: BackslashAndQuote) bool {
        return ((bsq.bs_bits -% 1) & bsq.quote_bits) != 0;
    }
    fn has_backslash(bsq: BackslashAndQuote) bool {
        return ((bsq.quote_bits -% 1) & bsq.bs_bits) != 0;
    }
    fn quote_index(bsq: BackslashAndQuote) u32 {
        return @ctz(u32, bsq.quote_bits);
    }
    fn backslash_index(bsq: BackslashAndQuote) u32 {
        return @ctz(u32, bsq.bs_bits);
    }

    pub fn format(value: BackslashAndQuote, comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = try writer.print("bs_bits {b:0>32} quote_bits {b:0>32}", .{ value.bs_bits, value.quote_bits });
    }
};

pub const Value128 = struct {
    low: u64,
    high: u64,
};

pub const CharUtils = struct {
    const digit_to_val32 = [886]u32{
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0x0,        0x1,        0x2,        0x3,        0x4,        0x5,
        0x6,        0x7,        0x8,        0x9,        0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xa,
        0xb,        0xc,        0xd,        0xe,        0xf,        0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xa,        0xb,        0xc,        0xd,        0xe,
        0xf,        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0x0,        0x10,       0x20,       0x30,       0x40,       0x50,
        0x60,       0x70,       0x80,       0x90,       0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xa0,
        0xb0,       0xc0,       0xd0,       0xe0,       0xf0,       0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xa0,       0xb0,       0xc0,       0xd0,       0xe0,
        0xf0,       0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0x0,        0x100,      0x200,      0x300,      0x400,      0x500,
        0x600,      0x700,      0x800,      0x900,      0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xa00,
        0xb00,      0xc00,      0xd00,      0xe00,      0xf00,      0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xa00,      0xb00,      0xc00,      0xd00,      0xe00,
        0xf00,      0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0x0,        0x1000,     0x2000,     0x3000,     0x4000,     0x5000,
        0x6000,     0x7000,     0x8000,     0x9000,     0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xa000,
        0xb000,     0xc000,     0xd000,     0xe000,     0xf000,     0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xa000,     0xb000,     0xc000,     0xd000,     0xe000,
        0xf000,     0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
        0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
    };

    pub inline fn hex_to_u32_nocheck(src: [*]const u8) u32 {
        const v1 = digit_to_val32[630 + @as(u16, src[0])];
        const v2 = digit_to_val32[420 + @as(u16, src[1])];
        const v3 = digit_to_val32[210 + @as(u16, src[2])];
        const v4 = digit_to_val32[0 + @as(u16, src[3])];
        return v1 | v2 | v3 | v4;
    }

    pub inline fn codepoint_to_utf8(cp: u32, c: [*]u8) u3 {
        if (cp <= 0x7F) {
            c[0] = @truncate(u8, cp);
            return 1; // ascii
        }
        if (cp <= 0x7FF) {
            c[0] = @truncate(u8, (cp >> 6) + 192);
            c[1] = @truncate(u8, (cp & 63) + 128);
            return 2; // universal plane
            //  Surrogates are treated elsewhere...
            //} //else if (0xd800 <= cp && cp <= 0xdfff) {
            //  return 0; // surrogates // could put assert here
        } else if (cp <= 0xFFFF) {
            c[0] = @truncate(u8, (cp >> 12) + 224);
            c[1] = @truncate(u8, ((cp >> 6) & 63) + 128);
            c[2] = @truncate(u8, (cp & 63) + 128);
            return 3;
        } else if (cp <= 0x10FFFF) { // if you know you have a valid code point, this
            // is not needed
            c[0] = @truncate(u8, (cp >> 18) + 240);
            c[1] = @truncate(u8, ((cp >> 12) & 63) + 128);
            c[2] = @truncate(u8, ((cp >> 6) & 63) + 128);
            c[3] = @truncate(u8, (cp & 63) + 128);
            return 4;
        }
        // will return 0 when the code point was too large.
        return 0; // bad r

    }

    // return non-zero if not a structural or whitespace char
    // zero otherwise
    pub fn is_not_structural_or_whitespace(c: u8) bool {
        return structural_or_whitespace_negated[c] != 0;
    }

    pub fn is_structural_or_whitespace(c: u8) bool {
        return structural_or_whitespace[c] != 0;
    }

    const structural_or_whitespace_negated = [256]u1{
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    };

    const structural_or_whitespace = [256]u1{
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    };

    pub fn full_multiplication(value1: u64, value2: u64) Value128 {
        const r = @as(u128, value1) * value2;
        return .{
            .low = @truncate(u64, r),
            .high = @truncate(u64, r >> 64),
        };
    }
};
