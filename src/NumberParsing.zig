const std = @import("std");
const mem = std.mem;
usingnamespace @import("vector_types.zig");
usingnamespace @import("llvm_intrinsics.zig");
const main = @import("main.zig");
const Error = main.Error;
const println = main.println;
const CharUtils = @import("StringParsing.zig").CharUtils;
const Iterator = main.Iterator;
const TapeBuilder = main.TapeBuilder;
const TapeType = main.TapeType;

fn INVALID_NUMBER(src: *std.io.StreamSource) Error {
    _ = src;
    return error.NUMBER_ERROR;
}

pub fn parse_number(
    value: u8,
    iter: *Iterator,
    tb: *TapeBuilder,
) Error!void {
    const negative = value == '-';
    const src = &iter.parser.stream_source;
    const start_digits_pos = (try src.getPos()) + @boolToInt(negative);
    //
    // Parse the integer part.
    //
    // PERF NOTE: we don't use is_made_of_eight_digits_fast because large integers like 123456789 are rare

    var i: u64 = 0;

    var buf: [1]u8 = .{value};
    const first_digit = if (!negative) value else blk: {
        const nbytes = try src.read(&buf);
        if (nbytes != 0)
            break :blk buf[0]
        else
            return INVALID_NUMBER(src);
    };
    while (true) {
        if (!parse_digit(u64, buf[0], &i))
            break;
        const nbytes = try src.read(&buf);
        if (nbytes == 0) break;
    }
    var digit_count = (try src.getPos()) - start_digits_pos;
    // println("parse_digit i {} digit_count {}", .{ i, digit_count });
    if (digit_count == 0 or '0' == first_digit) {
        return INVALID_NUMBER(src);
    }

    // //
    // // Handle floats if there is a . or e (or both)
    // //
    // var exponent: u64 = 0;
    // var is_float = false;
    // if ('.' == *p) {
    //     is_float = true;
    //     // ++p;
    //     try parse_decimal(src, p, i, exponent);
    //     digit_count = p - start_digits; // used later to guard against overflows
    // }
    // if (('e' == *p) || ('E' == *p)) {
    //     is_float = true;
    //     // ++p;
    //     try parse_exponent(src, p, exponent);
    // }
    // if (is_float) {
    //     const dirty_end = CharUtils.is_not_structural_or_whitespace(*p);
    //     try write_float(src, negative, i, start_digits, digit_count, exponent, writer);
    //     if (dirty_end) {
    //         return INVALID_NUMBER(src);
    //     }
    //     return SUCCESS;
    // }

    // // The longest negative 64-bit number is 19 digits.
    // // The longest positive 64-bit number is 20 digits.
    // // We do it this way so we don't trigger this branch unless we must.
    // const longest_digit_count = if (negative) 19 else 20;
    // if (digit_count > longest_digit_count) {
    //     return INVALID_NUMBER(src);
    // }
    // if (digit_count == longest_digit_count) {
    //     if (negative) {
    //         // Anything negative above INT64_MAX+1 is invalid
    //         if (i > uint64_t(INT64_MAX) + 1) {
    //             return INVALID_NUMBER(src);
    //         }
    //         WRITE_INTEGER(~i + 1, src, writer);
    //         if (CharUtils.is_not_structural_or_whitespace(*p)) {
    //             return INVALID_NUMBER(src);
    //         }
    //         return SUCCESS;
    //         // Positive overflow check:
    //         // - A 20 digit number starting with 2-9 is overflow, because 18,446,744,073,709,551,615 is the
    //         //   biggest uint64_t.
    //         // - A 20 digit number starting with 1 is overflow if it is less than INT64_MAX.
    //         //   If we got here, it's a 20 digit number starting with the digit "1".
    //         // - If a 20 digit number starting with 1 overflowed (i*10+digit), the result will be smaller
    //         //   than 1,553,255,926,290,448,384.
    //         // - That is smaller than the smallest possible 20-digit number the user could write:
    //         //   10,000,000,000,000,000,000.
    //         // - Therefore, if the number is positive and lower than that, it's overflow.
    //         // - The value we are looking at is less than or equal to 9,223,372,036,854,775,808 (INT64_MAX).
    //         //
    //     } else if (src[0] != '1' or i <= uint64_t(INT64_MAX)) {
    //         return INVALID_NUMBER(src);
    //     }
    // }

    // Write unsigned if it doesn't fit in a signed integer.
    if (i > @as(u64, std.math.maxInt(i64))) {

        // WRITE_UNSIGNED(i, src, writer);
        try tb.append2(iter, 0, i, .INT64);
    } else {
        // WRITE_INTEGER(if (negative) (~i + 1) else i, src, writer);
        try tb.append2(iter, 0, if (negative) (~i + 1) else i, .INT64);
    }

    // TODO
    // if (CharUtils.is_not_structural_or_whitespace(*p)) {
    //     return INVALID_NUMBER(src);
    // }

}

// We deliberately allow overflow here and check later
fn parse_digit(comptime I: type, char: u8, i: *I) bool {
    const digit = char -% '0';
    if (digit > 9) {
        return false;
    }
    // PERF NOTE: multiplication by 10 is cheaper than arbitrary integer multiplication
    i.* = 10 *% i.* + digit; // might overflow, we will handle the overflow later
    return true;
}
