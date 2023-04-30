const std = @import("std");
const mem = std.mem;
const main = @import("main.zig");
const CharUtils = @import("string_parsing.zig").CharUtils;
const dom = @import("dom.zig");
const Iterator = dom.Iterator;
const TapeBuilder = dom.TapeBuilder;
const TapeType = dom.TapeType;
const common = @import("common.zig");
const Error = common.Error;
const println = common.println;

fn INVALID_NUMBER(src: [*]const u8) Error {
    _ = src;
    return error.NUMBER_ERROR;
}

fn WRITE_DOUBLE(VALUE: f64, _: [*]const u8, WRITER: *TapeBuilder) void {
    (WRITER).append_double(VALUE);
}

fn WRITE_INTEGER(VALUE: u64, _: [*]const u8, WRITER: *TapeBuilder) void {
    (WRITER).append_i64(VALUE);
}

fn WRITE_UNSIGNED(VALUE: u64, _: [*]const u8, WRITER: *TapeBuilder) void {
    (WRITER).append_u64(VALUE);
}

pub fn parse_number(
    src: [*]const u8,
    tb: *TapeBuilder,
) Error!void {
    const negative = src[0] == '-';
    var p = src + @boolToInt(negative);

    //
    // Parse the integer part.
    //
    // PERF NOTE: we don't use is_made_of_eight_digits_fast because large integers like 123456789 are rare
    const start_digits = p;
    var i: u64 = 0;

    while (parse_digit(u64, p[0], &i)) p += 1;

    var digit_count = try common.ptr_diff(u32, p, start_digits);
    // println("parse_digit i {} digit_count {}", .{ i, digit_count });
    if (digit_count == 0 or ('0' == start_digits[0] and digit_count > 1)) {
        return INVALID_NUMBER(src);
    }

    //
    // Handle floats if there is a . or e (or both)
    //
    var exponent: i64 = 0;
    var is_float = false;
    if ('.' == p[0]) {
        is_float = true;
        p += 1;
        // std.log.debug("calling parse_decimal p[0] {c} i {} ", .{ p[0], i });
        try parse_decimal(src, &p, &i, &exponent);
        digit_count = try common.ptr_diff(u32, p, start_digits); // used later to guard against overflows
    }
    if ('e' == p[0] or 'E' == p[0]) {
        is_float = true;
        p += 1;
        try parse_exponent(src, &p, &exponent);
    }
    if (is_float) {
        // main.println("is_float {c}", .{p[0]});
        const dirty_end = CharUtils.is_not_structural_or_whitespace(p[0]);
        try write_float(src, negative, i, start_digits, digit_count, exponent, tb);
        if (dirty_end) {
            return INVALID_NUMBER(src);
        }
        return;
    }

    // The longest negative 64-bit number is 19 digits.
    // The longest positive 64-bit number is 20 digits.
    // We do it this way so we don't trigger this branch unless we must.
    const longest_digit_count: usize = if (negative) 19 else 20;
    if (digit_count > longest_digit_count) {
        return INVALID_NUMBER(src);
    }
    if (digit_count == longest_digit_count) {
        if (negative) {
            // Anything negative above INT64_MAX+1 is invalid
            if (i > @as(u64, std.math.maxInt(i64)) + 1) {
                return INVALID_NUMBER(src);
            }
            WRITE_INTEGER(~i + 1, src, tb);
            if (CharUtils.is_not_structural_or_whitespace(p[0])) {
                return INVALID_NUMBER(src);
            }
            return;
            // Positive overflow check:
            // - A 20 digit number starting with 2-9 is overflow, because 18,446,744,073,709,551,615 is the
            //   biggest uint64_t.
            // - A 20 digit number starting with 1 is overflow if it is less than INT64_MAX.
            //   If we got here, it's a 20 digit number starting with the digit "1".
            // - If a 20 digit number starting with 1 overflowed (i*10+digit), the result will be smaller
            //   than 1,553,255,926,290,448,384.
            // - That is smaller than the smallest possible 20-digit number the user could write:
            //   10,000,000,000,000,000,000.
            // - Therefore, if the number is positive and lower than that, it's overflow.
            // - The value we are looking at is less than or equal to 9,223,372,036,854,775,808 (INT64_MAX).
            //
        } else if (src[0] != '1' or i <= @as(u64, std.math.maxInt(i64))) {
            return INVALID_NUMBER(src);
        }
    }

    // Write unsigned if it doesn't fit in a signed integer.
    if (i > @as(u64, std.math.maxInt(i64))) {
        WRITE_UNSIGNED(i, src, tb);
        // try tb.append2(iter, 0, i, .INT64);
    } else {
        WRITE_INTEGER(if (negative) (~i +% 1) else i, src, tb);
        // try tb.append2(iter, 0, if (negative) (~i +% 1) else i, .INT64);
    }
    // std.log.debug("parse number last '{c}'(0x{x}:{})", .{ p[0], p[0], p[0] });
    if (CharUtils.is_not_structural_or_whitespace(p[0]))
        return INVALID_NUMBER(src);
}

// We deliberately allow overflow here and check later
fn parse_digit(comptime I: type, char: u8, i: *I) bool {
    const digit = char -% '0';
    if (digit > 9) return false;

    // PERF NOTE: multiplication by 10 is cheaper than arbitrary integer multiplication
    i.* = 10 *% i.* + digit; // might overflow, we will handle the overflow later
    return true;
}

fn parse_decimal(src: [*]const u8, p: *[*]const u8, i: *u64, exponent: *i64) !void {
    // we continue with the fiction that we have an integer. If the
    // floating point number is representable as x * 10^z for some integer
    // z that fits in 53 bits, then we will be able to convert back the
    // the integer into a float in a lossless manner.
    const first_after_period = p.*;

    // TODO
    // #ifdef SWAR_NUMBER_PARSING
    //   // this helps if we have lots of decimals!
    //   // this turns out to be frequent enough.
    //   if (is_made_of_eight_digits_fast(p)) {
    //     i = i * 100000000 + parse_eight_digits_unrolled(p);
    //     p += 8;
    //   }
    // #endif
    // Unrolling the first digit makes a small difference on some implementations (e.g. westmere)
    // if (parse_digit(u64, p.*[0], i))
    //     p.* += 1;

    while (parse_digit(u64, p.*[0], i)) p.* += 1;

    exponent.* =
        (std.math.cast(i64, @ptrToInt(first_after_period)) orelse return error.Overflow) -
        (std.math.cast(i64, @ptrToInt(p.*)) orelse return error.Overflow);
    // std.log.debug("exponent {} firstap {*} p {*}", .{ exponent.*, first_after_period, p });
    // Decimal without digits (123.) is illegal
    if (exponent.* == 0) {
        return INVALID_NUMBER(src);
    }
}

inline fn parse_exponent(src: [*]const u8, p: *[*]const u8, exponent: *i64) !void {
    // Exp Sign: -123.456e[-]78
    const neg_exp = ('-' == p.*[0]);
    if (neg_exp or '+' == p.*[0]) {
        p.* += 1;
    } // Skip + as well

    // Exponent: -123.456e-[78]
    var start_exp = p.*;
    var exp_number: i64 = 0;
    while (parse_digit(i64, p.*[0], &exp_number)) p.* += 1;

    // It is possible for parse_digit to overflow.
    // In particular, it could overflow to INT64_MIN, and we cannot do - INT64_MIN.
    // Thus we *must* check for possible overflow before we negate exp_number.

    // Performance notes: it may seem like combining the two "simdjson_unlikely checks" below into
    // a single simdjson_unlikely path would be faster. The reasoning is sound, but the compiler may
    // not oblige and may, in fact, generate two distinct paths in any case. It might be
    // possible to do uint64_t(p - start_exp - 1) >= 18 but it could end up trading off
    // instructions for a simdjson_likely branch, an unconclusive gain.

    // If there were no digits, it's an error.
    if (p.* == start_exp) {
        return INVALID_NUMBER(src);
    }
    // We have a valid positive exponent in exp_number at this point, except that
    // it may have overflowed.

    // If there were more than 18 digits, we may have overflowed the integer. We have to do
    // something!!!!
    if (@ptrToInt(p.*) > @ptrToInt(start_exp + 18)) {
        // Skip leading zeroes: 1e000000000000000000001 is technically valid and doesn't overflow
        while (start_exp[0] == '0') start_exp += 1;

        // 19 digits could overflow int64_t and is kind of absurd anyway. We don't
        // support exponents smaller than -999,999,999,999,999,999 and bigger
        // than 999,999,999,999,999,999.
        // We can truncate.
        // Note that 999999999999999999 is assuredly too large. The maximal ieee64 value before
        // infinity is ~1.8e308. The smallest subnormal is ~5e-324. So, actually, we could
        // truncate at 324.
        // Note that there is no reason to fail per se at this point in time.
        // E.g., 0e999999999999999999999 is a fine number.
        if (@ptrToInt(p.*) > @ptrToInt(start_exp + 18)) {
            exp_number = 999999999999999999;
        }
    }
    // At this point, we know that exp_number is a sane, positive, signed integer.
    // It is <= 999,999,999,999,999,999. As long as 'exponent' is in
    // [-8223372036854775808, 8223372036854775808], we won't overflow. Because 'exponent'
    // is bounded in magnitude by the size of the JSON input, we are fine in this universe.
    // To sum it up: the next line should never overflow.
    exponent.* += if (neg_exp) -exp_number else exp_number;
}

fn significant_digits(start_digits: [*]const u8, digit_count: usize) usize {
    // It is possible that the integer had an overflow.
    // We have to handle the case where we have 0.0000somenumber.
    var start = start_digits;
    while ((start[0] == '0') or (start[0] == '.')) start += 1;

    // we over-decrement by one when there is a '.'
    return digit_count -% @ptrToInt(start) - @ptrToInt(start_digits);
}

fn slow_float_parsing(src: [*]const u8, writer: *TapeBuilder) !void {
    var d: f64 = undefined;
    if (parse_float_fallback(src, &d)) {
        writer.append_double(d);
        return;
    }
    return INVALID_NUMBER(src);
}

const AdjustedMantissa = struct {
    mantissa: u64 = 0,
    power2: i32 = 0,
};

const max_digits = 768;
const decimal_point_range = 2047;

const Decimal = struct {
    num_digits: u32,
    decimal_point: i32,
    negative: bool,
    truncated: bool,
    digits: [max_digits]u8,
};

fn is_integer(c: u8) bool {
    return (c >= '0' and c <= '9');
}

// This should always succeed since it follows a call to parse_number.
fn parse_decimal2(p_: [*]const u8) Decimal {
    var p = p_;
    var answer: Decimal = undefined;
    answer.num_digits = 0;
    answer.decimal_point = 0;
    answer.truncated = false;
    answer.negative = (p[0] == '-');
    if (p[0] == '-' or p[0] == '+')
        p += 1;

    while (p[0] == '0')
        p += 1;

    while (is_integer(p[0])) {
        if (answer.num_digits < max_digits)
            answer.digits[answer.num_digits] = p[0] - '0';

        answer.num_digits += 1;
        p += 1;
    }
    if (p[0] == '.') {
        p += 1;
        const first_after_period = p;
        // if we have not yet encountered a zero, we have to skip it as well
        if (answer.num_digits == 0) {
            // skip zeros
            while (p[0] == '0') {
                p += 1;
            }
        }
        while (is_integer(p[0])) {
            if (answer.num_digits < max_digits) {
                answer.digits[answer.num_digits] = p[0] - '0';
            }
            answer.num_digits += 1;
            p += 1;
        }

        answer.decimal_point = @truncate(i32, @bitCast(isize, @ptrToInt(first_after_period) -% @ptrToInt(p)));
    }
    if (answer.num_digits > 0) {
        var preverse = p - 1;
        var trailing_zeros: i32 = 0;
        while ((preverse[0] == '0') or (preverse[0] == '.')) {
            if (preverse[0] == '0') trailing_zeros += 1;
            preverse -= 1;
        }
        answer.decimal_point += @intCast(i32, answer.num_digits);
        answer.num_digits -= @intCast(u32, trailing_zeros);
    }
    if (answer.num_digits > max_digits) {
        answer.num_digits = max_digits;
        answer.truncated = true;
    }
    if (('e' == p[0]) or ('E' == p[0])) {
        p += 1;
        var neg_exp = false;
        if ('-' == p[0]) {
            neg_exp = true;
            p += 1;
        } else if ('+' == p[0]) {
            p += 1;
        }
        var exp_number: i32 = 0; // exponential part
        while (is_integer(p[0])) {
            var digit = p[0] - '0';
            if (exp_number < 0x10000)
                exp_number = 10 * exp_number + digit;

            p += 1;
        }
        answer.decimal_point += if (neg_exp) -exp_number else exp_number;
    }
    return answer;
}

// remove all final zeroes
inline fn trim(h: *Decimal) void {
    while ((h.num_digits > 0) and (h.digits[h.num_digits - 1] == 0)) {
        h.num_digits -= 1;
    }
}

// computes h * 2^shift
fn decimal_right_shift(h: *Decimal, shift: u6) void {
    var read_index: u32 = 0;
    var write_index: u32 = 0;

    var n: u64 = 0;

    while ((n >> shift) == 0) {
        if (read_index < h.num_digits) {
            n = (10 * n) + h.digits[read_index];
            read_index += 1;
        } else if (n == 0) {
            return;
        } else {
            while ((n >> shift) == 0) {
                n = 10 * n;
                read_index += 1;
            }
            break;
        }
    }
    h.decimal_point -= @intCast(i32, read_index - 1);
    if (h.decimal_point < -decimal_point_range) { // it is zero
        h.num_digits = 0;
        h.decimal_point = 0;
        h.negative = false;
        h.truncated = false;
        return;
    }
    const mask = (@as(u64, 1) << shift) - 1;
    while (read_index < h.num_digits) {
        const new_digit = @intCast(u8, n >> shift);
        n = (10 * (n & mask)) + h.digits[read_index];
        read_index += 1;
        h.digits[write_index] = new_digit;
        write_index += 1;
    }
    while (n > 0) {
        const new_digit = @intCast(u8, n >> shift);
        n = 10 * (n & mask);
        if (write_index < max_digits) {
            h.digits[write_index] = new_digit;
            write_index += 1;
        } else if (new_digit > 0) {
            h.truncated = true;
        }
    }
    h.num_digits = write_index;
    trim(h);
}

fn number_of_digits_decimal_left_shift(h: *Decimal, shift_: u32) u32 {
    var shift = shift_ & 63;
    const number_of_digits_decimal_left_shift_table = [65]u16{
        0x0000, 0x0800, 0x0801, 0x0803, 0x1006, 0x1009, 0x100D, 0x1812, 0x1817,
        0x181D, 0x2024, 0x202B, 0x2033, 0x203C, 0x2846, 0x2850, 0x285B, 0x3067,
        0x3073, 0x3080, 0x388E, 0x389C, 0x38AB, 0x38BB, 0x40CC, 0x40DD, 0x40EF,
        0x4902, 0x4915, 0x4929, 0x513E, 0x5153, 0x5169, 0x5180, 0x5998, 0x59B0,
        0x59C9, 0x61E3, 0x61FD, 0x6218, 0x6A34, 0x6A50, 0x6A6D, 0x6A8B, 0x72AA,
        0x72C9, 0x72E9, 0x7B0A, 0x7B2B, 0x7B4D, 0x8370, 0x8393, 0x83B7, 0x83DC,
        0x8C02, 0x8C28, 0x8C4F, 0x9477, 0x949F, 0x94C8, 0x9CF2, 0x051C, 0x051C,
        0x051C, 0x051C,
    };
    const x_a: u32 = number_of_digits_decimal_left_shift_table[shift];
    const x_b: u32 = number_of_digits_decimal_left_shift_table[shift + 1];
    const num_new_digits: u32 = x_a >> 11;
    const pow5_a: u32 = 0x7FF & x_a;
    const pow5_b: u32 = 0x7FF & x_b;
    const number_of_digits_decimal_left_shift_table_powers_of_5 = [0x051C]u8{
        5, 2, 5, 1, 2, 5, 6, 2, 5, 3, 1, 2, 5, 1, 5, 6, 2, 5, 7, 8, 1, 2, 5,
        3, 9, 0, 6, 2, 5, 1, 9, 5, 3, 1, 2, 5, 9, 7, 6, 5, 6, 2, 5, 4, 8, 8,
        2, 8, 1, 2, 5, 2, 4, 4, 1, 4, 0, 6, 2, 5, 1, 2, 2, 0, 7, 0, 3, 1, 2,
        5, 6, 1, 0, 3, 5, 1, 5, 6, 2, 5, 3, 0, 5, 1, 7, 5, 7, 8, 1, 2, 5, 1,
        5, 2, 5, 8, 7, 8, 9, 0, 6, 2, 5, 7, 6, 2, 9, 3, 9, 4, 5, 3, 1, 2, 5,
        3, 8, 1, 4, 6, 9, 7, 2, 6, 5, 6, 2, 5, 1, 9, 0, 7, 3, 4, 8, 6, 3, 2,
        8, 1, 2, 5, 9, 5, 3, 6, 7, 4, 3, 1, 6, 4, 0, 6, 2, 5, 4, 7, 6, 8, 3,
        7, 1, 5, 8, 2, 0, 3, 1, 2, 5, 2, 3, 8, 4, 1, 8, 5, 7, 9, 1, 0, 1, 5,
        6, 2, 5, 1, 1, 9, 2, 0, 9, 2, 8, 9, 5, 5, 0, 7, 8, 1, 2, 5, 5, 9, 6,
        0, 4, 6, 4, 4, 7, 7, 5, 3, 9, 0, 6, 2, 5, 2, 9, 8, 0, 2, 3, 2, 2, 3,
        8, 7, 6, 9, 5, 3, 1, 2, 5, 1, 4, 9, 0, 1, 1, 6, 1, 1, 9, 3, 8, 4, 7,
        6, 5, 6, 2, 5, 7, 4, 5, 0, 5, 8, 0, 5, 9, 6, 9, 2, 3, 8, 2, 8, 1, 2,
        5, 3, 7, 2, 5, 2, 9, 0, 2, 9, 8, 4, 6, 1, 9, 1, 4, 0, 6, 2, 5, 1, 8,
        6, 2, 6, 4, 5, 1, 4, 9, 2, 3, 0, 9, 5, 7, 0, 3, 1, 2, 5, 9, 3, 1, 3,
        2, 2, 5, 7, 4, 6, 1, 5, 4, 7, 8, 5, 1, 5, 6, 2, 5, 4, 6, 5, 6, 6, 1,
        2, 8, 7, 3, 0, 7, 7, 3, 9, 2, 5, 7, 8, 1, 2, 5, 2, 3, 2, 8, 3, 0, 6,
        4, 3, 6, 5, 3, 8, 6, 9, 6, 2, 8, 9, 0, 6, 2, 5, 1, 1, 6, 4, 1, 5, 3,
        2, 1, 8, 2, 6, 9, 3, 4, 8, 1, 4, 4, 5, 3, 1, 2, 5, 5, 8, 2, 0, 7, 6,
        6, 0, 9, 1, 3, 4, 6, 7, 4, 0, 7, 2, 2, 6, 5, 6, 2, 5, 2, 9, 1, 0, 3,
        8, 3, 0, 4, 5, 6, 7, 3, 3, 7, 0, 3, 6, 1, 3, 2, 8, 1, 2, 5, 1, 4, 5,
        5, 1, 9, 1, 5, 2, 2, 8, 3, 6, 6, 8, 5, 1, 8, 0, 6, 6, 4, 0, 6, 2, 5,
        7, 2, 7, 5, 9, 5, 7, 6, 1, 4, 1, 8, 3, 4, 2, 5, 9, 0, 3, 3, 2, 0, 3,
        1, 2, 5, 3, 6, 3, 7, 9, 7, 8, 8, 0, 7, 0, 9, 1, 7, 1, 2, 9, 5, 1, 6,
        6, 0, 1, 5, 6, 2, 5, 1, 8, 1, 8, 9, 8, 9, 4, 0, 3, 5, 4, 5, 8, 5, 6,
        4, 7, 5, 8, 3, 0, 0, 7, 8, 1, 2, 5, 9, 0, 9, 4, 9, 4, 7, 0, 1, 7, 7,
        2, 9, 2, 8, 2, 3, 7, 9, 1, 5, 0, 3, 9, 0, 6, 2, 5, 4, 5, 4, 7, 4, 7,
        3, 5, 0, 8, 8, 6, 4, 6, 4, 1, 1, 8, 9, 5, 7, 5, 1, 9, 5, 3, 1, 2, 5,
        2, 2, 7, 3, 7, 3, 6, 7, 5, 4, 4, 3, 2, 3, 2, 0, 5, 9, 4, 7, 8, 7, 5,
        9, 7, 6, 5, 6, 2, 5, 1, 1, 3, 6, 8, 6, 8, 3, 7, 7, 2, 1, 6, 1, 6, 0,
        2, 9, 7, 3, 9, 3, 7, 9, 8, 8, 2, 8, 1, 2, 5, 5, 6, 8, 4, 3, 4, 1, 8,
        8, 6, 0, 8, 0, 8, 0, 1, 4, 8, 6, 9, 6, 8, 9, 9, 4, 1, 4, 0, 6, 2, 5,
        2, 8, 4, 2, 1, 7, 0, 9, 4, 3, 0, 4, 0, 4, 0, 0, 7, 4, 3, 4, 8, 4, 4,
        9, 7, 0, 7, 0, 3, 1, 2, 5, 1, 4, 2, 1, 0, 8, 5, 4, 7, 1, 5, 2, 0, 2,
        0, 0, 3, 7, 1, 7, 4, 2, 2, 4, 8, 5, 3, 5, 1, 5, 6, 2, 5, 7, 1, 0, 5,
        4, 2, 7, 3, 5, 7, 6, 0, 1, 0, 0, 1, 8, 5, 8, 7, 1, 1, 2, 4, 2, 6, 7,
        5, 7, 8, 1, 2, 5, 3, 5, 5, 2, 7, 1, 3, 6, 7, 8, 8, 0, 0, 5, 0, 0, 9,
        2, 9, 3, 5, 5, 6, 2, 1, 3, 3, 7, 8, 9, 0, 6, 2, 5, 1, 7, 7, 6, 3, 5,
        6, 8, 3, 9, 4, 0, 0, 2, 5, 0, 4, 6, 4, 6, 7, 7, 8, 1, 0, 6, 6, 8, 9,
        4, 5, 3, 1, 2, 5, 8, 8, 8, 1, 7, 8, 4, 1, 9, 7, 0, 0, 1, 2, 5, 2, 3,
        2, 3, 3, 8, 9, 0, 5, 3, 3, 4, 4, 7, 2, 6, 5, 6, 2, 5, 4, 4, 4, 0, 8,
        9, 2, 0, 9, 8, 5, 0, 0, 6, 2, 6, 1, 6, 1, 6, 9, 4, 5, 2, 6, 6, 7, 2,
        3, 6, 3, 2, 8, 1, 2, 5, 2, 2, 2, 0, 4, 4, 6, 0, 4, 9, 2, 5, 0, 3, 1,
        3, 0, 8, 0, 8, 4, 7, 2, 6, 3, 3, 3, 6, 1, 8, 1, 6, 4, 0, 6, 2, 5, 1,
        1, 1, 0, 2, 2, 3, 0, 2, 4, 6, 2, 5, 1, 5, 6, 5, 4, 0, 4, 2, 3, 6, 3,
        1, 6, 6, 8, 0, 9, 0, 8, 2, 0, 3, 1, 2, 5, 5, 5, 5, 1, 1, 1, 5, 1, 2,
        3, 1, 2, 5, 7, 8, 2, 7, 0, 2, 1, 1, 8, 1, 5, 8, 3, 4, 0, 4, 5, 4, 1,
        0, 1, 5, 6, 2, 5, 2, 7, 7, 5, 5, 5, 7, 5, 6, 1, 5, 6, 2, 8, 9, 1, 3,
        5, 1, 0, 5, 9, 0, 7, 9, 1, 7, 0, 2, 2, 7, 0, 5, 0, 7, 8, 1, 2, 5, 1,
        3, 8, 7, 7, 7, 8, 7, 8, 0, 7, 8, 1, 4, 4, 5, 6, 7, 5, 5, 2, 9, 5, 3,
        9, 5, 8, 5, 1, 1, 3, 5, 2, 5, 3, 9, 0, 6, 2, 5, 6, 9, 3, 8, 8, 9, 3,
        9, 0, 3, 9, 0, 7, 2, 2, 8, 3, 7, 7, 6, 4, 7, 6, 9, 7, 9, 2, 5, 5, 6,
        7, 6, 2, 6, 9, 5, 3, 1, 2, 5, 3, 4, 6, 9, 4, 4, 6, 9, 5, 1, 9, 5, 3,
        6, 1, 4, 1, 8, 8, 8, 2, 3, 8, 4, 8, 9, 6, 2, 7, 8, 3, 8, 1, 3, 4, 7,
        6, 5, 6, 2, 5, 1, 7, 3, 4, 7, 2, 3, 4, 7, 5, 9, 7, 6, 8, 0, 7, 0, 9,
        4, 4, 1, 1, 9, 2, 4, 4, 8, 1, 3, 9, 1, 9, 0, 6, 7, 3, 8, 2, 8, 1, 2,
        5, 8, 6, 7, 3, 6, 1, 7, 3, 7, 9, 8, 8, 4, 0, 3, 5, 4, 7, 2, 0, 5, 9,
        6, 2, 2, 4, 0, 6, 9, 5, 9, 5, 3, 3, 6, 9, 1, 4, 0, 6, 2, 5,
    };
    const pow5 = @ptrCast([*]const u8, &number_of_digits_decimal_left_shift_table_powers_of_5[pow5_a]);
    var i: u32 = 0;
    var n: u32 = pow5_b - pow5_a;
    while (i < n) : (i += 1) {
        if (i >= h.num_digits) {
            return num_new_digits - 1;
        } else if (h.digits[i] == pow5[i]) {
            continue;
        } else if (h.digits[i] < pow5[i]) {
            return num_new_digits - 1;
        } else {
            return num_new_digits;
        }
    }
    return num_new_digits;
}

fn round(h: *Decimal) u64 {
    if ((h.num_digits == 0) or (h.decimal_point < 0)) {
        return 0;
    } else if (h.decimal_point > 18) {
        return std.math.maxInt(u64);
    }
    // at this point, we know that h.decimal_point >= 0
    const dp: usize = @intCast(u32, h.decimal_point);
    var n: u64 = 0;
    var i: u32 = 0;

    while (i < dp) : (i += 1) {
        n = (10 * n) + if (i < h.num_digits) h.digits[i] else 0;
    }
    var round_up = false;
    if (dp < h.num_digits) {
        round_up = h.digits[dp] >= 5; // normally, we round up
        // but we may need to round to even!
        if ((h.digits[dp] == 5) and (dp + 1 == h.num_digits)) {
            round_up = h.truncated or ((dp > 0) and (1 & h.digits[dp - 1] != 0));
        }
    }
    if (round_up) {
        n += 1;
    }
    return n;
}

// computes h * 2^-shift
fn decimal_left_shift(h: *Decimal, shift: u6) void {
    if (h.num_digits == 0)
        return;

    const num_new_digits = number_of_digits_decimal_left_shift(h, shift);
    var read_index = @intCast(i32, h.num_digits - 1);
    var write_index = h.num_digits - 1 + num_new_digits;
    var n: u64 = 0;

    while (read_index >= 0) {
        n += @as(u64, h.digits[@intCast(u32, read_index)]) << shift;
        const quotient = n / 10;
        const remainder = n - (10 * quotient);
        if (write_index < max_digits) {
            h.digits[write_index] = @intCast(u8, remainder);
        } else if (remainder > 0) {
            h.truncated = true;
        }
        n = quotient;
        write_index -%= 1;
        read_index -= 1;
    }
    while (n > 0) {
        const quotient = n / 10;
        const remainder = n - (10 * quotient);
        if (write_index < max_digits) {
            h.digits[write_index] = @intCast(u8, remainder);
        } else if (remainder > 0) {
            h.truncated = true;
        }
        n = quotient;
        write_index -%= 1;
    }
    h.num_digits += num_new_digits;
    if (h.num_digits > max_digits) {
        h.num_digits = max_digits;
    }
    h.decimal_point += @intCast(i32, num_new_digits);
    trim(h);
}

fn compute_float(d: *Decimal) AdjustedMantissa {
    var answer: AdjustedMantissa = undefined;
    if (d.num_digits == 0) {
        // should be zero
        answer.power2 = 0;
        answer.mantissa = 0;
        return answer;
    }
    // At this point, going further, we can assume that d.num_digits > 0.
    // We want to guard against excessive decimal point values because
    // they can result in long running times. Indeed, we do
    // shifts by at most 60 bits. We have that log(10**400)/log(2**60) ~= 22
    // which is fine, but log(10**299995)/log(2**60) ~= 16609 which is not
    // fine (runs for a long time).
    //
    if (d.decimal_point < -324) {
        // We have something smaller than 1e-324 which is always zero
        // in binary64 and binary32.
        // It should be zero.
        answer.power2 = 0;
        answer.mantissa = 0;
        return answer;
    } else if (d.decimal_point >= 310) {
        // We have something at least as large as 0.1e310 which is
        // always infinite.
        answer.power2 = BinaryFormat.infinite_power;
        answer.mantissa = 0;
        return answer;
    }

    const max_shift = 60;
    const num_powers = 19;
    const powers: [19]u6 = .{
        0, 3, 6, 9, 13, 16, 19, 23, 26, 29, //
        33, 36, 39, 43, 46, 49, 53, 56, 59, //
    };
    var exp2: i32 = 0;
    while (d.decimal_point > 0) {
        const n = @intCast(u32, d.decimal_point);
        const shift = if (n < num_powers) powers[n] else max_shift;
        decimal_right_shift(d, @intCast(u6, shift));
        if (d.decimal_point < -decimal_point_range) {
            // should be zero
            answer.power2 = 0;
            answer.mantissa = 0;
            return answer;
        }
        exp2 += @as(i32, shift);
    }
    // We shift left toward [1/2 ... 1].
    while (d.decimal_point <= 0) {
        var shift: u6 = undefined;
        if (d.decimal_point == 0) {
            if (d.digits[0] >= 5) {
                break;
            }
            shift = if (d.digits[0] < 2) 2 else 1;
        } else {
            var n = @intCast(u32, -d.decimal_point);
            shift = if (n < num_powers) powers[n] else max_shift;
        }
        decimal_left_shift(d, shift);
        if (d.decimal_point > decimal_point_range) {
            // we want to get infinity:
            answer.power2 = 0xFF;
            answer.mantissa = 0;
            return answer;
        }
        exp2 -= @intCast(i32, shift);
    }
    // We are now in the range [1/2 ... 1] but the binary format uses [1 ... 2].
    exp2 -= 1;
    var minimum_exponent: i32 = BinaryFormat.minimum_exponent;
    while ((minimum_exponent + 1) > exp2) {
        var n = @intCast(u6, (minimum_exponent + 1) - exp2);
        if (n > max_shift) {
            n = max_shift;
        }
        decimal_right_shift(d, n);
        exp2 += @as(i32, n);
    }
    if ((exp2 - minimum_exponent) >= BinaryFormat.infinite_power) {
        answer.power2 = BinaryFormat.infinite_power;
        answer.mantissa = 0;
        return answer;
    }

    const mantissa_size_in_bits = BinaryFormat.mantissa_explicit_bits + 1;
    decimal_left_shift(d, mantissa_size_in_bits);

    var mantissa = round(d);
    // It is possible that we have an overflow, in which case we need
    // to shift back.
    if (mantissa >= (@as(u64, 1) << mantissa_size_in_bits)) {
        decimal_right_shift(d, 1);
        exp2 += 1;
        mantissa = round(d);
        if ((exp2 - minimum_exponent) >= BinaryFormat.infinite_power) {
            answer.power2 = BinaryFormat.infinite_power;
            answer.mantissa = 0;
            return answer;
        }
    }
    answer.power2 = exp2 - BinaryFormat.minimum_exponent;
    if (mantissa < (@as(u64, 1) << BinaryFormat.mantissa_explicit_bits)) {
        answer.power2 -= 1;
    }
    answer.mantissa =
        mantissa & ((@as(u64, 1) << BinaryFormat.mantissa_explicit_bits) - 1);
    return answer;
}

fn parse_long_mantissa(comptime F: type, first: [*]const u8) AdjustedMantissa {
    _ = F;
    var d = parse_decimal2(first);
    return compute_float(&d);
}

const BinaryFormat = struct {
    const mantissa_explicit_bits = 52;
    const minimum_exponent = -1023;
    const infinite_power = 0x7FF;
    const sign_index = 63;
};

fn from_chars(first_: [*]const u8) f64 {
    var first = first_;
    const negative = first[0] == '-';
    if (negative) {
        first += 1;
    }
    const am = parse_long_mantissa(f64, first);
    var word = am.mantissa;
    word |= @intCast(u64, @as(i64, am.power2)) << BinaryFormat.mantissa_explicit_bits;
    word = if (negative) word | (@as(u64, 1) << BinaryFormat.sign_index) else word;
    var value: f64 = undefined;
    //   std::memcpy(&value, &word, sizeof(double));
    @memcpy(@ptrCast([*]u8, &value)[0..@sizeOf(f64)], @ptrCast([*]const u8, &word)[0..@sizeOf(f64)]);
    return value;
}

// We call a fallback floating-point parser that might be slow. Note
// it will accept JSON numbers, but the JSON spec. is more restrictive so
// before you call parse_float_fallback, you need to have validated the input
// string with the JSON grammar.
// It will return an error (false) if the parsed number is infinite.
// The string parsing itself always succeeds. We know that there is at least
// one digit.
fn parse_float_fallback(ptr: [*]const u8, outDouble: *f64) bool {
    // @setFloatMode(.Optimized);
    outDouble.* = from_chars(ptr);
    // We do not accept infinite values.

    // Detecting finite values in a portable manner is ridiculously hard, ideally
    // we would want to do:
    // return !std::isfinite(*outDouble);
    // but that mysteriously fails under legacy/old libc++ libraries, see
    // https://github.com/simdjson/simdjson/issues/1286
    //
    // Therefore, fall back to this solution (the extra parens are there
    // to handle that max may be a macro on windows).
    // println("parse_float_fallback {} std.math.f64_min {} std.math.f64_max {} {} {}", .{ outDouble.*, std.math.f64_min, std.math.f64_max, std.math.f64_min < outDouble.*, outDouble.* < std.math.f64_max });
    return !(outDouble.* > std.math.f64_max or outDouble.* < -std.math.f64_max);
}

fn write_float(
    src: [*]const u8,
    negative: bool,
    i: u64,
    start_digits: [*]const u8,
    digit_count: usize,
    exponent: i64,
    writer: *TapeBuilder,
) !void {
    // If we frequently had to deal with long strings of digits,
    // we could extend our code by using a 128-bit integer instead
    // of a 64-bit integer. However, this is uncommon in practice.
    //
    // 9999999999999999999 < 2**64 so we can accommodate 19 digits.
    // If we have a decimal separator, then digit_count - 1 is the number of digits, but we
    // may not have a decimal separator!
    if (digit_count > 19 and significant_digits(start_digits, digit_count) > 19) {
        // Ok, chances are good that we had an overflow!
        // this is almost never going to get called!!!
        // we start anew, going slowly!!!
        // This will happen in the following examples:
        // 10000000000000000000000000000000000000000000e+308
        // 3.1415926535897932384626433832795028841971693993751
        //
        // NOTE: This makes a *copy* of the writer and passes it to slow_float_parsing. This happens
        // because slow_float_parsing is a non-inlined function. If we passed our writer reference to
        // it, it would force it to be stored in memory, preventing the compiler from picking it apart
        // and putting into registers. i.e. if we pass it as reference, it gets slow.
        // This is what forces the skip_double, as well.
        return slow_float_parsing(src, writer);
        // TLS: don't need to call skip_double(). maybe in the future i'll discover that it would be
        // faster to pass a copy somewhere and decide to put it back.
    }
    // NOTE: it's weird that the simdjson_unlikely() only wraps half the if, but it seems to get slower any other
    // way we've tried: https://github.com/simdjson/simdjson/pull/990#discussion_r448497331
    // To future reader: we'd love if someone found a better way, or at least could explain this result!
    if (exponent < smallest_power or exponent > largest_power) {
        //
        // Important: smallest_power is such that it leads to a zero value.
        // Observe that 18446744073709551615e-343 == 0, i.e. (2**64 - 1) e -343 is zero
        // so something x 10^-343 goes to zero, but not so with  something x 10^-342.
        if (smallest_power > -342) @compileError("smallest_power is not small enough");
        //
        if (exponent < smallest_power or i == 0) {
            WRITE_DOUBLE(0, src, writer);
            return;
        } else { // (exponent > largest_power) and (i != 0)
            // We have, for sure, an infinite value and simdjson refuses to parse infinite values.
            return INVALID_NUMBER(src);
        }
    }
    // std.log.debug("exponent {}, i {}, negative {}", .{ exponent, i, negative });

    var d: f64 = undefined;
    if (!compute_float_64(exponent, i, negative, &d)) {
        // we are almost never going to get here.

        if (!parse_float_fallback(src, &d)) {
            return INVALID_NUMBER(src);
        }
    }
    WRITE_DOUBLE(d, src, writer);
}

const smallest_power = -342;
const largest_power = 308;

const power_of_ten = [_]f64{
    1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,  1e8,  1e9,  1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
};

fn to_double(_mantissa: u64, real_exponent: u64, negative: u1) f64 {
    var d: f64 = undefined;
    var mantissa = _mantissa;
    mantissa &= ~(@as(u64, 1) << 52);
    mantissa |= real_exponent << 52;
    mantissa |= (@as(u64, negative) << 63);
    // std::memcpy(&d, &mantissa, sizeof(d));
    @memcpy(@ptrCast([*]u8, &d)[0..@sizeOf(f64)], @ptrCast([*]const u8, &mantissa)[0..@sizeOf(f64)]);
    return d;
}

inline fn compute_float_64(power: i64, _i: u64, negative: bool, d: *f64) bool {
    var i = _i;
    // we start with a fast path
    // It was described in
    // Clinger WD. How to read floating point numbers accurately.
    // ACM SIGPLAN Notices. 1990
    // TODO
    // #ifndef FLT_EVAL_METHOD
    // #error "FLT_EVAL_METHOD should be defined, please include cfloat."
    // #endif
    // #if (FLT_EVAL_METHOD != 1) && (FLT_EVAL_METHOD != 0)
    //   // We cannot be certain that x/y is rounded to nearest.
    if (0 <= power and power <= 22 and i <= 9007199254740991) {
        // #else
        //   if (-22 <= power and power <= 22 and i <= 9007199254740991) {
        // #endif
        // convert the integer into a double. This is lossless since
        // 0 <= i <= 2^53 - 1.
        // d = double(i);
        d.* = @intToFloat(f64, i);
        //
        // The general idea is as follows.
        // If 0 <= s < 2^53 and if 10^0 <= p <= 10^22 then
        // 1) Both s and p can be represented exactly as 64-bit floating-point
        // values
        // (binary64).
        // 2) Because s and p can be represented exactly as floating-point values,
        // then s * p
        // and s / p will produce correctly rounded values.
        //
        if (power < 0) {
            d.* /= power_of_ten[@intCast(usize, -power)];
        } else {
            d.* *= power_of_ten[@intCast(usize, power)];
        }
        if (negative) {
            d.* = -d.*;
        }
        return true;
    }
    // When 22 < power && power <  22 + 16, we could
    // hope for another, secondary fast path.  It was
    // described by David M. Gay in  "Correctly rounded
    // binary-decimal and decimal-binary conversions." (1990)
    // If you need to compute i * 10^(22 + x) for x < 16,
    // first compute i * 10^x, if you know that result is exact
    // (e.g., when i * 10^x < 2^53),
    // then you can still proceed and do (i * 10^x) * 10^22.
    // Is this worth your time?
    // You need  22 < power *and* power <  22 + 16 *and* (i * 10^(x-22) < 2^53)
    // for this second fast path to work.
    // If you you have 22 < power *and* power <  22 + 16, and then you
    // optimistically compute "i * 10^(x-22)", there is still a chance that you
    // have wasted your time if i * 10^(x-22) >= 2^53. It makes the use cases of
    // this optimization maybe less common than we would like. Source:
    // http://www.exploringbinary.com/fast-path-decimal-to-floating-point-conversion/
    // also used in RapidJSON: https://rapidjson.org/strtod_8h_source.html

    // The fast path has now failed, so we are failing back on the slower path.

    // In the slow path, we need to adjust i so that it is > 1<<63 which is always
    // possible, except if i == 0, so we handle i == 0 separately.
    if (i == 0) {
        d.* = 0.0;
        return true;
    }

    // The exponent is 1024 + 63 + power
    //     + floor(log(5**power)/log(2)).
    // The 1024 comes from the ieee64 standard.
    // The 63 comes from the fact that we use a 64-bit word.
    //
    // Computing floor(log(5**power)/log(2)) could be
    // slow. Instead we use a fast function.
    //
    // For power in (-400,350), we have that
    // (((152170 + 65536) * power ) >> 16);
    // is equal to
    //  floor(log(5**power)/log(2)) + power when power >= 0
    // and it is equal to
    //  ceil(log(5**-power)/log(2)) + power when power < 0
    //
    // The 65536 is (1<<16) and corresponds to
    // (65536 * power) >> 16 ---> power
    //
    // ((152170 * power ) >> 16) is equal to
    // floor(log(5**power)/log(2))
    //
    // Note that this is not magic: 152170/(1<<16) is
    // approximatively equal to log(5)/log(2).
    // The 1<<16 value is a power of two; we could use a
    // larger power of 2 if we wanted to.
    //
    const exponent: i64 = (((152170 + 65536) * power) >> 16) + 1024 + 63;

    // We want the most significant bit of i to be 1. Shift if needed.
    var lz = @intCast(u7, @clz(i));
    i <<= @intCast(u6, lz);

    // We are going to need to do some 64-bit arithmetic to get a precise product.
    // We use a table lookup approach.
    // It is safe because
    // power >= smallest_power
    // and power <= largest_power
    // We recover the mantissa of the power, it has a leading 1. It is always
    // rounded down.
    //
    // We want the most significant 64 bits of the product. We know
    // this will be non-zero because the most significant bit of i is
    // 1.
    const index: u32 = 2 * @intCast(u32, power - smallest_power);
    // Optimization: It may be that materializing the index as a variable might confuse some compilers and prevent effective complex-addressing loads. (Done for code clarity.)
    //
    // The full_multiplication function computes the 128-bit product of two 64-bit words
    // with a returned value of type value128 with a "low component" corresponding to the
    // 64-bit least significant bits of the product and with a "high component" corresponding
    // to the 64-bit most significant bits of the product.
    var firstproduct = CharUtils.full_multiplication(i, power_of_five_128[index]);
    // Both i and power_of_five_128[index] have their most significant bit set to 1 which
    // implies that the either the most or the second most significant bit of the product
    // is 1. We pack values in this manner for efficiency reasons: it maximizes the use
    // we make of the product. It also makes it easy to reason about the product: there
    // is 0 or 1 leading zero in the product.

    // Unless the least significant 9 bits of the high (64-bit) part of the full
    // product are all 1s, then we know that the most significant 55 bits are
    // exact and no further work is needed. Having 55 bits is necessary because
    // we need 53 bits for the mantissa but we have to have one rounding bit and
    // we can waste a bit if the most significant bit of the product is zero.
    if ((firstproduct.high & 0x1FF) == 0x1FF) {
        // We want to compute i * 5^q, but only care about the top 55 bits at most.
        // Consider the scenario where q>=0. Then 5^q may not fit in 64-bits. Doing
        // the full computation is wasteful. So we do what is called a "truncated
        // multiplication".
        // We take the most significant 64-bits, and we put them in
        // power_of_five_128[index]. Usually, that's good enough to approximate i * 5^q
        // to the desired approximation using one multiplication. Sometimes it does not suffice.
        // Then we store the next most significant 64 bits in power_of_five_128[index + 1], and
        // then we get a better approximation to i * 5^q. In very rare cases, even that
        // will not suffice, though it is seemingly very hard to find such a scenario.
        //
        // That's for when q>=0. The logic for q<0 is somewhat similar but it is somewhat
        // more complicated.
        //
        // There is an extra layer of complexity in that we need more than 55 bits of
        // accuracy in the round-to-even scenario.
        //
        // The full_multiplication function computes the 128-bit product of two 64-bit words
        // with a returned value of type value128 with a "low component" corresponding to the
        // 64-bit least significant bits of the product and with a "high component" corresponding
        // to the 64-bit most significant bits of the product.
        var secondproduct = CharUtils.full_multiplication(i, power_of_five_128[index + 1]);
        firstproduct.low +%= secondproduct.high;
        if (secondproduct.high > firstproduct.low) {
            firstproduct.high += 1;
        }
        // At this point, we might need to add at most one to firstproduct, but this
        // can only change the value of firstproduct.high if firstproduct.low is maximal.
        if (firstproduct.low == 0xFFFFFFFFFFFFFFFF) {
            // This is very unlikely, but if so, we need to do much more work!
            return false;
        }
    }
    const lower: u64 = firstproduct.low;
    const upper: u64 = firstproduct.high;
    // The final mantissa should be 53 bits with a leading 1.
    // We shift it so that it occupies 54 bits with a leading 1.
    ///////
    const upperbit = @intCast(u6, upper >> 63);
    var mantissa: u64 = upper >> (upperbit + 9);
    lz +%= 1 ^ upperbit;

    // Here we have mantissa < (1<<54).
    var real_exponent: i64 = exponent - lz;
    if (real_exponent <= 0) { // we have a subnormal?
        // Here have that real_exponent <= 0 so -real_exponent >= 0
        if (-real_exponent + 1 >= 64) { // if we have more than 64 bits below the minimum exponent, you have a zero for sure.
            d.* = 0.0;
            return true;
        }
        // next line is safe because -real_exponent + 1 < 0
        mantissa >>= @bitCast(u6, @intCast(i6, -real_exponent + 1));
        // Thankfully, we can't have both "round-to-even" and subnormals because
        // "round-to-even" only occurs for powers close to 0.
        mantissa += (mantissa & 1); // round up
        mantissa >>= 1;
        // There is a weird scenario where we don't have a subnormal but just.
        // Suppose we start with 2.2250738585072013e-308, we end up
        // with 0x3fffffffffffff x 2^-1023-53 which is technically subnormal
        // whereas 0x40000000000000 x 2^-1023-53  is normal. Now, we need to round
        // up 0x3fffffffffffff x 2^-1023-53  and once we do, we are no longer
        // subnormal, but we can only know this after rounding.
        // So we only declare a subnormal if we are smaller than the threshold.
        real_exponent = if (mantissa < (@as(u64, 1) << 52)) 0 else 1;
        d.* = to_double(mantissa, @bitCast(u64, real_exponent), @boolToInt(negative));
        return true;
    }
    // We have to round to even. The "to even" part
    // is only a problem when we are right in between two floats
    // which we guard against.
    // If we have lots of trailing zeros, we may fall right between two
    // floating-point values.
    //
    // The round-to-even cases take the form of a number 2m+1 which is in (2^53,2^54]
    // times a power of two. That is, it is right between a number with binary significand
    // m and another number with binary significand m+1; and it must be the case
    // that it cannot be represented by a float itself.
    //
    // We must have that w * 10 ^q == (2m+1) * 2^p for some power of two 2^p.
    // Recall that 10^q = 5^q * 2^q.
    // When q >= 0, we must have that (2m+1) is divible by 5^q, so 5^q <= 2^54. We have that
    //  5^23 <=  2^54 and it is the last power of five to qualify, so q <= 23.
    // When q<0, we have  w  >=  (2m+1) x 5^{-q}.  We must have that w<2^{64} so
    // (2m+1) x 5^{-q} < 2^{64}. We have that 2m+1>2^{53}. Hence, we must have
    // 2^{53} x 5^{-q} < 2^{64}.
    // Hence we have 5^{-q} < 2^{11}$ or q>= -4.
    //
    // We require lower <= 1 and not lower == 0 because we could not prove that
    // that lower == 0 is implied; but we could prove that lower <= 1 is a necessary and sufficient test.
    if ((lower <= 1) and (power >= -4) and (power <= 23) and ((mantissa & 3) == 1)) {
        if ((mantissa << @intCast(u6, upperbit + (64 - 53 - 2))) == upper) {
            mantissa &= ~@as(u64, 1); // flip it so that we do not round up
        }
    }

    mantissa += mantissa & 1;
    mantissa >>= 1;

    // Here we have mantissa < (1<<53), unless there was an overflow
    if (mantissa >= (1 << 53)) {
        //////////
        // This will happen when parsing values such as 7.2057594037927933e+16
        ////////
        mantissa = (1 << 52);
        real_exponent += 1;
    }
    mantissa &= ~@as(u64, 1 << 52);
    // we have to check that real_exponent is in range, otherwise we bail out
    if (real_exponent > 2046) {
        // We have an infinite value!!! We could actually throw an error here if we could.
        return false;
    }
    d.* = to_double(mantissa, @bitCast(u64, real_exponent), @boolToInt(negative));
    return true;
}

test "shr neg i32" {
    var x: u64 = 0xffffffffffffffff;
    x >>= @bitCast(u6, @as(i6, -3));
    try std.testing.expectEqual(@as(u64, 7), x);
}

pub const power_of_five_128 = [_]u64{
    0xeef453d6923bd65a, 0x113faa2906a13b3f,
    0x9558b4661b6565f8, 0x4ac7ca59a424c507,
    0xbaaee17fa23ebf76, 0x5d79bcf00d2df649,
    0xe95a99df8ace6f53, 0xf4d82c2c107973dc,
    0x91d8a02bb6c10594, 0x79071b9b8a4be869,
    0xb64ec836a47146f9, 0x9748e2826cdee284,
    0xe3e27a444d8d98b7, 0xfd1b1b2308169b25,
    0x8e6d8c6ab0787f72, 0xfe30f0f5e50e20f7,
    0xb208ef855c969f4f, 0xbdbd2d335e51a935,
    0xde8b2b66b3bc4723, 0xad2c788035e61382,
    0x8b16fb203055ac76, 0x4c3bcb5021afcc31,
    0xaddcb9e83c6b1793, 0xdf4abe242a1bbf3d,
    0xd953e8624b85dd78, 0xd71d6dad34a2af0d,
    0x87d4713d6f33aa6b, 0x8672648c40e5ad68,
    0xa9c98d8ccb009506, 0x680efdaf511f18c2,
    0xd43bf0effdc0ba48, 0x212bd1b2566def2,
    0x84a57695fe98746d, 0x14bb630f7604b57,
    0xa5ced43b7e3e9188, 0x419ea3bd35385e2d,
    0xcf42894a5dce35ea, 0x52064cac828675b9,
    0x818995ce7aa0e1b2, 0x7343efebd1940993,
    0xa1ebfb4219491a1f, 0x1014ebe6c5f90bf8,
    0xca66fa129f9b60a6, 0xd41a26e077774ef6,
    0xfd00b897478238d0, 0x8920b098955522b4,
    0x9e20735e8cb16382, 0x55b46e5f5d5535b0,
    0xc5a890362fddbc62, 0xeb2189f734aa831d,
    0xf712b443bbd52b7b, 0xa5e9ec7501d523e4,
    0x9a6bb0aa55653b2d, 0x47b233c92125366e,
    0xc1069cd4eabe89f8, 0x999ec0bb696e840a,
    0xf148440a256e2c76, 0xc00670ea43ca250d,
    0x96cd2a865764dbca, 0x380406926a5e5728,
    0xbc807527ed3e12bc, 0xc605083704f5ecf2,
    0xeba09271e88d976b, 0xf7864a44c633682e,
    0x93445b8731587ea3, 0x7ab3ee6afbe0211d,
    0xb8157268fdae9e4c, 0x5960ea05bad82964,
    0xe61acf033d1a45df, 0x6fb92487298e33bd,
    0x8fd0c16206306bab, 0xa5d3b6d479f8e056,
    0xb3c4f1ba87bc8696, 0x8f48a4899877186c,
    0xe0b62e2929aba83c, 0x331acdabfe94de87,
    0x8c71dcd9ba0b4925, 0x9ff0c08b7f1d0b14,
    0xaf8e5410288e1b6f, 0x7ecf0ae5ee44dd9,
    0xdb71e91432b1a24a, 0xc9e82cd9f69d6150,
    0x892731ac9faf056e, 0xbe311c083a225cd2,
    0xab70fe17c79ac6ca, 0x6dbd630a48aaf406,
    0xd64d3d9db981787d, 0x92cbbccdad5b108,
    0x85f0468293f0eb4e, 0x25bbf56008c58ea5,
    0xa76c582338ed2621, 0xaf2af2b80af6f24e,
    0xd1476e2c07286faa, 0x1af5af660db4aee1,
    0x82cca4db847945ca, 0x50d98d9fc890ed4d,
    0xa37fce126597973c, 0xe50ff107bab528a0,
    0xcc5fc196fefd7d0c, 0x1e53ed49a96272c8,
    0xff77b1fcbebcdc4f, 0x25e8e89c13bb0f7a,
    0x9faacf3df73609b1, 0x77b191618c54e9ac,
    0xc795830d75038c1d, 0xd59df5b9ef6a2417,
    0xf97ae3d0d2446f25, 0x4b0573286b44ad1d,
    0x9becce62836ac577, 0x4ee367f9430aec32,
    0xc2e801fb244576d5, 0x229c41f793cda73f,
    0xf3a20279ed56d48a, 0x6b43527578c1110f,
    0x9845418c345644d6, 0x830a13896b78aaa9,
    0xbe5691ef416bd60c, 0x23cc986bc656d553,
    0xedec366b11c6cb8f, 0x2cbfbe86b7ec8aa8,
    0x94b3a202eb1c3f39, 0x7bf7d71432f3d6a9,
    0xb9e08a83a5e34f07, 0xdaf5ccd93fb0cc53,
    0xe858ad248f5c22c9, 0xd1b3400f8f9cff68,
    0x91376c36d99995be, 0x23100809b9c21fa1,
    0xb58547448ffffb2d, 0xabd40a0c2832a78a,
    0xe2e69915b3fff9f9, 0x16c90c8f323f516c,
    0x8dd01fad907ffc3b, 0xae3da7d97f6792e3,
    0xb1442798f49ffb4a, 0x99cd11cfdf41779c,
    0xdd95317f31c7fa1d, 0x40405643d711d583,
    0x8a7d3eef7f1cfc52, 0x482835ea666b2572,
    0xad1c8eab5ee43b66, 0xda3243650005eecf,
    0xd863b256369d4a40, 0x90bed43e40076a82,
    0x873e4f75e2224e68, 0x5a7744a6e804a291,
    0xa90de3535aaae202, 0x711515d0a205cb36,
    0xd3515c2831559a83, 0xd5a5b44ca873e03,
    0x8412d9991ed58091, 0xe858790afe9486c2,
    0xa5178fff668ae0b6, 0x626e974dbe39a872,
    0xce5d73ff402d98e3, 0xfb0a3d212dc8128f,
    0x80fa687f881c7f8e, 0x7ce66634bc9d0b99,
    0xa139029f6a239f72, 0x1c1fffc1ebc44e80,
    0xc987434744ac874e, 0xa327ffb266b56220,
    0xfbe9141915d7a922, 0x4bf1ff9f0062baa8,
    0x9d71ac8fada6c9b5, 0x6f773fc3603db4a9,
    0xc4ce17b399107c22, 0xcb550fb4384d21d3,
    0xf6019da07f549b2b, 0x7e2a53a146606a48,
    0x99c102844f94e0fb, 0x2eda7444cbfc426d,
    0xc0314325637a1939, 0xfa911155fefb5308,
    0xf03d93eebc589f88, 0x793555ab7eba27ca,
    0x96267c7535b763b5, 0x4bc1558b2f3458de,
    0xbbb01b9283253ca2, 0x9eb1aaedfb016f16,
    0xea9c227723ee8bcb, 0x465e15a979c1cadc,
    0x92a1958a7675175f, 0xbfacd89ec191ec9,
    0xb749faed14125d36, 0xcef980ec671f667b,
    0xe51c79a85916f484, 0x82b7e12780e7401a,
    0x8f31cc0937ae58d2, 0xd1b2ecb8b0908810,
    0xb2fe3f0b8599ef07, 0x861fa7e6dcb4aa15,
    0xdfbdcece67006ac9, 0x67a791e093e1d49a,
    0x8bd6a141006042bd, 0xe0c8bb2c5c6d24e0,
    0xaecc49914078536d, 0x58fae9f773886e18,
    0xda7f5bf590966848, 0xaf39a475506a899e,
    0x888f99797a5e012d, 0x6d8406c952429603,
    0xaab37fd7d8f58178, 0xc8e5087ba6d33b83,
    0xd5605fcdcf32e1d6, 0xfb1e4a9a90880a64,
    0x855c3be0a17fcd26, 0x5cf2eea09a55067f,
    0xa6b34ad8c9dfc06f, 0xf42faa48c0ea481e,
    0xd0601d8efc57b08b, 0xf13b94daf124da26,
    0x823c12795db6ce57, 0x76c53d08d6b70858,
    0xa2cb1717b52481ed, 0x54768c4b0c64ca6e,
    0xcb7ddcdda26da268, 0xa9942f5dcf7dfd09,
    0xfe5d54150b090b02, 0xd3f93b35435d7c4c,
    0x9efa548d26e5a6e1, 0xc47bc5014a1a6daf,
    0xc6b8e9b0709f109a, 0x359ab6419ca1091b,
    0xf867241c8cc6d4c0, 0xc30163d203c94b62,
    0x9b407691d7fc44f8, 0x79e0de63425dcf1d,
    0xc21094364dfb5636, 0x985915fc12f542e4,
    0xf294b943e17a2bc4, 0x3e6f5b7b17b2939d,
    0x979cf3ca6cec5b5a, 0xa705992ceecf9c42,
    0xbd8430bd08277231, 0x50c6ff782a838353,
    0xece53cec4a314ebd, 0xa4f8bf5635246428,
    0x940f4613ae5ed136, 0x871b7795e136be99,
    0xb913179899f68584, 0x28e2557b59846e3f,
    0xe757dd7ec07426e5, 0x331aeada2fe589cf,
    0x9096ea6f3848984f, 0x3ff0d2c85def7621,
    0xb4bca50b065abe63, 0xfed077a756b53a9,
    0xe1ebce4dc7f16dfb, 0xd3e8495912c62894,
    0x8d3360f09cf6e4bd, 0x64712dd7abbbd95c,
    0xb080392cc4349dec, 0xbd8d794d96aacfb3,
    0xdca04777f541c567, 0xecf0d7a0fc5583a0,
    0x89e42caaf9491b60, 0xf41686c49db57244,
    0xac5d37d5b79b6239, 0x311c2875c522ced5,
    0xd77485cb25823ac7, 0x7d633293366b828b,
    0x86a8d39ef77164bc, 0xae5dff9c02033197,
    0xa8530886b54dbdeb, 0xd9f57f830283fdfc,
    0xd267caa862a12d66, 0xd072df63c324fd7b,
    0x8380dea93da4bc60, 0x4247cb9e59f71e6d,
    0xa46116538d0deb78, 0x52d9be85f074e608,
    0xcd795be870516656, 0x67902e276c921f8b,
    0x806bd9714632dff6, 0xba1cd8a3db53b6,
    0xa086cfcd97bf97f3, 0x80e8a40eccd228a4,
    0xc8a883c0fdaf7df0, 0x6122cd128006b2cd,
    0xfad2a4b13d1b5d6c, 0x796b805720085f81,
    0x9cc3a6eec6311a63, 0xcbe3303674053bb0,
    0xc3f490aa77bd60fc, 0xbedbfc4411068a9c,
    0xf4f1b4d515acb93b, 0xee92fb5515482d44,
    0x991711052d8bf3c5, 0x751bdd152d4d1c4a,
    0xbf5cd54678eef0b6, 0xd262d45a78a0635d,
    0xef340a98172aace4, 0x86fb897116c87c34,
    0x9580869f0e7aac0e, 0xd45d35e6ae3d4da0,
    0xbae0a846d2195712, 0x8974836059cca109,
    0xe998d258869facd7, 0x2bd1a438703fc94b,
    0x91ff83775423cc06, 0x7b6306a34627ddcf,
    0xb67f6455292cbf08, 0x1a3bc84c17b1d542,
    0xe41f3d6a7377eeca, 0x20caba5f1d9e4a93,
    0x8e938662882af53e, 0x547eb47b7282ee9c,
    0xb23867fb2a35b28d, 0xe99e619a4f23aa43,
    0xdec681f9f4c31f31, 0x6405fa00e2ec94d4,
    0x8b3c113c38f9f37e, 0xde83bc408dd3dd04,
    0xae0b158b4738705e, 0x9624ab50b148d445,
    0xd98ddaee19068c76, 0x3badd624dd9b0957,
    0x87f8a8d4cfa417c9, 0xe54ca5d70a80e5d6,
    0xa9f6d30a038d1dbc, 0x5e9fcf4ccd211f4c,
    0xd47487cc8470652b, 0x7647c3200069671f,
    0x84c8d4dfd2c63f3b, 0x29ecd9f40041e073,
    0xa5fb0a17c777cf09, 0xf468107100525890,
    0xcf79cc9db955c2cc, 0x7182148d4066eeb4,
    0x81ac1fe293d599bf, 0xc6f14cd848405530,
    0xa21727db38cb002f, 0xb8ada00e5a506a7c,
    0xca9cf1d206fdc03b, 0xa6d90811f0e4851c,
    0xfd442e4688bd304a, 0x908f4a166d1da663,
    0x9e4a9cec15763e2e, 0x9a598e4e043287fe,
    0xc5dd44271ad3cdba, 0x40eff1e1853f29fd,
    0xf7549530e188c128, 0xd12bee59e68ef47c,
    0x9a94dd3e8cf578b9, 0x82bb74f8301958ce,
    0xc13a148e3032d6e7, 0xe36a52363c1faf01,
    0xf18899b1bc3f8ca1, 0xdc44e6c3cb279ac1,
    0x96f5600f15a7b7e5, 0x29ab103a5ef8c0b9,
    0xbcb2b812db11a5de, 0x7415d448f6b6f0e7,
    0xebdf661791d60f56, 0x111b495b3464ad21,
    0x936b9fcebb25c995, 0xcab10dd900beec34,
    0xb84687c269ef3bfb, 0x3d5d514f40eea742,
    0xe65829b3046b0afa, 0xcb4a5a3112a5112,
    0x8ff71a0fe2c2e6dc, 0x47f0e785eaba72ab,
    0xb3f4e093db73a093, 0x59ed216765690f56,
    0xe0f218b8d25088b8, 0x306869c13ec3532c,
    0x8c974f7383725573, 0x1e414218c73a13fb,
    0xafbd2350644eeacf, 0xe5d1929ef90898fa,
    0xdbac6c247d62a583, 0xdf45f746b74abf39,
    0x894bc396ce5da772, 0x6b8bba8c328eb783,
    0xab9eb47c81f5114f, 0x66ea92f3f326564,
    0xd686619ba27255a2, 0xc80a537b0efefebd,
    0x8613fd0145877585, 0xbd06742ce95f5f36,
    0xa798fc4196e952e7, 0x2c48113823b73704,
    0xd17f3b51fca3a7a0, 0xf75a15862ca504c5,
    0x82ef85133de648c4, 0x9a984d73dbe722fb,
    0xa3ab66580d5fdaf5, 0xc13e60d0d2e0ebba,
    0xcc963fee10b7d1b3, 0x318df905079926a8,
    0xffbbcfe994e5c61f, 0xfdf17746497f7052,
    0x9fd561f1fd0f9bd3, 0xfeb6ea8bedefa633,
    0xc7caba6e7c5382c8, 0xfe64a52ee96b8fc0,
    0xf9bd690a1b68637b, 0x3dfdce7aa3c673b0,
    0x9c1661a651213e2d, 0x6bea10ca65c084e,
    0xc31bfa0fe5698db8, 0x486e494fcff30a62,
    0xf3e2f893dec3f126, 0x5a89dba3c3efccfa,
    0x986ddb5c6b3a76b7, 0xf89629465a75e01c,
    0xbe89523386091465, 0xf6bbb397f1135823,
    0xee2ba6c0678b597f, 0x746aa07ded582e2c,
    0x94db483840b717ef, 0xa8c2a44eb4571cdc,
    0xba121a4650e4ddeb, 0x92f34d62616ce413,
    0xe896a0d7e51e1566, 0x77b020baf9c81d17,
    0x915e2486ef32cd60, 0xace1474dc1d122e,
    0xb5b5ada8aaff80b8, 0xd819992132456ba,
    0xe3231912d5bf60e6, 0x10e1fff697ed6c69,
    0x8df5efabc5979c8f, 0xca8d3ffa1ef463c1,
    0xb1736b96b6fd83b3, 0xbd308ff8a6b17cb2,
    0xddd0467c64bce4a0, 0xac7cb3f6d05ddbde,
    0x8aa22c0dbef60ee4, 0x6bcdf07a423aa96b,
    0xad4ab7112eb3929d, 0x86c16c98d2c953c6,
    0xd89d64d57a607744, 0xe871c7bf077ba8b7,
    0x87625f056c7c4a8b, 0x11471cd764ad4972,
    0xa93af6c6c79b5d2d, 0xd598e40d3dd89bcf,
    0xd389b47879823479, 0x4aff1d108d4ec2c3,
    0x843610cb4bf160cb, 0xcedf722a585139ba,
    0xa54394fe1eedb8fe, 0xc2974eb4ee658828,
    0xce947a3da6a9273e, 0x733d226229feea32,
    0x811ccc668829b887, 0x806357d5a3f525f,
    0xa163ff802a3426a8, 0xca07c2dcb0cf26f7,
    0xc9bcff6034c13052, 0xfc89b393dd02f0b5,
    0xfc2c3f3841f17c67, 0xbbac2078d443ace2,
    0x9d9ba7832936edc0, 0xd54b944b84aa4c0d,
    0xc5029163f384a931, 0xa9e795e65d4df11,
    0xf64335bcf065d37d, 0x4d4617b5ff4a16d5,
    0x99ea0196163fa42e, 0x504bced1bf8e4e45,
    0xc06481fb9bcf8d39, 0xe45ec2862f71e1d6,
    0xf07da27a82c37088, 0x5d767327bb4e5a4c,
    0x964e858c91ba2655, 0x3a6a07f8d510f86f,
    0xbbe226efb628afea, 0x890489f70a55368b,
    0xeadab0aba3b2dbe5, 0x2b45ac74ccea842e,
    0x92c8ae6b464fc96f, 0x3b0b8bc90012929d,
    0xb77ada0617e3bbcb, 0x9ce6ebb40173744,
    0xe55990879ddcaabd, 0xcc420a6a101d0515,
    0x8f57fa54c2a9eab6, 0x9fa946824a12232d,
    0xb32df8e9f3546564, 0x47939822dc96abf9,
    0xdff9772470297ebd, 0x59787e2b93bc56f7,
    0x8bfbea76c619ef36, 0x57eb4edb3c55b65a,
    0xaefae51477a06b03, 0xede622920b6b23f1,
    0xdab99e59958885c4, 0xe95fab368e45eced,
    0x88b402f7fd75539b, 0x11dbcb0218ebb414,
    0xaae103b5fcd2a881, 0xd652bdc29f26a119,
    0xd59944a37c0752a2, 0x4be76d3346f0495f,
    0x857fcae62d8493a5, 0x6f70a4400c562ddb,
    0xa6dfbd9fb8e5b88e, 0xcb4ccd500f6bb952,
    0xd097ad07a71f26b2, 0x7e2000a41346a7a7,
    0x825ecc24c873782f, 0x8ed400668c0c28c8,
    0xa2f67f2dfa90563b, 0x728900802f0f32fa,
    0xcbb41ef979346bca, 0x4f2b40a03ad2ffb9,
    0xfea126b7d78186bc, 0xe2f610c84987bfa8,
    0x9f24b832e6b0f436, 0xdd9ca7d2df4d7c9,
    0xc6ede63fa05d3143, 0x91503d1c79720dbb,
    0xf8a95fcf88747d94, 0x75a44c6397ce912a,
    0x9b69dbe1b548ce7c, 0xc986afbe3ee11aba,
    0xc24452da229b021b, 0xfbe85badce996168,
    0xf2d56790ab41c2a2, 0xfae27299423fb9c3,
    0x97c560ba6b0919a5, 0xdccd879fc967d41a,
    0xbdb6b8e905cb600f, 0x5400e987bbc1c920,
    0xed246723473e3813, 0x290123e9aab23b68,
    0x9436c0760c86e30b, 0xf9a0b6720aaf6521,
    0xb94470938fa89bce, 0xf808e40e8d5b3e69,
    0xe7958cb87392c2c2, 0xb60b1d1230b20e04,
    0x90bd77f3483bb9b9, 0xb1c6f22b5e6f48c2,
    0xb4ecd5f01a4aa828, 0x1e38aeb6360b1af3,
    0xe2280b6c20dd5232, 0x25c6da63c38de1b0,
    0x8d590723948a535f, 0x579c487e5a38ad0e,
    0xb0af48ec79ace837, 0x2d835a9df0c6d851,
    0xdcdb1b2798182244, 0xf8e431456cf88e65,
    0x8a08f0f8bf0f156b, 0x1b8e9ecb641b58ff,
    0xac8b2d36eed2dac5, 0xe272467e3d222f3f,
    0xd7adf884aa879177, 0x5b0ed81dcc6abb0f,
    0x86ccbb52ea94baea, 0x98e947129fc2b4e9,
    0xa87fea27a539e9a5, 0x3f2398d747b36224,
    0xd29fe4b18e88640e, 0x8eec7f0d19a03aad,
    0x83a3eeeef9153e89, 0x1953cf68300424ac,
    0xa48ceaaab75a8e2b, 0x5fa8c3423c052dd7,
    0xcdb02555653131b6, 0x3792f412cb06794d,
    0x808e17555f3ebf11, 0xe2bbd88bbee40bd0,
    0xa0b19d2ab70e6ed6, 0x5b6aceaeae9d0ec4,
    0xc8de047564d20a8b, 0xf245825a5a445275,
    0xfb158592be068d2e, 0xeed6e2f0f0d56712,
    0x9ced737bb6c4183d, 0x55464dd69685606b,
    0xc428d05aa4751e4c, 0xaa97e14c3c26b886,
    0xf53304714d9265df, 0xd53dd99f4b3066a8,
    0x993fe2c6d07b7fab, 0xe546a8038efe4029,
    0xbf8fdb78849a5f96, 0xde98520472bdd033,
    0xef73d256a5c0f77c, 0x963e66858f6d4440,
    0x95a8637627989aad, 0xdde7001379a44aa8,
    0xbb127c53b17ec159, 0x5560c018580d5d52,
    0xe9d71b689dde71af, 0xaab8f01e6e10b4a6,
    0x9226712162ab070d, 0xcab3961304ca70e8,
    0xb6b00d69bb55c8d1, 0x3d607b97c5fd0d22,
    0xe45c10c42a2b3b05, 0x8cb89a7db77c506a,
    0x8eb98a7a9a5b04e3, 0x77f3608e92adb242,
    0xb267ed1940f1c61c, 0x55f038b237591ed3,
    0xdf01e85f912e37a3, 0x6b6c46dec52f6688,
    0x8b61313bbabce2c6, 0x2323ac4b3b3da015,
    0xae397d8aa96c1b77, 0xabec975e0a0d081a,
    0xd9c7dced53c72255, 0x96e7bd358c904a21,
    0x881cea14545c7575, 0x7e50d64177da2e54,
    0xaa242499697392d2, 0xdde50bd1d5d0b9e9,
    0xd4ad2dbfc3d07787, 0x955e4ec64b44e864,
    0x84ec3c97da624ab4, 0xbd5af13bef0b113e,
    0xa6274bbdd0fadd61, 0xecb1ad8aeacdd58e,
    0xcfb11ead453994ba, 0x67de18eda5814af2,
    0x81ceb32c4b43fcf4, 0x80eacf948770ced7,
    0xa2425ff75e14fc31, 0xa1258379a94d028d,
    0xcad2f7f5359a3b3e, 0x96ee45813a04330,
    0xfd87b5f28300ca0d, 0x8bca9d6e188853fc,
    0x9e74d1b791e07e48, 0x775ea264cf55347e,
    0xc612062576589dda, 0x95364afe032a81a0,
    0xf79687aed3eec551, 0x3a83ddbd83f52210,
    0x9abe14cd44753b52, 0xc4926a9672793580,
    0xc16d9a0095928a27, 0x75b7053c0f178400,
    0xf1c90080baf72cb1, 0x5324c68b12dd6800,
    0x971da05074da7bee, 0xd3f6fc16ebca8000,
    0xbce5086492111aea, 0x88f4bb1ca6bd0000,
    0xec1e4a7db69561a5, 0x2b31e9e3d0700000,
    0x9392ee8e921d5d07, 0x3aff322e62600000,
    0xb877aa3236a4b449, 0x9befeb9fad487c3,
    0xe69594bec44de15b, 0x4c2ebe687989a9b4,
    0x901d7cf73ab0acd9, 0xf9d37014bf60a11,
    0xb424dc35095cd80f, 0x538484c19ef38c95,
    0xe12e13424bb40e13, 0x2865a5f206b06fba,
    0x8cbccc096f5088cb, 0xf93f87b7442e45d4,
    0xafebff0bcb24aafe, 0xf78f69a51539d749,
    0xdbe6fecebdedd5be, 0xb573440e5a884d1c,
    0x89705f4136b4a597, 0x31680a88f8953031,
    0xabcc77118461cefc, 0xfdc20d2b36ba7c3e,
    0xd6bf94d5e57a42bc, 0x3d32907604691b4d,
    0x8637bd05af6c69b5, 0xa63f9a49c2c1b110,
    0xa7c5ac471b478423, 0xfcf80dc33721d54,
    0xd1b71758e219652b, 0xd3c36113404ea4a9,
    0x83126e978d4fdf3b, 0x645a1cac083126ea,
    0xa3d70a3d70a3d70a, 0x3d70a3d70a3d70a4,
    0xcccccccccccccccc, 0xcccccccccccccccd,
    0x8000000000000000, 0x0,
    0xa000000000000000, 0x0,
    0xc800000000000000, 0x0,
    0xfa00000000000000, 0x0,
    0x9c40000000000000, 0x0,
    0xc350000000000000, 0x0,
    0xf424000000000000, 0x0,
    0x9896800000000000, 0x0,
    0xbebc200000000000, 0x0,
    0xee6b280000000000, 0x0,
    0x9502f90000000000, 0x0,
    0xba43b74000000000, 0x0,
    0xe8d4a51000000000, 0x0,
    0x9184e72a00000000, 0x0,
    0xb5e620f480000000, 0x0,
    0xe35fa931a0000000, 0x0,
    0x8e1bc9bf04000000, 0x0,
    0xb1a2bc2ec5000000, 0x0,
    0xde0b6b3a76400000, 0x0,
    0x8ac7230489e80000, 0x0,
    0xad78ebc5ac620000, 0x0,
    0xd8d726b7177a8000, 0x0,
    0x878678326eac9000, 0x0,
    0xa968163f0a57b400, 0x0,
    0xd3c21bcecceda100, 0x0,
    0x84595161401484a0, 0x0,
    0xa56fa5b99019a5c8, 0x0,
    0xcecb8f27f4200f3a, 0x0,
    0x813f3978f8940984, 0x4000000000000000,
    0xa18f07d736b90be5, 0x5000000000000000,
    0xc9f2c9cd04674ede, 0xa400000000000000,
    0xfc6f7c4045812296, 0x4d00000000000000,
    0x9dc5ada82b70b59d, 0xf020000000000000,
    0xc5371912364ce305, 0x6c28000000000000,
    0xf684df56c3e01bc6, 0xc732000000000000,
    0x9a130b963a6c115c, 0x3c7f400000000000,
    0xc097ce7bc90715b3, 0x4b9f100000000000,
    0xf0bdc21abb48db20, 0x1e86d40000000000,
    0x96769950b50d88f4, 0x1314448000000000,
    0xbc143fa4e250eb31, 0x17d955a000000000,
    0xeb194f8e1ae525fd, 0x5dcfab0800000000,
    0x92efd1b8d0cf37be, 0x5aa1cae500000000,
    0xb7abc627050305ad, 0xf14a3d9e40000000,
    0xe596b7b0c643c719, 0x6d9ccd05d0000000,
    0x8f7e32ce7bea5c6f, 0xe4820023a2000000,
    0xb35dbf821ae4f38b, 0xdda2802c8a800000,
    0xe0352f62a19e306e, 0xd50b2037ad200000,
    0x8c213d9da502de45, 0x4526f422cc340000,
    0xaf298d050e4395d6, 0x9670b12b7f410000,
    0xdaf3f04651d47b4c, 0x3c0cdd765f114000,
    0x88d8762bf324cd0f, 0xa5880a69fb6ac800,
    0xab0e93b6efee0053, 0x8eea0d047a457a00,
    0xd5d238a4abe98068, 0x72a4904598d6d880,
    0x85a36366eb71f041, 0x47a6da2b7f864750,
    0xa70c3c40a64e6c51, 0x999090b65f67d924,
    0xd0cf4b50cfe20765, 0xfff4b4e3f741cf6d,
    0x82818f1281ed449f, 0xbff8f10e7a8921a4,
    0xa321f2d7226895c7, 0xaff72d52192b6a0d,
    0xcbea6f8ceb02bb39, 0x9bf4f8a69f764490,
    0xfee50b7025c36a08, 0x2f236d04753d5b4,
    0x9f4f2726179a2245, 0x1d762422c946590,
    0xc722f0ef9d80aad6, 0x424d3ad2b7b97ef5,
    0xf8ebad2b84e0d58b, 0xd2e0898765a7deb2,
    0x9b934c3b330c8577, 0x63cc55f49f88eb2f,
    0xc2781f49ffcfa6d5, 0x3cbf6b71c76b25fb,
    0xf316271c7fc3908a, 0x8bef464e3945ef7a,
    0x97edd871cfda3a56, 0x97758bf0e3cbb5ac,
    0xbde94e8e43d0c8ec, 0x3d52eeed1cbea317,
    0xed63a231d4c4fb27, 0x4ca7aaa863ee4bdd,
    0x945e455f24fb1cf8, 0x8fe8caa93e74ef6a,
    0xb975d6b6ee39e436, 0xb3e2fd538e122b44,
    0xe7d34c64a9c85d44, 0x60dbbca87196b616,
    0x90e40fbeea1d3a4a, 0xbc8955e946fe31cd,
    0xb51d13aea4a488dd, 0x6babab6398bdbe41,
    0xe264589a4dcdab14, 0xc696963c7eed2dd1,
    0x8d7eb76070a08aec, 0xfc1e1de5cf543ca2,
    0xb0de65388cc8ada8, 0x3b25a55f43294bcb,
    0xdd15fe86affad912, 0x49ef0eb713f39ebe,
    0x8a2dbf142dfcc7ab, 0x6e3569326c784337,
    0xacb92ed9397bf996, 0x49c2c37f07965404,
    0xd7e77a8f87daf7fb, 0xdc33745ec97be906,
    0x86f0ac99b4e8dafd, 0x69a028bb3ded71a3,
    0xa8acd7c0222311bc, 0xc40832ea0d68ce0c,
    0xd2d80db02aabd62b, 0xf50a3fa490c30190,
    0x83c7088e1aab65db, 0x792667c6da79e0fa,
    0xa4b8cab1a1563f52, 0x577001b891185938,
    0xcde6fd5e09abcf26, 0xed4c0226b55e6f86,
    0x80b05e5ac60b6178, 0x544f8158315b05b4,
    0xa0dc75f1778e39d6, 0x696361ae3db1c721,
    0xc913936dd571c84c, 0x3bc3a19cd1e38e9,
    0xfb5878494ace3a5f, 0x4ab48a04065c723,
    0x9d174b2dcec0e47b, 0x62eb0d64283f9c76,
    0xc45d1df942711d9a, 0x3ba5d0bd324f8394,
    0xf5746577930d6500, 0xca8f44ec7ee36479,
    0x9968bf6abbe85f20, 0x7e998b13cf4e1ecb,
    0xbfc2ef456ae276e8, 0x9e3fedd8c321a67e,
    0xefb3ab16c59b14a2, 0xc5cfe94ef3ea101e,
    0x95d04aee3b80ece5, 0xbba1f1d158724a12,
    0xbb445da9ca61281f, 0x2a8a6e45ae8edc97,
    0xea1575143cf97226, 0xf52d09d71a3293bd,
    0x924d692ca61be758, 0x593c2626705f9c56,
    0xb6e0c377cfa2e12e, 0x6f8b2fb00c77836c,
    0xe498f455c38b997a, 0xb6dfb9c0f956447,
    0x8edf98b59a373fec, 0x4724bd4189bd5eac,
    0xb2977ee300c50fe7, 0x58edec91ec2cb657,
    0xdf3d5e9bc0f653e1, 0x2f2967b66737e3ed,
    0x8b865b215899f46c, 0xbd79e0d20082ee74,
    0xae67f1e9aec07187, 0xecd8590680a3aa11,
    0xda01ee641a708de9, 0xe80e6f4820cc9495,
    0x884134fe908658b2, 0x3109058d147fdcdd,
    0xaa51823e34a7eede, 0xbd4b46f0599fd415,
    0xd4e5e2cdc1d1ea96, 0x6c9e18ac7007c91a,
    0x850fadc09923329e, 0x3e2cf6bc604ddb0,
    0xa6539930bf6bff45, 0x84db8346b786151c,
    0xcfe87f7cef46ff16, 0xe612641865679a63,
    0x81f14fae158c5f6e, 0x4fcb7e8f3f60c07e,
    0xa26da3999aef7749, 0xe3be5e330f38f09d,
    0xcb090c8001ab551c, 0x5cadf5bfd3072cc5,
    0xfdcb4fa002162a63, 0x73d9732fc7c8f7f6,
    0x9e9f11c4014dda7e, 0x2867e7fddcdd9afa,
    0xc646d63501a1511d, 0xb281e1fd541501b8,
    0xf7d88bc24209a565, 0x1f225a7ca91a4226,
    0x9ae757596946075f, 0x3375788de9b06958,
    0xc1a12d2fc3978937, 0x52d6b1641c83ae,
    0xf209787bb47d6b84, 0xc0678c5dbd23a49a,
    0x9745eb4d50ce6332, 0xf840b7ba963646e0,
    0xbd176620a501fbff, 0xb650e5a93bc3d898,
    0xec5d3fa8ce427aff, 0xa3e51f138ab4cebe,
    0x93ba47c980e98cdf, 0xc66f336c36b10137,
    0xb8a8d9bbe123f017, 0xb80b0047445d4184,
    0xe6d3102ad96cec1d, 0xa60dc059157491e5,
    0x9043ea1ac7e41392, 0x87c89837ad68db2f,
    0xb454e4a179dd1877, 0x29babe4598c311fb,
    0xe16a1dc9d8545e94, 0xf4296dd6fef3d67a,
    0x8ce2529e2734bb1d, 0x1899e4a65f58660c,
    0xb01ae745b101e9e4, 0x5ec05dcff72e7f8f,
    0xdc21a1171d42645d, 0x76707543f4fa1f73,
    0x899504ae72497eba, 0x6a06494a791c53a8,
    0xabfa45da0edbde69, 0x487db9d17636892,
    0xd6f8d7509292d603, 0x45a9d2845d3c42b6,
    0x865b86925b9bc5c2, 0xb8a2392ba45a9b2,
    0xa7f26836f282b732, 0x8e6cac7768d7141e,
    0xd1ef0244af2364ff, 0x3207d795430cd926,
    0x8335616aed761f1f, 0x7f44e6bd49e807b8,
    0xa402b9c5a8d3a6e7, 0x5f16206c9c6209a6,
    0xcd036837130890a1, 0x36dba887c37a8c0f,
    0x802221226be55a64, 0xc2494954da2c9789,
    0xa02aa96b06deb0fd, 0xf2db9baa10b7bd6c,
    0xc83553c5c8965d3d, 0x6f92829494e5acc7,
    0xfa42a8b73abbf48c, 0xcb772339ba1f17f9,
    0x9c69a97284b578d7, 0xff2a760414536efb,
    0xc38413cf25e2d70d, 0xfef5138519684aba,
    0xf46518c2ef5b8cd1, 0x7eb258665fc25d69,
    0x98bf2f79d5993802, 0xef2f773ffbd97a61,
    0xbeeefb584aff8603, 0xaafb550ffacfd8fa,
    0xeeaaba2e5dbf6784, 0x95ba2a53f983cf38,
    0x952ab45cfa97a0b2, 0xdd945a747bf26183,
    0xba756174393d88df, 0x94f971119aeef9e4,
    0xe912b9d1478ceb17, 0x7a37cd5601aab85d,
    0x91abb422ccb812ee, 0xac62e055c10ab33a,
    0xb616a12b7fe617aa, 0x577b986b314d6009,
    0xe39c49765fdf9d94, 0xed5a7e85fda0b80b,
    0x8e41ade9fbebc27d, 0x14588f13be847307,
    0xb1d219647ae6b31c, 0x596eb2d8ae258fc8,
    0xde469fbd99a05fe3, 0x6fca5f8ed9aef3bb,
    0x8aec23d680043bee, 0x25de7bb9480d5854,
    0xada72ccc20054ae9, 0xaf561aa79a10ae6a,
    0xd910f7ff28069da4, 0x1b2ba1518094da04,
    0x87aa9aff79042286, 0x90fb44d2f05d0842,
    0xa99541bf57452b28, 0x353a1607ac744a53,
    0xd3fa922f2d1675f2, 0x42889b8997915ce8,
    0x847c9b5d7c2e09b7, 0x69956135febada11,
    0xa59bc234db398c25, 0x43fab9837e699095,
    0xcf02b2c21207ef2e, 0x94f967e45e03f4bb,
    0x8161afb94b44f57d, 0x1d1be0eebac278f5,
    0xa1ba1ba79e1632dc, 0x6462d92a69731732,
    0xca28a291859bbf93, 0x7d7b8f7503cfdcfe,
    0xfcb2cb35e702af78, 0x5cda735244c3d43e,
    0x9defbf01b061adab, 0x3a0888136afa64a7,
    0xc56baec21c7a1916, 0x88aaa1845b8fdd0,
    0xf6c69a72a3989f5b, 0x8aad549e57273d45,
    0x9a3c2087a63f6399, 0x36ac54e2f678864b,
    0xc0cb28a98fcf3c7f, 0x84576a1bb416a7dd,
    0xf0fdf2d3f3c30b9f, 0x656d44a2a11c51d5,
    0x969eb7c47859e743, 0x9f644ae5a4b1b325,
    0xbc4665b596706114, 0x873d5d9f0dde1fee,
    0xeb57ff22fc0c7959, 0xa90cb506d155a7ea,
    0x9316ff75dd87cbd8, 0x9a7f12442d588f2,
    0xb7dcbf5354e9bece, 0xc11ed6d538aeb2f,
    0xe5d3ef282a242e81, 0x8f1668c8a86da5fa,
    0x8fa475791a569d10, 0xf96e017d694487bc,
    0xb38d92d760ec4455, 0x37c981dcc395a9ac,
    0xe070f78d3927556a, 0x85bbe253f47b1417,
    0x8c469ab843b89562, 0x93956d7478ccec8e,
    0xaf58416654a6babb, 0x387ac8d1970027b2,
    0xdb2e51bfe9d0696a, 0x6997b05fcc0319e,
    0x88fcf317f22241e2, 0x441fece3bdf81f03,
    0xab3c2fddeeaad25a, 0xd527e81cad7626c3,
    0xd60b3bd56a5586f1, 0x8a71e223d8d3b074,
    0x85c7056562757456, 0xf6872d5667844e49,
    0xa738c6bebb12d16c, 0xb428f8ac016561db,
    0xd106f86e69d785c7, 0xe13336d701beba52,
    0x82a45b450226b39c, 0xecc0024661173473,
    0xa34d721642b06084, 0x27f002d7f95d0190,
    0xcc20ce9bd35c78a5, 0x31ec038df7b441f4,
    0xff290242c83396ce, 0x7e67047175a15271,
    0x9f79a169bd203e41, 0xf0062c6e984d386,
    0xc75809c42c684dd1, 0x52c07b78a3e60868,
    0xf92e0c3537826145, 0xa7709a56ccdf8a82,
    0x9bbcc7a142b17ccb, 0x88a66076400bb691,
    0xc2abf989935ddbfe, 0x6acff893d00ea435,
    0xf356f7ebf83552fe, 0x583f6b8c4124d43,
    0x98165af37b2153de, 0xc3727a337a8b704a,
    0xbe1bf1b059e9a8d6, 0x744f18c0592e4c5c,
    0xeda2ee1c7064130c, 0x1162def06f79df73,
    0x9485d4d1c63e8be7, 0x8addcb5645ac2ba8,
    0xb9a74a0637ce2ee1, 0x6d953e2bd7173692,
    0xe8111c87c5c1ba99, 0xc8fa8db6ccdd0437,
    0x910ab1d4db9914a0, 0x1d9c9892400a22a2,
    0xb54d5e4a127f59c8, 0x2503beb6d00cab4b,
    0xe2a0b5dc971f303a, 0x2e44ae64840fd61d,
    0x8da471a9de737e24, 0x5ceaecfed289e5d2,
    0xb10d8e1456105dad, 0x7425a83e872c5f47,
    0xdd50f1996b947518, 0xd12f124e28f77719,
    0x8a5296ffe33cc92f, 0x82bd6b70d99aaa6f,
    0xace73cbfdc0bfb7b, 0x636cc64d1001550b,
    0xd8210befd30efa5a, 0x3c47f7e05401aa4e,
    0x8714a775e3e95c78, 0x65acfaec34810a71,
    0xa8d9d1535ce3b396, 0x7f1839a741a14d0d,
    0xd31045a8341ca07c, 0x1ede48111209a050,
    0x83ea2b892091e44d, 0x934aed0aab460432,
    0xa4e4b66b68b65d60, 0xf81da84d5617853f,
    0xce1de40642e3f4b9, 0x36251260ab9d668e,
    0x80d2ae83e9ce78f3, 0xc1d72b7c6b426019,
    0xa1075a24e4421730, 0xb24cf65b8612f81f,
    0xc94930ae1d529cfc, 0xdee033f26797b627,
    0xfb9b7cd9a4a7443c, 0x169840ef017da3b1,
    0x9d412e0806e88aa5, 0x8e1f289560ee864e,
    0xc491798a08a2ad4e, 0xf1a6f2bab92a27e2,
    0xf5b5d7ec8acb58a2, 0xae10af696774b1db,
    0x9991a6f3d6bf1765, 0xacca6da1e0a8ef29,
    0xbff610b0cc6edd3f, 0x17fd090a58d32af3,
    0xeff394dcff8a948e, 0xddfc4b4cef07f5b0,
    0x95f83d0a1fb69cd9, 0x4abdaf101564f98e,
    0xbb764c4ca7a4440f, 0x9d6d1ad41abe37f1,
    0xea53df5fd18d5513, 0x84c86189216dc5ed,
    0x92746b9be2f8552c, 0x32fd3cf5b4e49bb4,
    0xb7118682dbb66a77, 0x3fbc8c33221dc2a1,
    0xe4d5e82392a40515, 0xfabaf3feaa5334a,
    0x8f05b1163ba6832d, 0x29cb4d87f2a7400e,
    0xb2c71d5bca9023f8, 0x743e20e9ef511012,
    0xdf78e4b2bd342cf6, 0x914da9246b255416,
    0x8bab8eefb6409c1a, 0x1ad089b6c2f7548e,
    0xae9672aba3d0c320, 0xa184ac2473b529b1,
    0xda3c0f568cc4f3e8, 0xc9e5d72d90a2741e,
    0x8865899617fb1871, 0x7e2fa67c7a658892,
    0xaa7eebfb9df9de8d, 0xddbb901b98feeab7,
    0xd51ea6fa85785631, 0x552a74227f3ea565,
    0x8533285c936b35de, 0xd53a88958f87275f,
    0xa67ff273b8460356, 0x8a892abaf368f137,
    0xd01fef10a657842c, 0x2d2b7569b0432d85,
    0x8213f56a67f6b29b, 0x9c3b29620e29fc73,
    0xa298f2c501f45f42, 0x8349f3ba91b47b8f,
    0xcb3f2f7642717713, 0x241c70a936219a73,
    0xfe0efb53d30dd4d7, 0xed238cd383aa0110,
    0x9ec95d1463e8a506, 0xf4363804324a40aa,
    0xc67bb4597ce2ce48, 0xb143c6053edcd0d5,
    0xf81aa16fdc1b81da, 0xdd94b7868e94050a,
    0x9b10a4e5e9913128, 0xca7cf2b4191c8326,
    0xc1d4ce1f63f57d72, 0xfd1c2f611f63a3f0,
    0xf24a01a73cf2dccf, 0xbc633b39673c8cec,
    0x976e41088617ca01, 0xd5be0503e085d813,
    0xbd49d14aa79dbc82, 0x4b2d8644d8a74e18,
    0xec9c459d51852ba2, 0xddf8e7d60ed1219e,
    0x93e1ab8252f33b45, 0xcabb90e5c942b503,
    0xb8da1662e7b00a17, 0x3d6a751f3b936243,
    0xe7109bfba19c0c9d, 0xcc512670a783ad4,
    0x906a617d450187e2, 0x27fb2b80668b24c5,
    0xb484f9dc9641e9da, 0xb1f9f660802dedf6,
    0xe1a63853bbd26451, 0x5e7873f8a0396973,
    0x8d07e33455637eb2, 0xdb0b487b6423e1e8,
    0xb049dc016abc5e5f, 0x91ce1a9a3d2cda62,
    0xdc5c5301c56b75f7, 0x7641a140cc7810fb,
    0x89b9b3e11b6329ba, 0xa9e904c87fcb0a9d,
    0xac2820d9623bf429, 0x546345fa9fbdcd44,
    0xd732290fbacaf133, 0xa97c177947ad4095,
    0x867f59a9d4bed6c0, 0x49ed8eabcccc485d,
    0xa81f301449ee8c70, 0x5c68f256bfff5a74,
    0xd226fc195c6a2f8c, 0x73832eec6fff3111,
    0x83585d8fd9c25db7, 0xc831fd53c5ff7eab,
    0xa42e74f3d032f525, 0xba3e7ca8b77f5e55,
    0xcd3a1230c43fb26f, 0x28ce1bd2e55f35eb,
    0x80444b5e7aa7cf85, 0x7980d163cf5b81b3,
    0xa0555e361951c366, 0xd7e105bcc332621f,
    0xc86ab5c39fa63440, 0x8dd9472bf3fefaa7,
    0xfa856334878fc150, 0xb14f98f6f0feb951,
    0x9c935e00d4b9d8d2, 0x6ed1bf9a569f33d3,
    0xc3b8358109e84f07, 0xa862f80ec4700c8,
    0xf4a642e14c6262c8, 0xcd27bb612758c0fa,
    0x98e7e9cccfbd7dbd, 0x8038d51cb897789c,
    0xbf21e44003acdd2c, 0xe0470a63e6bd56c3,
    0xeeea5d5004981478, 0x1858ccfce06cac74,
    0x95527a5202df0ccb, 0xf37801e0c43ebc8,
    0xbaa718e68396cffd, 0xd30560258f54e6ba,
    0xe950df20247c83fd, 0x47c6b82ef32a2069,
    0x91d28b7416cdd27e, 0x4cdc331d57fa5441,
    0xb6472e511c81471d, 0xe0133fe4adf8e952,
    0xe3d8f9e563a198e5, 0x58180fddd97723a6,
    0x8e679c2f5e44ff8f, 0x570f09eaa7ea7648,
};

const integer_string_finisher = [256]Error!void{
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, {},
    {},                 error.NUMBER_ERROR,   error.NUMBER_ERROR, {},                 error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   {},                 error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, {},
    error.NUMBER_ERROR, error.INCORRECT_TYPE, error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, {},                 error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.INCORRECT_TYPE,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, {},                   error.NUMBER_ERROR, {},                 error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.INCORRECT_TYPE, error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, {},                 error.NUMBER_ERROR,
    {},                 error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR, error.NUMBER_ERROR,   error.NUMBER_ERROR, error.NUMBER_ERROR, error.NUMBER_ERROR,
    error.NUMBER_ERROR,
};
// Parse any number from  -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
pub fn parse_integer(src: [*]const u8) !u64 {
    //
    // Check for minus sign
    //
    const negative = (src[0] == '-');
    var p = src + @boolToInt(negative);

    //
    // Parse the integer part.
    //
    // PERF NOTE: we don't use is_made_of_eight_digits_fast because large integers like 123456789 are rare
    const start_digits = p;
    var i: u64 = 0;
    while (parse_digit(u64, p[0], &i)) : (p += 1) {}

    // If there were no digits, or if the integer starts with 0 and has more than one digit, it's an error.
    // Optimization note: size_t is expected to be unsigned.
    var digit_count = try common.ptr_diff(u32, p, start_digits);
    // The longest negative 64-bit number is 19 digits.
    // The longest positive 64-bit number is 20 digits.
    // We do it this way so we don't trigger this branch unless we must.
    const longest_digit_count: u64 = if (negative) 19 else 20;
    // Optimization note: the compiler can probably merge
    // ((digit_count == 0) || (digit_count > longest_digit_count))
    // into a single  branch since digit_count is unsigned.
    if ((digit_count == 0) or (digit_count > longest_digit_count)) {
        return error.INCORRECT_TYPE;
    }
    // Here digit_count > 0.
    if (('0' == start_digits[0]) and (digit_count > 1)) {
        return error.NUMBER_ERROR;
    }
    // We can do the following...
    // if (!jsoncharutils::is_structural_or_whitespace(*p)) {
    //  return (*p == '.' || *p == 'e' || *p == 'E') ? INCORRECT_TYPE : NUMBER_ERROR;
    // }
    // as a single table lookup:
    if (integer_string_finisher[p[0]]) {} else |err| return err;
    if (digit_count == longest_digit_count) {
        if (negative) {
            // Anything negative above INT64_MAX+1 is invalid
            if (i > std.math.maxInt(i64) + 1) return error.INCORRECT_TYPE;
            return ~i + 1;

            // Positive overflow check:
            // - A 20 digit number starting with 2-9 is overflow, because 18,446,744,073,709,551,615 is the
            //   biggest uint64_t.
            // - A 20 digit number starting with 1 is overflow if it is less than INT64_MAX.
            //   If we got here, it's a 20 digit number starting with the digit "1".
            // - If a 20 digit number starting with 1 overflowed (i*10+digit), the result will be smaller
            //   than 1,553,255,926,290,448,384.
            // - That is smaller than the smallest possible 20-digit number the user could write:
            //   10,000,000,000,000,000,000.
            // - Therefore, if the number is positive and lower than that, it's overflow.
            // - The value we are looking at is less than or equal to 9,223,372,036,854,775,808 (INT64_MAX).
            //
        } else if (src[0] != '1' or i <= std.math.maxInt(i64)) return error.INCORRECT_TYPE;
    }

    return if (negative) (~i +% 1) else i;
}

pub fn parse_double(src_: [*]const u8) !f64 {
    //
    // Check for minus sign
    //
    const negative = (src_[0] == '-');
    var src = src_ + @boolToInt(negative);

    //
    // Parse the integer part.
    //
    var i: u64 = 0;
    var p = src;
    p += @boolToInt(parse_digit(u64, p[0], &i));
    const leading_zero = (i == 0);
    while (parse_digit(u64, p[0], &i)) {
        p += 1;
    }
    // no integer digits, or 0123 (zero must be solo)
    if (p == src) return error.INCORRECT_TYPE;
    if ((leading_zero and p != src + 1)) {
        return error.NUMBER_ERROR;
    }

    //
    // Parse the decimal part.
    //
    var exponent: i64 = 0;
    var overflow: bool = undefined;
    if (p[0] == '.') {
        p += 1;
        const start_decimal_digits = p;
        if (!parse_digit(u64, p[0], &i)) return error.NUMBER_ERROR; // no decimal digits
        p += 1;
        while (parse_digit(u64, p[0], &i)) {
            p += 1;
        }
        exponent = -try common.ptr_diff(i64, p, start_decimal_digits);

        // Overflow check. More than 19 digits (minus the decimal) may be overflow.
        overflow = (try common.ptr_diff(u16, p, src)) - 1 > 19;
        if (overflow and leading_zero) {
            // Skip leading 0.00000 and see if it still overflows
            var start_digits = src + 2;
            while (start_digits[0] == '0') {
                start_digits += 1;
            }
            overflow = (try common.ptr_diff(u32, start_digits, src)) > 19;
        }
    } else {
        overflow = (try common.ptr_diff(u16, p, src)) > 19;
    }

    //
    // Parse the exponent
    //
    if (p[0] == 'e' or p[0] == 'E') {
        p += 1;
        const exp_neg = p[0] == '-';
        p += @boolToInt(exp_neg or p[0] == '+');

        var exp: u64 = 0;
        const start_exp_digits = p;
        while (parse_digit(u64, p[0], &exp)) {
            p += 1;
        }
        // no exp digits, or 20+ exp digits
        const num_exp_digits = try common.ptr_diff(u16, p, start_exp_digits);
        if (num_exp_digits == 0 or num_exp_digits > 19) return error.NUMBER_ERROR;

        exponent += @bitCast(i64, if (exp_neg) 0 -% exp else exp);
    }

    if (CharUtils.is_not_structural_or_whitespace(p[0])) return error.NUMBER_ERROR;

    overflow = overflow or exponent < smallest_power or exponent > largest_power;

    //
    // Assemble (or slow-parse) the float
    //
    var d: f64 = undefined;
    if (!overflow) {
        if (compute_float_64(exponent, i, negative, &d)) {
            return d;
        }
    }
    if (!parse_float_fallback(src - @boolToInt(negative), &d)) {
        return error.NUMBER_ERROR;
    }
    return d;
}
