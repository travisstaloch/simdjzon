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

fn INVALID_NUMBER(src: [*]const u8) Error {
    _ = src;
    return error.NUMBER_ERROR;
}

pub fn parse_number(
    src: [*]const u8,
    iter: *Iterator,
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

    var digit_count = @ptrToInt(p) - @ptrToInt(start_digits);
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
        digit_count = @ptrToInt(p) - @ptrToInt(start_digits); // used later to guard against overflows
    }
    if ('e' == p[0] or 'E' == p[0]) {
        is_float = true;
        p += 1;
        try parse_exponent(src, &p, &exponent);
    }
    if (is_float) {
        // std.log.debug("is_float {c}", .{p[0]});
        const dirty_end = CharUtils.is_not_structural_or_whitespace(p[0]);
        try write_float(src, negative, i, start_digits, digit_count, exponent, tb, iter);
        if (dirty_end) {
            return INVALID_NUMBER(src);
        }
        return;
    }

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
    //         WRITE_INTEGER(~i + 1, src, writer, iter);
    //         if (CharUtils.is_not_structural_or_whitespace(p[0])) {
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
        try tb.append2(iter, 0, if (negative) (~i +% 1) else i, .INT64);
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
        (try std.math.cast(i64, @ptrToInt(first_after_period))) -
        (try std.math.cast(i64, @ptrToInt(p.*)));
    std.log.debug("exponent {} firstap {*} p {*}", .{ exponent.*, first_after_period, p });
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
    return digit_count - @ptrToInt(start) - @ptrToInt(start_digits);
}

fn write_float(src: [*]const u8, negative: bool, i: u64, start_digits: [*]const u8, digit_count: usize, exponent: i64, writer: *TapeBuilder, iter: *Iterator) !void {
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
        if (true) @panic("TODO: slow_float_parsing");
        defer writer.skip_double();
        // try slow_float_parsing(src, writer, iter);
        return;
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
            try WRITE_DOUBLE(0, src, writer, iter);
            return;
        } else { // (exponent > largest_power) and (i != 0)
            // We have, for sure, an infinite value and simdjson refuses to parse infinite values.
            return INVALID_NUMBER(src);
        }
    }
    _ = negative;
    // @panic("TODO: compute_float_64");
    std.log.debug("exponent {}, i {}, negative {}", .{ exponent, i, negative });

    // var d: f64 = undefined;
    // if (!compute_float_64(exponent, i, negative, &d)) {
    //     // we are almost never going to get here.
    //     @panic("TODO: parse_float_fallback");
    //     // if (!parse_float_fallback(src, &d)) { return INVALID_NUMBER(src); }
    // }
    // try WRITE_DOUBLE(d, src, writer, iter);
}

fn WRITE_DOUBLE(VALUE: f64, _: [*]const u8, WRITER: *TapeBuilder, iter: *Iterator) !void {
    try (WRITER).append_double(iter, VALUE);
}
const smallest_power = -342;
const largest_power = 308;

const power_of_ten = [_]f64{
    1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,  1e8,  1e9,  1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
};

inline fn compute_float_64(power: i64, i: u64, negative: bool, d: *f64) bool {
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
        d = @intToFloat(f64, i);
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
            d = d / power_of_ten[-power];
        } else {
            d = d * power_of_ten[power];
        }
        if (negative) {
            d = -d;
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
        d = 0.0;
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
    const lz = leading_zeroes(i);
    i <<= lz;

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
    const index: u32 = 2 * power - smallest_power;
    // Optimization: It may be that materializing the index as a variable might confuse some compilers and prevent effective complex-addressing loads. (Done for code clarity.)
    //
    // The full_multiplication function computes the 128-bit product of two 64-bit words
    // with a returned value of type value128 with a "low component" corresponding to the
    // 64-bit least significant bits of the product and with a "high component" corresponding
    // to the 64-bit most significant bits of the product.
    const firstproduct = CharUtils.full_multiplication(i, power_of_five_128[index]);
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
        var secondproduct = CharUtile.full_multiplication(i, power_of_five_128[index + 1]);
        firstproduct.low += secondproduct.high;
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
    const upperbit: u64 = upper >> 63;
    const mantissa: u64 = upper >> (upperbit + 9);
    lz += int(1 ^ upperbit);

    // Here we have mantissa < (1<<54).
    const real_exponent: i64 = exponent - lz;
    if (real_exponent <= 0) { // we have a subnormal?
        // Here have that real_exponent <= 0 so -real_exponent >= 0
        if (-real_exponent + 1 >= 64) { // if we have more than 64 bits below the minimum exponent, you have a zero for sure.
            d = 0.0;
            return true;
        }
        // next line is safe because -real_exponent + 1 < 0
        mantissa >>= -real_exponent + 1;
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
        real_exponent = if (mantissa < (uint64_t(1) << 52)) 0 else 1;
        d = to_double(mantissa, real_exponent, negative);
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
        if ((mantissa << (upperbit + 64 - 53 - 2)) == upper) {
            mantissa &= ~1; // flip it so that we do not round up
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
    mantissa &= ~(1 << 52);
    // we have to check that real_exponent is in range, otherwise we bail out
    if (real_exponent > 2046) {
        // We have an infinite value!!! We could actually throw an error here if we could.
        return false;
    }
    d = to_double(mantissa, real_exponent, negative);
    return true;
}
