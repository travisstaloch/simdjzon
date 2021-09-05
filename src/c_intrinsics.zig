const v = @import("vector_types.zig");

pub extern fn _prev1(v.u8x32, v.u8x32) v.u8x32;
pub extern fn _prev2(v.u8x32, v.u8x32) v.u8x32;
pub extern fn _prev3(v.u8x32, v.u8x32) v.u8x32;
// pub extern fn mm256_cmpgt_epi8(v.i8x32, v.i8x32) v.i8x32;
// pub extern fn mm256_permute2x128_si256(v.u8x32, v.u8x32) v.u8x32;
// extern fn __check_multibyte_lengths(input: v.u8x32, prev_input: v.u8x32, sc: v.u8x32) v.u8x32;
// extern fn __must_be_2_3_continuation(v.i8x32, v.i8x32) v.u8x32;
