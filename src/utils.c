#include <immintrin.h>
#include <emmintrin.h>
// #include <smmintrin.h>


// __m256i mm256_cmpgt_epi8(__m256i a, __m256i b) {
//     return _mm256_cmpgt_epi8(a, b);
// }

// __m256i mm256_permute2x128_si256(__m256i a, __m256i b) {
//     return _mm256_permute2x128_si256(a,b,0x21);
// }

__m256i _prev1(__m256i a, __m256i b) {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256(b, a, 0x21), 16 - 1);
}

__m256i _prev2(__m256i a, __m256i b) {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256(b, a, 0x21), 16 - 2);
}

__m256i _prev3(__m256i a, __m256i b) {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256(b, a, 0x21), 16 - 3);
}

// __m256i __must_be_2_3_continuation(__m256i prev2, __m256i prev3) {
//   __m256i is_third_byte  = _mm256_subs_epu8(prev2, _mm256_set1_epi8(0b11100000u-1)); // Only 111_____ will be > 0
//   __m256i is_fourth_byte = _mm256_subs_epu8(prev3, _mm256_set1_epi8(0b11110000u-1)); // Only 1111____ will be > 0
//   // Caller requires a bool (all 1's). All values resulting from the subtraction will be <= 64, so signed comparison is fine.
//   return _mm256_cmpgt_epi8(_mm256_or_si256(is_third_byte, is_fourth_byte),  _mm256_set1_epi8(0));
// }

// __m256i __check_multibyte_lengths(__m256i input, __m256i prev_input, __m256i sc) {
//     __m256i prev2 = _prev2(input, prev_input);
//     __m256i prev3 = _prev3(input, prev_input);
//     __m256i must23 = __must_be_2_3_continuation(prev2, prev3);
//     __m256i must23_80 = _mm256_and_si256(must23, _mm256_set1_epi8(0x80));
//     return must23_80 ^ sc;
// }
