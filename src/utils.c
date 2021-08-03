#include <immintrin.h>
#include <emmintrin.h>
// #include <smmintrin.h>


__m256i mm256_cmpgt_epi8(__m256i a, __m256i b) {
    return _mm256_cmpgt_epi8(a, b);
}

__m256i _prev1(__m256i a, __m256i b) {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256(b, a, 0x21), 16 - 1);
}

__m256i _prev2(__m256i a, __m256i b) {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256(b, a, 0x21), 16 - 2);
}

__m256i _prev3(__m256i a, __m256i b) {
    return _mm256_alignr_epi8(a, _mm256_permute2x128_si256(b, a, 0x21), 16 - 3);
}
