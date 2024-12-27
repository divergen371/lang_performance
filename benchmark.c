#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

// 内部ループの計算を事前に行う関数
static inline uint32_t calc_inner_sum(const uint32_t u) {
    uint32_t base_sum = 0;
    for (uint32_t i = 0; i < u; i++) {
        base_sum += i;
    }
    
    const uint32_t quotient = 99999 / u;
    const uint32_t remainder = 99999 % u;
    
    uint32_t remainder_sum = 0;
    for (uint32_t i = 0; i <= remainder; i++) {
        remainder_sum += i % u;
    }
    
    return base_sum * quotient + remainder_sum;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <number>\n", argv[0]);
        return 1;
    }

    const uint32_t u = (uint32_t)atoi(argv[1]);
    
    srand(time(NULL));
    const uint32_t r = rand() % 10000;
    
    // 内部ループの計算を1回だけ行う
    const uint32_t inner_sum = calc_inner_sum(u);
    
    // ベクトル演算の最適化のヒントを追加
    #pragma GCC ivdep
    uint32_t a[10000] __attribute__((aligned(64)));
    for (uint32_t i = 0; i < 10000; i++) {
        a[i] = inner_sum + r;
    }
    
    printf("%u\n", a[r]);
    return 0;
}
