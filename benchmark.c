#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// 内部合計を計算する関数
int calc_inner_sum(int u) {
    // 0からu-1までの余りの合計を計算
    int base_sum = 0;
    for (int i = 0; i < u; i++) {
        base_sum += i;
    }
    
    // 完全なu個のグループの数と余りを計算
    int quotient = 99999 / u;
    int remainder = 99999 % u;
    
    // 合計を計算
    int total = base_sum * quotient;
    
    // 残りの数の合計
    for (int i = 0; i <= remainder; i++) {
        total += i % u;
    }
    
    return total;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <number>\n", argv[0]);
        return 1;
    }

    // 乱数初期化
    srand(time(NULL));
    
    int u = atoi(argv[1]);
    int r = rand() % 10000;
    
    // 配列のメモリ割り当て
    int *a = (int *)calloc(10000, sizeof(int));
    if (a == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    
    // 内部ループの計算を1回だけ行う
    int inner_sum = calc_inner_sum(u);
    
    // 配列の更新を最適化
    for (int i = 0; i < 10000; i++) {
        a[i] = inner_sum + r;
    }
    
    printf("%d\n", a[r]);
    
    // メモリ解放
    free(a);
    return 0;
}
