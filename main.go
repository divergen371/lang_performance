package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"time"
)

func calcInnerSum(u int) int {
	sum := 0
	// 0からu-1までの余りの合計を計算
	for i := 0; i < u; i++ {
		sum += i
	}
	// その結果を使って全体の合計を計算
	quotient := 99999 / u
	remainder := 99999 % u

	// 完全なu個のグループの合計
	total := sum * quotient

	// 残りの数の合計
	for i := 0; i <= remainder; i++ {
		total += i % u
	}

	return total
}

func main() {
	rand.Seed(time.Now().UnixNano())

	u, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic(err)
	}

	r := rand.Intn(10000)
	a := make([]int, 10000)

	// 内部ループの計算を1回だけ行う
	innerSum := calcInnerSum(u)

	// 配列の更新を最適化
	for i := 0; i < 10000; i++ {
		a[i] = innerSum + r
	}

	fmt.Println(a[r])
}
