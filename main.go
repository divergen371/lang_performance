package main

import (
	"fmt"
	"math/rand"
	"os"
	"runtime"
	"strconv"
	"sync"
)

// 内部ループの計算を行う関数
func calcInnerSum(u uint32) uint32 {
	var baseSum uint32
	for i := uint32(0); i < u; i++ {
		baseSum += i
	}

	quotient := uint32(99999) / u
	remainder := uint32(99999) % u

	var remainderSum uint32
	for i := uint32(0); i <= remainder; i++ {
		remainderSum += i % u
	}

	return baseSum*quotient + remainderSum
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <number>\n", os.Args[0])
		os.Exit(1)
	}

	input, err := strconv.ParseUint(os.Args[1], 10, 32)
	if err != nil {
		panic(err)
	}
	u := uint32(input)

	r := uint32(rand.Int31n(10000))

	// 内部ループの計算を1回だけ行う
	innerSum := calcInnerSum(u)

	// 配列の初期化を並列化
	a := make([]uint32, 10000)
	numCPU := runtime.NumCPU()
	var wg sync.WaitGroup
	chunkSize := (10000 + numCPU - 1) / numCPU

	for i := 0; i < numCPU; i++ {
		wg.Add(1)
		start := i * chunkSize
		end := start + chunkSize
		if end > 10000 {
			end = 10000
		}

		go func(start, end int) {
			defer wg.Done()
			for i := start; i < end; i++ {
				a[i] = innerSum + r
			}
		}(start, end)
	}

	wg.Wait()
	fmt.Println(a[r])
}
