#!/bin/bash

RESULTS_FILE="benchmark_results.json"
RUNS=10

function benchmark() {
    local cmd="$1"
    local name="$2"
    echo "Running benchmark for $name..."
    
    hyperfine --warmup 3 --runs $RUNS --export-json "temp_${name}.json" "$cmd"
    
    if [ -f "temp_${name}.json" ]; then
        stats=$(cat "temp_${name}.json" | jq '.results[0] | {mean: .mean, stddev: .stddev, min: .min, max: .max}')
        echo "\"$name\": $stats," >>$RESULTS_FILE
        rm "temp_${name}.json"
    else
        echo "\"$name\": {\"error\": \"Benchmark failed\"}," >>$RESULTS_FILE
    fi
}

# Initialize results file
echo "{" >$RESULTS_FILE

# Compile with maximum optimization for all languages
echo "Compiling..."
# Go: Enable optimizations without debug flags
go build -o benchmark_go -ldflags="-w -s" main.go
javac -O BenchmarkJava.java
scalac -optimize BenchmarkScala.scala
gcc -O3 -march=native -flto benchmark.c -o c_bench
cargo build --release

# Execute Benchmarks
benchmark "./benchmark_go 7" "Go"
benchmark "java BenchmarkJava 7" "Java"
benchmark "scala BenchmarkScala 7" "Scala"
benchmark "elixir Benchmark.exs 7" "Elixir"
benchmark "clojure -M benchmark.clj 7" "Clojure"
benchmark "./c_bench 7" "C"
benchmark "./target/release/benchmark 7" "Rust"

# Remove trailing comma and close JSON
sed -i '' '$ s/,$//' $RESULTS_FILE
echo "}" >>$RESULTS_FILE

# Format and display results
echo "Results:"
jq '.' $RESULTS_FILE

# Cleanup
rm -f benchmark_go c_bench *.class
