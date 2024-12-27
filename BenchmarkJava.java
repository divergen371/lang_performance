import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.IntStream;

public class BenchmarkJava {
    // JITコンパイラのヒント
    @HotSpotIntrinsicCandidate
    private static int calcInnerSum(int u) {
        // 0からu-1までの余りの合計を計算
        int baseSum = IntStream.range(0, u).sum();
        
        // 完全なu個のグループの数と余りを計算
        int quotient = 99999 / u;
        int remainder = 99999 % u;
        
        // 合計を計算
        int remainderSum = IntStream.rangeClosed(0, remainder)
            .map(x -> x % u)
            .sum();
        
        return baseSum * quotient + remainderSum;
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: java BenchmarkJava <number>");
            System.exit(1);
        }

        int u = Integer.parseInt(args[0]);
        
        Random rand = new Random();
        int r = rand.nextInt(10000);
        
        // 内部ループの計算を1回だけ行う
        int innerSum = calcInnerSum(u);
        
        // 配列の初期化を並列化
        int[] a = new int[10000];
        int processors = Runtime.getRuntime().availableProcessors();
        ForkJoinPool customThreadPool = new ForkJoinPool(processors);
        
        try {
            customThreadPool.submit(() -> 
                Arrays.parallelSetAll(a, i -> innerSum + r)
            ).get();
        } catch (Exception e) {
            System.err.println("Error during parallel execution: " + e.getMessage());
            System.exit(1);
        }
        
        System.out.println(a[r]);
    }
}
