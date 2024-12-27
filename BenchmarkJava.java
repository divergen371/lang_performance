import java.util.Random;

public class BenchmarkJava {
    private static int calcInnerSum(int u) {
        // 0からu-1までの余りの合計を計算
        int sum = 0;
        for (int i = 0; i < u; i++) {
            sum += i;
        }
        
        // 完全なu個のグループの合計を計算
        int quotient = 99999 / u;
        int remainder = 99999 % u;
        
        // 合計を計算（Javaの整数演算の特性を活用）
        int total = sum * quotient;
        
        // 残りの数の合計
        for (int i = 0; i <= remainder; i++) {
            total += i % u;
        }
        
        return total;
    }

    public static void main(String[] args) {
        int u = Integer.parseInt(args[0]);
        Random rand = new Random();
        int r = rand.nextInt(10000);
        
        // 配列操作を最適化
        int[] a = new int[10000];
        int innerSum = calcInnerSum(u);
        
        // JVMのループ最適化を活用
        for (int i = 0; i < 10000; i++) {
            a[i] = innerSum + r;
        }
        
        System.out.println(a[r]);
    }
}
