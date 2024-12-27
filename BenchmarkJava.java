import java.util.Random;

public class BenchmarkJava {
    public static void main(String[] args) {
        int u = Integer.parseInt(args[0]);
        Random rand = new Random();
        int r = rand.nextInt(10000);
        int[] a = new int[10000];

        for (int i = 0; i < 10000; i++) {
            for (int j = 0; j < 100000; j++) {
                a[i] = a[i] + j % u;
            }
            a[i] += r;
        }
        System.out.println(a[r]);
    }
}
