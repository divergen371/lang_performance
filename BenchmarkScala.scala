object BenchmarkScala {
    // 関数型スタイルで内部合計を計算
    def calcInnerSum(u: Int): Int = {
        // 0からu-1までの余りの合計を計算（関数型の特徴を活用）
        val baseSum = (0 until u).sum
        
        // 完全なu個のグループの数と余りを計算
        val quotient = 99999 / u
        val remainder = 99999 % u
        
        // 合計を計算
        val total = baseSum * quotient + 
                   (0 to remainder).map(_ % u).sum
        
        total
    }

    def main(args: Array[String]): Unit = {
        val u = args(0).toInt
        val r = scala.util.Random.nextInt(10000)
        
        // 不変配列を使用（Scalaの特徴を活用）
        val innerSum = calcInnerSum(u)
        val a = Array.fill(10000)(innerSum + r)
        
        println(a(r))
    }
}
