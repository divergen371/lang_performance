import scala.collection.parallel.CollectionConverters._
import scala.util.Random

object BenchmarkScala {
  // 内部ループの計算を行う関数
  @inline
  private def calcInnerSum(u: Int): Int = {
    // 0からu-1までの余りの合計を計算
    val baseSum = (0 until u).sum
    
    // 完全なu個のグループの数と余りを計算
    val quotient = 99999 / u
    val remainder = 99999 % u
    
    // 合計を計算
    val remainderSum = (0 to remainder).map(_ % u).sum
    
    baseSum * quotient + remainderSum
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: scala BenchmarkScala <number>")
      System.exit(1)
    }

    val u = args(0).toInt
    val r = Random.nextInt(10000)
    
    // 内部ループの計算を1回だけ行う
    val innerSum = calcInnerSum(u)
    
    // 配列の初期化を並列化
    val a = (0 until 10000).par
      .map(_ => innerSum + r)
      .toArray
    
    println(a(r))
  }
}
