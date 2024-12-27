import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

object BenchmarkScala {
  // 内部ループの計算を行う関数
  @inline
  private def calcInnerSum(u: Int): Int = {
    // 0からu-1までの余りの合計を計算
    val baseSum = (0.until(u)).sum
    
    // 完全なu個のグループの数と余りを計算
    val quotient = 99999 / u
    val remainder = 99999 % u
    
    // 合計を計算
    val remainderSum = (0.to(remainder)).map(_ % u).sum
    
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
    
    // 配列の初期化を並列化（Futureを使用）
    val numCPU = Runtime.getRuntime.availableProcessors
    val chunkSize = (10000 + numCPU - 1) / numCPU
    
    val futures = (0 until numCPU).map { i =>
      val start = i * chunkSize
      val end = math.min(start + chunkSize, 10000)
      Future {
        Array.fill(end - start)(innerSum + r)
      }
    }
    
    val result = Await.result(
      Future.sequence(futures).map(_.flatten),
      10.seconds
    )
    
    println(result(r))
  }
}
