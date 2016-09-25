package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var count = 0
    chars.foreach(_ match {
      case '(' => count += 1
      case ')' => {
        count -= 1
        if(count < 0) {
          return false
        }
      }
      case _ => {}
    })
    return count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int) : (Int, Int) = {
//      println("until: " + until + "  from: " + from)
      if(until - from < threshold) {
        var count = 0
        var minCount = 0
        (from until until).foreach(i => chars(i) match {
          case '(' => count += 1
          case ')' => {
            count -= 1
            if(count < minCount) {
              minCount = count
            }
          }
          case _ => {}
        })
//        println("returning: " + count + " , " + minCount)
        (count, minCount)
      }
      else {
        val (t1, t2) = parallel(traverse(from, (from + until)/2), traverse((from + until)/2 + 1, until))
        (t1._1 + t2._1, Math.min(t1._2, t2._2))
      }
    }

    def reduce(from: Int, until: Int): Boolean = {
      val (count, minCount) = traverse(from, until)
      return count == 0 && minCount >= 0
    }

    reduce(0, chars.length)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
