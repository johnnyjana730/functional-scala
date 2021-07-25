package reductions

import scala.annotation._
import org.scalameter._

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
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var count = 0
    var bool_balance = true
    for (n <- chars) {
      if (n == '(') {count  =  count + 1}
      else if (n == ')') {count  =  count - 1}
      if (count < 0) { bool_balance = false}
    }
    bool_balance
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, acc: Int, sign: Int): (Int, Int) = {
      if (idx >= until) (acc, sign)
      else {
        val (newAcc, newSign) = chars(idx) match {
          case '(' => (acc + 1, if (sign == 0) sign + 1 else sign)
          case ')' => (acc - 1, if (sign == 0) sign - 1 else sign)
          case _ => (acc, sign)
        }
        traverse(idx + 1, until, newAcc, newSign)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = from + (until - from) / 2
        val (resL, resR) = parallel(reduce(from, middle), reduce(middle, until))
        (Math.min(resL._1, resL._2 + resR._1), resL._2 + resR._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

//  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
//
//    def traverse(idx: Int, until: Int): (Int, Int) = {
//      var t_idex = idx
//      var t_c = 0
//      var cur = 0
//      while (t_idex < until) {
//        if (chars(t_idex) == '(') {
//          cur += 1
//        }
//        else if (chars(t_idex) == ')') {
//          if (cur <= 0) {t_c += 1}
//          else {cur -= 1}
//        }
//        t_idex += 1
//      }
//      (cur, t_c)
//    }
//
//    def reduce(from: Int, until: Int): (Int, Int) = {
//      if (threshold >= until - from) {traverse(from, until)}
//      val n = from + (until - from) / 2
//      if (n == 0) (0,0)
//      else {
//        val ((p1_o, p1_c), (p2_o, p2_c))  = parallel(reduce(from, n), reduce(n, until))
//        if (p1_o > p2_c) { (p1_o - p2_c + p2_o, p1_c) }
//        else  (p2_o, p2_c - p1_o + p1_c)
//      }}
//    (0, 0) == reduce(0, chars.length)
////      if (o == 0 && c == 0) true
////      else false
//    }



  // For those who want more:
  // Prove that your reduction operator is associative!

}
