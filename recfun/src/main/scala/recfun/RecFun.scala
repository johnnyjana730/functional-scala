package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(s"${pascal(col, row)} ")
    //      println()
    //    }
    //    print(balance("(if (zero? x) max (/ 1 x))".toList))
    //    print(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    //    print(balance(":-)".toList))
    print(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def recur(cur: Int, n: Int): Int = {
      if (n > 0) {
        recur(cur * n, n - 1)
      }
      else {
        cur
      }
    }

    if (c == 0 || c == r) {
       1
    }
    else {
      val x = recur(1, r)
      val c_1 = recur(recur(1, c), r - c)
       x / c_1
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    //    print(chars)
    val ca1 = "(".charAt(0)
    val ca2 = ")".charAt(0)

    def recur(chars: List[Char], count: Int): Boolean = {
      //      println("st = ",chars, count)
      if (count < 0) {
        false
      }
      val sz = chars.length
      for (i <- 0 to sz - 1) {
        //        println(i)
        val stc = chars(i)
        //        println(stc, stc == ca1, count)
        if (stc == ca1) {
          //          (chars(i+1) to chars(sz-1)).toList
          val tmp = chars.splitAt(i)
          chars.slice(i + 1, sz - 1)
          recur(chars.slice(i + 1, sz), count + 1)
        }
        if (stc == ca2) {
          //          val tmp = chars.splitAt(i)
          recur(chars.slice(i + 1, sz), count - 1)
        }
      }
      true
    }

    recur(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val coins_sz = coins.length
    def recur( money: Int, index: Int, max: Int): Int =
      if (money == 0) 1
      else if (money < 0) 0
      else if (index >= coins_sz) 0
      else if (coins(index) >= max && money > 0)
        recur(money - coins(index), index, coins(index)) +  recur(money, index + 1, max)
      else 0
    recur(money, 0, 0)
  }
}
//
//def countChange(money: Int, coins: List[Int]): Int = {
//  val coins_sz = coins.length
//  def recur( money: Int, index: Int, max: Int): Int = {
//  //      print(money)
//  if (money == 0) {
//  return 1
//}
//  if (money < 0) {
//  return 0
//}
//  if (index >= coins_sz){
//  return 0
//}
//  if (coins(index) >= max && money > 0){
//  val t1 = recur(money - coins(index), index, coins(index))
//  val t2 = recur(money, index + 1, max)
//  return t1 + t2
//}
//  //      return t1 + t2
//  0
//}
//  recur(money, 0, 0)
//}
//
//
//if (index >= coins_sz){
//  1
//  //        val total = recur(money, index + 1, max)
//  //       ；+ recur(money - coins(index), index, coins(index))
//  //        total_1 + total_2
//  //        val total_1 = recur(money, index + 1, max)
  //        val total_2 = recur(money - coins(index), index, coins(index))
  //        total_1 + total_2
  //        total
//}