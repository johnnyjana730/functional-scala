package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000

    val input_2 = (0 until length).map(_ % 100 * 1.0f)
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
//    println(input_2)
//    println(output)

    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    output(0) = 0
    for( i <- 1 until input.length){
      output(i) = Math.max(input(i) / i, output(i-1))
    }
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
//  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
//    var tmp = 0f
//    for( i <- from until until){
//      tmp = Math.max(input(i) / i, tmp)
//    }
//    tmp
//  }
  def calculateAngle(index: Int, height: Float): Float =
    if (index == 0) 0 else height / index

  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var i = from
    var currMax = 0f

    while (i < until) {
      currMax = Math.max(currMax, calculateAngle(i, input(i)))
      i += 1
    }
    currMax
  }


  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if (end - from <= threshold)
      Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val mid = from + (end - from) / 2
      val (left, right) = parallel(
        upsweep(input, from, mid, threshold),
        upsweep(input, mid, end, threshold)
      )
      Node(left, right)
    }
  }
  //  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
//    if (end - from <= threshold) { Leaf(from, end, upsweepSequential(input, from, end))}
//    else {
//      val n = from  + (end - from) / 2
//      val (left, right) = parallel(upsweep(input: Array[Float], from: Int, n: Int, threshold), upsweep(input: Array[Float], n: Int, end: Int, threshold))
//      Node(left, right)
//    }
//  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
//  def downsweepSequential(input: Array[Float], output: Array[Float],
//    startingAngle: Float, from: Int, until: Int): Unit = {
////    var idx_f = 0
//    if (from < until) {
//      output(from) = Math.max(input(from) / from, startingAngle)
//      downsweepSequential(input, output, output(from), from + 1, until)
////      for (i <- from + 1 until until) {
////        output(i) = Math.max(input(i) / i, output(i - 1))
////      }
//    }
//  }

  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {
    var i = from
    var currMax = startingAngle

    while (i < until) {
      currMax = Math.max(currMax, calculateAngle(i, input(i)))
      output.update(i, currMax)
      i += 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */

  def downsweep(input: Array[Float], output: Array[Float],
                startingAngle: Float, tree: Tree): Unit = tree match {
    case Leaf(from, until, maxPrevious) =>
      downsweepSequential(input, output, startingAngle, from, until)
    case Node(left, right) => parallel(
      downsweep(input, output, startingAngle, left),
      downsweep(input, output, left.maxPrevious, right)
    )
  }

//  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
//    tree: Tree): Unit = {
//    tree match {
//      case Node(left, right) => { parallel(downsweep(input, output, startingAngle, left), downsweep(input, output, left.maxPrevious max startingAngle, right)) }
//      case Leaf(from, until, maxPrevious) => { downsweepSequential(input, output, startingAngle, from, until) }
//    }
//  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float], threshold: Int): Unit = {
    val tree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0f, tree)
  }

  //  def parLineOfSight(input: Array[Float], output: Array[Float],
//    threshold: Int): Unit = {
//    val node = upsweep(input: Array[Float], 1: Int, output.length: Int, threshold: Int)
//    downsweep(input: Array[Float], output: Array[Float], 0f: Float, node: Tree)
//  }
}
