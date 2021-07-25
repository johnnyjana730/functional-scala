package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  type Rank = Int
  case class Node(x: A, r: Rank, c: List[Node])
  override type H = List[Node]

  lazy val genHeap: Gen[H] = oneOf(
    Gen.const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(Gen.const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

//  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
//    const(Map.empty[Int,Int]),
//    for {
//      k <- arbitrary[Int]
//      v <- arbitrary[Int]
//      m <- oneOf(const(Map.empty[Int,Int]), genMap)
//    } yield m.updated(k, v)
//  )
//
//
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll {
      a: Int =>
      forAll { b: Int =>
        forAll { c: Int =>
          val h = insert(c,insert(b, insert(a, empty)))
          findMin(h) == Math.min(Math.min(a, b), c)
        }
      }
  }

  property("link") = forAll {
    a: Int =>
      forAll { b: Int =>
        val gn = meld(insert(a, empty), insert(b, empty))
        findMin(gn) == Math.min(a, b)
        }
  }
  property("link_2") = forAll {
    a: Int =>
      forAll { b: Int =>
        val gn = meld(insert(3, empty), insert(5, empty))
        findMin(gn) == 3 && findMin(deleteMin(gn)) == 5
      }
  }

//  property("deleteMin") = forAll {
//    a: Int => {
//      Node(5, 0, List(Node(3,)))
//        val gn = meld(insert(3, empty), insert(5, empty))
//        findMin(gn) == 3 && findMin(deleteMin(gn)) == 5
//      }
//  }

  property("deleteMin") = forAll {
    a: Int =>
      forAll { b: Int =>
        forAll { c: Int =>
          val h = meld(meld(insert(3, empty), insert(5, empty)), meld(insert(3, empty), insert(5, empty)))
          val h2 = deleteMin(deleteMin(h))
          findMin(h2) == 5
        }
      }
  }

}



