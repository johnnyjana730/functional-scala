import patmat.Huffman.{chars, combine, decode, encode, makeOrderedLeafList, string2Chars, weight}
import patmat.{Fork, Huffman, HuffmanInterface, Leaf}

import org.junit._
import org.junit.Assert.assertEquals


class HuffmanSuite{
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    println(weight(t1))
//    string2Chars(t1)
  }

//  weight(t1)
//
//  @Test def `weight of a larger tree (10pts)`: Unit =
//    new TestTrees {
//      assertEquals(5, weight(t1))
//    }

}
