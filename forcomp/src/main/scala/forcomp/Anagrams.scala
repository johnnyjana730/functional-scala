package forcomp

import forcomp.Anagrams.{Occurrences, Sentence}

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = Dictionary.loadDictionary

  def wordOccurrences(w: Word): Occurrences = w.toLowerCase().toList.groupBy((element: Char) => element).map{case (k,v) => (k.toLower, v.size)}.toList.sorted


  def sentenceOccurrences(s: Sentence): Occurrences = { wordOccurrences(s.mkString) }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences(_))


//    .map{word => ((wordOccurrences(word), )).map{case (k,v) => (k, v.size)}.toList
//    .map{lword => (lword, sentenceOccurrences(lword))}
//    groupBy((a: Occurrences, b:  Word) => b)
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).getOrElse(List())

//  def combinations(occurrences: Occurrences): List[Occurrences] =
//    List() :: (for {
//      (xs, ys) <- occurrences
//      i       <- 1 to ys
//      j       <- combinations(occurrences.filter(pair => pair._1 > xs))
//    } yield List((xs, i)) ++ j))
//
//  def combinations(occurrences: Occurrences): List[Occurrences] = {
//    var tmp: List[Occurrences]  = List[Occurrences]()
//    def combinall(occurrences: Occurrences, curlist: List[Occurrences]): List[Occurrences]  = {
//      println(occurrences)
////      println(curlist)
////      if (occurrences.length == 1) {
////        println(occurrences)
////        println(curlist)
////      }
//      if (occurrences.isEmpty) curlist
//      else{(for {
//      (c,intt) <- (occurrences)
//      n       <- (1 to intt)
//      j       <- combinall(occurrences.filter(pair => pair._1 > c), List((c, n)) ::  curlist)
//      } yield if (c == 'b') {j} else {(List((c,n)) ++ j)})
//    }}
//    List() :: combinall(occurrences, tmp)
//  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    List() :: (for {
      (xs, ys) <- occurrences
      i       <- 1 to ys
      j       <- combinations(occurrences.filter(pair => pair._1 > xs))
    } yield List((xs, i)) ++ j)
  }


  //    val tmp = y.foldLeft(Map[Char, Int]()) { (m, p1) => m(p1._1) = p1._2 }
  //    (!tmp.contains(p1._1) || tmp(p1._1) < p1._2) m ++ (p1._1, p1._2)}
  def subtract(x: Occurrences, y: Occurrences): Occurrences = (for {
    (xi,xj) <- x
    if xj > y.toMap.getOrElse(xi,0)
  } yield (xi,xj- y.toMap.getOrElse(xi,0)))

//  List[Sentence]List[Sentence]

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def parse(remain: Occurrences): List[Sentence] = {
      remain match {
        case List() => List(Nil)
        case _ =>
          //      if (remain.isEmpty) {List(Nil)}
          (for {
            j <- (combinations(remain)
            if (dictionaryByOccurrences.contains(j))
            j2 <- (dictionaryByOccurrences.get(j).get)
            k <- (parse(subtract(remain, j)))
          } yield List(j2) ++ k)
      }}

    parse(sentenceOccurrences(sentence))
  }
//
//  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
//    def anagrams(occurrences: Occurrences): List[Sentence] = {
//      occurrences match{
//        case List() => List(Nil)
//        case _ => (for{
//          xs <- combinations(occurrences)
//          if(dictionaryByOccurrences.contains(xs))
//          ys <- dictionaryByOccurrences.get(xs).get
//          zs <- anagrams(subtract(occurrences, xs))
//        } yield List(ys) ++ zs)
//      }
//    }
//    anagrams(sentenceOccurrences(sentence))
//  }
//

//  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
//      def parse(remain: Occurrences): List[Sentence] = {
//        if (remain.isEmpty) {List(Nil)}
//        (for {
//          j <- (combinations(remain).filter(_.length >= 1).filter(_ != remain))
//          if(dictionaryByOccurrences.contains(xs))
//          j2 <- (dictionaryByOccurrences.getOrElse(subtract(remain, j), List()))
//          k <- (parse(j))
//        } yield List(j2) ++ k)}
//      parse(sentenceOccurrences(sentence))
//  }

  //        println(remain)
  //        (for{
  //          xs <- combinations(remain)
  //          if(dictionaryByOccurrences.contains(xs))
  //          ys <- dictionaryByOccurrences.get(xs).get
  //          zs <- parse(subtract(remain, xs))
  //        } yield List(ys) ++ zs)}
  //        if (remain.isEmpty) {List(Nil)}
//    def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???


}

//sentence anagrams$colon Lukas Rytz (10pts)(forcomp.AnagramsSuite)
//expected:<HashSet(List(Ku, Salz, try), List(Salz, try, Ku), List(Ku, try, Salz), List(Salz, Ku, try), List(Katz, surly), List(try, Salz, Ku), List(try, Ku, Salz), List(surly, Katz))> but was:<Set()>
//def combinations(occurrences: Occurrences): List[Occurrences] = {
//List() :: (for {
//(xs, ys) <- occurrences
//i       <- 1 to ys
//j       <- combinations(occurrences.filter(pair => pair._1 > xs))
//} yield List((xs, i)) ++ j)
//}


object Main extends App {
  println(Anagrams.wordOccurrences("kolkdflksdlfdsf"))
  val tmp = List("You") ++ List("I") ++ List("love")
  println(Anagrams.sentenceOccurrences(tmp))
}


object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
