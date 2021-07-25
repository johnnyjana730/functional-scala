//package streams

case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

import streams.Bloxorz.InfiniteLevel.terrain
import streams.GameDef

trait StringParserTerrain extends GameDef {

  val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
//    val tmp = (for {(for {
//      levelVec <- levelVector} yield levelVec)
//      level2 <- levelVec
//    } yield level2)
//    println(tmp)
    def valid(pos: Pos) = {
      if (levelVector(pos.row)(pos.col) == 'S' || levelVector(pos.row)(pos.col) == 'T' || levelVector(pos.row)(pos.col) == 'o') true
      else false
    }
    valid
  }

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = ???

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\r?\n").map(str => Vector(str: _*)).toIndexedSeq: _*)
  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)
}

new StringParserTerrain {
  print(terrain(Pos(0,0)))
//  assert(terrain(Pos(0,0)), "0,0")
//  assert(terrain(Pos(1,1)), "1,1") // start
//  assert(terrain(Pos(4,7)), "4,7") // goal
}
