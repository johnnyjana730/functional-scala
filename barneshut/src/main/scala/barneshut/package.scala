import barneshut.{Body, Fork, Leaf, Quad}

import java.util.concurrent._
import scala.{collection => coll}
import scala.util.DynamicVariable
import barneshut.conctrees._

import scala.collection.Seq

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad extends QuadInterface {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = 0f
    def massY: Float = 0f
    def mass: Float = 0f
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }


//  @Test def `Fork with 3 empty quadrants and 1 leaf (nw)`: Unit = {
//    val b = new Body(123f, 18f, 26f, 0f, 0f)
//    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
//    val ne = Empty(22.5f, 27.5f, 5f)
//    val sw = Empty(17.5f, 32.5f, 5f)
//    val se = Empty(22.5f, 32.5f, 5f)
//    val quad = Fork(nw, ne, sw, se)
//    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
//    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
//    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
//    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
//    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
//    assert(quad.total == 1, s"${quad.total} should be 1")

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = ( nw.centerX + ne.centerX + sw.centerX + se.centerX ) / 4
    val centerY: Float = ( nw.centerY + ne.centerY + sw.centerY + se.centerY ) / 4
    val size: Float = nw.size + ne.size
    val mass: Float = (nw.mass + ne.mass + sw.mass + se.mass)
    val massX: Float = (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / ( nw.mass + ne.mass + sw.mass + se.mass )
    val massY: Float = (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY) / ( nw.mass + ne.mass + sw.mass + se.mass )
    val total: Int = nw.total + ne.total + sw.total + se.total
    def insert(b: Body): Fork = {
        if(b.x < centerX)
          if(b.y < centerY)
            Fork(nw.insert(b), ne, sw, se)
          else
            Fork(nw, ne, sw.insert(b), se)
        else
          if(b.y < centerY)
            Fork(nw, ne.insert(b), sw, se)
          else
            Fork(nw, ne, sw, se.insert(b))
      }
    }


  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: coll.Seq[Body])
  extends Quad {
    val bodies_mass: Float = bodies.foldLeft((0f)) {case (accA, body) => (accA + body.mass)}
    val bodies_massX: Float = bodies.foldLeft((0f)) {case (accA, body) => (accA + body.mass * body.x / bodies_mass)}
    val bodies_massY: Float = bodies.foldLeft((0f)) {case (accA, body) => (accA + body.mass * body.y / bodies_mass)}
    val (mass, massX, massY) = (bodies_mass : Float, bodies_massX : Float, bodies_massY : Float)
    val total: Int = bodies.size
    def insert(b: Body): Quad = {
      if (size <= minimumSize)
        Leaf(centerX, centerY, size, bodies :+ b)
      else {
        val (new_l_x, new_h_x, new_l_y, new_h_y) = (centerX - size / 4, centerX + size / 4, centerY - size / 4, centerY + size / 4)
        val n_nw = Empty(new_l_x, new_l_y, size / 2)
        val n_sw = Empty(new_l_x, new_h_y, size / 2)
        val n_ne = Empty(new_h_x, new_l_y, size / 2)
        val n_se = Empty(new_h_x, new_h_y, size / 2)
        (bodies :+ b).foldLeft(Fork(n_nw, n_ne, n_sw, n_se))((a, b) => a.insert(b))
      }
    }
  }


  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) => addForce(0f,0f,0f)
          // no force
        case Leaf(_, _, _, bodies) =>
          bodies.map{b => addForce(b.mass, b.x, b.y)}
//          addForce(0f,0f,0f)
          // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
          val dist1 = distance(x,y, nw.centerX, nw.centerY)
          if (quad.size / dist1 < theta) {addForce(nw.mass, nw.massX, nw.massY)}
          else {traverse(nw)}

          val dist2 = distance(x,y, ne.centerX, ne.centerY)
          if (quad.size / dist2 < theta) {addForce(ne.mass, ne.massX, ne.massY)}
          else {traverse(ne)}

          val dist3 = distance(x,y, sw.centerX, sw.centerY)
          if (quad.size / dist3 < theta) {addForce(sw.mass, sw.massX, sw.massY)}
          else {traverse(sw)}

          val dist4 = distance(x,y, se.centerX, se.centerY)
          if (quad.size / dist4 < theta) {addForce(se.mass, se.massX, se.massY)}
          else {traverse(se)}
//
//          val massX: Float = (nw.mass * nw.centerX + ne.mass * ne.centerX + sw.mass * sw.centerX + se.mass * se.centerX) / ( nw.mass + ne.mass + sw.mass + se.mass )
//          val massY
//          ce(x0: Float, y0: Float, x1: Float, y1: Float): Fl
//          f(centerX: Float, centerY: Float, size: Float, bodies: coll.Seq[
//          i nw
//          distance


//          val centerX: Float = ( nw.centerX + ne.centerX + sw.centerX + se.centerX ) / 4
//          val centerY: Float = ( nw.centerY + ne.centerY + sw.centerY + se.centerY )
//
//          quad.size / dist < theta
          // see if node is far enough from the body,
          // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) extends SectorMatrixInterface {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      def getPos(p1: Float, p2: Float):Int = {
        ((p1 - p2) / sectorSize).toInt max 0 min sectorPrecision - 1
      }
      this(getPos(b.x,boundaries.minX),  getPos(b.y,boundaries.minY)) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- 0 until matrix.length) {
//        matrix(i) combine that.matrix(i)
        matrix.update(i, matrix(i).combine(that.matrix(i)))
      }
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}
