package io.github.scala_tessella
package dcel

import spire.compat.numeric
import spire.implicits.*
import spire.math.Rational

import scala.annotation.targetName
import scala.collection.mutable
import scala.util.boundary

/**
 * Planar geometry toolbox using Spire for precise calculations.
 *
 * This object provides an alternative to [[Geometry]], using Spire's numeric types like [[spire.math.BigDecimal]]
 * to avoid floating-point inaccuracies.
 */
object BigDecimalGeometry:

  val ACCURACY = 1.0E-12

  opaque type AngleDegree = Rational

  object AngleDegree:

    def apply(r: Rational): AngleDegree =
      r

    def apply(i: Int): AngleDegree =
      Rational(i)

    /**
     * Calculates the value of a single interior angle of a regular n-gon.
     *
     * @param sides The number of sides (and vertices) of the regular polygon.
     * @return The interior angle in degrees.
     */
    def regularPolygonInteriorAngle(sides: Int): AngleDegree =
      AngleDegree(180) * (sides - 2) / sides

    /**
     * Validates the list of interior angles for a simple polygon.
     * It checks that no angle is a full circle and that the sum of the angles is correct.
     *
     * @param angles A list of interior angles in degrees.
     * @return Either a String with an error message, or Unit if validation succeeds.
     */
    def validatePolygonAngles(angles: List[AngleDegree]): Either[String, Unit] =
      val n = angles.length
      if angles.exists(_.isFullCircle) then
        Left("The polygon cannot have full circles as interior angles.")
      else
        val angleSum = angles.map(_.normalised.toRational).sum
        val expectedAngleSum = AngleDegree(180) * (n - 2)
        if (angleSum - expectedAngleSum.toRational).abs > ACCURACY then
          Left(f"The sum of interior angles is incorrect for a polygon with $n sides. Expected ${expectedAngleSum.toRational.toDouble}%.2f, but got ${angleSum.toDouble}%.2f.")
        else
          Right(())

  extension (d: AngleDegree)

    def toRational: Rational =
      d

    def toBigRadian: BigRadian =
      BigDecimal(spire.math.pi) * (d / 180).toDouble

    def normalised: AngleDegree =
      d.toRational.fmod(Rational(360))

    def isFullCircle: Boolean =
      normalised == Rational(0)

    def inverted: AngleDegree =
      -d

    @targetName("plusDegree")
    def +(that: AngleDegree): AngleDegree =
      d.toRational + that

    @targetName("minusDegree")
    def -(that: AngleDegree): AngleDegree =
      d.toRational - that

    @targetName("timesInt")
    def *(int: Int): AngleDegree =
      d.toRational * int

    @targetName("divideInt")
    def /(int: Int): AngleDegree =
      d.toRational / int

  /** Standard unit of angular measure, represented by a [[spire.math.BigDecimal]]. */
  opaque type BigRadian = BigDecimal

  /** Companion object for [[BigRadian]] */
  object BigRadian:
    /** Create a [[BigRadian]] from a `Double` */
    def apply(d: Double): BigRadian = BigDecimal(d)

    /** Create a [[BigRadian]] from a `BigDecimal` */
    def apply(b: BigDecimal): BigRadian = b

    /** Tau (2 * Pi), the circle constant. [[https://tauday.com/]] */
    val TAU: BigRadian = BigDecimal(spire.math.pi) * 2
    /** Pi, half of Tau. */
    val TAU_2: BigRadian = BigDecimal(spire.math.pi)
    val TAU_3: BigRadian = TAU / 3
    /** Half of Pi. */
    val TAU_4: BigRadian = BigDecimal(spire.math.pi) / 2
    val TAU_6: BigRadian = TAU_2 / 3

  extension (r: BigRadian)
    /** @return the underlying `BigDecimal` */
    def toBigDecimal: BigDecimal =
      r

    @targetName("plus")
    def +(that: BigRadian): BigRadian = r.toBigDecimal + that

    @targetName("minus")
    def -(that: BigRadian): BigRadian = r.toBigDecimal - that

    @targetName("times")
    def *(i: Int): BigRadian = r.toBigDecimal * i

    @targetName("divide")
    def /(i: Int): BigRadian = r.toBigDecimal / i

    /** Tests whether this `SpireRadian` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: BigRadian, accuracy: Double = ACCURACY): Boolean =
      (r - that).abs < BigDecimal(accuracy)

  /** A point in the plane defined by its 2 Cartesian coordinates x and y using [[spire.math.BigDecimal]]. */
  case class BigPoint(x: BigDecimal, y: BigDecimal):

    /** Sum of two points */
    def plus(that: BigPoint): BigPoint =
      BigPoint(this.x + that.x, this.y + that.y)

    /** Tests whether this `BigPoint` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: BigPoint, accuracy: Double = ACCURACY): Boolean =
      (this.x - that.x).abs < BigDecimal(accuracy) && (this.y - that.y).abs < BigDecimal(accuracy)

    /** New point moved by polar coordinates */
    def plusPolar(rho: BigDecimal)(theta: BigRadian): BigPoint =
      plus(BigPoint.createPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: BigRadian => BigPoint =
      plusPolar(BigDecimal(1.0))

    /** Calculates the horizontal angle of the vector from this point to another point. */
    def angleTo(other: BigPoint): BigRadian =
      BigLineSegment(this, other).horizontalAngle

    /** Calculates the distance to another point. */
    def distanceTo(other: BigPoint): BigDecimal =
      BigLineSegment(this, other).length

  object BigPoint:
    /** Creates a point at origin (0,0) */
    def apply(): BigPoint = BigPoint(BigDecimal(0), BigDecimal(0))

    /** Creates a point from polar coordinates */
    def createPolar(rho: BigDecimal, theta: BigRadian): BigPoint =
      BigPoint(rho * spire.math.cos(theta), rho * spire.math.sin(theta))

    /**
     * Finds the orientation of the ordered triplet (p, q, r).
     *
     * @return 0 if points are collinear, 1 if clockwise, 2 if counterclockwise
     */
    def orientation(p: BigPoint, q: BigPoint, r: BigPoint): Int =
      val v = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
      if spire.math.abs(v) < ACCURACY then 0 // Collinear
      else if (v > 0) then 1 // Clockwise
      else 2 // Counterclockwise

    /**
     * Checks if point q lies on segment pr, assuming they are collinear.
     */
    def onSegment(p: BigPoint, q: BigPoint, r: BigPoint): Boolean =
      q.x <= spire.math.max(p.x, r.x) && q.x >= spire.math.min(p.x, r.x)
        && q.y <= spire.math.max(p.y, r.y) && q.y >= spire.math.min(p.y, r.y)

    /**
     * Checks if a list of points contains any pair of `almostEquals` points at a given accuracy.
     *
     * This method uses a grid-based approach (spatial hashing) for efficient checking.
     * Its performance is typically O(n) for uniformly distributed data, which is much
     * faster than a naive O(n^2) pair-wise comparison. In the worst case (all points
     * in the same grid cell), performance degrades to O(n^2).
     *
     * The algorithm partitions the 2D space into a grid of cells, where each cell's
     * dimension is determined by the `accuracy`. Each point is placed into a cell.
     * To check for duplicates, each point only needs to be compared with other points
     * in its own cell and the eight adjacent cells.
     *
     * @param points   The list of points to check.
     * @param accuracy The tolerance value. Two points are `almostEquals` if their x and y
     *                 coordinate differences are both less than this value.
     * @return `true` if no two points are almost equal, `false` otherwise.
     */
    def hasNoAlmostEqualPoints(points: List[BigPoint], accuracy: Double = ACCURACY): Boolean =
      if (points.length < 2) return true

      // Accuracy must be positive for the grid logic to work.
      val bigDecimalAccuracy = BigDecimal(accuracy).abs

      val grid = mutable.Map.empty[(Long, Long), mutable.ListBuffer[BigPoint]]

      boundary:
        for (p <- points)
          val cellX = (p.x / bigDecimalAccuracy).toLong
          val cellY = (p.y / bigDecimalAccuracy).toLong

          // Check current and 8 neighboring cells for almost equal points.
          for (i <- -1 to 1; j <- -1 to 1)
            val key = (cellX + i, cellY + j)
            grid.get(key) match
              case Some(neighbors) =>
                if neighbors.exists(_.almostEquals(p, accuracy)) then
                  boundary.break(false)
              case None => ()

          // Add the current point to its cell in the grid.
          val cellKey = (cellX, cellY)
          grid.getOrElseUpdate(cellKey, scala.collection.mutable.ListBuffer.empty).append(p)

        true

    /**
     * Checks if a polygon defined by a list of points is simple (does not self-intersect).
     */
    def isSimple(points: List[BigPoint]): Boolean =

      val n = points.length
      if n < 4 then return true // Triangles cannot self-intersect

      val segments = (0 until n).map(i => BigLineSegment(points(i), points((i + 1) % n))).toList

      boundary:
        for i <- 0 until n do
          for j <- i + 1 until n do
            val s1 = segments(i)
            val s2 = segments(j)

            // Non-adjacent segments
            if i != (j + 1) % n && j != (i + 1) % n then
              if BigLineSegment.doIntersect(s1, s2) then boundary.break(false)
        true

  /** A line segment in the plane defined by its 2 endpoints using [[spire.math.BigDecimal]]. */
  case class BigLineSegment(p1: BigPoint, p2: BigPoint):
    /** The length of the line segment. */
    lazy val length: BigDecimal =
      spire.math.sqrt((p2.x - p1.x).pow(2) + (p2.y - p1.y).pow(2))

    def midPoint: BigPoint =
      BigPoint((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)

    /** The horizontal angle of the line segment. */
    def horizontalAngle: BigRadian =
      spire.math.atan2(p2.y - p1.y, p2.x - p1.x)

  object BigLineSegment:

    /**
     * Checks if two line segments, s1 and s2, intersect.
     */
    def doIntersect(s1: BigLineSegment, s2: BigLineSegment): Boolean =
      val o1 = BigPoint.orientation(s1.p1, s1.p2, s2.p1)
      val o2 = BigPoint.orientation(s1.p1, s1.p2, s2.p2)
      val o3 = BigPoint.orientation(s2.p1, s2.p2, s1.p1)
      val o4 = BigPoint.orientation(s2.p1, s2.p2, s1.p2)

      // General case: segments cross each other
      if o1 != 0 && o2 != 0 && o3 != 0 && o4 != 0 then
        o1 != o2 && o3 != o4
      // Special Cases for collinear points
      else
        (o1 == 0 && BigPoint.onSegment(s1.p1, s2.p1, s1.p2))
          || (o2 == 0 && BigPoint.onSegment(s1.p1, s2.p2, s1.p2))
          || (o3 == 0 && BigPoint.onSegment(s2.p1, s1.p1, s2.p2))
          || (o4 == 0 && BigPoint.onSegment(s2.p1, s1.p2, s2.p2))

  case class BigBox(x0: BigDecimal, x1: BigDecimal, y0: BigDecimal, y1: BigDecimal):

    def contains(point: BigPoint): Boolean =
      if point.x < x0 then false
      else if point.y < y0 then false
      else if point.x > x1 then false
      else !(point.y > y1)

    def enlarge(r: BigDecimal): BigBox =
      BigBox(x0 - r, x1 + r, y0 - r, y1 + r)
