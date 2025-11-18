package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.{ACCURACY, Orientation}
import io.github.scala_tessella.dcel.geometry.BigLineSegment
import io.github.scala_tessella.ring_seq.RingSeq.slidingO
import spire.implicits.*

import scala.collection.mutable
import scala.util.boundary

opaque type BigPoint = (x: BigDecimal, y: BigDecimal)

object BigPoint:

  val origin: BigPoint = (BigDecimal(0), BigDecimal(0))

  inline def apply(x: BigDecimal, y: BigDecimal): BigPoint =
    (x, y)

  /** Creates a point from polar coordinates */
  def fromPolar(rho: BigDecimal, theta: BigRadian): BigPoint =
    (rho * spire.math.cos(theta.toBigDecimal), rho * spire.math.sin(theta.toBigDecimal))

  /** Finds the orientation of the ordered triplet (p, q, r).
    *
    * @return
    *   0 if points are collinear, 1 if are clockwise, 2 if are counterclockwise
    */
  def orientation(p: BigPoint, q: BigPoint, r: BigPoint): Orientation =
    val v = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
    if spire.math.abs(v) < ACCURACY then Orientation.Collinear
    else if v > 0 then Orientation.Clockwise
    else Orientation.Counterclockwise

  /** Checks if point q lies on segment pr, assuming they are collinear.
    */
  def onSegment(p: BigPoint, q: BigPoint, r: BigPoint): Boolean =
    q.x <= spire.math.max(p.x, r.x) && q.x >= spire.math.min(p.x, r.x)
      && q.y <= spire.math.max(p.y, r.y) && q.y >= spire.math.min(p.y, r.y)

  /** A point in the plane defined by its 2 Cartesian coordinates x and y. */
  extension (point: BigPoint)

    inline def x: BigDecimal =
      point.x

    inline def y: BigDecimal =
      point.y

    // Provide common vector-like ops and keep names consistent
    def +(that: BigPoint): BigPoint =
      (point.x + that.x, point.y + that.y)

    def -(that: BigPoint): BigPoint =
      (point.x - that.x, point.y - that.y)

    def dot(that: BigPoint): BigDecimal =
      point.x * that.x + point.y * that.y

    def cross(that: BigPoint): BigDecimal =
      point.x * that.y - point.y * that.x

    /** Sum of two points (kept for source compatibility, prefer +) */
    def plus(that: BigPoint): BigPoint =
      (point.x + that.x, point.y + that.y)

    /** Tests whether this `BigPoint` is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigPoint, accuracy: Double = ACCURACY): Boolean =
      val a = BigDecimal(accuracy)
      (point.x - that.x).abs < a && (point.y - that.y).abs < a

    /** New point moved by polar coordinates */
    def plusPolar(rho: BigDecimal)(theta: BigRadian): BigPoint =
      plus(BigPoint.fromPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: BigRadian => BigPoint =
      plusPolar(BigDecimal(1.0))

    /** Calculates the horizontal angle of the vector from this point to another point. */
    def angleTo(other: BigPoint): BigRadian =
      BigLineSegment(point, other).horizontalAngle

    /** Calculates the distance to another point. */
    def distanceTo(other: BigPoint): BigDecimal =
      BigLineSegment(point, other).length
      
    def hasUnitDistanceTo(other: BigPoint): Boolean =
      (distanceTo(other) - BigDecimal(1.0)).abs <= ACCURACY  

    def scaled(scale: Double): BigPoint =
      (point.x * scale, point.y * scale)

    def flippedY: BigPoint =
      (point.x, -point.y)

  extension (points: List[BigPoint])

    def centroid: BigPoint =
      if points.nonEmpty then
        // single pass reduce to avoid building intermediate lists
        val (sx, sy) = points.foldLeft((BigDecimal(0), BigDecimal(0))) { case ((ax, ay), p) =>
          (ax + p.x, ay + p.y)
        }
        BigPoint(sx / points.length, sy / points.length)
      else
        BigPoint.origin // origin (0,0)

    /** Checks if a list of points contains any pair of `almostEquals` points at given accuracy. */
    def hasNoAlmostEqualPoints(accuracy: Double = ACCURACY): Boolean =
      if points.length < 2 then return true

      val cellSize = BigDecimal(accuracy).abs
      if cellSize == 0 then
        // With zero tolerance, exact equality is required; use a Set for O(n)
        val seen = mutable.HashSet.empty[(BigDecimal, BigDecimal)]
        boundary:
          for p <- points do
            val key = (p.x, p.y)
            if seen.contains(key) then boundary.break(false)
            seen += key
          true
      else
        val grid = mutable.HashMap.empty[(Long, Long), mutable.ListBuffer[BigPoint]]

        boundary:
          for p <- points do
            val cx = (p.x / cellSize).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong
            val cy = (p.y / cellSize).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong

            // Check current and 8 neighboring cells for almost equal points.
            var found = false
            var i     = -1
            while i <= 1 && !found do
              var j = -1
              while j <= 1 && !found do
                grid.get((cx + i, cy + j)) match
                  case Some(neighbors) =>
                    if neighbors.exists(_.almostEquals(p, accuracy)) then
                      found = true
                  case None            => ()
                j += 1
              i += 1
            if found then boundary.break(false)

            grid.getOrElseUpdate((cx, cy), mutable.ListBuffer.empty).append(p)

          true

    /** Checks if a polygon defined by a list of points is simple (does not self-intersect).
      */
    def isSimplePolygon: Boolean =
      val n = points.length
      if n < 4 then return true // Triangles cannot self-intersect

      val segments = (0 until n).iterator.map(i => BigLineSegment(points(i), points((i + 1) % n))).toArray

      boundary:
        var i = 0
        while i < n do
          var j = i + 1
          while j < n do
            // Non-adjacent segments; also exclude first and last which share a vertex
            if i != (j + 1) % n && j != (i + 1) % n && !(i == 0 && j == n - 1) then
              if segments(i).intersects(segments(j)) then boundary.break(false)
            j += 1
          i += 1
        true

    // Shoelace formula
    def area: BigDecimal =
      if points.size < 3 then
        BigDecimal(0)
      else
        val sum =
          points.slidingO(2).map {
            (_: @unchecked) match
              case p1 :: p2 :: Nil => p1.x * p2.y - p2.x * p1.y
          }.sum
        sum.abs / 2
