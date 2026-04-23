package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.{ACCURACY, Orientation, almostEqual}
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

  /** Creates a point from polar coordinates.
    *
    * ADR-0009 candidate A: uses `java.lang.Math.{cos,sin}` on `Double` rather than `spire.math.*` on
    * `BigDecimal`. The `BigDecimal` trig pipeline was ~1000× slower and offered no precision benefit —
    * `AngleDegree.toBigRadian` already capped the input at `Double` (ADR finding 1). The returned coordinates
    * are still `BigDecimal`-typed so downstream arithmetic (accumulation in `unitPath`, orientation /
    * distance tests) is unchanged.
    */
  def fromPolar(rho: BigDecimal, theta: BigRadian): BigPoint =
    val t = theta.toDouble
    val r = rho.toDouble
    (BigDecimal(r * Math.cos(t)), BigDecimal(r * Math.sin(t)))

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
    val acc = BigDecimal(ACCURACY)
    q.x <= spire.math.max(p.x, r.x) + acc && q.x >= spire.math.min(p.x, r.x) - acc
    && q.y <= spire.math.max(p.y, r.y) + acc && q.y >= spire.math.min(p.y, r.y) - acc

  /** A point in the plane defined by its 2 Cartesian coordinates x and y. */
  extension (point: BigPoint)

    inline def x: BigDecimal =
      point.x

    inline def y: BigDecimal =
      point.y

    private def zipWith(op: (BigDecimal, BigDecimal) => BigDecimal)(that: BigPoint): BigPoint =
      (op(point.x, that.x), op(point.y, that.y))

    private def zipWith(op: (BigDecimal, BigDecimal) => BigDecimal)(value: BigDecimal): BigPoint =
      (op(point.x, value), op(point.y, value))

    // Provide common vector-like ops and keep names consistent
    def +(that: BigPoint): BigPoint =
      zipWith(_ + _)(that)

    def -(that: BigPoint): BigPoint =
      zipWith(_ - _)(that)

    def /(divisor: BigDecimal): BigPoint =
      zipWith(_ / _)(divisor)

    def dot(that: BigPoint): BigDecimal =
      point.x * that.x + point.y * that.y

    def cross(that: BigPoint): BigDecimal =
      point.x * that.y - point.y * that.x

    /** Tests whether this `BigPoint` is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigPoint, accuracy: Double = ACCURACY): Boolean =
      val acc = BigDecimal(accuracy)
      (point.x - that.x).abs < acc && (point.y - that.y).abs < acc

    /** New point moved by polar coordinates */
    def plusPolar(rho: BigDecimal)(theta: BigRadian): BigPoint =
      point + BigPoint.fromPolar(rho, theta)

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
      distanceTo(other).almostEqual(BigDecimal(1.0))

    def scaled(scale: BigDecimal): BigPoint =
      point.zipWith(_ * _)(scale)

    def flippedY: BigPoint =
      (point.x, -point.y)

  extension (points: List[BigPoint])

    def centroid: BigPoint =
      points match
        case Nil      => BigPoint.origin
        case p :: Nil => p
        case _        =>
          val len    = BigDecimal(points.length)
          val summed =
            points.foldLeft(BigPoint.origin):
              _ + _
          summed / len

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
                    if neighbors.exists: neighborBigPoint =>
                        neighborBigPoint.almostEquals(p, accuracy)
                    then
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
      if !hasNoAlmostEqualPoints() then return false

      val segments =
        (0 until n)
          .iterator.map: i =>
            BigLineSegment(points(i), points((i + 1) % n))
          .toArray

      boundary:
        var i = 0
        while i < n do
          var j = i + 1
          while j < n do
            // Non-adjacent segments; also exclude first and last which share a vertex
            if i != (j + 1) % n && j != (i + 1) % n && !(i == 0 && j == n - 1) then
              if segments(i).interiorIntersects(segments(j)) then boundary.break(false)
            j += 1
          i += 1
        true

    // Shoelace formula
    def area: BigDecimal =
      if points.size < 3 then
        BigDecimal(0)
      else
        val sum =
          points.slidingO(2)
            .map:
              (_: @unchecked) match
                case p1 :: p2 :: Nil => p1.x * p2.y - p2.x * p1.y
            .sum
        sum.abs / 2
