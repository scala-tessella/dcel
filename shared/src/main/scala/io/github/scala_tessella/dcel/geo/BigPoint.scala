package io.github.scala_tessella.dcel.geo

import BigDecimalGeometry.{ACCURACY, Orientation}
import io.github.scala_tessella.dcel.geo.BigLineSegment
import spire.implicits.*

import scala.collection.mutable
import scala.util.boundary

opaque type BigPoint = (x: BigDecimal, y: BigDecimal)

object BigPoint:

  inline def apply(x: BigDecimal, y: BigDecimal): BigPoint =
    (x, y)

  /** Creates a point at origin (0,0) */
  def apply(): BigPoint = (BigDecimal(0), BigDecimal(0))

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

    /** Sum of two points */
    def plus(that: BigPoint): BigPoint =
      (point.x + that.x, point.y + that.y)

    /** Tests whether this `BigPoint` is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigPoint, accuracy: Double = ACCURACY): Boolean =
      (point.x - that.x).abs < BigDecimal(accuracy) && (point.y - that.y).abs < BigDecimal(accuracy)

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

    def scaled(scale: Double): BigPoint =
      (point.x * scale, point.y * scale)

    def flippedY: BigPoint =
      (point.x, -point.y)

  extension (points: List[BigPoint])

    def centroid: BigPoint =
      if points.nonEmpty then
        val sumX = points.map(_.x).sum
        val sumY = points.map(_.y).sum
        BigPoint(sumX / points.length, sumY / points.length)
      else
        BigPoint.apply() // origin (0,0)

    /** Checks if a list of points contains any pair of `almostEquals` points at given accuracy.
      *
      * This method uses a grid-based approach (spatial hashing) for efficient checking. Its performance is
      * typically O(n) for uniformly distributed data, which is much faster than a naive O(n^2) pair-wise
      * comparison. In the worst case (all points in the same grid cell), performance degrades to O(n^2).
      *
      * The algorithm partitions the 2D space into a grid of cells, where each cell's dimension is determined
      * by the `accuracy`. Each point is placed into a cell. To check for duplicates, each point only needs to
      * be compared with other points in its own cell and the eight adjacent cells.
      *
      * @param accuracy
      *   The tolerance value. Two points are `almostEquals` if their x and y coordinate differences are both
      *   less than this value.
      * @return
      *   `true` if no two points are almost equal, `false` otherwise.
      */
    def hasNoAlmostEqualPoints(accuracy: Double = ACCURACY): Boolean =
      if points.length < 2 then return true

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
              case None            => ()

          // Add the current point to its cell in the grid.
          val cellKey = (cellX, cellY)
          grid.getOrElseUpdate(cellKey, scala.collection.mutable.ListBuffer.empty).append(p)

        true

    /** Checks if a polygon defined by a list of points is simple (does not self-intersect).
      */
    def isSimplePolygon: Boolean =

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
              if s1.intersects(s2) then boundary.break(false)
        true
