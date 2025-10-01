package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geo.BigRadian
import spire.compat.numeric
import spire.implicits.*

import scala.collection.mutable
import scala.util.boundary

/** Planar geometry toolbox using Spire for precise calculations.
  *
  * This object uses Spire's numeric types to avoid floating-point inaccuracies.
  */
object BigDecimalGeometry:

  extension (bigDecimal: BigDecimal)

    /** Formats a decimal number to a maximum of 6 decimal places, removing trailing zeros */
    def format: String =
      val formatted = f"${bigDecimal.toDouble}%.6f".replaceAll(",", ".")
      if formatted.contains('.') then
        formatted.replaceAll("0+$", "").replaceAll("\\.$", "")
      else
        formatted

  val ACCURACY = 1.0e-12

  /** A point in the plane defined by its 2 Cartesian coordinates x and y. */
  case class BigPoint(x: BigDecimal, y: BigDecimal):

    /** Sum of two points */
    def plus(that: BigPoint): BigPoint =
      BigPoint(this.x + that.x, this.y + that.y)

    /** Tests whether this `BigPoint` is approximately equal to another, within given accuracy. */
    def almostEquals(that: BigPoint, accuracy: Double = ACCURACY): Boolean =
      (this.x - that.x).abs < BigDecimal(accuracy) && (this.y - that.y).abs < BigDecimal(accuracy)

    /** New point moved by polar coordinates */
    def plusPolar(rho: BigDecimal)(theta: BigRadian): BigPoint =
      plus(BigPoint.fromPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: BigRadian => BigPoint =
      plusPolar(BigDecimal(1.0))

    /** Calculates the horizontal angle of the vector from this point to another point. */
    def angleTo(other: BigPoint): BigRadian =
      BigLineSegment(this, other).horizontalAngle

    /** Calculates the distance to another point. */
    def distanceTo(other: BigPoint): BigDecimal =
      BigLineSegment(this, other).length

    def scaled(scale: Double): BigPoint =
      BigPoint(x * scale, y * scale)

    def flippedY: BigPoint =
      BigPoint(x, -y)

  enum Orientation:
    case Collinear, Clockwise, Counterclockwise

  object BigPoint:
    /** Creates a point at origin (0,0) */
    def apply(): BigPoint = BigPoint(BigDecimal(0), BigDecimal(0))

    /** Creates a point from polar coordinates */
    def fromPolar(rho: BigDecimal, theta: BigRadian): BigPoint =
      BigPoint(rho * spire.math.cos(theta.toBigDecimal), rho * spire.math.sin(theta.toBigDecimal))

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

  /** A spatial grid for efficient line segment intersection detection. Divides the 2D space into cells and
    * allows for faster neighbor queries.
    */
  class SpatialGrid(bounds: BigBox, cellSize: BigDecimal):

    private val minX = bounds.minX
    private val minY = bounds.minY
    private val maxX = bounds.maxX
    private val maxY = bounds.maxY

    private val width  = maxX - minX
    private val height = maxY - minY

    private val numCols = math.max(1, math.ceil((width / cellSize).toDouble).toInt)
    private val numRows = math.max(1, math.ceil((height / cellSize).toDouble).toInt)

    private val grid = Array.ofDim[mutable.Set[BigLineSegment]](numRows, numCols)

    // Initialize the grid with empty sets
    for
      row <- 0 until numRows
      col <- 0 until numCols
    do
      grid(row)(col) = mutable.Set.empty[BigLineSegment]

    /** Converts a point to grid cell coordinates
      */
    private def cellCoordinates(point: BigPoint): (Int, Int) =
      val col = math.min(math.max(((point.x - minX) / cellSize).toInt, 0), numCols - 1)
      val row = math.min(math.max(((point.y - minY) / cellSize).toInt, 0), numRows - 1)
      (row, col)

    /** Gets all cells that a line segment crosses
      */
    private def getCellsForSegment(segment: BigLineSegment): Seq[(Int, Int)] =
      val (row1, col1) = cellCoordinates(segment.p1)
      val (row2, col2) = cellCoordinates(segment.p2)

      // Bresenham-like algorithm to determine cells crossed by the segment
      val cells = mutable.ArrayBuffer[(Int, Int)]()

      // Always include the start and end cells
      cells += ((row1, col1))

      if row1 != row2 || col1 != col2 then
        cells += ((row2, col2))

        // For non-trivial segments, determine intermediate cells
        val dx = (segment.p2.x - segment.p1.x).abs
        val dy = (segment.p2.y - segment.p1.y).abs

        // Only add more cells for longer segments
        if dx > cellSize || dy > cellSize then
          // Simple approximation: add all cells in the bounding box
          val minRow = math.min(row1, row2)
          val maxRow = math.max(row1, row2)
          val minCol = math.min(col1, col2)
          val maxCol = math.max(col1, col2)

          for
            row <- minRow to maxRow
            col <- minCol to maxCol
            if (row, col) != (row1, col1) && (row, col) != (row2, col2)
          do
            cells += ((row, col))

      cells.distinct.toSeq

    /** Adds a line segment to the grid
      */
    def addSegment(segment: BigLineSegment): Unit =
      val cells = getCellsForSegment(segment)
      cells.foreach { case (row, col) =>
        grid(row)(col) += segment
      }

    /** Adds multiple line segments to the grid
      */
    def addSegments(segments: Seq[BigLineSegment]): Unit =
      segments.foreach(addSegment)

    /** Finds all segments in the grid that could potentially intersect the given segment
      */
    def getPotentialIntersections(segment: BigLineSegment): Set[BigLineSegment] =
      val cells      = getCellsForSegment(segment)
      val candidates = mutable.Set.empty[BigLineSegment]

      cells.foreach { case (row, col) =>
        candidates ++= grid(row)(col)
      }

      candidates.toSet

  /** Methods for detecting intersections between collections of line segments with spatial partitioning
    */
  object IntersectionDetection:

    /** Checks if there are any proper intersections between two collections of line segments. Uses spatial
      * partitioning for better performance with large collections.
      *
      * @param segments1
      *   First collection of line segments
      * @param segments2
      *   Second collection of line segments
      * @param cellSize
      *   Size of each grid cell for spatial partitioning (auto-calculated if None)
      * @return
      *   true if any segment from segments1 properly intersects any segment from segments2
      */
    def hasProperIntersection(
        segments1: Seq[BigLineSegment],
        segments2: Seq[BigLineSegment],
        cellSize: Option[BigDecimal] = None
    ): Boolean =
      // Empty case handling
      if segments1.isEmpty || segments2.isEmpty then return false

      // If both collections are very small, use brute force approach
      if segments1.length * segments2.length <= 100 then
        return segments1.exists(s1 => segments2.exists(s2 => s1.properlyIntersects(s2)))

      // Determine cell size - if not provided, estimate based on average segment length
      val actualCellSize = cellSize.getOrElse {
        val avgLength =
          (
            segments1.map(s => s.p1.distanceTo(s.p2)).sum +
              segments2.map(s => s.p1.distanceTo(s.p2)).sum
          ) / (segments1.length + segments2.length)

        // Cell size should be larger than average segment to reduce redundant checks
        avgLength * 2
      }

      // Create bounding box for all segments
      val allPoints = segments1.flatMap(s => List(s.p1, s.p2)) ++ segments2.flatMap(s => List(s.p1, s.p2))
      val bounds    = BigBox.fromPoints(allPoints).expand(actualCellSize)

      // Create spatial grid and add the larger collection
      val (smaller, larger) =
        if segments1.length <= segments2.length then (segments1, segments2) else (segments2, segments1)

      val grid = SpatialGrid(bounds, actualCellSize)
      grid.addSegments(larger)

      // Check for intersections by only comparing segments from smaller collection
      // with potentially intersecting segments from the larger collection
      smaller.exists { segment =>
        val candidates = grid.getPotentialIntersections(segment)
        candidates.exists(candidate => segment.properlyIntersects(candidate))
      }

  /** A line segment in the plane defined by its 2 endpoints. */
  case class BigLineSegment(p1: BigPoint, p2: BigPoint):
    /** The length of the line segment. */
    lazy val length: BigDecimal =
      spire.math.sqrt((p2.x - p1.x).pow(2) + (p2.y - p1.y).pow(2))

    def midPoint: BigPoint =
      BigPoint((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)

    /** The horizontal angle of the line segment. */
    def horizontalAngle: BigRadian =
      BigRadian(spire.math.atan2(p2.y - p1.y, p2.x - p1.x))

    private def orientations(that: BigLineSegment): (Orientation, Orientation, Orientation, Orientation) =
      val o1 = BigPoint.orientation(this.p1, this.p2, that.p1)
      val o2 = BigPoint.orientation(this.p1, this.p2, that.p2)
      val o3 = BigPoint.orientation(that.p1, that.p2, this.p1)
      val o4 = BigPoint.orientation(that.p1, that.p2, this.p2)
      (o1, o2, o3, o4)

    /** Checks if this bounding box intersects with another one. */
    def intersects(that: BigLineSegment): Boolean =
      val (o1, o2, o3, o4) = orientations(that)

      // General case: segments cross each other
      if o1 != Orientation.Collinear
        && o2 != Orientation.Collinear
        && o3 != Orientation.Collinear
        && o4 != Orientation.Collinear
      then
        o1 != o2 && o3 != o4
      // Special Cases for collinear points
      else
        (o1 == Orientation.Collinear && BigPoint.onSegment(this.p1, that.p1, this.p2))
        || (o2 == Orientation.Collinear && BigPoint.onSegment(this.p1, that.p2, this.p2))
        || (o3 == Orientation.Collinear && BigPoint.onSegment(that.p1, this.p1, that.p2))
        || (o4 == Orientation.Collinear && BigPoint.onSegment(that.p1, this.p2, that.p2))

    def properlyIntersects(that: BigLineSegment): Boolean =

      // If segments share an endpoint, it's not a proper intersection
      val thisPoints = Set(this.p1, this.p2)
      val thatPoints = Set(that.p1, that.p2)

      if thisPoints.exists(p1 => thatPoints.exists(p2 => p1.almostEquals(p2))) then
        false
      else
        val (o1, o2, o3, o4) = orientations(that)
        // General case: segments cross each other in their interiors
        o1 != o2 && o3 != o4

  extension (segments: List[BigLineSegment])

    /** Checks if this list of segments has any proper intersections with another list. Uses spatial
      * partitioning for better performance.
      *
      * @param other
      *   another list of segments
      * @param cellSize
      *   Size of each grid cell for spatial partitioning, defaulted to 2 that is double of unit segment
      */
    def hasProperIntersections(other: List[BigLineSegment], cellSize: Option[BigDecimal] = Some(2)): Boolean =
      IntersectionDetection.hasProperIntersection(segments, other, cellSize)

  case class BigBox(minX: BigDecimal, minY: BigDecimal, maxX: BigDecimal, maxY: BigDecimal):

    def contains(point: BigPoint): Boolean =
      if point.x < minX then false
      else if point.y < minY then false
      else if point.x > maxX then false
      else !(point.y > maxY)

    /** Checks if this bounding box intersects with another one. */
    def intersects(that: BigBox): Boolean =
      !(that.minX > this.maxX || that.maxX < this.minX || that.minY > this.maxY || that.maxY < this.minY)

    /** Expands the bounding box by a given amount in all directions. */
    def expand(by: BigDecimal): BigBox =
      BigBox(minX - by, minY - by, maxX + by, maxY + by)

  object BigBox:
    /** Creates a BoundingBox that encloses a collection of points. */
    def fromPoints(points: Iterable[BigPoint]): BigBox =
      if points.isEmpty then BigBox(0, 0, 0, 0)
      else
        val xs = points.map(_.x)
        val ys = points.map(_.y)
        BigBox(xs.min, ys.min, xs.max, ys.max)

    /** Creates a BoundingBox for a single line segment. */
    def fromSegment(segment: BigLineSegment): BigBox =
      fromPoints(List(segment.p1, segment.p2))
