package io.github.scala_tessella.dcel.geometry

import io.github.scala_tessella.dcel.geometry.{BigBox, BigLineSegment, BigPoint}

import scala.collection.mutable

/** Planar geometry toolbox using Spire for precise calculations.
  *
  * This object uses Spire's numeric types to avoid floating-point inaccuracies.
  */
object BigDecimalGeometry:

  val ACCURACY: Double   = 1.0e-10
  val BigAcc: BigDecimal = BigDecimal(ACCURACY)

  extension (bigDecimal: BigDecimal)

    def almostEqual(other: BigDecimal): Boolean =
      (bigDecimal - other).abs <= BigAcc

    /** Formats a decimal number to a maximum of 6 decimal places, removing trailing zeros */
    def format: String =
      val formatted = f"${bigDecimal.toDouble}%.6f".replaceAll(",", ".")
      if formatted.contains('.') then
        formatted.replaceAll("0+$", "").replaceAll("\\.$", "")
      else
        formatted

  enum Orientation:
    case Collinear, Clockwise, Counterclockwise

  /** A spatial grid for efficient line segment intersection detection. Divides the 2D space into cells and
    * allows for faster neighbor queries.
    */
  class SpatialGrid(bounds: BigBox, cellSize: BigDecimal):

    private val minX = bounds.min.x
    private val minY = bounds.min.y
    private val maxX = bounds.max.x
    private val maxY = bounds.max.y

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
      cells.foreach:
        (row, col) => grid(row)(col) += segment

    /** Adds multiple line segments to the grid
      */
    def addSegments(segments: Seq[BigLineSegment]): Unit =
      segments.foreach: 
        addSegment

    /** Finds all segments in the grid that could potentially intersect the given segment
      */
    def getPotentialIntersections(segment: BigLineSegment): Set[BigLineSegment] =
      val cells      = getCellsForSegment(segment)
      val candidates = mutable.Set.empty[BigLineSegment]

      cells.foreach:
        (row, col) => candidates ++= grid(row)(col)

      candidates.toSet

  /** Methods for detecting intersections between collections of line segments with spatial partitioning
    */
  object IntersectionDetection:

    private def smallerAndSpatialGrid(
        segments1: Seq[BigLineSegment],
        segments2: Seq[BigLineSegment],
        cellSize: Option[BigDecimal]
    ): (Seq[BigLineSegment], SpatialGrid) =

      // Determine cell size - if not provided, estimate based on average segment length
      val actualCellSize = cellSize.getOrElse:
        val avgLength =
          (segments1.totalLength + segments2.totalLength) / (segments1.length + segments2.length)

        // Cell size should be larger than the average segment to reduce redundant checks
        avgLength * 2

      // Create a bounding box for all segments
      val allPoints = segments1.toPoints ++ segments2.toPoints
      val bounds    = BigBox.fromPoints(allPoints).expand(actualCellSize)

      // Create a spatial grid and add the larger collection
      val (smaller, larger) =
        if segments1.length <= segments2.length then (segments1, segments2) else (segments2, segments1)

      val grid = SpatialGrid(bounds, actualCellSize)
      grid.addSegments(larger)

      (smaller, grid)

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
      if segments1.isEmpty || segments2.isEmpty then
        false
      else

        // If both collections are very small, use a brute force approach
        if segments1.length * segments2.length <= 100 then
          segments1.exists: s1 =>
            segments2.exists: s2 =>
              s1.properlyIntersects(s2)
        else

          val (smaller, grid) = smallerAndSpatialGrid(segments1, segments2, cellSize)

          // Check for intersections by only comparing segments from the smaller collection
          // with potentially intersecting segments from the larger collection
          smaller.exists: segment =>
            val candidateSegments = grid.getPotentialIntersections(segment)
            candidateSegments.exists:
              segment.properlyIntersects

    def properIntersections(
        segments1: Seq[BigLineSegment],
        segments2: Seq[BigLineSegment],
        cellSize: Option[BigDecimal] = None
    ): List[(BigLineSegment, BigLineSegment)] =
      // Empty case handling
      if segments1.isEmpty || segments2.isEmpty then
        Nil
      else
        // If both collections are small, use a brute force approach
        if segments1.length * segments2.length <= 100 then
          (for
            s1 <- segments1
            s2 <- segments2
            if s1.properlyIntersects(s2)
          yield (s1, s2)).toList
        else

          val (smaller, grid) = smallerAndSpatialGrid(segments1, segments2, cellSize)

          // Collect all proper intersections avoiding duplicates
          val intersections = scala.collection.mutable.ListBuffer.empty[(BigLineSegment, BigLineSegment)]
          val seen          = scala.collection.mutable.HashSet.empty[(BigLineSegment, BigLineSegment)]

          smaller.foreach: segment =>
            val candidateSegments = grid.getPotentialIntersections(segment)
            candidateSegments.foreach: candidateSegment =>
              if segment.properlyIntersects(candidateSegment) then
                val pair = (segment, candidateSegment)
                if !seen.contains(pair) then
                  seen += pair
                  intersections += pair

          intersections.toList

    /** Efficiently checks if a single collection of segments contains any internal proper intersections.
      * Skips checks between segments that share an endpoint.
      */
    def hasSelfIntersection(
        segments: Seq[BigLineSegment],
        cellSize: Option[BigDecimal] = None
    ): Boolean =
      if segments.length < 2 then return false

      val actualCellSize = cellSize.getOrElse(segments.totalLength / segments.length * 2)
      val bounds         = BigBox.fromPoints(segments.toPoints).expand(actualCellSize)
      val grid           = SpatialGrid(bounds, actualCellSize)

      // To avoid O(N^2), we add segments one by one and check against already added ones
      segments.exists: segment =>
        val candidates = grid.getPotentialIntersections(segment)
        val found      =
          candidates.exists:
            _.properlyIntersects(segment)
        grid.addSegment(segment)
        found
