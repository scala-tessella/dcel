package io.github.scala_tessella
package dcel

import scala.collection.mutable.ListBuffer
import scala.math.*

object TilingBuilder:

  // A small tolerance for floating-point comparisons
  private val Epsilon = 1E-9

  /**
   * Contains geometric helper types and functions.
   */
  private object Geometry:
    /**
     * Internal representation of a 2D point for geometric calculations.
     */
    case class Point(x: Double, y: Double)

    /**
     * Represents a line segment defined by two points.
     */
    case class Segment(p1: Point, p2: Point)

    /**
     * Finds the orientation of the ordered triplet (p, q, r).
     * @return 0 if points are collinear, 1 if clockwise, 2 if counterclockwise
     */
    private def orientation(p: Point, q: Point, r: Point): Int =
      val v = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
      if (abs(v) < Epsilon) 0 // Collinear
      else if (v > 0) 1       // Clockwise
      else 2                  // Counterclockwise

    /**
     * Checks if point q lies on segment pr, assuming they are collinear.
     */
    private def onSegment(p: Point, q: Point, r: Point): Boolean =
      q.x <= Math.max(p.x, r.x) && q.x >= Math.min(p.x, r.x) &&
        q.y <= Math.max(p.y, r.y) && q.y >= Math.min(p.y, r.y)

    /**
     * Checks if two line segments, s1 and s2, intersect.
     */
    private def doIntersect(s1: Segment, s2: Segment): Boolean =
      val o1 = orientation(s1.p1, s1.p2, s2.p1)
      val o2 = orientation(s1.p1, s1.p2, s2.p2)
      val o3 = orientation(s2.p1, s2.p2, s1.p1)
      val o4 = orientation(s2.p1, s2.p2, s1.p2)

      // General case: segments cross each other
      if o1 != 0 && o2 != 0 && o3 != 0 && o4 != 0 then
        o1 != o2 && o3 != o4
      // Special Cases for collinear points
      else
        (o1 == 0 && onSegment(s1.p1, s2.p1, s1.p2)) ||
          (o2 == 0 && onSegment(s1.p1, s2.p2, s1.p2)) ||
          (o3 == 0 && onSegment(s2.p1, s1.p1, s2.p2)) ||
          (o4 == 0 && onSegment(s2.p1, s1.p2, s2.p2))

    /**
     * Checks if a polygon defined by a list of points is simple (does not self-intersect).
     */
    def isSimple(points: List[Point]): Boolean =
      val n = points.length
      if n < 4 then return true // Triangles cannot self-intersect

      val segments = (0 until n).map(i => Segment(points(i), points((i + 1) % n))).toList

      for i <- 0 until n do
        for j <- i + 1 until n do
          val s1 = segments(i)
          val s2 = segments(j)

          // Non-adjacent segments
          if i != (j + 1) % n && j != (i + 1) % n then
            if doIntersect(s1, s2) then return false
      true

  /**
   * Creates a TilingDCEL for a single simple polygon with unit-length sides.
   *
   * @param angles A list of interior angles in degrees. The angles are ordered for a
   *               counter-clockwise traversal of the polygon boundary.
   * @return       Either a String explaining the validation error, or the successfully created TilingDCEL.
   */
  def createSimplePolygon(angles: List[Double]): Either[String, TilingDCEL] =
    val n = angles.length
    if n < 3 then
      return Left(s"A polygon must have at least 3 sides, but $n were specified.")

    // Preliminary check: The sum of the interior angles of a simple n-gon is (n-2) * 180 degrees.
    val angleSum = angles.sum
    val expectedAngleSum = (n - 2) * 180.0
    if abs(angleSum - expectedAngleSum) > Epsilon then
      return Left(f"The sum of interior angles is incorrect for a polygon with $n sides. Expected $expectedAngleSum%.2f, but got $angleSum%.2f.")

    // 1. First, validate the geometry and get vertex positions
    calculateVertexPoints(angles, performSimplicityCheck = true).flatMap(points =>
      // 2. If geometry is valid, construct the DCEL
      buildDCELFromPoints(points, angles)
    )

  /**
   * Creates a TilingDCEL for a single regular polygon with unit-length sides.
   *
   * @param sides The number of sides for the regular polygon.
   * @return      Either a String explaining the validation error, or the successfully created TilingDCEL.
   */
  def createRegularPolygon(sides: Int): Either[String, TilingDCEL] =
    if sides < 3 then
      return Left(s"A regular polygon must have at least 3 sides, but $sides were specified.")

    val angle = (sides - 2) * 180.0 / sides
    val angles = List.fill(sides)(angle)

    // Regular polygons are always simple, so we can skip the self-intersection check.
    // The angle sum is also correct by definition.
    calculateVertexPoints(angles, performSimplicityCheck = false).flatMap(points =>
      buildDCELFromPoints(points, angles)
    )

  /**
   * Given validated points and angles, builds the TilingDCEL structure.
   */
  private def buildDCELFromPoints(points: List[Geometry.Point], angles: List[Double]): Either[String, TilingDCEL] =
    val n = points.length

    // Create vertices from the calculated points
    val vertices = points.zipWithIndex.map { case (p, i) => Vertex(s"V$i", p.x, p.y) }

    // Create the two faces: one for the polygon, one for the outside
    val fPoly = Face("F_Poly")
    val fOuter = Face("F_Outer")

    // Create all inner and outer half-edges, indexed by their origin vertex
    val innerEdges = vertices.map(HalfEdge.apply(_))
    val outerEdges = vertices.map(HalfEdge.apply(_))

    // Link all components together
    for (i <- 0 until n)
      val next_i = (i + 1) % n

      val inner_current = innerEdges(i)
      val inner_next = innerEdges(next_i)

      val outer_current = outerEdges(i)
      val outer_next = outerEdges(next_i)

      // Set vertex leaving edge
      vertices(i).leaving = Some(inner_current)

      // Link inner loop (counter-clockwise)
      inner_current.next = Some(inner_next)
      inner_next.prev = Some(inner_current)
      inner_current.incidentFace = Some(fPoly)
      inner_current.angle = angles(i)

      // The twin of the inner edge V_i -> V_{i+1} is the outer edge V_{i+1} -> V_i
      inner_current.twin = Some(outer_next)
      outer_next.twin = Some(inner_current)

      // Link outer loop (clockwise)
      val prev_i = (i + n - 1) % n
      val outer_prev = outerEdges(prev_i)
      outer_current.next = Some(outer_prev)
      outer_prev.prev = Some(outer_current)
      outer_current.incidentFace = Some(fOuter)
      outer_current.angle = 360.0 - angles(i)

    fPoly.outerComponent = innerEdges.headOption
    fOuter.outerComponent = outerEdges.headOption

    Right(TilingDCEL(
      vertices = vertices,
      halfEdges = innerEdges ++ outerEdges,
      innerFaces = List(fPoly),
      outerFace = fOuter
    ))

  /**
   * Calculates the coordinates of a polygon's vertices and validates that it's a closed polygon
   * with the correct side lengths and angles.
   */
  private def calculateVertexPoints(angles: List[Double], performSimplicityCheck: Boolean): Either[String, List[Geometry.Point]] =
    import Geometry.Point
    val n = angles.length
    // Start with V0 at the origin and V1 on the X-axis
    val points = ListBuffer(Point(0.0, 0.0), Point(1.0, 0.0))
    var currentPoint = Point(1.0, 0.0)
    var heading = 0.0 // The heading of the segment V0->V1 is 0 degrees

    // Calculate the positions of V2 through V(n-1)
    for (i <- 1 until n - 1)
      val interiorAngle = angles(i)
      val turnAngle = 180.0 - interiorAngle
      heading += turnAngle

      currentPoint = Point(currentPoint.x + cos(toRadians(heading)), currentPoint.y + sin(toRadians(heading)))
      points.append(currentPoint)

    val pointsList = points.toList
    if performSimplicityCheck && !Geometry.isSimple(pointsList) then
      return Left("The polygon is not simple (it intersects itself).")

    // --- Validation ---
    // Check if the final edge, from V(n-1) back to V0, has the correct length and angles
    val p_n_minus_1 = points.last
    val p_0 = points.head
    val dx = p_0.x - p_n_minus_1.x
    val dy = p_0.y - p_n_minus_1.y
    val lastEdgeLength = sqrt(dx * dx + dy * dy)

    if abs(lastEdgeLength - 1.0) > Epsilon then
      return Left(f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0.")

    // Check the last interior angle at V(n-1)
    val heading_n_minus_1_to_0 = toDegrees(math.atan2(dy, dx))
    var turn_at_n_minus_1 = heading_n_minus_1_to_0 - heading
    while (turn_at_n_minus_1 <= -180) turn_at_n_minus_1 += 360
    val calculatedFinalAngle = 180.0 - turn_at_n_minus_1

    if abs(calculatedFinalAngle - angles.last) > Epsilon then
      return Left(f"Angle at V${n-1} is incorrect. Expected ${angles.last}%.2f, but calculated ${calculatedFinalAngle}%.2f.")

    // Check the first interior angle at V0
    var turn_at_0 = 0.0 - heading_n_minus_1_to_0
    while (turn_at_0 <= -180) turn_at_0 += 360
    val calculatedFirstAngle = 180.0 - turn_at_0

    if abs(calculatedFirstAngle - angles.head) > Epsilon then
      return Left(f"Angle at V0 is incorrect. Expected ${angles.head}%.2f, but calculated ${calculatedFirstAngle}%.2f.")

    Right(pointsList)