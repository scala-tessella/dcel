package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.{RegularPolygon, SimplePolygon}
import spire.implicits.*

import scala.collection.mutable.ListBuffer

object TilingBuilder:

  def empty: TilingDCEL =
    TilingDCEL(
      vertices = List.empty,
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face(Face.outerId)
    )

  def validateSides(sides: Int, polygonType: String): Either[String, Unit] =
    if sides >= 3 then Right(())
    else Left(s"A $polygonType polygon must have at least 3 sides, but $sides were specified.")

  /**
   * Creates a TilingDCEL for a single simple polygon with unit-length sides.
   *
   * @param angles A list of interior angles in degrees. The angles are ordered for a
   *               counter-clockwise traversal of the polygon boundary.
   * @return       Either a String explaining the validation error, or the successfully created TilingDCEL.
   */
  def createSimplePolygon(angles: List[AngleDegree]): Either[String, TilingDCEL] =
    for
      _      <- validateSides(angles.length, "simple")
      _      <- SimplePolygon.validatePolygonAngles(angles)
      points <- calculateVertexPoints(angles, performSimplicityCheck = true)
      result <- buildDCELFromPoints(points, angles)
    yield
      result

  /**
   * Creates a TilingDCEL for a single regular polygon with unit-length sides.
   *
   * @param sides The number of sides for the regular polygon.
   * @return      Either a String explaining the validation error, or the successfully created TilingDCEL.
   */
  def createRegularPolygon(sides: Int): Either[String, TilingDCEL] =
    for
      _      <- validateSides(sides, "regular")
      angle = RegularPolygon(sides).alphaDegree
      angles = List.fill(sides)(angle)
      // Regular polygons are always simple, so we can skip the self-intersection check.
      // The angle sum is also correct by definition.
      points <- calculateVertexPoints(angles, performSimplicityCheck = false)
      result <- buildDCELFromPoints(points, angles)
    yield
      result

        /**
   * Given validated points and angles, builds the TilingDCEL structure.
   */
  private def buildDCELFromPoints(points: List[BigPoint], angles: List[AngleDegree]): Either[String, TilingDCEL] =
    val n = points.length

    // Create vertices from the calculated points
    val vertices = points.zipWithIndex.map { case (p, i) => Vertex(s"V${i + 1}", p) }

    // Create the two faces: one for the polygon, one for the outside
    val fPoly = Face(Face.firstInnerId)
    val fOuter = Face(Face.outerId)

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
      inner_current.angle = Some(angles(i))

      // The twin of the inner edge V_i -> V_{i+1} is the outer edge V_{i+1} -> V_i
      inner_current.twin = Some(outer_next)
      outer_next.twin = Some(inner_current)

      // Link outer loop (clockwise)
      val prev_i = (i + n - 1) % n
      val outer_prev = outerEdges(prev_i)
      outer_current.next = Some(outer_prev)
      outer_prev.prev = Some(outer_current)
      outer_current.incidentFace = Some(fOuter)
      outer_current.angle = Some(AngleDegree(360) - angles(i))

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
  def calculateVertexPoints(angles: List[AngleDegree], performSimplicityCheck: Boolean): Either[String, List[BigPoint]] =
    val n = angles.length
    // Start with V0 at the origin and V1 on the X-axis
    val points = ListBuffer(BigPoint(0.0, 0.0), BigPoint(1.0, 0.0))
    var currentPoint = BigPoint(1.0, 0.0)
    var heading: AngleDegree = AngleDegree(0) // The heading of the segment V0->V1 is 0 degrees

    // Calculate the positions of V2 through V(n-1)
    for (i <- 1 until n - 1)
      val interiorAngle = angles(i)
      val turnAngle = AngleDegree(180) - interiorAngle
      heading += turnAngle
      val radian = heading.toBigRadian.toBigDecimal
      currentPoint = BigPoint(
        currentPoint.x + spire.math.cos(radian),
        currentPoint.y + spire.math.sin(radian)
      )
      points.append(currentPoint)

    val pointsList = points.toList
    if performSimplicityCheck && !pointsList.hasNoAlmostEqualPoints() then
      return Left("The polygon is not simple (it has vertices that are equal, which is not allowed).")

    // --- Validation ---
    // Check if the final edge, from V(n-1) back to V0, has the correct length and angles
    val p_n_minus_1 = points.last
    val p_0 = points.head
    val dx = p_0.x - p_n_minus_1.x
    val dy = p_0.y - p_n_minus_1.y
    val lastEdgeLength = spire.math.sqrt(dx * dx + dy * dy)

    if spire.math.abs(lastEdgeLength - 1.0) > ACCURACY then
      return Left(f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0.")

    // Check the last interior angle at V(n-1)
    // @TODO why the conversion toDouble is needed?  
    val heading_n_minus_1_to_0 = spire.math.toDegrees(spire.math.atan2(dy.toDouble, dx.toDouble))
    var turn_at_n_minus_1 = heading_n_minus_1_to_0 - heading.toRational.toDouble
    while (turn_at_n_minus_1 <= -180) turn_at_n_minus_1 += 360
    val calculatedFinalAngle = 180.0 - turn_at_n_minus_1

    if spire.math.abs(calculatedFinalAngle - angles.last.toRational.toDouble) > ACCURACY then
      return Left(f"Angle at V${n-1} is incorrect. Expected ${angles.last.toRational.toDouble}%.2f, but calculated $calculatedFinalAngle%.2f.")

    // Check the first interior angle at V0
    var turn_at_0 = 0.0 - heading_n_minus_1_to_0
    while (turn_at_0 <= -180) turn_at_0 += 360
    val calculatedFirstAngle = 180.0 - turn_at_0

    if spire.math.abs(calculatedFirstAngle - angles.head.toRational.toDouble) > ACCURACY then
      return Left(f"Angle at V0 is incorrect. Expected ${angles.head.toRational.toDouble}%.2f, but calculated $calculatedFirstAngle%.2f.")

    Right(pointsList)