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
      outerFace = Face.outer
    )

  def validateSides(sides: Int, polygonType: String): Either[String, Unit] =
    if sides >= 3 then Right(())
    else Left(s"A $polygonType polygon must have at least 3 sides, but $sides were specified.")

  def validatePoints(points: List[BigPoint]): Either[String, Unit] =
    // Check if the final edge, from V(n-1) back to V0, has the correct length and angles
    val lastEdgeLength = points.head.distanceTo(points.last)
    if spire.math.abs(lastEdgeLength - 1.0) > ACCURACY then
      return Left(f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0.")

    if !points.hasNoAlmostEqualPoints() then
      return Left("The polygon is not simple (it has vertices that are equal, which is not allowed).")

    Right(())

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
      points = calculateVertexPoints(angles)
      _      <- validatePoints(points)
      result <- buildDCELFromPoints(points, angles)
    yield
      result

  def createSimplePolygon(degrees: Int *): Either[String, TilingDCEL] =
    createSimplePolygon(degrees.map(AngleDegree(_)).toList)
    
  /**
   * Creates a TilingDCEL for a single regular polygon with unit-length sides.
   *
   * @param sides The number of sides for the regular polygon.
   * @return      Either a String explaining the validation error, or the successfully created TilingDCEL.
   */
  def createRegularPolygon(sides: Int): Either[String, TilingDCEL] =
    for
      _      <- validateSides(sides, "regular")
      angle = RegularPolygon(sides).alpha
      angles = List.fill(sides)(angle)
      // Regular polygons are always simple, so we can skip the self-intersection check.
      // The angle sum is also correct by definition.
      points = calculateVertexPoints(angles)
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
    val fOuter = Face.outer

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

  def calculateVertexPoints2(
    angles: List[AngleDegree],
    start: BigPoint = BigPoint(),
    direction: AngleDegree = AngleDegree(0)
  ): List[BigPoint] =
    val p1: BigPoint =
      if direction == AngleDegree(0) then
        start.plus(BigPoint(1, 0))
      else
        start.plus(BigPoint.fromPolar(1, direction.toBigRadian))
    calculateVertexPoints(angles, start, p1)

  // Start with V0 at the origin and V1 on the X-axis

  /**
   * Calculates the coordinates of a polygon's vertices and validates that it's a closed polygon
   * with the correct side lengths and angles.
   */
  def calculateVertexPoints(
    angles: List[AngleDegree], 
    p0: BigPoint = BigPoint(),
    p1: BigPoint = BigPoint(1, 0)
  ): List[BigPoint] =
    val n = angles.length
    // Start with V0 at the origin and V1
    val points = ListBuffer(p0, p1)
    var currentPoint = p1
    var heading = p0.angleTo(p1)
    // Calculate the positions of V2 through V(n-1)
    for (i <- 1 until n - 1)
      val interiorAngle = angles(i)
      val turnAngle = AngleDegree(180) - interiorAngle
      heading += turnAngle.toBigRadian
      currentPoint = currentPoint.plus(BigPoint.fromPolar(1, heading))
      points.append(currentPoint)

    points.toList

  /**
   * Create a tiling made of a net of identical rhombuses
   *
   * @param width  number of rhombuses on each row
   * @param height number of rhombuses on each colum
   * @param angle  degree of the first interior angle of each rhombus, the default angle creates a square net
   */
  def createRhombusNet(width: Int, height: Int, angle: AngleDegree = AngleDegree(90)): TilingDCEL =
    if width <= 0 || height <= 0 then
      return TilingBuilder.empty

    val alpha1 = angle
    val alpha2 = AngleDegree(180) - angle

    val rad = angle.toBigRadian.toBigDecimal
    val v_vec_x = spire.math.cos(rad)
    val v_vec_y = spire.math.sin(rad)

    val points = Array.tabulate(height + 1, width + 1) { (j, i) =>
      BigPoint(BigDecimal(i) + v_vec_x * j, v_vec_y * j)
    }

    val vertices = Array.tabulate(height + 1, width + 1) { (j, i) =>
      Vertex(s"V${j}_$i", points(j)(i))
    }

    val faces = Array.tabulate(height, width) { (j, i) =>
      Face(s"F${j * width + i + 1}")
    }
    val fOuter = Face.outer

    def createTwinPair(v1: Vertex, v2: Vertex): (HalfEdge, HalfEdge) =
      val e1 = HalfEdge(v1)
      val e2 = HalfEdge(v2)
      e1.twinWith(e2)
      (e1, e2)

    val horizontal = Array.tabulate(height + 1, width) { (j, i) =>
      createTwinPair(vertices(j)(i), vertices(j)(i + 1))
    }
    val vSlope = Array.tabulate(height, width + 1) { (j, i) =>
      createTwinPair(vertices(j)(i), vertices(j + 1)(i))
    }

    // Set leaving edges for vertices
    for j <- 0 to height; i <- 0 to width do
      val v = vertices(j)(i)
      if i < width then v.leaving = Some(horizontal(j)(i)._1)
      else if j < height then v.leaving = Some(vSlope(j)(i)._1)
      else if i > 0 then v.leaving = Some(horizontal(j)(i - 1)._2)
      else if j > 0 then v.leaving = Some(vSlope(j - 1)(i)._2)

    // Link inner faces
    for j <- 0 until height; i <- 0 until width do
      val face = faces(j)(i)
      val e1 = horizontal(j)(i)._1 // v(j,i) -> v(j,i+1)
      val e2 = vSlope(j)(i + 1)._1 // v(j,i+1) -> v(j+1,i+1)
      val e3 = horizontal(j + 1)(i)._2 // v(j+1,i+1) -> v(j+1,i)
      val e4 = vSlope(j)(i)._2 // v(j+1,i) -> v(j,i)

      List(e1, e2, e3, e4).linkInCycle()

      e1.incidentFace = Some(face); e2.incidentFace = Some(face)
      e3.incidentFace = Some(face); e4.incidentFace = Some(face)

      face.outerComponent = Some(e1)

      e1.angle = Some(alpha1)
      e2.angle = Some(alpha2)
      e3.angle = Some(alpha1)
      e4.angle = Some(alpha2)

    // Link outer face boundary
    val innerBoundaryEdgesCCW = new ListBuffer[HalfEdge]()
    // Bottom boundary
    for (i <- 0 until width) innerBoundaryEdgesCCW += horizontal(0)(i)._1
    // Right boundary
    for (j <- 0 until height) innerBoundaryEdgesCCW += vSlope(j)(width)._1
    // Top boundary
    for (i <- (0 until width).reverse) innerBoundaryEdgesCCW += horizontal(height)(i)._2
    // Left boundary
    for (j <- (0 until height).reverse) innerBoundaryEdgesCCW += vSlope(j)(0)._2

    val outerBoundaryCW = innerBoundaryEdgesCCW.toList.map(_.twin.get).reverse
    outerBoundaryCW.linkInCycle()
    outerBoundaryCW.foreach(_.incidentFace = Some(fOuter))
    fOuter.outerComponent = outerBoundaryCW.headOption

    val allHalfEdges =
      horizontal.flatMap(row => row.flatMap(p => List(p._1, p._2))).toList ++
        vSlope.flatMap(row => row.flatMap(p => List(p._1, p._2))).toList

    // Set outer angles
    for outerEdge <- outerBoundaryCW do
      val vertex = outerEdge.origin
      val incident = allHalfEdges.filter(_.origin == vertex)
      val innerAnglesSum = incident.filterNot(_.incidentFace.contains(fOuter)).flatMap(_.angle).sum2
      outerEdge.angle = Some(innerAnglesSum.conjugate)

    TilingDCEL(
      vertices = vertices.flatten.toList,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.toList,
      outerFace = fOuter
    )
