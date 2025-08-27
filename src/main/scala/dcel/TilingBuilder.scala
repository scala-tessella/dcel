package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.{RegularPolygon, SimplePolygon}
import spire.implicits.*

import scala.collection.mutable

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
    val points = mutable.ListBuffer(p0, p1)
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
   * Create a tiling made of a net of regular triangles
   *
   * @param width  number of triangle pairs (rhombi) on each row
   * @param height number of triangle pairs (rhombi) on each colum
   */
  def createTriangleNet(width: Int, height: Int): TilingDCEL =
    if width <= 0 || height <= 0 then
      return TilingBuilder.empty

    val triangleAngle = AngleDegree(60)

    val rad = triangleAngle.toBigRadian.toBigDecimal
    val v_vec_x = spire.math.cos(rad)
    val v_vec_y = spire.math.sin(rad)

    val points = Array.tabulate(height + 1, width + 1) { (j, i) =>
      BigPoint(BigDecimal(i) + v_vec_x * j, v_vec_y * j)
    }

    val vertices = Array.tabulate(height + 1, width + 1) { (j, i) =>
      val vertexId = j * (width + 1) + i + 1
      Vertex(s"V$vertexId", points(j)(i))
    }

    // Two triangular faces per rhombus cell
    val faces = Array.tabulate(height, width, 2) { (j, i, k) =>
      Face(s"F${(j * width + i) * 2 + k + 1}")
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
    // These diagonals split each rhombus into two equilateral triangles
    val diagonals = Array.tabulate(height, width) { (j, i) =>
      createTwinPair(vertices(j)(i + 1), vertices(j + 1)(i))
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
      val face1 = faces(j)(i)(0) // Triangle (v_ji, v_ji1, v_j1i)
      val face2 = faces(j)(i)(1) // Triangle (v_ji1, v_j1i1, v_j1i)

      val e1 = horizontal(j)(i)._1 // v_ji -> v_ji1
      val e2 = vSlope(j)(i + 1)._1 // v_ji1 -> v_j1i1
      val e3 = horizontal(j + 1)(i)._2 // v_j1i1 -> v_j1i
      val e4 = vSlope(j)(i)._2 // v_j1i -> v_ji
      val e_diag = diagonals(j)(i)._1 // v_ji1 -> v_j1i
      val e_diag_rev = diagonals(j)(i)._2 // v_j1i -> v_ji1

      // Link face1
      List(e1, e_diag, e4).linkInCycle()
      e1.incidentFace = Some(face1); e_diag.incidentFace = Some(face1); e4.incidentFace = Some(face1)
      face1.outerComponent = Some(e1)
      e1.angle = Some(triangleAngle); e_diag.angle = Some(triangleAngle); e4.angle = Some(triangleAngle)

      // Link face2
      List(e2, e3, e_diag_rev).linkInCycle()
      e2.incidentFace = Some(face2); e3.incidentFace = Some(face2); e_diag_rev.incidentFace = Some(face2)
      face2.outerComponent = Some(e2)
      e2.angle = Some(triangleAngle); e3.angle = Some(triangleAngle); e_diag_rev.angle = Some(triangleAngle)

    // Link outer face boundary
    val innerBoundaryEdgesCCW = new mutable.ListBuffer[HalfEdge]()
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
        vSlope.flatMap(row => row.flatMap(p => List(p._1, p._2))).toList ++
        diagonals.flatMap(row => row.flatMap(p => List(p._1, p._2))).toList

    // Set outer angles
    for outerEdge <- outerBoundaryCW do
      val vertex = outerEdge.origin
      val incident = allHalfEdges.filter(_.origin == vertex)
      val innerAnglesSum = incident.filterNot(_.incidentFace.contains(fOuter)).flatMap(_.angle).sum2
      outerEdge.angle = Some(innerAnglesSum.conjugate)

    TilingDCEL(
      vertices = vertices.flatten.toList,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.flatten.toList,
      outerFace = fOuter
    )

  /**
   * Create a tiling made of a net of identical rhombi
   *
   * @param width  number of rhombi on each row
   * @param height number of rhombi on each colum
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
      val vertexId = j * (width + 1) + i + 1
      Vertex(s"V$vertexId", points(j)(i))
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
    val innerBoundaryEdgesCCW = new mutable.ListBuffer[HalfEdge]()
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

  /**
   * Create a tiling made of a net of identical hexagons
   *
   * @param width  number of hexagons on each row
   * @param height number of hexagons on each column
   * @param angle  interior angle (in degrees) for vertices 0 and 3 of each hexagon.
   *               The remaining four interior angles are all equal and computed to satisfy the polygon angle sum.
   *               Constraint: 0 < angle < 180. Default 120 creates the regular honeycomb.
   */
  def createHexagonNet(width: Int, height: Int, angle: AngleDegree = AngleDegree(120)): TilingDCEL =
    if width <= 0 || height <= 0 then
      return TilingBuilder.empty

    // Validate the provided angle
    if angle.isFullCircle || angle.toRational <= 0 || angle.toRational >= 180 then
      return TilingBuilder.empty

    // Hexagon per-face interior angles pattern: [a, b, b, a, b, b]
    // Sum must be 720 => 2a + 4b = 720 => b = 180 - a/2
    val alpha = angle
    val beta: AngleDegree = AngleDegree(180) - (alpha / 2)

    // Compute the three edge direction headings (in radians) for this hexagon shape.
    // Starting from heading 0 for edge 0, add exterior turn angles (180 - interior).
    val ext0 = AngleDegree(180) - alpha
    val ext1 = AngleDegree(180) - beta
    val ext2 = AngleDegree(180) - beta

    val h0 = BigDecimalGeometry.BigRadian(0) // edge 0 heading
    val h1 = (h0 + ext0.toBigRadian) // edge 1 heading
    val h2 = (h1 + ext1.toBigRadian) // edge 2 heading

    // Unit edge direction vectors d0, d1, d2 (others are opposite).
    val d0x = spire.math.cos(h0.toBigDecimal);
    val d0y = spire.math.sin(h0.toBigDecimal)
    val d1x = spire.math.cos(h1.toBigDecimal);
    val d1y = spire.math.sin(h1.toBigDecimal)
    val d2x = spire.math.cos(h2.toBigDecimal);
    val d2y = spire.math.sin(h2.toBigDecimal)

    // Derive a linear lattice mapping f(U,V) = U*A + V*B so that the differences between
    // our integer corner keys map to the actual edge direction vectors around each hexagon.
    // From the key deltas and edge directions it follows:
    //   A = -d1/2,  B = -d2
    val Ax = -d1x / 2;
    val Ay = -d1y / 2
    val Bx = -d2x;
    val By = -d2y

    // Map from integer lattice key to vertex instance
    val vertexByKey = mutable.Map[(Int, Int), Vertex]()
    var vertexCounter = 1

    def keyToCoords(U: Int, V: Int): BigPoint =
      val x = BigDecimal(U) * Ax + BigDecimal(V) * Bx
      val y = BigDecimal(U) * Ay + BigDecimal(V) * By
      BigPoint(x, y)

    def getOrCreateVertex(U: Int, V: Int): Vertex =
      vertexByKey.getOrElseUpdate((U, V), {
        val v = Vertex(s"V$vertexCounter", keyToCoords(U, V))
        vertexCounter += 1
        v
      })

    // For each hexagon cell (i, j) we need the 6 corner keys (U, V) in CCW order.
    // Flat-top-like indexing via integer keys (independent from coordinates).
    // For hex at (i, j), let s = i % 2 (0 or 1). Then the six corners have:
    //   k=0: (U, V) = ( 3i + 2 - 2j - s,     2j + s     )
    //   k=1: (U, V) = ( 3i     - 2j - s,     2j + s + 1 )
    //   k=2: (U, V) = ( 3i - 2 - 2j - s,     2j + s + 1 )
    //   k=3: (U, V) = ( 3i - 2 - 2j - s,     2j + s     )
    //   k=4: (U, V) = ( 3i     - 2j - s,     2j + s - 1 )
    //   k=5: (U, V) = ( 3i + 2 - 2j - s,     2j + s - 1 )
    def hexCornerKeys(i: Int, j: Int): List[(Int, Int)] =
      val s = i & 1
      val base = 3 * i - 2 * j - s
      val v0 = 2 * j + s
      List(
        (base + 2, v0), // k = 0
        (base, v0 + 1), // k = 1
        (base - 2, v0 + 1), // k = 2
        (base - 2, v0), // k = 3
        (base, v0 - 1), // k = 4
        (base + 2, v0 - 1) // k = 5
      )

    // Faces and outer face
    val faces = Array.tabulate(height, width) { (j, i) => Face(s"F${j * width + i + 1}") }
    val fOuter = Face.outer

    // Create reusable twin pairs per directed vertex pair. Store both orientations.
    val halfEdgeByDir = mutable.Map[(Vertex, Vertex), HalfEdge]()

    def getOrCreateHalfEdge(v1: Vertex, v2: Vertex): HalfEdge =
      halfEdgeByDir.getOrElseUpdate((v1, v2), {
        val e1 = HalfEdge(v1)
        val e2 = HalfEdge(v2)
        e1.twinWith(e2)
        halfEdgeByDir((v2, v1)) = e2
        e1
      })

    // Build all hex faces, their vertices and their inner half-edges
    for j <- 0 until height; i <- 0 until width do
      val face = faces(j)(i)
      val corners = hexCornerKeys(i, j).map((U, V) => getOrCreateVertex(U, V))

      // Per-corner interior angles for this hexagon: [a, b, b, a, b, b]
      val cornerAngles: Array[AngleDegree] =
        Array(alpha, beta, beta, alpha, beta, beta)

      // Create/get the six directed half-edges in CCW order
      val edgesCCW =
        (0 until 6).toList.map { k =>
          val v1 = corners(k)
          val v2 = corners((k + 1) % 6)
          getOrCreateHalfEdge(v1, v2)
        }

      // Link inner face cycle and set per-corner angles
      edgesCCW.linkInCycle()
      edgesCCW.zipWithIndex.foreach { case (e, k) =>
        e.incidentFace = Some(face)
        e.angle = Some(cornerAngles(k))
      }
      face.outerComponent = edgesCCW.headOption

    // Collect all vertices and half-edges
    val allVertices = vertexByKey.values.toList
    val allHalfEdges = halfEdgeByDir.values.toList

    // Set leaving edges for vertices (any outgoing will do)
    allVertices.foreach { v =>
      v.leaving = allHalfEdges.find(_.origin eq v)
    }

    // Boundary edges are those half-edges whose incidentFace is not defined yet (their twin belongs to a face)
    val boundaryEdgesCW = allHalfEdges.filter(_.incidentFace.isEmpty)

    // Order boundary into a single cycle by connectivity
    def orderBoundary(edges: List[HalfEdge]): List[HalfEdge] =
      if edges.isEmpty then return Nil
      val remaining = mutable.HashSet.from(edges)
      val ordered = mutable.ListBuffer[HalfEdge]()
      var current = edges.head
      ordered += current
      remaining -= current
      while remaining.nonEmpty do
        val nextOpt = remaining.find(e => current.destination.contains(e.origin))
        nextOpt match
          case Some(next) =>
            ordered += next
            remaining -= next
            current = next
          case None =>
            return ordered.toList
      ordered.toList

    val boundaryOrdered = orderBoundary(boundaryEdgesCW)

    // Link and assign outer face
    if boundaryOrdered.nonEmpty then
      boundaryOrdered.linkInCycle()
      boundaryOrdered.foreach(_.incidentFace = Some(fOuter))
      fOuter.outerComponent = boundaryOrdered.headOption

      // Set outer angles at boundary vertices as conjugates of inner angles sum
      boundaryOrdered.foreach { outerEdge =>
        val v = outerEdge.origin
        val incidentAtV = allHalfEdges.filter(_.origin eq v)
        val innerAnglesSum = incidentAtV.filterNot(_.incidentFace.contains(fOuter)).flatMap(_.angle).sum2
        outerEdge.angle = Some(innerAnglesSum.conjugate)
      }

    TilingDCEL(
      vertices = allVertices,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.toList,
      outerFace = fOuter
    )
