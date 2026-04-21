package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.{
  AngleDegree,
  BigLineSegment,
  BigPoint,
  BigRadian,
  RegularPolygon,
  SimplePolygon
}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.dcel.TilingAddition.addRegularPolygonToBoundary
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import spire.implicits.*

import scala.collection.mutable

object TilingBuilder:

  private type PositiveInt = Int :| Positive

  final private case class RectNetEdges(
      vertices: Array[Array[Vertex]],
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      outerFace: Face
  )

  private def validatePositiveDimensions(
      width: Int,
      height: Int
  ): Either[TilingError, (PositiveInt, PositiveInt)] =
    for
      refinedWidth  <- refineOrError[Int, Positive](width, "width")
      refinedHeight <- refineOrError[Int, Positive](height, "height")
    yield (refinedWidth, refinedHeight)

  private def refineOrError[A, C](value: A, label: String)(using
      RuntimeConstraint[A, C]
  ): Either[TilingError, A :| C] =
    value
      .refineEither[C]
      .left
      .map: message =>
        ValidationError(s"Invalid $label: $message")

  private def buildRectNetEdges(
      height: Int,
      width: Int,
      angle: AngleDegree
  ): RectNetEdges =
    val (_, vertices)        = pointsVertices(height, width, angle)
    val (horizontal, vSlope) = horizontalAndVSlope(height, width, vertices)
    setLeavingEdges(height, width, vertices, horizontal, vSlope)
    RectNetEdges(vertices, horizontal, vSlope, Face.outer)

  private def finalizeOuterBoundary(
      height: Int,
      width: Int,
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      outerFace: Face,
      allHalfEdges: List[HalfEdge]
  ): Unit =
    val outerBoundaryCW = linkOuterFace(height, width, horizontal, vSlope, outerFace)
    allHalfEdges.setOuterEdgeAngles(outerBoundaryCW, outerFace)

  def validatePoints(points: List[BigPoint]): Either[TilingError, Unit] =
    // Check if the final edge, from V(n-1) back to V0, has the correct length and angles
    val lastEdgeLength = points.head.distanceTo(points.last)
    if spire.math.abs(lastEdgeLength - 1.0) > ACCURACY then
      return Left(TopologyError(
        f"The polygon does not close. The final edge has length $lastEdgeLength%.4f instead of 1.0."
      ))

    if !points.hasNoAlmostEqualPoints() then
      return Left(
        TopologyError("The polygon is not simple (it has vertices that are equal, which is not allowed).")
      )

    Right(())

  private[dcel] def createSimplePolygonUnsafe(simple: SimplePolygon): TilingDCEL =
    buildDCELFromPointsUnsafe(
      calculateVertexPoints(simple.toAngles),
      simple.toAngles.toList
    )

  /** Creates a TilingDCEL for a single simple polygon with unit-length sides.
    *
    * @param degrees
    *   Sequence of interior angles in degrees, ordered for a CCW traversal of the polygon boundary.
    * @return
    *   Either a TilingError explaining the validation error, or the successfully created TilingDCEL.
    */
  def createSimplePolygon(degrees: Int*): Either[TilingError, TilingDCEL] =
    for
      validatedSimplePolygon <- SimplePolygon.fromUntrusted(degrees*)
    yield createSimplePolygonUnsafe(validatedSimplePolygon)

  def createSimplePolygon(angles: Vector[AngleDegree]): Either[TilingError, TilingDCEL] =
    for
      validatedSimplePolygon <- SimplePolygon.fromUntrusted(angles)
    yield createSimplePolygonUnsafe(validatedSimplePolygon)

  /** Creates a TilingDCEL for a single regular polygon with unit-length sides.
    *
    * @param polygon
    *   The [[geometry.RegularPolygon]] to create.
    */
  def createRegularPolygon(polygon: RegularPolygon): TilingDCEL =
    val angles = polygon.angles
    val points = calculateVertexPoints(angles)
    buildDCELFromPointsUnsafe(points, angles.toList)

  /** Given validated points and angles, builds the TilingDCEL structure.
    */
  private[dcel] def buildDCELFromPointsUnsafe(
      points: List[BigPoint],
      angles: List[AngleDegree]
  ): TilingDCEL =
    val n = points.length

    // Create vertices from the calculated points
    val vertices =
      points.zipWithIndex.map: (p, i) =>
        Vertex(VertexId(i + 1), p)

    // Create the two faces: one for the polygon, one for the outside
    val polygonFace = Face(FaceId.firstInnerId)
    val outerFace   = Face.outer

    // Create all inner and outer half-edges, indexed by their origin vertex
    val innerEdges =
      vertices.map: vertex =>
        HalfEdge.apply(vertex)
    val outerEdges =
      vertices.map: vertex =>
        HalfEdge.apply(vertex)

    // Link all components together
    for i <- vertices.indices do
      val nextIndex = (i + 1)     % n
      val prevIndex = (i + n - 1) % n

      val currentInnerEdge = innerEdges(i)
      val nextInnerEdge    = innerEdges(nextIndex)
      val currentOuterEdge = outerEdges(i)
      val nextOuterEdge    = outerEdges(nextIndex)
      val prevOuterEdge    = outerEdges(prevIndex)

      // Set vertex leaving edge
      vertices(i).leaving = Some(currentInnerEdge)

      // Link inner loop (counter-clockwise)
      currentInnerEdge.next = Some(nextInnerEdge)
      nextInnerEdge.prev = Some(currentInnerEdge)
      currentInnerEdge.incidentFace = Some(polygonFace)
      currentInnerEdge.angle = Some(angles(i))

      // The twin of the inner edge V_i -> V_{i+1} is the outer edge V_{i+1} -> V_i
      currentInnerEdge.twin = Some(nextOuterEdge)
      nextOuterEdge.twin = Some(currentInnerEdge)

      // Link outer loop (clockwise)
      currentOuterEdge.next = Some(prevOuterEdge)
      prevOuterEdge.prev = Some(currentOuterEdge)
      currentOuterEdge.incidentFace = Some(outerFace)
      currentOuterEdge.angle = Some(angles(i).conjugate)

    polygonFace.outerComponent = innerEdges.headOption
    outerFace.outerComponent = outerEdges.headOption

    TilingDCEL(
      vertices = vertices,
      halfEdges = innerEdges ++ outerEdges,
      innerFaces = List(polygonFace),
      outerFace = outerFace
    )

  /** Calculates the coordinates of a polygon's vertices */
  private[dcel] def calculateVertexPoints(
      angles: Vector[AngleDegree],
      p0: BigPoint = BigPoint.origin,
      p1: BigPoint = BigPoint(1, 0)
  ): List[BigPoint] =
    BigLineSegment(p0, p1).unitPath(angles)

  /** Generates a grid of vertices for tessellation patterns.
    *
    * @param height
    *   The number of rows in the grid
    * @param width
    *   The number of columns in the grid
    * @param angle
    *   The angle for vertex positioning
    */
  private def pointsVertices(
      height: Int,
      width: Int,
      angle: AngleDegree
  ): (Array[Array[BigPoint]], Array[Array[Vertex]]) =
    val rad     = angle.toBigRadian.toBigDecimal
    val v_vec_x = spire.math.cos(rad)
    val v_vec_y = spire.math.sin(rad)

    val points =
      Array.tabulate(height + 1, width + 1): (j, i) =>
        BigPoint(BigDecimal(i) + v_vec_x * j, v_vec_y * j)

    val vertices =
      Array.tabulate(height + 1, width + 1): (j, i) =>
        val id = j * (width + 1) + i + 1
        Vertex(VertexId(id), points(j)(i))

    (points, vertices)

  private def createTwinPair(v1: Vertex, v2: Vertex): (HalfEdge, HalfEdge) =
    val e1 = HalfEdge(v1)
    val e2 = HalfEdge(v2)
    e1.twinWith(e2)
    (e1, e2)

  private def horizontalAndVSlope(
      height: Int,
      width: Int,
      vertices: Array[Array[Vertex]]
  ): (Array[Array[(HalfEdge, HalfEdge)]], Array[Array[(HalfEdge, HalfEdge)]]) =
    val horizontal =
      Array.tabulate(height + 1, width): (j, i) =>
        createTwinPair(vertices(j)(i), vertices(j)(i + 1))
    val vSlope     =
      Array.tabulate(height, width + 1): (j, i) =>
        createTwinPair(vertices(j)(i), vertices(j + 1)(i))

    (horizontal, vSlope)

  // Set leaving edges for vertices
  private def setLeavingEdges(
      height: Int,
      width: Int,
      vertices: Array[Array[Vertex]],
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]]
  ): Unit =
    for j <- 0 to height; i <- 0 to width do
      val v = vertices(j)(i)
      if i < width then v.leaving = Some(horizontal(j)(i)._1)
      else if j < height then v.leaving = Some(vSlope(j)(i)._1)
      else if i > 0 then v.leaving = Some(horizontal(j)(i - 1)._2)
      else if j > 0 then v.leaving = Some(vSlope(j - 1)(i)._2)

  private def netFaces(height: Int, width: Int): Array[Array[Face]] =
    Array.tabulate(height, width): (j, i) =>
      Face(FaceId(j * width + i + 1))

  // Link outer face boundary
  private def linkOuterFace(
      height: Int,
      width: Int,
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      fOuter: Face
  ): List[HalfEdge] =
    val innerBoundaryEdgesCCW = new mutable.ListBuffer[HalfEdge]()
    // Bottom boundary
    for i <- 0 until width do
      innerBoundaryEdgesCCW += horizontal(0)(i)._1
    // Right boundary
    for j <- 0 until height do
      innerBoundaryEdgesCCW += vSlope(j)(width)._1
    // Top boundary
    for i <- (0 until width).reverse do
      innerBoundaryEdgesCCW += horizontal(height)(i)._2
    // Left boundary
    for j <- (0 until height).reverse do
      innerBoundaryEdgesCCW += vSlope(j)(0)._2

    val outerBoundaryCW =
      innerBoundaryEdgesCCW.toList
        .map: halfEdge =>
          halfEdge.twin.get
        .reverse
    outerBoundaryCW.linkInCycle()
    outerBoundaryCW.foreach: halfEdge =>
      halfEdge.incidentFace = Some(fOuter)
    fOuter.outerComponent = outerBoundaryCW.headOption
    outerBoundaryCW

  private def toHalfEdges(pairs: Array[Array[(HalfEdge, HalfEdge)]]): List[HalfEdge] =
    pairs
      .flatMap: row =>
        row.flatMap: (halfEdge1, halfEdge2) =>
          List(halfEdge1, halfEdge2)
      .toList

  private def netHalfEdges(
      horizontal: Array[Array[(HalfEdge, HalfEdge)]],
      vSlope: Array[Array[(HalfEdge, HalfEdge)]],
      i: Int,
      j: Int
  ): (HalfEdge, HalfEdge, HalfEdge, HalfEdge) =
    val e1 = horizontal(j)(i)._1     // v_ji -> v_ji1
    val e2 = vSlope(j)(i + 1)._1     // v_ji1 -> v_j1i1
    val e3 = horizontal(j + 1)(i)._2 // v_j1i1 -> v_j1i
    val e4 = vSlope(j)(i)._2         // v_j1i -> v_ji
    (e1, e2, e3, e4)

  /** Create a tiling made of a net of regular triangles
    *
    * @param width
    *   number of triangle pairs (rhombi) on each row
    * @param height
    *   number of triangle pairs (rhombi) on each colum
    */
  def createTriangleNet(width: Int, height: Int): Either[TilingError, TilingDCEL] =
    for
      (refinedWidth, refinedHeight) <- validatePositiveDimensions(width, height)
      tiling                        <- createTriangleNetRefined(refinedWidth, refinedHeight)
    yield tiling

  private def createTriangleNetRefined(
      width: PositiveInt,
      height: PositiveInt
  ): Either[TilingError, TilingDCEL] =
    Right(createTriangleNetUnsafe(width, height))

  private def createTriangleNetUnsafe(
      width: PositiveInt,
      height: PositiveInt
  ): TilingDCEL =
    val w: Int = width
    val h: Int = height

    val triangleAngle = AngleDegree(60)

    val RectNetEdges(vertices, horizontal, vSlope, fOuter) =
      buildRectNetEdges(h, w, triangleAngle)

    // Two triangular faces per rhombus cell
    val faces     =
      Array.tabulate(h, w, 2): (j, i, k) =>
        Face(FaceId((j * w + i) * 2 + k + 1))
    // These diagonals split each rhombus into two equilateral triangles
    val diagonals =
      Array.tabulate(h, w): (j, i) =>
        createTwinPair(vertices(j)(i + 1), vertices(j + 1)(i))

    // Link inner faces
    for j <- 0 until h; i <- 0 until w do
      val face1 = faces(j)(i)(0) // Triangle (v_ji, v_ji1, v_j1i)
      val face2 = faces(j)(i)(1) // Triangle (v_ji1, v_j1i1, v_j1i)

      val (e1, e2, e3, e4) = netHalfEdges(horizontal, vSlope, i, j)
      val e_diag           = diagonals(j)(i)._1 // v_ji1 -> v_j1i
      val e_diag_rev       = diagonals(j)(i)._2 // v_j1i -> v_ji1

      // Link face1
      List(e1, e_diag, e4).linkFace(face1, triangleAngle)

      // Link face2
      List(e2, e3, e_diag_rev).linkFace(face2, triangleAngle)

    val allHalfEdges =
      toHalfEdges(horizontal) ++ toHalfEdges(vSlope) ++ toHalfEdges(diagonals)

    finalizeOuterBoundary(h, w, horizontal, vSlope, fOuter, allHalfEdges)

    TilingDCEL(
      vertices = vertices.flatten.toList,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.flatten.toList,
      outerFace = fOuter
    )

  /** Create a tiling made of a net of identical rhombi
    *
    * @param width
    *   number of rhombi on each row
    * @param height
    *   number of rhombi on each colum
    * @param angle
    *   degree of the first interior angle of each rhombus, the default angle creates a square net
    */
  def createRhombusNet(
      width: Int,
      height: Int,
      angle: AngleDegree = AngleDegree(90)
  ): Either[TilingError, TilingDCEL] =
    for
      (refinedWidth, refinedHeight) <- validatePositiveDimensions(width, height)
      refinedAngle                  <-
        refineOrError[AngleDegree, HexagonInteriorAngle](angle, "angle")

      tiling <- createRhombusNetRefined(refinedWidth, refinedHeight, refinedAngle)
    yield tiling

  private def createRhombusNetRefined(
      width: PositiveInt,
      height: PositiveInt,
      angle: HexagonAngle = defaultRhombusAngle
  ): Either[TilingError, TilingDCEL] =
    Right(createRhombusNetUnsafe(width, height, angle))

  private def createRhombusNetUnsafe(
      width: PositiveInt,
      height: PositiveInt,
      angle: HexagonAngle
  ): TilingDCEL =
    val w: Int = width
    val h: Int = height

    val alpha1 = angle
    val alpha2 = angle.supplement

    val RectNetEdges(vertices, horizontal, vSlope, fOuter) =
      buildRectNetEdges(h, w, angle)
    val faces                                              = netFaces(h, w)

    // Link inner faces
    for j <- 0 until h; i <- 0 until w do
      val face             = faces(j)(i)
      val (e1, e2, e3, e4) = netHalfEdges(horizontal, vSlope, i, j)

      List(e1, e2, e3, e4).linkFace(face, alpha1)
      e2.angle = Some(alpha2)
      e4.angle = Some(alpha2)

    val allHalfEdges =
      toHalfEdges(horizontal) ++ toHalfEdges(vSlope)

    finalizeOuterBoundary(h, w, horizontal, vSlope, fOuter, allHalfEdges)

    TilingDCEL(
      vertices = vertices.flatten.toList,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.toList,
      outerFace = fOuter
    )

  /** Create a tiling made of a net of identical hexagons
    *
    * @param width
    *   number of hexagons on each row
    * @param height
    *   number of hexagons on each column
    * @param angle
    *   interior angle (in degrees) for vertices 0 and 3 of each hexagon. The remaining four interior angles
    *   are all equal and computed to satisfy the polygon angle sum. Constraint: 0 < angle < 180. Default 120
    *   creates the regular honeycomb.
    */
  def createHexagonNet(
      width: Int,
      height: Int,
      angle: AngleDegree = AngleDegree(120)
  ): Either[TilingError, TilingDCEL] =
    for
      (refinedWidth, refinedHeight) <- validatePositiveDimensions(width, height)
      refinedAngle                  <-
        refineOrError[AngleDegree, HexagonInteriorAngle](angle, "angle")
      tiling                        <- createHexagonNetRefined(refinedWidth, refinedHeight, refinedAngle)
    yield tiling

  final private class HexagonInteriorAngle
  private given Constraint[AngleDegree, HexagonInteriorAngle] with
    inline def test(inline value: AngleDegree): Boolean =
      !value.isFullCircle && value.toRational > 0 && value.toRational < 180
    inline def message: String                          =
      "Angle must be between 0 and 180 degrees (exclusive)."

  private type HexagonAngle = AngleDegree :| HexagonInteriorAngle
  private val defaultHexagonAngle: HexagonAngle =
    AngleDegree(120).refineUnsafe[HexagonInteriorAngle]
  private val defaultRhombusAngle: HexagonAngle =
    AngleDegree(90).refineUnsafe[HexagonInteriorAngle]

  private def createHexagonNetRefined(
      width: PositiveInt,
      height: PositiveInt,
      angle: HexagonAngle = defaultHexagonAngle
  ): Either[TilingError, TilingDCEL] =
    Right(createHexagonNetUnsafe(width, height, angle))

  private def createHexagonNetUnsafe(
      width: PositiveInt,
      height: PositiveInt,
      angle: HexagonAngle
  ): TilingDCEL =
    val w: Int             = width
    val h: Int             = height
    val alpha: AngleDegree = angle

    val beta: AngleDegree = (alpha / 2).supplement

    // Interior angles per vertex in CCW order: [alpha, beta, beta, alpha, beta, beta]
    // Exterior turns at vertices: exterior(k) = 180 - interior(k)
    val exteriorAngles: Array[AngleDegree] =
      Array(alpha, beta, beta, alpha, beta, beta).map: angleDegree =>
        angleDegree.supplement

    // IMPORTANT: The exterior turn exterior(k) is applied at vertex k to go from edge k to edge (k+1).
    // Edge headings h(k) are the directions of edges (k) from vertex k to k+1.
    // Hence, h1 = h0 + exterior(1), h2 = h1 + exterior(2). (exterior(0) turns from edge5 to edge0.)
    val h0 = BigRadian(0)
    val h1 = h0 + exteriorAngles(1).toBigRadian
    val h2 = h1 + exteriorAngles(2).toBigRadian

    // Three global unit directions g0, g1, g2 (others are their opposites)
    val g0x = spire.math.cos(h0.toBigDecimal)
    val g0y = spire.math.sin(h0.toBigDecimal)
    val g1x = spire.math.cos(h1.toBigDecimal)
    val g1y = spire.math.sin(h1.toBigDecimal)
    val g2x = spire.math.cos(h2.toBigDecimal)
    val g2y = spire.math.sin(h2.toBigDecimal)

    // Corner offsets as integer triples in the (g0,g1,g2) basis:
    // v0=0, v1=g0, v2=g0+g1, v3=g0+g1+g2, v4=g1+g2, v5=g2
    val cornerTriples: Array[(Int, Int, Int)] =
      Array(
        (0, 0, 0), // k=0
        (1, 0, 0), // k=1
        (1, 1, 0), // k=2
        (1, 1, 1), // k=3
        (0, 1, 1), // k=4
        (0, 0, 1)  // k=5
      )

    inline def baseTriple(i: Int, j: Int): (Int, Int, Int) =
      // T1 = g0+g1, T2 = g1+g2 => base = i*T1 + j*T2 = (i, i+j, j)
      (i, i + j, j)

    inline def addTriples(a: (Int, Int, Int), b: (Int, Int, Int)): (Int, Int, Int) =
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)

    val vertexByTriple = mutable.Map[(Int, Int, Int), Vertex]()
    var vertexCounter  = 1

    def tripleToPoint(n0: Int, n1: Int, n2: Int): BigPoint =
      val x = BigDecimal(n0) * g0x + BigDecimal(n1) * g1x + BigDecimal(n2) * g2x
      val y = BigDecimal(n0) * g0y + BigDecimal(n1) * g1y + BigDecimal(n2) * g2y
      BigPoint(x, y)

    def getOrCreateVertexByTriple(key: (Int, Int, Int)): Vertex =
      vertexByTriple.getOrElseUpdate(
        key, {
          val (n0, n1, n2) = key
          val v            = Vertex(VertexId(vertexCounter), tripleToPoint(n0, n1, n2))
          vertexCounter += 1
          v
        }
      )

    val halfEdgeByDir = mutable.Map[(Vertex, Vertex), HalfEdge]()

    def getOrCreateHalfEdge(v1: Vertex, v2: Vertex): HalfEdge =
      halfEdgeByDir.getOrElseUpdate(
        (v1, v2), {
          val e1 = HalfEdge(v1)
          val e2 = HalfEdge(v2)
          e1.twinWith(e2)
          halfEdgeByDir((v2, v1)) = e2
          e1
        }
      )

    val faces  = netFaces(h, w)
    val fOuter = Face.outer

    for
      j <- 0 until h
      i <- 0 until w
    do
      val face       = faces(j)(i)
      val b          = baseTriple(i, j)
      val cornerKeys =
        (0 until 6)
          .map: k =>
            addTriples(b, cornerTriples(k))
          .toList
      val corners    = cornerKeys.map: (i, j, k) =>
        getOrCreateVertexByTriple(i, j, k)

      val cornerAngles: Array[AngleDegree] =
        Array(alpha, beta, beta, alpha, beta, beta)

      val edgesCCW =
        (0 until 6).toList.map: k =>
          val v1 = corners(k)
          val v2 = corners((k + 1) % 6)
          getOrCreateHalfEdge(v1, v2)

      edgesCCW.linkInCycle()
      edgesCCW.zipWithIndex.foreach: (e, k) =>
        e.incidentFace = Some(face)
        e.angle = Some(cornerAngles(k))
      face.outerComponent = edgesCCW.headOption

    val allVertices  = vertexByTriple.values.toList
    val allHalfEdges = halfEdgeByDir.values.toList

    allVertices.foreach: vertex =>
      vertex.leaving =
        allHalfEdges.find: halfEdge =>
          halfEdge.origin eq vertex

    val boundaryEdgesCW =
      allHalfEdges.filter: halfEdge =>
        halfEdge.incidentFace.isEmpty

    val boundaryOrdered = boundaryEdgesCW.orderBoundary

    if boundaryOrdered.nonEmpty then
      boundaryOrdered.linkInCycle()
      boundaryOrdered.foreach: halfEdge =>
        halfEdge.incidentFace = Some(fOuter)
      fOuter.outerComponent = boundaryOrdered.headOption

      allHalfEdges.setOuterEdgeAngles(boundaryOrdered, fOuter)

    TilingDCEL(
      vertices = allVertices,
      halfEdges = allHalfEdges,
      innerFaces = faces.flatten.toList,
      outerFace = fOuter
    )

  /** Creates a ring structure based on the given regular polygon. If the n sides of the regular polygon are
    * even, the ring is made of n such polygons, plus an inner one if n > 4. If odd, the ring is made of n * 2
    * such polygons; plus an inner one, if n > 3
    *
    * @param polygon
    *   the regular polygon that serves as the basis for the ring structure creation
    */
  def createRing(polygon: RegularPolygon): Either[TilingError, TilingDCEL] =
    val sides: Int = polygon.toSides
    if sides < 3 then
      return Left(
        ValidationError(s"Invalid number of sides: $sides. A regular polygon must have at least 3 sides.")
      )

    val first                     = createRegularPolygon(polygon)
    val areEven: Boolean          = sides % 2 == 0
    val start                     = (sides - (if areEven then 0 else 1)) / 2 + 2
    val step                      = sides - 2
    val end                       = start + step * (sides * (if areEven then 1 else 2) - 1)
    val vertexIds: List[VertexId] =
      Range(start, end, step)
        .map: i =>
          VertexId(i)
        .toList
    vertexIds.foldLeft(Right(first): Either[TilingError, TilingDCEL]): (either, vertexId) =>
      either.flatMap: ring =>
        ring.addRegularPolygonToBoundary(vertexId, polygon)

  def createHoledTriangleNet(
      width: Int,
      height: Int
  )(f: (Int, Int) => Boolean): Either[TilingError, TilingDCEL] =
    val transform: (Int, Int) => VertexId = (x, y) => VertexId(x + 1 + y * (width + 1))
    val holes: IndexedSeq[VertexId]       =
      for
        y <- 0 to height
        x <- 0 to width
        if f(x, y)
      yield transform(x, y)
    for
      base   <- createTriangleNet(width, height)
      tiling <- holes
                  .foldLeft(Right(base): Either[TilingError, TilingDCEL]): (either, vertexId) =>
                    either.flatMap: tilingDCEL =>
                      tilingDCEL.maybeDeleteVertex(vertexId)
    yield tiling

  extension (halfEdges: List[HalfEdge])

    def setOuterEdgeAngles(boundaryEdges: List[HalfEdge], outerFace: Face): Unit =
      boundaryEdges.foreach: outerEdge =>
        val vertex         = outerEdge.origin
        val incidentAtV    =
          halfEdges.filter: halfEdge =>
            halfEdge.origin eq vertex
        val innerAnglesSum = incidentAtV.interiorAnglesSum(outerFace)
        outerEdge.angle = Some(innerAnglesSum.conjugate)
