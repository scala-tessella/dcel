package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.BigDecimalGeometry.*
import io.github.scala_tessella.dcel.geometry.{
  AngleDegree,
  BigLineSegment,
  BigPoint,
  RegularPolygon,
  SimplePolygon
}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.dcel.TilingAddition.addRegularPolygonToBoundary
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import spire.implicits.*

/** Constructors for [[TilingDCEL]] instances. Two families:
  *
  *   - **Single polygons.** [[createRegularPolygon]] and [[createSimplePolygon]] produce a tiling whose only
  *     inner face is the given polygon.
  *   - **Lattices and rings.** [[createTriangleNet]], [[createRhombusNet]], [[createHexagonNet]],
  *     [[createHoledTriangleNet]] and [[createRing]] build edge-to-edge tilings of arbitrary size from
  *     unit-side regular pieces.
  *
  * All constructors validate their inputs and return `Either[TilingError, Tiling]` (regular polygon is the
  * one exception: its construction cannot fail). The constructed tilings are certified [[Tiling]] values —
  * guaranteed by construction to satisfy the full DCEL invariants (ADR-0017).
  *
  * This object owns input validation and the single-polygon builds; the lattice-construction plumbing lives
  * in the package-private [[TilingNetBuilder]].
  */
object TilingBuilder:

  private type PositiveInt = Int :| Positive

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

  /** Closure check for a candidate polygon's vertex coordinates: the final edge (from the last back to the
    * first point) must have unit length. Used internally by the polygon constructors; exposed for callers
    * that build their own point lists. Returns [[TopologyError]] on failure.
    */
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
  def createSimplePolygon(degrees: Int*): Either[TilingError, Tiling] =
    for
      validatedSimplePolygon <- SimplePolygon.fromUntrusted(degrees*)
    yield Tiling.trusted(createSimplePolygonUnsafe(validatedSimplePolygon))

  /** Variant of [[createSimplePolygon(degrees:Int*)*]] taking a vector of [[geometry.AngleDegree]] values,
    * which allows rational angles. Same validation semantics.
    */
  def createSimplePolygon(angles: Vector[AngleDegree]): Either[TilingError, Tiling] =
    for
      validatedSimplePolygon <- SimplePolygon.fromUntrusted(angles)
    yield Tiling.trusted(createSimplePolygonUnsafe(validatedSimplePolygon))

  /** Creates a TilingDCEL for a single regular polygon with unit-length sides.
    *
    * @param polygon
    *   The [[geometry.RegularPolygon]] to create.
    */
  def createRegularPolygon(polygon: RegularPolygon): Tiling =
    val angles = polygon.angles
    val points = calculateVertexPoints(angles)
    Tiling.trusted(buildDCELFromPointsUnsafe(points, angles.toList))

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

  /** Create a tiling made of a net of regular triangles
    *
    * @param width
    *   number of triangle pairs (rhombi) on each row
    * @param height
    *   number of triangle pairs (rhombi) on each colum
    */
  def createTriangleNet(width: Int, height: Int): Either[TilingError, Tiling] =
    for
      (refinedWidth, refinedHeight) <- validatePositiveDimensions(width, height)
    yield Tiling.trusted(TilingNetBuilder.createTriangleNetUnsafe(refinedWidth, refinedHeight))

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
  ): Either[TilingError, Tiling] =
    for
      (refinedWidth, refinedHeight) <- validatePositiveDimensions(width, height)
      refinedAngle                  <-
        refineOrError[AngleDegree, HexagonInteriorAngle](angle, "angle")
    yield Tiling.trusted(TilingNetBuilder.createRhombusNetUnsafe(refinedWidth, refinedHeight, refinedAngle))

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
  ): Either[TilingError, Tiling] =
    for
      (refinedWidth, refinedHeight) <- validatePositiveDimensions(width, height)
      refinedAngle                  <-
        refineOrError[AngleDegree, HexagonInteriorAngle](angle, "angle")
    yield Tiling.trusted(TilingNetBuilder.createHexagonNetUnsafe(refinedWidth, refinedHeight, refinedAngle))

  final private class HexagonInteriorAngle
  private given Constraint[AngleDegree, HexagonInteriorAngle] with
    inline def test(inline value: AngleDegree): Boolean =
      !value.isFullCircle && value.toRational > 0 && value.toRational < 180
    inline def message: String                          =
      "Angle must be between 0 and 180 degrees (exclusive)."

  /** Creates a ring structure based on the given regular polygon. If the n sides of the regular polygon are
    * even, the ring is made of n such polygons, plus an inner one if n > 4. If odd, the ring is made of n * 2
    * such polygons; plus an inner one, if n > 3
    *
    * @param polygon
    *   the regular polygon that serves as the basis for the ring structure creation
    */
  def createRing(polygon: RegularPolygon): Either[TilingError, Tiling] =
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
    vertexIds
      .foldLeft(Right(first): Either[TilingError, TilingDCEL]): (either, vertexId) =>
        either.flatMap: ring =>
          ring.addRegularPolygonToBoundary(vertexId, polygon)
      .map(Tiling.trusted)

  /** Creates a triangle net of the given dimensions and then deletes the faces around every vertex selected
    * by the predicate `f`, producing a tiling with intentional holes.
    *
    * @param width
    *   number of triangle pairs (rhombi) on each row, same as [[createTriangleNet]]
    * @param height
    *   number of triangle pairs (rhombi) on each column, same as [[createTriangleNet]]
    * @param f
    *   `(x, y) => true` to mark the vertex at grid position `(x, y)` as a hole centre. The triangles around
    *   each marked vertex are removed; the resulting boundary may have inner holes.
    * @return
    *   The holed tiling, or a [[TilingError]] if the underlying net construction or any of the deletions
    *   violates an invariant.
    */
  def createHoledTriangleNet(
      width: Int,
      height: Int
  )(f: (Int, Int) => Boolean): Either[TilingError, Tiling] =
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
                  .foldLeft(Right(base): Either[TilingError, Tiling]): (either, vertexId) =>
                    either.flatMap: certified =>
                      certified.maybeDeleteVertex(vertexId)
    yield tiling
