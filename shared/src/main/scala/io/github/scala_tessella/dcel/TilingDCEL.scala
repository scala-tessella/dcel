package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingMultiplication.*
import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingUniformity.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.Tree
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.conversion.TilingDOT.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.*
import io.github.scala_tessella.dcel.geometry.{
  AngleDegree,
  BigPoint,
  BigRadian,
  RegularPolygon,
  SimplePolygon
}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.startAt

/** An edge-to-edge tessellation of unit-side polygons, modelled as a Doubly Connected Edge List (DCEL): each
  * edge is represented by two oppositely oriented half-edges, and each half-edge knows its origin vertex,
  * incident face, twin, predecessor and successor. The tiling has exactly one unbounded `outerFace`; all
  * other faces are inner.
  *
  * Construct via the companion's smart constructors ([[TilingDCEL.empty]],
  * [[TilingDCEL.createRegularPolygon]], [[TilingDCEL.createSimplePolygon]], [[TilingDCEL.fromUntrusted]]) or
  * via [[TilingBuilder]] for lattices and rings. The primary constructor is private to enforce validation on
  * untrusted input.
  *
  * Mutating operations ([[maybeAddRegularPolygonToBoundary]], [[maybeDeleteFace]], …) return a fresh
  * `Either[TilingError, TilingDCEL]` and operate on an internal deep copy — the original is never modified.
  *
  * @param vertices
  *   All vertices in the tiling.
  * @param halfEdges
  *   All half-edges in the tiling (each edge appears twice, once per direction).
  * @param innerFaces
  *   The tiling's bounded interior faces.
  * @param outerFace
  *   The single unbounded outer face.
  */
final case class TilingDCEL private (
    vertices: List[Vertex],
    halfEdges: List[HalfEdge],
    innerFaces: List[Face],
    outerFace: Face
):

  /** True when the tiling has no vertices (i.e. [[TilingDCEL.empty]]). */
  def isEmpty: Boolean =
    vertices.isEmpty

  /** All vertex coordinates indexed by `VertexId`. Computed on each call. */
  def coordinates: Map[VertexId, BigPoint] =
    vertices
      .map: vertex =>
        vertex.id -> vertex.coords
      .toMap

  /** All faces of the tiling, both inner and outer (the outer face is the head). */
  def faces: List[Face] =
    outerFace :: innerFaces

  /** True when every inner face is a unit regular polygon (all interior angles equal). */
  def hasUnitRegularPolygonsOnly: Boolean =
    innerFaces.forall: face =>
      face.hasEqualAnglesUnsafe

  private[dcel] def findVertexUnsafe(vertexId: VertexId): Option[Vertex] =
    vertices.find: vertex =>
      vertex.id == vertexId

  /** Look up a vertex by id. */
  def findVertex(vertexId: VertexId): Either[NotFoundError, Vertex] =
    findVertexUnsafe(vertexId).toRight(NotFoundError("Vertex", vertexId.toPrefixedString))

  /** Look up any face (inner or outer) by id. */
  def findFace(faceId: FaceId): Either[NotFoundError, Face] =
    faces
      .find: face =>
        face.id == faceId
      .toRight(NotFoundError("Face", faceId.toPrefixedString))

  /** Look up an inner (bounded) face by id. Returns [[NotFoundError]] if `faceId` matches the outer face. */
  def findInnerFace(faceId: FaceId): Either[NotFoundError, Face] =
    innerFaces
      .find: face =>
        face.id == faceId
      .toRight(NotFoundError("Inner face", faceId.toPrefixedString))

  /** Checks if the given edge is on the boundary.
    * @return
    *   false if the edge is not on the boundary or doesn't belong to the tiling, true otherwise.
    */
  def isBoundaryEdge(halfEdge: HalfEdge): Boolean =
    halfEdge.hasIncidentFace(outerFace)

  /** Finds the two vertices and the half-edge starting at the first and ending at the second.
    *
    * @param vertexId1
    *   id of the origin vertex
    * @param vertexId2
    *   id of the destination vertex
    * @return
    *   `Right((v1, v2, edge))` on success, or [[NotFoundError]] if either vertex is missing or no edge
    *   connects them.
    */
  def findVerticesAndEdgeBetween(
      vertexId1: VertexId,
      vertexId2: VertexId
  ): Either[NotFoundError, (Vertex, Vertex, HalfEdge)] =
    for
      v1   <- findVertex(vertexId1)
      v2   <- findVertex(vertexId2)
      edge <-
        v1.findEdgeBetweenUnsafe(v2)
          .toRight(NotFoundError(
            "Edge",
            s"between ${vertexId1.toPrefixedString} and ${vertexId2.toPrefixedString}"
          ))
    yield (v1, v2, edge)

  /** Pairs each inner face id with the vertices on its boundary, in face-traversal order. */
  def innerFacesVertices: List[(FaceId, List[Vertex])] =
    innerFaces.map: face =>
      (face.id, face.getVerticesUnsafe)

  /** Vertices of the inner face with the given id, in face-traversal order. */
  def findInnerFaceVertices(faceId: FaceId): Either[NotFoundError, List[Vertex]] =
    for
      face <- findInnerFace(faceId)
    yield face.getVerticesUnsafe

  private[dcel] def getAnglesAtVertexUnsafe(vertex: Vertex): List[AngleDegree] =
    val edges = vertex.incidentEdgesUnsafe
    edges.map: halfEdge =>
      halfEdge.angle.get

  /** Returns the ordered angles at the given vertex (full surround for interior vertices, partial for
    * boundary vertices).
    *
    * @param vertexId
    *   id of the vertex
    */
  def getAnglesAtVertex(vertexId: VertexId): Either[NotFoundError, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId)
    yield getAnglesAtVertexUnsafe(vertex)

  private[dcel] def getInnerAnglesAtVertexUnsafe(vertex: Vertex): List[AngleDegree] =
    val edges             = vertex.incidentEdgesUnsafe
    val boundaryEdgeIndex =
      edges.indexWhere: halfEdge =>
        isBoundaryEdge(halfEdge)
    val filteredEdges     =
      boundaryEdgeIndex match
        case -1 => edges
        case i  => edges.startAt(i).tail
    filteredEdges.map: halfEdge =>
      halfEdge.angle.get

  /** Returns the ordered inner angles at the given vertex. For a boundary vertex this excludes the outer-face
    * angle; for an interior vertex it equals [[getAnglesAtVertex]].
    *
    * @param vertexId
    *   id of the vertex
    */
  def getInnerAnglesAtVertex(vertexId: VertexId): Either[NotFoundError, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId)
    yield getInnerAnglesAtVertexUnsafe(vertex)

  /** Retrieves a reduced TilingDCEL around a vertex containing only the polygons reached within the given
    * vertex-distance. Distance is clamped to >= 0.
    *
    * @param vertexId
    *   The ID of the vertex around which the reduced TilingDCEL is generated.
    * @param distance
    *   The vertex-radius to include (0 = only polygons incident to the center; 1 = also polygons around the
    *   neighbors of the center; etc.).
    * @return
    *   An `Either` containing the reduced TilingDCEL if the operation succeeds or a `NotFoundError` if the
    *   specified vertex is not found.
    */
  def getDcelAtVertex(vertexId: VertexId, distance: Int = 0): Either[NotFoundError, TilingDCEL] =
    this.getStructureAtVertex(vertexId, distance).map:
      (newVertices, newHalfEdges, localOuter, newInnerFaces) =>
        TilingDCEL(
          vertices = newVertices,
          halfEdges = newHalfEdges,
          innerFaces = newInnerFaces,
          outerFace = localOuter
        )

  /** Compressed uniformity tree of the tiling: leaves are groups of vertex ids that share the same local
    * surround signature, branches reflect the equivalence-class hierarchy.
    */
  def uniformityTree: Tree[List[VertexId]] =
    this.uniformityTreeUncompressed().compress:
      _ ::: _

  /** Computes the gonality trees for the given uniformity tree structure, generating a list of partial trees
    * with representative vertex ids. It ensures that the root branches are not empty.
    *
    * @return
    *   A list of trees where each tree represents a simplified slice of the original uniformity tree, with
    *   just one representative vertex id instead of the full list.
    */
  def gonalityTrees: List[Tree[VertexId]] =
    val adjusted = uniformityTree match
      case Leaf(Nil)         => Leaf(Nil)
      case Leaf(value)       => Branch(value, List(Leaf(value)))
      case branch: Branch[?] => branch
    adjusted.ensureDepthOneBranchesHaveValidValues(_.isEmpty, _.head.firstLeaf.get)
      .children
      .map: child =>
        child.map: vertexIds =>
          vertexIds.headOption.getOrElse(VertexId(-1))
      .map:
        case leaf: Leaf[VertexId]     => leaf
        case child @ Branch(value, _) =>
          Branch(
            value,
            child.flattenLeaves.map: vertexId =>
              Leaf(vertexId)
          )

  /** Pairs each gonality tree with the list of regular polygons incident to its representative vertex.
    * Unsafe: skips validation and throws if the representative vertex's surround is broken.
    */
  def gonalityTreesUnsafe: List[(List[RegularPolygon], Tree[VertexId])] =
    gonalityTrees.map: tree =>
      (this.regularPolygonsUnsafeFrom(tree.value), tree)

  /** True when every inner face is reachable from any other via face-adjacency steps (no disconnected
    * components). Empty tilings are considered connected.
    */
  def hasConnectedFaces: Boolean =
    innerFaces.isConnected

  /** Finds the outer boundary of the tiling.
    *
    * The traversal follows the half-edges of the outer face, which are linked in a clockwise order around the
    * perimeter.
    *
    * @return
    *   Either a Vector of Vertices forming the perimeter in clockwise order, or a [[TopologyError]] if the
    *   outer-face traversal fails. An empty Vector is returned when the outer face has no boundary component.
    */
  def boundaryVertices: Either[TopologyError, Vector[Vertex]] =
    outerFace.outerComponent match
      case None            => Right(Vector.empty)
      case Some(startEdge) =>
        startEdge
          .faceTraversal: halfEdge =>
            halfEdge.origin
          .map: vertices =>
            vertices.toVector

  /** Unsafe counterpart of [[boundaryVertices]]: assumes the tiling's outer face is well-formed and skips
    * topology validation. Throws if the outer-face traversal is broken.
    */
  private[dcel] def boundaryVerticesUnsafe: Vector[Vertex] =
    outerFace.outerComponent match
      case None            => Vector.empty
      case Some(startEdge) =>
        startEdge
          .faceTraversalUnsafe: halfEdge =>
            halfEdge.origin
          .toVector

  /** All vertices in the tiling, except those on the outer boundary. */
  def innerVertices: List[Vertex] =
    vertices.diff(boundaryVerticesUnsafe)

  /** Finds the ordered half-edges forming the outer boundary of the tiling.
    *
    * @return
    *   Either the ordered boundary half-edges, or a [[TopologyError]] if the outer-face traversal fails. An
    *   empty list is returned when the outer face has no boundary component.
    */
  def boundaryEdges: Either[TopologyError, List[HalfEdge]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal()
      case None            => Right(List.empty)

  /** Unsafe counterpart of [[boundaryEdges]]: assumes the tiling's outer face is well-formed and skips
    * topology validation. Throws if the outer-face traversal is broken.
    */
  private[dcel] def boundaryEdgesUnsafe: List[HalfEdge] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalUnsafe()
      case None            => List.empty

  /** The outer boundary expressed as a `SimplePolygon` (angles are the conjugates of the boundary half-edge
    * angles, since the polygon is traversed externally). Cached.
    */
  lazy val boundarySimplePolygon: SimplePolygon =
    SimplePolygon(
      boundaryEdgesUnsafe
        .map: halfEdge =>
          halfEdge.angle.get.conjugate
        .toVector
    )

  /** Adds a regular polygon to the tiling along the outer boundary.
    *
    * Preconditions:
    *   - onEdgeStartingWithVertexId identifies a half-edge that belongs to the current outer boundary.
    *   - sides >= 3.
    *   - The operation must not introduce self-intersections on the boundary.
    *
    * Postconditions on success:
    *   - A new inner face representing the regular polygon is added.
    *   - The outer boundary is updated to exclude any edges consumed by the new face and include the new
    *     outer edges.
    *   - All DCEL invariants are preserved: every inserted half-edge has a twin, coherent next/prev links,
    *     correct incident faces, and updated vertex leaving edges.
    *   - The returned TilingDCEL is a new instance reflecting the mutation.
    *
    * Failure cases:
    *   - Returns a TilingError when the edge is not on the boundary, sides are invalid, or the growth would
    *     cause boundary intersections or violate topology/geometry constraints.
    */
  def maybeAddRegularPolygonToBoundary(
      onEdgeStartingWith: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addRegularPolygonToBoundary(onEdgeStartingWith, polygon)

  /** Adds an arbitrary simple polygon (described by its interior angles, in degrees) to the outer boundary,
    * starting at the given vertex. The polygon's first edge sits on the boundary edge starting at
    * `onEdgeStartingWith`.
    *
    * @return
    *   A fresh tiling with the polygon appended, or a `TilingError` if the angles do not close, the polygon
    *   would self-intersect, or the precondition checks fail.
    */
  def maybeAddSimplePolygonToBoundary(
      onEdgeStartingWith: VertexId,
      angles: Vector[AngleDegree]
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addUntrustedSimplePolygonToBoundary(onEdgeStartingWith, angles)

  /** Adds a regular polygon to the outer boundary along the path between two boundary vertices, growing
    * outward. Use this overload when you want to specify both endpoints rather than only the starting edge.
    */
  def maybeAddRegularPolygon(
      start: VertexId,
      end: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addRegularPolygon(start, end, polygon)

  /** Two-endpoint variant of [[maybeAddSimplePolygonToBoundary]]: adds a simple polygon (described by its
    * interior angles) to the boundary along the path from `start` to `end`.
    */
  def maybeAddSimplePolygon(
      start: VertexId,
      end: VertexId,
      angles: Vector[AngleDegree]
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addUntrustedSimplePolygon(start, end, angles)

  /** Doubles the tiling area by translating it across one of its parallelogon period directions (or by
    * reflecting it across the centroid for an equilateral-triangle boundary). Useful for growing periodic
    * tilings without computing per-vertex additions.
    *
    * @return
    *   The doubled tiling, or [[ValidationError]] when the tiling's boundary is not a parallelogon (and not
    *   an equilateral triangle).
    */
  def doubleArea: Either[TilingError, TilingDCEL] =
    if isEmpty then
      Right(this)
    else
      val polygon       = boundarySimplePolygon
      lazy val boundary = boundaryVerticesUnsafe
      polygon.parallelogonDoubleIndices match
        case None if polygon.isEquilateralTriangle =>
          val angles = polygon.toAngles
          val origin =
            angles.indexWhere: angleDegree =>
              angleDegree == AngleDegree(60)
          val repeat = (angles.size / 3) + origin
          Right(this.rawDouble(
            boundary(origin),
            boundary(repeat),
            withInversion = true
          ))
        case None                                  => Left(ValidationError("Tiling is not a parallelogon, cannot fill the whole plane."))
        case Some((origin, repeat))                =>
          Right(this.rawDouble(boundary(origin), boundary(repeat)))

  /** Adds a copy of the whole tiling to itself under the given [[Isometry]] (translation, rotation,
    * reflection or glide reflection), merging it in and validating the result in full. The single primitive
    * behind the four named `maybeAdd…Copy` convenience methods.
    *
    * The result is valid only if the copy lands outside the existing tiling or exactly follows its
    * composition: coincident vertices are unified, shared edges collapsed and fully-overlapping faces
    * deduplicated. An isometry that maps the tiling onto an existing symmetry reproduces the original.
    *
    * @return
    *   The grown tiling, or a [[TilingError]] when the copy conflicts with the existing composition (partial
    *   overlap, crossing edges, over-full vertex angles, a degenerate reflection axis, ...).
    */
  def maybeAddCopy(isometry: Isometry): Either[TilingError, TilingDCEL] =
    isometry match
      case Isometry.Translation(from, to)           =>
        val delta: BigPoint = to - from
        this.addIsometricCopy(_ + delta)
      case Isometry.Rotation(center, degrees)       =>
        val angle: BigRadian = degrees.toBigRadian
        this.addIsometricCopy(_.rotatedAround(center, angle))
      case Isometry.Reflection(axisP1, axisP2)      =>
        if axisP1.almostEquals(axisP2) then
          Left(ValidationError("A mirror axis requires two distinct points."))
        else
          this.addReflectedCopy(_.reflectedAcross(axisP1, axisP2))
      case Isometry.GlideReflection(axisP1, axisP2) =>
        if axisP1.almostEquals(axisP2) then
          Left(ValidationError("A glide-reflection axis requires two distinct points."))
        else
          val glide: BigPoint = axisP2 - axisP1
          this.addReflectedCopy(point => point.reflectedAcross(axisP1, axisP2) + glide)

  def maybeAddTranslatedCopy(from: BigPoint, to: BigPoint): Either[TilingError, TilingDCEL] =
    maybeAddCopy(Isometry.Translation(from, to))

  /** Adds a rotated copy of the whole tiling to itself, rotating it about `center` (an arbitrary point — a
    * vertex, an edge midpoint, a face centre, anything) by `degrees`.
    *
    * Sign convention (ADR-0011): `degrees` is positive **clockwise** as rendered in the SVG view (negative
    * counterclockwise). Internally this is a counterclockwise rotation in the y-up model frame, which the
    * `flippedY` export turns into a clockwise on-screen rotation.
    *
    * As with [[maybeAddTranslatedCopy]], the result is valid only if the copy lands outside the existing
    * tiling or exactly follows its composition; the merged tiling is validated in full. Because the copy must
    * snap onto the existing unit-edge composition, only rotations by a local symmetry angle (a multiple of
    * 60°/90°/... by configuration) will succeed — most arbitrary angles are rejected.
    *
    * @return
    *   The grown tiling, or a [[TilingError]] when the copy conflicts with the existing composition.
    */
  def maybeAddRotatedCopy(center: BigPoint, degrees: AngleDegree): Either[TilingError, TilingDCEL] =
    maybeAddCopy(Isometry.Rotation(center, degrees))

  /** Adds a mirrored copy of the whole tiling to itself, reflecting it across the line through `axisP1` and
    * `axisP2` (arbitrary points — vertices, edge midpoints, anything).
    *
    * Reflection is the orientation-reversing isometry, so the copy's DCEL wiring is rebuilt with reversed
    * orientation (unlike translate/rotate); the coordinate map itself is exact in `BigDecimal`. As with the
    * other copy operations, the result is valid only if the copy lands outside the existing tiling or exactly
    * follows its composition, and the merged tiling is validated in full. Reflecting across an existing
    * symmetry axis reproduces the original.
    *
    * @return
    *   The grown tiling, or a [[TilingError]] when the two axis points coincide, or when the copy conflicts
    *   with the existing composition.
    */
  def maybeAddMirroredCopy(axisP1: BigPoint, axisP2: BigPoint): Either[TilingError, TilingDCEL] =
    maybeAddCopy(Isometry.Reflection(axisP1, axisP2))

  /** Adds a glide-reflected copy of the whole tiling to itself: reflect across the line through `axisP1` and
    * `axisP2`, then slide along that line by the vector `axisP2 - axisP1`. The fourth plane isometry,
    * orientation-reversing like a plain reflection but with a built-in translation along the axis (the
    * symmetry of running-bond and many monohedral tilings).
    *
    * @return
    *   The grown tiling, or a [[TilingError]] when the two axis points coincide, or when the copy conflicts
    *   with the existing composition.
    */
  def maybeAddGlideReflectedCopy(axisP1: BigPoint, axisP2: BigPoint): Either[TilingError, TilingDCEL] =
    maybeAddCopy(Isometry.GlideReflection(axisP1, axisP2))

  /** Fills the gap around `vertexId` with as many triangles as fit, producing a fan whose apex is the named
    * vertex. Useful for closing small angular gaps on the boundary.
    */
  def fanAt(vertexId: VertexId): Either[TilingError, TilingDCEL] =
    for
      vertex <- findVertex(vertexId)
      result <- this.rawFan(vertex)
    yield result

  /** Fans `order` rotated copies of the whole tiling around an arbitrary `center` point — each rotated by a
    * full `360 / order` slice — into one rotationally-symmetric patch. The general, full-ring counterpart of
    * [[fanAt]]: the centre may be any point (a face centroid, an edge midpoint, ...), not only a boundary
    * vertex.
    *
    * The ring always spans the complete 360°: it succeeds only if every wedge fits the existing composition
    * (a centre face that maps onto itself is deduplicated; coincident vertices and edges are unified) and the
    * whole patch validates — otherwise it returns a [[TilingError]] (strict). For example, mirroring a
    * pentagon across an edge and then `fanAround`-ing the central pentagon's centroid with `order = 5`
    * produces the six-pentagon Dürer cluster.
    *
    * For a regular n-gon centred on its own centroid, `order = n` is the canonical full fan; any divisor of
    * `n` is also geometrically valid. A best-effort partial fan is intentionally not offered here — chain
    * [[maybeAddRotatedCopy]] until the first `Left` to fill one side as far as it goes.
    *
    * @return
    *   The fanned tiling, or a [[TilingError]] if `order < 2` or any wedge conflicts with the composition.
    */
  def fanAround(center: BigPoint, order: Int): Either[TilingError, TilingDCEL] =
    this.rawFanAround(center, order)

  /** Repeats the whole tiling `count` times in a strip, each copy translated by a further step of the vector
    * from `from` to `to` (`k · (to − from)` for `k = 0 until count`). The translational counterpart of
    * [[fanAround]]; the `count = 2` period-detected special case is [[doubleArea]].
    *
    * All copies are merged and the completed strip is validated once; a copy that conflicts with the
    * composition surfaces as a self-intersection or over-full vertex in that final check. Copies that exactly
    * overlap (a period equal to part of the tiling) are deduplicated.
    *
    * @return
    *   The repeated tiling (the original unchanged when `count == 1`), or a [[TilingError]] if `count < 1` or
    *   a copy conflicts with the composition.
    */
  def repeatAlong(from: BigPoint, to: BigPoint, count: Int): Either[TilingError, TilingDCEL] =
    this.rawRepeatAlong(to - from, count)

  /** Repeats the whole tiling over a 2-D lattice: `countA` copies stepped by `toA − from`, each of those rows
    * then repeated `countB` times stepped by `toB − from`. A convenience composition of two [[repeatAlong]]
    * sweeps (the editor can equally chain `repeatAlong(...).flatMap(_.repeatAlong(...))`).
    *
    * @return
    *   The lattice patch, or a [[TilingError]] if either count is `< 1` or a copy conflicts with the
    *   composition.
    */
  def repeatGrid(
      from: BigPoint,
      toA: BigPoint,
      countA: Int,
      toB: BigPoint,
      countB: Int
  ): Either[TilingError, TilingDCEL] =
    repeatAlong(from, toA, countA).flatMap: strip =>
      strip.repeatAlong(from, toB, countB)

  /** Removes the vertex and the edges/faces incident to it. Fails with a [[TopologyError]] if the deletion
    * would disconnect the tiling or break a topological invariant.
    */
  def maybeDeleteVertex(vertexId: VertexId): Either[TilingError, TilingDCEL] =
    this.deepCopy.deleteVertex(vertexId)

  /** Removes the edge connecting `startVertexId` to `endVertexId` (and its twin), merging the two incident
    * faces. Fails with a [[TopologyError]] if the edge is missing or the merge would violate an invariant.
    */
  def maybeDeleteEdge(startVertexId: VertexId, endVertexId: VertexId): Either[TilingError, TilingDCEL] =
    this.deepCopy.deleteEdge(startVertexId, endVertexId)

  /** Deletes an inner face from the tiling.
    *
    * Preconditions:
    *   - faceId references an existing inner (bounded) face.
    *   - Deleting the face does not partition the tiling into disconnected components except for the intended
    *     boundary change.
    *
    * Postconditions on success:
    *   - The face and its incident half-edges that are not shared with other faces are removed or repurposed.
    *   - If the deleted face touches the outer boundary, the boundary expands accordingly by relinking or
    *     creating appropriate boundary half-edges.
    *   - All DCEL invariants remain satisfied (twin, next/prev, incidentFace, vertex leaving edges).
    *   - The returned TilingDCEL is a new instance reflecting the mutation. If the deleted face was the only
    *     inner face, the result may be an empty tessellation.
    *
    * Failure cases:
    *   - Returns a TilingError when the face does not exist, when the removal would split the tiling
    *     invalidly, or when integrity checks fail.
    */
  def maybeDeleteFace(faceId: FaceId): Either[TilingError, TilingDCEL] =
    this.deepCopy.deleteFace(faceId)

  /** Generates an SVG representation of the tiling. The width, height, and viewBox are automatically
    * calculated to fit the tiling at the given scale.
    *
    * @param strokeWidth
    *   The width of the edge lines.
    * @param padding
    *   The padding around the tiling within the SVG viewBox.
    * @param scale
    *   The factor by which to scale the tiling coordinates.
    * @return
    *   A String containing the SVG markup.
    */
  def toSVG(
      strokeWidth: Double = 1.0,
      padding: Double = 20.0,
      scale: Double = 50.0,
      showHalfEdgeTraversal: Boolean = false,
      leavingEdgeMarkers: Boolean = false,
      faceIdsOnEdges: Boolean = false,
      showUniformity: Boolean = false
  ): String =
    this.toScalableVectorGraphics(
      strokeWidth,
      padding,
      scale,
      showHalfEdgeTraversal,
      leavingEdgeMarkers,
      faceIdsOnEdges,
      showUniformity
    )

  /** [[toSVG]] taking a bundled [[conversion.TilingSVG.SvgOptions]] instead of individual parameters. */
  def toSVG(options: SvgOptions): String =
    this.toScalableVectorGraphics(options)

  /** Renders the tiling as a Graphviz DOT graph: vertices become nodes, edges become labelled connections.
    * Useful for visualising the half-edge topology.
    */
  def toDOT: String =
    this.toSimplifiedDOT

object TilingDCEL:

  /** Private internal constructor that bypasses validation. Use [[fromUntrusted]] for input from untrusted
    * sources (parsed files, hand-built fixtures) — it validates before returning.
    */
  private[dcel] def apply(
      vertices: List[Vertex],
      halfEdges: List[HalfEdge],
      innerFaces: List[Face],
      outerFace: Face
  ): TilingDCEL =
    new TilingDCEL(vertices, halfEdges, innerFaces, outerFace)

  /** Builds a tiling from a hand-assembled set of components and validates it via
    * [[TilingValidation.validate]].
    *
    * @return
    *   The validated tiling, or a [[TilingError]] describing why the candidate is rejected.
    */
  def fromUntrusted(
      vertices: List[Vertex],
      halfEdges: List[HalfEdge],
      innerFaces: List[Face],
      outerFace: Face
  ): Either[TilingError, TilingDCEL] =
    val candidateTiling = apply(vertices, halfEdges, innerFaces, outerFace)
    validate(candidateTiling).map: _ =>
      candidateTiling

  /** The empty tiling: no vertices, no edges, no inner faces, just the bare outer face. */
  def empty: TilingDCEL =
    TilingDCEL(
      vertices = List.empty,
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

  /** Creates a tiling consisting of a single simple polygon described by its interior angles in integer
    * degrees. Delegates to [[TilingBuilder.createSimplePolygon]].
    */
  def createSimplePolygon(degrees: Int*): Either[TilingError, TilingDCEL] =
    TilingBuilder.createSimplePolygon(degrees*)

  /** Variant of [[createSimplePolygon(degrees:Int*)*]] taking a vector of [[geometry.AngleDegree]] values
    * (allowing rational angles).
    */
  def createSimplePolygon(angles: Vector[AngleDegree]): Either[TilingError, TilingDCEL] =
    TilingBuilder.createSimplePolygon(angles)

  /** Creates a tiling consisting of a single regular polygon. Total: delegates to
    * [[TilingBuilder.createRegularPolygon]].
    */
  def createRegularPolygon(polygon: RegularPolygon): TilingDCEL =
    TilingBuilder.createRegularPolygon(polygon)
