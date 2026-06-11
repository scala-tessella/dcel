package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingUniformity.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.Tree
import io.github.scala_tessella.dcel.conversion.TilingDOT.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.*
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.startAt

/** An edge-to-edge tessellation of unit-side polygons, modelled as a Doubly Connected Edge List (DCEL): each
  * edge is represented by two oppositely oriented half-edges, and each half-edge knows its origin vertex,
  * incident face, twin, predecessor and successor. The tiling has exactly one unbounded `outerFace`; all
  * other faces are inner.
  *
  * This is the raw, uncertified structure: queries live here, but every mutating operation lives on
  * [[Tiling]], the certified subtype proving its value passed [[TilingValidation.validate]] (ADR-0017).
  * Consumers normally hold a `Tiling`, obtained from the companion's smart constructors
  * ([[TilingDCEL.createRegularPolygon]], [[TilingDCEL.createSimplePolygon]], [[TilingDCEL.fromUntrusted]]),
  * from [[TilingBuilder]] for lattices and rings, or by certifying a raw value with [[Tiling.from]]. The
  * primary constructor is private to enforce validation on untrusted input.
  *
  * Mutating operations on [[Tiling]] return a fresh `Either[TilingError, Tiling]` and operate on an internal
  * deep copy — the original is never modified.
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

  /** True when the tiling is the structurally empty one: no components at all and a bare outer face. The
    * empty tiling is valid (a blank canvas); a tiling with empty lists but a wired outer face is not.
    */
  private[dcel] def isStructurallyEmpty: Boolean =
    vertices.isEmpty && halfEdges.isEmpty && innerFaces.isEmpty &&
      outerFace.outerComponent.isEmpty && outerFace.innerComponents.isEmpty

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
    * angles, since the polygon is traversed externally). Cached on the instance. Unsafe: assumes a
    * well-formed boundary with angles set; the public view is `Tiling.boundarySimplePolygon`.
    */
  private[dcel] lazy val boundarySimplePolygonUnsafe: SimplePolygon =
    SimplePolygon(
      boundaryEdgesUnsafe
        .map: halfEdge =>
          halfEdge.angle.get.conjugate
        .toVector
    )

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
  ): Either[TilingError, Tiling] =
    val candidateTiling = apply(vertices, halfEdges, innerFaces, outerFace)
    validate(candidateTiling).map: _ =>
      Tiling.trusted(candidateTiling)

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
  def createSimplePolygon(degrees: Int*): Either[TilingError, Tiling] =
    TilingBuilder.createSimplePolygon(degrees*)

  /** Variant of [[createSimplePolygon(degrees:Int*)*]] taking a vector of [[geometry.AngleDegree]] values
    * (allowing rational angles).
    */
  def createSimplePolygon(angles: Vector[AngleDegree]): Either[TilingError, Tiling] =
    TilingBuilder.createSimplePolygon(angles)

  /** Creates a tiling consisting of a single regular polygon. Total: delegates to
    * [[TilingBuilder.createRegularPolygon]].
    */
  def createRegularPolygon(polygon: RegularPolygon): Tiling =
    TilingBuilder.createRegularPolygon(polygon)
