package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingUniformity.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.Tree
import io.github.scala_tessella.dcel.Tree.{Branch, Leaf}
import io.github.scala_tessella.dcel.conversion.TilingDOT.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.*
//import io.github.scala_tessella.dcel.geometry.SimplePolygon.ParallelogramTranslation
import io.github.scala_tessella.dcel.geometry.{AngleDegree, BigPoint, RegularPolygon, SimplePolygon}
import io.github.scala_tessella.dcel.structure.{Face, FaceId, HalfEdge, Vertex, VertexId}
import io.github.scala_tessella.ring_seq.RingSeq.startAt

/** Represents the entire tiling structure as a container for its components.
  *
  * @param vertices
  *   List of all vertices in the tiling.
  * @param halfEdges
  *   List of all half-edges in the tiling.
  * @param innerFaces
  *   List of the tiling's interior faces.
  * @param outerFace
  *   The single, unbounded outer face of the tiling.
  */
final case class TilingDCEL private (
    vertices: List[Vertex],
    halfEdges: List[HalfEdge],
    innerFaces: List[Face],
    outerFace: Face
):

  def isEmpty: Boolean =
    vertices.isEmpty

  def coordinates: Map[VertexId, BigPoint] =
    vertices
      .map: vertex =>
        vertex.id -> vertex.coords
      .toMap

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  def hasUnitRegularPolygonsOnly: Boolean =
    innerFaces.forall: face =>
      face.hasEqualAnglesUnsafe

  private[dcel] def findVertexUnsafe(vertexId: VertexId): Option[Vertex] =
    vertices.find: vertex =>
      vertex.id == vertexId

  def findVertex(vertexId: VertexId): Either[NotFoundError, Vertex] =
    findVertexUnsafe(vertexId).toRight(NotFoundError("Vertex", vertexId.toPrefixedString))

  def findFace(faceId: FaceId): Either[NotFoundError, Face] =
    faces
      .find: face =>
        face.id == faceId
      .toRight(NotFoundError("Face", faceId.toPrefixedString))

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

  /** Finds the edge between the two given vertices.
    *
    * @param vertexId1
    *   id of the first vertex
    * @param vertexId2
    *   id the second vertex
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

  def innerFacesVertices: List[(FaceId, List[Vertex])] =
    innerFaces.map: face =>
      (face.id, face.getVerticesUnsafe)

  def findInnerFaceVertices(faceId: FaceId): Either[NotFoundError, List[Vertex]] =
    for
      face <- findInnerFace(faceId)
    yield face.getVerticesUnsafe

  private[dcel] def getAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex = findVertexUnsafe(vertexId).get
    val edges  = vertex.incidentEdgesUnsafe
    edges.map: halfEdge =>
      halfEdge.angle.get

  /** Returns the ordered angles at the given vertex.
    *
    * @param vertexId
    *   id of the vertex
    */
  def getAnglesAtVertex(vertexId: VertexId): Either[NotFoundError, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId)
    yield getAnglesAtVertexUnsafe(vertexId)

  private[dcel] def getInnerAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex            = findVertexUnsafe(vertexId).get
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

  /** Returns the ordered inner angles at the given vertex.
    *
    * @param vertexId
    *   id of the vertex
    */
  def getInnerAnglesAtVertex(vertexId: VertexId): Either[NotFoundError, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId)
    yield getInnerAnglesAtVertexUnsafe(vertexId)

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
    uniformityTree
      .ensureDepthOneBranchesHaveValidValues(_.isEmpty, _.flatMap(_.value))
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

  def gonalityTreesUnsafe: List[(List[RegularPolygon], Tree[VertexId])] =
    gonalityTrees.map: tree =>
      (this.regularPolygonsUnsafeFrom(tree.value), tree)

  def hasConnectedFaces: Boolean =
    innerFaces.isConnected

  /** Finds the outer boundary of the tiling.
    *
    * The traversal follows the half-edges of the outer face, which are linked in a clockwise order around the
    * perimeter.
    *
    * @return
    *   A Vector of Vertices forming the perimeter, in clockwise order. Returns an empty Vector if the outer
    *   face has no boundary component.
    */
  def boundaryVertices: Vector[Vertex] =
    outerFace.outerComponent match
      case None            => Vector.empty
      case Some(startEdge) =>
        startEdge
          .faceTraversalUnsafe: halfEdge =>
            halfEdge.origin
          .toVector

  /** For validation purposes only. */
  private[dcel] def boundaryVerticesSafer: Either[TopologyError, Vector[Vertex]] =
    outerFace.outerComponent match
      case None            => Right(Vector.empty)
      case Some(startEdge) =>
        startEdge
          .faceTraversal: halfEdge =>
            halfEdge.origin
          .map: vertices =>
            vertices.toVector

  /** All vertices in the tiling, except those on the outer boundary. */
  def innerVertices: List[Vertex] =
    vertices.diff(boundaryVertices)

  /** Finds the ordered half-edges forming the outer boundary of the tiling. */
  def boundaryEdges: List[HalfEdge] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalUnsafe()
      case None            => List.empty

  /** For validation purposes only. */
  private[dcel] def boundaryEdgesSafer: Either[TopologyError, List[HalfEdge]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal()
      case None            => Right(List.empty)

  lazy val boundarySimplePolygon: SimplePolygon =
    SimplePolygon(
      boundaryEdges
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

  def maybeAddSimplePolygonToBoundary(
      onEdgeStartingWith: VertexId,
      angles: Vector[AngleDegree]
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addUntrustedSimplePolygonToBoundary(onEdgeStartingWith, angles)

  def maybeAddRegularPolygon(
      start: VertexId,
      end: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addRegularPolygon(start, end, polygon)

  def maybeAddSimplePolygon(
      start: VertexId,
      end: VertexId,
      angles: Vector[AngleDegree]
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addUntrustedSimplePolygon(start, end, angles)

  def doubleArea: Either[TilingError, TilingDCEL] =
    if isEmpty then
      Right(this)
    else
      boundarySimplePolygon.parallelogonDoubleIndices match
        case None if boundarySimplePolygon.isEquilateralTriangle =>
          val angles = boundarySimplePolygon.toAngles
          val origin =
            angles.indexWhere: angleDegree =>
              angleDegree == AngleDegree(60)
          val repeat = (angles.size / 3) + origin
          Right(this.rawDouble(boundaryVertices(origin), boundaryVertices(repeat), withInversion = true))
        case None                                                => Left(ValidationError("Tiling is not a parallelogon, cannot fill the whole plane."))
        case Some((origin, repeat))                              =>
          Right(this.rawDouble(boundaryVertices(origin), boundaryVertices(repeat)))

  def maybeDeleteVertex(vertexId: VertexId): Either[TilingError, TilingDCEL] =
    this.deepCopy.deleteVertex(vertexId)

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

  def toSVG(options: SvgOptions): String =
    this.toScalableVectorGraphics(options)

  def toDOT: String =
    this.toSimplifiedDOT

object TilingDCEL:

  // Private internal constructor that bypasses validation
  private[dcel] def apply(
      vertices: List[Vertex],
      halfEdges: List[HalfEdge],
      innerFaces: List[Face],
      outerFace: Face
  ): TilingDCEL =
    new TilingDCEL(vertices, halfEdges, innerFaces, outerFace)

  // Smart constructor for untrusted sources
  def fromUntrusted(
      vertices: List[Vertex],
      halfEdges: List[HalfEdge],
      innerFaces: List[Face],
      outerFace: Face
  ): Either[TilingError, TilingDCEL] =
    val candidateTiling = apply(vertices, halfEdges, innerFaces, outerFace)
    validate(candidateTiling).map: _ =>
      candidateTiling

  def empty: TilingDCEL =
    TilingDCEL(
      vertices = List.empty,
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

  def createSimplePolygon(degrees: Int*): Either[TilingError, TilingDCEL] =
    TilingBuilder.createSimplePolygon(degrees*)

  def createSimplePolygon(angles: Vector[AngleDegree]): Either[TilingError, TilingDCEL] =
    TilingBuilder.createSimplePolygon(angles)

  def createRegularPolygon(polygon: RegularPolygon): TilingDCEL =
    TilingBuilder.createRegularPolygon(polygon)
