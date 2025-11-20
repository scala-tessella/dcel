package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingBuilder.*
import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingUniformity.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.Tree
import io.github.scala_tessella.dcel.conversion.TilingDOT.*
import io.github.scala_tessella.dcel.conversion.TilingSVG.*
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
    vertices.map(vertex => vertex.id -> vertex.coords).toMap

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  private[dcel] def findVertexUnsafe(vertexId: VertexId): Option[Vertex] =
    vertices.find(_.id == vertexId)

  def findVertex(vertexId: VertexId): Either[NotFoundError, Vertex] =
    findVertexUnsafe(vertexId).toRight(NotFoundError("Vertex", vertexId.value))

  def findFace(faceId: FaceId): Either[NotFoundError, Face] =
    faces.find(_.id == faceId).toRight(NotFoundError("Face", faceId.value))

  def findInnerFace(faceId: FaceId): Either[NotFoundError, Face] =
    innerFaces.find(_.id == faceId).toRight(NotFoundError("Inner face", faceId.value))

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
        v1.findEdgeBetweenUnsafe(v2).toRight(NotFoundError("Edge", s"between $vertexId1 and $vertexId2"))
    yield (v1, v2, edge)

  def innerFacesVertices: List[(FaceId, List[Vertex])] =
    innerFaces.map(face => (face.id, face.getVerticesUnsafe))

  def findInnerFaceVertices(faceId: FaceId): Either[NotFoundError, List[Vertex]] =
    for
      face <- findInnerFace(faceId)
    yield face.getVerticesUnsafe

  private[dcel] def getAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex = findVertexUnsafe(vertexId).get
    val edges  = vertex.incidentEdgesUnsafe
    edges.map(_.angle.get)

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
    val vertex        = findVertexUnsafe(vertexId).get
    val edges         = vertex.incidentEdgesUnsafe
    val filteredEdges =
      edges.indexWhere(isBoundaryEdge) match
        case -1 => edges
        case i  => edges.startAt(i).tail
    filteredEdges.map(_.angle.get)

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
    this.getStructureAtVertex(vertexId, distance).map {
      case (newVertices, newHalfEdges, localOuter, newInnerFaces) =>
        TilingDCEL(
          vertices = newVertices,
          halfEdges = newHalfEdges,
          innerFaces = newInnerFaces,
          outerFace = localOuter
        )
    }

  def uniformityTree: Tree[List[VertexId]] =
    this.uniformityTreeUncompressed().compress(_ ::: _)

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
      case Some(startEdge) => startEdge.faceTraversalUnsafe(_.origin).toVector
      case None            => Vector.empty

  /** For validation purposes only. */
  private[dcel] def boundaryVerticesSafer: Either[TopologyError, Vector[Vertex]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal(_.origin).map(_.toVector)
      case None            => Right(Vector.empty)

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

  def boundarySimplePolygon: SimplePolygon =
    SimplePolygon(boundaryEdges.map(_.angle.get.conjugate).toVector)

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
      onEdgeStartingWithVertexId: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addRegularPolygonToBoundary(onEdgeStartingWithVertexId, polygon)

  def maybeAddSimplePolygonToBoundary(
      onEdgeStartingWithVertexId: VertexId,
      simple: SimplePolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addSimplePolygonToBoundary(onEdgeStartingWithVertexId, simple)

  def maybeAddRegularPolygon(
      startVertexId: VertexId,
      endVertexId: VertexId,
      polygon: RegularPolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addRegularPolygon(startVertexId, endVertexId, polygon)

  def maybeAddSimplePolygon(
      startVertexId: VertexId,
      endVertexId: VertexId,
      simple: SimplePolygon
  ): Either[TilingError, TilingDCEL] =
    this.deepCopy.addSimplePolygon(startVertexId, endVertexId, simple)

  /** Doubles the tiling if it is a parallelogon.
   * If yes, adds another identical joining them side by side
   *
   * @return
   * Either a TilingError if the tiling is not a parallelogon
   * or a new TilingDCEL with the doubled tiling
   */
  def growDouble: Either[TilingError, TilingDCEL] =
    if isEmpty then
      Right(this)
    else
      boundarySimplePolygon.parallelogonEquivalences match
        case Nil => Left(ValidationError("Tiling is not a parallelogon"))
        case groups =>
          // Choose the pair of equivalent boundary vertices that defines the translation side
          val group =
            groups.find(_.size == 4).getOrElse(
              groups.find(_.size == 3).get
            )
          val two =
            if (group(1) - group.head) >= vertices.size / 2 then group.takeRight(2)
            else group.take(2)
          val third = if group.size == 3 then group.diff(two).head else group(2)
          println(s"Indices, origin: ${two.head} and repeat: ${two.last} and repeatOnOtherAxis: $third")

          // NOTE: `group`/`two` index into the polygon's vertex sequence.
          val origin = boundaryVertices(two.head)
          val repeat = boundaryVertices(two.last)
          val repeatOnOtherAxis = boundaryVertices(third)

          println(s"Vertices, origin: $origin and repeat: $repeat and repeatOnOtherAxis: $repeatOnOtherAxis")

          // Compute the translation vector from origin to repeat
          val dx = repeat.coords.x - origin.coords.x
          val dy = repeat.coords.y - origin.coords.y
          val translation = BigPoint(dx, dy)

          // ---------------------------------------------------------------------------
          // 1. Clone and translate vertices
          // ---------------------------------------------------------------------------
          val maxVertexId = vertices.map(vertex => idFromVertexId(vertex.id)).maxOption.get
          var nextVertexId = maxVertexId + 1

          val vertexMap: Map[Vertex, Vertex] =
            vertices.map { v =>
              val newV = Vertex(
                id = vertexIdV(nextVertexId),
                coords = BigPoint(
                  v.coords.x + translation.x,
                  v.coords.y + translation.y
                ),
                leaving = None
              )
              nextVertexId += 1
              v -> newV
            }.toMap

          val clonedVertices: List[Vertex] = vertexMap.values.toList
          println(s"Cloned vertices: $clonedVertices")

          // ---------------------------------------------------------------------------
          // 2. Clone inner faces (outer face is shared, not duplicated)
          // ---------------------------------------------------------------------------
          val existingFaceIds = innerFaces.map(face => idFromFaceId(face.id))
          val baseFaceId = if existingFaceIds.isEmpty then 0 else existingFaceIds.max
          var nextFaceId = baseFaceId + 1

          val faceMapPairs: List[(Face, Face)] =
            innerFaces.map { f =>
              val newF = Face(faceIdF(nextFaceId))
              nextFaceId += 1
              f -> newF
            }

          val faceMap: Map[Face, Face] = faceMapPairs.toMap
          val clonedInnerFaces: List[Face] = faceMap.values.toList
          println(s"Cloned inner faces: $clonedInnerFaces")

          // ---------------------------------------------------------------------------
          // 3. Clone half-edges (two-pass: create, then wire)
          // ---------------------------------------------------------------------------
          val edgeMap = scala.collection.mutable.Map.empty[HalfEdge, HalfEdge]

          // First pass: create edges with origin / incidentFace / angle, but no links yet
          halfEdges.foreach { oldE =>
            val newOrigin = vertexMap(oldE.origin)
            val newFaceOpt =
              oldE.incidentFace.map { f =>
                if f eq outerFace then outerFace
                else faceMap(f)
              }

            val newE = HalfEdge(
              origin = newOrigin,
              twin = None,
              incidentFace = newFaceOpt,
              next = None,
              prev = None,
              angle = oldE.angle
            )

            edgeMap += oldE -> newE
          }

          // Second pass: wire twin / next / prev
          edgeMap.foreach { case (oldE, newE) =>
            newE.twin = oldE.twin.map(edgeMap)
            newE.next = oldE.next.map(edgeMap)
            newE.prev = oldE.prev.map(edgeMap)
          }

          val clonedHalfEdges: List[HalfEdge] = edgeMap.values.toList

          // ---------------------------------------------------------------------------
          // 4. Fix leaving edge for cloned vertices
          // ---------------------------------------------------------------------------
          vertexMap.foreach { case (oldV, newV) =>
            newV.leaving = oldV.leaving.map(edgeMap)
          }

          // ---------------------------------------------------------------------------
          // 5. Fix outerComponent / innerComponents for cloned faces
          // ---------------------------------------------------------------------------
          faceMapPairs.foreach { case (oldF, newF) =>
            newF.outerComponent = oldF.outerComponent.map(edgeMap)
            newF.innerComponents = oldF.innerComponents.map(_.map(edgeMap))
          }

          // ---------------------------------------------------------------------------
          // 6. Stitch along the chosen boundary segment (repeat -> repeatOnOtherAxis)
          // ---------------------------------------------------------------------------


          // -----------------------------------------------------------------------
          // 7. Build final doubled tiling and validate
          // -----------------------------------------------------------------------
          TilingDCEL.fromUntrusted(
            vertices = vertices ++ clonedVertices,
            halfEdges = ???,
            innerFaces = innerFaces ++ clonedInnerFaces,
            outerFace = outerFace
          )

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
    validate(candidateTiling).map(_ => candidateTiling)

  def empty: TilingDCEL =
    TilingDCEL(
      vertices = List.empty,
      halfEdges = List.empty,
      innerFaces = List.empty,
      outerFace = Face.outer
    )

  def createSimplePolygon(simple: SimplePolygon): Either[TilingError, TilingDCEL] =
    TilingBuilder.createSimplePolygon(simple)

  def createRegularPolygon(polygon: RegularPolygon): TilingDCEL =
    TilingBuilder.createRegularPolygon(polygon)
