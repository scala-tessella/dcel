package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingAddition.*
import io.github.scala_tessella.dcel.TilingDeletion.*
import io.github.scala_tessella.dcel.TilingEquivalency.*
import io.github.scala_tessella.dcel.TilingValidation.validate
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
  ): Either[TilingError, (Vertex, Vertex, HalfEdge)] =
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

  /** Computes a mapping between each vertex and the list of its inner angles in the tiling.
    */
  def degreesMap: Map[VertexId, List[AngleDegree]] =
    vertices.map(vertex => vertex.id -> getInnerAnglesAtVertexUnsafe(vertex.id)).toMap

  /** Gets a reduced TilingDCEL made only of the polygons sharing the given vertex */
  def vertexDCEL(vertexId: VertexId): Either[NotFoundError, TilingDCEL] =
    for
      center <- findVertex(vertexId)
    yield
      // Collect incident inner faces in cyclic order around the vertex
      val incidentEdges      = center.incidentEdgesUnsafe
      val incidentInnerFaces =
        incidentEdges
          .flatMap(e => e.incidentFace.toList)
          .filterNot(_.isOuter) // exclude outerFace
          .distinct

      // Maps from original ids to cloned elements for the local DCEL
      val vMap  = scala.collection.mutable.HashMap[VertexId, Vertex]()
      val heMap = scala.collection.mutable.HashMap[
        (VertexId, VertexId),
        HalfEdge
      ]()
      val fMap  = scala.collection.mutable.HashMap[FaceId, Face]()

      // Helper to clone or reuse vertices
      def cloneVertex(v: Vertex): Vertex =
        vMap.getOrElseUpdate(v.id, Vertex(v.id, v.coords, leaving = None))

      // First pass: clone faces and boundary cycles, clone half-edges without wiring next/prev/twin
      incidentInnerFaces.foreach { f =>
        val newFace = Face(f.id, outerComponent = None, innerComponents = Nil)
        fMap.put(f.id, newFace): Unit

        val cycle = f.outerComponent.get.faceTraversalUnsafe[HalfEdge]() // ordered half-edges around face
        cycle.foreach { he =>
          val key = he.key.get
          if !heMap.contains(key) then
            val o  = cloneVertex(he.origin)
            val nh = HalfEdge(
              origin = o,
              twin = None,
              next = None,
              prev = None,
              incidentFace = None,
              angle = he.angle
            )
            heMap.put(key, nh): Unit
            // set leaving edge for vertex if missing
            if o.leaving.isEmpty then o.leaving = Some(nh)
        }
      }

      // Second pass: set next/prev/incidentFace for inner faces
      incidentInnerFaces.foreach { f =>
        val newFace            = fMap(f.id)
        val cycle              = f.outerComponent.get.faceTraversalUnsafe[HalfEdge]()
        var firstNew: HalfEdge = null
        var prevNew: HalfEdge  = null
        cycle.foreach { he =>
          val nh = heMap(he.key.get)
          nh.incidentFace = Some(newFace)
          if firstNew eq null then firstNew = nh
          if prevNew != null then
            prevNew.next = Some(nh)
            nh.prev = Some(prevNew)
          prevNew = nh
        }
        // close the cycle
        prevNew.next = Some(firstNew)
        firstNew.prev = Some(prevNew)
        newFace.outerComponent = Some(firstNew)
      }

      // Third pass: wire twins when both sides are inside the local subgraph
      incidentInnerFaces.foreach { f =>
        val cycle = f.outerComponent.get.faceTraversalUnsafe[HalfEdge]()
        cycle.foreach { he =>
          val nh = heMap(he.key.get)
          he.twin.foreach { t =>

            if heMap.contains(t.key.get) then
              val nt = heMap(t.key.get)
              nh.twin = Some(nt)
              nt.twin = Some(nh)
          }
        }
      }

      // Build outer boundary half-edges for any inner half-edge whose twin is missing
      val outerFace     = Face.outer
      val boundaryStubs = scala.collection.mutable.ArrayBuffer[HalfEdge]()

      heMap.values.foreach { nh =>

        if nh.twin.isEmpty then
          // Create boundary twin
          val b = HalfEdge(
            origin = nh.next.get.origin, // boundary runs opposite direction
            twin = Some(nh),
            next = None,
            prev = None,
            incidentFace = Some(outerFace),
            angle = nh.angle.map(_.conjugate)
          )
          nh.twin = Some(b)
          boundaryStubs += b
      }

      // Stitch boundary half-edges into chains by following inner prev-links
      // For a boundary edge b (twin = nh), its next boundary should correspond to nh.prev's twin boundary.
      // That next boundary must start at nh.prev.destination (which equals b.destination).
      val keyOf: HalfEdge => (VertexId, VertexId) = he => (he.origin.id, he.destination.get.id)
      val stubByKey                               = boundaryStubs.map(b => keyOf(b) -> b).toMap

      def nextBoundaryOf(b: HalfEdge): Option[HalfEdge] =
        val inner     = b.twin.get
        val innerPrev = inner.prev.get
        // the boundary twin corresponding to innerPrev should be the boundary whose origin is innerPrev.destination
        // i.e., key = (innerPrev.destination, innerPrev.origin) in terms of vertex ids
        val wantedKey = (innerPrev.destination.get.id, innerPrev.origin.id)
        stubByKey.get(wantedKey)

      // Build all boundary cycles (some chains may share vertices; we only accept closed loops)
      val visited        = scala.collection.mutable.HashSet[(VertexId, VertexId)]()
      val boundaryCycles = scala.collection.mutable.ArrayBuffer[HalfEdge]()

      boundaryStubs.foreach { start =>
        val startKey = keyOf(start)
        if !visited.contains(startKey) then
          var cur   = start
          var first = start
          var ok    = true
          while ok && !visited.contains(keyOf(cur)) do
            visited += keyOf(cur)
            nextBoundaryOf(cur) match
              case Some(n) =>
                cur.next = Some(n)
                n.prev = Some(cur)
                cur = n
              case None    =>
                ok = false
          // close only if we returned to first
          if ok && (cur eq first) then
            boundaryCycles += first
      }

      // If we have no complete outer boundary cycle yet, attempt to order stubs around the outside
      // as a fallback to avoid leaving dangling prev/next.
      if boundaryCycles.isEmpty && boundaryStubs.nonEmpty then
        val ordered = boundaryStubs.toList.orderBoundary
        if ordered.nonEmpty then
          ordered.linkInCycle()
          boundaryCycles += ordered.head

      // Assign one of the cycles (if any) as the outer component
      if boundaryCycles.nonEmpty then
        outerFace.outerComponent = Some(boundaryCycles.head)

      // Assemble final collections
      val newVertices = vMap.values.toList
      val newInner    = fMap.values.toList
      val newHalf     = (heMap.values ++ boundaryStubs).toList

      // Fix boundary angles so that at every outer vertex:
      // boundaryAngle = conjugate(sum(inner incident angles at that vertex))
      outerFace.outerComponent.foreach { start =>
        val boundaryLoop = start.faceTraversalUnsafe[HalfEdge]()
        boundaryLoop.foreach { be =>
          val v = be.origin
          val incidentAtV = newHalf.filter(_.origin eq v)
          val innerSumAtV = incidentAtV
            .filterNot(_.hasIncidentFace(outerFace))
            .flatMap(_.angle)
            .sumExact
          be.angle = Some(innerSumAtV.conjugate)
        }
      }

      // Return new DCEL
      TilingDCEL(
        vertices = newVertices,
        halfEdges = newHalf,
        innerFaces = newInner,
        outerFace = outerFace
      )

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
  private[dcel] def boundaryVerticesSafer: Either[TilingError, Vector[Vertex]] =
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
  private[dcel] def boundaryEdgesSafer: Either[TilingError, List[HalfEdge]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal()
      case None            => Right(List.empty)

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
      faceIdsOnEdges: Boolean = false
  ): String =
    this.toScalableVectorGraphics(
      strokeWidth,
      padding,
      scale,
      showHalfEdgeTraversal,
      leavingEdgeMarkers,
      faceIdsOnEdges
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
