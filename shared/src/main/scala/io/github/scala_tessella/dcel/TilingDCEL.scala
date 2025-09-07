package io.github.scala_tessella.dcel

import BigDecimalGeometry.{AngleDegree, BigPoint, hasNoAlmostEqualPoints}
import TilingAddition.*
import TilingDeletion.*
import TilingSVG.*

import scala.collection.mutable

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
    vertices.map(v => v.id -> v.coords).toMap

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  private[dcel] def findVertexUnsafe(vertexId: VertexId): Option[Vertex] =
    vertices.find(_.id == vertexId)

  def findVertex(vertexId: VertexId): Either[TilingError, Vertex] =
    findVertexUnsafe(vertexId).toRight(NotFoundError("Vertex", vertexId.value))

  def findFace(faceId: FaceId): Either[TilingError, Face] =
    faces.find(_.id == faceId).toRight(NotFoundError("Face", faceId.value))

  def findInnerFace(faceId: FaceId): Either[TilingError, Face] =
    innerFaces.find(_.id == faceId).toRight(NotFoundError("Inner face", faceId.value))

  def isBoundaryEdge(halfEdge: HalfEdge): Boolean =
    halfEdge.hasIncidentFace(outerFace)

  def findEdgeBetween(v1: Vertex, v2: Vertex): Option[HalfEdge] =
    v1.incidentEdgesUnsafe.find(_.destination.contains(v2))

  def findVerticesAndEdgeBetween(
      vertexId1: VertexId,
      vertexId2: VertexId
  ): Either[TilingError, (Vertex, Vertex, HalfEdge)] =
    for
      v1   <- findVertex(vertexId1)
      v2   <- findVertex(vertexId2)
      edge <- findEdgeBetween(v1, v2).toRight(NotFoundError("Edge", s"between $vertexId1 and $vertexId2"))
    yield (v1, v2, edge)

  private[dcel] def getAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex = findVertexUnsafe(vertexId).get
    val edges  = vertex.incidentEdgesUnsafe
    edges.map(_.angle.get)

  def getAnglesAtVertex(vertexId: VertexId): Either[TilingError, List[AngleDegree]] =
    for
      vertex     <- findVertex(vertexId)
      edges      <- vertex.incidentEdges
      maybeAngles = edges.map(_.angle)
      angles     <-
        if maybeAngles.contains(None) then
          Left(ValidationError(s"Vertex with ID $vertexId has at least one edge with no angle."))
        else
          Right(maybeAngles.flatten)
    yield angles

  private[dcel] def getInnerAnglesAtVertexUnsafe(vertexId: VertexId): List[AngleDegree] =
    val vertex = findVertexUnsafe(vertexId).get
    val edges  = vertex.incidentEdgesUnsafe
    edges.filterNot(isBoundaryEdge).map(_.angle.get)

  def getInnerAnglesAtVertex(vertexId: VertexId): Either[TilingError, List[AngleDegree]] =
    for
      vertex     <- findVertex(vertexId)
      edges      <- vertex.incidentEdges
      innerEdges  = edges.filterNot(isBoundaryEdge)
      maybeAngles = innerEdges.map(_.angle)
      angles     <-
        if (maybeAngles.contains(None))
          Left(
            ValidationError(s"Vertex with ID $vertexId has at least one inner edge with no angle defined.")
          )
        else
          Right(maybeAngles.flatten)
    yield angles

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
  private[dcel] def boundaryVerticesUnsafe: Vector[Vertex] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalUnsafe(_.origin).toVector
      case None            => Vector.empty

  def boundaryVertices: Either[TilingError, Vector[Vertex]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal(_.origin).map(_.toVector)
      case None            => Right(Vector.empty)

  private[dcel] def boundaryEdgesUnsafe: List[HalfEdge] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalUnsafe()
      case None            => List.empty

  /** Helper method to get all half-edges forming the outer boundary loop.
    */
  def boundaryEdges: Either[TilingError, List[HalfEdge]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal()
      case None            => Right(List.empty)

  private[dcel] def boundaryEdgesPathUnsafe(from: Vertex, to: Vertex): List[HalfEdge] =
    boundaryEdgesUnsafe.getPath(from, to)

  def boundaryEdgesPath(from: VertexId, to: VertexId): Either[TilingError, List[HalfEdge]] =
    for
      fromV <- findVertex(from)
      toV   <- findVertex(to)
      edges <- boundaryEdges
    yield edges.getPath(fromV, toV)

  /** Finds a boundary half-edge that originates at the vertex with the given ID.
    *
    * @param vertexId
    *   The ID of the origin vertex.
    * @return
    *   An Option containing the HalfEdge if found, otherwise None.
    */
  private def findBoundaryEdge(vertexId: VertexId): Option[HalfEdge] =
    boundaryEdges.toOption.flatMap(_.find(_.origin.id == vertexId))

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
      sides: Int
  ): Either[TilingError, TilingDCEL] =
    this.addRegularPolygonToBoundary(onEdgeStartingWithVertexId, sides)

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
    this.deleteFace(faceId)

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

  def createSimplePolygon(angles: List[AngleDegree]): Either[TilingError, TilingDCEL] =
    TilingBuilder.createSimplePolygon(angles)

  def createRegularPolygon(sides: Int): Either[TilingError, TilingDCEL] =
    TilingBuilder.createRegularPolygon(sides)

  def validateTopologically(tiling: TilingDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    // Check vertex consistency
    tiling.vertices.foreach { vertex =>

      vertex.leaving match
        case None       => errors += s"Vertex ${vertex.id} has no leaving edge"
        case Some(edge) =>
          if edge.origin ne vertex then
            errors += s"Vertex ${vertex.id} leaving edge doesn't originate from it"
    }

    // Check half-edge consistency
    tiling.halfEdges.foreach { edge =>
      edge.twin match
        case None       => errors += s"Edge from ${edge.origin.id} has no twin"
        case Some(twin) =>
          if !twin.twin.contains(edge) then
            errors += s"Edge from ${edge.origin.id} twin relationship is not symmetric"

      edge.next match
        case None       => errors += s"Edge from ${edge.origin.id} has no next edge"
        case Some(next) =>
          if !next.prev.contains(edge) then
            errors += s"Next/prev relationship broken: $edge has next edge $next which has prev edge ${next.prev}"
    }

    // Check face consistency
    tiling.faces.foreach { face =>

      face.halfEdges match
        case Left(error)  => errors += s"Face ${face.id} has a broken edge cycle: $error"
        case Right(edges) =>
          edges.foreach { edge =>

            if !edge.incidentFace.contains(face) then
              errors += s"Face consistency error: $face contains $edge which references back to another incident ${edge.incidentFace}"
          }
    }

    if tiling.faces.exists(_.outerComponent.isEmpty) then
      errors += "Face with no outer component edge"

    // This is specific to the tessellation we want, without holes, because holes are just other inner polygons
    if tiling.innerFaces.exists(_.hasHoles) then
      errors += "Face with inner holes"

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.topology))

  def validateGeometrically(tiling: TilingDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    // Check if all half-edges have an angle
    if tiling.halfEdges.exists(_.angle.isEmpty) then
      return Left(ValidationError("Tiling has at least one half-edge with no angle defined."))

    // Check angles' sum for each inner face
    tiling.innerFaces.foreach { face =>

      face.halfEdges match
        case Right(edges) =>
          val angles = edges.flatMap(_.angle)
          if angles.length == edges.length && angles.length >= 3 then
            Polygon.SimplePolygon.validatePolygonAngles(angles).left.foreach(error =>
              errors += s"Face ${face.id}: $error"
            )
        case Left(_)      => // NOTE: topological error, handled in validateTopologically
    }

    // Check angles' sum for the tiling boundary (interior view)
    tiling.boundaryVertices match
      case Right(boundaryVertices) if boundaryVertices.length >= 3 =>
        val boundaryAngles = boundaryVertices.map(_.currentInteriorAngleSum(tiling.outerFace)).toList
        if boundaryAngles.exists(_.isLeft) then
          boundaryAngles.filter(_.isLeft).map(_.swap.toOption.get).foreach(error =>
            errors += s"Boundary angles calculation failed: $error"
          )
        else
          Polygon.SimplePolygon.validatePolygonAngles(boundaryAngles.map(_.toOption.get)).left.foreach(
            error =>
              errors += s"Boundary angles sum is incorrect: $error"
          )
      case Left(_)                                                 => // NOTE: topological error
      case _                                                       => // Not enough vertices to form a polygon

    // Check angles' sum for the tiling boundary (exterior view)
    tiling.boundaryEdges match
      case Right(boundaryEdges) if boundaryEdges.length >= 3 =>
        val boundaryAngles = boundaryEdges.flatMap(_.angle)
        if boundaryAngles.exists(_.isFullCircle) then
          errors += s"Full circle boundary angles are invalid: ${boundaryAngles.mkString("; ")}"
        else
          Polygon.SimplePolygon.validatePolygonAngles(boundaryAngles.map(_.conjugate)).left.foreach(error =>
            errors += s"Boundary edge angles sum is incorrect: $error"
          )
      case Left(_)                                           => // NOTE: topological error
      case _                                                 => // Not enough edges

    // Check angles' sum for each interior vertex
    val boundaryVertices = tiling.boundaryVerticesUnsafe.toSet
    val interiorVertices = tiling.vertices.filterNot(boundaryVertices.contains)
    interiorVertices.foreach { vertex =>

      tiling.getAnglesAtVertex(vertex.id) match
        case Right(angles) =>
          val sum = angles.sum2
          if !sum.isFullCircle then
            errors += s"Angles around interior vertex ${vertex.id} do not sum to a full circle: $sum."
        case Left(error)   =>
          errors += s"Could not validate angles for interior vertex ${vertex.id} due to: $error"
    }

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.geometry))

  def validateSpatially(tiling: TilingDCEL): Either[TilingError, Unit] =
    val errors = mutable.ListBuffer[String]()

    tiling.boundaryVertices match
      case Right(boundaryVertices) =>
        if boundaryVertices.length >= 3 then
          if !boundaryVertices.map(_.coords).toList.hasNoAlmostEqualPoints() then
            errors += "Coordinates: boundary with vertices in the same position"
      case Left(error)             => // NOTE: topological error, handled in validateTopologically

    if errors.isEmpty then Right(()) else Left(TilingError.combineErrors(errors.toList, TilingError.spatial))

  def validate(tiling: TilingDCEL): Either[TilingError, Unit] =
    val topoErrors  = validateTopologically(tiling).left.toOption.map(_.message)
    val geoErrors   = validateGeometrically(tiling).left.toOption.map(_.message)
    val spaceErrors = validateSpatially(tiling).left.toOption.map(_.message)
    val allErrors   = topoErrors.toList ++ geoErrors.toList ++ spaceErrors.toList
    if allErrors.isEmpty then Right(())
    else Left(TilingError.combineErrors(allErrors, TilingError.validation))
