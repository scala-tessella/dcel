package io.github.scala_tessella
package dcel

import BigDecimalGeometry.{AngleDegree, hasNoAlmostEqualPoints}
import TilingAddition.*
import TilingDeletion.*
import TilingSVG.*
import spire.implicits.monoidOps

import scala.collection.mutable

/**
 * Represents the entire tiling structure as a container for its components.
 *
 * @param vertices   List of all vertices in the tiling.
 * @param halfEdges  List of all half-edges in the tiling.
 * @param innerFaces List of the tiling's interior faces.
 * @param outerFace  The single, unbounded outer face of the tiling.
 */
case class TilingDCEL(
  vertices: List[Vertex],
  halfEdges: List[HalfEdge],
  innerFaces: List[Face],
  outerFace: Face
):

  /** @return a list of all faces, both inner and outer */
  def faces: List[Face] =
    outerFace :: innerFaces

  def findVertex(id: String): Option[Vertex] =
    vertices.find(_.id == id)

  def findFace(id: String): Option[Face] =
    faces.find(_.id == id)

  def findEdgeBetween(v1: Vertex, v2: Vertex): Option[HalfEdge] =
    v1.incidentEdges.find(_.destination.contains(v2))

  def getAnglesAtVertex(vertexId: String): Either[String, List[AngleDegree]] =
    for
      vertex <- findVertex(vertexId).toRight(s"Vertex with ID $vertexId not found.")
      edges <- vertex.incidentEdgesSafe
      maybeAngles = edges.map(_.angle)
      angles <- if (maybeAngles.contains(None))
        Left(s"Vertex with ID $vertexId has at least one edge with no angle.")
      else
        Right(maybeAngles.flatten)
    yield angles

  def isConnected: Boolean =
    if innerFaces.isEmpty then true
    else
      val adjacency = Face.adjacencyMap(innerFaces)
      val reachable = Face.breadthFirstSearch(innerFaces.head, adjacency)
      reachable.size == innerFaces.size

  /**
   * Finds the outer boundary of the tiling.
   *
   * The traversal follows the half-edges of the outer face, which are linked in
   * a clockwise order around the perimeter.
   *
   * @return A Vector of Vertices forming the perimeter, in clockwise order.
   *         Returns an empty Vector if the outer face has no boundary component.
   */
  def boundary: Vector[Vertex] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversal(_.origin).toVector
      case None => Vector.empty

  def boundarySafe: Either[String, Vector[Vertex]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalWithGuards(_.origin).map(_.toVector)
      case None => Right(Vector.empty)

  /**
   * Helper method to get all half-edges forming the outer boundary loop.
   */
  def getBoundaryEdges: Either[String, List[HalfEdge]] =
    outerFace.outerComponent match
      case Some(startEdge) => startEdge.faceTraversalWithGuards()
      case None => Right(List.empty)

  def getBoundaryEdgesPath(from: Vertex, to: Vertex): List[HalfEdge] =
    val allBoundaryEdges = getBoundaryEdges.getOrElse(List.empty)
    val startEdgeOpt = allBoundaryEdges.find(_.origin == from)

    startEdgeOpt match
      case Some(startEdge) =>
        val holeEdgesList = mutable.ListBuffer[HalfEdge]()
        var currentEdge = startEdge

        while (currentEdge.destination.get != to && !holeEdgesList.contains(currentEdge))
          holeEdgesList += currentEdge
          currentEdge = currentEdge.next.get

        if currentEdge.destination.get == to then
          holeEdgesList += currentEdge

        holeEdgesList.toList
      case None => List.empty

  /**
   * Finds a boundary half-edge that originates at the vertex with the given ID.
   *
   * @param vertexId The ID of the origin vertex.
   * @return An Option containing the HalfEdge if found, otherwise None.
   */
  private def findBoundaryEdge(vertexId: String): Option[HalfEdge] =
    getBoundaryEdges.toOption.flatMap(_.find(_.origin.id == vertexId))

  def maybeAddRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
    this.addRegularPolygon(sides, onEdgeStartingWithVertexId)
    
  def maybeDeletePolygon(faceId: String): Either[String, TilingDCEL] =
    this.deletePolygon(faceId)  

  /**
   * Generates an SVG representation of the tiling.
   * The width, height, and viewBox are automatically calculated to fit the tiling at the given scale.
   *
   * @param strokeWidth The width of the edge lines.
   * @param padding     The padding around the tiling within the SVG viewBox.
   * @param scale       The factor by which to scale the tiling coordinates.
   * @return A String containing the SVG markup.
   */
  def toSVG(
    strokeWidth: Double = 1.0,
    padding: Double = 20.0,
    scale: Double = 50.0,
    showHalfEdgeTraversal: Boolean = false,
    leavingEdgeMarkers: Boolean = false,
    faceIdsOnEdges: Boolean = false
  ): String =
    this.toScalableVectorGraphics(strokeWidth, padding, scale, showHalfEdgeTraversal, leavingEdgeMarkers, faceIdsOnEdges)

object TilingDCEL:

  def empty: TilingDCEL =
    TilingBuilder.empty

  def createSimplePolygon(angles: List[AngleDegree]): Either[String, TilingDCEL] =
    TilingBuilder.createSimplePolygon(angles)

  def createRegularPolygon(sides: Int): Either[String, TilingDCEL] =
    TilingBuilder.createRegularPolygon(sides): Either[String, TilingDCEL]

  def validate(tiling: TilingDCEL): Either[String, Unit] =
    val errors = mutable.ListBuffer[String]()

    // Check vertex consistency
    tiling.vertices.foreach { vertex =>
      vertex.leaving match
        case None => errors += s"Vertex ${vertex.id} has no leaving edge"
        case Some(edge) =>
          if edge.origin ne vertex then
            errors += s"Vertex ${vertex.id} leaving edge doesn't originate from it"
    }

    // Check half-edge consistency
    tiling.halfEdges.foreach { edge =>
      edge.twin match
        case None => errors += s"Edge from ${edge.origin.id} has no twin"
        case Some(twin) =>
          if twin.twin.contains(edge) then () // OK
          else errors += s"Edge from ${edge.origin.id} twin relationship is not symmetric"

      edge.next match
        case None => errors += s"Edge from ${edge.origin.id} has no next edge"
        case Some(next) =>
          if next.prev.contains(edge) then () // OK
          else errors += s"Next/prev relationship broken: $edge has next edge $next which has prev edge ${next.prev}"
    }

    // Check face consistency
    (tiling.outerFace :: tiling.innerFaces).foreach { face =>
      face.halfEdges match
        case Left(error) => errors += error
        case Right(edges) =>
          edges.foreach { edge =>
            if !edge.incidentFace.contains(face) then
              errors += s"Face consistency error: $face contains $edge which references back to another incident ${edge.incidentFace}"
          }
    }

    if tiling.faces.exists(_.outerComponent.isEmpty) then
      errors += "Face with outer issues"

    // This is specific to the tessellation we want, without holes, because holes are just other inner polygons
    if tiling.faces.exists(_.hasHoles) then
      errors += "Face with inner holes"

    // Check angles' sum for each inner face
    tiling.innerFaces.foreach { face =>
      face.halfEdges match
        case Right(edges) =>
          val angles = edges.flatMap(_.angle)
          if angles.length == edges.length then
            if angles.length >= 3 then
              Polygon.SimplePolygon.validatePolygonAngles(angles).left.foreach(error =>
                errors += s"Face ${face.id}: $error"
              )
          else
            errors += s"Face ${face.id} has missing angles on some edges."
        case Left(error) =>
          errors += s"Could not validate angles for face ${face.id} due to: $error"
    }

    // Check angles' sum for the tiling boundary
    tiling.boundarySafe match
      case Right(boundaryVertices) =>
        if boundaryVertices.length >= 3 then
          val boundaryAngles =
            boundaryVertices.map { _.getCurrentInteriorAngleSumSafe(tiling.outerFace) }.toList
          if boundaryAngles.exists(_.isLeft) then
            boundaryAngles.filter(_.isLeft).map(_.swap.toOption.get).foreach(error => errors += s"Boundary angles: $error")
          else
            Polygon.SimplePolygon.validatePolygonAngles(boundaryAngles.map(_.toOption.get)).left.foreach(error =>
              errors += s"Boundary: $error"
            )
      case Left(error) =>
        errors += s"Could not validate boundary angles due to: $error"

    // Check angles' sum for the tiling boundary seen from the outer edges
    tiling.getBoundaryEdges match
      case Right(boundaryEdges) =>
        if boundaryEdges.length >= 3 then
          val boundaryAngles = boundaryEdges.map(_.angle)
          if boundaryAngles.exists(_.isEmpty) then
            errors += s"Undefined boundary angles: ${boundaryAngles.mkString("; ")}"
          else if boundaryAngles.exists(_.get.isFullCircle) then
            errors += s"Full circle boundary angles: ${boundaryAngles.mkString("; ")}"
          else
            Polygon.SimplePolygon.validatePolygonAngles(boundaryAngles.map(_.get.conjugate)).left.foreach(error =>
              errors += s"Boundary edge: $error"
            )
      case Left(error) =>
        errors += s"Could not validate boundary edges due to: $error"

    // Check angles' sum for each vertex
    tiling.vertices.foreach { vertex =>
      tiling.getAnglesAtVertex(vertex.id) match
        case Right(angles) =>
          val sum = angles.fold(AngleDegree(0))(_ + _)
          if !sum.isFullCircle then
            errors += s"Vertex ${vertex.id} is not a full circle: $sum."
        case Left(error) =>
          errors += s"Could not validate angles for vertex ${vertex.id} due to: $error"
    }

    if errors.isEmpty then Right(()) else Left(errors.mkString("; "))

  def spatiallyValidate(tiling: TilingDCEL): Either[String, Unit] =
    val errors = mutable.ListBuffer[String]()

    tiling.boundarySafe match
      case Right(boundaryVertices) =>
        if boundaryVertices.length >= 3 then
          if !boundaryVertices.map(_.coords).toList.hasNoAlmostEqualPoints() then
            errors += "Coordinates: boundary with vertices in the same position"
      case Left(error) =>
        errors += s"Could not validate boundary angles due to: $error"

    if errors.isEmpty then Right(()) else Left(errors.mkString("; "))
