package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon

object TilingAddition:

  def calculateNewVertices(sides: Int, p1: BigPoint, p2: BigPoint): List[BigPoint] =
    val angle = RegularPolygon(sides).alphaDegree
    val rotation = (AngleDegree(180) + angle).toBigRadian
    val startingDirection = p1.angleTo(p2)

    LazyList.unfold((p2, 3, startingDirection)) { case (curr, step, direction) =>
      if step > sides then None
      else
        val nextDirection = direction + rotation
        val next = curr.plus(BigPoint.fromPolar(1, nextDirection))
        Some(next, (next, step + 1, nextDirection))
    }.toList

  private def createVertices(points: List[BigPoint], startingIndex: Int): List[Vertex] =
    points.zipWithIndex.map { (point, index) =>
      Vertex(s"V${startingIndex + index + 1}", point)
    }

  // Extract polygon angle calculation
  private def polygonAngle(sides: Int): AngleDegree =
    RegularPolygon(sides).alphaDegree

  // More descriptive boundary angle calculation
  private def boundaryAngleForVertex(
    vertex: Vertex,
    outerFace: Face,
    additionalInteriorAngle: AngleDegree
  ): AngleDegree =
    val currentInteriorSum = vertex.getCurrentInteriorAngleSum(outerFace)
    (currentInteriorSum + additionalInteriorAngle).conjugate

  // Use proper case class pattern matching instead of List extraction
  private def createEdgePairs(
    vertices: List[Vertex],
    outerFace: Face,
    newFace: Face,
    startVertexAngle: AngleDegree,
    polygonAngle: AngleDegree
  ): List[(HalfEdge, HalfEdge)] =
    vertices.sliding(2).zipWithIndex.map {
      case (origin :: destination :: Nil, index) =>
        val boundaryAngle = if index == 0 then startVertexAngle else polygonAngle.conjugate
        HalfEdge.createTwinHalfEdges(
          origin, destination, outerFace, newFace,
          boundaryAngle, polygonAngle
        )
      case _ =>
        throw IllegalArgumentException("Invalid vertex sequence")
    }.toList

  // Simplify face generation using template interpolation
  private def generateFaceId(existingFaceCount: Int): String =
    s"F${existingFaceCount + 1}"

  extension (tilingDCEL: TilingDCEL)

    def addRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      for
        _ <- TilingBuilder.validateSides(sides, "regular")
        boundaryEdges <- tilingDCEL.getBoundaryEdges
        edgeToBuildOn <- boundaryEdges
          .find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        (startVertex, endVertex) <- edgeToBuildOn.endpointsAsVertices
          .toRight("Edge has no destination vertex.")
      yield
        given outerFace: Face = tilingDCEL.outerFace

        val polyAngle = polygonAngle(sides)

        // Calculate boundary angles
        val boundaryAngles = BoundaryAngles(
          start = boundaryAngleForVertex(startVertex, outerFace, polyAngle),
          end = boundaryAngleForVertex(endVertex, outerFace, polyAngle),
          newVertices = polyAngle.conjugate
        )

        var sharedEdges = List(edgeToBuildOn)

        // Add possible shared edges
        var startCheck = boundaryAngles.start
        var startEdge = edgeToBuildOn.prev.get
        var startCounter = 0
        while startCheck.isFullCircle
        do
          startCheck = boundaryAngleForVertex(startEdge.origin, outerFace, polyAngle)
          sharedEdges = startEdge :: sharedEdges
          startEdge = startEdge.prev.get
          startCounter += 1

        var endCheck = boundaryAngles.end
        var endEdge = edgeToBuildOn.next.get
        var endCounter = 0
        while endCheck.isFullCircle
        do
          endCheck = boundaryAngleForVertex(endEdge.destination.get, outerFace, polyAngle)
          sharedEdges = sharedEdges :+ endEdge
          endEdge = endEdge.next.get
          endCounter += 1

        val hasMoreThanOneSharedEdge = sharedEdges.length > 1

        // Different start and end vertex
        val revisedStartVertex = startEdge.destination.get
        val revisedEndVertex = endEdge.origin

        // Different boundary angles
        val revisedBoundaryAngles = BoundaryAngles(
          start = startCheck,
          end = endCheck,
          newVertices = polyAngle.conjugate
        )

        // Different boundary
        val completeBoundary = BoundaryState(Some(startEdge), Some(endEdge))

        // Create new components
        val vertexPoints = calculateNewVertices(sides, endVertex.coords, startVertex.coords)
        val revisedVertexPoints = vertexPoints.drop(startCounter).dropRight(endCounter)
        val newVertices = createVertices(revisedVertexPoints, tilingDCEL.vertices.size)

        val newFace = Face(generateFaceId(tilingDCEL.innerFaces.size))

        val allVertices = revisedStartVertex :: newVertices ::: revisedEndVertex :: Nil

        val edgePairs = createEdgePairs(allVertices, outerFace, newFace, revisedBoundaryAngles.start, polyAngle)
        val (newBoundaryEdges, newInnerEdges) = edgePairs.unzip

        // Update existing structures
        updateExistingStructures(
          sharedEdges, newFace, polyAngle,
          newBoundaryEdges, completeBoundary, revisedBoundaryAngles
        )

        // Link new face edges
        linkNewFaceEdges(edgeToBuildOn, sharedEdges, newInnerEdges.reverse, newFace)

        // Connect to boundary
        connectNewBoundaryEdges(newBoundaryEdges, completeBoundary, outerFace, sharedEdges)

        // Update vertex leaving edges
        updateVertexLeavingEdges(revisedStartVertex :: newVertices, newBoundaryEdges)

        // Return new DCEL with updated components
        tilingDCEL.copy(
          vertices = tilingDCEL.vertices ::: newVertices,
          halfEdges = tilingDCEL.halfEdges ::: newBoundaryEdges ::: newInnerEdges,
          innerFaces = tilingDCEL.innerFaces :+ newFace
        )

  // Helper case classes for better structure
  private case class BoundaryAngles(
    start: AngleDegree,
    end: AngleDegree,
    newVertices: AngleDegree
  )

  private case class BoundaryState(
    prev: Option[HalfEdge],
    next: Option[HalfEdge]
  )

  private def updateExistingStructures(
    sharedEdges: List[HalfEdge],
    newFace: Face,
    polyAngle: AngleDegree,
    newBoundaryEdges: List[HalfEdge],
    originalBoundary: BoundaryState,
    boundaryAngles: BoundaryAngles
  ): Unit =
    // Update shared edges
    sharedEdges.foreach(_.incidentFace = Some(newFace))
    sharedEdges.foreach(_.angle = Some(polyAngle))

    // Update last boundary edge angle
    newBoundaryEdges.lastOption.foreach(_.angle = Some(boundaryAngles.newVertices))

    // Update existing boundary edge from end vertex
    originalBoundary.next.foreach { nextEdge =>
      if nextEdge.origin.id == sharedEdges.last.destination.map(_.id).getOrElse("") then
        nextEdge.angle = Some(boundaryAngles.end)
    }

    // Update boundary in special shared edges case
    if sharedEdges.length > 1 && newBoundaryEdges.length == 1 then
      newBoundaryEdges.head.angle = Some(newBoundaryEdges.head.angle.get - polyAngle)

  private def linkNewFaceEdges(
    edgeToBuildOn: HalfEdge,
    sharedEdges: List[HalfEdge],
    reversedInnerEdges: List[HalfEdge],
    newFace: Face
  ): Unit =
    val allInnerEdges = sharedEdges ::: reversedInnerEdges
    allInnerEdges.linkInCycle()
    newFace.outerComponent = Some(edgeToBuildOn)

  private def connectNewBoundaryEdges(
     newBoundaryEdges: List[HalfEdge],
     originalBoundary: BoundaryState,
     outerFace: Face,
     sharedEdges: List[HalfEdge]
  ): Unit =
    HalfEdge.insertBoundarySegment(
      originalBoundary.prev.get,
      originalBoundary.next.get,
      newBoundaryEdges
    )

    // Update outer face component if necessary
    if outerFace.outerComponent.exists(e => sharedEdges.contains(e)) then
      outerFace.outerComponent = newBoundaryEdges.headOption

  private def updateVertexLeavingEdges(
    verticesWithNewEdges: List[Vertex],
    newBoundaryEdges: List[HalfEdge],
  ): Unit =
    verticesWithNewEdges.zip(newBoundaryEdges).foreach { (vertex, edge) =>
      vertex.leaving = Some(edge)
    }
